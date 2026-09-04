defmodule NativeElixirPdfUtilities.Validators.OutlineValidator do
  @moduledoc """
  Validation and normalization for PDF document outlines.

  The validator is the single source of truth for caller-provided outline
  trees and outline structures read from existing PDFs. Prepared items use
  one-based page numbers so writers can resolve them against their own page
  object allocation.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Pdf.InfoCodec
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  @item_keys [:title, :page, :view, :open, :children]

  @typedoc "A supported PDF destination view."
  @type view ::
          :fit
          | :fit_b
          | {:fit_h, number() | nil}
          | {:fit_v, number() | nil}
          | {:fit_bh, number() | nil}
          | {:fit_bv, number() | nil}
          | {:fit_r, number(), number(), number(), number()}
          | {:xyz, number() | nil, number() | nil, number() | nil}

  @typedoc "A normalized outline item."
  @type item :: %{
          required(:title) => String.t(),
          required(:page) => pos_integer() | nil,
          required(:view) => view(),
          required(:open) => boolean(),
          required(:children) => [item()]
        }

  @doc """
  Validates and normalizes a caller-provided outline tree.
  """
  @spec normalize(term(), non_neg_integer()) ::
          {:ok, [item()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def normalize(input, page_count) do
    case is_list(input) and is_integer(page_count) and page_count >= 0 do
      true ->
        case normalize_items(input, page_count, 1, %{items: 0, title_bytes: 0}, []) do
          {:ok, items, _budget} -> {:ok, items}
          {:error, _error} = outline_error -> outline_error
        end

      false ->
        error(:invalid_outlines, "outlines must be a list and page count must be valid")
    end
  end

  @doc """
  Reads and normalizes the active outline tree from a shared PDF context.

  A document without an outline returns an empty list.
  """
  @spec extract(PdfValidator.context()) ::
          {:ok, [item()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def extract(context) do
    case context do
      %{document: document, catalog: catalog, pages: pages}
      when is_map(document) and is_map(catalog) and is_list(pages) ->
        page_lookup =
          pages |> Enum.with_index(1) |> Map.new(fn {page, number} -> {page.ref, number} end)

        case Map.get(catalog, "Outlines") do
          nil ->
            {:ok, []}

          {:ref, root_ref} = root_value ->
            with {:ok, root} <- PdfValidator.dictionary(document, root_value),
                 :ok <- validate_outline_root(root),
                 {:ok, named_destinations} <- named_destinations(document, catalog),
                 {:ok, items, _budget} <-
                   read_siblings(
                     document,
                     Map.get(root, "First"),
                     root_ref,
                     nil,
                     Map.get(root, "Last"),
                     page_lookup,
                     named_destinations,
                     1,
                     %{items: 0, title_bytes: 0, seen: %{}},
                     []
                   ) do
              {:ok, items}
            else
              {:error, _error} = outline_error -> outline_error
            end

          _ ->
            error(:invalid_pdf_input, "catalog Outlines entry must be an indirect reference")
        end

      _ ->
        error(:invalid_pdf_input, "shared PDF validation context is malformed")
    end
  end

  @doc false
  @spec remap_for_selection([item()], [pos_integer()]) :: [item()]
  def remap_for_selection(items, selected_page_numbers) do
    page_map =
      selected_page_numbers
      |> Enum.with_index(1)
      |> Map.new(fn {source_page, output_page} -> {source_page, output_page} end)

    remap_items(items, page_map, [])
  end

  @doc false
  @spec count_items([item()]) :: non_neg_integer()
  def count_items(items) do
    Enum.reduce(items, 0, fn item, count -> count + 1 + count_items(item.children) end)
  end

  @doc false
  @spec validate_incremental_capacity(PdfValidator.context(), [item()]) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_incremental_capacity(context, items) do
    case context do
      %{document: %{trailer: %{"Size" => size}}} when is_integer(size) and size > 0 ->
        generated_objects = if items == [], do: 0, else: count_items(items) + 1

        case size + generated_objects <= Limits.get(:max_pdf_objects) + 1 do
          true -> :ok
          false -> limit_error("PDF object count cannot accommodate an outline update")
        end

      _ ->
        error(:invalid_pdf_input, "active PDF trailer Size is malformed")
    end
  end

  defp normalize_items(items, page_count, depth, budget, normalized) do
    cond do
      items != [] and depth > Limits.get(:max_pdf_outline_depth) ->
        limit_error("outline nesting depth exceeds the limit")

      true ->
        items
        |> Enum.reduce_while({:ok, normalized, budget}, fn input, {:ok, normalized, budget} ->
          case normalize_item(input, page_count, depth, budget) do
            {:ok, item, budget} -> {:cont, {:ok, [item | normalized], budget}}
            {:error, _error} = outline_error -> {:halt, outline_error}
          end
        end)
        |> case do
          {:ok, normalized, budget} -> {:ok, Enum.reverse(normalized), budget}
          {:error, _error} = outline_error -> outline_error
        end
    end
  end

  defp normalize_item(input, page_count, depth, budget) do
    normalized_input =
      case input do
        {title, page} -> {:ok, %{title: title, page: page}}
        {title, page, children} -> {:ok, %{title: title, page: page, children: children}}
        input when is_map(input) -> {:ok, input}
        _ -> :error
      end

    with {:ok, input} <- normalized_input,
         true <- Enum.all?(Map.keys(input), &(&1 in @item_keys)),
         {:ok, title, budget} <- normalize_title(Map.get(input, :title), budget),
         {:ok, page} <- normalize_page(Map.get(input, :page), page_count),
         {:ok, view} <- normalize_view(Map.get(input, :view, :fit)),
         {:ok, open} <- normalize_open(Map.get(input, :open, true)),
         children when is_list(children) <- Map.get(input, :children, []),
         {:ok, children, budget} <- normalize_items(children, page_count, depth + 1, budget, []) do
      {:ok,
       %{
         title: title,
         page: page,
         view: view,
         open: normalized_open(open, children),
         children: children
       }, budget}
    else
      false ->
        error(:invalid_outlines, "outline item contains unsupported fields")

      :error ->
        error(:invalid_outlines, "outline item must be a map or supported tuple")

      {:error, _error} = outline_error ->
        outline_error

      children when not is_list(children) ->
        error(:invalid_outlines, "outline children must be a list")
    end
  end

  defp normalize_title(title, budget) do
    case is_binary(title) and String.valid?(title) and String.trim(title) != "" do
      true ->
        bytes = byte_size(title)
        item_count = budget.items + 1
        total_bytes = budget.title_bytes + bytes

        cond do
          item_count > Limits.get(:max_pdf_outline_items) ->
            limit_error("outline item count exceeds the limit")

          bytes > Limits.get(:max_pdf_outline_title_bytes) ->
            limit_error("outline title exceeds the byte limit")

          total_bytes > Limits.get(:max_pdf_outline_total_title_bytes) ->
            limit_error("aggregate outline title bytes exceed the limit")

          true ->
            {:ok, title, %{budget | items: item_count, title_bytes: total_bytes}}
        end

      false ->
        error(:invalid_outlines, "outline title must be a non-empty UTF-8 string")
    end
  end

  defp normalize_page(page, page_count) do
    case page do
      nil -> {:ok, nil}
      page when is_integer(page) and page >= 1 and page <= page_count -> {:ok, page}
      _ -> error(:invalid_outlines, "outline page must be nil or a page number within the PDF")
    end
  end

  defp normalize_open(open) do
    case is_boolean(open) do
      true -> {:ok, open}
      false -> error(:invalid_outlines, "outline open value must be boolean")
    end
  end

  defp normalize_view(view) do
    valid? =
      case view do
        view when view in [:fit, :fit_b] -> true
        {kind, value} when kind in [:fit_h, :fit_v, :fit_bh, :fit_bv] -> nullable_number?(value)
        {:fit_r, left, bottom, right, top} -> Enum.all?([left, bottom, right, top], &is_number/1)
        {:xyz, left, top, zoom} -> Enum.all?([left, top, zoom], &nullable_number?/1)
        _ -> false
      end

    case valid? do
      true -> {:ok, view}
      false -> error(:invalid_outlines, "outline destination view is unsupported or malformed")
    end
  end

  defp nullable_number?(value) do
    is_nil(value) or is_number(value)
  end

  defp validate_outline_root(root) do
    first = Map.get(root, "First")
    last = Map.get(root, "Last")

    cond do
      not is_nil(Map.get(root, "Type")) and Map.get(root, "Type") != {:name, "Outlines"} ->
        error(:invalid_pdf_input, "outline root Type is malformed")

      is_nil(first) and is_nil(last) ->
        :ok

      match?({:ref, _ref}, first) and match?({:ref, _ref}, last) ->
        :ok

      true ->
        error(:invalid_pdf_input, "outline root First and Last entries are inconsistent")
    end
  end

  defp read_siblings(
         document,
         current,
         parent_ref,
         previous_ref,
         expected_last,
         page_lookup,
         named_destinations,
         depth,
         budget,
         items
       ) do
    cond do
      depth > Limits.get(:max_pdf_outline_depth) ->
        limit_error("outline nesting depth exceeds the limit")

      is_nil(current) ->
        actual_last =
          case items do
            [] -> nil
            _ -> {:ref, previous_ref}
          end

        case actual_last == expected_last do
          true ->
            {:ok, Enum.reverse(items), budget}

          false ->
            error(:invalid_pdf_input, "outline Last entry does not match its sibling chain")
        end

      true ->
        {:ref, current_ref} = current

        cond do
          Map.has_key?(budget.seen, current_ref) ->
            error(:invalid_pdf_input, "outline hierarchy contains a cycle or repeated item")

          budget.items >= Limits.get(:max_pdf_outline_items) ->
            limit_error("outline item count exceeds the limit")

          true ->
            with {:ok, dictionary} <- PdfValidator.dictionary(document, current),
                 :ok <- validate_outline_links(dictionary, parent_ref, previous_ref),
                 {:ok, title, budget} <-
                   decode_title(document, Map.get(dictionary, "Title"), budget),
                 {:ok, page, view} <-
                   outline_destination(document, dictionary, page_lookup, named_destinations),
                 {:ok, first, last} <- child_bounds(dictionary),
                 budget = %{budget | seen: Map.put(budget.seen, current_ref, true)},
                 {:ok, children, budget} <-
                   read_siblings(
                     document,
                     first,
                     current_ref,
                     nil,
                     last,
                     page_lookup,
                     named_destinations,
                     depth + 1,
                     budget,
                     []
                   ),
                 {:ok, next} <- next_reference(Map.get(dictionary, "Next")) do
              item = %{
                title: title,
                page: page,
                view: view,
                open: normalized_open(outline_open?(dictionary), children),
                children: children
              }

              read_siblings(
                document,
                next,
                parent_ref,
                current_ref,
                expected_last,
                page_lookup,
                named_destinations,
                depth,
                budget,
                [item | items]
              )
            else
              {:error, _error} = outline_error -> outline_error
            end
        end
    end
  end

  defp validate_outline_links(dictionary, parent_ref, previous_ref) do
    expected_parent = {:ref, parent_ref}
    expected_previous = if is_nil(previous_ref), do: nil, else: {:ref, previous_ref}

    cond do
      Map.get(dictionary, "Parent") != expected_parent ->
        error(:invalid_pdf_input, "outline item Parent entry is inconsistent")

      Map.get(dictionary, "Prev") != expected_previous ->
        error(:invalid_pdf_input, "outline item Prev entry is inconsistent")

      not valid_outline_count?(Map.get(dictionary, "Count")) ->
        error(:invalid_pdf_input, "outline item Count entry is malformed")

      true ->
        :ok
    end
  end

  defp valid_outline_count?(count) do
    is_nil(count) or is_integer(count)
  end

  defp child_bounds(dictionary) do
    first = Map.get(dictionary, "First")
    last = Map.get(dictionary, "Last")

    case {first, last} do
      {nil, nil} -> {:ok, nil, nil}
      {{:ref, _first}, {:ref, _last}} -> {:ok, first, last}
      _ -> error(:invalid_pdf_input, "outline item First and Last entries are inconsistent")
    end
  end

  defp next_reference(value) do
    case value do
      nil -> {:ok, nil}
      {:ref, _ref} -> {:ok, value}
      _ -> error(:invalid_pdf_input, "outline item Next entry must be an indirect reference")
    end
  end

  defp decode_title(document, value, budget) do
    with {:ok, value} <- PdfValidator.resolve(document, value),
         {kind, bytes} when kind in [:string, :hex] and is_binary(bytes) <- value,
         {:ok, title} <- InfoCodec.decode_text(bytes) do
      normalize_title(title, budget)
    else
      _ -> error(:invalid_pdf_input, "outline item Title must be a valid PDF text string")
    end
  end

  @doc false
  @spec named_destinations(PdfValidator.document(), map()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def named_destinations(document, catalog) do
    with {:ok, legacy, count} <- legacy_destinations(document, Map.get(catalog, "Dests")),
         {:ok, modern} <- modern_destinations(document, Map.get(catalog, "Names"), count) do
      {:ok, Map.merge(legacy, modern)}
    end
  end

  @doc false
  @spec resolve_named_destination(
          PdfValidator.document(),
          {:name | :string | :hex, binary()},
          map()
        ) ::
          {:ok, [PdfValidator.value()] | nil}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def resolve_named_destination(document, named_destination, named_destinations) do
    {kind, name} = named_destination
    key = if kind == :name, do: {:name, name}, else: {:text, name}

    case Map.fetch(named_destinations, key) do
      {:ok, named_destination} ->
        destination =
          case PdfValidator.resolve(document, named_destination) do
            {:ok, %{"D" => destination}} -> destination
            {:ok, destination} -> destination
            {:error, _error} -> :invalid
          end

        case PdfValidator.resolve(document, destination) do
          {:ok, [{:ref, _page_ref}, {:name, _view_name} | _operands] = destination} ->
            {:ok, destination}

          _ ->
            error(:invalid_pdf_input, "named destination is malformed")
        end

      :error ->
        {:ok, nil}
    end
  end

  defp legacy_destinations(document, value) do
    case value do
      nil ->
        {:ok, %{}, 0}

      value ->
        case PdfValidator.dictionary(document, value) do
          {:ok, destinations} ->
            count = map_size(destinations)

            case count <= Limits.get(:max_pdf_named_destinations) do
              true ->
                {:ok,
                 Map.new(destinations, fn {name, destination} ->
                   {{:name, name}, destination}
                 end), count}

              false ->
                limit_error("named destination count exceeds the limit")
            end

          {:error, _error} ->
            error(:invalid_pdf_input, "catalog Dests entry must resolve to a dictionary")
        end
    end
  end

  defp modern_destinations(document, names_value, count) do
    case names_value do
      nil ->
        {:ok, %{}}

      names_value ->
        with {:ok, names} <- PdfValidator.dictionary(document, names_value),
             {:ok, destinations} <-
               read_name_tree(
                 document,
                 Map.get(names, "Dests"),
                 %{nodes: 0, entries: count, seen: %{}, destinations: %{}}
               ) do
          {:ok, destinations.destinations}
        else
          {:error, _error} = destination_error -> destination_error
        end
    end
  end

  defp read_name_tree(document, value, state) do
    case value do
      nil ->
        {:ok, state}

      value ->
        ref =
          case value do
            {:ref, ref} -> ref
            _ -> nil
          end

        cond do
          not is_nil(ref) and Map.has_key?(state.seen, ref) ->
            error(:invalid_pdf_input, "destination name tree contains a cycle")

          state.nodes >= Limits.get(:max_pdf_name_tree_nodes) ->
            limit_error("destination name tree node count exceeds the limit")

          true ->
            with {:ok, node} <- PdfValidator.dictionary(document, value),
                 {:ok, destinations, entries} <-
                   name_tree_entries(document, Map.get(node, "Names"), state.entries),
                 {:ok, kids} <- name_tree_kids(document, Map.get(node, "Kids")) do
              state = %{
                state
                | nodes: state.nodes + 1,
                  entries: entries,
                  seen: if(is_nil(ref), do: state.seen, else: Map.put(state.seen, ref, true)),
                  destinations: Map.merge(state.destinations, destinations)
              }

              Enum.reduce_while(kids, {:ok, state}, fn kid, {:ok, state} ->
                case read_name_tree(document, kid, state) do
                  {:ok, state} -> {:cont, {:ok, state}}
                  {:error, _error} = tree_error -> {:halt, tree_error}
                end
              end)
            else
              {:error, _error} = tree_error -> tree_error
            end
        end
    end
  end

  defp name_tree_entries(document, value, count) do
    case value do
      nil ->
        {:ok, %{}, count}

      value ->
        case PdfValidator.resolve(document, value) do
          {:ok, entries} when is_list(entries) ->
            entry_length = length(entries)

            cond do
              rem(entry_length, 2) != 0 ->
                error(
                  :invalid_pdf_input,
                  "destination name tree Names entry must resolve to pairs"
                )

              div(entry_length, 2) > Limits.get(:max_pdf_named_destinations) - count ->
                limit_error("named destination count exceeds the limit")

              true ->
                pair_count = div(entry_length, 2)

                entries
                |> Enum.chunk_every(2)
                |> Enum.reduce_while({:ok, %{}}, fn pair, {:ok, destinations} ->
                  case pair do
                    [{kind, name}, destination]
                    when kind in [:string, :hex] and is_binary(name) ->
                      {:cont, {:ok, Map.put(destinations, {:text, name}, destination)}}

                    _ ->
                      {:halt,
                       error(
                         :invalid_pdf_input,
                         "destination name tree Names array is malformed"
                       )}
                  end
                end)
                |> case do
                  {:ok, destinations} -> {:ok, destinations, count + pair_count}
                  {:error, _error} = destination_error -> destination_error
                end
            end

          _ ->
            error(:invalid_pdf_input, "destination name tree Names entry must resolve to pairs")
        end
    end
  end

  defp name_tree_kids(document, value) do
    case value do
      nil ->
        {:ok, []}

      value ->
        case PdfValidator.resolve(document, value) do
          {:ok, kids} when is_list(kids) ->
            case Enum.all?(kids, &match?({:ref, _ref}, &1)) do
              true ->
                {:ok, kids}

              false ->
                error(
                  :invalid_pdf_input,
                  "destination name tree Kids entry must resolve to references"
                )
            end

          _ ->
            error(
              :invalid_pdf_input,
              "destination name tree Kids entry must resolve to references"
            )
        end
    end
  end

  defp outline_destination(document, dictionary, page_lookup, named_destinations) do
    destination =
      case Map.get(dictionary, "Dest") do
        nil -> goto_destination(document, Map.get(dictionary, "A"))
        destination -> {:ok, destination}
      end

    case destination do
      {:ok, nil} ->
        {:ok, nil, :fit}

      {:ok, destination} ->
        decode_destination(document, destination, page_lookup, named_destinations)

      {:error, _error} = outline_error ->
        outline_error
    end
  end

  defp goto_destination(document, action) do
    case action do
      nil ->
        {:ok, nil}

      action ->
        case PdfValidator.resolve(document, action) do
          {:ok, %{"S" => {:name, "GoTo"}} = action} -> {:ok, Map.get(action, "D")}
          {:ok, action} when is_map(action) -> {:ok, nil}
          _ -> error(:invalid_pdf_input, "outline action must resolve to a dictionary")
        end
    end
  end

  defp decode_destination(document, destination, page_lookup, named_destinations) do
    case PdfValidator.resolve(document, destination) do
      {:ok, [{:ref, page_ref}, {:name, view_name} | operands]} ->
        decode_explicit_destination(page_ref, view_name, operands, page_lookup)

      {:ok, {kind, _name} = named_destination} when kind in [:name, :string, :hex] ->
        case resolve_named_destination(document, named_destination, named_destinations) do
          {:ok, [{:ref, page_ref}, {:name, view_name} | operands]} ->
            decode_explicit_destination(page_ref, view_name, operands, page_lookup)

          {:ok, nil} ->
            {:ok, nil, :fit}

          {:error, _error} = destination_error ->
            destination_error
        end

      _ ->
        error(:invalid_pdf_input, "outline destination is malformed")
    end
  end

  defp decode_explicit_destination(page_ref, view_name, operands, page_lookup) do
    case Map.fetch(page_lookup, page_ref) do
      {:ok, page} ->
        case decoded_view(view_name, operands) do
          {:ok, view} -> {:ok, page, view}
          :error -> error(:invalid_pdf_input, "outline destination view is malformed")
        end

      :error ->
        error(:invalid_pdf_input, "outline destination references an unknown page")
    end
  end

  defp decoded_view(name, operands) do
    view =
      case {name, operands} do
        {"Fit", []} ->
          :fit

        {"FitB", []} ->
          :fit_b

        {"FitH", [top]} ->
          {:fit_h, pdf_nullable_number(top)}

        {"FitV", [left]} ->
          {:fit_v, pdf_nullable_number(left)}

        {"FitBH", [top]} ->
          {:fit_bh, pdf_nullable_number(top)}

        {"FitBV", [left]} ->
          {:fit_bv, pdf_nullable_number(left)}

        {"FitR", [left, bottom, right, top]} ->
          {:fit_r, left, bottom, right, top}

        {"XYZ", [left, top, zoom]} ->
          {:xyz, pdf_nullable_number(left), pdf_nullable_number(top), pdf_nullable_number(zoom)}

        _ ->
          :error
      end

    case view do
      :error ->
        :error

      {:fit_r, left, bottom, right, top} = view ->
        if Enum.all?([left, bottom, right, top], &is_number/1), do: {:ok, view}, else: :error

      {kind, value} = view when kind in [:fit_h, :fit_v, :fit_bh, :fit_bv] ->
        if nullable_number?(value), do: {:ok, view}, else: :error

      {:xyz, left, top, zoom} = view ->
        if Enum.all?([left, top, zoom], &nullable_number?/1), do: {:ok, view}, else: :error

      view ->
        {:ok, view}
    end
  end

  defp pdf_nullable_number(value) do
    case value do
      nil -> nil
      value when is_number(value) -> value
      _ -> :invalid
    end
  end

  defp outline_open?(dictionary) do
    case Map.get(dictionary, "Count") do
      count when is_integer(count) and count < 0 -> false
      _ -> true
    end
  end

  defp normalized_open(open, children) do
    children == [] or open
  end

  defp remap_items(items, page_map, remapped) do
    items
    |> Enum.reduce(remapped, fn item, remapped ->
      children = remap_items(item.children, page_map, [])
      original_page = item.page
      page = if is_nil(original_page), do: nil, else: Map.get(page_map, original_page)

      case not is_nil(original_page) and is_nil(page) and children == [] do
        true -> remapped
        false -> [%{item | page: page, children: children} | remapped]
      end
    end)
    |> Enum.reverse()
  end

  defp error(reason, message) do
    Diagnostics.error(:outlines, reason, message, module: __MODULE__)
  end

  defp limit_error(message) do
    Diagnostics.error(:limits, :resource_limit_exceeded, message, module: __MODULE__)
  end
end
