defmodule NativeElixirPdfUtilities.Validators.MergeValidator do
  @moduledoc """
  Merge-specific validation and rewrite preparation.

  The validator consumes a shared PDF validation context, materializes the
  effective page values needed by a flattened output page tree, proves that
  retained objects can be serialized, and constructs complete generation-aware
  reference-remapping tables before the writer runs.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Tokenizer
  alias NativeElixirPdfUtilities.Validators.OutlineValidator
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  @typedoc "A parsed object retained for merge serialization."
  @type object_context :: %{
          required(:obj) => non_neg_integer(),
          required(:gen) => non_neg_integer(),
          required(:tokens) => [Tokenizer.token()],
          required(:value) => PdfValidator.value(),
          optional(:value_override) => PdfValidator.value()
        }

  @typedoc "Prepared inherited page tokens keyed by original page reference."
  @type inherited_pages :: %{
          optional(PdfValidator.ref() | non_neg_integer()) => %{
            required(:resources) => [Tokenizer.token()] | nil,
            required(:mediabox) => [Tokenizer.token()],
            required(:cropbox) => [Tokenizer.token()] | nil,
            required(:rotate) => [Tokenizer.token()] | nil,
            required(:resources_value) => PdfValidator.value(),
            required(:mediabox_value) => [number()],
            required(:cropbox_value) => [number()] | nil,
            required(:rotate_value) => integer() | nil
          }
        }

  @typedoc "A validated input ready for reference allocation and PDF writing."
  @type input_context :: %{
          required(:objects) => [object_context()],
          required(:pages) => [PdfValidator.ref()],
          required(:inherited) => inherited_pages(),
          required(:outlines) => [OutlineValidator.item()],
          optional(:map) => %{optional(PdfValidator.ref()) => non_neg_integer()}
        }

  @doc """
  Validates the public merge input boundary.
  """
  @spec validate_inputs(term()) ::
          {:ok, [binary()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_inputs(inputs) do
    case inputs do
      [] ->
        Diagnostics.error(:merge, :empty_pdf_list, "merge/1 expects at least one PDF binary",
          operation: :merge,
          module: __MODULE__
        )

      inputs when is_list(inputs) ->
        inputs
        |> Enum.reduce_while({:ok, 0, 0}, fn input, {:ok, count, aggregate_bytes} ->
          cond do
            not is_binary(input) ->
              {:halt, :invalid}

            count >= Limits.get(:max_merge_inputs) ->
              {:halt, resource_limit_error("merge input count exceeds the limit")}

            aggregate_bytes + byte_size(input) > Limits.get(:max_aggregate_merge_input_bytes) ->
              {:halt, resource_limit_error("aggregate merge input bytes exceed the limit")}

            true ->
              {:cont, {:ok, count + 1, aggregate_bytes + byte_size(input)}}
          end
        end)
        |> case do
          {:ok, _count, _aggregate_bytes} ->
            {:ok, inputs}

          :invalid ->
            Diagnostics.error(
              :merge,
              :invalid_pdf_input,
              "merge/1 expects a list of PDF binaries",
              operation: :merge,
              module: __MODULE__
            )

          {:error, _} = limit_error ->
            limit_error
        end

      _ ->
        Diagnostics.error(:merge, :invalid_pdf_input, "merge/1 expects a list of PDF binaries",
          operation: :merge,
          module: __MODULE__
        )
    end
  end

  @doc """
  Prepares one shared validated PDF context for merge rewriting.
  """
  @spec prepare(PdfValidator.context()) ::
          {:ok, input_context()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare(pdf_context) do
    case pdf_context do
      %{document: document, pages: pages} ->
        objects =
          document.objects
          |> Enum.reject(fn {_ref, object} -> structural_reader_object?(object.value) end)
          |> Enum.map(fn {{object, generation}, parsed} ->
            %{obj: object, gen: generation, tokens: parsed.tokens, value: parsed.value}
          end)
          |> Enum.sort_by(&{&1.obj, &1.gen})

        object_by_ref = Map.new(objects, &{{&1.obj, &1.gen}, &1})

        with :ok <- validate_serializable_objects(objects),
             {:ok, objects} <-
               normalize_named_link_destinations(
                 document,
                 Map.get(pdf_context, :catalog, %{}),
                 objects
               ),
             {:ok, inherited} <- prepare_page_inheritances(document, pages, object_by_ref),
             {:ok, outlines} <- OutlineValidator.extract(pdf_context) do
          {:ok,
           %{
             objects: objects,
             pages: Enum.map(pages, & &1.ref),
             inherited: inherited,
             outlines: outlines
           }}
        end

      _ ->
        error(:merge_validation, "shared PDF validation context is malformed")
    end
  end

  defp normalize_named_link_destinations(document, catalog, objects) do
    case Enum.any?(objects, &named_link_destination?(document, &1.value)) do
      false ->
        {:ok, objects}

      true ->
        result =
          case OutlineValidator.named_destinations(document, catalog) do
            {:ok, named_destinations} ->
              Enum.reduce_while(objects, {:ok, []}, fn object, {:ok, normalized} ->
                case normalize_named_link_object(document, named_destinations, object) do
                  {:ok, object} -> {:cont, {:ok, [object | normalized]}}
                  {:error, _error} = destination_error -> {:halt, destination_error}
                end
              end)

            {:error, _error} = destination_error ->
              destination_error
          end

        case result do
          {:ok, normalized} ->
            {:ok, Enum.reverse(normalized)}

          {:error, {reason, diagnostic}} ->
            stage = if reason == :resource_limit_exceeded, do: :limits, else: :annotations

            {:error,
             {reason,
              diagnostic
              |> Map.put(:stage, stage)
              |> Map.put(:operation, :merge)
              |> Map.put(:module, __MODULE__)}}
        end
    end
  end

  defp named_link_destination?(document, value) do
    case value do
      %{"Subtype" => {:name, "Link"}} = dictionary ->
        case link_destination(document, dictionary) do
          {:ok, {_location, destination}} -> named_destination?(document, destination)
          :none -> false
        end

      _ ->
        false
    end
  end

  defp link_destination(document, dictionary) do
    case Map.fetch(dictionary, "Dest") do
      {:ok, destination} ->
        {:ok, {:destination, destination}}

      :error ->
        case Map.get(dictionary, "A") do
          nil ->
            :none

          action ->
            case PdfValidator.resolve(document, action) do
              {:ok, %{"S" => {:name, "GoTo"}} = action} ->
                {:ok, {{:action, action}, Map.get(action, "D")}}

              _ ->
                :none
            end
        end
    end
  end

  defp named_destination?(document, destination) do
    case PdfValidator.resolve(document, destination) do
      {:ok, {kind, value}} when kind in [:name, :string, :hex] and is_binary(value) -> true
      _ -> false
    end
  end

  defp normalize_named_link_object(document, named_destinations, object) do
    case object.value do
      %{"Subtype" => {:name, "Link"}} = dictionary ->
        with {:ok, dictionary} <-
               normalize_named_link_dictionary(document, named_destinations, dictionary) do
          case dictionary == object.value do
            true -> {:ok, object}
            false -> {:ok, Map.put(object, :value_override, dictionary)}
          end
        end

      _ ->
        {:ok, object}
    end
  end

  defp normalize_named_link_dictionary(document, named_destinations, dictionary) do
    case link_destination(document, dictionary) do
      {:ok, {location, destination}} ->
        case PdfValidator.resolve(document, destination) do
          {:ok, {kind, _name} = named_destination} when kind in [:name, :string, :hex] ->
            case OutlineValidator.resolve_named_destination(
                   document,
                   named_destination,
                   named_destinations
                 ) do
              {:ok, nil} ->
                {:ok, dictionary}

              {:ok, destination} ->
                case location do
                  :destination ->
                    {:ok, Map.put(dictionary, "Dest", destination)}

                  {:action, action} ->
                    {:ok, Map.put(dictionary, "A", Map.put(action, "D", destination))}
                end

              {:error, _error} = destination_error ->
                destination_error
            end

          _ ->
            {:ok, dictionary}
        end

      :none ->
        {:ok, dictionary}
    end
  end

  @doc """
  Allocates non-colliding output identifiers and validates complete reference remapping.
  """
  @spec prepare_remapping([input_context()], pos_integer()) ::
          {:ok, [input_context()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_remapping(inputs, start_id) do
    case is_list(inputs) and is_integer(start_id) and start_id > 0 do
      true ->
        with :ok <- validate_generated_entry_count(start_id) do
          inputs
          |> Enum.reduce_while({:ok, [], start_id, 0}, fn input,
                                                          {:ok, prepared, next_id, page_count} ->
            case input do
              %{objects: objects, pages: pages} when is_list(objects) and is_list(pages) ->
                page_count = page_count + length(pages)

                cond do
                  page_count > Limits.get(:max_merged_pages) ->
                    {:halt, resource_limit_error("merged page count exceeds the limit")}

                  true ->
                    case allocate_dense_ids(objects, next_id) do
                      {:ok, id_map, next_id} ->
                        {:cont,
                         {:ok, [Map.put(input, :map, id_map) | prepared], next_id, page_count}}

                      {:error, _} = limit_error ->
                        {:halt, limit_error}
                    end
                end

              _ ->
                {:halt, error(:reference_remapping, "merge remapping inputs are malformed")}
            end
          end)
          |> case do
            {:ok, prepared, next_id, _page_count} ->
              prepared = Enum.reverse(prepared)

              with :ok <- validate_outline_object_capacity(prepared, next_id) do
                validate_reference_remapping(prepared)
              end

            {:error, _} = remapping_error ->
              remapping_error
          end
        end

      false ->
        error(:reference_remapping, "merge remapping inputs are malformed")
    end
  end

  defp validate_generated_entry_count(start_id) do
    case start_id <= merged_output_entry_limit() do
      true -> :ok
      false -> resource_limit_error("merged object count exceeds the limit")
    end
  end

  defp allocate_dense_ids(objects, next_id) do
    output_entry_limit = merged_output_entry_limit()

    Enum.reduce_while(objects, {:ok, %{}, next_id}, fn object, {:ok, id_map, next_id} ->
      case object do
        %{obj: object, gen: generation}
        when is_integer(object) and object >= 0 and is_integer(generation) and generation >= 0 ->
          case next_id >= output_entry_limit do
            true ->
              {:halt, resource_limit_error("merged object count exceeds the limit")}

            false ->
              {:cont, {:ok, Map.put(id_map, {object, generation}, next_id), next_id + 1}}
          end

        _ ->
          {:halt, error(:reference_remapping, "merge remapping inputs are malformed")}
      end
    end)
  end

  defp merged_output_entry_limit do
    min(Limits.get(:max_merged_objects), Limits.get(:max_pdf_objects))
  end

  defp validate_outline_object_capacity(inputs, next_id) do
    outline_items =
      Enum.reduce(inputs, 0, fn input, count ->
        count + OutlineValidator.count_items(Map.get(input, :outlines, []))
      end)

    generated_objects = if outline_items == 0, do: 0, else: outline_items + 1

    case next_id + generated_objects <= merged_output_entry_limit() do
      true -> :ok
      false -> resource_limit_error("merged object count exceeds the limit")
    end
  end

  @doc """
  Takes one complete serialized PDF value from the beginning of a token list.

  Merge rewriting uses this prepared-token utility when replacing top-level
  dictionary entries without reparsing PDF bytes.
  """
  @spec take_value_tokens([Tokenizer.token()]) ::
          {:ok, [Tokenizer.token()], [Tokenizer.token()]} | :error
  def take_value_tokens(tokens) do
    case tokens do
      [:dict_start | rest] ->
        take_until_matching(rest, :dict_start, :dict_end, 1, [:dict_start])

      [:lbracket | rest] ->
        take_until_matching(rest, :lbracket, :rbracket, 1, [:lbracket])

      [{:int, object}, {:int, generation}, :R | rest] ->
        {:ok, [{:int, object}, {:int, generation}, :R], rest}

      [token | rest] ->
        {:ok, [token], rest}

      [] ->
        :error
    end
  end

  @doc """
  Splits a dictionary's top-level token list around a named value.

  Returns the tokens before the key, the complete value tokens, and the tokens
  following the value. Names nested inside dictionary or array values are not
  considered. Missing keys and incomplete values return `:error`.
  """
  @spec split_dictionary_value([Tokenizer.token()], binary()) ::
          {:ok, [Tokenizer.token()], [Tokenizer.token()], [Tokenizer.token()]} | :error
  def split_dictionary_value(tokens, name) do
    split_dictionary_value(tokens, name, [])
  end

  defp prepare_page_inheritances(document, pages, object_by_ref) do
    Enum.reduce_while(pages, {:ok, %{}}, fn page, {:ok, inheritances} ->
      page_number = elem(page.ref, 0)

      with {:ok, media_box} <-
             resolved_rectangle(document, page.media_box, "MediaBox", page_number),
           {:ok, crop_box} <- optional_rectangle(document, page.crop_box, "CropBox", page_number),
           {:ok, rotate} <- resolved_rotation(document, page.rotate, page_number),
           :ok <- validate_resources(document, page.resources, page_number),
           {:ok, resources} <- inherited_tokens(page, "Resources", object_by_ref),
           :ok <- validate_page_rewrite_target(object_by_ref, page.ref) do
        prepared = %{
          resources: resources,
          mediabox: number_tokens(media_box),
          cropbox: optional_number_tokens(crop_box),
          rotate: rotation_tokens(rotate),
          resources_value: page.resources,
          mediabox_value: media_box,
          cropbox_value: crop_box,
          rotate_value: rotate
        }

        inheritances =
          inheritances
          |> Map.put(page.ref, prepared)
          |> Map.put(page_number, prepared)

        {:cont, {:ok, inheritances}}
      else
        {:error, _} = page_error -> {:halt, page_error}
      end
    end)
  end

  defp resolved_rectangle(document, value, name, page) do
    case value do
      nil ->
        Diagnostics.error(
          :page_tree,
          :invalid_pdf_input,
          "page #{page} is missing an effective #{name}",
          source: "page #{page}"
        )

      value ->
        case PdfValidator.number_array(document, value, 4) do
          {:ok, numbers} ->
            {:ok, numbers}

          _ ->
            Diagnostics.error(
              :page_tree,
              :invalid_pdf_input,
              "page #{page} has a malformed effective #{name}",
              source: "page #{page}"
            )
        end
    end
  end

  defp optional_rectangle(document, value, name, page) do
    case value do
      nil -> {:ok, nil}
      value -> resolved_rectangle(document, value, name, page)
    end
  end

  defp resolved_rotation(document, value, page) do
    case value do
      nil ->
        {:ok, nil}

      value ->
        case PdfValidator.resolve(document, value) do
          {:ok, rotation} when is_integer(rotation) and rem(rotation, 90) == 0 ->
            {:ok, rotation}

          _ ->
            Diagnostics.error(
              :page_tree,
              :invalid_pdf_input,
              "page #{page} has a malformed effective Rotate",
              source: "page #{page}"
            )
        end
    end
  end

  defp validate_resources(document, value, page) do
    case PdfValidator.resolve(document, value) do
      {:ok, resources} when is_map(resources) or is_nil(resources) ->
        :ok

      _ ->
        Diagnostics.error(
          :page_tree,
          :invalid_pdf_input,
          "page #{page} has malformed effective Resources",
          source: "page #{page}"
        )
    end
  end

  defp inherited_tokens(page, key, object_by_ref) do
    case Map.get(page.inherited, key) do
      nil ->
        {:ok, nil}

      %{source_ref: source_ref} ->
        with {:ok, object} <- fetch_object(object_by_ref, source_ref),
             {:ok, tokens} <- find_value_after_name(object.tokens, key) do
          {:ok, tokens}
        else
          _ -> error(:serialization, "inherited page value #{key} has no serializable tokens")
        end
    end
  end

  defp fetch_object(object_by_ref, ref) do
    case Map.fetch(object_by_ref, ref) do
      {:ok, object} -> {:ok, object}
      :error -> error(:serialization, "validated page object is unavailable for rewriting")
    end
  end

  defp validate_page_rewrite_target(object_by_ref, ref) do
    with {:ok, object} <- fetch_object(object_by_ref, ref) do
      case object do
        %{value: %{"Type" => {:name, "Page"}}, tokens: [:dict_start | _rest]} ->
          :ok

        _ ->
          error(:serialization, "validated page object is not a dictionary ready for rewriting")
      end
    end
  end

  defp find_value_after_name(tokens, name) do
    case take_value_tokens(tokens) do
      {:ok, [:dict_start | dictionary_tokens], _remaining} ->
        dictionary_tokens
        |> Enum.drop(-1)
        |> split_dictionary_value(name)
        |> case do
          {:ok, _left, value, _right} -> {:ok, value}
          :error -> :error
        end

      _ ->
        :error
    end
  end

  defp split_dictionary_value(tokens, name, preceding_tokens) do
    case tokens do
      [{:name, key} = key_token | rest] ->
        case take_value_tokens(rest) do
          {:ok, value, remaining} when key == name ->
            {:ok, Enum.reverse(preceding_tokens), value, remaining}

          {:ok, value, remaining} ->
            preceding_tokens = Enum.reverse([key_token | value], preceding_tokens)
            split_dictionary_value(remaining, name, preceding_tokens)

          :error ->
            :error
        end

      _ ->
        :error
    end
  end

  defp take_until_matching(tokens, opening, closing, depth, acc) do
    case tokens do
      [] ->
        :error

      [token | rest] ->
        cond do
          token == opening ->
            take_until_matching(rest, opening, closing, depth + 1, [token | acc])

          token == closing and depth == 1 ->
            {:ok, Enum.reverse([token | acc]), rest}

          token == closing ->
            take_until_matching(rest, opening, closing, depth - 1, [token | acc])

          true ->
            take_until_matching(rest, opening, closing, depth, [token | acc])
        end
    end
  end

  defp validate_serializable_objects(objects) do
    Enum.reduce_while(objects, :ok, fn object, :ok ->
      case serializable_tokens?(object.tokens) do
        true -> {:cont, :ok}
        false -> {:halt, error(:serialization, "PDF object tokens cannot be serialized")}
      end
    end)
  end

  defp serializable_tokens?(tokens) do
    Enum.all?(tokens, fn token ->
      case token do
        {:name, _name} ->
          true

        {:stream_data, data} ->
          is_binary(data)

        {:string, data} ->
          is_binary(data)

        {:hex_string, data} ->
          is_binary(data)

        {:int, value} ->
          is_integer(value)

        {:real, value} ->
          is_float(value)

        token when token in [:dict_start, :dict_end, :lbracket, :rbracket, :stream, :endstream] ->
          true

        token when token in [true, false, :null, :R] ->
          true

        _ ->
          false
      end
    end)
  end

  defp validate_reference_remapping(inputs) do
    Enum.reduce_while(inputs, {:ok, inputs}, fn input, {:ok, inputs} ->
      missing_reference =
        Enum.find_value(input.objects, fn object ->
          object
          |> object_references()
          |> Enum.find(fn ref -> not Map.has_key?(input.map, ref) end)
        end)

      case missing_reference do
        nil ->
          {:cont, {:ok, inputs}}

        {object, generation} ->
          {:halt,
           error(
             :reference_remapping,
             "indirect reference #{object} #{generation} has no retained output object"
           )}
      end
    end)
  end

  defp object_references(object) do
    case Map.fetch(object, :value_override) do
      {:ok, value} -> value_references(value)
      :error -> token_references(object.tokens)
    end
  end

  defp value_references(value) do
    case value do
      {:ref, ref} ->
        [ref]

      values when is_list(values) ->
        Enum.flat_map(values, &value_references/1)

      dictionary when is_map(dictionary) ->
        dictionary |> Map.values() |> Enum.flat_map(&value_references/1)

      _ ->
        []
    end
  end

  defp token_references(tokens) do
    {references, _remaining} =
      Enum.reduce(tokens, {[], []}, fn token, {references, pending} ->
        pending = Enum.take([token | pending], 3)

        case pending do
          [:R, {:int, generation}, {:int, object}] ->
            {[{object, generation} | references], []}

          _ ->
            {references, pending}
        end
      end)

    references
  end

  defp number_tokens(numbers) do
    [:lbracket | Enum.map(numbers, &number_token/1)] ++ [:rbracket]
  end

  defp optional_number_tokens(numbers) do
    case numbers do
      nil -> nil
      numbers -> number_tokens(numbers)
    end
  end

  defp number_token(number) do
    case number do
      number when is_integer(number) -> {:int, number}
      number -> {:real, number}
    end
  end

  defp rotation_tokens(rotation) do
    case rotation do
      nil -> nil
      rotation -> [{:int, rotation}]
    end
  end

  defp structural_reader_object?(value) do
    case value do
      %{"Type" => {:name, type}} when type in ["XRef", "ObjStm"] -> true
      _ -> false
    end
  end

  defp error(stage, message) do
    Diagnostics.error(stage, :invalid_pdf_input, message,
      operation: :merge,
      module: __MODULE__
    )
  end

  defp resource_limit_error(message) do
    Diagnostics.error(:limits, :resource_limit_exceeded, message,
      operation: :merge,
      module: __MODULE__
    )
  end
end
