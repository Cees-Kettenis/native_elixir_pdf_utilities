defmodule NativeElixirPdfUtilities.Validators.TransformValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.AssemblyValidator
  alias NativeElixirPdfUtilities.Validators.MergeValidator
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  @type page_selector :: pos_integer() | Range.t()

  @doc false
  @spec prepare_pick(PdfValidator.context(), term()) ::
          {:ok, MergeValidator.input_context()}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_pick(context, selection) do
    with {:ok, page_count} <- page_count(context),
         {:ok, page_numbers} <- expand_page_selection(selection, page_count, false) do
      prepare_output(context, page_numbers)
    end
  end

  @doc false
  @spec prepare_delete(PdfValidator.context(), term()) ::
          {:ok, MergeValidator.input_context()}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_delete(context, selection) do
    with {:ok, page_count} <- page_count(context),
         {:ok, deleted} <- expand_page_selection(selection, page_count, true),
         deleted = MapSet.new(deleted),
         remaining = Enum.reject(page_numbers(page_count), &MapSet.member?(deleted, &1)),
         :ok <- validate_nonempty_output(remaining) do
      prepare_output(context, remaining)
    end
  end

  @doc false
  @spec prepare_rotation(PdfValidator.context(), term(), term()) ::
          {:ok, MergeValidator.input_context()}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_rotation(context, degrees, options) do
    with {:ok, rotation} <- validate_rotation(degrees),
         {:ok, selection} <- rotation_selection(options),
         {:ok, page_count} <- page_count(context),
         {:ok, page_numbers} <- expand_rotation_selection(selection, page_count),
         rotations = Map.new(page_numbers, &{&1, rotation}) do
      prepare_output(context, page_numbers(page_count), rotations)
    end
  end

  @doc false
  @spec prepare_output(PdfValidator.context(), [pos_integer()], map()) ::
          {:ok, MergeValidator.input_context()}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_output(context, page_numbers, rotations \\ %{}) do
    with {:ok, input} <- AssemblyValidator.prepare(context, page_numbers, rotations),
         {:ok, [input]} <- MergeValidator.prepare_remapping([input], 3) do
      {:ok, input}
    end
  end

  @doc false
  @spec expand_page_selection(term(), non_neg_integer(), boolean()) ::
          {:ok, [pos_integer()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def expand_page_selection(selection, page_count, allow_empty) do
    case is_list(selection) and is_integer(page_count) and page_count >= 0 do
      true ->
        with {:ok, page_numbers} <- expand_selectors(selection, page_count),
             :ok <- validate_selection_presence(page_numbers, allow_empty) do
          {:ok, page_numbers}
        end

      false ->
        selection_error("page selection must be a list of page numbers or ascending ranges")
    end
  end

  defp page_count(context) do
    case context do
      %{pages: pages} when is_list(pages) -> {:ok, length(pages)}
      _ -> error(:validation, :invalid_pdf_input, "shared PDF validation context is malformed")
    end
  end

  defp expand_selectors(selectors, page_count) do
    selectors
    |> Enum.reduce_while({:ok, [], MapSet.new()}, fn selector, {:ok, expanded, seen} ->
      case expand_selector(selector, page_count) do
        {:ok, page_numbers} ->
          case append_unique_pages(page_numbers, expanded, seen) do
            {:ok, expanded, seen} -> {:cont, {:ok, expanded, seen}}
            {:error, _error} = selection_error -> {:halt, selection_error}
          end

        {:error, _error} = selection_error ->
          {:halt, selection_error}
      end
    end)
    |> case do
      {:ok, expanded, _seen} -> {:ok, Enum.reverse(expanded)}
      {:error, _error} = selection_error -> selection_error
    end
  end

  defp expand_selector(selector, page_count) do
    case selector do
      page when is_integer(page) and page > 0 ->
        case page <= page_count do
          true -> {:ok, [page]}
          false -> page_out_of_bounds(page, page_count)
        end

      %Range{first: first, last: last, step: 1}
      when is_integer(first) and is_integer(last) and first > 0 and last >= first ->
        case last <= page_count do
          true -> {:ok, Enum.to_list(first..last)}
          false -> page_out_of_bounds(max(first, page_count + 1), page_count)
        end

      _ ->
        selection_error("page selection contains an invalid page number or range")
    end
  end

  defp append_unique_pages(page_numbers, expanded, seen) do
    Enum.reduce_while(page_numbers, {:ok, expanded, seen}, fn page, {:ok, expanded, seen} ->
      case MapSet.member?(seen, page) do
        true ->
          {:halt, selection_error("page selection must not contain duplicate pages")}

        false ->
          {:cont, {:ok, [page | expanded], MapSet.put(seen, page)}}
      end
    end)
  end

  defp validate_selection_presence(page_numbers, allow_empty) do
    case allow_empty or page_numbers != [] do
      true -> :ok
      false -> selection_error("page selection must contain at least one page")
    end
  end

  defp page_out_of_bounds(page, page_count) do
    error(
      :page_selection,
      :page_out_of_bounds,
      "page #{page} is outside the document's 1..#{page_count} page range"
    )
  end

  defp validate_nonempty_output(remaining) do
    case remaining do
      [] -> selection_error("page deletion must leave at least one page")
      _ -> :ok
    end
  end

  defp validate_rotation(degrees) do
    case is_integer(degrees) and rem(degrees, 90) == 0 do
      true ->
        {:ok, Integer.mod(degrees, 360)}

      false ->
        error(:rotation, :invalid_rotation, "rotation must be an integer multiple of 90 degrees")
    end
  end

  defp rotation_selection(options) do
    case options do
      options when is_list(options) ->
        case Keyword.keyword?(options) and
               length(options) == length(Keyword.keys(options) |> Enum.uniq()) and
               Enum.all?(Keyword.keys(options), &(&1 == :pages)) do
          true -> {:ok, Keyword.get(options, :pages, :all)}
          false -> error(:options, :invalid_options, "rotation options only accept :pages")
        end

      _ ->
        error(:options, :invalid_options, "rotation options must be a keyword list")
    end
  end

  defp expand_rotation_selection(selection, page_count) do
    case selection do
      :all ->
        case page_count do
          0 -> selection_error("rotation requires at least one page")
          page_count -> {:ok, page_numbers(page_count)}
        end

      selection ->
        expand_page_selection(selection, page_count, false)
    end
  end

  defp page_numbers(page_count) do
    case page_count do
      0 -> []
      page_count -> Enum.to_list(1..page_count)
    end
  end

  defp selection_error(message) do
    error(:page_selection, :invalid_page_selection, message)
  end

  defp error(stage, reason, message) do
    Diagnostics.error(stage, reason, message, operation: :transform, module: __MODULE__)
  end
end
