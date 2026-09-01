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
         remaining = Enum.reject(page_numbers(page_count), &(&1 in deleted)),
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
        with {:ok, page_numbers} <- expand_selectors(selection),
             :ok <- validate_selection_presence(page_numbers, allow_empty),
             :ok <- validate_unique_pages(page_numbers),
             :ok <- validate_page_bounds(page_numbers, page_count) do
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

  defp expand_selectors(selectors) do
    selectors
    |> Enum.reduce_while({:ok, []}, fn selector, {:ok, expanded} ->
      case expand_selector(selector) do
        {:ok, page_numbers} -> {:cont, {:ok, [page_numbers | expanded]}}
        {:error, _error} = selection_error -> {:halt, selection_error}
      end
    end)
    |> case do
      {:ok, expanded} -> {:ok, expanded |> Enum.reverse() |> List.flatten()}
      {:error, _error} = selection_error -> selection_error
    end
  end

  defp expand_selector(selector) do
    case selector do
      page when is_integer(page) and page > 0 ->
        {:ok, [page]}

      %Range{first: first, last: last, step: 1}
      when is_integer(first) and is_integer(last) and first > 0 and last >= first ->
        {:ok, Enum.to_list(first..last)}

      _ ->
        selection_error("page selection contains an invalid page number or range")
    end
  end

  defp validate_selection_presence(page_numbers, allow_empty) do
    case allow_empty or page_numbers != [] do
      true -> :ok
      false -> selection_error("page selection must contain at least one page")
    end
  end

  defp validate_unique_pages(page_numbers) do
    case length(page_numbers) == MapSet.size(MapSet.new(page_numbers)) do
      true -> :ok
      false -> selection_error("page selection must not contain duplicate pages")
    end
  end

  defp validate_page_bounds(page_numbers, page_count) do
    case Enum.find(page_numbers, &(&1 > page_count)) do
      nil ->
        :ok

      page ->
        error(
          :page_selection,
          :page_out_of_bounds,
          "page #{page} is outside the document's 1..#{page_count} page range"
        )
    end
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
