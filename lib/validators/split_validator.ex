defmodule NativeElixirPdfUtilities.Validators.SplitValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.PdfValidator
  alias NativeElixirPdfUtilities.Validators.TransformValidator

  @doc false
  @spec prepare_each_page(PdfValidator.context()) ::
          {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_each_page(context) do
    with {:ok, page_count} <- page_count(context),
         groups = Enum.map(page_numbers(page_count), &[&1]),
         :ok <- validate_output_count(length(groups)) do
      prepare_groups(context, groups)
    end
  end

  @doc false
  @spec prepare_ranges(PdfValidator.context(), term()) ::
          {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_ranges(context, ranges) do
    with {:ok, page_count} <- page_count(context),
         {:ok, groups} <- validate_ranges(ranges, page_count),
         :ok <- validate_output_count(length(groups)) do
      prepare_groups(context, groups)
    end
  end

  @doc false
  @spec prepare_after_page(PdfValidator.context(), term()) ::
          {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_after_page(context, page_number) do
    with {:ok, page_count} <- page_count(context),
         :ok <- validate_split_point(page_number, page_count),
         groups = [Enum.to_list(1..page_number), Enum.to_list((page_number + 1)..page_count)] do
      prepare_groups(context, groups)
    end
  end

  @doc false
  @spec validate_aggregate_output_bytes([binary()]) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_aggregate_output_bytes(outputs) do
    bytes = Enum.reduce(outputs, 0, &(byte_size(&1) + &2))

    case bytes <= Limits.get(:max_aggregate_split_output_bytes) do
      true -> :ok
      false -> limit_error("aggregate split output bytes exceed the limit")
    end
  end

  defp page_count(context) do
    case context do
      %{pages: pages} when is_list(pages) -> {:ok, length(pages)}
      _ -> error(:validation, :invalid_pdf_input, "shared PDF validation context is malformed")
    end
  end

  defp page_numbers(page_count) do
    case page_count do
      0 -> []
      page_count -> Enum.to_list(1..page_count)
    end
  end

  defp validate_ranges(ranges, page_count) do
    case is_list(ranges) and ranges != [] do
      true ->
        ranges
        |> Enum.reduce_while({:ok, []}, fn range, {:ok, groups} ->
          case range do
            %Range{} ->
              case TransformValidator.expand_page_selection([range], page_count, false) do
                {:ok, page_numbers} -> {:cont, {:ok, [page_numbers | groups]}}
                {:error, _error} = range_error -> {:halt, range_error}
              end

            _ ->
              {:halt,
               error(
                 :page_range,
                 :invalid_page_range,
                 "split ranges must be ascending Range values"
               )}
          end
        end)
        |> case do
          {:ok, groups} -> {:ok, Enum.reverse(groups)}
          {:error, _error} = range_error -> range_error
        end

      false ->
        error(:page_range, :invalid_page_range, "split ranges must be a non-empty list")
    end
  end

  defp validate_split_point(page_number, page_count) do
    case is_integer(page_number) and page_number >= 1 and page_number < page_count do
      true ->
        :ok

      false ->
        error(
          :page_selection,
          :invalid_page_selection,
          "split point must be between page 1 and page #{max(page_count - 1, 0)}"
        )
    end
  end

  defp validate_output_count(output_count) do
    case output_count <= Limits.get(:max_split_outputs) do
      true -> :ok
      false -> limit_error("split output count exceeds the limit")
    end
  end

  defp prepare_groups(context, groups) do
    groups
    |> Enum.reduce_while({:ok, [], 0}, fn group, {:ok, prepared, object_writes} ->
      case TransformValidator.prepare_output(context, group) do
        {:ok, input} ->
          object_writes = object_writes + length(input.objects) + 2

          case object_writes <= Limits.get(:max_split_object_writes) do
            true -> {:cont, {:ok, [input | prepared], object_writes}}
            false -> {:halt, limit_error("split object writes exceed the limit")}
          end

        {:error, _error} = preparation_error ->
          {:halt, preparation_error}
      end
    end)
    |> case do
      {:ok, prepared, _object_writes} -> {:ok, Enum.reverse(prepared)}
      {:error, _error} = preparation_error -> preparation_error
    end
  end

  defp limit_error(message) do
    Diagnostics.error(:limits, :resource_limit_exceeded, message,
      operation: :split,
      module: __MODULE__
    )
  end

  defp error(stage, reason, message) do
    Diagnostics.error(stage, reason, message, operation: :split, module: __MODULE__)
  end
end
