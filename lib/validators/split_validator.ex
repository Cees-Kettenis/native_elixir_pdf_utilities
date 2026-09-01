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
         :ok <- validate_output_count(page_count),
         groups = Enum.map(page_numbers(page_count), &[&1]) do
      prepare_groups(context, groups)
    end
  end

  @doc false
  @spec prepare_ranges(PdfValidator.context(), term()) ::
          {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_ranges(context, ranges) do
    with {:ok, page_count} <- page_count(context),
         :ok <- validate_range_list(ranges) do
      prepare_range_groups(context, ranges, page_count)
    end
  end

  @doc false
  @spec prepare_after_page(PdfValidator.context(), term()) ::
          {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_after_page(context, page_number) do
    with {:ok, page_count} <- page_count(context),
         :ok <- validate_split_point(page_number, page_count),
         :ok <- validate_output_count(2),
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

  defp validate_range_list(ranges) do
    case ranges do
      [] ->
        error(:page_range, :invalid_page_range, "split ranges must be a non-empty list")

      ranges when is_list(ranges) ->
        ranges
        |> Enum.reduce_while(0, fn _range, output_count ->
          case output_count < Limits.get(:max_split_outputs) do
            true -> {:cont, output_count + 1}
            false -> {:halt, :limit_exceeded}
          end
        end)
        |> case do
          :limit_exceeded -> limit_error("split output count exceeds the limit")
          _output_count -> :ok
        end

      _ ->
        error(:page_range, :invalid_page_range, "split ranges must be a non-empty list")
    end
  end

  defp validate_range(range, page_count) do
    case range do
      %Range{} ->
        TransformValidator.expand_page_selection([range], page_count, false)

      _ ->
        error(:page_range, :invalid_page_range, "split ranges must be ascending Range values")
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
      case prepare_group(context, group, object_writes) do
        {:ok, input, object_writes} ->
          {:cont, {:ok, [input | prepared], object_writes}}

        {:error, _error} = preparation_error ->
          {:halt, preparation_error}
      end
    end)
    |> case do
      {:ok, prepared, _object_writes} -> {:ok, Enum.reverse(prepared)}
      {:error, _error} = preparation_error -> preparation_error
    end
  end

  defp prepare_range_groups(context, ranges, page_count) do
    ranges
    |> Enum.reduce_while({:ok, [], 0}, fn range, {:ok, prepared, object_writes} ->
      with {:ok, group} <- validate_range(range, page_count),
           {:ok, input, object_writes} <- prepare_group(context, group, object_writes) do
        {:cont, {:ok, [input | prepared], object_writes}}
      else
        {:error, _error} = preparation_error -> {:halt, preparation_error}
      end
    end)
    |> case do
      {:ok, prepared, _object_writes} -> {:ok, Enum.reverse(prepared)}
      {:error, _error} = preparation_error -> preparation_error
    end
  end

  defp prepare_group(context, group, object_writes) do
    case TransformValidator.prepare_output(context, group) do
      {:ok, input} ->
        object_writes = object_writes + length(input.objects) + 2

        case object_writes <= Limits.get(:max_split_object_writes) do
          true -> {:ok, input, object_writes}
          false -> limit_error("split object writes exceed the limit")
        end

      {:error, _error} = preparation_error ->
        preparation_error
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
