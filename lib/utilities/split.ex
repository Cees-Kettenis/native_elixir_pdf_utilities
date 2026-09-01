defmodule NativeElixirPdfUtilities.Split do
  @moduledoc """
  Rebuilds one PDF as multiple valid PDF documents.

  Page numbers are one-based and follow the source document's page-tree order.
  Ranges are inclusive and must be ascending. Every output receives a new
  catalog, page tree, cross-reference table, and trailer.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.AssemblyWriter
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.SplitValidator

  @type error_reason ::
          :encrypted_pdf
          | :invalid_page_range
          | :invalid_page_selection
          | :invalid_pdf_input
          | :page_out_of_bounds
          | :resource_limit_exceeded
          | :unsupported_pdf_feature

  @doc """
  Rebuilds every source page as an individual PDF.

  A document with no pages returns an empty output list.
  """
  @spec by_page(binary()) ::
          {:ok, [binary()]} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def by_page(pdf) do
    with {:ok, context} <- Reader.read_validated(pdf),
         {:ok, inputs} <- SplitValidator.prepare_each_page(context),
         {:ok, outputs} <- write_outputs(inputs) do
      {:ok, outputs}
    else
      {:error, error} -> owned_error(error, :split_by_page)
    end
  end

  @doc """
  Rebuilds each inclusive source-page range as a separate PDF.

  Ranges may overlap because each range describes an independent output.
  """
  @spec by_ranges(binary(), [Range.t()]) ::
          {:ok, [binary()]} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def by_ranges(pdf, ranges) do
    with {:ok, context} <- Reader.read_validated(pdf),
         {:ok, inputs} <- SplitValidator.prepare_ranges(context, ranges),
         {:ok, outputs} <- write_outputs(inputs) do
      {:ok, outputs}
    else
      {:error, error} -> owned_error(error, :split_by_ranges)
    end
  end

  @doc """
  Rebuilds a PDF as two non-empty PDFs split after `page_number`.

  The selected page is the final page in the first output. The split point must
  be between page one and the penultimate page.
  """
  @spec after_page(binary(), pos_integer()) ::
          {:ok, {binary(), binary()}}
          | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def after_page(pdf, page_number) do
    with {:ok, context} <- Reader.read_validated(pdf),
         {:ok, inputs} <- SplitValidator.prepare_after_page(context, page_number),
         {:ok, [before_pdf, after_pdf]} <- write_outputs(inputs) do
      {:ok, {before_pdf, after_pdf}}
    else
      {:error, error} -> owned_error(error, :split_after_page)
    end
  end

  defp write_outputs(inputs) do
    inputs
    |> Enum.reduce_while({:ok, []}, fn input, {:ok, outputs} ->
      case AssemblyWriter.write([input]) do
        {:ok, output} ->
          case SplitValidator.validate_aggregate_output_bytes([output | outputs]) do
            :ok -> {:cont, {:ok, [output | outputs]}}
            {:error, _error} = limit_error -> {:halt, limit_error}
          end
      end
    end)
    |> case do
      {:ok, outputs} -> {:ok, Enum.reverse(outputs)}
      {:error, _error} = limit_error -> limit_error
    end
  end

  defp owned_error({reason, diagnostic}, operation) do
    {:error,
     {reason,
      diagnostic
      |> Map.put(:operation, operation)
      |> Map.put(:module, __MODULE__)}}
  end
end
