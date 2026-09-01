defmodule NativeElixirPdfUtilities.Merge do
  @moduledoc """
  PDF utilities for merging documents through the shared native reader.

  The merger rebuilds its output with a new PDF 1.7 catalog, flat page tree,
  cross-reference table, and trailer. It resolves modern input structures,
  rewrites active indirect references, materializes inherited page values, and
  preserves stream bytes.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.AssemblyWriter
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.MergeValidator

  @type pdf_bin :: binary()
  @type error_reason :: :empty_pdf_list | Reader.error_reason()

  @doc """
  Merges a non-empty list of PDF binaries into one rebuilt PDF.

  Input page order determines output page order. Catalog-level structures such
  as outlines, named destinations, and forms are not preserved.
  """
  @spec merge([pdf_bin()]) ::
          {:ok, pdf_bin()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def merge(inputs) do
    with {:ok, inputs} <- MergeValidator.validate_inputs(inputs),
         {:ok, prepared} <- prepare_inputs(inputs),
         {:ok, remapped} <- MergeValidator.prepare_remapping(prepared, 3) do
      AssemblyWriter.write(remapped)
    else
      {:reader_error, {reason, diagnostic}} ->
        {:error,
         {reason,
          diagnostic
          |> Map.put(:operation, :merge)
          |> Map.put(:module, __MODULE__)}}

      {:error, {reason, diagnostic}} ->
        {:error,
         {reason,
          diagnostic
          |> Map.put(:operation, :merge)
          |> Map.put(:module, __MODULE__)}}

      {:preparation_error, {reason, diagnostic}} ->
        Diagnostics.error(
          :merge,
          :invalid_pdf_input,
          "merge/1 received an invalid PDF (#{reason} at #{diagnostic.stage}): #{diagnostic.message}",
          operation: :merge,
          module: __MODULE__,
          source: Map.get(diagnostic, :source)
        )
    end
  end

  defp prepare_inputs(inputs) do
    inputs
    |> Enum.reduce_while({:ok, []}, fn input, {:ok, prepared} ->
      with {:ok, context} <- reader_context(input),
           {:ok, assembly_input} <- MergeValidator.prepare(context) do
        {:cont, {:ok, [assembly_input | prepared]}}
      else
        {:reader_error, _error} = reader_error -> {:halt, reader_error}
        {:error, error} -> {:halt, {:preparation_error, error}}
      end
    end)
    |> case do
      {:ok, prepared} -> {:ok, Enum.reverse(prepared)}
      error -> error
    end
  end

  defp reader_context(input) do
    case Reader.read_validated(input) do
      {:ok, context} -> {:ok, context}
      {:error, error} -> {:reader_error, error}
    end
  end
end
