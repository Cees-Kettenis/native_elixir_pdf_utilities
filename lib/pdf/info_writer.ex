defmodule NativeElixirPdfUtilities.Pdf.InfoWriter do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.IncrementalWriter
  alias NativeElixirPdfUtilities.Validators.InfoValidator
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  @doc false
  @spec write(PdfValidator.context(), map()) ::
          {:ok, binary()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def write(context, dictionary) do
    case context do
      %{document: %{binary: pdf, trailer: trailer, xref: xref, xref_offset: previous}}
      when is_binary(pdf) and is_map(trailer) and is_map(xref) and is_integer(previous) and
             is_map(dictionary) ->
        with :ok <- InfoValidator.validate_incremental_object_capacity(trailer, xref) do
          object = Map.fetch!(trailer, "Size")
          context = put_in(context.document.trailer["Info"], {:ref, {object, 0}})
          IncrementalWriter.write(context, [{object, 0, {:value, dictionary}}])
        end

      _ ->
        Diagnostics.error(
          :incremental_write,
          :invalid_pdf_input,
          "prepared information write context is malformed"
        )
    end
  end
end
