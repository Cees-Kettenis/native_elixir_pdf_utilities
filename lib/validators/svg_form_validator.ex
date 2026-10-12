defmodule NativeElixirPdfUtilities.Validators.SvgFormValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.Reader

  @doc false
  @spec prepare_source(term()) :: {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_source(source) do
    with {:ok, document} <- Reader.read(source),
         [page] <- document.pages,
         %{value: %{"Contents" => {:ref, content_ref}} = page_value} <-
           Map.get(document.objects, page.ref),
         %{value: content_dictionary, stream: stream} <-
           Map.get(document.objects, content_ref),
         [0, 0, width, height] <- page.media_box,
         true <- is_map(page.resources) and is_map(content_dictionary) and is_binary(stream),
         true <- is_number(width) and is_number(height) and width > 0 and height > 0 do
      {:ok,
       %{
         width: width,
         height: height,
         stream: stream,
         content_dictionary: content_dictionary,
         resources: page.resources,
         group: Map.get(page_value, "Group"),
         objects: document.objects
       }}
    else
      {:error, {_reason, _diagnostic}} = error ->
        error

      _ ->
        Diagnostics.error(
          :svg,
          :invalid_pdf_input,
          "SVG vector conversion must produce one page with a bounded content stream and resources",
          operation: :vectorize,
          module: __MODULE__,
          source: "SVG vector PDF"
        )
    end
  end
end
