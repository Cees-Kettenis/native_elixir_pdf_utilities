defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgDocumentVectorBrowserParityTest do
  use ExUnit.Case, async: false
  @moduletag :browser_parity
  @moduletag :svg_document_vector
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  test "selected document glyphs use actual byte fonts in converted artwork" do
    path =
      Path.expand("test/fixtures/html_to_pdf/browser_parity/svg_document_selected_glyphs.html")

    PdfVisualCompare.with_embedded_font_configuration(path, fn ->
      stats =
        PdfVisualCompare.assert_browser_match!(path,
          native_renderer: fn html, opts ->
            PdfVisualCompare.render_embedded_font_bytes(html, opts, path)
          end,
          svg_reference: :png,
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count == 1
    end)
  end
end
