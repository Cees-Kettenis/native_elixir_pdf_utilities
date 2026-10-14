defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgDocumentVectorBrowserParityTest do
  use ExUnit.Case, async: false
  @moduletag :browser_parity
  @moduletag :svg_document_vector
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  for name <- [
        "svg_marker_document_contexts.html",
        "html_svg_document_viewports.html",
        "html_svg_document_intrinsic_standalone.html",
        "html_svg_document_intrinsic.html"
      ] do
    @fixture name
    test "unchanged selected SVG document #{@fixture}" do
      PdfVisualCompare.assert_browser_match!(
        Path.expand("test/fixtures/html_to_pdf/browser_parity/" <> @fixture),
        render_opts: [fonts: []],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )
    end
  end

  test "selected document glyphs use actual byte fonts in converted artwork" do
    path =
      Path.expand("test/fixtures/html_to_pdf/browser_parity/svg_document_selected_glyphs.html")

    PdfVisualCompare.with_embedded_font_configuration(path, fn ->
      stats =
        PdfVisualCompare.assert_browser_match!(path,
          native_renderer: fn html, opts ->
            PdfVisualCompare.render_embedded_font_bytes(html, opts, path)
          end,
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count == 1
    end)
  end
end
