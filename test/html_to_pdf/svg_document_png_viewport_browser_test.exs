defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgDocumentPngViewportBrowserTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :svg_document_png_viewport

  test "the document converter keeps the actual styled font-relative SVG root viewport" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        "test/fixtures/html_to_pdf/browser_parity/svg_document_font_relative_viewport.html",
        render_opts: [fonts: []],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    refute File.exists?(Path.join(stats.artifact_dir, "chromium-input.html"))
  end
end
