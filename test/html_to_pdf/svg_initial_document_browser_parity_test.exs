defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgInitialDocumentBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :svg_initial_document

  test "original SVG roots retain iframe sizing and actual object canvases" do
    source =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/html_svg_initial_document_geometry.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(source,
        render_opts: [fonts: []],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    refute File.exists?(Path.join(stats.artifact_dir, "chromium-input.html"))
  end

  test "skipped SVG child roots retain original host backgrounds and following flow" do
    source =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/html_svg_skipped_document_geometry.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(source,
        render_opts: [fonts: []],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    refute File.exists?(Path.join(stats.artifact_dir, "chromium-input.html"))
    refute File.read!(Path.join(stats.artifact_dir, "native.pdf")) =~ "/Subtype /Image"
  end
end
