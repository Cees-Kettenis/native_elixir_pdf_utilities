defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgForeignObjectOriginalBrowserTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity

  test "original selected SVG document retains its grid canvas" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/html_svg_document_grid_initial_canvas.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        render_opts: [fonts: []],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    refute File.exists?(Path.join(stats.artifact_dir, "chromium-input.html"))
  end
end
