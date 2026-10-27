defmodule NativeElixirPdfUtilities.HtmlToPdf.CssCommentBoundariesBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  @moduletag :browser_parity

  test "comment boundaries in selectors, dimensions, functions and inline CSS match Chromium" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/css_comment_boundaries.html"),
        artifact_dir: "tmp/browser_parity/css_comment_boundaries",
        render_opts: [system_font_discovery: false],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
