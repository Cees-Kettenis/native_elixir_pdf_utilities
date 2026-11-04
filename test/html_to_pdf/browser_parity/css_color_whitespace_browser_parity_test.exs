alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.CssColorWhitespaceBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @tag :css_color_whitespace
  test "multiline RGB function whitespace keeps Chrome source colors and selected font" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/html_multiline_rgb_colors.html"),
        artifact_dir:
          "tmp/test_artifacts/css_color_whitespace_browser_parity/multiline-rgb-browser",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1 and stats.scripting == :disabled
  end
end
