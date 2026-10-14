defmodule NativeElixirPdfUtilities.HtmlToPdf.BodyClosureBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @tag :body_closure_rgb
  test "multiline RGB function whitespace keeps genuine Chrome source colors and selected font" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/html_multiline_rgb_colors.html",
          __DIR__
        ),
        artifact_dir: ".quality/orchestration/multiline-rgb-browser",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1 and stats.scripting == :disabled
  end
end
