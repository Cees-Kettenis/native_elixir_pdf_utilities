alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.CssDimensionsBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity

  test "calculated box dimensions and ordinary inline padding match the original Chromium source" do
    fixture =
      Paths.fixture("html_to_pdf/browser_parity/css_calculated_dimensions_inline_padding.html")

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/css_calculated_dimensions_inline_padding",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    assert stats.scripting == :disabled
  end
end
