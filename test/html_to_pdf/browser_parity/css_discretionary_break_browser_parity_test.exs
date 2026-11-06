alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.CssDiscretionaryBreakBrowserParityTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  test "soft hyphens paint only at selected breaks and formatting characters do not add spacing" do
    fixture =
      Paths.fixture("html_to_pdf/browser_parity/css_discretionary_hyphens.html")

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir:
          "tmp/test_artifacts/css_discretionary_break_browser_parity/discretionary-browser",
        render_opts: [system_font_discovery: false],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled
    assert stats.page_count == 2
  end
end
