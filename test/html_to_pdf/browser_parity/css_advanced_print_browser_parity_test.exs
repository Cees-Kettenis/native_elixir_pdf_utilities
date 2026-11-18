defmodule NativeElixirPdfUtilities.HtmlToPdf.CssAdvancedPrintBrowserParityTest do
  use ExUnit.Case, async: true
  @moduletag :browser_parity

  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}

  test "advanced paints match a hand-composited static reference" do
    root = "html_to_pdf/browser_parity/css_advanced_print_paint"

    stats =
      PdfVisualCompare.assert_browser_match!(Paths.fixture(root <> ".html"),
        chromium_transform: fn _ -> File.read!(Paths.fixture(root <> "_reference.html")) end,
        artifact_dir: "tmp/browser_parity/css_advanced_print_paint",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
