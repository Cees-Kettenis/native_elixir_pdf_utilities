defmodule NativeElixirPdfUtilities.HtmlToPdf.GridAutoStretchBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  @moduletag :browser_parity

  test "automatic columns stretch while explicit alignment retains intrinsic widths" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/grid_auto_stretch.html"),
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
