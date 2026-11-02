defmodule NativeElixirPdfUtilities.HtmlToPdf.RelativePercentageOffsetsBrowserParityTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  @moduletag :browser_parity

  test "relative percentages use definite containing heights across block and inline flow" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/relative_percentage_offsets.html"),
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
