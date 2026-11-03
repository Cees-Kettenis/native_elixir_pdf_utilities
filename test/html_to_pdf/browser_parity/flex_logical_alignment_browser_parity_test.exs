defmodule NativeElixirPdfUtilities.HtmlToPdf.FlexLogicalAlignmentBrowserParityTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}

  @moduletag :browser_parity

  test "logical and flex edges match Chromium across reversal, wrapping, margins and overflow" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/flex_logical_alignment.html"),
        max_changed_ratio: 0.001,
        max_average_delta: 0.001
      )

    assert stats.page_count == 1
  end
end
