defmodule NativeElixirPdfUtilities.HtmlToPdf.FlowLayoutBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  @moduletag :browser_parity

  test "flow algorithms retain color and line geometry" do
    # The existing grid sheet has space, but embedding this source changed a
    # native rule endpoint by one raster pixel (maximum channel delta 7).
    # Keep its original canvas rather than weakening exact sheet equivalence.
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/flow_layout_algorithms.html"),
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
