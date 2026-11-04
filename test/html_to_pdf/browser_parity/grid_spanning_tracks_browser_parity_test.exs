defmodule NativeElixirPdfUtilities.HtmlToPdf.GridSpanningTracksBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  @moduletag :browser_parity

  test "spanning items size auto and mixed tracks before following flow" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/grid_spanning_tracks.html"),
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
