defmodule NativeElixirPdfUtilities.HtmlToPdf.AbsoluteStaticPositionBrowserParityTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  @moduletag :browser_parity

  test "automatic absolute insets retain block and inline source positions" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/absolute_static_position.html"),
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
