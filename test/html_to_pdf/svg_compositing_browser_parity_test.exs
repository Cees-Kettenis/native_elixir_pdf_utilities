defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgCompositingBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :svg_compositing

  test "actual graphics compositing, isolated opacity and mask transfer" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        "test/fixtures/html_to_pdf/browser_parity/svg_image_linear_compositing.html",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
