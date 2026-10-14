defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgRasterColorBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :svg_raster_color

  test "restricted SVG images preserve gradient interpolation, alpha and filter input" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        "test/fixtures/html_to_pdf/browser_parity/svg_image_gradient_color.html",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
