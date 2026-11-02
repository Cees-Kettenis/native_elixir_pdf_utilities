defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgAdaptiveDensityBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  @moduletag :browser_parity

  test "repeated large intrinsic SVG images retain their visible CSS size" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/svg_adaptive_density.html"),
        artifact_dir: Paths.project("tmp/browser_parity/svg_adaptive_density"),
        regions: [first_image: {100, 2, 20, 8}, repeated_image: {100, 14, 20, 8}],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
