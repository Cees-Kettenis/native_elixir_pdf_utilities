defmodule NativeElixirPdfUtilities.HtmlToPdf.SmallAffineScaleBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  @moduletag :browser_parity

  test "small nonzero scale retains visible paint and zero scale remains invisible" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/small_affine_scale.html"),
        artifact_dir: Paths.project("tmp/browser_parity/small_affine_scale"),
        regions: [scaled_paint: {8, 8, 8, 8}],
        max_changed_ratio: 0.005,
        max_average_delta: 0.005
      )

    assert stats.page_count == 1
    assert hd(hd(stats.pages).regions).native_ink_pixels == 64
  end
end
