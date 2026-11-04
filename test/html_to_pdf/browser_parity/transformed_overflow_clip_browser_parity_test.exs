defmodule NativeElixirPdfUtilities.HtmlToPdf.TransformedOverflowClipBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  @moduletag :browser_parity

  test "ancestor clips stay outside child scales reflections and nested transforms" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/transformed_overflow_clip.html"),
        artifact_dir: Paths.project("tmp/browser_parity/transformed_overflow_clip"),
        regions: [
          scaled: {8, 8, 8, 8},
          parent: {98, 8, 16, 8},
          nested: {20, 50, 16, 8},
          reflected: {92, 50, 8, 8}
        ],
        max_changed_ratio: 0.01,
        max_average_delta: 0.005
      )

    assert stats.page_count == 1
    assert Enum.map(hd(stats.pages).regions, & &1.native_ink_pixels) == [64, 128, 128, 64]
  end

  test "a clipped tall child does not extend the containing parent's page allocation" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/transformed_overflow_tall_child.html"),
        artifact_dir: Paths.project("tmp/browser_parity/transformed_overflow_tall_child"),
        regions: [inside: {4, 4, 20, 20}, edge: {4, 20, 20, 20}],
        max_changed_ratio: 0.001,
        max_average_delta: 0.001
      )

    assert stats.page_count == 1
    assert Enum.map(hd(stats.pages).regions, & &1.native_ink_pixels) == [400, 200]
  end

  test "percentage transforms use each physical fragment and preserve later clipped paint" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/transformed_overflow_fragments.html"),
        artifact_dir: Paths.project("tmp/browser_parity/transformed_overflow_fragments"),
        regions: [top: {5, 5, 20, 20}, body: {5, 25, 20, 20}],
        max_changed_ratio: 0.001,
        max_average_delta: 0.001
      )

    assert stats.page_count == 4

    assert Enum.map(stats.pages, fn page -> Enum.map(page.regions, & &1.native_ink_pixels) end) ==
             [[200, 400], [400, 400], [400, 400], [400, 400]]
  end
end
