defmodule NativeElixirPdfUtilities.HtmlToPdf.OverflowPaginationBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  @moduletag :browser_parity

  test "ordinary overflow clipping preserves the following flow and excludes hidden sheets" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/overflow_hidden_flow.html"),
        artifact_dir: Paths.project("tmp/browser_parity/overflow_hidden_flow"),
        regions: [inside: {4, 4, 20, 20}, following: {4, 34, 20, 8}, edge: {40, 20, 10, 20}],
        max_changed_ratio: 0.001,
        max_average_delta: 0.001
      )

    assert stats.page_count == 1
    assert Enum.map(hd(stats.pages).regions, & &1.native_ink_pixels) == [400, 160, 100]
  end

  test "a long containing viewport retains every visible fragment but no clipped continuation" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/overflow_hidden_flow_fragments.html"),
        artifact_dir: Paths.project("tmp/browser_parity/overflow_hidden_flow_fragments"),
        regions: [inside: {4, 4, 20, 20}],
        max_changed_ratio: 0.001,
        max_average_delta: 0.001
      )

    assert stats.page_count == 4
    assert Enum.map(stats.pages, &hd(&1.regions).native_ink_pixels) == [400, 400, 400, 400]
  end

  test "line-break gaps count toward a clipped fixed-height block's remaining space" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/overflow_hidden_text_fragments.html"),
        artifact_dir: Paths.project("tmp/browser_parity/overflow_hidden_text_fragments"),
        regions: [text: {0, 0, 30, 72}],
        max_changed_ratio: 0.01,
        max_average_delta: 0.005
      )

    assert stats.page_count == 4
    assert Enum.all?(stats.pages, &(hd(&1.regions).native_ink_pixels > 100))
  end

  test "a partially occupied final sheet clips text and retains the following painted block" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/overflow_hidden_text_tail.html"),
        artifact_dir: Paths.project("tmp/browser_parity/overflow_hidden_text_tail"),
        regions: [text: {0, 0, 30, 50}, tail: {0, 57, 30, 18}],
        max_changed_ratio: 0.01,
        max_average_delta: 0.005
      )

    assert stats.page_count == 4
    assert Enum.at(List.last(stats.pages).regions, 1).native_ink_pixels >= 210
  end
end
