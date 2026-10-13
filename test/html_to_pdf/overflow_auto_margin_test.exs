defmodule NativeElixirPdfUtilities.HtmlToPdf.OverflowAutoMarginTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  @image "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAICAIAAAB/FOjAAAAAGElEQVR4nGM4oSGHFYkELMCKGEY10EIDAJ7zhIFJAvtOAAAAAElFTkSuQmCC"

  test "overflowing main-axis automatic margins become zero before authored justification" do
    for {direction, justify, expected} <- [
          {"row", "center", -10.0},
          {"row", "flex-end", -20.0},
          {"row", "space-around", -10.0},
          {"row", "space-evenly", -10.0},
          {"row", "space-between", 0.0},
          {"row-reverse", "flex-start", -20.0},
          {"row-reverse", "flex-end", 0.0},
          {"row-reverse", "center", -10.0}
        ] do
      html =
        "<div style='display:flex;flex-direction:#{direction};justify-content:#{justify};width:100pt;height:80pt'><img src='#{@image}' style='flex:none;width:120pt;height:10pt;margin-left:auto;margin-right:auto'></div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout(styled, page_size: {150, 100}, margin: 0)
      [image] = Enum.filter(layout.boxes, &(&1.type == :image))
      assert image.x == expected, "#{direction} #{justify}"
    end
  end

  test "column overflow uses actual negative remaining height" do
    html =
      "<div style='display:flex;flex-direction:column;justify-content:center;width:100pt;height:80pt'><img src='#{@image}' style='flex:none;width:20pt;height:100pt;margin:auto'></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {150, 200}, margin: 0)
    [image] = Enum.filter(layout.boxes, &(&1.type == :image))
    assert {image.x, image.y, image.width, image.height} == {40.0, 110.0, 20.0, 100.0}
  end

  test "reverse directions retain physical margins and start at the reversed main edge" do
    for {direction, expected} <- [
          {"row-reverse", {80.0, 190.0}},
          {"column-reverse", {0.0, 120.0}}
        ] do
      html =
        "<div style='display:flex;flex-direction:#{direction};width:100pt;height:80pt'><img src='#{@image}' style='flex:none;width:20pt;height:10pt'></div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout(styled, page_size: {150, 200}, margin: 0)
      [image] = Enum.filter(layout.boxes, &(&1.type == :image))
      assert {image.x, image.y} == expected
    end
  end

  @tag :browser_parity
  @tag :flex_distribution_regression_original
  test "existing flex direction and explicit cross-size originals remain unchanged" do
    for {name, delta} <- [
          {"flex_direction_and_justification.html", 0.035},
          {"flex_explicit_cross_sizes.html", 0.03}
        ] do
      fixture = Path.expand("../fixtures/html_to_pdf/browser_parity/#{name}", __DIR__)

      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/overflow-margin-regression-#{name}",
        max_changed_ratio: 0.01,
        max_average_delta: delta
      )
    end
  end

  @tag :browser_parity
  @tag :overflow_auto_margins_original
  test "overflow automatic margins match original Chromium" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/html_overflow_auto_margins.html",
        __DIR__
      )

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/html_overflow_auto_margins",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
