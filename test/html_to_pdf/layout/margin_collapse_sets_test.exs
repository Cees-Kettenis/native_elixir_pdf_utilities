defmodule NativeElixirPdfUtilities.HtmlToPdf.MarginCollapseSetsTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style, MarginGeometry}

  # CSS 2.1 REC 7 June 2011, 8.3.1: largest positive plus most negative.
  test "collapse sets keep extremes through nested and empty boxes" do
    assert MarginGeometry.join(MarginGeometry.join(20, -10), 15) == {20, -10}
    assert MarginGeometry.distance({20, -10}) == 10

    for middle <- [
          "<div style='margin-top:20pt;margin-bottom:-10pt'></div>",
          "<div style='margin-top:20pt'><div style='margin-top:-10pt'></div></div>",
          "<div style='margin-top:20pt;background:red'><div style='margin-bottom:-10pt'></div></div>"
        ] do
      boxes =
        layout(
          "<div style='display:flow-root'><div style='height:10pt;margin-bottom:15pt'></div>#{middle}<div style='height:10pt;margin-top:12pt;background:blue'></div></div>"
        )

      blue = Enum.find(boxes, &(Map.get(&1, :fill_color) == {0, 0, 1}))
      assert_in_delta 300 - blue.y - blue.height, 20, 0.01
    end
  end

  test "nested parent edge collapse retains the original extrema" do
    boxes =
      layout(
        "<div style='display:flow-root'><div style='margin-top:15pt'><div style='margin-top:20pt'><div style='margin-top:-10pt;height:10pt;background:blue'></div></div></div></div>"
      )

    blue = Enum.find(boxes, &(Map.get(&1, :fill_color) == {0, 0, 1}))
    assert_in_delta 300 - blue.y - blue.height, 10, 0.01
  end

  test "padding borders and independent formatting contexts separate parent child margins" do
    for {boundary, expected} <- [
          {"padding-top:1pt", 21},
          {"border-top:1pt solid red", 20.75},
          {"display:flow-root", 20},
          {"overflow:hidden", 20},
          {"position:absolute;top:0", 20}
        ] do
      boxes =
        layout(
          "<div style='#{boundary};width:100pt;height:80pt;background:red'><div style='margin-top:20pt;height:10pt;background:blue'></div></div>"
        )

      red = Enum.find(boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
      blue = Enum.find(boxes, &(Map.get(&1, :fill_color) == {0, 0, 1}))
      assert_in_delta red.y + red.height - blue.y - blue.height, expected, 0.01
    end
  end

  defp layout(source) do
    assert {:ok, dom} = HtmlParser.parse_detailed(source)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, result} = Layout.layout(styled, page_size: {300, 300}, margin: 0)
    result.boxes
  end
end
