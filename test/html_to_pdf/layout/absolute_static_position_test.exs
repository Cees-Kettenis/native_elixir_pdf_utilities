defmodule NativeElixirPdfUtilities.HtmlToPdf.AbsoluteStaticPositionTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  test "block source anchors survive following flow and unused container space" do
    for {before, top} <- [{"", 0}, {"<div style='height:40pt;background:blue'></div>", 40}] do
      boxes =
        layout("""
        <div style='position:relative;height:200pt;width:200pt'>#{before}
          <div style='position:absolute;width:20pt;height:20pt;background:red'></div>
          <div style='height:30pt;background:#00ff00'></div>
        </div>
        """)

      red = Enum.find(boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
      green = Enum.find(boxes, &(Map.get(&1, :fill_color) == {0, 1, 0}))
      assert_in_delta red.x, 0, 0.001
      assert_in_delta 300 - red.y - red.height, top, 0.001
      assert_in_delta 300 - green.y - green.height, top, 0.001
    end
  end

  test "static positions retain nested flow coordinates and explicit insets win" do
    for {insets, x, top} <- [
          {"", 20, 60},
          {"left:7pt;top:9pt", 7, 9},
          {"right:7pt;bottom:9pt", 173, 171}
        ] do
      boxes =
        layout("""
        <div style='position:relative;height:200pt;width:200pt'>
          <div style='padding:20pt'><div style='height:40pt'></div>
            <div style='position:absolute;#{insets};width:20pt;height:20pt;background:red'></div>
          </div>
        </div>
        """)

      red = Enum.find(boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
      assert_in_delta red.x, x, 0.001
      assert_in_delta 300 - red.y - red.height, top, 0.001
    end
  end

  test "inline and hypothetical block anchors do not interrupt surrounding text" do
    for {display, top} <- [{"inline", 0}, {"block", 20}] do
      source = fn absolute ->
        "<div style='position:relative;height:200pt;line-height:20pt'>" <>
          "<span style='color:blue'>Before</span>" <>
          absolute <>
          "<span style='color:green'>After</span></div>"
      end

      ordinary = layout(source.(""))

      boxes =
        layout(
          source.(
            "<span style='position:absolute;display:#{display};width:20pt;height:20pt;background:red'></span>"
          )
        )

      following = Enum.find(ordinary, &(Map.get(&1, :text) == "After"))
      red = Enum.find(boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
      assert_in_delta 300 - red.y - red.height, top, 0.001
      assert_in_delta red.x, if(display == "inline", do: following.x, else: 0), 0.02

      for text <- ["Before", "After"] do
        plain = Enum.find(ordinary, &(Map.get(&1, :text) == text))
        actual = Enum.find(boxes, &(Map.get(&1, :text) == text))
        assert_in_delta actual.x, plain.x, 0.001
        assert_in_delta actual.y, plain.y, 0.001
      end
    end
  end

  test "empty source anchors preserve sibling margin collapsing" do
    source = fn absolute ->
      "<div style='position:relative'><div style='height:20pt;margin-bottom:12pt'></div>" <>
        absolute <> "<div style='height:20pt;margin-top:18pt;background:blue'></div></div>"
    end

    ordinary = layout(source.(""))

    boxes =
      layout(
        source.("<div style='position:absolute;width:10pt;height:10pt;background:red'></div>")
      )

    plain = Enum.find(ordinary, &(Map.get(&1, :fill_color) == {0, 0, 1}))
    actual = Enum.find(boxes, &(Map.get(&1, :fill_color) == {0, 0, 1}))
    assert_in_delta actual.y, plain.y, 0.001
  end

  defp layout(source) do
    source =
      "<style>body{margin:0;font-size:12pt;font-family:'DejaVu Sans';line-height:20pt}</style>" <>
        source

    assert {:ok, dom} = HtmlParser.parse_detailed(source)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, result} = Layout.layout(styled, page_size: {300, 300}, margin: 0)
    result.boxes
  end
end
