defmodule NativeElixirPdfUtilities.HtmlToPdf.FlexRowAlignmentTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  test "fractional row origins use layout precision without changing column or nowrap geometry" do
    for {direction, wrap, height, top} <- [
          {"row", "wrap", "height:100pt", 299.75390625},
          {"row", "nowrap", "height:100pt", 299.75},
          {"column", "wrap", "height:100pt", 299.75},
          {"column", "nowrap", "height:100pt", 299.75},
          {"row", "wrap", "", 299.75}
        ] do
      [item] =
        boxes("""
        <div style='height:0.25pt'></div>
        <div style='display:flex;flex-direction:#{direction};flex-wrap:#{wrap};width:100pt;#{height};align-content:flex-start'>
          <div style='width:20pt;height:20pt;flex:none;background:red'></div>
        </div>
        """)

      assert_in_delta item.y + item.height, top, 0.000001
      assert item.height == 20
    end
  end

  test "fractional stretch additions retain layout precision and distribute the remainder" do
    [first, second, third] =
      boxes("""
      <div style='display:flex;flex-wrap:wrap;width:100pt;height:31px'>
        <div style='width:60pt;flex:none;background:red'></div>
        <div style='width:60pt;flex:none;background:blue'></div>
        <div style='width:60pt;flex:none;background:lime'></div>
      </div>
      """)

    assert_in_delta first.height, 661 / 64 * 0.75, 0.00001
    assert_in_delta second.height, 661 / 64 * 0.75, 0.00001
    assert_in_delta third.height, 662 / 64 * 0.75, 0.00001
    assert_in_delta third.y, 300 - 31 * 0.75, 0.00001
  end

  test "wrapped rows distribute spare height including gaps and unequal row heights" do
    for {alignment, offsets} <- [
          {"normal", [0, 50]},
          {"stretch", [0, 50]},
          {"flex-start", [0, 30]},
          {"flex-end", [40, 70]},
          {"center", [20, 50]},
          {"space-between", [0, 70]},
          {"space-around", [10, 60]},
          {"space-evenly", [40 / 3, 170 / 3]}
        ] do
      items =
        boxes("""
        <div style='display:flex;flex-wrap:wrap;width:100pt;height:100pt;row-gap:10pt;align-content:#{alignment}'>
          <div style='width:60pt;height:20pt;flex:none;background:red'></div>
          <div style='width:60pt;height:30pt;flex:none;background:blue'></div>
        </div>
        """)

      for {item, offset} <- Enum.zip(items, offsets),
          do: assert_in_delta(item.y + item.height, 300 - offset, 0.75 / 64)

      assert Enum.map(items, & &1.height) == [20.0, 30.0]
    end
  end

  test "stretch and automatic cross margins use the row's allocated height" do
    [first, second, third, fourth] =
      boxes("""
      <div style='display:flex;flex-wrap:wrap;width:100pt;height:100pt;row-gap:10pt'>
        <div style='width:50pt;height:20pt;flex:none;background:red'></div>
        <div style='width:50pt;flex:none;background:blue'></div>
        <div style='width:50pt;height:30pt;flex:none;background:red'></div>
        <div style='width:50pt;height:10pt;flex:none;margin-top:auto;background:blue'></div>
      </div>
      """)

    assert_in_delta first.y + first.height, 300, 0.75 / 64
    assert_in_delta second.y + second.height, 300, 0.75 / 64
    assert_in_delta second.height, 40, 0.75 / 64
    assert_in_delta third.y + third.height, 250, 0.75 / 64
    assert_in_delta fourth.y, 200, 0.75 / 64
  end

  test "one occupied row honors wrap alignment while nowrap fills the cross size" do
    for {wrap, alignment, offset} <- [
          {"wrap", "flex-start", 0},
          {"wrap", "flex-end", 80},
          {"wrap", "center", 40},
          {"wrap", "space-between", 0},
          {"wrap", "space-around", 40},
          {"wrap", "space-evenly", 40},
          {"nowrap", "flex-end", 0}
        ] do
      [item] =
        boxes("""
        <div style='display:flex;flex-wrap:#{wrap};width:100pt;height:100pt;align-content:#{alignment}'>
          <div style='width:50pt;height:20pt;flex:none;background:red'></div>
        </div>
        """)

      assert_in_delta item.y + item.height, 300 - offset, 0.75 / 64
    end

    for wrap <- ["wrap", "nowrap"] do
      [fixed, stretched] =
        boxes("""
        <div style='display:flex;flex-wrap:#{wrap};width:100pt;height:100pt'>
          <div style='width:50pt;height:20pt;flex:none;background:red'></div>
          <div style='width:50pt;flex:none;background:blue'></div>
        </div>
        """)

      assert_in_delta fixed.height, 20, 0.75 / 64
      assert_in_delta stretched.height, 100, 0.75 / 64
    end
  end

  test "overflowing rows retain their heights and gaps when aligning the group" do
    for {alignment, offset} <- [
          {"stretch", 0},
          {"flex-end", -50},
          {"center", -25},
          {"space-between", 0},
          {"space-around", -25},
          {"space-evenly", -25}
        ] do
      [first, second] =
        boxes("""
        <div style='display:flex;flex-wrap:wrap;width:100pt;height:100pt;row-gap:10pt;align-content:#{alignment}'>
          <div style='width:60pt;height:70pt;flex:none;background:red'></div>
          <div style='width:60pt;height:70pt;flex:none;background:blue'></div>
        </div>
        """)

      assert_in_delta first.y + first.height, 300 - offset, 0.75 / 64
      assert_in_delta second.y + second.height, 220 - offset, 0.75 / 64
      assert_in_delta first.height, 70, 0.75 / 64
      assert_in_delta second.height, 70, 0.75 / 64
    end
  end

  test "resolved cross sizes preserve following flow and row alignment" do
    for {height, container_height} <- [
          {"", 60},
          {"min-height:100pt", 100},
          {"height:50%", 100},
          {"height:150pt;max-height:100pt", 100},
          {"height:40pt;min-height:100pt", 100}
        ] do
      [first, second, following] =
        boxes("""
        <div style='height:200pt'>
          <div style='display:flex;flex-wrap:wrap;width:100pt;row-gap:10pt;align-content:flex-end;#{height}'>
            <div style='width:60pt;height:20pt;flex:none;background:red'></div>
            <div style='width:60pt;height:30pt;flex:none;background:blue'></div>
          </div><div style='height:10pt;background:lime'></div>
        </div>
        """)

      assert_in_delta first.y + first.height, 300 - container_height + 60, 0.75 / 64
      assert_in_delta second.y, 300 - container_height, 0.75 / 64
      assert_in_delta following.y + following.height, 300 - container_height, 0.75 / 64
    end
  end

  test "border-box padding and item alignment use the stretched row height" do
    [first, second] =
      boxes("""
      <div style='display:flex;flex-wrap:wrap;width:120pt;height:120pt;box-sizing:border-box;padding:4pt;border:6pt solid black;row-gap:10pt;align-items:center'>
        <div style='width:60pt;height:20pt;flex:none;background:red'></div>
        <div style='width:60pt;height:30pt;flex:none;background:blue'></div>
      </div>
      """)

    assert_in_delta first.x, 10, 0.75 / 64
    assert_in_delta first.y + first.height, 280, 0.75 / 64
    assert_in_delta second.y + second.height, 230, 0.75 / 64
  end

  defp boxes(html) do
    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {300, 300}, margin: 0)

    Enum.filter(layout.boxes, fn box ->
      box.type == :rect and Map.get(box, :fill_color) in [{1, 0, 0}, {0, 0, 1}, {0, 1, 0}]
    end)
  end
end
