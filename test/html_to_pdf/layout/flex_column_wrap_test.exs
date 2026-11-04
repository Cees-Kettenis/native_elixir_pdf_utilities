defmodule NativeElixirPdfUtilities.HtmlToPdf.FlexColumnWrapTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{HtmlToPdf, Info}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  test "the public renderer accepts columns that wrap within an explicit height" do
    html = """
    <div style='display:flex;flex-direction:column;flex-wrap:wrap;width:100pt;height:30pt'>
      <div style='height:20pt;flex-shrink:0'>A</div>
      <div style='height:20pt;flex-shrink:0'>B</div>
    </div>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, 1} = Info.page_count(pdf)
  end

  test "wrapped columns share spare width and retain their main-axis direction" do
    for {direction, top} <- [{"column", 300}, {"column-reverse", 290}] do
      [first, second, following] =
        boxes("""
        <div style='display:flex;flex-direction:#{direction};flex-wrap:wrap;width:100pt;height:30pt'>
          <div style='width:40pt;height:20pt;flex:none;background:red'></div>
          <div style='width:40pt;height:20pt;flex:none;background:blue'></div>
        </div><div style='height:10pt;background:lime'></div>
        """)

      assert_in_delta first.x, 0, 0.001
      assert_in_delta second.x, 50, 0.001
      assert_in_delta first.y + first.height, top, 0.001
      assert_in_delta second.y + second.height, top, 0.001
      assert_in_delta following.y + following.height, 270, 0.001
    end
  end

  test "column widths include gaps and equal stretch additions to unequal intrinsic widths" do
    for {alignment, expected} <- [
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
        <div style='display:flex;flex-direction:column;flex-wrap:wrap;width:100pt;height:30pt;column-gap:10pt;align-content:#{alignment}'>
          <div style='width:20pt;height:20pt;flex:none;background:red'></div>
          <div style='width:30pt;height:20pt;flex:none;background:blue'></div>
        </div>
        """)

      for {item, x} <- Enum.zip(items, expected), do: assert_in_delta(item.x, x, 0.001)
    end
  end

  test "automatic cross sizes and margins use their own column rather than the whole container" do
    [first, second] =
      boxes("""
      <div style='display:flex;flex-direction:column;flex-wrap:wrap;width:100pt;height:30pt;column-gap:10pt'>
        <div style='height:20pt;flex:none;background:red'></div>
        <div style='width:20pt;height:20pt;flex:none;margin-left:auto;background:blue'></div>
      </div>
      """)

    assert_in_delta first.width, 35, 0.001
    assert_in_delta second.x, 80, 0.001
    assert_in_delta second.width, 20, 0.001
  end

  test "resolved percentage and border-box heights keep the following flow below the container" do
    [first, second, following] =
      boxes("""
      <div style='height:100pt'>
        <div style='display:flex;flex-direction:column;flex-wrap:wrap;width:120pt;height:50%;box-sizing:border-box;padding:4pt;border:6pt solid black'>
          <div style='width:40pt;height:20pt;flex:none;background:red'></div>
          <div style='width:40pt;height:20pt;flex:none;background:blue'></div>
        </div><div style='height:10pt;background:lime'></div>
      </div>
      """)

    assert_in_delta first.x, 10, 0.001
    assert_in_delta second.x, 60, 0.001
    assert_in_delta first.y + first.height, 290, 0.001
    assert_in_delta following.y + following.height, 250, 0.001
  end

  test "overflowing columns keep their widths and gaps when aligning the whole group" do
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
        <div style='display:flex;flex-direction:column;flex-wrap:wrap;width:100pt;height:30pt;column-gap:10pt;align-content:#{alignment}'>
          <div style='width:70pt;height:20pt;flex:none;background:red'></div>
          <div style='width:70pt;height:20pt;flex:none;background:blue'></div>
        </div>
        """)

      assert_in_delta first.x, offset, 0.001
      assert_in_delta second.x, offset + 80, 0.001
      assert_in_delta first.width, 70, 0.001
      assert_in_delta second.width, 70, 0.001
    end
  end

  test "one occupied column uses wrap-mode alignment while nowrap keeps its full cross size" do
    for {wrap, expected_x} <- [{"wrap", 80}, {"nowrap", 0}] do
      [item] =
        boxes("""
        <div style='display:flex;flex-direction:column;flex-wrap:#{wrap};width:100pt;height:30pt;align-content:flex-end'>
          <div style='width:20pt;height:20pt;flex:none;background:red'></div>
        </div>
        """)

      assert_in_delta item.x, expected_x, 0.001
    end
  end

  test "unconstrained columns keep their natural height and definite columns preserve min and max" do
    for {height, expected} <- [
          {"", 45},
          {"min-height:60pt", 60},
          {"height:100pt;max-height:30pt", 30},
          {"height:20pt;min-height:30pt", 30}
        ] do
      items =
        boxes("""
        <div style='display:flex;flex-direction:column;flex-wrap:wrap;width:100pt;row-gap:5pt;#{height}'>
          <div style='width:40pt;height:20pt;flex:none;background:red'></div>
          <div style='width:40pt;height:20pt;flex:none;background:blue'></div>
        </div><div style='height:10pt;background:lime'></div>
        """)

      following = List.last(items)
      assert_in_delta following.y + following.height, 300 - expected, 0.001
    end
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
