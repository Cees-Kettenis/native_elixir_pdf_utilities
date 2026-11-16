defmodule NativeElixirPdfUtilities.HtmlToPdf.FloatLayoutTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  # CSS 2.2, section 9.5: floats occupy margin boxes, do not advance normal
  # block flow and constrain the available interval of each overlapping line.
  test "opposing floats share a band and clearing uses their margin edges" do
    layout =
      layout("""
      <div style='width:200pt;display:flow-root'>
        <div style='float:left;width:40pt;height:30pt;margin:5pt;background:red'></div>
        <div style='float:right;width:60pt;height:50pt;background:blue'></div>
        <div style='clear:both;height:10pt;background:lime'></div>
      </div>
      """)

    [left, right, cleared] = rects(layout)
    assert {left.x, left.y + left.height} == {5.0, 295.0}
    assert {right.x, right.y + right.height} == {140.0, 300.0}
    assert cleared.y + cleared.height == 250
  end

  test "a float that cannot fit beside its predecessor moves below it" do
    layout =
      layout("""
      <div style='width:100pt;display:flow-root'>
        <div style='float:left;width:60pt;height:20pt;background:red'></div>
        <div style='float:left;width:60pt;height:30pt;background:blue'></div>
      </div>
      <div style='height:10pt;background:lime'></div>
      """)

    [first, second, following] = rects(layout)
    assert first.y + first.height == 300
    assert second.y + second.height == 280
    assert following.y + following.height == 250
  end

  test "paragraph lines return to full width after a float's bottom edge" do
    layout =
      layout("""
      <div style='width:100pt;font-size:10pt;line-height:10pt'>
        <div style='float:left;width:40pt;height:20pt;background:red'></div>
        <p style='margin:0'>word word word word word word word word word word word word</p>
      </div>
      """)

    texts = Enum.filter(layout.boxes, &(&1.type == :text))
    lines = Enum.group_by(texts, & &1.y) |> Enum.sort_by(&elem(&1, 0), :desc)
    assert length(lines) >= 3
    assert Enum.min(Enum.map(elem(Enum.at(lines, 0), 1), & &1.x)) == 40
    assert Enum.min(Enum.map(elem(Enum.at(lines, 1), 1), & &1.x)) == 40
    assert Enum.min(Enum.map(elem(Enum.at(lines, 2), 1), & &1.x)) == 0
  end

  test "a separate formatting context avoids floats and contains its own descendants" do
    layout =
      layout("""
      <div style='width:100pt'>
        <div style='float:left;width:40pt;height:30pt;background:red'></div>
        <div style='display:flow-root;height:10pt;background:blue'></div>
        <div style='clear:left;height:10pt;background:lime'></div>
      </div>
      """)

    [_, independent, cleared] = rects(layout)
    assert {independent.x, independent.width} == {40.0, 60.0}
    assert cleared.y + cleared.height == 270
  end

  test "logical float and clear edges follow inherited direction" do
    for {direction, x} <- [{"ltr", 0}, {"rtl", 80}] do
      layout =
        layout("""
        <div style='direction:#{direction};width:100pt'>
          <span style='float:inline-start;width:20pt;height:20pt;background:red'></span>
          <div style='clear:inline-start;height:10pt;background:blue'></div>
        </div>
        """)

      [floating, cleared] = rects(layout)
      assert floating.x == x
      assert cleared.y + cleared.height == 280
    end
  end

  test "source floats inside paragraphs share the current line and retain nested inline ownership" do
    for float <- [
          "<span style='float:left;width:30pt;height:20pt;background:red'></span>",
          "<em><span style='float:left;width:30pt;height:20pt;background:red'></span></em>"
        ] do
      layout =
        layout(
          "<p style='margin:0;width:100pt;font-size:10pt;line-height:10pt'>a #{float}word word word word word word</p>"
        )

      [floating] = rects(layout)
      assert floating.y + floating.height == 300
      first = layout.boxes |> Enum.filter(&(&1.type == :text)) |> Enum.max_by(& &1.y)
      assert first.x >= 30
    end
  end

  test "an indivisible word wider than a float's remaining band starts below the float" do
    layout =
      layout("""
      <div style='width:100pt;font-size:10pt;line-height:10pt'>
        <div style='float:left;width:80pt;height:25pt;background:red'></div>
        <p style='margin:0'>unbreakable</p>
      </div>
      """)

    word = Enum.find(layout.boxes, &(Map.get(&1, :text) == "unbreakable"))
    assert word.x == 0
    assert word.y < 275
  end

  test "floats and clear are ignored on flex and grid items" do
    for display <- ["flex", "grid"] do
      layout =
        layout("""
        <div style='display:#{display};width:100pt;grid-template-columns:20pt 20pt'>
          <div style='float:right;width:20pt;height:20pt;background:red'></div>
          <div style='clear:both;float:right;width:20pt;height:20pt;background:blue'></div>
        </div>
        """)

      [first, second] = rects(layout)
      assert first.x == 0
      assert second.x == 20
      assert first.y == second.y
    end
  end

  defp rects(layout), do: Enum.filter(layout.boxes, &(&1.type == :rect))

  test "logical end floats and clearance follow both inline directions" do
    for {direction, x} <- [{"ltr", 80}, {"rtl", 0}] do
      tree =
        layout("""
        <div style='width:100pt;direction:#{direction}'>
          <div style='float:inline-end;width:20pt;height:20pt;background:red'></div>
          <div style='clear:inline-end;height:5pt;background:blue'></div>
        </div>
        """)

      red = Enum.find(tree.boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
      blue = Enum.find(tree.boxes, &(Map.get(&1, :fill_color) == {0, 0, 1}))
      assert_in_delta red.x, x, 0.001
      assert_in_delta blue.y + blue.height, 280, 0.001
    end
  end

  test "formatting-context avoidance considers floats below its initial top" do
    alias NativeElixirPdfUtilities.HtmlToPdf.FloatLayout

    FloatLayout.with_context(fn ->
      render = fn node, x, top, width ->
        height = node.style.height
        {:ok, [%{type: :rect, x: x, y: top - height, width: width, height: height}], top - height}
      end

      floating = %{
        type: :element,
        style: %{display: :block, float: :right, width: 40, height: 30, line_height: 12}
      }

      assert {:ok, _, 80} =
               FloatLayout.layout(floating, 0, 80, 100, render, fn _, _ -> {40, 40} end)

      independent = %{
        type: :element,
        style: %{display: :block, _flow_root: true, width: 80, height: 40}
      }

      assert {:ok, [box], bottom} =
               FloatLayout.layout(independent, 0, 100, 100, render, fn _, _ -> {80, 80} end)

      assert bottom == 10
      assert box.y + box.height == 50
    end)

    assert FloatLayout.fit_line([], 10, 12) == []
  end

  test "inside curves and inline padding reserve the complete first indivisible item" do
    tree =
      layout("""
      <p style='margin:0;width:40pt;height:40pt;font-size:10pt;line-height:10pt;shape-inside:circle(20pt)'>WW WW WW WW</p>
      """)

    first = Enum.find(tree.boxes, &(&1.type == :text))
    assert first.x > 0
    assert first.y < 290

    tree =
      layout("""
      <div style='width:100pt;font-size:10pt;line-height:12pt'><div style='float:left;width:40pt;height:20pt'></div><p style='margin:0'><span style='padding:0 10pt'>WWWWWW</span></p></div>
      """)

    first = Enum.find(tree.boxes, &(&1.type == :text))
    assert first.y < 280
  end

  test "successive inline floats measure a prefix that already wraps around an exclusion" do
    tree =
      layout(
        "<p style='width:100pt;font-size:10pt;line-height:10pt;margin:0'><span style='float:left;width:40pt;height:30pt'></span>one two three four five <span style='float:right;width:20pt;height:10pt'></span>six seven eight</p>"
      )

    assert Enum.filter(tree.boxes, &(&1.type == :text)) |> Enum.map(& &1.text) |> Enum.join(" ") =~
             "eight"
  end

  test "emergency word wrapping beside floats retains every source character" do
    tree =
      layout(
        "<div style='width:100pt;font-size:10pt;line-height:10pt;overflow-wrap:anywhere'><div style='float:left;width:80pt;height:30pt'></div>abcdefghijklmn</div>"
      )

    assert Enum.filter(tree.boxes, &(&1.type == :text)) |> Enum.map(& &1.text) |> Enum.join() ==
             "abcdefghijklmn"
  end

  test "flex and grid item automatic heights include their contained floats" do
    for display <- ["flex", "grid;grid-template-columns:20pt"] do
      tree =
        layout(
          "<div style='display:#{display};width:100pt'><div style='background:red'><div style='float:left;width:20pt;height:20pt;background:blue'></div></div></div><div style='height:10pt;background:lime'></div>"
        )

      owner = Enum.find(tree.boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
      following = Enum.find(tree.boxes, &(Map.get(&1, :fill_color) == {0, 1, 0}))
      assert_in_delta owner.height, 20, 0.02
      assert_in_delta following.y + following.height, 280, 0.02
    end
  end

  defp layout(html) do
    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {300, 300}, margin: 0)
    layout
  end
end
