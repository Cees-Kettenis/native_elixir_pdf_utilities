defmodule NativeElixirPdfUtilities.HtmlToPdf.FlexLogicalAlignmentTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  test "logical edges retain item order and differ from reversed flex edges" do
    for direction <- ["row", "row-reverse", "column", "column-reverse"],
        alignment <- ["start", "end", "flex-start", "flex-end"],
        size <- [100, 40] do
      reversed = String.ends_with?(direction, "reverse")
      row = String.starts_with?(direction, "row")
      free = size - 60

      offset =
        case alignment do
          "start" -> 0
          "end" -> free
          "flex-start" -> if reversed, do: free, else: 0
          "flex-end" -> if reversed, do: 0, else: free
        end

      items =
        boxes("""
        <div style='display:flex;flex-direction:#{direction};width:#{size}pt;height:#{size}pt;gap:10pt;justify-content:#{alignment}'>
          <div style='width:20pt;height:20pt;flex:none;background:red'></div>
          <div style='width:30pt;height:30pt;flex:none;background:blue'></div>
        </div>
        """)

      red = Enum.find(items, &(&1.fill_color == {1, 0, 0}))
      blue = Enum.find(items, &(&1.fill_color == {0, 0, 1}))
      red_position = if row, do: red.x, else: 300 - red.y - red.height
      blue_position = if row, do: blue.x, else: 300 - blue.y - blue.height
      assert_in_delta red_position, offset + if(reversed, do: 40, else: 0), 0.000001
      assert_in_delta blue_position, offset + if(reversed, do: 0, else: 30), 0.000001
      assert {red.width, red.height, blue.width, blue.height} == {20, 20, 30, 30}
    end
  end

  test "logical alignment applies independently to each wrapped line" do
    for direction <- ["row-reverse", "column-reverse"],
        {alignment, offset} <- [{"start", 0}, {"end", 40}] do
      row = direction == "row-reverse"
      {width, height} = if row, do: {60, 20}, else: {20, 60}

      items =
        boxes("""
        <div style='display:flex;flex-direction:#{direction};flex-wrap:wrap;width:100pt;height:100pt;justify-content:#{alignment};align-content:center;gap:10pt'>
          <div style='width:#{width}pt;height:#{height}pt;flex:none;background:red'></div>
          <div style='width:#{width}pt;height:#{height}pt;flex:none;background:blue'></div>
        </div>
        """)

      for {item, index} <- Enum.with_index(items) do
        main = if row, do: item.x, else: 300 - item.y - item.height
        cross = if row, do: 300 - item.y - item.height, else: item.x
        assert_in_delta main, offset, 0.000001
        assert_in_delta cross, 25 + index * 30, 0.75 / 64
      end
    end
  end

  test "automatic main margins still consume free space before logical alignment" do
    for direction <- ["row-reverse", "column-reverse"],
        alignment <- ["start", "end", "flex-start", "flex-end"] do
      row = direction == "row-reverse"
      margin = if row, do: "margin-left", else: "margin-top"

      items =
        boxes("""
        <div style='display:flex;flex-direction:#{direction};width:100pt;height:100pt;gap:10pt;justify-content:#{alignment}'>
          <div style='width:20pt;height:20pt;flex:none;#{margin}:auto;background:red'></div>
          <div style='width:20pt;height:20pt;flex:none;background:blue'></div>
        </div>
        """)

      for item <- items do
        position = if row, do: item.x, else: 300 - item.y - item.height
        assert_in_delta position, if(item.fill_color == {1, 0, 0}, do: 80, else: 0), 0.000001
      end
    end
  end

  test "grid content retains logical edge alignment on both axes" do
    for {alignment, x, top} <- [
          {"start", 0, 0},
          {"flex-start", 0, 0},
          {"end", 40, 90},
          {"flex-end", 40, 90}
        ] do
      [first, second] =
        boxes("""
        <div style='display:grid;width:100pt;height:100pt;grid-template-columns:20pt 30pt;grid-template-rows:10pt;column-gap:10pt;justify-content:#{alignment};align-content:#{alignment}'>
          <div style='background:red'></div><div style='background:blue'></div>
        </div>
        """)

      assert_in_delta first.x, x, 0.000001
      assert_in_delta second.x, x + 30, 0.000001
      assert_in_delta first.y + first.height, 300 - top, 0.000001
      assert {first.width, second.width, first.height, second.height} == {20, 30, 10, 10}
    end
  end

  test "wrapped cross-axis start and end retain the earlier row and column fixes" do
    for direction <- ["row", "row-reverse", "column", "column-reverse"],
        {alignment, offset} <- [{"start", 0}, {"flex-start", 0}, {"end", 50}, {"flex-end", 50}] do
      row = String.starts_with?(direction, "row")
      {width, height} = if row, do: {60, 20}, else: {20, 60}

      items =
        boxes("""
        <div style='display:flex;flex-direction:#{direction};flex-wrap:wrap;width:100pt;height:100pt;gap:10pt;align-content:#{alignment}'>
          <div style='width:#{width}pt;height:#{height}pt;flex:none;background:red'></div>
          <div style='width:#{width}pt;height:#{height}pt;flex:none;background:blue'></div>
        </div>
        """)

      for {item, index} <- Enum.with_index(items) do
        cross = if row, do: 300 - item.y - item.height, else: item.x
        assert_in_delta cross, offset + 30 * index, 0.75 / 64
      end
    end
  end

  defp boxes(html) do
    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {300, 300}, margin: 0)

    Enum.filter(layout.boxes, fn box ->
      box.type == :rect and Map.get(box, :fill_color) in [{1, 0, 0}, {0, 0, 1}]
    end)
  end
end
