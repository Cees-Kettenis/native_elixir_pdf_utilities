defmodule NativeElixirPdfUtilities.HtmlToPdf.EmptyCellsTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  # CSS 2.2 sections 17.5.1 and 17.6.1.1 require empty cells to be
  # transparent through every table layer, and all-hidden empty rows to collapse.
  test "hidden empty cells expose the table background through row and group layers" do
    layout =
      layout("""
      <style>td{padding:0;border:1pt solid black;background:red;width:20pt;height:20pt}</style>
      <table style='empty-cells:hide;border-spacing:0;background:yellow'>
        <tbody style='background:blue'><tr style='background:lime'><td> </td><td>Text</td></tr></tbody>
      </table>
      """)

    cells = Enum.filter(layout.boxes, &(Map.get(&1, :role) == :table_cell_background))
    assert length(cells) == 1
    [cell] = cells
    assert cell.x > 20

    for box <- layout.boxes,
        Map.get(box, :role) in [:table_row_background, :table_row_group_background] do
      assert box.clip.x >= cell.x
    end
  end

  test "all-hidden empty rows remove their height and one adjoining spacing band" do
    layout =
      layout("""
      <style>td{padding:0;border:0;line-height:10pt;font-size:10pt;width:20pt}</style>
      <table style='empty-cells:hide;border-spacing:3pt'>
        <tr style='height:20pt'><td style='background:red'>A</td></tr>
        <tr style='height:30pt'><td></td></tr>
        <tr style='height:20pt'><td style='background:blue'>B</td></tr>
      </table><div style='height:10pt;background:lime'></div>
      """)

    a = Enum.find(layout.boxes, &(Map.get(&1, :text) == "A"))
    b = Enum.find(layout.boxes, &(Map.get(&1, :text) == "B"))
    assert_in_delta a.y - b.y, 23, 0.75 / 64
    following = Enum.find(layout.boxes, &(Map.get(&1, :fill_color) == {0, 1, 0}))
    assert_in_delta following.y + following.height, 251, 0.75 / 64
  end

  test "empty-cells preserves in-flow empty elements and is ignored by collapsed borders" do
    for {collapse, content} <- [{"separate", "<span></span>"}, {"collapse", ""}] do
      layout =
        layout("""
        <table style='border-collapse:#{collapse};empty-cells:hide'>
          <tr><td style='height:20pt;width:20pt;background:red;border:1pt solid black'>#{content}</td></tr>
        </table>
        """)

      assert Enum.any?(layout.boxes, &(Map.get(&1, :role) == :table_cell_background))
    end
  end

  test "visible spanning cells retain the allocation of a row with otherwise hidden empty cells" do
    tree =
      layout(
        "<table style='empty-cells:hide;border-spacing:0'><tr style='height:20pt'><td rowspan='2' style='background:red'>Span</td><td></td></tr><tr style='height:20pt'><td></td></tr></table>"
      )

    cells = Enum.filter(tree.boxes, &(Map.get(&1, :role) == :table_cell_background))
    assert_in_delta Enum.sum(Enum.map(cells, & &1.height)), 40, 0.02
  end

  defp layout(html) do
    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {300, 300}, margin: 0)
    layout
  end
end
