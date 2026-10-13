defmodule NativeElixirPdfUtilities.HtmlToPdf.TableZeroEdgeGridTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlTableValidator

  test "an all-none collapsed grid preserves row dimensions backgrounds and scoped cell semantics" do
    html =
      "<table style='width:120pt;height:60pt;border-collapse:collapse;background:red'><tbody style='background:blue'><tr><th id=h scope=col style='border-width:12pt;border-style:none'>Header</th><td headers=h>Value</td></tr><tr><td colspan=2>Spanning</td></tr></tbody></table>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    [table] = styled.children

    assert {:ok, %{collapsed_edges: edges, rows: [first, second]}} =
             HtmlTableValidator.form(table.children, table.style)

    assert edges == %{}
    [heading, value] = first.cells
    assert value.cell.style._table_cell.headers == [heading.cell.style._table_cell.source]
    assert hd(second.cells).colspan == 2
    assert {:ok, %{boxes: boxes}} = Layout.layout(styled, page_size: {240, 240}, margin: 0)
    refute Enum.any?(boxes, &(Map.get(&1, :role) in [:table_border, :table_outline]))
    table_background = Enum.find(boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
    assert table_background.width == 120.0 and table_background.height == 60.0
    assert Enum.any?(boxes, &(Map.get(&1, :role) == :table_row_group_background))
    assert Enum.any?(boxes, &(Map.get(&1, :text) == "Spanning" and &1.table_cell.colspan == 2))
  end

  test "hidden suppression remains explicit and column candidates still resolve against none cells" do
    for {css, expected_style} <- [
          {"border:12pt hidden", :hidden},
          {"border-left:1px solid red", :solid}
        ] do
      html =
        "<table style='width:120pt;border-collapse:collapse'><colgroup><col style='width:50%;#{css}'><col style='width:50%'></colgroup><tr><td></td><td></td></tr></table>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      [table] = styled.children

      assert {:ok, %{collapsed_edges: edges}} =
               HtmlTableValidator.form(table.children, table.style)

      assert map_size(edges) > 0
      assert edges[{:vertical, 0, 0}].style == expected_style
      assert edges[{:vertical, 0, 0}].width == if(expected_style == :hidden, do: 0.0, else: 0.75)
    end
  end
end
