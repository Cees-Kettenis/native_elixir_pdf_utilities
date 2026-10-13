defmodule NativeElixirPdfUtilities.HtmlToPdf.TableCellBackgroundGeometryTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}
  alias NativeElixirPdfUtilities.Validators.HtmlTableValidator
  alias NativeElixirPdfUtilities.Limits

  setup do
    {:ok, dom} =
      HtmlParser.parse_detailed(
        "<table><tbody style='background:#f0dcb8'><tr style='background:#eef'><td rowspan='3'>Span</td><td>First</td></tr><tr style='background:#fee'><td>Second</td></tr><tr><td>Third</td></tr></tbody></table>"
      )

    {:ok, %{children: [table]}} = Style.compute_detailed(dom)
    {:ok, %{rows: rows}} = HtmlTableValidator.form(table.children, table.style)
    {:ok, rows: rows}
  end

  test "row coverage belongs to originating cells while group/image positioning retains the actual grouping box",
       %{rows: rows} do
    assert {:ok, geometry} =
             HtmlTableValidator.prepare_cell_background_geometry(
               rows,
               [10.0, 20.0, 30.0],
               [40.0, 60.0],
               {2.0, 0.5}
             )

    [group, row] = geometry[{0, 0}]
    assert group.role == :table_row_group_background
    assert group.position == %{x: 0.0, y: -61.0, width: 106.0, height: 61.0}
    assert group.coverage == %{x: 2.0, y: -61.0, width: 40.0, height: 61.0}
    assert row.position == %{x: 0.0, y: -10.0, width: 106.0, height: 10.0}
    assert row.coverage == group.coverage
    [next_group, next_row] = geometry[{1, 1}]
    assert next_group.position == group.position
    assert next_row.coverage == %{x: 44.0, y: -30.5, width: 60.0, height: 20.0}
    assert next_row.coverage.x >= row.coverage.x + row.coverage.width
  end

  test "actual unfragmented viewports and inline atomic tables retain cell-owned row layers" do
    html =
      "<style>html,body{margin:0}table{width:120pt;border-collapse:collapse}td{height:20pt;padding:2pt;border:1pt solid;vertical-align:top}tbody{background:#f0dcb8}tr:nth-child(2){background:#dce8ef}.span{background:rgba(40,100,180,.35)}</style><table><tbody><tr><td rowspan='3' class='span'>Span</td><td>First</td></tr><tr><td>Second</td></tr><tr><td>Third</td></tr></tbody></table>"

    for mode <- [:viewport, :atomic] do
      source =
        if mode == :atomic,
          do:
            String.replace(html, "<table>", "<div style='display:inline-block'><table>") <>
              "</div>",
          else: html

      assert {:ok, dom} = HtmlParser.parse_document_detailed(source)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)

      assert {:ok, layout} =
               if(mode == :viewport,
                 do: Layout.layout_viewport(styled, 160.0, 140.0),
                 else: Layout.layout(styled, page_size: {160, 140}, margin: 10)
               )

      span =
        Enum.find(
          layout.boxes,
          &(Map.get(&1, :role) == :table_cell_background and
              Map.get(&1, :fill_color) == {40 / 255, 100 / 255, 180 / 255, 0.35})
        )

      row = Enum.find(layout.boxes, &(Map.get(&1, :role) == :table_row_background))
      assert row.x >= span.x + span.width
      assert row.width < 120.0
      assert row.height < span.height
      assert Enum.all?(layout.boxes, &(not Map.has_key?(&1, :table_span_fragment)))
    end
  end

  test "invalid direct measured background inputs diagnose instead of indexing outside the canonical grid",
       %{rows: rows} do
    for {heights, widths, spacing} <- [
          {[10.0], [40.0, 60.0], {0.0, 0.0}},
          {[10.0, 20.0, 30.0], [40.0], {0.0, 0.0}},
          {[10.0, :bad, 30.0], [40.0, 60.0], {0.0, 0.0}},
          {[10.0, 20.0, 30.0], [40.0 | :bad], {0.0, 0.0}},
          {[10.0, 20.0, 30.0], [40.0, 60.0], :bad}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               HtmlTableValidator.prepare_cell_background_geometry(rows, heights, widths, spacing)

      assert diagnostic.stage == :layout
      assert diagnostic.reason == :invalid_layout
      assert diagnostic.module == HtmlTableValidator
      assert diagnostic.operation == :prepare_cell_background_geometry
      assert diagnostic.message =~ "recompute table geometry"
    end
  end

  test "group positioning rejects discontinuous identities and inconsistent prepared group styles",
       %{rows: [first, middle, last]} do
    for rows <- [
          [first, %{middle | row_group: {:implicit, 99}}, last],
          [first, %{middle | group_style: %{}}, last],
          [Map.delete(first, :group_style), middle, last]
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               HtmlTableValidator.prepare_cell_background_geometry(
                 rows,
                 [10.0, 20.0, 30.0],
                 [40.0, 60.0],
                 {0.0, 0.0}
               )

      assert diagnostic.operation == :prepare_cell_background_geometry
      assert diagnostic.message =~ "recompute table geometry"
    end
  end

  test "bounded grid work is reserved before allocating background geometry", %{rows: rows} do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_table_grid_work: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlTableValidator.prepare_cell_background_geometry(
               rows,
               [10.0, 20.0, 30.0],
               [40.0, 60.0],
               {0.0, 0.0}
             )

    assert diagnostic.message =~ "max_table_grid_work"
    assert diagnostic.stage == :layout
  end
end
