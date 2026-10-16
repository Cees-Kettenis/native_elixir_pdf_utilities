defmodule NativeElixirPdfUtilities.Validators.HtmlTableModelTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlTableValidator, HtmlValidator}

  test "positive spans imply logical rows independently of clipped CSS geometry" do
    html =
      "<table><tbody><tr><td rowspan=3>A</td><td>B</td></tr></tbody><tbody><tr><td>C</td></tr></tbody></table>"

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    [table] = styled.children
    model = table.style._table_model
    assert {model.width, model.height} == {2, 4}
    assert Enum.map(model.rows, &is_nil(&1.source)) == [false, true, true, false]

    assert Enum.map(model.cells, &{&1.row, &1.column, &1.rowspan}) == [
             {0, 0, 3},
             {0, 1, 1},
             {3, 0, 1}
           ]

    assert model.uncovered_slots == [{1, 1}, {1, 2}, {1, 3}]
    assert Enum.any?(model.errors, &String.contains?(&1.message, "row 2 has no anchored cell"))

    assert Enum.all?(
             model.errors,
             &(&1.stage == :style and &1.reason == :invalid_document and
                 &1.operation == :table_model and &1.module == HtmlTableValidator)
           )

    assert {:ok, %{rows: [first, second]}} = HtmlTableValidator.form(table.children)
    assert hd(first.cells).rowspan == 1
    assert hd(second.cells).column == 0
    assert {:ok, %{boxes: boxes}} = Layout.layout(styled)
    assert Enum.any?(boxes, &match?(%{table_cell: %{rowspan: 3}}, &1))
    assert {:ok, other_dom} = HtmlParser.parse(String.replace(html, "rowspan=3", "rowspan=1"))
    assert {:ok, other_styled} = Style.compute_detailed(other_dom)
    assert {:ok, %{boxes: other_boxes}} = Layout.layout(other_styled)

    assert Enum.map(boxes, &Map.delete(&1, :table_cell)) ==
             Enum.map(other_boxes, &Map.delete(&1, :table_cell))

    assert {:ok, _pdf} = HtmlToPdf.render(html)
  end

  test "zero spans include positive implied rows and remain inside their logical row group" do
    [table] =
      prepared(
        "<table><tbody><tr><td rowspan=0>Z</td><td rowspan=4>P</td></tr><tr><td>N</td></tr></tbody><tbody><tr><td>Next</td></tr></tbody></table>"
      )

    model = table._table_model

    assert Enum.map(model.cells, &{&1.row, &1.column, &1.rowspan}) == [
             {0, 0, 4},
             {0, 1, 4},
             {1, 2, 1},
             {4, 0, 1}
           ]

    assert Enum.map(model.row_groups, & &1.rowspan) == [4, 1]
  end

  test "direct row runs end at empty groups and deferred footers follow body rows" do
    [table] =
      prepared(
        "<table><tr><td rowspan=0>Direct</td></tr><tbody></tbody><tr><td>Following</td></tr><tbody><tr><td>Body</td></tr></tbody><tfoot><tr><td>Foot</td></tr></tfoot></table>"
      )

    [direct, following, body, foot] = table._table_model.cells
    assert direct.rowspan == 1 and direct.row_group == nil
    assert following.row == 1
    assert body.row == 2 and foot.row == 3
    assert body.row_group != foot.row_group
  end

  test "empty tables have no slots and declared columns retain anchor diagnostics" do
    for html <- ["<table></table>", "<table><tbody></tbody></table>", "<table><tr></tr></table>"] do
      [table] = prepared(html)
      assert table._table_model.width == 0
      assert table._table_model.uncovered_slots == []
      assert table._table_model.errors == []
    end

    [table] =
      prepared(
        "<table><colgroup span=3></colgroup><colgroup><col span=2></colgroup><tr><td colspan=2>A</td><td>B</td></tr></table>"
      )

    assert table._table_model.width == 5

    assert Enum.map(table._table_model.column_groups, &{&1.column, &1.colspan}) == [
             {0, 3},
             {3, 2}
           ]

    assert table._table_model.uncovered_slots == [{3, 0}, {4, 0}]

    assert Enum.any?(
             table._table_model.errors,
             &String.contains?(&1.message, "column 2 has no anchored cell")
           )
  end

  test "logical overlap retains actual slot diagnostics even for display none source cells" do
    html =
      "<table><tr><td>A</td><td rowspan=2 style='display:none'>Hidden</td></tr><tr><td colspan=2>B</td></tr></table>"

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    [table] = styled.children

    assert Enum.any?(
             table.style._table_model.errors,
             &String.contains?(&1.message, "row 2, column 2: overlapping source cells")
           )

    assert {:ok, _pdf} = HtmlToPdf.render(html)
  end

  test "explicit headers uses first document ID and accepts td while removing self duplicates empty and foreign table cells" do
    html = """
    <p id=blocked hidden>First</p>
    <table><tr><th id=outside>Outside</th></tr></table>
    <table><tr><th id=blocked>Blocked</th><th id=h abbr='Short'>Header</th><td id=d>Data header</td><th id=empty> \t\r\n\f </th><th id=element><span></span></th><th id=nbsp>&nbsp;</th></tr>
    <tr><td id=self headers='h h d self missing empty blocked outside element nbsp'>Value</td><td headers=''>None</td></tr></table>
    """

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    [_, _, table] = styled.children
    cells = Map.new(table.style._table_model.cells, &{&1.id, &1})
    assert cells["self"].headers == Enum.map(~w(h d element nbsp), &cells[&1].source)
    assert cells["h"].abbr == "Short"
    assert cells["empty"].empty
    refute cells["element"].empty
    refute cells["nbsp"].empty
    assert List.last(table.style._table_model.cells).headers == []
    assert {:ok, _pdf} = HtmlToPdf.render(html)
  end

  test "automatic scope scans rows and columns and recovers invalid values to Auto" do
    [table] =
      prepared(
        "<table><tr><th id=a scope=invalid>A</th><th id=b scope=COL>B</th></tr><tr><th id=r scope=ROW>R</th><td id=x>X</td></tr></table>"
      )

    cells = Map.new(table._table_model.cells, &{&1.id, &1})
    assert cells["a"].scope == :auto
    assert cells["x"].headers == [cells["r"].source, cells["b"].source]
    assert cells["r"].headers == [cells["a"].source]
  end

  test "Auto row headers require a data-free column and group headers respect anchored group and both bounds" do
    [table] =
      prepared("""
      <table><colgroup span=2></colgroup><colgroup span=2></colgroup>
      <thead><tr><th id=c scope=colgroup colspan=2>C</th><th id=other scope=colgroup colspan=2>Other</th></tr></thead>
      <tbody><tr><th id=g scope=rowgroup>G</th><td id=a>A</td><td id=b>B</td><td id=cross>C</td></tr><tr><th id=r>R</th><td id=x>X</td><td>Y</td><td>Z</td></tr></tbody>
      <tbody><tr><th id=n>N</th><td id=next>Next</td><td>Other</td><td>Last</td></tr></tbody></table>
      """)

    cells = Map.new(table._table_model.cells, &{&1.id, &1})
    assert cells["x"].headers == [cells["r"].source, cells["c"].source, cells["g"].source]
    assert cells["b"].headers == [cells["other"].source, cells["g"].source]
    assert cells["next"].headers == [cells["n"].source, cells["c"].source]
    refute cells["other"].source in cells["a"].headers
  end

  test "opaque header blocks stop earlier headers with equal spans and scan across vacant slots" do
    [table] =
      prepared(
        "<table><tr><th id=top scope=col>Top</th></tr><tr><td>Barrier</td></tr><tr><th id=near scope=col>Near</th></tr><tr><td id=value>Value</td></tr></table>"
      )

    cells = Map.new(table._table_model.cells, &{&1.id, &1})
    assert cells["value"].headers == [cells["near"].source]
    assert cells["near"].headers == []

    [table] =
      prepared(
        "<table><tr><th id=h scope=row>Header</th><td colspan=2>Barrier</td><th id=n scope=row>Near</th><td id=x>X</td></tr><tr><td colspan=2>Gap</td></tr></table>"
      )

    cells = Map.new(table._table_model.cells, &{&1.id, &1})
    assert cells["x"].headers == [cells["n"].source]
  end

  test "first ID excludes inert template content but includes its element and duplicate hidden source nodes" do
    nodes =
      prepared(
        "<template id=template><span id=h>Inert</span><table><tr><th id=inside>Inside</th></tr></table></template><div id=dupe hidden>First</div><table><tr><th id=h>Header</th><th id=dupe>Duplicate</th><td headers='h dupe template inside'>Data</td></tr></table>"
      )

    [template, _, table] = nodes
    refute Map.has_key?(template, :_table_model)
    [header, _, value] = table._table_model.cells
    assert value.headers == [header.source]
  end

  test "canonical identity is preserved while path locators and tree scopes remain separate" do
    assert {:ok, outside_dom} = HtmlParser.parse("<p id=h>Different scope</p>")

    assert {:ok, table_dom} =
             HtmlParser.parse(
               "<table><tr><th id=h>Header</th><td headers=h>Data</td></tr></table>"
             )

    [outside] = outside_dom.children
    [table] = table_dom.children
    [row] = table.children
    [authored_header, _] = row.children
    nodes = HtmlValidator.assign_render_metadata([outside, table])
    assert {:ok, [_, table]} = HtmlTableValidator.prepare_document(nodes)
    [header, value] = table._table_model.cells

    assert header.identity ==
             Map.take(authored_header, [:document_id, :tree_scope_id, :node_id, :parent_id])

    assert header.source_metadata == authored_header.source
    refute header.identity.tree_scope_id == outside.tree_scope_id
    assert value.headers == [header.source]
    assert header.source == [1, 0, 0]
  end

  test "caller supplied styled table boxes may omit logical source metadata" do
    assert {:ok, dom} = HtmlParser.parse("<table><tr><td>Legacy cell</td></tr></table>")
    assert {:ok, styled} = Style.compute_detailed(dom)
    [table] = styled.children
    [row] = table.children
    [cell] = row.children
    cell = %{cell | style: Map.delete(cell.style, :_table_cell)}
    row = %{row | children: [cell]}
    table = %{table | style: Map.delete(table.style, :_table_model), children: [row]}
    assert {:ok, %{boxes: boxes}} = Layout.layout(%{styled | children: [table]})
    refute Enum.any?(boxes, &Map.has_key?(&1, :table_cell))
  end

  test "namespaced IDs do not block an authored unnamespaced ID and comment-only cells are empty" do
    assert {:ok, dom} =
             HtmlParser.parse(
               "<span id=h>Namespaced</span><table><tr><th id=h>Header</th><td headers=h>Data</td><th id=empty></th></tr></table>"
             )

    [outside, table] = dom.children

    outside =
      Map.put(outside, :attribute_namespaces, %{"id" => "http://www.w3.org/XML/1998/namespace"})

    [row] = table.children
    [header, data, empty] = row.children
    empty = %{empty | children: [%{type: :comment, text: "inert comment"}]}
    table = %{table | children: [%{row | children: [header, data, empty]}]}

    assert {:ok, [_, table]} =
             HtmlTableValidator.prepare_document(
               HtmlValidator.assign_render_metadata([outside, table])
             )

    [header, data, empty] = table._table_model.cells
    assert data.headers == [header.source]
    assert empty.empty
    foreign = %{type: :element, tag: "table", namespace: :svg, attributes: %{}, children: []}
    assert HtmlTableValidator.prepare_document([foreign]) == {:ok, [foreign]}
  end

  test "header scans skip vacant slots and ambiguous overlapped slots in the recovered logical model" do
    [table] =
      prepared(
        "<table><tr><th id=h scope=col>H</th><td>A</td></tr><tr></tr><tr><td id=gap>Gap</td></tr></table>"
      )

    cells = Map.new(table._table_model.cells, &{&1.id, &1})
    assert cells["gap"].headers == [cells["h"].source]

    [table] =
      prepared(
        "<table><tr><th id=h scope=col>H</th><th id=span scope=col rowspan=2>S</th></tr><tr><th colspan=2 scope=col id=over>Over</th></tr><tr><td id=a>A</td><td id=b>B</td></tr></table>"
      )

    cells = Map.new(table._table_model.cells, &{&1.id, &1})
    assert cells["b"].headers == [cells["span"].source]
    assert cells["a"].headers == [cells["over"].source, cells["h"].source]

    # Long data-only prefixes on either scan axis cannot hide a header.
    for axis <- [:column, :row] do
      data =
        Enum.map_join(1..80, fn index ->
          cell = "<td id='v#{index}'>Value #{index}</td>"
          if axis == :column, do: "<tr>#{cell}</tr>", else: cell
        end)

      html =
        if axis == :column,
          do: "<table><tr><th id=h scope=col>Header</th></tr>#{data}</table>",
          else: "<table><tr><th id=h scope=row>Header</th>#{data}</tr></table>"

      [table] = prepared(html)
      cells = Map.new(table._table_model.cells, &{&1.id, &1})
      for index <- 1..80, do: assert(cells["v#{index}"].headers == [cells["h"].source])
    end
  end

  test "metadata-free caller DOM retains source locations without issuing canonical IDs" do
    assert {:ok, parsed} = HtmlParser.parse("<table><tr><td rowspan=3>Implied</td></tr></table>")
    dom = NativeElixirPdfUtilities.HtmlTreeHelpers.tree_content(parsed)
    [table] = dom.children
    table = Map.put(table, :source, %{text: "<table>", line: 7, column: 4})
    assert {:ok, styled} = Style.compute_detailed(%{dom | children: [table]})
    model = hd(styled.children).style._table_model
    assert model.identity == %{}
    assert model.source == [0]
    assert model.source_metadata == table.source
    assert Enum.all?(model.errors, &(&1.line == 7 and &1.column == 4 and &1.source == "<table>"))

    assert {:ok, standalone} =
             Style.compute_detailed(%{
               type: :document,
               children: [
                 %{type: :element, tag: "td", attributes: %{"rowspan" => "0"}, children: []}
               ]
             })

    assert hd(standalone.children).style.rowspan == 0
    refute Map.has_key?(hd(standalone.children).style, :_table_cell)
  end

  test "explicit td-only associations remain available when there are no th cells" do
    [table] =
      prepared(
        "<table><tr><td id=h>Data header</td><td id=x headers='h h'>Value</td></tr></table>"
      )

    [header, data] = table._table_model.cells
    assert header.headers == []
    assert data.headers == [header.source]
  end

  defp prepared(html) do
    assert {:ok, dom} = HtmlParser.parse(html)

    assert {:ok, nodes} =
             HtmlTableValidator.prepare_document(
               HtmlValidator.assign_render_metadata(dom.children)
             )

    nodes
  end
end
