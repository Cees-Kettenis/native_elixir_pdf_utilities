defmodule NativeElixirPdfUtilities.Validators.HtmlTableValidatorTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.Validators.{HtmlTableValidator, HtmlValidator}

  test "zero rowspans include empty rows and end before the next group" do
    children = [
      group([row([cell(%{rowspan: 0}), cell()]), row([]), row([cell()])]),
      group([row([cell()])])
    ]

    assert {:ok, %{rows: [first, empty, last, next], columns: []}} =
             HtmlTableValidator.form(children)

    assert Enum.map(first.cells, &{&1.column, &1.rowspan}) == [{0, 3}, {1, 1}]
    assert empty.cells == [] and empty.active_columns == [0]
    assert [%{column: 1}] = last.cells
    assert [%{column: 0, rowspan: 1}] = next.cells
    refute first.row_group == next.row_group
  end

  test "empty groups separate direct row runs and hidden rows do not extend visible spans" do
    hidden = %{style: %{display: :none}, children: []}

    assert {:ok, %{rows: [first, last]}} =
             HtmlTableValidator.form([
               row([cell(%{rowspan: 0})]),
               group([]),
               row([cell()]),
               hidden
             ])

    assert hd(first.cells).rowspan == 1
    assert hd(last.cells).column == 0

    assert {:ok, %{rows: [first, last]}} =
             HtmlTableValidator.form([
               group([row([cell(%{rowspan: 0})]), hidden, row([hidden, cell()])])
             ])

    assert hd(first.cells).rowspan == 2
    assert hd(last.cells).column == 1
  end

  test "spans skip covered starting slots but overlapping colspan ranges diagnose their slot" do
    assert {:ok, %{rows: [_first, second]}} =
             HtmlTableValidator.form([
               group([row([cell(%{rowspan: 2, colspan: 2})]), row([cell()])])
             ])

    assert [%{column: 2}] = second.cells

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlTableValidator.form([
               group([row([cell(), cell(%{rowspan: 2})]), row([cell(%{colspan: 2})])])
             ])

    assert %{
             stage: :layout,
             reason: :invalid_layout,
             operation: :layout,
             module: HtmlTableValidator,
             source: "td"
           } = diagnostic

    assert diagnostic.message =~ "row 2, column 1"
    assert diagnostic.message =~ "column 2"
    assert diagnostic.message =~ "reduce the spans"
  end

  test "visible column hints normalize spans and inherit the group width" do
    column = %{style: %{display: :table_column, span: 2, width: 12.0}, children: []}
    hidden = %{style: %{display: :none}, children: []}

    group = %{
      style: %{display: :table_column_group, span: 3, width: 20.0},
      children: [column, hidden, %{column | style: %{display: :table_column}}]
    }

    assert {:ok, %{columns: [%{width: 12.0}, %{width: 12.0}, %{width: 20.0}]}} =
             HtmlTableValidator.form([group])

    assert {:ok, %{columns: columns}} = HtmlTableValidator.form([%{group | children: [hidden]}])
    assert columns == List.duplicate(%{width: 20.0}, 3)

    assert {:ok, %{columns: columns}} =
             HtmlTableValidator.form([
               %{group | children: [], style: %{display: :table_column_group, span: 1_001}}
             ])

    assert length(columns) == 1_000
  end

  test "table formation returns shared actionable diagnostics for invalid styled structures" do
    column_group = %{style: %{display: :table_column_group}, children: []}

    for invalid <- [
          nil,
          [nil],
          [%{}],
          [row([]) |> Map.put(:children, :bad)],
          [group([row([])]) |> Map.put(:children, :bad)],
          [
            group([row([])])
            |> Map.put(:style, %{display: :table_row_group, table_section: :invalid})
          ],
          [row([cell() |> Map.put(:children, :bad)])],
          [group([row([cell() |> Map.put(:children, :bad)])])],
          [%{column_group | style: %{display: :table_column_group, span: :bad}}],
          [row([cell(%{rowspan: -1})])],
          [row([cell(%{colspan: :bad})])]
        ] do
      assert {:error, {:invalid_layout, diagnostic}} = HtmlTableValidator.form(invalid)

      assert %{
               stage: :layout,
               reason: :invalid_layout,
               operation: :layout,
               module: HtmlTableValidator
             } = diagnostic

      assert is_binary(diagnostic.source) and diagnostic.message != ""
    end
  end

  test "shared HTML span parsing applies recovery defaults ASCII prefixes and protocol column bounds" do
    for {name, value, expected} <- [
          {"rowspan", "0", 0},
          {"rowspan", "-0", 1},
          {"rowspan", " -2", 1},
          {"colspan", "0", 1},
          {"span", "bad", 1},
          {"span", "\u00A02", 1},
          {"colspan", " +02 trailing", 2},
          {"span", "1001", 1_000}
        ] do
      assert HtmlValidator.validate_table_span_attribute(%{name => value}, name) ==
               {:ok, expected}
    end

    assert HtmlValidator.normalize_table_span("span", 0) == {:ok, 1}
    assert HtmlValidator.normalize_table_span("rowspan", 0) == {:ok, 0}
    assert HtmlValidator.normalize_table_span("bad", 1) == :error
    assert HtmlValidator.normalize_table_span("span", -1) == :error
    assert HtmlValidator.normalize_table_span("span", "1") == :error

    assert {:ok, %{rows: [%{cells: [placement]}]}} =
             HtmlTableValidator.form([row([cell(%{colspan: 1_001})])])

    assert placement.colspan == 1_000 and placement.cell.style.colspan == 1_000
    assert {:ok, %{rows: []}} = HtmlTableValidator.form([])

    assert {:ok, %{rows: [%{consumed_columns: 0, cells: []}]}} =
             HtmlTableValidator.form([row([])])
  end

  defp cell(extra \\ %{}) do
    %{tag: "td", type: :element, children: [], style: Map.merge(%{display: :table_cell}, extra)}
  end

  defp row(cells) do
    %{tag: "tr", type: :element, children: cells, style: %{display: :table_row}}
  end

  defp group(rows) do
    %{tag: "tbody", type: :element, children: rows, style: %{display: :table_row_group}}
  end
end
