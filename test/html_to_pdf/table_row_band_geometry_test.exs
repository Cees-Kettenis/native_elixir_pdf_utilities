defmodule NativeElixirPdfUtilities.HtmlToPdf.TableRowBandGeometryTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlTableValidator
  alias NativeElixirPdfUtilities.Limits

  setup do
    {:ok, dom} =
      HtmlParser.parse_detailed(
        "<table><tr><td rowspan='3'>Span</td><td>First</td></tr><tr><td>Second</td></tr><tr><td>Third</td></tr></table>"
      )

    {:ok, %{children: [table]}} = Style.compute_detailed(dom)
    {:ok, %{rows: rows}} = HtmlTableValidator.form(table.children, table.style)
    {:ok, rows: rows}
  end

  test "actual canonical rowspan receives each measured row and internal spacing", %{rows: rows} do
    assert {:ok, %{{0, 0} => bands}} =
             HtmlTableValidator.prepare_row_band_geometry(rows, [10.0, 20.0, 30.0], 0.5)

    assert bands == [
             %{row: 0, offset: 0.0, height: 10.5},
             %{row: 1, offset: 10.5, height: 20.5},
             %{row: 2, offset: 31.0, height: 30.0}
           ]

    assert Enum.sum(Enum.map(bands, & &1.height)) == 61.0

    assert {:ok, %{{0, 0} => zero_bands}} =
             HtmlTableValidator.prepare_row_band_geometry(rows, [0.0, 0.0, 0.0], 0.0)

    assert Enum.all?(zero_bands, &(&1.height == 0.0))
  end

  test "malformed measured grids retain actionable shared diagnostics", %{rows: rows} do
    [first | rest] = rows
    [span | cells] = first.cells
    overrun = [%{first | cells: [%{span | rowspan: 4} | cells]} | rest]

    for {input_rows, heights, spacing} <- [
          {rows, [10.0], 0.0},
          {rows, [10.0, -1.0, 20.0], 0.0},
          {rows, [10.0, 20.0, 30.0], :bad},
          {overrun, [10.0, 20.0, 30.0], 0.0},
          {rows, [10.0 | :bad], 0.0}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               HtmlTableValidator.prepare_row_band_geometry(input_rows, heights, spacing)

      assert diagnostic.stage == :layout
      assert diagnostic.reason == :invalid_layout
      assert diagnostic.module == HtmlTableValidator
      assert diagnostic.operation == :prepare_row_band_geometry
      assert diagnostic.message =~ "recompute table geometry"
    end
  end

  test "grid quota rejects canonical span plans before band allocation", %{rows: rows} do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_table_grid_work: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlTableValidator.prepare_row_band_geometry(rows, [10.0, 20.0, 30.0], 0.0)

    assert diagnostic.stage == :layout
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.HtmlValidator
    assert diagnostic.message =~ "max_table_grid_work"
  end
end
