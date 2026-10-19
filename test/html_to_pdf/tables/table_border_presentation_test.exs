defmodule NativeElixirPdfUtilities.HtmlToPdf.TableBorderPresentationTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, PdfWriter, Style}
  alias NativeElixirPdfUtilities.Validators.{CssValueValidator, HtmlTableValidator}

  test "magic borders and frame rules enter below author CSS and keep nearest table ownership" do
    html = """
    <table border=bad><tr><td>Default</td></tr></table>
    <table border=0><tr><td>Zero</td></tr></table>
    <table border=4 bordercolor=red rules=ALL frame=HSIDES style='border-left:2pt solid blue'><tr><td style='border:3pt dashed green'>CSS</td></tr></table>
    """

    styled = styled(html)
    [default, zero, overridden] = styled.children
    [default_row] = default.children
    [default_cell] = default_row.children
    assert default.style.border_widths.top == 0.75
    assert default.style.border_styles.top == :outset
    assert default.style.border_colors.top == {0, 0, 0}
    assert default_cell.style.border_styles.top == :inset
    assert default_cell.style.border_colors == default.style.border_colors
    assert zero.style.border_widths.top == 0.0
    assert overridden.style.border_collapse == :collapse
    assert overridden.style.border_widths.top == 0.75
    assert overridden.style.border_styles.right == :hidden
    assert overridden.style.border_colors.top == {1, 0, 0}
    assert overridden.style.border_colors.left == {0, 0, 1}
    [row] = overridden.children
    [cell] = row.children
    assert cell.style.border_styles.top == :dashed
    assert_in_delta elem(cell.style.border_colors.top, 1), 128 / 255, 1.0e-9

    assert {:ok, %{border_styles: %{top: :none}}} =
             HtmlTableValidator.presentation_hints(
               %{tag: "table", attributes: %{"border" => "0", "rules" => "invalid"}},
               [],
               %{}
             )
  end

  test "authored separated borders keep legacy cell rules and cell CSS overrides" do
    for {rules, expected_top, expected_left} <- [
          {"rows", :solid, :none},
          {"cols", :none, :solid},
          {"all", :solid, :solid}
        ] do
      styled =
        styled(
          "<table rules=#{rules} style='border-collapse:separate'><tr><td style='height:18pt'>Rules</td><td style='border:none'>Override</td></tr></table>"
        )

      [table] = styled.children
      [row] = table.children
      [cell, override] = row.children
      assert table.style.border_collapse == :separate
      assert cell.style.border_styles.top == expected_top
      assert cell.style.border_styles.left == expected_left
      assert override.style.border_widths == CssValueValidator.edges(0.0)
      assert {:ok, %{boxes: boxes}} = Layout.layout(styled)

      assert Enum.any?(
               boxes,
               &(Map.get(&1, :role) == :table_cell_background and &1.stroke_width > 0)
             )
    end
  end

  test "hidden wins unconditionally and none loses to a visible neighbor" do
    grid =
      grid(
        "<table style='border-collapse:collapse'><tr><td style='border-right:12pt double red'></td><td style='border-left:0 hidden blue'></td></tr><tr><td style='border-right:0 none red'></td><td style='border-left:1px dotted blue'></td></tr></table>"
      )

    assert %{style: :hidden, width: +0.0, color: {0, 0, 1}} =
             grid.collapsed_edges[{:vertical, 0, 1}]

    assert %{style: :dotted, width: 0.75, color: {0, 0, 1}} =
             grid.collapsed_edges[{:vertical, 1, 1}]
  end

  test "width precedes style and equal width uses CSS style priority then top and left ties" do
    for {left, right, expected_style, expected_color} <- [
          {"2pt double red", "4pt dotted blue", :dotted, {0, 0, 1}},
          {"3pt solid red", "3pt double blue", :double, {0, 0, 1}},
          {"3pt solid red", "3pt solid blue", :solid, {1, 0, 0}}
        ] do
      grid =
        grid(
          "<table style='border-collapse:collapse'><tr><td style='border-right:#{left}'></td><td style='border-left:#{right}'></td></tr></table>"
        )

      assert %{style: ^expected_style, color: ^expected_color} =
               grid.collapsed_edges[{:vertical, 0, 1}]
    end

    grid =
      grid(
        "<table style='border-collapse:collapse'><tr><td style='border-bottom:3pt solid red'></td></tr><tr><td style='border-top:3pt solid blue'></td></tr></table>"
      )

    assert grid.collapsed_edges[{:horizontal, 1, 0}].color == {1, 0, 0}
  end

  test "origin ties prefer cells rows groups columns column groups then table" do
    html =
      "<table style='border-collapse:collapse;border:2pt solid black'><colgroup style='border:2pt solid blue'><col style='border:2pt solid green'></colgroup><tbody style='border:2pt solid blue'><tr style='border:2pt solid red'><td style='border:2pt solid white'></td></tr></tbody></table>"

    grid = grid(html)
    assert grid.columns == [%{width: nil}]
    assert grid.collapsed_edges[{:horizontal, 0, 0}].color == {1, 1, 1}
    grid = grid(String.replace(html, "border:2pt solid white", "border:0 none"))
    assert grid.collapsed_edges[{:horizontal, 0, 0}].color == {1, 0, 0}

    grid =
      grid(
        String.replace(html, "border:2pt solid white", "border:0 none")
        |> String.replace("border:2pt solid red", "border:0 none")
      )

    assert grid.collapsed_edges[{:horizontal, 0, 0}].color == {0, 0, 1}

    grid =
      grid(
        String.replace(html, "border:2pt solid white", "border:0 none")
        |> String.replace("border:2pt solid red", "border:0 none")
        |> String.replace("<tbody style='border:2pt solid blue'>", "<tbody>")
      )

    assert_in_delta elem(grid.collapsed_edges[{:horizontal, 0, 0}].color, 1), 128 / 255, 1.0e-9
  end

  test "spanning cells retain partial winning edges and row rules do not cut their interior" do
    html =
      "<table style='width:120pt;table-layout:fixed;border-collapse:collapse'><colgroup><col style='width:50%'><col style='width:50%'></colgroup><tbody><tr style='border-bottom:1pt solid green'><td colspan=2 style='height:18pt;border-bottom:3pt solid red'></td></tr><tr><td style='height:18pt;border-top:2pt solid blue'></td><td style='border-top:4pt solid blue'></td></tr></tbody></table>"

    styled = styled(html)
    [table] = styled.children
    assert {:ok, grid} = HtmlTableValidator.form(table.children, table.style)
    assert grid.collapsed_edges[{:horizontal, 1, 0}].color == {1, 0, 0}
    assert grid.collapsed_edges[{:horizontal, 1, 1}].color == {0, 0, 1}
    assert {:ok, %{boxes: boxes}} = Layout.layout(styled, page_size: {240, 240}, margin: 0)

    red =
      Enum.find(
        boxes,
        &(Map.get(&1, :role) == :table_border and
            &1.border_colors.bottom == {1, 0, 0} and &1.border_widths.bottom > 0)
      )

    assert red.clip.width == red.width / 2 and red.clip.x == red.x
    assert red.table_cell.colspan == 2

    partial =
      styled(
        "<table style='width:120pt;table-layout:fixed;border-collapse:collapse'><colgroup><col style='width:50%;border-top:2pt solid green'><col style='width:50%;border-top:4pt solid blue'></colgroup><tr><td colspan=2 style='height:18pt;border-top:3pt solid red'></td></tr></table>"
      )

    assert {:ok, %{boxes: boxes}} = Layout.layout(partial, page_size: {240, 240}, margin: 0)

    red =
      Enum.find(
        boxes,
        &(Map.get(&1, :role) == :table_border and &1.border_colors.top == {1, 0, 0})
      )

    assert red.clip.width == red.width / 2 and red.clip.x == red.x
    assert red.table_cell.colspan == 2

    grid =
      grid(
        "<table rules=rows><tbody><tr><td rowspan=2></td><td></td></tr><tr><td></td></tr></tbody></table>"
      )

    refute Map.has_key?(grid.collapsed_edges, {:horizontal, 1, 0})
    assert grid.collapsed_edges[{:horizontal, 1, 1}].style == :solid
  end

  test "hidden cells and rows do not contribute border candidates and internal metadata is rejected" do
    grid =
      grid(
        "<table rules=all><tr style='display:none'><td style='border:12pt double red'></td></tr><tr><td style='display:none;border:12pt double red'></td><td></td></tr></table>"
      )

    assert length(grid.rows) == 1 and length(hd(grid.rows).cells) == 1
    assert Enum.all?(grid.collapsed_edges, fn {_key, candidate} -> candidate.width == 0.0 end)
    styled = styled("<table><tr><td>Cell</td></tr></table>")

    for key <- [:_collapsed_border_segments, :_collapsed_border_winners] do
      invalid =
        put_in(styled.children, [
          %{hd(styled.children) | style: Map.put(hd(styled.children).style, key, %{})}
        ])

      assert {:error, {:invalid_layout, diagnostic}} =
               Layout.layout(invalid)

      assert diagnostic.stage == :layout and diagnostic.reason == :invalid_layout
    end
  end

  test "table-part borders diagnose uncovered slots while sparse table and cell borders still render" do
    html =
      "<table style='width:120pt;table-layout:fixed;border-collapse:collapse;border:2pt solid red'><colgroup><col><col></colgroup><tr><td style='height:18pt;border:1px solid black'>Cell</td></tr></table>"

    assert {:ok, _layout} = Layout.layout(styled(html))

    for table_part <- ["<tr style='border-bottom:1px solid black'>", "<tr>"] do
      html =
        if table_part == "<tr>",
          do:
            String.replace(html, "<colgroup>", "<colgroup style='border-right:1px solid black'>"),
          else: String.replace(html, "<tr>", table_part)

      assert {:error, {:unsupported_html, diagnostic}} = Layout.layout(styled(html))
      assert diagnostic.stage == :layout and diagnostic.reason == :unsupported_html
      assert diagnostic.module == HtmlTableValidator and diagnostic.operation == :layout
      assert diagnostic.source == "tr uncovered table slots"

      assert diagnostic.message =~ "anonymous cell formation" and
               diagnostic.message =~ "authored cells"
    end
  end

  test "table relief gives dark borders two source-defined contrasting shades" do
    for role <- [:table_cell_background, :table_separate_background, :table_border] do
      box = %{
        type: :rect,
        role: role,
        x: 12.0,
        y: 20.0,
        width: 40.0,
        height: 24.0,
        fill_color: nil,
        stroke_color: {0, 0, 0},
        stroke_width: 0.75,
        border_widths: CssValueValidator.edges(0.75),
        border_colors: CssValueValidator.edges({0, 0, 0}),
        border_styles: CssValueValidator.edges(:inset),
        border_radius: 0.0
      }

      assert {:ok, pdf} = PdfWriter.render([%{size: {120, 120}, boxes: [box]}], [])
      assert pdf =~ "0.3294 0.3294 0.3294 rg"
      assert pdf =~ "0.6588 0.6588 0.6588 rg"
    end
  end

  defp styled(html) do
    {:ok, dom} = HtmlParser.parse_detailed(html)
    {:ok, styled} = Style.compute_detailed(dom)
    styled
  end

  defp grid(html) do
    [table] = styled(html).children
    {:ok, grid} = HtmlTableValidator.form(table.children, table.style)
    grid
  end
end
