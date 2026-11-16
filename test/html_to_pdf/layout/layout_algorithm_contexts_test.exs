defmodule NativeElixirPdfUtilities.HtmlToPdf.LayoutAlgorithmContextsTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Pagination, Style}
  alias NativeElixirPdfUtilities.Validators.{CssGridValidator, HtmlTableValidator}

  test "flex shorthand ordering preserves grow, shrink and intrinsic bases" do
    for {source, basis} <- [
          {"20pt", 20},
          {"2 20pt", 20},
          {"20pt 2", 20},
          {"20pt 2 1", 20},
          {"calc(10pt + 10pt)", 20}
        ] do
      tree =
        layout(
          "<div style='display:flex;width:100pt'><div style='flex:#{source};height:10pt;background:red'></div><div style='flex:none;width:80pt;height:10pt'></div></div>"
        )

      assert_in_delta red(tree).width, basis, 0.02
    end

    {:ok, dom} = HtmlParser.parse("<div style='flex:20pt 30pt'>Invalid</div>")
    assert {:error, :invalid_document} = Style.compute(dom, system_font_discovery: false)
  end

  test "intrinsic grid track functions retain spanning contributions and automatic minimum edges" do
    for tracks <- [
          "min-content",
          "max-content",
          "fit-content(30pt)",
          "minmax(min-content,min-content)",
          "minmax(max-content,1fr)"
        ] do
      tree =
        layout(
          "<div style='display:inline-grid;grid-template-columns:#{tracks}'><div style='width:20pt;height:10pt;background:red'></div></div>"
        )

      assert_in_delta red(tree).width, 20, 0.02
    end

    tree =
      layout(
        "<div style='display:grid;width:80pt;grid-template-columns:fit-content(40pt) fit-content(40pt)'><div style='grid-column:span 2;width:80pt;height:10pt;background:red'></div></div>"
      )

    assert_in_delta red(tree).width, 80, 0.02

    tree =
      layout(
        "<div style='display:grid;width:20pt;grid-template-columns:1fr;grid-auto-columns:10pt 20pt'><div style='min-width:20pt;box-sizing:border-box;height:10pt;background:red'></div></div>"
      )

    assert_in_delta red(tree).width, 20, 0.02
  end

  test "row subgrid descendants contribute their intrinsic block sizes to the parent" do
    tree =
      layout(
        "<div style='display:grid;width:100pt;grid-template-rows:auto auto'><div style='display:grid;grid-row:1 / 3;grid-template-rows:subgrid'><div style='height:10pt;background:red'></div><div style='height:20pt;background:blue'></div></div></div>"
      )

    assert_in_delta red(tree).height, 10, 0.02

    assert Enum.any?(
             tree.boxes,
             &(Map.get(&1, :fill_color) == {0, 0, 1} and abs(&1.height - 20) < 0.02)
           )
  end

  test "physical and self alignment resolve opposite grid directions before safe fallback" do
    for {justify, expected} <- [{"left", 0}, {"right", 80}] do
      tree =
        layout(
          "<div style='display:grid;direction:rtl;width:100pt;grid-template-columns:20pt;justify-content:#{justify}'><div style='height:10pt;background:red'></div></div>"
        )

      assert_in_delta red(tree).x, expected, 0.02
    end

    for {justify, expected} <- [
          {"self-start", 0},
          {"self-end", 80},
          {"left", 0},
          {"right", 80},
          {"safe self-start", 0},
          {"unsafe self-end", 80}
        ] do
      tree =
        layout(
          "<div style='display:grid;direction:rtl;width:100pt;grid-template-columns:100pt'><div style='direction:ltr;justify-self:#{justify};width:20pt;height:10pt;background:red'></div></div>"
        )

      assert_in_delta red(tree).x, expected, 0.02
    end
  end

  test "baseline groups measure nested flex, grid and table descendants" do
    for display <- ["flex", "grid", "table"] do
      tree =
        layout(
          "<div style='display:flex;align-items:baseline;width:100pt'><div style='display:#{display};background:red'><div>Nested</div></div><span>Peer</span></div>"
        )

      texts = Enum.filter(tree.boxes, &(&1.type == :text))
      assert Enum.map(texts, & &1.text) == ["Nested", "Peer"]
      assert abs(hd(texts).y - List.last(texts).y) < 0.02
    end
  end

  test "forced column breaks and context-specific avoidance share the pagination engine" do
    for policy <- ["avoid-page", "avoid-column"] do
      tree =
        layout(
          "<div style='columns:2;height:30pt;column-fill:auto'><p style='break-inside:#{policy};margin:0'>A<br>B</p><p style='break-before:column;margin:0'>C</p></div>"
        )

      assert {:ok, pages} = Pagination.paginate(tree)
      assert length(pages) == 1
    end

    assert {:ok, :show} = HtmlTableValidator.prepare_empty_cells("SHOW")
    assert :error = HtmlTableValidator.prepare_empty_cells("other")
    parser = &NativeElixirPdfUtilities.Validators.HtmlValidator.parse_css_length(&1, :nonnegative)

    assert {:ok, _} =
             CssGridValidator.template("grid-template", "\"a\" 20pt [end] / 10pt", parser)
  end

  test "safe distributions and reverse wrapping retain physical alignment" do
    for display <- [
          "flex;flex-wrap:wrap-reverse",
          "grid;grid-template-columns:20pt;grid-template-rows:10pt"
        ] do
      tree =
        layout(
          "<div style='display:#{display};width:100pt;height:50pt;align-content:safe end;justify-content:safe end'><div style='flex:none;width:20pt;height:10pt;background:red'></div></div>"
        )

      assert_in_delta red(tree).x, 80, 0.02
      assert_in_delta red(tree).y, 250, 0.02
    end
  end

  test "table cells run their own column formatting context and default rule color" do
    tree =
      layout(
        "<table style='border-spacing:0'><tr><td style='padding:0;width:100pt;columns:2;column-gap:10pt;column-rule-width:1pt;column-rule-style:solid;color:red'>One<br>Two</td></tr></table>"
      )

    assert Enum.any?(tree.boxes, &(Map.get(&1, :stroke_color) == {1, 0, 0}))
    assert Enum.filter(tree.boxes, &(&1.type == :text)) |> Enum.map(& &1.text) == ["One", "Two"]
  end

  test "fieldsets align their legend at the flex end of the available inline space" do
    tree =
      layout(
        "<fieldset style='width:100pt;padding:0;border:0;margin:0'><legend style='justify-self:flex-end;width:20pt;padding:0;margin:0'>End</legend>Body</fieldset>"
      )

    legend = Enum.find(tree.boxes, &(Map.get(&1, :text) == "End"))
    assert_in_delta legend.x, 80, 0.02
  end

  test "source grid areas and independent gutters remain declarative until applied" do
    alias NativeElixirPdfUtilities.Validators.{CssValueValidator, HtmlValidator}

    for {property, value} <- [{"row-gap", "10%"}, {"column-gap", "2em"}] do
      assert {:ok, _} =
               CssValueValidator.prepare_unapplied_declaration(
                 %{declaration: {property, value}},
                 HtmlValidator.new_css_budget()
               )
    end

    assert {:ok, %{grid_template_areas: [["a"]]}} =
             CssValueValidator.validate_resolved(%{}, "grid-template-areas", "\"a\"")

    assert {:error, :invalid_document} =
             CssValueValidator.validate_resolved(%{}, "flex", "20pt 30pt")

    assert {:error, :invalid_document} =
             CssValueValidator.validate_resolved(%{}, "flex", "20pt 30pt 40pt")

    refute match?({:ok, _}, CssValueValidator.validate_resolved(%{}, "flex-basis", "broken("))

    for value <- ["broken(", "bananas"] do
      assert {:error, {:invalid_css, %{stage: :css, source: source, line: 1, column: 1}}} =
               CssValueValidator.prepare_unapplied_declaration(
                 %{
                   declaration: {"flex-basis", value},
                   source: %{text: "flex-basis:" <> value, line: 1, column: 1}
                 },
                 HtmlValidator.new_css_budget()
               )

      assert source == "flex-basis:" <> value
    end
  end

  defp layout(html) do
    {:ok, dom} = HtmlParser.parse(html)
    {:ok, styled} = Style.compute(dom, system_font_discovery: false)
    {:ok, tree} = Layout.layout(styled, page_size: {300, 300}, margin: 0)
    tree
  end

  defp red(tree), do: Enum.find(tree.boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
end
