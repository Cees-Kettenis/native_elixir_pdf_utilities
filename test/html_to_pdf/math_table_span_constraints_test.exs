defmodule NativeElixirPdfUtilities.HtmlToPdf.MathTableSpanConstraintsTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    Font,
    HtmlParser,
    Layout,
    MathLayout,
    ResourceContext,
    Style
  }

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    MathLayoutValidator,
    MathTableValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, font} = Font.resolve("math", 400, :normal, registry)

    style = %{
      display: :inline_table,
      font_size: 20.0,
      font_face: font,
      color: {0, 0, 0},
      line_height: 24.0,
      math_style: :compact,
      text_transform: :none
    }

    {:ok, style: style}
  end

  defp node(tag, children, style, attributes \\ %{}) do
    %{
      type: :element,
      namespace: :mathml,
      tag: tag,
      attributes: attributes,
      children: children,
      style: style
    }
  end

  defp cell(text, style, attributes \\ %{}, dimensions \\ %{}) do
    content = node("mn", [%{type: :text, text: text, style: style}], %{style | display: :inline})

    node(
      "mtd",
      [content],
      Map.merge(style, %{display: :table_cell, text_align: :center}) |> Map.merge(dimensions),
      attributes
    )
  end

  defp table(rows, style),
    do: node("mtable", Enum.map(rows, &node("mtr", &1, %{style | display: :table_row})), style)

  defp geometry(table) do
    assert {:ok, prepared} = MathLayoutValidator.prepare_element(table, %{})
    # Use complete real measured content from the same selected-font preparation.
    measured =
      Enum.map(table.children, fn row ->
        Enum.map(row.children, fn cell ->
          anonymous = %{
            cell
            | tag: "mrow",
              style: MathLayoutValidator.anonymous_box_style(cell.style)
          }

          assert {:ok, measured} = MathLayout.measure(anonymous)
          measured
        end)
      end)

    assert {:ok, geometry} = MathTableValidator.prepare_geometry(prepared, measured)
    {prepared, measured, geometry}
  end

  test "spanning-only columns allocate real preferred widths without duplicating content", %{
    style: style
  } do
    source = table([[cell("123456", style, %{"columnspan" => "2"})]], style)
    {_prepared, _measured, result} = geometry(source)
    assert length(result.columns) == 2
    assert_in_delta Enum.sum(Enum.map(result.columns, & &1.width)), result.width, 1.0e-9
    assert {:ok, measured} = MathLayout.measure(source)
    assert measured.actual_text == "123456"
    assert length(measured.glyphs) == 6
  end

  test "row spans use shared baseline allocation and preserve authored row order", %{style: style} do
    source =
      table(
        [
          [cell("1", style, %{"rowspan" => "2"}, %{height: 80.0}), cell("2", style)],
          [cell("3", style)]
        ],
        style
      )

    {prepared, _measured, result} = geometry(source)
    assert Enum.map(hd(prepared.rows), & &1.column) == [0, 1]
    assert hd(List.last(prepared.rows)).column == 1
    assert hd(hd(prepared.rows)).rowspan == 2
    assert result.ascent + result.descent >= 80
    assert hd(result.cells).content_ascent + hd(result.cells).content_descent >= 80
    assert {:ok, measured} = MathLayout.measure(source)
    assert measured.actual_text == "123"
    assert length(measured.glyphs) == 3
  end

  test "explicit table and cell constraints allocate geometry before decoration", %{style: style} do
    source =
      table(
        [[cell("1", style, %{}, %{width: 60.0, height: 30.0}), cell("2", style)]],
        Map.merge(style, %{width: 120.0, height: 60.0})
      )

    {_prepared, _measured, result} = geometry(source)
    assert_in_delta result.width, 120, 1.0e-9
    assert_in_delta result.ascent + result.descent, 60, 1.0e-9
    assert_in_delta hd(result.cells).content_width, 60, 1.0e-9
    assert_in_delta List.last(result.cells).content_width, 60, 1.0e-9
    assert {:ok, measured} = MathLayout.measure(source)
    assert_in_delta measured.width, 120, 1.0e-9
    assert measured.actual_text == "12"
  end

  test "cell border-box constraints and min expressions use shared CSS sizing", %{style: style} do
    edges = %{top: 2.0, right: 2.0, bottom: 2.0, left: 2.0}

    source =
      table(
        [[cell("1", style, %{}, %{width: 40.0, padding: edges, box_sizing: :border_box})]],
        Map.merge(style, %{width: {:min, [80.0, 100.0]}, min_width: 50.0, max_width: 90.0})
      )

    {_prepared, _measured, result} = geometry(source)
    assert_in_delta result.width, 80, 1.0e-9
    assert_in_delta hd(result.cells).content_width, 76, 1.0e-9
  end

  test "definite matrix percentage basis resolves table and cell constraints", %{style: style} do
    source =
      table(
        [[cell("1", style, %{}, %{width: {:percent, 0.25}}), cell("2", style)]],
        Map.put(style, :width, {:percent, 1.0})
      )

    assert {:ok, intrinsic} = MathLayout.measure(source)
    assert intrinsic.width < 200
    assert {:ok, definite} = MathLayout.measure(source, %{available_inline_size: 200.0})
    assert_in_delta definite.width, 200, 1.0e-9
    assert definite.actual_text == "12"

    for invalid <- [-1, :auto, {:percent, 1.0}, "200", Limits.get(:max_css_numeric_magnitude) + 1] do
      assert {:error, {reason, diagnostic}} =
               MathLayout.measure(source, %{available_inline_size: invalid})

      assert diagnostic.reason == reason and diagnostic.stage == :layout
      assert diagnostic.operation in [:measure_math, :validate_css_size]
      assert diagnostic.message != ""
    end
  end

  test "block percentage matrices remeasure at actual nested container width and preserve source" do
    html =
      "<div style='width:200pt'><math display='block'><mtable style='width:100%;background:lightblue'><mtr><mtd style='width:25%'><mn>1</mn></mtd><mtd><mn>23</mn></mtd></mtr></mtable></math></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, {styled, layout}} =
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
               with {:ok, styled} <- Style.compute_detailed(dom, system_font_discovery: false),
                    {:ok, layout} <- Layout.layout(styled, margin: 0) do
                 {:ok, {styled, layout}}
               end
             end)

    [container] = styled.children
    [math] = container.children
    [atom] = math.children
    assert atom._math_layout_source.node_id == math.node_id
    assert atom.style._math_box.width < 200
    color = hd(atom._math_layout_source.children).style.background_color
    background = Enum.find(layout.boxes, &(&1.type == :rect and &1.fill_color == color))
    assert_in_delta background.width, 200, 1.0e-9
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "123"} = Text.extract(pdf, layout: false)

    for children <- [[], [%{}], [atom, atom], [%{atom | style: %{}}]] do
      assert {:error, {reason, diagnostic}} = MathLayout.resolve_block_flow(children, 200)
      assert reason == diagnostic.reason and diagnostic.stage == :layout
      assert diagnostic.operation == :measure_math
    end

    assert {:error, {_, diagnostic}} = MathLayout.resolve_block_flow([atom], nil)
    assert diagnostic.message =~ "definite"
  end

  test "block width dependency excludes independently sized equations and diagnoses bad source",
       %{style: style} do
    assert {:ok, false} =
             MathTableValidator.prepare_block_flow_dependency(table([[cell("1", style)]], style))

    assert {:ok, true} =
             MathTableValidator.prepare_block_flow_dependency(
               table(
                 [[cell("1", style)]],
                 Map.put(style, :min_width, {:min, [100.0, {:percent, 0.5}]})
               )
             )

    for source <- [
          %{},
          %{type: :element, children: [%{}]},
          %{type: :element, children: [cell("1", style) | :bad]}
        ] do
      assert {:error, {reason, diagnostic}} =
               MathTableValidator.prepare_block_flow_dependency(source)

      assert diagnostic.reason == reason and diagnostic.source == "math"
    end

    html = "<math display='block'><mn>1</mn></math>"
    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, %{children: [math]}} =
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
               Style.compute_detailed(dom, system_font_discovery: false)
             end)

    assert math.style._math_flow_requires_inline_size == false
    refute Map.has_key?(hd(math.children), :_math_layout_source)
  end

  test "invalid prepared sizes and forged span placements diagnose in validators", %{style: style} do
    for invalid <- [-1, {:percent, -1}, {:min, [10 | :bad]}, {:min, [:auto]}, {:min, [nil]}] do
      assert {:error, {reason, diagnostic}} =
               MathLayout.measure(table([[cell("1", style)]], Map.put(style, :width, invalid)))

      assert reason == diagnostic.reason and diagnostic.stage == :layout
      assert diagnostic.source == "mtable"
    end

    source = table([[cell("1", style, %{"columnspan" => "2"})]], style)
    {prepared, measured, _} = geometry(source)
    cell = hd(hd(prepared.rows))

    assert {:error, {_, diagnostic}} =
             MathTableValidator.prepare_geometry(
               %{prepared | rows: [[%{cell | column: 1}]]},
               measured
             )

    assert diagnostic.source == "mtd" and diagnostic.message =~ "nonoverlapping"
  end

  test "nested percentage matrices keep an indefinite anonymous Math row basis" do
    html =
      "<math><mtable style='width:180pt'><mtr><mtd style='width:90pt;padding:0'><mtable style='width:100%;background:lightblue'><mtr><mtd style='padding:0'><mn>1</mn></mtd></mtr></mtable></mtd><mtd style='padding:0'><mn>2</mn></mtd></mtr></mtable></math>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, %{children: [math]}} =
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
               Style.compute_detailed(dom, system_font_discovery: false)
             end)

    assert_in_delta math.style._math_box.width, 180, 1.0e-9
    backgrounds = Enum.filter(math.style._math_box.drawing_items, &(&1.kind == :background))
    assert length(backgrounds) == 1
    glyph = Enum.find(math.style._math_box.glyphs, &(&1.text == "1"))
    assert_in_delta hd(backgrounds).width, glyph.width, 1.0e-9
    assert hd(backgrounds).width < 90
    assert math.style._math_box.actual_text == "12"
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "12"} = Text.extract(pdf, layout: false)
  end

  test "ordinary HTML token content wraps at its allocated matrix cell width" do
    text = "Alpha beta gamma delta epsilon"

    html =
      "<math><mtable style='width:100pt'><mtr><mtd style='padding:0'><mtext style='font-family:DejaVu Sans;font-size:12pt;line-height:18pt;display:inline'><span>#{text}</span></mtext></mtd></mtr></mtable></math>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, %{children: [math]}} =
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
               Style.compute_detailed(dom, system_font_discovery: false)
             end)

    measured = math.style._math_box
    assert_in_delta measured.width, 100, 1.0e-9
    assert measured.actual_text == text
    assert_in_delta measured.ascent + measured.descent, 54, 1.0e-9

    lines =
      measured.drawing_items
      |> Enum.filter(&(&1.kind == :native))
      |> Enum.flat_map(fn item ->
        if item.box.type == :glyph_group, do: item.box.boxes, else: [item.box]
      end)
      |> Enum.filter(&(&1.type == :text))
      |> Enum.map(& &1.y)
      |> Enum.uniq()

    assert length(lines) > 1
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, ^text} = Text.extract(pdf, layout: false)
  end

  test "cell reflow request failures use the shared diagnostic contract" do
    for cell <- [
          %{},
          %{content_layout_source: %{}, content_layout_context: %{}},
          %{
            content_layout_source: %{type: :element, namespace: :mathml, tag: "mrow"},
            content_layout_context: nil
          }
        ] do
      assert {:error, {reason, diagnostic}} = MathTableValidator.prepare_cell_reflow(cell, 10)
      assert reason == diagnostic.reason and diagnostic.source == "mtd"
      assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
    end

    cell = %{
      content_layout_source: %{type: :element, namespace: :mathml, tag: "mrow"},
      content_layout_context: %{}
    }

    assert {:error, {_, diagnostic}} = MathTableValidator.prepare_cell_reflow(cell, nil)
    assert diagnostic.message =~ "definite"
    assert {:error, {_, diagnostic}} = MathTableValidator.prepare_cell_reflow(cell, 10)
    assert diagnostic.operation == :measure_math and diagnostic.stage == :layout
  end

  test "wide spans reserve redistribution work before allocation", %{style: style} do
    source = table([[cell("1", style, %{"columnspan" => "100"})]], style)
    Limits.install(Map.put(Limits.effective(), :max_table_grid_work, 400))
    assert {:error, {:resource_limit_exceeded, diagnostic}} = MathLayout.measure(source)
    assert diagnostic.reason == :resource_limit_exceeded and diagnostic.stage == :layout
    assert diagnostic.message =~ "max_table_grid_work"
  end

  test "actual authored cell identity survives span preparation and decorated paint" do
    html =
      "<math><mtable><mtr id='row'><mtd id='wide' columnspan='2' style='background:lightblue'><mn>123</mn></mtd></mtr></mtable></math>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    authored =
      dom.children
      |> hd()
      |> Map.fetch!(:children)
      |> hd()
      |> Map.fetch!(:children)
      |> hd()
      |> Map.fetch!(:children)
      |> hd()

    assert authored.tag == "mtd"

    assert {:ok, %{children: [math]}} =
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
               Style.compute_detailed(dom, system_font_discovery: false)
             end)

    paint =
      Enum.find(
        math.style._math_box.drawing_items,
        &(&1.kind == :background and get_in(&1, [:style, :_math_source, :tag]) == "mtd")
      )

    assert paint.style._math_source.attributes == authored.attributes
    assert is_reference(paint.style._math_source._selector_id)

    for key <- [:node_id, :parent_id, :document_id, :tree_scope_id] do
      assert Map.fetch!(paint.style._math_source, key) == Map.fetch!(authored, key)
    end

    assert paint.style._math_source.source == authored.source
  end

  test "spaced native text, atomic HTML content, radical and nested matrix cells compose" do
    for content <- [
          "<mtext style='font-family:DejaVu Sans'><span style='display:inline-block;width:20pt'>A</span></mtext>",
          "<msqrt><mn>2</mn></msqrt>",
          "<mtable><mtr><mtd><mn>3</mn></mtd></mtr></mtable>"
        ] do
      html =
        "<math><mtable style='border-spacing:0.5pt 0.375pt'><mtr><mtd>#{content}</mtd></mtr></mtable></math>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)

      assert {:ok, %{children: [math]}} =
               ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
                 Style.compute_detailed(dom, system_font_discovery: false)
               end)

      assert math.style._math_box.width > 0
      assert math.style._math_box.actual_text != ""
      assert :ok == MathLayoutValidator.validate_measured_geometry(math.style._math_box)
    end
  end

  test "original public span markup retains logical PDF text once" do
    html =
      "<math><mtable><mtr><mtd columnspan='2'><mn>123</mn></mtd></mtr><mtr><mtd rowspan='2'><mn>4</mn></mtd><mtd><mn>5</mn></mtd></mtr><mtr><mtd><mn>6</mn></mtd></mtr></mtable></math>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "123456"} = Text.extract(pdf, layout: false)
  end
end
