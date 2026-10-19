defmodule NativeElixirPdfUtilities.HtmlToPdf.MathLayoutValidationTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, MathLayout, ResourceContext, Style}

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    MathLayoutValidator,
    MathPaintValidator,
    MathTableValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, font} = Font.resolve("math", 400, :normal, registry)

    style = %{
      display: :block,
      font_size: 20,
      font_face: font,
      color: {0, 0, 0},
      line_height: 24,
      math_style: :compact
    }

    {:ok, style: style, registry: registry}
  end

  defp node(tag, children, style, attributes \\ %{}) do
    %{
      type: :element,
      namespace: :mathml,
      tag: tag,
      children: children,
      attributes: attributes,
      style: style
    }
  end

  defp token(text, style), do: node("mn", [%{type: :text, text: text, style: style}], style)

  defp measured(html) do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      assert {:ok, dom} = HtmlParser.parse_detailed(html)

      assert {:ok, %{children: [root]}} =
               Style.compute_detailed(dom, system_font_discovery: false)

      root.style._math_box
    end)
  end

  defp document(html) do
    budget = HtmlValidator.new_image_budget()

    ResourceContext.run([], budget, fn _ ->
      assert {:ok, dom} = HtmlParser.parse_document_detailed(html)

      assert {:ok, document} =
               HtmlToPdf.prepare_document(
                 dom,
                 [page_size: {180, 120}, margin: 10, system_font_discovery: false],
                 budget
               )

      document
    end)
  end

  test "Math page markers reject malformed pagination geometry and clipped markers vanish" do
    prepared = document("<math><mn>7</mn></math>")

    boxes =
      Enum.map(hd(prepared.pages).boxes, fn box ->
        if Map.get(box, :role) == :math_paint, do: Map.put(box, :out_of_flow, false), else: box
      end)

    marker = Enum.find(boxes, &(Map.get(&1, :role) == :math_paint))
    assert marker != nil

    for args <- [
          {"bad", 100, {180, 120}, 110},
          {100, "bad", {180, 120}, 110},
          {100, 100, {180, 120}, "bad"},
          {100, 100, :bad, 110}
        ] do
      {group_top, target_top, page, top} = args

      assert {:error, {_, diagnostic}} =
               MathPaintValidator.prepare_pagination_top(boxes, group_top, target_top, page, top)

      assert is_atom(diagnostic.stage) and diagnostic.message != ""
    end

    assert {:error, {_, diagnostic}} =
             MathPaintValidator.prepare_pagination_top(
               [%{marker | math_paint_width: "bad"}],
               100,
               100,
               {180, 120},
               110
             )

    assert diagnostic.message =~ "visible width"

    assert {:error, {_, diagnostic}} =
             MathPaintValidator.prepare_pagination_top([], 100, nil, {180, 120}, 110)

    assert diagnostic.message =~ "numeric prepared top"

    clipped =
      Map.put(marker, :clip, %{
        x: marker.x + marker.math_paint_width + 1,
        y: marker.y,
        width: 1,
        height: marker.height
      })

    assert {:ok, []} = MathPaintValidator.prepare_flow_boxes([clipped], 180, 120)

    assert {:error, {_, diagnostic}} =
             MathPaintValidator.prepare_flow_boxes([%{marker | x: 181}], 180, 120)

    assert diagnostic.message =~ "outside"

    assert {:error, {_, diagnostic}} =
             MathPaintValidator.prepare_flow_boxes(
               [Map.put(marker, :out_of_flow, :bad)],
               180,
               120
             )

    assert diagnostic.message =~ "bounded placed geometry"
    assert {:error, {_, diagnostic}} = MathPaintValidator.prepare_flow_boxes([nil], 180, 120)
    assert diagnostic.message =~ "typed layout boxes"
  end

  test "native Math paint fails at the selected-face glyph boundary and propagates nested errors" do
    box = measured("<math><mtext><span style='font-family:DejaVu Sans'>7</span></mtext></math>")
    native = Enum.find(box.drawing_items, &(&1.kind == :native and &1.box.type == :glyph_group))
    assert native != nil
    [leaf | _] = native.box.boxes
    bad = Map.put(leaf, :glyph_ids, [65_535])
    item = %{native | box: %{native.box | boxes: [bad]}}
    malformed = %{box | drawing_items: [item]}
    assert {:error, {_, diagnostic}} = MathPaintValidator.prepare_geometry(malformed)
    assert diagnostic.stage in [:layout, :font] and diagnostic.message =~ "glyph"
  end

  test "canonical Math owner adapters preserve unwrapped schemas and ordinary token styles", %{
    style: style
  } do
    ordinary = Map.put(style, :_math_formatting, false)
    assert MathLayoutValidator.owner_atom_style(%{tag: "mn", style: ordinary}) == ordinary
    assert MathLayoutValidator.owner_atom_style(%{tag: "mfrac", style: style}) == style
    assert MathLayoutValidator.owner_atom_style(%{tag: "mo", style: style}).display == :block

    assert MathLayoutValidator.owner_atom_style(%{tag: "mfrac", style: ordinary}).display ==
             :block

    refute MathLayoutValidator.independent_math_context?([%{namespace: :mathml, tag: "mrow"}])

    assert :ok =
             MathLayoutValidator.validate_element_support(
               node("mspace", [%{type: :processing_instruction}], style)
             )
  end

  test "mpadded failures preserve authored length diagnostics and bounded source geometry", %{
    style: style
  } do
    for attrs <- [%{"height" => :bad}, %{"width" => :bad}] do
      assert {:error, {_, diagnostic}} =
               MathLayout.measure(node("mpadded", [token("1", style)], style, attrs))

      assert diagnostic.stage == :layout and diagnostic.source == "mpadded"
    end

    padded = Map.put(style, :width, {:math_padded_length, "-2pt"})
    assert {:error, {_, diagnostic}} = MathLayoutValidator.compute_style(padded)
    assert diagnostic.message =~ "nonnegative"
    source = node("mpadded", [], Map.put(style, :width, {:math_padded_length, "20pt"}))
    assert {:ok, false} = MathLayoutValidator.prepare_block_flow_dependency(source)

    assert {:error, {_, diagnostic}} =
             MathLayoutValidator.prepare_block_flow_dependency(
               node("mrow", [], Map.put(style, :width, -1))
             )

    assert diagnostic.message =~ "bounded"
    Limits.install(Map.put(Limits.effective(), :max_html_depth, 1))
    deep = node("mrow", [node("mrow", [token("1", style)], style)], style)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             MathLayoutValidator.prepare_block_flow_dependency(deep)

    assert diagnostic.message =~ "max_html_depth"
  end

  test "matrix allocated vertical alignment and row spans retain every selected glyph" do
    for align <- ~w(top middle bottom) do
      html =
        "<math><mtable style='width:100pt'><mtr style='height:40pt;background:yellow'><mtd rowspan='2' style='vertical-align:#{align};background:blue'><mn>1</mn></mtd><mtd><mn>2</mn></mtd></mtr><mtr style='height:30pt;background:yellow'><mtd><mn>3</mn></mtd></mtr></mtable></math>"

      box = measured(html)
      assert box.actual_text == "123"
      assert :ok == MathLayoutValidator.validate_measured_geometry(box)
      assert length(box.glyphs) == 3
    end

    for html <- [
          "<math><mtable style='width:80pt'><mtr><mtd style='width:0;padding:0'/><mtd style='padding:0'/></mtr></mtable></math>",
          "<math><mtable><mtr><mtd columnspan='2'><mn>123</mn></mtd><mtd><mn>4</mn></mtd></mtr><mtr><mtd><mn>5</mn></mtd><mtd><mn>6</mn></mtd><mtd><mn>7</mn></mtd></mtr></mtable></math>"
        ] do
      assert :ok == MathLayoutValidator.validate_measured_geometry(measured(html))
    end

    assert {:error, {_, diagnostic}} = MathTableValidator.validate_style(nil, "mtable")
    assert diagnostic.message =~ "prepared style map"
  end

  test "matrix native operator reflow is derived from allocated content" do
    box =
      measured(
        "<math><mtable style='width:80pt;border-spacing:2pt'><mtr><mtd><mo><a href='https://example.test/operator'>Words</a></mo></mtd><mtd><mn>2</mn></mtd></mtr></mtable></math>"
      )

    assert box.actual_text == "Words2"
    assert Enum.any?(box.drawing_items, &(&1.kind == :native))
  end

  test "authored legacy hints and Math CSS properties use the selected inherited context", %{
    style: style
  } do
    hinted =
      node("mi", [], style, %{
        "mathcolor" => "invalid",
        "mathsize" => "18pt",
        "displaystyle" => "false",
        "mathvariant" => "normal"
      })

    hints = MathLayoutValidator.style_hints(hinted)
    assert {"font-size", "18pt"} in hints
    assert {"math-style", "compact"} in hints
    assert {"text-transform", "none"} in hints
    refute Enum.any?(hints, &(elem(&1, 0) == "color"))

    assert MathLayoutValidator.style_hints(%{
             hinted
             | attributes: %{"mathsize" => "invalid", "displaystyle" => "invalid"}
           }) == []

    context = Map.put(style, :_root_font_size, 30)
    assert {:ok, 10.0} = MathLayoutValidator.parse_font_size("50%", context)
    assert {:ok, 60.0} = MathLayoutValidator.parse_font_size("2rem", context)

    assert {:ok, %{text_transform: :math_auto}} =
             MathLayoutValidator.parse_property("text-transform", "math-auto", context)

    assert {:ok, %{math_depth: 1}} =
             MathLayoutValidator.parse_property("math-depth", "auto-add", context)

    assert {:ok, %{math_depth: 0}} =
             MathLayoutValidator.parse_property(
               "math-depth",
               "auto-add",
               Map.put(context, :math_style, :normal)
             )

    assert :error = MathLayoutValidator.parse_display(12)
    assert :error = MathLayoutValidator.parse_display(<<255>>)

    refute MathLayoutValidator.independent_math_context?([
             %{tag: "span"},
             %{namespace: :mathml, tag: "mrow"}
           ])
  end

  test "unallocated Math table children and malformed row source return actionable diagnostics",
       %{style: style} do
    for tag <- ~w(mtd mtr) do
      assert {:error, {_, diagnostic}} =
               MathLayout.measure(
                 node(
                   tag,
                   [],
                   Map.put(style, :display, if(tag == "mtd", do: :table_cell, else: :table_row))
                 )
               )

      assert diagnostic.stage == :layout and diagnostic.source == tag
      assert diagnostic.message =~ "containing mtable"
    end

    assert {:error, {_, diagnostic}} = MathLayout.measure(node("mrow", [nil], style))
    assert diagnostic.stage == :layout and diagnostic.message =~ "typed source"
    bounded = Map.put(style, :_root_font_size, 20)
    Limits.install(Map.put(Limits.effective(), :max_css_numeric_magnitude, 50))

    assert {:error, {_, diagnostic}} =
             MathLayout.measure(node("mpadded", [], bounded, %{"height" => "3rem"}))

    assert diagnostic.source == "mpadded" and diagnostic.message =~ "bounds"
  end

  test "partial multiscript requests reject incomplete prepared source pairs", %{
    style: style
  } do
    children = Enum.map(~w(1 2 3), &token(&1, style))

    assert {:ok, prepared} =
             MathLayoutValidator.prepare_element(node("mmultiscripts", children, style), %{})

    partial = Enum.take(children, 2)

    boxes =
      Enum.map(partial, fn child ->
        assert {:ok, box} = MathLayout.measure(child)
        box
      end)

    assert {:error, {_, diagnostic}} =
             MathLayoutValidator.prepare_multiscript_placement(
               %{prepared | children: Enum.take(prepared.children, 2)},
               boxes
             )

    assert diagnostic.stage == :layout and diagnostic.message =~ "complete script pairs"
  end

  test "ordinary CSS schema character children retain selected font layout and malformed children diagnose",
       %{style: style} do
    ordinary = Map.merge(style, %{_math_formatting: false, display: :inline_block})
    box = measured("<math><mfrac style='display:inline-block'>123</mfrac></math>")
    assert box.actual_text == "123" and box.width > 0
    assert {:error, {_, diagnostic}} = MathLayout.measure(node("mfrac", [nil], ordinary))
    assert diagnostic.stage == :layout and diagnostic.message != ""
    assert :error = MathLayoutValidator.parse_property("display", "inline math", style)

    for tag <- ~w(mtd mtr) do
      display = if tag == "mtd", do: :table_cell, else: :table_row

      assert {:error, {_, diagnostic}} =
               MathLayout.measure(node(tag, [], Map.put(style, :display, display)))

      assert diagnostic.source == tag and diagnostic.message =~ "containing mtable"
    end

    preserved = Map.merge(style, %{white_space: :pre, display: :inline})
    direct = %{type: :text, text: "1\t2", style: preserved}
    assert {:ok, normalized} = MathLayout.measure(node("mn", [direct], style))
    assert normalized.actual_text == "1 2"

    span = %{
      type: :element,
      namespace: :html,
      tag: "span",
      attributes: %{},
      style: preserved,
      children: [direct]
    }

    assert {:error, {_, diagnostic}} = MathLayout.measure(node("mn", [span], style))
    assert diagnostic.stage == :layout and diagnostic.message =~ "preserved breaks"
  end

  test "primitive selected-font ex and unavailable ideographic Math operator units remain bounded" do
    for length <- ~w(2ex 1rex 1ic) do
      box = measured("<math><mo lspace='#{length}'>+</mo><mn>1</mn></math>")
      assert box.actual_text == "+1" and box.width > 0
    end
  end

  test "allocated span columns preserve unaffected cell widths" do
    box =
      measured(
        "<math><mtable style='width:100pt'><mtr><mtd columnspan='2'><mn>12</mn></mtd><mtd><mn>3</mn></mtd></mtr><mtr><mtd><mn>4</mn></mtd><mtd><mn>5</mn></mtd><mtd><mn>6</mn></mtd></mtr></mtable></math>"
      )

    assert box.actual_text == "123456" and length(box.glyphs) == 6
    assert :ok == MathLayoutValidator.validate_measured_geometry(box)
  end

  test "bounded padded shifts account for every positioned glyph", %{style: style} do
    Limits.install(Map.put(Limits.effective(), :max_css_numeric_magnitude, 40))

    assert {:error, {_, diagnostic}} =
             MathLayout.measure(
               node("mpadded", [token("12", style)], style, %{"lspace" => "40pt"})
             )

    assert diagnostic.source == "mpadded" and diagnostic.message =~ "placement exceeds"
  end

  test "allocated table baselines reject combined cell insets beyond shared numeric bounds",
       %{style: style} do
    Limits.install(Map.put(Limits.effective(), :max_css_numeric_magnitude, 50))
    padding = %{top: 30.0, bottom: 30.0, left: 0.0, right: 0.0}
    cell_style = Map.merge(style, %{display: :table_cell, padding: padding, vertical_align: :top})
    row_style = Map.put(style, :display, :table_row)
    table_style = Map.put(style, :display, :inline_table)
    cell = node("mtd", [token("1", style)], cell_style)
    table = node("mtable", [node("mtr", [cell], row_style)], table_style)
    assert {:error, {:resource_limit_exceeded, diagnostic}} = MathLayout.measure(table)
    assert diagnostic.stage == :layout and diagnostic.message =~ "max_css_numeric_magnitude"
  end

  test "public script placements reject incomplete caller font descriptors before layout", %{
    style: style
  } do
    children = Enum.map(~w(1 2 3), &token(&1, style))

    boxes =
      Enum.map(children, fn child ->
        assert {:ok, box} = MathLayout.measure(child)
        box
      end)

    [base, sub, sup] = boxes
    [glyph] = sub.glyphs
    incomplete = %{glyph | font_face: %{type: :embedded}}

    invalid_boxes = [
      base,
      %{sub | glyphs: [incomplete], drawing_items: [Map.put(incomplete, :kind, :glyph)]},
      sup
    ]

    for tag <- ~w(msubsup mmultiscripts) do
      assert {:ok, prepared} =
               MathLayoutValidator.prepare_element(node(tag, children, style), %{})

      result =
        if tag == "mmultiscripts",
          do: MathLayoutValidator.prepare_multiscript_placement(prepared, invalid_boxes),
          else: MathLayoutValidator.prepare_script_placement(prepared, invalid_boxes)

      assert {:error, {_, diagnostic}} = result
      assert diagnostic.stage == :font and diagnostic.message != ""
    end
  end

  test "prepared property failures keep their canonical Math diagnostics", %{style: style} do
    assert :error = MathLayoutValidator.parse_display("inline flow")

    assert {:error, {_, diagnostic}} =
             MathLayout.measure(node("mspace", [], Map.put(style, :display, :table_column)))

    assert diagnostic.stage == :layout and diagnostic.message =~ "display"

    assert {:error, {_, diagnostic}} =
             MathLayout.measure(node("mspace", [], Map.put(style, :link_url, :bad)))

    assert diagnostic.message =~ "links"

    assert {:error, {_, diagnostic}} =
             MathLayout.measure(node("mrow", [], Map.put(style, :margin, :bad)))

    assert diagnostic.message =~ "bounded box edges"

    assert {:error, {_, diagnostic}} =
             MathLayout.measure(
               node("mo", [%{type: :text, text: "+", style: style}], style, %{"lspace" => 42})
             )

    assert diagnostic.stage == :layout
    assert {:error, {_, diagnostic}} = MathLayoutValidator.prepare_multiscript_placement(nil, [])
    assert diagnostic.message =~ "validator-prepared"
    Limits.install(Map.put(Limits.effective(), :max_css_numeric_magnitude, 50))
    bounded = Map.put(style, :_root_font_size, 20)

    assert {:error, {_, diagnostic}} =
             MathLayout.measure(node("mpadded", [], bounded, %{"width" => "3rem"}))

    assert diagnostic.source == "mpadded" and diagnostic.message =~ "bounds"
  end

  test "native Math owner styles require the resource scope", %{style: style} do
    owner = node("mrow", [token("1", style)], Map.put(style, :position, :relative))
    assert {:error, {:invalid_options, diagnostic}} = MathLayout.measure(owner)
    assert diagnostic.stage == :resource and diagnostic.message =~ "scoped context"
  end

  test "unsupported space backgrounds and exhausted native-image box quotas diagnose", %{
    style: style
  } do
    image = %{
      format: :png,
      data: <<0, 0, 0>>,
      width_px: 1,
      height_px: 1,
      color_space: :device_rgb,
      bits_per_component: 8
    }

    assert {:ok, space} =
             MathLayout.measure(node("mspace", [], Map.put(style, :background_image, image)))

    assert [%{kind: :background, style: %{background_image: ^image}}] = space.drawing_items
    Limits.install(Map.put(Limits.effective(), :max_layout_boxes, 0))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             MathLayoutValidator.validate_background_style(%{background_image: image}, "mrow")

    assert diagnostic.source == "mrow" and diagnostic.message =~ "max_layout_boxes"
  end

  test "native matrix reflow preserves shared font and grid quota diagnostics" do
    html =
      "<math><mtable style='width:100pt'><mtr><mtd><mtext><span>Actual source words</span></mtext></mtd></mtr></mtable></math>"

    limits = Limits.effective()

    run = fn ->
      ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
        assert {:ok, dom} = HtmlParser.parse_detailed(html)
        result = Style.compute_detailed(dom, system_font_discovery: false)
        # Read actual consumed work, including this runtime's font-cache state.
        # The assertion is the public quota failure, not an invented cost receipt.
        font = Process.get({NativeElixirPdfUtilities.Validators.FontValidator, :budget})
        grid = Process.get({HtmlValidator, :render_budget})

        {result,
         %{
           max_font_math_work: Map.fetch!(font, :max_font_math_work),
           max_table_grid_work: Map.fetch!(grid, :max_table_grid_work)
         }}
      end)
    end

    assert {{:ok, _document}, usage} = run.()

    for {limit, used} <- usage,
        deficit <- if(limit == :max_table_grid_work, do: [4, 2, 1], else: [32, 16, 1]) do
      Limits.install(Map.put(limits, limit, max(used - deficit, 0)))
      {result, _usage} = run.()
      assert {:error, {:resource_limit_exceeded, diagnostic}} = result
      assert diagnostic.stage in [:font, :layout] and diagnostic.message =~ Atom.to_string(limit)
      assert diagnostic.operation == :measure_math
    end
  end

  test "direct native cell allocation respects both cumulative resource quotas", %{
    style: style,
    registry: registry
  } do
    style =
      Map.merge(style, %{
        _font_registry: registry,
        font_families: ["math"],
        font_weight: 400,
        font_style: :normal,
        white_space: :normal,
        letter_spacing: 0.0,
        word_spacing: 0.0
      })

    span = %{
      type: :element,
      namespace: :html,
      tag: "a",
      attributes: %{"href" => "https://example.test/source"},
      style: Map.merge(style, %{display: :inline, link_url: "https://example.test/source"}),
      children: [%{type: :text, text: "Actual source words", style: style}]
    }

    text = node("mtext", [span], style)
    cell = node("mtd", [text], Map.put(style, :display, :table_cell))
    row = node("mtr", [cell], Map.put(style, :display, :table_row))
    table = node("mtable", [row], Map.merge(style, %{display: :inline_table, width: 100.0}))
    limits = Limits.effective()

    run = fn ->
      ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
        result = MathLayout.measure(table, %{resource_context: context})
        font = Process.get({NativeElixirPdfUtilities.Validators.FontValidator, :budget})
        grid = Process.get({HtmlValidator, :render_budget})

        {result,
         %{
           max_font_math_work: Map.fetch!(font, :max_font_math_work),
           max_table_grid_work: Map.fetch!(grid, :max_table_grid_work)
         }}
      end)
    end

    assert {{:ok, _box}, usage} = run.()

    for {limit, used} <- usage, deficit <- [4, 2, 1] do
      Limits.install(Map.put(limits, limit, used - deficit))
      {result, _usage} = run.()
      assert {:error, {:resource_limit_exceeded, diagnostic}} = result
      assert diagnostic.stage in [:font, :layout] and diagnostic.message =~ Atom.to_string(limit)
      assert diagnostic.operation == :measure_math
    end
  end

  test "visible children of mspace report the invalid source content model", %{style: style} do
    for child <- [token("1", style), %{type: :text, text: "visible", style: style}] do
      assert {:error, {:unsupported_html, diagnostic}} =
               MathLayout.measure(node("mspace", [child], style))

      assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
      assert diagnostic.source == "mspace" and diagnostic.message =~ "content model"
    end
  end

  test "invalid positioned math descendants retain the child diagnostic" do
    assert {:error, {reason, diagnostic}} =
             HtmlToPdf.render(
               "<math><mrow style='position:absolute;left:10pt;top:10pt'><mn>à</mn></mrow></math>",
               system_font_discovery: false
             )

    assert diagnostic.reason == reason and diagnostic.stage == :layout
    assert diagnostic.operation == :measure_math and diagnostic.source == "à"
    assert diagnostic.message =~ "combining"
  end
end
