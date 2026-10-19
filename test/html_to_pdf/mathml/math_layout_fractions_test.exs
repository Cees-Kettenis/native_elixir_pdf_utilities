defmodule NativeElixirPdfUtilities.HtmlToPdf.MathLayoutFractionsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, MathLayout, Style}
  alias NativeElixirPdfUtilities.Validators.MathLayoutValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, font} = Font.resolve("math", 400, :normal, registry)
    {:ok, metrics} = Font.math_metrics(font)
    {:ok, bounds} = Font.glyph_bounds(font)

    style = %{
      display: :block,
      font_size: 20,
      font_face: font,
      color: {1, 0, 0},
      line_height: 24,
      math_style: :compact,
      text_transform: :none
    }

    {:ok, font: font, metrics: metrics, bounds: bounds, style: style}
  end

  defp token(tag, text, style) do
    %{
      type: :element,
      namespace: :mathml,
      tag: tag,
      attributes: %{},
      style: style,
      children: [%{type: :text, text: text, style: style}]
    }
  end

  defp fraction(children, style, attributes \\ %{}) do
    %{
      type: :element,
      namespace: :mathml,
      tag: "mfrac",
      attributes: attributes,
      style: style,
      children: children
    }
  end

  defp styled_math(html) do
    with {:ok, dom} <- HtmlParser.parse_detailed(html),
         {:ok, %{children: [node]}} <- Style.compute_detailed(dom, system_font_discovery: false) do
      atom = if Map.get(node.style, :_math_box), do: node, else: hd(node.children)
      {:ok, atom.style._math_box}
    end
  end

  test "compact and display gaps, axis and centering follow independent selected font values",
       c do
    for mode <- [:compact, :normal] do
      style = %{c.style | math_style: mode}
      n = token("mn", "1", style)
      d = token("mn", "g", style)
      assert {:ok, measured} = MathLayout.measure(fraction([n, d], style))
      constants = c.metrics.constants
      scale = 20 / c.metrics.units_per_em
      axis = constants.axis_height.value * scale
      thickness = constants.fraction_rule_thickness.value * scale
      {_xmin, nymin, _xmax, nymax} = elem(c.bounds, c.font.cmap[?1])
      {_xmin, dymin, _xmax, dymax} = elem(c.bounds, c.font.cmap[?g])

      {up_name, ng_name, down_name, dg_name} =
        if mode == :compact,
          do:
            {:fraction_numerator_shift_up, :fraction_numerator_gap_min,
             :fraction_denominator_shift_down, :fraction_denominator_gap_min},
          else:
            {:fraction_numerator_display_style_shift_up, :fraction_num_display_style_gap_min,
             :fraction_denominator_display_style_shift_down,
             :fraction_denom_display_style_gap_min}

      up =
        max(
          constants[up_name].value * scale,
          axis + thickness / 2 + constants[ng_name].value * scale - nymin * scale
        )

      down =
        max(
          constants[down_name].value * scale,
          thickness / 2 + constants[dg_name].value * scale + dymax * scale - axis
        )

      nw = Enum.at(c.font.widths, c.font.cmap[?1]) * scale
      dw = Enum.at(c.font.widths, c.font.cmap[?g]) * scale
      width = max(nw, dw)
      [numerator, denominator] = measured.glyphs
      assert_in_delta numerator.y, up, 1.0e-10
      assert_in_delta denominator.y, -down, 1.0e-10
      assert_in_delta numerator.x, (width - nw) / 2, 1.0e-10
      assert_in_delta denominator.x, (width - dw) / 2, 1.0e-10
      assert_in_delta measured.ascent, max(up + nymax * scale, axis + thickness / 2), 1.0e-10
      assert_in_delta measured.descent, max(down - dymin * scale, 0), 1.0e-10
      [rule] = Enum.filter(measured.drawing_items, &(&1.kind == :native))
      assert_in_delta rule.box.y + rule.box.height / 2, axis, 1.0e-10
      assert rule.box.fill_color == {1, 0, 0}
      assert rule.box.width == width
      assert measured.actual_text == "1g"

      assert up + nymin * scale - (axis + thickness / 2) >=
               constants[ng_name].value * scale - 1.0e-10
    end
  end

  test "zero rule stacks split a positive gap deficit and never paint a rule", c do
    space = fn attributes ->
      %{
        type: :element,
        namespace: :mathml,
        tag: "mspace",
        attributes: attributes,
        children: [],
        style: c.style
      }
    end

    n = space.(%{"width" => "3pt", "height" => "40pt", "depth" => "20pt"})
    d = space.(%{"width" => "9pt", "height" => "50pt", "depth" => "30pt"})
    constants = c.metrics.constants
    scale = 20 / c.metrics.units_per_em
    top = constants.stack_top_shift_up.value * scale
    bottom = constants.stack_bottom_shift_down.value * scale
    deficit = max(constants.stack_gap_min.value * scale - (bottom - 50 + top - 20), 0)

    assert {:ok, measured} =
             MathLayout.measure(fraction([n, d], c.style, %{"linethickness" => "0"}))

    assert measured.drawing_items == []
    assert measured.width == 9
    assert_in_delta measured.ascent, top + deficit / 2 + 40, 1.0e-10
    assert_in_delta measured.descent, bottom + deficit / 2 + 30, 1.0e-10

    assert {:ok, small} =
             MathLayout.measure(
               fraction([token("mn", "1", c.style), token("mn", "2", c.style)], c.style, %{
                 "linethickness" => "-2pt"
               })
             )

    refute Enum.any?(small.drawing_items, &(&1.kind == :native))
  end

  test "rule thickness uses the selected default, percentage, lengths and invalid fallback", c do
    children = [token("mn", "1", c.style), token("mn", "2", c.style)]
    default = c.metrics.constants.fraction_rule_thickness.value * 20 / c.metrics.units_per_em

    for {value, expected} <- [
          {nil, default},
          {"invalid", default},
          {"thin", default},
          {"200%", 2 * default},
          {"-20%", 0},
          {"2px", 1.5},
          {"0.1em", 2},
          {"1rem", 30},
          {"1ex", nil}
        ] do
      style = Map.put(c.style, :_root_font_size, 30)
      attributes = if value, do: %{"linethickness" => value}, else: %{}

      assert {:ok, prepared} =
               MathLayoutValidator.prepare_element(fraction(children, style, attributes), %{})

      if expected,
        do: assert_in_delta(prepared.rule_thickness, expected, 1.0e-10),
        else: assert(prepared.rule_thickness > 0)
    end
  end

  test "fraction UA depth and padding preserve authored precedence and nested selected scaling",
       c do
    scale = c.metrics.constants.script_percent_scale_down / 100
    scriptscale = c.metrics.constants.script_script_percent_scale_down / 100

    assert {:ok, inline} =
             styled_math(
               "<math style='font-size:20pt'><mfrac><mn>1</mn><mn>2</mn></mfrac></math>"
             )

    assert Enum.all?(inline.glyphs, &(&1.font_size == 20 * scale))
    widths = Enum.map(inline.glyphs, & &1.width)
    assert_in_delta inline.width, Enum.max(widths) + 1.5, 1.0e-10

    [numerator, denominator] = inline.glyphs
    {_left, _bottom, _right, top} = elem(c.bounds, numerator.glyph_id)
    {_left, bottom, _right, _top} = elem(c.bounds, denominator.glyph_id)

    assert_in_delta inline.ascent,
                    numerator.y + top * numerator.font_size / c.font.units_per_em,
                    1.0e-10

    assert_in_delta inline.descent,
                    -denominator.y - bottom * denominator.font_size / c.font.units_per_em,
                    1.0e-10

    assert {:ok, display} =
             styled_math(
               "<math display=block style='font-size:20pt'><mfrac><mn>1</mn><mn>2</mn></mfrac></math>"
             )

    assert Enum.all?(display.glyphs, &(&1.font_size == 20))

    assert {:ok, nested} =
             styled_math(
               "<math style='font-size:20pt'><mfrac><mfrac><mn>1</mn><mn>2</mn></mfrac><mn>3</mn></mfrac></math>"
             )

    assert Enum.map(nested.glyphs, & &1.font_size) == [
             20 * scriptscale,
             20 * scriptscale,
             20 * scale
           ]

    [script_lookup] = c.metrics.substitutions["ssty"]

    assert Enum.map(nested.glyphs, & &1.glyph_id) == [
             Enum.at(script_lookup[c.font.cmap[?1]], 1),
             Enum.at(script_lookup[c.font.cmap[?2]], 1),
             Enum.at(script_lookup[c.font.cmap[?3]], 0)
           ]

    assert {:ok, authored} =
             styled_math(
               "<math style='font-size:20pt'><mfrac style='padding:0'><mn style='math-depth:0;font-size:18pt'>1</mn><mn scriptlevel=0>2</mn></mfrac></math>"
             )

    assert Enum.map(authored.glyphs, & &1.font_size) == [18, 20]
    assert_in_delta authored.width, Enum.max(Enum.map(authored.glyphs, & &1.width)), 1.0e-10

    parent = %{
      namespace: :mathml,
      tag: "mfrac",
      children: [%{type: :element, _selector_id: 1}, %{type: :element, _selector_id: 2}]
    }

    assert MathLayoutValidator.schema_child_defaults(%{_selector_id: 2}, c.style, [parent]).math_shift ==
             :compact

    refute Map.has_key?(
             MathLayoutValidator.schema_child_defaults(%{_selector_id: 1}, c.style, [parent]),
             :math_shift
           )
  end

  test "nested compact fraction matches independently decoded source outline geometry", c do
    # The standalone design API retains the pinned source-outline contract;
    # DOM Math separately follows the Chromium print fragment metrics.
    root_style = Map.merge(c.style, %{font_size: 24, math_depth: 0})
    script = Map.merge(root_style, %{font_size: 16.8, math_depth: 1})
    small = Map.merge(script, %{font_size: 13.2, math_depth: 2})
    padding = %{top: 0.0, right: 0.75, bottom: 0.0, left: 0.75}

    upper =
      fraction(
        [token("mn", "1", small), token("mn", "2", small)],
        Map.put(script, :padding, padding)
      )

    lower = %{
      token("mn", "", script)
      | tag: "mrow",
        children: [token("mn", "34", script), token("mo", "+", script), token("mn", "5", script)]
    }

    equation = %{
      token("mn", "", root_style)
      | tag: "math",
        children: [fraction([upper, lower], Map.put(root_style, :padding, padding))]
    }

    assert {:ok, measured} = MathLayout.measure(equation)
    assert_in_delta measured.width, 48.64826666666666, 1.0e-10
    assert_in_delta measured.ascent, 37.3908, 1.0e-10
    assert_in_delta measured.descent, 14.8128, 1.0e-10
    assert measured.actual_text == "1234+5"

    expected = [
      {4523, 13.2, 20.62153333333333, 28.296},
      {4524, 13.2, 20.63473333333333, 8.64},
      {4276, 16.8, 0.75, -14.04},
      {4277, 16.8, 9.5532, -14.04},
      {4283, 16.8, 22.76173333333333, -14.04},
      {4278, 16.8, 39.46466666666666, -14.04}
    ]

    for {glyph, {gid, size, x, y}} <- Enum.zip(measured.glyphs, expected) do
      assert glyph.glyph_id == gid
      assert_in_delta glyph.font_size, size, 1.0e-10
      assert_in_delta glyph.x, x, 1.0e-10
      assert_in_delta glyph.y, y, 1.0e-10
    end

    html =
      "<math style='font-size:24pt;math-style:compact'><mfrac><mfrac><mn>1</mn><mn>2</mn></mfrac><mrow><mn>34</mn><mo>+</mo><mn>5</mn></mrow></mfrac></math>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, "1234+5"} = Text.extract(pdf, layout: false)
  end

  test "explicit content width centers both children and keeps separate layout and ink", c do
    children = [token("mn", "1", c.style), token("mn", "23", c.style)]
    style = Map.put(c.style, :width, 60)
    assert {:ok, measured} = MathLayout.measure(fraction(children, style))
    assert measured.width == 60
    [numerator | denominator] = measured.glyphs
    assert_in_delta numerator.x + numerator.width / 2, 30, 1.0e-10

    assert_in_delta hd(denominator).x + Enum.sum(Enum.map(denominator, & &1.width)) / 2,
                    30,
                    1.0e-10

    [rule] = Enum.filter(measured.drawing_items, &(&1.kind == :native))
    assert rule.box.width == 60
    assert {:ok, zero} = MathLayout.measure(fraction(children, Map.put(style, :width, 0)))
    refute Enum.any?(zero.drawing_items, &(&1.kind == :native))
    assert length(zero.glyphs) == 3

    assert {:error, {:invalid_layout, %{stage: :layout}}} =
             MathLayout.measure(fraction(children, Map.put(style, :width, -1)))
  end

  test "non-two-child fractions use the prescribed row and retain original source", c do
    for children <- [
          [],
          [token("mn", "1", c.style)],
          [token("mn", "1", c.style), token("mn", "2", c.style), token("mn", "3", c.style)]
        ] do
      assert {:ok, prepared} =
               MathLayoutValidator.prepare_element(fraction(children, c.style), %{})

      assert prepared.kind == :row
      assert {:ok, measured} = MathLayout.measure(fraction(children, c.style))
      assert Enum.all?(measured.glyphs, &(&1.y == 0))
      refute Enum.any?(measured.drawing_items, &(&1.kind == :native))
    end

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<math><mfrac>text</mfrac><mfrac><mn>1</mn><mn>2</mn><mn>3</mn></mfrac></math>"
             )

    assert {:ok, "123"} = Text.extract(pdf, layout: false)
  end

  test "public fractions preserve nested logical text, colors and HTML link composition" do
    html =
      "<p>Before <math><mfrac mathcolor=red><mrow><mn>123</mn><mo>+</mo><mn>4</mn></mrow><mfrac linethickness=0><mn>5</mn><mtext><a href='https://fraction.test/'>Link</a></mtext></mfrac></mfrac><mo>&#x2062;</mo><mn>6</mn></math> After</p>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "123+45Link⁢6"
    assert pdf =~ "https://fraction.test/"
    assert {:ok, 1} = NativeElixirPdfUtilities.Info.page_count(pdf)
  end

  test "zero-rule fractions with only invisible operators retain authored Unicode" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<math><mfrac linethickness=0><mo>&#x2062;</mo><mo>&#x2061;</mo></mfrac></math>"
             )

    assert {:ok, "⁢⁡"} = Text.extract(pdf, layout: false)
    assert {:ok, plain} = HtmlToPdf.render("<math><mo>&#x2062;</mo><mo>&#x2061;</mo></math>")
    assert {:ok, "⁢⁡"} = Text.extract(plain, layout: false)
  end

  test "fraction embellishment passes only numerator constraints and applies spacing once", c do
    plus = %{token("mo", "+", c.style) | attributes: %{"lspace" => "3pt", "rspace" => "7pt"}}
    f = fraction([plus, token("mn", "2", c.style)], c.style)

    row = %{
      type: :element,
      namespace: :mathml,
      tag: "math",
      attributes: %{},
      style: c.style,
      children: [f, token("mn", "3", c.style)]
    }

    assert {:ok, prepared} = MathLayoutValidator.prepare_element(row, %{})
    [f, _] = prepared.children
    assert f.core_operator.form == :prefix
    assert hd(f.children).core_operator == f.core_operator
    assert {:ok, measured} = MathLayout.measure(row)
    assert measured.actual_text == "+23"
    assert {:ok, bare} = MathLayout.measure(fraction([plus, token("mn", "2", c.style)], c.style))
    three_width = Enum.at(c.font.widths, c.font.cmap[?3]) * 20 / c.metrics.units_per_em
    assert_in_delta measured.width, bare.width + three_width + 10, 1.0e-10

    arrow = token("mo", "⏞", c.style)

    assert {:ok, stretched} =
             MathLayout.measure(fraction([arrow, arrow], c.style), %{inline_stretch_size: 40})

    numerator = Enum.filter(stretched.glyphs, &(&1.y > 0))
    denominator = Enum.filter(stretched.glyphs, &(&1.y < 0))
    assert Enum.sum(Enum.map(numerator, & &1.width)) > Enum.sum(Enum.map(denominator, & &1.width))
  end

  test "fraction public resource failures return directly inside existing cumulative scopes", c do
    alias NativeElixirPdfUtilities.Validators.{FontValidator, HtmlValidator}
    children = [token("mn", "1", c.style), token("mn", "2", c.style)]

    for kind <- [:font, :layout] do
      limits = Limits.effective()

      limited =
        if kind == :font,
          do: %{limits | max_font_math_work: 1},
          else: %{limits | max_layout_boxes: 1}

      Limits.install(limited)

      scope =
        if kind == :font,
          do: &FontValidator.with_budget/1,
          else: &HtmlValidator.with_render_budget/1

      result =
        scope.(fn ->
          result = MathLayout.measure(fraction(children, c.style))
          Process.put(:fraction_caller_resumed, true)
          result
        end)

      assert {:error, {:resource_limit_exceeded, diagnostic}} = result
      assert diagnostic.operation == :measure_math
      assert Process.delete(:fraction_caller_resumed) == true
      Limits.install(limits)
    end
  end

  test "fraction validator owns malformed prepared shape and shared length parsing", c do
    alias NativeElixirPdfUtilities.Validators.MathFractionValidator
    assert {:error, {:invalid_document, diagnostic}} = MathFractionValidator.prepare(nil, %{})
    assert diagnostic.stage == :layout
    assert diagnostic.operation == :measure_math
    assert diagnostic.source == "mfrac"
    assert diagnostic.message =~ "prepared numerator and denominator"

    assert {:ok, prepared} =
             MathLayoutValidator.prepare_element(
               fraction([token("mn", "1", c.style), token("mn", "2", c.style)], c.style),
               %{}
             )

    assert {:ok, parsed} = MathLayoutValidator.math_length("1ex", c.style)
    assert parsed > 0
    assert prepared.rule_thickness > 0
  end

  test "public malformed fraction state, oversized geometry and missing font metrics diagnose",
       c do
    children = [token("mn", "1", c.style), token("mn", "2", c.style)]

    assert {:error, {_, diagnostic}} =
             MathLayout.measure(fraction(children, %{c.style | math_style: :invalid}))

    assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
    assert diagnostic.message =~ "normal or compact"

    assert {:ok, ordinary} =
             HtmlToPdf.render(
               "<math><mfrac style='display:block'><mn>1</mn><mn>2</mn></mfrac></math>"
             )

    assert {:ok, "12"} = Text.extract(ordinary, layout: false)

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlToPdf.render(
               "<math><mfrac linethickness=1000pt><mn>1</mn><mn>2</mn></mfrac></math>",
               page_size: {100, 100},
               margin: 0
             )

    assert diagnostic.message =~ "exceeds"

    assert {:error, {:unsupported_glyph, missing}} =
             HtmlToPdf.render("<math><mfrac><mn>1</mn><mn>\uE777</mn></mfrac></math>")

    assert missing.stage == :layout
    assert missing.operation == :measure_math
    assert missing.message =~ "selected math font has no usable glyph for U+E777"

    Limits.install(%{Limits.effective() | max_font_math_work: 1})

    assert {:error, {:resource_limit_exceeded, %{stage: :font}}} =
             MathLayout.measure(fraction(children, c.style))
  end
end
