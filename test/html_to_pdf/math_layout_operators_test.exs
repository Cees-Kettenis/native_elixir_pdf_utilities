defmodule NativeElixirPdfUtilities.HtmlToPdf.MathLayoutOperatorsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, MathLayout}
  alias NativeElixirPdfUtilities.Validators.MathLayoutValidator

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
      math_style: :compact,
      text_transform: :none
    }

    node = fn tag, text, attributes ->
      %{
        type: :element,
        namespace: :mathml,
        tag: tag,
        attributes: attributes,
        style: style,
        children: [%{type: :text, text: text, style: style}]
      }
    end

    row = fn children ->
      %{
        type: :element,
        namespace: :mathml,
        tag: "mrow",
        attributes: %{},
        style: style,
        children: children
      }
    end

    {:ok, font: font, style: style, node: node, row: row}
  end

  test "outer embellished form ignores space-like siblings and retains explicit form", %{
    node: n,
    row: r
  } do
    plus = n.("mo", "+", %{})
    wrapped = r.([n.("mtext", "", %{}), r.([plus]), n.("mspace", "", %{})])
    row = r.([n.("mspace", "", %{}), wrapped, n.("mn", "1", %{}), plus])
    assert {:ok, prepared} = MathLayoutValidator.prepare_element(row, %{})
    [_, first, _, last] = prepared.children
    assert first.core_operator.form == :prefix
    assert last.core_operator.form == :postfix
    assert first.children |> Enum.at(1) |> Map.fetch!(:core_operator) == first.core_operator

    explicit = %{
      plus
      | attributes: %{"form" => "INFIX", "fence" => "true", "separator" => "true"}
    }

    assert {:ok, prepared} =
             MathLayoutValidator.prepare_element(r.([explicit, n.("mn", "1", %{})]), %{})

    [first, _] = prepared.children
    assert first.core_operator.explicit_form
    assert first.core_operator.fence and first.core_operator.separator
    assert first.core_operator.lspace == 20 * 4 / 18
  end

  test "straight row text has anonymous mtext space-like semantics before form inference", %{
    node: n,
    row: r,
    style: style
  } do
    text = %{type: :text, text: "words", style: style}
    plus = n.("mo", "+", %{})
    assert {:ok, prepared} = MathLayoutValidator.prepare_element(r.([plus, text]), %{})
    assert prepared.core_operator.form == :infix

    assert {:ok, outer} =
             MathLayoutValidator.prepare_element(r.([r.([text, plus]), n.("mn", "1", %{})]), %{})

    [wrapped, _] = outer.children
    assert wrapped.core_operator.form == :prefix
    assert {:ok, measured} = MathLayout.measure(r.([plus, text]))
    assert measured.actual_text == "+words"
  end

  test "only the outer non-embellished row adds core spacing", %{node: n, row: r, font: font} do
    plus = n.("mo", "+", %{"lspace" => "3pt", "rspace" => "7pt"})
    assert {:ok, bare} = MathLayout.measure(plus)
    assert {:ok, wrapper} = MathLayout.measure(r.([r.([plus])]))
    assert_in_delta wrapper.width, bare.width, 1.0e-10

    assert {:ok, sum} =
             MathLayout.measure(r.([n.("mn", "1", %{}), r.([plus]), n.("mn", "2", %{})]))

    one_width = Enum.at(font.widths, font.cmap[?1]) * 20 / font.units_per_em
    two_width = Enum.at(font.widths, font.cmap[?2]) * 20 / font.units_per_em
    assert_in_delta sum.width, one_width + bare.width + two_width + 10, 1.0e-10
    [_, operator, two] = sum.glyphs
    assert_in_delta operator.x, one_width + 3, 1.0e-10
    assert_in_delta two.x, one_width + 3 + bare.width + 7, 1.0e-10
  end

  test "ordinary ink determines tall paired fence targets before any fence is stretched", %{
    node: n,
    row: r,
    style: style,
    font: font
  } do
    tall = %{
      n.("mspace", "", %{"height" => "110pt", "depth" => "40pt", "width" => "12pt"})
      | children: []
    }

    left = n.("mo", "(", %{})
    right = n.("mo", ")", %{})
    assert {:ok, measured} = MathLayout.measure(r.([r.([left]), tall, right]))
    assert measured.actual_text == "()"
    {:ok, metrics} = Font.math_metrics(font)
    axis = metrics.constants.axis_height.value * style.font_size / font.units_per_em
    assert_in_delta measured.ascent - measured.descent, 2 * axis, 1.0e-8
    assert measured.ascent >= 110 and measured.descent >= 40
    assert length(measured.glyphs) > 2
    assert Enum.any?(measured.glyphs, &(&1.glyph_id != font.cmap[?(]))
    assert measured.min_content_width >= measured.width

    assert :ok =
             MathLayoutValidator.validate_flow_geometry(
               [%{style: %{_math_box: measured}}],
               250,
               250
             )
  end

  test "glyph percentages, negative bounds and max below min normalize in the validator", %{
    node: n,
    row: r,
    font: font
  } do
    {:ok, bounds} = Font.glyph_bounds(font)
    {_, ymin, _, ymax} = elem(bounds, font.cmap[?(])
    natural = (ymax - ymin) * 20 / font.units_per_em
    tall = %{n.("mspace", "", %{"height" => "100pt"}) | children: []}

    for {attributes, expected} <- [
          {%{"minsize" => "200%", "maxsize" => "1pt"}, natural * 2},
          {%{"minsize" => "-1pt", "maxsize" => "10pt"}, 10.0},
          {%{"minsize" => "2em", "maxsize" => "20%"}, 40.0}
        ] do
      assert {:ok, prepared} = MathLayoutValidator.prepare_element(n.("mo", "(", attributes), %{})
      assert {:ok, resolved} = MathLayoutValidator.prepare_operator_stretch(prepared, {100, 0})
      assert_in_delta resolved.target_ascent + resolved.target_descent, expected, 1.0e-8
      assert {:ok, measured} = MathLayout.measure(r.([n.("mo", "(", attributes), tall]))
      assert Enum.all?(measured.glyphs, &is_number(&1.y))
    end

    assert {:ok, prepared} =
             MathLayoutValidator.prepare_element(n.("mo", "(", %{"minsize" => "2ex"}), %{})

    assert {:ok, resolved} = MathLayoutValidator.prepare_operator_stretch(prepared, {0, 0})
    {:ok, x_height} = NativeElixirPdfUtilities.Validators.FontValidator.prepare_x_height(font)
    assert_in_delta resolved.target_ascent + resolved.target_descent, 40 * x_height, 1.0e-8
  end

  @tag :math_operator_unadjusted
  test "in-range asymmetric targets retain their requested ascent even below the baseline", %{
    node: n,
    font: font
  } do
    operator =
      n.("mo", "|", %{
        "stretchy" => "true",
        "symmetric" => "false",
        "minsize" => "0pt",
        "maxsize" => "24pt"
      })

    assert {:ok, prepared} = MathLayoutValidator.prepare_element(operator, %{})

    for {ascent, descent} = target <- [{-2, 14}, {0, 12}, {2, 10}, {6, 6}, {-10, 34}] do
      assert {:ok, resolved} = MathLayoutValidator.prepare_operator_stretch(prepared, target)
      assert_in_delta resolved.target_ascent, ascent, 1.0e-10
      assert_in_delta resolved.target_descent, descent, 1.0e-10
      assert {:ok, measured} = MathLayout.measure(operator, %{block_stretch_size: target})
      assert {:ok, shape} = MathLayout.stretch("|", font, 20, :vertical, ascent + descent)
      shift = (ascent - descent - (shape.ascent - shape.descent)) / 2
      assert Enum.map(measured.glyphs, & &1.glyph_id) == Enum.map(shape.glyphs, & &1.glyph_id)

      for {glyph, shaped} <- Enum.zip(measured.glyphs, shape.glyphs) do
        assert_in_delta glyph.x, shaped.x, 1.0e-10
        assert_in_delta glyph.y, shaped.y + shift, 1.0e-10
        assert glyph.font_face == shaped.font_face and glyph.font_size == shaped.font_size
      end

      assert measured.actual_text == "|"
      assert_in_delta measured.width, shape.width, 1.0e-10
      assert_in_delta measured.ink_ascent - measured.ink_descent, ascent - descent, 1.0e-10
    end
  end

  test "operator min/max adjustment keeps its dated Core axis clamp and shared failures", %{
    node: n,
    font: font
  } do
    {:ok, metrics} = Font.math_metrics(font)
    axis = metrics.constants.axis_height.value * 20 / font.units_per_em

    for {attributes, target, wanted_total} <- [
          {%{"minsize" => "24pt", "maxsize" => "30pt"}, {-2, 14}, 24},
          {%{"minsize" => "0pt", "maxsize" => "8pt"}, {2, 10}, 8},
          {%{"minsize" => "24pt", "maxsize" => "30pt"}, {0, 0}, 24}
        ] do
      operator =
        n.("mo", "|", Map.merge(attributes, %{"symmetric" => "false", "stretchy" => "true"}))

      assert {:ok, prepared} = MathLayoutValidator.prepare_element(operator, %{})
      assert {:ok, resolved} = MathLayoutValidator.prepare_operator_stretch(prepared, target)
      {ascent, descent} = target

      expected =
        if ascent + descent <= 0,
          do: wanted_total / 2 + axis,
          else: max(0, (ascent - axis) * wanted_total / (ascent + descent) + axis)

      assert_in_delta resolved.target_ascent, expected, 1.0e-10
      assert_in_delta resolved.target_ascent + resolved.target_descent, wanted_total, 1.0e-10

      for invalid <- [{:bad, 2}, {1.0e308, 2}, {-1.0e308, 2}] do
        assert {:error, {:invalid_document, diagnostic}} =
                 MathLayoutValidator.prepare_operator_stretch(prepared, invalid)

        assert diagnostic.stage == :layout and diagnostic.reason == :invalid_document
        assert diagnostic.operation == :measure_math and diagnostic.module == MathLayoutValidator
        assert diagnostic.message =~ "bounded point extents"
      end
    end
  end

  test "asymmetric operators preserve target center; all-stretch rows use zero target", %{
    node: n,
    row: r,
    font: font
  } do
    operator = n.("mo", "(", %{"symmetric" => "false", "minsize" => "0pt"})
    tall = %{n.("mspace", "", %{"height" => "35pt", "depth" => "5pt"}) | children: []}
    assert {:ok, measured} = MathLayout.measure(r.([operator, tall]))
    assert_in_delta measured.ascent - measured.descent, 30, 1.0e-8
    assert {:ok, empty_target} = MathLayout.measure(r.([operator, n.("mo", ")", %{})]))
    assert Enum.all?(empty_target.glyphs, &(&1.height >= 0))
    # A selected font without a construction uses text layout, never a blank box.
    plain = n.("mo", "A", %{"stretchy" => "true"})
    assert {:ok, fallback} = MathLayout.measure(r.([plain, tall]))
    assert [%{glyph_id: id}] = fallback.glyphs
    assert id == font.cmap[?A]
  end

  test "large operators use variants only in normal math-style and keep movable-limits metadata",
       %{node: n, style: style, font: font} do
    sum = n.("mo", "∑", %{})
    assert {:ok, compact} = MathLayout.measure(sum)
    assert {:ok, display} = MathLayout.measure(%{sum | style: %{style | math_style: :normal}})
    assert display.ascent + display.descent > compact.ascent + compact.descent
    {:ok, metrics} = Font.math_metrics(font)
    construction = metrics.variants.vertical[font.cmap[0x2211]]

    variant =
      Enum.find(
        construction.variants,
        &(&1.advance >= metrics.constants.display_operator_min_height)
      ) || List.last(construction.variants)

    assert [%{glyph_id: id}] = display.glyphs
    assert id == variant.glyph
    assert display.core_operator.movablelimits

    disabled = %{
      sum
      | attributes: %{"largeop" => "false", "movablelimits" => "false"},
        style: %{style | math_style: :normal}
    }

    assert {:ok, small} = MathLayout.measure(disabled)
    assert small.glyphs == compact.glyphs
    refute small.core_operator.movablelimits
    plain = %{n.("mo", "A", %{"largeop" => "true"}) | style: %{style | math_style: :normal}}
    assert {:ok, fallback} = MathLayout.measure(plain)
    assert [%{glyph_id: id}] = fallback.glyphs
    assert id == font.cmap[?A]
  end

  test "horizontal stretch consumes source assembly geometry and keeps natural intrinsic width",
       %{node: n, font: font} do
    assert {:ok, prepared} = MathLayoutValidator.prepare_element(n.("mo", "⏞", %{}), %{})
    assert prepared.core_operator.stretch_axis == :horizontal
    assert {:ok, resolved} = MathLayoutValidator.prepare_operator_stretch(prepared, 60)
    assert resolved.shape.kind == :assembly
    assert resolved.shape.stretch_size >= 60
    source_scale = 20 / font.units_per_em

    expected =
      Enum.sum(Enum.map(resolved.shape.parts, & &1.full_advance)) * source_scale -
        (length(resolved.shape.parts) - 1) * resolved.shape.overlap

    assert_in_delta resolved.shape.stretch_size, expected, 1.0e-8

    assert_in_delta resolved.intrinsic_width,
                    Enum.at(font.widths, font.cmap[0x23DE]) * 20 / font.units_per_em,
                    1.0e-8

    assert {:ok, natural} = MathLayout.measure(n.("mo", "⏞", %{}))
    assert length(natural.glyphs) == 1
    assert {:ok, arrow} = MathLayoutValidator.prepare_element(n.("mo", "→", %{}), %{})
    assert {:ok, fallback} = MathLayoutValidator.prepare_operator_stretch(arrow, 100)
    assert fallback.shape.kind == :glyph
    {:ok, metrics} = Font.math_metrics(font)

    assert fallback.shape.glyph.glyph_id ==
             List.last(metrics.variants.horizontal[font.cmap[0x2192]].variants).glyph
  end

  test "independent pinned Core references assert selected pieces, actual widths and axis", %{
    node: n,
    style: style
  } do
    manifest =
      :json.decode(
        File.read!(
          Path.expand(
            "../fixtures/html_to_pdf/browser_parity/assets/math-operator-core-reference.json",
            __DIR__
          )
        )
      )

    assert manifest["font_sha256"] ==
             Base.encode16(:crypto.hash(:sha256, style.font_face.data), case: :lower)

    for {key, record} <- manifest["records"] do
      attrs =
        cond do
          key == "bracket16" -> %{"symmetric" => "false"}
          key == "min-bracket16" -> %{"minsize" => "150%", "maxsize" => "150%"}
          true -> %{}
        end

      source_style = %{style | font_size: record["font_size"]}

      node = %{
        n.("mo", record["text"], attrs)
        | style: source_style,
          children: [%{type: :text, text: record["text"], style: source_style}]
      }

      target = if record["font_size"] == 16, do: {35, 12}, else: {65, 25}
      assert {:ok, measured} = MathLayout.measure(node, %{block_stretch_size: target})
      assert_in_delta measured.width, record["width"], 1.0e-8
      assert_in_delta measured.ascent, record["ascent"], 1.0e-8
      assert_in_delta measured.descent, record["descent"], 1.0e-8
      assert measured.actual_text == record["text"]

      assert Enum.map(measured.glyphs, & &1.glyph_id) ==
               Enum.map(record["pieces"], & &1["glyph_id"])

      for {glyph, piece} <- Enum.zip(measured.glyphs, record["pieces"]) do
        assert_in_delta glyph.x, piece["x"], 1.0e-8
        assert_in_delta glyph.y, piece["y"], 1.0e-8
      end

      if key != "bracket16",
        do: assert_in_delta(measured.ascent - measured.descent, 2 * record["axis"], 1.0e-8)
    end
  end

  test "horizontal constraints propagate through a wrapped operator and remain recoverable", %{
    node: n,
    row: r,
    style: style
  } do
    brace = n.("mo", "⏞", %{})
    assert {:ok, direct} = MathLayout.measure(brace, %{inline_stretch_size: 60})
    assert {:ok, wrapped} = MathLayout.measure(r.([brace]), %{inline_stretch_size: 60})
    assert direct.glyphs == wrapped.glyphs
    assert length(direct.glyphs) > 1
    assert direct.min_content_width < direct.width

    for context <- [
          %{inline_stretch_size: 1, block_stretch_size: {2, 3}},
          %{inline_stretch_size: {1, 2}},
          %{block_stretch_size: 1},
          %{block_stretch_size: :invalid}
        ] do
      assert {:error, {:invalid_document, diagnostic}} = MathLayout.measure(brace, context)
      assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
    end

    no_assembly = n.("mo", "∑", %{"stretchy" => "true"})

    assert {:ok, fallback} =
             MathLayout.measure(r.([no_assembly]), %{block_stretch_size: {50, 50}})

    assert length(fallback.glyphs) == 1
    huge_style = %{style | font_size: Limits.get(:max_css_numeric_magnitude)}

    huge = %{
      n.("mo", "(", %{"minsize" => "1000000000%"})
      | style: huge_style,
        children: [%{type: :text, text: "(", style: huge_style}]
    }

    assert {:error, {:invalid_document, diagnostic}} = MathLayout.measure(r.([huge]))
    assert diagnostic.message =~ "bounded nonnegative target"
    assert diagnostic.operation == :measure_math
  end

  test "multi-character and HTML operators reuse ordinary token fallback", %{
    node: n,
    style: style
  } do
    token = n.("mo", "!=", %{})
    assert {:ok, measured} = MathLayout.measure(token)
    assert measured.actual_text == "!=" and length(measured.glyphs) == 2

    child = %{
      type: :element,
      namespace: :html,
      tag: "span",
      attributes: %{},
      style: Map.put(style, :display, :inline),
      children: [%{type: :text, text: "b", style: style}]
    }

    mixed = %{token | children: [%{type: :text, text: "a", style: style}, child]}
    assert {:ok, measured} = MathLayout.measure(mixed)
    assert measured.actual_text == "ab" and measured.descent > 0

    assert {:ok, hidden} =
             MathLayout.measure(%{token | style: Map.put(style, :_math_formatting, false)})

    assert hidden.actual_text == "!="
  end

  test "operator API rejects invalid transport and reports missing required glyphs", %{
    node: n,
    font: font
  } do
    for {prepared, constraint} <- [{nil, nil}, {%{}, 1}, {%{kind: :operator}, {1, 1}}] do
      assert {:error, {:invalid_document, diagnostic}} =
               MathLayoutValidator.prepare_operator_stretch(prepared, constraint)

      assert diagnostic.stage == :layout and diagnostic.reason == :invalid_document
      assert diagnostic.message =~ "validator-prepared"
    end

    assert {:ok, prepared} = MathLayoutValidator.prepare_element(n.("mo", "(", %{}), %{})

    for invalid <- [{:bad, 0}, {1, 2, 3}, -1, 1.0e20] do
      assert {:error, {:invalid_document, diagnostic}} =
               MathLayoutValidator.prepare_operator_stretch(prepared, invalid)

      assert diagnostic.message =~ "constraints"
    end

    missing = %{font | cmap: Map.delete(font.cmap, ?+)}
    operator = n.("mo", "+", %{})

    assert {:error, {:unsupported_glyph, diagnostic}} =
             MathLayout.measure(%{
               operator
               | style: %{operator.style | font_face: missing},
                 children: [
                   %{type: :text, text: "+", style: %{operator.style | font_face: missing}}
                 ]
             })

    assert diagnostic.source == "+" and diagnostic.operation == :measure_math
    assert diagnostic.message =~ "no usable glyph"

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlToPdf.render("<math><mo minsize='2vw'>(</mo></math>")

    assert diagnostic.module == NativeElixirPdfUtilities.Validators.MathOperatorValidator
    assert diagnostic.source == "2vw"
    Limits.install(Map.put(Limits.effective(), :max_css_numeric_magnitude, 100))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<math><mo minsize='100pc'>(</mo></math>")

    assert diagnostic.reason == :resource_limit_exceeded

    # A bounded explicit media context also reaches the operator's own limit,
    # rather than stopping at the derived page viewport's numeric reservation.
    assert {:error, {:resource_limit_exceeded, operator_diagnostic}} =
             HtmlToPdf.render("<math><mo minsize='100pc'>(</mo></math>", print_viewport: {80, 80})

    assert operator_diagnostic.message =~ "max_css_numeric_magnitude"
  end

  test "public operators use caller font bytes and diagnose missing glyphs or MATH data" do
    bytes = File.read!(Path.expand("../../priv/fonts/stix/STIXTwoMath-Regular.ttf", __DIR__))

    html =
      "<math style='font-family:Caller Operators'><mo>(</mo><mspace height='90pt' depth='30pt'/><mn>12</mn><mo>)</mo><mo>+</mo><mn>3</mn></math>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html, fonts: [%{family: "Caller Operators", data: bytes}])

    assert {:ok, "(12)+3"} = Text.extract(pdf, layout: false)

    assert {:error, {:unsupported_glyph, diagnostic}} =
             HtmlToPdf.render("<math><mo>😀</mo></math>")

    assert diagnostic.source == "😀" and diagnostic.message =~ "selected math font"

    assert {:error, {_, diagnostic}} =
             HtmlToPdf.render("<math style='font-family:DejaVu Sans Mono'><mo>+</mo></math>")

    assert diagnostic.message =~ "MATH"
    assert diagnostic.reason == :invalid_document and diagnostic.stage == :font
  end

  test "public operator rendering preserves original text once through assemblies and invisible operators" do
    html = """
    <p>Before <math><mo>(</mo><mspace height='40pt' depth='12pt' width='4pt'/><mn>1</mn><mo>+</mo><mn>2</mn><mo>)</mo></math> After</p>
    <math display='block'><mo>∑</mo><mn>3</mn><mo>=</mo><mn>4</mn></math>
    <math><mn>5</mn><mo>&#x2061;</mo><mo>&#x2062;</mo><mo>&#x2063;</mo><mo>&#x2064;</mo><mn>6</mn><mo>!=</mo><mn>7</mn><mo></mo></math>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "(1+2)"
    assert text =~ "Before" and text =~ "After"
    assert text =~ "∑3=4"
    assert text =~ "5\u2061\u2062\u2063\u20646!=7"
  end
end
