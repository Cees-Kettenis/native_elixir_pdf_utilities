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

  test "discarded row text does not affect Chromium operator form", %{
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
    assert measured.actual_text == "+"
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

  test "unconstrained vertical operators retain natural paint and selected-font preferred widths",
       %{node: n, font: font} do
    assert {:ok, metrics} = Font.math_metrics(font)
    operator = n.("mo", "(", %{})
    assert {:ok, measured} = MathLayout.measure(operator)
    assert {:ok, natural} = MathLayout.glyph("(", font, 20)
    assert measured.glyphs == Enum.map(natural.glyphs, &Map.put(&1, :color, {0, 0, 0}))
    assert measured.actual_text == "("
    construction = metrics.variants.vertical[font.cmap[?(]]

    ids =
      [font.cmap[?(]] ++
        Enum.map(construction.variants, & &1.glyph) ++
        Enum.map(construction.assembly.parts, & &1.glyph)

    preferred = Enum.max(Enum.map(ids, &(Enum.at(font.widths, &1) * 20 / font.units_per_em)))
    assert_in_delta measured.min_content_width, preferred, 1.0e-10
    assert_in_delta measured.max_content_width, preferred, 1.0e-10
    assert measured.width < preferred
  end

  test "operator font lengths use the core face and size through outer embellishment", %{
    node: n,
    row: r,
    font: font,
    style: style
  } do
    attrs = %{"lspace" => " 2CH ", "rspace" => "0.5CAP", "minsize" => "1.5LH"}
    operator = n.("mo", "(", attrs)
    assert {:ok, prepared} = MathLayoutValidator.prepare_element(r.([operator]), %{})
    core = prepared.core_operator
    assert {:ok, zero} = MathLayout.glyph("0", font, 20)
    assert {:ok, capital} = MathLayout.glyph("H", font, 20)
    assert_in_delta core.lspace, 2 * zero.width, 1.0e-10
    assert_in_delta core.rspace, capital.ascent / 2, 1.0e-10
    assert core.minsize == 1.5 * style.line_height

    wrapper = r.([operator])

    assert {:ok, outer} =
             MathLayoutValidator.prepare_element(
               %{wrapper | style: %{style | font_size: 60}},
               %{}
             )

    assert outer.core_operator == core

    assert {:ok, resolved} =
             MathLayoutValidator.prepare_operator_stretch(hd(prepared.children), {0, 0})

    assert_in_delta resolved.target_ascent + resolved.target_descent, core.minsize, 1.0e-10
  end

  test "operator preferred-size and large-variant scans share the font work budget", %{
    node: n,
    style: style
  } do
    assert {:ok, vertical} = MathLayoutValidator.prepare_element(n.("mo", "(", %{}), %{})
    sum = %{n.("mo", "∑", %{}) | style: %{style | math_style: :normal}}
    assert {:ok, large} = MathLayoutValidator.prepare_element(sum, %{})
    Limits.install(Map.put(Limits.effective(), :max_font_math_work, 1))

    for prepared <- [vertical, large] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               MathLayoutValidator.prepare_operator_stretch(prepared, nil)

      assert diagnostic.stage == :font
      assert diagnostic.operation == :measure_math
      assert diagnostic.message =~ "max_font_math_work"
    end
  end

  test "font-relative attributes diagnose absent selected-face metric glyphs", %{
    node: n,
    font: font,
    style: style
  } do
    for {unit, character} <- [{"ch", ?0}, {"cap", ?H}] do
      missing_style = %{style | font_face: %{font | cmap: Map.delete(font.cmap, character)}}
      operator = %{n.("mo", "+", %{"lspace" => "2#{unit}"}) | style: missing_style}
      assert {:error, {:unsupported_glyph, diagnostic}} = MathLayout.measure(operator)
      assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
      assert diagnostic.source == <<character::utf8>>
      assert diagnostic.message =~ "no usable glyph"

      for value <- ["invalid #{unit}", "calc(1#{unit} +2px)"] do
        assert {:ok, fallback} =
                 MathLayout.measure(%{operator | attributes: %{"lspace" => value}})

        assert fallback.core_operator.lspace == 20 * 4 / 18
      end
    end
  end

  test "all-stretch rows use initial ink from each selected font face", %{
    node: n,
    row: r,
    font: font,
    style: style
  } do
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, other} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    assert Base.encode16(:crypto.hash(:sha256, other.data), case: :lower) ==
             "57f73e11f51999432bf7ab22ce55b6f945d5eca1bf824404cfa9ec2e3718c84e"

    {:ok, other_bounds} = Font.glyph_bounds(other)
    {:ok, other_metrics} = Font.math_metrics(other)
    {_, bottom, _, top} = elem(other_bounds, other.cmap[?)])
    height = (top - bottom) * 40 / other.units_per_em
    axis = other_metrics.constants.axis_height.value * 40 / other.units_per_em
    initial_ascent = height / 2 + axis
    initial_descent = height / 2 - axis
    {:ok, metrics} = Font.math_metrics(font)
    small_axis = metrics.constants.axis_height.value * 20 / font.units_per_em
    target = 2 * max(initial_ascent - small_axis, initial_descent + small_axis)

    variant =
      Enum.find(
        metrics.variants.vertical[font.cmap[?(]].variants,
        &(&1.advance * 20 / font.units_per_em >= target)
      )

    assert variant.glyph == 1307
    other_style = %{style | font_face: other, font_size: 40}

    right = %{
      n.("mo", ")", %{})
      | style: other_style,
        children: [%{type: :text, text: ")", style: other_style}]
    }

    assert {:ok, measured} = MathLayout.measure(r.([n.("mo", "(", %{}), right]))
    assert [left, right] = measured.glyphs
    assert left.glyph_id == variant.glyph and left.font_face == font
    assert right.glyph_id == other.cmap[?)] and right.font_face == other
    assert_in_delta right.height, height, 1.0e-8
    assert measured.actual_text == "()"
  end

  test "all-stretch rows relayout to the initial source ink maxima", %{
    node: n,
    row: r,
    font: font,
    style: style
  } do
    assert Base.encode16(:crypto.hash(:sha256, font.data), case: :lower) ==
             "49b22767985171bce579307b21d98f4d35012fa39e316acc4ffcab8644dfb69b"

    {:ok, bounds} = Font.glyph_bounds(font)
    {:ok, metrics} = Font.math_metrics(font)
    operators = [{?(, 20}, {?), 40}]

    initial =
      Enum.map(operators, fn {character, size} ->
        {_, bottom, _, top} = elem(bounds, font.cmap[character])
        height = (top - bottom) * size / font.units_per_em
        axis = metrics.constants.axis_height.value * size / font.units_per_em
        {height / 2 + axis, height / 2 - axis}
      end)

    ascent = Enum.max(Enum.map(initial, &elem(&1, 0)))
    descent = Enum.max(Enum.map(initial, &elem(&1, 1)))

    expected =
      Enum.map(operators, fn {character, size} ->
        scale = size / font.units_per_em
        axis = metrics.constants.axis_height.value * scale
        target = 2 * max(ascent - axis, descent + axis)
        base = font.cmap[character]
        {_, bottom, _, top} = elem(bounds, base)

        id =
          if (top - bottom) * scale >= target do
            base
          else
            Enum.find(metrics.variants.vertical[base].variants, &(&1.advance * scale >= target)).glyph
          end

        {_, bottom, _, top} = elem(bounds, id)

        %{
          id: id,
          width: Enum.at(font.widths, id) * scale,
          height: (top - bottom) * scale,
          y: axis - (top + bottom) * scale / 2
        }
      end)

    assert Enum.map(expected, & &1.id) == [1306, 1065]
    left = n.("mo", "(", %{"lspace" => "3pt", "rspace" => "7pt"})
    large_style = %{style | font_size: 40}

    right = %{
      n.("mo", ")", %{})
      | style: large_style,
        children: [%{type: :text, text: ")", style: large_style}]
    }

    invisible = n.("mo", "\u2062", %{"stretchy" => "true"})

    for children <- [[left, right], [r.([left]), invisible, right]] do
      assert {:ok, measured} = MathLayout.measure(r.(children))
      assert length(measured.glyphs) == 2

      for {glyph, source} <- Enum.zip(measured.glyphs, expected) do
        assert glyph.glyph_id == source.id
        assert_in_delta glyph.width, source.width, 1.0e-8
        assert_in_delta glyph.height, source.height, 1.0e-8
        assert_in_delta glyph.y, source.y, 1.0e-8
      end

      assert_in_delta measured.width, Enum.sum(Enum.map(expected, & &1.width)) + 10, 1.0e-8
      assert_in_delta hd(measured.glyphs).x, 3, 1.0e-8
      assert_in_delta measured.ascent, 29.0, 1.0e-8
      assert_in_delta measured.descent, 18.68, 1.0e-8
      assert_in_delta measured.ink_ascent, measured.ascent, 1.0e-8
      assert_in_delta measured.ink_descent, measured.descent, 1.0e-8

      preferred =
        Enum.sum(
          Enum.map(operators, fn {character, size} ->
            construction = metrics.variants.vertical[font.cmap[character]]

            ids =
              [font.cmap[character]] ++
                Enum.map(construction.variants, & &1.glyph) ++
                Enum.map(construction.assembly.parts, & &1.glyph)

            ids |> Enum.map(&(Enum.at(font.widths, &1) * size / font.units_per_em)) |> Enum.max()
          end)
        ) + 10

      assert_in_delta measured.min_content_width, preferred, 1.0e-8
      assert_in_delta measured.max_content_width, preferred, 1.0e-8
      assert preferred > measured.width

      assert length(
               Enum.uniq(
                 Enum.map(
                   Enum.filter(measured.drawing_items, &(&1.kind == :glyph)),
                   & &1.logical_group
                 )
               )
             ) == 2

      assert measured.actual_text == if(length(children) == 2, do: "()", else: "(\u2062)")

      assert :ok =
               MathLayoutValidator.validate_flow_geometry(
                 [%{style: %{_math_box: measured}}],
                 250,
                 250
               )
    end

    # Empty ordinary boxes and nonstretchy invisible operators belong to the
    # ordinary list even though their ink target is zero.
    for ordinary <- [r.([]), n.("mo", "\u2062", %{})] do
      assert {:ok, measured} = MathLayout.measure(r.([left, ordinary, right]))
      assert Enum.map(measured.glyphs, & &1.glyph_id) == [1064, 1065]
    end

    assert {:ok, constrained} =
             MathLayout.measure(r.([left, right]), %{block_stretch_size: {0, 0}})

    assert Enum.map(constrained.glyphs, & &1.glyph_id) == [1064, 1065]
    assert {:ok, empty} = MathLayout.measure(r.([invisible, invisible]))
    assert empty.glyphs == [] and empty.actual_text == "\u2062\u2062"
    assert length(empty.drawing_items) == 2
  end

  test "large operator selection tests actual outline height independently of stretch advances",
       %{
         node: n,
         style: style,
         font: font
       } do
    assert {:ok, bounds} = Font.glyph_bounds(font)
    assert elem(elem(bounds, 1304), 3) - elem(elem(bounds, 1304), 1) == 1906

    node = %{
      n.("mo", "(", %{"stretchy" => "false", "largeop" => "true"})
      | style: %{style | math_style: :normal}
    }

    assert {:ok, prepared} = MathLayoutValidator.prepare_element(node, %{})
    # A controlled prepared minimum isolates the real STIX 1906/1907 boundary.
    # Original font bytes, source glyph IDs and outline metrics stay unchanged.
    assert Enum.find(
             prepared.source_glyph.source.metrics.variants.vertical[font.cmap[?(]].variants,
             &(&1.glyph == 1304)
           ).advance == 1907

    prepared =
      put_in(prepared.source_glyph.source.metrics.constants.display_operator_min_height, 1907)

    assert {:ok, %{shape: %{glyph: glyph}}} =
             MathLayoutValidator.prepare_operator_stretch(prepared, nil)

    assert glyph.glyph_id == 1305 and glyph.font_face.data == font.data
    assert elem(glyph.bounds, 3) - elem(glyph.bounds, 1) == 2144
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

    for unit <- ["vw", "cqw"] do
      assert {:ok, pdf} = HtmlToPdf.render("<math><mo minsize='2#{unit}'>(</mo></math>")
      assert byte_size(pdf) > 0
    end

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

  test "ignored calculated minmax retains the natural selected glyph minimum", %{
    node: n,
    row: r
  } do
    assert {:ok, natural} = MathLayoutValidator.prepare_element(n.("mo", "(", %{}), %{})
    glyph = natural.source_glyph

    height =
      (elem(glyph.bounds, 3) - elem(glyph.bounds, 1)) * glyph.font_size / glyph.units_per_em

    operator = n.("mo", "(", %{"minsize" => "calc(200% + 4pt)", "maxsize" => "max(100%, 10pt)"})
    assert {:ok, prepared} = MathLayoutValidator.prepare_element(operator, %{})
    assert {:ok, constrained} = MathLayoutValidator.prepare_operator_stretch(prepared, {0, 0})

    assert_in_delta constrained.target_ascent +
                      constrained.target_descent,
                    height,
                    1.0e-10

    assert {:ok, measured} = MathLayout.measure(r.([operator]))
    assert measured.actual_text == "("
  end

  test "ignored calculated spacing requires no root font metrics through embellishment", %{
    node: n,
    row: r,
    style: style
  } do
    operator = n.("mo", "+", %{"lspace" => "calc(1rch + 0.5cap + 0.25rlh + 1vw)"})
    wrapper = %{r.([operator]) | style: %{style | font_size: 40}}
    assert {:ok, prepared} = MathLayoutValidator.prepare_element(wrapper, %{})
    assert_in_delta prepared.core_operator.lspace, style.font_size * 4 / 18, 1.0e-10
  end

  test "viewport context uses actual print dimensions and child client viewport" do
    assert {:ok, context} = MathLayoutValidator.prepare_css_context(print_viewport: {800, 400})
    assert context == %{viewport_width: 600.0, viewport_height: 300.0}

    assert {:ok, context} =
             MathLayoutValidator.prepare_css_context(
               __client_css_viewport__: %{width: 200, height: 100}
             )

    assert context == %{viewport_width: 150.0, viewport_height: 75.0}

    assert {:error, {:invalid_options, diagnostic}} =
             MathLayoutValidator.prepare_css_context(print_viewport: {-1, 400})

    assert diagnostic.stage == :options
    assert diagnostic.message =~ "print_viewport"
  end

  test "styled operators inherit the actual html root face and line height, not body metrics" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

    html = """
    <html style="font-family:DejaVu Sans;font-size:12pt;line-height:24pt">
    <body style="font-size:18pt;line-height:36pt">
    <math style="font-size:20pt"><mo lspace="0.5rlh" rspace="1rch">+</mo></math>
    </body></html>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    [body] = Enum.reject(styled.children, &(&1.type == :text))
    [math] = Enum.reject(body.children, &(&1.type == :text))
    assert math.style._root_font_size == 12
    assert math.style._root_line_height == 24
    assert math.style._root_font_face.family == "DejaVu Sans"
    assert math.style._math_box.core_operator.lspace == 12
    assert {:ok, zero} = MathLayout.glyph("0", math.style._root_font_face, 12)
    assert_in_delta math.style._math_box.core_operator.rspace, zero.width, 1.0e-10
  end
end
