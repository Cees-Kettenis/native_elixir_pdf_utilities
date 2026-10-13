defmodule NativeElixirPdfUtilities.HtmlToPdf.MathLayoutRadicalsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, MathLayout, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.{FontValidator, HtmlValidator, MathLayoutValidator}

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

  defp element(tag, children, style, attributes \\ %{}) do
    %{
      type: :element,
      namespace: :mathml,
      tag: tag,
      children: children,
      style: style,
      attributes: attributes
    }
  end

  defp token(text, style) do
    element("mn", [%{type: :text, text: text, style: style}], style)
  end

  defp styled_math(html) do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _context ->
      with {:ok, dom} <- HtmlParser.parse_detailed(html),
           {:ok, %{children: [node]}} <- Style.compute_detailed(dom, system_font_discovery: false) do
        {:ok, node.style._math_box}
      end
    end)
  end

  defp links(box) do
    List.wrap(Map.get(box, :link_url)) ++ Enum.flat_map(Map.get(box, :boxes, []), &links/1)
  end

  test "square roots select font variants by Core advance and align bar and base independently",
       c do
    for mode <- [:compact, :normal], text <- ["2", "g"] do
      style = %{c.style | math_style: mode}
      assert {:ok, measured} = MathLayout.measure(element("msqrt", [token(text, style)], style))
      constants = c.metrics.constants
      scale = 20 / c.metrics.units_per_em

      gap =
        constants[
          if(mode == :compact,
            do: :radical_vertical_gap,
            else: :radical_display_style_vertical_gap
          )
        ].value * scale

      thickness = constants.radical_rule_thickness.value * scale
      extra = constants.radical_extra_ascender.value * scale
      {_, ymin, _, ymax} = elem(c.bounds, c.font.cmap[String.to_charlist(text) |> hd()])
      target = (ymax - ymin) * scale + gap + thickness
      construction = c.metrics.variants.vertical[c.font.cmap[0x221A]]
      natural = elem(c.bounds, c.font.cmap[0x221A])

      selected =
        if (elem(natural, 3) - elem(natural, 1)) * scale >= target,
          do: c.font.cmap[0x221A],
          else: Enum.find(construction.variants, &(&1.advance * scale >= target)).glyph

      {_, symin, _, symax} = elem(c.bounds, selected)
      ascent = max(ymax * scale, ymax * scale + gap + thickness + extra)
      descent = max(-ymin * scale, (symax - symin) * scale + extra - ascent)
      [surd, base] = measured.glyphs
      [rule] = Enum.filter(measured.drawing_items, &(&1.kind == :native))
      assert surd.glyph_id == selected
      assert surd.color == {1, 0, 0}
      assert_in_delta surd.y, ascent - extra - symax * scale, 1.0e-10
      assert_in_delta base.x, surd.width, 1.0e-10
      assert base.y == 0
      assert_in_delta measured.ascent, ascent, 1.0e-10
      assert_in_delta measured.descent, descent, 1.0e-10
      assert_in_delta rule.box.y, ascent - extra - thickness, 1.0e-10
      assert rule.box.width == base.width and rule.box.x == base.x
      assert rule.box.fill_color == {1, 0, 0}
      assert measured.actual_text == text
      assert Enum.find(measured.drawing_items, &(&1.kind == :glyph)).logical_text == ""

      ids =
        [c.font.cmap[0x221A]] ++
          Enum.map(construction.variants, & &1.glyph) ++
          Enum.map(construction.assembly.parts, & &1.glyph)

      preferred = Enum.max(Enum.map(ids, &(Enum.at(c.font.widths, &1) * scale)))
      assert_in_delta measured.min_content_width, preferred + base.width, 1.0e-10
    end
  end

  test "caller-selected font without radical construction paints its original surd", c do
    # Remove only optional variant coverage in real source bytes. All outlines,
    # advances, constants and glyph IDs still come through the ordinary parser.
    <<_version::32, count::16, _search::binary-size(6), directory::binary-size(count * 16),
      _::binary>> = c.font.data

    [{math_offset, math_length}] =
      for <<tag::binary-size(4), _checksum::32, offset::32, length::32 <- directory>>,
          tag == "MATH",
          do: {offset, length}

    <<_major::16, _minor::16, _constants::16, _glyphs::16, variants::16>> =
      binary_part(c.font.data, math_offset, 10)

    start = math_offset + variants
    <<overlap::16, _::binary>> = binary_part(c.font.data, start, 10)
    <<prefix::binary-size(^start), _old::binary-size(10), suffix::binary>> = c.font.data
    data = prefix <> <<overlap::16, 0::16, 0::16, 0::16, 0::16>> <> suffix

    checksum = fn bytes ->
      padded = bytes <> :binary.copy(<<0>>, rem(4 - rem(byte_size(bytes), 4), 4))

      Enum.reduce(for(<<word::32 <- padded>>, do: word), 0, fn word, sum ->
        Bitwise.band(sum + word, 0xFFFFFFFF)
      end)
    end

    records = for <<record::binary-size(16) <- directory>>, do: record
    table_index = Enum.find_index(records, &match?(<<"MATH", _::binary>>, &1))
    record_offset = 12 + table_index * 16 + 4
    math_checksum = checksum.(binary_part(data, math_offset, math_length))
    <<before_record::binary-size(^record_offset), _old_checksum::32, after_record::binary>> = data
    data = before_record <> <<math_checksum::32>> <> after_record

    [{head_offset, _}] =
      for <<"head", _checksum::32, offset::32, length::32 <- directory>>, do: {offset, length}

    adjustment_offset = head_offset + 8
    <<before_head::binary-size(^adjustment_offset), _adjustment::32, after_head::binary>> = data
    zeroed = before_head <> <<0::32>> <> after_head
    adjustment = Bitwise.band(0xB1B0AFBA - checksum.(zeroed), 0xFFFFFFFF)
    data = before_head <> <<adjustment::32>> <> after_head
    assert checksum.(data) == 0xB1B0AFBA

    assert {:ok, registry} =
             Font.load_registry(
               fonts: [%{family: "Selected natural radical", data: data}],
               system_font_discovery: false
             )

    assert {:ok, _, font} = Font.resolve("Selected natural radical", 400, :normal, registry)
    assert font.data == data
    assert {:ok, metrics} = Font.math_metrics(font)
    assert metrics.variants.vertical == %{}
    style = %{c.style | font_face: font}
    space = element("mspace", [], style, %{"width" => "30pt", "height" => "100pt"})
    assert {:ok, measured} = MathLayout.measure(element("msqrt", [space], style))
    assert [surd] = measured.glyphs
    assert surd.glyph_id == font.cmap[0x221A]
    assert surd.font_face.data == data

    assert_in_delta surd.width,
                    Enum.at(font.widths, surd.glyph_id) * 20 / font.units_per_em,
                    1.0e-10

    assert_in_delta measured.min_content_width, surd.width + 30, 1.0e-10
    assert measured.actual_text == ""

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<math style='font-family:Selected natural radical'><msqrt><mn>2</mn></msqrt></math>",
               fonts: [%{family: "Selected natural radical", data: data}],
               system_font_discovery: false
             )

    assert {:ok, "2"} = Text.extract(pdf, layout: false)
  end

  test "square root anonymous base never repeats its owner's background or dimensions", c do
    style =
      c.style
      |> Map.put(:background_color, {0.2, 0.4, 0.6})
      |> Map.put(:width, 60)
      |> Map.put(:height, 40)
      |> Map.put(:padding, %{top: 2, right: 3, bottom: 4, left: 5})

    root = element("msqrt", [token("2", c.style)], style)
    assert {:ok, prepared} = MathLayoutValidator.prepare_element(root, %{})
    refute Map.has_key?(prepared.base.style, :background_color)
    refute Map.has_key?(prepared.base.style, :width)
    refute Map.has_key?(prepared.base.style, :height)
    assert {:ok, measured} = MathLayout.measure(element("mrow", [root], c.style))
    assert measured.width == 68
    assert_in_delta measured.ascent + measured.descent, 46, 1.0e-10

    assert [%{width: width, height: height}] =
             Enum.filter(measured.drawing_items, &(&1.kind == :background))

    assert width == 68 and height == 46
    assert measured.actual_text == "2"
  end

  test "large bases use source assembly parts, one empty logical group and no row constraint",
       c do
    space =
      element("mspace", [], c.style, %{"width" => "30pt", "height" => "100pt", "depth" => "30pt"})

    root = element("msqrt", [space], c.style)
    assert {:ok, measured} = MathLayout.measure(root)
    assert {:ok, constrained} = MathLayout.measure(root, %{block_stretch_size: {1, 1}})
    assert Map.drop(measured, [:drawing_items]) == Map.drop(constrained, [:drawing_items])
    parts = c.metrics.variants.vertical[c.font.cmap[0x221A]].assembly.parts
    assert length(measured.glyphs) > length(parts)

    assert MapSet.new(Enum.map(measured.glyphs, & &1.glyph_id)) ==
             MapSet.new(Enum.map(parts, & &1.glyph))

    glyph_items = Enum.filter(measured.drawing_items, &(&1.kind == :glyph))
    assert length(Enum.uniq(Enum.map(glyph_items, & &1.logical_group))) == 1
    assert Enum.all?(glyph_items, &(&1.logical_text == ""))
    [first | _] = measured.glyphs
    {_, ymin, _, _} = elem(c.bounds, first.glyph_id)
    assert_in_delta first.y + ymin * 20 / c.metrics.units_per_em, -measured.ink_descent, 1.0e-10
  end

  test "index kern clamps, baseline raise and dated Core line metrics use selected constants",
       c do
    base = token("16", c.style)
    index_style = %{c.style | font_size: 10}
    index = token("3", index_style)
    assert {:ok, square} = MathLayout.measure(element("msqrt", [base], c.style))
    assert {:ok, degree} = MathLayout.measure(index)
    assert {:ok, root} = MathLayout.measure(element("mroot", [base, index], c.style))
    scale = 20 / c.metrics.units_per_em
    before = max(0, c.metrics.constants.radical_kern_before_degree.value * scale)
    after_degree = max(-degree.width, c.metrics.constants.radical_kern_after_degree.value * scale)

    raise =
      c.metrics.constants.radical_degree_bottom_raise_percent / 100 *
        (square.ascent + square.descent)

    [surd | _] = root.glyphs
    index_glyph = List.last(root.glyphs)
    assert_in_delta surd.x, before + degree.width + after_degree, 1.0e-10
    assert_in_delta index_glyph.x, before, 1.0e-10
    assert_in_delta index_glyph.y, -square.descent + raise + degree.descent, 1.0e-10

    assert_in_delta root.ascent,
                    max(square.ascent, -square.descent + raise + degree.ascent),
                    1.0e-10

    assert_in_delta root.descent,
                    max(square.descent, square.descent - raise + degree.ascent),
                    1.0e-10

    assert root.actual_text == "163"
  end

  test "actual HTML applies root depth, compact shift, authored precedence and child classification",
       c do
    assert {:ok, root} =
             styled_math(
               "<math style='font-size:20pt'><mroot><mn>16</mn><mn>3</mn></mroot></math>"
             )

    assert List.last(root.glyphs).font_size ==
             20 * c.metrics.constants.script_script_percent_scale_down / 100

    assert {:ok, authored} =
             styled_math(
               "<math style='font-size:20pt'><mroot><mn>16</mn><mn style='math-depth:0;font-size:18pt'>3</mn></mroot></math>"
             )

    assert List.last(authored.glyphs).font_size == 18

    assert MathLayoutValidator.defaults(%{tag: "msqrt", attributes: %{}}, c.style).math_shift ==
             :compact

    assert MathLayoutValidator.defaults(%{tag: "mroot", attributes: %{}}, c.style).math_shift ==
             :compact

    assert MathLayoutValidator.schema_child_defaults(%{}, c.style, [
             %{namespace: :mathml, tag: "mroot", children: []}
           ]) == %{}

    for html <- [
          "<mroot/>",
          "<mroot><mn>1</mn></mroot>",
          "<mroot><mn>1</mn><mn>2</mn><mn>3</mn></mroot>"
        ] do
      assert {:ok, row} = styled_math("<math>#{html}</math>")
      refute Enum.any?(row.glyphs, &(&1.text == "√"))
    end

    assert {:ok, hidden} =
             styled_math(
               "<math><mroot><mn>1</mn><mn>2</mn><mn style='display:none'>3</mn></mroot></math>"
             )

    assert hidden.actual_text == "12" and hd(hidden.glyphs).text == "√"
    assert {:ok, anonymous} = styled_math("<math><msqrt>1<mo>+</mo><mn>2</mn></msqrt></math>")
    assert anonymous.actual_text == "1+2"
  end

  test "square anonymous row decoration stays local and empty roots have no invented text", c do
    n = token("2", c.style)

    assert {:ok, plain} =
             MathLayout.measure(element("mrow", [element("msqrt", [n], c.style)], c.style))

    style = Map.put(c.style, :padding, %{top: 2, right: 3, bottom: 4, left: 5})

    assert {:ok, padded} =
             MathLayout.measure(element("mrow", [element("msqrt", [n], style)], c.style))

    assert_in_delta padded.width, plain.width + 8, 1.0e-10
    assert_in_delta hd(padded.glyphs).x, hd(plain.glyphs).x + 5, 1.0e-10
    assert {:ok, empty} = HtmlToPdf.render("<math><msqrt/></math>")
    assert {:error, {:no_extractable_text, _}} = Text.extract(empty)
  end

  test "native HTML links and nested fractions keep ordered original Unicode and paint" do
    html =
      "<p>Before <math><mroot mathcolor=blue><msqrt><mfrac><mn>12</mn><mtext><a href='https://radical.test/'>Link</a></mtext></mfrac><mo>&#x2062;</mo><mn>4</mn></msqrt><mn>3</mn></mroot></math> After</p>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "https://radical.test/"
    assert {:ok, text} = Text.extract(pdf, layout: false)
    compact = String.replace(text, ~r/\s/u, "")
    assert compact == "Before12Link\u206243After"

    assert {:ok, measured} =
             styled_math(
               "<math><msqrt><mtext><a href='https://radical.test/'>Link</a></mtext></msqrt></math>"
             )

    [rule, surd | _] = measured.drawing_items
    assert rule.kind == :native and surd.kind == :glyph

    assert "https://radical.test/" in Enum.flat_map(
             measured.drawing_items,
             &links(Map.get(&1, :box, %{}))
           )
  end

  test "radical resource errors return inside the caller's cumulative scopes", c do
    root = element("msqrt", [token("2", c.style)], c.style)

    for kind <- [:font, :layout] do
      limits = Limits.effective()

      Limits.install(
        if(kind == :font,
          do: %{limits | max_font_math_work: 1},
          else: %{limits | max_layout_boxes: 1}
        )
      )

      scope =
        if kind == :font,
          do: &FontValidator.with_budget/1,
          else: &HtmlValidator.with_render_budget/1

      result =
        scope.(fn ->
          result = MathLayout.measure(root)
          Process.put(:radical_caller_resumed, true)
          result
        end)

      assert {:error, {:resource_limit_exceeded, diagnostic}} = result
      assert diagnostic.operation == :measure_math
      assert diagnostic.message != "" and diagnostic.stage in [:layout, :font]
      assert Process.delete(:radical_caller_resumed) == true
      Limits.install(limits)
    end
  end

  test "radical validator rejects malformed and excessive measured state with actionable diagnostics",
       c do
    alias NativeElixirPdfUtilities.Validators.MathRadicalValidator
    assert {:error, {:invalid_document, diagnostic}} = MathRadicalValidator.prepare(nil, nil, nil)
    assert diagnostic.stage == :layout
    assert diagnostic.operation == :measure_math
    assert diagnostic.source == "msqrt/mroot"
    assert diagnostic.message =~ "prepared base"

    assert {:ok, prepared} =
             MathLayoutValidator.prepare_element(
               element("msqrt", [token("2", c.style)], c.style),
               %{}
             )

    assert {:error, {:invalid_document, diagnostic}} =
             MathRadicalValidator.prepare_stretch(prepared, %{
               ink_ascent: Limits.get(:max_css_numeric_magnitude),
               ink_descent: 1
             })

    assert diagnostic.message =~ "numeric bounds"
  end

  test "malformed states, unsupported CSS, missing glyphs and oversized roots diagnose", c do
    assert {:error, {_, diagnostic}} = MathLayoutValidator.prepare_radical_stretch(nil, nil)
    assert diagnostic.operation == :measure_math and diagnostic.stage == :layout

    assert {:error, {_, diagnostic}} =
             MathLayout.measure(element("msqrt", [], %{c.style | math_style: :invalid}))

    assert diagnostic.message =~ "normal or compact"

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render("<math><msqrt style='display:block'><mn>2</mn></msqrt></math>")

    assert diagnostic.source == "msqrt"

    assert {:error, {:unsupported_glyph, diagnostic}} =
             HtmlToPdf.render("<math><msqrt><mn>\uE777</mn></msqrt></math>")

    assert diagnostic.source == "\uE777"

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlToPdf.render("<math><msqrt><mspace height='1000pt'/></msqrt></math>",
               page_size: {100, 100},
               margin: 0
             )

    assert diagnostic.message =~ "exceeds"
  end
end
