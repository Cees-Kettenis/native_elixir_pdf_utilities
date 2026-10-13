defmodule NativeElixirPdfUtilities.HtmlToPdf.MathLayoutScriptsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, MathLayout, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, MathLayoutValidator}

  @geometry Path.expand("../fixtures/mathml_scripts_geometry.json", __DIR__)

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
      math_shift: :normal,
      text_transform: :none
    }

    {:ok, font: font, style: style}
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

  defp token(text, style, tag \\ "mn") do
    element(tag, [%{type: :text, text: text, style: style}], style)
  end

  defp rational(value) do
    case value do
      %{"numerator" => n, "denominator" => d} -> n / d
      number -> number
    end
  end

  defp source_tree(tree, style) do
    style =
      Map.merge(style, %{
        font_size: rational(tree["style"]["font_size"]),
        math_depth: tree["style"]["math_depth"],
        math_style: String.to_existing_atom(tree["style"]["math_style"]),
        math_shift: String.to_existing_atom(tree["style"]["math_shift"]),
        text_transform: if(tree["tag"] == "mi", do: :math_auto, else: :none)
      })

    attributes =
      Map.new(tree["attributes"], fn {key, value} ->
        {key, if(key in ["width", "height", "depth"], do: value <> "pt", else: value)}
      end)

    children =
      if Map.has_key?(tree, "text"),
        do: [%{type: :text, text: tree["text"], style: style}],
        else: Enum.map(tree["children"], &source_tree(&1, style))

    element(tree["tag"], children, style, attributes)
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

  test "all 15 original authored public equations retain their logical Unicode" do
    html =
      File.read!(
        Path.expand("../fixtures/html_to_pdf/browser_parity/mathml_scripts.html", __DIR__)
      )

    expected = [
      {"Sub", "x1"},
      {"Super", "f2"},
      {"Pair", "x12"},
      {"Limits", "∑1n"},
      {"Movable", "∑1n"},
      {"Under", "7_"},
      {"Over", "x^"},
      {"Nested", "xi23"},
      {"Linked", "BaseLowUp"},
      {"Assembly", "⏞"},
      {"Variant", "⏞"},
      {"Stack", "⏞12"},
      {"Embellished", "(12)"},
      {"Cramped", "f2"},
      {"Authored", "73"}
    ]

    for {name, text} <- expected do
      [_, math] = Regex.run(~r/<p>#{name} (<math\b[^>]*>.*?<\/math>) end\.<\/p>/s, html)
      assert {:ok, measured} = styled_math(math)
      assert measured.actual_text == text
      assert measured.drawing_items != []
      assert :ok = MathLayoutValidator.validate_measured_geometry(measured)
      assert {:ok, pdf} = HtmlToPdf.render(math, system_font_discovery: false)
      assert {:ok, ^text} = Text.extract(pdf, layout: false)
    end
  end

  test "21 independent pinned source witnesses retain selected glyphs, point origins and line/ink distinction",
       c do
    package = File.read!(@geometry) |> :json.decode()

    assert package["font_sha256"] ==
             Base.encode16(:crypto.hash(:sha256, c.font.data), case: :lower)

    assert package["core_sha256"] ==
             "f61ebea74ba54c489646e6969983ef977f448c26602edff75ec4e3ba77216373"

    assert package["fonttools"] == "4.60.1"
    assert length(package["cases"]) == 21

    for record <- package["cases"] do
      assert {:ok, measured} = MathLayout.measure(source_tree(record["tree"], c.style))
      expected = record["expected"]

      for field <- [
            :width,
            :min_content_width,
            :max_content_width,
            :ascent,
            :descent,
            :ink_ascent,
            :ink_descent
          ] do
        assert_in_delta Map.fetch!(measured, field),
                        rational(expected[Atom.to_string(field)]),
                        1.0e-10,
                        "#{record["name"]} #{field}"
      end

      assert measured.actual_text == expected["text"]

      assert Enum.map(measured.glyphs, & &1.glyph_id) ==
               Enum.map(expected["glyphs"], & &1["glyph_id"])

      for {actual, source} <- Enum.zip(measured.glyphs, expected["glyphs"]),
          field <- [:x, :y, :width, :font_size] do
        assert_in_delta Map.fetch!(actual, field),
                        rational(source[Atom.to_string(field)]),
                        1.0e-10,
                        "#{record["name"]} glyph #{field}"
      end
    end
  end

  test "independent OpenType corner witnesses retain vertical Core metrics and selected glyphs",
       c do
    package =
      File.read!(Path.expand("../fixtures/mathml_script_corner_geometry.json", __DIR__))
      |> :json.decode()

    assert package["opentype_corner_source"] ==
             "https://learn.microsoft.com/en-us/typography/opentype/spec/math#mathkerninfo-table"

    assert length(package["cases"]) == 27

    for record <- package["cases"] do
      assert {:ok, measured} = MathLayout.measure(source_tree(record["tree"], c.style))
      expected = record["expected"]
      assert measured.actual_text == expected["text"]

      assert Enum.map(measured.glyphs, & &1.glyph_id) ==
               Enum.map(expected["glyphs"], & &1["glyph_id"])

      for field <- [
            :width,
            :min_content_width,
            :max_content_width,
            :ascent,
            :descent,
            :ink_ascent,
            :ink_descent
          ] do
        assert_in_delta measured[field],
                        rational(expected[Atom.to_string(field)]),
                        1.0e-10,
                        "#{record["name"]} #{field}"
      end

      for {actual, source} <- Enum.zip(measured.glyphs, expected["glyphs"]) do
        assert actual.glyph_id == source["glyph_id"]

        for field <- [:x, :y, :width, :font_size] do
          assert_in_delta actual[field],
                          rational(source[Atom.to_string(field)]),
                          1.0e-10,
                          "#{record["name"]} glyph #{field}"
        end
      end
    end
  end

  test "actual source selectors apply depth, script style, cramped shift and authored precedence" do
    assert {:ok, measured} =
             styled_math("""
             <math style='font-size:20pt'><msubsup><mi>x</mi><mn>1</mn><msup><mn>2</mn><mn>3</mn></msup></msubsup></math>
             """)

    assert Enum.map(measured.glyphs, &Float.round(&1.font_size * 1.0, 8)) == [
             20.0,
             14.0,
             14.0,
             11.0
           ]

    assert measured.actual_text == "x123"

    assert {:ok, override} =
             styled_math("""
             <math style='font-size:20pt'><msup><mn>1</mn><mn style='font-size:10pt;math-depth:0;math-style:normal'>2</mn></msup></math>
             """)

    assert Enum.map(override.glyphs, & &1.font_size) == [20, 10]
    assert List.last(override.glyphs).glyph_id == 1139

    assert {:ok, under} =
             styled_math(
               "<math style='font-size:20pt'><msub><mn>1</mn><msup><mn style='font-size:1pt'>2</mn><mn style='font-size:1pt'>3</mn></msup></msub></math>"
             )

    assert {:ok, normal} =
             styled_math(
               "<math style='font-size:20pt'><msup><mn>1</mn><msup><mn style='font-size:1pt'>2</mn><mn style='font-size:1pt'>3</mn></msup></msup></math>"
             )

    [_, lower_base, cramped] = under.glyphs
    [_, upper_base, ordinary] = normal.glyphs
    assert cramped.y - lower_base.y < ordinary.y - upper_base.y
  end

  test "accent flags are ASCII matches, retain depth substitutions and accept authored size overrides" do
    for tag <- ["mover", "munder", "munderover"],
        value <- ["true", "TRUE", " true ", "invalid"] do
      extra = if tag == "munderover", do: "<mn>3</mn>", else: ""
      attribute = if tag == "munder", do: "accentunder", else: "accent"

      assert {:ok, measured} =
               styled_math(
                 "<math style='font-size:20pt'><#{tag} #{attribute}='#{value}'><mn>1</mn><mn>2</mn>#{extra}</#{tag}></math>"
               )

      expected = if value in ["true", "TRUE"], do: 20, else: 14
      assert List.last(measured.glyphs).font_size == expected
      assert List.last(measured.glyphs).glyph_id != if(tag == "munderover", do: 1140, else: 1139)
    end

    assert {:ok, measured} =
             styled_math(
               "<math style='font-size:20pt'><munderover accentunder='TRUE' accent='true'><mn>1</mn><mn>2</mn><mn style='font-size:12pt;math-depth:0'>3</mn></munderover></math>"
             )

    assert Enum.map(measured.glyphs, & &1.font_size) == [20, 20, 12]
  end

  test "wrong in-flow child counts use prescribed rows and keep script defaults", c do
    for tag <- ~w(msub msup msubsup munder mover munderover), count <- [0, 1, 4] do
      children = List.duplicate(token("1", c.style), count)

      assert {:ok, prepared} =
               MathLayoutValidator.prepare_element(element(tag, children, c.style), %{})

      assert prepared.kind == :row
      assert {:ok, measured} = MathLayout.measure(element(tag, children, c.style))
      assert measured.actual_text == String.duplicate("1", count)
      assert Enum.all?(measured.glyphs, &(&1.y == 0))
    end

    assert {:ok, measured} =
             styled_math(
               "<math style='font-size:20pt'><msup><mn style='display:none'>0</mn><mn>1</mn><mn>2</mn></msup></math>"
             )

    assert Enum.map(measured.glyphs, & &1.font_size) == [14, 14]
    assert measured.actual_text == "12"
    assert List.last(measured.glyphs).y > 0
  end

  test "script operators have postfix context while the embellished base uses the outer row", c do
    base = token("+", c.style, "mo")
    script = token("(", c.style, "mo")
    schema = element("msup", [base, script], c.style)
    row = element("math", [schema, token("1", c.style)], c.style)
    assert {:ok, prepared} = MathLayoutValidator.prepare_element(row, %{})
    [scripted, _] = prepared.children
    [base, script] = scripted.children
    assert base.core_operator.form == :prefix
    assert scripted.core_operator == base.core_operator
    assert script.core_operator.form == :postfix
    assert {:ok, measured} = MathLayout.measure(row)
    assert hd(measured.glyphs).x == base.core_operator.lspace

    assert {:ok, detached} =
             MathLayout.measure(schema, %{operator_context: %{parent: "math", index: 0, count: 2}})

    assert {:ok, number} = MathLayout.measure(token("1", c.style))

    assert_in_delta measured.width,
                    detached.width + base.core_operator.lspace + base.core_operator.rspace +
                      number.width,
                    1.0e-10
  end

  test "italic and selected-font corner corrections contribute at their own script sides",
       c do
    for text <- ["∑", "x"], tag <- ["msub", "msup", "msubsup"] do
      base = token(text, %{c.style | math_style: :normal}, if(text == "∑", do: "mo", else: "mi"))
      base = if text == "x", do: put_in(base.style.text_transform, :math_auto), else: base
      script = token("1", %{c.style | font_size: 14})
      children = [base, script] ++ if(tag == "msubsup", do: [script], else: [])
      assert {:ok, natural} = MathLayout.measure(base)
      assert {:ok, measured} = MathLayout.measure(element(tag, children, c.style))
      [_, first | rest] = measured.glyphs

      correction =
        if text == "∑" and tag != "msup",
          do: -natural.italic_correction,
          else: if(text == "x" and tag == "msup", do: natural.italic_correction, else: 0.0)

      {:ok, metrics} = Font.math_metrics(c.font)
      record = metrics.glyph_info.kerning[c.font.cmap[?x]].bottom_right
      assert record.heights == [] and hd(record.values).value == 35
      kern = if text == "x" and tag != "msup", do: 35 * 20 / c.font.units_per_em, else: 0.0
      assert_in_delta first.x, natural.width + correction + kern, 1.0e-10

      if rest != [],
        do:
          assert_in_delta(
            hd(rest).x,
            natural.width + if(text == "x", do: natural.italic_correction, else: 0.0),
            1.0e-10
          )
    end
  end

  test "single-glyph boxes with extra width use the box script position", c do
    base = token("x", c.style)
    script = token("1", %{c.style | font_size: 14})
    boxed = element("mrow", [base, element("mspace", [], c.style, %{"width" => "3pt"})], c.style)
    assert {:ok, natural} = MathLayout.measure(base)
    assert {:ok, boxed_base} = MathLayout.measure(boxed)
    assert {:ok, ordinary} = MathLayout.measure(element("msub", [base, script], c.style))
    assert {:ok, measured} = MathLayout.measure(element("msub", [boxed, script], c.style))
    assert_in_delta List.last(ordinary.glyphs).x, natural.width + 0.7, 1.0e-10
    assert_in_delta boxed_base.width, natural.width + 3, 1.0e-10
    assert_in_delta List.last(measured.glyphs).x, boxed_base.width, 1.0e-10
    assert measured.actual_text == "x1"
  end

  test "an external stretch constraint applies to the base alone including wrapped bases", c do
    op = token("(", c.style, "mo")
    script = token(")", c.style, "mo")

    for tag <- ["msup", "mover"] do
      schema = element(tag, [op, script], c.style)
      assert {:ok, measured} = MathLayout.measure(schema, %{block_stretch_size: {40, 10}})
      assert {:ok, base} = MathLayout.measure(op, %{block_stretch_size: {40, 10}})
      assert hd(measured.glyphs).glyph_id == hd(base.glyphs).glyph_id
      assert List.last(measured.glyphs).glyph_id == c.font.cmap[?)]
    end

    wrapped = element("mrow", [element("msub", [op, token("1", c.style)], c.style)], c.style)
    assert {:ok, measured} = MathLayout.measure(wrapped, %{block_stretch_size: {40, 10}})
    assert hd(measured.glyphs).glyph_id != c.font.cmap[?(]
  end

  test "limits stretch all horizontal children and honor an external base target", c do
    brace = token("⏞", c.style, "mo")
    schema = element("munderover", [brace, brace, brace], c.style)
    assert {:ok, ordinary} = MathLayout.measure(schema)
    assert length(ordinary.glyphs) >= 3
    assert {:ok, stretched} = MathLayout.measure(schema, %{inline_stretch_size: 80})
    assert stretched.width >= 80
    assert length(stretched.glyphs) > length(ordinary.glyphs)
    assert stretched.actual_text == "⏞⏞⏞"

    assert {:ok, public} =
             styled_math(
               "<math style='font-size:20pt'><mover accent='true'><mspace width='80pt' height='5pt'/><mo>⏞</mo></mover></math>"
             )

    assert length(public.glyphs) > 3
  end

  test "empty paired scripts still enforce the source minimum gap", c do
    base = token("1", c.style)
    empty = element("mrow", [], c.style)
    assert {:ok, measured} = MathLayout.measure(element("msubsup", [base, empty, empty], c.style))
    assert measured.actual_text == "1"
    assert measured.ascent > 0 and measured.descent > 0
    assert {:ok, under} = MathLayout.measure(element("munder", [base, empty], c.style))
    assert under.descent > 0
  end

  test "composed intrinsic and final script bounds propagate validator failures through consumers",
       c do
    maximum = Limits.get(:max_css_numeric_magnitude)
    space = element("mspace", [], c.style, %{"width" => "#{maximum}pt"})
    wide_row = element("mrow", [space, space], c.style)

    assert {:error, {:invalid_document, diagnostic}} =
             MathLayout.measure(element("mover", [wide_row, token("^", c.style, "mo")], c.style))

    assert diagnostic.operation == :measure_math and diagnostic.message =~ "bounded point extents"

    assert {:error, {:invalid_document, diagnostic}} =
             MathLayout.measure(element("msup", [space, space], c.style))

    assert diagnostic.operation == :measure_math and diagnostic.message =~ "placement exceeds"
  end

  test "native HTML script links and first-baseline boxes translate once in original source order" do
    html = """
    <math style='font-size:20pt'><msubsup>
    <mtext><a href='https://script.test/base'>Base</a></mtext>
    <mtext><span style='display:inline-block;font-family:DejaVu Sans Mono;font-size:10pt;line-height:12pt;background-color:yellow'><a href='https://script.test/lower'>Low<br>Line</a></span></mtext>
    <mtext><a href='https://script.test/upper'>Up</a></mtext>
    </msubsup></math>
    """

    assert {:ok, measured} = styled_math(html)
    assert measured.actual_text == "BaseLowLineUp"
    native = Enum.filter(measured.drawing_items, &(&1.kind == :native))

    assert Enum.sort(Enum.flat_map(native, &links(&1.box)) |> Enum.uniq()) == [
             "https://script.test/base",
             "https://script.test/lower",
             "https://script.test/upper"
           ]

    assert Enum.any?(native, &(&1.y < 0)) and Enum.any?(native, &(&1.y > 0))
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "BaseLowLineUp"} = Text.extract(pdf, layout: false)
    for suffix <- ["base", "lower", "upper"], do: assert(pdf =~ "https://script.test/#{suffix}")
  end

  test "invisible source text and assembled accents retain separate original logical groups" do
    html =
      "<math style='font-size:20pt'><msup><mo>⁡</mo><mn>2</mn></msup><mover accent='true'><mn>12345</mn><mo>⏞</mo></mover></math>"

    assert {:ok, measured} = styled_math(html)
    assert measured.actual_text == "⁡212345⏞"
    assert Enum.any?(measured.drawing_items, &(&1.kind == :logical and &1.actual_text == "⁡"))
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "⁡212345⏞"} = Text.extract(pdf, layout: false)
  end

  test "same-DOM script controls keep form ownership and ordinary native painting" do
    html =
      "<math style='font-size:18pt'><msup><mn>1</mn><mtext><input name='script-value' value='42' style='font-family:DejaVu Sans;font-size:10pt;width:24pt;height:12pt'></mtext></msup></math>"

    for forms <- [:static, :interactive] do
      assert {:ok, pdf} = HtmlToPdf.render(html, forms: forms, system_font_discovery: false)
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert text =~ "1"

      if forms == :static do
        assert text =~ "42"
      else
        assert pdf =~ "script-value" and pdf =~ "/Widget"
      end
    end
  end

  test "pending stretch and placement requests diagnose malformed measurement transport", c do
    node = element("mover", [token("1", c.style), token("2", c.style)], c.style)
    assert {:ok, prepared} = MathLayoutValidator.prepare_element(node, %{})
    assert {:ok, child} = MathLayout.measure(token("1", c.style))

    for {state, measured} <- [
          {%{}, %{}},
          {prepared, %{3 => child}},
          {prepared, %{0 => %{child | width: "bad"}}},
          {prepared, %{0 => Map.delete(child, :ink_ascent)}}
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               MathLayoutValidator.prepare_limit_stretch(state, measured)

      assert diagnostic.stage == :layout and diagnostic.message != ""
    end

    assert {:ok, +0.0} = MathLayoutValidator.prepare_limit_stretch(prepared, %{})

    assert {:ok, _} =
             MathLayoutValidator.prepare_script_placement(prepared, [
               child,
               Map.delete(child, :top_accent_attachment)
             ])

    for {state, measured} <- [
          {%{}, []},
          {prepared, [child]},
          {prepared, [child, Map.put(child, :top_accent_attachment, "bad")]},
          {prepared, [%{child | width: 1.0e20}, child]}
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               MathLayoutValidator.prepare_script_placement(state, measured)

      assert diagnostic.stage == :layout and diagnostic.reason == :invalid_document
    end
  end

  test "script state and corner font work reject malformed or unbounded transport", c do
    node = element("msup", [token("1", c.style), token("2", c.style)], c.style)
    assert {:ok, prepared} = MathLayoutValidator.prepare_element(node, %{})
    assert {:ok, base} = MathLayout.measure(token("1", c.style))
    assert {:ok, upper} = MathLayout.measure(token("2", c.style))

    for state <- [
          Map.delete(prepared, :constants),
          %{prepared | constants: %{}},
          %{prepared | constants: Map.put(prepared.constants, :superscript_shift_up, :invalid)},
          %{prepared | lower: 0},
          %{prepared | accent: :invalid}
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               MathLayoutValidator.prepare_script_placement(state, [base, upper])

      assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
      assert diagnostic.message =~ "script placement"
    end

    maximum = Limits.get(:max_css_numeric_magnitude)
    glyphs = [%{hd(upper.glyphs) | font_size: 1.0e308}]

    assert {:error, {:invalid_document, diagnostic}} =
             MathLayoutValidator.prepare_script_placement(prepared, [
               base,
               %{upper | glyphs: glyphs}
             ])

    assert diagnostic.message =~ "bounded point extents"

    constants = Map.put(prepared.constants, :superscript_shift_up, maximum)

    assert {:error, {:invalid_document, diagnostic}} =
             MathLayoutValidator.prepare_script_placement(%{prepared | constants: constants}, [
               base,
               %{upper | ink_descent: -maximum}
             ])

    assert diagnostic.message =~ "bounded point junction heights"
  end

  test "public unsupported CSS, numeric extents, resource limits and missing script glyphs stay diagnostic",
       c do
    for html <- [
          "<math><msup style='display:block'><mn>1</mn><mn>2</mn></msup></math>",
          "<math><mover style='overflow:hidden'><mn>1</mn><mn>2</mn></mover></math>",
          "<math><msub style='position:absolute'><mn>1</mn><mn>2</mn></msub></math>"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(html, system_font_discovery: false)

      assert diagnostic.stage == :layout and diagnostic.source != nil
    end

    schema =
      element("msup", [token("1", c.style), token("2", c.style)], %{
        c.style
        | math_shift: :invalid
      })

    assert {:error, {:invalid_document, diagnostic}} = MathLayout.measure(schema)
    assert diagnostic.operation == :measure_math
    missing = %{c.font | cmap: Map.delete(c.font.cmap, ?2)}

    schema =
      element("msup", [token("1", c.style), token("2", %{c.style | font_face: missing})], c.style)

    assert {:error, {:unsupported_glyph, diagnostic}} = MathLayout.measure(schema)
    assert diagnostic.source == "2"

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlToPdf.render(
               "<math style='font-size:300pt'><msubsup><mi>x</mi><mn>1</mn><mn>2</mn></msubsup></math>",
               page_size: {100, 100},
               margin: 0
             )

    assert diagnostic.message =~ "atomic MathML"
    Limits.install(Map.put(Limits.effective(), :max_font_math_work, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<math><msup><mn>1</mn><mn>2</mn></msup></math>",
               system_font_discovery: false
             )

    assert diagnostic.stage == :font and diagnostic.message =~ "max_font_math_work"
  end
end
