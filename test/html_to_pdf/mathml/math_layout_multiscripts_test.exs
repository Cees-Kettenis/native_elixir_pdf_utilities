alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.MathLayoutMultiscriptsTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, MathLayout, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, MathLayoutValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, font} = Font.resolve("math", 400, :normal, registry)

    style = %{
      display: :block,
      font_size: 18,
      font_face: font,
      color: {0, 0, 0},
      line_height: 24,
      math_style: :compact,
      math_shift: :normal,
      text_transform: :none
    }

    {:ok, style: style}
  end

  defp element(tag, children, style) do
    %{
      type: :element,
      namespace: :mathml,
      tag: tag,
      children: children,
      style: style,
      attributes: %{}
    }
  end

  defp token(text, style), do: element("mn", [%{type: :text, text: text, style: style}], style)

  defp styled(html) do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      with {:ok, dom} <- HtmlParser.parse_detailed(html),
           {:ok, %{children: [node]}} <- Style.compute_detailed(dom, system_font_discovery: false),
           do: {:ok, node}
    end)
  end

  test "one postscript pair is exactly the existing selected-font msubsup contract" do
    for base <- ["<mn>7</mn>", "<mi>f</mi>", "<mo>∑</mo>"] do
      body = base <> "<mn>1</mn><mn>2</mn>"

      assert {:ok, pair} =
               styled("<math style='font-size:20pt'><msubsup>#{body}</msubsup></math>")

      assert {:ok, multi} =
               styled(
                 "<math style='font-size:20pt'><mmultiscripts>#{body}</mmultiscripts></math>"
               )

      left = pair.style._math_box
      right = multi.style._math_box

      for field <- [
            :width,
            :ascent,
            :descent,
            :ink_ascent,
            :ink_descent,
            :min_content_width,
            :max_content_width
          ] do
        assert_in_delta Map.fetch!(left, field), Map.fetch!(right, field), 0.0000001
      end

      glyph_fields = [:x, :y, :text, :glyph_id, :font_size, :width, :height, :color]

      assert Enum.map(left.glyphs, &Map.take(&1, glyph_fields)) ==
               Enum.map(right.glyphs, &Map.take(&1, glyph_fields))

      assert left.actual_text == right.actual_text
    end
  end

  test "prescripts have selected-font advances, right alignment and DOM Unicode", %{
    style: style
  } do
    empty = element("mprescripts", [], style)
    children = [token("77", style), empty, token("12", style), token("345", style)]

    assert {:ok, prepared} =
             MathLayoutValidator.prepare_element(element("mmultiscripts", children, style), %{})

    assert prepared.roles == %{pre: [{2, 3}], post: [], marker: 1}

    measured =
      Enum.map(children, fn child ->
        {:ok, box} = MathLayout.measure(child)
        box
      end)

    assert {:ok, placed} = MathLayoutValidator.prepare_multiscript_placement(prepared, measured)
    [{base_x, base_y}, {marker_x, marker_y}, {sub_x, down}, {sup_x, up}] = placed.offsets
    assert base_y == 0 and marker_y == 0
    assert marker_x == base_x
    assert down < 0 and up > 0

    assert_in_delta sub_x + Enum.at(measured, 2).width,
                    sup_x + Enum.at(measured, 3).width,
                    0.0000001

    assert_in_delta base_x,
                    max(Enum.at(measured, 2).width, Enum.at(measured, 3).width) +
                      prepared.constants.space_after_script,
                    0.0000001

    assert_in_delta placed.width, base_x + hd(measured).width, 0.0000001
    assert {:ok, box} = MathLayout.measure(element("mmultiscripts", children, style))
    assert box.actual_text == "7712345"
  end

  test "all pairs share the greatest vertical clearance", %{style: style} do
    tall =
      element("mspace", [], style)
      |> Map.put(:attributes, %{"height" => "35pt", "depth" => "8pt"})

    children = [token("7", style), token("1", style), token("2", style), tall, token("4", style)]

    assert {:ok, prepared} =
             MathLayoutValidator.prepare_element(element("mmultiscripts", children, style), %{})

    measured =
      Enum.map(children, fn child ->
        {:ok, box} = MathLayout.measure(child)
        box
      end)

    assert {:ok, placed} = MathLayoutValidator.prepare_multiscript_placement(prepared, measured)
    assert elem(Enum.at(placed.offsets, 1), 1) == elem(Enum.at(placed.offsets, 3), 1)
    assert elem(Enum.at(placed.offsets, 2), 1) == elem(Enum.at(placed.offsets, 4), 1)
    assert -elem(Enum.at(placed.offsets, 1), 1) >= 35 - prepared.constants.subscript_top_max
    assert placed.descent >= 8 - elem(Enum.at(placed.offsets, 3), 1)
  end

  test "parsed marker selects script depth and lower math-shift on both sides" do
    assert {:ok, math} =
             styled(
               "<math style='font-size:20pt'><mmultiscripts><mn>0</mn><mn>1</mn><mn>2</mn><mprescripts/><mn>3</mn><mn>4</mn></mmultiscripts></math>"
             )

    source =
      "<math><mmultiscripts><mn>0</mn><mn>1</mn><mn>2</mn><mprescripts/><mn>3</mn><mn>4</mn></mmultiscripts></math>"

    assert {:ok, %{children: [parsed_math]}} = HtmlParser.parse_detailed(source)
    [parsed_math] = HtmlValidator.assign_render_metadata([parsed_math])
    [scripts] = parsed_math.children
    [base, sub, sup, marker, pre_sub, pre_sup] = scripts.children
    inherited = math.style
    assert MathLayoutValidator.schema_child_defaults(base, inherited, [scripts]) == %{}

    for child <- [sub, sup, marker, pre_sub, pre_sup] do
      defaults = MathLayoutValidator.schema_child_defaults(child, inherited, [scripts])
      assert defaults.math_depth == inherited.math_depth + 1
      assert defaults.math_style == :compact
    end

    for child <- [sub, pre_sub] do
      assert MathLayoutValidator.schema_child_defaults(child, inherited, [scripts]).math_shift ==
               :compact
    end

    for child <- [sup, pre_sup] do
      refute Map.has_key?(
               MathLayoutValidator.schema_child_defaults(child, inherited, [scripts]),
               :math_shift
             )
    end

    assert hd(math.style._math_box.glyphs).font_size == 20

    for glyph <- tl(math.style._math_box.glyphs),
        do: assert_in_delta(glyph.font_size, 14, 0.0000001)

    assert math.style._math_box.actual_text == "01234"
  end

  test "odd pairs, duplicate markers, marker base and empty lists use mrow fallback", %{
    style: style
  } do
    marker = element("mprescripts", [], style)
    one = token("1", style)

    for children <- [
          [],
          [one, one],
          [marker, one, one],
          [one, marker, marker],
          [one, marker, one]
        ] do
      assert {:ok, fallback} = MathLayout.measure(element("mmultiscripts", children, style))
      assert {:ok, row} = MathLayout.measure(element("mrow", children, style))

      assert Map.take(fallback, [:width, :ascent, :descent, :glyphs, :actual_text]) ==
               Map.take(row, [:width, :ascent, :descent, :glyphs, :actual_text])
    end
  end

  test "mprescripts is a mrow outside the schema and preserves intentional marker ink", %{
    style: style
  } do
    marker = element("mprescripts", [token("8", style)], style)
    assert {:ok, box} = MathLayout.measure(marker)
    assert box.actual_text == "8" and box.glyphs != []

    assert {:ok, multi} =
             MathLayout.measure(element("mmultiscripts", [token("7", style), marker], style))

    assert multi.actual_text == "78"
    assert length(multi.glyphs) == 2
    assert_in_delta hd(multi.glyphs).x, List.last(multi.glyphs).x, 0.0000001
  end

  test "public prepared geometry diagnoses malformed roles, constants and improper lists", %{
    style: style
  } do
    children = Enum.map(["7", "1", "2"], &token(&1, style))

    assert {:ok, prepared} =
             MathLayoutValidator.prepare_element(element("mmultiscripts", children, style), %{})

    measured =
      Enum.map(children, fn child ->
        {:ok, box} = MathLayout.measure(child)
        box
      end)

    for {state, values} <- [
          {%{prepared | roles: %{pre: [], post: [{1, 9}], marker: nil}}, measured},
          {%{prepared | constants: Map.put(prepared.constants, :space_after_script, "bad")},
           measured},
          {%{prepared | children: [hd(prepared.children) | :bad]}, measured},
          {prepared, [hd(measured) | :bad]},
          {%{
             prepared
             | children: [
                 Map.put(hd(prepared.children), :prescript_marker, :bad) | tl(prepared.children)
               ]
           }, measured},
          {prepared, [Map.delete(hd(measured), :min_content_width) | tl(measured)]},
          {prepared,
           [Map.put(hd(measured), :glyphs, [hd(hd(measured).glyphs) | :bad]) | tl(measured)]}
        ] do
      assert {:error, {reason, diagnostic}} =
               MathLayoutValidator.prepare_multiscript_placement(state, values)

      assert reason in [:invalid_document, :unsupported_html]
      assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
      assert is_binary(diagnostic.message) and diagnostic.message != ""
    end
  end

  test "multi-pair work obeys the existing shared MATH quota", %{style: style} do
    children = Enum.map(["7", "1", "2", "3", "4"], &token(&1, style))

    assert {:ok, prepared} =
             MathLayoutValidator.prepare_element(element("mmultiscripts", children, style), %{})

    measured =
      Enum.map(children, fn child ->
        {:ok, box} = MathLayout.measure(child)
        box
      end)

    Limits.install(%{Limits.effective() | max_font_math_work: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             MathLayoutValidator.prepare_multiscript_placement(prepared, measured)

    assert diagnostic.message =~ "max_font_math_work"
    assert diagnostic.operation == :measure_math
  end

  test "six source-only selected-font witnesses prove left corner kerning", %{style: style} do
    path = Paths.fixture("mathml_prescript_corner_geometry.json")
    package = File.read!(path) |> :json.decode()

    assert package["font_sha256"] ==
             "49b22767985171bce579307b21d98f4d35012fa39e316acc4ffcab8644dfb69b"

    number = fn value ->
      case value do
        %{"numerator" => n, "denominator" => d} -> n / d
        n -> n
      end
    end

    for c <- package["cases"] do
      children =
        Enum.map([c["base"], c["sub"], c["sup"]], fn source ->
          inherited =
            Map.merge(style, %{
              font_size: number.(source["style"]["font_size"]),
              math_depth: source["style"]["math_depth"],
              math_style: String.to_existing_atom(source["style"]["math_style"]),
              math_shift: String.to_existing_atom(source["style"]["math_shift"]),
              text_transform: if(source["tag"] == "mi", do: :math_auto, else: :none)
            })

          element(
            source["tag"],
            [%{type: :text, text: source["text"], style: inherited}],
            inherited
          )
        end)

      [base, sub, sup] = children
      root_style = Map.merge(style, Map.take(base.style, [:font_size, :math_depth]))
      marker = element("mprescripts", [], root_style)
      source = element("mmultiscripts", [base, marker, sub, sup], root_style)
      assert {:ok, prepared} = MathLayoutValidator.prepare_element(source, %{})

      measured =
        Enum.map(source.children, fn child ->
          {:ok, box} = MathLayout.measure(child)
          box
        end)

      assert {:ok, placed} = MathLayoutValidator.prepare_multiscript_placement(prepared, measured)
      assert_in_delta placed.width, number.(c["expected"]["width"]), 0.0000001

      for {{x, y}, [ex, ey]} <- Enum.zip(placed.offsets, c["expected"]["offsets"]) do
        assert_in_delta x, number.(ex), 0.0000001
        assert_in_delta y, number.(ey), 0.0000001
      end

      assert Enum.flat_map(measured, fn child -> Enum.map(child.glyphs, & &1.glyph_id) end) ==
               c["expected"]["glyph_ids"]
    end

    assert Enum.any?(package["cases"], &(number.(&1["expected"]["lower_kern"]) != 0))
    assert Enum.any?(package["cases"], &(number.(&1["expected"]["upper_kern"]) != 0))
  end

  test "public PDF tensor scripts preserve all logical Unicode in source order" do
    html =
      "<math><mmultiscripts><mi>α</mi><mn>1</mn><mn>2</mn><mprescripts/><mi>β</mi><mn>3</mn></mmultiscripts></math>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "α12β3"} = Text.extract(pdf, layout: false)
  end
end
