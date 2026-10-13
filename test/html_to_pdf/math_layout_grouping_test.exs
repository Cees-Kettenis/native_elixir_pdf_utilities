defmodule NativeElixirPdfUtilities.HtmlToPdf.MathLayoutGroupingTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, MathLayout, Style}
  alias NativeElixirPdfUtilities.Validators.MathLayoutValidator

  defp styled_math(content) do
    with {:ok, dom} <- HtmlParser.parse_detailed(content),
         {:ok, %{children: [node]}} <- Style.compute_detailed(dom, system_font_discovery: false) do
      {:ok, node.style._math_box}
    end
  end

  setup do
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

    node = fn tag, children, attributes ->
      %{
        type: :element,
        namespace: :mathml,
        tag: tag,
        children: children,
        attributes: attributes,
        style: style
      }
    end

    token = fn tag, text, attributes ->
      node.(tag, [%{type: :text, text: text, style: style}], attributes)
    end

    {:ok, node: node, token: token}
  end

  test "grouping retains row geometry, logical Unicode and one outer operator spacing", %{
    node: n,
    token: t
  } do
    content = [
      t.("mtext", "words", %{}),
      t.("mo", "+", %{"lspace" => "3pt", "rspace" => "7pt"}),
      n.("mspace", [], %{"width" => "2pt"})
    ]

    for tag <- ~w(mrow mstyle merror) do
      node = n.("math", [n.(tag, [n.("mstyle", content, %{})], %{}), t.("mn", "1", %{})], %{})
      assert {:ok, prepared} = MathLayoutValidator.prepare_element(node, %{})
      assert [wrapper, _] = prepared.children
      assert wrapper.core_operator.form == :prefix
      assert wrapper.children |> hd() |> Map.fetch!(:core_operator) == wrapper.core_operator
      assert {:ok, measured} = MathLayout.measure(node)
      assert measured.actual_text == "words+1"

      row_node =
        n.("math", [n.("mrow", [n.("mrow", content, %{})], %{}), t.("mn", "1", %{})], %{})

      assert {:ok, row} = MathLayout.measure(row_node)
      assert Map.delete(measured, :drawing_items) == Map.delete(row, :drawing_items)

      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<math><#{tag}><mstyle><mtext>words</mtext><mo>+</mo></mstyle></#{tag}><mn>1</mn></math>"
               )

      assert {:ok, "words+1"} = Text.extract(pdf, layout: false)
    end
  end

  test "mstyle font and legacy depth attributes inherit through nested row groups" do
    assert {:ok, measured} =
             styled_math("""
             <math style='font-size:20pt'><mstyle scriptlevel='1' mathcolor='red'>
             <mn>1</mn><mstyle scriptlevel='+1' displaystyle='true' style='color:blue'>
             <mn>2</mn></mstyle><mstyle style='math-depth:0;font-size:18pt'><mn>3</mn></mstyle>
             </mstyle></math>
             """)

    assert [first, second, third] = measured.glyphs
    assert first.font_size == 14
    assert second.font_size == 11
    assert third.font_size == 18
    assert first.color == {1.0, 0.0, 0.0}
    assert second.color == {0.0, 0.0, 1.0}
    assert third.color == {1.0, 0.0, 0.0}
    assert first.font_face == second.font_face and second.font_face == third.font_face
    assert measured.actual_text == "123"
  end

  test "mstyle forwards external stretch targets and ignores space-like groups for form", %{
    node: n,
    token: t
  } do
    wrapper = n.("mstyle", [t.("mo", "(", %{})], %{})
    space = n.("mstyle", [t.("mtext", " ", %{}), n.("mspace", [], %{"width" => "2pt"})], %{})
    node = n.("math", [wrapper, space, t.("mn", "1", %{})], %{})

    assert {:ok, prepared} = MathLayoutValidator.prepare_element(node, %{})
    assert hd(prepared.children).core_operator.form == :prefix
    assert {:ok, wrapped} = MathLayout.measure(wrapper, %{block_stretch_size: {40, 10}})
    assert {:ok, bare} = MathLayout.measure(hd(wrapper.children), %{block_stretch_size: {40, 10}})

    fields = [
      :width,
      :ascent,
      :descent,
      :ink_ascent,
      :ink_descent,
      :min_content_width,
      :max_content_width,
      :glyphs,
      :actual_text
    ]

    assert Map.take(wrapped, fields) == Map.take(bare, fields)
  end

  test "merror UA highlight uses shared box painting and authored CSS overrides it" do
    for {css, border, background} <- [
          {"", 0.75, {1.0, 1.0, 224 / 255}},
          {"border:2pt solid blue;background-color:white", 1.5, {1.0, 1.0, 1.0}}
        ] do
      assert {:ok, measured} =
               styled_math(
                 "<math style='font-size:20pt'><merror style='#{css}'><mtext>bad</mtext><mn>2</mn></merror></math>"
               )

      assert [%{kind: :background, width: width, height: height, style: style} | _] =
               measured.drawing_items

      assert width == measured.width and height == measured.ascent + measured.descent
      assert style.background_color == background
      assert Enum.all?(Map.values(style.border_widths), &(&1 == border))
      assert Enum.all?(Map.values(style.border_styles), &(&1 == :solid))
      color = if css == "", do: {1.0, 0.0, 0.0}, else: {0.0, 0.0, 1.0}
      assert Enum.all?(Map.values(style.border_colors), &(&1 == color))
      assert measured.actual_text == "bad2"
      assert :ok == MathLayoutValidator.validate_measured_geometry(measured)

      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<math><merror style='#{css}'><mtext>bad</mtext><mn>2</mn></merror></math>"
               )

      assert {:ok, "bad2"} = Text.extract(pdf, layout: false)
    end
  end

  test "groups compose implemented tensor scripts and preserve native logical content" do
    for tag <- ~w(mstyle merror) do
      html =
        "<math><#{tag}><mmultiscripts><mi>α</mi><mn>1</mn><mn>2</mn><mprescripts/><mi>β</mi><mn>3</mn></mmultiscripts></#{tag}></math>"

      assert {:ok, measured} = styled_math(html)
      assert measured.actual_text == "α12β3" and measured.drawing_items != []
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, "α12β3"} = Text.extract(pdf, layout: false)
    end
  end

  test "grouping leaves unsupported descendants and composition under validator authority" do
    for tag <- ~w(mstyle merror),
        suffix <- [
          "<msup style='position:absolute'><mn>1</mn><mn>2</mn></msup>",
          "<mpadded style='position:absolute'/>"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render("<math><#{tag}>#{suffix}</#{tag}></math>")

      assert diagnostic.stage == :layout
      assert diagnostic.operation == :measure_math
      assert diagnostic.module == MathLayoutValidator
      assert diagnostic.source in ["msup", "mpadded"]
    end

    for tag <- ~w(mstyle merror) do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(
                 "<math><#{tag} style='position:absolute'><mn>1</mn></#{tag}></math>"
               )

      assert diagnostic.stage == :layout and diagnostic.reason == :unsupported_html
      assert diagnostic.operation == :measure_math
      assert diagnostic.source == tag
      assert diagnostic.message =~ "positioned"
    end
  end
end
