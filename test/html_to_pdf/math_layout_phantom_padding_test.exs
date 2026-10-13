defmodule NativeElixirPdfUtilities.HtmlToPdf.MathLayoutPhantomPaddingTest do
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
      font_size: 20,
      font_face: font,
      color: {0, 0, 0},
      line_height: 24,
      math_style: :compact,
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

  defp token(tag, text, style, attributes \\ %{}) do
    element(tag, [%{type: :text, text: text, style: style}], style, attributes)
  end

  defp styled_math(html) do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _context ->
      with {:ok, dom} <- HtmlParser.parse_detailed(html),
           {:ok, %{children: [node]}} <- Style.compute_detailed(dom, system_font_discovery: false) do
        {:ok, node.style._math_box}
      end
    end)
  end

  test "phantom retains every row metric and core property while omitting all paint", %{
    style: style
  } do
    content = [
      token("mn", "12", style),
      token("mo", "+", style, %{"form" => "infix"}),
      token("mi", "x", style)
    ]

    assert {:ok, row} = MathLayout.measure(element("mrow", content, style))
    assert {:ok, hidden} = MathLayout.measure(element("mphantom", content, style))
    assert hidden.glyphs == [] and hidden.drawing_items == [] and hidden.actual_text == ""

    assert Map.drop(hidden, [:glyphs, :drawing_items, :actual_text]) ==
             Map.drop(row, [:glyphs, :drawing_items, :actual_text])

    assert :ok == MathLayoutValidator.validate_measured_geometry(hidden)
  end

  test "phantom decoration, rules and native linked children remain absent from PDF", %{
    style: style
  } do
    for content <- [
          "<mn>Hidden</mn>",
          "<mfrac><mn>1</mn><mn>2</mn></mfrac>",
          "<mtext><a href='https://hidden.test'>Hidden</a></mtext>"
        ] do
      css = "background-color:red;border:1px solid blue;padding:2pt"

      assert {:ok, visible} =
               styled_math(
                 "<math style='font-size:20pt'><mrow style='#{css}'>#{content}</mrow></math>"
               )

      assert {:ok, hidden} =
               styled_math(
                 "<math style='font-size:20pt'><mphantom style='#{css}'>#{content}</mphantom></math>"
               )

      assert Map.drop(hidden, [:glyphs, :drawing_items, :actual_text]) ==
               Map.drop(visible, [:glyphs, :drawing_items, :actual_text])

      assert hidden.drawing_items == [] and hidden.glyphs == [] and hidden.actual_text == ""
      assert :ok == MathLayoutValidator.validate_measured_geometry(hidden)

      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<math><mn>7</mn><mphantom style='#{css}'>#{content}</mphantom><mn>8</mn></math>"
               )

      assert {:ok, "78"} = Text.extract(pdf, layout: false)
      refute pdf =~ "hidden.test"
    end

    assert {:ok, empty} = MathLayout.measure(element("mphantom", [], style))
    assert empty.width == 0 and empty.ascent == 0 and empty.descent == 0
  end

  test "phantom retains embellished form, external stretch and outer spacing", %{style: style} do
    op = token("mo", "(", style)
    hidden = element("mphantom", [op], style)
    assert {:ok, wrapper} = MathLayout.measure(hidden, %{block_stretch_size: {40, 10}})

    assert {:ok, ordinary} =
             MathLayout.measure(element("mrow", [op], style), %{block_stretch_size: {40, 10}})

    assert Map.drop(wrapper, [:glyphs, :drawing_items, :actual_text]) ==
             Map.drop(ordinary, [:glyphs, :drawing_items, :actual_text])

    assert wrapper.core_operator.stretchy and wrapper.glyphs == []
    plus = token("mo", "+", style, %{"lspace" => "3pt", "rspace" => "7pt"})
    number = token("mn", "1", style)

    for tag <- ~w(mrow mphantom) do
      node = element("math", [element(tag, [plus], style), number], style)
      assert {:ok, prepared} = MathLayoutValidator.prepare_element(node, %{})
      assert hd(prepared.children).core_operator.form == :prefix
      assert {:ok, measured} = MathLayout.measure(node)
      assert {:ok, glyph} = MathLayout.measure(plus)
      assert {:ok, digit} = MathLayout.measure(number)
      assert_in_delta measured.width, glyph.width + 10 + digit.width, 1.0e-10
      assert measured.actual_text == if(tag == "mrow", do: "+1", else: "1")
    end
  end

  test "hidden content still uses semantic validation and selected-font work limits", %{
    style: style
  } do
    for tag <- ~w(msup mstyle mpadded merror) do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render("<math><mphantom><#{tag}/></mphantom></math>")

      assert diagnostic.stage == :layout and diagnostic.source == tag
      assert diagnostic.module == MathLayoutValidator and diagnostic.reason == :unsupported_html
    end

    assert {:error, {:invalid_css, diagnostic}} =
             HtmlToPdf.render(
               "<math><mphantom style='visibility:visible'><mn>1</mn></mphantom></math>"
             )

    assert diagnostic.stage == :css and diagnostic.message =~ "visibility"
    Limits.install(Map.put(Limits.effective(), :max_font_math_work, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             MathLayout.measure(element("mphantom", [token("mn", "1", style)], style))

    assert diagnostic.message =~ "max_font_math_work"
    assert diagnostic.operation == :measure_math
  end
end
