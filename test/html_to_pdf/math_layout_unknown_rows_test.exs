defmodule NativeElixirPdfUtilities.HtmlToPdf.MathLayoutUnknownRowsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, MathLayout, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, MathLayoutValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  defp styled_math(html) do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _context ->
      with {:ok, dom} <- HtmlParser.parse_detailed(html),
           {:ok, %{children: [node]}} <- Style.compute_detailed(dom, system_font_discovery: false) do
        {:ok, node.style._math_box}
      end
    end)
  end

  test "unknown MathML names use actual mrow glyph and intrinsic geometry" do
    for content <- [
          "",
          "<mn>12</mn><mo>+</mo><mn>α</mn>",
          "<mo>+</mo><mn>12</mn><mo>!</mo>",
          "<apply><mn>β</mn></apply>",
          " α <mn>12</mn> β "
        ] do
      assert {:ok, expected} = styled_math("<math><mrow>#{content}</mrow></math>")

      assert {:ok, unknown} =
               styled_math("<math><equation-data>#{content}</equation-data></math>")

      for field <- [
            :width,
            :ascent,
            :descent,
            :min_content_width,
            :max_content_width,
            :ink_ascent,
            :ink_descent,
            :actual_text,
            :glyphs
          ] do
        assert Map.fetch!(unknown, field) == Map.fetch!(expected, field)
      end

      assert :ok == MathLayoutValidator.validate_measured_geometry(unknown)
    end
  end

  test "unknown nested embellished rows retain operator form and space-like classification" do
    for tag <- ["mrow", "equation-data"] do
      assert {:ok, measured} =
               styled_math(
                 "<math><#{tag}><mo>+</mo><annotation style='display:none'>Hidden</annotation></#{tag}><mn>1</mn></math>"
               )

      assert {:ok, ordinary} = styled_math("<math><mo>+</mo><mn>1</mn></math>")
      assert measured.width == ordinary.width and measured.glyphs == ordinary.glyphs

      assert {:ok, spaced} =
               styled_math("<math><#{tag}><mtext>α</mtext></#{tag}><mo>+</mo><mn>1</mn></math>")

      assert {:ok, ordinary} = styled_math("<math><mtext>α</mtext><mo>+</mo><mn>1</mn></math>")
      assert spaced.width == ordinary.width and spaced.glyphs == ordinary.glyphs
    end
  end

  test "unknown row source identity and selected-font Unicode stay original" do
    html =
      "<math><equation-data id='authored' data-value='original'><mn>αβ12</mn></equation-data></math>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    [unknown] = hd(dom.children).children
    assert unknown.namespace == :mathml and unknown.tag == "equation-data"
    assert unknown.attributes == %{"id" => "authored", "data-value" => "original"}
    assert unknown.node_id != unknown.parent_id
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, "αβ12"} = Text.extract(pdf, layout: false)
  end

  test "known unimplemented Core schemata cannot bypass semantic authority through unknown rows" do
    for tag <-
          ~w(msup msub msubsup mmultiscripts mover munder munderover mstyle merror mpadded mprescripts) do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render("<math><equation-data><#{tag}/></equation-data></math>")

      assert diagnostic.stage == :layout and diagnostic.source == tag
      assert diagnostic.reason == :unsupported_html and diagnostic.module == MathLayoutValidator
      assert diagnostic.operation == :measure_math
    end
  end

  test "anonymous row text inherits font properties while owner boxes paint once" do
    for tag <- ["mrow", "equation-data"],
        css <- [
          "padding:2pt;border:1px solid blue;background-color:lightyellow",
          "width:60pt;padding:2pt;border:1px solid blue;background-color:lightyellow"
        ] do
      assert {:ok, explicit} =
               styled_math(
                 "<math style='font-size:20pt'><#{tag} style='#{css}'><mtext>12</mtext><mn>3</mn><mtext>α</mtext></#{tag}></math>"
               )

      assert {:ok, anonymous} =
               styled_math(
                 "<math style='font-size:20pt'><#{tag} style='#{css}'>12<mn>3</mn>α</#{tag}></math>"
               )

      assert anonymous.actual_text == "123α"
      assert anonymous.glyphs == explicit.glyphs

      for field <- [
            :width,
            :ascent,
            :descent,
            :min_content_width,
            :max_content_width,
            :ink_ascent,
            :ink_descent
          ] do
        assert Map.fetch!(anonymous, field) == Map.fetch!(explicit, field)
      end

      assert Enum.count(anonymous.drawing_items, &(&1.kind == :background)) == 1
      assert :ok == MathLayoutValidator.validate_measured_geometry(anonymous)
    end
  end

  test "malformed direct unknown element names return shared input diagnostics" do
    assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
    assert {:ok, _, font} = Font.resolve("math", 400, :normal, registry)
    style = %{display: :block, font_size: 12, font_face: font, color: {0, 0, 0}, line_height: 18}

    for tag <- [nil, :unknown, %{}, "", "two names", "1unknown", "bad/name", <<255>>] do
      node = %{
        type: :element,
        namespace: :mathml,
        tag: tag,
        attributes: %{},
        style: style,
        children: []
      }

      assert {:error, {reason, diagnostic}} = MathLayout.measure(node)
      assert diagnostic.reason == reason and diagnostic.operation == :measure_math
      assert diagnostic.stage == :layout and diagnostic.module == MathLayoutValidator
      assert diagnostic.message =~ "tag names"
    end
  end

  test "unknown rows obey shared font work and ordinary-flow diagnostics" do
    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render(
               "<math><equation-data style='display:inline-block'><mn>1</mn></equation-data></math>"
             )

    assert diagnostic.source == "equation-data" and diagnostic.stage == :layout
    assert diagnostic.message =~ "ordinary flow"

    Limits.install(Map.put(Limits.effective(), :max_font_math_work, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             styled_math("<math><equation-data><mn>12</mn></equation-data></math>")

    assert diagnostic.stage == :font and diagnostic.message =~ "max_font_math_work"
  end
end
