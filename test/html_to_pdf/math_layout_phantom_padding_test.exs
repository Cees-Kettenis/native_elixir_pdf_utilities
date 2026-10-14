defmodule NativeElixirPdfUtilities.HtmlToPdf.MathLayoutPhantomPaddingTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, MathLayout, ResourceContext, Style}

  alias NativeElixirPdfUtilities.Validators.{
    CssValueValidator,
    HtmlValidator,
    MathLayoutValidator
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

  test "phantom scripts retain actual selected-font geometry while omitting paint" do
    content = "<msup><mn>1</mn><mn>2</mn></msup>"
    assert {:ok, visible} = styled_math("<math><mrow>#{content}</mrow></math>")
    assert {:ok, phantom} = styled_math("<math><mphantom>#{content}</mphantom></math>")

    assert Map.drop(phantom, [:actual_text, :glyphs, :drawing_items]) ==
             Map.drop(visible, [:actual_text, :glyphs, :drawing_items])

    assert phantom.actual_text == "" and phantom.drawing_items == []
  end

  test "padding defaults and ignored percentage/legacy values preserve the inner row", %{
    style: style
  } do
    content = [token("mn", "23", style), token("mi", "x", style)]
    assert {:ok, row} = MathLayout.measure(element("mrow", content, style))

    for attributes <- [
          %{},
          %{
            "width" => "200%",
            "height" => "50%",
            "depth" => "40%",
            "lspace" => "25%",
            "voffset" => "-10%"
          },
          %{"width" => "2width", "height" => "nonsense", "depth" => "1depth"}
        ] do
      assert {:ok, padded} = MathLayout.measure(element("mpadded", content, style, attributes))
      # The source owner changes while the inferred row geometry stays the same.
      assert Map.drop(padded, [:drawing_items, :css_math_owner]) ==
               Map.drop(row, [:drawing_items, :css_math_owner])
    end

    assert {:ok, hidden} =
             styled_math(
               "<math><mn>2</mn><mphantom><mpadded width='40pt' height='15pt' depth='4pt'><mn>Hidden</mn></mpadded></mphantom><mn>3</mn></math>"
             )

    assert hidden.actual_text == "23"
    assert length(hidden.glyphs) == 2
  end

  test "padding requested dimensions are independent from glyph translation and ink", %{
    style: style
  } do
    content = [token("mn", "23", style)]
    assert {:ok, inner} = MathLayout.measure(element("mrow", content, style))

    for shift <- [-5, 4] do
      attrs = %{
        "width" => "40pt",
        "height" => "22pt",
        "depth" => "8pt",
        "lspace" => "3pt",
        "voffset" => "#{shift}pt"
      }

      assert {:ok, measured} = MathLayout.measure(element("mpadded", content, style, attrs))

      assert measured.width == 40 and measured.min_content_width == 40 and
               measured.max_content_width == 40

      assert measured.ascent == 22 and measured.descent == 8
      assert_in_delta measured.ink_ascent, inner.ink_ascent + shift, 1.0e-10
      assert_in_delta measured.ink_descent, inner.ink_descent - shift, 1.0e-10

      for {glyph, source} <- Enum.zip(measured.glyphs, inner.glyphs) do
        assert_in_delta glyph.x, source.x + 3, 1.0e-10
        assert_in_delta glyph.y, source.y + shift, 1.0e-10
        assert glyph.glyph_id == source.glyph_id and glyph.font_face == source.font_face
      end

      assert measured.actual_text == "23"
    end

    attrs = %{
      "width" => "-1pt",
      "height" => "-2pt",
      "depth" => "-3pt",
      "lspace" => "-4pt",
      "voffset" => "-5pt"
    }

    assert {:ok, measured} = MathLayout.measure(element("mpadded", content, style, attrs))
    assert measured.width == inner.width and measured.ascent == 0 and measured.descent == 0
    assert hd(measured.glyphs).x == 0 and hd(measured.glyphs).y == -5
  end

  test "padding width hints honor final selected font, CSS override and generic CSS boundaries",
       %{style: style, font: font} do
    {:ok, x_height} = NativeElixirPdfUtilities.Validators.FontValidator.prepare_x_height(font)

    for {width, css, expected} <- [
          {"2em", "font-size:18pt", 36},
          {"2ex", "font-size:18pt", 36 * x_height},
          {"3rem", "font-size:18pt", 36},
          {"30pt", "width:17pt", 17},
          {"30pt", "width:auto", nil},
          {"30pt", "width:2em;font-size:18pt", 36}
        ] do
      assert {:ok, measured} =
               styled_math(
                 "<math style='font-size:20pt'><mpadded width='#{width}' style='#{css}'><mn>23</mn></mpadded></math>"
               )

      if expected,
        do: assert_in_delta(measured.width, expected, 1.0e-10),
        else: assert(measured.width < 30)
    end

    for attrs <- [
          %{"lspace" => " 1EM ", "voffset" => " -1ex "},
          %{"lspace" => "1rem", "voffset" => "2pt"}
        ] do
      assert {:ok, measured} =
               MathLayout.measure(element("mpadded", [token("mn", "2", style)], style, attrs))

      assert_in_delta hd(measured.glyphs).x, 20, 1.0e-10
    end

    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, selected} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    {:ok, selected_x_height} =
      NativeElixirPdfUtilities.Validators.FontValidator.prepare_x_height(selected)

    assert selected_x_height != x_height

    assert {:ok, switched} =
             styled_math(
               "<math style='font-size:20pt'><mpadded width='2ex' style='font-size:18pt;font-family:DejaVu Sans'><mtext>word</mtext></mpadded></math>"
             )

    assert_in_delta switched.width, 36 * selected_x_height, 1.0e-10

    assert {:ok, %{width: 40.0}} = CssValueValidator.validate_resolved(style, "width", "2em")

    assert {:ok, %{width: {:percent, 0.5}}} =
             CssValueValidator.validate_resolved(style, "width", "50%")

    assert :error ==
             CssValueValidator.validate_resolved(
               Map.put(style, :_math_element, {:mathml, "mpadded"}),
               "width",
               "-2pt"
             )

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render("<math><mpadded style='width:50%'><mn>2</mn></mpadded></math>")

    assert diagnostic.source == "mpadded" and diagnostic.message =~ "composition"

    assert {:error, {:invalid_document, diagnostic}} =
             MathLayoutValidator.compute_style(%{width: {:math_padded_length, "1ex"}})

    assert diagnostic.source == "mpadded" and diagnostic.message =~ "context"
  end

  test "padding retains native token content, fraction rules and original visible Unicode" do
    html =
      "<math style='font-size:18pt'><mpadded width='70pt' height='25pt' depth='8pt' lspace='4pt' voffset='3pt' style='background-color:lightblue;padding:2pt'><mtext><a href='https://visible.test'>Visible</a></mtext><mfrac><mn>2</mn><mn>3</mn></mfrac><mi>α</mi></mpadded></math>"

    assert {:ok, measured} = styled_math(html)
    assert measured.actual_text == "Visible23α"
    assert hd(measured.drawing_items).kind == :background
    assert Enum.any?(measured.drawing_items, &(Map.get(&1, :kind) == :native))
    assert :ok == MathLayoutValidator.validate_measured_geometry(measured)
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, "Visible23α"} = Text.extract(pdf, layout: false)
    assert pdf =~ "visible.test"
  end

  test "padding placement rejects malformed preferred geometry and offset overflow through validators",
       %{style: style} do
    node = element("mpadded", [token("mn", "2", style)], style, %{"lspace" => "4pt"})
    assert {:ok, prepared} = MathLayoutValidator.prepare_element(node, %{})
    assert {:ok, inner} = MathLayout.measure(element("mrow", node.children, style))

    for bad <- [Map.delete(inner, :min_content_width), Map.put(inner, :max_content_width, :bad)] do
      assert {:error, {:invalid_document, diagnostic}} =
               MathLayoutValidator.prepare_padded_placement(prepared, bad)

      assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
      assert diagnostic.source == "mpadded"
      assert diagnostic.message =~ "intrinsic"
    end

    for bad <- [
          put_in(prepared.parameters.voffset, :bad),
          put_in(prepared.parameters.lspace, 1.0e308),
          %{}
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               MathLayoutValidator.prepare_padded_placement(bad, inner)

      assert diagnostic.source == "mpadded" and diagnostic.message =~ "prepared"
    end

    Limits.install(Map.put(Limits.effective(), :max_css_numeric_magnitude, 30))

    assert {:error, {:invalid_document, diagnostic}} =
             MathLayoutValidator.prepare_padded_placement(
               put_in(prepared.parameters.voffset, 30),
               inner
             )

    assert diagnostic.source == "mpadded" and diagnostic.message =~ "bounds"
  end

  test "padded space-like rows preserve authoritative outer operator form", %{style: style} do
    padded = element("mpadded", [token("mtext", "word", style)], style, %{"width" => "30pt"})
    root = element("math", [padded, token("mo", "+", style), token("mn", "1", style)], style)
    assert {:ok, prepared} = MathLayoutValidator.prepare_element(root, %{})
    assert Enum.at(prepared.children, 1).core_operator.form == :prefix
    assert {:ok, measured} = MathLayout.measure(root)
    assert measured.actual_text == "word+1"
  end

  test "padded inferred row shares one owner decoration with actual script and native content" do
    html =
      "<math style='font-size:18pt'><mpadded width='70pt' lspace='3pt' voffset='2pt' style='background-color:lightblue;padding:2pt'><msubsup><mn>7</mn><mn>1</mn><mn>2</mn></msubsup><mtext><a href='https://padded.test'>Link</a></mtext></mpadded></math>"

    assert {:ok, measured} = styled_math(html)
    assert Enum.count(measured.drawing_items, &(Map.get(&1, :kind) == :background)) == 1
    assert measured.actual_text == "712Link"
    assert Enum.any?(measured.drawing_items, &(Map.get(&1, :kind) == :native))
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "712Link"} = Text.extract(pdf, layout: false)
    assert pdf =~ "padded.test"
  end

  test "hidden content still uses semantic validation and selected-font work limits", %{
    style: style
  } do
    for tag <- ~w(mpadded) do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(
                 "<math><mphantom><#{tag} style='position:absolute'/></mphantom></math>"
               )

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
