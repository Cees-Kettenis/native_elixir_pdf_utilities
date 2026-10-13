defmodule NativeElixirPdfUtilities.HtmlToPdf.MathLayoutSchemaCssFlowTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, MathLayout, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  defp styled_math(html) do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      with {:ok, dom} <- HtmlParser.parse_detailed(html),
           {:ok, %{children: [node]}} <- Style.compute_detailed(dom, system_font_discovery: false) do
        {:ok, node.style._math_box}
      end
    end)
  end

  test "ordinary inline schemas use CSS flow and retain selected math children" do
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, font} = Font.resolve("math", 400, :normal, registry)
    {:ok, one} = MathLayout.glyph("1", font, 20)
    {:ok, two} = MathLayout.glyph("2", font, 20)

    for tag <- ["mfrac", "msqrt", "mroot"], display <- ["inline", "inline-block"] do
      html =
        "<math style='font-size:20pt'><#{tag} style='display:#{display};padding:0'><mn style='display:inline math;math-depth:0;font-size:20pt'>1</mn><mn style='display:inline math;math-depth:0;font-size:20pt'>2</mn></#{tag}></math>"

      assert {:ok, measured} = styled_math(html)
      assert measured.actual_text == "12"
      assert_in_delta measured.width, one.width + two.width, 1.0e-10
      assert measured.glyphs == []
      assert Enum.all?(measured.drawing_items, &(&1.kind == :native))
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, "12"} = Text.extract(pdf, layout: false)
    end
  end

  test "ordinary block children use the shared CSS final line baseline" do
    for tag <- ["mfrac", "msqrt", "mroot"] do
      html =
        "<math style='font-size:20pt'><#{tag} style='display:inline-block;padding:0'><mn style='math-depth:0;font-size:20pt'>12</mn><mn style='math-depth:0;font-size:20pt'>34</mn></#{tag}></math>"

      assert {:ok, measured} = styled_math(html)
      {:ok, registry} = Font.load_registry(system_font_discovery: false)
      {:ok, _, font} = Font.resolve("math", 400, :normal, registry)
      {:ok, bounds} = Font.glyph_bounds(font)
      scale = 20 / font.units_per_em
      extents = Enum.map(~c"1234", &elem(bounds, font.cmap[&1]))
      [one, two, three, four] = extents
      first_ascent = max(elem(one, 3), elem(two, 3)) * scale
      first_descent = -min(elem(one, 1), elem(two, 1)) * scale
      second_ascent = max(elem(three, 3), elem(four, 3)) * scale
      second_descent = -min(elem(three, 1), elem(four, 1)) * scale
      assert_in_delta measured.ascent, first_ascent + first_descent + second_ascent, 1.0e-10
      assert_in_delta measured.descent, second_descent, 1.0e-10
      assert measured.actual_text == "1234"
    end
  end

  test "authored ordinary content width wraps math children through shared CSS layout" do
    children =
      "<mn style='display:inline math;math-depth:0;font-size:20pt'>12</mn><mn style='display:inline math;math-depth:0;font-size:20pt'>34</mn>"

    assert {:ok, natural} =
             styled_math(
               "<math style='font-size:20pt'><mfrac style='display:inline-block;padding:0'>#{children}</mfrac></math>"
             )

    assert {:ok, constrained} =
             styled_math(
               "<math style='font-size:20pt'><mfrac style='display:inline-block;padding:0;width:25pt'>#{children}</mfrac></math>"
             )

    assert_in_delta constrained.width, 25, 0.75 / 64
    assert constrained.ascent + constrained.descent > natural.ascent + natural.descent
    assert constrained.actual_text == natural.actual_text
  end

  test "ordinary inline schemas preserve CSS's ignored width and height behavior" do
    children =
      "<mn style='display:inline math;math-depth:0;font-size:20pt'>12</mn><mn style='display:inline math;math-depth:0;font-size:20pt'>34</mn>"

    for tag <- ["mfrac", "msqrt", "mroot"] do
      html =
        "<math style='font-size:20pt'><#{tag} style='display:inline;padding:2pt'>#{children}</#{tag}></math>"

      sized =
        String.replace(
          html,
          "display:inline;padding:2pt",
          "display:inline;padding:2pt;width:100pt;height:80pt"
        )

      assert {:ok, ordinary} = styled_math(html)
      assert {:ok, authored} = styled_math(sized)
      assert authored.width == ordinary.width
      assert authored.ascent == ordinary.ascent
      assert authored.descent == ordinary.descent
      assert authored.actual_text == ordinary.actual_text
    end
  end

  test "ordinary schema box sizing and min/max dimensions use native CSS geometry" do
    children =
      "<mn style='display:inline math;math-depth:0;font-size:20pt'>12</mn><mn style='display:inline math;math-depth:0;font-size:20pt'>34</mn>"

    for tag <- ["mfrac", "msqrt", "mroot"] do
      assert {:ok, border_box} =
               styled_math(
                 "<math style='font-size:20pt'><#{tag} style='display:inline-block;box-sizing:border-box;width:50pt;height:40pt;padding:4pt;border:1pt solid blue'>#{children}</#{tag}></math>"
               )

      assert border_box.width == 50
      # Ordinary CSS dimensions retain the shared fixed 1/64 CSS-pixel precision.
      assert_in_delta border_box.ascent + border_box.descent, 40, 0.75 / 64
      assert border_box.actual_text == "1234"

      assert {:ok, constrained} =
               styled_math(
                 "<math style='font-size:20pt'><#{tag} style='display:inline-block;max-width:25pt;min-height:40pt;padding:0'>#{children}</#{tag}></math>"
               )

      assert_in_delta constrained.width, 25, 0.75 / 64
      assert constrained.ascent + constrained.descent >= 40
      assert_in_delta constrained.ascent + constrained.descent, 2 * 24.75, 0.75 / 64
      assert constrained.actual_text == "1234"
    end
  end

  test "ordinary schema preserves nested native rules and roots and authored child depths" do
    html =
      "<math style='font-size:20pt'><mfrac style='display:inline-block;padding:3pt;background:#eef'><msqrt><mn>1</mn></msqrt><mfrac displaystyle=true><mn style='math-depth:0'>2</mn><mn style='font-size:16pt'>3</mn></mfrac><mn style='display:none'>Hidden</mn></mfrac></math>"

    assert {:ok, measured} = styled_math(html)
    assert measured.actual_text == "123"
    assert Enum.any?(measured.drawing_items, &match?(%{kind: :native, box: %{type: :rect}}, &1))
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "123"} = Text.extract(pdf, layout: false)
    assert pdf =~ "/ActualText"
    refute measured.actual_text =~ "√"
  end

  test "CSS schema children retain link artwork and independently authored boxes" do
    html =
      "<math><mroot style='display:block'><mtext style='padding:2pt;background:#efe'><a href='https://schema.test/'>Link</a></mtext><mn style='math-depth:0;font-size:18pt'>4</mn></mroot></math>"

    assert {:ok, measured} = styled_math(html)
    assert measured.actual_text == "Link4"
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert pdf =~ "https://schema.test/"
    assert {:ok, "Link4"} = Text.extract(pdf, layout: false)
  end
end
