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

      assert measured.width ==
               NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator.snap_fragment_width(
                 one.width
               ) +
                 NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator.snap_fragment_width(
                   two.width
                 )

      assert measured.glyphs == []
      assert Enum.all?(measured.drawing_items, &(&1.kind == :native))
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, "12"} = Text.extract(pdf, layout: false)
    end
  end

  test "CSS schema Math items use their issued first line baseline" do
    for tag <- ["mfrac", "msqrt", "mroot"] do
      html =
        "<math style='font-size:20pt'><#{tag} style='display:inline-block;padding:0'><mn style='math-depth:0;font-size:20pt'>12</mn><mn style='math-depth:0;font-size:20pt'>34</mn></#{tag}></math>"

      assert {:ok, measured} = styled_math(html)
      {:ok, registry} = Font.load_registry(system_font_discovery: false)
      {:ok, _, font} = Font.resolve("math", 400, :normal, registry)

      bounds =
        Enum.map(~c"1234", fn codepoint ->
          {:ok, outlines} = Font.glyph_bounds(font)
          {left, bottom, right, top} = elem(outlines, font.cmap[codepoint])
          scale = 20 / font.units_per_em
          {left * scale, bottom * scale, right * scale, top * scale}
        end)

      [one, two, three, four] = bounds
      first_ascent = max(elem(one, 3), elem(two, 3))
      first_descent = -min(elem(one, 1), elem(two, 1))
      second_ascent = max(elem(three, 3), elem(four, 3))
      second_descent = -min(elem(three, 1), elem(four, 1))
      assert_in_delta measured.ascent, first_ascent, 1.0e-10
      assert_in_delta measured.descent, first_descent + second_ascent + second_descent, 1.0e-10
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

  test "source parent context determines inline schema used boxes and dimensions" do
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
      assert_in_delta authored.width, 104, 0.75 / 64
      assert_in_delta authored.ascent + authored.descent, 84, 0.75 / 64
      assert authored.width > ordinary.width
      assert authored.actual_text == ordinary.actual_text

      flow =
        "<math style='font-size:20pt'><msqrt style='display:inline-block;padding:0'>#{html |> String.replace("<math style='font-size:20pt'>", "") |> String.replace("</math>", "")}</msqrt></math>"

      sized_flow =
        String.replace(
          flow,
          "display:inline;padding:2pt",
          "display:inline;padding:2pt;width:100pt;height:80pt"
        )

      assert {:ok, ordinary_flow} = styled_math(flow)
      assert {:ok, authored_flow} = styled_math(sized_flow)
      assert authored_flow.width == ordinary_flow.width
      assert authored_flow.ascent == ordinary_flow.ascent
      assert authored_flow.descent == ordinary_flow.descent
      assert authored_flow.actual_text == ordinary_flow.actual_text
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

      assert_in_delta border_box.width, 50, 0.75 / 64
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

  test "nested CSS schemas retain each owner box exactly once" do
    child =
      "<msqrt style='display:inline-block;padding:3pt;background:#eef'><mn style='display:inline math;math-depth:0;font-size:20pt'>12</mn></msqrt>"

    assert {:ok, inner} = styled_math("<math style='font-size:20pt'>#{child}</math>")

    assert {:ok, nested} =
             styled_math(
               "<math style='font-size:20pt'><mfrac style='display:inline-block;padding:2pt'>#{child}</mfrac></math>"
             )

    assert_in_delta nested.width - inner.width, 4, 0.75 / 64
    assert nested.actual_text == inner.actual_text

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<math style='font-size:20pt'><mfrac style='display:inline-block;padding:2pt'>#{child}</mfrac></math>"
             )

    assert {:ok, "12"} = Text.extract(pdf, layout: false)
  end

  test "nested complete CSS owners keep constraints and images on the source box" do
    image =
      File.read!(
        Path.expand("../fixtures/html_to_pdf/browser_parity/assets/iframe-bars.png", __DIR__)
      )

    for tag <- ["mfrac", "msqrt", "mroot"] do
      child =
        "<#{tag} style='display:inline-block;max-width:25pt;min-height:40pt;padding:3pt'><mn style='display:inline math;math-depth:0;font-size:20pt'>12</mn></#{tag}>"

      assert {:ok, inner} = styled_math("<math>#{child}</math>")

      assert {:ok, nested} =
               styled_math(
                 "<math><mfrac style='display:inline-block;padding:2pt'>#{child}</mfrac></math>"
               )

      assert_in_delta nested.width, inner.width + 4, 0.75 / 64
      assert_in_delta nested.ascent + nested.descent, inner.ascent + inner.descent + 4, 0.75 / 64

      painted =
        String.replace(
          child,
          "max-width:25pt;",
          "background-image:url(schema.png);background-repeat:no-repeat;background-size:25pt 20pt;max-width:25pt;"
        )

      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<math><msqrt style='display:inline-block;padding:2pt'>#{painted}</msqrt></math>",
                 assets: %{"schema.png" => {:bytes, image}},
                 system_font_discovery: false
               )

      assert length(Regex.scan(~r/\/Im\d+ Do/, pdf)) == 1
      assert {:ok, "12"} = Text.extract(pdf, layout: false)
    end
  end

  test "fraction and radical flex/grid CSS reuse native containers and real math children" do
    for tag <- ["mfrac", "msqrt", "mroot"],
        display <- ["flex", "inline-flex", "grid", "inline-grid"] do
      html =
        "<math style='font-size:20pt'><#{tag} style='display:#{display};padding:0;gap:3pt;grid-template-columns:20pt 20pt'><mn style='math-depth:0;font-size:20pt'>1</mn><mn style='math-depth:0;font-size:20pt'>2</mn></#{tag}></math>"

      assert {:ok, measured} = styled_math(html)
      assert measured.actual_text == "12"
      assert measured.width > 20
      assert measured.glyphs == []
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, "12"} = Text.extract(pdf, layout: false)
    end
  end

  test "ordinary CSS schema backgrounds use the caller's real authorized image asset" do
    image =
      File.read!(
        Path.expand("../fixtures/html_to_pdf/browser_parity/assets/iframe-bars.png", __DIR__)
      )

    html =
      "<math><mfrac style='display:inline-block;width:40pt;height:25pt;background-image:url(schema.png);background-size:40pt 25pt'><mn style='math-depth:0;font-size:18pt'>1</mn></mfrac></math>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               assets: %{"schema.png" => {:bytes, image}},
               system_font_discovery: false
             )

    assert pdf =~ "/Subtype /Image"
    assert {:ok, "1"} = Text.extract(pdf, layout: false)
  end

  test "ordinary CSS schema overflow clips through the published shared native viewport" do
    for tag <- ["mfrac", "msqrt", "mroot"] do
      html =
        "<math><#{tag} style='display:inline-block;padding:0;width:15pt;height:12pt;overflow:hidden'><mn style='display:inline math;math-depth:0;font-size:20pt'>1234</mn></#{tag}></math>"

      assert {:ok, measured} = styled_math(html)
      assert measured.width == 15
      assert measured.actual_text == "1234"
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert pdf =~ "re W n"
      assert {:ok, "1234"} = Text.extract(pdf, layout: false)
    end
  end

  test "ordinary CSS schema relative placement retains the native source box" do
    for tag <- ["mfrac", "msqrt", "mroot"] do
      child = "<mn style='display:inline math;math-depth:0;font-size:20pt'>1234</mn>"

      plain =
        "<math><#{tag} style='display:inline-block;padding:2pt;background:#eef'>#{child}</#{tag}></math>"

      shifted =
        String.replace(
          plain,
          "display:inline-block;",
          "display:inline-block;position:relative;left:3pt;top:2pt;"
        )

      assert {:ok, natural} = styled_math(plain)
      assert {:ok, moved} = styled_math(shifted)
      assert moved.width == natural.width
      assert moved.ascent == natural.ascent
      assert moved.descent == natural.descent
      [natural_box | _] = natural.drawing_items
      [moved_box | _] = moved.drawing_items
      assert_in_delta moved_box.box.x - natural_box.box.x, 3, 0.75 / 64
      assert_in_delta moved_box.box.y - natural_box.box.y, -2, 0.75 / 64
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
