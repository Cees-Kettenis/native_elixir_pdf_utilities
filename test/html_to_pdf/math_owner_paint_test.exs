defmodule NativeElixirPdfUtilities.HtmlToPdf.MathOwnerPaintTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, ResourceContext, Style}

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    MathLayoutValidator,
    MathTableValidator
  }

  alias NativeElixirPdfUtilities.TestSupport.MathPaintGeometry

  defp box(html) do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      with {:ok, dom} <- HtmlParser.parse_detailed(html),
           {:ok, %{children: [node]}} <- Style.compute_detailed(dom, system_font_discovery: false),
           do: {:ok, node.style._math_box}
    end)
  end

  test "default Math groups retain real source artwork under relative CSS placement" do
    for tag <- ~w(math mrow mstyle merror custom-group) do
      owner =
        "<#{tag} style='position:relative;left:3pt;top:2pt;background:yellow;padding:2pt'><mn>12</mn></#{tag}>"

      html = if tag == "math", do: owner, else: "<math>#{owner}</math>"
      assert {:ok, measured} = box(html)
      assert measured.actual_text == "12"
      assert Enum.any?(measured.drawing_items, &(&1.kind == :native))
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, "12"} = Text.extract(pdf, layout: false)
    end
  end

  test "native CSS paint preserves source glyph attachments through owner edges" do
    assert {:ok, natural} = box("<math><mi>f</mi></math>")

    assert {:ok, placed} =
             box(
               "<math><mrow style='position:relative;left:3pt;padding:2pt;border:1px solid blue'><mi>f</mi></mrow></math>"
             )

    assert_in_delta placed.italic_correction, natural.italic_correction + 2.75, 1.0e-9
  end

  test "default Math groups clip overflow through the native CSS owner" do
    assert {:ok, measured} =
             box(
               "<math><mrow style='width:15pt;height:14pt;overflow:hidden;padding:2pt;background:yellow'><mn>123456</mn></mrow></math>"
             )

    assert measured.width < 20
    assert measured.actual_text == "123456"
    assert Enum.any?(measured.drawing_items, &(&1.kind == :native))

    assert {:ok, _pdf} =
             HtmlToPdf.render(
               "<math><mrow style='width:15pt;height:14pt;overflow:hidden;padding:2pt;background:yellow'><mn>123456</mn></mrow></math>",
               system_font_discovery: false
             )
  end

  test "real decoded table, row and cell images retain source content and row origin" do
    image =
      "data:image/svg+xml;base64," <>
        Base.encode64(
          "<svg xmlns='http://www.w3.org/2000/svg' width='40' height='20'><rect width='20' height='20' fill='red'/><rect x='20' width='20' height='20' fill='blue'/></svg>"
        )

    for role <- ~w(mtable mtr mtd) do
      css = "background-image:url(#{image});background-size:100% 100%;background-repeat:no-repeat"
      attrs = fn tag -> if tag == role, do: " style='#{css}'", else: "" end

      html =
        "<math><mtable#{attrs.("mtable")}><mtr#{attrs.("mtr")}><mtd#{attrs.("mtd")}><mn>12</mn></mtd><mtd><mn>345</mn></mtd></mtr></mtable></math>"

      assert {:ok, measured} = box(html)
      assert measured.actual_text == "12345"
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert pdf =~ "/Subtype /Image"
      assert {:ok, "12345"} = Text.extract(pdf, layout: false)

      if role == "mtr" do
        paints =
          for %{kind: :native, box: %{type: :image} = paint} <- measured.drawing_items, do: paint

        assert length(paints) == 2
        [first, second] = paints
        assert first.x == second.x and first.width == second.width
        assert first.clip.x < second.clip.x
        assert first.clip.width != second.clip.width
      end
    end
  end

  test "CSS Math table spacing uses integer CSS pixels and retains the authored source" do
    html =
      "<math style='font-family:STIX Two Math;font-size:20pt'><mtable style='border-spacing:4pt'><mtr><mtd><mn>12</mn></mtd><mtd><mn>345</mn></mtd></mtr></mtable></math>"

    assert {:ok, measured} = box(html)
    assert_in_delta measured.width, 92.75, 1.0e-9

    retained =
      "<div style='width:200pt'><math display='block' style='font-size:20pt'><mtable style='width:100%;border-spacing:4pt'><mtr><mtd><mn>12</mn></mtd></mtr></mtable></math></div>"

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, dom} = HtmlParser.parse_detailed(retained)

      assert {:ok, %{children: [container]}} =
               Style.compute_detailed(dom, [], context.image_budget)

      [math] = container.children
      [atom] = math.children
      [source] = atom._math_layout_source.children
      assert source.style.border_spacing == {4.0, 4.0}
      assert {:ok, prepared} = MathTableValidator.prepare(source, %{resource_context: context})
      assert prepared.style.border_spacing == {3.75, 3.75}
      assert source.style.border_spacing == {4.0, 4.0}
    end)
  end

  test "nonzero used CSS table spacing validates actual nested native and Math content" do
    for content <- [
          "<mtext><span style='background:yellow'>Native</span></mtext>",
          "<msqrt><mn>2</mn></msqrt>",
          "<mroot><mn>2</mn><mn>3</mn></mroot>",
          "<mtable><mtr><mtd><mn>2</mn></mtd></mtr></mtable>"
        ] do
      html =
        "<math><mtable style='border-spacing:1px 2px'><mtr><mtd>#{content}</mtd></mtr></mtable></math>"

      assert {:ok, measured} = box(html)
      assert measured.width > 0
      assert :ok == MathLayoutValidator.validate_measured_geometry(measured)
    end
  end

  test "source row and cell images share the canonical background tile work limit" do
    image =
      "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAICAIAAAB/FOjAAAAAGElEQVR4nGM4oSGHFYkELMCKGEY10EIDAJ7zhIFJAvtOAAAAAElFTkSuQmCC"

    for role <- ~w(mtr mtd) do
      css = "background-image:url(#{image});background-size:0.0001pt 0.0001pt"
      attrs = fn tag -> if tag == role, do: " style='#{css}'", else: "" end

      html =
        "<math><mtable><mtr#{attrs.("mtr")}><mtd#{attrs.("mtd")}><mn>12</mn></mtd></mtr></mtable></math>"

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render(html, system_font_discovery: false)

      assert diagnostic.reason == :resource_limit_exceeded and diagnostic.stage == :limits
      assert diagnostic.message =~ "background image tile"
    end
  end

  test "separate-border Math rows ignore their authored padding and borders" do
    source =
      "<math><mtable><mtr style='background:yellow;EXTRA'><mtd><mn>12</mn></mtd><mtd><mn>345</mn></mtd></mtr></mtable></math>"

    assert {:ok, natural} = box(String.replace(source, "EXTRA", ""))

    assert {:ok, decorated} =
             box(String.replace(source, "EXTRA", "padding:8pt;border:4pt solid red"))

    assert Map.take(decorated, [:width, :ascent, :descent]) ==
             Map.take(natural, [:width, :ascent, :descent])

    assert MathPaintGeometry.glyphs(decorated) == MathPaintGeometry.glyphs(natural)

    backgrounds =
      for %{kind: :background, style: style} <- decorated.drawing_items,
          get_in(style, [:_math_source, :tag]) == "mtr",
          do: style

    assert length(backgrounds) == 2

    assert Enum.all?(backgrounds, fn style ->
             Enum.all?(Map.values(style.padding), &(&1 == 0)) and
               Enum.all?(Map.values(style.border_widths), &(&1 == 0))
           end)
  end

  test "hidden atomic CSS owners retain the independent first content baseline" do
    children =
      for overflow <- ["visible", "hidden"] do
        html =
          "<div style='display:inline-block;width:70pt;height:40pt;overflow:#{overflow};padding:3pt'><span>First</span></div>"

        assert {:ok, dom} = HtmlParser.parse_detailed(html)

        assert {:ok, %{children: [node]}} =
                 Style.compute_detailed(dom, system_font_discovery: false)

        ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
          assert {:ok, child} =
                   Layout.measure_child(node, ResourceContext.current(), %{
                     formatting_context: :math
                   })

          child
        end)
      end

    [visible, hidden] = children
    assert hidden.ascent > visible.ascent

    assert_in_delta hidden.ascent - hidden.first_alphabetic_baseline,
                    visible.ascent - visible.first_alphabetic_baseline,
                    1.0e-9
  end

  test "invalid prepared owner paint reports its missing source context" do
    for prepared <- [nil, %{}, %{source_node: %{style: %{}}, source_context: nil}] do
      assert {:error, {reason, diagnostic}} =
               MathLayoutValidator.prepare_owner_paint(prepared, %{})

      assert diagnostic.reason == reason and diagnostic.stage == :layout
      assert diagnostic.operation == :measure_math
      assert diagnostic.message =~ "prepared source node"
    end
  end

  @tag :browser_parity
  @tag :math_owner_paint_original
  test "relative, clipped and image-decorated Math owners match original Chromium" do
    fixture =
      Path.expand("../fixtures/html_to_pdf/browser_parity/mathml_owner_paint.html", __DIR__)

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/mathml_owner_paint",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
