defmodule NativeElixirPdfUtilities.HtmlToPdf.MathDefaultCssBoxesTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, MathLayout, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  defp measured(html) do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      with {:ok, dom} <- HtmlParser.parse_detailed(html),
           {:ok, %{children: [node]}} <- Style.compute_detailed(dom, system_font_discovery: false) do
        {:ok, node.style._math_box}
      end
    end)
  end

  test "default root and nested group boxes use canonical CSS constraints once" do
    for tag <- ~w(math mrow mstyle merror custom-group) do
      owner =
        "<#{tag} style='font-size:20pt;width:min(60pt,70pt);height:40pt;box-sizing:border-box;padding:4pt;border:1pt solid blue;background:yellow'><mn>12</mn></#{tag}>"

      html = if tag == "math", do: owner, else: "<math>#{owner}</math>"
      assert {:ok, box} = measured(html)
      assert_in_delta box.width, 60, 0.012
      assert_in_delta box.ascent + box.descent, 40, 0.012
      assert box.actual_text == "12"

      assert [%{width: width, height: height}] =
               Enum.filter(box.drawing_items, &match?(%{kind: :background}, &1))

      assert_in_delta width, 60, 0.012
      assert_in_delta height, 40, 0.012
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, "12"} = Text.extract(pdf, layout: false)
    end
  end

  test "block root CSS owner retains its constraints around the atomic content" do
    html =
      "<math display='block' style='width:90pt;height:42pt;padding:4pt;border:1pt solid blue;background:yellow;box-sizing:border-box'><mn>12</mn></math>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [root]}} = Style.compute_detailed(dom, system_font_discovery: false)
    assert root.style.width == 90 and root.style.height == 42
    assert root.style.padding.left == 4
    assert root.style.border_widths.left == 0.75
    assert [atom] = root.children
    refute Map.has_key?(atom.style, :padding)
    refute Map.has_key?(atom.style, :width)
    refute Enum.any?(atom.style._math_box.drawing_items, &(&1.kind == :background))
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "12"} = Text.extract(pdf, layout: false)
  end

  test "default owners paint decoded background resources once" do
    image =
      "data:image/svg+xml;base64," <>
        Base.encode64(
          "<svg xmlns='http://www.w3.org/2000/svg' width='40' height='30'><rect width='40' height='30' fill='#80aacc'/></svg>"
        )

    for tag <- ~w(math mrow mstyle merror custom-group) do
      owner =
        "<#{tag} style='width:60pt;height:35pt;background-image:url(#{image});background-repeat:no-repeat;background-size:60pt 35pt'><mn>12</mn></#{tag}>"

      html = if tag == "math", do: owner, else: "<math>#{owner}</math>"
      assert {:ok, box} = measured(html)
      assert [background] = Enum.filter(box.drawing_items, &(&1.kind == :background))
      assert background.style.background_image != nil
      assert box.actual_text == "12"
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert pdf =~ "/Subtype /Image"
      assert {:ok, "12"} = Text.extract(pdf, layout: false)
    end
  end

  test "group constraints leave authored content and overflow intact" do
    assert {:ok, natural} = measured("<math><mrow><mn>1234</mn></mrow></math>")

    assert {:ok, box} =
             measured(
               "<math><mrow style='max-width:15pt;min-height:40pt;padding-left:3pt'><mn>1234</mn></mrow></math>"
             )

    assert_in_delta box.width, 18, 0.012
    assert_in_delta box.ascent + box.descent, 40, 0.012
    assert Enum.map(box.glyphs, & &1.glyph_id) == Enum.map(natural.glyphs, & &1.glyph_id)
    assert_in_delta hd(box.glyphs).x - hd(natural.glyphs).x, 3, 0.012
    assert List.last(box.glyphs).x > box.width
  end

  test "padding and borders contribute separately to source stretch ink" do
    for tag <- ~w(mn mo) do
      assert {:ok, natural} = measured("<math><#{tag}>1</#{tag}></math>")
      assert {:ok, padded} = measured("<math><#{tag} style='padding:5pt'>1</#{tag}></math>")

      assert {:ok, bordered} =
               measured(
                 "<math><#{tag} style='padding:5pt;border:1pt solid blue'>1</#{tag}></math>"
               )

      assert_in_delta padded.width - natural.width, 10, 0.012
      assert_in_delta padded.ink_ascent, natural.ink_ascent, 0.012
      assert_in_delta padded.ink_descent, natural.ink_descent, 0.012
      assert bordered.ink_ascent > padded.ink_ascent
      assert bordered.ink_descent > padded.ink_descent
    end
  end

  test "invalid prepared dimensions and unsupported paint have shared diagnostics" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<math><mn>1</mn></math>")
    assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, system_font_discovery: false)

    source = %{
      node
      | children: [
          %{
            type: :element,
            namespace: :mathml,
            tag: "mn",
            attributes: %{},
            style: node.style,
            children: [%{type: :text, text: "1"}]
          }
        ]
    }

    assert {:error, {_, %{stage: :layout, source: "math", message: message}}} =
             MathLayout.measure(%{source | style: Map.put(source.style, :width, -1)})

    assert message =~ "bounded"

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<math style='position:absolute;left:10pt;top:10pt'><mn>1</mn></math>"
             )

    assert {:ok, "1"} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end
end
