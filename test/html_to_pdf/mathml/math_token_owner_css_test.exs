defmodule NativeElixirPdfUtilities.HtmlToPdf.MathTokenOwnerCssTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  defp box(html) do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      with {:ok, dom} <- HtmlParser.parse_detailed(html),
           {:ok, %{children: [node]}} <- Style.compute_detailed(dom, system_font_discovery: false),
           do: {:ok, node.style._math_box}
    end)
  end

  defp leaves(box) do
    Enum.flat_map(Map.get(box, :boxes, []), &leaves/1) ++ [box]
  end

  test "default token and annotation owners resolve CSS boxes without changing selected glyphs" do
    for tag <- ~w(mi mn mtext ms) do
      assert {:ok, natural} = box("<math style='font-size:20pt'><#{tag}>12</#{tag}></math>")

      owner =
        "<#{tag} style='width:min(44pt,60pt);height:30pt;box-sizing:border-box;padding:3pt;border:1px solid blue;background:yellow'>12</#{tag}>"

      for content <- [owner, "<mrow style='display:inline-block'>#{owner}</mrow>"] do
        assert {:ok, measured} = box("<math style='font-size:20pt'>#{content}</math>")
        assert_in_delta measured.width, 44, 0.012
        assert_in_delta measured.ascent + measured.descent, 30, 0.012
        assert measured.actual_text == "12"

        if content == owner do
          for {glyph, source} <- Enum.zip(measured.glyphs, natural.glyphs) do
            assert_in_delta glyph.x, source.x + 3.75, 0.012
            assert_in_delta glyph.y, source.y, 0.012
          end
        end

        real =
          measured.glyphs ++
            for(
              %{kind: :native, box: native} <- measured.drawing_items,
              leaf <- leaves(native),
              Map.get(leaf, :type) == :text,
              gid <- Map.get(leaf, :glyph_ids, []),
              do: %{glyph_id: gid, font_face: leaf.font_face}
            )

        assert Enum.map(real, & &1.glyph_id) == Enum.map(natural.glyphs, & &1.glyph_id)
        assert Enum.all?(real, &(&1.font_face.data == hd(natural.glyphs).font_face.data))
      end
    end
  end

  test "clipped relative tokens paint their leaves and preserve logical text" do
    for tag <- ~w(mi mn mtext ms) do
      html =
        "<math><#{tag} style='position:relative;left:3pt;top:2pt;width:12pt;height:16pt;padding:2pt;overflow:hidden;background:yellow'>123456</#{tag}></math>"

      assert {:ok, measured} = box(html)
      assert_in_delta measured.width, 16, 0.012
      assert measured.actual_text == "123456"

      commands =
        for %{kind: :native, box: native} <- measured.drawing_items,
            leaf <- leaves(native),
            Map.get(leaf, :type) == :text,
            do: leaf

      assert Enum.flat_map(commands, &Map.get(&1, :glyph_ids, [])) != []
      assert Enum.any?(commands, &Map.has_key?(&1, :clip))
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, "123456"} = Text.extract(pdf, layout: false)
    end
  end

  test "default annotation HTML descendants use the owner content width and first baseline" do
    html =
      "<math><annotation-xml encoding='text/html' style='display:inline-block;width:80pt;box-sizing:border-box;padding:3pt;border:1px solid blue'><span style='display:block;width:50%;font-family:DejaVu Sans;font-size:12pt;line-height:18pt;white-space:normal;background:yellow'>Alpha beta gamma delta</span></annotation-xml></math>"

    assert {:ok, measured} = box(html)

    rects =
      for %{kind: :native, box: native} <- measured.drawing_items,
          leaf <- leaves(native),
          Map.get(leaf, :fill_color) == {1, 1, 0},
          do: leaf

    assert length(rects) == 1
    assert_in_delta hd(rects).width, 36.25, 0.012
    assert measured.ascent + measured.descent > 36
    assert measured.actual_text == "Alpha beta gamma delta"
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "Alpha beta gamma delta"} = Text.extract(pdf, layout: false)
  end

  test "block equations remeasure token percentage boxes at the enclosing width" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Layout

    html =
      "<div style='width:200pt'><math display='block'><mtext style='width:calc(50% + 10pt);background:yellow'>12</mtext></math></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, {styled, laid}} =
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
               with {:ok, styled} <- Style.compute_detailed(dom, system_font_discovery: false),
                    {:ok, laid} <- Layout.layout(styled, margin: 0),
                    do: {:ok, {styled, laid}}
             end)

    [container] = styled.children
    [math] = container.children
    assert math.style._math_flow_requires_inline_size
    [atom] = math.children
    assert atom._math_layout_source.node_id == math.node_id
    rect = Enum.find(laid.boxes, &(Map.get(&1, :fill_color) == {1, 1, 0}))
    assert_in_delta rect.width, 110, 0.012
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "12"} = Text.extract(pdf, layout: false)
  end

  test "default token direct inline HTML children use canonical Math block-container sizing" do
    html =
      "<math><mtext style='width:80pt;box-sizing:border-box;padding:3pt;border:1px solid blue'><span style='width:50%;font-family:DejaVu Sans;font-size:12pt;line-height:18pt;white-space:normal;background:yellow'>Alpha beta gamma delta</span></mtext></math>"

    assert {:ok, measured} = box(html)

    rects =
      for %{kind: :native, box: native} <- measured.drawing_items,
          leaf <- leaves(native),
          Map.get(leaf, :fill_color) == {1, 1, 0},
          do: leaf

    assert length(rects) == 1
    assert_in_delta hd(rects).width, 36.25, 0.012
    assert measured.actual_text == "Alpha beta gamma delta"
  end

  test "selected XML annotation keeps SVG artwork and independently prepared nested Math" do
    svg =
      "<svg width='24' height='18' viewBox='0 0 24 18'><rect width='24' height='18' fill='blue'/><text x='2' y='12' font-family='DejaVu Sans' font-size='10'>Q</text></svg>"

    html =
      "<math><annotation-xml encoding='text/html' style='display:inline-block;width:80pt;padding:3pt;background:yellow'><span>Alpha <math><mfrac><mn>1</mn><mn>2</mn></mfrac></math></span>#{svg}</annotation-xml></math>"

    assert {:ok, measured} = box(html)
    assert Enum.any?(measured.drawing_items, &(&1.kind == :native))
    assert measured.actual_text == "Alpha 12"
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "Alpha 12"} = Text.extract(pdf, layout: false)
    assert pdf =~ "/FontFile2"
    assert pdf =~ "/Subtype /Image"
  end

  test "token owner constraints retain shared diagnostics and hidden selection avoids assets" do
    assert {:ok, measured} =
             box(
               "<math><mtext style='width:30pt;min-width:60pt;max-width:40pt;min-height:36pt'>12</mtext></math>"
             )

    assert_in_delta measured.width, 60, 0.012
    assert measured.ascent + measured.descent >= 36

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render("<math><mtext style='position:absolute'>12</mtext></math>")

    assert diagnostic.stage == :layout and diagnostic.source == "mtext"
    assert diagnostic.message =~ "static or relative"

    html =
      "<math><semantics><mn>1</mn><annotation style='width:min(40pt,60pt);overflow:hidden;background-image:url(https://unselected.invalid/image.png)'>Hidden</annotation></semantics></math>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, "1"} = Text.extract(pdf, layout: false)
  end

  test "default token owner backgrounds use decoded image bytes and retain font source" do
    image =
      "data:image/svg+xml;base64," <>
        Base.encode64(
          "<svg xmlns='http://www.w3.org/2000/svg' width='8' height='8'><rect width='8' height='8' fill='blue'/></svg>"
        )

    for tag <- ~w(mi mn mtext ms) do
      assert {:ok, measured} =
               box(
                 "<math><#{tag} style='width:30pt;height:24pt;background-image:url(#{image});background-repeat:no-repeat;background-size:8pt 8pt'>12</#{tag}></math>"
               )

      assert Enum.any?(
               measured.drawing_items,
               &(&1.kind == :background and is_map(&1.style.background_image))
             )

      assert measured.actual_text == "12"
      assert Enum.all?(measured.glyphs, &is_binary(&1.font_face.data))
    end
  end

  test "block dependency discovery preserves mpadded's admitted natural-size width" do
    for width <- ["+2em", "150%", "-1width"] do
      html = "<math display='block'><mpadded width='#{width}'><mn>12</mn></mpadded></math>"
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, "12"} = Text.extract(pdf, layout: false)
    end
  end

  test "source token fragment targets emit destinations on their native paint owner" do
    alias NativeElixirPdfUtilities.Pdf.Reader

    for tag <- ~w(mi mn mtext ms mrow mo msqrt) do
      content = if tag in ~w(mrow msqrt), do: "<mn>12</mn>", else: "12"

      html =
        "<style>html,body,p{margin:0}a{text-decoration:none}</style><p><a href='#token'>Go</a></p><math><mn>1</mn><#{tag} id='token' style='padding:3pt;position:relative;left:3pt'>#{content}</#{tag}><mn>2</mn></math>"

      assert {:ok, pdf} =
               HtmlToPdf.render(html,
                 page_size: {180, 120},
                 margin: 10,
                 system_font_discovery: false
               )

      assert {:ok, context} = Reader.read_validated(pdf)

      links =
        for {_ref, %{value: %{"Subtype" => {:name, "Link"}} = value}} <-
              context.document.objects,
            do: value

      assert [link] = links
      assert [{:ref, page}, {:name, "XYZ"}, x, y, nil] = link["Dest"]
      assert page == hd(context.pages).ref
      assert x > 13 and x < 70
      assert y > 10 and y < 110
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert text =~ "Go"
      assert text =~ "1122"
    end
  end

  test "Math background failures keep the canonical actionable diagnostic contract" do
    alias NativeElixirPdfUtilities.Validators.MathLayoutValidator

    assert {:error, {:unsupported_html, diagnostic}} =
             MathLayoutValidator.validate_background_style(%{background_image: %{}}, "mtext")

    assert diagnostic.stage == :layout
    assert diagnostic.operation == :measure_math
    assert diagnostic.module == MathLayoutValidator
    assert diagnostic.source == "mtext"
    assert diagnostic.message =~ "validated decoded image data"

    assert {:error, {:invalid_document, diagnostic}} =
             MathLayoutValidator.validate_background_style(nil, "annotation-xml")

    assert diagnostic.stage == :layout
    assert diagnostic.source == "annotation-xml"
    assert diagnostic.message =~ "prepared style map"
  end
end
