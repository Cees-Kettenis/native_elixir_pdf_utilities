defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout, SvgBackend, SvgScene}
  alias NativeElixirPdfUtilities.Validators.{FontValidator, HtmlValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    %{limits: limits, data: data}
  end

  test "selected alias transports exact authorized bytes and real CID glyphs for multibyte and ligature clusters",
       %{data: data} do
    original = "office ffi ﬃ café á a\u0301 Ω 𝄞"
    # DejaVu lacks the musical symbol: use a covered supplementary scalar.
    original = String.replace(original, "𝄞", "𝔸")

    html =
      "<svg width=300 height=50 style='font-family:SceneFace;font-size:20px'><text x=3 y=30>#{original}</text></svg>"

    options = [fonts: [%{family: "SceneFace", data: data}], system_font_discovery: false]
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom, options)
    scene = svg.style.svg_scene
    assert [%{data: ^data, family: "SceneFace", source: :configured}] = scene.selected_fonts
    assert scene.source =~ "font-family=\"NativeSvgFont0\""
    assert scene.logical_text == [original]
    assert {:ok, pdf} = HtmlToPdf.render(html, options)
    assert {:ok, document} = Text.extract_spans(pdf)
    extracted = Enum.map_join(document.pages, fn page -> Enum.map_join(page.spans, & &1.text) end)
    assert extracted =~ original
    assert {:ok, public_text} = Text.extract(pdf)
    assert public_text =~ original
    assert pdf =~ "/CIDToGIDMap /Identity"
    assert pdf =~ "/FontFile2"
    refute pdf =~ "/Subtype /Image"
    assert {:ok, doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    streams = Enum.map(Map.values(doc.objects), &(Map.get(&1, :stream) || ""))
    assert data in streams
    content = Enum.join(streams)
    refute content =~ "3 Tr"
    assert content =~ "/ActualText"
    # Ligatures are one real glyph with the original multi-character cluster.
    assert content =~ "(ffi)"
    assert content =~ " Tj"
  end

  test "tspan scalar positions anchors and independently selected styles preserve logical order" do
    html = """
    <svg width=300 height=100 font-family='DejaVu Sans' font-size=20 text-anchor=middle>
    <text x=150 y=25>One<tspan dx=4 font-weight=700>Bold</tspan><tspan x=150 y=65 font-style=italic>café Ω</tspan></text>
    </svg>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    scene = svg.style.svg_scene
    assert length(scene.selected_fonts) == 3
    assert scene.source =~ "text-anchor=\"middle\""
    assert scene.source =~ "dx=\"4.0\""
    assert scene.logical_text == ["OneBoldcafé Ω"]
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, document} = Text.extract_spans(pdf)
    extracted = Enum.map_join(document.pages, fn page -> Enum.map_join(page.spans, & &1.text) end)
    assert {:ok, public_text} = Text.extract(pdf)
    for text <- ["One", "Bold", "café Ω"], do: assert(extracted =~ text and public_text =~ text)
  end

  test "uppercase original font attributes use shared CSS authority and the selected physical face" do
    html =
      "<svg width=200 height=50 font-family='DejaVu Sans'><text y=25 font-size='20px' font-weight='BOLD' font-style='ITALIC' letter-spacing='NORMAL'>Original Ω</text></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, "Original Ω"} = Text.extract(pdf)
    assert {:ok, doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    data = File.read!("priv/fonts/dejavu/DejaVuSans-BoldOblique.ttf")
    assert Enum.any?(Map.values(doc.objects), &(Map.get(&1, :stream) == data))
  end

  test "raw backend font declarations retain shared inherited relative size policy" do
    data = File.read!("priv/fonts/dejavu/DejaVuSans-BoldOblique.ttf")

    raw =
      source(
        "<g font-size='20px'><text y='25' font-size='50%' font-weight='BOLD' font-style='ITALIC' letter-spacing='NORMAL' font-family='NativeSvgFont0'>Relative Ω</text></g>"
      )

    assert {:ok, pdf} =
             SvgBackend.vectorize(raw, [width: 200, height: 50], %{
               mode: :inline,
               fonts: [%{data: data}]
             })

    assert {:ok, "Relative Ω"} = Text.extract(pdf)
    assert {:ok, spans} = Text.extract_spans(pdf)
    assert Enum.all?(hd(spans.pages).spans, &(&1.font_size == 10 and &1.paints_text?))
  end

  test "original letter-spacing participates in the host presentation cascade" do
    html =
      "<svg width=200 height=50 font-family='DejaVu Sans'><text y=30 font-size=20 letter-spacing='2px'>AA</text></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, doc} = Text.extract_spans(pdf)
    assert [first, second] = hd(doc.pages).spans
    assert first.text == "A" and second.text == "A"

    assert {:ok, original_pdf} =
             HtmlToPdf.render(String.replace(html, "letter-spacing='2px'", ""))

    assert {:ok, original} = Text.extract_spans(original_pdf)
    assert [unspaced_first, unspaced_second] = hd(original.pages).spans
    assert_in_delta second.x - first.x - (unspaced_second.x - unspaced_first.x), 2 * 0.75, 0.001
  end

  test "invalid original font declarations are diagnosed even when host CSS overrides them" do
    for {property, invalid, override} <- [
          {"font-weight", "450", "700"},
          {"font-style", "oblique", "italic"},
          {"font-size", "bad", "20px"},
          {"letter-spacing", "bad", "normal"}
        ] do
      html =
        "<svg width=200 height=50 font-family='DejaVu Sans'><text y=25 #{property}='#{invalid}' style='#{property}:#{override}'>Original Ω</text></svg>"

      assert {:error, {reason, diagnostic}} = HtmlToPdf.render(html)
      assert reason in [:invalid_document, :invalid_css]
      assert diagnostic.reason == reason
      assert diagnostic.stage == :css
      assert diagnostic.message =~ property
      assert diagnostic.message =~ invalid
    end
  end

  test "supplied styled SVG trees cannot hide invalid original font attributes behind cached scenes" do
    html =
      "<svg width=200 height=50 font-family='DejaVu Sans'><text y=25 font-weight=700>Original Ω</text></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]} = styled} = Style.compute_detailed(dom)
    [text] = hd(dom.children).children

    text =
      text
      |> Map.put(:style, Map.drop(svg.style, [:svg_scene]))
      |> Map.put(:attributes, Map.put(text.attributes, "font-weight", "450"))

    svg = %{svg | children: [text]}

    for result <- [
          SvgScene.prepare(svg, HtmlValidator.new_image_budget()),
          Layout.layout(%{styled | children: [svg]}, page_size: {200, 100}, margin: 0)
        ] do
      assert {:error, {:invalid_css, diagnostic}} = result
      assert diagnostic.reason == :invalid_css
      assert diagnostic.stage == :css
      assert diagnostic.message =~ "font-weight"
      assert diagnostic.message =~ "450"
    end
  end

  test "original font source validation diagnoses an incomplete supplied node" do
    assert {:error, {:invalid_document, diagnostic}} =
             NativeElixirPdfUtilities.Validators.SvgTextValidator.validate_original_fonts(%{})

    assert diagnostic.stage == :layout
    assert diagnostic.reason == :invalid_document
    assert diagnostic.message =~ "original attributes and children"
    assert diagnostic.operation == :prepare
  end

  test "same face text reuse expands outlines and original logical clusters in instance order" do
    html = """
    <svg width=220 height=70 font-family='DejaVu Sans' font-size=16>
    <defs><g id=label><text y=20>Repeat Ω</text></g></defs>
    <use href='#label' x=10 /><use href='#label' x=110 y=30 />
    </svg>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    assert svg.style.svg_scene.logical_text == ["Repeat Ω", "Repeat Ω"]
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, document} = Text.extract_spans(pdf)
    extracted = Enum.map_join(document.pages, fn page -> Enum.map_join(page.spans, & &1.text) end)
    assert length(Regex.scan(~r/Repeat Ω/, extracted)) == 2
    assert {:ok, public_text} = Text.extract(pdf)
    assert length(Regex.scan(~r/Repeat Ω/, public_text)) == 2
    assert {:ok, doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    assert Enum.count(
             Map.values(doc.objects),
             &(get_in(&1, [:value, "Subtype"]) == {:name, "Type0"})
           ) == 1
  end

  test "hidden text does not create detached logical extraction while visible overridden spans survive" do
    html = """
    <svg width=200 height=80 font-family='DejaVu Sans'>
    <text x=10 y=20 visibility=hidden>Hidden<tspan visibility=visible>Visible Ω</tspan></text>
    <text x=10 y=40 display=none>Removed</text>
    <rect x=10 y=55 width=30 height=10 />
    </svg>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, document} = Text.extract_spans(pdf)
    text = Enum.map_join(document.pages, fn page -> Enum.map_join(page.spans, & &1.text) end)
    assert {:ok, public_text} = Text.extract(pdf)
    assert public_text =~ "Visible Ω"
    refute public_text =~ "Hidden" or public_text =~ "Removed"
    assert text =~ "Visible Ω"
    refute text =~ "Hidden"
    refute text =~ "Removed"
  end

  test "reused hidden text keeps only explicitly visible logical spans" do
    html =
      "<svg width=200 height=50 font-family='DejaVu Sans'><defs><g id='label'><text y=25>Hidden<tspan visibility='visible'>Visible Ω</tspan></text></g></defs><use href='#label' visibility='hidden' /></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    assert svg.style.svg_scene.logical_text == ["Visible Ω"]
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, "Visible Ω"} = Text.extract(pdf)
  end

  test "raw inline aliases diagnose missing coverage and never use another supplied face", %{
    data: data
  } do
    font = %{data: data}

    assert {:ok, pdf} =
             SvgBackend.vectorize(
               source("<text x='5' y='25' font-family='NativeSvgFont0'>office Ω</text>"),
               [width: 200, height: 50],
               %{mode: :inline, fonts: [font]}
             )

    assert {:ok, document} = Text.extract_spans(pdf)
    text = Enum.map_join(document.pages, fn page -> Enum.map_join(page.spans, & &1.text) end)
    assert text =~ "office Ω"

    for family <- ["DejaVu Sans", "serif", "NativeSvgFont9"] do
      assert {:error, {:unsupported_html, diagnostic}} =
               SvgBackend.vectorize(
                 source("<text y='25' font-family='#{family}'>A</text>"),
                 [width: 200, height: 50],
                 %{mode: :inline, fonts: [font]}
               )

      assert diagnostic.message =~ "alias"
    end

    assert {:error, {:unsupported_html, diagnostic}} =
             SvgBackend.vectorize(
               source("<text y='25' font-family='NativeSvgFont0'>\u{10FFFF}</text>"),
               [width: 200, height: 50],
               %{mode: :inline, fonts: [font]}
             )

    assert diagnostic.message =~ "cover"
  end

  test "exact aliases select physical faces even when CSS metadata disagrees with internal font names",
       %{data: normal} do
    bold = File.read!("priv/fonts/dejavu/DejaVuSans-Bold.ttf")

    source =
      source(
        "<text x='5' y='25' font-size='20' font-weight='400' font-family='NativeSvgFont0'>Physical face Ω</text>"
      )

    context = %{mode: :inline, fonts: [%{data: bold}, %{data: normal}]}
    assert {:ok, pdf} = SvgBackend.vectorize(source, [width: 200, height: 50], context)
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    streams = Enum.map(Map.values(document.objects), &(Map.get(&1, :stream) || ""))
    assert bold in streams
    refute normal in streams
    assert {:ok, png} = SvgBackend.rasterize(source, [width: 200, height: 50], context)

    assert {:ok, ^png} =
             SvgBackend.rasterize(source, [width: 200, height: 50], %{
               mode: :inline,
               fonts: [%{data: bold}]
             })

    assert {:ok, selected} = Text.extract_spans(pdf)
    assert Enum.all?(hd(selected.pages).spans, &(&1.paints_text? and &1.font_size == 20))
  end

  test "cluster replacement retains ligature advance and diagnoses incomplete multi-glyph selection bounds",
       %{data: data} do
    source =
      source("<text x='5' y='25' font-size='20' font-family='NativeSvgFont0'>ffi ffi á</text>")

    assert {:ok, pdf} =
             SvgBackend.vectorize(source, [width: 200, height: 50], %{
               mode: :inline,
               fonts: [%{data: data}]
             })

    assert {:ok, document} = Text.extract_spans(pdf)
    clusters = Enum.filter(hd(document.pages).spans, &(&1.text == "ffi"))
    assert length(clusters) == 2
    assert [%{x: x, end_x: end_x, y: y, end_y: end_y, font_size: 20.0}] = Enum.take(clusters, 1)
    assert_in_delta x, 3.75, 0.001
    assert_in_delta y, 18.75, 0.001
    assert_in_delta end_y, y, 0.001
    assert end_x > x + 10 and end_x < x + 20
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "ffi ffi á"

    assert {:ok, composed_pdf} =
             SvgBackend.vectorize(
               source("<text x='5' y='25' font-size='20' font-family='NativeSvgFont0'>á̈</text>"),
               [width: 200, height: 50],
               %{mode: :inline, fonts: [%{data: data}]}
             )

    assert {:ok, composed} = Text.extract_spans(composed_pdf)
    assert [%{text: "á̈", x: 3.75, end_x: cluster_end, font_size: 20.0}] = hd(composed.pages).spans
    assert {:ok, parsed} = FontValidator.prepare_ttf(data)

    advance =
      Enum.at(parsed.widths, Map.fetch!(parsed.cmap, ?a)) * 20 / parsed.units_per_em * 0.75

    assert_in_delta cluster_end, 3.75 + advance, 0.001
    assert {:ok, raw} = NativeElixirPdfUtilities.Pdf.Reader.read(composed_pdf)

    content =
      raw.objects
      |> Map.values()
      |> Enum.map(&(Map.get(&1, :stream) || ""))
      |> Enum.find(&String.contains?(&1, "/ActualText"))

    assert length(Regex.scan(~r/ Tj/, content)) >= 2
    assert length(Regex.scan(~r/BDC/, content)) == 1
    assert {:ok, "á̈"} = Text.extract(composed_pdf)

    mono = File.read!("priv/fonts/dejavu/DejaVuSansMono.ttf")

    assert {:error, {:invalid_document, diagnostic}} =
             SvgBackend.vectorize(
               source("<text y='25' font-family='NativeSvgFont0'>á̈</text>"),
               [width: 200, height: 50],
               %{mode: :inline, fonts: [%{data: mono}]}
             )

    assert diagnostic.message =~ "multi-glyph cluster"
    assert diagnostic.message =~ "selection bounds"
  end

  test "unsupported text paints and instance font changes remain explicit diagnostics", %{
    data: data
  } do
    for content <- [
          "<text y='25' font-family='NativeSvgFont0' stroke='red'>A</text>",
          "<defs><linearGradient id='r'><stop offset='0' /><stop offset='1' /></linearGradient></defs><text y='25' font-family='NativeSvgFont0' fill='url(#r)'>A</text>"
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               SvgBackend.vectorize(source(content), [width: 200, height: 50], %{
                 mode: :inline,
                 fonts: [%{data: data}]
               })

      assert diagnostic.message =~ "solid RGB fill"
    end

    html =
      "<svg width=200 height=50 font-family='DejaVu Sans'><defs><g id='label'><text y=25>A</text></g></defs><use href='#label' font-weight=700 /></svg>"

    assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html)
    assert diagnostic.message =~ "same selected face"
  end

  test "affine text and viewBox retain actual selected glyph geometry", %{data: data} do
    source =
      "<svg width='100' height='50' viewBox='0 0 200 100'><text x='5' y='25' font-size='20' transform='translate(20 10)' font-family='NativeSvgFont0'>Affine Ω</text></svg>"

    assert {:ok, pdf} =
             SvgBackend.vectorize(source, [width: 100, height: 50], %{
               mode: :inline,
               fonts: [%{data: data}]
             })

    assert {:ok, "Affine Ω"} = Text.extract(pdf)
    assert {:ok, text} = Text.extract_spans(pdf)
    first = hd(hd(text.pages).spans)
    assert first.font_size == 20 and first.paints_text?
    assert_in_delta first.x, 9.375, 0.001
    assert_in_delta first.y, 13.125, 0.001
  end

  test "group clip opacity and fill alpha preserve painting text and shared surface accounting",
       %{data: data, limits: limits} do
    source =
      source(
        "<defs><clipPath id='c'><rect x='0' y='0' width='60' height='40' /></clipPath></defs><g clip-path='url(#c)' opacity='.5'><text x='5' y='25' font-family='NativeSvgFont0' fill-opacity='.4'>Clipped Ω</text></g>"
      )

    context = %{mode: :inline, fonts: [%{data: data}]}
    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 1))
    assert {:ok, pdf} = SvgBackend.vectorize(source, [width: 200, height: 50], context)
    assert {:ok, "Clipped Ω"} = Text.extract(pdf)
    assert {:ok, doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    content = doc.objects |> Map.values() |> Enum.map_join(&(Map.get(&1, :stream) || ""))
    assert content =~ "W"
    assert pdf =~ "/ca 0.4" and pdf =~ "/ca 0.5"
    refute content =~ "3 Tr"

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgBackend.rasterize(source, [width: 200, height: 50], context)

    assert diagnostic.message =~ "max_aggregate_svg_surface_pixels"
    Limits.install(limits)
  end

  test "spaces remain logical text and hidden-only roots do not issue detached font painting" do
    html =
      "<svg width=200 height=50 font-family='DejaVu Sans'><text y=25>  One   <tspan> Two </tspan>  Three  </text></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, "One Two Three"} = Text.extract(pdf)

    hidden =
      "<svg width=200 height=50 font-family='DejaVu Sans'><text y=25 visibility=hidden>Hidden Ω</text></svg>"

    assert {:ok, hidden_pdf} = HtmlToPdf.render(hidden)
    refute hidden_pdf =~ "/FontFile2"
    assert {:ok, %{pages: [%{spans: []}]}} = Text.extract_spans(hidden_pdf)
  end

  test "embedding restrictions use the shared font policy before native output", %{data: data} do
    # Locate OS/2 fsType within the original valid font without rebuilding tables.
    <<_::binary-size(4), count::16, _::binary-size(6), rest::binary>> = data

    table =
      for <<tag::binary-size(4), _checksum::32, offset::32,
            _length::32 <- binary_part(rest, 0, count * 16)>>, into: %{}, do: {tag, offset}

    offset = Map.fetch!(table, "OS/2") + 8

    for flags <- [0x0002, 0x0200] do
      denied =
        binary_part(data, 0, offset) <>
          <<flags::16>> <> binary_part(data, offset + 2, byte_size(data) - offset - 2)

      assert {:error, {:invalid_document, diagnostic}} =
               SvgBackend.vectorize(
                 source("<text y='25' font-family='NativeSvgFont0'>A</text>"),
                 [width: 200, height: 50],
                 %{mode: :inline, fonts: [%{data: denied}]}
               )

      assert diagnostic.stage == :font
      assert diagnostic.message =~ "does not permit outline embedding"
    end

    no_subset =
      binary_part(data, 0, offset) <>
        <<0x0100::16>> <> binary_part(data, offset + 2, byte_size(data) - offset - 2)

    assert {:ok, pdf} =
             SvgBackend.vectorize(
               source("<text y='25' font-family='NativeSvgFont0'>Original bytes</text>"),
               [width: 200, height: 50],
               %{mode: :inline, fonts: [%{data: no_subset}]}
             )

    assert {:ok, doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    assert Enum.any?(Map.values(doc.objects), &(Map.get(&1, :stream) == no_subset))
  end

  test "selected fonts require validated TrueType outlines rather than metrics-only text omission",
       %{data: data} do
    <<_::binary-size(4), count::16, _::binary-size(6), records::binary>> = data

    index =
      Enum.find(0..(count - 1), fn index -> binary_part(records, index * 16, 4) == "glyf" end)

    offset = 12 + index * 16

    missing =
      binary_part(data, 0, offset) <>
        "none" <> binary_part(data, offset + 4, byte_size(data) - offset - 4)

    assert {:ok, _metadata} = FontValidator.prepare_ttf(missing)

    assert {:error, {reason, diagnostic}} =
             SvgBackend.vectorize(
               source("<text y='25' font-family='NativeSvgFont0'>Cannot omit</text>"),
               [width: 200, height: 50],
               %{mode: :inline, fonts: [%{data: missing}]}
             )

    assert reason == :invalid_document
    assert diagnostic.message != ""
    assert diagnostic.stage in [:font, :writer]
  end

  test "expanded glyph outlines and authorized font byte work keep independent ceilings", %{
    data: data,
    limits: limits
  } do
    source =
      source(
        "<defs><g id='label'><text y='25' font-family='NativeSvgFont0'>Bounded office</text></g></defs><use href='#label' /><use href='#label' x='80' />"
      )

    context = %{mode: :inline, fonts: [%{data: data}]}
    assert {:ok, _} = SvgBackend.vectorize(source, [width: 200, height: 50], context)

    for {key, ceiling} <- [
          max_svg_font_work_bytes: 2 * byte_size(data) - 1,
          max_svg_stroke_work: 10,
          max_svg_expansion_work: 15
        ] do
      Limits.install(Map.put(limits, key, ceiling))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               SvgBackend.vectorize(source, [width: 200, height: 50], context)

      assert diagnostic.message =~ Atom.to_string(key)
    end

    Limits.install(limits)
    assert {:ok, _} = SvgBackend.vectorize(source, [width: 200, height: 50], context)
  end

  test "unsupported text grammar and shaping diagnose instead of omitting artwork", %{data: data} do
    for attrs <- [
          "x='1 bad'",
          "rotate='20deg'",
          "textLength='-30'",
          "direction='rtl'",
          "writing-mode='vertical-rl'",
          "xml:space='preserve'",
          "font-size='bad'",
          "font-size='-4'",
          "font-weight='NaN'",
          "font-style='nonsense'",
          "letter-spacing='broken'",
          "text-anchor='wrong'",
          "fill-opacity='bad'"
        ] do
      assert {:error, {_reason, diagnostic}} =
               SvgBackend.vectorize(
                 source("<text y='25' font-family='NativeSvgFont0' #{attrs}>A</text>"),
                 [width: 200, height: 50],
                 %{mode: :inline, fonts: [%{data: data}]}
               )

      assert diagnostic.stage ==
               if(String.starts_with?(attrs, ["font-", "letter-spacing"]),
                 do: :css,
                 else: :layout
               )

      assert diagnostic.message != ""
    end

    assert {:error, {:invalid_document, diagnostic}} =
             SvgBackend.vectorize(
               source("<text y='25' font-family='NativeSvgFont0'>אבג</text>"),
               [width: 200, height: 50],
               %{mode: :inline, fonts: [%{data: data}]}
             )

    assert diagnostic.message =~ "Unicode order"
  end

  defp source(content),
    do: "<svg xmlns='http://www.w3.org/2000/svg' width='200' height='50'>#{content}</svg>"
end
