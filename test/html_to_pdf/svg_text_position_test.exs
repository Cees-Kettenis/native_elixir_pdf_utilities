defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextPositionTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, SvgScene, SvgBackend, Layout}

  alias NativeElixirPdfUtilities.Validators.{
    SvgValidator,
    SvgTextPositionValidator,
    HtmlValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits, data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf")}
  end

  test "raw absolute lists paint selected Unicode glyphs at each authored coordinate", %{
    data: data
  } do
    source =
      "<svg width='200' height='100'><text x='5 35,65' y='25,35 25' font-size='20' font-family='NativeSvgFont0'>AΩé</text></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})
    assert {:ok, "AΩé"} = Text.extract(pdf, layout: false)
    assert {:ok, %{pages: [page]}} = Text.extract_spans(pdf)
    assert Enum.map(page.spans, & &1.text) == ["A", "Ω", "é"]

    for {span, {x, y}} <- Enum.zip(page.spans, [{3.75, 18.75}, {26.25, 26.25}, {48.75, 18.75}]) do
      assert span.paints_text? and span.font_size == 20
      assert_in_delta span.x, x, 0.001
      assert_in_delta span.y, y, 0.001
      assert span.end_x > span.x
    end

    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    assert Enum.any?(Map.values(document.objects), &(Map.get(&1, :stream) == data))
    refute pdf =~ "3 Tr"
    refute pdf =~ "/Subtype /Image"
  end

  test "the former scalar-only rejection now paints its original one-character source and budgets unused entries",
       %{data: data} do
    source =
      "<svg xmlns='http://www.w3.org/2000/svg' width='200' height='50'><text y='25' font-family='NativeSvgFont0' x='1 2'>A</text></svg>"

    assert {:ok, pdf} =
             SvgBackend.vectorize(source, [width: 200, height: 50], %{
               mode: :inline,
               fonts: [%{data: data}]
             })

    assert {:ok, "A"} = Text.extract(pdf)
    assert {:ok, %{pages: [%{spans: [span]}]}} = Text.extract_spans(pdf)
    assert span.paints_text?
    assert_in_delta span.x, 0.75, 0.001
    assert {:ok, summary} = SvgValidator.validate_processing(source, %{mode: :inline})
    assert summary.expanded_position_work == 3
  end

  test "percentage coordinates use their actual horizontal and vertical viewport axes", %{
    data: data
  } do
    source =
      "<svg width='200' height='100' viewBox='0 0 200 100'><text x='10% 20%' y='20% 40%' font-family='NativeSvgFont0'>AB</text></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})
    assert {:ok, "AB"} = Text.extract(pdf, layout: false)
    assert {:ok, %{pages: [page]}} = Text.extract_spans(pdf)

    for {span, {x, y}} <- Enum.zip(page.spans, [{15.0, 15.0}, {30.0, 30.0}]) do
      assert_in_delta span.x, x, 0.001
      assert_in_delta span.y, y, 0.001
    end
  end

  test "honest host preparation binds identical attributes to their distinct inherited font contexts" do
    html =
      "<svg width=200 height=100><g font-size=20><text x='1em 2em' y='1rem'>AB</text></g><g font-size=30><text x='1em 2em' y='1rem'>CD</text></g></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.prepare_svg_detailed(dom)
    assert {:ok, scene} = SvgScene.prepare(svg, HtmlValidator.new_image_budget())
    assert scene.source =~ "x=\"20.0 40.0\""
    assert scene.source =~ "x=\"30.0 60.0\""
    assert length(Regex.scan(~r/y="16.0"/, scene.source)) == 2
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "AB" and text =~ "CD"
  end

  test "reused em lists retain each actual instance size and original Unicode order" do
    html =
      "<svg width=300 height=100 font-size=20><defs><g id=label><text x='1em 2em 3em' y='1.3em'>Run</text></g></defs><use href='#label' x='4' font-size='18' /><use href='#label' x='150' y='35' font-size='24' /></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    assert svg.style.svg_scene.source =~ "x=\"1.0em 2.0em 3.0em\""
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert length(Regex.scan(~r/Run/, text)) == 2
    assert {:ok, %{pages: [page]}} = Text.extract_spans(pdf)
    assert Enum.map(page.spans, & &1.font_size) == [18, 18, 18, 24, 24, 24]
    [first, _, _, second | _] = page.spans
    assert_in_delta first.x, 16.5, 0.001
    assert_in_delta second.x, 130.5, 0.001
  end

  test "absolute character boundaries and cluster-first relative offsets keep genuine glyph selection",
       %{data: data} do
    for {positions, expected} <- [
          {"x='5 35 65 95'", ["a", "́", "̈", "B"]},
          {"x='5' dx='0 0 0 30'", ["á̈", "B"]}
        ] do
      source =
        "<svg width='200' height='50'><text #{positions} y='25' font-size='20' font-family='NativeSvgFont0'>á̈B</text></svg>"

      assert {:ok, pdf} =
               SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})

      assert {:ok, "á̈B"} = Text.extract(pdf, layout: false)
      assert {:ok, %{pages: [page]}} = Text.extract_spans(pdf)
      assert Enum.map(page.spans, & &1.text) == expected
      assert Enum.all?(page.spans, &(&1.paints_text? and &1.font_size == 20))

      if length(expected) == 4 do
        for {span, x} <- Enum.zip(page.spans, [3.75, 26.25, 48.75, 71.25]),
            do: assert_in_delta(span.x, x, 0.001)
      else
        [cluster, next] = page.spans
        assert cluster.end_x > cluster.x
        assert_in_delta next.x, cluster.end_x + 22.5, 0.001
      end
    end
  end

  test "nonzero continuation offsets inside original shaping clusters diagnose in both inline paint modes",
       %{data: data} do
    for positions <- ["dx='0 10 20 30'", "dy='0 5 0 0'"],
        backend <- [&SvgBackend.vectorize/3, &SvgBackend.rasterize/3] do
      source =
        "<svg width='200' height='50'><text x='5' #{positions} y='25' font-size='20' font-family='NativeSvgFont0'>á̈B</text></svg>"

      assert {:error, {:invalid_document, diagnostic}} =
               backend.(source, [], %{mode: :inline, fonts: [%{data: data}]})

      assert diagnostic.reason == :invalid_document and diagnostic.stage == :layout
      assert diagnostic.operation == :prepare and diagnostic.source == "SVG"
      assert diagnostic.message =~ "nonzero relative offsets inside a shaped Unicode cluster"
    end
  end

  test "malformed list and raw unsupported units return actionable original-attribute diagnostics",
       %{data: data} do
    for value <- ["", "1,,2", ",1", "1,", "1 bad", "NaN", "1e999"] do
      source =
        "<svg width='200' height='100'><text x='#{value}' y='25' font-family='NativeSvgFont0'>AB</text></svg>"

      assert {:error, {:invalid_document, diagnostic}} =
               SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})

      assert diagnostic.reason == :invalid_document and diagnostic.stage == :layout
      assert diagnostic.source == "text x" and diagnostic.operation == :prepare
      assert diagnostic.module == SvgTextPositionValidator
      assert diagnostic.message =~ "SVG text x"
    end

    for value <- ["1rem 2rem", "1q 2q"] do
      source =
        "<svg width='200' height='100'><text x='#{value}' y='25' font-family='NativeSvgFont0'>AB</text></svg>"

      assert {:error, {:unsupported_html, diagnostic}} =
               SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})

      assert diagnostic.module == SvgTextPositionValidator and diagnostic.source == "text x"
      assert diagnostic.message =~ "validated font/unit preparation"
    end
  end

  test "position preparation preserves vector-only surface admission and rejects actual raster requests",
       %{data: data, limits: limits} do
    source =
      "<svg width='200' height='100'><text x='5 35 65' y='25' font-family='NativeSvgFont0'>AΩé</text></svg>"

    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 1))
    assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})
    assert {:ok, "AΩé"} = Text.extract(pdf, layout: false)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgBackend.rasterize(source, [], %{mode: :inline, fonts: [%{data: data}]})

    assert diagnostic.message =~ "max_aggregate_svg_surface_pixels"

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<svg width=200 height=100><text x='5 35 65' y=25>AΩé</text></svg>")

    assert diagnostic.message =~ "max_aggregate_svg_surface_pixels"
  end

  test "unused trailing lists and reused targets charge shared expansion work without raster cost",
       %{limits: limits} do
    source =
      "<svg width='200' height='100'><defs><g id='label'><text x='1 2 3 4 5' y='25'>A</text></g></defs><use href='#label'/><use href='#label'/></svg>"

    assert {:ok, summary} = SvgValidator.validate_processing(source, %{mode: :inline})
    assert summary.expanded_position_work == 18
    assert summary.opacity_layers == 0 and summary.clip_references == 0
    total = summary.expanded_position_work + summary.expanded_work
    Limits.install(Map.put(limits, :max_svg_expansion_work, total - 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgValidator.validate_processing(source, %{mode: :inline})

    assert diagnostic.stage == :limits and diagnostic.message =~ "max_svg_expansion_work"
    Limits.install(Map.put(limits, :max_svg_expansion_work, total))
    assert {:ok, boundary} = SvgValidator.validate_processing(source, %{mode: :inline})
    assert boundary.expanded_work == summary.expanded_work
    assert boundary.expanded_position_work == 18
    Limits.install(limits)
    assert {:ok, image_summary} = SvgValidator.validate_processing(source, %{mode: :image})
    assert image_summary.expanded_work == summary.expanded_work
    assert image_summary.expanded_position_work == 18
  end

  test "prepared context and plan presence cannot replace original source or budget validation",
       %{limits: limits} do
    assert {:error, {:invalid_document, diagnostic}} = SvgTextPositionValidator.prepare(%{})
    assert diagnostic.message =~ "original attributes and a tag"
    assert diagnostic.stage == :layout and diagnostic.reason == :invalid_document

    assert {:error, {:invalid_document, diagnostic}} =
             SvgTextPositionValidator.prepare(%{tag: "text", attributes: %{"x" => nil}})

    assert diagnostic.message =~ "original length-list strings"

    for style <- [
          %{},
          %{font_size: "20", _root_font_size: 12},
          %{font_size: 12, _root_font_size: -1}
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               SvgTextPositionValidator.prepare(%{
                 tag: "text",
                 attributes: %{"x" => "1rem 2rem"},
                 style: style
               })

      assert diagnostic.module == SvgTextPositionValidator and diagnostic.source == "text"
      assert diagnostic.message =~ "inherited font/root"
    end

    node = %{tag: "text", attributes: %{"x" => "1 2 3"}}
    Limits.install(Map.put(limits, :max_svg_expansion_work, 5))

    assert :ok ==
             HtmlValidator.with_render_budget(fn ->
               assert {:ok, _plan} = SvgTextPositionValidator.prepare(node)

               assert {:error, {:resource_limit_exceeded, diagnostic}} =
                        SvgTextPositionValidator.prepare(node)

               assert diagnostic.message =~ "max_svg_expansion_work"
               assert diagnostic.source == "SVG positions" and diagnostic.operation == :prepare
               :ok
             end)
  end

  test "qualified coordinate lookalikes stay passive through original host and raw source routes",
       %{data: data} do
    html =
      "<svg width=200 height=50 xmlns:q='urn:passive'><text q:x='not a length' y=25>Passive Ω</text></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, "Passive Ω"} = Text.extract(pdf)

    source =
      "<svg width='200' height='50' xmlns:q='urn:passive'><text q:x='not a length' y='25' font-family='NativeSvgFont0'>Passive Ω</text></svg>"

    assert {:ok, summary} = SvgValidator.validate_processing(source, %{mode: :inline})
    assert summary.expanded_position_work == 1
    assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})
    assert {:ok, "Passive Ω"} = Text.extract(pdf)
  end

  test "relative mapping follows normalized Unicode scalars and tspan reset scopes", %{data: data} do
    for content <- [
          "<text x='5' y='25' dx='0 0 10 20 30' font-family='NativeSvgFont0'>Ωá̈B</text>",
          "<text x='5' y='25' font-family='NativeSvgFont0'>Ω<tspan dx='0 10 20 30' font-family='NativeSvgFont0'>á̈B</tspan></text>",
          "<text x='5' y='25' font-family='NativeSvgFont0'> Ω <tspan dx='0 0 10 20 30' font-family='NativeSvgFont0'> á̈B </tspan></text>"
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               SvgBackend.vectorize("<svg width='200' height='50'>#{content}</svg>", [], %{
                 mode: :inline,
                 fonts: [%{data: data}]
               })

      assert diagnostic.message =~ "relative"
      assert diagnostic.message =~ "Unicode" or diagnostic.message =~ "codepoint"
    end

    source =
      "<svg width='200' height='50'><text x='5' y='25' dx='0 0 0 0 30' font-family='NativeSvgFont0'>Ωá̈B</text></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})
    assert {:ok, "Ωá̈B"} = Text.extract(pdf, layout: false)
    assert {:ok, %{pages: [page]}} = Text.extract_spans(pdf)
    assert Enum.map(page.spans, & &1.text) == ["Ω", "á̈", "B"]
    assert Enum.all?(page.spans, & &1.paints_text?)
  end

  test "token work fails before allocating a caller-sized plan and independent preparation recovers",
       %{limits: limits} do
    node = %{tag: "text", attributes: %{"x" => String.duplicate("1 ", 10_000)}}
    Limits.install(Map.put(limits, :max_svg_expansion_work, 4))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgTextPositionValidator.prepare(node)

    assert diagnostic.reason == :resource_limit_exceeded and diagnostic.stage == :limits
    assert diagnostic.source == "text x" and diagnostic.message =~ "max_svg_expansion_work"
    Limits.install(limits)

    assert {:ok, %{lengths: %{"x" => [{:user, 1.0}, {:user, 2.0}]}, work: 2}} =
             SvgTextPositionValidator.prepare(%{node | attributes: %{"x" => "1 2"}})
  end

  test "supplied prepared and cached trees cannot authorize changed malformed original lists" do
    html = "<svg width=200 height=100><text x='5 25' y=25>AB</text></svg>"
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, prepared} = Style.prepare_svg_detailed(dom)
    [svg] = prepared.children
    [text] = svg.children
    invalid = %{text | attributes: Map.put(text.attributes, "x", "1,,2")}

    assert {:error, {:invalid_document, diagnostic}} =
             SvgScene.prepare(%{svg | children: [invalid]}, HtmlValidator.new_image_budget())

    assert diagnostic.module == SvgTextPositionValidator and diagnostic.source == "text x"
    assert {:ok, cached} = Style.compute_detailed(dom)
    [svg] = cached.children

    scene = %{
      svg.style.svg_scene
      | source: String.replace(svg.style.svg_scene.source, "x=\"5.0 25.0\"", "x=\"1,,2\"")
    }

    svg = %{svg | style: %{svg.style | svg_scene: scene}}

    assert {:error, {:invalid_document, diagnostic}} =
             Layout.layout(%{cached | children: [svg]}, page_size: {200, 100}, margin: 0)

    assert diagnostic.source == "text x" and diagnostic.module == SvgTextPositionValidator
  end
end
