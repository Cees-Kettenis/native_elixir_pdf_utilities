defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextLengthTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgBackend
  alias NativeElixirPdfUtilities.Validators.{SvgValidator, SvgBackendValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test "spacing and glyph scaling paint real selected characters across the authored length", %{
    data: data
  } do
    for mode <- ["spacing", "spacingAndGlyphs"] do
      source =
        "<svg width='240' height='80'><text x='20' y='40' font-size='20' font-family='NativeSvgFont0' textLength='120' lengthAdjust='#{mode}'>ABCD</text></svg>"

      assert {:ok, pdf} =
               SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})

      assert {:ok, "ABCD"} = Text.extract(pdf, layout: false)
      assert {:ok, %{pages: [%{spans: spans}]}} = Text.extract_spans(pdf)
      assert Enum.all?(spans, &(&1.paints_text? and &1.font_size == 20))
      assert_in_delta List.last(spans).end_x - hd(spans).x, 90, 0.001
      assert {:ok, doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
      assert Enum.any?(Map.values(doc.objects), &(Map.get(&1, :stream) == data))
      refute pdf =~ "3 Tr"
      refute pdf =~ "/Subtype /Image"
    end
  end

  test "length units and anchors retain actual host font and viewport contexts" do
    html =
      "<svg width=280 height=150 font-size=20><text x=140 y=30 text-anchor=middle textLength='6em' lengthAdjust=spacing>ABCD</text><text x=140 y=80 text-anchor=end textLength='50%' lengthAdjust=spacingAndGlyphs>ABCD</text></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert length(Regex.scan(~r/ABCD/, text)) == 2
    assert {:ok, %{pages: [%{spans: spans}]}} = Text.extract_spans(pdf)
    [first, second] = Enum.chunk_every(spans, 4)
    assert_in_delta List.last(first).end_x - hd(first).x, 90, 0.001

    assert_in_delta List.last(second).end_x - hd(second).x,
                    280 * 0.5 * 0.75,
                    0.001
  end

  test "original invalid length and adjustment declarations return validator diagnostics", %{
    data: data
  } do
    for attrs <- [
          "textLength='-1'",
          "textLength='1 2'",
          "textLength='NaN'",
          "lengthAdjust='other'"
        ] do
      source =
        "<svg width='200' height='80'><text y='40' font-family='NativeSvgFont0' #{attrs}>AB</text></svg>"

      for operation <- [:vectorize, :rasterize] do
        assert {:error, {:invalid_document, d}} =
                 apply(SvgBackend, operation, [
                   source,
                   [],
                   %{mode: :inline, fonts: [%{data: data}]}
                 ])

        assert d.stage == :layout
        assert d.source in ["text textLength", "text lengthAdjust"]
        assert is_binary(d.message) and d.message != ""
      end
    end
  end

  test "ignored tiny glyph scaling diagnoses actual geometry before either inline painter", %{
    data: data
  } do
    source =
      "<svg width='200' height='80'><text y='40' font-family='NativeSvgFont0' textLength='0.00001' lengthAdjust='spacingAndGlyphs'>AB</text></svg>"

    for operation <- [:vectorize, :rasterize] do
      assert {:error, {:invalid_document, d}} =
               apply(SvgBackend, operation, [source, [], %{mode: :inline, fonts: [%{data: data}]}])

      assert d.stage == :layout
      assert d.message =~ "requested advance is not preserved"
      assert d.module == NativeElixirPdfUtilities.Validators.SvgBackendValidator
    end
  end

  test "parent length distributes across descendants while absolute chunk distributions stay diagnosed" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<svg width=200 height=80><text y='40' textLength='120'>A<tspan>B</tspan></text></svg>"
             )

    assert {:ok, "AB"} = Text.extract(pdf, layout: false)
    assert {:ok, %{pages: [%{spans: spans}]}} = Text.extract_spans(pdf)
    assert_in_delta List.last(spans).end_x - hd(spans).x, 90, 0.001

    assert {:error, {:unsupported_html, d}} =
             HtmlToPdf.render(
               "<svg width=200 height=80><text y='40' x='0 30' textLength='120'>AB</text></svg>"
             )

    assert d.message =~ "length distribution"
    assert d.stage == :layout
  end

  test "source preparation preserves original Unicode and IDs and normalizes before both paints",
       %{data: data} do
    source =
      "<svg width='240' height='120'><g id='NativeTextLength1'/><text x='120' y='40' text-anchor='middle' textLength='50%' font-family='NativeSvgFont0'>AΩB</text></svg>"

    assert {:ok, request} =
             SvgBackendValidator.prepare_vector(
               source,
               [],
               %{mode: :inline, fonts: [%{data: data}]},
               nil
             )

    assert request.source =~ "id='NativeTextLength1'"
    assert request.source =~ "id=\"NativeTextLength2\""
    assert request.source =~ "textLength='120'"
    assert request.source =~ "x='60'"
    assert request.source =~ "text-anchor='start'"
    assert request.source =~ ">AΩB</text>"
    assert request.root["width"] == "240"
    assert request.fonts == [data]
    assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})
    assert {:ok, "AΩB"} = Text.extract(pdf, layout: false)
    assert {:ok, %{pages: [%{spans: spans}]}} = Text.extract_spans(pdf)
    assert_in_delta hd(spans).x, 45, 0.001
    assert_in_delta List.last(spans).end_x, 135, 0.001
    assert {:ok, png} = SvgBackend.rasterize(source, [], %{mode: :inline, fonts: [%{data: data}]})
    assert png =~ <<137, 80, 78, 71>>
  end

  test "normalization source and work ceilings remain shared and do not add vector raster cost",
       %{data: data, limits: limits} do
    source =
      "<svg width='240' height='120'><text x='120' y='40' text-anchor='middle' textLength='120' font-family='NativeSvgFont0'>ABC</text></svg>"

    context = %{mode: :inline, fonts: [%{data: data}]}

    for {key, value} <- [
          {:max_aggregate_svg_source_bytes, byte_size(source)},
          {:max_svg_bytes, byte_size(source)},
          {:max_svg_expansion_work, 10}
        ] do
      Limits.install(Map.put(limits, key, value))
      assert {:error, {:resource_limit_exceeded, d}} = SvgBackend.vectorize(source, [], context)
      assert d.message =~ Atom.to_string(key)
    end

    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 1))
    assert {:ok, _pdf} = SvgBackend.vectorize(source, [], context)
    assert {:error, {:resource_limit_exceeded, d}} = SvgBackend.rasterize(source, [], context)
    assert d.message =~ "max_aggregate_svg_surface_pixels"
  end

  test "independent adjusted spans use original UTF-8 ranges and scaled cursor gaps remain diagnosed",
       %{data: data} do
    source =
      "<svg width='240' height='120'><text x='20' y='40' font-family='NativeSvgFont0'>A<tspan font-family='NativeSvgFont0' textLength='120' lengthAdjust='spacing'>ΩBéD</tspan>F</text></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})
    assert {:ok, "AΩBéDF"} = Text.extract(pdf, layout: false)
    assert {:ok, %{pages: [%{spans: spans}]}} = Text.extract_spans(pdf)
    [_prefix, first, _, _, last, suffix] = spans
    assert_in_delta last.end_x - first.x, 90, 0.001
    assert_in_delta suffix.x, last.end_x, 0.001
    scaled = String.replace(source, "lengthAdjust='spacing'", "lengthAdjust='spacingAndGlyphs'")

    for operation <- [:vectorize, :rasterize] do
      assert {:error, {:invalid_document, d}} =
               apply(SvgBackend, operation, [scaled, [], %{mode: :inline, fonts: [%{data: data}]}])

      assert d.message =~ "following-cursor distribution"
    end
  end

  test "each authored and expanded length occurrence charges existing complexity", %{
    limits: limits
  } do
    source =
      "<svg width='200' height='80'><defs><g id='label'><text textLength='100'>AB</text></g></defs><use href='#label'/><use href='#label'/></svg>"

    assert {:ok, summary} = SvgValidator.validate_processing(source, %{mode: :inline})
    assert summary.expanded_position_work == 3
    Limits.install(Map.put(limits, :max_svg_expansion_work, 0))

    assert {:error, {:resource_limit_exceeded, d}} =
             SvgValidator.validate_processing(source, %{mode: :inline})

    assert d.message =~ "max_svg_expansion_work"
  end
end
