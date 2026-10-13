defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextPathTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgBackend
  alias NativeElixirPdfUtilities.Validators.SvgValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test "curved path text paints exact selected glyphs and original Unicode", %{data: data} do
    source =
      "<svg width='400' height='120'><defs><path id='curve' d='M20 85 Q200 -30 380 85'/></defs><text font-family='NativeSvgFont0' font-size='20'><textPath href='#curve' font-family='NativeSvgFont0' startOffset='10%'>Path Ωé ABC</textPath></text></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})
    assert {:ok, "Path Ωé ABC"} = Text.extract(pdf, layout: false)
    assert {:ok, %{pages: [%{spans: spans}]}} = Text.extract_spans(pdf)
    assert Enum.all?(spans, &(&1.paints_text? and &1.font_size == 20))
    assert Enum.any?(spans, &(abs(&1.y - hd(spans).y) > 1))
    refute pdf =~ "3 Tr"
    refute pdf =~ "/Subtype /Image"
    assert {:ok, doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    assert Enum.any?(Map.values(doc.objects), &(Map.get(&1, :stream) == data))
    assert {:ok, png} = SvgBackend.rasterize(source, [], %{mode: :inline, fonts: [%{data: data}]})
    assert png =~ <<137, 80, 78, 71>>
  end

  test "path-axis percentages and middle anchor retain geometry within native arclength precision",
       %{
         data: data
       } do
    source =
      "<svg width='300' height='100' xmlns:xlink='http://www.w3.org/1999/xlink'><defs><line id='axis' x1='20' y1='50' x2='220' y2='50'/></defs><text font-family='NativeSvgFont0' font-size='20' text-anchor='middle'><textPath xlink:href='#axis' startOffset='50%' font-family='NativeSvgFont0'>AB</textPath></text></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})
    assert {:ok, "AB"} = Text.extract(pdf, layout: false)
    assert {:ok, %{pages: [%{spans: [first, last]}]}} = Text.extract_spans(pdf)
    # usvg collect_normals uses a fixed 0.5px inverse-arclength accuracy
    # at unit scale. This geometry bound follows that numeric algorithm.
    assert_in_delta (first.x + last.end_x) / 2, 90, 0.5 * 0.75
    assert_in_delta first.y, 37.5, 0.001
    assert_in_delta first.end_y, first.y, 0.001
    assert first.font_size == 20
  end

  test "explicit default path modes and ignored target ancestor transforms preserve source coordinates",
       %{data: data} do
    source =
      "<svg width='240' height='180'><defs><g transform='translate(60 30)'><path id='axis' d='M0 50 H220' transform='translate(5)'/></g></defs><g transform='translate(10 20)'><text font-family='NativeSvgFont0' font-size='20'><textPath href='#axis' font-family='NativeSvgFont0' method='align' spacing='exact' side='left'>AB Ω</textPath></text></g></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})
    assert {:ok, "AB Ω"} = Text.extract(pdf, layout: false)
    assert {:ok, %{pages: [%{spans: [first | _]}]}} = Text.extract_spans(pdf)
    assert_in_delta first.x, 11.25, 0.5 * 0.75
    assert_in_delta first.y, 52.5, 0.001

    assert {:ok, _png} =
             SvgBackend.rasterize(source, [], %{mode: :inline, fonts: [%{data: data}]})
  end

  test "host path/tspan typography and direct text reuse preserve logical source" do
    html =
      "<svg width=400 height=150 font-size=20><defs><path id='line' d='M20 45 H380'/><text id='label' y=100>Reuse Ωé</text></defs><text><textPath href='#line' startOffset='1em'>Path <tspan fill='#a03333'>Ωé</tspan></textPath></text><use href='#label' x='20'/></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "Path Ωé"
    assert text =~ "Reuse Ωé"
  end

  test "path-reference and unfinished path-layout semantics fail before both painters", %{
    data: data
  } do
    for {body, reason, message} <- [
          {"<text><textPath href='#missing'>AB</textPath></text>", :invalid_document, "target"},
          {"<text><textPath href='remote.svg#p'>AB</textPath></text>", :invalid_document,
           "not authorized"},
          {"<textPath href='#p'>AB</textPath>", :invalid_document, "direct text parent"},
          {"<text><textPath href='#p' method='stretch'>AB</textPath></text>", :unsupported_html,
           "path-layout"},
          {"<text startOffset='1'><textPath href='#p'>AB</textPath></text>", :invalid_document,
           "belongs"},
          {"<text><textPath href='#p' startOffset='1 2'>AB</textPath></text>", :invalid_document,
           "exactly one"}
        ] do
      source =
        "<svg width='200' height='100'><defs><path id='p' d='M0 50 H200'/></defs><g font-family='NativeSvgFont0'>#{body}</g></svg>"

      for operation <- [:vectorize, :rasterize] do
        assert {:error, {^reason, d}} =
                 apply(SvgBackend, operation, [
                   source,
                   [],
                   %{mode: :inline, fonts: [%{data: data}]}
                 ])

        assert d.message =~ message
        assert d.stage in [:layout, :style]
        assert is_binary(d.source)
      end
    end
  end

  test "uncalibrated path lengths and dropped path glyphs remain diagnosed", %{data: data} do
    for source <- [
          "<svg width='200' height='100'><defs><path id='p' d='M0 50 H200' pathLength='100'/></defs><text font-family='NativeSvgFont0'><textPath href='#p'>AB</textPath></text></svg>",
          "<svg width='200' height='100'><path id='p' d='M0 50 H1'/><text font-family='NativeSvgFont0'><textPath href='#p'>ABCD</textPath></text></svg>"
        ] do
      for operation <- [:vectorize, :rasterize] do
        assert {:error, {_reason, d}} =
                 apply(SvgBackend, operation, [
                   source,
                   [],
                   %{mode: :inline, fonts: [%{data: data}]}
                 ])

        assert d.stage == :layout

        assert d.message =~ "pathLength" or d.message =~ "logical" or
                 d.message =~ "Unicode"
      end
    end
  end

  test "path offset and expanded reuse charge existing work without vector raster allocation", %{
    data: data,
    limits: limits
  } do
    source =
      "<svg width='200' height='80'><defs><path id='p' d='M0 40 H200'/><text id='t' font-family='NativeSvgFont0'><textPath href='#p' font-family='NativeSvgFont0' startOffset='5'>AB</textPath></text></defs><use href='#t'/><use href='#t' y='20'/></svg>"

    assert {:ok, summary} = SvgValidator.validate_processing(source, %{mode: :inline})
    assert summary.expanded_position_work == 3
    Limits.install(Map.put(limits, :max_svg_expansion_work, 0))

    assert {:error, {:resource_limit_exceeded, cost}} =
             SvgValidator.validate_processing(source, %{mode: :inline})

    assert cost.message =~ "max_svg_expansion_work"
    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 1))

    assert {:ok, _pdf} =
             SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})

    assert {:error, {:resource_limit_exceeded, d}} =
             SvgBackend.rasterize(source, [], %{mode: :inline, fonts: [%{data: data}]})

    assert d.message =~ "max_aggregate_svg_surface_pixels"
  end
end
