Code.require_file("../support/svg_converted_image.ex", __DIR__)
Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextPathTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test "original path and tspan typography paint real converter glyph pixels" do
    html =
      "<svg width=400 height=150 font-size=20><defs><path id='line' d='M20 45 H380'/><text id='label' y=100>Reuse Ωé</text></defs><text><textPath href='#line' startOffset='1em'>Path <tspan fill='#a03333'>Ωé</tspan></textPath></text><use href='#label' x='20'/></svg>"

    image = NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.render(html)
    assert NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.coverage(image) > 100
  end

  test("curved path text paints exact selected glyphs and original Unicode", %{data: data}) do
    source =
      "<svg width='400' height='120'><defs><path id='curve' d='M20 85 Q200 -30 380 85'/></defs><text font-family='NativeSvgFont0' font-size='20'><textPath href='#curve' font-family='NativeSvgFont0' startOffset='10%'>Path Ωé ABC</textPath></text></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline,
               fonts: [%{data: data}]
             })

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, _doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    assert {:ok, png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
               mode: :inline,
               fonts: [%{data: data}]
             })

    assert png =~ <<137, 80, 78, 71>>
  end

  test(
    "path-axis percentages and middle anchor retain geometry within native arclength precision",
    %{data: data}
  ) do
    source =
      "<svg width='300' height='100' xmlns:xlink='http://www.w3.org/1999/xlink'><defs><line id='axis' x1='20' y1='50' x2='220' y2='50'/></defs><text font-family='NativeSvgFont0' font-size='20' text-anchor='middle'><textPath xlink:href='#axis' startOffset='50%' font-family='NativeSvgFont0'>AB</textPath></text></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline,
               fonts: [%{data: data}]
             })

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test(
    "explicit default path modes and ignored target ancestor transforms preserve source coordinates",
    %{data: data}
  ) do
    source =
      "<svg width='240' height='180'><defs><g transform='translate(60 30)'><path id='axis' d='M0 50 H220' transform='translate(5)'/></g></defs><g transform='translate(10 20)'><text font-family='NativeSvgFont0' font-size='20'><textPath href='#axis' font-family='NativeSvgFont0' method='align' spacing='exact' side='left'>AB Ω</textPath></text></g></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline,
               fonts: [%{data: data}]
             })

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, _png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
               mode: :inline,
               fonts: [%{data: data}]
             })
  end

  test "host path/tspan typography and direct text reuse preserve logical source" do
    html =
      "<svg width=400 height=150 font-size=20><defs><path id='line' d='M20 45 H380'/><text id='label' y=100>Reuse Ωé</text></defs><text><textPath href='#line' startOffset='1em'>Path <tspan fill='#a03333'>Ωé</tspan></textPath></text><use href='#label' x='20'/></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test("calibrated numeric and font offsets use the transformed actual path length", %{data: data}) do
    for {offset, _expected} <- [{"10", 52.5}, {"1em", 82.5}, {"10%", 52.5}] do
      source =
        "<svg width='440' height='100'><defs><g transform='translate(200)'><path id='p' d='M0 50 H200' transform='translate(10) scale(2 1)' pathLength='100'/></g></defs><text font-family='NativeSvgFont0' font-size='20' transform='translate(20)'><textPath font-family='NativeSvgFont0' href='#p' startOffset='#{offset}'>AB Ωé</textPath></text></svg>"

      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
                 mode: :inline,
                 fonts: [%{data: data}]
               })

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      assert {:ok, _doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

      assert {:ok, _} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
                 mode: :inline,
                 fonts: [%{data: data}]
               })
    end
  end

  test("SVG2 inline paths return an explicit converter limitation", %{data: data}) do
    for attributes <- [
          "path='M20 50 H380'",
          "path='M20 50 H380' href='#other'",
          "path='' href='#other'"
        ] do
      source =
        "<svg width='400' height='100'><defs><path id='other' d='M20 50 H380'/></defs><text font-family='NativeSvgFont0' font-size='20'><textPath font-family='NativeSvgFont0' #{attributes} startOffset='10%'>Inline Ωé</textPath></text></svg>"

      for result <- [
            NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
              mode: :inline,
              fonts: [%{data: data}]
            }),
            NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
              mode: :inline,
              fonts: [%{data: data}]
            })
          ] do
        assert {:error, {:unsupported_html, diagnostic}} = result
        assert diagnostic.source == "textPath path"
        assert diagnostic.stage == :layout
        assert diagnostic.message =~ "referenced path with href"
      end
    end
  end

  test("zero author path length retains zero and percentage offsets", %{data: data}) do
    for offset <- ["0", "10%"] do
      source =
        "<svg width='200' height='100'><defs><path id='p' d='M0 50 H200' pathLength='0'/></defs><text font-family='NativeSvgFont0'><textPath font-family='NativeSvgFont0' href='#p' startOffset='#{offset}'>AB</textPath></text></svg>"

      assert {:ok, pdf} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
                 mode: :inline,
                 fonts: [%{data: data}]
               })

      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      assert {:ok, _} =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
                 mode: :inline,
                 fonts: [%{data: data}]
               })
    end

    _source =
      "<svg width='200' height='100'><defs><path id='p' d='M0 50 H200' pathLength='0'/></defs><text font-family='NativeSvgFont0'><textPath font-family='NativeSvgFont0' href='#p' startOffset='1'>AB</textPath></text></svg>"
  end
end
