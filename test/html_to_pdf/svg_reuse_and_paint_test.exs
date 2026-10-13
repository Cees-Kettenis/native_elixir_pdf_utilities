defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgReuseAndPaintTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, SvgBackend}

  alias NativeElixirPdfUtilities.Validators.{
    SvgReferenceValidator,
    SvgPaintValidator,
    SvgValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "referenced descendants retain local CSS while each use supplies inherited paints and currentColor" do
    html = """
    <style>.fixed{fill:blue} #explicit{fill:inherit}</style>
    <svg width=100 height=40><defs fill=yellow><g id=tile><rect width=10 height=10 /><circle class=fixed r=3 /><path id=explicit d='M0 0H5V5Z' /><path d='M0 20H10' stroke=currentColor /></g></defs>
    <use href='#tile' fill=red color=green /><use href='#tile' x=20 fill=green color=red /></svg>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    source = svg.style.svg_scene.source
    assert source =~ "fill=\"inherit\""
    assert source =~ "stroke=\"currentColor\""
    assert source =~ "rgb(0,0,255)"
    assert {:ok, pdf} = HtmlToPdf.render(html)
    streams = streams(pdf)
    assert streams =~ "1 0 0 rg"
    assert streams =~ "0 0.5019608 0 rg"
    refute pdf =~ "/Subtype /Image"
  end

  test "referenced em geometry resolves each instance font size" do
    html =
      "<svg width=100 height=40><defs><rect id=tile width='1em' height='1em' /></defs><use href='#tile' style='font-size:10px' /><use href='#tile' x=20 style='font-size:20px' /></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    assert svg.style.svg_scene.source =~ "width=\"1.0em\""
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert streams(pdf) =~ "10 10 l"
    assert streams(pdf) =~ "20 20 l"
  end

  test "local symbols, nested reuse and legacy xlink references share the byte-only shape backend" do
    source =
      "<svg xmlns:xlink='http://www.w3.org/1999/xlink' width='100' height='40'><defs><symbol id='tile' viewBox='0 0 10 10'><rect width='10' height='10' /></symbol><g id='pair'><use href='#tile' width='20' height='20' /><use href='#tile' x='25' width='20' height='20' /></g></defs><use xlink:href='#pair' fill='red' /></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [width: 100, height: 40], %{mode: :inline})
    assert streams(pdf) =~ "2 0 0 2"
    assert streams(pdf) =~ "1 0 0 rg"
    assert {:ok, summary} = SvgValidator.validate_processing(source, %{mode: :inline})
    assert summary.expanded_work > summary.nodes
    assert summary.nested_viewports >= 2
  end

  test "instance validation diagnoses missing or ineligible targets and unsupported structure" do
    for content <- [
          "<use />",
          "<rect id='c' /><use href='#missing' />",
          "<defs id='c' /><use href='#c' />",
          "<defs><symbol id='c' viewBox='0 0 -1 1' /></defs>",
          "<symbol id='c' />",
          "<g id='c' /><use href='#c'><rect /></use>",
          "<g id='c' /><use href='#c' transform='bad' />",
          "<g id='c' /><use href='#c' style='opacity:.5' />"
        ] do
      assert {:error, {reason, diagnostic}} =
               SvgBackend.vectorize(svg(content), [width: 100, height: 40], %{mode: :inline})

      assert reason == :invalid_document
      assert diagnostic.message != ""
      assert diagnostic.module in [SvgReferenceValidator, SvgValidator, SvgBackend]
    end

    for content <- [
          "<g id='c'><use href='#c' /></g><use href='#c' />",
          "<g id='c' /><use href='file:///tmp/private.svg#c' />"
        ] do
      assert {:error, {:invalid_document, %{source: "SVG"}}} =
               SvgBackend.vectorize(svg(content), [width: 100, height: 40], %{mode: :inline})
    end
  end

  test "expanded use copies retain reference, path, opacity and clipping budgets", %{
    limits: limits
  } do
    source =
      svg(
        "<defs><clipPath id='c'><rect width='10' height='10' /></clipPath><g id='tile' clip-path='url(#c)' opacity='.5'><path d='M0 0H10V10H0Z' /></g></defs><use href='#tile' /><use href='#tile' x='20' />"
      )

    assert {:ok, summary} = SvgValidator.validate_processing(source, %{mode: :inline})
    assert summary.clip_references >= 3

    for {key, value} <- [
          max_svg_references: 1,
          max_svg_expansion_work: 10,
          max_svg_expanded_path_bytes: 20,
          max_svg_stroke_work: 4,
          max_aggregate_svg_surface_pixels: 100
        ] do
      Limits.install(Map.put(limits, key, value))

      assert {:error, {:resource_limit_exceeded, %{message: message}}} =
               SvgBackend.rasterize(source, [width: 100, height: 40], %{mode: :inline})

      assert message =~ Atom.to_string(key)
    end
  end

  test "linear and radial gradients produce native RGB PDF shadings with multiple stop functions" do
    for gradient <- [
          "<linearGradient id='g'><stop offset='0' stop-color='red' /><stop offset='.3' stop-color='green' /><stop offset='1' stop-color='blue' /></linearGradient>",
          "<radialGradient id='g' fx='25%' fy='30%'><stop offset='0' stop-color='red' /><stop offset='1' stop-color='blue' /></radialGradient>"
        ] do
      source = svg("<defs>#{gradient}</defs><rect width='40' height='40' fill='url(#g)' />")
      assert {:ok, pdf} = SvgBackend.vectorize(source, [width: 100, height: 40], %{mode: :inline})
      assert pdf =~ "/ShadingType"
      assert pdf =~ "/FunctionType 2"
      assert pdf =~ "/ColorSpace /DeviceRGB"
      assert streams(pdf) =~ " sh"
      refute pdf =~ "/Subtype /Image"
    end
  end

  test "gradient stop CSS and currentColor use host source identities and authored transforms" do
    html = """
    <style>stop{stop-color:currentColor;stop-opacity:.5} #last{stop-color:blue} #ramp{transform:rotate(20 .5 .5)}</style>
    <svg width=100 height=40 color=red><defs><linearGradient id=ramp gradientTransform='rotate(10 .5 .5)'><stop offset=0 /><stop id=last offset=1 /></linearGradient></defs><rect width=100 height=40 fill='url(#ramp)' /></svg>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    assert svg.style.svg_scene.source =~ "gradientTransform=\"matrix("
    assert svg.style.svg_scene.source =~ "stop-color=\"rgb(255,0,0)\""
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "/ShadingType 2"
    assert pdf =~ "/ca 0.5"

    assert {:error, {:invalid_document, %{source: "SVG", message: message}}} =
             HtmlToPdf.render(
               "<svg width=20 height=20><rect width=20 height=20 fill='url(#missing)' /></svg>"
             )

    assert message =~ "no target"
  end

  test "unfinished gradient modes and malformed semantics diagnose before native paint omission" do
    for {attrs, stops, expected} <- [
          {"spreadMethod='repeat'", stops(), :unsupported_html},
          {"spreadMethod='reflect'", stops(), :unsupported_html},
          {"color-interpolation='linearRGB'", stops(), :unsupported_html},
          {"x1='1rem'", stops(), :unsupported_html},
          {"x1='1q'", stops(), :unsupported_html},
          {"href='#other'", stops(), :unsupported_html},
          {"fr='.2'", stops(), :unsupported_html},
          {"gradientUnits='bad'", stops(), :invalid_document},
          {"gradientTransform='bad'", stops(), :invalid_document},
          {"x1='1e999'", stops(), :invalid_document},
          {"", "<stop offset='0' stop-color='red' />", :unsupported_html},
          {"", "<stop offset='0' stop-color='red' /><stop offset='0' stop-color='blue' />",
           :unsupported_html},
          {"", "<stop offset='.2' stop-color='red' /><stop offset='1' stop-color='blue' />",
           :unsupported_html},
          {"", "<stop offset='bad' stop-color='red' /><stop offset='1' stop-color='blue' />",
           :invalid_document},
          {"",
           "<stop offset='0' stop-color='red' stop-opacity='.2' /><stop offset='1' stop-color='blue' stop-opacity='.8' />",
           :unsupported_html},
          {"",
           "<stop offset='0' stop-color='rgba(255,0,0,.5)' /><stop offset='1' stop-color='blue' />",
           :invalid_document},
          {"", "<stop offset='0' style='stop-color:red' /><stop offset='1' stop-color='blue' />",
           :unsupported_html},
          {"", "<stop offset='0'><rect /></stop><stop offset='1' />", :invalid_document}
        ] do
      other = "<linearGradient id='other'>#{stops()}</linearGradient>"

      source =
        svg(
          "<defs>#{other}<linearGradient id='g' #{attrs}>#{stops}</linearGradient></defs><rect width='40' height='40' fill='url(#g)' />"
        )

      assert {:error, {^expected, diagnostic}} =
               SvgBackend.vectorize(source, [width: 100, height: 40], %{mode: :inline})

      assert diagnostic.module == SvgPaintValidator
      assert diagnostic.stage == :layout
      assert diagnostic.operation == :prepare
      assert diagnostic.message != ""
    end

    for content <- [
          "<stop />",
          "<rect id='bad' /><rect fill='url(#bad)' />",
          "<defs><linearGradient id='g'>#{stops()}</linearGradient></defs><rect style='fill:url(#g)' />"
        ] do
      assert {:error, {_reason, %{module: SvgPaintValidator}}} =
               SvgBackend.vectorize(svg(content), [width: 100, height: 40], %{mode: :inline})
    end
  end

  test "inherited and authored CSS interpolation modes diagnose unsupported linear RGB" do
    for css <- [
          "svg{color-interpolation:linearRGB}",
          "defs{color-interpolation:linearRGB}",
          "linearGradient{color-interpolation:linearRGB}"
        ] do
      html =
        "<style>#{css}</style><svg width=100 height=40><defs><linearGradient id=g>#{stops()}</linearGradient></defs><rect width=100 height=40 fill='url(#g)' /></svg>"

      assert {:error, {:unsupported_html, %{module: SvgPaintValidator, message: message}}} =
               HtmlToPdf.render(html)

      assert message =~ "sRGB"
    end
  end

  test "foreign type selectors retain SVG case while HTML selectors fold ASCII case" do
    html =
      "<style>DIV{color:red} linearGradient{color:blue} lineargradient{color:green} clipPath{clip-rule:evenodd}</style><div><svg width=100 height=40><defs><linearGradient id=g>#{stops()}</linearGradient><clipPath id=c><rect width=20 height=20 /></clipPath></defs><rect width=40 height=40 fill='url(#g)' /></svg></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [div]}} = Style.prepare_svg_detailed(dom)
    assert div.style.color == {1, 0, 0}
    [svg] = div.children
    [defs, _] = svg.children
    [gradient, clip] = defs.children
    assert gradient.style.color == {0, 0, 1}
    assert clip.style.svg_clip_rule == :evenodd
    assert {:ok, _} = HtmlToPdf.render(html)
  end

  test "symbol defaults clip viewports while explicit CSS overflow remains supported" do
    for {css, expected} <- [{"", :hidden}, {"symbol{overflow:visible}", :visible}] do
      html =
        "<style>#{css}</style><svg width=100 height=40><defs><symbol id=s viewBox='0 0 10 10'><rect width=20 height=20 /></symbol></defs><use href='#s' width=20 height=20 /></svg>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, %{children: [svg]}} = Style.prepare_svg_detailed(dom)
      [defs, _] = svg.children
      [symbol] = defs.children
      assert symbol.style.overflow == expected
      assert {:ok, _} = HtmlToPdf.render(html)
    end
  end

  test "gradient stop ordering must survive the fixed native binary32 transport" do
    source =
      svg(
        "<defs><linearGradient id='g'><stop offset='0' stop-color='red' /><stop offset='.50000001' stop-color='green' /><stop offset='.50000002' stop-color='blue' /><stop offset='1' stop-color='white' /></linearGradient></defs><rect width='40' height='40' fill='url(#g)' />"
      )

    assert {:error, {:unsupported_html, %{module: SvgPaintValidator, message: message}}} =
             SvgBackend.vectorize(source, [width: 100, height: 40], %{mode: :inline})

    assert message =~ "native binary32"
  end

  test "gradient function work is bounded independently of path work and low raster surface budgets",
       %{limits: limits} do
    source =
      svg(
        "<defs><linearGradient id='g'>#{stops()}</linearGradient><rect id='tile' width='10' height='10' fill='url(#g)' /></defs><use href='#tile' /><use href='#tile' x='20' />"
      )

    Limits.install(Map.put(limits, :max_svg_gradient_work, 3))

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             SvgBackend.vectorize(source, [width: 100, height: 40], %{mode: :inline})

    assert message =~ "max_svg_gradient_work"
    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 1))
    assert {:ok, _} = SvgBackend.vectorize(source, [width: 100, height: 40], %{mode: :inline})

    assert {:error, {:resource_limit_exceeded, _}} =
             SvgBackend.rasterize(source, [width: 100, height: 40], %{mode: :inline})
  end

  test "new original reuse and gradient fixtures preserve native artwork and surrounding text" do
    for name <- ["inline_svg_reuse.html", "inline_svg_gradients.html"] do
      html = File.read!(Path.expand("../fixtures/html_to_pdf/browser_parity/" <> name, __DIR__))
      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "Ω" and text =~ "café" and text =~ "artwork survived"
      refute pdf =~ "/Subtype /Image"
    end
  end

  defp svg(content), do: "<svg width='100' height='40'>#{content}</svg>"
  defp stops, do: "<stop offset='0' stop-color='red' /><stop offset='1' stop-color='blue' />"

  defp streams(pdf) do
    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    context.objects |> Map.values() |> Enum.map_join("\n", &(Map.get(&1, :stream) || ""))
  end
end
