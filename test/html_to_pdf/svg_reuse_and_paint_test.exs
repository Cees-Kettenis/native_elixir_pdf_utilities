Code.require_file("../support/svg_converted_image.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgReuseAndPaintTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.SvgConvertedImage
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "referenced descendants retain local CSS while each use supplies inherited paints and currentColor" do
    html =
      "<style>.fixed{fill:blue} #explicit{fill:inherit}</style>\n<svg width=100 height=40><defs fill=yellow><g id=tile><rect width=10 height=10 /><circle class=fixed r=3 /><path id=explicit d='M0 0H5V5Z' /><path d='M0 20H10' stroke=currentColor /></g></defs>\n<use href='#tile' fill=red color=green /><use href='#tile' x=20 fill=green color=red /></svg>\n"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    source = svg.style.svg_scene.source
    assert source =~ "fill=\"inherit\""
    assert source =~ "stroke=\"currentColor\""
    assert source =~ "rgb(0,0,255)"
    assert {:ok, pdf} = HtmlToPdf.render(html)
    image = SvgConvertedImage.render(html)

    pixels =
      for <<r, g, b <- image.data>> do
        {r, g, b}
      end

    assert {255, 0, 0} in pixels
    assert {0, 128, 0} in pixels
    assert pdf =~ "/Subtype /Image"
  end

  test "referenced em geometry resolves each instance font size" do
    html =
      "<svg width=100 height=40><defs><rect id=tile width='1em' height='1em' /></defs><use href='#tile' style='font-size:10px' /><use href='#tile' x=20 style='font-size:20px' /></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    assert svg.style.svg_scene.source =~ "width=\"1.0em\""
    assert SvgConvertedImage.coverage(SvgConvertedImage.render(html)) > 100
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

  test "new original reuse and gradient fixtures preserve native artwork and surrounding text" do
    for name <- ["inline_svg_reuse.html", "inline_svg_gradients.html"] do
      html = File.read!(Path.expand("../fixtures/html_to_pdf/browser_parity/" <> name, __DIR__))
      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "Ω" and text =~ "café" and text =~ "artwork survived"
      assert pdf =~ "/Subtype /Image"
    end
  end

  defp stops do
    "<stop offset='0' stop-color='red' /><stop offset='1' stop-color='blue' />"
  end
end
