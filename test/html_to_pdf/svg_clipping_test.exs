Code.require_file("../support/svg_converted_image.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgClippingTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.SvgConvertedImage
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "host cascade composes local clip references with inherited clip rules" do
    html =
      "<style>.art{clip-path:url(#cut)} #art{clip-path:none} g{clip-rule:evenodd}</style>\n<svg width=40 height=40><defs><g  /></defs></svg>\n"

    assert {:ok, _pdf} = HtmlToPdf.render(html)

    html =
      "<style>.art{clip-path:url(#cut)} #art{clip-path:none} defs{clip-rule:evenodd}</style>\n<svg width=40 height=40><defs><clipPath id=cut><path d='M0 0H40V40H0Z M10 10H30V30H10Z' /></clipPath></defs>\n<rect class=art width=40 height=40 fill=red /><rect class=art id=art width=5 height=5 fill=blue /></svg>\n"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    assert svg.style.svg_scene.source =~ "clip-path=\"url(#cut)\""
    assert svg.style.svg_scene.source =~ "clip-rule=\"evenodd\""
    assert SvgConvertedImage.coverage(SvgConvertedImage.render(html)) > 0
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "/Subtype /Image"
    assert pdf =~ "/Subtype /Image"
  end

  test "multiple children object bounds and linked reference clips compose actual vector geometry" do
    definitions = [
      "<clipPath id=c><rect width=20 height=40 /><circle cx=30 cy=20 r=10 /></clipPath>",
      "<clipPath id=c />",
      "<clipPath id=c clipPathUnits=objectBoundingBox><rect width='.5' height='1' /></clipPath>",
      "<clipPath id=c clip-path='url(#other)'><rect width=30 height=30 /></clipPath>",
      "<clipPath id=c><rect width=30 height=30 clip-path='url(#other)' /><circle cx=30 cy=30 r=8 /></clipPath>",
      "<clipPath id=c><use href='#shape' x=10 /></clipPath>"
    ]

    for definition <- definitions do
      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<svg width=40 height=40><defs><rect id=shape width=20 height=30 /><clipPath id=other><circle cx=15 cy=15 r=10 /></clipPath>#{definition}</defs><rect width=40 height=40 clip-path='url(#c)' /></svg>"
               )

      assert pdf =~ "/Subtype /Image"
      assert {:ok, _} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    end
  end

  test "original path clip browser fixture paints PNG artwork with surrounding logical text" do
    html =
      File.read!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/inline_svg_path_clips_scoped.html",
          __DIR__
        )
      )

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "host cascade Ω"
    assert text =~ "viewports café"
    assert text =~ "Clip artwork survived"
    assert pdf =~ "/Subtype /Image"
  end
end
