Code.require_file("../support/svg_converted_image.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextLengthTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.SvgConvertedImage
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test "length units and anchors retain actual host font and viewport contexts" do
    html =
      "<svg width=280 height=150 font-size=20><text x=140 y=30 text-anchor=middle textLength='6em' lengthAdjust=spacing>ABCD</text><text x=140 y=80 text-anchor=end textLength='50%' lengthAdjust=spacingAndGlyphs>ABCD</text></svg>"

    image = SvgConvertedImage.render(html)
    natural = SvgConvertedImage.render(Regex.replace(~r/ textLength='[^']+'/, html, ""))
    assert SvgConvertedImage.coverage(image) > 100
    refute image.alpha == natural.alpha
  end

  test "parent length and positioned chunks paint through the existing converter" do
    source =
      "<svg width=200 height=80><text y='40' textLength='120'>A<tspan>B</tspan></text></svg>"

    assert SvgConvertedImage.coverage(SvgConvertedImage.render(source)) > 20
    source = "<svg width=200 height=80><text y='40' x='0 30' textLength='120'>AB</text></svg>"
    assert SvgConvertedImage.coverage(SvgConvertedImage.render(source)) > 20
  end
end
