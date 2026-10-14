Code.require_file("../support/svg_converted_image.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextStrokeTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test "host cascade strokes coexist with positions lengths paths and transformed clipping" do
    html =
      "<svg width='400' height='200' font-size='26'><defs><path id='p' d='M20 140 Q200 60 380 140'/><clipPath id='c'><rect x='10' y='5' width='370' height='180'/></clipPath></defs><g transform='translate(5 8) skewX(5)' clip-path='url(#c)' opacity='.7' stroke='#143e8a' stroke-width='1.5'><text x='20' y='45' textLength='220' lengthAdjust='spacingAndGlyphs'>Length Ωé</text><text fill='none'><textPath href='#p' startOffset='10%'>Path ffi Ωé</textPath></text></g></svg>"

    image = NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.render(html)
    assert NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.coverage(image) > 100
  end
end
