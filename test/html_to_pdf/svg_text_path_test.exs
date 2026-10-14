Code.require_file("../support/svg_converted_image.ex", __DIR__)

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
end
