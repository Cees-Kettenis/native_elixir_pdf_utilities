Code.require_file("../support/svg_converted_image.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextRotationTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test "equivalent large authored turns preserve actual converter glyph pixels" do
    html =
      "<svg width=220 height=100><text x=20 y=50 font-size=20 rotate='360000030 -360000030'>AB</text></svg>"

    image = NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.render(html)

    canonical =
      NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.render(
        String.replace(html, "360000030 -360000030", "30 -30")
      )

    assert image == canonical
    assert NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.coverage(image) > 20
  end
end
