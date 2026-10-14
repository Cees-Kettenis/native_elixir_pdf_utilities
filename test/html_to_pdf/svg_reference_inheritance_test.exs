Code.require_file("../support/svg_converted_image.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgReferenceInheritanceTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.SvgConvertedImage
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "naturally inherited live instance paints and prepared static opacity remain supported" do
    html =
      "<svg width=100 height=80><defs><g id=target><rect width=10 height=10 fill=inherit stroke=currentColor /></g></defs><use href='#target' fill=blue color=red /><use href='#target' x=20 fill=green color=blue /><g opacity='.5'><g opacity='inherit'><rect x=40 width=10 height=10 fill=red /></g></g></svg>"

    image = SvgConvertedImage.render(html)

    pixels =
      for <<r, g, b <- image.data>> do
        {r, g, b}
      end

    assert {0, 0, 255} in pixels
    assert {0, 128, 0} in pixels
    assert SvgConvertedImage.coverage(image) > 100
  end

  test "qualified passive attributes do not become inherited SVG presentation declarations" do
    html =
      "<svg width=100 height=80 xmlns:q='urn:example:passive'><defs><g id=target q:opacity='inherit'><rect width=40 height=30 fill=red /></g></defs><use href='#target' opacity='.5' /></svg>"

    assert {:ok, _pdf} = HtmlToPdf.render(html)
  end
end
