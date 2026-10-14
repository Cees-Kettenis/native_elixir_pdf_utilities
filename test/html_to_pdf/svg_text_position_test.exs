Code.require_file("../support/svg_converted_image.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextPositionTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, SvgScene}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits, data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf")}
  end

  test "honest host preparation binds identical attributes to their distinct inherited font contexts" do
    html =
      "<svg width=200 height=100><g font-size=20><text x='1em 2em' y='1rem'>AB</text></g><g font-size=30><text x='1em 2em' y='1rem'>CD</text></g></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.prepare_svg_detailed(dom)
    assert {:ok, scene} = SvgScene.prepare(svg, HtmlValidator.new_image_budget())
    assert scene.source =~ "1em 2em"
    image = NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.render(html)
    assert NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.coverage(image) > 30
  end

  test "reused em lists retain each actual instance size and original Unicode order" do
    html =
      "<svg width=300 height=100 font-size=20><defs><g id=label><text x='1em 2em 3em' y='1.3em'>Run</text></g></defs><use href='#label' x='4' font-size='18' /><use href='#label' x='150' y='35' font-size='24' /></svg>"

    image = NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.render(html)

    single =
      NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.render(
        String.replace(html, "<use href='#label' x='150' y='35' font-size='24' />", "")
      )

    assert NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.coverage(image) >
             NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.coverage(single)

    assert NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.coverage(single) > 20
  end
end
