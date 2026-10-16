Code.require_file("../support/svg_converted_image.ex", __DIR__)
Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

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

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, SvgScene}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator}

  test "host plans normalize large authored turns before native float conversion" do
    html =
      "<svg width=220 height=100><text x=20 y=50 font-size=20 rotate='360000030 -360000030'>AB</text></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.prepare_svg_detailed(dom)
    assert {:ok, scene} = SvgScene.prepare(svg, HtmlValidator.new_image_budget())
    assert scene.source =~ "rotate=\"360000030 -360000030\""
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, canonical} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               String.replace(html, "360000030 -360000030", "30 -30")
             )

    refute canonical =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               canonical,
               false
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(canonical)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(canonical, layout: false)
  end
end
