alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("svg_converted_image.ex"))
Code.require_file(Paths.support("svg_public_workflow.ex"))
Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextMetadataTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, SvgScene}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test("prepared scene font count stays bounded and independent preparation recovers", %{
    limits: limits
  }) do
    html =
      "<svg width=200 height=50><text y=25>Normal<tspan font-weight=700>Bold</tspan></text></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.prepare_svg_detailed(dom)
    assert {:ok, original_scene} = SvgScene.prepare(svg, HtmlValidator.new_image_budget())
    assert length(original_scene.selected_fonts) == 2
    Limits.install(Map.put(limits, :max_font_count, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html)

    assert diagnostic.stage == :font and diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.FontValidator
    assert diagnostic.operation == :load_registry
    assert diagnostic.message =~ "max_font_count"
    Limits.install(limits)
    assert {:ok, scene} = SvgScene.prepare(svg, HtmlValidator.new_image_budget())
    assert length(scene.selected_fonts) == 2
  end

  test "descriptive metadata within visible text never changes converter pixels" do
    html =
      "<svg width=200 height=50><text y=25><title>Title only</title><desc>Description only</desc>Visible Ω<tspan>!</tspan></text></svg>"

    image = NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.render(html)

    without_metadata =
      html
      |> String.replace("<title>Title only</title>", "")
      |> String.replace("<desc>Description only</desc>", "")

    reference = NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.render(without_metadata)
    assert image == reference
    assert NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.coverage(image) > 20
  end

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
end
