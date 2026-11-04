alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("svg_converted_image.ex"))
Code.require_file(Paths.support("svg_public_workflow.ex"))
Code.require_file(Paths.support("png_fixture.ex"))

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

  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  test "honest host preparation binds identical attributes to their distinct inherited font contexts" do
    html =
      "<svg width=200 height=100><g font-size=20><text x='1em 2em' y='1rem'>AB</text></g><g font-size=30><text x='1em 2em' y='1rem'>CD</text></g></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.prepare_svg_detailed(dom)
    assert {:ok, scene} = SvgScene.prepare(svg, HtmlValidator.new_image_budget())
    assert scene.source =~ "x=\"1em 2em\""
    assert scene.source =~ "x=\"1em 2em\""
    assert length(Regex.scan(~r/y="1rem"/, scene.source)) == 2
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test "reused em lists retain each instance size and original Unicode order" do
    html =
      "<svg width=300 height=100 font-size=20><defs><g id=label><text x='1em 2em 3em' y='1.3em'>Run</text></g></defs><use href='#label' x='4' font-size='18' /><use href='#label' x='150' y='35' font-size='24' /></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    assert svg.style.svg_scene.source =~ "x=\"1em 2em 3em\""
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test(
    "position preparation preserves vector-only surface admission and rejects actual raster requests",
    %{data: data, limits: limits}
  ) do
    source =
      "<svg width='200' height='100'><text x='5 35 65' y='25' font-family='NativeSvgFont0'>AΩé</text></svg>"

    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source, [], %{
               mode: :inline,
               fonts: [%{data: data}]
             })

    assert diagnostic.message =~ "max_aggregate_svg_surface_pixels"

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=200 height=100><text x='5 35 65' y=25>AΩé</text></svg>"
             )

    assert diagnostic.message =~ "max_aggregate_svg_surface_pixels"
  end
end
