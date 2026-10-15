Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgNonScalingServersTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "converter limitation: resolved original object bbox gradient is mapped by the stroke's real CTM" do
    source =
      "<svg width='240' height='120'><defs><linearGradient id='p'><stop stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient></defs><path transform='scale(2 .5)' d='M10 40 H180 L190 60' fill='none' stroke='url(#p)' stroke-width='3' vector-effect='non-scaling-stroke'/></svg>"

    assert {:error, {:unsupported_html, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(source, system_font_discovery: false)

    assert diagnostic.stage == :layout
    assert diagnostic.source == "vector-effect"
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
    assert diagnostic.message =~ "non-scaling strokes"
  end

  test "converter limitation: the same source gradient retains separate original fill and host stroke mappings" do
    source =
      "<svg width='240' height='120'><defs><linearGradient id='p'><stop stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient></defs><path transform='scale(2 .5)' d='M10 40 H180 L190 60 Z' fill='url(#p)' stroke='url(#p)' stroke-width='3' paint-order='stroke fill' vector-effect='non-scaling-stroke'/></svg>"

    assert {:error, {:unsupported_html, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(source, system_font_discovery: false)

    assert diagnostic.stage == :layout
    assert diagnostic.source == "vector-effect"
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
    assert diagnostic.message =~ "non-scaling strokes"
  end

  test "converter limitation: radial and user-space server transforms retain their genuine resolved resources under affine strokes" do
    for server <- [
          "<linearGradient id='p' gradientUnits='userSpaceOnUse' x1='5' y1='10' x2='160' y2='90' gradientTransform='rotate(15)'><stop stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient>",
          "<radialGradient id='p' cx='.6' cy='.5' r='.65' fx='.3' fy='.4' gradientTransform='scale(1 .8)'><stop stop-color='red'/><stop offset='1' stop-color='blue'/></radialGradient>"
        ],
        transform <- ["matrix(1.2 .2 .4 .8 10 15)", "translate(220 0) scale(-1 .7)"] do
      source =
        "<svg width='240' height='120'><defs>#{server}</defs><path transform='#{transform}' d='M10 40 Q80 0 160 70 L190 90' fill='none' stroke='url(#p)' stroke-width='4' stroke-opacity='.6' vector-effect='non-scaling-stroke'/></svg>"

      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(source, system_font_discovery: false)

      assert diagnostic.stage == :layout
      assert diagnostic.source == "vector-effect"
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.message =~ "non-scaling strokes"
    end
  end

  test "converter limitation: user-space pattern is sampled using its original tile and real host matrix" do
    source =
      "<svg width='240' height='120'><defs><pattern id='p' patternUnits='userSpaceOnUse' x='3' y='4' width='12' height='10' patternTransform='translate(2 3)'><rect width='12' height='10' fill='gold'/><circle cx='4' cy='4' r='2' fill='blue'/></pattern></defs><path transform='scale(2 .5)' d='M10 40 H100 L110 60' fill='none' stroke='url(#p)' stroke-width='5' vector-effect='non-scaling-stroke'/></svg>"

    assert {:error, {:unsupported_html, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(source, system_font_discovery: false)

    assert diagnostic.stage == :layout
    assert diagnostic.source == "vector-effect"
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
    assert diagnostic.message =~ "non-scaling strokes"
  end

  test "converter limitation: prepared server native stroke binds its paint and stroking opacity in registered resources" do
    source =
      "<svg width='240' height='120'><defs><pattern id='p' patternUnits='userSpaceOnUse' width='12' height='10'><rect width='12' height='10' fill='gold'/><circle cx='4' cy='4' r='2' fill='blue'/></pattern></defs><path transform='scale(2 .5)' d='M10 40 H100 L110 60' fill='none' stroke='url(#p)' stroke-width='5' stroke-opacity='.4' vector-effect='non-scaling-stroke'/></svg>"

    assert {:error, {:unsupported_html, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(source, system_font_discovery: false)

    assert diagnostic.stage == :layout
    assert diagnostic.source == "vector-effect"
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
    assert diagnostic.message =~ "non-scaling strokes"
  end

  test "converter limitation: object-bbox pattern and viewBox content remain bound inside clip and opacity forms" do
    html =
      "<svg width='240' height='120'><defs><clipPath id='c'><rect width='230' height='110'/></clipPath><pattern id='p' width='.2' height='.5' viewBox='0 0 12 10' preserveAspectRatio='none'><rect width='12' height='10' fill='gold'/><circle cx='4' cy='4' r='2' fill='blue'/></pattern></defs><g opacity='.5' clip-path='url(#c)'><path transform='scale(2 .5)' d='M10 40 H100 L110 60' fill='none' stroke='url(#p)' stroke-width='5' vector-effect='non-scaling-stroke'/></g></svg>"

    assert {:error, {:unsupported_html, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, system_font_discovery: false)

    assert diagnostic.stage == :layout
    assert diagnostic.source == "vector-effect"
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
    assert diagnostic.message =~ "non-scaling strokes"
  end
end
