alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("svg_converted_image.ex"))
Code.require_file(Paths.support("svg_public_workflow.ex"))
Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgVectorEffectTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.SvgConvertedImage
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.Validators.SvgStyleValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "canonical vector-effect is non-inherited and accepts explicit inherit and unset" do
    parent = SvgStyleValidator.defaults(%{})
    assert {:ok, parent} = SvgStyleValidator.apply(parent, "vector-effect", "non-scaling-stroke")
    parent_context = Map.put(parent, :_svg_inherited, SvgStyleValidator.parent_properties(parent))
    child = SvgStyleValidator.defaults(parent_context)
    assert child.svg_vector_effect == "none"
    assert {:ok, inherited} = SvgStyleValidator.apply(child, "vector-effect", "inherit")
    assert inherited.svg_vector_effect == "non-scaling-stroke"
    assert {:ok, reset} = SvgStyleValidator.apply(inherited, "vector-effect", "unset")
    assert reset.svg_vector_effect == "none"

    assert {:error, {:invalid_css, diagnostic}} =
             SvgStyleValidator.apply(child, "vector-effect", "fixed-position")

    assert diagnostic.source == "vector-effect"
    assert diagnostic.stage == :css
    assert diagnostic.message =~ "fixed-position"
  end

  test "independent converter input diagnoses whitespace around non-scaling stroke values" do
    for value <- ["non-scaling-stroke", " non-scaling-stroke ", "\tnon-scaling-stroke\n"] do
      source = "<svg width='20' height='20'><path d='M0 0L20 20' vector-effect='#{value}'/></svg>"

      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer.rasterize(source, [], nil)

      assert diagnostic.stage == :layout
      assert diagnostic.operation == :convert
      assert diagnostic.source == "vector-effect"
      assert diagnostic.message =~ "non-scaling strokes"
    end
  end

  test "CSS cascade diagnoses unsupported strokes and retains ordinary converter strokes" do
    html =
      "<style>svg .n {vector-effect:non-scaling-stroke} svg .n > path {vector-effect:inherit}</style><svg width='240' height='120' viewBox='0 0 120 60'><defs><clipPath id='c'><rect width='115' height='55'/></clipPath></defs><g class='n' transform='translate(5 5) scale(1.2 .7)' opacity='.7' clip-path='url(#c)'><path d='M5 20 H80 L90 30' fill='none' stroke='#174a86' stroke-width='3'/></g></svg>"

    assert {:error, {:unsupported_html, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, system_font_discovery: false)

    assert diagnostic.stage == :layout
    assert diagnostic.source == "vector-effect"
    assert diagnostic.message =~ "non-scaling strokes"
    natural = SvgConvertedImage.render(String.replace(html, "non-scaling-stroke", "none"))
    assert SvgConvertedImage.coverage(natural) > 100
  end

  test "converter limitation: solid non-scaling strokes rebase geometry under nonuniform and sheared CTMs" do
    for transform <- ["scale(2 .5)", "matrix(2 .2 .4 .6 15 20)", "translate(200 0) scale(-2 .5)"] do
      source =
        "<svg width='240' height='120'><g transform='#{transform}' opacity='.7'><path d='M10 40 H80' fill='none' stroke='#174a86' stroke-width='3' stroke-opacity='.4' vector-effect='non-scaling-stroke'/></g></svg>"

      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(source, system_font_discovery: false)

      assert diagnostic.stage == :layout
      assert diagnostic.source == "vector-effect"
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.message =~ "non-scaling strokes"
    end
  end

  test "converter limitation: CSS cascade, explicit inheritance, viewBox and imported clip/opacity resources retain strokes" do
    html =
      "<style>svg .n {vector-effect:non-scaling-stroke} svg .n > path {vector-effect:inherit}</style><svg width='240' height='120' viewBox='0 0 120 60'><defs><clipPath id='c'><rect width='115' height='55'/></clipPath></defs><g class='n' transform='translate(5 5) scale(1.2 .7)' opacity='.7' clip-path='url(#c)'><path d='M5 20 H80 L90 30' fill='none' stroke='#174a86' stroke-width='3'/></g></svg>"

    assert {:error, {:unsupported_html, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, system_font_discovery: false)

    assert diagnostic.stage == :layout
    assert diagnostic.source == "vector-effect"
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
    assert diagnostic.message =~ "non-scaling strokes"
  end

  test "converter limitation: the original non-scaling server denial now paints the same source geometry" do
    source =
      "<svg width='240' height='120'><defs><linearGradient id='p'><stop stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient></defs><path d='M10 40 H180 L190 60' fill='none' stroke='url(#p)' vector-effect='non-scaling-stroke'/></svg>"

    assert {:error, {:unsupported_html, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(source, system_font_discovery: false)

    assert diagnostic.stage == :layout
    assert diagnostic.source == "vector-effect"
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
    assert diagnostic.message =~ "non-scaling strokes"
  end
end
