Code.require_file("../support/svg_converted_image.ex", __DIR__)

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

  test "CSS cascade and resource clipping retain real converter stroke pixels" do
    html =
      "<style>svg .n {vector-effect:non-scaling-stroke} svg .n > path {vector-effect:inherit}</style><svg width='240' height='120' viewBox='0 0 120 60'><defs><clipPath id='c'><rect width='115' height='55'/></clipPath></defs><g class='n' transform='translate(5 5) scale(1.2 .7)' opacity='.7' clip-path='url(#c)'><path d='M5 20 H80 L90 30' fill='none' stroke='#174a86' stroke-width='3'/></g></svg>"

    image = SvgConvertedImage.render(html)
    assert SvgConvertedImage.coverage(image) > 100
    natural = SvgConvertedImage.render(String.replace(html, "non-scaling-stroke", "none"))
    refute image.alpha == natural.alpha
  end
end
