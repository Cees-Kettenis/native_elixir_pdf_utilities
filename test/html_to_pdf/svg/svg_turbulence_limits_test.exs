defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTurbulenceLimitsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer

  alias NativeElixirPdfUtilities.Validators.{
    LimitsValidator,
    SvgFilterValidator,
    SvgStyleValidator
  }

  setup do
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    :ok
  end

  test "default and bounded octave counts remain admitted" do
    for attributes <- [
          %{},
          %{"numOctaves" => "0"},
          %{"numOctaves" => "1"},
          %{"numOctaves" => "16"},
          %{"numOctaves" => "  +2e0\t"},
          %{"numOctaves" => ".5"}
        ] do
      assert :ok = SvgFilterValidator.validate_source_attributes("feTurbulence", attributes)
    end
  end

  test "the configured ceiling applies before converter rounding and can be raised" do
    assert {:ok, limits} = LimitsValidator.validate(max_svg_turbulence_octaves: 2)
    Limits.install(limits)

    assert :ok =
             SvgFilterValidator.validate_source_attributes("feTurbulence", %{"numOctaves" => "2"})

    for value <- ["2.01", "3", "1000000000"] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               SvgFilterValidator.validate_source_attributes("feTurbulence", %{
                 "numOctaves" => value
               })

      assert diagnostic.stage == :limits and diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.operation == :prepare and diagnostic.module == SvgFilterValidator
      assert diagnostic.source == "feTurbulence"
      assert diagnostic.message =~ "max_svg_turbulence_octaves (2)"
      assert diagnostic.message =~ value and diagnostic.message =~ "reduce numOctaves"
    end

    assert {:ok, limits} = LimitsValidator.validate(max_svg_turbulence_octaves: 32)
    Limits.install(limits)

    assert :ok =
             SvgFilterValidator.validate_source_attributes("feTurbulence", %{
               "numOctaves" => "32"
             })
  end

  test "malformed and nonfinite counts cannot fall through to the converter" do
    for value <- ["", "-1", "NaN", "Infinity", "1e999", "2px", "2%", "1 2", "2,3", nil] do
      assert {:error, {:invalid_document, diagnostic}} =
               SvgFilterValidator.validate_source_attributes("feTurbulence", %{
                 "numOctaves" => value
               })

      assert diagnostic.stage == :layout and diagnostic.reason == :invalid_document
      assert diagnostic.source == "feTurbulence" and diagnostic.module == SvgFilterValidator
      assert diagnostic.operation == :prepare
      assert diagnostic.message =~ "numOctaves requires a finite nonnegative unitless number"
    end
  end

  test "qualified lookalikes cannot replace the actual octave parameter" do
    assert {:ok, []} =
             SvgStyleValidator.presentation_declarations(%{
               tag: "feTurbulence",
               attributes: %{"numOctaves" => "1000000000"},
               attribute_namespaces: %{"numOctaves" => "urn:other"}
             })

    assert {:error, {:resource_limit_exceeded, %{source: "feTurbulence"}}} =
             SvgStyleValidator.presentation_declarations(%{
               tag: "feTurbulence",
               attributes: %{"numOctaves" => "1000000000"}
             })
  end

  test "inline SVG and mapped SVG images reject expensive filters with public diagnostics" do
    svg = """
    <svg xmlns="http://www.w3.org/2000/svg" width="2" height="2">
      <filter id="noise"><feTurbulence baseFrequency=".1" numOctaves="1000000000"/></filter>
      <rect width="2" height="2" filter="url(#noise)"/>
    </svg>
    """

    for {html, options} <- [
          {svg, []},
          {"<img src='noise.svg'>", [assets: %{"noise.svg" => {:bytes, svg}}]}
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render(html, [system_font_discovery: false] ++ options)

      assert diagnostic.stage == :limits and diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.operation == :prepare and diagnostic.module == SvgFilterValidator
      assert diagnostic.source == "feTurbulence"
      assert diagnostic.message =~ "max_svg_turbulence_octaves"
    end

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgRasterizer.rasterize(svg, [], nil)

    assert diagnostic.line == 2
    assert diagnostic.source == "feTurbulence" and diagnostic.stage == :limits
    assert diagnostic.message =~ "max_svg_turbulence_octaves"
  end

  test "ordinary turbulence still renders through the converter" do
    svg =
      "<svg xmlns='http://www.w3.org/2000/svg' width='2' height='2'><filter id='noise'><feTurbulence baseFrequency='.1' numOctaves='2'/></filter><rect width='2' height='2' filter='url(#noise)'/></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(svg, system_font_discovery: false)
    assert pdf =~ "/Subtype /Image"
    assert {:ok, <<137, 80, 78, 71, _rest::binary>>} = SvgRasterizer.rasterize(svg, [], nil)
  end
end
