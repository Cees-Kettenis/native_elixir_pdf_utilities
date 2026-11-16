defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgStaticRenderingPolicyTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.SvgStyleValidator

  test "SVG rendering hints retain their own inherited converter fields" do
    source =
      "<svg style='image-rendering:pixelated;text-rendering:geometricPrecision;color-rendering:optimizeQuality;shape-rendering:crispEdges'><rect width='10' height='10'/></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(source)
    assert {:ok, %{children: [svg]}} = Style.prepare_svg_detailed(dom)
    [rect] = svg.children

    for style <- [svg.style, rect.style] do
      assert style.svg_image_rendering == "pixelated"
      assert style.svg_text_rendering == "geometricPrecision"
      assert style.svg_color_rendering == "optimizeQuality"
      assert style.svg_shape_rendering == "crispEdges"
    end

    assert {:ok, bad_dom} =
             HtmlParser.parse_detailed(
               "<svg><rect width='10' height='10' style='fill:invalid'/></svg>"
             )

    assert {:error, _} = Style.prepare_svg_detailed(bad_dom)

    for property <- ~w(image-rendering text-rendering color-rendering) do
      assert :error == SvgStyleValidator.parse(property, "invalid")
    end
  end

  test "relative and mixed SVG paints use the selected system palette and current color" do
    source =
      "<svg style='color:blue;color-scheme:dark'><rect width='10' height='10' style='fill:light-dark(red, lime);stroke:rgb(from currentColor r g b)'/></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(source)
    assert {:ok, %{children: [svg]}} = Style.prepare_svg_detailed(dom)
    [rect] = svg.children
    assert rect.style.svg_fill == {:color, {0, 1, 0}}
    assert rect.style.svg_stroke == {:color, {0.0, 0.0, 1.0}}
  end
end
