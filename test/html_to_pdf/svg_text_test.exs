Code.require_file("../support/svg_converted_image.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    %{limits: limits, data: data}
  end

  test "span positions and selected styles paint actual converter glyph pixels" do
    html =
      "<svg width=300 height=100 font-family='DejaVu Sans' font-size=20 text-anchor=middle>\n<text x=150 y=25>One<tspan dx=4 font-weight=700>Bold</tspan><tspan x=150 y=65 font-style=italic>café Ω</tspan></text>\n</svg>\n"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    assert length(svg.style.svg_scene.selected_fonts) == 3
    image = painted_image(html)
    assert coverage(image) > 100
  end

  test "uppercase original font attributes use shared CSS authority and paint glyphs" do
    html =
      "<svg width=200 height=50 font-family='DejaVu Sans'><text y=25 font-size='20px' font-weight='BOLD' font-style='ITALIC' letter-spacing='NORMAL'>Original Ω</text></svg>"

    image = painted_image(html)
    assert coverage(image) > 100
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    [text] = svg.style.svg_scene.original_tree.children
    assert text.style.font_face.data == File.read!("priv/fonts/dejavu/DejaVuSans-BoldOblique.ttf")
  end

  test "original letter-spacing changes actual converter glyph placement" do
    html =
      "<svg width=200 height=50 font-family='DejaVu Sans'><text y=30 font-size=20 letter-spacing='2px'>AA</text></svg>"

    spaced = painted_image(html)
    unspaced = painted_image(String.replace(html, "letter-spacing='2px'", ""))
    assert coverage(spaced) > 20 and coverage(unspaced) > 20
    refute spaced.alpha == unspaced.alpha
  end

  test "same face text reuse paints both original converter instances" do
    html =
      "<svg width=220 height=70 font-family='DejaVu Sans' font-size=16>\n<defs><g id=label><text y=20>Repeat Ω</text></g></defs>\n<use href='#label' x=10 /><use href='#label' x=110 y=30 />\n</svg>\n"

    image = painted_image(html)
    single = painted_image(String.replace(html, "<use href='#label' x=110 y=30 />", ""))
    assert coverage(image) > coverage(single)
    assert coverage(single) > 50
  end

  test "hidden source text remains unpainted while explicitly visible spans survive" do
    html =
      "<svg width=200 height=80 font-family='DejaVu Sans'>\n<text x=10 y=20 visibility=hidden>Hidden<tspan visibility=visible>Visible Ω</tspan></text>\n<text x=10 y=40 display=none>Removed</text>\n<rect x=10 y=55 width=30 height=10 />\n</svg>\n"

    image = painted_image(html)
    hidden = painted_image(String.replace(html, "visibility=visible", "visibility=hidden"))
    assert coverage(image) > coverage(hidden)
    assert coverage(hidden) > 0
  end

  test "reused hidden text paints only explicitly visible converter spans" do
    html =
      "<svg width=200 height=50 font-family='DejaVu Sans'><defs><g id='label'><text y=25>Hidden<tspan visibility='visible'>Visible Ω</tspan></text></g></defs><use href='#label' visibility='hidden' /></svg>"

    visible = painted_image(html)
    hidden = painted_image(String.replace(html, "visibility='visible'", "visibility='hidden'"))
    assert coverage(visible) > 20
    assert coverage(hidden) == 0
  end

  test "original whitespace paints glyphs while hidden-only source has no alpha coverage" do
    html =
      "<svg width=200 height=50 font-family='DejaVu Sans'><text y=25>  One   <tspan> Two </tspan>  Three  </text></svg>"

    assert coverage(painted_image(html)) > 100

    hidden =
      "<svg width=200 height=50 font-family='DejaVu Sans'><text y=25 visibility=hidden>Hidden Ω</text></svg>"

    assert coverage(painted_image(hidden)) == 0
  end

  defp painted_image(html) do
    NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.render(html)
  end

  defp coverage(image) do
    NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.coverage(image)
  end
end
