defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgFilterInkTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}

  test "generated and constant effect outputs do not claim unrelated original glyphs" do
    for primitive <- [
          "<feFlood flood-color='blue'/>",
          "<feTurbulence baseFrequency='.05'/>",
          "<feGaussianBlur result='unused'/><feFlood flood-color='blue'/>",
          "<feFlood result='flood' flood-color='blue'/><feOffset in='flood' dx='2'/>",
          "<feComposite operator='arithmetic' k4='1'/>",
          "<feColorMatrix type='matrix' values='0 0 0 0 .2 0 0 0 0 .3 0 0 0 0 .5 0 0 0 0 1'/>",
          "<feComponentTransfer><feFuncR type='linear' slope='0' intercept='.2'/><feFuncG type='linear' slope='0' intercept='.3'/><feFuncB type='linear' slope='0' intercept='.5'/><feFuncA type='linear' slope='0' intercept='1'/></feComponentTransfer>"
        ],
        original <- [
          "<text x='5' y='25' font-family='DejaVu Sans'>Original glyphs</text>",
          "<foreignObject width='150' height='35'><div style='font-family:DejaVu Sans'>Original glyphs</div></foreignObject>"
        ] do
      html =
        "<svg width='200' height='60'><defs><filter id='f'>#{primitive}</filter></defs><g filter='url(#f)'>#{original}</g></svg>"

      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert pdf =~ "/Subtype /Image"
      refute pdf =~ "/ActualText"
      assert {:error, {:no_extractable_text, _}} = Text.extract(pdf, layout: false)
    end
  end

  test "a live graph dependency retains its genuinely effected selected Unicode" do
    for primitive <- [
          "<feFlood result='flood' flood-color='blue'/><feBlend in='SourceGraphic' in2='flood'/>",
          "<feGaussianBlur result='blur' stdDeviation='1'/><feFlood result='flood' flood-color='blue'/><feMerge><feMergeNode in='flood'/><feMergeNode in='blur'/></feMerge>",
          "<feComposite in='SourceGraphic' operator='arithmetic' k2='1'/>",
          "<feColorMatrix type='saturate' values='.5'/>"
        ] do
      html =
        "<svg width='200' height='60'><defs><filter id='f'>#{primitive}</filter></defs><g filter='url(#f)'><foreignObject width='150' height='35'><div style='font-family:DejaVu Sans'>Original café Ω</div></foreignObject></g></svg>"

      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert pdf =~ "/Subtype /Image"
      assert pdf =~ "/ActualText"
      assert {:ok, "Original café Ω"} = Text.extract(pdf, layout: false)
    end
  end
end
