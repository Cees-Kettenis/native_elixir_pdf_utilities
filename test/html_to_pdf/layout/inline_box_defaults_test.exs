defmodule NativeElixirPdfUtilities.HtmlToPdf.InlineBoxDefaultsTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.CssValueValidator

  test "individual edge declarations prepare missing ordinary inline box defaults" do
    for {property, key, edge, value} <- [
          {"margin-left", :margin, :left, 1.0},
          {"margin-right", :margin, :right, 2.0},
          {"padding-top", :padding, :top, 3.0}
        ] do
      assert {:ok, style} =
               CssValueValidator.validate_resolved(%{font_size: 12.0}, property, "#{value}pt")

      assert style[key][edge] == value
    end

    html = "<p>A<span style='width:20pt;margin-left:1pt;margin-right:2pt'>ordinary</span>B</p>"
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    [paragraph] = styled.children
    span = Enum.find(paragraph.children, &(Map.get(&1, :tag) == "span"))
    assert span.style.display == :inline
    assert span.style.width == 20.0
    assert span.style.margin.left == 1.0 and span.style.margin.right == 2.0
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    assert String.starts_with?(pdf, "%PDF-")
  end
end
