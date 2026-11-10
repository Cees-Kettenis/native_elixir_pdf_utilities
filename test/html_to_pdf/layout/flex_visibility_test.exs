defmodule NativeElixirPdfUtilities.HtmlToPdf.FlexVisibilityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Forms, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}

  # Flexbox 1 CRD 2025-10-14, 4.4 and 9.4. Collapsed items retain line struts.
  test "collapsed items remove main-axis space and interaction but preserve cross-axis struts" do
    for direction <- ["row", "column"] do
      html =
        "<div style='display:flex;flex-direction:#{direction};width:100pt;gap:5pt;background:lime'><div style='visibility:collapse;width:40pt;height:30pt'><span style='visibility:visible'>Hidden</span><input name=secret></div><div style='width:20pt;height:10pt;background:blue'>Visible</div></div><p>After</p>"

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, text} = Text.extract(pdf)
      refute text =~ "Hidden"
      assert text =~ "Visible" and text =~ "After"
      assert {:ok, []} = Forms.fields(pdf)
      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, tree} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, result} = Layout.layout(tree, page_size: {200, 200}, margin: 0)
      green = Enum.find(result.boxes, &(Map.get(&1, :fill_color) == {0, 1, 0}))
      blue = Enum.find(result.boxes, &(Map.get(&1, :fill_color) == {0, 0, 1}))
      assert blue.x == 0
      assert_in_delta green.height, if(direction == "row", do: 30, else: 10), 0.75
    end
  end

  test "wrapping restarts after collapsing and an entirely collapsed line still keeps its strut" do
    for content <- [
          "<div style='visibility:collapse;width:50pt;height:30pt'></div>",
          "<div style='width:50pt;height:10pt'>First</div><div style='visibility:collapse;width:50pt;height:30pt'></div><div style='width:50pt;height:10pt'>Last</div>"
        ] do
      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<div style='display:flex;flex-wrap:wrap;width:100pt;background:green'>#{content}</div><p>After</p>",
                 system_font_discovery: false
               )

      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "After"
    end
  end
end
