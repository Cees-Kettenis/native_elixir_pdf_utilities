defmodule NativeElixirPdfUtilities.HtmlToPdf.InlineAutoWidthClampTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  test "ordinary auto inline blocks wrap within the genuine containing content width after edges" do
    html =
      "<body style='margin:0;font-family:DejaVu Sans;font-size:12pt;line-height:18pt'><div style='width:100pt'><span style='display:inline-block;padding:5pt;border:1pt solid;background:red;margin:0 3pt'>Alpha beta gamma delta epsilon</span></div></body>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {220, 180}, margin: 0)
    [background] = Enum.filter(layout.boxes, &(Map.get(&1, :fill_color) == {1.0, 0.0, 0.0}))
    assert_in_delta background.width, 94.0, 1.0e-9
    text = Enum.filter(layout.boxes, &(&1.type == :text))
    assert length(Enum.uniq(Enum.map(text, & &1.y))) == 3
    assert Enum.all?(text, &(&1.x >= 8.75 and &1.x + &1.annotation_width <= 91.25))
  end

  test "authored inline block width retains its overflow geometry" do
    html =
      "<body style='margin:0'><div style='width:100pt'><span style='display:inline-block;width:120pt;padding:5pt;border:1pt solid;background:red;margin:0 3pt'>Alpha beta gamma delta epsilon</span></div></body>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {220, 180}, margin: 0)
    [background] = Enum.filter(layout.boxes, &(Map.get(&1, :fill_color) == {1.0, 0.0, 0.0}))
    assert_in_delta background.width, 131.5, 1.0e-9
  end
end
