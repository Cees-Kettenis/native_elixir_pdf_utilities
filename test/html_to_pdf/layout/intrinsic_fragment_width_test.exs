defmodule NativeElixirPdfUtilities.HtmlToPdf.IntrinsicFragmentWidthTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}

  @image "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAICAIAAAB/FOjAAAAAGElEQVR4nGM4oSGHFYkELMCKGEY10EIDAJ7zhIFJAvtOAAAAAElFTkSuQmCC"

  test "auto max-content allocation fits its styled text and atomic image fragments" do
    for container <- [
          "display:flex;gap:6pt",
          "display:grid;grid-template-columns:auto auto;gap:6pt"
        ] do
      html =
        "<style>body{font-size:10.5pt;line-height:13.5pt;margin:0}</style><body><div style='#{container}'><span>Flex<img src='#{@image}' style='width:30pt;height:24pt'>end</span><span>Peer</span></div></body>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled, page_size: {240, 360}, margin: 12)
      texts = Enum.filter(layout.boxes, &(&1.type == :text))
      first = Enum.find(texts, &(&1.text == "Flex"))
      last = Enum.find(texts, &(&1.text == "end"))
      assert first.y == last.y
      [image] = Enum.filter(layout.boxes, &(&1.type == :image))
      assert image.width == 30 and image.height == 24
      assert first.x < image.x and image.x + image.width <= last.x
    end
  end

  test "shrink-to-fit inline boxes use the same fragment advances as their line breaker" do
    html =
      "<p><span style='display:inline-block;font-size:10.5pt;line-height:13.5pt'>Flex<img src='#{@image}' style='width:30pt;height:24pt'>end</span>after</p>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 360}, margin: 12)
    texts = Enum.filter(layout.boxes, &(&1.type == :text))
    assert Enum.find(texts, &(&1.text == "Flex")).y == Enum.find(texts, &(&1.text == "end")).y
    assert Enum.any?(texts, &(&1.text == "after"))
  end
end
