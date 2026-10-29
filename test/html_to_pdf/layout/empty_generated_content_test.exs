defmodule NativeElixirPdfUtilities.HtmlToPdf.EmptyGeneratedContentLayoutTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  test "empty block pseudo-elements paint their dimensions and reserve vertical space" do
    for pseudo <- ["before", "after"] do
      source =
        "<style>p{margin:0;font-size:10pt;line-height:12pt}.decorated::#{pseudo}{content:CONTENT;display:block;width:30pt;height:30pt;background:red}</style><p class=decorated>Text</p><p>Next</p>"

      reference = layout(String.replace(source, "CONTENT", "none"))
      decorated = layout(String.replace(source, "CONTENT", "''"))
      rectangle = Enum.find(decorated.boxes, &(Map.get(&1, :fill_color) == {1.0, 0.0, 0.0}))

      assert rectangle != nil
      assert rectangle.width == 30.0 and rectangle.height == 30.0

      for text <- ["Text", "Next"] do
        original = Enum.find(reference.boxes, &(Map.get(&1, :text) == text))
        shifted = Enum.find(decorated.boxes, &(Map.get(&1, :text) == text))
        displacement = if pseudo == "after" and text == "Text", do: 0.0, else: 30.0
        assert_in_delta original.y - shifted.y, displacement, 0.001
      end
    end
  end

  test "empty inline-block pseudo-elements retain borders, padding and horizontal space" do
    source =
      "<style>p{margin:0;font-size:10pt;line-height:12pt}span::before{content:'';display:inline-block;width:20pt;height:10pt;padding:3pt;margin:0 2pt;border:4px solid red;background:blue}</style><p>A<span></span>B</p>"

    decorated = layout(source)
    rectangle = Enum.find(decorated.boxes, &(Map.get(&1, :fill_color) == {0.0, 0.0, 1.0}))
    assert rectangle != nil
    assert_in_delta rectangle.width, 32.0, 0.01
    assert_in_delta rectangle.height, 22.0, 0.01
    assert rectangle.stroke_width == 3.0

    shifted_b = Enum.find(decorated.boxes, &(Map.get(&1, :text) == "B"))
    shifted_a = Enum.find(decorated.boxes, &(Map.get(&1, :text) == "A"))
    assert shifted_a != nil and shifted_b != nil
    assert_in_delta shifted_b.x - shifted_a.x - shifted_a.annotation_width, 36.0, 0.01
  end

  test "empty generated spacer height participates in public rendering and pagination" do
    html =
      "<style>p{margin:0;font-size:10pt;line-height:12pt}.decorated::before{content:'';display:block;width:30pt;height:64pt;background:red}</style><p class=decorated>First</p><p>Next</p>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html,
               page_size: {120, 80},
               margin: 0,
               system_font_discovery: false
             )

    assert {:ok, 2} = NativeElixirPdfUtilities.Info.page_count(pdf)
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "First" and text =~ "Next"
  end

  defp layout(html) do
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 240}, margin: 10)
    layout
  end
end
