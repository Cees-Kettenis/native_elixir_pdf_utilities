defmodule NativeElixirPdfUtilities.HtmlToPdf.RubyDecorationTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}

  test "Ruby bases retain ancestor decorations while default annotations start a separate scope" do
    for origin <- [
          "<p style='text-decoration:underline 3pt #336699'><ruby>",
          "<p><ruby style='text-decoration:underline 3pt #336699'>"
        ],
        line <- ["underline", "overline", "line-through"] do
      origin = String.replace(origin, "underline", line)
      assert {:ok, dom} = HtmlParser.parse_detailed(origin <> "Base<rt>Reading</rt></ruby></p>")

      assert {:ok, %{children: [paragraph]} = styled} =
               Style.compute_detailed(dom, system_font_discovery: false)

      [%{children: [base, annotation]}] = paragraph.children
      assert [_] = base.style._text_decorations
      refute Map.has_key?(annotation.style, :_text_decorations)
      [%{style: annotation_text_style}] = annotation.children
      refute Map.has_key?(annotation_text_style, :_text_decorations)
      assert {:ok, layout} = Layout.layout(styled, page_size: {300, 300}, margin: 12)
      lines = Enum.filter(layout.boxes, &(Map.get(&1, :role) == :text_decoration))
      assert length(lines) == 1
    end
  end

  test "annotations preserve their own decorations and child decorations" do
    for content <- [
          "<rt style='text-decoration:underline 2pt red'>Reading</rt>",
          "<rt><u>Reading</u></rt>"
        ] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<p style='text-decoration:underline 3pt blue'><ruby>Base#{content}</ruby></p>"
               )

      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled, page_size: {300, 300}, margin: 12)
      lines = Enum.filter(layout.boxes, &(Map.get(&1, :role) == :text_decoration))
      assert length(lines) == 2
    end
  end

  test "authored display overrides and legacy fallback rt retain ordinary decoration propagation" do
    for content <- [
          "<ruby>Base<rt style='display:inline'>Reading</rt></ruby>",
          "<rt>Reading</rt>"
        ] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<p style='text-decoration:underline 3pt blue'>#{content}</p>"
               )

      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled, page_size: {300, 300}, margin: 12)
      reading = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Reading"))
      lines = Enum.filter(layout.boxes, &(Map.get(&1, :role) == :text_decoration))
      assert Enum.any?(lines, &(&1.x == reading.x and &1.width == reading.annotation_width))
    end
  end

  test "all propagated layers stop before an annotation, including a cleared child" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<p style='text-decoration:underline red'><u><ruby>Base<rt style='text-decoration:none'>Reading</rt></ruby></u></p>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {300, 300}, margin: 12)
    base = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Base"))
    reading = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Reading"))
    lines = Enum.filter(layout.boxes, &(Map.get(&1, :role) == :text_decoration))
    assert length(lines) == 2
    assert Enum.all?(lines, &(&1.x == base.x and &1.width == base.annotation_width))
    refute Enum.any?(lines, &(&1.y > reading.y - 4.0))
  end
end
