defmodule NativeElixirPdfUtilities.HtmlToPdf.ListRubyTablePaginationTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.Text
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}

  test "combined lists ruby captions and rowspans preserve original text once and repeated headings" do
    html = File.read!("test/fixtures/html_to_pdf/browser_parity/list_ruby_table_pagination.html")
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, text} = Text.extract(pdf)

    for label <- [
          "List one",
          "List two",
          "Base",
          "Reading",
          "Spanning cell continuation",
          "One",
          "Two",
          "Three",
          "Four",
          "Five",
          "Six",
          "First row",
          "Second row",
          "Third row",
          "Fourth row",
          "After table"
        ] do
      assert length(Regex.scan(Regex.compile!("\\b" <> Regex.escape(label) <> "\\b"), text)) == 1
    end

    assert length(Regex.scan(~r/Heading/, text)) == 2
    assert pdf =~ "/Subtype /Link"
  end

  test "ruby borrows preceding lower leading without shifting first-paragraph ruby" do
    html = File.read!("test/fixtures/html_to_pdf/browser_parity/list_ruby_table_pagination.html")
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)
    last_list_line = Enum.find(layout.boxes, &(Map.get(&1, :text) == "List two"))
    base = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Base"))
    assert_in_delta last_list_line.y - base.y, 14.25, 0.000001

    source = File.read!("test/fixtures/html_to_pdf/browser_parity/ruby_inline_contexts.html")
    assert {:ok, dom} = HtmlParser.parse_document_detailed(source)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {360, 240}, margin: 18)
    first_line = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Before "))
    assert first_line.line_box_height == 24.75
  end
end
