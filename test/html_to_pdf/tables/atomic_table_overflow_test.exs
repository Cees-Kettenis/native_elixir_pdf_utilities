defmodule NativeElixirPdfUtilities.HtmlToPdf.AtomicTableOverflowTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.Text
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout, Pagination}

  test "atomic allocation preserves following flow independently of visible or clipped overflow" do
    for {fixture, count} <- [
          {"atomic_table_visible_overflow.html", 2},
          {"atomic_table_hidden_overflow.html", 1}
        ] do
      html = File.read!("test/fixtures/html_to_pdf/browser_parity/" <> fixture)
      assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)
      assert {:ok, pages} = Pagination.paginate(layout)
      assert length(pages) == count
      assert Enum.any?(hd(pages).boxes, &(Map.get(&1, :text) == "After oversized atomic table"))

      if count == 2 do
        assert Enum.any?(List.last(pages).boxes, &(Map.get(&1, :text) == "Third row"))
      end

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "After oversized atomic table"
    end
  end

  test "atomic overflow keeps its source page among unrelated physical pages" do
    source =
      File.read!("test/fixtures/html_to_pdf/browser_parity/atomic_table_visible_overflow.html")
      |> String.replace("<body>", "<body><p style='break-after:page'>Prefix</p>")
      |> String.replace("</body>", "<p style='break-before:page'>Suffix</p></body>")

    assert {:ok, pdf} = HtmlToPdf.render(source, system_font_discovery: false)
    assert {:ok, pages} = NativeElixirPdfUtilities.Split.by_page(pdf)
    assert {:ok, first} = Text.extract(hd(pages))
    assert first =~ "Prefix"
    refute first =~ "Oversized"
    assert {:ok, second} = Text.extract(Enum.at(pages, 1))
    assert second =~ "Oversized atomic table" and second =~ "After oversized atomic table"
    assert {:ok, all} = Text.extract(pdf)
    assert all =~ "Third row" and all =~ "Suffix"
  end
end
