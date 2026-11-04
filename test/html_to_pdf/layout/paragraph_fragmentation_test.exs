defmodule NativeElixirPdfUtilities.HtmlToPdf.ParagraphFragmentationTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Pagination, Style}

  test "short paragraphs keep their default two lines while explicit one permits splitting" do
    for {counts, expected} <- [{"", [0, 2]}, {"orphans:1;widows:1", [1, 1]}] do
      pages = paragraph_pages("<div style=height:88pt>Before</div>", 2, counts, 100)
      assert Enum.map(pages, &line_count/1) == expected
    end
  end

  test "oversized paragraphs honor fitting first and final reservations across pages" do
    assert Enum.map(paragraph_pages("", 10, "", 50), &line_count/1) == [4, 4, 2]
    assert Enum.map(paragraph_pages("", 10, "widows:3", 50), &line_count/1) == [4, 3, 3]
    pages = paragraph_pages("<div style=height:36pt>Before</div>", 8, "orphans:3", 60)
    assert Enum.map(pages, &line_count/1) == [0, 5, 3]

    assert Enum.map(paragraph_pages("", 10, "orphans:100;widows:100", 50), &line_count/1) == [
             4,
             4,
             2
           ]
  end

  test "counts inherit and authored initial inherit unset resolve in the original cascade" do
    html =
      "<div style='orphans:3;widows:4'><p style='orphans:9;orphans:inherit;widows:initial'>Text</p><p style='orphans:unset'>Other</p></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [div]}} = Style.compute_detailed(dom)
    [first, second] = div.children
    assert first.style.orphans == 3 and first.style.widows == 2
    assert second.style.orphans == 3 and second.style.widows == 4
    refute Map.has_key?(first.style, :_inherited_orphans)
    assert {:ok, _} = HtmlToPdf.render("<p style='orphans:+003;widows:2'>Count</p>")
  end

  test "forced block breaks precede the first line and do not repeat on each fragment" do
    pages = paragraph_pages("<div>Before</div>", 2, "break-before:page;orphans:3;widows:3", 100)
    assert length(pages) == 2 and Enum.map(pages, &line_count/1) == [0, 2]
  end

  test "invalid CSS and forged style or line metadata return shared diagnostics" do
    for property <- ["orphans", "widows"], value <- ["0", "-1", "2.0", "2px", "2suffix"] do
      assert {:error, {:invalid_css, diagnostic}} =
               HtmlToPdf.render("<p style='#{property}:#{value}'>Text</p>")

      assert diagnostic.stage == :css and diagnostic.message != ""
    end

    assert {:ok, dom} = HtmlParser.parse_detailed("<p>Text</p>")
    assert {:ok, %{children: [node]} = styled} = Style.compute_detailed(dom)

    for forged <- [
          put_in(node.style.orphans, 0),
          put_in(node.style.widows, "2"),
          put_in(node.style[:_root_body_fragmentation], true)
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               Layout.layout(%{styled | children: [forged]})

      assert diagnostic.stage == :layout and diagnostic.message != ""
    end

    assert {:ok, layout} = Layout.layout(styled)
    box = hd(layout.boxes)

    assert {:error, {:invalid_layout, diagnostic}} =
             Pagination.paginate(%{layout | boxes: [Map.put(box, :widows, 0)]})

    assert diagnostic.stage == :pagination and diagnostic.source == "orphans/widows"
  end

  defp paragraph_pages(prefix, lines, counts, height) do
    content = Enum.map_join(1..lines, "<br>", &"Line#{&1}")

    html =
      "<style>body{margin:0;font-size:10pt;line-height:12pt}p{margin:0}</style><body>#{prefix}<p style='#{counts}'>#{content}</p></body>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {200, height}, margin: 0)
    assert {:ok, pages} = Pagination.paginate(layout)
    pages
  end

  defp line_count(page),
    do: Enum.count(page.boxes, &String.starts_with?(Map.get(&1, :text, ""), "Line"))
end
