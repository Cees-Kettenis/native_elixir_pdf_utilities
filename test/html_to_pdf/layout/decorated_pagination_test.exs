defmodule NativeElixirPdfUtilities.HtmlToPdf.DecoratedPaginationTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Pagination, PdfWriter, Style}

  test "decorated visual lines consume available page capacity and slice edge paints" do
    html =
      File.read!("test/fixtures/html_to_pdf/browser_parity/html_decorated_block_fragments.html")

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {210, 180}, margin: 12)
    assert {:ok, pages} = Pagination.paginate(layout)

    destinations =
      for page <- pages,
          box <- page.boxes,
          Map.has_key?(box, :link_destination),
          do: box.link_destination

    assert [{:xyz, 1, _x, target_y}] = destinations
    assert_in_delta target_y, 114.0, 0.001
    assert {:ok, _pdf} = PdfWriter.render(pages)
  end

  test "clipped early target copies resolve to their later visible source position" do
    html =
      File.read!("test/fixtures/html_to_pdf/browser_parity/html_decorated_block_fragments.html")

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {210, 180}, margin: 12)

    boxes =
      Enum.map(layout.boxes, fn box ->
        case Map.get(box, :html_targets) do
          nil -> box
          targets -> Map.put(box, :html_targets, Enum.map(targets, &%{&1 | dy: &1.dy - 150}))
        end
      end)

    assert {:ok, pages} = Pagination.paginate(%{layout | boxes: boxes})

    destinations =
      for page <- pages,
          box <- page.boxes,
          Map.has_key?(box, :link_destination),
          do: box.link_destination

    assert [{:xyz, 2, _x, target_y}] = destinations
    assert target_y >= 12 and target_y <= 168
    assert {:ok, _} = PdfWriter.render(pages)
  end

  test "outside ordered-list markers retain their first line and appear once across decorated fragments" do
    html =
      File.read!("test/fixtures/html_to_pdf/browser_parity/html_decorated_list_fragments.html")

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {210, 180}, margin: 12)
    assert {:ok, pages} = Pagination.paginate(layout)
    boxes = Enum.flat_map(pages, & &1.boxes)
    assert Enum.count(boxes, &(Map.get(&1, :text) == "7.")) == 1
    assert Enum.count(boxes, &(Map.get(&1, :text) == "8.")) == 1
    first_line = Enum.find(hd(pages).boxes, &(Map.get(&1, :text) == "Numbered line 1"))
    marker = Enum.find(hd(pages).boxes, &(Map.get(&1, :text) == "7."))
    assert marker != nil and first_line != nil
    assert marker.fragment_id == first_line.fragment_id
    assert marker.fieldset_ancestors == first_line.fieldset_ancestors

    item_border =
      hd(pages).boxes
      |> Enum.filter(&(&1.type == :rect and Map.get(&1, :role) == :block_fragment_paint))
      |> Enum.max_by(& &1.x)

    assert marker.x + marker.width < item_border.x
    assert {:ok, _pdf} = PdfWriter.render(pages)
  end

  test "outside markers select the first flowing line when positioned descendants precede it" do
    html =
      "<style>body{margin:0;font-size:10pt;line-height:12pt}ol{margin:0;padding-left:24pt}li{position:relative;border:1pt solid blue;padding:3pt}</style><ol start=4><li><span style='position:absolute;top:30pt'>Positioned</span>First flow line<br>Second flow line<br>Third flow line</li></ol>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {210, 80}, margin: 12)
    marker = Enum.find(layout.boxes, &(Map.get(&1, :text) == "4."))
    first = Enum.find(layout.boxes, &(Map.get(&1, :text) == "First flow line"))
    assert marker.y == first.y
    assert marker.fragment_id == first.fragment_id
    assert {:ok, pages} = Pagination.paginate(layout)
    assert {:ok, _} = PdfWriter.render(pages)
  end

  test "fully clipped decoration retains page order and recoverable writer geometry" do
    html =
      File.read!("test/fixtures/html_to_pdf/browser_parity/html_decorated_block_fragments.html")

    html = String.replace(html, "href=\"#decorated\"", "")
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {210, 180}, margin: 12)

    boxes =
      Enum.map(layout.boxes, fn box ->
        if Map.has_key?(box, :fieldset_paint_id),
          do:
            Map.put(box, :clip, %{
              x: box.x,
              y: box.y,
              width: 0,
              height: box.height,
              border_radius: 0.0
            }),
          else: box
      end)

    assert {:ok, pages} = Pagination.paginate(%{layout | boxes: boxes})
    assert length(pages) == 3
    assert {:ok, _} = PdfWriter.render(pages)
  end

  test "caption fragmentation uses the available capacity independently of table rows" do
    html =
      File.read!("test/fixtures/html_to_pdf/browser_parity/html_table_caption_fragments.html")

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {210, 180}, margin: 12)
    assert {:ok, pages} = Pagination.paginate(layout)
    texts = Enum.map(pages, fn page -> Enum.map(page.boxes, &Map.get(&1, :text)) end)
    assert length(pages) == 2
    assert "Top caption" in hd(texts)
    refute "First captioned row" in hd(texts)
    assert "First captioned row" in List.last(texts)
    assert Enum.any?(texts, &("Bottom caption" in &1 and "Final captioned row" in &1))
    assert {:ok, _pdf} = PdfWriter.render(pages)
  end

  test "oversized block captions preserve every line without repeating table headings" do
    caption = Enum.map_join(1..18, "<br>", &"Caption line #{&1}")

    html =
      "<style>body{margin:0;font-size:10pt;line-height:12pt}table{border-collapse:collapse;width:180pt}caption{padding:3pt;border:1pt solid blue}td{padding:0}</style><table><caption>#{caption}</caption><thead><tr><td>Actual heading</td></tr></thead><tbody><tr><td>Body row</td></tr></tbody></table>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {200, 100}, margin: 10)
    assert {:ok, pages} = Pagination.paginate(layout)
    assert length(pages) >= 3
    texts = for page <- pages, box <- page.boxes, is_binary(Map.get(box, :text)), do: box.text
    for index <- 1..18, do: assert(Enum.count(texts, &(&1 == "Caption line #{index}")) == 1)
    assert Enum.count(texts, &(&1 == "Actual heading")) == 1
    assert Enum.count(texts, &(&1 == "Body row")) == 1
    assert {:ok, _} = PdfWriter.render(pages)
  end

  test "nested background images retain source tile offsets and clips on every physical page" do
    html =
      File.read!(
        "test/fixtures/html_to_pdf/browser_parity/html_decorated_background_fragments.html"
      )

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {210, 180}, margin: 12)
    assert {:ok, pages} = Pagination.paginate(layout)
    assert length(pages) == 3

    for page <- pages do
      paints = Enum.filter(page.boxes, &Map.has_key?(&1, :fieldset_paint_id))
      assert length(Enum.uniq_by(paints, & &1.fieldset_paint_id)) == 2
      images = Enum.filter(paints, &(&1.type == :image))
      assert images != []
      assert Enum.all?(images, &(&1.clip.y >= 12 and &1.clip.y + &1.clip.height <= 168))
    end

    assert {:ok, _} = PdfWriter.render(pages)
  end

  test "forged caption context fails through the canonical validator with actionable diagnostics" do
    for extra <- [
          %{table_caption_side: :left, table_id: :table},
          %{table_caption_side: nil, table_id: :table},
          %{table_caption_side: :top}
        ] do
      box = Map.merge(%{type: :flow_marker, x: 0, y: 20, width: 0, height: 0}, extra)

      assert {:error, {:invalid_layout, diagnostic}} =
               Pagination.paginate(%{type: :layout, page_size: {100, 100}, boxes: [box]})

      assert diagnostic.stage == :pagination
      assert diagnostic.reason == :invalid_layout
      assert diagnostic.operation == :paginate
      assert diagnostic.message =~ "captions require"
    end
  end
end
