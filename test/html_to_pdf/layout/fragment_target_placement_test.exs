defmodule NativeElixirPdfUtilities.HtmlToPdf.FragmentTargetPlacementTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}
  alias NativeElixirPdfUtilities.Validators.HtmlLinkValidator

  test "a target on discretionary text keeps its authored origin after glyph grouping" do
    html =
      "<style>html,body{margin:0;font-family:DejaVu Sans;font-size:12pt;line-height:15pt}p{margin:0;width:25pt}</style><p><span id='word'>wa&shy;ter</span></p><a href='#word'>Jump</a>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {200, 200})
    group = Enum.find(layout.boxes, &(&1.type == :glyph_group))
    assert group.actual_text =~ "wa\u00AD"
    [target] = group.html_targets
    assert {:ok, [page]} = NativeElixirPdfUtilities.HtmlToPdf.Pagination.paginate(layout)
    link = Enum.find(page.boxes, &Map.has_key?(&1, :link_destination))
    assert {:xyz, 1, x, y} = link.link_destination
    assert_in_delta x, group.x + target.dx, 0.000001
    assert_in_delta y, group.y + target.dy, 0.000001
  end

  test "the same selected DOM target binds to its first visible physical fragment" do
    html =
      "<style>html,body{margin:0}p{margin:0}</style><p><a href='#delayed'>Jump</a></p><p id='delayed'>Target</p>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {200, 100}, margin: 10)
    link = Enum.find(layout.boxes, &Map.has_key?(&1, :link_destination))
    target = Enum.find(layout.boxes, &(Map.get(&1, :html_targets, []) != []))
    [anchor] = target.html_targets
    point = {target.x + anchor.dx, target.y + anchor.dy}

    group = fn dy ->
      %{
        type: :drawing_group,
        x: 0,
        y: dy,
        width: 200,
        height: 100,
        baseline: 0,
        transform: {1, 0, 0, 1, 0, dy},
        clip: %{x: 0, y: 0, width: 200, height: 100},
        boxes: [target]
      }
    end

    pages = [
      %{size: {200, 100}, boxes: [link, group.(100)]},
      %{size: {200, 100}, boxes: [group.(0)]},
      %{size: {200, 100}, boxes: [group.(5)]}
    ]

    for page <- pages do
      assert :ok = HtmlLinkValidator.validate_boxes(page.boxes, layout.link_scope)
    end

    assert {:ok, [first | _]} =
             HtmlLinkValidator.resolve_pages(
               pages,
               layout.link_scope,
               layout.link_expected_targets
             )

    resolved = Enum.find(first.boxes, &Map.has_key?(&1, :link_destination))
    {x, y} = point
    assert resolved.link_destination == {:xyz, 2, x, y}

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlLinkValidator.resolve_pages(
               [hd(pages)],
               layout.link_scope,
               layout.link_expected_targets
             )

    assert diagnostic.stage == :pagination
    assert diagnostic.operation == :paginate
    assert diagnostic.source == "delayed"
    assert diagnostic.message =~ "no representable destination"
  end

  @tag :fragment_group_targets
  test "table row and group targets use their local clip and source origin" do
    html =
      "<style>html,body{margin:0}table{width:120pt;border-collapse:collapse}td{height:20pt;padding:2pt;border:1pt solid}tbody{background:#eee}p{margin:0}</style><table><tbody id='group'><tr id='row'><td rowspan='2' id='cell'>Span</td><td>First</td></tr><tr><td>Second</td></tr></tbody></table><p><a href='#group'>Group</a> <a href='#row'>Row</a> <a href='#cell'>Cell</a></p>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {160, 140}, margin: 10)

    origin =
      Enum.find(
        layout.boxes,
        &(Map.get(&1, :role) == :table_row_group_background and
            Map.get(&1, :table_span_fragment, %{})[:offset] == 0)
      )

    source = hd(origin.boxes)
    point = {origin.x + source.x, origin.y + source.y + source.height}
    assert {:ok, [page]} = NativeElixirPdfUtilities.HtmlToPdf.Pagination.paginate(layout)
    links = Enum.filter(page.boxes, &Map.has_key?(&1, :link_destination))
    assert length(links) == 3

    for link <- links do
      assert {:xyz, 1, x, y} = link.link_destination
      assert_in_delta x, elem(point, 0), 0.000001
      assert_in_delta y, elem(point, 1), 0.000001
    end
  end
end
