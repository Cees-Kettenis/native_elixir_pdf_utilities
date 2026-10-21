defmodule NativeElixirPdfUtilities.HtmlToPdf.AutomaticTableSizingTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Pagination, Style}
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}

  @css "<style>html,body{margin:0;font-family:'DejaVu Sans';font-size:12px}table{border-collapse:collapse}td{padding:0;border:0;background:white}p{margin:0}</style>"

  test "fixed-size block children contribute proportionally to automatic columns" do
    cells =
      layout_cells(
        "<tr><td><div style='width:20pt;height:10pt'></div></td><td><div style='width:40pt;height:10pt'></div></td></tr>"
      )

    assert_in_delta Enum.at(cells, 0).width, 60, 0.00001
    assert_in_delta Enum.at(cells, 1).width, 120, 0.00001
  end

  test "nested child padding borders and margins contribute to intrinsic width" do
    cells =
      layout_cells(
        "<tr><td><section><div style='width:40pt;height:10pt;padding:5pt;border:3pt solid;margin:0 10pt'></div></section></td><td><div style='width:38pt;height:10pt'></div></td></tr>"
      )

    assert_in_delta Enum.at(cells, 0).width, 120, 0.00001
    assert_in_delta Enum.at(cells, 1).width, 60, 0.00001
  end

  test "multiple stacked blocks use the widest child rather than their combined width" do
    cells =
      layout_cells(
        "<tr><td><div style='width:40pt;height:10pt'></div><div style='width:60pt;height:10pt'></div></td><td><div style='width:30pt;height:10pt'></div></td></tr>"
      )

    assert_in_delta Enum.at(cells, 0).width, 120, 0.00001
    assert_in_delta Enum.at(cells, 1).width, 60, 0.00001
  end

  test "block text retains the same preferred widths as inline text" do
    [block, peer] =
      layout_cells(
        "<tr><td><p>Two words and more</p></td><td><div style='width:40pt;height:10pt'></div></td></tr>"
      )

    [inline, inline_peer] =
      layout_cells(
        "<tr><td>Two words and more</td><td><div style='width:40pt;height:10pt'></div></td></tr>"
      )

    assert_in_delta block.width, inline.width, 0.02
    assert_in_delta peer.width, inline_peer.width, 0.02
  end

  test "block minimums expand a narrow table rather than overlap adjacent columns" do
    [first, second] =
      layout_cells(
        "<tr><td><div style='width:120pt;height:10pt'></div></td><td><div style='width:60pt;height:10pt'></div></td></tr>",
        "width:90pt"
      )

    assert_in_delta first.width, 120, 0.00001
    assert_in_delta second.width, 60, 0.00001
    assert_in_delta second.x, first.x + first.width, 0.00001
  end

  test "spanning block minimums constrain every covered column before surplus distribution" do
    rows =
      "<tr><td colspan=2><div style='width:120pt;height:10pt'></div></td><td><div style='width:60pt;height:10pt'></div></td></tr><tr><td><div style='width:30pt;height:10pt'></div></td><td><div style='width:30pt;height:10pt'></div></td><td></td></tr>"

    for width <- ["width:180pt", "width:90pt"] do
      [span, peer, first, second, _last] = layout_cells(rows, width)
      assert_in_delta span.width, 120, 0.00001
      assert_in_delta peer.width, 60, 0.00001
      assert_in_delta first.width, 60, 0.00001
      assert_in_delta second.width, 60, 0.00001
    end
  end

  test "spanning block widths populate otherwise empty tracks" do
    [span, peer, first, second, _last] =
      layout_cells(
        "<tr><td colspan=2><div style='width:120pt;height:10pt'></div></td><td><div style='width:60pt;height:10pt'></div></td></tr><tr style='height:1pt'><td></td><td></td><td></td></tr>"
      )

    assert_in_delta span.width, 120, 0.00001
    assert_in_delta peer.width, 60, 0.00001
    assert_in_delta first.width, 60, 0.00001
    assert_in_delta second.width, 60, 0.00001
  end

  test "a spanning cell that already fits preserves percentage column preferences" do
    rows =
      "<tr><td style='width:10%'>LONGWORD</td><td style='width:20%'>Text</td><td style='width:70%'>Other words</td></tr>"

    original = layout_cells(rows)
    with_span = layout_cells(rows <> "<tr><td colspan=2>A</td><td></td></tr>")
    assert Enum.map(original, & &1.width) == Enum.map(Enum.take(with_span, 3), & &1.width)
  end

  test "percentage columns reserve their share without adding cell decorations twice" do
    [percentage, block] =
      layout_cells(
        "<tr><td style='width:30%;padding:3pt'><div style='width:10pt;height:10pt'></div></td><td><div style='width:40pt;height:10pt'></div></td></tr>"
      )

    assert_in_delta percentage.width, 54, 0.00001
    assert_in_delta block.width, 126, 0.00001
  end

  test "hidden block children do not contribute width" do
    [first, second] =
      layout_cells(
        "<tr><td><div style='display:none;width:200pt'></div><div style='width:20pt;height:10pt'></div></td><td><div style='width:40pt;height:10pt'></div></td></tr>"
      )

    assert_in_delta first.width, 60, 0.00001
    assert_in_delta second.width, 120, 0.00001
  end

  test "an empty block column has zero intrinsic width beside nonempty content" do
    cells =
      layout_cells(
        "<tr><td><div></div></td><td><div style='width:40pt;height:10pt'></div></td></tr>"
      )

    # A zero-width background does not paint; all available width goes to its peer.
    assert [peer] = cells
    assert_in_delta peer.width, 180, 0.00001
  end

  test "fixed layout still divides unassigned columns independently of their contents" do
    [first, second] =
      layout_cells(
        "<tr><td><div style='width:20pt;height:10pt'></div></td><td><div style='width:40pt;height:10pt'></div></td></tr>",
        "width:180pt;table-layout:fixed"
      )

    assert_in_delta first.width, 90, 0.00001
    assert_in_delta second.width, 90, 0.00001
  end

  test "automatic column allocation remains stable across repeated page headings" do
    html = File.read!(Paths.fixture("html_to_pdf/browser_parity/auto_table_pagination.html"))
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {200, 120}, margin: 12)
    assert {:ok, pages} = Pagination.paginate(layout)
    assert length(pages) > 1

    for page <- pages do
      labels = Enum.filter(page.boxes, &(Map.get(&1, :text) == "Label"))
      assert length(labels) == 1

      blocks =
        Enum.filter(page.boxes, &(Map.get(&1, :fill_color) == {23 / 255, 74 / 255, 134 / 255}))

      assert blocks != []
      assert Enum.all?(blocks, &(&1.width == 54))
    end
  end

  for scenario <-
        ~w(block_width block_margins spanning_block spanning_overflow spanning_spacing wrapping percentage rowspan pagination) do
    @scenario scenario
    @tag :browser_parity
    test "automatic table #{@scenario} matches Chromium" do
      stats =
        PdfVisualCompare.assert_browser_match!(
          Paths.fixture("html_to_pdf/browser_parity/auto_table_#{@scenario}.html"),
          artifact_dir: "tmp/browser_parity/auto_table_#{@scenario}",
          render_opts: [system_font_discovery: false],
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count >= 1
      assert stats.scripting == :disabled
    end
  end

  defp layout_cells(rows, table_style \\ "width:180pt") do
    assert {:ok, dom} =
             HtmlParser.parse_document_detailed(
               @css <> "<table style='#{table_style}'>#{rows}</table>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 300}, margin: 0)
    Enum.filter(layout.boxes, &(Map.get(&1, :role) == :table_cell_background))
  end
end
