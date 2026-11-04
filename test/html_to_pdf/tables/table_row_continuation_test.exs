defmodule NativeElixirPdfUtilities.HtmlToPdf.TableRowContinuationTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    HtmlParser,
    Style,
    Layout,
    Pagination,
    DrawingGeometry,
    PageGeometry
  }

  alias NativeElixirPdfUtilities.Validators.HtmlTableValidator

  defp layout(html, size \\ {240, 160}) do
    html =
      "<style>html,body{margin:0}body{font-family:'DejaVu Sans';font-size:9pt;line-height:12pt}table{width:216pt;table-layout:fixed;border-collapse:collapse}td,th{padding:3pt;border:1px solid #789;vertical-align:top}th{font-weight:normal}p{margin:0}</style>" <>
        html

    {:ok, dom} = HtmlParser.parse_document_detailed(html)
    {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    {:ok, tree} = Layout.layout(styled, page_size: size, margin: 12)
    tree
  end

  defp artwork(boxes, dy \\ 0.0) do
    Enum.flat_map(boxes, fn box ->
      case box do
        %{type: :drawing_group, boxes: children, transform: {_a, _b, _c, _d, _e, f}} ->
          artwork(children, dy + f)

        %{type: :glyph_group, boxes: children} ->
          artwork(children, dy)

        _ ->
          [DrawingGeometry.translate_box(box, 0.0, dy)]
      end
    end)
  end

  test "an atomic cell taller than a whole page remains intact on its first page" do
    html =
      "<style>@page{size:100pt 80pt;margin:10pt}html,body{margin:0}p{margin:0}</style><table><tr><td><div style='display:inline-block;width:20pt;height:200pt;background:blue'></div></td></tr></table><p>After</p>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, system_font_discovery: false)

    assert {:ok, %{pages: [first, second]}} = NativeElixirPdfUtilities.Text.extract_spans(pdf)
    assert first.spans == []
    assert Enum.map_join(second.spans, & &1.text) == "After"
  end

  test "spanned math and discretionary text keep their selected glyphs through fragment alignment" do
    html =
      "<table style='width:80pt'><tr><td rowspan=2><math><mi>x</mi></math>wa&shy;ter</td><td>One</td></tr><tr><td>Two</td></tr></table>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, system_font_discovery: false)

    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
    assert text =~ "x" and text =~ "wa\u00ADter"
    assert text =~ "One" and text =~ "Two"
  end

  test "oversized ordinary row consumes remaining capacity and repeats headers" do
    lines = Enum.map_join(1..20, "<br>", &"Line#{&1}")

    tree =
      layout(
        "<p>Before</p><table><thead><tr><th>Heading</th><th>Peer</th></tr></thead><tbody><tr><td>#{lines}</td><td>Short</td></tr></tbody></table><p>After</p>"
      )

    assert {:ok, pages} = Pagination.paginate(tree)
    assert length(pages) == 3
    texts = Enum.flat_map(pages, &artwork(&1.boxes)) |> Enum.filter(&(&1.type == :text))
    for index <- 1..20, do: assert(Enum.count(texts, &(&1.text == "Line#{index}")) == 1)
    assert Enum.count(texts, &(&1.text == "Heading")) == 3
    assert Enum.count(texts, &(&1.text == "Short")) == 1
    assert Enum.count(texts, &(&1.text == "After")) == 1

    for page <- pages, text <- artwork(page.boxes), text.type == :text do
      {top, bottom} = PageGeometry.box_vertical_bounds(text)
      assert top <= 148.00001
      assert bottom >= 11.99999
    end
  end

  test "long spanning text moves whole lines and resolves a later source target" do
    lines = Enum.map_join(1..15, "<br>", &"Line#{&1}") <> "<br><span id='later'>Line16</span>"

    tree =
      layout(
        "<table><tr><td rowspan='4'>#{lines}</td><td>First</td></tr><tr><td>Second</td></tr><tr><td>Third</td></tr><tr><td>Fourth</td></tr></table><p><a href='#later'>Jump</a></p>"
      )

    assert {:ok, pages} = Pagination.paginate(tree)
    paints = Enum.flat_map(pages, &artwork(&1.boxes))

    for index <- 1..16,
        do: assert(Enum.count(paints, &(Map.get(&1, :text) == "Line#{index}")) == 1)

    assert {:xyz, 2, _x, _y} = Enum.find(paints, &(Map.get(&1, :text) == "Jump")).link_destination

    for page <- pages, text <- artwork(page.boxes), Map.get(text, :text, "") =~ "Line" do
      {_top, bottom} = PageGeometry.box_vertical_bounds(text)
      assert bottom >= 11.99999
    end
  end

  test "a fitting avoid row moves intact even while its spanning source text continues" do
    lines = Enum.map_join(1..16, "<br>", &"Line#{&1}")

    tree =
      layout(
        "<table><tr><td rowspan='4'>#{lines}</td><td>First</td></tr><tr><td>Second</td></tr><tr style='break-inside:avoid'><td>Third</td></tr><tr><td>Fourth</td></tr></table>"
      )

    assert {:ok, [first, second]} = Pagination.paginate(tree)
    refute Enum.any?(artwork(first.boxes), &(Map.get(&1, :text) == "Third"))
    assert Enum.any?(artwork(second.boxes), &(Map.get(&1, :text) == "Third"))
  end

  test "whole interactive fields retain page-level hosts and appearances after a row cut" do
    before = Enum.map_join(1..10, "<br>", &"Before#{&1}")
    after_field = Enum.map_join(1..10, "<br>", &"After#{&1}")

    tree =
      layout(
        "<table><tbody><tr><td>#{before}<br><input name='field' value='Value' style='width:70pt;height:18pt;font-family:DejaVu Sans;font-size:9pt'><br>#{after_field}</td><td>Peer</td></tr></tbody></table>"
      )

    assert {:ok, pages} = Pagination.paginate(tree)

    hosts =
      for {page, number} <- Enum.with_index(pages, 1),
          box <- page.boxes,
          is_map(Map.get(box, :form_control)),
          do: {number, box}

    assert [{2, host}] = hosts
    assert host.y >= 12.0
    assert host.y + host.height <= 148.0
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.PdfWriter.render(pages)
    assert {:ok, [field]} = NativeElixirPdfUtilities.Forms.fields(pdf)
    assert field.name == "field"
    assert field.value == "Value"
    assert [%{page: 2, rect: [left, bottom, right, top]}] = field.widgets
    assert left >= 12.0 and right <= 228.0
    assert bottom >= 12.0 and top <= 148.0
    assert {:ok, flattened} = NativeElixirPdfUtilities.Forms.flatten(pdf)
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(flattened)
    assert text =~ "Value"
  end

  test "an oversized row carries a spanning cell through the same physical fragments" do
    span = Enum.map_join(1..24, "<br>", &"Span#{&1}")
    peer = Enum.map_join(1..20, "<br>", &"Peer#{&1}")

    tree =
      layout(
        "<table><tbody><tr><td rowspan='2'>#{span}</td><td>#{peer}</td></tr><tr><td>Last</td></tr></tbody></table>"
      )

    assert {:ok, pages} = Pagination.paginate(tree)
    paints = Enum.flat_map(pages, &artwork(&1.boxes))

    for {prefix, maximum} <- [{"Span", 24}, {"Peer", 20}], index <- 1..maximum do
      assert Enum.count(paints, &(Map.get(&1, :text) == "#{prefix}#{index}")) == 1
    end
  end

  test "canonical row cut moves an intersected atomic artwork and keeps positioned paint" do
    rect = %{
      type: :rect,
      x: 0.0,
      y: 0.0,
      width: 80.0,
      height: 100.0,
      fill_color: {1, 1, 1},
      stroke_width: 0.0,
      border_radius: 0.0,
      stroke_color: nil,
      role: :table_cell_background
    }

    image = %{
      type: :rect,
      x: 10.0,
      y: 40.0,
      width: 20.0,
      height: 30.0,
      fill_color: {1, 0, 0},
      stroke_width: 0.0,
      border_radius: 0.0,
      stroke_color: nil
    }

    positioned = %{image | y: 30.0} |> Map.put(:out_of_flow, true)

    assert {:ok, [fragment], 30.0} =
             HtmlTableValidator.prepare_row_page_fragment(
               [rect, image, positioned],
               100.0,
               0.0,
               50.0
             )

    assert fragment.y == 50.0
    assert Enum.all?(fragment.boxes, &(Map.get(&1, :role) == :table_cell_background))

    assert {:ok, [later], 70.0} =
             HtmlTableValidator.prepare_row_page_fragment(
               [rect, image, positioned],
               70.0,
               0.0,
               100.0
             )

    assert Enum.any?(later.boxes, &Map.get(&1, :out_of_flow, false))
  end

  test "the first collapsed-border row moving after a fitting caption preserves its physical half-border inset" do
    html =
      "<style>@page{size:210pt 180pt;margin:12pt}html,body{margin:0;padding:0;font-family:DejaVu Sans;font-size:10.5pt;line-height:13.5pt}table{border-collapse:collapse;width:186pt}caption{text-align:left;padding:3pt;border:1.5pt solid #315b80;background:#e6edf3}td{padding:3pt;border:1.5pt solid #315b80}</style><div style='height:117pt'>Before top caption</div><table><caption>Top caption</caption><tbody><tr><td>First captioned row</td></tr><tr><td>Second captioned row</td></tr></tbody></table>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)

    assert {:ok, %{pages: [_first, second], page_count: 2}} =
             NativeElixirPdfUtilities.Text.extract_spans(pdf)

    assert String.starts_with?(Enum.map_join(second.spans, & &1.text), "First captioned row")
    row = hd(second.spans)
    assert row.paints_text? and row.render_mode == 0
    assert_in_delta row.x, 16.5, 0.0001
    assert_in_delta row.y, 26.9999, 0.0001
  end

  test "invalid direct row continuation intent retains the canonical operation and source" do
    for {boxes, delta, bottom} <- [
          {[], :bad, 12},
          {[1], 0, 12},
          {[], 0, -1},
          {[:bad | :tail], 0, 12}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               HtmlTableValidator.prepare_row_continuation_intent(boxes, delta, bottom)

      assert diagnostic.stage == :pagination
      assert diagnostic.operation == :prepare_row_continuation_intent
      assert diagnostic.module == HtmlTableValidator
      assert diagnostic.source == "table row fragment"
      assert diagnostic.message =~ "actual page capacity"
    end
  end

  test "invalid direct row fragment geometry uses shared actionable diagnostics" do
    for {boxes, top, bottom, capacity} <- [
          {[], :bad, 0, 10},
          {[], 0, 10, 2},
          {[], 10, 0, -1},
          {[1], 10, 0, 4},
          {[:bad | :tail], 10, 0, 4}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               HtmlTableValidator.prepare_row_page_fragment(boxes, top, bottom, capacity)

      assert diagnostic.stage == :pagination or diagnostic.stage == :pdf
      assert diagnostic.reason == :invalid_layout
      assert is_binary(diagnostic.message)
    end
  end
end
