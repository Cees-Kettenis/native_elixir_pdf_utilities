alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.TableCellPageFragmentTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    HtmlParser,
    Style,
    Layout,
    Pagination,
    DrawingGeometry
  }

  alias NativeElixirPdfUtilities.Validators.HtmlTableValidator
  alias NativeElixirPdfUtilities.Limits

  @fixture Paths.fixture("html_to_pdf/browser_parity/html_table_span_layered_backgrounds.html")

  setup do
    {:ok, dom} = HtmlParser.parse_document_detailed(File.read!(@fixture))
    {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)
    {:ok, layout: layout}
  end

  defp artwork(boxes) do
    Enum.flat_map(boxes, fn box ->
      case box do
        %{boxes: children} -> [box | artwork(children)]
        _ -> [box]
      end
    end)
  end

  test "page consumption preserves source text exactly once and repeats headers", %{
    layout: layout
  } do
    assert {:ok, pages} = Pagination.paginate(layout)
    assert length(pages) == 2
    boxes = Enum.flat_map(pages, &artwork(&1.boxes))
    texts = Enum.map(boxes, &Map.get(&1, :text))

    for text <- ["One", "Two", "Three", "Four", "Five", "Six"] do
      assert Enum.count(texts, &(&1 == text)) == 1
    end

    assert Enum.count(texts, &(&1 == "Heading")) == 2
    link = Enum.find(boxes, &(Map.get(&1, :text) == "Six"))
    assert {:xyz, 1, _x, _y} = link.link_destination

    surfaces =
      Enum.filter(
        boxes,
        &(Map.get(&1, :role) == :table_cell_background and Map.has_key?(&1, :table_span_fragment))
      )

    assert length(surfaces) == 2
    [first, second] = surfaces
    assert first.table_span_fragment.identity == second.table_span_fragment.identity
    assert first.table_span_fragment.source == second.table_span_fragment.source
    assert first.table_cell == second.table_cell
    assert_in_delta first.y, layout.margins.bottom, 0.000001
    assert_in_delta first.height + second.height, first.table_span_fragment.total_height, 0.000001
  end

  test "a target in later span content retains one visible source paint and its final destination" do
    lines = Enum.map_join(1..15, "<br>", &"Line#{&1}") <> "<br><span id='later'>Line16</span>"

    html =
      "<style>html,body{margin:0}body{font-family:'DejaVu Sans';font-size:9pt;line-height:12pt}table{width:216pt;table-layout:fixed;border-collapse:collapse}td{height:36pt;padding:3pt;border:1px solid #789;vertical-align:top}p{margin:0}</style><table><tr><td rowspan='4'>#{lines}</td><td>First</td></tr><tr><td>Second</td></tr><tr><td>Third</td></tr><tr><td>Fourth</td></tr></table><p><a href='#later'>Jump</a></p>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)
    assert {:ok, pages} = Pagination.paginate(layout)
    assert length(pages) == 2
    paints = Enum.flat_map(pages, &artwork(&1.boxes))
    assert Enum.count(paints, &(Map.get(&1, :text) == "Line16")) == 1
    link = Enum.find(paints, &(Map.get(&1, :text) == "Jump"))
    assert {:xyz, 2, x, y} = link.link_destination

    first_band =
      Enum.filter(layout.boxes, fn box ->
        case Map.get(box, :table_span_fragment) do
          %{offset: offset} when offset == 0 -> Map.get(box, :role) != :table_border
          _ -> false
        end
      end)

    assert {:ok, first_paint, _state} =
             HtmlTableValidator.prepare_cell_page_fragments(first_band, %{}, 12.0)

    refute Enum.any?(
             artwork(first_paint),
             &(Map.get(&1, :html_targets, []) != [])
           )

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.PdfWriter.render(pages)
    assert {:ok, %{pages: [first, second]}} = NativeElixirPdfUtilities.Text.extract_spans(pdf)
    refute Enum.any?(first.spans, &(&1.text == "Line16"))
    assert [target] = Enum.filter(second.spans, &(&1.text == "Line16"))
    assert target.paints_text? and target.render_mode == 0
    assert target.font_size == 12.0
    [a, b, _c, _d, _x, _y] = target.ctm
    assert_in_delta target.font_size * :math.sqrt(a * a + b * b), 9.0, 0.0001
    assert_in_delta target.x, x, 0.0001
    assert y >= 160.0 - target.y and y <= 148.0
    assert target.y >= 12.0

    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)
    source_font = Enum.find(paints, &(Map.get(&1, :text) == "Line16")).font_face
    assert source_font.family == "DejaVu Sans"

    glyph_ids =
      for %{type: :text, text: text, font_face: ^source_font} <- paints,
          codepoint <- String.to_charlist(text),
          do: Map.fetch!(source_font.cmap, codepoint)

    assert {:ok, subset} =
             NativeElixirPdfUtilities.HtmlToPdf.TrueTypeSubset.subset(source_font, glyph_ids)

    fontfiles =
      for {_, %{value: value}} <- context.document.objects,
          is_map(value),
          {:ref, _} = ref <- [value["FontFile2"]] do
        assert {:ok, bytes} =
                 NativeElixirPdfUtilities.Pdf.Reader.decoded_stream(context.document, ref)

        bytes
      end

    assert subset in fontfiles

    destinations =
      for {_ref, %{value: %{"Subtype" => {:name, "Link"}, "Dest" => dest}}} <-
            context.document.objects,
          do: dest

    assert [[page_ref, {:name, "XYZ"}, dest_x, dest_y, _zoom]] = destinations
    assert page_ref == {:ref, Enum.at(context.pages, 1).ref}
    assert_in_delta dest_x, x, 0.0001
    assert_in_delta dest_y, y, 0.0001

    root = Path.join(System.tmp_dir!(), "table-target-#{System.unique_integer([:positive])}")
    File.mkdir_p!(root)
    on_exit(fn -> File.rm_rf!(root) end)
    path = Path.join(root, "target.pdf")
    File.write!(path, pdf)
    assert {text, 0} = System.cmd("pdftotext", ["-layout", path, "-"], stderr_to_stdout: true)
    refute text =~ "Syntax Error"
    refute text =~ "Syntax Warning"
    assert length(Regex.scan(~r/\bLine16\b/, text)) == 1
  end

  test "direct fragment preparation uses available space and validates coherent state", %{
    layout: layout
  } do
    first =
      Enum.find(
        layout.boxes,
        &(Map.get(&1, :role) == :table_cell_background and Map.has_key?(&1, :table_span_fragment))
      )

    assert {:ok, [paint], consumed} =
             HtmlTableValidator.prepare_cell_page_fragments([first], %{}, layout.margins.bottom)

    assert paint.height > first.height
    assert consumed[first.table_span_fragment.identity] == paint.height
    next = DrawingGeometry.translate_box(first, 0.0, 148.0 - first.y - first.height)

    assert {:ok, [last], final} =
             HtmlTableValidator.prepare_cell_page_fragments([next], consumed, 12.0)

    assert_in_delta paint.height + last.height, first.table_span_fragment.total_height, 0.000001
    assert final[first.table_span_fragment.identity] == first.table_span_fragment.total_height

    inconsistent =
      put_in(
        first,
        [:table_span_fragment, :total_height],
        first.table_span_fragment.total_height + 1.0
      )

    for {boxes, state} <- [
          {[first], %{make_ref() => :bad}},
          {[first], %{first.table_span_fragment.identity => -1.0}},
          {[first],
           %{first.table_span_fragment.identity => first.table_span_fragment.total_height + 1.0}},
          {[first, inconsistent], %{}},
          {[Map.put(first, :table_span_fragment, :bad)], %{}},
          {[%{first | transform: {2, 0, 0, 1, first.x, first.y}}], %{}},
          {[first | :bad], %{}}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               HtmlTableValidator.prepare_cell_page_fragments(boxes, state, 12.0)

      assert diagnostic.stage == :pagination
      assert diagnostic.reason == :invalid_layout
      assert diagnostic.module == HtmlTableValidator
      assert diagnostic.operation == :prepare_cell_page_fragments
      assert diagnostic.source == "table_span_fragment"
      assert diagnostic.message =~ "continuation"
    end
  end

  test "direct row-band paint rejects discontinuous bands and malformed nested native artwork", %{
    layout: layout
  } do
    boxes = artwork(layout.boxes)
    text = Enum.find(boxes, &(Map.get(&1, :type) == :text))
    bands = [%{row: 0, offset: 0.0, height: 10.0}, %{row: 1, offset: 10.0, height: 20.0}]
    assert {:ok, clips} = HtmlTableValidator.prepare_cell_band_paint([text], bands, 50.0)
    assert length(clips) == 2
    assert hd(clips).cell_geometry.total_height == 30.0
    assert hd(clips).cell_geometry.identity == List.last(clips).cell_geometry.identity

    for invalid <- [
          [%{row: 0, offset: 1.0, height: 10.0}],
          [%{row: 0, offset: 0.0, height: 10.0}, %{row: 2, offset: 10.0, height: 20.0}],
          [%{row: 0, offset: 0.0, height: 10.0}, %{row: 1, offset: 9.0, height: 20.0}],
          [hd(bands) | :bad]
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               HtmlTableValidator.prepare_cell_band_paint([text], invalid, 50.0)

      assert diagnostic.stage == :layout
      assert diagnostic.module == HtmlTableValidator
      assert diagnostic.operation == :prepare_cell_band_paint
    end

    [target | _] =
      Enum.find_value(boxes, fn box ->
        case Map.get(box, :html_targets, []) do
          [] -> nil
          targets -> targets
        end
      end)

    malformed_annotation = Map.put(text, :html_targets, [target | :bad])

    assert {:error, {:invalid_layout, annotation}} =
             HtmlTableValidator.prepare_cell_band_paint([malformed_annotation], bands, 50.0)

    assert annotation.module == NativeElixirPdfUtilities.Validators.HtmlLinkValidator
    assert annotation.source == "html_targets/link_destination"
    glyph = %{type: :glyph_group, actual_text: text.text, boxes: [text]}

    assert {:error, {_reason, diagnostic}} =
             HtmlTableValidator.prepare_cell_band_paint([%{glyph | boxes: [glyph]}], bands, 50.0)

    assert diagnostic.stage == :pdf
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.WriterValidator
    assert diagnostic.message =~ "writer"
  end

  test "page-fragment quota precedes artwork copying", %{layout: layout} do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_table_grid_work: 1})
    first = Enum.find(layout.boxes, &Map.has_key?(&1, :table_span_fragment))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlTableValidator.prepare_cell_page_fragments([first], %{}, 12.0)

    assert diagnostic.stage == :pagination
    assert diagnostic.message =~ "max_table_grid_work"
  end
end
