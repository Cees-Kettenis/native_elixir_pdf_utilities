defmodule NativeElixirPdfUtilities.HtmlToPdf.TableClosureContractTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Forms, Text, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout, ResourceContext}
  alias NativeElixirPdfUtilities.Validators.{HtmlTableValidator, HtmlValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "final hidden controls and word-break objects retain authored cascade without becoming table ancestors" do
    for html <- [
          "<p>Before<input type=hidden name=secret value=Hidden style='display:inline-block!important'>After</p>",
          "<p>Before<wbr style='display:inline-block;width:20pt;height:24pt;background:red'>After</p>"
        ] do
      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, []} = Forms.fields(pdf)
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert text =~ "Before" and text =~ "After"
      refute text =~ "Hidden"
    end

    assert {:ok, dom} = HtmlParser.parse_detailed("<p>Visible</p>")
    assert {:ok, styled} = Style.compute_detailed(dom)
    [paragraph] = styled.children
    forged = %{paragraph | style: Map.put(paragraph.style, :_table_atomic_context, true)}
    assert {:error, {:invalid_layout, diagnostic}} = Layout.layout(%{styled | children: [forged]})
    assert diagnostic.module == HtmlTableValidator
    assert diagnostic.message =~ "actual inline-block ancestor"
  end

  test "empty canonical cell artwork and malformed collection callers return bounded public results" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<table><tr></tr></table>")
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled)
    assert layout.boxes == []

    assert {:ok, []} =
             HtmlTableValidator.prepare_cell_band_paint(
               layout.boxes,
               [%{row: 0, offset: 0.0, height: 0.0}],
               20.0
             )

    for boxes <- [nil, [nil], [%{}], [%{type: :rect}], [nil | :invalid]] do
      assert {:error, {_reason, diagnostic}} = HtmlTableValidator.prepare_row_start_inset(boxes)
      assert diagnostic.stage in [:layout, :pdf, :pagination] and is_binary(diagnostic.message)
    end
  end

  test "canonical row and source continuation reject impossible caller geometry without losing diagnostics" do
    html =
      File.read!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/html_table_span_whole_line_continuation.html",
          __DIR__
        )
      )

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)
    span = Enum.find(layout.boxes, &Map.has_key?(&1, :table_span_fragment))
    row = Enum.filter(layout.boxes, &(Map.get(&1, :flow_id) == span.flow_id))
    assert {:ok, _} = HtmlTableValidator.prepare_row_continuation_intent(row, 0.0, 12.0)

    for {boxes, delta, bottom} <- [
          {nil, 0.0, 12.0},
          {[nil], 0.0, 12.0},
          {row, :bad, 12.0},
          {row, 0.0, -1.0},
          {row, Limits.get(:max_pdf_numeric_magnitude) + 1, 12.0}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               HtmlTableValidator.prepare_row_continuation_intent(boxes, delta, bottom)

      assert diagnostic.operation == :prepare_row_continuation_intent
      assert diagnostic.stage == :pagination
    end

    for {sources, page, previous, top, bottom} <- [
          {[span], nil, %{}, 148.0, 12.0},
          {[span], [nil], %{}, 148.0, 12.0},
          {[span], [], nil, 148.0, 12.0},
          {[span], [], %{}, 12.0, 148.0},
          {[Map.put(span, :transform, {1, 0, 0, 1, 0, 0})], [], %{}, 148.0, 12.0}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               HtmlTableValidator.prepare_cell_overflow_fragments(
                 sources,
                 page,
                 previous,
                 top,
                 bottom
               )

      assert diagnostic.module == HtmlTableValidator and diagnostic.message != ""
    end

    assert {:ok, [], _} = HtmlTableValidator.prepare_cell_page_fragments([], %{}, 12.0)

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlTableValidator.prepare_cell_page_fragments([], nil, 12.0)

    assert diagnostic.message =~ "issued cell identities"

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlTableValidator.prepare_cell_page_fragments([nil], %{}, 12.0)

    assert diagnostic.message =~ "proper page boxes"

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlTableValidator.prepare_cell_page_fragments([span], %{}, -1.0)

    assert diagnostic.stage == :pagination
  end

  test "canonical source hosts require a real positive canvas and supported form mode before issuing carriers" do
    html = "<form id=owner><input name=field value=Value style='width:70pt;height:18pt'></form>"
    budget = HtmlValidator.new_image_budget()

    assert :ok ==
             ResourceContext.run([], budget, fn context ->
               assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
               assert {:ok, styled} = Style.compute_detailed(dom)
               assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)
               assert Enum.any?(layout.boxes, &is_map(Map.get(&1, :form_control)))
               canvas = %{x: 0.0, y: 0.0, width: 240.0, height: 160.0}

               for bad <- [
                     nil,
                     %{},
                     %{canvas | width: 0.0},
                     %{canvas | height: :invalid},
                     %{canvas | x: Limits.get(:max_pdf_numeric_magnitude) + 1}
                   ] do
                 assert {:error, {:invalid_layout, diagnostic}} =
                          HtmlTableValidator.prepare_cell_fields(
                            layout.boxes,
                            bad,
                            context,
                            :interactive
                          )

                 assert diagnostic.operation == :prepare_cell_fields
                 assert diagnostic.source == "table cell controls"
               end

               assert {:error, {_reason, diagnostic}} =
                        HtmlTableValidator.prepare_cell_fields(
                          layout.boxes,
                          canvas,
                          context,
                          :invalid
                        )

               assert is_binary(diagnostic.message)

               assert {:ok, static} =
                        HtmlTableValidator.prepare_cell_fields(
                          layout.boxes,
                          canvas,
                          context,
                          :static
                        )

               refute Enum.any?(static, &Map.has_key?(&1, :embedded_widgets))
               :ok
             end)
  end

  test "detached template table source validates spans before styling instead of raising" do
    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlToPdf.render("<table><?marker name=A></table><template for=A><col></template>")

    assert diagnostic.stage == :layout and diagnostic.reason == :invalid_layout
    assert diagnostic.operation == :render
    assert is_binary(diagnostic.message)
  end

  test "source row groups retain actual transform scope and spanning widget identity" do
    html =
      "<style>body{font-family:DejaVu Sans;font-size:9pt;line-height:12pt;margin:0}table{border-collapse:collapse;width:180pt}td{padding:3pt;border:1px solid black;vertical-align:top}</style><form id=owner><table><tbody style='transform:translate(3pt,2pt)'><tr><td rowspan=2><input name=field value=Value style='width:70pt;height:18pt'><br>Tail</td><td>One</td></tr><tr><td>Two</td></tr></tbody></table></form>"

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 160}, margin: 12)
    assert {:ok, [%{name: "field", value: "Value", widgets: [_]}]} = Forms.fields(pdf)
    assert {:ok, filled} = Forms.fill(pdf, %{"field" => "Changed"})
    assert {:ok, flat} = Forms.flatten(filled)
    assert {:ok, text} = Text.extract(flat, layout: false)

    for word <- ["Changed", "Tail", "One", "Two"],
        do: assert(length(Regex.scan(Regex.compile!(word), text)) == 1)
  end

  test "decorated table fragments preserve source lines and actual deferred targets when source wrappers shrink" do
    lines = Enum.map_join(1..30, "<br>", &"Body#{&1}")

    html =
      "<style>html,body{margin:0}body{font-family:DejaVu Sans;font-size:9pt;line-height:12pt}fieldset{padding:3pt}table{width:200pt;table-layout:fixed;border-collapse:collapse}td,th{padding:3pt;border:1px solid black;vertical-align:top}</style><fieldset id=outer><table><thead><tr><th rowspan=2>Heading</th><th>HeadOne</th></tr><tr><th>HeadTwo</th></tr></thead><tbody><tr style='break-before:page'><td id=source rowspan=2>#{lines}</td><td>One</td></tr><tr><td>Two</td></tr></tbody></table></fieldset><a href='#source'>Jump</a>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)
    assert {:ok, pages} = NativeElixirPdfUtilities.HtmlToPdf.Pagination.paginate(layout)

    span =
      Enum.find(
        hd(pages).boxes,
        &(Map.has_key?(&1, :table_span_fragment) and Map.has_key?(&1, :fieldset_source_y) and
            Map.get(&1, :role) != :table_border)
      )

    assert is_map(span)

    assert {:ok, reframed, height} =
             HtmlTableValidator.prepare_row_page_fragment(
               [span],
               span.y + span.height,
               span.y,
               min(span.height, 20.0)
             )

    assert height > 0.0
    assert Enum.all?(reframed, &Map.has_key?(&1, :fieldset_source_y))
    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 160}, margin: 12)
    assert {:ok, text} = Text.extract(pdf, layout: false)

    for index <- 1..30,
        do: assert(length(Regex.scan(Regex.compile!("Body#{index}(?![0-9])"), text)) == 1)

    assert length(Regex.scan(~r/Heading/, text)) == 1
    assert length(Regex.scan(~r/Jump/, text)) == 1
  end

  test "native table callers reject damaged continuation and nested paint while preserving issued source identities" do
    html =
      "<style>body{font-family:DejaVu Sans;font-size:9pt;line-height:12pt}td{padding:3pt;vertical-align:top}</style><table><tr><td rowspan=2>One<br>Two<br>Three<br>Four</td><td>Peer</td></tr><tr><td>Last</td></tr></table>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)

    span =
      Enum.find(
        layout.boxes,
        &(Map.has_key?(&1, :table_span_fragment) and Map.get(&1, :role) != :table_border)
      )

    descriptor = span.table_span_fragment
    assert is_reference(descriptor.identity)

    for damaged <- [
          Map.put(span, :table_fragment_source_gap, 1.0),
          Map.put(span, :table_fragment_source_gap, 0.25),
          Map.put(span, :table_span_fragment, :invalid),
          %{span | table_span_fragment: %{descriptor | offset: descriptor.total_height + 1}}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               HtmlTableValidator.prepare_cell_page_fragments([damaged], %{}, 12.0)

      assert diagnostic.stage == :pagination and diagnostic.message != ""
    end

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlTableValidator.prepare_cell_page_fragments(
               [span],
               %{descriptor.identity => descriptor.total_height + 1},
               12.0
             )

    assert diagnostic.message =~ "consumed"

    assert {:error, {:invalid_layout, source_diagnostic}} =
             HtmlTableValidator.prepare_cell_page_fragments(
               [span],
               %{descriptor.content_identity => descriptor.total_height + 1},
               12.0
             )

    assert source_diagnostic.message =~ "source height"

    assert {:error, {:invalid_layout, span_diagnostic}} =
             HtmlTableValidator.prepare_row_page_fragment(
               [
                 %{
                   span
                   | table_span_fragment: %{descriptor | offset: descriptor.total_height + 1}
                 }
               ],
               span.y + span.height,
               span.y,
               span.height
             )

    assert span_diagnostic.message =~ "canonical"
    broken_flow = %{span | boxes: Enum.map(span.boxes, &Map.put(&1, :table_cell_flow, %{}))}

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlTableValidator.prepare_row_page_fragment(broken_flow.boxes, 148.0, 12.0, 136.0)

    assert diagnostic.message =~ "source flow"
    glyph = %{type: :glyph_group, actual_text: "Original", boxes: [nil | :invalid]}

    for boxes <- [[glyph], [%{span | boxes: [nil]}], [%{span | boxes: [glyph]}]] do
      assert {:error, {_reason, diagnostic}} =
               HtmlTableValidator.prepare_cell_band_paint(
                 boxes,
                 [%{row: 0, offset: 0.0, height: 20.0}],
                 148.0
               )

      assert diagnostic.message != ""
    end

    assert {:error, {_reason, diagnostic}} =
             HtmlTableValidator.prepare_row_page_fragment([glyph], 148.0, 12.0, 136.0)

    assert diagnostic.message != ""

    assert {:error, {_reason, diagnostic}} =
             HtmlTableValidator.prepare_row_continuation_intent(
               [%{span | boxes: [nil]}],
               0.0,
               12.0
             )

    assert diagnostic.message != ""

    assert {:error, {_reason, diagnostic}} =
             HtmlTableValidator.validate_atomic_pagination(layout.boxes, [nil], 136.0)

    assert diagnostic.message != ""
  end

  test "source target markers and empty native groups consume no paint and preserve half-open row cuts" do
    html =
      "<style>td{vertical-align:top}</style><div id=empty></div><a href=#empty>Jump</a><table><tr><td>Text</td></tr></table>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)
    markers = Enum.filter(layout.boxes, &(Map.get(&1, :type) == :flow_marker))
    assert markers != []

    assert {:ok, []} =
             HtmlTableValidator.prepare_cell_band_paint(
               markers,
               [%{row: 0, offset: 0.0, height: 20.0}],
               148.0
             )

    assert {:ok, [], _height} =
             HtmlTableValidator.prepare_row_page_fragment(markers, 148.0, 12.0, 136.0)

    empty_group = %{
      type: :drawing_group,
      x: 0.0,
      y: 0.0,
      width: 20.0,
      height: 20.0,
      transform: {1, 0, 0, 1, 0, 0},
      boxes: []
    }

    assert {:ok, []} =
             HtmlTableValidator.prepare_cell_band_paint(
               [empty_group],
               [%{row: 0, offset: 0.0, height: 20.0}],
               148.0
             )

    for bad_band <- [
          nil,
          %{row: :bad, offset: 0.0, height: 20.0},
          %{row: 0, offset: -1.0, height: 20.0}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               HtmlTableValidator.prepare_cell_band_paint(markers, [bad_band], 148.0)

      assert diagnostic.message != ""
    end
  end

  test "reused canonical spanning header paint starts from its issued source rather than body consumption" do
    assert {:ok, dom} =
             HtmlParser.parse_document_detailed(
               "<table><thead><tr><th rowspan=2>Heading</th><th>Peer</th></tr><tr><th>Next</th></tr></thead><tbody><tr><td>Body</td><td>Last</td></tr></tbody></table>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)

    header =
      Enum.filter(
        layout.boxes,
        &(Map.has_key?(&1, :table_span_fragment) and Map.get(&1, :table_section) == :head and
            Map.get(&1, :role) != :table_border)
      )

    assert header != []

    previous =
      Map.new(header, &{&1.table_span_fragment.identity, &1.table_span_fragment.total_height})

    assert {:ok, retained, _state} =
             HtmlTableValidator.prepare_cell_page_fragments(header, previous, 12.0)

    assert Enum.any?(retained, &(&1.boxes != []))
  end

  test "canonical row model rejects lost colspan and broken baseline descendants before allocation" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<table><tr><td rowspan=2>Span</td><td>Peer</td></tr><tr><td>Next</td></tr><tr><td>Last</td><td>End</td></tr></table>"
             )

    assert {:ok, %{children: [table]}} = Style.compute_detailed(dom)
    assert {:ok, %{rows: rows}} = HtmlTableValidator.form(table.children, table.style)
    [first | rest] = rows
    [placement | peers] = first.cells
    damaged = [%{first | cells: [Map.delete(placement, :colspan) | peers]} | rest]

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlTableValidator.prepare_cell_background_geometry(
               damaged,
               [10.0, 10.0, 10.0],
               [40.0, 40.0],
               {0.0, 0.0}
             )

    assert diagnostic.operation == :prepare_cell_background_geometry

    baseline_cell = %{
      placement.cell
      | style: Map.put(placement.cell.style, :vertical_align, :baseline),
        children: [nil | :invalid]
    }

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlTableValidator.validate_presentation_style(
               baseline_cell,
               baseline_cell.style,
               :layout
             )

    assert diagnostic.message =~ "descendants"

    metrics =
      Map.new(
        for {row, index} <- Enum.with_index(rows),
            cell <- row.cells,
            do:
              {{index, cell.column},
               %{
                 intrinsic_height: 20.0,
                 minimum_height: 20.0,
                 baseline: 15.0,
                 inset_bottom: 0.0,
                 has_flow: true
               }}
      )

    metrics = Map.update!(metrics, {0, 0}, &%{&1 | intrinsic_height: 60.0, minimum_height: 60.0})
    assert {:ok, geometry} = HtmlTableValidator.prepare_baseline_geometry(rows, metrics, 0.0)
    assert length(geometry.heights) == 3
    assert List.last(geometry.heights) == 20.0
  end

  test "printed alphabetic hyphens expose the same min-content and wrapping boundary as Chromium" do
    alias NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator

    for {source, expected} <- [
          {"STYLE-5376", ["STYLE-", "5376"]},
          {"STYLE-0638", ["STYLE-", "0638"]},
          {"ITEM-TEST-7116", ["ITEM-", "TEST-", "7116"]},
          {"-5376", ["-5376"]},
          {"12-34", ["12-34"]},
          {"Plain", ["Plain"]}
        ] do
      assert {:ok, ^expected} = HtmlLineBreakValidator.segments(source)
      assert Enum.join(expected) == source
    end

    assert {:ok, true} = HtmlLineBreakValidator.boundary?("STYLE-", "5376")
    assert {:ok, false} = HtmlLineBreakValidator.boundary?("12-", "34")

    assert {:ok, ["STYLE-", "5376 ", " ", "End"]} =
             HtmlLineBreakValidator.segments("STYLE-5376  End", "", true)
  end

  test "actual issued fields reserve the union of both widgets and omit a genuinely disjoint paint clip" do
    alias NativeElixirPdfUtilities.Validators.{HtmlFormValidator, EmbeddedFormValidator}

    html =
      "<form id=owner><input name=first value=First style='width:50pt;height:18pt'><input name=second value=Second style='width:50pt;height:18pt'></form>"

    assert :ok ==
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
               assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
               assert {:ok, styled} = Style.compute_detailed(dom)
               assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)

               assert {:ok, [_page], fields} =
                        HtmlFormValidator.prepare_scoped(
                          [%{size: {240.0, 160.0}, boxes: layout.boxes}],
                          [],
                          context
                        )

               assert length(fields) == 2
               assert {:ok, descriptor} = EmbeddedFormValidator.issue(context, fields)

               group = %{
                 type: :drawing_group,
                 x: 0.0,
                 y: 0.0,
                 width: 240.0,
                 height: 160.0,
                 transform: {1, 0, 0, 1, 0, 0},
                 boxes: [],
                 embedded_widgets: descriptor
               }

               assert {:ok, [band]} =
                        HtmlTableValidator.prepare_cell_band_paint(
                          [group],
                          [%{row: 0, offset: 0.0, height: 160.0}],
                          160.0
                        )

               [lefts, bottoms, rights, tops] =
                 fields |> Enum.map(& &1.rect) |> Enum.zip() |> Enum.map(&Tuple.to_list/1)

               assert_in_delta band.clip.x, Enum.min(lefts), 0.00001
               assert_in_delta band.clip.width, Enum.max(rights) - Enum.min(lefts), 0.00001
               assert Enum.max(tops) > Enum.min(bottoms)
               clipped = Map.put(group, :clip, %{x: 0.0, y: 0.0, width: 1.0, height: 1.0})

               assert {:ok, []} =
                        HtmlTableValidator.prepare_cell_band_paint(
                          [clipped],
                          [%{row: 0, offset: 0.0, height: 160.0}],
                          160.0
                        )

               :ok
             end)
  end

  test "zero-line-height authored target markers defer with source flow through nested native clips" do
    html =
      "<style>body{font-family:DejaVu Sans;font-size:9pt;line-height:12pt}td{vertical-align:top;width:60pt;padding:0}p{margin:0}</style><table><tr><td><div style='overflow:hidden;width:50pt;height:80pt'><p>alpha&shy;betagamma</p><p><span id=zero style='line-height:0'></span>Last</p></div></td></tr></table><a href='#zero'>Jump</a>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)
    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 160}, margin: 12)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "Last" and text =~ "Jump"
    rows = Enum.filter(layout.boxes, &(Map.get(&1, :table_id) != nil))

    assert {:ok, fragments, height} =
             HtmlTableValidator.prepare_row_page_fragment(rows, 148.0, 12.0, 36.0, 136.0)

    assert fragments == [] and height == 0.0

    assert {:ok, complete, painted_height} =
             HtmlTableValidator.prepare_row_page_fragment(rows, 148.0, 12.0, 136.0, 136.0)

    assert complete != [] and painted_height > 0.0 and painted_height <= 136.0
  end

  test "source-field validation rejects invalid context and complete field limits before carrier issuance" do
    html = "<form><input name=first value=First><input name=second value=Second></form>"

    assert :ok ==
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
               assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
               assert {:ok, styled} = Style.compute_detailed(dom)
               assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)
               canvas = %{x: 0.0, y: 0.0, width: 240.0, height: 160.0}

               assert {:error, {_reason, invalid_context}} =
                        HtmlTableValidator.prepare_cell_fields(
                          layout.boxes,
                          canvas,
                          nil,
                          :interactive
                        )

               assert is_binary(invalid_context.message)
               original = Limits.effective()
               Limits.install(%{original | max_pdf_form_fields: 1})

               assert {:error, {:resource_limit_exceeded, field_limit}} =
                        HtmlTableValidator.prepare_cell_fields(
                          layout.boxes,
                          canvas,
                          context,
                          :interactive
                        )

               assert field_limit.message =~ "max_pdf_form_fields"
               Limits.install(original)

               assert {:ok, carriers} =
                        HtmlTableValidator.prepare_cell_fields(
                          layout.boxes,
                          canvas,
                          context,
                          :interactive
                        )

               assert Enum.count(carriers, &Map.has_key?(&1, :embedded_widgets)) == 2
               :ok
             end)
  end

  test "actual row target wrappers retain source anchors through partial ordinary and spanning cuts" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Pagination
    lines = Enum.map_join(1..24, "<br>", &"Line#{&1}")

    html =
      "<style>html,body{margin:0}body{font-family:DejaVu Sans;font-size:9pt;line-height:12pt}td{padding:0;vertical-align:top}fieldset{padding:0}</style><fieldset><table><tr id=row><td rowspan=2>#{lines}</td><td>Peer</td></tr><tr><td>Last</td></tr></table></fieldset><a href='#row'>Jump</a>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)

    span =
      Enum.find(
        layout.boxes,
        &(Map.has_key?(&1, :table_span_fragment) and Map.get(&1, :html_targets, []) != [])
      )

    assert is_map(span)
    row = Enum.filter(layout.boxes, &(Map.get(&1, :flow_id) == span.flow_id))

    assert {:ok, prepared, height} =
             HtmlTableValidator.prepare_row_page_fragment(
               row,
               span.y + span.height,
               span.y,
               min(span.height, 50.0)
             )

    assert height > 0.0
    assert Enum.any?(prepared, &(Map.get(&1, :html_targets, []) != []))
    assert {:error, {:unsupported_html, target_diagnostic}} = Pagination.paginate(layout)
    assert target_diagnostic.source == "row"
    assert target_diagnostic.message =~ "ancestor clips"
    descriptor = span.table_span_fragment

    assert {:ok, carried} =
             HtmlTableValidator.prepare_cell_overflow_fragments(
               [span],
               [],
               %{descriptor.content_identity => 50.0},
               148.0,
               12.0
             )

    assert Enum.any?(carried, &(Map.get(&1, :html_targets, []) != []))
  end

  test "genuine source projection checks current numeric limits before allocating widget paint" do
    alias NativeElixirPdfUtilities.Validators.{HtmlFormValidator, EmbeddedFormValidator}

    assert {:error, {:invalid_pdf_input, diagnostic}} =
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
               assert {:ok, dom} =
                        HtmlParser.parse_document_detailed(
                          "<input name=field value=Value style='display:block;width:40pt;height:18pt'>"
                        )

               assert {:ok, styled} = Style.compute_detailed(dom)
               assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)

               assert {:ok, [_], fields} =
                        HtmlFormValidator.prepare_scoped(
                          [%{size: {240.0, 160.0}, boxes: layout.boxes}],
                          [],
                          context
                        )

               assert {:ok, descriptor} = EmbeddedFormValidator.issue(context, fields)
               original = Limits.effective()

               try do
                 Limits.install(%{original | max_pdf_numeric_magnitude: 100})

                 group = %{
                   type: :drawing_group,
                   x: 0.0,
                   y: 0.0,
                   width: 100.0,
                   height: 100.0,
                   transform: {0.1, 0, 0, 0.1, 0, 0},
                   boxes: [],
                   embedded_widgets: descriptor
                 }

                 HtmlTableValidator.prepare_cell_band_paint(
                   [group],
                   [%{row: 0, offset: 0.0, height: 100.0}],
                   100.0
                 )
               after
                 Limits.install(original)
               end
             end)

    assert diagnostic.message =~ "max_pdf_numeric_magnitude"
    assert diagnostic.stage == :pdf
  end

  test "actual paid decorative row fragments retain their bounded CSS source advance" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Pagination

    html =
      "<style>body{font-family:DejaVu Sans;font-size:9pt;line-height:12pt}td{height:400pt;padding:0;vertical-align:top;border:1px solid black}</style><p style='margin:0'>Before</p><table><tr style='break-before:page'><td>Short</td></tr></table>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)
    assert {:ok, pages} = Pagination.paginate(layout)

    fragment =
      Enum.find(
        Enum.flat_map(pages, & &1.boxes),
        &(Map.get(&1, :table_fragment_source_gap, 0.0) > 0)
      )

    assert is_map(fragment) and fragment.table_fragment_source_gap < 0.75

    assert {:ok, [retained], _state} =
             HtmlTableValidator.prepare_cell_page_fragments([fragment], %{}, 12.0)

    assert retained == fragment
  end

  test "actual source target markers carry their real paragraph identity into table source units" do
    html =
      "<style>body{font-family:DejaVu Sans;font-size:9pt;line-height:12pt}td{padding:0;vertical-align:top}</style><table><tr><td rowspan=2><span id=zero style='line-height:0'></span>Text<br>Last</td><td>Peer</td></tr><tr><td>Next</td></tr></table><a href='#zero'>Jump</a>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)

    span =
      Enum.find(
        layout.boxes,
        &(Map.has_key?(&1, :table_span_fragment) and Map.get(&1, :role) != :table_border)
      )

    marker =
      Enum.find(
        span.boxes,
        &(Map.get(&1, :type) == :flow_marker and Map.get(&1, :height) == 0 and
            Map.get(&1, :html_targets, []) != [])
      )

    assert is_map(marker)
    assert %{flow_id: id, fragment_id: {:line, id, _index}} = marker.table_cell_flow

    assert {:ok, [], _height} =
             HtmlTableValidator.prepare_row_page_fragment([marker], span.height, 0.0, span.height)

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlTableValidator.prepare_row_continuation_intent(
               [%{span | type: :rect}],
               0.0,
               12.0
             )

    assert diagnostic.message =~ "native group"
  end

  test "translated source clips reject numeric overflow with a public failure rather than dropping it" do
    html =
      "<style>td{background:#eee;height:30pt;vertical-align:top}</style><table><tr><td rowspan=2>Span</td><td>Peer</td></tr><tr><td>Next</td></tr></table>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)

    span =
      Enum.find(
        layout.boxes,
        &(Map.has_key?(&1, :table_span_fragment) and Map.get(&1, :role) == :table_cell_background)
      )

    assert is_map(span)
    [first | rest] = span.boxes

    first =
      Map.put(first, :clip, %{
        x: 0.0,
        y: Limits.get(:max_pdf_numeric_magnitude) - 10,
        width: 1.0,
        height: 1.0
      })

    bottom = 8.0 - span.height
    shifted = %{span | y: bottom, transform: {1, 0, 0, 1, span.x, bottom}, boxes: [first | rest]}
    consumed = min(span.table_span_fragment.total_height - 1, span.height + 15.0)

    assert {:error, {_reason, diagnostic}} =
             HtmlTableValidator.prepare_cell_page_fragments(
               [shifted],
               %{span.table_span_fragment.identity => consumed},
               0.0
             )

    assert is_binary(diagnostic.message) and diagnostic.message != ""
  end

  test "prepared table pagination diagnoses damaged source descriptors and native row paint" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Pagination

    for {html, mutation} <- [
          {"<table><tr><td rowspan=2>Span</td><td>Peer</td></tr><tr><td>Last</td></tr></table>",
           :descriptor},
          {"<table><tr><td rowspan=2>#{Enum.map_join(1..24, "<br>", &"Line#{&1}")}</td><td>Peer</td></tr><tr><td>Last</td></tr></table>",
           :descriptor},
          {"<table><tr><td style='height:400pt;background:#eee'>Tall</td></tr></table>", :row},
          {"<table><caption style='height:125pt;background:#ddd'>Caption</caption><tr><td>Row</td></tr></table>",
           :row}
        ] do
      html =
        "<style>body{font-family:DejaVu Sans;font-size:9pt;line-height:12pt}td{padding:3pt;vertical-align:top;border:1px solid black}</style>" <>
          html

      assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)

      boxes =
        Enum.map(layout.boxes, fn box ->
          cond do
            mutation == :descriptor and Map.has_key?(box, :table_span_fragment) ->
              %{
                box
                | table_span_fragment: %{
                    box.table_span_fragment
                    | offset: box.table_span_fragment.total_height + 1
                  }
              }

            mutation == :row and Map.get(box, :type) == :rect and
                Map.get(box, :role) in [:table_border, :table_cell_background] ->
              Map.put(box, :border_radius, -1.0)

            true ->
              box
          end
        end)

      assert boxes != layout.boxes
      assert {:error, {_reason, diagnostic}} = Pagination.paginate(%{layout | boxes: boxes})
      assert diagnostic.stage in [:pagination, :layout, :pdf] and is_binary(diagnostic.message)
    end
  end

  test "zero-area inline destination follows the later printed line exactly once" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Pagination

    html =
      File.read!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/html_table_zero_target_continuation.html",
          __DIR__
        )
      )

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)
    assert {:ok, pages} = Pagination.paginate(layout)
    jump = Enum.find(Enum.flat_map(pages, & &1.boxes), &(Map.get(&1, :text) == "Jump"))
    assert {:xyz, 2, x, y} = jump.link_destination
    assert x >= 12.0 and x <= 228.0 and y >= 12.0 and y <= 148.0
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf, layout: false)

    for index <- 1..16,
        do: assert(length(Regex.scan(Regex.compile!("Line#{index}(?![0-9])"), text)) == 1)
  end

  test "non-group source text cannot claim decorative row-fragment slack" do
    assert {:ok, dom} =
             HtmlParser.parse_document_detailed("<table><tr><td>Source</td></tr></table>")

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)
    source = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Source"))
    assert source.type == :text and source.table_id != nil

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlTableValidator.prepare_cell_page_fragments(
               [Map.put(source, :table_fragment_source_gap, 0.25)],
               %{},
               12.0
             )

    assert diagnostic.operation == :prepare_cell_page_fragments
    assert diagnostic.message =~ "row fragment source slack"
  end

  test "a genuine document destination retains its source point through a table paint fragment" do
    html =
      "<style>body{font-family:DejaVu Sans;font-size:9pt;line-height:12pt}td{padding:0;vertical-align:top}</style><div id=root style='margin-left:12pt'></div><table><tr><td>Source destination</td></tr></table><a href='#root'>Jump</a>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)

    marker =
      Enum.find(layout.boxes, fn box ->
        Map.get(box, :type) == :flow_marker and Map.get(box, :html_targets, []) != [] and
          not Map.has_key?(box, :table_cell_flow)
      end)

    assert is_map(marker)
    rows = Enum.filter(layout.boxes, &(Map.get(&1, :table_id) != nil))

    assert {:ok, fragments, height} =
             HtmlTableValidator.prepare_row_page_fragment([marker | rows], 148.0, 12.0, 136.0)

    assert height > 0 and height <= 136.0
    fragment = Enum.find(fragments, &(Map.get(&1, :role) == :table_row_fragment))
    assert is_map(fragment)
    retained = Enum.find(fragment.boxes, &(Map.get(&1, :html_targets, []) == marker.html_targets))
    assert is_map(retained)
    assert_in_delta retained.x + fragment.x, marker.x, 0.0001
    assert_in_delta retained.y + fragment.y, marker.y, 0.0001

    assert {:ok, pages} = NativeElixirPdfUtilities.HtmlToPdf.Pagination.paginate(layout)
    jump = Enum.find(Enum.flat_map(pages, & &1.boxes), &(Map.get(&1, :text) == "Jump"))
    assert {:xyz, 1, x, y} = jump.link_destination
    assert_in_delta x, marker.x, 0.0001
    assert_in_delta y, marker.y, 0.0001
    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 160}, margin: 12)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert length(Regex.scan(~r/Source destination/, text)) == 1
    assert length(Regex.scan(~r/Jump/, text)) == 1
  end

  test "rows covered only by a spanning baseline cell allocate the real measured source once" do
    html =
      "<style>body{font-family:DejaVu Sans;font-size:9pt;line-height:12pt}td{padding:0;vertical-align:baseline}</style><table><tr style='height:18pt'><td rowspan=2>Span<br>Two</td></tr><tr></tr></table>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    [table] = Enum.filter(styled.children, &(Map.get(&1, :tag) == "table"))
    assert {:ok, %{rows: [first, second]}} = HtmlTableValidator.form(table.children)
    assert [%{rowspan: 2}] = first.cells
    assert first.row.style.height == 18.0
    assert second.cells == []
    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 160}, margin: 12)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert length(Regex.scan(~r/Span/, text)) == 1
    assert length(Regex.scan(~r/Two/, text)) == 1
  end

  test "an already styled real span retains actionable diagnostics when its cardinality budget changes" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed("<table><tr><td colspan=2>Span</td></tr></table>")

    assert {:ok, styled} = Style.compute_detailed(dom)
    [table] = styled.children
    Limits.install(%{Limits.effective() | max_layout_cardinality: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlTableValidator.form(table.children)

    assert diagnostic.operation == :layout and diagnostic.source == "td"
    assert diagnostic.message =~ "table span"
  end
end
