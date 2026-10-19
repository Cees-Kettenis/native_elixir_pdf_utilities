alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.TableAtomicBaselineTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Pagination, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlTableValidator

  test "atomic mixed fonts align measured first flow baselines and preserve cell identities" do
    html = """
    <div style='display:inline-block;width:180pt;font-family:DejaVu Sans;font-size:9pt;line-height:12pt'>
    <table style='width:180pt;table-layout:fixed;border-collapse:collapse'><tr>
    <th id=h scope=col style='vertical-align:baseline;font-size:8pt;line-height:12pt;font-weight:normal;padding:3pt'>Small</th>
    <td headers=h style='vertical-align:baseline;font-size:18pt;line-height:24pt;padding:3pt'><a href='https://table.test/baseline'>Large</a></td>
    </tr></table></div>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed("<body>" <> html <> "</body>")
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, %{boxes: boxes}} = Layout.layout(styled, page_size: {240, 240}, margin: 0)
    small = Enum.find(boxes, &(Map.get(&1, :text) == "Small"))
    large = Enum.find(boxes, &(Map.get(&1, :text) == "Large"))
    assert_in_delta small.y, large.y, 0.000001
    assert large.table_cell.headers == [small.table_cell.source]
    assert large.link_url == "https://table.test/baseline"
    refute Enum.any?(boxes, &Map.has_key?(&1, :_measured_baseline))
    refute Enum.any?(boxes, &(&1.type == :baseline_measurement))
  end

  test "cell minimum height does not replace the first block line or empty atomic baseline" do
    html = """
    <div style='display:inline-block;width:180pt;font-family:DejaVu Sans;font-size:9pt;line-height:12pt'>
    <table style='width:180pt;table-layout:fixed;border-collapse:collapse'><tr>
    <td style='vertical-align:baseline;height:60pt;padding:0'><p style='margin:0;font-size:8pt;line-height:12pt'>First</p><p style='margin:0;font-size:18pt;line-height:24pt'>Later</p></td>
    <td style='vertical-align:baseline;padding:0;font-size:18pt;line-height:24pt'>Peer</td></tr>
    <tr><td style='vertical-align:baseline;padding:0'><span style='display:inline-block;width:12pt;height:18pt;background:red'></span>Atomic</td>
    <td style='vertical-align:baseline;padding:0;height:36pt;background:blue'></td></tr></table></div>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed("<body>" <> html <> "</body>")
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, %{boxes: boxes}} = Layout.layout(styled, page_size: {240, 240}, margin: 0)
    first = Enum.find(boxes, &(Map.get(&1, :text) == "First"))
    peer = Enum.find(boxes, &(Map.get(&1, :text) == "Peer"))
    later = Enum.find(boxes, &(Map.get(&1, :text) == "Later"))
    assert_in_delta first.y, peer.y, 0.000001
    assert later.y < first.y
    red = Enum.find(boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
    atomic = Enum.find(boxes, &(Map.get(&1, :text) == "Atomic"))
    assert_in_delta atomic.y, red.y, 0.000001
    blue = Enum.find(boxes, &(Map.get(&1, :fill_color) == {0, 0, 1}))
    assert blue.height == 36.0
  end

  test "rowspan ascent uses canonical starting row and retains combined-height constraints" do
    html =
      "<div style='display:inline-block'><table style='width:120pt;border-collapse:collapse'><tr><td rowspan=2 style='vertical-align:baseline'>Span</td><td style='vertical-align:baseline'>First</td></tr><tr><td style='vertical-align:baseline'>Second</td></tr></table></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [%{children: [table]}]}} = Style.compute_detailed(dom)
    assert {:ok, %{rows: rows}} = HtmlTableValidator.form(table.children, table.style)

    metric = fn intrinsic, minimum, baseline, flow ->
      %{
        intrinsic_height: intrinsic,
        minimum_height: minimum,
        baseline: baseline,
        inset_bottom: 2.0,
        has_flow: flow
      }
    end

    metrics = %{
      {0, 0} => metric.(44.0, 60.0, 22.0, true),
      {0, 1} => metric.(18.0, 18.0, 10.0, true),
      {1, 1} => metric.(20.0, 20.0, 14.0, true)
    }

    assert {:ok, geometry} = HtmlTableValidator.prepare_baseline_geometry(rows, metrics, 0.0)
    assert geometry.baselines == [22.0, 14.0]
    assert geometry.offsets[{0, 1}] == 12.0
    assert geometry.heights == [35.0, 25.0]
    assert Enum.sum(geometry.heights) == 60.0

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlTableValidator.prepare_baseline_geometry(rows, Map.delete(metrics, {0, 0}), 0.0)

    assert diagnostic.stage == :layout and diagnostic.module == HtmlTableValidator
    assert diagnostic.message =~ "canonical cell"
  end

  test "geometry validates representations and permits intrinsic baselines below the content edge" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div style='display:inline-block'><table><tr><td style='vertical-align:baseline'>Flow</td><td></td></tr><tr></tr></table></div>"
             )

    assert {:ok, %{children: [%{children: [table]}]}} = Style.compute_detailed(dom)
    assert {:ok, %{rows: rows}} = HtmlTableValidator.form(table.children, table.style)

    metric = %{
      intrinsic_height: 12.0,
      minimum_height: 12.0,
      baseline: 18.0,
      inset_bottom: 2.0,
      has_flow: true
    }

    empty = %{
      intrinsic_height: 4.0,
      minimum_height: 36.0,
      baseline: 2.0,
      inset_bottom: 2.0,
      has_flow: false
    }

    metrics = %{{0, 0} => metric, {0, 1} => empty}
    assert {:ok, geometry} = HtmlTableValidator.prepare_baseline_geometry(rows, metrics, 0.0)
    assert geometry.heights == [36.0, 0.0]
    assert geometry.baselines == [18.0, 0.0]
    assert geometry.offsets[{0, 1}] == 0.0

    for {invalid_rows, invalid_metrics, spacing} <- [
          {[nil], metrics, 0.0},
          {rows, metrics, -1},
          {rows, %{{0, 0} => %{metric | minimum_height: 1.0}, {0, 1} => empty}, 0.0}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               HtmlTableValidator.prepare_baseline_geometry(
                 invalid_rows,
                 invalid_metrics,
                 spacing
               )

      assert diagnostic.stage == :layout and diagnostic.reason == :invalid_layout
    end

    excessive = NativeElixirPdfUtilities.Limits.get(:max_css_numeric_magnitude) + 1

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlTableValidator.prepare_baseline_geometry(
               rows,
               %{{0, 0} => %{metric | minimum_height: excessive}, {0, 1} => empty},
               0.0
             )

    assert diagnostic.stage == :layout and diagnostic.message =~ "max_css_numeric_magnitude"

    assert {:ok, %{heights: [], baselines: [], offsets: %{}}} =
             HtmlTableValidator.prepare_baseline_geometry([], %{}, 0.0)
  end

  test "rowspan geometry charges prefix and redistribution work through the shared budget" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div style='display:inline-block'><table><tr><td rowspan=2 style='vertical-align:baseline'>Span</td><td style='vertical-align:baseline'>First</td></tr><tr><td style='vertical-align:baseline'>Second</td></tr></table></div>"
             )

    assert {:ok, %{children: [%{children: [table]}]}} = Style.compute_detailed(dom)
    assert {:ok, %{rows: rows}} = HtmlTableValidator.form(table.children, table.style)

    metric = %{
      intrinsic_height: 12.0,
      minimum_height: 12.0,
      baseline: 9.0,
      inset_bottom: 1.0,
      has_flow: true
    }

    metrics = %{{0, 0} => %{metric | minimum_height: 48.0}, {0, 1} => metric, {1, 1} => metric}
    assert {:ok, geometry} = HtmlTableValidator.prepare_baseline_geometry(rows, metrics, 0.0)
    assert Enum.sum(geometry.heights) == 48.0
    limit = NativeElixirPdfUtilities.Limits.get(:max_table_grid_work)

    result =
      NativeElixirPdfUtilities.Validators.HtmlValidator.with_render_budget(fn ->
        NativeElixirPdfUtilities.Validators.HtmlValidator.reserve_render_resource(
          :max_table_grid_work,
          limit - 28,
          :layout
        )

        HtmlTableValidator.prepare_baseline_geometry(rows, metrics, 0.0)
      end)

    assert {:error, {:resource_limit_exceeded, diagnostic}} = result
    assert diagnostic.stage == :layout and diagnostic.message =~ "max_table_grid_work"
    [first, second] = rows
    [span, peer] = first.cells
    invalid = [%{first | cells: [%{span | rowspan: 3}, peer]}, second]

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlTableValidator.prepare_baseline_geometry(invalid, metrics, 0.0)

    assert diagnostic.message =~ "canonical cell"
  end

  test "direct cell geometry reserves validation and placement cardinality before allocation" do
    cells = String.duplicate("<td style='vertical-align:baseline'>Cell</td>", 32)

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div style='display:inline-block'><table><tr>#{cells}</tr></table></div>"
             )

    assert {:ok, %{children: [%{children: [table]}]}} = Style.compute_detailed(dom)
    assert {:ok, %{rows: rows}} = HtmlTableValidator.form(table.children, table.style)

    metric = %{
      intrinsic_height: 12.0,
      minimum_height: 12.0,
      baseline: 9.0,
      inset_bottom: 1.0,
      has_flow: true
    }

    metrics = Map.new(0..31, &{{0, &1}, metric})

    assert {:ok, %{heights: [12.0]}} =
             HtmlTableValidator.prepare_baseline_geometry(rows, metrics, 0.0)

    limit = NativeElixirPdfUtilities.Limits.get(:max_table_grid_work)

    result =
      NativeElixirPdfUtilities.Validators.HtmlValidator.with_render_budget(fn ->
        NativeElixirPdfUtilities.Validators.HtmlValidator.reserve_render_resource(
          :max_table_grid_work,
          limit - 4,
          :layout
        )

        HtmlTableValidator.prepare_baseline_geometry(rows, metrics, 0.0)
      end)

    assert {:error, {:resource_limit_exceeded, diagnostic}} = result
    assert diagnostic.stage == :layout and diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.HtmlValidator
    assert diagnostic.message =~ "max_table_grid_work"
  end

  test "direct baseline dependency scanning charges each descendant before inspecting it" do
    children = String.duplicate("<span>Flow</span>", 16)

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div style='display:inline-block'><table><tr><td style='vertical-align:baseline'>#{children}</td></tr></table></div>"
             )

    assert {:ok, %{children: [%{children: [table]}]}} = Style.compute_detailed(dom)
    assert {:ok, %{rows: rows}} = HtmlTableValidator.form(table.children, table.style)

    metrics = %{
      {0, 0} => %{
        intrinsic_height: 12.0,
        minimum_height: 12.0,
        baseline: 9.0,
        inset_bottom: 1.0,
        has_flow: true
      }
    }

    assert {:ok, _geometry} = HtmlTableValidator.prepare_baseline_geometry(rows, metrics, 0.0)
    limit = NativeElixirPdfUtilities.Limits.get(:max_table_grid_work)

    result =
      NativeElixirPdfUtilities.Validators.HtmlValidator.with_render_budget(fn ->
        NativeElixirPdfUtilities.Validators.HtmlValidator.reserve_render_resource(
          :max_table_grid_work,
          limit - 45,
          :layout
        )

        HtmlTableValidator.prepare_baseline_geometry(rows, metrics, 0.0)
      end)

    assert {:error, {:resource_limit_exceeded, diagnostic}} = result
    assert diagnostic.stage == :layout and diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_table_grid_work"
  end

  test "direct malformed active descendants diagnose while hidden descendants remain inactive" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div style='display:inline-block'><table><tr><td style='vertical-align:baseline'>Cell</td></tr></table></div>"
             )

    assert {:ok, %{children: [%{children: [table]}]}} = Style.compute_detailed(dom)
    assert {:ok, %{rows: [row]}} = HtmlTableValidator.form(table.children, table.style)
    [placement] = row.cells

    metrics = %{
      {0, 0} => %{
        intrinsic_height: 12.0,
        minimum_height: 12.0,
        baseline: 9.0,
        inset_bottom: 1.0,
        has_flow: true
      }
    }

    for child <- [
          %{tag: "div", children: :bad},
          :bad,
          %{tag: "div", children: [:bad]},
          %{tag: "div", children: [placement.cell | :bad]}
        ] do
      cell = %{placement.cell | children: [child]}
      altered = [%{row | cells: [%{placement | cell: cell}]}]

      assert {:error, {:invalid_layout, diagnostic}} =
               HtmlTableValidator.prepare_baseline_geometry(altered, metrics, 0.0)

      assert diagnostic.stage == :layout and diagnostic.reason == :invalid_layout
      assert diagnostic.module == HtmlTableValidator and diagnostic.operation == :layout
      assert diagnostic.message =~ "correct the prepared cell tree"
      assert is_binary(diagnostic.source)
    end

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlTableValidator.prepare_baseline_geometry([row | :bad], metrics, 0.0)

    assert diagnostic.stage == :layout

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlTableValidator.prepare_baseline_geometry(
               [%{row | cells: [placement | :bad]}],
               metrics,
               0.0
             )

    assert diagnostic.reason == :invalid_layout
    hidden = %{placement.cell | children: [%{style: %{display: :none}, children: :bad}]}

    assert {:ok, %{heights: [12.0]}} =
             HtmlTableValidator.prepare_baseline_geometry(
               [%{row | cells: [%{placement | cell: hidden}]}],
               metrics,
               0.0
             )
  end

  test "ordinary and atomic percentage baseline descendants render" do
    for wrapper <- ["block", "inline-block"] do
      html =
        "<div style='display:#{wrapper}'><table style='width:120pt;height:80pt'><tr><td style='vertical-align:baseline'><div style='height:50%'>Percent</div></td><td style='vertical-align:baseline'>Peer</td></tr></table></div>"

      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
      assert text =~ "Percent" and text =~ "Peer"
    end
  end

  test "nested table baselines use their first row and exclude captions" do
    for caption_css <- ["", "caption-side:bottom"] do
      html = """
      <body style='margin:0;font-family:DejaVu Sans;font-size:9pt;line-height:12pt'>
      <div style='display:inline-block;width:240pt'><table style='width:240pt;border-collapse:collapse'><tr>
      <td style='vertical-align:baseline;padding:0'><table style='width:120pt;border-collapse:collapse'><caption style='#{caption_css}'>Caption</caption><tr><td style='vertical-align:baseline;font-size:8pt;line-height:12pt;padding:0'>Small</td><td style='vertical-align:baseline;font-size:14pt;line-height:18pt;padding:0'>Nested</td></tr><tr><td colspan=2 style='padding:0'>Later</td></tr></table></td>
      <td style='vertical-align:baseline;font-size:18pt;line-height:24pt;padding:0'>Peer</td>
      </tr></table></div></body>
      """

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, %{boxes: boxes}} = Layout.layout(styled, page_size: {300, 240}, margin: 0)
      nested = Enum.find(boxes, &(Map.get(&1, :text) == "Nested"))
      peer = Enum.find(boxes, &(Map.get(&1, :text) == "Peer"))
      caption = Enum.find(boxes, &(Map.get(&1, :text) == "Caption"))
      assert_in_delta nested.y, peer.y, 0.000001
      refute caption.y == peer.y
      refute Enum.any?(boxes, &Map.has_key?(&1, :_measured_baseline))
      refute Enum.any?(boxes, &(&1.type == :baseline_measurement))
    end
  end

  test "preserved whitespace produces a first flow baseline" do
    html =
      "<body style='margin:0;font-family:DejaVu Sans;font-size:9pt;line-height:12pt'><div style='display:inline-block'><table style='width:120pt;border-collapse:collapse'><tr><td style='vertical-align:baseline;padding:0;white-space:pre;font-size:18pt;line-height:24pt'>   </td><td style='vertical-align:baseline;padding:0;font-size:8pt;line-height:12pt'>Peer</td></tr></table></div></body>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, %{boxes: boxes}} = Layout.layout(styled, page_size: {180, 120}, margin: 0)
    spaces = Enum.find(boxes, &(Map.get(&1, :text) == "   "))
    peer = Enum.find(boxes, &(Map.get(&1, :text) == "Peer"))
    assert_in_delta spaces.y, peer.y, 0.000001
    assert peer.y < 110.0
    refute Enum.any?(boxes, &(&1.type == :baseline_measurement))
  end

  test "atomic table pagination reserves the full outer line and keeps mixed flow together" do
    html =
      File.read!(
        "test/fixtures/html_to_pdf/browser_parity/html_table_atomic_whitespace_baselines.html"
      )

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)
    assert {:ok, [first, second]} = Pagination.paginate(layout, margin: 12)

    for page <- [first, second] do
      [peer] = Enum.filter(page.boxes, &(Map.get(&1, :text) == "Peer small"))
      [marker] = Enum.filter(page.boxes, &(Map.get(&1, :role) == :atomic_table_line))
      assert peer.flow_id == marker.flow_id and peer.fragment_id == marker.fragment_id
      assert marker.width == 0.0 and marker.height > 30.0
      refute Map.has_key?(marker, :table_id)
    end

    assert Enum.any?(
             first.boxes,
             &(Map.get(&1, :text) == "Preserved spaces provide a first flow baseline")
           )

    refute Enum.any?(first.boxes, &(Map.get(&1, :text) == "End whitespace baseline document"))
    assert Enum.any?(second.boxes, &(Map.get(&1, :text) == "End whitespace baseline document"))
  end

  test "atomic captions headers rowspans and group heights retain semantic identities through pagination" do
    html = """
    <body style='margin:0;font-family:DejaVu Sans;font-size:9pt;line-height:12pt'>
    <p style='margin:0;height:72pt'>Before</p><div style='display:inline-block;width:180pt;vertical-align:top'><table style='width:180pt;border-collapse:collapse;margin:0 0 10pt'><caption>Caption</caption><thead><tr><th id=h scope=col style='padding:0;vertical-align:baseline'>Heading</th><th style='padding:0;vertical-align:baseline'>Other</th></tr></thead><tbody style='height:48pt'><tr><td headers=h rowspan=2 style='padding:0;vertical-align:baseline'>Span</td><td style='padding:0;vertical-align:baseline'>First</td></tr><tr><td style='padding:0;vertical-align:baseline'>Second</td></tr></tbody></table></div><p style='margin:0'>After</p></body>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)
    assert {:ok, [first, second]} = Pagination.paginate(layout, margin: 12)
    assert Enum.any?(first.boxes, &(Map.get(&1, :text) == "Before"))
    refute Enum.any?(first.boxes, &(Map.get(&1, :text) in ["Heading", "Caption", "Span"]))
    [heading] = Enum.filter(second.boxes, &(Map.get(&1, :text) == "Heading"))
    span = Enum.find(second.boxes, &(Map.get(&1, :text) == "Span"))
    assert span.table_cell.headers == [heading.table_cell.source]
    assert span.table_cell.rowspan == 2
    assert Enum.any?(second.boxes, &(Map.get(&1, :text) == "Caption"))
    assert Enum.any?(second.boxes, &(Map.get(&1, :text) == "After"))
    assert heading.flow_id == span.flow_id
  end

  test "atomic flow intent is validator-owned and excludes hidden and positioned tables" do
    for table_css <- ["display:none", "position:absolute"] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<body><div style='display:inline-block'><table style='#{table_css}'><tr><td>Excluded</td></tr></table>Visible</div></body>"
               )

      assert {:ok, %{children: [%{children: [atomic]}]}} = Style.compute_detailed(dom)
      refute Map.has_key?(atomic.style, :_atomic_table_flow)
    end

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<body><div style='display:inline-block'><table><tr><td style='vertical-align:baseline'>Cell</td></tr></table></div></body>"
             )

    assert {:ok, %{children: [body]} = styled} = Style.compute_detailed(dom)
    [atomic] = body.children
    assert atomic.style._atomic_table_flow == true
    missing = %{atomic | style: Map.delete(atomic.style, :_atomic_table_flow)}

    assert {:error, {:invalid_layout, diagnostic}} =
             Layout.layout(%{styled | children: [%{body | children: [missing]}]})

    assert diagnostic.stage == :layout and diagnostic.message =~ "in-flow table descendants"

    assert {:error, {:invalid_layout, diagnostic}} =
             Layout.layout(%{
               styled
               | children: [%{body | style: Map.put(body.style, :_atomic_table_flow, true)}]
             })

    assert diagnostic.message =~ "actual inline-block owner"
  end

  test "atomic table context does not turn generated list marker text into an ancestor" do
    html =
      "<body><div style='display:inline-block'><ul><li>Item</li></ul><table><tr><td style='vertical-align:baseline'>Cell</td></tr></table></div></body>"

    assert {:ok, _pdf} = HtmlToPdf.render(html)
  end

  test "prepared context cannot be moved out of its atomic ancestor" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div style='display:inline-block'><table><tr><td style='vertical-align:baseline'>Cell</td></tr></table></div>"
             )

    assert {:ok, %{children: [atomic]} = styled} = Style.compute_detailed(dom)
    altered = %{styled | children: atomic.children}
    assert {:error, {:invalid_layout, diagnostic}} = Layout.layout(altered)
    assert diagnostic.stage == :layout
    assert diagnostic.message =~ "actual inline-block ancestor"

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div style='display:inline-block'><table><tr><td>Cell</td></tr></table></div>"
             )

    assert {:ok, %{children: [atomic]} = styled} = Style.compute_detailed(dom)

    assert {:error, {:invalid_layout, diagnostic}} =
             Layout.layout(%{styled | children: atomic.children})

    assert diagnostic.stage == :layout and diagnostic.message =~ "actual inline-block ancestor"
  end

  test "oversized original atomic table retains all rows captions and links across physical slices" do
    html =
      File.read!(
        Paths.fixture("html_to_pdf/browser_parity/atomic_table_oversized_pagination.html")
      )

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, pages} = NativeElixirPdfUtilities.Split.by_page(pdf)
    assert length(pages) == 3
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)

    for label <- [
          "Before oversized",
          "Oversized atomic",
          "Heading",
          "Span A",
          "Span B",
          "Span C",
          "First row",
          "Second row",
          "Third row",
          "After oversized"
        ] do
      assert length(Regex.scan(Regex.compile!(label), text)) == 1
    end

    assert pdf =~ "/Subtype /Link"
  end

  test "atomic allocation uses one outer fragment and includes visible child overflow" do
    marker = %{
      type: :flow_marker,
      role: :atomic_table_line,
      x: 0.0,
      y: 0.0,
      width: 0.0,
      height: 100.0,
      flow_id: {:inline_flow, 0, 100},
      fragment_lines: true,
      fragment_id: {:line, {:inline_flow, 0, 100}, 0}
    }

    large_marker = %{marker | height: 200.0}

    assert :ok =
             HtmlTableValidator.validate_atomic_pagination(
               [marker],
               [large_marker, marker],
               100.0
             )

    next_line = %{marker | y: -100.0, fragment_id: {:line, marker.flow_id, 1}}
    unfragmented = marker |> Map.put(:fragment_lines, false) |> Map.delete(:fragment_id)

    assert :ok =
             HtmlTableValidator.validate_atomic_pagination([unfragmented], [unfragmented], 100.0)

    assert :ok =
             HtmlTableValidator.validate_atomic_pagination(
               [marker, next_line],
               [marker, next_line],
               100.0
             )

    assert :ok = HtmlTableValidator.validate_atomic_pagination([marker], [marker], 99.0)

    overflow = %{
      type: :rect,
      x: 0.0,
      y: -1.0,
      width: 10.0,
      height: 10.0,
      flow_id: marker.flow_id,
      fragment_id: marker.fragment_id,
      fill_color: {1, 0, 0},
      stroke_width: 0.0,
      stroke_color: nil,
      border_radius: 0.0
    }

    assert :ok =
             HtmlTableValidator.validate_atomic_pagination([marker, overflow], [marker], 100.0)

    assert :ok =
             HtmlTableValidator.validate_atomic_pagination(
               [marker, Map.put(overflow, :out_of_flow, true)],
               [marker],
               100.0
             )
  end

  test "atomic admission reports malformed metadata and geometry through the shared contract" do
    marker = %{
      type: :flow_marker,
      role: :atomic_table_line,
      x: 0.0,
      y: 0.0,
      width: 0.0,
      height: 10.0,
      flow_id: :line,
      fragment_lines: true,
      fragment_id: {:line, :line, 0}
    }

    for invalid <- [
          Map.delete(marker, :fragment_id),
          Map.put(marker, :fragment_lines, :bad),
          Map.put(marker, :width, 1.0)
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               HtmlTableValidator.validate_atomic_pagination([invalid], [invalid], 20.0)

      assert diagnostic.stage == :pagination and diagnostic.reason == :invalid_layout
      assert diagnostic.operation == :paginate and diagnostic.module == HtmlTableValidator
      assert diagnostic.source =~ "atomic table line"
      assert diagnostic.message =~ "geometry" or diagnostic.message =~ "marker"
    end

    for {boxes, markers, height} <- [
          {[marker | :bad], [marker], 20.0},
          {[marker], [marker | :bad], 20.0},
          {:bad, [marker], 20.0},
          {[:bad], [marker], 20.0},
          {[marker], [marker], 0.0}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               HtmlTableValidator.validate_atomic_pagination(boxes, markers, height)

      assert diagnostic.stage == :pagination and diagnostic.module == HtmlTableValidator
    end
  end

  test "atomic geometry admission reserves traversal before allocation" do
    alias NativeElixirPdfUtilities.{Limits}
    alias NativeElixirPdfUtilities.Validators.HtmlValidator

    marker = %{
      type: :flow_marker,
      role: :atomic_table_line,
      x: 0.0,
      y: 0.0,
      width: 0.0,
      height: 10.0,
      flow_id: :line,
      fragment_lines: true,
      fragment_id: {:line, :line, 0}
    }

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlValidator.reserve_render_resource(
                 :max_table_grid_work,
                 Limits.get(:max_table_grid_work) - 1,
                 :pagination
               )

               HtmlTableValidator.validate_atomic_pagination(
                 List.duplicate(marker, 8),
                 [marker],
                 20.0
               )
             end)

    assert diagnostic.stage == :pagination and diagnostic.message =~ "max_table_grid_work"
  end
end
