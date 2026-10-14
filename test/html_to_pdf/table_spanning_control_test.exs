defmodule NativeElixirPdfUtilities.HtmlToPdf.TableSpanningControlTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.{FormValidator, HtmlFormValidator, HtmlTableValidator}

  defp source(controls) do
    before = Enum.map_join(1..10, "<br>", &"Before#{&1}")
    after_control = Enum.map_join(1..10, "<br>", &"After#{&1}")

    """
    <style>html,body{margin:0}body{font-family:'DejaVu Sans';font-size:9pt;line-height:12pt}
    table{width:216pt;table-layout:fixed;border-collapse:collapse}td{padding:3pt;border:1px solid #789;vertical-align:top}
    input,select{font-family:'DejaVu Sans';font-size:9pt}p{margin:0}</style>
    <form id='owner'><table><tr><td rowspan='4'>#{before}<br>#{controls}<br>#{after_control}</td><td>First</td></tr>
    <tr><td>Second</td></tr><tr><td>Third</td></tr><tr><td>Fourth</td></tr></table></form>
    """
  end

  test "spanning text control keeps one actual page widget and genuine filled appearance" do
    html = source("<input id='field' name='field' value='Value' style='width:70pt;height:18pt'>")
    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 160}, margin: 12)
    assert {:ok, [%{name: "field", value: "Value", widgets: [widget]}]} = Forms.fields(pdf)
    assert widget.page == 2
    [left, bottom, right, top] = widget.rect
    assert left >= 12 and right <= 228 and bottom >= 12 and top <= 148
    assert_in_delta right - left, 76, 0.01
    assert_in_delta top - bottom, 22.5, 0.01
    assert {:ok, flattened} = Forms.flatten(pdf)
    assert {:ok, text} = Text.extract(flattened, layout: false)
    assert length(Regex.scan(~r/Value/, text)) == 1

    for index <- 1..10 do
      assert length(Regex.scan(~r/Before#{index}(?!\d)/, text)) == 1
      assert length(Regex.scan(~r/After#{index}(?!\d)/, text)) == 1
    end

    assert {:ok, changed} = Forms.fill(pdf, %{"field" => "Changed"})
    assert {:ok, flattened} = Forms.flatten(changed)
    assert {:ok, text} = Text.extract(flattened, layout: false)
    assert text =~ "Changed"
    refute text =~ "Value"
  end

  test "an oversized spanning widget uses one real clipped appearance and advances its whole atomic source" do
    html = source("<input name='tall' value='Tall' style='width:70pt;height:150pt'>")
    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 160}, margin: 12)
    assert {:ok, [%{name: "tall", widgets: [widget]}]} = Forms.fields(pdf)
    [left, bottom, right, top] = widget.rect
    assert left >= 12 and right <= 228 and bottom >= 12 and top <= 148
    assert_in_delta top - bottom, 136, 0.01
    assert {:ok, flattened} = Forms.flatten(pdf)
    assert {:ok, text} = Text.extract(flattened, layout: false)
    assert length(Regex.scan(~r/Tall/, text)) == 1
    assert text =~ "After10"
  end

  test "a deferred spanning control target resolves to its actual widget page" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, ResourceContext}
    alias NativeElixirPdfUtilities.Validators.HtmlValidator

    html =
      source("<input id='field' name='field' value='Value' style='width:70pt;height:18pt'>") <>
        "<p><a href='#field'>Jump</a></p>"

    {:ok, dom} = HtmlParser.parse_document_detailed(html)
    budget = HtmlValidator.new_image_budget()

    for mode <- [:interactive, :static] do
      assert :ok ==
               ResourceContext.run([], budget, fn _ ->
                 assert {:ok, prepared} =
                          HtmlToPdf.prepare_document(
                            dom,
                            [page_size: {240, 160}, margin: 12, forms: mode],
                            budget
                          )

                 jump =
                   prepared.pages
                   |> Enum.flat_map(& &1.boxes)
                   |> Enum.find(&(Map.get(&1, :text) == "Jump"))

                 assert {:xyz, 2, _x, _y} = jump.link_destination
                 :ok
               end)
    end
  end

  test "root and spanning radio members preserve owner selection required flags and state appearances" do
    controls =
      "<input type='radio' name='group' value='span' required checked><input type='checkbox' name='tick' checked>"

    html =
      String.replace(
        source(controls),
        "<form id='owner'>",
        "<form id='owner'><input type='radio' name='group' value='root' checked>"
      )

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 160}, margin: 12)
    assert {:ok, fields} = Forms.fields(pdf)
    assert Map.new(fields, &{&1.name, &1.value}) == %{"group" => "span", "tick" => true}
    assert length(Enum.find(fields, &(&1.name == "group")).widgets) == 2
    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: inspected}} = FormValidator.inspect_document(context)
    assert Bitwise.band(Enum.find(inspected, &(&1.name == "group")).flags, 2) == 2
    assert {:ok, changed} = Forms.fill(pdf, %{"group" => "root", "tick" => false})
    assert {:ok, fields} = Forms.fields(changed)
    assert Map.new(fields, &{&1.name, &1.value}) == %{"group" => "root", "tick" => false}
    assert {:ok, _flattened} = Forms.flatten(changed)
  end

  test "spanning disabled paint and static mode preserve source artwork without fields" do
    for controls <- [
          "<input name='field' value='Static' style='width:70pt;height:18pt'>",
          "<input name='field' value='Static' disabled style='width:70pt;height:18pt'>"
        ] do
      assert {:ok, pdf} =
               HtmlToPdf.render(source(controls),
                 page_size: {240, 160},
                 margin: 12,
                 forms: :static
               )

      assert {:ok, []} = Forms.fields(pdf)
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert length(Regex.scan(~r/Static/, text)) == 1
    end
  end

  test "disabled controls retain readonly appearances and generated names stay distinct" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               source("<input value='Disabled' disabled style='width:70pt;height:18pt'>"),
               page_size: {240, 160},
               margin: 12
             )

    assert {:ok, [%{value: "Disabled"}]} = Forms.fields(pdf)
    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: [disabled]}} = FormValidator.inspect_document(context)
    assert Bitwise.band(disabled.flags, 1) == 1
    assert {:ok, flattened} = Forms.flatten(pdf)
    assert {:ok, text} = Text.extract(flattened, layout: false)
    assert length(Regex.scan(~r/Disabled/, text)) == 1

    html =
      String.replace(
        source("<input value='Span'>"),
        "<form id='owner'>",
        "<form id='owner'><input value='Root'>"
      )

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 160}, margin: 12)
    assert {:ok, fields} = Forms.fields(pdf)
    assert length(fields) == 2
    assert length(Enum.uniq_by(fields, & &1.name)) == 2
    assert MapSet.new(Enum.map(fields, & &1.value)) == MapSet.new(["Root", "Span"])
  end

  test "spanning choice controls preserve their actual source-page appearance frame" do
    html =
      source(
        "<select name='choice' size='2' style='width:70pt;height:36pt'><option value='a'>Alpha</option><option value='b' selected>Beta</option></select>"
      )

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 160}, margin: 12)
    assert {:ok, [%{name: "choice", value: "b", widgets: [%{page: 2}]}]} = Forms.fields(pdf)
    assert {:ok, flattened} = Forms.flatten(pdf)
    assert {:ok, text} = Text.extract(flattened, layout: false)
    assert text =~ "Beta"
    assert {:ok, changed} = Forms.fill(pdf, %{"choice" => "a"})
    assert {:ok, [%{value: "a"}]} = Forms.fields(changed)
    assert {:ok, _flattened} = Forms.flatten(changed)
  end

  test "crossing cells obey canonical duplicate-name diagnostics" do
    html =
      String.replace(
        source("<input name='same' value='Span'>"),
        "<form id='owner'>",
        "<form id='owner'><input name='same' value='Root'>"
      )

    assert {:error, {:invalid_form, diagnostic}} =
             HtmlToPdf.render(html, page_size: {240, 160}, margin: 12)

    assert diagnostic.stage == :forms and diagnostic.message =~ "duplicate HTML name"
  end

  test "native text control defaults keep one inline editor baseline and authored borders override appearance" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}

    html =
      "<style>html,body{margin:0}body{font-family:DejaVu Sans;font-size:9pt;line-height:12pt}input{width:70pt;height:18pt;font-family:DejaVu Sans;font-size:9pt}</style><div>Before<br><input value='Value'><br>After</div>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 160}, margin: 12)
    before = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Before"))
    after_control = Enum.find(layout.boxes, &(Map.get(&1, :text) == "After"))
    host = Enum.find(layout.boxes, &is_map(Map.get(&1, :form_control)))
    assert host.role == :native_menu
    assert_in_delta before.y - after_control.y, 34.5, 0.01
    assert_in_delta host.height, 22.5, 0.01

    assert {:ok, dom} =
             HtmlParser.parse_document_detailed(
               String.replace(html, "width:70pt;", "border:1px solid #789;width:70pt;")
             )

    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled)
    host = Enum.find(layout.boxes, &is_map(Map.get(&1, :form_control)))
    refute Map.get(host, :role) == :native_menu
  end

  test "small and tall authored source fields keep one bounded widget and genuine filled paint" do
    for height <- [0, 6, 18, 150] do
      html = source("<input name='size' value='A' style='width:20pt;height:#{height}pt'>")
      assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 160}, margin: 12)

      assert {:ok, [%{name: "size", widgets: [%{rect: [left, bottom, right, top]}]}]} =
               Forms.fields(pdf)

      assert left >= 12 and right <= 228 and bottom >= 12 and top <= 148
      assert_in_delta right - left, 26, 0.01
      assert_in_delta top - bottom, min(height + 4.5, 136), 0.01
      assert {:ok, changed} = Forms.fill(pdf, %{"size" => "B"})
      assert {:ok, flattened} = Forms.flatten(changed)
      assert {:ok, []} = Forms.fields(flattened)
      assert {:ok, text} = Text.extract(flattened, layout: false)
      assert length(Regex.scan(~r/(?<![A-Za-z])B(?![A-Za-z])/, text)) == 1

      for index <- 1..10 do
        assert length(Regex.scan(Regex.compile!("Before#{index}(?![0-9])"), text)) == 1
        assert length(Regex.scan(Regex.compile!("After#{index}(?![0-9])"), text)) == 1
      end
    end
  end

  test "authored spanning widow counts choose real source pages without changing widget geometry" do
    original =
      File.read!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/html_table_spanning_controls_long.html",
          __DIR__
        )
      )

    for widows <- [1, 2, 3] do
      html = String.replace(original, "td{height:", "td{widows:#{widows};height:")
      assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 160}, margin: 12)
      assert {:ok, %{pages: [_, second, third]}} = Text.extract_spans(pdf)
      second_lines = Enum.map(second.spans, & &1.text)
      third_lines = Enum.map(third.spans, & &1.text)
      assert "Line19" in second_lines == (widows == 1)
      assert "Line19" in third_lines == widows >= 2
      assert "Line18" in third_lines == (widows == 3)
      assert "Line20" in third_lines

      assert {:ok, [%{name: "field", widgets: [%{page: 2, rect: [_, bottom, _, top]}]}]} =
               Forms.fields(pdf)

      assert top <= 148 and bottom >= 12
      assert_in_delta top - bottom, 22.5, 0.01
      assert {:ok, text} = Text.extract(pdf, layout: false)

      for index <- 1..20,
          do: assert(length(Regex.scan(Regex.compile!("Line#{index}(?![0-9])"), text)) == 1)
    end
  end

  test "a source target on a deferred widow line resolves on its genuine painted page" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, ResourceContext}
    alias NativeElixirPdfUtilities.Validators.HtmlValidator

    html =
      File.read!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/html_table_spanning_controls_long.html",
          __DIR__
        )
      )
      |> String.replace("Line19<br>", "<span id='tail'>Line19</span><br>")

    html = String.replace(html, "</form>", "<p><a href='#tail'>Tail jump</a></p></form>")
    {:ok, dom} = HtmlParser.parse_document_detailed(html)
    budget = HtmlValidator.new_image_budget()

    for mode <- [:interactive, :static] do
      assert :ok ==
               ResourceContext.run([], budget, fn _ ->
                 assert {:ok, prepared} =
                          HtmlToPdf.prepare_document(
                            dom,
                            [page_size: {240, 160}, margin: 12, forms: mode],
                            budget
                          )

                 jump =
                   prepared.pages
                   |> Enum.flat_map(& &1.boxes)
                   |> Enum.find(&(Map.get(&1, :text) == "Tail jump"))

                 assert {:xyz, 3, _, _} = jump.link_destination
                 :ok
               end)
    end
  end

  test "authored ordinary cell orphans defer the genuine first lines from a partially used page" do
    lines = Enum.map_join(1..25, "<br>", &"Start#{&1}")

    for orphans <- [1, 2, 3] do
      html =
        "<style>html,body{margin:0}body{font-family:'DejaVu Sans';font-size:9pt;line-height:12pt}table{width:216pt;border-collapse:collapse}td{orphans:#{orphans};widows:1;padding:3pt;border:1px solid #789}</style><p style='height:110pt;margin:0'></p><table><tr><td>#{lines}</td></tr></table>"

      assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 160}, margin: 12)
      assert {:ok, %{pages: [first | _]}} = Text.extract_spans(pdf)
      assert Enum.any?(first.spans, &(&1.text == "Start1")) == (orphans == 1)
      assert {:ok, text} = Text.extract(pdf, layout: false)

      for index <- 1..25,
          do: assert(length(Regex.scan(Regex.compile!("Start#{index}(?![0-9])"), text)) == 1)
    end
  end

  test "an impossible orphan reservation relaxes on an empty page while retaining every real line" do
    lines = Enum.map_join(1..25, "<br>", &"Retain#{&1}")

    html =
      "<style>html,body{margin:0}body{font-family:'DejaVu Sans';font-size:9pt;line-height:12pt}td{orphans:30;widows:30;padding:3pt}</style><table><tr><td>#{lines}</td></tr></table>"

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {240, 160}, margin: 12)
    assert {:ok, text} = Text.extract(pdf, layout: false)

    for index <- 1..25,
        do: assert(length(Regex.scan(Regex.compile!("Retain#{index}(?![0-9])"), text)) == 1)

    assert {:ok, %{pages: pages}} = Text.extract_spans(pdf)
    assert length(pages) == 3
  end

  test "source paragraph count failures use the same canonical pagination diagnostic" do
    alias NativeElixirPdfUtilities.Validators.HtmlValidator

    for value <- [nil, %{widows: 0}, %{orphans: 1.5}] do
      assert {:error, {:invalid_layout, diagnostic}} =
               HtmlValidator.validate_paragraph_line_counts(value, :pagination)

      assert diagnostic.stage == :pagination and diagnostic.source == "orphans/widows"
      assert diagnostic.operation == :paginate
    end
  end

  test "fractional print source slack keeps the actual clip and cannot skip a tiny atomic unit" do
    table = {:table, 0.0, 100.0}

    surface = %{
      type: :rect,
      role: :table_cell_background,
      x: 0.0,
      y: 0.0,
      width: 80.0,
      height: 100.0,
      fill_color: {1, 1, 1},
      stroke_width: 0.0,
      border_radius: 0.0,
      stroke_color: nil,
      table_id: table,
      flow_id: {:table_row, table, 0}
    }

    assert {:ok, [fragment], 25.0} =
             HtmlTableValidator.prepare_row_page_fragment([surface], 100.0, 0.0, 25.0, 25.0)

    assert fragment.height == 25.0 and fragment.clip.height == 25.0
    assert fragment.table_fragment_source_gap == 0.5
    atomic = %{surface | role: :source_atomic, y: 74.7, width: 1.0, height: 0.1}

    assert {:ok, [fragment], 25.0} =
             HtmlTableValidator.prepare_row_page_fragment(
               [surface, atomic],
               100.0,
               0.0,
               25.0,
               25.0
             )

    assert fragment.table_fragment_source_gap == 0.0

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlTableValidator.prepare_row_page_fragment([surface], 100.0, 0.0, 25.0, 24.0)

    assert diagnostic.stage == :pagination and diagnostic.source == "table row fragment"
  end

  test "forms mode restores nested scope and canonical failures stay actionable" do
    assert HtmlFormValidator.layout_mode() == :interactive

    assert :static ==
             HtmlFormValidator.with_layout_mode(:static, fn ->
               assert :interactive ==
                        HtmlFormValidator.with_layout_mode(
                          :interactive,
                          &HtmlFormValidator.layout_mode/0
                        )

               HtmlFormValidator.layout_mode()
             end)

    assert HtmlFormValidator.layout_mode() == :interactive

    assert_raise RuntimeError, fn ->
      HtmlFormValidator.with_layout_mode(:static, fn -> raise "scope" end)
    end

    assert HtmlFormValidator.layout_mode() == :interactive

    assert {:error, {:invalid_options, diagnostic}} =
             HtmlFormValidator.with_layout_mode(:invalid, fn ->
               flunk("invalid callback executed")
             end)

    assert diagnostic.stage == :forms and diagnostic.source == "forms"

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlTableValidator.prepare_cell_fields(:invalid, %{}, nil, :interactive)

    assert diagnostic.stage == :layout and diagnostic.source == "table cell controls"

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlTableValidator.prepare_cell_overflow_fragments(
               [],
               [],
               %{make_ref() => -1.0},
               148.0,
               12.0
             )

    assert diagnostic.stage == :pagination and diagnostic.message =~ "nonnegative consumed"

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlTableValidator.prepare_cell_page_fragments([:invalid], %{}, 12.0)

    assert diagnostic.stage == :pagination and diagnostic.message =~ "proper page boxes"

    assert {:error, {_, diagnostic}} =
             HtmlTableValidator.prepare_cell_overflow_fragments(
               [%{table_span_fragment: %{identity: make_ref()}}],
               [],
               %{},
               148.0,
               12.0
             )

    assert is_binary(diagnostic.message)
  end
end
