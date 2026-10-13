defmodule NativeElixirPdfUtilities.HtmlToPdf.NativeTemporalTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Text}

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    HtmlParser,
    Layout,
    Pagination,
    PageGeometry,
    PdfWriter,
    Style,
    TemporalArtwork
  }

  alias NativeElixirPdfUtilities.Validators.{
    FormValidator,
    HtmlFormValidator,
    HtmlTemporalValidator
  }

  alias NativeElixirPdfUtilities.Pdf.Reader

  test "date fields use US ordering, placeholders, chosen font and bounded year metrics" do
    for {attributes, expected} <- [
          {"value=2026-10-13", "10/13/2026"},
          {"value=2000-02-29", "02/29/2000"},
          {"value=invalid", "mm/dd/yyyy"}
        ] do
      node = temporal_node("<input type=date #{attributes}>")
      assert Enum.map_join(node.style._temporal_display.parts, & &1.text) == expected
      assert node.style.font_face.family == "DejaVu Sans Mono"
      assert node.style.font_size == 10
      assert node.style._temporal_display.picker_visible
      assert node.style._temporal_picker_image.width > 0
      assert Enum.all?(node.style._temporal_display.parts, &(&1.maximum_width >= &1.width))
    end

    default = temporal_node("<input type=date value=2026-10-13>")
    bounded = temporal_node("<input type=date value=2026-10-13 min=2000-01-01 max=2030-12-31>")
    assert bounded.style.width < default.style.width
    assert HtmlTemporalValidator.validate_initial_layout(bounded) == :ok
  end

  test "time precision follows value, minimum and step, with correct noon and midnight" do
    for {attributes, expected} <- [
          {"", "--:-- --"},
          {"value=00:00", "12:00 AM"},
          {"value=12:00", "12:00 PM"},
          {"value=23:59", "11:59 PM"},
          {"value=13:02:03.100", "01:02:03.100 PM"},
          {"value=13:02 min=00:00:01", "01:02:00 PM"},
          {"value=13:02 step=.1", "01:02:00.000 PM"},
          {"value=13:02 step=any", "01:02 PM"},
          {"step=.1", "--:--:--.--- --"}
        ] do
      node = temporal_node("<input type=time #{attributes}>")
      assert Enum.map_join(node.style._temporal_display.parts, & &1.text) == expected
      assert node.style._temporal_display.indicator_margin == 6
      assert node.style._temporal_display.indicator_content_size == 10.5
    end
  end

  test "temporal UA line height resets inheritance while author metrics and paint win" do
    normal = temporal_node("<div style='line-height:40pt'><input type=date></div>")

    explicit =
      temporal_node(
        "<input type=date style='line-height:30pt;font-size:18pt;color:red;background:blue;border:2pt solid green;padding:3pt'>"
      )

    assert normal.style.line_height < 40
    assert explicit.style.line_height == 30
    assert explicit.style.font_size == 18
    assert explicit.style.color == {1.0, 0.0, 0.0}
    assert explicit.style.background_color == {0.0, 0.0, 1.0}
    refute explicit.style._native_menu
    assert explicit.style.padding.left == 3
    assert explicit.style._temporal_display.indicator_content_size == 18

    assert {:ok, _} =
             HtmlToPdf.render(
               "<input type=time style='appearance:none;height:6pt;width:20pt;border-radius:3pt'>",
               forms: :static
             )
  end

  test "readonly and disabled fields retain indicator geometry while hiding source art" do
    visible = temporal_node("<input type=date value=2026-10-13>")

    for attributes <- ["readonly", "disabled"] do
      node = temporal_node("<input type=date #{attributes} value=2026-10-13>")
      refute node.style._temporal_display.picker_visible
      refute Map.has_key?(node.style, :_temporal_picker_image)
      assert node.style.width == visible.style.width
      boxes = TemporalArtwork.boxes(node.style, 0, 0, 20, node.style.height + 3)
      assert [%{type: :glyph_group, actual_text: "2026-10-13", boxes: fields}] = boxes
      assert Enum.all?(fields, &(&1.clip.width == 3.25390625))
      assert Enum.all?(fields, &(&1.clip.border_radius == 0.0))
    end

    calendar = File.read!("priv/html_to_pdf/temporal/calendar_icon.svg")
    clock = File.read!("priv/html_to_pdf/temporal/time_icon.svg")

    assert TemporalArtwork.picker_source("date") ==
             String.replace(calendar, "WindowText", "#000000")

    assert TemporalArtwork.picker_source("time") == String.replace(clock, "WindowText", "#000000")
    assert File.read!("priv/html_to_pdf/temporal/LICENSE.chromium") =~ "The Chromium Authors"
  end

  test "static logical text and readonly scalar fields retain canonical HTML values" do
    html =
      "<input type=date readonly name=date value=2026-10-13><input type=time disabled name=time value=13:02:03.100>"

    assert {:ok, static} = HtmlToPdf.render(html, forms: :static)
    assert {:ok, text} = Text.extract(static, layout: false)
    assert text =~ "2026-10-13" and text =~ "13:02:03.100"
    refute text =~ "10/13/2026"
    assert {:ok, context} = Reader.read_validated(static)
    assert {:ok, %{fields: []}} = FormValidator.inspect_document(context)

    assert {:ok, interactive} = HtmlToPdf.render(html, forms: :interactive)
    assert {:ok, context} = Reader.read_validated(interactive)
    assert {:ok, %{fields: fields}} = FormValidator.inspect_document(context)
    assert Enum.map(fields, & &1.value) == ["2026-10-13", "13:02:03.100"]
    assert Enum.all?(fields, &(Bitwise.band(&1.flags, 1) == 1))
    assert {:ok, _} = Forms.flatten(interactive)
  end

  test "layout rejects forged segmented and canonical states before paint" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<input type=date readonly value=2026-10-13 style=display:block>"
             )

    assert {:ok, %{children: [node]} = styled} = Style.compute_detailed(dom, forms: :static)

    for forged <- [
          put_in(node.style._temporal_display.parts, []),
          put_in(node.style._temporal_state.value, 0),
          put_in(node.style._temporal_disabled, true),
          %{node | style: Map.delete(node.style, :_temporal_display)}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               Layout.layout(%{styled | children: [forged]})

      assert diagnostic.stage == :layout and diagnostic.operation == :layout
      assert diagnostic.source == "temporal" and diagnostic.message != ""
    end

    assert {:ok, layout} = Layout.layout(styled)
    assert {:ok, pages} = Pagination.paginate(layout)

    assert {:error, {:unsupported_form, diagnostic}} =
             PdfWriter.render(pages, forms: :interactive)

    assert diagnostic.source == "temporal" and diagnostic.message =~ "recompute styles"
    assert {:ok, _, []} = HtmlFormValidator.prepare(pages, forms: :static)
  end

  test "layout requires decoded picker resources and writer-owned font proof" do
    node = temporal_node("<input type=date value=2026-10-13>")
    missing = %{node | style: Map.delete(node.style, :_temporal_picker_image)}

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlTemporalValidator.validate_initial_layout(missing)

    assert diagnostic.stage == :layout and diagnostic.message =~ "picker resources"
    invalid = put_in(node.style._temporal_picker_image, %{})

    assert {:error, {:invalid_pdf_input, diagnostic}} =
             HtmlTemporalValidator.validate_initial_layout(invalid)

    assert diagnostic.stage == :pdf and diagnostic.message != ""
    control = node.style._form_control
    invalid_font = %{node.style | font_face: nil}

    assert {:error, {:invalid_pdf_input, diagnostic}} =
             HtmlTemporalValidator.prepare_display(
               invalid_font,
               control.initial_state,
               control.attributes,
               :static
             )

    assert diagnostic.stage == :pdf and diagnostic.message != ""
  end

  test "unsupported editing, locale and native date range return actionable public diagnostics" do
    assert {:error, {:unsupported_form, diagnostic}} = HtmlToPdf.render("<input type=date>")
    assert diagnostic.stage == :forms and diagnostic.source == "temporal"

    for html <- [
          "<div lang=en-GB><input type=date></div>",
          "<input type=time lang=fr>",
          "<input type=date value=275761-01-01>"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html, forms: :static)
      assert diagnostic.stage == :style and diagnostic.operation == :compute
      assert diagnostic.source in ["date", "time"] and diagnostic.message != ""
    end

    assert {:ok, _} = HtmlToPdf.render("<input type=date lang=fr style=display:none>")
    assert {:ok, _} = HtmlToPdf.render("<input type=date value=275760-09-13>", forms: :static)

    assert {:error, {:unsupported_html, _}} =
             HtmlToPdf.render("<input type=date value=275760-09-14>", forms: :static)

    assert {:ok, _} = HtmlToPdf.render("<input type=time lang=en-US>", forms: :static)
  end

  test "symbolic months and ISO weeks retain native literals, placeholders and bounded year sizing" do
    for {kind, attributes, expected} <- [
          {"month", "value=2026-10", "October 2026"},
          {"month", "", "--------- ----"},
          {"week", "value=2020-W53", "Week 53, 2020"},
          {"week", "value=bad", "Week --, ----"}
        ] do
      node = temporal_node("<input type=#{kind} #{attributes}>")
      assert Enum.map_join(node.style._temporal_display.parts, & &1.text) == expected
      assert node.style._temporal_display.picker_visible
      assert node.style._temporal_picker_image.width > 0
      assert HtmlTemporalValidator.validate_initial_layout(node) == :ok
    end

    january = temporal_node("<input type=month value=2026-01>")
    september = temporal_node("<input type=month value=2026-09>")
    empty = temporal_node("<input type=month>")
    assert january.style.width == september.style.width
    assert empty.style.width == september.style.width

    assert hd(january.style._temporal_display.parts).width <
             hd(september.style._temporal_display.parts).width

    bounded = temporal_node("<input type=week min=2000-W01 max=2030-W01>")
    assert bounded.style.width < temporal_node("<input type=week>").style.width
  end

  test "local datetime combines date and clock fields while canonical value keeps normalized ISO transport" do
    for {attributes, expected, value} <- [
          {"", "mm/dd/yyyy, --:-- --", ""},
          {"value=2026-10-13T13:02", "10/13/2026, 01:02 PM", "2026-10-13T13:02"},
          {"value='2026-10-13 00:02:03.100'", "10/13/2026, 12:02:03.100 AM",
           "2026-10-13T00:02:03.1"},
          {"value=2026-10-13T13:02 min=2026-10-13T00:00:01", "10/13/2026, 01:02:00 PM",
           "2026-10-13T13:02"},
          {"step=.1", "mm/dd/yyyy, --:--:--.--- --", ""}
        ] do
      node = temporal_node("<input type=datetime-local #{attributes}>")
      display = node.style._temporal_display
      assert Enum.map_join(display.parts, & &1.text) == expected
      assert display.value == value
      assert display.indicator_margin == 0
      assert HtmlTemporalValidator.validate_initial_layout(node) == :ok
    end
  end

  test "disabled field color comes from the prepared cascade and indicator geometry retains LayoutUnit fractions" do
    for kind <- ~w(date month week time datetime-local) do
      disabled = temporal_node("<input type=#{kind} disabled>")
      authored = temporal_node("<input type=#{kind} disabled style=color:red>")

      assert Enum.all?(
               disabled.style._temporal_display.parts,
               &(&1.color == {84 / 255, 84 / 255, 84 / 255})
             )

      assert Enum.all?(authored.style._temporal_display.parts, &(&1.color == {1.0, 0.0, 0.0}))
      refute authored.style._temporal_display.picker_visible

      if kind != "time" do
        assert authored.style._temporal_display.indicator_content_size == 9.99609375
        assert authored.style._temporal_display.intrinsic_height == 12.99609375
      end

      forged =
        put_in(authored.style._temporal_display.parts, [
          %{hd(authored.style._temporal_display.parts) | color: {0, 0, 0}}
        ])

      assert {:error, {:invalid_layout, %{source: "temporal"}}} =
               HtmlTemporalValidator.validate_initial_layout(forged)
    end
  end

  test "all readonly temporal types expose canonical scalar values and static ActualText" do
    values = [
      {"month", "2026-10"},
      {"week", "2020-W53"},
      {"datetime-local", "2026-10-13T13:02:03.1"}
    ]

    html =
      Enum.map_join(values, fn {kind, value} ->
        "<input type=#{kind} readonly name=#{kind} value=#{value}>"
      end)

    assert {:ok, pdf} = HtmlToPdf.render(html, forms: :static)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    for {_kind, value} <- values, do: assert(text =~ value)
    refute text =~ "October" or text =~ "Week"
    assert {:ok, pdf} = HtmlToPdf.render(html, forms: :interactive)
    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: fields}} = FormValidator.inspect_document(context)
    assert Enum.map(fields, & &1.value) == Enum.map(values, &elem(&1, 1))
    assert Enum.all?(fields, &(Bitwise.band(&1.flags, 1) == 1))
    assert {:ok, _} = Forms.flatten(pdf)
  end

  test "native month week and local date boundaries fail with shared diagnostics beyond Chromium's fixed range" do
    for {kind, admitted, excluded} <- [
          {"month", "275760-09", "275760-10"},
          {"week", "275760-W37", "275760-W38"},
          {"datetime-local", "275760-09-13T00:00", "275760-09-13T00:01"}
        ] do
      assert {:ok, _} = HtmlToPdf.render("<input type=#{kind} value=#{admitted}>", forms: :static)

      for attribute <- ~w(value min max) do
        assert {:error, {:unsupported_html, diagnostic}} =
                 HtmlToPdf.render("<input type=#{kind} #{attribute}=#{excluded}>", forms: :static)

        assert diagnostic.stage == :style and diagnostic.operation == :compute
        assert diagnostic.source == kind and diagnostic.message =~ "fixed native date range"
      end

      assert {:ok, _} =
               HtmlToPdf.render("<input type=#{kind} value=#{excluded} style=display:none>")

      assert {:error, {:unsupported_html, %{source: ^kind}}} =
               HtmlToPdf.render("<input type=#{kind} lang=fr>", forms: :static)
    end
  end

  test "rounded temporal CSS hosts reuse snapped outer and inner contours without changing select roles" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<html><body><input type=month readonly value=2026-09 style='display:block;width:40pt;border-radius:3pt'></body></html>"
             )

    assert {:ok, styled} =
             Style.compute_detailed(dom, forms: :static, system_font_discovery: false)

    assert {:ok, layout} = Layout.layout(styled)
    host = Enum.find(layout.boxes, &(Map.get(&1, :role) == :temporal_host))
    assert host.border_radius == 3

    assert [%{border_radius: 3.0}, %{border_radius: 1.5}] =
             PageGeometry.rounded_border_contours(host, 300)

    assert Enum.all?(Map.values(host.border_styles), &(&1 == :inset))
    assert {:ok, pages} = Pagination.paginate(layout)
    assert {:ok, _} = PdfWriter.render(pages, forms: :static)
  end

  test "unimplemented rounded temporal border shapes return a public style diagnostic" do
    for border <- ["border-left-width:3pt", "border-style:double", "border-style:dashed"] do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render("<input type=month style='border-radius:3pt;#{border}'>",
                 forms: :static
               )

      assert diagnostic.stage == :style and diagnostic.operation == :compute
      assert diagnostic.source == "month" and diagnostic.message =~ "uniform border"
    end

    assert {:ok, _} =
             HtmlToPdf.render("<input type=month style='border-radius:3pt;border:none'>",
               forms: :static
             )
  end

  defp temporal_node(html) do
    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, styled} =
             Style.compute_detailed(dom, forms: :static, system_font_discovery: false)

    nodes =
      Stream.iterate([styled], &Enum.flat_map(&1, fn node -> Map.get(node, :children, []) end))
      |> Enum.take_while(&(&1 != []))
      |> List.flatten()

    Enum.find(nodes, &(Map.get(&1, :tag) == "input"))
  end
end
