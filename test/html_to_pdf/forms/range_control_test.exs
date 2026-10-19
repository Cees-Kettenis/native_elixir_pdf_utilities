alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.RangeControlTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    HtmlParser,
    Style,
    Layout,
    Pagination,
    RangeArtwork,
    PdfWriter
  }

  alias NativeElixirPdfUtilities.Validators.{HtmlFormValidator, HtmlValidator, FormValidator}
  alias NativeElixirPdfUtilities.Pdf.Reader

  test "Range defaults, clamping, step ties and lexical state preserve authored attributes" do
    for {attributes, expected} <- [
          {%{}, "50"},
          {%{"value" => "invalid"}, "50"},
          {%{"value" => ""}, "50"},
          {%{"value" => "-10"}, "0"},
          {%{"value" => "150"}, "100"},
          {%{"min" => "0", "max" => "100", "step" => "20", "value" => "50"}, "60"},
          {%{"min" => "-1", "max" => "1", "step" => ".2", "value" => "-.5"}, "-0.4"},
          {%{"min" => "0.1", "max" => "1", "step" => ".2", "value" => ".4"}, "0.5"},
          {%{"min" => "9", "max" => "2"}, "9"},
          {%{"min" => "9", "max" => "2", "value" => "20", "step" => "any"}, "20"},
          {%{"min" => "0", "max" => "1", "value" => "0.12300", "step" => "ANY"}, "0.123"},
          {%{"value" => "01e1"}, "10"},
          {%{"value" => "-0"}, "0"},
          {%{"min" => "0", "max" => "1", "step" => "3", "value" => "1"}, "0"},
          {%{"max" => "0.1", "step" => "100", "value" => "-5"}, "0"},
          {%{"min" => "bad", "max" => "bad", "step" => "0"}, "50"},
          {%{"step" => "-2"}, "50"},
          {%{"step" => "bad"}, "50"},
          {%{"min" => "1e21", "max" => "1e22", "step" => "any", "value" => "1e21"}, "1e+21"},
          {%{"min" => "1e-8", "max" => "1e-7", "step" => "any", "value" => "1e-8"}, "1e-8"},
          {%{"min" => "-1e308", "max" => "1e308", "step" => "any"}, "0"},
          {%{"min" => "0", "max" => "-1", "step" => "1e308", "value" => "1.7e308"}, "1e+308"}
        ] do
      original = Map.put(attributes, "type", "range")

      doc = %{
        type: :document,
        children: [%{type: :element, tag: "input", attributes: original, children: []}]
      }

      assert {:ok, %{children: [node]}} = Style.compute_detailed(doc)
      state = node.style._form_control.initial_state
      assert state.value == expected, inspect(attributes)
      assert state.display == expected
      assert state.range.value == elem(HtmlValidator.parse_html_float(expected), 1)
      assert node.style._form_control.attributes == original
      assert state.range.position >= 0 and state.range.position <= 1
      assert HtmlFormValidator.valid_initial_range?(state, original)
    end

    state = HtmlFormValidator.prepare_initial_range(%{"value" => "1.2", "step" => ".5"})
    assert state.range.step_base == 1.2 and state.value == "1.2"

    state =
      HtmlFormValidator.prepare_initial_range(%{
        "min" => " +.5suffix",
        "max" => "2tail",
        "step" => ".5tail"
      })

    assert state.range.min == 0.5 and state.range.max == 2 and state.range.step == 0.5
  end

  test "step alignment preserves finite IEEE values for reversed bounds" do
    assert {:ok, _} =
             HtmlToPdf.render("<input type=range min=0 max=-1 step=1e308 value=1.7e308>",
               forms: :static
             )
  end

  test "Range ignores inapplicable attributes and resolves missing list without ticks" do
    for attr <-
          ~w(checked multiple readonly required size placeholder accept alpha alt colorspace dirname formaction formenctype formmethod formnovalidate formtarget height maxlength minlength pattern popovertarget popovertargetaction src width) do
      assert {:ok, pdf} = HtmlToPdf.render("<input type=range #{attr}='x'>", forms: :static)
      assert {:ok, reference} = HtmlToPdf.render("<input type=range>", forms: :static)
      assert pdf == reference
    end

    assert {:ok, pdf} = HtmlToPdf.render("<input type=range list=ticks>", forms: :static)
    assert {:ok, reference} = HtmlToPdf.render("<input type=range>", forms: :static)
    assert pdf == reference
    assert {:ok, dom} = HtmlParser.parse_detailed("<input type=range list=ticks>")
    assert {:ok, %{children: [node]}} = Style.compute_detailed(dom)
    assert node.style._form_control.attributes["list"] == "ticks"
    assert node.style._form_control.initial_state.datalist == nil
    assert node.style._range_state.ticks == []
  end

  test "disabled fieldsets and selectors use canonical Range state and retain source identities" do
    html =
      "<style>input:enabled{width:40pt}input:disabled{width:60pt}input:required{height:99pt}</style><fieldset disabled><legend><input type=range name=enabled></legend><input type=range name=disabled></fieldset>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    fieldset = List.last(styled.children)
    [legend, disabled] = fieldset.children
    enabled = hd(legend.children)
    refute enabled.style._form_control.initial_state.disabled
    assert disabled.style._form_control.initial_state.disabled
    assert enabled.style.width == 40 and disabled.style.width == 60
    assert disabled.style.height == 12
    refute HtmlValidator.control_state_matches?(:required, disabled.style._form_control)
    refute HtmlValidator.control_state_matches?(:optional, disabled.style._form_control)
    assert enabled.style._form_control.id != disabled.style._form_control.id
  end

  test "public static artwork paints Range and mutable interactive transport gives actionable diagnostic" do
    html = "<p>Range</p><input type=range name=volume value=50 step=20>"
    assert {:ok, pdf} = HtmlToPdf.render(html, forms: :static)
    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: []}} = FormValidator.inspect_document(context)
    assert {:error, {:unsupported_form, diagnostic}} = HtmlToPdf.render(html, forms: :interactive)
    assert diagnostic.stage == :forms and diagnostic.source == "range"
    assert diagnostic.operation == :write_pdf and diagnostic.module == HtmlFormValidator
    assert diagnostic.message =~ "forms: :static"

    assert {:ok, _} =
             HtmlToPdf.render("<input type=range style='display:none'>", forms: :interactive)
  end

  test "disabled interactive Range retains a read-only scalar and original artwork" do
    html =
      "<input type=range disabled name=volume min=0 max=100 step=20 value=50 style='display:block;width:80pt;height:30pt;position:relative;left:3pt'>"

    assert {:ok, pdf} = HtmlToPdf.render(html, forms: :interactive)
    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: [field]}} = FormValidator.inspect_document(context)
    assert field.name == "volume" and field.value == "60" and field.type == :text
    assert Bitwise.band(field.flags, 1) == 1
    assert {:ok, _} = NativeElixirPdfUtilities.Forms.flatten(pdf)
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled)
    assert {:ok, pages} = Pagination.paginate(layout)
    assert {:ok, _, [transport]} = HtmlFormValidator.prepare(pages, forms: :interactive)
    assert Map.keys(transport.appearance.paint_frame) |> Enum.sort() == [:height, :width, :x, :y]
    assert Enum.any?(transport.appearance.boxes, &(Map.get(&1, :role) == :range_thumb))
  end

  test "range layout and direct transport reject forged state before painting" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<input type=range disabled name=range style=display:block>"
             )

    assert {:ok, %{children: [node]} = styled} = Style.compute_detailed(dom)
    forged = put_in(node.style._range_state.value, 99.0)
    assert {:error, {:invalid_layout, diagnostic}} = Layout.layout(%{styled | children: [forged]})
    assert diagnostic.source == "range"
    missing = %{node | style: Map.delete(node.style, :_form_control)}
    assert {:error, {:invalid_layout, _}} = Layout.layout(%{styled | children: [missing]})
    assert {:ok, layout} = Layout.layout(styled)
    assert {:ok, [page]} = Pagination.paginate(layout)

    boxes =
      Enum.map(page.boxes, fn box ->
        if Map.get(box, :form_control),
          do: put_in(box.form_control.initial_state.range.position, 0.9),
          else: box
      end)

    assert {:error, {:invalid_form, diagnostic}} =
             HtmlFormValidator.prepare([%{page | boxes: boxes}], forms: :static)

    assert diagnostic.source == "initial_state"
    refute HtmlFormValidator.valid_initial_range?(nil, %{})

    malformed =
      put_in(hd(styled.children).style._form_control.initial_state.range.options, [
        "50" | :invalid
      ])

    refute HtmlFormValidator.valid_initial_range?(
             malformed.style._form_control.initial_state,
             malformed.style._form_control.attributes
           )
  end

  test "native Range uses fixed artwork at several dimensions and appearance none retains CSS background" do
    for {width, height} <- [{96.75, 12.0}, {30.0, 30.0}, {150.0, 6.0}] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<input type=range style='display:block;width:#{width}pt;height:#{height}pt;font-size:24pt'>"
               )

      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout(styled)
      thumb = Enum.find(layout.boxes, &(Map.get(&1, :role) == :range_thumb))
      track = Enum.find(layout.boxes, &(Map.get(&1, :role) == :range_track))
      assert thumb.width == 11.25 and thumb.height == 11.25
      assert track.height == min(height, 6.0)
    end

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<input type=range style='display:block;appearance:none;background:red'>"
             )

    assert {:ok, %{children: [node]} = styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled)
    assert Enum.any?(layout.boxes, &(Map.get(&1, :role) == :range_thumb))
    refute Enum.any?(layout.boxes, &(Map.get(&1, :role) == :range_track))
    assert Enum.any?(layout.boxes, &(Map.get(&1, :fill_color) == {1.0, 0.0, 0.0}))
    assert RangeArtwork.boxes(node.style, 0, 0, 0, 12) == []
  end

  test "anonymous Range pagination retains the source line top margin" do
    html =
      File.read!(Paths.fixture("html_to_pdf/browser_parity/html_disabled_range_controls.html"))

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {300, 260}, margin: 0)
    assert {:ok, [_, page]} = Pagination.paginate(layout)
    control = Enum.find(page.boxes, &(get_in(&1, [:form_control, :attributes, "name"]) == "none"))
    # html.css's fixed two CSS-pixel top margin remains inside the line when
    # that complete anonymous line moves to the next physical page.
    assert control.y + control.height == 260 - 1.5
    assert Enum.any?(page.boxes, &(Map.get(&1, :text) == "After disabled range controls"))
  end

  test "Range value snapping preserves validated rounded and inner overflow clips" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed("<input type=range value=25 style=display:block>")

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled)
    assert {:ok, [page]} = Pagination.paginate(layout)
    value = Enum.find(page.boxes, &(Map.get(&1, :role) == :range_value))

    clip = %{
      x: value.x + 3,
      y: value.y,
      width: 45.0,
      height: 6.0,
      border_radius: 2.0,
      inner_clip: %{x: value.x + 6, y: value.y, width: 39.0, height: 6.0}
    }

    snapped = RangeArtwork.snap_box(Map.put(value, :clip, clip), elem(page.size, 1))
    assert snapped.clip == clip
    assert snapped.range_fill_clip.width < clip.width

    assert {:ok, pdf} =
             PdfWriter.render([%{page | boxes: [Map.put(value, :clip, clip)]}],
               compress_streams: false,
               forms: :static
             )

    # The native fill extent and both caller clip contours are independent PDF
    # clipping operations. The inner contour cannot be replaced during snapping.
    assert length(Regex.scan(~r/W n/, pdf)) == 3
  end

  test "composed Range retains its local value clip inside a clipped drawing viewport" do
    html =
      "<input type=range value=25 style='display:block;width:96pt;margin:0'>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled)
    assert {:ok, [page]} = Pagination.paginate(layout)

    parts =
      Enum.filter(
        page.boxes,
        &(Map.get(&1, :role) in [:range_track, :range_value, :range_border, :range_thumb])
      )

    assert length(parts) == 4

    group = %{
      type: :drawing_group,
      transform: {1, 0, 0, 1, 12, 12},
      width: elem(page.size, 0),
      height: elem(page.size, 1),
      clip: %{x: 0, y: elem(page.size, 1) - 12, width: 48, height: 12},
      boxes: parts
    }

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {300, 900}, boxes: [group]}],
               compress_streams: false,
               forms: :static
             )

    assert pdf =~ "0 829.89 48 12 re W n"
    assert length(Regex.scan(~r/W n/, pdf)) == 2
  end

  test "static embedded Range retains valid nonpainting host geometry" do
    html =
      File.read!(Paths.fixture("html_to_pdf/browser_parity/html_range_overflow_controls.html"))

    assert {:ok, pdf} = HtmlToPdf.render(html, forms: :static, base_url: Path.expand("."))
    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "After clipped ranges"
  end

  test "inert Range retains artwork without creating interactive fields" do
    html = "<input type=range inert name=inert value=80 style=display:block>"
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [node]} = styled} = Style.compute_detailed(dom)
    refute Map.has_key?(node.style, :_form_control)
    assert node.style._range_control.initial_state.value == "80"
    assert {:ok, layout} = Layout.layout(styled)
    assert Enum.any?(layout.boxes, &(Map.get(&1, :role) == :range_thumb))
    assert {:ok, pdf} = HtmlToPdf.render(html, forms: :interactive)
    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)

    forged = put_in(node.style._range_control.attributes["type"], "number")

    assert {:error, {:invalid_layout, %{source: "range"}}} =
             Layout.layout(%{styled | children: [forged]})
  end

  test "public PDF writer rejects malformed or unbounded native Range frames" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<input type=range style=display:block>")
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled)
    assert {:ok, [page]} = Pagination.paginate(layout)
    thumb = Enum.find(page.boxes, &(Map.get(&1, :role) == :range_thumb))

    for frame <- [
          :invalid,
          {nil, 0, 100, 12, 0, 0},
          {0, 0, 0, 12, 0, 0},
          {0, 0, 100, 12, 1.0e20, 0}
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               PdfWriter.render([%{page | boxes: [Map.put(thumb, :range_frame, frame)]}])

      assert diagnostic.stage == :pdf and diagnostic.operation == :write_pdf
      assert diagnostic.message =~ "valid pages"
    end

    assert {:error, {:invalid_pdf_input, _}} =
             PdfWriter.render([%{page | boxes: [Map.delete(thumb, :range_frame)]}])
  end

  test "disabled overflowing thumbs diagnose unavailable PDF transport and preserve static output" do
    for style <- [
          "height:6pt;width:30pt",
          "height:12pt;width:6pt"
        ] do
      html = "<input type=range disabled value=80 style='#{style}'>"
      assert {:ok, _} = HtmlToPdf.render(html, forms: :static)

      assert {:error, {:unsupported_form, diagnostic}} =
               HtmlToPdf.render(html, forms: :interactive)

      assert diagnostic.stage == :forms and diagnostic.source == "range"
      assert diagnostic.operation == :write_pdf and diagnostic.message =~ "forms: :static"
    end

    assert {:ok, _} =
             HtmlToPdf.render(
               "<input type=range disabled style='height:12pt;width:30pt;padding:12pt'>",
               forms: :interactive
             )

    assert {:ok, dom} =
             HtmlParser.parse_detailed("<input type=range disabled style=display:block>")

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled)
    assert {:ok, [page]} = Pagination.paginate(layout)

    boxes =
      Enum.map(page.boxes, fn box ->
        if Map.get(box, :role) == :range_thumb,
          do: Map.put(box, :range_frame, :invalid),
          else: box
      end)

    assert {:error, {:invalid_form, %{stage: :forms, source: "range"}}} =
             HtmlFormValidator.prepare([%{page | boxes: boxes}], forms: :interactive)
  end

  test "existing numeric and layout budgets return public Range diagnostics" do
    previous = Limits.effective()
    on_exit(fn -> Limits.install(previous) end)
    Limits.install(Map.put(previous, :max_html_numeric_token_bytes, 4))
    assert Limits.get(:max_html_numeric_token_bytes) == 4

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<input type=range value=12345>", forms: :static)

    assert diagnostic.stage == :html and diagnostic.message =~ "max_html_numeric_token_bytes"
  end

  test "datalist range ticks use exact bounds and authored step base without selecting suggestions" do
    html =
      "<input type=range list=ticks min=-1 max=1 step=.5 value=.4><datalist id=ticks><option value=-1><option value=-.5 label=low><option value=-.25><option value=0><option value=.5 disabled><option value=1><option value=2><option value=bad><option value='.5suffix'><option value=' +.5'><option value=1e0></datalist>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    node = hd(styled.children)
    state = node.style._form_control.initial_state
    assert state.value == "0.5"
    assert Enum.map(state.range.ticks, & &1.value) == [-1.0, -0.5, 0.0, 1.0, 1.0]
    assert Enum.map(state.range.ticks, & &1.position) == [0.0, 0.25, 0.5, 1.0, 1.0]
    assert "-.25" in state.range.options and ".5" not in state.range.options
    assert HtmlFormValidator.valid_initial_range?(state, node.style._form_control.attributes)

    for {attrs, options, expected} <- [
          {%{"value" => ".5", "step" => "1"}, [".5", "1", "1.5"], [0.5, 1.5]},
          {%{"min" => "0.1", "max" => "1", "step" => ".2"}, [".3", ".4", ".5"], [0.3, 0.5]},
          {%{"min" => "0", "max" => "1", "step" => "any"}, [".123", "1", ""], [0.123, 1.0]},
          {%{"min" => "9", "max" => "2"}, ["9", "20"], []},
          {%{"min" => "0", "max" => "0"}, ["0"], []},
          {%{"min" => "-1e308", "max" => "1e308", "step" => "any"}, ["-1e308", "0", "1e308"],
           [-1.0e308, 0.0, 1.0e308]}
        ] do
      prepared = HtmlFormValidator.prepare_initial_range(attrs, options)
      assert Enum.map(prepared.range.ticks, & &1.value) == expected
      assert HtmlFormValidator.valid_initial_range?(prepared, attrs)
    end
  end

  test "associated empty and shadowed datalists retain source applicability and intrinsic height" do
    for {html, associated, ticks} <- [
          {"<input type=range list=t><datalist id=t></datalist>", true, []},
          {"<input type=range list=t><datalist id=t><option value=bad></datalist>", true, []},
          {"<input type=range list=t><span id=t></span><datalist id=t><option value=50></datalist>",
           false, []},
          {"<input type=range list=T><datalist id=t><option value=50></datalist>", false, []},
          {"<input type=range list=t><datalist id=t><select><optgroup disabled label=Grouped><option value=20></optgroup><option value=80></select></datalist>",
           true, [80.0]}
        ] do
      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      node = hd(styled.children)
      assert node.style._form_control.initial_state.datalist != nil == associated
      assert node.style.height == if(associated, do: 16.5, else: 12.0)
      assert Enum.map(node.style._range_state.ticks, & &1.value) == ticks

      assert {:ok, pdf} =
               HtmlToPdf.render(html,
                 forms: :static,
                 asset_resolver: fn request ->
                   flunk("suggestions fetched #{inspect(request)}")
                 end
               )

      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
    end
  end

  test "qualified datalist value attributes remain source metadata for range ticks" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<input type=range list=t><datalist id=t><option value=20>50</option></datalist>"
             )

    [input, list] = dom.children
    [option] = list.children
    option = Map.put(option, :attribute_namespaces, %{"value" => "urn:other"})

    assert {:ok, styled} =
             Style.compute_detailed(%{dom | children: [input, %{list | children: [option]}]})

    assert hd(styled.children).style._range_state.ticks == [%{value: 50.0, position: 0.5}]

    [_, prepared_list] =
      HtmlFormValidator.prepare_initial_nodes([input, %{list | children: [option]}])

    assert hd(prepared_list.children).attributes["value"] == "20"
  end

  test "tick boxes paint text color and preserve canonical frames through PDF and readonly appearance" do
    html =
      "<input type=range disabled name=level list=t value=60 style='display:block;width:150pt;color:#884422'><datalist id=t><option value=0 label=Low><option value=50 label=Middle><option value=100 label=High></datalist>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled)
    ticks = Enum.filter(layout.boxes, &(Map.get(&1, :role) == :range_tick))
    assert length(ticks) == 3

    assert Enum.all?(
             ticks,
             &(&1.width == 0.75 and &1.height == 3.0 and &1.border_radius == 0.0 and
                 &1.fill_color == {136 / 255, 68 / 255, 34 / 255})
           )

    assert Enum.all?(
             ticks,
             &NativeElixirPdfUtilities.Validators.WriterValidator.valid_range_frame?/1
           )

    assert {:ok, pages} = Pagination.paginate(layout)
    assert {:ok, _, [transport]} = HtmlFormValidator.prepare(pages, forms: :interactive)
    assert Enum.count(transport.appearance.boxes, &(Map.get(&1, :role) == :range_tick)) == 3
    assert {:ok, pdf} = HtmlToPdf.render(html, forms: :interactive)
    assert {:ok, [field]} = NativeElixirPdfUtilities.Forms.fields(pdf)
    assert field.name == "level" and field.value == "60" and field.read_only
    assert {:ok, flattened} = NativeElixirPdfUtilities.Forms.flatten(pdf)
    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(flattened)

    assert {:error, {:no_extractable_text, diagnostic}} =
             NativeElixirPdfUtilities.Text.extract(flattened)

    assert diagnostic.stage == :text_extraction and diagnostic.operation == :extract
    assert {:ok, [page]} = Pagination.paginate(layout)

    assert {:error, {:invalid_pdf_input, diagnostic}} =
             PdfWriter.render([%{page | boxes: [Map.delete(hd(ticks), :range_frame)]}])

    assert diagnostic.stage == :pdf and diagnostic.operation == :write_pdf

    for frame <- [:invalid, {0.0, 150.0, 1.1}, {0.0, 1.0e20, 0.5}, {nil, 150.0, 0.5}] do
      assert {:error, {:invalid_pdf_input, %{stage: :pdf, operation: :write_pdf}}} =
               PdfWriter.render([%{page | boxes: [Map.put(hd(ticks), :range_tick_frame, frame)]}])
    end

    assert {:error, {:invalid_pdf_input, _}} =
             PdfWriter.render([%{page | boxes: [Map.delete(hd(ticks), :range_tick_frame)]}])

    node = hd(styled.children)
    unassociated = put_in(node.style._form_control.initial_state.datalist, nil)

    assert {:error, {:invalid_layout, %{source: "range"}}} =
             Layout.layout(%{styled | children: [unassociated | tl(styled.children)]})

    forged_pages =
      Enum.map(pages, fn p ->
        %{
          p
          | boxes:
              Enum.map(p.boxes, fn box ->
                if Map.get(box, :form_control),
                  do: put_in(box.form_control.initial_state.datalist, nil),
                  else: box
              end)
        }
      end)

    assert {:error, {:invalid_form, %{source: "initial_state", stage: :forms}}} =
             HtmlFormValidator.prepare(forged_pages, forms: :static)

    forged = put_in(node.style._range_state.ticks, [%{value: 25.0, position: 0.25}])

    assert {:error, {:invalid_layout, %{source: "range"}}} =
             Layout.layout(%{styled | children: [forged | tl(styled.children)]})
  end

  test "hidden, inert, and appearance none preserve suggested initial state without invented tick labels" do
    for style <- ["display:none", "appearance:none"] do
      html =
        "<input type=range inert list=t style='#{style}'><datalist id=t><option value=20 label=Twenty></datalist>"

      assert {:ok, dom} = HtmlParser.parse_detailed("<body>" <> html <> "</body>")
      assert {:ok, styled} = Style.compute_detailed(dom)
      [prepared | _] = HtmlFormValidator.prepare_initial_nodes(hd(dom.children).children)
      assert prepared._initial_form_state.value == "50"

      assert prepared._initial_form_state.range.ticks == [
               %{value: 20.0, position: 0.2}
             ]

      assert {:ok, layout} = Layout.layout(styled)
      refute Enum.any?(layout.boxes, &(Map.get(&1, :role) == :range_tick))
      assert {:ok, pdf} = HtmlToPdf.render(html, forms: :interactive)
      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
    end
  end

  test "static ticks may overflow an authored short height while PDF widget bounds remain truthful" do
    html =
      "<input type=range disabled name=short list=t style='height:12pt;width:60pt'><datalist id=t><option value=0><option value=100></datalist>"

    assert {:ok, static} = HtmlToPdf.render(html, forms: :static)
    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(static)
    assert {:error, {:unsupported_form, diagnostic}} = HtmlToPdf.render(html, forms: :interactive)

    assert diagnostic.stage == :forms and diagnostic.source == "range" and
             diagnostic.operation == :write_pdf

    assert diagnostic.message =~ "artwork" and diagnostic.message =~ "forms: :static"
  end

  test "tick numeric work observes the existing public shared budget" do
    previous = Limits.effective()
    on_exit(fn -> Limits.install(previous) end)
    Limits.install(Map.put(previous, :max_html_numeric_token_bytes, 4))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(
               "<input type=range list=t><datalist id=t><option value=12345></datalist>",
               forms: :static
             )

    assert diagnostic.message =~ "max_html_numeric_token_bytes"
  end
end
