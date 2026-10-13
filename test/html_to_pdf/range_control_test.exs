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

  test "Range rejects inapplicable attributes and diagnoses queued list artwork" do
    for attr <-
          ~w(checked multiple readonly required size placeholder accept alpha alt colorspace dirname formaction formenctype formmethod formnovalidate formtarget height maxlength minlength pattern popovertarget popovertargetaction src width) do
      assert {:error, {reason, diagnostic}} =
               HtmlToPdf.render("<input type=range #{attr}='x'>", forms: :static)

      assert reason == diagnostic.reason and diagnostic.stage == :html
      assert diagnostic.message != ""
    end

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render("<input type=range list=ticks>", forms: :static)

    assert diagnostic.source == "range/list" and diagnostic.message =~ "tick artwork"
    assert diagnostic.operation == :render
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
      File.read!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/html_disabled_range_controls.html",
          __DIR__
        )
      )

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
      File.read!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/html_range_overflow_controls.html",
          __DIR__
        )
      )

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

    assert :ok =
             HtmlFormValidator.validate_initial_range_attributes(%{
               namespace: :svg,
               tag: "input",
               attributes: %{"type" => "range", "list" => "ticks"}
             })
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
end
