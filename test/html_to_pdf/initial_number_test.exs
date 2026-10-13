defmodule NativeElixirPdfUtilities.HtmlToPdf.InitialNumberTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Forms, Limits, Text}

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    HtmlParser,
    Style,
    Layout,
    Pagination,
    PdfWriter
  }

  alias NativeElixirPdfUtilities.Validators.{HtmlFormValidator, HtmlValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "number sanitation preserves valid authored lexical values and sanitizes invalid ones" do
    for {authored, expected} <- [
          {"", ""},
          {"-0", "-0"},
          {"01.20", "01.20"},
          {".25", ".25"},
          {"-.25", "-.25"},
          {"1e+2", "1e+2"},
          {"-1E-2", "-1E-2"},
          {"1e100", "1e100"},
          {"1e-999", "1e-999"},
          {"0e999", "0e999"},
          {"+2", ""},
          {" 2", ""},
          {"2 ", ""},
          {"1.", ""},
          {"1.e3", ""},
          {"1e-", ""},
          {"2px", ""},
          {"NaN", ""},
          {"Infinity", ""},
          {"1e999", ""},
          {"٢", ""},
          {"A\rB\nC", ""}
        ] do
      assert {:ok, dom} = HtmlParser.parse_detailed("<input type=NuMbEr value='#{authored}'>")
      [node] = HtmlFormValidator.prepare_initial_nodes(dom.children)
      state = node._initial_form_state
      assert state.value == expected and state.display == expected
      assert node.attributes["value"] == String.replace(authored, ~r/\r\n?/, "\n")

      assert state.number.value ==
               if(expected == "",
                 do: nil,
                 else: elem(HtmlValidator.parse_html_float(expected), 1)
               )
    end

    assert HtmlFormValidator.prepare_initial_number(%{"type" => "number"}) ==
             HtmlFormValidator.prepare_initial_number(%{"type" => "number", "value" => ""})
  end

  test "bounds and step prepare renderable state with min then authored value then zero base" do
    for {attrs, expected} <- [
          {%{}, {nil, nil, 1.0, 0.0}},
          {%{"value" => ".25"}, {nil, nil, 1.0, 0.25}},
          {%{"min" => "-2", "max" => "3", "value" => "-4", "step" => ".5"},
           {-2.0, 3.0, 0.5, -2.0}},
          {%{"min" => "invalid", "max" => "Infinity", "step" => "bad"}, {nil, nil, 1.0, 0.0}},
          {%{"min" => " +2tail", "max" => " 9tail", "value" => "1e-", "step" => " +.5tail"},
           {2.0, 9.0, 0.5, 2.0}},
          {%{"step" => "ANY"}, {nil, nil, :any, 0.0}},
          {%{"step" => " any "}, {nil, nil, 1.0, 0.0}},
          {%{"step" => "0"}, {nil, nil, 1.0, 0.0}},
          {%{"step" => "-2"}, {nil, nil, 1.0, 0.0}}
        ] do
      state = HtmlFormValidator.prepare_initial_number(attrs).number
      assert {state.min, state.max, state.step, state.step_base} == expected
    end

    for value <- ["-4", "4"] do
      state =
        HtmlFormValidator.prepare_initial_number(%{"value" => value, "min" => "-2", "max" => "3"})

      assert state.number.range_underflow == (value == "-4")
      assert state.number.range_overflow == (value == "4")
      assert state.value == value

      for mode <- [:static, :interactive] do
        assert {:ok, _pdf} =
                 HtmlToPdf.render("<input type=number min=-2 max=3 value=#{value}>", forms: mode)
      end
    end

    state =
      HtmlFormValidator.prepare_initial_number(%{"value" => "3", "min" => "5", "max" => "1"})

    assert state.number.range_underflow and state.number.range_overflow
    state = HtmlFormValidator.prepare_initial_number(%{"value" => "", "min" => "5", "max" => "1"})
    refute state.number.range_underflow or state.number.range_overflow
  end

  test "pinned Decimal sizing reuses the shared bounded numeric parser" do
    for {source, expected} <- [
          {"-12.34", {true, 4, -2}},
          {"001.20", {false, 3, -2}},
          {"0", {false, 1, 0}},
          {"-0.000", {false, 1, 0}},
          {".001", {false, 1, -3}},
          {"1e20", {false, 1, 20}},
          {"1e-1024", {false, 1, 0}},
          {"123456789012345678901.2", {false, 18, 3}},
          {"1.12345678901234567890", {false, 18, -17}}
        ] do
      assert {:ok, number} = HtmlValidator.parse_html_float(source, :decimal)
      assert {number.negative, number.coefficient_digits, number.exponent} == expected
    end

    assert :error = HtmlValidator.parse_html_float("invalid", :decimal)

    for {attrs, columns, decoration} <- [
          {%{}, 20, false},
          {%{"min" => "0"}, 20, false},
          {%{"min" => "0", "max" => "100"}, 3, true},
          {%{"min" => "-12.34", "max" => "9", "step" => ".01"}, 6, true},
          {%{"min" => "0", "max" => "100", "step" => "ANY"}, 20, false},
          {%{"min" => "0", "max" => "100", "step" => "bad"}, 3, true},
          {%{"min" => "0", "max" => "100", "step" => "-2"}, 3, true},
          {%{"min" => "0", "max" => "100", "step" => "1e20"}, 3, true}
        ] do
      number = HtmlFormValidator.prepare_initial_number(attrs).number
      assert {number.columns, number.decoration} == {columns, decoration}
    end
  end

  test "number applicability, fieldset state and selectors use canonical initial metadata" do
    for attribute <- ["checked", "multiple", "size"] do
      assert {:error, {_, diagnostic}} =
               HtmlParser.parse_detailed("<input type=number #{attribute}=2>")

      assert diagnostic.stage == :html and diagnostic.source =~ "<input type=number"
      assert is_integer(diagnostic.line) and is_integer(diagnostic.column)
    end

    for type <- ["text", "checkbox"] do
      assert {:error, _} = HtmlParser.parse_detailed("<input type=#{type} min=1>")
    end

    html = """
    <style>input:read-write {color:red} input:read-only {color:blue}
    input:required {background:#eeeeee} input:optional {background:#ffffff}</style>
    <fieldset disabled><legend><input name=legend type=number required value=1></legend>
    <input name=disabled type=number value=2></fieldset>
    <input name=readonly type=number readonly required value=3><label for=target>Label</label>
    <form id=owner></form><input id=target name=owned type=number form=owner value=4>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    nodes = HtmlFormValidator.prepare_initial_nodes(dom.children)

    [fieldset, readonly, label, owner, owned] =
      Enum.filter(nodes, &(Map.get(&1, :tag) != "style"))

    [legend, disabled] = Enum.filter(fieldset.children, &(&1.type == :element))
    [legend_control] = legend.children
    refute legend_control._initial_form_state.disabled
    assert disabled._initial_form_state.disabled
    assert owned._initial_form_state.owner == owner._selector_id
    assert label._initial_label_control == owned._selector_id
    assert HtmlValidator.editing_state_matches?(:read_write, legend_control, [])
    assert HtmlValidator.editing_state_matches?(:read_only, disabled, [])
    assert HtmlValidator.editing_state_matches?(:read_only, readonly, [])
    assert HtmlValidator.control_state_matches?(:required, readonly)
    assert HtmlValidator.control_state_matches?(:optional, owned)
    assert {:ok, _styled} = Style.compute_detailed(dom)
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)

    assert {:ok, %{fields: fields}} =
             NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(document)

    fields = Map.new(fields, &{&1.name, &1})
    assert Bitwise.band(fields["legend"].flags, 2) == 2 and not fields["legend"].read_only
    assert fields["disabled"].read_only and fields["readonly"].read_only
    assert Bitwise.band(fields["readonly"].flags, 2) == 0
  end

  test "number font metrics, author dimensions and inner clip preserve the full PDF scalar" do
    html =
      "<input name=large type=number min=-9 max=999 step=.01 value=12345678901234567890 style='display:block;font-family:DejaVu Sans Mono;font-size:12pt;border:1pt solid black;width:45pt;height:18pt;padding:2pt'>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [node]} = styled} = Style.compute_detailed(dom)
    assert node.style.width == 45.0 and node.style.height == 18.0
    assert node.style.form_kind == :text and node.style.font_family == "DejaVu Sans Mono"
    refute node.style._native_menu
    assert {:ok, layout} = Layout.layout(styled)
    text = Enum.find(layout.boxes, &(&1.type == :text))
    assert text.clip.width == 33.75
    assert {:ok, pages} = Pagination.paginate(layout)

    for mode <- [:static, :interactive] do
      assert {:ok, pdf} = PdfWriter.render(pages, forms: mode)
      assert {:ok, flat} = Forms.flatten(pdf)
      assert {:ok, extracted} = Text.extract(flat)
      assert extracted =~ "12345678901234567890"
      assert {:ok, fields} = Forms.fields(pdf)

      if mode == :interactive do
        assert [%{name: "large", type: :text, value: "12345678901234567890"}] = fields
      else
        assert fields == []
      end
    end

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<input type=number min=0 max=100 style='font-family:DejaVu Sans Mono;font-size:12pt'>"
             )

    assert {:ok, %{children: [node]}} = Style.compute_detailed(dom)

    assert {:ok, reference} =
             HtmlParser.parse_detailed(
               "<input size=3 style='font-family:DejaVu Sans Mono;font-size:12pt'>"
             )

    assert {:ok, %{children: [reference]}} = Style.compute_detailed(reference)
    assert node.style.width >= reference.style.width + 11.25
    assert node.style._native_menu
  end

  test "placeholders are prepared separately from scalar exports" do
    html =
      "<input name=empty type=number placeholder='Number&#10;hint'><input name=bad type=number value=bad placeholder=Sanitized><input name=valid type=number value=.25 placeholder=Unused>"

    for mode <- [:static, :interactive] do
      assert {:ok, pdf} = HtmlToPdf.render(html, forms: mode)
      assert {:ok, flat} = Forms.flatten(pdf)
      assert {:ok, text} = Text.extract(flat)
      assert text =~ "Numberhint" and text =~ "Sanitized" and text =~ ".25"
      refute text =~ "bad" or text =~ "Unused"
      assert {:ok, fields} = Forms.fields(pdf)

      if mode == :interactive do
        assert Enum.map(fields, &{&1.name, &1.value}) == [
                 {"empty", ""},
                 {"bad", ""},
                 {"valid", ".25"}
               ]
      else
        assert fields == []
      end
    end
  end

  test "forged canonical number transport fails through shared diagnostics in both modes" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<input name=n type=number value=3 min=1 max=5 style=display:block>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled)
    assert {:ok, [page]} = Pagination.paginate(layout)
    control_box = Enum.find(page.boxes, &is_map(Map.get(&1, :form_control)))
    control = control_box.form_control

    for replacement <- [
          %{value: "bad"},
          %{display: "wrong"},
          %{placeholder_shown: true},
          %{number: nil},
          %{number: Map.put(control.initial_state.number, :value, 4.0)},
          %{number: Map.put(control.initial_state.number, :step, 0.0)},
          %{number: Map.put(control.initial_state.number, :columns, 100)}
        ],
        mode <- [:static, :interactive] do
      state = Map.merge(control.initial_state, replacement)
      box = %{control_box | form_control: %{control | initial_state: state}}

      assert {:error, {:invalid_form, diagnostic}} =
               PdfWriter.render([%{page | boxes: [box]}], forms: mode)

      assert diagnostic.stage == :forms and diagnostic.reason == :invalid_form
      assert diagnostic.operation == :write_pdf and diagnostic.module == HtmlFormValidator
      assert diagnostic.source == "initial_state" and diagnostic.message =~ "validated initial"
    end
  end

  test "malformed canonical number attributes return public diagnostics before any reparsing" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed("<input name=n type=number value=3 style=display:block>")

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled)
    assert {:ok, [page]} = Pagination.paginate(layout)
    control_box = Enum.find(page.boxes, &is_map(Map.get(&1, :form_control)))

    for attributes <- [
          Map.put(control_box.form_control.attributes, "type", nil),
          Map.put(control_box.form_control.attributes, "step", nil),
          Map.put(control_box.form_control.attributes, "value", 3),
          Map.put(control_box.form_control.attributes, "min", <<255>>),
          Map.put(control_box.form_control.attributes, 3, "value")
        ],
        mode <- [:static, :interactive] do
      box = %{control_box | form_control: %{control_box.form_control | attributes: attributes}}

      assert {:error, {:invalid_form, diagnostic}} =
               PdfWriter.render([%{page | boxes: [box]}], forms: mode)

      assert diagnostic.stage == :forms and diagnostic.reason == :invalid_form
      assert diagnostic.operation == :write_pdf and diagnostic.source == "initial_state"
      assert diagnostic.message =~ "validated initial"
    end
  end

  test "number appearance transport retains the original page frame and scalar type" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<input type=number name=n value=3 style='display:block;position:relative;left:3pt'>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {340, 420})
    assert {:ok, pages} = Pagination.paginate(layout)
    assert {:ok, _cleaned, [field]} = HtmlFormValidator.prepare(pages, forms: :interactive)
    [left, bottom, right, top] = field.rect
    assert field.type == :text and field.value == "3"
    assert field.appearance.paint_frame == %{x: left, y: bottom, width: 340.0, height: 420.0}
    assert field.appearance.size == {right - left, top - bottom}
    assert Enum.all?(field.appearance.boxes, &(&1.x >= left))
  end

  test "advanced number layout rejects forged state and appearance metadata" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed("<input type=number style=display:block value=3>")

    assert {:ok, %{children: [node]} = styled} = Style.compute_detailed(dom)

    for style <- [
          Map.put(node.style, :_number_state, nil),
          Map.put(node.style, :_native_menu, nil),
          Map.delete(node.style, :_form_control),
          put_in(node.style, [:_form_control, :tag], "textarea"),
          put_in(node.style, [:_form_control, :attributes, "type"], "text"),
          put_in(node.style, [:_form_control, :attributes, "type"], nil),
          put_in(node.style, [:_form_control, :attributes, "step"], nil),
          put_in(node.style, [:_form_control, :initial_state, :readonly], true)
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               Layout.layout(%{styled | children: [%{node | style: style}]})

      assert diagnostic.stage == :layout and diagnostic.reason == :invalid_layout
      assert diagnostic.operation == :layout and diagnostic.source == "number"
      assert diagnostic.message =~ "canonical initial number state"
    end

    refute HtmlFormValidator.valid_initial_number?(nil, %{})
    refute HtmlFormValidator.valid_initial_number?(%{}, nil)
  end

  test "source inner editor line-height rules preserve author heights and reset only applicable cases" do
    for {size, line_height, height, expected} <- [
          {16.0, 13.5, nil, true},
          {8.0, 13.5, nil, false},
          {10.0, 13.5, 18.0, true},
          {10.0, 13.5, 12.0, false},
          {10.0, 13.5, {:percent, 50.0}, true},
          {10.0, 13.5, :auto, false}
        ] do
      assert HtmlFormValidator.number_uses_normal_line_height?(%{
               font_size: size,
               line_height: line_height,
               height: height
             }) == expected
    end

    for appearance <- ["none", "auto"] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<input type=number style='font-size:16pt;line-height:13.5pt;appearance:#{appearance}'>"
               )

      assert {:ok, %{children: [node]}} = Style.compute_detailed(dom)
      assert node.style.line_height > 16.0
      assert node.style._native_menu == (appearance == "auto")
    end
  end

  test "disabled native number borders reuse effective fieldset state without losing values" do
    html = "<fieldset disabled><input type=number name=n value=3></fieldset>"
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled)
    control = Enum.find(layout.boxes, &is_map(Map.get(&1, :form_control)))
    assert control.role == :native_menu
    assert control.stroke_color == {118 / 255, 118 / 255, 118 / 255, 0.3 * 0.75}
    assert control.form_control.initial_state.disabled
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, [%{read_only: true, value: "3"}]} = Forms.fields(pdf)
  end

  test "invalid path-like authored numbers sanitize without calling an asset resolver" do
    owner = self()

    resolver = fn source ->
      send(owner, {:unexpected_number_asset, source})
      {:error, :not_found}
    end

    for mode <- [:static, :interactive] do
      assert {:ok, pdf} =
               HtmlToPdf.render("<input type=number name=n value='/path/not-to-read'>",
                 forms: mode,
                 asset_resolver: resolver
               )

      assert {:ok, fields} = Forms.fields(pdf)
      if mode == :interactive, do: assert([%{value: ""}] = fields), else: assert(fields == [])
    end

    refute_received {:unexpected_number_asset, _}
  end

  test "hidden initial numbers retain DOM state and create no artwork or PDF fields" do
    for attributes <- ["hidden", "style='display:none'"] do
      html = "<input type=number name=n value=invalid-number #{attributes}><p>Visible caption</p>"
      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      [number, _caption] = HtmlFormValidator.prepare_initial_nodes(dom.children)
      assert number._initial_form_state.value == ""
      assert number.attributes["value"] == "invalid-number"

      for mode <- [:static, :interactive] do
        assert {:ok, pdf} = HtmlToPdf.render(html, forms: mode)
        assert {:ok, []} = Forms.fields(pdf)
        assert {:ok, text} = Text.extract(pdf)
        assert text =~ "Visible caption"
        refute text =~ "invalid-number"
      end
    end
  end

  test "authored number heights center the inner editor using fixed LayoutUnit division" do
    style = %{line_height: 12.0}
    assert HtmlFormValidator.initial_number_content_offset(style, 36.0) == 12.0
    assert HtmlFormValidator.initial_number_content_offset(style, 3.0) == -4.5
    assert HtmlFormValidator.initial_number_content_offset(style, 3.01) == -4.48828125
    assert HtmlFormValidator.initial_number_content_offset(style, 12.0) == 0.0

    for mode <- [:static, :interactive] do
      html =
        File.read!("test/fixtures/html_to_pdf/browser_parity/html_number_height_controls.html")

      assert {:ok, pdf} = HtmlToPdf.render(html, forms: mode)
      assert {:ok, flat} = Forms.flatten(pdf)
      assert {:ok, text} = Text.extract(flat)
      assert text =~ "42.5" and text =~ "After tall number" and text =~ "After short number"
      assert {:ok, fields} = Forms.fields(pdf)

      if mode == :interactive,
        do: assert(Enum.map(fields, & &1.value) == ["42.5", "123"]),
        else: assert(fields == [])
    end
  end

  test "shared numeric token and geometry limits report actionable public failures" do
    Limits.install(Map.put(Limits.defaults(), :max_html_numeric_token_bytes, 3))

    for attributes <- ["value=1234", "min=1234", "max=1234", "step=1234"] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render("<input type=number #{attributes}>")

      assert diagnostic.stage == :html and diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.operation == :render and diagnostic.module == HtmlValidator
      assert diagnostic.message =~ "max_html_numeric_token_bytes"
    end

    Limits.install(Map.put(Limits.defaults(), :max_css_numeric_magnitude, 10))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<input type=number>")

    assert diagnostic.message =~ "max_css_numeric_magnitude"
  end
end
