defmodule NativeElixirPdfUtilities.FormsTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.Pdf.{Reader, IncrementalWriter}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "option labels and text-derived values follow separate initial rules" do
    for {option, value, label} <- [
          {"<option> \tAlpha\n Beta\r\f </option>", "Alpha Beta", "Alpha Beta"},
          {"<option label='Shown'> Inner\n Value </option>", "Inner Value", "Shown"},
          {"<option label='' value=''>Fallback</option>", "", "Fallback"},
          {"<option label='Label' value=' Raw\n Value '>Unused</option>", " Raw\n Value ",
           "Label"},
          {"<option>&nbsp;A&nbsp;</option>", "\u00A0A\u00A0", "\u00A0A\u00A0"}
        ] do
      html = "<select name='choice'>#{option}</select>"
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)

      assert {:ok, [%{value: ^value, choices: [%{value: ^value, label: ^label}]}]} =
               Forms.fields(pdf)

      assert {:ok, flattened} = Forms.flatten(pdf)
      assert {:ok, text} = Text.extract(flattened, layout: false)
      assert String.trim(text) == String.trim(label)
      assert {:ok, static} = HtmlToPdf.render(html, forms: :static, system_font_discovery: false)
      assert {:ok, text} = Text.extract(static, layout: false)
      assert String.trim(text) == String.trim(label)
    end
  end

  test "optgroups preserve choice order and inherited disabled default selection" do
    for {groups, expected} <- [
          {"<optgroup label='Unavailable' disabled><option value='a'>Alpha</option></optgroup><optgroup label='Available'><option value='b' label='Bee'>Beta</option></optgroup>",
           "b"},
          {"<optgroup label='Unavailable' disabled><option value='a' selected>Alpha</option></optgroup><option value='b'>Beta</option>",
           "a"},
          {"<optgroup label='Unavailable' disabled='false'><option value='a'>Alpha</option></optgroup>",
           nil},
          {"<optgroup label='Empty'></optgroup>", nil}
        ] do
      assert {:ok, pdf} =
               HtmlToPdf.render("<select name='choice'>#{groups}</select>",
                 system_font_discovery: false
               )

      assert {:ok, [%{type: :choice, value: ^expected, choices: choices}]} = Forms.fields(pdf)

      assert Enum.map(choices, & &1.value) ==
               if(groups =~ "value='b'",
                 do: ["a", "b"],
                 else: if(groups =~ "value='a'", do: ["a"], else: [])
               )

      assert {:ok, _} = Forms.flatten(pdf)
    end
  end

  test "single selects retain the last selection and default to the first enabled option" do
    for {options, expected, label} <- [
          {"<option value='a' selected>Alpha</option><option value='b' selected>Beta</option>",
           "b", "Beta"},
          {"<option value='a' disabled>Alpha</option><option value='b'>Beta</option>", "b",
           "Beta"},
          {"<option value='a' disabled selected>Alpha</option><option value='b'>Beta</option>",
           "a", "Alpha"},
          {"<option value=''>Empty value</option>", "", "Empty value"}
        ] do
      html = "<p>Selection</p><select name='choice'>#{options}</select>"
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, [%{name: "choice", type: :choice, value: ^expected}]} = Forms.fields(pdf)
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert text =~ "Selection"
      assert {:ok, flattened} = Forms.flatten(pdf)
      assert {:ok, []} = Forms.fields(flattened)
      assert {:ok, text} = Text.extract(flattened, layout: false)
      assert text =~ label
      assert {:ok, static} = HtmlToPdf.render(html, forms: :static, system_font_discovery: false)
      assert {:ok, text} = Text.extract(static, layout: false)
      assert text =~ label
    end
  end

  test "empty and all-disabled selects have an absent value without raising" do
    for {options, expected, choices} <- [
          {"", nil, []},
          {"<option value='a' disabled>Alpha</option>", nil, [%{value: "a", label: "Alpha"}]},
          {"<option></option>", "", [%{value: "", label: ""}]}
        ] do
      html = "<p>Before</p><select name='choice'>#{options}</select><p>After</p>"
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, [%{name: "choice", value: ^expected, choices: ^choices}]} = Forms.fields(pdf)
      assert {:ok, filled} = Forms.fill(pdf, %{"choice" => nil})
      assert {:ok, [%{value: nil}]} = Forms.fields(filled)
      assert {:ok, flattened} = Forms.flatten(pdf)
      assert {:ok, []} = Forms.fields(flattened)
      assert {:ok, text} = Text.extract(flattened, layout: false)
      assert text =~ "Before" and text =~ "After"
      refute text =~ "Alpha"
      assert {:ok, _} = HtmlToPdf.render(html, forms: :static, system_font_discovery: false)
    end
  end

  test "last initially checked radio wins in artwork and interactive values" do
    html =
      "<form><label><input type='radio' name='mode' value='a' checked>Alpha</label><label><input type='radio' name='mode' value='b' checked>Beta</label></form>"

    reference = String.replace(html, "value='a' checked", "value='a'")

    for forms <- [:static, :interactive] do
      opts = [forms: forms, system_font_discovery: false]
      assert {:ok, actual} = HtmlToPdf.render(html, opts)
      assert {:ok, expected} = HtmlToPdf.render(reference, opts)
      assert actual == expected
      assert {:ok, text} = Text.extract(actual, layout: false)
      assert text =~ "Alpha" and text =~ "Beta"

      if forms == :interactive do
        assert {:ok, [%{name: "mode", type: :radio, value: "b"}]} = Forms.fields(actual)
      else
        assert {:ok, []} = Forms.fields(actual)
      end
    end
  end

  test "independent owner groups cannot silently merge into a shared PDF field name" do
    html =
      "<form id='one'><label><input type='radio' name='shared' value='a' checked>Alpha</label><input type='radio' name='shared' value='b'></form><form id='two'><label><input type='radio' name='shared' value='c' checked>Beta</label><input type='radio' name='shared' value='d'></form>"

    assert {:error, {:unsupported_form, diagnostic}} =
             HtmlToPdf.render(html, system_font_discovery: false)

    assert diagnostic.stage == :forms and diagnostic.reason == :unsupported_form
    assert diagnostic.source == "shared"
    assert diagnostic.message =~ "different HTML form owners"
    assert diagnostic.message =~ "forms: :static"
    assert {:ok, pdf} = HtmlToPdf.render(html, forms: :static, system_font_discovery: false)
    assert {:ok, []} = Forms.fields(pdf)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "Alpha" and text =~ "Beta"
  end

  test "native appearances support one thousand editable controls under default page limits" do
    for count <- [1, 100, 1_000] do
      html =
        Enum.map_join(1..count, fn i ->
          "<div><input name='field#{i}' value='Initial#{i}'></div>"
        end)

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, fields} = Forms.fields(pdf)

      assert Enum.map(fields, &{&1.name, &1.value}) ==
               Enum.map(1..count, &{"field#{&1}", "Initial#{&1}"})

      assert {:ok, context} = Reader.read_validated(pdf)
      assert length(context.pages) < Limits.get(:max_rendered_pages)

      catalogs =
        for {_ref, %{value: %{"Type" => {:name, "Catalog"}}}} <- context.document.objects,
            do: true

      assert length(catalogs) == 1

      assert {:ok, form} =
               NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

      for field <- form.fields do
        assert field.flags == 0 and field.type == :text
        [widget] = field.widgets
        normal = widget.dictionary["AP"]["N"]
        background = widget.dictionary["NEPUBackground"]
        assert {:ok, normal_dictionary} = Reader.dictionary(context.document, normal)
        assert {:ok, background_dictionary} = Reader.dictionary(context.document, background)
        assert normal_dictionary["Type"] == {:name, "XObject"}
        assert normal_dictionary["Subtype"] == {:name, "Form"}
        assert normal_dictionary["BBox"] == background_dictionary["BBox"]
        assert normal != background
      end

      values = Map.new(1..count, &{"field#{&1}", "Updated#{&1}"})
      assert {:ok, filled} = Forms.fill(pdf, values)
      assert {:ok, fields} = Forms.fields(filled)

      assert Enum.map(fields, &{&1.name, &1.value}) ==
               Enum.map(1..count, &{"field#{&1}", "Updated#{&1}"})

      assert {:ok, flat} = Forms.flatten(filled)
      assert {:ok, []} = Forms.fields(flat)
      assert {:ok, text} = Text.extract(flat)
      for i <- 1..count, do: assert(String.contains?(text, "Updated#{i}"))
      refute text =~ "Initial"
    end
  end

  test "clearing a single choice remains distinct from selecting its empty export value" do
    html = """
    <div><select name="choice"><option value="">Empty option</option><option value="B" selected>Bee</option></select></div>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, context} = Reader.read_validated(pdf)

    assert {:ok, form} =
             NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

    [field] = form.fields
    {:ref, {id, generation}} = field.ref

    assert {:ok, pdf} =
             IncrementalWriter.write(context, [
               {id, generation, {:value, Map.put(field.dictionary, "I", [1])}}
             ])

    assert {:ok, cleared} = Forms.fill(pdf, %{"choice" => nil})
    assert {:ok, [%{value: nil}]} = Forms.fields(cleared)
    assert {:ok, context} = Reader.read_validated(cleared)

    assert {:ok, form} =
             NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

    [cleared_field] = form.fields
    assert cleared_field.dictionary["V"] == nil
    refute Map.has_key?(cleared_field.dictionary, "I")
    assert {:ok, flattened} = Forms.flatten(cleared)
    assert {:error, {:no_extractable_text, _}} = Text.extract(flattened)
    assert {:ok, direct_flattened} = Forms.fill(pdf, %{"choice" => nil}, flatten: true)
    assert {:error, {:no_extractable_text, _}} = Text.extract(direct_flattened)

    assert {:ok, selected} = Forms.fill(cleared, %{"choice" => ""})
    assert {:ok, [%{value: ""}]} = Forms.fields(selected)
    assert {:ok, flattened} = Forms.flatten(selected)
    assert {:ok, "Empty option"} = Text.extract(flattened)
  end

  test "partial fills preserve the appearance request for untouched fields" do
    validator = NativeElixirPdfUtilities.Validators.FormValidator

    assert {:ok, original} =
             HtmlToPdf.render("""
             <div><input name="a" value="First"><input name="b" value="Second"></div>
             """)

    assert {:ok, context} = Reader.read_validated(original)
    assert {:ok, form} = validator.inspect_document(context)
    b = Enum.find(form.fields, &(&1.name == "b"))
    [widget] = b.widgets
    {:ref, {widget_id, widget_generation}} = widget.ref
    {:ref, {form_id, form_generation}} = context.catalog["AcroForm"]

    for missing? <- [true, false], request <- [true, false] do
      dictionary = if missing?, do: Map.delete(widget.dictionary, "AP"), else: widget.dictionary

      assert {:ok, imported} =
               IncrementalWriter.write(context, [
                 {widget_id, widget_generation, {:value, dictionary}},
                 {form_id, form_generation,
                  {:value, Map.put(form.form, "NeedAppearances", request)}}
               ])

      assert {:ok, partial} = Forms.fill(imported, %{"a" => "Updated"})
      assert {:ok, partial_context} = Reader.read_validated(partial)
      assert {:ok, partial_form} = validator.inspect_document(partial_context)
      assert partial_form.form["NeedAppearances"] == request
      untouched = Enum.find(partial_form.fields, &(&1.name == "b"))
      assert untouched.value == "Second"
      assert hd(untouched.widgets).dictionary["AP"] == dictionary["AP"]

      assert {:ok, complete} = Forms.fill(imported, %{"a" => "Updated", "b" => "Also updated"})
      assert {:ok, complete_context} = Reader.read_validated(complete)
      assert {:ok, complete_form} = validator.inspect_document(complete_context)
      assert complete_form.form["NeedAppearances"] == false

      assert Enum.all?(complete_form.fields, fn field ->
               Enum.all?(field.widgets, &Map.has_key?(&1.ap, "N"))
             end)
    end
  end

  test "generates, fills and flattens named text and unnamed checkbox fields" do
    {:ok, pdf} =
      HtmlToPdf.render(
        "<div><input name=\"full_name\" value=\"Old\"><input type=\"checkbox\"></div>"
      )

    assert {:ok, [%{name: "full_name", value: "Old"}, %{name: "CHECKBOX_1_2", value: false}]} =
             Forms.fields(pdf)

    assert {:ok, filled} =
             Forms.fill(pdf, %{"full_name" => "Cees Kettenis", "CHECKBOX_1_2" => true})

    assert {:ok, [%{value: "Cees Kettenis"}, %{value: true}]} = Forms.fields(filled)
    assert {:ok, flat} = Forms.flatten(filled, fields: ["full_name"])
    assert {:ok, [%{name: "CHECKBOX_1_2"}]} = Forms.fields(flat)
    assert {:ok, text} = Text.extract(flat)
    assert text =~ "Cees Kettenis"
    refute text =~ "Old"
    assert {:ok, all} = Forms.flatten(flat)
    assert {:ok, []} = Forms.fields(all)
    assert {:ok, _} = Reader.read(all)
    assert {:ok, ^all} = Forms.flatten(all)
    assert {:ok, ^pdf} = Forms.fill(pdf, %{})
    assert {:ok, flattened} = Forms.fill(pdf, %{"full_name" => "Cees"}, flatten: true)
    assert {:ok, [%{type: :checkbox}]} = Forms.fields(flattened)
  end

  test "flatten preserves screen visibility for mixed widgets and all-hidden pages" do
    {:ok, pdf} =
      HtmlToPdf.render("""
      <div>Public text<input name="secret" value="Confidential">
      <input type="radio" name="choice" value="shown" checked>
      <input type="radio" name="choice" value="hidden"></div>
      <div style="break-before: page"><input name="second" value="Private"></div>
      """)

    {:ok, context} = Reader.read_validated(pdf)
    {:ok, form} = NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

    for flags <- [2, 32, 36] do
      patches =
        for field <- form.fields,
            {widget, index} <- Enum.with_index(field.widgets),
            field.name != "choice" or index == 1 do
          {:ref, {id, gen}} = widget.ref
          {id, gen, {:value, Map.put(widget.dictionary, "F", flags)}}
        end

      {:ok, hidden} = IncrementalWriter.write(context, patches)
      assert {:ok, flattened} = Forms.flatten(hidden)
      assert {:ok, []} = Forms.fields(flattened)
      assert {:ok, flat_context} = Reader.read_validated(flattened)
      assert Enum.all?(flat_context.pages, &(&1.dictionary["Annots"] == []))
      assert {:ok, text} = Text.extract(flattened)
      refute text =~ "Confidential"
      refute text =~ "Private"

      assert {:ok, filled} = Forms.fill(hidden, %{"secret" => "Still private"}, flatten: true)
      assert {:ok, text} = Text.extract(filled)
      refute text =~ "Still private"
      assert {:ok, remaining} = Forms.fields(filled)
      assert Enum.map(remaining, & &1.name) == ["choice", "second"]
    end

    # Both printable and screen-only widgets retain their screen appearances.
    for flags <- [0, 1, 4] do
      field = Enum.find(form.fields, &(&1.name == "secret"))
      widget = hd(field.widgets)
      {:ref, {id, gen}} = widget.ref

      {:ok, visible} =
        IncrementalWriter.write(context, [
          {id, gen, {:value, Map.put(widget.dictionary, "F", flags)}}
        ])

      assert {:ok, flat} = Forms.flatten(visible, fields: ["secret"])
      assert {:ok, text} = Text.extract(flat)
      assert text =~ "Confidential"
    end
  end

  test "radio groups, multiline text and choices retain their semantics" do
    html = """
    <div><input type="radio" name="contact" value="email" checked>
    <input name="other"><input type="radio" name="contact" value="phone">
    <textarea name="notes">First\nSecond</textarea>
    <select name="country"><option value="NL">Netherlands</option><option value="MY" selected>Malaysia</option></select></div>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, fields} = Forms.fields(pdf)
    assert Enum.find(fields, &(&1.name == "contact")).value == "email"

    assert {:ok, filled} =
             Forms.fill(pdf, %{"contact" => "phone", "notes" => "Hello\nWorld", "country" => "NL"})

    assert {:ok, fields} = Forms.fields(filled)
    assert Enum.find(fields, &(&1.name == "contact")).value == "phone"
    assert Enum.find(fields, &(&1.name == "country")).value == "NL"
    assert {:ok, flat} = Forms.flatten(filled)
    assert {:ok, []} = Forms.fields(flat)
    assert {:ok, text} = Text.extract(flat)
    assert text =~ "Netherlands"
  end

  test "mixed disabled radio groups fail consistently across order, intervening fields and pages" do
    enabled = ~s(<input type="radio" name="contact" value="email">)
    disabled = ~s(<input type="radio" name="contact" value="phone" disabled>)

    for [first, second] <- [[enabled, disabled], [disabled, enabled]],
        separator <- ["", ~s(<input name="other">), ~s(</div><div style="break-before: page">)] do
      html = "<div>#{first}#{separator}#{second}</div>"

      assert {:error, {:unsupported_form, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :forms
      assert diagnostic.reason == :unsupported_form
      assert diagnostic.module == HtmlToPdf
      assert diagnostic.operation == :render
      assert diagnostic.source == "contact"
      assert diagnostic.message =~ "mixes enabled and disabled"
      assert diagnostic.message =~ "all members enabled or all disabled"
      assert diagnostic.message =~ "forms: :static"

      assert {:ok, static} = HtmlToPdf.render(html, forms: :static)
      assert {:ok, []} = Forms.fields(static)
    end
  end

  test "uniformly enabled and disabled radio groups keep their flags and selection in either order" do
    for disabled? <- [false, true], order <- [["email", "phone"], ["phone", "email"]] do
      controls =
        Enum.map_join(order, fn value ->
          disabled = if disabled?, do: "disabled", else: ""
          checked = if value == "email", do: "checked", else: ""
          ~s(<input type="radio" name="contact" value="#{value}" #{disabled} #{checked}>)
        end)

      assert {:ok, pdf} = HtmlToPdf.render("<div>#{controls}</div>")
      assert {:ok, [field]} = Forms.fields(pdf)
      assert field.name == "contact"
      assert field.type == :radio
      assert field.value == "email"
      assert field.read_only == disabled?
      assert Enum.sort(field.export_values) == ["email", "phone"]
      assert length(field.widgets) == 2

      if disabled? do
        assert {:error, {:read_only_form_field, diagnostic}} =
                 Forms.fill(pdf, %{"contact" => "phone"})

        assert diagnostic.stage == :forms
        assert diagnostic.module == Forms
        assert diagnostic.operation == :fill
      else
        assert {:ok, filled} = Forms.fill(pdf, %{"contact" => "phone"})
        assert {:ok, [%{value: "phone", read_only: false}]} = Forms.fields(filled)
      end
    end
  end

  test "static rendering and names after pagination" do
    html =
      "<div><input name=\"TEXT_1_2\"><input></div><div style=\"break-before: page\"><input><input type=\"checkbox\"></div>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, fields} = Forms.fields(pdf)
    assert Enum.map(fields, & &1.name) == ["TEXT_1_2", "TEXT_1_2_1", "TEXT_2_1", "CHECKBOX_2_2"]
    assert {:ok, static} = HtmlToPdf.render(html, forms: :static)
    assert {:ok, []} = Forms.fields(static)
    assert {:error, {:invalid_options, _}} = HtmlToPdf.render(html, forms: :bad)
  end

  test "rejects ambiguous HTML names and radio values" do
    for controls <- [
          "<input name=\"a\"><input name=\"a\">",
          "<input type=\"radio\" name=\"a\"><input type=\"radio\" name=\"a\">"
        ] do
      assert {:error, {:invalid_form, %{stage: :forms, message: _}}} =
               HtmlToPdf.render("<div>#{controls}</div>")
    end
  end

  test "reports public input failures with operation and field context" do
    {:ok, pdf} =
      HtmlToPdf.render(
        "<div><input name=\"name\"><input name=\"locked\" disabled><input type=\"checkbox\" name=\"agree\"></div>"
      )

    for {values, reason} <- [
          {%{"missing" => "Cees"}, :unknown_form_field},
          {%{"name" => 123}, :invalid_form_value},
          {%{"name" => "a\nb"}, :invalid_form_value},
          {%{"locked" => "a"}, :read_only_form_field},
          {%{"agree" => "yes"}, :invalid_form_value}
        ] do
      assert {:error, {^reason, %{module: Forms, operation: :fill, message: message}}} =
               Forms.fill(pdf, values)

      assert is_binary(message)
    end

    for values <- [nil, [], %{name: "a"}] do
      assert {:error, {:invalid_form_value, _}} = Forms.fill(pdf, values)
    end

    for opts <- [nil, [unknown: true], [flatten: nil], [flatten: true, flatten: false]] do
      assert {:error, {:invalid_options, _}} = Forms.fill(pdf, %{}, opts)
    end

    for opts <- [nil, [fields: 1], [fields: [1]], [fields: ["a", "a"]]] do
      assert {:error, {:invalid_options, _}} = Forms.flatten(pdf, opts)
    end

    assert {:error, {_, %{module: Forms, operation: :fields}}} = Forms.fields("bad")
    assert {:error, {_, %{module: Forms, operation: :flatten}}} = Forms.flatten("bad")
    assert {:error, {_, %{module: Forms, operation: :fill}}} = Forms.fill("bad", %{})
  end

  test "supports nested field trees and rejects cycles and XFA" do
    {:ok, pdf} = HtmlToPdf.render("<div><input name=\"child\"></div>")
    {:ok, context} = Reader.read_validated(pdf)
    {:ok, form} = Reader.dictionary(context.document, context.catalog["AcroForm"])
    [child] = form["Fields"]
    id = context.document.trailer["Size"]
    catalog = Map.put(context.catalog, "AcroForm", Map.put(form, "Fields", [{:ref, {id, 0}}]))
    {catalog_id, gen} = context.catalog_ref

    assert {:ok, nested} =
             IncrementalWriter.write(context, [
               {id, 0, {:value, %{"T" => {:string, "parent"}, "Kids" => [child]}}},
               {catalog_id, gen, {:value, catalog}}
             ])

    assert {:ok, [%{name: "parent.child"}]} = Forms.fields(nested)
    assert {:ok, filled} = Forms.fill(nested, %{"parent.child" => "Nested"}, flatten: true)
    assert {:ok, []} = Forms.fields(filled)

    assert {:ok, cyclic} =
             IncrementalWriter.write(context, [
               {id, 0, {:value, %{"T" => {:string, "parent"}, "Kids" => [{:ref, {id, 0}}]}}},
               {catalog_id, gen, {:value, catalog}}
             ])

    assert {:error, {:invalid_form, _}} = Forms.fields(cyclic)
    catalog = Map.put(context.catalog, "AcroForm", Map.put(form, "XFA", {:string, "xfa"}))
    assert {:ok, xfa} = IncrementalWriter.write(context, [{catalog_id, gen, {:value, catalog}}])
    assert {:error, {:unsupported_form, _}} = Forms.fields(xfa)
  end

  test "field and appearance resource limits" do
    {:ok, pdf} = HtmlToPdf.render("<div><input name=\"a\"></div>")
    Limits.install(Map.put(Limits.effective(), :max_pdf_form_text_bytes, 2))
    assert {:error, {:resource_limit_exceeded, %{source: "a"}}} = Forms.fill(pdf, %{"a" => "abc"})
    assert {:ok, _} = Forms.fill(pdf, %{"a" => "ab"})
    Limits.install(Map.put(Limits.effective(), :max_pdf_form_fields, 1))
    assert {:error, {:resource_limit_exceeded, _}} = HtmlToPdf.render("<div><input><input></div>")
  end

  test "HTML readonly controls preserve values, combined flags and fill diagnostics" do
    for {tag, initial, expected} <- [
          {"input", ~s(type="text" value="A&#10;B"), "AB"},
          {"input", ~s(type="search" value="A&#13;B"), "AB"},
          {"textarea", "", "Line1\nLine2"}
        ],
        attribute <- ["readonly", "READONLY", ~s(readonly="false")],
        disabled <- ["", "disabled"] do
      contents = if tag == "textarea", do: expected, else: ""
      html = ~s(<#{tag} #{initial} #{attribute} #{disabled} name="locked">#{contents}</#{tag}>)
      html = if tag == "input", do: String.replace(html, "</input>", ""), else: html
      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, [field]} = Forms.fields(pdf)
      assert field.name == "locked" and field.type == :text and field.read_only
      assert field.value == expected
      assert {:ok, context} = Reader.read_validated(pdf)

      assert {:ok, %{fields: [internal]}} =
               NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

      assert internal.flags == if(tag == "textarea", do: 4097, else: 1)

      assert {:error, {:read_only_form_field, diagnostic}} =
               Forms.fill(pdf, %{"locked" => "Changed"})

      assert diagnostic.stage == :forms and diagnostic.reason == :read_only_form_field
      assert diagnostic.module == Forms and diagnostic.operation == :fill
      assert diagnostic.source == "locked"
      assert diagnostic.message =~ "read-only"
      assert {:ok, flattened} = Forms.flatten(pdf)
      assert {:ok, []} = Forms.fields(flattened)
      assert {:ok, text} = Text.extract(flattened, layout: false)
      assert String.replace(text, ~r/\s+/u, "") == String.replace(expected, ~r/\s+/u, "")
    end
  end

  test "telephone URL and email use text filling flattening and readonly flags" do
    for type <- ["tel", "url", "email"],
        attribute <- ["", "readonly", "disabled", "readonly disabled"] do
      html = ~s(<input type="#{type}" #{attribute} name="contact" value="A&#13;B&#10;C">)
      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, [field]} = Forms.fields(pdf)
      assert field.type == :text and field.value == "ABC"
      assert field.read_only == (attribute != "")
      assert {:ok, context} = Reader.read_validated(pdf)

      assert {:ok, %{fields: [internal]}} =
               NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

      assert internal.flags == if(attribute == "", do: 0, else: 1)

      if attribute == "" do
        assert {:ok, filled} = Forms.fill(pdf, %{"contact" => "Updated"})
        assert {:ok, [%{value: "Updated"}]} = Forms.fields(filled)
      else
        assert {:error, {:read_only_form_field, %{stage: :forms, source: "contact"}}} =
                 Forms.fill(pdf, %{"contact" => "Updated"})
      end

      assert {:ok, flattened} = Forms.flatten(pdf)
      assert {:ok, []} = Forms.fields(flattened)
      assert {:ok, text} = Text.extract(flattened, layout: false)
      assert String.trim(text) == "ABC"
    end
  end

  test "multiple email remains a text field and preserves readonly flags" do
    for readonly <- ["", "readonly"], multiple <- ["multiple", ~s(multiple="false")] do
      assert {:ok, pdf} =
               HtmlToPdf.render(
                 ~s(<input type="EMAIL" #{multiple} #{readonly} name="contacts" value=" a@example.com ,&#10; b@example.com ">)
               )

      assert {:ok, [field]} = Forms.fields(pdf)
      assert field.type == :text and field.value == "a@example.com,b@example.com"
      assert field.read_only == (readonly != "")
      assert {:ok, context} = Reader.read_validated(pdf)

      assert {:ok, %{fields: [internal]}} =
               NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

      assert internal.flags == if(readonly == "", do: 0, else: 1)

      if readonly == "" do
        assert {:ok, filled} = Forms.fill(pdf, %{"contacts" => "Updated"})
        assert {:ok, [%{value: "Updated"}]} = Forms.fields(filled)
      else
        assert {:error, {:read_only_form_field, %{stage: :forms, source: "contacts"}}} =
                 Forms.fill(pdf, %{"contacts" => "Updated"})
      end

      assert {:ok, flattened} = Forms.flatten(pdf)
      assert {:ok, text} = Text.extract(flattened, layout: false)
      assert String.replace(text, ~r/\s+/, "") == "a@example.com,b@example.com"
    end

    assert {:ok, pdf} =
             HtmlToPdf.render(
               ~s(<input type="email" multiple name="contacts" value=" A&#10;B , C&#13;D ">)
             )

    assert {:ok, [%{value: "A\nB,C\rD"}]} = Forms.fields(pdf)
  end

  test "defaulted input types use text names values and readonly flags" do
    for type <- ["", "unknown", "hidden ", "checKbox"], readonly <- ["", "readonly"] do
      assert {:ok, pdf} =
               HtmlToPdf.render(~s(<input type="#{type}" #{readonly} value="A&#13;B&#10;C">))

      assert {:ok, [field]} = Forms.fields(pdf)
      assert field.name == "TEXT_1_1" and field.type == :text and field.value == "ABC"
      assert field.read_only == (readonly != "")

      if readonly == "" do
        assert {:ok, filled} = Forms.fill(pdf, %{"TEXT_1_1" => "Updated"})
        assert {:ok, [%{value: "Updated"}]} = Forms.fields(filled)
      else
        assert {:error, {:read_only_form_field, %{stage: :forms, source: "TEXT_1_1"}}} =
                 Forms.fill(pdf, %{"TEXT_1_1" => "Updated"})
      end

      assert {:ok, flattened} = Forms.flatten(pdf)
      assert {:ok, text} = Text.extract(flattened, layout: false)
      assert String.trim(text) == "ABC"
    end
  end

  test "button input labels stay in page content and do not consume field names or limits" do
    Limits.install(%{Limits.effective() | max_pdf_form_fields: 3, max_appearance_widgets: 2})

    buttons =
      ~s(<input type="submit" name="TEXT_1_1"><input type="reset" name="shared"><input type="button" name="shared" value="Custom"><button value="Unprinted"></button>)

    for forms <- [:static, :interactive] do
      assert {:ok, pdf} = HtmlToPdf.render(buttons <> ~s(<input value="Editable">), forms: forms)

      if forms == :interactive do
        assert {:ok, [%{name: "TEXT_1_1", type: :text, value: "Editable"}]} = Forms.fields(pdf)
        assert {:ok, filled} = Forms.fill(pdf, %{"TEXT_1_1" => "Updated"})
        assert {:ok, flat} = Forms.flatten(filled)
        assert {:ok, text} = Text.extract(flat, layout: false)
        assert text =~ "Submit" and text =~ "Reset" and text =~ "Custom" and text =~ "Updated"
      else
        assert {:ok, []} = Forms.fields(pdf)
      end

      assert {:ok, text} = Text.extract(pdf, layout: false)
      refute text =~ "Unprinted"
    end

    for type <- ["button", "reset", "submit"] do
      assert {:ok, pdf} = HtmlToPdf.render(~s(<input type="#{type}" name="a.b" value="Visible">))
      assert {:ok, []} = Forms.fields(pdf)
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert String.trim(text) == "Visible"
    end
  end

  test "password fields preserve values and flags while initial filled and flattened artwork stays masked" do
    assert {:ok, pdf} =
             HtmlToPdf.render(~s(<input type="PASSWORD" name="secret" value="A&#13;é🦊">))

    assert {:ok, [%{type: :text, name: "secret", value: "Aé🦊", read_only: false}]} =
             Forms.fields(pdf)

    assert {:ok, context} = Reader.read_validated(pdf)

    assert {:ok, %{fields: [%{flags: 8192}]}} =
             NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

    assert {:ok, initial} = Forms.flatten(pdf)
    assert {:ok, text} = Text.extract(initial, layout: false)
    assert String.trim(text) == "•••"

    for direct <- [true, false] do
      assert {:ok, filled} = Forms.fill(pdf, %{"secret" => "New é🦊"}, flatten: direct)
      flat = if direct, do: filled, else: elem(Forms.flatten(filled), 1)
      assert {:ok, []} = Forms.fields(flat)
      assert {:ok, text} = Text.extract(flat, layout: false)
      assert String.trim(text) == "••••••"
      refute text =~ "New"

      unless direct do
        assert {:ok, [%{value: "New é🦊"}]} = Forms.fields(filled)
      end
    end

    assert {:error, {:invalid_form_value, diagnostic}} = Forms.fill(pdf, %{"secret" => "A\nB"})

    assert diagnostic.stage == :forms and diagnostic.operation == :fill and
             diagnostic.source == "secret"

    assert {:ok, locked} =
             HtmlToPdf.render(
               ~s(<input type="password" name="secret" readonly="false" value="Initial">)
             )

    assert {:ok, context} = Reader.read_validated(locked)

    assert {:ok, %{fields: [%{flags: 8193}]}} =
             NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

    assert {:error, {:read_only_form_field, diagnostic}} =
             Forms.fill(locked, %{"secret" => "New"})

    assert diagnostic.operation == :fill and diagnostic.source == "secret"
    assert {:ok, _} = Forms.flatten(locked)
  end

  test "password fill counts rendered mask bytes in aggregate appearance limits" do
    assert {:ok, pdf} = HtmlToPdf.render(~s(<input type="password" name="secret">))
    Limits.install(Map.put(Limits.effective(), :max_appearance_text_bytes, 6))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Forms.fill(pdf, %{"secret" => "ABC"})

    assert diagnostic.stage == :forms and diagnostic.operation == :fill and
             diagnostic.source == "secret"

    assert diagnostic.message =~ "aggregate"
    assert {:ok, _} = Forms.fill(pdf, %{"secret" => "AB"})
  end

  test "password display limits propagate through fill and unsupported text flags stay rejected" do
    assert {:ok, pdf} = HtmlToPdf.render(~s(<input type="password" name="secret">))
    Limits.install(Map.put(Limits.effective(), :max_rendered_text_bytes, 8))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Forms.fill(pdf, %{"secret" => "ABC"})

    assert diagnostic.stage == :forms and diagnostic.operation == :fill and
             diagnostic.source == "secret"

    assert diagnostic.message =~ "max_rendered_text_bytes"
    assert {:ok, _} = Forms.fill(pdf, %{"secret" => "AB"})

    assert {:ok, context} = Reader.read_validated(pdf)

    assert {:ok, %{fields: [field]}} =
             NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

    {:ref, {id, generation}} = field.ref

    for flags <- [16_777_216, 33_554_432, 16_785_408, 33_562_624] do
      assert {:ok, unsupported} =
               IncrementalWriter.write(context, [
                 {id, generation, {:value, Map.put(field.dictionary, "Ff", flags)}}
               ])

      assert {:error, {:unsupported_form, diagnostic}} =
               Forms.fill(unsupported, %{"secret" => "AB"})

      assert diagnostic.stage == :forms and diagnostic.operation == :fill and
               diagnostic.source == "secret"

      assert diagnostic.message =~ "comb and rich-text"
    end
  end
end
