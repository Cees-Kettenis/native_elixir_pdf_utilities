defmodule NativeElixirPdfUtilities.FormsTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.Pdf.{Reader, IncrementalWriter}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
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
          "<input type=\"radio\" name=\"a\"><input type=\"radio\" name=\"a\">",
          "<input type=\"radio\" name=\"a\" value=\"a\" checked><input type=\"radio\" name=\"a\" value=\"b\" checked>"
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
end
