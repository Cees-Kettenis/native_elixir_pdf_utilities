defmodule NativeElixirPdfUtilities.IndirectFormMetadataTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.Pdf.{IncrementalWriter, Reader}
  alias NativeElixirPdfUtilities.Validators.FormValidator

  setup do
    assert {:ok, pdf} =
             HtmlToPdf.render("<input name='field' value='Initial'>",
               system_font_discovery: false
             )

    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, form} = FormValidator.inspect_document(context)
    assert [field] = form.fields
    %{context: context, field: field, form: form}
  end

  test "direct and indirect metadata produce identical public fields", %{
    context: context,
    field: field
  } do
    next = context.document.trailer["Size"]

    for {key, value} <- [
          {"T", {:string, "renamed"}},
          {"FT", {:name, "Tx"}},
          {"Ff", 1},
          {"Q", 2},
          {"MaxLen", 8}
        ] do
      direct = field_pdf(context, field, %{key => value})

      indirect =
        field_pdf(context, field, %{key => {:ref, {next, 0}}}, [{next, 0, {:value, value}}])

      assert {:ok, expected} = Forms.fields(direct)
      assert Forms.fields(indirect) == {:ok, expected}
    end
  end

  test "inherited indirect metadata survives fill and flatten", %{
    context: context,
    field: field,
    form: form
  } do
    next = context.document.trailer["Size"]
    {catalog_id, catalog_generation} = context.catalog_ref
    {:ref, {field_id, field_generation}} = field.ref

    metadata = [{"FT", {:name, "Tx"}}, {"Ff", 4096}, {"Q", 2}, {"MaxLen", 8}]

    objects =
      Enum.with_index(metadata, fn {_key, value}, index -> {next + index, 0, {:value, value}} end)

    inherited =
      Enum.with_index(metadata, fn {key, _value}, index -> {key, {:ref, {next + index, 0}}} end)
      |> Map.new()

    parent_id = next + 4
    name_id = next + 5
    parent = Map.merge(inherited, %{"T" => {:string, "parent"}, "Kids" => [field.ref]})

    child =
      field.dictionary |> Map.drop(Map.keys(inherited)) |> Map.put("T", {:ref, {name_id, 0}})

    catalog =
      Map.put(context.catalog, "AcroForm", Map.put(form.form, "Fields", [{:ref, {parent_id, 0}}]))

    assert {:ok, pdf} =
             IncrementalWriter.write(
               context,
               objects ++
                 [
                   {name_id, 0, {:value, {:string, "child"}}},
                   {parent_id, 0, {:value, parent}},
                   {field_id, field_generation, {:value, child}},
                   {catalog_id, catalog_generation, {:value, catalog}}
                 ]
             )

    assert {:ok, [%{name: "parent.child", type: :text, read_only: false}]} = Forms.fields(pdf)
    assert {:ok, filled} = Forms.fill(pdf, %{"parent.child" => "Updated"})
    assert {:ok, [%{name: "parent.child", value: "Updated"}]} = Forms.fields(filled)
    assert {:ok, filled_context} = Reader.read_validated(filled)
    assert {:ok, preserved} = Reader.dictionary(filled_context.document, field.ref)
    assert preserved["T"] == {:ref, {name_id, 0}}

    assert {:ok, preserved_parent} =
             Reader.dictionary(filled_context.document, {:ref, {parent_id, 0}})

    assert Map.take(preserved_parent, Map.keys(inherited)) == inherited

    assert {:error, {:invalid_form_value, diagnostic}} =
             Forms.fill(pdf, %{"parent.child" => "Too long!"})

    assert diagnostic.message =~ "MaxLen"
    assert diagnostic.source == "parent.child"

    assert {:ok, flattened} = Forms.flatten(filled)
    assert {:ok, []} = Forms.fields(flattened)
    assert {:ok, text} = Text.extract(flattened)
    assert text =~ "Updated"
  end

  test "invalid resolved metadata and missing references retain diagnostics", %{
    context: context,
    field: field
  } do
    next = context.document.trailer["Size"]

    for {key, value} <- [{"T", 42}, {"Ff", -1}, {"Q", 3}] do
      pdf = field_pdf(context, field, %{key => {:ref, {next, 0}}}, [{next, 0, {:value, value}}])
      assert {:error, {:invalid_form, diagnostic}} = Forms.fields(pdf)
      assert diagnostic.stage == :forms
      assert diagnostic.reason == :invalid_form
      assert diagnostic.operation == :fields
      assert diagnostic.message != ""
    end

    for key <- ["T", "FT", "Ff", "Q", "MaxLen"] do
      pdf = field_pdf(context, field, %{key => {:ref, {next, 0}}})
      assert {:error, {:invalid_pdf_input, diagnostic}} = Forms.fields(pdf)
      assert diagnostic.stage == :resolution
      assert diagnostic.reason == :invalid_pdf_input
      assert diagnostic.operation == :fields
      assert diagnostic.message != ""
    end
  end

  test "indirect choice entries and pair members survive inspection, filling and flattening" do
    assert {:ok, pdf} =
             HtmlToPdf.render("<select name='choice'><option value='a'>Alpha</option></select>",
               system_font_discovery: false
             )

    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: [field]}} = FormValidator.inspect_document(context)
    next = context.document.trailer["Size"]

    for {direct, indirect, objects} <- [
          {[{:string, "a"}], [{:ref, {next, 0}}], [{next, 0, {:value, {:string, "a"}}}]},
          {[[{:string, "a"}, {:string, "Alpha"}]], [{:ref, {next, 0}}],
           [{next, 0, {:value, [{:string, "a"}, {:string, "Alpha"}]}}]},
          {[[{:string, "a"}, {:string, "Alpha"}]], [[{:ref, {next, 0}}, {:ref, {next + 1, 0}}]],
           [{next, 0, {:value, {:string, "a"}}}, {next + 1, 0, {:value, {:string, "Alpha"}}}]}
        ] do
      baseline = field_pdf(context, field, %{"Opt" => direct})
      pdf = field_pdf(context, field, %{"Opt" => indirect}, objects)
      assert Forms.fields(pdf) == Forms.fields(baseline)
      assert {:ok, filled} = Forms.fill(pdf, %{"choice" => "a"})
      assert {:ok, [%{value: "a"}]} = Forms.fields(filled)
      assert {:ok, flattened} = Forms.flatten(filled)
      assert {:ok, []} = Forms.fields(flattened)
    end
  end

  test "indirect widget subtypes preserve merged and child widget behavior", %{
    context: context,
    field: field
  } do
    next = context.document.trailer["Size"]
    [widget] = field.widgets
    {:ref, {widget_id, widget_generation}} = widget.ref

    for child? <- [false, true] do
      {context, widget_ref} =
        if child? do
          {:ref, {field_id, field_generation}} = field.ref
          [page] = context.pages
          {page_id, page_generation} = page.ref
          child_ref = {:ref, {next, 0}}
          parent = field.dictionary |> Map.delete("Subtype") |> Map.put("Kids", [child_ref])

          child =
            widget.dictionary
            |> Map.drop(~w(T FT Ff V DV DA Q Opt MaxLen Kids))
            |> Map.put("Parent", field.ref)

          assert {:ok, pdf} =
                   IncrementalWriter.write(context, [
                     {field_id, field_generation, {:value, parent}},
                     {next, 0, {:value, child}},
                     {page_id, page_generation,
                      {:value, Map.put(page.dictionary, "Annots", [child_ref])}}
                   ])

          assert {:ok, context} = Reader.read_validated(pdf)
          {context, child_ref}
        else
          {context, {:ref, {widget_id, widget_generation}}}
        end

      {:ref, {id, generation}} = widget_ref
      assert {:ok, dictionary} = Reader.dictionary(context.document, widget_ref)
      assert {:ok, expected} = Forms.fields(context.document.binary)
      name_id = context.document.trailer["Size"]

      assert {:ok, pdf} =
               IncrementalWriter.write(context, [
                 {id, generation, {:value, Map.put(dictionary, "Subtype", {:ref, {name_id, 0}})}},
                 {name_id, 0, {:value, {:name, "Widget"}}}
               ])

      assert Forms.fields(pdf) == {:ok, expected}
      assert {:ok, filled} = Forms.fill(pdf, %{"field" => "Updated"})
      assert {:ok, [%{value: "Updated", widgets: [_]}]} = Forms.fields(filled)
      assert {:ok, flattened} = Forms.flatten(filled)
      assert {:ok, []} = Forms.fields(flattened)
      assert {:ok, text} = Text.extract(flattened)
      assert text =~ "Updated"
    end
  end

  test "missing and cyclic option and widget references retain resolution diagnostics", %{
    context: context,
    field: field
  } do
    next = context.document.trailer["Size"]

    for patch <- [
          %{"Opt" => [{:ref, {next, 0}}]},
          %{"Opt" => [[{:ref, {next, 0}}, {:string, "label"}]]},
          %{"Opt" => [[{:string, "value"}, {:ref, {next, 0}}]]},
          %{"Subtype" => {:ref, {next, 0}}}
        ],
        objects <- [[], [{next, 0, {:value, {:ref, {next, 0}}}}]] do
      pdf = field_pdf(context, field, patch, objects)
      assert {:error, {:invalid_pdf_input, diagnostic}} = Forms.fields(pdf)
      assert diagnostic.stage == :resolution
      assert diagnostic.reason == :invalid_pdf_input
      assert diagnostic.operation == :fields
      assert diagnostic.message != ""
    end
  end

  defp field_pdf(context, field, patch, objects \\ []) do
    {:ref, {id, generation}} = field.ref

    assert {:ok, pdf} =
             IncrementalWriter.write(context, [
               {id, generation, {:value, Map.merge(field.dictionary, patch)}} | objects
             ])

    pdf
  end
end
