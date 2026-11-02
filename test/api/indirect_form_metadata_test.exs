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

  defp field_pdf(context, field, patch, objects \\ []) do
    {:ref, {id, generation}} = field.ref

    assert {:ok, pdf} =
             IncrementalWriter.write(context, [
               {id, generation, {:value, Map.merge(field.dictionary, patch)}} | objects
             ])

    pdf
  end
end
