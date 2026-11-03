defmodule NativeElixirPdfUtilities.PdfNullDefaultsTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{Attachments, Forms, HtmlToPdf, Info, Stamp, Text}
  alias NativeElixirPdfUtilities.Pdf.{IncrementalWriter, Reader}
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  setup do
    assert {:ok, pdf} =
             HtmlToPdf.render("<p>Defaults</p>",
               page_size: {200, 200},
               margin: 0,
               system_font_discovery: false
             )

    assert {:ok, context} = Reader.read_validated(pdf)
    {:ok, pdf: pdf, context: context}
  end

  test "optional catalog and page entries use the same defaults for omitted and resolved null", %{
    pdf: pdf,
    context: context
  } do
    [page] = context.pages
    next = context.document.trailer["Size"]

    for {reference, dictionary, key, operation} <- [
          {context.catalog_ref, context.catalog, "AcroForm", &Forms.fields/1},
          {context.catalog_ref, context.catalog, "Names", &Attachments.list/1},
          {page.ref, page.dictionary, "Annots", &Forms.fields/1},
          {page.ref, page.dictionary, "UserUnit", &Info.page_sizes/1}
        ] do
      {id, generation} = reference
      assert {:ok, expected} = operation.(pdf)

      for {value, objects} <- [
            {:omitted, []},
            {nil, []},
            {{:ref, {next, 0}}, [{next, 0, {:value, nil}}]}
          ] do
        dictionary =
          if value == :omitted,
            do: Map.delete(dictionary, key),
            else: Map.put(dictionary, key, value)

        assert {:ok, candidate} =
                 IncrementalWriter.write(context, [
                   {id, generation, {:value, dictionary}} | objects
                 ])

        assert {:ok, ^expected} = operation.(candidate), "#{key}: #{inspect(value)}"
      end

      for {value, objects} <- [
            {false, []},
            {{:ref, {next, 0}}, [{next, 0, {:value, false}}]},
            {{:ref, {next, 0}}, []}
          ] do
        assert {:ok, candidate} =
                 IncrementalWriter.write(context, [
                   {id, generation, {:value, Map.put(dictionary, key, value)}} | objects
                 ])

        assert {:error, {reason, diagnostic}} = operation.(candidate)
        assert reason in [:invalid_form, :invalid_pdf_input]
        assert diagnostic.reason == reason
        assert is_atom(diagnostic.stage)
        assert is_binary(diagnostic.message) and diagnostic.message != ""
      end
    end

    assert {:error, {:invalid_pdf_input, _}} = PdfValidator.dictionary(context.document, nil)

    assert {:error, {:invalid_pdf_input, _}} =
             PdfValidator.number_array(context.document, [0, nil, 200, 200], 4)
  end

  test "all stamp operations use unit scale for omitted and resolved null UserUnit", %{
    pdf: pdf,
    context: context
  } do
    [page] = context.pages
    {id, generation} = page.ref
    next = context.document.trailer["Size"]
    assert {:ok, expected_sizes} = Info.page_sizes(pdf)

    for {value, objects} <- [
          {:omitted, []},
          {nil, []},
          {{:ref, {next, 0}}, [{next, 0, {:value, nil}}]},
          {{:ref, {next, 0}},
           [{next, 0, {:value, {:ref, {next + 1, 0}}}}, {next + 1, 0, {:value, nil}}]},
          {1, []},
          {{:ref, {next, 0}}, [{next, 0, {:value, 1}}]}
        ] do
      dictionary =
        if value == :omitted,
          do: Map.delete(page.dictionary, "UserUnit"),
          else: Map.put(page.dictionary, "UserUnit", value)

      assert {:ok, source} =
               IncrementalWriter.write(context, [{id, generation, {:value, dictionary}} | objects])

      for {operation, marker} <- [
            {&Stamp.text(&1, "Mark", system_font_discovery: false), "Mark"},
            {&Stamp.watermark(&1, "Draft", system_font_discovery: false), "Draft"},
            {&Stamp.page_numbers(&1, format: "Page {{page}}", system_font_discovery: false),
             "Page 1"},
            {&Stamp.overlay(&1, pdf), "Defaults"}
          ] do
        assert {:ok, expected} = operation.(pdf)
        assert {:ok, stamped} = operation.(source)
        assert String.starts_with?(stamped, source)
        assert {:ok, ^expected_sizes} = Info.page_sizes(stamped)
        assert {:ok, text} = Text.extract(stamped, layout: false)
        assert text =~ marker
        assert stamp_commands(stamped) == stamp_commands(expected)
      end

      assert {:ok, overlaid} = Stamp.overlay(pdf, source)
      assert {:ok, ^expected_sizes} = Info.page_sizes(overlaid)
    end
  end

  test "metadata fields treat omitted and resolved null alike when reading and updating", %{
    context: context
  } do
    next = context.document.trailer["Size"]
    context = put_in(context.document.trailer["Info"], {:ref, {next, 0}})

    for {field, key, concrete, expected} <- [
          {:title, "Title", {:string, "Title"}, "Title"},
          {:author, "Author", {:string, "Author"}, "Author"},
          {:subject, "Subject", {:string, "Subject"}, "Subject"},
          {:keywords, "Keywords", {:string, "Keywords"}, "Keywords"},
          {:producer, "Producer", {:string, "Producer"}, "Producer"},
          {:creation_date, "CreationDate", {:string, "D:20260102"}, ~N[2026-01-02 00:00:00]},
          {:modification_date, "ModDate", {:string, "D:20260304"}, ~N[2026-03-04 00:00:00]}
        ],
        {value, objects, expected} <- [
          {:omitted, [], nil},
          {nil, [], nil},
          {{:ref, {next + 1, 0}}, [{next + 1, 0, {:value, nil}}], nil},
          {{:ref, {next + 1, 0}},
           [
             {next + 1, 0, {:value, {:ref, {next + 2, 0}}}},
             {next + 2, 0, {:value, nil}}
           ], nil},
          {concrete, [], expected},
          {{:ref, {next + 1, 0}}, [{next + 1, 0, {:value, concrete}}], expected}
        ] do
      dictionary = %{"Custom" => {:string, "Preserved"}}

      dictionary =
        if value == :omitted, do: dictionary, else: Map.put(dictionary, key, value)

      assert {:ok, source} =
               IncrementalWriter.write(context, [{next, 0, {:value, dictionary}} | objects])

      assert {:ok, info} = Info.get(source)
      assert info[field] == expected
      patch_field = if field == :author, do: :title, else: :author
      assert {:ok, updated} = Info.put(source, [{patch_field, "Changed"}])
      assert String.starts_with?(updated, source)
      assert {:ok, info} = Info.get(updated)
      assert info[field] == expected
      assert info[patch_field] == "Changed"
      assert {:ok, document} = Reader.read(updated)
      assert {:ok, dictionary} = Reader.resolve(document, document.trailer["Info"])
      assert dictionary["Custom"] == {:string, "Preserved"}

      assert {:ok, removed} = Info.put(source, [{field, nil}])
      assert {:ok, info} = Info.get(removed)
      assert info[field] == nil
    end
  end

  test "a null Info dictionary is absent for both reading and metadata creation", %{
    context: context
  } do
    next = context.document.trailer["Size"]

    for {value, objects} <- [
          {:omitted, []},
          {nil, []},
          {{:ref, {next, 0}}, [{next, 0, {:value, nil}}]}
        ] do
      trailer =
        if value == :omitted,
          do: Map.delete(context.document.trailer, "Info"),
          else: Map.put(context.document.trailer, "Info", value)

      assert {:ok, source} =
               IncrementalWriter.write(put_in(context.document.trailer, trailer), objects)

      assert {:ok, info} = Info.get(source)
      assert Enum.all?(info, fn {_key, value} -> is_nil(value) end)
      assert {:ok, updated} = Info.put(source, author: "New author")
      assert {:ok, %{author: "New author", title: nil}} = Info.get(updated)
    end
  end

  test "malformed optional values still return diagnostics instead of defaults", %{
    context: context
  } do
    [page] = context.pages
    {id, generation} = page.ref
    next = context.document.trailer["Size"]

    for {value, objects} <- [
          {false, []},
          {{:ref, {next, 0}}, [{next, 0, {:value, false}}]},
          {{:ref, {next, 0}}, []},
          {{:ref, {next, 0}}, [{next, 0, {:value, {:ref, {next, 0}}}}]}
        ] do
      assert {:ok, source} =
               IncrementalWriter.write(context, [
                 {id, generation, {:value, Map.put(page.dictionary, "UserUnit", value)}} | objects
               ])

      assert {:error, {:invalid_pdf_input, diagnostic}} = Stamp.text(source, "Mark")
      assert diagnostic.stage == :geometry
      assert diagnostic.operation == :stamp_text
      assert diagnostic.message =~ "page 1"

      for dictionary <- [%{"Title" => value}, %{"CreationDate" => value}] do
        metadata_context = put_in(context.document.trailer["Info"], {:ref, {next + 1, 0}})

        assert {:ok, source} =
                 IncrementalWriter.write(metadata_context, [
                   {next + 1, 0, {:value, dictionary}} | objects
                 ])

        for result <- [Info.get(source), Info.put(source, author: "Changed")] do
          assert {:error, {:invalid_pdf_input, diagnostic}} = result
          assert diagnostic.stage == :info
          assert diagnostic.reason == :invalid_pdf_input
          assert diagnostic.message =~ "information field"
        end
      end
    end
  end

  defp stamp_commands(pdf) do
    assert {:ok, context} = Reader.read_validated(pdf)

    assert {:ok, contents} =
             PdfValidator.content_references(context.document, hd(context.pages).dictionary)

    assert {:ok, commands} = Reader.decoded_stream(context.document, List.last(contents))
    commands
  end
end
