defmodule NativeElixirPdfUtilities.PdfNullDefaultsTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{Attachments, Forms, HtmlToPdf, Info}
  alias NativeElixirPdfUtilities.Pdf.{IncrementalWriter, Reader}
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  test "optional catalog and page entries use the same defaults for omitted and resolved null" do
    assert {:ok, pdf} =
             HtmlToPdf.render("<p>Defaults</p>",
               page_size: {200, 200},
               margin: 0,
               system_font_discovery: false
             )

    assert {:ok, context} = Reader.read_validated(pdf)
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
end
