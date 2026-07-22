defmodule NativeElixirPdfUtilities.MergeTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.Merge
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Text

  @fixture_directory Path.expand("fixtures/pdf_reader", __DIR__)

  defp merge_pdf(objects, root_id \\ 1) do
    header = "%PDF-1.7\n"

    {body, offsets} =
      Enum.reduce(objects, {header, %{}}, fn {id, source}, {body, offsets} ->
        rendered = "#{id} 0 obj\n#{source}\nendobj\n"
        {body <> rendered, Map.put(offsets, id, byte_size(body))}
      end)

    maximum = Enum.max(Map.keys(offsets))
    xref_offset = byte_size(body)

    entries =
      for object <- 0..maximum do
        case Map.get(offsets, object) do
          nil -> "0000000000 " <> if(object == 0, do: "65535 f \n", else: "00000 f \n")
          offset -> String.pad_leading(Integer.to_string(offset), 10, "0") <> " 00000 n \n"
        end
      end

    body <>
      "xref\n0 #{maximum + 1}\n" <>
      Enum.join(entries) <>
      "trailer\n<< /Size #{maximum + 1} /Root #{root_id} 0 R >>\n" <>
      "startxref\n#{xref_offset}\n%%EOF\n"
  end

  test "rejects an empty input list" do
    assert {:error,
            {:empty_pdf_list,
             %{
               stage: :merge,
               reason: :empty_pdf_list,
               operation: :merge,
               module: NativeElixirPdfUtilities.Merge,
               message: "merge/1 expects at least one PDF binary"
             }}} = Merge.merge([])
  end

  test "rejects invalid input with diagnostic details" do
    assert {:error,
            {:invalid_pdf_input,
             %{
               stage: :merge,
               reason: :invalid_pdf_input,
               operation: :merge,
               module: NativeElixirPdfUtilities.Merge,
               message: "merge/1 expects a list of PDF binaries"
             }}} = Merge.merge(["%PDF-1.7", :not_pdf])

    assert {:error,
            {:invalid_pdf_input,
             %{
               stage: :merge,
               reason: :invalid_pdf_input,
               operation: :merge,
               module: NativeElixirPdfUtilities.Merge,
               message: "merge/1 expects a list of PDF binaries"
             }}} = Merge.merge(:not_a_list)

    for malformed_pdf <- [
          "garbage",
          "%PDF-1.7\n1 0 obj @ endobj",
          "%PDF-1.7\n1 0 obj <4142",
          "%PDF-1.7\n1 0 obj <<",
          "%PDF-1.7\n1 0 obj << /Length 1 >> stream\naendobj",
          "%PDF-1.7\n0 0 obj << >> endobj",
          "%PDF-1.7\n1 0 obj << >> endobj\n1 0 obj << >> endobj"
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} = Merge.merge([malformed_pdf])
      assert diagnostic.stage == :merge
      assert diagnostic.reason == :invalid_pdf_input
      assert diagnostic.operation == :merge
      assert diagnostic.module == NativeElixirPdfUtilities.Merge
      assert diagnostic.message =~ "merge/1 received an invalid PDF ("
    end

    encrypted = File.read!(Path.join(@fixture_directory, "encrypted.pdf"))
    assert {:error, {:invalid_pdf_input, diagnostic}} = Merge.merge([encrypted])
    assert diagnostic.message =~ "encrypted_pdf at encryption"
  end

  test "renumbers pages and injects inherited page attributes" do
    content = """
    BT
    1 0 0 1 0 0 Tm
    (Hi) Tj
    ET
    """

    pdf =
      merge_pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2,
         "<< /Type /Pages /Kids [ 3 0 R ] /Count 1 /Resources << /Font << /F1 4 0 R >> >> /MediaBox [ 0 0 612 792 ] >>"},
        {3, "<< /Type /Page /Parent 2 0 R /Contents 10 0 R /Annots [] >>"},
        {4, "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"},
        {10, "<< /Length #{byte_size(content)} >>\nstream\n#{content}endstream"}
      ])

    assert {:ok, merged} = Merge.merge([pdf, pdf])

    assert merged =~ "%PDF-1.7"
    assert merged =~ "/Type /Pages /Kids [ 6 0 R 17 0 R ] /Count 2"
    assert merged =~ "/Parent 1 0 R"
    assert merged =~ "/Resources << /Font << /F1 7 0 R >> >>"
    assert merged =~ "/MediaBox [ 0 0 612 792 ]"
    assert merged =~ "xref\n0 25\n"
    assert merged =~ " 00000 f"
  end

  test "uses the trailer catalog when unrelated catalog objects are present" do
    pdf =
      merge_pdf(
        [
          {1, "<< /Type /Catalog >>"},
          {3, "<< /Type /Catalog /Pages 4 0 R >>"},
          {4, "<< /Type /Pages /Kids [5 0 R] /Count 1 /MediaBox [0 0 200 300] >>"},
          {5, "<< /Type /Page /Parent 4 0 R >>"}
        ],
        3
      )

    assert {:ok, source_document} = Reader.read(pdf)
    assert hd(source_document.pages).media_box == [0, 0, 200, 300]

    assert {:ok, merged} = Merge.merge([pdf])
    assert {:ok, merged_document} = Reader.read(merged)
    assert hd(merged_document.pages).media_box == [0, 0, 200, 300]
  end

  test "handles sparse and unusual object bodies without changing stream bytes" do
    pdf =
      merge_pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
        {3,
         "<< /Type /Page /Parent 2 0 R /MediaBox [1 2 /nope 4] /Resources [] /Contents 10 0 R >>"},
        {4,
         "<< /Flag true /Other false /Nothing null /Real 1.25 /One 1.0 /Name /AName /Hex <0F> /Literal (a\\n\\r\\t\\b\\f\\(\\)\\\\\\001) /Ref 3 0 R >>"},
        {10,
         """
         << /Length 6 >>
         stream
         abc123
         endstream
         """}
      ])

    assert {:ok, merged} = Merge.merge([pdf])

    assert merged =~ "/MediaBox [ 0 0 595 842 ]"
    assert merged =~ "/Flag true /Other false /Nothing null"
    assert merged =~ "/Real 1.25 /One 1"
    assert merged =~ "/Hex <0F>"
    assert merged =~ "/Literal (a\\n\\r\\t\\b\\f\\("
    assert merged =~ "\nstream\nabc123\nendstream"
  end

  test "replaces a scalar MediaBox with the default page box" do
    pdf =
      merge_pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
        {3, "<< /Type /Page /Parent 2 0 R /MediaBox 42 >>"}
      ])

    assert {:ok, merged} = Merge.merge([pdf])
    assert merged =~ "/MediaBox [ 0 0 595 842 ]"
  end

  test "rejects inputs without valid catalogs and page trees" do
    no_catalog = merge_pdf([{1, "<< /Type /NotCatalog >>"}])
    no_root_pages = merge_pdf([{1, "<< /Type /Catalog >>"}, {2, "<< /Type /NotPage >>"}])

    assert {:error, {:invalid_pdf_input, _diagnostic}} = Merge.merge([no_catalog])
    assert {:error, {:invalid_pdf_input, _diagnostic}} = Merge.merge([no_root_pages])
  end

  test "covers page defaults without inherited resources" do
    pdf =
      merge_pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [ 3 0 R ] /Count 1 /Resources [] >>"},
        {3, "<< /Type /Page /Resources [] /MediaBox [ 0 0 200 300 ] >>"}
      ])

    assert {:ok, merged} = Merge.merge([pdf])

    assert merged =~ "/MediaBox [ 0 0 200 300 ]"
    assert merged =~ "/Resources [ ]"
  end

  test "handles nested page dictionaries" do
    nested_page_pdf =
      merge_pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [ 3 0 R ] /Count 1 >>"},
        {3,
         "<< /Type /Page /Resources << /ProcSet [ /PDF ] /Font << /F1 4 0 R >> >> /MediaBox [ 0.5 0 200.25 300 ] /Contents 5 0 R /AltParent 3 0 R >>"},
        {4, "<< /Type /Font >>"},
        {5, "<< /Length 2 >> stream\nHi\nendstream"}
      ])

    assert {:ok, merged} = Merge.merge([nested_page_pdf])

    assert merged =~ "/MediaBox [ 0.5 0 200.25 300 ]"
    assert merged =~ "/Resources << /ProcSet [ /PDF ] /Font << /F1"
    assert merged =~ "/AltParent 6 0 R"
  end

  test "preserves inherited page attributes from intermediate Pages nodes" do
    pdf =
      merge_pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [ 3 0 R ] /Count 1 >>"},
        {3,
         "<< /Type /Pages /Parent 2 0 R /Kids [ 4 0 R ] /Count 1 /MediaBox [ 0 0 123 456 ] /Resources << /Font << /F1 5 0 R >> >> >>"},
        {4, "<< /Type /Page /Parent 3 0 R /Contents 6 0 R >>"},
        {5, "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"},
        {6, "<< /Length 6 >> stream\nBT ET\nendstream"}
      ])

    assert {:ok, merged} = Merge.merge([pdf])
    assert merged =~ "/MediaBox [ 0 0 123 456 ]"
    assert merged =~ "/Resources << /Font << /F1 8 0 R >> >>"
  end

  test "remaps Parent references outside rewritten Page objects" do
    pdf =
      merge_pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
        {3, "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 100 100] >>"},
        {4, "<< /Type /Example /Parent 1 0 R >>"}
      ])

    assert {:ok, merged} = Merge.merge([pdf])
    assert merged =~ "/Type /Example /Parent 4 0 R"
  end

  test "handles empty and cyclic Pages trees without looping" do
    empty_pages =
      merge_pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [] /Count 0 >>"}
      ])

    cyclic_pages =
      merge_pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [ 2 0 R ] /Count 0 >>"}
      ])

    assert {:ok, empty_output} = Merge.merge([empty_pages])
    assert empty_output =~ "/Type /Pages /Kids [  ] /Count 0"

    assert {:error, {:invalid_pdf_input, _diagnostic}} = Merge.merge([cyclic_pages])
  end

  test "merges xref-stream and object-stream PDFs through the shared reader" do
    xref_stream = File.read!(Path.join(@fixture_directory, "xref-stream.pdf"))
    object_stream = File.read!(Path.join(@fixture_directory, "object-stream.pdf"))

    assert {:ok, merged} = Merge.merge([xref_stream, object_stream])
    assert {:ok, document} = Reader.read(merged)
    assert length(document.pages) == 2

    assert Text.extract(merged, layout: false) ==
             {:ok, "Reader milestone fixture\nReader milestone fixture"}
  end
end
