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
      refute diagnostic.stage == :merge
      assert diagnostic.reason == :invalid_pdf_input
      assert diagnostic.operation == :merge
      assert diagnostic.module == NativeElixirPdfUtilities.Merge
      assert is_binary(diagnostic.message)
    end

    encrypted = File.read!(Path.join(@fixture_directory, "encrypted.pdf"))

    assert {:error,
            {:encrypted_pdf,
             %{
               stage: :encryption,
               reason: :encrypted_pdf,
               operation: :merge,
               module: NativeElixirPdfUtilities.Merge
             }}} = Merge.merge([encrypted])
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

  test "does not classify a nested Type Page value as a page object" do
    pdf =
      merge_pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
        {3,
         "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 100 100] /Resources <<>> /PieceInfo 5 0 R >>"},
        {5, "<< /Meta << /Type /Page >> >>"}
      ])

    assert {:ok, source_document} = Reader.read(pdf)
    assert length(source_document.pages) == 1

    assert {:ok, merged} = Merge.merge([pdf])
    assert {:ok, merged_document} = Reader.read(merged)
    assert length(merged_document.pages) == 1

    assert Enum.any?(merged_document.objects, fn {_ref, object} ->
             object.value == %{"Meta" => %{"Type" => {:name, "Page"}}}
           end)
  end

  test "resolves an indirect Pages Kids array through the shared reader model" do
    pdf =
      merge_pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids 4 0 R /Count 5 0 R /MediaBox [0 0 200 300] >>"},
        {3, "<< /Type /Page /Parent 2 0 R >>"},
        {4, "[3 0 R]"},
        {5, "1"}
      ])

    assert {:ok, source_document} = Reader.read(pdf)
    assert length(source_document.pages) == 1

    assert {:ok, merged} = Merge.merge([pdf])
    assert {:ok, merged_document} = Reader.read(merged)
    assert [page] = merged_document.pages
    assert page.media_box == [0, 0, 200, 300]
  end

  test "handles sparse and unusual object bodies without changing stream bytes" do
    pdf =
      merge_pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
        {3,
         "<< /Type /Page /Parent 2 0 R /MediaBox [1 2 300 400] /Resources [] /Contents 10 0 R >>"},
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

    assert merged =~ "/MediaBox [ 1 2 300 400 ]"
    assert merged =~ "/Flag true /Other false /Nothing null"
    assert merged =~ "/Real 1.25 /One 1"
    assert merged =~ "/Hex <0F>"
    assert merged =~ "/Literal (a\\n\\r\\t\\b\\f\\("
    assert merged =~ "\nstream\nabc123\nendstream"
  end

  test "re-encodes escaped PDF names without changing their values" do
    pdf =
      merge_pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
        {3,
         "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 100 100] /F#20X /A#2FB /Hash#23Name /Paren#28Value#29 /Binary#FF /Control#01 >>"}
      ])

    assert {:ok, source_document} = Reader.read(pdf)
    source_page = hd(source_document.pages)
    assert {:ok, source_dictionary} = Reader.dictionary(source_document, {:ref, source_page.ref})
    assert source_dictionary["F X"] == {:name, "A/B"}
    assert source_dictionary["Hash#Name"] == {:name, "Paren(Value)"}
    assert source_dictionary[<<"Binary", 255>>] == {:name, <<"Control", 1>>}

    assert {:ok, merged} = Merge.merge([pdf])
    assert merged =~ "/F#20X /A#2FB"
    assert merged =~ "/Hash#23Name /Paren#28Value#29"
    assert merged =~ "/Binary#FF /Control#01"
    refute merged =~ "/F X"

    assert {:ok, merged_document} = Reader.read(merged)
    merged_page = hd(merged_document.pages)
    assert {:ok, merged_dictionary} = Reader.dictionary(merged_document, {:ref, merged_page.ref})
    assert merged_dictionary["F X"] == {:name, "A/B"}
    assert merged_dictionary["Hash#Name"] == {:name, "Paren(Value)"}
    assert merged_dictionary[<<"Binary", 255>>] == {:name, <<"Control", 1>>}
  end

  test "rejects a malformed MediaBox instead of silently replacing it" do
    pdf =
      merge_pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
        {3, "<< /Type /Page /Parent 2 0 R /MediaBox 42 >>"}
      ])

    assert {:error, {:invalid_pdf_input, diagnostic}} = Merge.merge([pdf])
    assert diagnostic.stage == :merge
    assert diagnostic.reason == :invalid_pdf_input
    assert diagnostic.source == "page 3"
    assert diagnostic.message =~ "page 3 has a malformed effective MediaBox"
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

  test "materializes all inherited page attributes and resolves indirect values" do
    pdf =
      merge_pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2,
         "<< /Type /Pages /Kids [3 0 R] /Count 1 /Resources 8 0 R /MediaBox 9 0 R /CropBox 10 0 R /Rotate 11 0 R >>"},
        {3,
         "<< /Type /Page /Parent 2 0 R /BleedBox [1 2 199 99] /TrimBox [2 3 198 98] /ArtBox [3 4 197 97] /UserUnit 2 >>"},
        {8, "<< /ProcSet [/PDF] >>"},
        {9, "12 0 R"},
        {10, "[10 5 190 95]"},
        {11, "17 0 R"},
        {12, "[13 0 R 14 0 R 15 0 R 16 0 R]"},
        {13, "0"},
        {14, "0"},
        {15, "200"},
        {16, "100"},
        {17, "90"}
      ])

    assert {:ok, merged} = Merge.merge([pdf])
    assert {:ok, document} = Reader.read(merged)
    page = hd(document.pages)
    assert {:ok, dictionary} = Reader.dictionary(document, {:ref, page.ref})

    assert dictionary["MediaBox"] == [0, 0, 200, 100]
    assert dictionary["CropBox"] == [10, 5, 190, 95]
    assert dictionary["Rotate"] == 90
    assert dictionary["BleedBox"] == [1, 2, 199, 99]
    assert dictionary["TrimBox"] == [2, 3, 198, 98]
    assert dictionary["ArtBox"] == [3, 4, 197, 97]
    assert dictionary["UserUnit"] == 2

    assert {:ok, %{"ProcSet" => [{:name, "PDF"}]}} =
             Reader.resolve(document, dictionary["Resources"])
  end

  test "uses the nearest page-tree value and page-local overrides" do
    pdf =
      merge_pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2,
         "<< /Type /Pages /Kids [3 0 R] /Count 1 /Resources << /Marker /Root >> /MediaBox [0 0 600 800] /CropBox [0 0 590 790] /Rotate 0 >>"},
        {3,
         "<< /Type /Pages /Parent 2 0 R /Kids [4 0 R] /Count 1 /Resources << /Marker /Intermediate >> /MediaBox [0 0 300 400] /CropBox [1 1 290 390] /Rotate 90 >>"},
        {4,
         "<< /Type /Page /Parent 3 0 R /Resources << /Marker /Page >> /CropBox [2 2 280 380] /Rotate 180 >>"}
      ])

    assert {:ok, merged} = Merge.merge([pdf])
    assert {:ok, document} = Reader.read(merged)
    page = hd(document.pages)
    assert {:ok, dictionary} = Reader.dictionary(document, {:ref, page.ref})

    assert dictionary["Resources"] == %{"Marker" => {:name, "Page"}}
    assert dictionary["MediaBox"] == [0, 0, 300, 400]
    assert dictionary["CropBox"] == [2, 2, 280, 380]
    assert dictionary["Rotate"] == 180
  end

  test "keeps CropBox and Rotate defaults implicit" do
    pdf =
      merge_pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 200 100] >>"},
        {3, "<< /Type /Page /Parent 2 0 R >>"}
      ])

    assert {:ok, merged} = Merge.merge([pdf])
    assert {:ok, document} = Reader.read(merged)
    page = hd(document.pages)
    assert {:ok, dictionary} = Reader.dictionary(document, {:ref, page.ref})

    assert dictionary["MediaBox"] == [0, 0, 200, 100]
    refute Map.has_key?(dictionary, "CropBox")
    refute Map.has_key?(dictionary, "Rotate")
  end

  test "rejects missing or malformed effective page geometry" do
    invalid_pages = [
      {"", "missing an effective MediaBox"},
      {"/MediaBox [0 0 /bad 100]", "malformed effective MediaBox"},
      {"/MediaBox [0 0 200 100] /CropBox 9 0 R", "malformed effective CropBox"},
      {"/MediaBox [0 0 200 100] /CropBox [0 0 180 /bad]", "malformed effective CropBox"},
      {"/MediaBox [0 0 200 100] /Rotate 45", "malformed effective Rotate"},
      {"/MediaBox [0 0 200 100] /Rotate 90.0", "malformed effective Rotate"}
    ]

    for {geometry, message} <- invalid_pages do
      pdf =
        merge_pdf([
          {1, "<< /Type /Catalog /Pages 2 0 R >>"},
          {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
          {3, "<< /Type /Page /Parent 2 0 R #{geometry} >>"}
        ])

      assert {:error, {:invalid_pdf_input, diagnostic}} = Merge.merge([pdf])
      assert diagnostic.stage == :merge
      assert diagnostic.source == "page 3"
      assert diagnostic.message =~ message
    end
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
