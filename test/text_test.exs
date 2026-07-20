defmodule NativeElixirPdfUtilities.TextTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.Text

  test "extracts a Type0 ToUnicode CMap without mixing it with another font" do
    cmap =
      "begincmap\n1 begincodespacerange\n<0000> <FFFF>\nendcodespacerange\n2 beginbfchar\n<0001> <0048>\n<0002> <0069>\nendbfchar\nendcmap"

    content = "BT /F1 12 Tf 1 0 0 1 10 20 Tm [<00010002>] TJ ET"

    pdf =
      page_pdf(content,
        font:
          "<< /Type /Font /Subtype /Type0 /Encoding /Identity-H /DescendantFonts [8 0 R] /ToUnicode 7 0 R >>",
        cmap: cmap
      )

    assert Text.extract(pdf, layout: false) == {:ok, "Hi"}
  end

  test "extracts FCI-style WinAnsi text from arbitrary font resource names" do
    pdf =
      page_pdf(<<"BT /f-0-0 12 Tf (Caf", 233, ") Tj ET">>,
        font_name: "f-0-0",
        font: "<< /Type /Font /Subtype /TrueType /Encoding /WinAnsiEncoding >>"
      )

    assert Text.extract(pdf, layout: false) == {:ok, "Café"}
  end

  test "supports bfrange arrays, escaped literal strings, hex strings, and TJ adjustments" do
    cmap =
      "begincmap\n1 begincodespacerange\n<00> <FF>\nendcodespacerange\n3 beginbfchar\n<28> <0028>\n<58> <0058>\n<29> <0029>\nendbfchar\n1 beginbfrange\n<01> <03> [<0041> <0042> <0043>]\nendbfrange\nendcmap"

    content = "BT /ABC 10 Tf <01> Tj [<02> -120 <03>] TJ (\\(X\\)) Tj ET"

    pdf =
      page_pdf(content, font_name: "ABC", font: "<< /Type /Font /ToUnicode 7 0 R >>", cmap: cmap)

    assert Text.extract(pdf, layout: false) == {:ok, "A BC (X)"}
  end

  test "uses inherited resources, multiple content streams, and Form XObjects" do
    form =
      stream_object(
        "/Type /XObject /Subtype /Form /Resources << /Font << /f-1-0 5 0 R >> >>",
        "BT /f-1-0 10 Tf (Form) Tj ET"
      )

    objects = [
      {1, "<< /Type /Catalog /Pages 2 0 R >>"},
      {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 612 792] /Resources 4 0 R >>"},
      {3, "<< /Type /Page /Parent 2 0 R /Contents [6 0 R 7 0 R] >>"},
      {4, "<< /Font << /f-1-0 5 0 R >> /XObject << /x-form 8 0 R >> >>"},
      {5, "<< /Type /Font /Subtype /TrueType /Encoding /WinAnsiEncoding >>"},
      {6, stream_object("", "BT /f-1-0 10 Tf (First) Tj ET")},
      {7, stream_object("", "BT /f-1-0 10 Tf (Second) Tj /x-form Do ET")},
      {8, form}
    ]

    assert Text.extract(pdf(objects), layout: false) == {:ok, "First Second Form"}
  end

  test "opens objects stored in an object stream" do
    path = Path.expand("fixtures/pdf_reader/object-stream.pdf", __DIR__)
    assert Text.extract(File.read!(path), layout: false) == {:ok, "Reader milestone fixture"}
  end

  test "layout mode keeps positional columns" do
    content =
      "BT /F1 12 Tf 1 0 0 1 10 20 Tm (DESC) Tj 1 0 0 1 240 20 Tm (USE) Tj 1 0 0 1 10 36 Tm (NEXT) Tj ET"

    assert {:ok, text} = Text.extract(page_pdf(content))
    [first, second] = String.split(text, "\n")
    assert first == "NEXT"
    assert second =~ "DESC"
    assert second =~ "USE"
  end

  test "rejects malformed xrefs, bad stream lengths, and encrypted PDFs" do
    malformed =
      page_pdf("BT /F1 10 Tf (x) Tj ET")
      |> String.replace(~r/startxref\n\d+/, "startxref\n999999")

    assert_error(Text.extract(malformed), :invalid_pdf_input, :xref)

    bad_length = page_pdf("BT /F1 10 Tf (x) Tj ET") |> String.replace("/Length 22", "/Length 21")
    assert_error(Text.extract(bad_length), :invalid_pdf_input, :object)

    encrypted =
      pdf(
        [{1, "<< /Type /Catalog /Pages 2 0 R >>"}, {2, "<< /Type /Pages /Kids [] /Count 0 >>"}],
        "/Root 1 0 R /Encrypt 9 0 R"
      )

    assert_error(Text.extract(encrypted), :encrypted_pdf, :encryption)
  end

  test "rejects unsupported filters, image-only PDFs, and custom encodings without Unicode" do
    unsupported =
      page_pdf("BT /F1 10 Tf (x) Tj ET", content_dictionary: "/Filter /DCTDecode")

    assert_error(Text.extract(unsupported), :unsupported_pdf_feature, :filter)

    image_only =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
        {3, "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] >>"}
      ])

    assert_error(Text.extract(image_only), :no_extractable_text, :text_extraction)

    custom =
      page_pdf("BT /F1 10 Tf <01> Tj ET",
        font: "<< /Type /Font /Subtype /Type0 /Encoding /Identity-H /DescendantFonts [8 0 R] >>"
      )

    assert_error(Text.extract(custom), :unsupported_text_encoding, :text_encoding)
  end

  test "reports option, input, and file diagnostics" do
    assert_error(Text.extract(:not_a_pdf), :invalid_pdf_input, :input)
    assert_error(Text.extract("%PDF-1.7", [:not_a_keyword]), :invalid_options, :options)

    assert {:error, {:invalid_path, %{stage: :file, operation: :extract_file, module: Text}}} =
             Text.extract_file(:not_a_path)

    missing = Path.join(System.tmp_dir!(), "native-elixir-pdf-missing.pdf")

    assert {:error, {:enoent, %{source: ^missing, operation: :read, module: Text}}} =
             Text.extract_file(missing)
  end

  test "adds source context when extraction from a file fails" do
    path = Path.join(System.tmp_dir!(), "native-elixir-pdf-encrypted.pdf")

    File.write!(
      path,
      pdf(
        [{1, "<< /Type /Catalog /Pages 2 0 R >>"}, {2, "<< /Type /Pages /Kids [] /Count 0 >>"}],
        "/Root 1 0 R /Encrypt 9 0 R"
      )
    )

    assert {:error, {:encrypted_pdf, %{source: ^path, operation: :extract_file, module: Text}}} =
             Text.extract_file(path)
  after
    File.rm(Path.join(System.tmp_dir!(), "native-elixir-pdf-encrypted.pdf"))
  end

  test "decodes PNG and TIFF predictors without raising" do
    decoded = "BT /F1 12 Tf (Predictor) Tj ET"
    png_data = :zlib.compress(<<0>> <> decoded)

    png_pdf =
      page_pdf(png_data,
        content_dictionary:
          "/Filter /FlateDecode /DecodeParms << /Predictor 15 /Colors 1 /BitsPerComponent 8 /Columns #{byte_size(decoded)} >>"
      )

    assert Text.extract(png_pdf, layout: false) == {:ok, "Predictor"}

    {tiff_data, _previous} =
      decoded
      |> :binary.bin_to_list()
      |> Enum.map_reduce(0, fn byte, previous -> {Integer.mod(byte - previous, 256), byte} end)

    tiff_data = tiff_data |> :erlang.list_to_binary() |> :zlib.compress()

    tiff_pdf =
      page_pdf(tiff_data,
        content_dictionary:
          "/Filter /FlateDecode /DecodeParms << /Predictor 2 /Colors 1 /BitsPerComponent 8 /Columns #{byte_size(decoded)} >>"
      )

    assert Text.extract(tiff_pdf, layout: false) == {:ok, "Predictor"}
  end

  test "returns diagnostics for malformed predictor parameters" do
    decoded = "BT /F1 12 Tf (Predictor) Tj ET"

    pdf =
      page_pdf(:zlib.compress(<<0>> <> decoded),
        content_dictionary:
          "/Filter /FlateDecode /DecodeParms << /Predictor 15 /Columns /invalid >>"
      )

    assert_error(Text.extract(pdf), :invalid_pdf_input, :filter)
  end

  test "rejects malformed text operations instead of returning partial text" do
    pdf = page_pdf("BT /F1 12 Tf (OK) Tj (LOST) (EXTRA) Tj ET")
    assert_error(Text.extract(pdf), :invalid_pdf_input, :content)
  end

  test "rejects unknown simple-font encodings instead of guessing ASCII" do
    pdf =
      page_pdf("BT /F1 12 Tf (ABC) Tj ET",
        font: "<< /Type /Font /Subtype /Type1 /Encoding /MadeUpEncoding >>"
      )

    assert {:error, {:unsupported_text_encoding, diagnostic}} = Text.extract(pdf)
    assert diagnostic.stage == :text_encoding
    assert diagnostic.message =~ "; page 1"
    assert diagnostic.message =~ "; font F1"
    refute Map.has_key?(diagnostic, :page)
    refute Map.has_key?(diagnostic, :font)
  end

  test "decodes all supported standard simple-font encodings" do
    cases = [
      {"StandardEncoding", <<39>>, "’"},
      {"WinAnsiEncoding", <<128>>, "€"},
      {"MacRomanEncoding", <<128>>, "Ä"},
      {"MacExpertEncoding", <<86>>, "ﬀ"},
      {"SymbolEncoding", <<65>>, "Α"},
      {"ZapfDingbatsEncoding", <<33>>, "✁"},
      {"PDFDocEncoding", <<128>>, "•"}
    ]

    for {encoding, source, expected} <- cases do
      pdf =
        page_pdf(<<"BT /F1 12 Tf (", source::binary, ") Tj ET">>,
          font: "<< /Type /Font /Subtype /Type1 /Encoding /#{encoding} >>"
        )

      assert Text.extract(pdf, layout: false) == {:ok, expected}
    end
  end

  test "decodes Differences with Adobe glyph names and rejects malformed arrays" do
    font =
      "<< /Type /Font /Subtype /Type1 /Encoding << /BaseEncoding /WinAnsiEncoding /Differences [65 /Aacute 66 /f_f_i] >> >>"

    assert Text.extract(page_pdf("BT /F1 12 Tf (AB) Tj ET", font: font), layout: false) ==
             {:ok, "Áffi"}

    malformed_font =
      "<< /Type /Font /Subtype /Type1 /Encoding << /BaseEncoding /WinAnsiEncoding /Differences [/Aacute] >> >>"

    assert_error(
      Text.extract(page_pdf("BT /F1 12 Tf (A) Tj ET", font: malformed_font)),
      :invalid_pdf_input,
      :text_encoding
    )
  end

  test "decodes high-byte bfrange sources without treating bytes as UTF-8 strings" do
    cmap =
      "begincmap\n1 begincodespacerange\n<00> <FF>\nendcodespacerange\n1 beginbfrange\n<80> <81> <0041>\nendbfrange\nendcmap"

    pdf =
      page_pdf("BT /F1 12 Tf <8081> Tj ET",
        font:
          "<< /Type /Font /Subtype /Type0 /Encoding /Identity-H /DescendantFonts [8 0 R] /ToUnicode 7 0 R >>",
        cmap: cmap
      )

    assert Text.extract(pdf, layout: false) == {:ok, "AB"}
  end

  test "rejects unsupported CMap inheritance and malformed declared counts" do
    inherited =
      "/Adobe-Identity-UCS usecmap\n1 begincodespacerange\n<00> <FF>\nendcodespacerange\n1 beginbfchar\n<01> <0041>\nendbfchar"

    font =
      "<< /Type /Font /Subtype /Type0 /Encoding /Identity-H /DescendantFonts [8 0 R] /ToUnicode 7 0 R >>"

    assert_error(
      Text.extract(page_pdf("BT /F1 12 Tf <01> Tj ET", font: font, cmap: inherited)),
      :unsupported_text_encoding,
      :cmap
    )

    malformed =
      "begincmap\n2 begincodespacerange\n<00> <FF>\nendcodespacerange\n1 beginbfchar\n<01> <0041>\nendbfchar"

    assert_error(
      Text.extract(page_pdf("BT /F1 12 Tf <01> Tj ET", font: font, cmap: malformed)),
      :invalid_pdf_input,
      :cmap
    )
  end

  test "uses page rotation when ordering layout text" do
    content =
      "BT /F1 12 Tf 1 0 0 1 10 20 Tm (FIRST) Tj 1 0 0 1 40 20 Tm (SECOND) Tj ET"

    assert {:ok, "FIRST\nSECOND"} = Text.extract(page_pdf(content, rotate: 90))
  end

  test "TJ and Tj advance text by the same font widths" do
    font =
      "<< /Type /Font /Subtype /Type1 /Encoding /WinAnsiEncoding /FirstChar 65 /Widths [1000 1000] >>"

    tj = page_pdf("BT /F1 20 Tf 1 0 0 1 10 20 Tm (A) Tj (B) Tj ET", font: font)
    array = page_pdf("BT /F1 20 Tf 1 0 0 1 10 20 Tm [(A)] TJ (B) Tj ET", font: font)

    assert Text.extract(tj) == Text.extract(array)
  end

  test "returns diagnostics for missing Form resources and malformed Form matrices" do
    missing = page_pdf("BT /F1 12 Tf (OK) Tj /missing Do ET")
    assert_error(Text.extract(missing), :invalid_pdf_input, :resolution)

    objects = [
      {1, "<< /Type /Catalog /Pages 2 0 R >>"},
      {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 612 792] >>"},
      {3, "<< /Type /Page /Parent 2 0 R /Resources 4 0 R /Contents 6 0 R >>"},
      {4, "<< /Font << /F1 5 0 R >> /XObject << /form 7 0 R >> >>"},
      {5, "<< /Type /Font /Subtype /Type1 /Encoding /WinAnsiEncoding >>"},
      {6, stream_object("", "/form Do")},
      {7,
       stream_object(
         "/Type /XObject /Subtype /Form /Matrix [1 0 /bad 1 0 0] /Resources << /Font << /F1 5 0 R >> >>",
         "BT /F1 12 Tf (Form) Tj ET"
       )}
    ]

    assert_error(Text.extract(pdf(objects)), :invalid_pdf_input, :content)
  end

  test "does not return text painted with an invisible rendering mode" do
    pdf = page_pdf("BT /F1 12 Tf (Visible) Tj 3 Tr (Hidden) Tj ET")
    assert Text.extract(pdf, layout: false) == {:ok, "Visible"}
  end

  test "extract_spans preserves rendering modes that the string projection excludes" do
    pdf =
      page_pdf("BT /F1 12 Tf 1 0 0 1 10 20 Tm (Painted) Tj 3 Tr (Hidden) Tj 7 Tr (Clip) Tj ET")

    assert {:ok, %{page_count: 1, pages: [%{number: 1, spans: spans}]}} =
             Text.extract_spans(pdf)

    assert Enum.map(spans, & &1.text) == ["Painted", "Hidden", "Clip"]
    assert Enum.map(spans, & &1.source_index) == [0, 1, 2]

    assert Enum.map(spans, &{&1.render_mode, &1.paints_text?, &1.adds_to_clip_path?}) == [
             {0, true, false},
             {3, false, false},
             {7, false, true}
           ]

    assert Text.extract(pdf, layout: false) == {:ok, "Painted"}
    assert Text.extract(pdf, layout: true) == {:ok, "Painted"}
  end

  test "extract_spans exposes baseline geometry, matrices, font context, and TJ joins" do
    font =
      "<< /Type /Font /Subtype /Type1 /Encoding /WinAnsiEncoding /FirstChar 65 /Widths [1000 1000] >>"

    pdf = page_pdf("BT /F1 20 Tf 1 0 0 1 10 20 Tm [(A) -100 (B)] TJ ET", font: font)

    assert {:ok, %{pages: [%{media_box: [0, 0, 612, 792], rotation: 0, spans: [a, b]}]}} =
             Text.extract_spans(pdf)

    assert %{
             text: "A",
             source_index: 0,
             font_resource: "F1",
             joins_previous?: false
           } = a

    assert {a.x, a.y, a.end_x, a.end_y, a.font_size} == {10.0, 772.0, 30.0, 772.0, 20.0}
    assert a.text_matrix == [1.0, 0.0, 0.0, 1.0, 10.0, 20.0]
    assert a.ctm == [1.0, 0.0, 0.0, 1.0, 0.0, 0.0]
    assert b.text == "B"
    assert b.source_index == 1
    assert b.x == 32.0
    assert b.end_x == 52.0
    assert b.joins_previous?
  end

  test "extract_spans keeps source order and can provide best-effort visual order" do
    content =
      "BT /F1 12 Tf 1 0 0 1 100 20 Tm (RIGHT) Tj " <>
        "1 0 0 1 10 20.8 Tm (LEFT) Tj 1 0 0 1 10 40 Tm (TOP) Tj ET"

    pdf = page_pdf(content)

    assert {:ok, %{pages: [%{spans: source}]}} = Text.extract_spans(pdf)
    assert Enum.map(source, & &1.text) == ["RIGHT", "LEFT", "TOP"]
    assert Enum.map(source, & &1.source_index) == [0, 1, 2]
    assert_in_delta Enum.at(source, 0).y, Enum.at(source, 1).y, 1.0

    assert {:ok, %{pages: [%{spans: visual}]}} = Text.extract_spans(pdf, order: :visual)
    assert Enum.map(visual, & &1.text) == ["TOP", "LEFT", "RIGHT"]
    assert Enum.map(visual, & &1.source_index) == [2, 1, 0]
  end

  test "extract_spans preserves multiple pages including an empty page" do
    objects = [
      {1, "<< /Type /Catalog /Pages 2 0 R >>"},
      {2,
       "<< /Type /Pages /Kids [3 0 R 4 0 R 9 0 R] /Count 3 /MediaBox [0 0 612 792] /Resources 5 0 R >>"},
      {3, "<< /Type /Page /Parent 2 0 R /Contents 7 0 R >>"},
      {4, "<< /Type /Page /Parent 2 0 R >>"},
      {9, "<< /Type /Page /Parent 2 0 R /Contents 8 0 R >>"},
      {5, "<< /Font << /F1 6 0 R >> >>"},
      {6, "<< /Type /Font /Subtype /Type1 /Encoding /WinAnsiEncoding >>"},
      {7, stream_object("", "BT /F1 12 Tf (One) Tj ET")},
      {8, stream_object("", "BT /F1 12 Tf (Two) Tj ET")}
    ]

    pdf = pdf(objects)

    assert {:ok, %{page_count: 3, pages: pages}} = Text.extract_spans(pdf)
    assert Enum.map(pages, & &1.number) == [1, 2, 3]

    assert Enum.map(pages, fn page -> Enum.map(page.spans, & &1.text) end) == [
             ["One"],
             [],
             ["Two"]
           ]

    assert Text.extract(pdf, layout: false) == {:ok, "One\nTwo"}
  end

  test "extract_spans returns empty pages for a valid PDF without text" do
    image_only =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
        {3, "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] >>"}
      ])

    assert Text.extract_spans(image_only) ==
             {:ok,
              %{
                page_count: 1,
                pages: [%{number: 1, media_box: [0, 0, 612, 792], rotation: 0, spans: []}]
              }}
  end

  test "extract_spans applies page rotation to normalized baseline coordinates" do
    pdf = page_pdf("BT /F1 12 Tf 1 0 0 1 10 20 Tm (A) Tj ET", rotate: 90)

    assert {:ok, %{pages: [%{rotation: 90, spans: [span]}]}} = Text.extract_spans(pdf)
    assert span.x == 20.0
    assert span.y == 10.0
    assert span.end_x == 20.0
    assert span.end_y == 16.0
  end

  test "extract_spans indexes multiple content streams and nested transformed Forms" do
    objects = [
      {1, "<< /Type /Catalog /Pages 2 0 R >>"},
      {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 612 792] >>"},
      {3, "<< /Type /Page /Parent 2 0 R /Resources 4 0 R /Contents [6 0 R 7 0 R] >>"},
      {4, "<< /Font << /F1 5 0 R >> /XObject << /outer 8 0 R >> >>"},
      {5, "<< /Type /Font /Subtype /Type1 /Encoding /WinAnsiEncoding >>"},
      {6, stream_object("", "BT /F1 10 Tf (A) Tj ET")},
      {7, stream_object("", "/outer Do BT /F1 10 Tf (D) Tj ET")},
      {8,
       stream_object(
         "/Type /XObject /Subtype /Form /Matrix [1 0 0 1 20 30] /Resources 10 0 R",
         "BT /F1 10 Tf (B) Tj ET /inner Do"
       )},
      {9,
       stream_object(
         "/Type /XObject /Subtype /Form /Matrix [2 0 0 2 3 4] /Resources << /Font << /F1 5 0 R >> >>",
         "BT /F1 10 Tf (C) Tj ET"
       )},
      {10, "<< /Font << /F1 5 0 R >> /XObject << /inner 9 0 R >> >>"}
    ]

    assert {:ok, %{pages: [%{spans: spans}]}} = Text.extract_spans(pdf(objects))
    assert Enum.map(spans, & &1.text) == ["A", "B", "C", "D"]
    assert Enum.map(spans, & &1.source_index) == [0, 1, 2, 3]
    assert Enum.at(spans, 1).ctm == [1.0, 0.0, 0.0, 1.0, 20.0, 30.0]
    assert Enum.at(spans, 2).ctm == [2.0, 0.0, 0.0, 2.0, 23.0, 34.0]
    assert {Enum.at(spans, 1).x, Enum.at(spans, 1).y} == {20.0, 762.0}
    assert {Enum.at(spans, 2).x, Enum.at(spans, 2).y} == {23.0, 758.0}
  end

  test "extract_spans validates options, input, encoding, and file diagnostics" do
    assert {:error, {:invalid_pdf_input, input_diagnostic}} = Text.extract_spans(:not_a_pdf)
    assert input_diagnostic.stage == :input
    assert input_diagnostic.operation == :extract_spans
    assert input_diagnostic.module == Text

    assert {:error, {:invalid_options, %{stage: :options, operation: :extract_spans}}} =
             Text.extract_spans("%PDF-1.7", [:not_a_keyword])

    assert {:error, {:invalid_options, %{stage: :options, operation: :extract_spans}}} =
             Text.extract_spans("%PDF-1.7", layout: true)

    assert {:error, {:invalid_options, %{stage: :options, operation: :extract_spans}}} =
             Text.extract_spans("%PDF-1.7", order: :invalid)

    custom =
      page_pdf("BT /F1 10 Tf <01> Tj ET",
        font: "<< /Type /Font /Subtype /Type0 /Encoding /Identity-H /DescendantFonts [8 0 R] >>"
      )

    assert {:error, {:unsupported_text_encoding, encoding_diagnostic}} =
             Text.extract_spans(custom)

    assert encoding_diagnostic.stage == :text_encoding
    assert encoding_diagnostic.operation == :extract_spans
    assert encoding_diagnostic.module == Text

    assert {:error,
            {:invalid_path, %{stage: :file, operation: :extract_file_spans, module: Text}}} =
             Text.extract_file_spans(:not_a_path)

    missing = Path.join(System.tmp_dir!(), "native-elixir-pdf-missing-spans.pdf")

    assert {:error, {:enoent, %{source: ^missing, operation: :read, module: Text}}} =
             Text.extract_file_spans(missing)
  end

  test "extract_file_spans returns spans and adds source context to extraction errors" do
    valid_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-spans.pdf")
    encrypted_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-encrypted-spans.pdf")
    on_exit(fn -> File.rm(valid_path) end)
    on_exit(fn -> File.rm(encrypted_path) end)

    File.write!(valid_path, page_pdf("BT /F1 12 Tf (File) Tj ET"))

    assert {:ok, %{pages: [%{spans: [%{text: "File"}]}]}} =
             Text.extract_file_spans(valid_path)

    File.write!(
      encrypted_path,
      pdf(
        [{1, "<< /Type /Catalog /Pages 2 0 R >>"}, {2, "<< /Type /Pages /Kids [] /Count 0 >>"}],
        "/Root 1 0 R /Encrypt 9 0 R"
      )
    )

    assert {:error,
            {:encrypted_pdf,
             %{source: ^encrypted_path, operation: :extract_file_spans, module: Text}}} =
             Text.extract_file_spans(encrypted_path)
  end

  test "handles empty text and reports text shown without an active font" do
    assert_error(
      Text.extract(page_pdf("BT /F1 12 Tf () Tj ET")),
      :no_extractable_text,
      :text_extraction
    )

    assert_error(
      Text.extract(page_pdf("BT (A) Tj ET")),
      :unsupported_text_encoding,
      :text_encoding
    )
  end

  test "decodes the supported stream filters and their abbreviations" do
    decoded = "BT /F1 12 Tf (Filters) Tj ET"

    cases = [
      {"Fl", :zlib.compress(decoded)},
      {"ASCIIHexDecode", Base.encode16(decoded) <> ">"},
      {"AHx", Base.encode16(decoded) <> ">"},
      {"ASCII85Decode", ascii85(decoded)},
      {"A85", ascii85(decoded)},
      {"RunLengthDecode", run_length_encode(decoded)},
      {"RL", run_length_encode(decoded)},
      {"LZWDecode", lzw_literal_encode(decoded)},
      {"LZW", lzw_literal_encode(decoded)}
    ]

    for {filter, data} <- cases do
      assert Text.extract(page_pdf(data, content_dictionary: "/Filter /#{filter}"), layout: false) ==
               {:ok, "Filters"}
    end
  end

  test "returns diagnostics for malformed filter data" do
    cases = [
      {"ASCIIHexDecode", "41", :invalid_pdf_input},
      {"ASCIIHexDecode", "GG>", :invalid_pdf_input},
      {"ASCII85Decode", "!!!!", :invalid_pdf_input},
      {"ASCII85Decode", "v~>", :invalid_pdf_input},
      {"ASCII85Decode", "!~>", :invalid_pdf_input},
      {"ASCII85Decode", "!!z!!~>", :invalid_pdf_input},
      {"RunLengthDecode", <<0, ?A>>, :invalid_pdf_input},
      {"RunLengthDecode", <<2, ?A, 128>>, :invalid_pdf_input},
      {"RunLengthDecode", <<255>>, :invalid_pdf_input},
      {"LZWDecode", <<0>>, :invalid_pdf_input}
    ]

    for {filter, data, reason} <- cases do
      assert_error(
        Text.extract(page_pdf(data, content_dictionary: "/Filter /#{filter}")),
        reason,
        :filter
      )
    end

    invalid_early_change =
      page_pdf(lzw_literal_encode("x"),
        content_dictionary: "/Filter /LZWDecode /DecodeParms << /EarlyChange 2 >>"
      )

    assert_error(Text.extract(invalid_early_change), :invalid_pdf_input, :filter)
  end

  test "decodes every PNG row filter and rejects invalid or truncated rows" do
    decoded = "BT /F1 12 Tf (PNG) Tj ET"

    for filter <- 0..4 do
      predicted = png_predict(decoded, filter)

      pdf =
        page_pdf(:zlib.compress(<<filter>> <> predicted),
          content_dictionary:
            "/Filter /FlateDecode /DecodeParms << /Predictor 15 /Columns #{byte_size(decoded)} >>"
        )

      assert Text.extract(pdf, layout: false) == {:ok, "PNG"}
    end

    invalid =
      page_pdf(:zlib.compress(<<5>> <> decoded),
        content_dictionary:
          "/Filter /FlateDecode /DecodeParms << /Predictor 15 /Columns #{byte_size(decoded)} >>"
      )

    assert_error(Text.extract(invalid), :invalid_pdf_input, :filter)

    truncated =
      page_pdf(:zlib.compress(<<0, 1>>),
        content_dictionary: "/Filter /FlateDecode /DecodeParms << /Predictor 15 /Columns 10 >>"
      )

    assert_error(Text.extract(truncated), :invalid_pdf_input, :filter)
  end

  test "enforces decompression ratio limits through diagnostics" do
    expanded = :binary.copy(" ", 30_000) <> "BT /F1 12 Tf (Too large) Tj ET"
    pdf = page_pdf(:zlib.compress(expanded), content_dictionary: "/Filter /FlateDecode")
    assert_error(Text.extract(pdf), :resource_limit_exceeded, :limits)
  end

  test "interprets supported text and graphics-state operators" do
    content =
      "q 1 0 0 1 2 3 cm Q BT /F1 12 Tf 1 0 0 1 10 40 Tm 1 Tc 2 Tw 90 Tz 14 TL 1 Ts 0 Tr (A) Tj 0 -1 Td (B) Tj 0 -1 TD (C) Tj T* (D) Tj (E) ' 1 2 (F) \" [(G) -100 (H)] TJ ET"

    assert {:ok, text} = Text.extract(page_pdf(content), layout: false)
    assert text =~ "A"
    assert text =~ "H"
  end

  test "rejects invalid operands for supported operators" do
    invalid_streams = [
      "1 q",
      "Q",
      "1 cm",
      "BT /F1 Tf ET",
      "BT 1 Tm ET",
      "BT 1 Td ET",
      "BT 1 TD ET",
      "BT /bad TL ET",
      "BT /bad Tc ET",
      "BT /bad Tw ET",
      "BT /bad Tz ET",
      "BT /bad Ts ET",
      "BT 8 Tr ET",
      "BT 1 2 ' ET",
      "BT 1 2 \" ET",
      "(outside) Tj",
      "[(outside)] TJ"
    ]

    for content <- invalid_streams do
      result = Text.extract(page_pdf(content))

      assert match?({:error, {:invalid_pdf_input, %{stage: :content}}}, result),
             "expected invalid content for #{inspect(content)}, got: #{inspect(result)}"
    end
  end

  test "rejects malformed content syntax and Contents values" do
    assert_error(Text.extract(page_pdf("[1 2")), :invalid_pdf_input, :content)
    assert_error(Text.extract(page_pdf("1")), :invalid_pdf_input, :content)
    assert_error(Text.extract(page_pdf("(unterminated")), :invalid_pdf_input, :content)

    base = [
      {1, "<< /Type /Catalog /Pages 2 0 R >>"},
      {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 612 792] >>"}
    ]

    assert_error(
      Text.extract(pdf(base ++ [{3, "<< /Type /Page /Parent 2 0 R /Contents [6 0 R 9] >>"}])),
      :invalid_pdf_input,
      :content
    )

    assert_error(
      Text.extract(pdf(base ++ [{3, "<< /Type /Page /Parent 2 0 R /Contents 9 >>"}])),
      :invalid_pdf_input,
      :content
    )
  end

  test "uses standard base-font encodings and rejects custom fonts without mappings" do
    helvetica = "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"

    assert Text.extract(page_pdf("BT /F1 12 Tf (A) Tj ET", font: helvetica), layout: false) ==
             {:ok, "A"}

    symbol = "<< /Type /Font /Subtype /Type1 /BaseFont /Symbol >>"

    assert Text.extract(page_pdf("BT /F1 12 Tf (A) Tj ET", font: symbol), layout: false) ==
             {:ok, "Α"}

    zapf = "<< /Type /Font /Subtype /Type1 /BaseFont /ZapfDingbats >>"

    assert Text.extract(page_pdf("BT /F1 12 Tf (!) Tj ET", font: zapf), layout: false) ==
             {:ok, "✁"}

    custom = "<< /Type /Font /Subtype /Type1 /BaseFont /CustomEmbedded >>"

    assert_error(
      Text.extract(page_pdf("BT /F1 12 Tf (A) Tj ET", font: custom)),
      :unsupported_text_encoding,
      :text_encoding
    )
  end

  test "validates simple-font width and encoding dictionaries" do
    invalid_widths =
      "<< /Type /Font /Subtype /Type1 /Encoding /WinAnsiEncoding /FirstChar /bad /Widths [500] >>"

    assert_error(
      Text.extract(page_pdf("BT /F1 12 Tf (A) Tj ET", font: invalid_widths)),
      :invalid_pdf_input,
      :font
    )

    invalid_descriptor =
      "<< /Type /Font /Subtype /Type1 /Encoding /WinAnsiEncoding /FontDescriptor 9 0 R >>"

    pdf_with_descriptor =
      page_pdf("BT /F1 12 Tf (A) Tj ET",
        font: invalid_descriptor,
        extra_objects: [{9, "<< /MissingWidth /bad >>"}]
      )

    assert_error(Text.extract(pdf_with_descriptor), :invalid_pdf_input, :font)

    malformed_encoding =
      "<< /Type /Font /Subtype /Type1 /Encoding << /BaseEncoding 42 >> >>"

    assert_error(
      Text.extract(page_pdf("BT /F1 12 Tf (A) Tj ET", font: malformed_encoding)),
      :invalid_pdf_input,
      :text_encoding
    )

    scalar_differences =
      "<< /Type /Font /Subtype /Type1 /Encoding << /BaseEncoding /WinAnsiEncoding /Differences 42 >> >>"

    assert_error(
      Text.extract(page_pdf("BT /F1 12 Tf (A) Tj ET", font: scalar_differences)),
      :invalid_pdf_input,
      :text_encoding
    )
  end

  test "uses CID listed and ranged widths and rejects malformed W arrays" do
    cmap =
      "begincmap\n1 begincodespacerange\n<00> <FF>\nendcodespacerange\n4 beginbfchar\n<01> <0041>\n<02> <0042>\n<03> <0043>\n<04> <0044>\nendbfchar\nendcmap"

    font =
      "<< /Type /Font /Subtype /Type0 /Encoding /Identity-H /DescendantFonts [8 0 R] /ToUnicode 7 0 R >>"

    valid =
      page_pdf("BT /F1 12 Tf <01020304> Tj ET",
        font: font,
        cmap: cmap,
        descendant: "<< /Type /Font /Subtype /CIDFontType2 /DW 500 /W [1 [400 500] 3 4 600] >>"
      )

    assert Text.extract(valid, layout: false) == {:ok, "ABCD"}

    invalid =
      page_pdf("BT /F1 12 Tf <01> Tj ET",
        font: font,
        cmap: cmap,
        descendant: "<< /Type /Font /Subtype /CIDFontType2 /DW 500 /W [1 [/bad]] >>"
      )

    assert_error(Text.extract(invalid), :invalid_pdf_input, :font)

    invalid_range =
      page_pdf("BT /F1 12 Tf <01> Tj ET",
        font: font,
        cmap: cmap,
        descendant: "<< /Type /Font /Subtype /CIDFontType2 /DW 500 /W [1 2] >>"
      )

    assert_error(Text.extract(invalid_range), :invalid_pdf_input, :font)
  end

  test "normalizes 180 and 270 degree page rotation" do
    content = "BT /F1 12 Tf 1 0 0 1 10 20 Tm (A) Tj ET"
    assert {:ok, _} = Text.extract(page_pdf(content, rotate: 180))
    assert {:ok, _} = Text.extract(page_pdf(content, rotate: 270))
    assert_error(Text.extract(page_pdf(content, rotate: 45)), :invalid_pdf_input, :page_tree)
  end

  test "covers public option, file-success, and strict content error paths" do
    assert_error(Text.extract(page_pdf(""), layout: :invalid), :invalid_options, :options)

    path = Path.join(System.tmp_dir!(), "native-elixir-text-success.pdf")
    File.write!(path, page_pdf("BT /F1 12 Tf (File) Tj ET"))
    assert Text.extract_file(path, layout: false) == {:ok, "File"}
    File.rm!(path)

    assert_error(Text.extract(page_pdf("]")), :invalid_pdf_input, :content)
    assert_error(Text.extract(page_pdf("BT /F1 12 Tf 1 Tj ET")), :invalid_pdf_input, :content)

    assert_error(
      Text.extract(page_pdf("BT /F1 12 Tf [/Name] TJ ET")),
      :invalid_pdf_input,
      :content
    )

    assert_error(
      Text.extract(page_pdf("BT /F1 12 Tf /bad 2 (X) \" ET")),
      :invalid_pdf_input,
      :content
    )

    assert Text.extract(page_pdf("0 0 m BT /F1 12 Tf (Known) Tj ET"), layout: false) ==
             {:ok, "Known"}
  end

  test "validates missing resources and ignores valid non-Form XObjects" do
    missing_font = page_pdf("BT /Missing 12 Tf (X) Tj ET")
    assert_error(Text.extract(missing_font), :invalid_pdf_input, :resources)

    objects = [
      {1, "<< /Type /Catalog /Pages 2 0 R >>"},
      {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 612 792] >>"},
      {3, "<< /Type /Page /Parent 2 0 R /Resources 4 0 R /Contents 6 0 R >>"},
      {4, "<< /Font << /F1 5 0 R >> /XObject << /image 7 0 R >> >>"},
      {5, "<< /Type /Font /Subtype /Type1 /Encoding /WinAnsiEncoding >>"},
      {6, stream_object("", "BT /F1 12 Tf (Visible) Tj ET /image Do")},
      {7, stream_object("/Type /XObject /Subtype /Image", "bytes")}
    ]

    assert Text.extract(pdf(objects), layout: false) == {:ok, "Visible"}
  end

  test "validates remaining font metric and encoding failure modes" do
    descriptor =
      "<< /Type /Font /Subtype /Type1 /Encoding /WinAnsiEncoding /FontDescriptor 9 0 R >>"

    assert Text.extract(
             page_pdf("BT /F1 12 Tf (A) Tj ET",
               font: descriptor,
               extra_objects: [{9, "<< /MissingWidth 700 >>"}]
             ),
             layout: false
           ) == {:ok, "A"}

    no_descendant =
      "<< /Type /Font /Subtype /Type0 /Encoding /Identity-H /ToUnicode 7 0 R >>"

    cmap =
      "begincmap\n1 begincodespacerange\n<00> <FF>\nendcodespacerange\n1 beginbfchar\n<01> <0041>\nendbfchar"

    assert_error(
      Text.extract(page_pdf("BT /F1 12 Tf <01> Tj ET", font: no_descendant, cmap: cmap)),
      :invalid_pdf_input,
      :font
    )

    scalar_widths =
      "<< /Type /Font /Subtype /Type0 /Encoding /Identity-H /DescendantFonts [8 0 R] /ToUnicode 7 0 R >>"

    assert_error(
      Text.extract(
        page_pdf("BT /F1 12 Tf <01> Tj ET",
          font: scalar_widths,
          cmap: cmap,
          descendant: "<< /Type /Font /Subtype /CIDFontType2 /W 42 >>"
        )
      ),
      :invalid_pdf_input,
      :font
    )

    unsupported_base =
      "<< /Type /Font /Subtype /Type1 /Encoding << /BaseEncoding /Unknown >> >>"

    assert_error(
      Text.extract(page_pdf("BT /F1 12 Tf (A) Tj ET", font: unsupported_base)),
      :unsupported_text_encoding,
      :text_encoding
    )

    implicit_base =
      "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica /Encoding << /Differences [65 /Aacute] >> >>"

    assert Text.extract(page_pdf("BT /F1 12 Tf (A) Tj ET", font: implicit_base), layout: false) ==
             {:ok, "Á"}

    assert_error(
      Text.extract(
        page_pdf("BT /F1 12 Tf <00> Tj ET",
          font: "<< /Type /Font /Subtype /Type1 /Encoding /StandardEncoding >>"
        )
      ),
      :unsupported_text_encoding,
      :text_encoding
    )

    no_base_font = "<< /Type /Font /Subtype /Type1 >>"

    assert_error(
      Text.extract(page_pdf("BT /F1 12 Tf (A) Tj ET", font: no_base_font)),
      :unsupported_text_encoding,
      :text_encoding
    )

    invalid_base_font =
      <<"<< /Type /Font /Subtype /Type1 /BaseFont /", 255, " >>">>

    assert_error(
      Text.extract(page_pdf("BT /F1 12 Tf (A) Tj ET", font: invalid_base_font)),
      :invalid_pdf_input,
      :text_encoding
    )

    base_without_differences =
      "<< /Type /Font /Subtype /Type1 /Encoding << /BaseEncoding /WinAnsiEncoding >> >>"

    assert Text.extract(
             page_pdf("BT /F1 12 Tf (A) Tj ET", font: base_without_differences),
             layout: false
           ) == {:ok, "A"}
  end

  test "validates malformed and oversized CMap sections" do
    font =
      "<< /Type /Font /Subtype /Type0 /Encoding /Identity-H /DescendantFonts [8 0 R] /ToUnicode 7 0 R >>"

    cmaps = [
      {"1 beginbfchar\n<01> <0041>\nendbfchar", :invalid_pdf_input},
      {"1 begincodespacerange\n<00> <FF>\nendcodespacerange\n1 beginbfchar\n<01> <0>\nendbfchar",
       :invalid_pdf_input},
      {"1 begincodespacerange\n<00> <FF>\nendcodespacerange\n2 beginbfchar\n<01> <0041>\nendbfchar",
       :invalid_pdf_input},
      {"1 begincodespacerange\n<00> <FF>\nendcodespacerange\n1 beginbfrange\n<01> <02> [<0041>]\nendbfrange",
       :invalid_pdf_input},
      {"1 begincodespacerange\n<00> <FF>\nendcodespacerange\n2 beginbfrange\n<01> <02> <0041>\nendbfrange",
       :invalid_pdf_input},
      {"1 begincodespacerange\n<00> <FF>\nendcodespacerange\n1 beginbfrange\n<01> <02> <FFFF>\nendbfrange",
       :invalid_pdf_input},
      {"1 begincodespacerange\n<00> <FF>\nendcodespacerange\n1 beginbfrange\n<01> <01> [<D800>]\nendbfrange",
       :invalid_pdf_input},
      {"1 begincodespacerange\n<00> <FF>\nendcodespacerange\n1 beginbfrange\n<01> <01> <0>\nendbfrange",
       :invalid_pdf_input},
      {"1 begincodespacerange\n<00> <FF>\nendcodespacerange", :unsupported_text_encoding}
    ]

    for {cmap, reason} <- cmaps do
      assert_error(
        Text.extract(page_pdf("BT /F1 12 Tf <01> Tj ET", font: font, cmap: cmap)),
        reason,
        :cmap
      )
    end

    oversized =
      "1 begincodespacerange\n<00> <FF>\nendcodespacerange\n1 beginbfchar\n<01> <0041>\nendbfchar" <>
        :binary.copy(" ", 1_000_001)

    assert_error(
      Text.extract(page_pdf("BT /F1 12 Tf <01> Tj ET", font: font, cmap: oversized)),
      :resource_limit_exceeded,
      :cmap
    )
  end

  test "reports missing and out-of-codespace ToUnicode mappings" do
    font =
      "<< /Type /Font /Subtype /Type0 /Encoding /Identity-H /DescendantFonts [8 0 R] /ToUnicode 7 0 R >>"

    missing =
      "1 begincodespacerange\n<00> <FF>\nendcodespacerange\n" <>
        "1 beginbfchar\n<01> <0041>\nendbfchar"

    assert_error(
      Text.extract(page_pdf("BT /F1 12 Tf <02> Tj ET", font: font, cmap: missing)),
      :unsupported_text_encoding,
      :text_encoding
    )

    outside =
      "1 begincodespacerange\n<01> <01>\nendcodespacerange\n" <>
        "1 beginbfchar\n<01> <0041>\nendbfchar"

    assert_error(
      Text.extract(page_pdf("BT /F1 12 Tf <02> Tj ET", font: font, cmap: outside)),
      :unsupported_text_encoding,
      :text_encoding
    )
  end

  test "decodes a sequential ToUnicode bfrange" do
    font =
      "<< /Type /Font /Subtype /Type0 /Encoding /Identity-H /DescendantFonts [8 0 R] /ToUnicode 7 0 R >>"

    cmap =
      "1 begincodespacerange\n<00> <FF>\nendcodespacerange\n" <>
        "1 beginbfrange\n<01> <02> <0041>\nendbfrange"

    assert Text.extract(page_pdf("BT /F1 12 Tf <0102> Tj ET", font: font, cmap: cmap),
             layout: false
           ) == {:ok, "AB"}
  end

  test "limits recursive forms and accepts real content operands and form matrices" do
    recursive_objects = [
      {1, "<< /Type /Catalog /Pages 2 0 R >>"},
      {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 /MediaBox [0 0 612 792] >>"},
      {3, "<< /Type /Page /Parent 2 0 R /Resources 4 0 R /Contents 6 0 R >>"},
      {4, "<< /Font << /F1 5 0 R >> /XObject << /form 7 0 R >> >>"},
      {5, "<< /Type /Font /Subtype /Type1 /Encoding /WinAnsiEncoding >>"},
      {6, stream_object("", "/form Do")},
      {7, stream_object("/Type /XObject /Subtype /Form /Resources 4 0 R", "/form Do")}
    ]

    assert_error(Text.extract(pdf(recursive_objects)), :resource_limit_exceeded, :limits)

    valid_form_objects =
      List.replace_at(recursive_objects, 6, {
        7,
        stream_object(
          "/Type /XObject /Subtype /Form /Matrix [1.0 0 0 1 2 3] /Resources 4 0 R",
          "BT /F1 12 Tf 1.5 0 Td (Form) Tj ET"
        )
      })

    assert Text.extract(pdf(valid_form_objects), layout: false) == {:ok, "Form"}

    scalar_matrix_objects =
      List.replace_at(recursive_objects, 6, {
        7,
        stream_object(
          "/Type /XObject /Subtype /Form /Matrix /bad /Resources 4 0 R",
          "BT /F1 12 Tf (Form) Tj ET"
        )
      })

    assert_error(Text.extract(pdf(scalar_matrix_objects)), :invalid_pdf_input, :content)
  end

  defp page_pdf(content, options \\ []) do
    font_name = Keyword.get(options, :font_name, "F1")

    font =
      Keyword.get(
        options,
        :font,
        "<< /Type /Font /Subtype /TrueType /Encoding /WinAnsiEncoding >>"
      )

    cmap = Keyword.get(options, :cmap)
    content_dictionary = Keyword.get(options, :content_dictionary, "")

    descendant =
      Keyword.get(options, :descendant, "<< /Type /Font /Subtype /CIDFontType2 /DW 500 >>")

    extra_objects = Keyword.get(options, :extra_objects, [])
    rotate = Keyword.get(options, :rotate)
    rotate_entry = if is_integer(rotate), do: " /Rotate #{rotate}", else: ""

    objects =
      [
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
        {3,
         "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792]#{rotate_entry} /Resources 4 0 R /Contents 6 0 R >>"},
        {4, "<< /Font << /#{font_name} 5 0 R >> >>"},
        {5, font},
        {6, stream_object(content_dictionary, content)},
        {8, descendant}
      ] ++ extra_objects

    objects = if cmap, do: objects ++ [{7, stream_object("", cmap)}], else: objects
    pdf(objects)
  end

  defp stream_object(dictionary, stream) do
    "<< #{dictionary} /Length #{byte_size(stream)} >>\nstream\n#{stream}\nendstream"
  end

  defp ascii85(data) do
    encoded =
      data
      |> :binary.bin_to_list()
      |> Enum.chunk_every(4)
      |> Enum.map_join(fn chunk ->
        size = length(chunk)
        padded = chunk ++ List.duplicate(0, 4 - size)
        value = :binary.decode_unsigned(:erlang.list_to_binary(padded))

        if value == 0 and size == 4 do
          "z"
        else
          {digits, _value} =
            Enum.reduce(1..5, {[], value}, fn _, {digits, value} ->
              {[rem(value, 85) + 33 | digits], div(value, 85)}
            end)

          digits |> Enum.take(size + 1) |> :erlang.list_to_binary()
        end
      end)

    "<~" <> encoded <> "~>"
  end

  defp run_length_encode(data) do
    chunks =
      data
      |> :binary.bin_to_list()
      |> Enum.chunk_every(128)
      |> Enum.map(fn chunk -> [length(chunk) - 1 | chunk] end)

    IO.iodata_to_binary([chunks, <<128>>])
  end

  defp lzw_literal_encode(data) do
    bits =
      [256 | :binary.bin_to_list(data)]
      |> Kernel.++([257])
      |> Enum.reduce(<<>>, fn code, bits -> <<bits::bitstring, code::9>> end)

    padding = Integer.mod(8 - Integer.mod(bit_size(bits), 8), 8)
    <<bits::bitstring, 0::size(padding)>>
  end

  defp png_predict(data, filter) do
    {encoded, _previous} =
      data
      |> :binary.bin_to_list()
      |> Enum.map_reduce(0, fn byte, left ->
        predictor = if filter in [1, 3, 4], do: left, else: 0
        predictor = if filter == 3, do: div(left, 2), else: predictor
        {Integer.mod(byte - predictor, 256), byte}
      end)

    :erlang.list_to_binary(encoded)
  end

  defp pdf(objects, trailer \\ "/Root 1 0 R") do
    header = "%PDF-1.7\n"

    {body, offsets, _position} =
      Enum.reduce(objects, {header, %{}, byte_size(header)}, fn {object, value},
                                                                {body, offsets, position} ->
        rendered = "#{object} 0 obj\n#{value}\nendobj\n"
        {body <> rendered, Map.put(offsets, object, position), position + byte_size(rendered)}
      end)

    max_object = max(1, Enum.max(Map.keys(offsets), fn -> 1 end))
    xref_position = byte_size(body)

    entries =
      for object <- 0..max_object do
        if object == 0 do
          "0000000000 65535 f \n"
        else
          offset = Map.get(offsets, object, 0)

          String.pad_leading(Integer.to_string(offset), 10, "0") <>
            " 00000 " <> if(offset == 0, do: "f \n", else: "n \n")
        end
      end

    body <>
      "xref\n0 #{max_object + 1}\n" <>
      Enum.join(entries) <>
      "trailer\n<< /Size #{max_object + 1} #{trailer} >>\nstartxref\n#{xref_position}\n%%EOF\n"
  end

  defp assert_error({:error, {reason, diagnostic}}, reason, stage) do
    assert diagnostic.reason == reason
    assert diagnostic.stage == stage
    assert diagnostic.operation == :extract
    assert diagnostic.module == Text
  end
end
