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
    compressed =
      object_stream([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
        {3,
         "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources 4 0 R /Contents 6 0 R >>"},
        {4, "<< /Font << /f-0-0 5 0 R >> >>"},
        {5, "<< /Type /Font /Subtype /TrueType /Encoding /WinAnsiEncoding >>"}
      ])

    assert Text.extract(
             pdf([
               {6, stream_object("", "BT /f-0-0 10 Tf (Object stream) Tj ET")},
               {7, compressed}
             ]),
             layout: false
           ) ==
             {:ok, "Object stream"}
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
      {"1 begincodespacerange\n<00> <FF>\nendcodespacerange\n1 beginbfrange\n<01> <02> <FFFF>\nendbfrange",
       :invalid_pdf_input}
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

  defp object_stream(entries) do
    {body, offsets, _offset} =
      Enum.reduce(entries, {"", [], 0}, fn {object, value}, {body, offsets, offset} ->
        value = value <> "\n"
        {body <> value, offsets ++ [{object, offset}], offset + byte_size(value)}
      end)

    header =
      offsets
      |> Enum.map_join(" ", fn {object, offset} -> "#{object} #{offset}" end)
      |> then(&(&1 <> " "))

    stream = header <> body

    "<< /Type /ObjStm /N #{length(entries)} /First #{byte_size(header)} /Length #{byte_size(stream)} >>\nstream\n#{stream}\nendstream"
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
