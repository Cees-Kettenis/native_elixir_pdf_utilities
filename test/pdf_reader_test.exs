defmodule NativeElixirPdfUtilities.Pdf.ReaderTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Merge
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Text

  @fixture_directory Path.expand("fixtures/pdf_reader", __DIR__)

  test "reads committed classic, xref-stream, object-stream, hybrid, and incremental fixtures" do
    for fixture <- [
          "classic-xref.pdf",
          "xref-stream.pdf",
          "object-stream.pdf",
          "hybrid-xref.pdf"
        ] do
      assert {:ok, document} = fixture |> fixture!() |> Reader.read()
      assert length(document.pages) == 1
      assert {:ok, stream} = Reader.decoded_stream(document, {:ref, {5, 0}})
      assert stream =~ "Reader milestone fixture"
    end

    assert {:ok, incremental} = "incremental-update.pdf" |> fixture!() |> Reader.read()
    assert {:ok, updated_stream} = Reader.decoded_stream(incremental, {:ref, {5, 0}})
    assert updated_stream =~ "Incremental fixture"

    assert {:compressed, 6, 0} = incremental_xref("object-stream.pdf", 1)
    assert {:compressed, 6, 0} = incremental_xref("hybrid-xref.pdf", 1)
  end

  test "rejects committed encrypted and malformed fixtures" do
    assert_error(Reader.read(fixture!("encrypted.pdf")), :encrypted_pdf, :encryption)
    assert_error(Reader.read(fixture!("malformed-xref.pdf")), :invalid_pdf_input, :xref)
  end

  test "resolves indirect chains and rejects reference cycles" do
    document = %{
      objects: %{
        {1, 0} => %{value: {:ref, {2, 0}}},
        {2, 0} => %{value: {:ref, {3, 0}}},
        {3, 0} => %{value: 42}
      }
    }

    assert Reader.resolve(document, {:ref, {1, 0}}) == {:ok, 42}

    cyclic = put_in(document.objects[{3, 0}].value, {:ref, {1, 0}})
    assert_error(Reader.resolve(cyclic, {:ref, {1, 0}}), :invalid_pdf_input, :resolution)
  end

  test "resolves indirect stream chains and rejects stream-reference cycles" do
    document = %{
      objects: %{
        {1, 0} => %{value: {:ref, {2, 0}}, stream: nil},
        {2, 0} => %{value: %{"Length" => 2}, stream: "ok"}
      }
    }

    assert Reader.decoded_stream(document, {:ref, {1, 0}}) == {:ok, "ok"}

    cyclic = put_in(document.objects[{2, 0}], %{value: {:ref, {1, 0}}, stream: nil})
    assert_error(Reader.decoded_stream(cyclic, {:ref, {1, 0}}), :invalid_pdf_input, :resolution)

    assert_error(Reader.dictionary(document, {:ref, {9, 0}}), :invalid_pdf_input, :resolution)

    bad_length = %{
      objects: %{
        {1, 0} => %{value: %{"Length" => {:ref, {2, 0}}}, stream: "ok"},
        {2, 0} => %{value: 3, stream: nil}
      }
    }

    assert_error(
      Reader.decoded_stream(bad_length, {:ref, {1, 0}}),
      :invalid_pdf_input,
      :stream
    )
  end

  test "public resolver functions return diagnostics for invalid values" do
    assert_error(Reader.read(:invalid), :invalid_pdf_input, :input)

    document = %{objects: %{{1, 0} => %{value: 42, stream: nil, offset: nil}}}
    assert Reader.resolve(document, 42) == {:ok, 42}

    assert {:error, {:invalid_pdf_input, diagnostic}} =
             Reader.resolve(document, {:ref, {2, 0}})

    assert diagnostic.message == "indirect object reference is missing; object 2 0"
    refute Map.has_key?(diagnostic, :object)

    assert_error(Reader.decoded_stream(document, {:ref, {1, 0}}), :invalid_pdf_input, :stream)
    assert_error(Reader.decoded_stream(document, {:ref, {2, 0}}), :invalid_pdf_input, :resolution)
    assert_error(Reader.decoded_stream(document, 1), :invalid_pdf_input, :stream)
    assert_error(Reader.dictionary(document, 42), :invalid_pdf_input, :resolution)
    assert_error(Reader.fetch(document, %{}, 42), :invalid_pdf_input, :resolution)
    assert Reader.fetch(document, %{"A" => 1}, "A") == {:ok, 1}
  end

  test "public resolver functions diagnose malformed document object records" do
    malformed_object = %{objects: %{{1, 0} => %{unexpected: true}}}

    for result <- [
          Reader.resolve(malformed_object, {:ref, {1, 0}}),
          Reader.dictionary(malformed_object, {:ref, {1, 0}}),
          Reader.fetch(malformed_object, {:ref, {1, 0}}, "Value")
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} = result
      assert diagnostic.stage == :resolution
      assert diagnostic.operation == :read
      assert diagnostic.module == Reader
      assert diagnostic.message == "indirect object record is malformed; object 1 0"
    end

    assert {:error, {:invalid_pdf_input, stream_diagnostic}} =
             Reader.decoded_stream(
               %{objects: %{{1, 0} => %{value: 42}}},
               {:ref, {1, 0}}
             )

    assert stream_diagnostic.stage == :stream
    assert stream_diagnostic.operation == :read
    assert stream_diagnostic.module == Reader
    assert stream_diagnostic.message == "stream object record is malformed; object 1 0"
  end

  test "rejects malformed headers, final pointers, lexical syntax, and trailers" do
    assert_error(Reader.read("not a pdf"), :invalid_pdf_input, :header)
    assert_error(Reader.read("%PDF-9.0\n"), :invalid_pdf_input, :header)
    assert_error(Reader.read("%PDF-1.7\n"), :invalid_pdf_input, :xref)

    malformed_syntax =
      pdf([{1, "<< /Type /Catalog /Pages 2 0 R >>"}, {2, "<< /Type /Pages /Kids [] /Count 0 >>"}])
      |> String.replace("2 0 obj", "2 0 obj\n(unterminated")
      |> repoint_xref()

    assert_error(Reader.read(malformed_syntax), :invalid_pdf_input, :object)

    assert_error(
      Reader.read(pdf([{1, "42"}], "/Info 1 0 R")),
      :invalid_pdf_input,
      :trailer
    )

    malformed_trailer =
      pdf([{1, "42"}], "/Root 1 0 R")
      |> String.replace("trailer\n<<", "trailer\n42\n<<")

    assert_error(Reader.read(repoint_xref(malformed_trailer)), :invalid_pdf_input, :trailer)

    malformed_values = [
      {3, "[ /Name invalid-operator ]"},
      {3, "<< /Key >>"},
      {3, "<< 42 >>"}
    ]

    for malformed_value <- malformed_values do
      malformed =
        pdf([
          {1, "<< /Type /Catalog /Pages 2 0 R >>"},
          {2, "<< /Type /Pages /Kids [] /Count 0 >>"},
          malformed_value
        ])

      assert_error(Reader.read(malformed), :invalid_pdf_input, :object)
    end
  end

  test "parses all direct PDF value forms" do
    objects = [
      {1, "<< /Type /Catalog /Pages 2 0 R >>"},
      {2, "<< /Type /Pages /Kids [] /Count 0 >>"},
      {3, "[1 1.5 /Name (text) <4142> true false null << /Nested 4 >>]"}
    ]

    assert {:ok, document} = Reader.read(pdf(objects))

    assert Reader.resolve(document, {:ref, {3, 0}}) ==
             {:ok,
              [
                1,
                1.5,
                {:name, "Name"},
                {:string, "text"},
                {:hex, "AB"},
                true,
                false,
                nil,
                %{"Nested" => 4}
              ]}
  end

  test "validates page-tree structure" do
    invalid_catalog = pdf([{1, "<< /Type /NotCatalog /Pages 2 0 R >>"}])
    assert_error(Reader.read(invalid_catalog), :invalid_pdf_input, :page_tree)

    non_name_catalog = pdf([{1, "<< /Type 42 /Pages 2 0 R >>"}])
    assert_error(Reader.read(non_name_catalog), :invalid_pdf_input, :page_tree)

    missing_pages = pdf([{1, "<< /Type /Catalog >>"}])
    assert_error(Reader.read(missing_pages), :invalid_pdf_input, :page_tree)

    invalid_kids =
      pdf([{1, "<< /Type /Catalog /Pages 2 0 R >>"}, {2, "<< /Type /Pages /Kids 3 /Count 0 >>"}])

    assert_error(Reader.read(invalid_kids), :invalid_pdf_input, :page_tree)

    invalid_type =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
        {3, "<< /Type /Unknown /Parent 2 0 R >>"}
      ])

    assert_error(Reader.read(invalid_type), :invalid_pdf_input, :page_tree)

    cycle =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
        {3, "<< /Type /Pages /Parent 2 0 R /Kids [2 0 R] /Count 1 >>"}
      ])

    assert_error(Reader.read(cycle), :invalid_pdf_input, :page_tree)
  end

  test "validates exact page-tree Parent relationships" do
    invalid_parents = [
      {"", "page tree node is missing its required Parent; object 3 0"},
      {"/Parent /invalid", "page tree node has a malformed Parent; object 3 0"},
      {"/Parent 4 0 R",
       "page tree node Parent does not match its containing Pages node; object 3 0"}
    ]

    for {parent, message} <- invalid_parents do
      malformed =
        pdf([
          {1, "<< /Type /Catalog /Pages 2 0 R >>"},
          {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
          {3, "<< /Type /Page #{parent} >>"}
        ])

      assert {:error, {:invalid_pdf_input, diagnostic}} = Reader.read(malformed)
      assert diagnostic.stage == :page_tree
      assert diagnostic.message == message
    end

    missing_parent =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
        {3, "<< /Type /Page >>"}
      ])

    assert {:error, {:invalid_pdf_input, merge_diagnostic}} = Merge.merge([missing_parent])
    assert merge_diagnostic.stage == :page_tree
    assert merge_diagnostic.operation == :merge
    assert merge_diagnostic.module == Merge
    assert merge_diagnostic.message == "page tree node is missing its required Parent; object 3 0"

    root_parent =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Parent 1 0 R /Kids [] /Count 0 >>"}
      ])

    assert {:error, {:invalid_pdf_input, root_diagnostic}} = Reader.read(root_parent)
    assert root_diagnostic.stage == :page_tree
    assert root_diagnostic.message == "page tree root must not declare Parent; object 2 0"

    cross_branch =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R 4 0 R] /Count 2 >>"},
        {3, "<< /Type /Pages /Parent 2 0 R /Kids [5 0 R] /Count 1 >>"},
        {4, "<< /Type /Pages /Parent 2 0 R /Kids [6 0 R] /Count 1 >>"},
        {5, "<< /Type /Page /Parent 4 0 R >>"},
        {6, "<< /Type /Page /Parent 4 0 R >>"}
      ])

    assert {:error, {:invalid_pdf_input, cross_branch_diagnostic}} =
             Reader.read(cross_branch)

    assert cross_branch_diagnostic.message ==
             "page tree node Parent does not match its containing Pages node; object 5 0"
  end

  test "rejects duplicate page-tree references and invalid descendant counts" do
    duplicate_sibling =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R 3 0 R] /Count 2 >>"},
        {3, "<< /Type /Page /Parent 2 0 R >>"}
      ])

    assert {:error, {:invalid_pdf_input, duplicate_diagnostic}} =
             Reader.read(duplicate_sibling)

    assert duplicate_diagnostic.stage == :page_tree
    assert duplicate_diagnostic.message == "page tree contains a duplicate reference; object 3 0"

    duplicate_branch =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R 4 0 R] /Count 2 >>"},
        {3, "<< /Type /Pages /Parent 2 0 R /Kids [5 0 R] /Count 1 >>"},
        {4, "<< /Type /Pages /Parent 2 0 R /Kids [5 0 R] /Count 1 >>"},
        {5, "<< /Type /Page /Parent 3 0 R >>"}
      ])

    assert_error(Reader.read(duplicate_branch), :invalid_pdf_input, :page_tree)

    invalid_counts = [
      {"", "Pages node is missing a valid Count; object 2 0"},
      {"/Count /bad", "Pages node is missing a valid Count; object 2 0"},
      {"/Count -1", "Pages node is missing a valid Count; object 2 0"},
      {"/Count 2", "Pages node Count 2 does not match 1 descendant pages; object 2 0"}
    ]

    for {count, message} <- invalid_counts do
      malformed =
        pdf([
          {1, "<< /Type /Catalog /Pages 2 0 R >>"},
          {2, "<< /Type /Pages /Kids [3 0 R] #{count} >>"},
          {3, "<< /Type /Page /Parent 2 0 R >>"}
        ])

      assert {:error, {:invalid_pdf_input, diagnostic}} = Reader.read(malformed)
      assert diagnostic.stage == :page_tree
      assert diagnostic.message == message
    end

    nested_mismatch =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
        {3, "<< /Type /Pages /Parent 2 0 R /Kids [4 0 R] /Count 2 >>"},
        {4, "<< /Type /Page /Parent 3 0 R >>"}
      ])

    assert {:error, {:invalid_pdf_input, nested_diagnostic}} = Reader.read(nested_mismatch)
    assert nested_diagnostic.message =~ "object 3 0"

    indirect =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids 4 0 R /Count 5 0 R >>"},
        {3, "<< /Type /Page /Parent 2 0 R >>"},
        {4, "[3 0 R]"},
        {5, "1"}
      ])

    assert {:ok, %{pages: [_page]}} = Reader.read(indirect)
  end

  test "shared consumers reject page trees rejected by the reader" do
    malformed =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R 3 0 R] /Count 2 /MediaBox [0 0 200 100] >>"},
        {3, "<< /Type /Page /Parent 2 0 R >>"}
      ])

    assert_error(Text.extract(malformed), :invalid_pdf_input, :page_tree)

    assert {:error, {:invalid_pdf_input, diagnostic}} = Merge.merge([malformed])
    assert diagnostic.stage == :page_tree
    assert diagnostic.operation == :merge
    assert diagnostic.module == NativeElixirPdfUtilities.Merge
    assert diagnostic.message =~ "page tree contains a duplicate reference"
  end

  test "resolves indirect stream lengths, filters, and DecodeParms" do
    decoded = "decoded"
    compressed = :zlib.compress(decoded)

    objects = [
      {1, "<< /Type /Catalog /Pages 2 0 R >>"},
      {2, "<< /Type /Pages /Kids [] /Count 0 >>"},
      {3,
       stream_object(
         "/Length 4 0 R /Filter 5 0 R /DecodeParms 6 0 R",
         compressed,
         false
       )},
      {4, Integer.to_string(byte_size(compressed))},
      {5, "[/FlateDecode]"},
      {6, "[null]"}
    ]

    assert {:ok, document} = Reader.read(pdf(objects))
    assert Reader.decoded_stream(document, {:ref, {3, 0}}) == {:ok, decoded}
  end

  test "uses an indirect Length to disambiguate endstream bytes inside stream data" do
    stream = "before\nendstream\nendobj\nafter"

    input =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [] /Count 0 >>"},
        {3, stream_object("/Length 4 0 R", stream, false)},
        {4, Integer.to_string(byte_size(stream))}
      ])

    assert {:ok, document} = Reader.read(input)
    assert document.objects[{3, 0}].stream == stream
    assert Reader.decoded_stream(document, {:ref, {3, 0}}) == {:ok, stream}

    for {length, message} <- [
          {"2", "boundary does not match"},
          {"/invalid", "does not resolve to a non-negative integer"}
        ] do
      malformed =
        pdf([
          {1, "<< /Type /Catalog /Pages 2 0 R >>"},
          {2, "<< /Type /Pages /Kids [] /Count 0 >>"},
          {3, stream_object("/Length 4 0 R", "abc", false)},
          {4, length}
        ])

      assert {:error, {:invalid_pdf_input, diagnostic}} = Reader.read(malformed)
      assert diagnostic.stage == :object
      assert diagnostic.message =~ message
      assert diagnostic.message =~ "object 3 0"
    end

    malformed_prefix =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [] /Count 0 >>"},
        {3, "<< /Length 4 0 R >> false\nstream\nabc\nendstream"},
        {4, "3"}
      ])

    assert {:error, {:invalid_pdf_input, diagnostic}} = Reader.read(malformed_prefix)
    assert diagnostic.stage == :object
    assert diagnostic.message =~ "indirect object boundary is malformed"
  end

  test "uses the stream dictionary Length instead of nested Length keys" do
    direct =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [] /Count 0 >>"},
        {3, stream_object("/Metadata << /Length 1 >> /Length 3", "abc", false)}
      ])

    indirect =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [] /Count 0 >>"},
        {3, stream_object("/Metadata << /Length 1 >> /Length 4 0 R", "abc", false)},
        {4, "3"}
      ])

    for input <- [direct, indirect] do
      assert {:ok, document} = Reader.read(input)
      assert Reader.decoded_stream(document, {:ref, {3, 0}}) == {:ok, "abc"}
    end
  end

  test "rejects malformed stream metadata and filter declarations" do
    cases = [
      {"/Length /bad", :invalid_pdf_input},
      {"/Filter 42", :invalid_pdf_input},
      {"/Filter [/FlateDecode 42]", :invalid_pdf_input},
      {"/Filter [/FlateDecode] /DecodeParms []", :invalid_pdf_input},
      {"/Filter [/FlateDecode] /DecodeParms [42]", :invalid_pdf_input},
      {"/Filter /Unsupported", :unsupported_pdf_feature}
    ]

    for {dictionary, reason} <- cases do
      objects = [
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [] /Count 0 >>"},
        {3, stream_object(dictionary, "data", not String.starts_with?(dictionary, "/Length"))}
      ]

      assert {:ok, document} = Reader.read(pdf(objects))
      assert_error(Reader.decoded_stream(document, {:ref, {3, 0}}), reason, :filter_or_stream)
    end
  end

  test "decodes filter edge cases and reports malformed encoded data" do
    assert decoded_stream("z~>", {:name, "ASCII85Decode"}) == {:ok, <<0, 0, 0, 0>>}

    assert decoded_stream("!!!!!z~>", {:name, "ASCII85Decode"}) ==
             {:ok, :binary.copy(<<0>>, 8)}

    assert decoded_stream("z!!!!!z~>", {:name, "A85"}) ==
             {:ok, :binary.copy(<<0>>, 12)}

    assert decoded_stream("<~z~>", {:name, "ASCII85Decode"}) == {:ok, <<0, 0, 0, 0>>}

    assert decoded_stream(<<0, "!!!!!\n z~>">>, {:name, "ASCII85Decode"}) ==
             {:ok, :binary.copy(<<0>>, 8)}

    assert decoded_stream("s8W-!~>", {:name, "ASCII85Decode"}) ==
             {:ok, <<255, 255, 255, 255>>}

    for {encoded, decoded_size} <- [{"!!~>", 1}, {"!!!~>", 2}, {"!!!!~>", 3}] do
      assert decoded_stream(encoded, {:name, "ASCII85Decode"}) ==
               {:ok, :binary.copy(<<0>>, decoded_size)}
    end

    assert_error(decoded_stream("!z~>", {:name, "ASCII85Decode"}), :invalid_pdf_input, :filter)
    assert_error(decoded_stream("!~>", {:name, "ASCII85Decode"}), :invalid_pdf_input, :filter)
    assert_error(decoded_stream("v~>", {:name, "ASCII85Decode"}), :invalid_pdf_input, :filter)

    for overflow <- ["s8W-\"~>", "uuuuu~>", "uu~>"] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               decoded_stream(overflow, {:name, "ASCII85Decode"})

      assert diagnostic.stage == :filter
      assert diagnostic.message =~ "group exceeds the 32-bit range"
    end

    assert decoded_stream(<<255, ?A, 128>>, {:name, "RunLengthDecode"}) ==
             {:ok, String.duplicate("A", 2)}

    assert_error(decoded_stream("invalid", {:name, "FlateDecode"}), :invalid_pdf_input, :filter)

    assert decoded_stream("61ff>", {:name, "ASCIIHexDecode"}) == {:ok, <<0x61, 0xFF>>}

    assert_error(
      decoded_stream(<<256::9, 300::9, 257::9, 0::5>>, {:name, "LZWDecode"}),
      :invalid_pdf_input,
      :filter
    )
  end

  test "validates predictor selection, dimensions, and truncated rows" do
    assert decoded_stream("726177>", {:name, "ASCIIHexDecode"}, %{"Predictor" => 1}) ==
             {:ok, "raw"}

    assert decoded_stream(">", {:name, "ASCIIHexDecode"}, %{"Predictor" => 15}) ==
             {:ok, <<>>}

    assert_error(
      decoded_stream("00>", {:name, "ASCIIHexDecode"}, %{"Predictor" => 9}),
      :unsupported_pdf_feature,
      :filter
    )

    assert_error(
      decoded_stream("00>", {:name, "ASCIIHexDecode"}, %{
        "Predictor" => 2,
        "Columns" => 2
      }),
      :invalid_pdf_input,
      :filter
    )

    assert_error(
      decoded_stream("000000>", {:name, "ASCIIHexDecode"}, %{
        "Predictor" => 15,
        "Columns" => 1
      }),
      :invalid_pdf_input,
      :filter
    )

    assert_error(
      decoded_stream("00>", {:name, "ASCIIHexDecode"}, %{
        "Predictor" => 2,
        "Colors" => 0
      }),
      :invalid_pdf_input,
      :filter
    )

    for {predictor, dimension} <- [
          {15, "Columns"},
          {15, "Colors"},
          {2, "Columns"},
          {2, "Colors"}
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               decoded_stream("00>", {:name, "ASCIIHexDecode"}, %{
                 "Predictor" => predictor,
                 dimension => 25_000_001
               })

      assert diagnostic.stage == :limits

      assert diagnostic.message ==
               "predictor row exceeds the decoded-stream safety limit; object 1 0"
    end
  end

  test "enforces final decoded-stream size and ratio limits" do
    oversized = :binary.copy(<<129, ?A>>, div(25_000_000, 128) + 1) <> <<128>>

    assert_error(
      decoded_stream(oversized, {:name, "RunLengthDecode"}),
      :resource_limit_exceeded,
      :limits
    )

    literal = :binary.list_to_bin(Enum.to_list(0..127))

    oversized_literal =
      :binary.copy(<<127, literal::binary>>, div(25_000_000, 128) + 1) <> <<128>>

    assert_error(
      decoded_stream(oversized_literal, {:name, "RunLengthDecode"}),
      :resource_limit_exceeded,
      :limits
    )

    assert_error(
      decoded_stream(:binary.copy("A", 25_000_001), nil),
      :resource_limit_exceeded,
      :limits
    )

    expanded = :binary.copy("A", 100_000)
    nested = expanded |> run_length_encode() |> :zlib.compress()

    assert_error(
      decoded_stream(nested, [{:name, "FlateDecode"}, {:name, "RunLengthDecode"}], [nil, nil]),
      :resource_limit_exceeded,
      :limits
    )
  end

  test "inflates compressed input in bounded chunks" do
    :rand.seed(:exsss, {11, 22, 33})
    data = :rand.bytes(40_000)
    compressed = :zlib.compress(data)
    assert byte_size(compressed) > 16_384
    assert decoded_stream(compressed, {:name, "FlateDecode"}) == {:ok, data}
  end

  test "decodes LZW dictionary references and grows the code width" do
    codes = [65 | Enum.to_list(258..510)]

    bits =
      Enum.reduce([256 | codes], <<>>, fn code, bits ->
        <<bits::bitstring, code::unsigned-big-size(9)>>
      end)

    encoded =
      pad_bits(<<bits::bitstring, 257::unsigned-big-size(10)>>) <> :binary.copy(<<0>>, 100)

    expected = for length <- 1..254, into: <<>>, do: :binary.copy("A", length)

    assert decoded_stream(encoded, {:name, "LZWDecode"}) == {:ok, expected}

    bomb =
      [256, 65 | Enum.to_list(258..510)]
      |> Enum.reduce(<<>>, fn code, bits -> <<bits::bitstring, code::unsigned-big-size(9)>> end)
      |> then(&pad_bits(<<&1::bitstring, 257::unsigned-big-size(10)>>))

    assert_error(
      decoded_stream(bomb, {:name, "LZWDecode"}),
      :resource_limit_exceeded,
      :limits
    )
  end

  defp stream_object(dictionary, stream, include_length?) do
    length = if include_length?, do: " /Length #{byte_size(stream)}", else: ""
    "<< #{dictionary}#{length} >>\nstream\n" <> stream <> "\nendstream"
  end

  defp decoded_stream(data, filter, parameters \\ nil) do
    document = %{
      objects: %{
        {1, 0} => %{
          value: %{"Length" => byte_size(data), "Filter" => filter, "DecodeParms" => parameters},
          stream: data
        }
      }
    }

    Reader.decoded_stream(document, {:ref, {1, 0}})
  end

  defp run_length_encode(data) do
    data
    |> :binary.bin_to_list()
    |> Enum.chunk_every(128)
    |> Enum.map(fn chunk -> [257 - length(chunk), hd(chunk)] end)
    |> then(&IO.iodata_to_binary([&1, <<128>>]))
  end

  defp pad_bits(bits) do
    padding = Integer.mod(8 - Integer.mod(bit_size(bits), 8), 8)
    <<bits::bitstring, 0::size(padding)>>
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

  defp repoint_xref(pdf) do
    [before_xref, _after_xref] = String.split(pdf, "xref\n", parts: 2)
    String.replace(pdf, ~r/startxref\n\d+/, "startxref\n#{byte_size(before_xref)}")
  end

  defp fixture!(name), do: File.read!(Path.join(@fixture_directory, name))

  defp incremental_xref(name, object) do
    assert {:ok, document} = name |> fixture!() |> Reader.read()
    Map.fetch!(document.xref, object)
  end

  defp assert_error({:error, {reason, diagnostic}}, reason, expected_stage) do
    assert diagnostic.reason == reason

    if expected_stage == :filter_or_stream do
      assert diagnostic.stage in [:filter, :stream]
    else
      assert diagnostic.stage == expected_stage
    end
  end
end
