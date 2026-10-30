alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.Pdf.ReaderResourceLimitsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Tokenizer
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  setup do
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    :ok
  end

  test "token accounting shares byte and token budgets with individual charges" do
    Limits.install(%{Limits.effective() | max_pdf_reader_work: 5, max_pdf_reader_tokens: 2})

    assert :ok = PdfValidator.charge_reader_token(100)

    assert :ok =
             PdfValidator.with_reader_budget(fn ->
               :ok = PdfValidator.charge_reader_token(2)
               :ok = PdfValidator.charge_reader_budget(:max_pdf_reader_work, 1)
               PdfValidator.charge_reader_token(2)
             end)

    for {bytes, key} <- [{3, :max_pdf_reader_work}, {0, :max_pdf_reader_tokens}] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               PdfValidator.with_reader_budget(fn ->
                 :ok = PdfValidator.charge_reader_token(2)
                 :ok = PdfValidator.charge_reader_token(1)
                 PdfValidator.charge_reader_token(bytes)
               end)

      assert diagnostic.stage == :limits
      assert diagnostic.message =~ Atom.to_string(key)
    end

    Limits.install(%{Limits.effective() | max_pdf_reader_tokens: 1})

    assert [{:int, 1}] =
             PdfValidator.with_reader_budget(fn ->
               Tokenizer.tokenize_all(Tokenizer.new("1    "))
             end)
  end

  test "resource object bodies retain streams, references and complete syntax" do
    bytes = <<0, 255, "endstream and endobj">>

    body =
      "<< /Length #{byte_size(bytes)} /Values [true false null 1.25 /Name (Text) <00FF> 2 0 R] >>\nstream\n" <>
        bytes <> "\nendstream"

    assert {:ok, [{1, 0, {:stream, dictionary, ^bytes}}, {2, 0, {:value, %{}}}]} =
             Reader.parse_object_bodies([{1, body}, {2, "<< >>"}])

    refute Map.has_key?(dictionary, "Length")

    assert dictionary["Values"] == [
             true,
             false,
             nil,
             1.25,
             {:name, "Name"},
             {:string, "Text"},
             {:hex, <<0, 255>>},
             {:ref, {2, 0}}
           ]

    assert {:ok, []} = Reader.parse_object_bodies([])

    for body <- [
          "",
          "1",
          "<< /A >>",
          "<< >> << >>",
          "<< >> @",
          "<< /A -1 0 R >>",
          "<< /A [1 65536 R] >>",
          "<< /Length 2 >>\nstream\nabc\nendstream",
          "<< >>\nstream\nabc\nendstream"
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} = Reader.parse_object_bodies([{1, body}])
      assert diagnostic.stage == :object and diagnostic.reason == :invalid_pdf_input
      assert diagnostic.operation == :read and diagnostic.module == Reader
      assert diagnostic.source == "object 1 0" and diagnostic.message != ""
    end

    for objects <- [nil, %{}, [{0, "<< >>"}], [{1, :invalid}], [{1, "<< >>"}, {1, "<< >>"}]] do
      assert {:error, {:invalid_pdf_input, %{stage: :object, operation: :read, module: Reader}}} =
               Reader.parse_object_bodies(objects)
    end

    for {value, stream} <- [
          {1, nil},
          {%{}, :invalid},
          {%{"Length" => 2}, "abc"},
          {%{"A" => {:ref, :bad}}, nil},
          {%{wrong_key: 1}, nil}
        ] do
      assert {:error, {:invalid_pdf_input, %{stage: :object}}} =
               PdfValidator.validate_object_body(value, stream, [])
    end
  end

  test "resource object parsing shares byte, object and nested reader budgets" do
    limits = Limits.effective()
    body = "<< /A 1 >>"
    objects = [{1, body}, {2, body}]
    Limits.install(%{limits | max_pdf_input_bytes: 2 * byte_size(body), max_pdf_objects: 2})
    assert {:ok, _} = Reader.parse_object_bodies(objects)
    assert_limit(Reader.parse_object_bodies(objects ++ [{3, body}]))
    assert_limit(Reader.parse_object_bodies([{3, body}]))
    Limits.install(%{limits | max_pdf_input_bytes: 2 * byte_size(body) - 1})
    assert_limit(Reader.parse_object_bodies(objects))

    for key <- [:max_pdf_reader_tokens, :max_pdf_reader_values, :max_pdf_reader_work] do
      Limits.install(Map.put(limits, key, 1))
      assert_limit(Reader.parse_object_bodies(objects))
    end

    Limits.install(%{limits | max_pdf_reader_values: 4})
    assert {:ok, _} = Reader.parse_object_bodies(objects)

    assert_limit(
      PdfValidator.with_reader_budget(fn ->
        PdfValidator.charge_reader_budget(:max_pdf_reader_values, 1)
        Reader.parse_object_bodies(objects)
      end)
    )

    assert {:ok, _} = Reader.parse_object_bodies(objects)
  end

  test "aggregate object stream expansion includes xref bytes at and above the boundary" do
    {pdf, decoded_bytes} = object_stream_pdf(8)
    limits = Limits.effective()

    Limits.install(%{
      limits
      | max_pdf_decoded_stream_bytes: 5_000,
        max_pdf_reader_decoded_bytes: decoded_bytes
    })

    assert {:ok, document} = Reader.read(pdf)
    assert {:string, value} = document.objects[{10, 0}].value
    assert byte_size(value) == 4_096

    Limits.install(%{
      limits
      | max_pdf_decoded_stream_bytes: 5_000,
        max_pdf_reader_decoded_bytes: decoded_bytes - 1
    })

    assert_limit(Reader.read(pdf))
    # The outer scope is discarded after failure, so independent reads can retry.
    Limits.install(limits)
    assert {:ok, _} = Reader.read(pdf)
  end

  test "all operation budgets accept their boundary and nested scopes cannot reset them" do
    for key <- [
          :max_pdf_reader_decoded_bytes,
          :max_pdf_reader_tokens,
          :max_pdf_reader_values,
          :max_pdf_reader_work
        ] do
      limit = Limits.get(key)

      assert :ok =
               PdfValidator.with_reader_budget(fn ->
                 PdfValidator.charge_reader_budget(key, limit)
               end)

      assert_limit(
        PdfValidator.with_reader_budget(fn ->
          PdfValidator.charge_reader_budget(key, limit)
          PdfValidator.with_reader_budget(fn -> PdfValidator.charge_reader_budget(key, 1) end)
        end)
      )
    end

    assert_raise RuntimeError, fn ->
      PdfValidator.with_reader_budget(fn -> raise "failed operation" end)
    end

    assert PdfValidator.remaining_reader_decoded_bytes() ==
             Limits.get(:max_pdf_reader_decoded_bytes)
  end

  test "reader token, value and work failures retain diagnostics" do
    pdf = File.read!(Paths.fixture("pdf_reader/classic-xref.pdf"))
    limits = Limits.effective()

    for key <- [:max_pdf_reader_tokens, :max_pdf_reader_values, :max_pdf_reader_work] do
      Limits.install(Map.put(limits, key, 1))
      assert_limit(Reader.read(pdf))
    end
  end

  test "ordinary objects and direct stream prefixes consume one token pass" do
    limits = Limits.effective()
    array = "[" <> String.duplicate("7 ", 1_000) <> "]"
    stream = "literal endobj and endstream bytes"

    for value <- [
          array,
          "<< /Values #{array} /Length #{byte_size(stream)} >>\nstream\n#{stream}\nendstream"
        ] do
      input = classic_pdf(value)
      Limits.install(%{limits | max_pdf_reader_tokens: 1_100})
      assert {:ok, document} = Reader.read(input)

      case document.objects[{3, 0}] do
        %{value: values, stream: nil} ->
          assert values == List.duplicate(7, 1_000)

        %{value: %{"Values" => values}, stream: ^stream} ->
          assert values == List.duplicate(7, 1_000)
      end

      Limits.install(%{limits | max_pdf_reader_tokens: 1_000})
      assert_limit(Reader.read(input))
    end
  end

  test "container entries count repeated dictionary keys as work" do
    limits = Limits.effective()
    Limits.install(%{limits | max_pdf_container_entries: 4})
    assert {:ok, _} = Reader.read(classic_pdf("[1 2 3 4]"))
    assert_limit(Reader.read(classic_pdf("[1 2 3 4 5]")))
    assert {:ok, _} = Reader.read(classic_pdf("<< /A 1 /A 2 /A 3 /A 4 >>"))
    assert_limit(Reader.read(classic_pdf("<< /A 1 /A 2 /A 3 /A 4 /A 5 >>")))
  end

  test "oversized decimal and binary integers fail before large integer construction" do
    limits = Limits.effective()
    Limits.install(%{limits | max_pdf_numeric_token_bytes: 10})
    assert [{:int, 1_111_111_111}] = Tokenizer.new("1111111111") |> Tokenizer.tokenize_all()
    assert_limit(Tokenizer.new("11111111111") |> Tokenizer.tokenize_all())
    assert_limit(Reader.read(classic_pdf("11111111111")))
    pdf = classic_pdf("1") |> String.replace(~r/startxref\n\d+/, "startxref\n11111111111")
    assert_limit(Reader.probe(pdf))
    pdf = classic_pdf("1") |> String.replace("3 0 obj", "11111111111 0 obj")

    pdf =
      Regex.replace(~r/startxref\n(\d+)/, pdf, fn _, offset ->
        "startxref\n#{String.to_integer(offset) + 10}"
      end)

    assert_limit(Reader.read(pdf))

    {pdf, _} = object_stream_pdf(1)
    assert_limit(Reader.read(String.replace(pdf, "/W [1 4 2]", "/W [1 11 2]")))
  end

  test "each stream decoder observes the remaining operation budget before flattening" do
    limits = Limits.effective()

    for {filter, encoded, decoded} <- [
          {"FlateDecode", :zlib.compress(String.duplicate("abcd", 100)),
           String.duplicate("abcd", 100)},
          {"ASCIIHexDecode", "61 62 63>", "abc"},
          {"ASCIIHexDecode", "61626>", "ab`"},
          {"ASCII85Decode", " \n<~z~>", <<0, 0, 0, 0>>},
          {"ASCII85Decode", "!!!!!~>", <<0, 0, 0, 0>>},
          {"ASCII85Decode", "!!!!~>", <<0, 0, 0>>},
          {"RunLengthDecode", <<254, ?x, 128>>, "xxx"}
        ] do
      stream = %{
        ref: {1, 0},
        stream: encoded,
        filters: [%{name: filter, parameters: %{"Predictor" => 1}}]
      }

      Limits.install(%{limits | max_pdf_reader_decoded_bytes: byte_size(decoded)})
      assert {:ok, ^decoded} = Reader.decode_prepared_stream(stream)
      Limits.install(%{limits | max_pdf_reader_decoded_bytes: byte_size(decoded) - 1})
      assert_limit(Reader.decode_prepared_stream(stream))
    end

    Limits.install(%{limits | max_pdf_decoded_stream_bytes: 2})

    assert_limit(
      Reader.decode_prepared_stream(%{
        ref: {1, 0},
        stream: "616263>",
        filters: [%{name: "ASCIIHexDecode", parameters: %{"Predictor" => 1}}]
      })
    )
  end

  test "filter stages consume the same cumulative decoded budget" do
    limits = Limits.effective()
    encoded = :zlib.compress("abc")

    stream = %{
      ref: {1, 0},
      stream: Base.encode16(encoded) <> ">",
      filters: [
        %{name: "ASCIIHexDecode", parameters: %{"Predictor" => 1}},
        %{name: "FlateDecode", parameters: %{"Predictor" => 1}}
      ]
    }

    Limits.install(%{limits | max_pdf_reader_decoded_bytes: byte_size(encoded) + 3})
    assert {:ok, "abc"} = Reader.decode_prepared_stream(stream)
    Limits.install(%{limits | max_pdf_reader_decoded_bytes: byte_size(encoded) + 2})
    assert_limit(Reader.decode_prepared_stream(stream))
  end

  test "LZW control codes consume work even when they produce no output" do
    limits = Limits.effective()
    bits = Enum.reduce(1..32, <<>>, fn _, acc -> <<acc::bitstring, 256::9>> end)
    bits = <<bits::bitstring, 257::9>>
    encoded = <<bits::bitstring, 0::size(rem(8 - rem(bit_size(bits), 8), 8))>>

    stream = %{
      ref: {1, 0},
      stream: encoded,
      filters: [%{name: "LZWDecode", parameters: %{"Predictor" => 1, "EarlyChange" => 1}}]
    }

    assert {:ok, <<>>} = Reader.decode_prepared_stream(stream)
    Limits.install(%{limits | max_pdf_reader_work: byte_size(encoded) + 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Reader.decode_prepared_stream(stream)

    assert diagnostic.stage == :limits and diagnostic.operation == :read
    assert diagnostic.source == "object 1 0"
    assert diagnostic.message =~ "max_pdf_reader_work"
  end

  defp assert_limit(result) do
    assert {:error, {:resource_limit_exceeded, diagnostic}} = result
    assert diagnostic.stage == :limits
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.operation in [:read, :tokenize_all]
    assert diagnostic.module == Reader
    assert is_binary(diagnostic.message)
  end

  test "indirect stream lengths share their reference allowance and retain limit diagnostics" do
    limits = Limits.effective()
    stream = "<< /Length 4 0 R >>\nstream\nx\nendstream"
    pdf = classic_pdf(stream, [{4, "5 0 R"}, {5, "1"}, {6, stream}, {7, stream}])
    assert {:ok, _} = Reader.read(pdf)
    Limits.install(%{limits | max_pdf_reference_resolution_work: 3})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = Reader.read(pdf)
    assert diagnostic.stage == :limits and diagnostic.operation == :read
    assert diagnostic.module == Reader
    assert diagnostic.message =~ "indirect reference resolution work"
    Limits.install(limits)
    assert {:ok, _} = Reader.read(pdf)
  end

  defp classic_pdf(value, extra_objects \\ []) do
    objects =
      [
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [] /Count 0 >>"},
        {3, value}
      ] ++ extra_objects

    {body, offsets} =
      Enum.reduce(objects, {"%PDF-1.7\n", []}, fn {id, value}, {body, offsets} ->
        {body <> "#{id} 0 obj\n#{value}\nendobj\n", offsets ++ [byte_size(body)]}
      end)

    entries =
      Enum.map_join(offsets, fn offset ->
        String.pad_leading(to_string(offset), 10, "0") <> " 00000 n \n"
      end)

    size = length(objects) + 1

    body <>
      "xref\n0 #{size}\n0000000000 65535 f \n" <>
      entries <>
      "trailer\n<< /Size #{size} /Root 1 0 R >>\nstartxref\n#{byte_size(body)}\n%%EOF\n"
  end

  defp object_stream_pdf(count) do
    objects = [
      {1, "<< /Type /Catalog /Pages 2 0 R >>"},
      {2, "<< /Type /Pages /Kids [] /Count 0 >>"}
    ]

    {body, offsets} =
      Enum.reduce(objects, {"%PDF-1.7\n", %{}}, fn {id, value}, {body, offsets} ->
        {body <> "#{id} 0 obj\n#{value}\nendobj\n", Map.put(offsets, id, byte_size(body))}
      end)

    {body, offsets, decoded_size} =
      Enum.reduce(0..(count - 1), {body, offsets, 0}, fn index, {body, offsets, total} ->
        value = :crypto.hash(:sha256, to_string(index)) |> Base.encode16() |> String.duplicate(64)
        header = "#{10 + index} 0 "
        decoded = header <> "(" <> value <> ")"
        compressed = :zlib.compress(decoded)

        object =
          "#{100 + index} 0 obj\n<< /Type /ObjStm /N 1 /First #{byte_size(header)} /Length #{byte_size(compressed)} /Filter /FlateDecode >>\nstream\n" <>
            compressed <> "\nendstream\nendobj\n"

        {body <> object, Map.put(offsets, 100 + index, byte_size(body)),
         total + byte_size(decoded)}
      end)

    offsets = Map.put(offsets, 200, byte_size(body))

    entries =
      for id <- 0..200, into: <<>> do
        cond do
          id == 0 -> <<0, 0::32, 65_535::16>>
          id >= 10 and id < 10 + count -> <<2, 100 + id - 10::32, 0::16>>
          Map.has_key?(offsets, id) -> <<1, Map.fetch!(offsets, id)::32, 0::16>>
          true -> <<0, 0::32, 0::16>>
        end
      end

    stream =
      "200 0 obj\n<< /Type /XRef /Size 201 /Root 1 0 R /W [1 4 2] /Length #{byte_size(entries)} >>\nstream\n" <>
        entries <> "\nendstream\nendobj\n"

    {body <> stream <> "startxref\n#{byte_size(body)}\n%%EOF\n",
     decoded_size + byte_size(entries)}
  end
end
