defmodule NativeElixirPdfUtilities.Pdf.ReaderBudgetTest do
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
    pdf = File.read!(Path.join(__DIR__, "fixtures/pdf_reader/classic-xref.pdf"))
    limits = Limits.effective()

    for key <- [:max_pdf_reader_tokens, :max_pdf_reader_values, :max_pdf_reader_work] do
      Limits.install(Map.put(limits, key, 1))
      assert_limit(Reader.read(pdf))
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

  defp assert_limit(result) do
    assert {:error, {:resource_limit_exceeded, diagnostic}} = result
    assert diagnostic.stage == :limits
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.operation in [:read, :tokenize_all]
    assert diagnostic.module == Reader
    assert is_binary(diagnostic.message)
  end

  defp classic_pdf(value) do
    objects = [
      {1, "<< /Type /Catalog /Pages 2 0 R >>"},
      {2, "<< /Type /Pages /Kids [] /Count 0 >>"},
      {3, value}
    ]

    {body, offsets} =
      Enum.reduce(objects, {"%PDF-1.7\n", []}, fn {id, value}, {body, offsets} ->
        {body <> "#{id} 0 obj\n#{value}\nendobj\n", offsets ++ [byte_size(body)]}
      end)

    entries =
      Enum.map_join(offsets, fn offset ->
        String.pad_leading(to_string(offset), 10, "0") <> " 00000 n \n"
      end)

    body <>
      "xref\n0 4\n0000000000 65535 f \n" <>
      entries <>
      "trailer\n<< /Size 4 /Root 1 0 R >>\nstartxref\n#{byte_size(body)}\n%%EOF\n"
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
