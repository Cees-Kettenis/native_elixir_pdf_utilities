defmodule NativeElixirPdfUtilities.Pdf.ReaderTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.Pdf.Reader

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

  test "rejects malformed headers, final pointers, lexical syntax, and trailers" do
    assert_error(Reader.read("not a pdf"), :invalid_pdf_input, :header)
    assert_error(Reader.read("%PDF-9.0\n"), :invalid_pdf_input, :header)
    assert_error(Reader.read("%PDF-1.7\n"), :invalid_pdf_input, :xref)

    malformed_syntax =
      pdf([{1, "<< /Type /Catalog /Pages 2 0 R >>"}, {2, "<< /Type /Pages /Kids [] /Count 0 >>"}])
      |> String.replace("2 0 obj", "2 0 obj\n(unterminated")
      |> repoint_xref()

    assert_error(Reader.read(malformed_syntax), :invalid_pdf_input, :syntax)

    assert_error(
      Reader.read(pdf([{1, "42"}], "/Info 1 0 R")),
      :invalid_pdf_input,
      :trailer
    )

    malformed_trailer =
      pdf([{1, "42"}], "/Root 1 0 R")
      |> String.replace("trailer\n<<", "trailer\n42\n<<")

    assert_error(Reader.read(repoint_xref(malformed_trailer)), :invalid_pdf_input, :trailer)
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

    missing_pages = pdf([{1, "<< /Type /Catalog >>"}])
    assert_error(Reader.read(missing_pages), :invalid_pdf_input, :page_tree)

    invalid_kids =
      pdf([{1, "<< /Type /Catalog /Pages 2 0 R >>"}, {2, "<< /Type /Pages /Kids 3 /Count 0 >>"}])

    assert_error(Reader.read(invalid_kids), :invalid_pdf_input, :page_tree)

    invalid_type =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
        {3, "<< /Type /Unknown >>"}
      ])

    assert_error(Reader.read(invalid_type), :invalid_pdf_input, :page_tree)

    cycle =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
        {3, "<< /Type /Pages /Kids [2 0 R] /Count 1 >>"}
      ])

    assert_error(Reader.read(cycle), :invalid_pdf_input, :page_tree)
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

  defp stream_object(dictionary, stream, include_length?) do
    length = if include_length?, do: " /Length #{byte_size(stream)}", else: ""
    "<< #{dictionary}#{length} >>\nstream\n" <> stream <> "\nendstream"
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

  defp assert_error({:error, {reason, diagnostic}}, reason, expected_stage) do
    assert diagnostic.reason == reason

    if expected_stage == :filter_or_stream do
      assert diagnostic.stage in [:filter, :stream]
    else
      assert diagnostic.stage == expected_stage
    end
  end
end
