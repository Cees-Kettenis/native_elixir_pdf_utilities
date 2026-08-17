defmodule NativeElixirPdfUtilities.Pdf.ReaderXrefTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Pdf.Reader

  @fixture_directory Path.expand("fixtures/pdf_reader", __DIR__)

  test "selects active generations and removes objects freed by later revisions" do
    generated =
      classic_pdf(
        [
          {1, 2, "<< /Type /Catalog /Pages 2 4 R >>"},
          {2, 4, "<< /Type /Pages /Kids [] /Count 0 >>"},
          {3, 7, "(active generation)"}
        ],
        "/Root 1 2 R"
      )

    assert {:ok, document} = Reader.read(generated.pdf)
    assert Map.has_key?(document.objects, {1, 2})
    assert Map.has_key?(document.objects, {2, 4})
    assert Reader.resolve(document, {:ref, {3, 7}}) == {:ok, {:string, "active generation"}}
    refute Map.has_key?(document.objects, {3, 0})

    freed = append_free_revision(generated.pdf, generated.xref, 3, 8, "/Root 1 2 R")
    assert {:ok, document} = Reader.read(freed)
    assert document.xref[3] == {:free, 0, 8}
    refute Map.has_key?(document.objects, {3, 7})
    assert_error(Reader.resolve(document, {:ref, {3, 7}}), :invalid_pdf_input, :resolution)
  end

  test "rejects malformed classic xref sections and revision pointers" do
    valid =
      classic_pdf([
        {1, 0, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, 0, "<< /Type /Pages /Kids [] /Count 0 >>"}
      ])

    malformed_cases = [
      String.replace(valid.pdf, "xref\n0 3", "xref\ninvalid", global: false),
      String.replace(valid.pdf, "0000000000 65535 f", "not-an-xref-record ", global: false),
      String.replace(valid.pdf, "0000000000 65535 f", "0000009999 65535 f", global: false),
      String.replace(valid.pdf, "trailer\n<<", "0 1\n0000000000 65535 f \ntrailer\n<<",
        global: false
      ),
      String.replace(valid.pdf, "/Root 1 0 R", "/Root 1 0 R /Prev /bad", global: false),
      String.replace(valid.pdf, "/Root 1 0 R", "/Root 1 0 R /XRefStm /bad", global: false)
    ]

    for malformed <- malformed_cases do
      assert_error(Reader.read(malformed), :invalid_pdf_input, :xref)
    end

    cyclic =
      valid.pdf
      |> String.replace("/Root 1 0 R", "/Root 1 0 R /Prev #{valid.xref}", global: false)

    assert_error(Reader.read(cyclic), :invalid_pdf_input, :xref)
  end

  test "requires object zero to be a free classic xref entry" do
    in_use_zero =
      classic_pdf([
        {0, 0, "(reserved object)"},
        {1, 0, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, 0, "<< /Type /Pages /Kids [] /Count 0 >>"}
      ])

    assert {:error,
            {:invalid_pdf_input,
             %{
               stage: :xref,
               message: "xref object 0 must be a free entry with generation 65535"
             }}} = Reader.read(in_use_zero.pdf)

    valid =
      classic_pdf([
        {1, 0, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, 0, "<< /Type /Pages /Kids [] /Count 0 >>"}
      ])

    missing_zero =
      String.replace(
        valid.pdf,
        "xref\n0 3\n" <> classic_entry(0, 65_535, "f"),
        "xref\n1 2\n",
        global: false
      )

    assert_error(Reader.read(missing_zero), :invalid_pdf_input, :xref)
  end

  test "parses xref stream widths and Index ranges and rejects invalid records" do
    assert {:ok, document} = Reader.read(xref_stream_pdf())
    assert document.xref[0] == {:free, 0, 65_535}
    assert match?({:uncompressed, _, 0}, document.xref[3])

    assert {:ok, indirect_length_document} =
             Reader.read(xref_stream_pdf(indirect_length: true))

    assert indirect_length_document.xref[4] |> elem(0) == :uncompressed

    malformed = [
      xref_stream_pdf(widths: [0, 4, 2], index: [1, 3]),
      xref_stream_pdf(object_zero_type: 1),
      xref_stream_pdf(widths: [1, 4]),
      xref_stream_pdf(widths: [0, 0, 0]),
      xref_stream_pdf(index: [0, 2, 1, 2]),
      xref_stream_pdf(index: [0, 5]),
      xref_stream_pdf(index: [0]),
      xref_stream_pdf(entry_type: 3),
      xref_stream_pdf(widths: [1, 4, 3], generation: 65_536),
      xref_stream_pdf(data_suffix: <<0>>),
      xref_stream_pdf(type: "NotXRef")
    ]

    for pdf <- malformed do
      assert {:error, {_reason, diagnostic}} = Reader.read(pdf)
      assert diagnostic.stage in [:xref, :object]
    end
  end

  test "rejects unsafe indirect xref stream Length objects" do
    malformed = [
      xref_stream_pdf(indirect_length: true, length_source: "/bad"),
      xref_stream_pdf(indirect_length: true, length_source: "1"),
      xref_stream_pdf(indirect_length: true, data_suffix: <<0>>)
    ]

    for pdf <- malformed do
      assert {:error, {:invalid_pdf_input, diagnostic}} = Reader.read(pdf)
      assert diagnostic.stage == :xref
      assert diagnostic.message =~ "Length reference cannot be resolved safely"
    end

    assert {:error, {:invalid_pdf_input, unconfirmed_diagnostic}} =
             Reader.read(xref_stream_pdf(indirect_length: true, length_xref_offset: 0))

    assert unconfirmed_diagnostic.stage == :object

    assert {:ok, _document} =
             Reader.read(xref_stream_pdf(indirect_length: true, candidate_suffix: "\n4 0 obj\n"))

    repeated_candidates = :binary.copy("4 0 obj\n35\nendobj\n", 1_001)

    assert {:error, {:resource_limit_exceeded, limit_diagnostic}} =
             Reader.read(
               xref_stream_pdf(indirect_length: true, candidate_prefix: repeated_candidates)
             )

    assert limit_diagnostic.stage == :limits
    assert limit_diagnostic.message == "xref stream Length candidate limit exceeded"

    missing_endobj =
      xref_stream_pdf()
      |> String.replace("\nendstream\nendobj\nstartxref", "\nendstream\nstartxref")

    assert_error(Reader.read(missing_endobj), :invalid_pdf_input, :object)
  end

  test "validates xref offsets against object headers and boundaries" do
    generated =
      classic_pdf([
        {1, 0, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, 0, "<< /Type /Pages /Kids [] /Count 0 >>"}
      ])

    wrong_object =
      String.replace(
        generated.pdf,
        classic_entry(generated.offsets[2], 0, "n"),
        classic_entry(0, 0, "f"),
        global: false
      )
      |> String.replace(
        classic_entry(generated.offsets[1], 0, "n"),
        classic_entry(generated.offsets[2], 0, "n"),
        global: false
      )

    assert_error(Reader.read(wrong_object), :invalid_pdf_input, :xref)

    duplicate_offsets =
      String.replace(
        generated.pdf,
        classic_entry(generated.offsets[2], 0, "n"),
        classic_entry(generated.offsets[1], 0, "n"),
        global: false
      )

    assert_error(Reader.read(duplicate_offsets), :invalid_pdf_input, :xref)

    non_object =
      String.replace(
        generated.pdf,
        classic_entry(generated.offsets[1], 0, "n"),
        classic_entry(0, 0, "n"),
        global: false
      )

    assert_error(Reader.read(non_object), :invalid_pdf_input, :object)
  end

  test "accepts xref offsets that point to whitespace before an object" do
    generated =
      classic_pdf([
        {1, 0, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, 0, "<< /Type /Pages /Kids [] /Count 0 >>"}
      ])

    whitespace = <<0, 9, 10, 12, 13, 32>>
    first_offset = generated.offsets[1]

    pdf =
      binary_part(generated.pdf, 0, first_offset) <>
        whitespace <>
        binary_part(generated.pdf, first_offset, byte_size(generated.pdf) - first_offset)

    shifted_second_offset = generated.offsets[2] + byte_size(whitespace)
    shifted_xref = generated.xref + byte_size(whitespace)

    pdf =
      pdf
      |> String.replace(
        classic_entry(generated.offsets[2], 0, "n"),
        classic_entry(shifted_second_offset, 0, "n"),
        global: false
      )
      |> String.replace(
        "startxref\n#{generated.xref}",
        "startxref\n#{shifted_xref}",
        global: false
      )

    assert {:ok, document} = Reader.read(pdf)
    assert document.objects[{1, 0}].offset == first_offset
  end

  test "validates object-stream containers and compressed entry indexes" do
    fixture = File.read!(Path.join(@fixture_directory, "object-stream.pdf"))

    assert_error(
      fixture |> String.replace("/N 4", "/N 5", global: false) |> Reader.read(),
      :invalid_pdf_input,
      :object_stream
    )

    assert_error(
      fixture |> String.replace("/ObjStm", "/BadStm", global: false) |> Reader.read(),
      :invalid_pdf_input,
      :object_stream
    )

    hybrid = File.read!(Path.join(@fixture_directory, "hybrid-xref.pdf"))

    no_direct_container =
      String.replace(hybrid, "0000000121 00000 n", "0000000000 00000 f", global: false)

    assert_error(Reader.read(no_direct_container), :invalid_pdf_input, :object_stream)

    mismatched_index =
      :binary.replace(
        hybrid,
        <<2, 6::32, 0::16, 2, 6::32, 1::16>>,
        <<2, 6::32, 3::16, 2, 6::32, 1::16>>
      )

    assert_error(Reader.read(mismatched_index), :invalid_pdf_input, :object_stream)

    catalog = "<< /Type /Catalog /Pages 2 0 R >>"
    pages = "<< /Type /Pages /Kids [] /Count 0 >>"
    header = "1 0 2 #{byte_size(catalog) + 1} "

    malformed_value = object_stream_pdf(header <> catalog <> "\ninvalid", byte_size(header))
    assert_error(Reader.read(malformed_value), :invalid_pdf_input, :object_stream)

    descending_offsets = object_stream_pdf("1 10 2 0 " <> catalog <> "\n" <> pages, 9)
    assert_error(Reader.read(descending_offsets), :invalid_pdf_input, :object_stream)

    duplicate_objects =
      object_stream_pdf(
        "1 0 1 #{byte_size(catalog) + 1} " <> catalog <> "\n" <> pages,
        byte_size(header)
      )

    assert_error(Reader.read(duplicate_objects), :invalid_pdf_input, :object_stream)

    malformed_header =
      object_stream_pdf(
        "1 0 X 2 #{byte_size(catalog) + 1} " <> catalog <> "\n" <> pages,
        byte_size(header) + 2
      )

    assert_error(Reader.read(malformed_header), :invalid_pdf_input, :object_stream)

    nested = String.duplicate("[", 100) <> "0" <> String.duplicate("]", 100)
    nested_catalog = "<< /Type /Catalog /Pages 2 0 R /Deep #{nested} >>"
    nested_header = "1 0 2 #{byte_size(nested_catalog) + 1} "

    assert {:error, {:resource_limit_exceeded, nested_diagnostic}} =
             Reader.read(
               object_stream_pdf(
                 nested_header <> nested_catalog <> "\n" <> pages,
                 byte_size(nested_header)
               )
             )

    assert nested_diagnostic.stage == :limits
    assert nested_diagnostic.message == "PDF value nesting depth exceeds the 100-level limit"
  end

  test "limits object-stream entries before scanning the decoded header" do
    excessive_entries = object_stream_pdf("", 0, 10_001)

    assert {:error, {:resource_limit_exceeded, diagnostic}} = Reader.read(excessive_entries)
    assert diagnostic.stage == :limits
    assert diagnostic.reason == :resource_limit_exceeded

    assert diagnostic.message ==
             "PDF object stream entry count exceeds the limit; object 6 0"
  end

  test "loads compressed Length objects before materializing ordinary streams" do
    stream = "length stored in an object stream"

    assert {:ok, document} = Reader.read(compressed_length_pdf(stream))
    assert document.objects[{3, 0}].stream == stream
    assert Reader.resolve(document, {:ref, {4, 0}}) == {:ok, byte_size(stream)}
    assert Reader.decoded_stream(document, {:ref, {3, 0}}) == {:ok, stream}
  end

  test "rejects malformed parser fallbacks, bounds, and resource limits" do
    assert_error(Reader.read(:binary.copy("x", 50_000_001)), :resource_limit_exceeded, :limits)

    generated =
      classic_pdf([
        {1, 0, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, 0, "<< /Type /Pages /Kids [42] /Count 1 >>"}
      ])

    assert_error(Reader.read(generated.pdf), :invalid_pdf_input, :page_tree)

    malformed_size = String.replace(generated.pdf, "/Size 3", "/Size 0", global: false)
    assert_error(Reader.read(malformed_size), :invalid_pdf_input, :xref)

    outside_previous =
      String.replace(generated.pdf, "/Root 1 0 R", "/Root 1 0 R /Prev 999999", global: false)

    assert_error(Reader.read(outside_previous), :invalid_pdf_input, :xref)

    xref_prefix = byte_size("%PDF-1.7\n")
    malformed_xref = "%PDF-1.7\nxrefgarbage\nstartxref\n#{xref_prefix}\n%%EOF\n"
    assert_error(Reader.read(malformed_xref), :invalid_pdf_input, :xref)

    body_pointer =
      String.replace(generated.pdf, ~r/startxref\n\d+/, "startxref\n1", global: false)

    assert_error(Reader.read(body_pointer), :invalid_pdf_input, :xref)

    missing_page =
      classic_pdf([
        {1, 0, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, 0, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"}
      ])

    assert_error(Reader.read(missing_page.pdf), :invalid_pdf_input, :resolution)

    free_entry = classic_entry(0, 0, "f")
    xref = byte_size("%PDF-1.7\n")

    too_many_objects =
      "%PDF-1.7\n" <>
        "xref\n0 100001\n" <>
        :binary.copy(free_entry, 100_001) <>
        "trailer\n<< /Size 100001 /Root 1 0 R >>\nstartxref\n#{xref}\n%%EOF\n"

    assert_error(Reader.read(too_many_objects), :resource_limit_exceeded, :limits)
  end

  test "limits incremental revision depth" do
    generated =
      classic_pdf([
        {1, 0, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, 0, "<< /Type /Pages /Kids [] /Count 0 >>"}
      ])

    {pdf, _xref} =
      Enum.reduce(1..1_000, {generated.pdf, generated.xref}, fn _revision, {pdf, previous_xref} ->
        xref = byte_size(pdf)

        revision =
          "xref\n0 1\n" <>
            classic_entry(0, 65_535, "f") <>
            "trailer\n<< /Size 3 /Root 1 0 R /Prev #{previous_xref} >>\n" <>
            "startxref\n#{xref}\n%%EOF\n"

        {pdf <> revision, xref}
      end)

    assert_error(Reader.read(pdf), :resource_limit_exceeded, :limits)
  end

  test "limits the number of resolved pages" do
    assert_error(Reader.read(large_page_pdf(10_001)), :resource_limit_exceeded, :limits)
  end

  defp classic_pdf(objects, trailer \\ "/Root 1 0 R") do
    header = "%PDF-1.7\n"

    {body, offsets} =
      Enum.reduce(objects, {header, %{}}, fn {object, generation, value}, {body, offsets} ->
        rendered = "#{object} #{generation} obj\n#{value}\nendobj\n"
        {body <> rendered, Map.put(offsets, object, {byte_size(body), generation})}
      end)

    maximum = offsets |> Map.keys() |> Enum.max()
    xref = byte_size(body)

    entries =
      for object <- 0..maximum do
        case Map.get(offsets, object) do
          nil -> classic_entry(0, if(object == 0, do: 65_535, else: 0), "f")
          {offset, generation} -> classic_entry(offset, generation, "n")
        end
      end

    pdf =
      body <>
        "xref\n0 #{maximum + 1}\n" <>
        Enum.join(entries) <>
        "trailer\n<< /Size #{maximum + 1} #{trailer} >>\n" <>
        "startxref\n#{xref}\n%%EOF\n"

    %{pdf: pdf, xref: xref, offsets: Map.new(offsets, fn {key, {offset, _}} -> {key, offset} end)}
  end

  defp append_free_revision(pdf, previous_xref, object, generation, trailer) do
    xref = byte_size(pdf)

    pdf <>
      "xref\n#{object} 1\n" <>
      classic_entry(0, generation, "f") <>
      "trailer\n<< /Size #{object + 1} #{trailer} /Prev #{previous_xref} >>\n" <>
      "startxref\n#{xref}\n%%EOF\n"
  end

  defp xref_stream_pdf(options \\ []) do
    header = "%PDF-1.7\n"
    candidate_prefix = Keyword.get(options, :candidate_prefix, "")
    header = header <> candidate_prefix
    object1 = "1 0 obj\n<< /Type /Catalog /Pages 2 0 R >>\nendobj\n"
    object2 = "2 0 obj\n<< /Type /Pages /Kids [] /Count 0 >>\nendobj\n"
    widths = Keyword.get(options, :widths, [1, 4, 2])
    indirect_length? = Keyword.get(options, :indirect_length, false)
    size = if indirect_length?, do: 5, else: 4
    index = Keyword.get(options, :index, [0, size])
    entry_type = Keyword.get(options, :entry_type, 1)
    data_suffix = Keyword.get(options, :data_suffix, <<>>)

    objects =
      index
      |> Enum.chunk_every(2)
      |> Enum.flat_map(fn
        [first, count] when is_integer(first) and is_integer(count) and count > 0 ->
          Enum.to_list(first..(first + count - 1))

        _ ->
          []
      end)

    row_width = Enum.sum(widths)
    declared_length = length(objects) * row_width + byte_size(data_suffix)

    length_object =
      case indirect_length? do
        true ->
          source = Keyword.get(options, :length_source, Integer.to_string(declared_length))
          "4 0 obj\n#{source}\nendobj\n"

        false ->
          ""
      end

    base = header <> object1 <> object2
    body = base <> length_object

    offsets = %{
      1 => byte_size(header),
      2 => byte_size(header <> object1),
      3 => byte_size(body),
      4 => byte_size(base)
    }

    data =
      Enum.reduce(objects, <<>>, fn object, data ->
        type =
          if object == 0,
            do: Keyword.get(options, :object_zero_type, 0),
            else: entry_type

        offset =
          case object do
            0 -> Keyword.get(options, :object_zero_offset, 0)
            4 -> Keyword.get(options, :length_xref_offset, Map.get(offsets, object, 0))
            _ -> Map.get(offsets, object, 0)
          end

        generation =
          if object == 0,
            do: Keyword.get(options, :object_zero_generation, 65_535),
            else: Keyword.get(options, :generation, 0)

        data <>
          unsigned(type, Enum.at(widths, 0, 0)) <>
          unsigned(offset, Enum.at(widths, 1, 0)) <>
          unsigned(generation, Enum.at(widths, 2, 0))
      end) <> data_suffix

    type = Keyword.get(options, :type, "XRef")
    index_text = Enum.map_join(index, " ", &to_string/1)
    widths_text = Enum.map_join(widths, " ", &to_string/1)

    dictionary =
      "<< /Type /#{type} /Size #{size} /Root 1 0 R /W [#{widths_text}] " <>
        "/Index [#{index_text}] /Length " <>
        if(indirect_length?, do: "4 0 R >>", else: "#{byte_size(data)} >>")

    xref_object = "3 0 obj\n#{dictionary}\nstream\n" <> data <> "\nendstream\nendobj\n"
    candidate_suffix = Keyword.get(options, :candidate_suffix, "")
    body <> xref_object <> candidate_suffix <> "startxref\n#{offsets[3]}\n%%EOF\n"
  end

  defp large_page_pdf(page_count) do
    kids = Enum.map_join(3..(page_count + 2), " ", &"#{&1} 0 R")

    objects =
      [
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [#{kids}] /Count #{page_count} >>"}
      ] ++
        Enum.map(3..(page_count + 2), fn object ->
          {object, "<< /Type /Page /Parent 2 0 R >>"}
        end)

    header = "%PDF-1.7\n"

    {pieces, offsets, _position} =
      Enum.reduce(objects, {[], %{}, byte_size(header)}, fn {object, value},
                                                            {pieces, offsets, position} ->
        rendered = "#{object} 0 obj\n#{value}\nendobj\n"

        {[rendered | pieces], Map.put(offsets, object, position), position + byte_size(rendered)}
      end)

    body = IO.iodata_to_binary([header, Enum.reverse(pieces)])
    xref = byte_size(body)
    maximum = page_count + 2

    entries =
      [classic_entry(0, 65_535, "f")] ++
        Enum.map(1..maximum, &classic_entry(Map.fetch!(offsets, &1), 0, "n"))

    body <>
      "xref\n0 #{maximum + 1}\n" <>
      IO.iodata_to_binary(entries) <>
      "trailer\n<< /Size #{maximum + 1} /Root 1 0 R >>\n" <>
      "startxref\n#{xref}\n%%EOF\n"
  end

  defp object_stream_pdf(data, first, count \\ 2) do
    header = "%PDF-1.7\n"

    object_stream =
      "6 0 obj\n<< /Type /ObjStm /N #{count} /First #{first} /Length #{byte_size(data)} >>\n" <>
        "stream\n" <> data <> "\nendstream\nendobj\n"

    body = header <> object_stream
    xref_offset = byte_size(body)
    offsets = %{6 => byte_size(header), 7 => xref_offset}

    entries =
      for object <- 0..7, into: <<>> do
        case object do
          0 -> <<0, 0::32, 65_535::16>>
          1 -> <<2, 6::32, 0::16>>
          2 -> <<2, 6::32, 1::16>>
          object when object in 3..5 -> <<0, 0::32, 0::16>>
          object -> <<1, Map.fetch!(offsets, object)::32, 0::16>>
        end
      end

    xref_stream =
      "7 0 obj\n<< /Type /XRef /Size 8 /Root 1 0 R /W [1 4 2] " <>
        "/Length #{byte_size(entries)} >>\nstream\n" <>
        entries <> "\nendstream\nendobj\n"

    body <> xref_stream <> "startxref\n#{xref_offset}\n%%EOF\n"
  end

  defp compressed_length_pdf(stream) do
    header = "%PDF-1.7\n"

    content_stream =
      "3 0 obj\n<< /Length 4 0 R >>\nstream\n" <> stream <> "\nendstream\nendobj\n"

    catalog = "<< /Type /Catalog /Pages 2 0 R >>"
    pages = "<< /Type /Pages /Kids [] /Count 0 >>"
    length = Integer.to_string(byte_size(stream))
    pages_offset = byte_size(catalog) + 1
    length_offset = pages_offset + byte_size(pages) + 1
    object_stream_header = "1 0 2 #{pages_offset} 4 #{length_offset} "
    object_stream_data = object_stream_header <> catalog <> "\n" <> pages <> "\n" <> length

    length_object = "5 0 obj\n#{byte_size(object_stream_data)}\nendobj\n"

    object_stream =
      "6 0 obj\n<< /Type /ObjStm /N 3 /First #{byte_size(object_stream_header)} " <>
        "/Length 5 0 R >>\nstream\n" <>
        object_stream_data <> "\nendstream\nendobj\n"

    body = header <> content_stream <> length_object <> object_stream
    xref_offset = byte_size(body)

    offsets = %{
      3 => byte_size(header),
      5 => byte_size(header <> content_stream),
      6 => byte_size(header <> content_stream <> length_object),
      7 => xref_offset
    }

    entries =
      for object <- 0..7, into: <<>> do
        case object do
          0 -> <<0, 0::32, 65_535::16>>
          1 -> <<2, 6::32, 0::16>>
          2 -> <<2, 6::32, 1::16>>
          3 -> <<1, Map.fetch!(offsets, 3)::32, 0::16>>
          4 -> <<2, 6::32, 2::16>>
          5 -> <<1, Map.fetch!(offsets, 5)::32, 0::16>>
          6 -> <<1, Map.fetch!(offsets, 6)::32, 0::16>>
          7 -> <<1, Map.fetch!(offsets, 7)::32, 0::16>>
        end
      end

    xref_stream =
      "7 0 obj\n<< /Type /XRef /Size 8 /Root 1 0 R /W [1 4 2] " <>
        "/Length #{byte_size(entries)} >>\nstream\n" <>
        entries <> "\nendstream\nendobj\n"

    body <> xref_stream <> "startxref\n#{xref_offset}\n%%EOF\n"
  end

  defp unsigned(value, width) do
    case width do
      0 -> <<>>
      width -> <<value::unsigned-big-integer-size(width * 8)>>
    end
  end

  defp classic_entry(offset, generation, marker) do
    String.pad_leading(Integer.to_string(offset), 10, "0") <>
      " " <>
      String.pad_leading(Integer.to_string(generation), 5, "0") <>
      " #{marker} \n"
  end

  defp assert_error({:error, {reason, diagnostic}}, reason, stage) do
    assert diagnostic.reason == reason
    assert diagnostic.stage == stage
  end
end
