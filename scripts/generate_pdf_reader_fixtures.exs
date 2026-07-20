defmodule PdfReaderFixtureGenerator do
  @output Path.expand("../test/fixtures/pdf_reader", __DIR__)
  @header "%PDF-1.7\n%\xE2\xE3\xCF\xD3\n"
  @content "BT /F1 14 Tf 30 120 Td (Reader milestone fixture) Tj ET"

  def run do
    File.mkdir_p!(@output)

    {classic, classic_xref} = classic_pdf(base_objects(), "/Root 1 0 R")
    classic_path = Path.join(@output, "classic-xref.pdf")
    File.write!(classic_path, classic)
    File.write!(Path.join(@output, "xref-stream.pdf"), xref_stream_pdf())
    File.write!(Path.join(@output, "object-stream.pdf"), object_stream_pdf())
    File.write!(Path.join(@output, "hybrid-xref.pdf"), hybrid_pdf())

    File.write!(
      Path.join(@output, "incremental-update.pdf"),
      incremental_pdf(classic, classic_xref)
    )

    {_output, 0} =
      System.cmd("qpdf", [
        "--static-id",
        "--allow-weak-crypto",
        "--encrypt",
        "reader",
        "owner",
        "40",
        "--",
        classic_path,
        Path.join(@output, "encrypted.pdf")
      ])

    [before_xref, xref] = String.split(classic, "xref\n", parts: 2)

    malformed_xref =
      xref
      |> String.split("\n")
      |> List.update_at(4, fn entry ->
        "9999999999" <> binary_part(entry, 10, byte_size(entry) - 10)
      end)
      |> Enum.join("\n")

    File.write!(
      Path.join(@output, "malformed-xref.pdf"),
      before_xref <> "xref\n" <> malformed_xref
    )
  end

  defp base_objects do
    [
      {1, "<< /Type /Catalog /Pages 2 0 R >>"},
      {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
      {3,
       "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 300 200] /Resources << /Font << /F1 4 0 R >> >> /Contents 5 0 R >>"},
      {4, "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica /Encoding /WinAnsiEncoding >>"},
      {5, stream(@content)}
    ]
  end

  defp xref_stream_pdf do
    {body, offsets} = render_objects(base_objects())
    xref_object = 6
    xref_offset = byte_size(body)
    offsets = Map.put(offsets, xref_object, xref_offset)

    entries =
      for object <- 0..xref_object, into: <<>> do
        if object == 0 do
          <<0, 0::32, 65_535::16>>
        else
          <<1, Map.fetch!(offsets, object)::32, 0::16>>
        end
      end

    xref_stream =
      indirect_object(
        xref_object,
        stream(
          entries,
          "/Type /XRef /Size 7 /Root 1 0 R /W [1 4 2] /Index [0 7] /Filter /FlateDecode",
          true
        )
      )

    body <> xref_stream <> "startxref\n#{xref_offset}\n%%EOF\n"
  end

  defp object_stream_pdf do
    compressed_objects = Enum.take(base_objects(), 4)
    {object_stream, _pairs} = object_stream(compressed_objects)
    direct_objects = [{5, stream(@content)}, {6, object_stream}]
    {body, offsets} = render_objects(direct_objects)
    xref_object = 7
    xref_offset = byte_size(body)
    offsets = Map.put(offsets, xref_object, xref_offset)

    entries =
      for object <- 0..xref_object, into: <<>> do
        case object do
          0 -> <<0, 0::32, 65_535::16>>
          object when object in 1..4 -> <<2, 6::32, object - 1::16>>
          object -> <<1, Map.fetch!(offsets, object)::32, 0::16>>
        end
      end

    xref_stream =
      indirect_object(
        xref_object,
        stream(
          entries,
          "/Type /XRef /Size 8 /Root 1 0 R /W [1 4 2] /Filter /FlateDecode",
          true
        )
      )

    body <> xref_stream <> "startxref\n#{xref_offset}\n%%EOF\n"
  end

  defp hybrid_pdf do
    compressed_objects = Enum.take(base_objects(), 4)
    {object_stream, _pairs} = object_stream(compressed_objects)
    {body, offsets} = render_objects([{5, stream(@content)}, {6, object_stream}])
    xref_stream_offset = byte_size(body)

    compressed_entries =
      for object <- 1..4, into: <<>> do
        <<2, 6::32, object - 1::16>>
      end

    xref_stream =
      indirect_object(
        7,
        stream(
          compressed_entries,
          "/Type /XRef /Size 8 /Root 1 0 R /W [1 4 2] /Index [1 4]",
          false
        )
      )

    body = body <> xref_stream
    offsets = Map.put(offsets, 7, xref_stream_offset)
    xref_offset = byte_size(body)

    entries =
      ["xref\n0 1\n0000000000 65535 f \n", "5 3\n"] ++
        Enum.map(5..7, fn object -> classic_entry(Map.fetch!(offsets, object), 0, "n") end)

    body <>
      IO.iodata_to_binary(entries) <>
      "trailer\n<< /Size 8 /Root 1 0 R /XRefStm #{xref_stream_offset} >>\n" <>
      "startxref\n#{xref_offset}\n%%EOF\n"
  end

  defp incremental_pdf(classic, previous_xref) do
    updated = stream("BT /F1 14 Tf 30 120 Td (Incremental fixture) Tj ET")
    object_offset = byte_size(classic)
    update_object = indirect_object(5, updated)
    xref_offset = object_offset + byte_size(update_object)

    classic <>
      update_object <>
      "xref\n5 1\n" <>
      classic_entry(object_offset, 0, "n") <>
      "trailer\n<< /Size 6 /Root 1 0 R /Prev #{previous_xref} >>\n" <>
      "startxref\n#{xref_offset}\n%%EOF\n"
  end

  defp classic_pdf(objects, trailer) do
    {body, offsets} = render_objects(objects)
    maximum = Enum.max(Map.keys(offsets))
    xref_offset = byte_size(body)

    entries =
      for object <- 0..maximum do
        case Map.get(offsets, object) do
          nil -> classic_entry(0, if(object == 0, do: 65_535, else: 0), "f")
          offset -> classic_entry(offset, 0, "n")
        end
      end

    pdf =
      body <>
        "xref\n0 #{maximum + 1}\n" <>
        Enum.join(entries) <>
        "trailer\n<< /Size #{maximum + 1} #{trailer} >>\n" <>
        "startxref\n#{xref_offset}\n%%EOF\n"

    {pdf, xref_offset}
  end

  defp render_objects(objects) do
    Enum.reduce(objects, {@header, %{}}, fn {object, value}, {body, offsets} ->
      rendered = indirect_object(object, value)
      {body <> rendered, Map.put(offsets, object, byte_size(body))}
    end)
  end

  defp object_stream(objects) do
    {header, bodies, _offset} =
      Enum.reduce(objects, {[], [], 0}, fn {object, value}, {header, bodies, offset} ->
        {[header, "#{object} #{offset} "], [bodies, value, "\n"], offset + byte_size(value) + 1}
      end)

    header = IO.iodata_to_binary(header)
    data = header <> IO.iodata_to_binary(bodies)

    {stream(
       data,
       "/Type /ObjStm /N #{length(objects)} /First #{byte_size(header)} /Filter /FlateDecode",
       true
     ), objects}
  end

  defp stream(data, dictionary \\ "", compress? \\ false) do
    data = if compress?, do: :zlib.compress(data), else: data
    "<< /Length #{byte_size(data)} #{dictionary} >>\nstream\n" <> data <> "\nendstream"
  end

  defp indirect_object(object, value), do: "#{object} 0 obj\n#{value}\nendobj\n"

  defp classic_entry(offset, generation, marker) do
    String.pad_leading(Integer.to_string(offset), 10, "0") <>
      " " <>
      String.pad_leading(Integer.to_string(generation), 5, "0") <>
      " #{marker} \n"
  end
end

PdfReaderFixtureGenerator.run()
