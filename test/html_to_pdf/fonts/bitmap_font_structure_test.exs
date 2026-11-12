Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("font_fixture.ex"))
Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.BitmapFontStructureTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.FontCorpus
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, PdfWriter}
  alias NativeElixirPdfUtilities.TestSupport.{FontFixture, PngFixture, Paths}
  alias NativeElixirPdfUtilities.Validators.{BitmapFontValidator, FontValidator}

  test "all CBLC index forms and applicable CBDT PNG/BGRA metrics agree with original one-pixel drawings" do
    source = File.read!(FontCorpus.path("selected-cbdt.ttf"))
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Bitmap", data: source}])

    for index <- 1..5, format <- [1, 2, 5, 6, 7, 17, 18, 19] do
      {bitmap, location} = cbdt(index, format)
      data = FontFixture.rebuild(source, %{"CBDT" => bitmap, "CBLC" => location})
      selected = %{font | data: data}
      result = BitmapFontValidator.prepare(selected)

      if format in [5, 19] and index not in [2, 5] do
        assert {:error, {:invalid_font, diagnostic}} = result
        assert diagnostic.message =~ "shared"
      else
        assert {:ok, [strike]} = result
        record = BitmapFontValidator.select([strike], 2, 20)
        assert record.x == 0
        assert record.y == 0
        assert record.expected == {1, 1}
        assert record.advance == 1

        assert Map.keys(strike.glyphs) |> Enum.sort() ==
                 if(index == 5, do: [2, 4], else: [2, 3, 4])

        assert {:ok, pdf} =
                 PdfWriter.render([
                   %{
                     size: {80, 60},
                     boxes: [
                       %{
                         type: :text,
                         text: "H",
                         x: 10,
                         y: 20,
                         font: Font.pdf_name(selected),
                         font_face: selected,
                         font_size: 15,
                         color: {0, 0, 0}
                       }
                     ]
                   }
                 ])

        assert {:ok, "H"} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
      end
    end
  end

  test "vertical bitmap glyphs preserve the page clip outside their glyph transform" do
    source = File.read!(FontCorpus.path("selected-cbdt.ttf"))
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Bitmap", data: source}])
    clip = %{x: 5.0, y: 5.0, width: 30.0, height: 30.0}

    box = %{
      type: :text,
      text: "H",
      x: 10,
      y: 20,
      font: Font.pdf_name(font),
      font_face: font,
      font_size: 15,
      color: {0, 0, 0},
      glyph_basis: {0, 1, -1, 0},
      clip: clip
    }

    assert {:ok, %{pages: [%{boxes: boxes}]}} =
             NativeElixirPdfUtilities.Validators.ColorFontValidator.prepare(
               %{color_fonts?: true, pages: [%{size: {80, 60}, boxes: [box]}]},
               []
             )

    assert hd(boxes) == %{type: :actual_text_start, text: "H"}
    assert List.last(boxes) == %{type: :actual_text_end}
    [outer, rotation] = Enum.filter(boxes, &(&1.type == :drawing_group_start))
    assert outer.clip == clip
    assert outer.transform == {1, 0, 0, 1, 0, 0}
    assert {0, 1, -1, 0, _, _} = rotation.transform
    image = Enum.find(boxes, &(&1.type == :image))
    assert image.type == :image and image.width > 0 and image.height > 0
    assert Map.get(image, :clip) == nil
    assert rotation.clip == nil
  end

  test "bitmap header, directory, sparse index, metrics and payload failures retain source diagnostics" do
    source = File.read!(FontCorpus.path("selected-cbdt.ttf"))
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Bitmap", data: source}])
    {bitmap, location} = cbdt(1, 17)

    cases =
      for {offset, bytes} <- [
            {0, <<0::16>>},
            {8, <<0::32>>},
            {20, <<1::32>>},
            {48, <<5::16>>},
            {56, <<5::16>>},
            {60, <<0::32>>},
            {64, <<6::16>>},
            {68, <<0xFFFFFF::32>>}
          ],
          do: {bitmap, FontFixture.replace(location, offset, bytes)}

    cases =
      cases ++
        [
          {FontFixture.replace(bitmap, 0, <<0::32>>), location},
          {FontFixture.replace(bitmap, 4, <<0>>), location},
          {FontFixture.replace(bitmap, 9, <<999::32>>), location},
          {binary_part(bitmap, 0, byte_size(bitmap) - 1), location}
        ]

    for {{bitmap, location}, case_index} <- Enum.with_index(cases) do
      data = FontFixture.rebuild(source, %{"CBDT" => bitmap, "CBLC" => location})

      result = BitmapFontValidator.prepare(%{font | data: data})
      assert match?({:error, {_, _}}, result), inspect({case_index, result})
      {:error, {reason, diagnostic}} = result

      assert reason in [:invalid_font, :unsupported_font]
      assert diagnostic.operation == :prepare_color_font
      assert diagnostic.source == "bitmap font"
    end

    for {index, format, change} <- [
          {4, 17, :duplicate},
          {5, 19, :duplicate},
          {1, 17, :vertical},
          {1, 99, :unchanged},
          {1, 1, :depth},
          {1, 1, :payload}
        ] do
      {bitmap, location} = cbdt(index, format)

      {bitmap, location} =
        case change do
          :duplicate ->
            {bitmap, FontFixture.replace(location, if(index == 4, do: 80, else: 90), <<2::16>>)}

          :vertical ->
            {bitmap, FontFixture.replace(location, 55, <<2>>)}

          :depth ->
            {bitmap, FontFixture.replace(location, 54, <<8>>)}

          :payload ->
            {FontFixture.replace(bitmap, 4, <<1, 2>>), location}

          _ ->
            {bitmap, location}
        end

      data = FontFixture.rebuild(source, %{"CBDT" => bitmap, "CBLC" => location})

      assert {:error, {_, %{message: message}}} =
               BitmapFontValidator.prepare(%{font | data: data})

      assert message != ""
    end

    assert {:error, {:invalid_font, _}} =
             BitmapFontValidator.prepare_source(FontFixture.rebuild(source, %{"head" => <<>>}))

    assert {:error, {:invalid_font, _}} =
             BitmapFontValidator.prepare_source(
               FontFixture.rebuild(source, %{"maxp" => <<0x5000::32, 0::16>>})
             )

    assert {:error, {:invalid_document, _}} = BitmapFontValidator.prepare(%{font | data: <<>>})
  end

  test "sbix ownership, duplicate cycles and encoding errors cannot reach image decoding" do
    source = File.read!(FontCorpus.path("selected-sbix.ttf"))
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Sbix", data: source}])
    {_scaler, tables} = FontFixture.tables(source)
    data = tables["sbix"]
    <<first::32>> = binary_part(data, 8, 4)
    <<glyph::32>> = binary_part(data, first + 4 + 2 * 4, 4)
    <<dupe::32>> = binary_part(data, first + 4 + 4 * 4, 4)

    for {offset, bytes} <- [
          {0, <<2::16>>},
          {2, <<0::16>>},
          {8, <<0::32>>},
          {first, <<0::16>>},
          {first + 4, <<0::32>>},
          {first + glyph + 4, "tiff"},
          {first + dupe + 8, <<4::16>>},
          {first + dupe + 8, <<65535::16>>}
        ] do
      changed = FontFixture.rebuild(source, %{"sbix" => FontFixture.replace(data, offset, bytes)})
      assert {:error, {_, diagnostic}} = BitmapFontValidator.prepare(%{font | data: changed})
      assert diagnostic.stage == :font
      assert diagnostic.message != ""
    end

    assert {:ok, _, _} = FontValidator.prepare_table_directory(source)
  end

  test "JPEG strikes, BGRA transparency and dimensions share the public writer diagnostics" do
    limits = NativeElixirPdfUtilities.Limits.effective()
    on_exit(fn -> NativeElixirPdfUtilities.Limits.install(limits) end)
    source = File.read!(FontCorpus.path("selected-sbix.ttf"))
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Bitmap", data: source}])
    jpeg = File.read!(Paths.fixture("images/jpeg/restart_rgb.jpg"))

    for {encoding, payload, valid?} <- [
          {"jpg ", jpeg, true},
          {"jpg ", <<1, 2, 3>>, false},
          {"dupe", <<2>>, false}
        ] do
      selected = %{
        font
        | data: FontFixture.rebuild(source, %{"sbix" => sbix_table(font, encoding, payload)})
      }

      box = %{
        type: :text,
        text: "H",
        font: Font.pdf_name(selected),
        font_face: selected,
        x: 10,
        y: 20,
        font_size: 15,
        color: {0, 0, 0}
      }

      result = PdfWriter.render([%{size: {600, 600}, boxes: [box]}], compress_streams: false)

      if valid? do
        assert {:ok, pdf} = result
        assert pdf =~ "/DCTDecode"
        NativeElixirPdfUtilities.Limits.install(Map.put(limits, :max_decoded_bytes, 1))

        assert {:error, {:resource_limit_exceeded, _}} =
                 PdfWriter.render([%{size: {600, 600}, boxes: [box]}])

        NativeElixirPdfUtilities.Limits.install(limits)
      else
        assert {:error, {:invalid_font, _}} = result
      end
    end

    source = File.read!(FontCorpus.path("selected-cbdt.ttf"))
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Bitmap", data: source}])

    for {format, change, valid?} <- [
          {1, :transparent, true},
          {1, :nonpremultiplied, false},
          {17, :dimensions, false}
        ] do
      {bitmap, location} = cbdt(1, format)

      bitmap =
        case change do
          :transparent -> FontFixture.replace(bitmap, 9, <<0, 0, 0, 0>>)
          :nonpremultiplied -> FontFixture.replace(bitmap, 9, <<0, 0, 255, 0>>)
          :dimensions -> FontFixture.replace(bitmap, 5, <<2>>)
        end

      selected = %{
        font
        | data: FontFixture.rebuild(source, %{"CBDT" => bitmap, "CBLC" => location})
      }

      box = %{
        type: :text,
        text: "H",
        font: Font.pdf_name(selected),
        font_face: selected,
        x: 10,
        y: 20,
        font_size: 15,
        color: {0, 0, 0},
        glyph_ids: [2]
      }

      result = PdfWriter.render([%{size: {80, 80}, boxes: [box]}])

      if valid?,
        do: assert(match?({:ok, _}, result)),
        else: assert(match?({:error, {:invalid_font, _}}, result))

      if valid? do
        NativeElixirPdfUtilities.Limits.install(Map.put(limits, :max_decoded_bytes, 1))

        assert {:error, {:resource_limit_exceeded, _}} =
                 PdfWriter.render([%{size: {80, 80}, boxes: [box]}])

        NativeElixirPdfUtilities.Limits.install(limits)
      end
    end

    {bitmap, location} = cbdt(1, 17)
    # Equal adjacent offsets omit one bitmap without dropping the strike.
    record_size = div(byte_size(bitmap) - 4, 3)
    location = FontFixture.replace(location, 76, <<0::32, record_size::32, 2 * record_size::32>>)

    selected = %{
      font
      | data: FontFixture.rebuild(source, %{"CBDT" => bitmap, "CBLC" => location})
    }

    assert {:ok, [strike]} = BitmapFontValidator.prepare(selected)
    refute Map.has_key?(strike.glyphs, 2)
    # A sparse image may not claim a glyph outside its declared range.
    {bitmap, location} = cbdt(4, 17)
    location = FontFixture.replace(location, 76, <<1::16>>)

    selected = %{
      font
      | data: FontFixture.rebuild(source, %{"CBDT" => bitmap, "CBLC" => location})
    }

    assert {:error, {:invalid_font, _}} = BitmapFontValidator.prepare(selected)
  end

  defp sbix_table(font, encoding, payload) do
    glyph = <<0::32, encoding::binary, payload::binary>>

    records =
      for id <- 0..(length(font.widths) - 1), do: if(id == font.cmap[?H], do: glyph, else: <<>>)

    start = 4 + (length(records) + 1) * 4

    {offsets, finish} =
      Enum.map_reduce(records, start, fn record, offset ->
        {offset, offset + byte_size(record)}
      end)

    <<1::16, 1::16, 1::32, 12::32, 20::16, 72::16,
      for(n <- offsets ++ [finish], into: <<>>, do: <<n::32>>)::binary,
      IO.iodata_to_binary(records)::binary>>
  end

  defp cbdt(index, format) do
    metrics = <<1, 1, 0, 1, 1>>
    big = metrics <> <<0, 0, 1>>
    pixel = <<0, 0, 255, 255>>
    png = PngFixture.build(1, 1, 6, 8, 0, fn _, _ -> [255, 0, 0, 255] end)
    encoded = if format in [17, 18, 19], do: <<byte_size(png)::32, png::binary>>, else: pixel

    glyph =
      cond do
        format in [5, 19] -> encoded
        format in [6, 7, 18] -> big <> encoded
        true -> metrics <> encoded
      end

    ids = if index == 5, do: [2, 4], else: [2, 3, 4]
    n = length(ids)
    offsets = for i <- 0..n, do: i * byte_size(glyph)

    directory =
      case index do
        1 ->
          for position <- offsets, into: <<>>, do: <<position::32>>

        3 ->
          for position <- offsets, into: <<>>, do: <<position::16>>

        2 ->
          <<byte_size(glyph)::32, big::binary>>

        5 ->
          <<byte_size(glyph)::32, big::binary, n::32,
            for(id <- ids, into: <<>>, do: <<id::16>>)::binary>>

        4 ->
          <<n::32,
            for(
              {id, position} <- Enum.zip(ids ++ [5], offsets),
              into: <<>>,
              do: <<id::16, position::16>>
            )::binary>>
      end

    subtable = <<index::16, format::16, 4::32, directory::binary>>
    array = <<2::16, 4::16, 8::32, subtable::binary>>

    location =
      <<3::16, 0::16, 1::32, 56::32, byte_size(array)::32, 1::32, 0::32, 0::size(24 * 8), 2::16,
        4::16, 20, 20, 32, 1, array::binary>>

    {<<3::16, 0::16, :binary.copy(glyph, n)::binary>>, location}
  end
end
