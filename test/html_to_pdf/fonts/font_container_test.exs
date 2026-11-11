defmodule NativeElixirPdfUtilities.HtmlToPdf.FontContainerTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, TrueTypeSubset}
  alias NativeElixirPdfUtilities.HtmlToPdf.Font.Sfnt
  alias NativeElixirPdfUtilities.TestSupport.Paths
  alias NativeElixirPdfUtilities.Validators.{FontContainerValidator, FontValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    %{
      woff: File.read!(Paths.fixture("fonts/selected-container.woff")),
      ttc: File.read!(Paths.fixture("fonts/selected-container.ttc"))
    }
  end

  test "WOFF reconstruction and both selected collection faces preserve independently decoded tables",
       %{woff: woff, ttc: ttc} do
    manifest = Paths.fixture("fonts/selected-containers.json") |> File.read!() |> :json.decode()

    for {input, index, face} <- [{woff, 0, 0}, {ttc, 0, 0}, {ttc, 1, 1}] do
      assert {:ok, reconstructed} = FontContainerValidator.prepare(input, index)
      assert {:ok, _, directory} = FontValidator.prepare_table_directory(reconstructed)
      expected = Enum.at(manifest["faces"], face)

      for {tag, hash} <- expected["tables"] do
        {offset, length} = directory[tag]

        assert Base.encode16(:crypto.hash(:sha256, binary_part(reconstructed, offset, length)),
                 case: :lower
               ) == hash
      end

      assert Sfnt.checksum(reconstructed) == 0xB1B0_AFBA

      assert {:ok, [font]} =
               Font.load_configured_faces([
                 %{family: "Container", data: input, collection_index: index}
               ])

      assert font.data == reconstructed

      for {code, [advance, _]} <- expected["metrics"] do
        code = String.to_integer(code)
        assert Enum.at(font.widths, font.cmap[code]) == advance
      end

      assert {:ok, subset} =
               TrueTypeSubset.subset(font, [font.cmap[?H], font.cmap[?x], font.cmap[?0]])

      assert Sfnt.checksum(subset) == 0xB1B0_AFBA

      assert {:ok, pdf} =
               HtmlToPdf.render("<p style='font-family:Container'>Hx0</p>",
                 fonts: [%{family: "Container", data: input, collection_index: index}],
                 system_font_discovery: false
               )

      assert {:ok, "Hx0"} = Text.extract(pdf, layout: false)
    end
  end

  test "cached file loading keeps different collection selections separate", %{ttc: ttc} do
    path =
      Path.join(System.tmp_dir!(), "font-collection-#{System.unique_integer([:positive])}.ttc")

    File.write!(path, ttc)

    try do
      for index <- [0, 1, 0, 1] do
        assert {:ok, [face]} =
                 Font.load_configured_faces([
                   %{family: "Selected", path: path, collection_index: index}
                 ])

        assert {:ok, expected} = FontContainerValidator.prepare(ttc, index)
        assert face.data == expected
      end
    after
      File.rm!(path)
    end
  end

  test "uncompressed WOFF tables, optional metadata and private bytes use the same admission" do
    tables = [{"DSIG", <<1, 2, 3, 4>>}, {"head", <<0::size(96)>>}, {"test", <<1, 2, 3, 4>>}]
    woff = woff_tables(tables)
    assert {:ok, data} = FontContainerValidator.prepare(woff)
    assert Sfnt.checksum(data) == 0xB1B0_AFBA
    assert {:ok, _, directory} = FontValidator.prepare_table_directory(data)
    refute Map.has_key?(directory, "DSIG")

    assert {:ok, _} =
             FontContainerValidator.prepare(woff_tables([{"head", <<0::96>>}, {"test", <<>>}]))

    meta = :zlib.compress("metadata")
    padding = :binary.copy(<<0>>, rem(4 - rem(byte_size(meta), 4), 4))
    extended = woff <> meta <> padding <> "private"
    extended = replace_u32(extended, 8, byte_size(extended))

    extended =
      replace_u32(extended, 24, byte_size(woff))
      |> replace_u32(28, byte_size(meta))
      |> replace_u32(32, 8)

    extended =
      replace_u32(extended, 36, byte_size(woff) + byte_size(meta) + byte_size(padding))
      |> replace_u32(40, 7)

    assert {:ok, ^data} = FontContainerValidator.prepare(extended)
    assert {:ok, "raw"} = FontContainerValidator.prepare("raw")
    assert {:error, {:invalid_font, _}} = FontContainerValidator.prepare(nil)
    assert {:error, {:invalid_font, _}} = FontContainerValidator.prepare("raw", 1)
  end

  test "malformed WOFF directories and compressed programs return actionable diagnostics", %{
    woff: original
  } do
    <<_::binary-size(44), _::32, first_offset::32, first_size::32, _::binary>> = original

    malformed = [
      <<"wOFF">>,
      replace_u32(original, 8, 0),
      replace_u32(original, 12, 0),
      replace_u32(original, 16, 1),
      replace_u32(original, 44, 0),
      replace_u32(original, 48, 0),
      replace_u32(original, 48, first_offset + 1),
      replace_u32(original, 52, byte_size(original)),
      replace_u32(original, 52, 0),
      replace_u32(original, 24, first_offset),
      replace_u32(original, 36, first_offset),
      replace_u32(original, 60, 0),
      replace_bytes(original, first_offset, :binary.copy(<<255>>, min(first_size, 8)))
    ]

    for data <- malformed do
      assert {:error, {:invalid_font, diagnostic}} = FontContainerValidator.prepare(data)
      assert diagnostic.stage == :font and diagnostic.reason == :invalid_font
      assert diagnostic.operation == :prepare_font_source
      assert is_binary(diagnostic.source) and is_binary(diagnostic.message)
    end

    # Exercise all length/stream failures with independently framed payloads.
    compressed = :zlib.compress(:binary.copy("x", 100))

    for {bytes, declared} <- [
          {compressed, 99},
          {compressed, 101},
          {binary_part(compressed, 0, byte_size(compressed) - 1), 100},
          {<<0, 1, 2>>, 20},
          {"abcdef", 2}
        ] do
      data = woff_tables([{"head", <<0::96>>}, {"test", {bytes, declared, 0}}])
      assert {:error, {:invalid_font, _}} = FontContainerValidator.prepare(data)
    end

    assert {:error, {:invalid_font, %{source: "head"}}} =
             FontContainerValidator.prepare(woff_tables([{"head", <<0::32>>}]))

    assert {:error, {:invalid_font, %{source: "head"}}} =
             FontContainerValidator.prepare(woff_tables([{"test", <<0::32>>}]))
  end

  test "collection bounds, table overlap and duplicate tags fail before rebuilding", %{ttc: ttc} do
    <<_::binary-size(12), first::32, second::32, _::binary>> = ttc
    <<_::binary-size(^first + 12), tag::32, _::binary>> = ttc

    malformed = [
      <<"ttcf">>,
      replace_u32(ttc, 4, 0),
      replace_u32(ttc, 8, 0),
      replace_u32(ttc, 12, byte_size(ttc)),
      replace_u32(ttc, 12, 0),
      replace_u32(ttc, 16, first + 4),
      replace_u32(ttc, second, 0),
      replace_u32(ttc, first + 12 + 16, tag),
      replace_u32(ttc, first + 12 + 8, 0),
      replace_u32(ttc, first + 12 + 12, byte_size(ttc)),
      replace_u32(ttc, first + 12 + 8, second)
    ]

    for data <- malformed do
      assert {:error, {:invalid_font, %{stage: :font, operation: :prepare_font_source}}} =
               FontContainerValidator.prepare(data)
    end

    assert {:error, {:invalid_font, _}} = FontContainerValidator.prepare(ttc, 2)

    assert :error =
             Font.normalize_configs([%{family: "Invalid", data: ttc, collection_index: -1}])

    assert :error = Font.normalize_configs([%{family: "Invalid", data: ttc, css_face: :bad}])
  end

  test "malformed container candidates retain diagnostics and ordered fallback", %{woff: woff} do
    assert {:ok, [selected]} =
             Font.load_configured_faces([
               %{family: "Fallback", data: ["wOFF", "unsupported", woff]}
             ])

    assert {:ok, expected} = FontContainerValidator.prepare(woff)
    assert selected.data == expected

    assert {:error, {:invalid_font, %{source: "WOFF header"}}} =
             Font.load_configured_faces([%{family: "Fallback", data: ["wOFF", "unsupported"]}])

    directory =
      Path.join(System.tmp_dir!(), "font-fallback-#{System.unique_integer([:positive])}")

    File.mkdir_p!(directory)

    try do
      paths =
        for {name, bytes} <- [
              {"bad.woff", "wOFF"},
              {"unsupported.ttf", "unsupported"},
              {"valid.woff", woff}
            ] do
          path = Path.join(directory, name)
          File.write!(path, bytes)
          path
        end

      assert {:ok, [selected]} = Font.load_configured_faces([%{family: "Fallback", path: paths}])
      assert selected.data == expected

      assert {:error, {:invalid_font, _}} =
               Font.load_configured_faces([%{family: "Fallback", path: Enum.take(paths, 2)}])
    after
      File.rm_rf!(directory)
    end
  end

  test "large compressed tables drain bounded output chunks" do
    bytes = :binary.copy("x", 200_000)

    woff =
      woff_tables([
        {"head", <<0::96>>},
        {"test", {:zlib.compress(bytes), byte_size(bytes), Sfnt.checksum(bytes)}}
      ])

    assert {:ok, data} = FontContainerValidator.prepare(woff)
    assert Sfnt.checksum(data) == 0xB1B0_AFBA
  end

  test "collection version two validates optional signature ranges", %{ttc: ttc} do
    <<_::64, count::32, offsets::binary-size(8), payload::binary>> = ttc
    positions = for <<offset::32 <- offsets>>, do: offset + 12
    new_offsets = for offset <- positions, into: <<>>, do: <<offset::32>>

    version_two =
      <<"ttcf", 0x0002_0000::32, count::32, new_offsets::binary, 0::96, payload::binary>>

    version_two =
      Enum.reduce(positions, version_two, fn offset, data ->
        <<_::32, tables::16, _::48>> = binary_part(data, offset, 12)

        Enum.reduce(0..(tables - 1), data, fn index, data ->
          position = offset + 12 + index * 16 + 8
          <<old::32>> = binary_part(data, position, 4)
          replace_u32(data, position, old + 12)
        end)
      end)

    assert {:ok, original} = FontContainerValidator.prepare(ttc)
    assert {:ok, ^original} = FontContainerValidator.prepare(version_two)
    signed = version_two <> "DSIG"

    signed =
      replace_u32(signed, 20, 0x4453_4947)
      |> replace_u32(24, 4)
      |> replace_u32(28, byte_size(version_two))

    assert {:ok, ^original} = FontContainerValidator.prepare(signed)

    assert {:error, {:invalid_font, %{source: "TTC DSIG"}}} =
             FontContainerValidator.prepare(replace_u32(version_two, 20, 1))

    [first_position | _] = positions
    overlapping = replace_u32(signed, 28, first_position)
    assert {:error, {:invalid_font, _}} = FontContainerValidator.prepare(overlapping)
  end

  test "decoded bytes and cumulative reconstruction work have independent limits", %{
    woff: woff,
    ttc: ttc
  } do
    limits = Limits.effective()
    Limits.install(%{limits | max_font_cmap_work: 1})

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             FontValidator.with_budget(fn ->
               NativeElixirPdfUtilities.Validators.CssFontValidator.prepare_face_properties(%{
                 unicode_range: [{1, 2}, {3, 4}]
               })
             end)

    assert message =~ "max_font_cmap_work"
    Limits.install(%{limits | max_font_decoded_bytes: 1})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = FontContainerValidator.prepare(woff)
    assert diagnostic.message =~ "max_font_decoded_bytes"
    Limits.install(%{limits | max_font_container_work: 1})

    for data <- [woff, ttc] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               FontContainerValidator.prepare(data)

      assert diagnostic.message =~ "max_font_container_work"
    end

    Limits.install(%{limits | max_font_candidates: 1})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = FontContainerValidator.prepare(ttc)
    assert diagnostic.message =~ "max_font_candidates"
  end

  defp replace_u32(data, offset, value), do: replace_bytes(data, offset, <<value::32>>)

  defp replace_bytes(data, offset, bytes) do
    binary_part(data, 0, offset) <>
      bytes <>
      binary_part(data, offset + byte_size(bytes), byte_size(data) - offset - byte_size(bytes))
  end

  defp woff_tables(tables) do
    count = length(tables)

    {records, payloads, offset, total} =
      Enum.reduce(tables, {[], [], 44 + count * 20, 12 + count * 16}, fn {tag, entry},
                                                                         {records, payloads,
                                                                          offset, total} ->
        {bytes, original, checksum} =
          case entry do
            {bytes, original, checksum} -> {bytes, original, checksum}
            bytes -> {bytes, byte_size(bytes), Sfnt.checksum(bytes)}
          end

        size = byte_size(bytes)
        padding = :binary.copy(<<0>>, rem(4 - rem(size, 4), 4))
        record = <<tag::binary-size(4), offset::32, size::32, original::32, checksum::32>>

        {[record | records], [[bytes, padding] | payloads], offset + size + byte_size(padding),
         total + original + rem(4 - rem(original, 4), 4)}
      end)

    IO.iodata_to_binary([
      <<"wOFF", 0x0001_0000::32, offset::32, count::16, 0::16, total::32, 1::16, 0::16, 0::160>>,
      Enum.reverse(records),
      Enum.reverse(payloads)
    ])
  end
end
