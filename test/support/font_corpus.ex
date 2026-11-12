Code.require_file("paths.ex", __DIR__)
Code.require_file("png_fixture.ex", __DIR__)
Code.require_file("cff_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.TestSupport.FontCorpus do
  @moduledoc """
  Small shared font sources and test-owned table variants. Generated files live
  under tmp/; no system font, download, Python or external encoder is needed.

  The literal COLR and variation records retain independently encoded FontTools
  inputs. Expected outlines/matrices remain in the separate fixture manifests.
  Repeated glyphs, bitmap strikes and long programs are assembled during setup.
  """
  import Bitwise
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PngFixture, CffFixture}

  @static_glyf Base.decode16!(
                 "000300500000020802BC00030007000B000073331123013311230333352350646401546464F0F0F002BCFD4402BCFE70" <>
                   "6400000100460000021201F4000B0000733727331737330717232707469696826464829696826464FAFAA0A0FAFAA0A0" <>
                   "0001003C0000021C02BC00050000612210333210012CF0F0F002BCFD4400FFFF00820028023A039D042600023228000F" <>
                   "000300C802EE1666"
               )

  @static_name Base.decode16!(
                 "0000000A007E000100000000000100110000000100000000000200070011000100000000000300110000000100000000" <>
                   "0004001100000001000000000006001000180003000104090001002200280003000104090002000E004A000300010409" <>
                   "000300220028000300010409000400220028000300010409000600200058417574686F726564205661726961626C6552" <>
                   "6567756C6172417574686F7265645661726961626C650041007500740068006F00720065006400200056006100720069" <>
                   "00610062006C00650052006500670075006C006100720041007500740068006F00720065006400560061007200690061" <>
                   "0062006C0065"
               )

  @palette Base.decode16!(
             "00010002000200040000001C000000020000002C00000000000000001F29F0FFF047148033BF1AFF1AB2F2FF00000001" <>
               "00000002"
           )

  @paint_palette Base.decode16!(
                   "00010002000200040000001C000000020000002C00000000000000001A1AE6FFE6331A8033CC1AFF1AB2E6FF00000001" <>
                     "00000002"
                 )

  @colr_layers Base.decode16!(
                 "000000020000000E0000001A0003000200000002000300020001000200000003000100040001"
               )

  @colr_gradients Base.decode16!(
                    "0001000000000000000000000000000000220000008E0000000000000000000000000000000400020000001C00030000" <>
                      "00220004000000440005000000560102000000000C00000700000A0B0002000080000000000000000000000080000064" <>
                      "0000001400000A000006000408000044012C015EC00040000A000006000206000032012C015E0190012C015E00190000" <>
                      "00020000000C000000310A0000060002040000100050000002080000005002BC0000020000000040004000000140000E" <>
                      "000008001E00640A00000600030200014000"
                  )

  @colr_paints Base.decode16!(
                 "000100000000000000000000000000000022000004350000045700000487000004AB0000003D00020000041F00060000" <>
                   "0172000700000408000800000178000900000181000A00000191000B000001A5000C000001B5000D000001C9000E0000" <>
                   "01E4000F0000020B00100000021100110000021400120000023300130000025600140000025E00150000026A00160000" <>
                   "027200170000027E00180000028A00190000029A001A000002A0001B000002AA001C000002B4001D000002C2001E0000" <>
                   "02C8001F000002D20020000002DC0021000002EA0022000002F20023000002FE00240000030A00250000031A00260000" <>
                   "032200270000032A00280000033200290000033A002A00000342002B0000034A002C00000352002D0000035A002E0000" <>
                   "0362002F0000036A00300000037200310000037A00320000038200330000038A00340000039200350000039A00360000" <>
                   "03A20037000003AA0038000003B20039000003BA003A000003C2003B000003CA003C000003D2003D000003DA003E0000" <>
                   "03E2003F000003EA0040000003F20041000003FA01020000000003000030000000000004000054005000000208003200" <>
                   "5002BC050000630050000002080032005002BC000000000600003000FA012C0019012C015E01900700003F00FA012C00" <>
                   "19012C015E0190000000000800000C012C015EC000400000000200000000400040000001300009000010012C015EC000" <>
                   "40000000000000000200000000400000000014400000013000000000160A00022500020B00020C00020B0000070000E6" <>
                   "660000199AFFFFE6660000E66600280000001400000D0001EC0000070000E6660000199AFFFFE6660000E66600280000" <>
                   "00140000000000000E0001C90032FFE20F0001C10032FFE200000000100001B533334666110001AD3333466600000000" <>
                   "120001A133334666012C015E1300019533334666012C015E000000001400018533331500017F33330000000016000175" <>
                   "3333012C015E1700016B3333012C015E000000001800015D071C19000157071C000000001A00014D071C012C015E1B00" <>
                   "0143071C012C015E000000001C000135038EFE391D00012D038EFE39000000001E000121038EFE39012C015E1F000115" <>
                   "038EFE39012C015E000000002000010503000110200000E0000000EB200000D8010000E3200000D0020000DB200000C8" <>
                   "030000D3200000C0040000CB200000B8050000C3200000B0060000BB200000A8070000B3200000A0080000AB20000098" <>
                   "090000A3200000900A00009B200000880B000093200000800C00008B200000780D000083200000700E00007B20000068" <>
                   "0F000073200000601000006B2000005811000063200000501200005B2000004813000053200000401400004B20000038" <>
                   "15000043200000301600003B2000002817000033200000201800002B2000001819000023200000101A00001B20000008" <>
                   "1B0000130A000006000202000030000A0000230004000000020000000C000000170A000006000202000040000A000006" <>
                   "000302000140000100000003000200020000270006002500001A0026004100002702FF9CFF9C038403840000001801FF" <>
                   "9CFF9C0384038400040020010003020504070609080B0A0D0C0F0E111013121514171619181B1A1D1C1F1E0001000000" <>
                   "0C00010000001C0002000100004000400000000000000000200000000100000102030405060708090A0B0C0D0E0F1011" <>
                   "12131415161718191A1B1C1D1E1F20"
               )

  @control_gvar Base.decode16!(
                  "000100000004000100000022000600000000002A00000006000C00180024002D00340000000040000000800100080002" <>
                    "00000083830080010008000200000083830080010008000F0000000B787878787878787878787878838F80010008000F" <>
                    "0000000B787878787878787878787878838F800100080009000000057878787878788389800100080005000000017878" <>
                    "8385"
                )

  @gasp_mvar Base.decode16!(
               "00010000000000080003002467737030000000026861736300000000786867740000000100010000000C000100000040" <>
                 "00020004000040004000000000000000C000C0000000000000000000000000000000000040004000000000000000C000" <>
                 "C0000000000300000004000000010002000332EC1EF114F80CFA04000000"
             )

  @vertical_vvar Base.decode16!(
                   "00010000000000180000003E00000000000000000000004800010000000C00010000001C000200010000400040000000" <>
                     "00000000000200000001000064140000000600000000000000000006010101010101"
                 )

  @paint_cmap Base.decode16!(
                "00000002000000030000001400030001000000140004005000000010001000030000002000300048007800C9E01FE11B" <>
                  "FFFF0000002000300048007800C9E000E100FFFFFFE1FFD4FFBAFF8BFF3C20061F260001000000000000000000000000" <>
                  "00000000"
              )

  @variants ~w(selected-sbix.ttf selected-cbdt.ttf selected-bitmap-only.ttf
    selected-colr.ttf selected-colr-v1.ttf selected-svg.ttf selected-svg-gzip.ttf
    selected-colr-paints.ttf selected-large-cff2.otf selected-large-seac.otf
    selected-random.otf selected-control-axes.ttf selected-vertical-cff2.otf
    selected-gasp-variable.ttf selected-collection.woff2
    selected-woff2-collection-253.woff2 selected-woff2-collection-506.woff2
    selected-woff2-collection-800.woff2)

  @doc "Returns the retained source or generated variant's path. Call prepare! once before the suite."
  @spec path(String.t()) :: String.t()
  def path(name) do
    if name in @variants,
      do: Paths.project("tmp/font-corpus/" <> name),
      else: Paths.fixture("fonts/" <> name)
  end

  @doc "Returns the reviewed HTML fixture's relative URL for a corpus member."
  @spec asset_url(String.t()) :: String.t()
  def asset_url(name) do
    if name in @variants,
      do: "../../../../tmp/font-corpus/" <> name,
      else: "../../fonts/" <> name
  end

  @doc "Constructs the shared corpus once per test invocation, outside the checked-in fixtures."
  @spec prepare!() :: :ok
  def prepare! do
    File.mkdir_p!(Paths.project("tmp/font-corpus"))

    for name <- @variants do
      bytes = build(name)
      File.write!(path(name), bytes)
    end

    :ok
  end

  # Only fixture construction lives here. Admission and every assertion still
  # exercise the production validators against the separate reference manifests.
  defp build(name) do
    cond do
      name == "selected-collection.woff2" ->
        collection()

      String.starts_with?(name, "selected-woff2-collection-") ->
        prefix =
          name
          |> String.trim_leading("selected-woff2-collection-")
          |> String.trim_trailing(".woff2")
          |> String.to_integer()

        padded_collection(prefix)

      name in ~w(selected-large-seac.otf selected-random.otf) ->
        {scaler, tables} = source_tables("cff-static-sans.otf")
        cff = tables["CFF "]
        {programs, _} = index(cff, 104, 2)

        program =
          if name == "selected-random.otf" do
            # 600 80 0 rmoveto random 500 mul 0 rlineto 0 700
            # rlineto -400 0 rlineto endchar. Seed 7, source glyph ID 3.
            <<248, 236, 219, 139, 21, 12, 23, 248, 136, 12, 24, 139, 5, 139, 249, 80, 5, 252, 36,
              139, 5, 14>>
          else
            <<248, 236, 219, 139, 21>> <>
              :binary.copy(<<140, 139, 5, 138, 139, 5>>, 5500) <>
              <<139, 249, 80, 5, 239, 139, 5, 139, 253, 80, 5, 14>>
          end

        programs = List.replace_at(programs, 3, program)

        programs =
          if name == "selected-large-seac.otf",
            do: List.replace_at(programs, 2, <<248, 236, 169, 179, 211, 247, 12, 14>>),
            else: programs

        private =
          if name == "selected-random.otf",
            do: %{{12, 19} => [7], 20 => [0], 21 => [0]},
            else: %{20 => [0], 21 => [0]}

        cff =
          CffFixture.face_fixture(
            glyph_count: 5,
            names: ["StaticCffSans"],
            charset: binary_part(cff, 95, 9),
            charstrings: programs,
            local_subrs: nil,
            global_subrs: [],
            private: private,
            top: %{{12, 7} => [1 / 2048, 0, 0, 1 / 2048, 0, 0], 5 => [-1000, 0, 1400, 2000]}
          )

        pack(scaler, Map.put(tables, "CFF ", cff))

      name == "selected-large-cff2.otf" ->
        {scaler, tables} = source_tables("selected-variable-cff2.otf")
        cff = tables["CFF2"]
        {programs, _} = index(cff, 97, 4)

        program =
          <<219, 139, 21>> <> :binary.copy(<<140, 255, 8192::32, 5, 138, 255, 8192::32, 5>>, 5500)

        programs = programs |> List.replace_at(2, program) |> cff2_index()
        # Fixed-width DICT operands keep offsets independent of program length.
        store = binary_part(cff, 17, 80)
        charstrings_offset = 5 + 19 + 4 + byte_size(store)
        fd_offset = charstrings_offset + byte_size(programs)
        top = <<29, fd_offset::32, 12, 36, 29, charstrings_offset::32, 17, 29, 28::32, 24>>
        # CFF2 global subroutines are empty; FDArray contains one empty Private DICT.
        fd = cff2_index([<<139, 139, 18>>])

        cff =
          <<2, 0, 5, byte_size(top)::16, top::binary, 0::32, store::binary, programs::binary,
            fd::binary>>

        pack(scaler, Map.put(tables, "CFF2", cff))

      name == "selected-vertical-cff2.otf" ->
        {scaler, tables} = source_tables("selected-variable-cff2.otf")

        tables =
          Map.merge(tables, %{
            "VVAR" => @vertical_vvar,
            "VORG" => <<1::16, 0::16, 900::16, 1::16, 2::16, 950::16>>,
            "vhea" => <<0x10000::32, 800::signed-16, -200::signed-16, 0::size(208), 1::16>>,
            "vmtx" => <<1000::16, 100::16, 100::16, 100::16, 100::16, 100::16, 100::16>>
          })

        pack(scaler, tables)

      name == "selected-gasp-variable.ttf" ->
        {scaler, tables} = source_tables("selected-variable.ttf")

        pack(
          scaler,
          Map.merge(tables, %{
            "gasp" => <<1::16, 2::16, 12::16, 15::16, 65535::16, 15::16>>,
            "MVAR" => @gasp_mvar
          })
        )

      name == "selected-control-axes.ttf" ->
        {scaler, tables} = source_tables("selected-variable.ttf")

        axes =
          for {{tag, low, default, high}, i} <-
                Enum.with_index([
                  {"ital", 0, 0, 1},
                  {"slnt", -30, 0, 0},
                  {"opsz", 8, 12, 72},
                  {"TEST", 0, 0, 1}
                ]),
              into: <<>>,
              do:
                <<tag::binary, low * 65536::signed-32, default * 65536::signed-32,
                  high * 65536::signed-32, 0::16, 256 + i::16>>

        tables = Map.drop(tables, ["avar", "HVAR", "MVAR"])

        pack(
          scaler,
          Map.merge(tables, %{
            "fvar" => <<0x10000::32, 16::16, 2::16, 4::16, 20::16, 0::16, 20::16, axes::binary>>,
            "gvar" => @control_gvar
          })
        )

      name == "selected-colr-paints.ttf" ->
        paint_font()

      true ->
        {scaler, tables} = source_tables("selected-variable.ttf")

        tables =
          tables
          |> Map.drop(["HVAR", "MVAR", "avar", "fvar", "gvar"])
          |> Map.merge(%{"glyf" => @static_glyf, "name" => @static_name})

        tables =
          case name do
            "selected-sbix.ttf" ->
              Map.put(tables, "sbix", sbix())

            "selected-cbdt.ttf" ->
              Map.merge(tables, cbdt())

            "selected-bitmap-only.ttf" ->
              tables
              |> Map.drop(["glyf", "loca"])
              |> Map.put("maxp", <<0x5000::32, 6::16>>)
              |> Map.merge(cbdt())

            "selected-colr.ttf" ->
              Map.merge(tables, %{"CPAL" => @palette, "COLR" => @colr_layers})

            "selected-colr-v1.ttf" ->
              Map.merge(tables, %{"CPAL" => @palette, "COLR" => @colr_gradients})

            name ->
              manifest = Paths.fixture("fonts/color-fonts.json") |> File.read!() |> :json.decode()
              svg = manifest["files"]["selected-svg.ttf"]["svg"]

              svg =
                if name == "selected-svg-gzip.ttf",
                  do: replace(:zlib.gzip(svg), 8, <<2, 255>>),
                  else: svg

              Map.merge(tables, %{
                "CPAL" => @palette,
                "SVG " =>
                  <<0::16, 10::32, 0::32, 1::16, 2::16, 3::16, 14::32, byte_size(svg)::32,
                    svg::binary>>
              })
          end

        pack(scaler, tables)
    end
  end

  defp bitmap(ppem) do
    PngFixture.build(div(ppem, 2), ppem, 6, 8, 0, fn x, y ->
      if x < div(ppem, 4),
        do: [240, 40, 30, 255],
        else: [20, 70, 240, if(y >= div(ppem, 2), do: 128, else: 255)]
    end)
  end

  defp sbix do
    strikes =
      for ppem <- [20, 40] do
        glyph = <<0::16, 0::16, "png ", bitmap(ppem)::binary>>
        size = byte_size(glyph)

        offsets =
          for offset <- [32, 32, 32, 32 + size, 32 + 2 * size, 42 + 2 * size, 42 + 2 * size],
              into: <<>>,
              do: <<offset::32>>

        <<ppem::16, 72::16, offsets::binary, glyph::binary, glyph::binary, 1::signed-16,
          -2::signed-16, "dupe", 2::16>>
      end

    [small, large] = strikes
    <<1::16, 1::16, 2::32, 16::32, 16 + byte_size(small)::32, small::binary, large::binary>>
  end

  defp cbdt do
    {data, records} =
      Enum.map_reduce([20, 40], 4, fn ppem, offset ->
        png = bitmap(ppem)
        glyph = <<ppem, div(ppem, 2), 0, ppem, div(ppem, 2), byte_size(png)::32, png::binary>>
        size = byte_size(glyph)
        {{:binary.copy(glyph, 3), {ppem, offset, size}}, offset + 3 * size}
      end)
      |> elem(0)
      |> Enum.unzip()

    sizes =
      for {{ppem, _, _}, i} <- Enum.with_index(records),
          into: <<>>,
          do: <<104 + 32 * i::32, 32::32, 1::32, 0::size(224), 2::16, 4::16, ppem, ppem, 32, 1>>

    indices =
      for {_, offset, size} <- records,
          into: <<>>,
          do:
            <<2::16, 4::16, 8::32, 1::16, 17::16, offset::32, 0::32, size::32, size * 2::32,
              size * 3::32>>

    %{
      "CBDT" => <<3::16, 0::16, IO.iodata_to_binary(data)::binary>>,
      "CBLC" => <<3::16, 0::16, 2::32, sizes::binary, indices::binary>>
    }
  end

  defp paint_font do
    {scaler, tables} = source_tables("selected-variable.ttf")
    h = binary_part(tables["glyf"], 0, 50)
    glyphs = tables["glyf"] <> :binary.copy(h, 60)

    loca =
      for offset <- [0, 0, 0, 50, 96, 126] ++ Enum.to_list(152..3152//50),
          into: <<>>,
          do: <<div(offset, 2)::16>>

    post =
      binary_part(tables["post"], 0, 32) <>
        <<66::16>> <>
        for(sid <- [0, 3, 43, 91, 19] ++ Enum.to_list(258..318), into: <<>>, do: <<sid::16>>) <>
        for name <-
              ["composite"] ++
                Enum.map(1..32, &("paint" <> to_string(&1))) ++
                Enum.map(0..27, &("composite" <> to_string(&1))),
            into: <<>>,
            do: <<byte_size(name), name::binary>>

    hmtx =
      for(lsb <- [0, 0, 80, 70, 60, 0], into: <<>>, do: <<600::16, lsb::16>>) <>
        <<1000::16, 0::16, 0::size(944)>>

    hhea = replace(tables["hhea"], 10, <<1000::16>>) |> replace(34, <<7::16>>)

    tables =
      tables
      |> Map.drop(["HVAR", "MVAR"])
      |> Map.merge(%{
        "CPAL" => @paint_palette,
        "COLR" => @colr_paints,
        "OS/2" => replace(tables["OS/2"], 66, <<0xE11B::16>>),
        "cmap" => @paint_cmap,
        "glyf" => glyphs,
        "loca" => loca,
        "post" => post,
        "hmtx" => hmtx,
        "hhea" => hhea,
        "maxp" => replace(tables["maxp"], 4, <<66::16>>),
        "gvar" => <<1::16, 0::16, 2::16, 0::16, 154::32, 66::16, 0::16, 154::32, 0::size(1072)>>
      })

    pack(scaler, tables)
  end

  defp source_tables(name) do
    data = name |> path() |> File.read!()
    <<scaler::32, count::16, _::binary>> = data

    tables =
      for i <- 0..(count - 1), into: %{} do
        <<tag::binary-size(4), _checksum::32, offset::32, size::32>> =
          binary_part(data, 12 + i * 16, 16)

        {tag, binary_part(data, offset, size)}
      end

    {scaler, tables}
  end

  defp replace(data, offset, value) do
    <<prefix::binary-size(^offset), _::binary-size(byte_size(^value)), rest::binary>> = data
    prefix <> value <> rest
  end

  defp index(data, offset, count_size) do
    count = :binary.decode_unsigned(binary_part(data, offset, count_size))
    width = :binary.at(data, offset + count_size)

    offsets =
      for i <- 0..count,
          do:
            :binary.decode_unsigned(binary_part(data, offset + count_size + 1 + i * width, width))

    start = offset + count_size + 1 + (count + 1) * width

    objects =
      offsets
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.map(fn [a, b] -> binary_part(data, start + a - 1, b - a) end)

    {objects, start + List.last(offsets) - 1}
  end

  defp cff2_index(objects) do
    {offsets, last} =
      Enum.map_reduce(objects, 1, fn object, offset -> {offset, offset + byte_size(object)} end)

    positions = for offset <- offsets ++ [last], into: <<>>, do: <<offset::32>>
    <<length(objects)::32, 4, positions::binary, IO.iodata_to_binary(objects)::binary>>
  end

  # This intentionally simple sfnt encoder is independent of the production
  # rebuild path. The directory is sorted; FontTools' physical table ordering
  # is retained so unchanged inputs preserve their original PDF font bytes.
  defp pack(scaler, tables) do
    tables = Map.update!(tables, "head", &replace(&1, 8, <<0::32>>))

    priority =
      if Map.has_key?(tables, "CFF "),
        do: ~w(head hhea maxp OS/2 name cmap post CFF\  hmtx),
        else: ~w(head hhea maxp OS/2 hmtx cmap loca glyf name post gasp)

    tags =
      Enum.filter(priority, &Map.has_key?(tables, &1)) ++
        ((Map.keys(tables) -- priority) |> Enum.sort())

    count = map_size(tables)
    power = floor(:math.log2(count))

    {entries, _} =
      Enum.map_reduce(tags, 12 + count * 16, fn tag, offset ->
        bytes = Map.fetch!(tables, tag)
        {{tag, bytes, offset}, offset + byte_size(bytes) + Integer.mod(-byte_size(bytes), 4)}
      end)

    directory =
      entries
      |> Enum.sort_by(&elem(&1, 0))
      |> Enum.map(fn {tag, bytes, offset} ->
        <<tag::binary, checksum(bytes)::32, offset::32, byte_size(bytes)::32>>
      end)

    payloads =
      Enum.map(entries, fn {_, bytes, _} ->
        [bytes, :binary.copy(<<0>>, Integer.mod(-byte_size(bytes), 4))]
      end)

    font =
      IO.iodata_to_binary([
        <<scaler::32, count::16, (1 <<< power) * 16::16, power::16,
          count * 16 - (1 <<< power) * 16::16>>,
        directory,
        payloads
      ])

    {_, _, head} = Enum.find(entries, &(elem(&1, 0) == "head"))
    replace(font, head + 8, <<0xB1B0AFBA - checksum(font) &&& 0xFFFFFFFF::32>>)
  end

  defp checksum(bytes) do
    bytes = bytes <> :binary.copy(<<0>>, Integer.mod(-byte_size(bytes), 4))
    Enum.reduce(for(<<word::32 <- bytes>>, do: word), 0, &+/2) &&& 0xFFFFFFFF
  end

  defp collection do
    # Two distinct outline pairs and a repeated face. Literal table payloads
    # isolate collection admission from the separately tested glyf transforms.
    {faces, records} =
      Enum.map_reduce(~w(selected-container-sans.ttf selected-container-mono.ttf), [], fn name,
                                                                                          records ->
        {_, tables} = source_tables(name)

        Enum.map_reduce(Enum.sort(tables), records, fn {tag, _bytes} = record, records ->
          existing =
            if tag in ["glyf", "loca"], do: nil, else: Enum.find_index(records, &(&1 == record))

          if existing, do: {existing, records}, else: {length(records), records ++ [record]}
        end)
      end)

    directory =
      for {tag, bytes} <- records,
          into: <<>>,
          do:
            <<if(tag in ["glyf", "loca"], do: 255, else: 63), tag::binary,
              base128(byte_size(bytes))::binary>>

    payload = records |> Enum.map(&elem(&1, 1)) |> IO.iodata_to_binary()
    # RFC 7932 uncompressed metablock, WBITS=22, followed by the empty final block.
    compressed = <<0x80000B + ((byte_size(payload) - 1) <<< 7)::little-24, payload::binary, 3>>
    [first, second] = faces

    members =
      for face <- [first, second, first],
          into: <<>>,
          do: <<length(face), 0x10000::32, for(i <- face, into: <<>>, do: <<i>>)::binary>>

    collection = <<0x10000::32, 3, members::binary>>
    woff2(directory, collection, compressed, length(records))
  end

  defp padded_collection(prefix) do
    source = path("selected-transformed.woff2") |> File.read!()
    <<_::binary-size(12), count::16, _::binary-size(6), compressed_size::32, _::binary>> = source

    finish =
      Enum.reduce(1..count, 48, fn _, offset ->
        flags = :binary.at(source, offset)
        offset = offset + 1 + if((flags &&& 63) == 63, do: 4, else: 0)
        {_, offset} = read_base128(source, offset)
        transformed? = if (flags &&& 63) in [10, 11], do: flags >>> 6 == 0, else: flags >>> 6 != 0
        if transformed?, do: elem(read_base128(source, offset), 1), else: offset
      end)

    directory = :binary.copy(<<63, "ZZZZ", 0>>, prefix) <> binary_part(source, 48, finish - 48)

    indices =
      for i <- prefix..(prefix + count - 1), into: <<>> do
        cond do
          i < 506 -> <<255, i - 253>>
          i < 762 -> <<254, i - 506>>
          true -> <<253, i::16>>
        end
      end

    members = <<0x20000::32, 253, 1::16, 253, count::16, 0x10000::32, indices::binary>>
    woff2(directory, members, binary_part(source, finish, compressed_size), count + prefix)
  end

  defp woff2(directory, members, compressed, count) do
    size = 48 + byte_size(directory) + byte_size(members) + byte_size(compressed)

    <<"wOF2", "ttcf", size::32, count::16, 0::16, 10000::32, byte_size(compressed)::32, 1::16,
      0::16, 0::size(160), directory::binary, members::binary, compressed::binary>>
  end

  defp base128(value) do
    if value < 128,
      do: <<value>>,
      else:
        base128(value >>> 7)
        |> :binary.bin_to_list()
        |> List.update_at(-1, &(&1 ||| 128))
        |> :erlang.list_to_binary()
        |> Kernel.<>(<<value &&& 127>>)
  end

  defp read_base128(data, offset) do
    Enum.reduce_while(offset..(offset + 4), {0, offset}, fn i, {value, _} ->
      byte = :binary.at(data, i)
      result = {(value <<< 7) + (byte &&& 127), i + 1}
      if byte < 128, do: {:halt, result}, else: {:cont, result}
    end)
  end
end
