Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("font_fixture.ex"))

defmodule NativeElixirPdfUtilities.TestSupport.OpenTypeFixture do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.Font.Sfnt
  alias NativeElixirPdfUtilities.TestSupport.{FontCorpus, FontFixture}

  @doc "Assembles a test-owned table with offsets measured from its fixed header."
  @spec join(([non_neg_integer()] -> binary()), [binary() | nil]) :: binary()
  def join(header, children) do
    start = byte_size(header.(List.duplicate(0, length(children))))

    {offsets, _} =
      Enum.map_reduce(children, start, fn child, offset ->
        if child == nil, do: {0, offset}, else: {offset, offset + byte_size(child)}
      end)

    header.(offsets) <> IO.iodata_to_binary(Enum.reject(children, &is_nil/1))
  end

  @doc "Encodes test-owned unsigned words."
  @spec words([integer()]) :: binary()
  def words(values), do: for(value <- values, into: <<>>, do: <<value::16>>)

  @doc "Encodes an explicit Coverage table with stable consecutive indices."
  @spec coverage([non_neg_integer()]) :: binary()
  def coverage(glyphs), do: words([1, length(glyphs) | glyphs])

  @doc "Builds a layout table from small authored lookups and script/language selections."
  @spec layout([map()], keyword()) :: binary()
  def layout(lookups, options \\ []) do
    features =
      Keyword.get(options, :features, [{"test", Enum.to_list(0..(length(lookups) - 1)//1)}])

    language =
      words(
        [0, Keyword.get(options, :required, 65535), length(features)] ++
          Enum.to_list(0..(length(features) - 1)//1)
      )

    script =
      join(
        fn [_default, _language] = offsets ->
          case Keyword.get(options, :language) do
            nil -> words([hd(offsets), 0])
            tag -> words([0, 1]) <> tag <> words([List.last(offsets)])
          end
        end,
        [
          if(Keyword.get(options, :language) == nil, do: language),
          if(Keyword.get(options, :language) != nil, do: language)
        ]
      )

    scripts =
      <<1::16, Keyword.get(options, :script, "DFLT")::binary-size(4), 8::16, script::binary>>

    feature_tables =
      Enum.map(features, fn {_tag, indices} -> words([0, length(indices) | indices]) end)

    feature_list =
      join(
        fn offsets ->
          words([length(features)]) <>
            (Enum.zip(features, offsets)
             |> Enum.map(fn {{tag, _}, offset} -> tag <> words([offset]) end)
             |> IO.iodata_to_binary())
        end,
        feature_tables
      )

    lookups =
      Enum.map(lookups, fn lookup ->
        subtables = lookup.subtables
        flags = Map.get(lookup, :flags, 0)

        join(
          fn offsets ->
            words(
              [lookup.type, flags, length(subtables)] ++
                offsets ++
                if(Bitwise.band(flags, 16) != 0, do: [Map.get(lookup, :mark_set, 0)], else: [])
            )
          end,
          subtables
        )
      end)

    lookup_list = join(fn offsets -> words([length(lookups) | offsets]) end, lookups)

    if Keyword.has_key?(options, :variations) do
      join(
        fn [scripts, features, lookups, variations] ->
          words([1, 1, scripts, features, lookups]) <> <<variations::32>>
        end,
        [scripts, feature_list, lookup_list, options[:variations]]
      )
    else
      join(fn [scripts, features, lookups] -> words([1, 0, scripts, features, lookups]) end, [
        scripts,
        feature_list,
        lookup_list
      ])
    end
  end

  @doc "Creates a minimal selected metric face for lookup arithmetic without storing a font file."
  @spec font([map()], [map()], keyword()) :: map()
  def font(substitutions \\ [], positions \\ [], options \\ []) do
    tables = [{"head", <<0::96>>}, {"maxp", <<0x10000::32, 64::16>>}]

    tables =
      if substitutions == [],
        do: tables,
        else: [{"GSUB", layout(substitutions, options)} | tables]

    position_options =
      if Keyword.has_key?(options, :gpos_features),
        do: Keyword.put(options, :features, options[:gpos_features]),
        else: options

    tables =
      if positions == [],
        do: tables,
        else: [{"GPOS", layout(positions, position_options)} | tables]

    tables =
      if Keyword.has_key?(options, :gdef), do: [{"GDEF", options[:gdef]} | tables], else: tables

    %{
      type: :embedded,
      id: "authored-layout",
      family: "Authored layout",
      data: Sfnt.rebuild(0x10000, Enum.sort(tables)),
      cmap: Map.new(Enum.with_index(?A..?Z, 1)),
      units_per_em: 1000,
      widths: List.duplicate(500, 64),
      default_width: 500,
      pdf_name: "AuthoredLayout",
      ascent: 800,
      descent: -200,
      bbox: {0, -200, 1000, 800}
    }
  end

  @doc "Builds an outlined font in memory for independent shaping or browser checks using the existing tiny corpus."
  @spec outline_font([map()], [map()], keyword()) :: binary()
  def outline_font(substitutions, positions, options \\ []) do
    source = File.read!(FontCorpus.path("selected-variable.ttf"))
    {_, tables} = FontFixture.tables(source)
    {_, layouts} = font(substitutions, positions, options).data |> FontFixture.tables()
    <<format::signed-16>> = binary_part(tables["head"], 50, 2)

    locations =
      if format == 0,
        do: for(<<offset::16 <- tables["loca"]>>, do: offset * 2),
        else: for(<<offset::32 <- tables["loca"]>>, do: offset)

    [first, last] =
      locations |> Enum.chunk_every(2, 1, :discard) |> Enum.find(fn [a, b] -> b > a end)

    glyph = binary_part(tables["glyf"], first, last - first)
    cmap = Keyword.get(options, :cmap, Map.new(Enum.with_index(?A..?Z, 1)))
    widths = Keyword.get(options, :widths, %{})
    space = Map.get(cmap, 32)

    glyphs = for id <- 0..63, do: if(id == 0 or id == space, do: <<>>, else: glyph)
    {locations, finish} = Enum.map_reduce(glyphs, 0, &{&2, &2 + byte_size(&1)})

    groups =
      cmap
      |> Enum.sort()
      |> Enum.map(fn {codepoint, id} -> <<codepoint::32, codepoint::32, id::32>> end)
      |> IO.iodata_to_binary()

    cmap =
      <<0::16, 1::16, 3::16, 10::16, 12::32, 12::16, 0::16, 16 + byte_size(groups)::32, 0::32,
        map_size(cmap)::32, groups::binary>>

    head =
      tables["head"]
      |> FontFixture.replace(18, <<1000::16>>)
      |> FontFixture.replace(50, <<1::signed-16>>)

    hhea =
      tables["hhea"]
      |> FontFixture.replace(4, <<800::signed-16, -200::signed-16, 0::16>>)
      |> FontFixture.replace(10, <<500::16>>)
      |> FontFixture.replace(34, <<64::16>>)

    os2 =
      tables["OS/2"]
      |> FontFixture.replace(62, <<64::16>>)
      |> FontFixture.replace(68, <<800::signed-16, -200::signed-16, 0::16, 800::16, 200::16>>)

    changes =
      Map.new(~w(fvar avar gvar HVAR VVAR MVAR cvar STAT MATH), &{&1, nil})
      |> Map.merge(%{
        "head" => head,
        "hhea" => hhea,
        "OS/2" => os2,
        "cmap" => cmap,
        "hmtx" =>
          for(id <- 0..63, into: <<>>, do: <<Map.get(widths, id, 500)::16, 0::signed-16>>),
        "loca" => for(offset <- locations ++ [finish], into: <<>>, do: <<offset::32>>),
        "glyf" => IO.iodata_to_binary(glyphs),
        "maxp" => FontFixture.replace(tables["maxp"], 4, <<64::16>>),
        "post" => <<0x30000::32, binary_part(tables["post"], 4, 28)::binary>>,
        "GSUB" => layouts["GSUB"],
        "GPOS" => layouts["GPOS"],
        "GDEF" => layouts["GDEF"]
      })

    FontFixture.rebuild(source, changes)
  end

  @doc "Builds authored vertical advances, bearings, alternate glyphs and vertical kerning in memory."
  @spec vertical_font() :: binary()
  def vertical_font do
    single = fn from, to ->
      join(fn [covered] -> words([2, covered, 1, to]) end, [coverage([from])])
    end

    pair =
      join(
        fn [covered, set] -> words([1, covered, 8, 0, 1, set]) end,
        [coverage([4]), words([1, 2, 80])]
      )

    source =
      outline_font(
        [
          %{type: 1, subtables: [single.(1, 4)]},
          %{type: 1, subtables: [single.(4, 5)]}
        ],
        [%{type: 2, subtables: [pair]}],
        features: [{"vert", [0]}, {"vrt2", [1]}],
        gpos_features: [{"vkrn", [0]}],
        widths: %{1 => 600, 2 => 800, 4 => 1000}
      )

    header =
      <<0x11000::32, 500::signed-16, -500::signed-16, 0::signed-16, 1200::16, 30::signed-16,
        0::signed-16, 800::signed-16, 0::signed-16, 1::signed-16, 0::signed-16, 0::64,
        0::signed-16, 3::16>>

    metrics =
      <<1100::16, 100::signed-16, 900::16, 30::signed-16, 1200::16, 70::signed-16>> <>
        :binary.copy(<<50::signed-16>>, 61)

    FontFixture.rebuild(source, %{"vhea" => header, "vmtx" => metrics})
  end

  @doc "Builds a context-dependent vertical alternate and its nominal fallback without vertical metric tables."
  @spec contextual_vertical_font() :: binary()
  def contextual_vertical_font do
    context =
      join(
        fn [first, following] -> words([3, 2, 1, first, following, 0, 1]) end,
        [coverage([1]), coverage([1])]
      )

    single = join(fn [covered] -> words([2, covered, 1, 4]) end, [coverage([1])])

    outline_font([%{type: 5, subtables: [context]}, %{type: 1, subtables: [single]}], [],
      features: [{"vert", [0]}],
      cmap: %{0xFF08 => 1, 32 => 63},
      widths: %{1 => 600, 4 => 1000}
    )
  end

  @doc "Builds Syriac contextual forms with distinct authored advances, using no additional font asset."
  @spec syriac_font() :: binary()
  def syriac_font do
    tags = ~w(isol fina fin2 fin3 medi med2 init)

    substitutions =
      tags
      |> Enum.with_index(1)
      |> Enum.map(fn {_tag, form} ->
        subtable =
          join(fn [offset] -> words([2, offset, 7] ++ Enum.map(1..7, &(&1 + form * 7))) end, [
            coverage(Enum.to_list(1..7))
          ])

        %{type: 1, subtables: [subtable]}
      end)

    cmap = %{
      0x710 => 1,
      0x712 => 2,
      0x717 => 3,
      0x715 => 4,
      0x72A => 5,
      0x716 => 6,
      0x72F => 7,
      0x730 => 58,
      32 => 59,
      0x200D => 60,
      0x200C => 61
    }

    widths =
      Map.new(1..56, &{&1, 400 + &1 * 3}) |> Map.merge(%{58 => 0, 59 => 200, 60 => 0, 61 => 0})

    outline_font(substitutions, [],
      script: "syrc",
      cmap: cmap,
      widths: widths,
      features:
        Enum.with_index(tags) |> Enum.map(fn {tag, index} -> {tag, [index]} end) |> Enum.sort()
    )
  end

  @doc "Builds a compact Indic face with authored basic features, advances and positioning records in memory."
  @spec indic_font() :: binary()
  def indic_font do
    ligature = fn first, rest, result ->
      set =
        join(fn [offset] -> words([1, offset]) end, [
          words([result, length(rest) + 1 | rest])
        ])

      join(fn [coverage, set] -> words([1, coverage, 1, set]) end, [
        coverage([first]),
        set
      ])
    end

    lookups =
      for {first, rest, result} <- [
            {1, [2, 3], 20},
            {5, [2], 21},
            {1, [2], 22},
            {6, [2], 23},
            {2, [5], 24}
          ],
          do: %{type: 4, subtables: [ligature.(first, rest, result)]}

    positions =
      for {glyph, x, y} <- [{4, 20, -30}, {21, -350, 700}] do
        join(fn [coverage] -> words([1, coverage, 3, x, y]) end, [
          coverage([glyph])
        ])
      end

    outline_font(lookups, [%{type: 1, subtables: positions}],
      script: "dev2",
      features: [{"akhn", [0]}, {"blwf", [4]}, {"half", [2, 3]}, {"rphf", [1]}],
      gpos_features: [{"dist", [0]}],
      cmap: %{
        32 => 63,
        0x915 => 1,
        0x94D => 2,
        0x937 => 3,
        0x93F => 4,
        0x930 => 5,
        0x928 => 6,
        0x200D => 7,
        0x200C => 8,
        0x93E => 9,
        0x902 => 10,
        0x903 => 11
      },
      widths: %{
        2 => 0,
        4 => 200,
        7 => 0,
        8 => 0,
        21 => 0,
        22 => 300,
        23 => 300,
        24 => 0,
        63 => 250
      }
    )
  end
end
