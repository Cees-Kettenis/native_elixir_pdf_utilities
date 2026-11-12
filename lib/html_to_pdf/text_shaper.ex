defmodule NativeElixirPdfUtilities.HtmlToPdf.TextShaper do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.{
    GlyphSequence,
    GlyphNormalization,
    IndicShaper,
    OpenTypeLookup,
    UnicodeTextData
  }

  alias NativeElixirPdfUtilities.Validators.{
    EmojiPresentationValidator,
    FontValidator,
    TextShapingValidator
  }

  @defaults Map.new(
              ~w(rvrn ccmp locl rlig calt liga clig kern mark mkmk curs dist abvm blwm),
              &{&1, 1}
            )
  # Registered form order: https://learn.microsoft.com/en-us/typography/script-development/syriac#features
  @joining_features ~w(isol fina fin2 fin3 medi med2 init)
  @language_tags %{
    "ar" => "ARA ",
    "fa" => "FAR ",
    "ur" => "URD ",
    "he" => "IWR ",
    "tr" => "TRK ",
    "az" => "AZE ",
    "sr" => "SRB ",
    "bg" => "BGR ",
    "mk" => "MKD ",
    "ro" => "ROM ",
    "nl" => "NLD ",
    "de" => "DEU ",
    "en" => "ENG ",
    "fr" => "FRA ",
    "es" => "ESP ",
    "hi" => "HIN ",
    "bn" => "BEN ",
    "pa" => "PAN ",
    "gu" => "GUJ ",
    "ta" => "TAM ",
    "te" => "TEL ",
    "kn" => "KAN ",
    "ml" => "MAL ",
    "si" => "SNH ",
    "th" => "THA ",
    "ja" => "JAN ",
    "ko" => "KOR ",
    "zh-hant" => "ZHT ",
    "zh-hans" => "ZHS ",
    "zh" => "ZHS "
  }
  @script_tags %{
    "Hira" => ["kana"],
    "Kana" => ["kana"],
    "Laoo" => ["lao "],
    "Nkoo" => ["nko "],
    "Vaii" => ["vai "],
    "Yiii" => ["yi  "],
    "Deva" => ["dev2", "deva"],
    "Beng" => ["bng2", "beng"],
    "Guru" => ["gur2", "guru"],
    "Gujr" => ["gjr2", "gujr"],
    "Orya" => ["ory2", "orya"],
    "Taml" => ["tml2", "taml"],
    "Telu" => ["tel2", "telu"],
    "Knda" => ["knd2", "knda"],
    "Mlym" => ["mlm2", "mlym"],
    "Mymr" => ["mym2", "mymr"]
  }

  @doc "Returns OpenType script candidates, including registered compatibility versions."
  @spec script_tags(String.t()) :: [String.t()]
  def script_tags(script), do: Map.get(@script_tags, script, [String.downcase(script)])

  @doc "Returns an available OpenType language-system tag for an admitted language and its fallback subtags."
  @spec language_tag(String.t()) :: String.t() | nil
  def language_tag(language) do
    parts = String.split(language, "-")
    tags = [language, Enum.take(parts, 2) |> Enum.join("-"), hd(parts)]
    Enum.find_value(tags, &Map.get(@language_tags, &1))
  end

  @doc "Shapes source text into positioned glyphs with logical source clusters in design units."
  @spec shape(term(), term(), keyword()) :: {:ok, map()} | {:error, {atom(), map()}}
  def shape(text, font, options \\ []) do
    FontValidator.with_budget(fn ->
      with {:ok, prepared} <- TextShapingValidator.prepare(text, font, options),
           {:ok, vertical_origins} <-
             if(prepared.direction == :ttb,
               do:
                 NativeElixirPdfUtilities.Validators.OpenTypeVerticalValidator.origins(
                   font,
                   prepared.layout.vertical
                 ),
               else: {:ok, nil}
             ) do
        widths = Map.get(font, :bitmap_widths) || List.to_tuple(font.widths)

        runs =
          Enum.map(prepared.itemization.runs, fn run ->
            clusters =
              Enum.slice(
                prepared.itemization.clusters,
                run.cluster_start,
                run.cluster_end - run.cluster_start
              )

            script = prepared.script || run.script
            glyphs = initial_glyphs(clusters, font, prepared.layout.sequences)

            direction =
              if prepared.direction == :auto do
                strong =
                  Enum.find_value(glyphs, fn node ->
                    case elem(UnicodeTextData.properties(hd(node.codepoints)), 1) do
                      :L -> :ltr
                      value when value in [:R, :AL] -> :rtl
                      _ -> nil
                    end
                  end)

                strong || :ltr
              else
                prepared.direction
              end

            state = %{
              sequence: GlyphSequence.new(glyphs),
              gdef: prepared.layout.gdef,
              table: :gsub,
              lookups: {},
              features: %{},
              ppem: prepared.ppem,
              units: font.units_per_em,
              direction: direction
            }

            features = Map.merge(@defaults, prepared.features)

            features =
              if IndicShaper.script?(script),
                do: Map.merge(IndicShaper.features(), features),
                else: features

            features =
              if script in ["Arab", "Syrc", "Mong", "Nkoo"],
                do: Map.merge(Map.new(@joining_features, &{&1, 1}), features),
                else: features

            features =
              if direction == :ttb,
                do:
                  features
                  |> Map.merge(%{
                    "vert" => 1,
                    "kern" => 0,
                    "vkrn" => Map.get(features, "kern", 1)
                  })
                  |> Map.merge(prepared.features),
                else: features

            gsub = select(prepared.layout.gsub, script, run.language, prepared, features)

            joining_stages =
              if script in ["Arab", "Syrc", "Mong", "Nkoo"],
                do: Enum.map(@joining_features, &[&1]),
                else: []

            stages =
              [~w(rvrn), ~w(ccmp locl)] ++
                joining_stages ++
                [~w(rlig), ~w(calt), ~w(liga clig)]

            staged = List.flatten(stages)

            selected_tags =
              gsub.selected |> Map.values() |> Enum.flat_map(&Map.keys/1) |> Enum.uniq()

            stages = stages ++ [selected_tags -- staged]

            state =
              if IndicShaper.script?(script) do
                IndicShaper.shape(state, gsub, script)
              else
                Enum.reduce(stages, state, fn tags, state ->
                  OpenTypeLookup.execute(state, gsub, tags)
                end)
              end

            sequence =
              Enum.reduce(GlyphSequence.to_list(state.sequence), state.sequence, fn glyph,
                                                                                    sequence ->
                width =
                  if glyph.glyph < tuple_size(widths),
                    do: elem(widths, glyph.glyph),
                    else: font.default_width

                # Indic fonts provide advances for syllable signs themselves.
                # Other shaping models use the ordinary nonspacing-mark rule.
                zero? = glyph.ignorable? or (glyph.mark? and not IndicShaper.script?(script))

                vertical_advance =
                  if prepared.layout.vertical.metrics == nil,
                    do: font.ascent - font.descent,
                    else: elem(elem(prepared.layout.vertical.metrics, glyph.glyph), 0)

                origin =
                  if vertical_origins == nil,
                    do: font.ascent,
                    else: elem(vertical_origins, glyph.glyph)

                GlyphSequence.put(sequence, glyph.id, %{
                  x_advance: if(direction == :ttb or zero?, do: 0, else: width),
                  y_advance: if(direction == :ttb and not zero?, do: -vertical_advance, else: 0),
                  x_offset: if(direction == :ttb, do: -div(width, 2), else: 0),
                  y_offset: if(direction == :ttb, do: -origin, else: 0)
                })
              end)

            state = %{state | sequence: sequence, table: :gpos}
            gpos = select(prepared.layout.gpos, script, run.language, prepared, features)

            state =
              OpenTypeLookup.execute(
                state,
                gpos,
                gpos.selected |> Map.values() |> Enum.flat_map(&Map.keys/1)
              )

            state =
              if direction != :ttb and Map.get(features, "kern", 0) != 0 and
                   not Enum.any?(gpos.selected, fn {_index, tags} ->
                     Map.has_key?(tags, "kern")
                   end), do: legacy_kern(state, Map.get(font, :kerning, %{})), else: state

            glyphs = resolve_positions(state)

            %{run | script: script}
            |> Map.merge(%{
              direction: direction,
              glyphs: glyphs,
              advance: Enum.reduce(glyphs, 0, &(&1.x_advance + &2)),
              vertical_advance: Enum.reduce(glyphs, 0, &(&1.y_advance + &2))
            })
          end)

        {:ok,
         %{
           text: prepared.itemization.text,
           clusters: prepared.itemization.clusters,
           runs: runs,
           glyphs: Enum.flat_map(runs, & &1.glyphs),
           units_per_em: font.units_per_em,
           vertical_advance: Enum.reduce(runs, 0, &(&1.vertical_advance + &2)),
           advance: Enum.reduce(runs, 0, &(&1.advance + &2))
         }}
      end
    end)
  catch
    {:invalid_shaping, error} -> error
  end

  @doc "Returns the default feature policy shared by shaping and ASCII fast-path admission."
  @spec default_features() :: %{binary() => non_neg_integer()}
  def default_features, do: @defaults

  @doc "Returns byte boundaries outside every overlapping grapheme and substituted glyph cluster."
  @spec cluster_boundaries(map()) :: [non_neg_integer()]
  def cluster_boundaries(shaped) do
    FontValidator.with_budget(fn ->
      count = length(shaped.clusters) + length(shaped.glyphs)

      FontValidator.reserve(
        :max_work,
        count * max(1, ceil(:math.log2(max(count, 1))))
      )

      intervals =
        Enum.map(shaped.clusters, &{&1.start, &1.end}) ++
          Enum.map(shaped.glyphs, &{&1.cluster_start, &1.cluster_end})

      merged =
        intervals
        |> Enum.sort()
        |> Enum.reduce([], fn {first, last}, merged ->
          case merged do
            [{previous_first, previous_last} | rest] when first < previous_last ->
              [{previous_first, max(previous_last, last)} | rest]

            _ ->
              [{first, last} | merged]
          end
        end)

      [0 | merged |> Enum.reverse() |> Enum.map(&elem(&1, 1))]
    end)
  end

  defp initial_glyphs(clusters, font, sequences) do
    glyphs =
      Enum.flat_map(clusters, fn cluster ->
        GlyphNormalization.prepare(cluster, font)
        |> Enum.map(fn %{codepoint: codepoint, properties: properties, fragments: fragments} ->
          %{
            glyph: Map.get(font.cmap, codepoint, 0),
            codepoints: [codepoint],
            cluster_start: cluster.start,
            cluster_end: cluster.end,
            scalar_start: cluster.scalar_start,
            scalar_end: cluster.scalar_end,
            fragments: fragments,
            syllabic_category: elem(properties, 5),
            positional_category: elem(properties, 6),
            mark?: elem(properties, 2) in [:Mn, :Me],
            joining: elem(properties, 4),
            ignorable?: TextShapingValidator.ignorable?(codepoint),
            forms: MapSet.new(),
            components: 1,
            component: 0,
            ligature: nil,
            x_advance: 0,
            y_advance: 0,
            x_offset: 0,
            y_offset: 0,
            attachment: nil
          }
        end)
      end)

    glyphs =
      Enum.reduce(glyphs, [], fn glyph, previous ->
        codepoint = hd(glyph.codepoints)

        if codepoint in 0xFE00..0xFE0F or codepoint in 0xE0100..0xE01EF or
             (codepoint in [0x180B, 0x180C, 0x180D, 0x180F] and Map.has_key?(sequences, codepoint)) do
          case previous do
            [base | rest] ->
              selected = Map.get(sequences, codepoint, %{nondefaults: %{}})
              glyph_id = Map.get(selected.nondefaults, hd(base.codepoints), base.glyph)

              [
                %{
                  base
                  | glyph: glyph_id,
                    codepoints: base.codepoints ++ glyph.codepoints,
                    fragments: base.fragments ++ glyph.fragments,
                    cluster_end: max(base.cluster_end, glyph.cluster_end),
                    scalar_end: max(base.scalar_end, glyph.scalar_end)
                }
                | rest
              ]

            [] ->
              [glyph]
          end
        else
          [glyph | previous]
        end
      end)
      |> Enum.reverse()

    glyphs =
      Enum.map(glyphs, fn glyph ->
        case glyph.codepoints do
          [codepoint] ->
            selector =
              case EmojiPresentationValidator.preference(<<codepoint::utf8>>, font) do
                :text -> 0xFE0E
                :emoji -> 0xFE0F
                :normal -> nil
              end

            variants = Map.get(sequences, selector, %{nondefaults: %{}})
            %{glyph | glyph: Map.get(variants.nondefaults, codepoint, glyph.glyph)}

          _ ->
            glyph
        end
      end)

    joining_forms(glyphs)
  end

  defp joining_forms(glyphs) do
    {following, _} =
      glyphs
      |> Enum.reverse()
      |> Enum.map_reduce(nil, fn glyph, next ->
        {next, if(glyph.joining == :T, do: next, else: glyph)}
      end)

    Enum.zip(glyphs, Enum.reverse(following))
    |> Enum.map_reduce(nil, fn {glyph, next}, previous ->
      before_type = if previous != nil, do: previous.joining
      after_type = if next != nil, do: next.joining
      codepoint = hd(glyph.codepoints)
      left? = before_type in [:D, :L, :C] and glyph.joining in [:D, :R, :C]
      right? = glyph.joining in [:D, :L, :C] and after_type in [:D, :R, :C]

      # Unicode 17 Syriac joining groups and the registered OpenType Alaph
      # forms distinguish a connected medial, two unconnected finals and an
      # isolated form before another base. Transparent marks retain no form.
      form =
        cond do
          glyph.joining == :T and codepoint in [0x180B, 0x180C, 0x180D, 0x180F] and
              previous != nil ->
            previous.forms

          glyph.joining in [:U, :T] ->
            MapSet.new()

          codepoint == 0x710 ->
            medial? = after_type in [:R, :D, :C]
            previous_codepoint = if previous != nil, do: hd(previous.codepoints)

            cond do
              left? and medial? -> "med2"
              left? -> "fina"
              medial? -> "isol"
              previous_codepoint in [0x715, 0x716, 0x72A, 0x72F] -> "fin3"
              before_type == :R -> "fin2"
              true -> "isol"
            end

          left? and right? ->
            "medi"

          left? ->
            "fina"

          right? ->
            "init"

          true ->
            "isol"
        end

      forms = if is_binary(form), do: MapSet.new([form]), else: form
      glyph = %{glyph | forms: forms}
      {glyph, if(glyph.joining == :T, do: previous, else: glyph)}
    end)
    |> elem(0)
  end

  defp select(table, script, language, prepared, enabled) do
    if table == nil do
      %{lookups: {}, selected: %{}}
    else
      scripts = script_tags(script) ++ ["DFLT", "latn"]
      system = Enum.find_value(scripts, &Map.get(table.scripts, &1))

      language_tag =
        if prepared.language_override == :normal,
          do: language_tag(language),
          else: prepared.language_override

      language = if system != nil, do: Map.get(system.languages, language_tag) || system.default

      selected =
        if language == nil do
          %{}
        else
          required = language.required

          indices =
            if required == nil, do: language.features, else: [required | language.features]

          variation =
            Enum.find(table.variations, fn variation ->
              Enum.all?(variation.conditions, fn {axis, low, high} ->
                value = Enum.at(prepared.layout.coordinates, axis)
                value >= low and value <= high
              end)
            end)

          Enum.reduce(indices, %{}, fn index, lookups ->
            feature = elem(table.features, index)

            value =
              if index == required,
                do: max(1, Map.get(enabled, feature.tag, 1)),
                else: Map.get(enabled, feature.tag, 0)

            indices =
              if variation == nil,
                do: feature.lookups,
                else: Map.get(variation.substitutions, index, feature.lookups)

            if value == 0 do
              lookups
            else
              Enum.reduce(indices, lookups, fn lookup, lookups ->
                Map.update(
                  lookups,
                  lookup,
                  %{feature.tag => value},
                  &Map.put(&1, feature.tag, value)
                )
              end)
            end
          end)
        end

      %{lookups: table.lookups, selected: selected}
    end
  end

  defp legacy_kern(state, kerning) do
    {sequence, _} =
      GlyphSequence.to_list(state.sequence)
      |> Enum.reduce({state.sequence, nil}, fn glyph, {sequence, previous} ->
        if previous == nil do
          {sequence, glyph}
        else
          amount = Map.get(kerning, {previous.glyph, glyph.glyph}, 0)

          {GlyphSequence.put(sequence, previous.id, %{x_advance: previous.x_advance + amount}),
           glyph}
        end
      end)

    %{state | sequence: sequence}
  end

  defp resolve_positions(state) do
    glyphs = GlyphSequence.to_list(state.sequence)
    total = Enum.reduce(glyphs, 0, &(&1.x_advance + &2))

    {origins, _} =
      Enum.map_reduce(glyphs, {0, 0}, fn glyph, {x, y} ->
        origin = if state.direction == :rtl, do: {total - x - glyph.x_advance, y}, else: {x, y}
        {{glyph.id, origin}, {x + glyph.x_advance, y + glyph.y_advance}}
      end)

    origins = Map.new(origins)

    {resolved, _} =
      Enum.map_reduce(glyphs, %{}, fn glyph, cache ->
        cache = resolve_attachment(state.sequence.nodes, origins, glyph.id, cache, %{})
        {Map.merge(glyph, Map.fetch!(cache, glyph.id)), cache}
      end)

    resolved
  end

  @spec resolve_attachment(map(), map(), non_neg_integer(), map(), map()) :: map()
  defp resolve_attachment(nodes, origins, id, cache, path) do
    FontValidator.reserve(:max_work, 1)

    case Map.fetch(cache, id) do
      {:ok, _} ->
        cache

      :error ->
        glyph = Map.fetch!(nodes, id)

        {offsets, cache} =
          if glyph.attachment == nil do
            {%{x_offset: glyph.x_offset, y_offset: glyph.y_offset}, cache}
          else
            attachment = glyph.attachment
            path = Map.put(path, id, true)
            TextShapingValidator.validate_attachment(attachment.parent, path)
            cache = resolve_attachment(nodes, origins, attachment.parent, cache, path)
            parent = Map.fetch!(cache, attachment.parent)
            {parent_x, parent_y} = Map.fetch!(origins, attachment.parent)
            {x, y} = Map.fetch!(origins, id)

            x_offset =
              if attachment.axes == :y,
                do: glyph.x_offset,
                else: glyph.x_offset + parent.x_offset + parent_x - x + attachment.dx

            {%{
               x_offset: x_offset,
               y_offset: glyph.y_offset + parent.y_offset + parent_y - y + attachment.dy
             }, cache}
          end

        Map.put(cache, id, offsets)
    end
  end
end
