defmodule NativeElixirPdfUtilities.Validators.OpenTypeAsciiValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.{FontValidator, OpenTypeLayoutValidator}
  alias NativeElixirPdfUtilities.HtmlToPdf.OpenTypeLookup

  @doc "Finds ASCII glyphs whose default OpenType behavior needs general shaping rather than legacy Latin metrics."
  @spec prepare(map()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(font) do
    FontValidator.with_budget(fn ->
      with {:ok, language} <-
             NativeElixirPdfUtilities.Validators.TextItemizationValidator.prepare_language(
               Map.get(font, :shaping_language, "und")
             ),
           {:ok, layout} <- OpenTypeLayoutValidator.prepare(font) do
        ascii_glyphs = for cp <- 32..126, glyph = Map.get(font.cmap, cp), glyph != nil, do: glyph
        FontValidator.reserve(:max_work, 95 + 2 * length(ascii_glyphs))
        ascii = MapSet.new(ascii_glyphs)
        multiplicity = Enum.frequencies(ascii_glyphs)

        ligatures =
          for {text, codepoint} <- Map.get(font, :latin_ligatures, %{}),
              inputs = Enum.map(String.to_charlist(text), &Map.get(font.cmap, &1)),
              output = Map.get(font.cmap, codepoint),
              output != nil and Enum.all?(inputs, &(&1 != nil)),
              do: {inputs, output}

        possible = Enum.reduce(ligatures, ascii, fn {_, glyph}, set -> MapSet.put(set, glyph) end)

        flags =
          Map.new([{"Latn", ["latn", "DFLT"]}, {"Zyyy", ["DFLT", "latn"]}], fn {script, tags} ->
            flags =
              Enum.reduce([:gsub, :gpos], %{}, fn kind, flags ->
                table = layout[kind]

                if table == nil do
                  flags
                else
                  system = Enum.find_value(tags, &Map.get(table.scripts, &1))

                  tag =
                    case Map.get(Map.get(font, :feature_plan, %{}), :language, :normal) do
                      :normal ->
                        NativeElixirPdfUtilities.HtmlToPdf.TextShaper.language_tag(language)

                      tag ->
                        tag
                    end

                  language =
                    if system != nil, do: Map.get(system.languages, tag) || system.default

                  case language do
                    nil ->
                      flags

                    language ->
                      indices =
                        Enum.uniq(
                          language.features ++
                            if(language.required == nil, do: [], else: [language.required])
                        )

                      kern_indices =
                        if kind == :gpos,
                          do: Enum.filter(indices, &(elem(table.features, &1).tag == "kern")),
                          else: []

                      kern_lookups =
                        kern_indices
                        |> Enum.flat_map(&elem(table.features, &1).lookups)
                        |> Enum.uniq()
                        |> Enum.sort()

                      kern_roots =
                        cond do
                          kern_indices == [] or kern_lookups == [] ->
                            MapSet.new()

                          language.required in kern_indices ->
                            possible

                          true ->
                            kerning_roots(
                              table,
                              kern_lookups,
                              possible,
                              Map.get(font, :kerning, %{}),
                              layout.gdef
                            )
                        end

                      Enum.reduce(indices, flags, fn index, flags ->
                        feature = elem(table.features, index)

                        key =
                          if language.required == index,
                            do: {:required, feature.tag},
                            else: feature.tag

                        roots =
                          if kind == :gpos and feature.tag == "kern" do
                            kern_roots
                          else
                            Enum.reduce(feature.lookups, MapSet.new(), fn index, roots ->
                              lookup = elem(table.lookups, index)

                              Enum.reduce(lookup.subtables, roots, fn subtable, roots ->
                                MapSet.union(
                                  roots,
                                  unsafe_roots(
                                    subtable,
                                    kind,
                                    feature.tag,
                                    possible,
                                    ligatures,
                                    multiplicity
                                  )
                                )
                              end)
                            end)
                          end

                        # A safe Latin presentation ligature can feed another
                        # lookup. Trigger general shaping at its original first
                        # ASCII glyph if that later lookup can change the result.
                        roots =
                          Enum.reduce(ligatures, roots, fn {[first | _], output}, roots ->
                            if MapSet.member?(roots, output),
                              do: MapSet.put(roots, first),
                              else: roots
                          end)
                          |> MapSet.intersection(ascii)

                        if MapSet.size(roots) == 0,
                          do: flags,
                          else: Map.update(flags, key, roots, &MapSet.union(&1, roots))
                      end)
                  end
                end
              end)

            {script, flags}
          end)

        dynamic? = Enum.any?([layout.gsub, layout.gpos], &(&1 != nil and &1.variations != []))

        {:ok,
         %{
           data: font.data,
           cmap: font.cmap,
           kerning: Map.get(font, :kerning, %{}),
           ligatures: Map.get(font, :latin_ligatures, %{}),
           flags: flags,
           dynamic?: dynamic?
         }}
      end
    end)
  end

  defp unsafe_roots(table, kind, tag, possible, ligatures, multiplicity) do
    FontValidator.reserve(:max_work, MapSet.size(possible))

    case {kind, table.kind} do
      {:gsub, :single} ->
        MapSet.new(
          for {glyph, target} <- table.replacements,
              MapSet.member?(possible, glyph) and target != glyph,
              do: glyph
        )

      {:gsub, :multiple} ->
        MapSet.new(
          for {glyph, targets} <- table.replacements,
              MapSet.member?(possible, glyph) and targets != [glyph],
              do: glyph
        )

      {:gsub, :alternate} ->
        MapSet.new(
          for {glyph, targets} <- table.replacements,
              MapSet.member?(possible, glyph) and List.first(targets) != glyph,
              do: glyph
        )

      {:gsub, :ligature} ->
        MapSet.new(
          for {glyph, rules} <- table.replacements,
              MapSet.member?(possible, glyph),
              Enum.any?(rules, fn rule ->
                inputs = [glyph | rule.components]
                possible? = Enum.all?(rule.components, &MapSet.member?(possible, &1))

                safe? =
                  tag == "liga" and {inputs, rule.glyph} in ligatures and
                    Enum.all?(inputs, &(Map.get(multiplicity, &1) == 1))

                possible? and not safe?
              end),
              do: glyph
        )

      {:gsub, :reverse} ->
        if pattern_possible?(table.backtrack ++ table.lookahead, :coverage, %{}, possible),
          do:
            MapSet.new(
              for {glyph, target} <- table.replacements,
                  MapSet.member?(possible, glyph) and target != glyph,
                  do: glyph
            ),
          else: MapSet.new()

      {_, mode} when mode in [:context, :chain] ->
        table.coverage
        |> MapSet.intersection(possible)
        |> Enum.filter(fn glyph ->
          key =
            case table.mode do
              :glyph -> glyph
              :class -> Map.get(table.classes.input, glyph, 0)
              :coverage -> :all
            end

          Enum.any?(Map.get(table.rules, key, []), fn rule ->
            rule.actions != [] and
              Enum.all?([:input, :backtrack, :lookahead], fn field ->
                pattern_possible?(
                  Map.fetch!(rule, field),
                  table.mode,
                  Map.get(table.classes, field, %{}),
                  possible
                )
              end)
          end)
        end)
        |> MapSet.new()

      {:gpos, :single} ->
        MapSet.new(
          for {glyph, value} <- table.values,
              MapSet.member?(possible, glyph) and not value_matches?(value, 0),
              do: glyph
        )

      {:gpos, :pair} ->
        possible
        |> Enum.filter(fn first ->
          Enum.any?(possible, fn second ->
            FontValidator.reserve(:max_work, 1)

            values = OpenTypeLookup.pair_values(table, first, second)

            case values do
              nil ->
                false

              {a, b} ->
                table.second_format != 0 or not value_matches?(a, 0) or not value_matches?(b, 0)
            end
          end)
        end)
        |> MapSet.new()

      {:gpos, :cursive} ->
        entry? =
          Enum.any?(possible, fn glyph ->
            anchor = Map.get(table.anchors, glyph)
            anchor != nil and anchor.entry != nil
          end)

        MapSet.new(
          for {glyph, anchor} <- table.anchors,
              entry? and MapSet.member?(possible, glyph) and anchor.exit != nil,
              do: glyph
        )

      {:gpos, mode} when mode in [:mark_base, :mark_ligature, :mark_mark] ->
        bases? = Enum.any?(Map.keys(table.bases), &MapSet.member?(possible, &1))

        if bases?,
          do: MapSet.intersection(MapSet.new(Map.keys(table.marks)), possible),
          else: MapSet.new()
    end
  end

  # When flags cannot skip a possible glyph and the second ValueFormat is
  # zero, PairPos adds an independent advance per pair. All selected lookups
  # must be combined, including absent pairs that suppress legacy fallback.
  defp kerning_roots(table, indices, possible, legacy, gdef) do
    lookups = Enum.map(indices, &elem(table.lookups, &1))

    plain? =
      Enum.all?(lookups, fn lookup ->
        # IgnoreMarks cannot skip any glyph in an all-base/ligature ASCII run.
        # Other flags retain the general scanner unless separately proved safe.
        (lookup.flags == 0 or
           (lookup.flags == 8 and
              Enum.all?(possible, &(Map.get(gdef.classes, &1, 1) != 3)))) and
          Enum.all?(lookup.subtables, &(&1.kind == :pair and &1.second_format == 0))
      end)

    if not plain? do
      possible
    else
      possible
      |> Enum.filter(fn first ->
        Enum.any?(possible, fn second ->
          actual =
            Enum.reduce_while(lookups, 0, fn lookup, total ->
              value =
                Enum.find_value(lookup.subtables, fn subtable ->
                  FontValidator.reserve(:max_work, 1)
                  OpenTypeLookup.pair_values(subtable, first, second)
                end)

              case value do
                nil ->
                  {:cont, total}

                {a, b} ->
                  if value_matches?(a, a.x_advance) and value_matches?(b, 0),
                    do: {:cont, total + a.x_advance},
                    else: {:halt, :unsafe}
              end
            end)

          actual == :unsafe or actual != Map.get(legacy, {first, second}, 0)
        end)
      end)
      |> MapSet.new()
    end
  end

  defp pattern_possible?(pattern, mode, classes, possible) do
    case mode do
      :glyph ->
        Enum.all?(pattern, &MapSet.member?(possible, &1))

      :coverage ->
        Enum.all?(pattern, &(not MapSet.disjoint?(&1, possible)))

      :class ->
        selected = MapSet.new(possible, &Map.get(classes, &1, 0))
        Enum.all?(pattern, &MapSet.member?(selected, &1))
    end
  end

  defp value_matches?(value, advance) do
    value.x_placement == 0 and value.y_placement == 0 and value.x_advance == advance and
      value.y_advance == 0 and value.devices == %{}
  end
end
