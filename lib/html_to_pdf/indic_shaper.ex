defmodule NativeElixirPdfUtilities.HtmlToPdf.IndicShaper do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.{GlyphSequence, OpenTypeLookup}
  alias NativeElixirPdfUtilities.Validators.FontValidator

  @scripts ~w(Deva Beng Guru Gujr Orya Taml Telu Knda Mlym Sinh)
  @features ~w(nukt akhn rphf rkrf pref blwf abvf half pstf vatu cjct pres abvs blws psts haln)
  @ra %{
    "Deva" => 0x930,
    "Beng" => 0x9B0,
    "Guru" => 0xA30,
    "Gujr" => 0xAB0,
    "Orya" => 0xB30,
    "Taml" => 0xBB0,
    "Telu" => 0xC30,
    "Knda" => 0xCB0,
    "Mlym" => 0xD30,
    "Sinh" => 0xDBB
  }

  @doc "Identifies scripts that use the Indic syllable and feature-order model."
  @spec script?(binary()) :: boolean()
  def script?(script), do: script in @scripts

  @doc "Returns the default Indic basic and presentation features."
  @spec features() :: map()
  def features, do: Map.new(@features, &{&1, 1})

  @doc "Applies syllable-local Indic substitutions and font-dependent consonant and matra reordering."
  @spec shape(map(), map(), binary()) :: map()
  def shape(state, selection, script) do
    glyphs = GlyphSequence.to_list(state.sequence)
    FontValidator.reserve(:max_work, length(glyphs))

    shaped =
      glyphs
      |> syllables()
      |> Enum.flat_map(&shape_syllable(&1, state, selection, script))

    %{state | sequence: GlyphSequence.new(shaped)}
  end

  defp syllables(glyphs) do
    {syllables, _, _} =
      Enum.reduce(glyphs, {[], false, false}, fn glyph, {syllables, halant?, joiner?} ->
        category = glyph.syllabic_category
        consonant? = category in [:Consonant, :Consonant_Dead, :Consonant_Head_Letter]
        vowel? = category == :Vowel_Independent

        mark? =
          glyph.mark? or
            category in [
              :Vowel_Dependent,
              :Nukta,
              :Virama,
              :Bindu,
              :Visarga,
              :Cantillation_Mark,
              :Tone_Mark,
              :Joiner,
              :Non_Joiner,
              :Consonant_Final,
              :Consonant_Medial,
              :Consonant_Succeeding_Repha
            ]

        continue? =
          syllables != [] and (mark? or ((consonant? or vowel?) and (halant? or joiner?)))

        syllables =
          case {continue?, syllables} do
            {true, [current | rest]} -> [[glyph | current] | rest]
            _ -> [[glyph] | syllables]
          end

        {syllables, category == :Virama or (halant? and category in [:Nukta, :Joiner]),
         category == :Joiner and not halant?}
      end)

    syllables |> Enum.reverse() |> Enum.map(&Enum.reverse/1)
  end

  defp shape_syllable(glyphs, state, selection, script) do
    first = hd(glyphs)
    last = List.last(glyphs)

    glyphs =
      Enum.map(glyphs, fn glyph ->
        Map.merge(glyph, %{
          cluster_start: first.cluster_start,
          cluster_end: last.cluster_end,
          scalar_start: first.scalar_start,
          scalar_end: last.scalar_end,
          indic_masks: MapSet.new()
        })
      end)

    local = %{state | sequence: GlyphSequence.new(glyphs)}
    local = OpenTypeLookup.execute(local, selection, ~w(rvrn ccmp locl))
    local = OpenTypeLookup.execute(local, selection, ~w(nukt))
    local = OpenTypeLookup.execute(local, selection, ~w(akhn))

    case GlyphSequence.to_list(local.sequence) do
      [] -> []
      glyphs -> shape_basic(local, selection, script, glyphs)
    end
  end

  defp shape_basic(local, selection, script, glyphs) do
    reph? =
      case glyphs do
        [ra, halant, following | _] ->
          hd(ra.codepoints) == @ra[script] and halant.syllabic_category == :Virama and
            if(script == "Sinh",
              do: hd(following.codepoints) == 0x200D,
              else: hd(following.codepoints) not in [0x200C, 0x200D]
            )

        _ ->
          false
      end

    local =
      if reph? do
        %{
          local
          | sequence:
              Enum.reduce(Enum.take(glyphs, 2), local.sequence, fn glyph, sequence ->
                GlyphSequence.put(sequence, glyph.id, %{indic_masks: MapSet.new(["rphf"])})
              end)
        }
      else
        local
      end

    local = OpenTypeLookup.execute(local, selection, ~w(rphf))
    glyphs = GlyphSequence.to_list(local.sequence)

    consonants =
      Enum.filter(glyphs, fn glyph ->
        glyph.syllabic_category in [
          :Consonant,
          :Consonant_Dead,
          :Consonant_Head_Letter,
          :Vowel_Independent
        ] and not applied?(glyph, "rphf")
      end)

    # Conjoining properties belong to the font. Probe only the context-free
    # halant/consonant pairs defined by the Indic OpenType model.
    forms =
      Enum.reduce(Enum.chunk_every(glyphs, 2, 1, :discard), %{}, fn [left, right], forms ->
        if left.syllabic_category == :Virama do
          probe = %{
            local
            | sequence:
                GlyphSequence.new([
                  Map.delete(left, :indic_masks),
                  Map.delete(right, :indic_masks)
                ])
          }

          tags =
            for tag <- ~w(pref blwf pstf),
                result = OpenTypeLookup.execute(probe, selection, [tag]),
                Enum.any?(GlyphSequence.to_list(result.sequence), &applied?(&1, tag)),
                do: tag

          Map.put(forms, right.id, tags)
        else
          forms
        end
      end)

    base =
      Enum.find(Enum.reverse(consonants), fn glyph ->
        Map.get(forms, glyph.id, []) == []
      end) || List.first(consonants)

    base_index = if base == nil, do: 0, else: Enum.find_index(glyphs, &(&1.id == base.id))

    sequence =
      glyphs
      |> Enum.with_index()
      |> Enum.reduce(local.sequence, fn {glyph, index}, sequence ->
        next = Map.get(sequence.nodes, glyph.next)
        following = if next == nil, do: nil, else: Map.get(sequence.nodes, next.next)
        blocked? = following != nil and hd(following.codepoints) == 0x200C

        standalone_half? =
          next != nil and next.syllabic_category == :Virama and
            following != nil and hd(following.codepoints) == 0x200D

        previous = Map.get(sequence.nodes, glyph.previous)

        halant_half? =
          glyph.syllabic_category == :Virama and previous != nil and
            MapSet.member?(previous.indic_masks, "half")

        half? = ((index < base_index or standalone_half?) and not blocked?) or halant_half?

        masks =
          MapSet.new(
            if(half?, do: ["half"], else: []) ++
              if(index >= base_index, do: ~w(pref blwf pstf), else: [])
          )

        GlyphSequence.put(sequence, glyph.id, %{indic_masks: masks})
      end)

    # Reph stays at the front until basic substitutions are complete.
    {reph, rest} = Enum.split_while(glyphs, &applied?(&1, "rphf"))

    {matras, rest} =
      Enum.split_with(
        rest,
        &(&1.positional_category == :Left and
            &1.syllabic_category == :Vowel_Dependent)
      )

    local = %{
      local
      | sequence: GlyphSequence.reorder(sequence, Enum.map(reph ++ matras ++ rest, & &1.id))
    }

    local =
      Enum.reduce(~w(rkrf pref blwf abvf half pstf vatu cjct), local, fn tag, local ->
        OpenTypeLookup.execute(local, selection, [tag])
      end)

    ordered = final_order(GlyphSequence.to_list(local.sequence), base)

    local = %{
      local
      | sequence: GlyphSequence.reorder(local.sequence, Enum.map(ordered, & &1.id))
    }

    basic = ~w(rvrn ccmp locl nukt akhn rphf rkrf pref blwf abvf half pstf vatu cjct)

    remaining =
      selection.selected |> Map.values() |> Enum.flat_map(&Map.keys/1) |> Enum.uniq()

    local = OpenTypeLookup.execute(local, selection, remaining -- basic)
    GlyphSequence.to_list(local.sequence)
  end

  defp applied?(glyph, tag),
    do: MapSet.member?(Map.get(glyph, :applied_features, MapSet.new()), tag)

  defp final_order(glyphs, base) do
    {reph, glyphs} = Enum.split_with(glyphs, &applied?(&1, "rphf"))

    {prebase, glyphs} =
      Enum.split_with(glyphs, fn glyph ->
        applied?(glyph, "pref") or
          (glyph.positional_category == :Left and glyph.syllabic_category == :Vowel_Dependent)
      end)

    base_index =
      Enum.find_index(glyphs, fn glyph ->
        base != nil and not MapSet.disjoint?(glyph.input_ids, base.input_ids)
      end) || 0

    {before, after_base} = Enum.split(glyphs, base_index)
    # A visible halant ends the preceding full form. A pre-base matra follows
    # that form, while a substituted half form remains after the matra.
    last_halant =
      before
      |> Enum.with_index()
      |> Enum.reduce(nil, fn {glyph, index}, found ->
        if glyph.syllabic_category == :Virama and not applied?(glyph, "half"),
          do: index,
          else: found
      end)

    insertion = if last_halant == nil, do: 0, else: last_halant + 1

    insertion =
      case Enum.at(before, insertion) do
        %{codepoints: [joiner | _]} when joiner in [0x200C, 0x200D] -> insertion + 1
        _ -> insertion
      end

    {left, right} = Enum.split(before, insertion)
    glyphs = left ++ prebase ++ right ++ after_base

    if reph == [] do
      glyphs
    else
      {body, signs} =
        Enum.split_while(glyphs, fn glyph ->
          glyph.syllabic_category not in [:Bindu, :Visarga, :Cantillation_Mark] or
            glyph.positional_category == :Left
        end)

      body ++ reph ++ signs
    end
  end
end
