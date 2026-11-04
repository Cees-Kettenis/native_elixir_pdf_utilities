defmodule NativeElixirPdfUtilities.Validators.MathFontValidator do
  @moduledoc false

  import Bitwise
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.FontValidator

  # The record order and 214-byte fixed portion are OpenType format invariants.
  @value_constants ~w(
    math_leading axis_height accent_base_height flattened_accent_base_height
    subscript_shift_down subscript_top_max subscript_baseline_drop_min
    superscript_shift_up superscript_shift_up_cramped superscript_bottom_min
    superscript_baseline_drop_max sub_superscript_gap_min
    superscript_bottom_max_with_subscript space_after_script
    upper_limit_gap_min upper_limit_baseline_rise_min
    lower_limit_gap_min lower_limit_baseline_drop_min
    stack_top_shift_up stack_top_display_style_shift_up stack_bottom_shift_down
    stack_bottom_display_style_shift_down stack_gap_min stack_display_style_gap_min
    stretch_stack_top_shift_up stretch_stack_bottom_shift_down
    stretch_stack_gap_above_min stretch_stack_gap_below_min
    fraction_numerator_shift_up fraction_numerator_display_style_shift_up
    fraction_denominator_shift_down fraction_denominator_display_style_shift_down
    fraction_numerator_gap_min fraction_num_display_style_gap_min fraction_rule_thickness
    fraction_denominator_gap_min fraction_denom_display_style_gap_min
    skewed_fraction_horizontal_gap skewed_fraction_vertical_gap
    overbar_vertical_gap overbar_rule_thickness overbar_extra_ascender
    underbar_vertical_gap underbar_rule_thickness underbar_extra_descender
    radical_vertical_gap radical_display_style_vertical_gap radical_rule_thickness
    radical_extra_ascender radical_kern_before_degree radical_kern_after_degree
  )a

  @doc false
  @spec failure(String.t(), String.t()) ::
          {:error, {:invalid_document, Diagnostics.diagnostic()}}
  def failure(message, table \\ "MATH") do
    Diagnostics.error(:font, :invalid_document, "OpenType #{table}: " <> message,
      operation: :math_metrics,
      module: __MODULE__,
      source: "selected font #{table} table"
    )
  end

  @doc "Validates selected glyph pairs and the bounded math-only positioning script."
  @spec validate_pair_request(term(), term(), term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_pair_request(glyph_count, pairs, script_tag) do
    valid? =
      is_integer(glyph_count) and glyph_count in 1..65_536 and is_list(pairs) and
        Enum.all?(pairs, fn pair ->
          case pair do
            {a, b} ->
              is_integer(a) and is_integer(b) and a >= 0 and b >= 0 and a < glyph_count and
                b < glyph_count

            _ ->
              false
          end
        end) and script_tag in ["latn", "grek", "math"]

    if valid?, do: :ok, else: failure("invalid requested pairs or script", "GPOS")
  end

  @doc false
  @spec prepare_pair_adjustments(term(), term(), term(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_pair_adjustments(data, glyph_count, pairs, script_tag) do
    FontValidator.with_budget(fn ->
      try do
        require_valid(
          is_binary(data) and is_integer(glyph_count) and glyph_count in 1..65_536,
          "pair adjustment requires table bytes and a valid glyph count"
        )

        case validate_pair_request(glyph_count, pairs, script_tag) do
          :ok -> :ok
          {:error, {_, diagnostic}} -> throw({:invalid_math_font, diagnostic.message})
        end

        <<major::16, minor::16, scripts::16, features::16, lookups::16>> = slice(data, 0, 10)
        require_valid(major == 1 and minor in [0, 1], "unsupported table version")

        require_valid(
          Enum.all?([scripts, features, lookups], &(&1 >= if(minor == 0, do: 10, else: 14))),
          "invalid table offsets"
        )

        if minor == 1 do
          <<variations::32>> = slice(data, 10, 4)
          require_valid(variations == 0, "variable pair positioning needs variation coordinates")
        end

        scripts_list = tagged_offsets(data, scripts, true)

        selected =
          List.keyfind(scripts_list, script_tag, 0) || List.keyfind(scripts_list, "DFLT", 0)

        result =
          case selected do
            nil ->
              %{}

            {_, offset} ->
              base = scripts + offset
              <<default::16, count::16>> = slice(data, base, 4)
              slice(data, base + 4, count * 6)

              if default == 0 do
                %{}
              else
                language = base + default
                <<order::16, required::16, count::16>> = slice(data, language, 6)
                require_valid(order == 0, "unsupported language lookup order")
                indices = for <<index::16 <- slice(data, language + 6, count * 2)>>, do: index
                indices = if required == 0xFFFF, do: indices, else: [required | indices]
                feature_list = tagged_offsets(data, features, false) |> List.to_tuple()

                require_valid(
                  Enum.all?(indices, &(&1 < tuple_size(feature_list))),
                  "feature index outside feature list"
                )

                <<lookup_count::16>> = slice(data, lookups, 2)

                lookup_offsets =
                  for(<<offset::16 <- slice(data, lookups + 2, lookup_count * 2)>>, do: offset)
                  |> List.to_tuple()

                selected =
                  Enum.flat_map(indices, fn index ->
                    case elem(feature_list, index) do
                      {"kern", offset} ->
                        <<parameters::16, count::16>> = slice(data, features + offset, 4)
                        require_valid(parameters == 0, "unsupported kerning feature parameters")

                        for <<index::16 <- slice(data, features + offset + 4, count * 2)>>,
                          do: index

                      _ ->
                        []
                    end
                  end)
                  |> Enum.uniq()
                  |> Enum.sort()

                require_valid(
                  Enum.all?(selected, &(&1 < lookup_count)),
                  "pair lookup index outside lookup list"
                )

                Enum.reduce(selected, %{}, fn index, result ->
                  base = lookups + elem(lookup_offsets, index)
                  <<type::16, flags::16, count::16>> = slice(data, base, 6)

                  require_valid(
                    type in [2, 9] and flags in [0, 8],
                    "kerning needs unsupported contextual positioning or glyph filtering"
                  )

                  offsets = for <<offset::16 <- slice(data, base + 6, count * 2)>>, do: offset

                  prepared =
                    Enum.reduce(offsets, %{}, fn offset, prepared ->
                      subtable = base + offset

                      subtable =
                        if type == 9 do
                          <<format::16, target::16, offset::32>> = slice(data, subtable, 8)

                          require_valid(
                            format == 1 and target == 2,
                            "unsupported positioning extension"
                          )

                          subtable + offset
                        else
                          subtable
                        end

                      # Only the first matching subtable in each lookup applies.
                      Map.merge(pair_adjustments(data, subtable, glyph_count, pairs), prepared)
                    end)

                  Map.merge(result, prepared, fn _pair, a, b -> a + b end)
                end)
              end
          end

        {:ok, result}
      catch
        {:invalid_math_font, message} -> failure(message, "GPOS")
      end
    end)
  end

  defp pair_adjustments(data, base, glyph_count, pairs) do
    <<format::16, coverage_offset::16, first_format::16, second_format::16>> =
      slice(data, base, 8)

    require_valid(
      first_format == 4 and second_format == 0,
      "pair positioning needs unsupported placement, vertical, device or variation adjustments"
    )

    covered = coverage(data, base, coverage_offset, glyph_count)

    case format do
      1 ->
        <<count::16>> = slice(data, base + 8, 2)
        require_valid(count == length(covered), "pair sets disagree with coverage")
        offsets = for <<offset::16 <- slice(data, base + 10, count * 2)>>, do: offset

        Enum.zip(covered, offsets)
        |> Enum.reduce(%{}, fn {first, offset}, result ->
          <<count::16>> = slice(data, base + offset, 2)
          FontValidator.reserve(:max_font_math_work, count)
          FontValidator.reserve(:max_font_kerning_pairs, count)

          records =
            for <<second::16, advance::signed-16 <- slice(data, base + offset + 2, count * 4)>>,
              do: {second, advance}

          require_valid(
            ordered?(Enum.map(records, &elem(&1, 0)), true) and
              Enum.all?(records, &(elem(&1, 0) < glyph_count)),
            "invalid pair-set glyphs"
          )

          Enum.reduce(records, result, fn {second, advance}, result ->
            if {first, second} in pairs,
              do: Map.put(result, {first, second}, advance),
              else: result
          end)
        end)

      2 ->
        <<first_offset::16, second_offset::16, first_count::16, second_count::16>> =
          slice(data, base + 8, 8)

        first_classes = pair_classes(data, base + first_offset, glyph_count, first_count)
        second_classes = pair_classes(data, base + second_offset, glyph_count, second_count)
        FontValidator.reserve(:max_font_math_work, first_count * second_count)
        FontValidator.reserve(:max_font_kerning_pairs, first_count * second_count)
        matrix = slice(data, base + 16, first_count * second_count * 2)

        Enum.reduce(pairs, %{}, fn {first, second} = pair, result ->
          if first in covered do
            index =
              (Map.get(first_classes, first, 0) * second_count +
                 Map.get(second_classes, second, 0)) * 2

            <<advance::signed-16>> = slice(matrix, index, 2)
            Map.put(result, pair, advance)
          else
            result
          end
        end)

      _ ->
        throw({:invalid_math_font, "unsupported pair adjustment format"})
    end
  end

  defp pair_classes(data, base, glyph_count, class_count) do
    <<format::16>> = slice(data, base, 2)

    records =
      case format do
        1 ->
          <<first::16, count::16>> = slice(data, base + 2, 4)
          FontValidator.reserve(:max_font_math_work, count)
          classes = for <<class::16 <- slice(data, base + 6, count * 2)>>, do: class
          classes |> Enum.with_index(first) |> Enum.map(fn {class, glyph} -> {glyph, class} end)

        2 ->
          <<count::16>> = slice(data, base + 2, 2)

          records =
            for <<first::16, last::16, class::16 <- slice(data, base + 4, count * 6)>>,
              do: {first, last, class}

          require_valid(
            Enum.chunk_every(records, 2, 1, :discard)
            |> Enum.all?(fn [{_, last, _}, {first, _, _}] -> last < first end),
            "unordered class ranges"
          )

          Enum.flat_map(records, fn {first, last, class} ->
            require_valid(first <= last and last < glyph_count, "invalid class range")
            FontValidator.reserve(:max_font_math_work, last - first + 1)
            Enum.map(first..last, &{&1, class})
          end)

        _ ->
          throw({:invalid_math_font, "unsupported class definition format"})
      end

    require_valid(
      class_count > 0 and
        Enum.all?(records, fn {glyph, class} -> glyph < glyph_count and class < class_count end),
      "class outside declared glyph or class counts"
    )

    Map.new(records)
  end

  @doc false
  @spec prepare_substitutions(term(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_substitutions(data, glyph_count) do
    with {:ok, features} <- prepare_substitution_features(data, glyph_count) do
      {:ok, features.substitutions}
    end
  end

  @doc false
  @spec prepare_substitution_features(term(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  @spec prepare_substitution_features(term(), term(), :math | :latin) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_substitution_features(data, glyph_count, policy \\ :math) do
    FontValidator.with_budget(fn ->
      try do
        require_valid(is_binary(data), "table bytes must be a binary")
        require_valid(is_integer(glyph_count) and glyph_count in 1..65_536, "invalid glyph count")
        <<major::16, minor::16, scripts::16, features::16, lookups::16>> = slice(data, 0, 10)
        require_valid(major == 1 and minor in [0, 1], "unsupported table version")
        header_size = if minor == 0, do: 10, else: 14

        require_valid(
          Enum.all?([scripts, features, lookups], &(&1 >= header_size)),
          "invalid table offsets"
        )

        if minor == 1 do
          <<variations::32>> = slice(data, 10, 4)
          require_valid(variations == 0, "math feature variations are not supported")
        end

        require_valid(policy in [:math, :latin], "unsupported substitution selection policy")
        script_records = tagged_offsets(data, scripts, true)

        selected_script =
          if policy == :math,
            do: List.keyfind(script_records, "math", 0),
            else:
              List.keyfind(script_records, "latn", 0) || List.keyfind(script_records, "DFLT", 0)

        substitutions =
          case selected_script do
            nil ->
              %{}

            {_, offset} ->
              script = scripts + offset
              <<default::16, language_count::16>> = slice(data, script, 4)
              # Math uses the default language system, not a named language.
              slice(data, script + 4, language_count * 6)

              if default == 0 do
                %{}
              else
                require_valid(default >= 4 + language_count * 6, "invalid language offset")
                language = script + default
                <<reserved::16, required::16, count::16>> = slice(data, language, 6)
                require_valid(reserved == 0, "reserved language lookup order")
                records = slice(data, language + 6, count * 2)
                indices = for <<index::16 <- records>>, do: index
                indices = if required == 0xFFFF, do: indices, else: [required | indices]
                feature_records = tagged_offsets(data, features, false) |> List.to_tuple()

                require_valid(
                  Enum.all?(indices, &(&1 < tuple_size(feature_records))),
                  "language feature index outside feature list"
                )

                <<lookup_count::16>> = slice(data, lookups, 2)
                records = slice(data, lookups + 2, lookup_count * 2)
                lookup_offsets = for(<<offset::16 <- records>>, do: offset) |> List.to_tuple()

                indices
                |> Enum.uniq()
                |> Enum.reduce(%{}, fn index, result ->
                  {tag, offset} = elem(feature_records, index)

                  if tag in if(policy == :math, do: ["ssty", "flac", "dtls"], else: ["liga"]) do
                    feature = features + offset
                    <<parameters::16, count::16>> = slice(data, feature, 4)
                    require_valid(parameters == 0, "unexpected math feature parameters")
                    records = slice(data, feature + 4, count * 2)
                    selected = for <<index::16 <- records>>, do: index

                    require_valid(
                      Enum.all?(selected, &(&1 < lookup_count)),
                      "feature lookup index outside lookup list"
                    )

                    Map.update(result, tag, selected, &(selected ++ &1))
                  else
                    result
                  end
                end)
                |> Map.new(fn {tag, indices} ->
                  parsed =
                    indices
                    |> Enum.uniq()
                    |> Enum.sort()
                    |> Enum.map(fn index ->
                      offset = elem(lookup_offsets, index)
                      require_valid(offset >= 2 + lookup_count * 2, "invalid lookup offset")
                      substitution_lookup(data, lookups + offset, glyph_count, policy)
                    end)

                  {tag, parsed}
                end)
              end
          end

        {:ok,
         %{
           substitutions:
             Map.new(substitutions, fn {tag, lookups} ->
               {tag, Enum.map(lookups, &elem(&1, 1))}
             end),
           substitution_kinds:
             Map.new(substitutions, fn {tag, lookups} ->
               {tag, Enum.map(lookups, &elem(&1, 0))}
             end)
         }}
      catch
        {:unsupported_latin_ligature, _message} ->
          {:ok,
           %{substitutions: %{}, substitution_kinds: %{}, latin_policy: :requires_full_shaping}}

        {:invalid_math_font, message} ->
          if policy == :latin,
            do:
              Diagnostics.error(:font, :invalid_document, "OpenType GSUB: " <> message,
                module: __MODULE__,
                operation: :font_ligatures,
                source: "selected font GSUB table"
              ),
            else: failure(message, "GSUB")
      end
    end)
  end

  defp tagged_offsets(data, base, strict) do
    <<count::16>> = slice(data, base, 2)
    records = slice(data, base + 2, count * 6)
    records = for <<tag::binary-size(4), offset::16 <- records>>, do: {tag, offset}
    require_valid(ordered?(Enum.map(records, &elem(&1, 0)), strict), "unordered table tags")

    require_valid(
      Enum.all?(records, fn {_, offset} -> offset >= 2 + count * 6 end),
      "invalid tagged table offset"
    )

    records
  end

  defp substitution_lookup(data, base, glyph_count, policy) do
    <<type::16, flags::16, count::16>> = slice(data, base, 6)
    # Nonzero mark-filtering flags require GDEF and are not context-free math substitutions.
    if policy == :latin and flags not in [0, 1],
      do: throw({:unsupported_latin_ligature, "lookup requires glyph filtering"})

    require_valid(flags in [0, 1], "math lookup requires unsupported glyph filtering")
    require_valid(count > 0, "empty substitution lookup")
    records = slice(data, base + 6, count * 2)

    Enum.reduce(for(<<offset::16 <- records>>, do: offset), {nil, %{}}, fn offset,
                                                                           {kind, result} ->
      require_valid(offset >= 6 + count * 2, "invalid substitution subtable offset")
      subtable = base + offset

      {resolved_type, substitutions} =
        if type == 7 do
          <<format::16, extended_type::16, extended_offset::32>> = slice(data, subtable, 8)

          require_valid(
            format == 1 and extended_offset >= 8,
            "unsupported extension substitution"
          )

          {extended_type,
           if(policy == :latin,
             do:
               latin_ligature_substitution(
                 data,
                 subtable + extended_offset,
                 extended_type,
                 glyph_count
               ),
             else: substitution(data, subtable + extended_offset, extended_type, glyph_count)
           )}
        else
          {type,
           if(policy == :latin,
             do: latin_ligature_substitution(data, subtable, type, glyph_count),
             else: substitution(data, subtable, type, glyph_count)
           )}
        end

      resolved_kind =
        if(policy == :latin,
          do: :ligature,
          else: if(resolved_type == 1, do: :single, else: :alternate)
        )

      require_valid(
        is_nil(kind) or kind == resolved_kind,
        "extension subtables have different lookup types"
      )

      # The first matching subtable wins; subsequent lookups remain separate.
      {resolved_kind, Map.merge(substitutions, result)}
    end)
  end

  defp latin_ligature_substitution(data, base, type, glyph_count) do
    if type != 4, do: throw({:unsupported_latin_ligature, "liga needs contextual shaping"})
    <<format::16, coverage_offset::16, count::16>> = slice(data, base, 6)

    require_valid(
      format == 1 and coverage_offset >= 6 + count * 2,
      "invalid ligature substitution header"
    )

    glyphs = coverage(data, base, coverage_offset, glyph_count)
    require_valid(length(glyphs) == count, "ligature set count differs from coverage")
    records = slice(data, base + 6, count * 2)
    offsets = for <<offset::16 <- records>>, do: offset

    Enum.zip(glyphs, offsets)
    |> Enum.reduce(%{}, fn {first, offset}, substitutions ->
      require_valid(offset >= 6 + count * 2, "invalid ligature set offset")
      set = base + offset
      <<ligature_count::16>> = slice(data, set, 2)
      records = slice(data, set + 2, ligature_count * 2)
      offsets = for <<offset::16 <- records>>, do: offset

      Enum.reduce(offsets, substitutions, fn offset, substitutions ->
        require_valid(offset >= 2 + ligature_count * 2, "invalid ligature offset")
        ligature = set + offset
        <<replacement::16, components::16>> = slice(data, ligature, 4)

        require_valid(
          components >= 2 and replacement < glyph_count,
          "invalid ligature glyph or component count"
        )

        records = slice(data, ligature + 4, (components - 1) * 2)
        remaining = for <<glyph::16 <- records>>, do: glyph

        require_valid(
          Enum.all?(remaining, &(&1 < glyph_count)),
          "ligature component outside maxp glyph count"
        )

        Map.put_new(substitutions, [first | remaining], replacement)
      end)
    end)
  end

  defp substitution(data, base, type, glyph_count) do
    require_valid(type in [1, 3], "math lookup is not a single or alternate substitution")
    <<format::16, coverage_offset::16, value::16>> = slice(data, base, 6)
    require_valid(coverage_offset >= 6, "invalid substitution coverage offset")
    glyphs = coverage(data, base, coverage_offset, glyph_count)

    replacements =
      case {type, format} do
        {1, 1} ->
          # Glyph arithmetic wraps at 16 bits, a fixed OpenType invariant.
          Enum.map(glyphs, &[band(&1 + value, 0xFFFF)])

        {1, 2} ->
          require_valid(length(glyphs) == value, "substitute count differs from coverage")
          records = slice(data, base + 6, value * 2)
          for <<glyph::16 <- records>>, do: [glyph]

        {3, 1} ->
          require_valid(length(glyphs) == value, "alternate count differs from coverage")
          records = slice(data, base + 6, value * 2)

          for <<offset::16 <- records>> do
            require_valid(offset >= 6 + value * 2, "invalid alternate set offset")
            <<count::16>> = slice(data, base + offset, 2)
            require_valid(count > 0, "empty alternate set")
            records = slice(data, base + offset + 2, count * 2)
            for <<glyph::16 <- records>>, do: glyph
          end

        _ ->
          throw({:invalid_math_font, "unsupported substitution format"})
      end

    require_valid(
      Enum.all?(replacements, fn glyphs -> Enum.all?(glyphs, &(&1 < glyph_count)) end),
      "substitute glyph outside maxp glyph count"
    )

    Map.new(Enum.zip(glyphs, replacements))
  end

  @doc false
  @spec prepare(term(), term()) :: {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare(data, glyph_count) do
    FontValidator.with_budget(fn ->
      try do
        require_valid(is_binary(data), "table bytes must be a binary")
        # Glyph IDs are unsigned 16-bit values, a fixed OpenType bound.
        require_valid(is_integer(glyph_count) and glyph_count in 1..65_536, "invalid glyph count")
        <<major::16, minor::16, constants::16, glyphs::16, variants::16>> = slice(data, 0, 10)
        require_valid(major == 1 and minor == 0, "unsupported table version")

        require_valid(
          Enum.all?([constants, glyphs, variants], &(&1 >= 10)),
          "invalid table offsets"
        )

        {:ok,
         %{
           constants: constants(data, constants),
           glyph_info: glyph_info(data, glyphs, glyph_count),
           variants: variants(data, variants, glyph_count)
         }}
      catch
        {:invalid_math_font, message} -> failure(message)
      end
    end)
  end

  defp constants(data, base) do
    <<script::signed-16, scriptscript::signed-16, delimited::16, display::16,
      records::binary-size(204), degree::signed-16>> = slice(data, base, 214)

    values = for <<value::signed-16, device::16 <- records>>, do: value(data, base, value, device)

    Map.new(Enum.zip(@value_constants, values))
    |> Map.merge(%{
      script_percent_scale_down: script,
      script_script_percent_scale_down: scriptscript,
      delimited_sub_formula_min_height: delimited,
      display_operator_min_height: display,
      radical_degree_bottom_raise_percent: degree
    })
  end

  defp glyph_info(data, base, glyph_count) do
    <<italics::16, accents::16, extended::16, kerns::16>> = slice(data, base, 8)

    %{
      italics_corrections: glyph_values(data, base, italics, glyph_count),
      top_accent_attachments: glyph_values(data, base, accents, glyph_count),
      extended_shapes: MapSet.new(coverage(data, base, extended, glyph_count)),
      kerning: glyph_kerns(data, base, kerns, glyph_count)
    }
  end

  defp glyph_values(data, parent, offset, glyph_count) do
    case offset do
      0 ->
        %{}

      _ ->
        base = parent + offset
        <<coverage_offset::16, count::16>> = slice(data, base, 4)
        glyphs = coverage(data, base, coverage_offset, glyph_count)
        require_valid(length(glyphs) == count, "glyph value count differs from coverage")
        records = slice(data, base + 4, count * 4)
        values = for <<v::signed-16, d::16 <- records>>, do: value(data, base, v, d)
        Map.new(Enum.zip(glyphs, values))
    end
  end

  defp glyph_kerns(data, parent, offset, glyph_count) do
    case offset do
      0 ->
        %{}

      _ ->
        base = parent + offset
        <<coverage_offset::16, count::16>> = slice(data, base, 4)
        glyphs = coverage(data, base, coverage_offset, glyph_count)
        require_valid(length(glyphs) == count, "kern count differs from coverage")
        records = slice(data, base + 4, count * 8)

        kerns =
          for <<tr::16, tl::16, br::16, bl::16 <- records>> do
            Map.new(
              Enum.zip([:top_right, :top_left, :bottom_right, :bottom_left], [tr, tl, br, bl]),
              fn {corner, offset} -> {corner, kern(data, base, offset)} end
            )
          end

        Map.new(Enum.zip(glyphs, kerns))
    end
  end

  defp kern(data, parent, offset) do
    case offset do
      0 ->
        nil

      _ ->
        base = parent + offset
        <<count::16>> = slice(data, base, 2)
        heights = slice(data, base + 2, count * 4)
        heights = for <<v::signed-16, d::16 <- heights>>, do: value(data, base, v, d)
        require_valid(ordered?(Enum.map(heights, & &1.value)), "unordered kern heights")
        records = slice(data, base + 2 + count * 4, (count + 1) * 4)
        values = for <<v::signed-16, d::16 <- records>>, do: value(data, base, v, d)
        %{heights: heights, values: values}
    end
  end

  defp variants(data, base, glyph_count) do
    <<overlap::16, vertical::16, horizontal::16, vcount::16, hcount::16>> = slice(data, base, 10)
    vg = coverage(data, base, vertical, glyph_count)
    hg = coverage(data, base, horizontal, glyph_count)

    require_valid(
      length(vg) == vcount and length(hg) == hcount,
      "variant counts differ from coverage"
    )

    records = slice(data, base + 10, (vcount + hcount) * 2)
    offsets = for <<offset::16 <- records>>, do: offset
    {vo, ho} = Enum.split(offsets, vcount)

    %{
      min_connector_overlap: overlap,
      vertical:
        Map.new(Enum.zip(vg, vo), fn {g, o} -> {g, construction(data, base, o, glyph_count)} end),
      horizontal:
        Map.new(Enum.zip(hg, ho), fn {g, o} -> {g, construction(data, base, o, glyph_count)} end)
    }
  end

  defp construction(data, parent, offset, glyph_count) do
    require_valid(offset > 0, "missing glyph construction offset")
    base = parent + offset
    <<assembly::16, count::16>> = slice(data, base, 4)
    records = slice(data, base + 4, count * 4)

    variants =
      for <<glyph::16, advance::16 <- records>> do
        require_valid(glyph < glyph_count, "variant glyph outside maxp glyph count")
        %{glyph: glyph, advance: advance}
      end

    require_valid(ordered?(Enum.map(variants, & &1.advance)), "unordered variant advances")
    %{variants: variants, assembly: assembly(data, base, assembly, glyph_count)}
  end

  defp assembly(data, parent, offset, glyph_count) do
    case offset do
      0 ->
        nil

      _ ->
        base = parent + offset
        <<v::signed-16, d::16, count::16>> = slice(data, base, 6)
        correction = value(data, base, v, d)
        require_valid(count > 0, "empty glyph assembly")
        records = slice(data, base + 6, count * 10)

        parts =
          for <<glyph::16, start::16, finish::16, advance::16, flags::16 <- records>> do
            require_valid(glyph < glyph_count, "assembly glyph outside maxp glyph count")
            require_valid(flags in [0, 1], "reserved assembly flags")

            %{
              glyph: glyph,
              start_connector: start,
              end_connector: finish,
              full_advance: advance,
              extender?: flags == 1
            }
          end

        %{italics_correction: correction, parts: parts}
    end
  end

  defp coverage(data, parent, offset, glyph_count) do
    case offset do
      0 ->
        []

      _ ->
        base = parent + offset
        <<format::16, count::16>> = slice(data, base, 4)

        case format do
          1 ->
            records = slice(data, base + 4, count * 2)
            glyphs = for <<glyph::16 <- records>>, do: glyph
            require_valid(ordered?(glyphs, true), "unordered coverage glyphs")

            require_valid(
              Enum.all?(glyphs, &(&1 < glyph_count)),
              "coverage glyph outside maxp glyph count"
            )

            glyphs

          2 ->
            records = slice(data, base + 4, count * 6)

            {ranges, _last, _index} =
              Enum.reduce(
                for(<<a::16, b::16, i::16 <- records>>, do: {a, b, i}),
                {[], -1, 0},
                fn {first, last, index}, {ranges, previous, expected} ->
                  require_valid(
                    first <= last and first > previous and last < glyph_count,
                    "invalid coverage range"
                  )

                  require_valid(index == expected, "nonconsecutive coverage indices")
                  FontValidator.reserve(:max_font_math_work, last - first + 1)
                  {[Enum.to_list(first..last) | ranges], last, expected + last - first + 1}
                end
              )

            ranges |> Enum.reverse() |> List.flatten()

          _ ->
            throw({:invalid_math_font, "unsupported coverage format"})
        end
    end
  end

  defp value(data, base, value, offset) do
    device =
      case offset do
        0 ->
          nil

        _ ->
          <<first::16, last::16, format::16>> = slice(data, base + offset, 6)

          case format do
            0x8000 ->
              %{variation_index: {first, last}}

            format when format in [1, 2, 3] ->
              require_valid(first <= last, "invalid device size range")
              bits = 1 <<< format
              count = last - first + 1
              words = div(count * bits + 15, 16)
              records = slice(data, base + offset + 6, words * 2)
              FontValidator.reserve(:max_font_math_work, count)
              deltas = for <<delta::signed-size(^bits) <- records>>, do: delta
              %{start_size: first, end_size: last, deltas: Enum.take(deltas, count)}

            _ ->
              throw({:invalid_math_font, "unsupported device format"})
          end
      end

    %{value: value, device: device}
  end

  defp ordered?(values, strict \\ false) do
    Enum.reduce_while(values, nil, fn value, previous ->
      if is_nil(previous) or value > previous or (not strict and value == previous),
        do: {:cont, value},
        else: {:halt, :invalid}
    end) != :invalid
  end

  defp slice(data, offset, length) do
    require_valid(
      offset >= 0 and length >= 0 and offset + length <= byte_size(data),
      "truncated table or out-of-bounds offset"
    )

    FontValidator.reserve(:max_font_math_work, length)
    binary_part(data, offset, length)
  end

  defp require_valid(condition, message) do
    if not condition, do: throw({:invalid_math_font, message})
  end
end
