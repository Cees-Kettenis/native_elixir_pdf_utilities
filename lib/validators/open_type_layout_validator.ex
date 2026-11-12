defmodule NativeElixirPdfUtilities.Validators.OpenTypeLayoutValidator do
  @moduledoc false
  import Bitwise
  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.Validators.{
    FontValidator,
    FontVariationValidator,
    VariationStoreValidator
  }

  alias NativeElixirPdfUtilities.Validators.OpenTypeDataValidator, as: Data

  @doc "Reuses admitted layout tables within an operation while checking the selected source bytes."
  @spec for_font(term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def for_font(font) do
    FontValidator.with_budget(fn ->
      case font do
        %{id: id, data: data} when is_binary(data) ->
          {cached_data, result} =
            FontValidator.memo(
              {:opentype_layout, id, Map.get(font, :normalized_coordinates)},
              fn -> {data, prepare(font)} end
            )

          if cached_data == data, do: result, else: prepare(font)

        _ ->
          prepare(font)
      end
    end)
  end

  @doc "Admits a selected font's GSUB, GPOS and GDEF tables as one bounded layout model."
  @spec prepare(term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(font) do
    FontValidator.with_budget(fn ->
      case font do
        %{data: data} when is_binary(data) ->
          directory =
            case FontValidator.prepare_table_directory(data) do
              {:error, {:invalid_document, diagnostic}} ->
                Diagnostics.error(:font, :invalid_font, diagnostic.message,
                  module: Map.get(diagnostic, :module, __MODULE__),
                  operation: Map.get(diagnostic, :operation, :prepare_opentype_layout),
                  source: Map.get(diagnostic, :source)
                )

              result ->
                result
            end

          with {:ok, _, directory} <- directory,
               {:ok, coordinates} <-
                 FontVariationValidator.prepare_normalized_coordinates(
                   Map.get(font, :normalized_coordinates, [])
                 ) do
            maxp = table(data, directory, "maxp")
            if maxp == nil, do: invalid("maxp", "OpenType layout requires maxp", 0)
            glyph_count = Data.u16(%{data: maxp, tag: "maxp"}, 4)
            if glyph_count == 0, do: invalid("maxp", "font has no glyphs", 4)
            context = %{glyph_count: glyph_count, coordinates: coordinates}
            gdef = definition(table(data, directory, "GDEF"), context)
            context = Map.put(context, :gdef, gdef)
            gsub = layout(table(data, directory, "GSUB"), "GSUB", context)
            gpos = layout(table(data, directory, "GPOS"), "GPOS", context)

            sequences =
              NativeElixirPdfUtilities.Validators.OpenTypeCmapValidator.prepare_sequences(
                table(data, directory, "cmap"),
                glyph_count
              )

            vertical_tables =
              Map.new(~w(vhea vmtx VORG), &{&1, table(data, directory, &1)})
              |> Map.put(:true_type?, Map.has_key?(directory, "glyf"))
              |> Map.put(
                :outlines?,
                Enum.any?(["glyf", "CFF ", "CFF2"], &Map.has_key?(directory, &1))
              )

            with {:ok, vertical} <-
                   NativeElixirPdfUtilities.Validators.OpenTypeVerticalValidator.prepare(
                     vertical_tables,
                     glyph_count
                   ),
                 {:ok, baselines} <-
                   NativeElixirPdfUtilities.Validators.FontBaselineValidator.prepare(
                     table(data, directory, "BASE"),
                     context
                   ) do
              {:ok,
               %{
                 baselines: baselines,
                 vertical: vertical,
                 gsub: gsub,
                 gpos: gpos,
                 gdef: gdef,
                 sequences: sequences,
                 glyph_count: glyph_count,
                 coordinates: coordinates
               }}
            end
          end

        _ ->
          Diagnostics.error(:font, :invalid_font, "OpenType layout requires selected font bytes",
            operation: :prepare_opentype_layout,
            module: __MODULE__
          )
      end
    end)
  catch
    {:invalid_open_type, error} -> error
  end

  defp table(data, directory, tag) do
    case Map.get(directory, tag) do
      nil -> nil
      {offset, size} -> binary_part(data, offset, size)
    end
  end

  defp layout(data, tag, shared) do
    if data == nil do
      nil
    else
      context = Map.merge(shared, %{data: data, tag: tag})
      <<major::16, minor::16, scripts::16, features::16, lookups::16>> = Data.read(context, 0, 10)

      if major != 1 or minor not in [0, 1],
        do: Data.invalid(context, "unsupported OpenType layout version", 0)

      header = if minor == 1, do: 14, else: 10
      features_base = Data.offset(context, 0, features, header)
      lookups_base = Data.offset(context, 0, lookups, header)
      lookup_count = Data.u16(context, lookups_base)
      features = features(context, features_base, lookup_count)
      scripts = scripts(context, Data.offset(context, 0, scripts, header), tuple_size(features))
      lookups = lookups(context, lookups_base, lookup_count)

      variations =
        if minor == 1 do
          offset = Data.u32(context, 10)

          if offset == 0,
            do: [],
            else:
              feature_variations(
                context,
                Data.offset(context, 0, offset, header),
                tuple_size(features),
                lookup_count
              )
        else
          []
        end

      %{scripts: scripts, features: features, lookups: lookups, variations: variations}
    end
  end

  defp tagged_records(context, base, strict?) do
    count = Data.u16(context, base)
    raw = Data.read(context, base + 2, count * 6)

    {records, _} =
      Enum.map_reduce(
        for(<<tag::binary-size(4), offset::16 <- raw>>, do: {tag, offset}),
        nil,
        fn {tag, offset}, previous ->
          if not Enum.all?(:binary.bin_to_list(tag), &(&1 in 32..126)),
            do: Data.invalid(context, "layout tag requires four printable ASCII bytes", base)

          if previous != nil and (tag < previous or (strict? and tag == previous)),
            do: Data.invalid(context, "layout tags are unordered or repeated", base)

          {{tag, Data.offset(context, base, offset, 2 + count * 6)}, tag}
        end
      )

    records
  end

  defp features(context, base, lookup_count) do
    for {tag, offset} <- tagged_records(context, base, false) do
      <<params::16, count::16>> = Data.read(context, offset, 4)
      indices = Data.words(context, offset + 4, count)

      if Enum.any?(indices, &(&1 >= lookup_count)),
        do: Data.invalid(context, "feature references an unavailable lookup", offset)

      params = if params == 0, do: nil, else: Data.offset(context, offset, params, 4 + count * 2)
      %{tag: tag, lookups: indices, params: params}
    end
    |> List.to_tuple()
  end

  defp scripts(context, base, feature_count) do
    for {tag, offset} <- tagged_records(context, base, true), into: %{} do
      <<default::16, count::16>> = Data.read(context, offset, 4)
      # Script's language records follow its nullable default offset.
      records = Data.read(context, offset + 4, count * 6)
      languages = for <<tag::binary-size(4), value::16 <- records>>, do: {tag, value}
      tags = Enum.map(languages, &elem(&1, 0))

      if tags != Enum.sort(Enum.uniq(tags)) or
           Enum.any?(tags, fn tag -> Enum.any?(:binary.bin_to_list(tag), &(&1 not in 32..126)) end),
         do: Data.invalid(context, "language tags are unordered or repeated", offset)

      minimum = 4 + count * 6

      languages =
        Map.new(languages, fn {name, value} ->
          {name, language(context, Data.offset(context, offset, value, minimum), feature_count)}
        end)

      default =
        if default == 0,
          do: nil,
          else: language(context, Data.offset(context, offset, default, minimum), feature_count)

      {tag, %{default: default, languages: languages}}
    end
  end

  defp language(context, base, feature_count) do
    <<order::16, required::16, count::16>> = Data.read(context, base, 6)
    features = Data.words(context, base + 6, count)
    if order != 0, do: Data.invalid(context, "LangSys lookup-order offset is reserved", base)

    if (required != 65535 and required >= feature_count) or
         Enum.any?(features, &(&1 >= feature_count)),
       do: Data.invalid(context, "language system references an unavailable feature", base)

    %{required: if(required == 65535, do: nil, else: required), features: features}
  end

  defp lookups(context, base, count) do
    offsets = Data.words(context, base + 2, count)

    for offset <- offsets do
      offset = Data.offset(context, base, offset, 2 + count * 2)
      <<kind::16, flags::16, subtables::16>> = Data.read(context, offset, 6)
      maximum = if context.tag == "GSUB", do: 8, else: 9

      if kind not in 1..maximum,
        do: Data.invalid(context, "unsupported layout lookup type", offset)

      if (flags &&& 224) != 0, do: Data.invalid(context, "lookup flag uses reserved bits", offset)
      use_marks = (flags &&& 16) != 0
      minimum = 6 + subtables * 2 + if(use_marks, do: 2, else: 0)
      mark_set = if use_marks, do: Data.u16(context, offset + 6 + subtables * 2), else: nil

      if mark_set != nil and mark_set >= tuple_size(context.gdef.mark_sets),
        do: Data.invalid(context, "lookup references an unavailable mark filtering set", offset)

      subtables = Data.words(context, offset + 6, subtables)

      parsed =
        for relative <- subtables do
          subtable(context, kind, Data.offset(context, offset, relative, minimum), count)
        end

      extension = if context.tag == "GSUB", do: 7, else: 9

      if kind == extension and length(Enum.uniq_by(parsed, & &1.kind)) > 1,
        do: Data.invalid(context, "extension subtables must use the same lookup type", offset)

      %{type: kind, flags: flags, mark_set: mark_set, subtables: parsed}
    end
    |> List.to_tuple()
  end

  defp subtable(context, kind, base, count) do
    extension = if context.tag == "GSUB", do: 7, else: 9

    {kind, base} =
      if kind == extension do
        <<format::16, target::16, offset::32>> = Data.read(context, base, 8)

        if format != 1 or target == extension,
          do: Data.invalid(context, "invalid extension lookup format or nested extension", base)

        {target, Data.offset(context, base, offset, 8)}
      else
        {kind, base}
      end

    if context.tag == "GSUB",
      do:
        NativeElixirPdfUtilities.Validators.OpenTypeSubstitutionValidator.prepare(
          context,
          kind,
          base,
          count
        ),
      else:
        NativeElixirPdfUtilities.Validators.OpenTypePositioningValidator.prepare(
          context,
          kind,
          base,
          count
        )
  end

  defp feature_variations(context, base, feature_count, lookup_count) do
    <<major::16, minor::16, count::32>> = Data.read(context, base, 8)

    if {major, minor} != {1, 0},
      do: Data.invalid(context, "unsupported FeatureVariations version", base)

    raw = Data.read(context, base + 8, count * 8)

    for <<condition::32, substitution::32 <- raw>> do
      conditions_base = Data.offset(context, base, condition, 8 + count * 8)
      conditions_count = Data.u16(context, conditions_base)
      raw = Data.read(context, conditions_base + 2, conditions_count * 4)

      conditions =
        for <<offset::32 <- raw>> do
          offset = Data.offset(context, conditions_base, offset, 2 + conditions_count * 4)

          <<format::16, axis::16, low::signed-16, high::signed-16>> =
            Data.read(context, offset, 8)

          if format != 1 or axis >= length(context.coordinates) or low < -16384 or high > 16384 or
               low > high,
             do:
               Data.invalid(
                 context,
                 "invalid variation condition axis, format or normalized range",
                 offset
               )

          {axis, low / 16384, high / 16384}
        end

      substitutions = Data.offset(context, base, substitution, 8 + count * 8)
      <<major::16, minor::16, records::16>> = Data.read(context, substitutions, 6)

      if {major, minor} != {1, 0},
        do: Data.invalid(context, "unsupported FeatureTableSubstitution version", substitutions)

      raw = Data.read(context, substitutions + 6, records * 6)

      Data.ordered(
        context,
        for(<<index::16, _offset::32 <- raw>>, do: index),
        "variation feature indices are unordered or repeated",
        substitutions
      )

      for <<index::16, offset::32 <- raw>> do
        if index >= feature_count,
          do:
            Data.invalid(
              context,
              "variation substitution references an unavailable feature",
              substitutions
            )

        target = Data.offset(context, substitutions, offset, 6 + records * 6)
        <<params::16, count::16>> = Data.read(context, target, 4)
        if params != 0, do: Data.offset(context, target, params, 4 + count * 2)
        lookups = Data.words(context, target + 4, count)

        if Enum.any?(lookups, &(&1 >= lookup_count)),
          do: Data.invalid(context, "variation feature references an unavailable lookup", target)

        {index, lookups}
      end
      |> then(&%{conditions: conditions, substitutions: Map.new(&1)})
    end
  end

  defp definition(data, shared) do
    empty = %{
      classes: %{},
      mark_classes: %{},
      mark_sets: {},
      attachments: %{},
      carets: %{},
      store: {}
    }

    if data == nil do
      empty
    else
      context = Map.merge(shared, %{data: data, tag: "GDEF"})

      <<major::16, minor::16, glyphs::16, attachments::16, carets::16, marks::16>> =
        Data.read(context, 0, 12)

      if major != 1 or minor not in [0, 2, 3],
        do: Data.invalid(context, "unsupported GDEF version", 0)

      minimum = %{0 => 12, 2 => 14, 3 => 18}[minor]

      optional_class = fn offset, bound ->
        if offset == 0,
          do: %{},
          else: Data.classes(context, Data.offset(context, 0, offset, minimum), bound)
      end

      sets = if minor >= 2, do: Data.u16(context, 12), else: 0

      sets =
        if sets == 0 do
          {}
        else
          base = Data.offset(context, 0, sets, minimum)
          <<format::16, count::16>> = Data.read(context, base, 4)
          if format != 1, do: Data.invalid(context, "unsupported mark glyph set format", base)
          raw = Data.read(context, base + 4, count * 4)

          for(<<offset::32 <- raw>>,
            do:
              Data.coverage(context, Data.offset(context, base, offset, 4 + count * 4))
              |> MapSet.new()
          )
          |> List.to_tuple()
        end

      store_offset = if minor == 3, do: Data.u32(context, 14), else: 0

      store =
        if store_offset == 0 do
          {}
        else
          case VariationStoreValidator.prepare(
                 data,
                 Data.offset(context, 0, store_offset, minimum),
                 context.coordinates
               ) do
            {:ok, store} -> store
            error -> throw({:invalid_open_type, error})
          end
        end

      attachments = attachment_points(context, attachments, minimum)
      carets = ligature_carets(Map.put(context, :gdef, %{store: store}), carets, minimum)

      %{
        empty
        | classes: optional_class.(glyphs, 5),
          mark_classes: optional_class.(marks, 256),
          mark_sets: sets,
          store: store,
          attachments: attachments,
          carets: carets
      }
    end
  end

  defp attachment_points(context, offset, minimum) do
    if offset == 0 do
      %{}
    else
      base = Data.offset(context, 0, offset, minimum)
      <<coverage::16, count::16>> = Data.read(context, base, 4)
      covered = Data.coverage(context, Data.offset(context, base, coverage, 4 + count * 2))

      if length(covered) != count,
        do: Data.invalid(context, "attachment list count differs from coverage", base)

      for {glyph, offset} <- Enum.zip(covered, Data.words(context, base + 4, count)), into: %{} do
        target = Data.offset(context, base, offset, 4 + count * 2)
        points = Data.words(context, target + 2, Data.u16(context, target))

        Data.ordered(
          context,
          points,
          "attachment point indices are unordered or repeated",
          target
        )

        {glyph, points}
      end
    end
  end

  defp ligature_carets(context, offset, minimum) do
    if offset == 0 do
      %{}
    else
      base = Data.offset(context, 0, offset, minimum)
      <<coverage::16, count::16>> = Data.read(context, base, 4)
      covered = Data.coverage(context, Data.offset(context, base, coverage, 4 + count * 2))

      if length(covered) != count,
        do: Data.invalid(context, "ligature caret count differs from coverage", base)

      for {glyph, offset} <- Enum.zip(covered, Data.words(context, base + 4, count)), into: %{} do
        target = Data.offset(context, base, offset, 4 + count * 2)
        count = Data.u16(context, target)

        values =
          for offset <- Data.words(context, target + 2, count) do
            record = Data.offset(context, target, offset, 2 + count * 2)

            case Data.u16(context, record) do
              1 ->
                %{value: Data.s16(context, record + 2), point: nil, device: nil}

              2 ->
                %{value: nil, point: Data.u16(context, record + 2), device: nil}

              3 ->
                %{
                  value: Data.s16(context, record + 2),
                  point: nil,
                  device: Data.device(context, record, Data.u16(context, record + 4))
                }

              _ ->
                Data.invalid(context, "unsupported ligature caret format", record)
            end
          end

        {glyph, values}
      end
    end
  end

  @spec invalid(String.t(), String.t(), integer()) :: no_return()
  defp invalid(tag, message, offset), do: Data.invalid(%{tag: tag}, message, offset)
end
