defmodule NativeElixirPdfUtilities.Validators.FontBaselineValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.{FontValidator, VariationStoreValidator}
  alias NativeElixirPdfUtilities.Validators.OpenTypeDataValidator, as: Data

  @doc "Admits an OpenType BASE 1.0/1.1 table using the selected font's glyph count and normalized coordinates."
  @spec prepare(binary() | nil, map()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(data, shared) do
    FontValidator.with_budget(fn ->
      case data do
        nil ->
          {:ok, %{horizontal: nil, vertical: nil}}

        data when is_binary(data) ->
          context = Map.merge(shared, %{data: data, tag: "BASE", module: __MODULE__})
          <<major::16, minor::16, horizontal::16, vertical::16>> = Data.read(context, 0, 8)

          if major != 1 or minor not in [0, 1],
            do: Data.invalid(context, "unsupported BASE version", 0)

          minimum = if minor == 0, do: 8, else: 12
          store_offset = if minor == 1, do: Data.u32(context, 8), else: 0

          store =
            if store_offset == 0 do
              nil
            else
              offset = Data.offset(context, 0, store_offset, minimum)

              case VariationStoreValidator.prepare(data, offset, shared.coordinates) do
                {:ok, store} ->
                  store

                {:error, {reason, diagnostic}} ->
                  throw(
                    {:invalid_open_type,
                     {:error,
                      {reason,
                       Diagnostics.with_context(
                         Map.put(
                           diagnostic,
                           :source,
                           "BASE byte #{offset}: #{Map.get(diagnostic, :source, "item variation store")}"
                         ),
                         operation: :prepare_opentype_layout,
                         module: __MODULE__
                       )}}}
                  )
              end
            end

          # BASE owns its variation store. A VariationIndex here must never
          # accidentally read the font's unrelated GDEF store.
          context = Map.put(context, :gdef, if(store == nil, do: nil, else: %{store: store}))

          {:ok,
           %{
             horizontal: axis(context, horizontal, minimum),
             vertical: axis(context, vertical, minimum)
           }}

        _ ->
          Diagnostics.error(:font, :invalid_font, "BASE requires selected table bytes",
            module: __MODULE__,
            operation: :prepare_opentype_layout,
            source: "BASE"
          )
      end
    end)
  catch
    {:invalid_open_type, error} -> error
  end

  @doc "Resolves selected script baselines in design units, reusing the font's admitted layout and variation store."
  @spec coordinates(map(), :horizontal | :vertical, [String.t()]) ::
          {:ok, map() | nil} | {:error, {atom(), map()}}
  @spec coordinates(map(), :horizontal | :vertical, [String.t()], number() | nil) ::
          {:ok, map() | nil} | {:error, {atom(), map()}}
  def coordinates(font, axis, scripts, ppem \\ nil) do
    FontValidator.with_budget(fn ->
      valid? =
        axis in [:horizontal, :vertical] and is_list(scripts) and
          Enum.all?(scripts, &(is_binary(&1) and byte_size(&1) == 4)) and
          (ppem == nil or (is_number(ppem) and ppem > 0))

      if valid? do
        FontValidator.check(:max_entries, length(scripts))
        FontValidator.reserve(:max_work, length(scripts))
        if ppem != nil, do: FontValidator.check(:max_numeric_magnitude, ppem)

        case font do
          %{type: :built_in} ->
            {:ok, nil}

          %{type: :embedded, data: data, units_per_em: units}
          when is_binary(data) and is_integer(units) and units > 0 ->
            with {:ok, layout} <-
                   NativeElixirPdfUtilities.Validators.OpenTypeLayoutValidator.for_font(font) do
              table = Map.get(layout.baselines, axis)
              script = if table != nil, do: Enum.find_value(scripts, &Map.get(table.scripts, &1))

              if script == nil do
                {:ok, nil}
              else
                values =
                  Map.new(script.values, fn {tag, value} ->
                    {tag, resolved_coordinate(value, ppem, units)}
                  end)

                extents = resolved_extents(script.extents, ppem, units)

                languages =
                  Map.new(script.languages, fn {tag, extents} ->
                    {tag, resolved_extents(extents, ppem, units)}
                  end)

                {:ok,
                 %{
                   default: script.default,
                   values: values,
                   extents: extents,
                   languages: languages
                 }}
              end
            end

          _ ->
            Diagnostics.error(:font, :invalid_font, "baseline metrics require a selected font",
              operation: :prepare_opentype_layout,
              module: __MODULE__,
              source: "BASE"
            )
        end
      else
        Diagnostics.error(
          :font,
          :invalid_font,
          "baseline selection requires an axis, four-byte script tags and an optional positive pixel size",
          operation: :prepare_opentype_layout,
          module: __MODULE__,
          source: "BASE"
        )
      end
    end)
  end

  defp resolved_coordinate(record, ppem, units) do
    if record == nil,
      do: nil,
      else: record.value + Data.device_adjustment(record.device, ppem, units)
  end

  defp resolved_extents(extents, ppem, units) do
    if extents == nil do
      nil
    else
      %{
        min: resolved_coordinate(extents.min, ppem, units),
        max: resolved_coordinate(extents.max, ppem, units),
        features:
          Map.new(extents.features, fn {tag, record} ->
            {tag,
             %{
               min: resolved_coordinate(record.min, ppem, units),
               max: resolved_coordinate(record.max, ppem, units)
             }}
          end)
      }
    end
  end

  defp axis(context, offset, minimum) do
    if offset == 0 do
      nil
    else
      base = Data.offset(context, 0, offset, minimum)
      <<tags_offset::16, scripts_offset::16>> = Data.read(context, base, 4)

      tags =
        if tags_offset == 0 do
          []
        else
          target = Data.offset(context, base, tags_offset, 4)
          count = Data.u16(context, target)
          FontValidator.reserve(:max_entries, count)
          tags = for <<tag::binary-size(4) <- Data.read(context, target + 2, count * 4)>>, do: tag
          tag_order(context, tags, target)
          tags
        end

      target = Data.offset(context, base, scripts_offset, 4)
      count = Data.u16(context, target)
      FontValidator.reserve(:max_entries, count)

      records =
        for <<tag::binary-size(4), offset::16 <- Data.read(context, target + 2, count * 6)>>,
          do: {tag, offset}

      tag_order(context, Enum.map(records, &elem(&1, 0)), target)

      scripts =
        Map.new(records, fn {tag, offset} ->
          script_base = Data.offset(context, target, offset, 2 + count * 6)
          <<values_offset::16, minmax_offset::16, count::16>> = Data.read(context, script_base, 6)
          FontValidator.reserve(:max_entries, count)

          languages =
            for <<language::binary-size(4),
                  offset::16 <- Data.read(context, script_base + 6, count * 6)>>,
                do: {language, offset}

          tag_order(context, Enum.map(languages, &elem(&1, 0)), script_base)
          minimum = 6 + count * 6

          values =
            if values_offset == 0 do
              %{default: nil, values: %{}}
            else
              values_base = Data.offset(context, script_base, values_offset, minimum)
              <<default::16, count::16>> = Data.read(context, values_base, 4)

              if count != length(tags) or (count > 0 and default >= count) or
                   (count == 0 and default != 0),
                 do:
                   Data.invalid(
                     context,
                     "BASE default or coordinate count differs from baseline tags",
                     values_base
                   )

              FontValidator.reserve(:max_entries, count)

              coordinates =
                Enum.map(Data.words(context, values_base + 4, count), fn offset ->
                  if offset == 0,
                    do:
                      Data.invalid(
                        context,
                        "required BASE coordinate offset is null",
                        values_base
                      )

                  coordinate(context, values_base, offset, 4 + count * 2)
                end)

              %{default: Enum.at(tags, default), values: Map.new(Enum.zip(tags, coordinates))}
            end

          Map.merge(values, %{
            extents: minmax(context, script_base, minmax_offset, minimum),
            languages:
              Map.new(languages, fn {language, offset} ->
                if offset == 0,
                  do: Data.invalid(context, "BASE language extent offset is null", script_base)

                {language, minmax(context, script_base, offset, minimum)}
              end)
          })
          |> then(&{tag, &1})
        end)

      %{tags: tags, scripts: scripts}
    end
  end

  defp minmax(context, parent, offset, minimum) do
    if offset == 0 do
      nil
    else
      base = Data.offset(context, parent, offset, minimum)
      <<lower::16, upper::16, count::16>> = Data.read(context, base, 6)
      FontValidator.reserve(:max_entries, count)

      records =
        for <<tag::binary-size(4), lower::16,
              upper::16 <- Data.read(context, base + 6, count * 8)>>,
            do: {tag, lower, upper}

      tag_order(context, Enum.map(records, &elem(&1, 0)), base)
      minimum = 6 + count * 8

      %{
        min: coordinate(context, base, lower, minimum),
        max: coordinate(context, base, upper, minimum),
        features:
          Map.new(records, fn {tag, lower, upper} ->
            {tag,
             %{
               min: coordinate(context, base, lower, minimum),
               max: coordinate(context, base, upper, minimum)
             }}
          end)
      }
    end
  end

  defp coordinate(context, parent, offset, minimum) do
    if offset == 0 do
      nil
    else
      base = Data.offset(context, parent, offset, minimum)
      <<format::16, value::signed-16>> = Data.read(context, base, 4)

      case format do
        1 ->
          %{value: value, glyph: nil, point: nil, device: nil}

        2 ->
          <<glyph::16, point::16>> = Data.read(context, base + 4, 4)
          glyph = Data.glyph(context, glyph, base + 4)
          # Native layout uses the unhinted design coordinate. Retain the
          # reference point for a rasterizer that applies TrueType grid fitting.
          %{value: value, glyph: glyph, point: point, device: nil}

        3 ->
          offset = Data.u16(context, base + 4)
          if offset != 0, do: Data.offset(context, base, offset, 6)
          device = Data.device(context, base, offset)

          if is_map(device) and Map.has_key?(device, :variation_index) and
               not Map.has_key?(device, :delta),
             do:
               Data.invalid(
                 context,
                 "BASE variation index requires its local item variation store",
                 base
               )

          %{value: value, glyph: nil, point: nil, device: device}

        _ ->
          Data.invalid(context, "unsupported BASE coordinate format", base)
      end
    end
  end

  defp tag_order(context, tags, offset) do
    Data.ordered(
      context,
      Enum.map(tags, &:binary.decode_unsigned/1),
      "BASE tags must be unique and alphabetically ordered",
      offset
    )
  end
end
