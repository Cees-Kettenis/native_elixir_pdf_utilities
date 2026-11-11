defmodule NativeElixirPdfUtilities.Validators.FontVariationValidator do
  @moduledoc false
  import Bitwise
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.Font.{Sfnt, TrueTypeGlyph}

  alias NativeElixirPdfUtilities.Validators.{
    FontValidator,
    CollectionValidator,
    GvarValidator,
    TrueTypeSubsetValidator,
    VariationStoreValidator
  }

  @mvar %{
          "hasc" => {"OS/2", 68},
          "hdsc" => {"OS/2", 70},
          "hlgp" => {"OS/2", 72},
          "hcla" => {"OS/2", 74},
          "hcld" => {"OS/2", 76},
          "vasc" => {"vhea", 4},
          "vdsc" => {"vhea", 6},
          "vlgp" => {"vhea", 8},
          "hcrs" => {"hhea", 18},
          "hcrn" => {"hhea", 20},
          "hcof" => {"hhea", 22},
          "vcrs" => {"vhea", 18},
          "vcrn" => {"vhea", 20},
          "vcof" => {"vhea", 22},
          "xhgt" => {"OS/2", 86},
          "cpht" => {"OS/2", 88},
          "sbxs" => {"OS/2", 10},
          "sbys" => {"OS/2", 12},
          "sbxo" => {"OS/2", 14},
          "sbyo" => {"OS/2", 16},
          "spxs" => {"OS/2", 18},
          "spys" => {"OS/2", 20},
          "spxo" => {"OS/2", 22},
          "spyo" => {"OS/2", 24},
          "strs" => {"OS/2", 26},
          "stro" => {"OS/2", 28},
          "unds" => {"post", 10},
          "undo" => {"post", 8}
        }
        |> Map.merge(Map.new(0..9, fn index -> {"gsp#{index}", {"gasp", 4 + index * 4}} end))

  @doc "Validates explicit axis coordinates and an optional zero-based named-instance index."
  @spec prepare_request(term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_request(settings, instance) do
    FontValidator.with_budget(fn ->
      try do
        settings =
          cond do
            is_map(settings) ->
              settings

            is_list(settings) ->
              case CollectionValidator.bounded_length(
                     settings,
                     Limits.get(:max_font_variation_work)
                   ) do
                {:ok, count} ->
                  FontValidator.reserve(:max_font_variation_work, count)
                  Map.new(settings)

                :invalid ->
                  invalid("variation settings must be a proper list of axis/value pairs")

                :limit_exceeded ->
                  FontValidator.check(
                    :max_font_variation_work,
                    Limits.get(:max_font_variation_work) + 1
                  )
              end

            true ->
              invalid("variation settings must be a map of four-byte axis tags to numbers")
          end

        unless (instance == nil or (is_integer(instance) and instance >= 0) or
                  (is_binary(instance) and String.valid?(instance))) and
                 Enum.all?(settings, fn {tag, value} ->
                   is_binary(tag) and byte_size(tag) == 4 and
                     Enum.all?(:binary.bin_to_list(tag), &(&1 in 32..126)) and is_number(value)
                 end),
               do:
                 invalid(
                   "variation coordinates require printable four-byte tags, finite numbers and a nonnegative named-instance index"
                 )

        FontValidator.reserve(:max_font_variation_work, map_size(settings))
        {:ok, %{settings: settings, instance: instance}}
      rescue
        ArgumentError -> failure("variation settings must contain axis/value pairs")
      catch
        {:invalid_instance, error} -> error
      end
    end)
  end

  @doc "Admits a proper, bounded list of normalized OpenType axis coordinates for all variation consumers."
  @spec prepare_normalized_coordinates(term()) :: {:ok, [number()]} | {:error, {atom(), map()}}
  def prepare_normalized_coordinates(coordinates) do
    FontValidator.with_budget(fn ->
      try do
        case CollectionValidator.bounded_length(coordinates, Limits.get(:max_font_variation_work)) do
          {:ok, count} ->
            FontValidator.reserve(:max_font_variation_work, count)

          :invalid ->
            invalid("normalized coordinates must be a proper list")

          :limit_exceeded ->
            FontValidator.check(
              :max_font_variation_work,
              Limits.get(:max_font_variation_work) + 1
            )
        end

        unless Enum.all?(coordinates, &(is_number(&1) and &1 >= -1 and &1 <= 1)),
          do: invalid("variation coordinates must be finite normalized values between -1 and 1")

        {:ok, coordinates}
      catch
        {:invalid_instance, error} -> error
      end
    end)
  end

  @doc "Instantiates a bounded variable sfnt face as static bytes, retaining the selected normalized coordinates."
  @spec prepare(term(), term(), term()) :: {:ok, binary(), map()} | {:error, {atom(), map()}}
  def prepare(data, settings \\ %{}, instance \\ nil) do
    FontValidator.with_budget(fn ->
      try do
        if is_binary(data), do: FontValidator.reserve_source(data)

        with {:ok, request} <- prepare_request(settings, instance),
             {:ok, scaler, directory} <- FontValidator.prepare_table_directory(data) do
          tables =
            Map.new(directory, fn {tag, {offset, size}} ->
              {tag, binary_part(data, offset, size)}
            end)

          axes_and_instances =
            case Map.get(tables, "fvar") do
              nil -> nil
              fvar -> axes(fvar)
            end

          # OpenType fvar requires zero-axis fonts to behave as static fonts;
          # variation-specific tables are ignored, preserving source hinting.
          inactive? = axes_and_instances == {[], []}

          tables =
            if inactive?,
              do: Map.drop(tables, ~w(fvar avar gvar cvar HVAR VVAR MVAR STAT DSIG)),
              else: tables

          data = if inactive?, do: rebuild(scaler, tables), else: data

          {normalized, metadata} =
            if axes_and_instances == nil or inactive? do
              if instance != nil, do: invalid("a named instance requires an fvar table")
              {[], %{}}
            else
              {axes, instances} = axes_and_instances

              selected =
                cond do
                  instance == nil ->
                    %{}

                  is_binary(instance) ->
                    found =
                      Enum.find(instances, fn candidate ->
                        case FontValidator.prepare_name_strings(data, candidate.name_id) do
                          {:ok, names} ->
                            Enum.any?(names, &(String.downcase(&1) == String.downcase(instance)))

                          _ ->
                            false
                        end
                      end)

                    if found == nil, do: %{}, else: found.coordinates

                  true ->
                    unless instance < length(instances),
                      do: invalid("named-instance index is outside fvar")

                    Enum.at(instances, instance).coordinates
                end

              settings = Map.merge(selected, request.settings)

              selected =
                Map.new(axes, fn axis ->
                  {axis.tag,
                   min(axis.maximum, max(axis.minimum, Map.get(settings, axis.tag, axis.default)))}
                end)

              normalized =
                Enum.map(axes, fn axis ->
                  value = selected[axis.tag]

                  cond do
                    value == axis.default -> 0
                    value < axis.default -> (value - axis.default) / (axis.default - axis.minimum)
                    true -> (value - axis.default) / (axis.maximum - axis.default)
                  end
                end)

              normalized =
                avar(Map.get(tables, "avar"), normalized)
                |> Enum.map(&(GvarValidator.ot_round(&1 * 16384) / 16384))

              {normalized,
               %{
                 variation_source: data,
                 variation_axes: axes,
                 variation_instances: instances,
                 variation_coordinates: selected,
                 normalized_coordinates: normalized
               }}
            end

          if metadata == %{} and not Map.has_key?(tables, "CFF2") do
            {:ok, data, %{}}
          else
            source_tables = tables

            with {:ok, tables} <- true_type(data, tables, normalized),
                 {:ok, tables} <- metric_variations(tables, source_tables, normalized),
                 {:ok, tables} <- cff2(tables, normalized),
                 {:ok, tables} <- global_metrics(tables, normalized) do
              tables =
                Map.drop(tables, [
                  "fvar",
                  "avar",
                  "gvar",
                  "cvar",
                  "HVAR",
                  "VVAR",
                  "MVAR",
                  "STAT",
                  "DSIG"
                ])

              {:ok, rebuild(scaler, tables), metadata}
            end
          end
        end
      rescue
        ArithmeticError ->
          failure("font variation interpolation exceeds a finite representable result")
      catch
        {:invalid_instance, error} -> error
        {:invalid_glyph, error} -> error
      end
    end)
  end

  defp axes(data) do
    <<version::32, offset::16, reserved::16, count::16, axis_size::16, instance_count::16,
      instance_size::16>> = read(data, 0, 16, "fvar header")

    unless version == 0x0001_0000 and reserved == 2 and axis_size >= 20 and
             (count == 0 or instance_size in [4 + count * 4, 6 + count * 4]) and offset >= 16,
           do: invalid("fvar version, axis/instance sizes or reserved field are invalid")

    if count == 0 do
      {[], []}
    else
      FontValidator.reserve(:max_font_variation_work, count + instance_count)
      records = read(data, offset, count * axis_size, "fvar axes")

      axes =
        for <<record::binary-size(^axis_size) <- records>> do
          <<tag::binary-size(4), minimum::signed-32, default::signed-32, maximum::signed-32,
            flags::16, name::16, _::binary>> = record

          unless minimum <= default and default <= maximum and
                   (flags &&& 0xFFFE) == 0 and name in 256..32767 and
                   Enum.all?(:binary.bin_to_list(tag), &(&1 in 32..126)),
                 do: invalid("fvar axis tag, range, flags or name ID is invalid")

          %{
            tag: tag,
            minimum: minimum / 65536,
            default: default / 65536,
            maximum: maximum / 65536,
            hidden?: (flags &&& 1) != 0,
            name_id: name
          }
        end

      unless length(Enum.uniq_by(axes, & &1.tag)) == count,
        do: invalid("fvar axis tags must be unique")

      records =
        read(data, offset + count * axis_size, instance_count * instance_size, "fvar instances")

      instances =
        for <<record::binary-size(^instance_size) <- records>> do
          <<name::16, flags::16, raw::binary-size(^count * 4), optional::binary>> = record
          postscript = if optional == <<>>, do: 65535, else: :binary.decode_unsigned(optional)

          unless (name in [2, 17] or name in 256..32767) and flags == 0 and
                   (postscript in [6, 65535] or postscript in 256..32767),
                 do: invalid("fvar instance name ID or flags are invalid")

          values = for <<value::signed-32 <- raw>>, do: value / 65536

          unless Enum.zip(values, axes)
                 |> Enum.all?(fn {value, axis} ->
                   value >= axis.minimum and value <= axis.maximum
                 end),
                 do: invalid("fvar named-instance coordinate is outside its axis")

          %{
            name_id: name,
            coordinates:
              Enum.zip(axes, values) |> Map.new(fn {axis, value} -> {axis.tag, value} end)
          }
        end

      {axes, instances}
    end
  end

  defp rebuild(scaler, tables) do
    head = required(tables, "head") |> replace(8, <<0::32>>)
    tables = Map.put(tables, "head", head)

    size =
      12 + map_size(tables) * 16 +
        Enum.reduce(tables, 0, fn {_, bytes}, sum ->
          sum + byte_size(bytes) + rem(4 - rem(byte_size(bytes), 4), 4)
        end)

    FontValidator.check(:max_font_decoded_bytes, size)
    FontValidator.reserve(:max_font_variation_work, size)
    Sfnt.rebuild(scaler, Enum.sort(tables))
  end

  defp avar(data, coordinates) do
    if data == nil do
      coordinates
    else
      <<major::16, minor::16, reserved::16, count::16>> = read(data, 0, 8, "avar header")

      unless major in [1, 2] and minor == 0 and reserved == 0 and count == length(coordinates),
        do: invalid("avar version, reserved field or axis count is invalid")

      {mapped, position} =
        Enum.map_reduce(coordinates, 8, fn value, position ->
          <<n::16>> = read(data, position, 2, "avar segment count")
          unless n >= 3, do: invalid("avar segment map requires its three identity anchors")

          [first_pair | _] =
            pairs =
            for <<a::signed-16,
                  b::signed-16 <- read(data, position + 2, n * 4, "avar segment map")>>,
                do: {a / 16384, b / 16384}

          unless first_pair == {-1.0, -1.0} and List.last(pairs) == {1.0, 1.0} and
                   {0.0, 0.0} in pairs and
                   Enum.chunk_every(pairs, 2, 1, :discard)
                   |> Enum.all?(fn [{a, b}, {c, d}] -> a < c and b <= d end),
                 do:
                   invalid("avar segment map must have ordered coordinates and identity anchors")

          output =
            Enum.find_value(Enum.chunk_every(pairs, 2, 1, :discard), fn [{a, b}, {c, d}] ->
              if value >= a and value <= c, do: b + (value - a) * (d - b) / (c - a)
            end)

          {output, position + 2 + n * 4}
        end)

      if major == 1 do
        mapped
      else
        <<map_offset::32, store_offset::32>> = read(data, position, 8, "avar v2 offsets")

        if store_offset == 0 do
          if map_offset != 0, do: invalid("avar v2 index map requires an item variation store")
          mapped
        else
          store = unwrap(VariationStoreValidator.prepare(data, store_offset, mapped))

          deltas =
            unwrap(VariationStoreValidator.prepare_map(data, map_offset, store, count, true))

          Enum.zip(mapped, deltas)
          |> Enum.map(fn {value, delta} -> min(1, max(-1, value + delta / 16384)) end)
        end
      end
    end
  end

  defp true_type(data, tables, coordinates) do
    if not Map.has_key?(tables, "glyf") do
      {:ok, tables}
    else
      with {:ok, outlines} <- TrueTypeSubsetValidator.prepare_outlines(data) do
        glyphs = outlines.glyphs
        count = tuple_size(glyphs)
        horizontal = metrics(tables, "hhea", "hmtx", count)

        vertical =
          if Map.has_key?(tables, "vmtx"),
            do: metrics(tables, "vhea", "vmtx", count),
            else: List.duplicate({0, 0}, count)

        phantoms =
          Enum.zip([Tuple.to_list(glyphs), horizontal, vertical])
          |> Enum.map(fn {glyph, {advance, lsb}, {vadvance, tsb}} ->
            {xmin, _, _, ymax} = glyph.bbox
            {xmin - lsb, xmin - lsb + advance, ymax + tsb, ymax + tsb - vadvance}
          end)

        with {:ok, results} <-
               GvarValidator.prepare(Map.get(tables, "gvar"), glyphs, phantoms, coordinates) do
          glyphs = Enum.map(results, & &1.glyph) |> List.to_tuple()

          {glyphs, _cache} =
            Enum.map_reduce(0..(count - 1), %{}, fn id, cache ->
              NativeElixirPdfUtilities.Validators.TrueTypeGlyphValidator.resolve(
                id,
                glyphs,
                cache,
                MapSet.new(),
                0
              )
            end)

          hvar? = Map.has_key?(tables, "HVAR")
          vvar? = Map.has_key?(tables, "VVAR")

          {horizontal, vertical} =
            Enum.zip([glyphs, results, horizontal, vertical])
            |> Enum.map(fn {glyph, result, h, v} ->
              {left, right, top, bottom} = result.phantoms
              {xmin, _, _, ymax} = glyph.bbox

              {{if(hvar?, do: elem(h, 0), else: GvarValidator.ot_round(right - left)),
                GvarValidator.ot_round(xmin - left)},
               {if(vvar?, do: elem(v, 0), else: GvarValidator.ot_round(top - bottom)),
                GvarValidator.ot_round(top - ymax)}}
            end)
            |> Enum.unzip()

          {parts, offsets, finish} =
            Enum.reduce(glyphs, {[], [], 0}, fn glyph, {parts, offsets, offset} ->
              validate_glyph(glyph)
              bytes = TrueTypeGlyph.serialize(glyph)
              FontValidator.check(:max_font_decoded_bytes, offset + byte_size(bytes))
              {[bytes | parts], [offset | offsets], offset + byte_size(bytes)}
            end)

          loca = for n <- Enum.reverse([finish | offsets]), into: <<>>, do: <<n::32>>

          bbox =
            Enum.reduce(glyphs, {32767, 32767, -32768, -32768}, fn glyph, {a, b, c, d} ->
              {x, y, z, w} = glyph.bbox
              {min(a, x), min(b, y), max(c, z), max(d, w)}
            end)

          {a, b, c, d} = bbox

          head =
            required(tables, "head")
            |> replace(36, <<a::signed-16, b::signed-16, c::signed-16, d::signed-16>>)
            |> replace(50, <<1::16>>)

          tables =
            tables
            |> Map.put("glyf", IO.iodata_to_binary(Enum.reverse(parts)))
            |> Map.put("loca", loca)
            |> Map.put("head", head)
            |> Map.drop(["cvt ", "fpgm", "prep", "hdmx", "LTSH", "VDMX"])

          tables = put_metrics(tables, "hhea", "hmtx", horizontal)

          tables =
            if Map.has_key?(tables, "vmtx"),
              do: put_metrics(tables, "vhea", "vmtx", vertical),
              else: tables

          {:ok, tables}
        end
      end
    end
  end

  defp cff2(tables, coordinates) do
    case Map.get(tables, "CFF2") do
      nil ->
        {:ok, tables}

      data ->
        unless not Map.has_key?(tables, "CFF ") and not Map.has_key?(tables, "glyf") and
                 not Map.has_key?(tables, "loca"),
               do: invalid("CFF2 cannot coexist with CFF 1 or TrueType outlines")

        <<count::16>> = read(required(tables, "maxp"), 4, 2, "maxp")

        unless tables["maxp"] == <<0x0000_5000::32, count::16>> and count > 0,
          do: invalid("CFF2 OpenType requires maxp version 0.5 and a positive glyph count")

        <<units::16>> = read(required(tables, "head"), 18, 2, "head unitsPerEm")
        unless units in 16..16384, do: invalid("CFF2 requires valid OpenType unitsPerEm")

        with {:ok, face} <-
               NativeElixirPdfUtilities.Validators.CffCharstringValidator.prepare(
                 data,
                 count,
                 coordinates
               ) do
          metrics = metrics(tables, "hhea", "hmtx", count)

          {programs, bounds} =
            Enum.with_index(metrics)
            |> Enum.map(fn {{width, _bearing}, index} ->
              commands =
                NativeElixirPdfUtilities.HtmlToPdf.Font.Outline.cff_design_commands(
                  face,
                  index,
                  units
                )

              FontValidator.reserve(:max_font_subset_work, length(commands))

              bbox =
                unwrap(
                  NativeElixirPdfUtilities.Validators.CffCharstringValidator.prepare_command_bounds(
                    commands,
                    "CFF2 glyph #{index}"
                  )
                )

              program =
                NativeElixirPdfUtilities.HtmlToPdf.Font.Cff.program(
                  %{commands: commands, width: width},
                  0
                )

              {program, bbox}
            end)
            |> Enum.unzip()

          {a, b, c, d} =
            Enum.reduce(bounds, {0, 0, 0, 0}, fn {x, y, z, w}, {a, b, c, d} ->
              {min(a, x), min(b, y), max(c, z), max(d, w)}
            end)

          bbox = {floor(a), floor(b), ceil(c), ceil(d)}

          unless Enum.all?(Tuple.to_list(bbox), &(&1 in -32768..32767)),
            do: invalid("static CFF2 design bounds exceed head's signed int16 fields")

          payload = NativeElixirPdfUtilities.HtmlToPdf.Font.Cff.static_face(programs, units, bbox)
          FontValidator.check(:max_font_subset_bytes, byte_size(payload))
          {a, b, c, d} = bbox

          head =
            replace(
              required(tables, "head"),
              36,
              <<a::signed-16, b::signed-16, c::signed-16, d::signed-16>>
            )

          {:ok, tables |> Map.delete("CFF2") |> Map.put("CFF ", payload) |> Map.put("head", head)}
        end
    end
  end

  defp validate_glyph(glyph) do
    FontValidator.reserve(
      :max_font_variation_work,
      length(glyph.points) + length(glyph.components)
    )

    # Decoding owns simple point/contour counts; resolution owns the expanded
    # composite point count. Instancing preserves both admitted structures.
    if glyph.contours >= 0 do
      Enum.reduce(glyph.points, {0, 0}, fn {x, y, _}, {px, py} ->
        unless x in -32768..32767 and y in -32768..32767 and (x - px) in -32768..32767 and
                 (y - py) in -32768..32767,
               do: invalid("instantiated glyph coordinates or deltas exceed signed int16")

        {x, y}
      end)
    else
      unless Enum.all?(Tuple.to_list(glyph.bbox), &(&1 in -32768..32767)),
        do: invalid("instantiated composite bounds exceed signed int16")
    end

    Enum.each(glyph.components, fn component ->
      {x, y} = component.args
      range = if component.xy?, do: -32768..32767, else: 0..65535

      unless x in range and y in range,
        do: invalid("instantiated component arguments exceed their 16-bit fields")
    end)
  end

  defp metrics(tables, header_tag, metrics_tag, count) do
    <<number::16>> = read(required(tables, header_tag), 34, 2, header_tag)

    unless number > 0 and number <= count,
      do: invalid("#{header_tag} metric count is outside maxp glyphs")

    data = required(tables, metrics_tag)
    raw = read(data, 0, number * 4, metrics_tag)
    metrics = for <<advance::16, bearing::signed-16 <- raw>>, do: {advance, bearing}
    {last, _} = List.last(metrics)
    raw = read(data, number * 4, (count - number) * 2, metrics_tag)
    metrics ++ for(<<bearing::signed-16 <- raw>>, do: {last, bearing})
  end

  defp put_metrics(tables, header_tag, metrics_tag, metrics) do
    bytes =
      for {advance, bearing} <- metrics, into: <<>> do
        unless advance in 0..65535 and bearing in -32768..32767,
          do: invalid("instantiated #{metrics_tag} exceeds its metric fields")

        <<advance::16, bearing::signed-16>>
      end

    header = required(tables, header_tag) |> replace(34, <<length(metrics)::16>>)
    Map.put(tables, metrics_tag, bytes) |> Map.put(header_tag, header)
  end

  defp metric_variations(tables, source_tables, coordinates) do
    <<count::16>> = read(required(tables, "maxp"), 4, 2, "maxp")

    tables =
      Enum.reduce([{"HVAR", "hhea", "hmtx"}, {"VVAR", "vhea", "vmtx"}], tables, fn {tag, header,
                                                                                    mtx},
                                                                                   tables ->
        case Map.get(tables, tag) do
          nil ->
            tables

          data ->
            <<version::32, store_offset::32, advance_map::32, bearing_map::32, other_map::32>> =
              read(data, 0, 20, tag)

            unless version == 0x0001_0000, do: invalid("#{tag} version must be one")
            store = unwrap(VariationStoreValidator.prepare(data, store_offset, coordinates))

            advances =
              unwrap(VariationStoreValidator.prepare_map(data, advance_map, store, count, true))

            bearings =
              unwrap(VariationStoreValidator.prepare_map(data, bearing_map, store, count, false)) ||
                List.duplicate(0, count)

            unwrap(VariationStoreValidator.prepare_map(data, other_map, store, count, false))

            origins =
              if tag == "VVAR" do
                <<vertical_origin_map::32>> = read(data, 20, 4, tag)

                unwrap(
                  VariationStoreValidator.prepare_map(
                    data,
                    vertical_origin_map,
                    store,
                    count,
                    false
                  )
                )
              end

            tables =
              if origins != nil and Map.has_key?(source_tables, "CFF2"),
                do: vertical_origins(tables, origins, count),
                else: tables

            metrics = metrics(tables, header, mtx, count)
            original = metrics(source_tables, header, mtx, count)

            selected =
              Enum.zip([metrics, original, advances, bearings])
              |> Enum.map(fn {{_, bearing}, {advance, original_bearing}, da, db} ->
                {advance + GvarValidator.ot_round(da),
                 if(bearing_map == 0,
                   do: bearing,
                   else: original_bearing + GvarValidator.ot_round(db)
                 )}
              end)

            put_metrics(tables, header, mtx, selected)
        end
      end)

    {:ok, tables}
  end

  defp vertical_origins(tables, deltas, glyph_count) do
    data = required(tables, "VORG")
    <<version::32, default::signed-16, count::16>> = read(data, 0, 8, "VORG header")
    unless version == 0x10000, do: invalid("VORG version must be one")

    records =
      for <<id::16, origin::signed-16 <- read(data, 8, count * 4, "VORG records")>>,
        do: {id, origin}

    ids = Enum.map(records, &elem(&1, 0))

    unless ids == Enum.sort(Enum.uniq(ids)) and Enum.all?(ids, &(&1 < glyph_count)),
      do: invalid("VORG glyph IDs must be sorted, unique and inside maxp")

    records = Map.new(records)

    values =
      Enum.with_index(deltas)
      |> Enum.map(fn {delta, id} ->
        value = Map.get(records, id, default) + GvarValidator.ot_round(delta)

        unless value in -32768..32767,
          do: invalid("VVAR-adjusted vertical origin exceeds its signed 16-bit field")

        {id, value}
      end)

    selected_default = default + GvarValidator.ot_round(hd(deltas))

    # Glyph zero was validated above and uses this same default plus delta.
    selected = Enum.reject(values, &(elem(&1, 1) == selected_default))

    data =
      <<0x10000::32, selected_default::signed-16, length(selected)::16,
        for({id, value} <- selected, into: <<>>, do: <<id::16, value::signed-16>>)::binary>>

    Map.put(tables, "VORG", data)
  end

  defp global_metrics(tables, coordinates) do
    case Map.get(tables, "MVAR") do
      nil ->
        {:ok, tables}

      data ->
        sync? =
          case {Map.get(tables, "OS/2"), Map.get(tables, "hhea")} do
            {os2, hhea}
            when is_binary(os2) and is_binary(hhea) and byte_size(os2) >= 74 and
                   byte_size(hhea) >= 10 ->
              binary_part(os2, 68, 6) == binary_part(hhea, 4, 6)

            _ ->
              false
          end

        <<version::32, reserved::16, size::16, count::16, offset::16>> =
          read(data, 0, 12, "MVAR header")

        unless version == 0x0001_0000 and reserved == 0 and size >= 8,
          do: invalid("MVAR version, reserved field or record size is invalid")

        store = unwrap(VariationStoreValidator.prepare(data, offset, coordinates))
        raw = read(data, 12, count * size, "MVAR records")
        records = for <<record::binary-size(^size) <- raw>>, do: record

        {tables, _} =
          Enum.reduce(records, {tables, nil}, fn record, {tables, previous} ->
            <<tag::binary-size(4), outer::16, inner::16, _::binary>> = record

            unless previous == nil or previous < tag,
              do: invalid("MVAR value records must be sorted and unique")

            delta =
              unwrap(VariationStoreValidator.lookup(store, outer, inner))
              |> GvarValidator.ot_round()

            tables =
              case Map.get(@mvar, tag) do
                nil ->
                  tables

                {table, position} ->
                  case Map.get(tables, table) do
                    nil ->
                      tables

                    bytes ->
                      if table == "gasp" do
                        <<version::16, ranges::16>> = read(bytes, 0, 4, "gasp header")

                        unless version in [0, 1] and div(position - 4, 4) < ranges - 1,
                          do:
                            invalid(
                              "MVAR gasp variations must reference an existing non-final range"
                            )

                        read(bytes, 4, ranges * 4, "gasp ranges")
                      end

                      raw = read(bytes, position, 2, tag)

                      value =
                        if tag in ["hcla", "hcld"] or table == "gasp",
                          do: :binary.decode_unsigned(raw),
                          else:
                            (
                              <<v::signed-16>> = raw
                              v
                            )

                      selected = value + delta

                      range =
                        if tag in ["hcla", "hcld"] or table == "gasp",
                          do: 0..65535,
                          else: -32768..32767

                      unless selected in range,
                        do: invalid("MVAR-adjusted #{tag} exceeds its 16-bit metric")

                      Map.put(tables, table, replace(bytes, position, <<selected::16>>))
                  end
              end

            {tables, tag}
          end)

        if Map.has_key?(tables, "gasp") do
          <<version::16, count::16>> = read(tables["gasp"], 0, 4, "gasp header")

          ranges =
            for <<maximum::16,
                  _behavior::16 <- read(tables["gasp"], 4, count * 4, "gasp ranges")>>,
                do: maximum

          unless version in [0, 1] and ranges != [] and List.last(ranges) == 65535 and
                   Enum.chunk_every(ranges, 2, 1, :discard) |> Enum.all?(fn [a, b] -> a < b end),
                 do:
                   invalid(
                     "MVAR-adjusted gasp ranges must remain strictly ordered and end at 65535"
                   )
        end

        tables =
          if sync?,
            do: Map.update!(tables, "hhea", &replace(&1, 4, binary_part(tables["OS/2"], 68, 6))),
            else: tables

        {:ok, tables}
    end
  end

  defp required(tables, tag) do
    case Map.fetch(tables, tag) do
      {:ok, data} -> data
      :error -> invalid("variation instance requires the #{tag} table")
    end
  end

  defp read(data, offset, size, source) do
    FontValidator.reserve(:max_font_variation_work, max(size, 1))

    unless offset >= 0 and size >= 0 and offset + size <= byte_size(data),
      do: invalid("#{source} byte range is outside its owning table")

    binary_part(data, offset, size)
  end

  defp replace(data, offset, bytes) do
    read(data, offset, byte_size(bytes), "instance metric")

    binary_part(data, 0, offset) <>
      bytes <>
      binary_part(data, offset + byte_size(bytes), byte_size(data) - offset - byte_size(bytes))
  end

  defp unwrap(result) do
    case result do
      {:ok, value} -> value
      {:error, _} = error -> throw({:invalid_instance, error})
    end
  end

  defp invalid(message), do: throw({:invalid_instance, failure(message)})

  defp failure(message),
    do:
      Diagnostics.error(:font, :invalid_font, message,
        module: __MODULE__,
        operation: :instantiate_font,
        source: "OpenType variations"
      )
end
