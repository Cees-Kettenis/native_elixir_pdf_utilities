defmodule NativeElixirPdfUtilities.Validators.ColrFontValidator do
  @moduledoc false
  import Bitwise
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.{FontValidator, VariationStoreValidator}

  @doc "Resolves a selected COLR glyph into a bounded static paint graph and selected CPAL colors."
  @spec prepare(map(), non_neg_integer(), tuple()) ::
          {:ok, map() | nil} | {:error, {atom(), map()}}
  def prepare(font, glyph, current) do
    FontValidator.with_budget(fn ->
      try do
        with {:ok, _, directory} <- FontValidator.prepare_table_directory(font.data) do
          tables =
            Map.new(directory, fn {tag, {offset, size}} ->
              {tag, binary_part(font.data, offset, size)}
            end)

          case tables["COLR"] do
            nil ->
              {:ok, nil}

            data ->
              palettes = palettes(tables["CPAL"] || invalid("COLR requires a CPAL table"))
              palette = select_palette(palettes, Map.get(font, :palette, :normal))
              colors = Tuple.insert_at(palette, tuple_size(palette), current)

              <<version::16, count::16, base::32, layers::32, layer_count::16>> =
                read(data, 0, 14)

              unless version in [0, 1], do: invalid("COLR version must be zero or one")
              work(count + layer_count)

              records =
                for <<id::16, first::16, n::16 <- read(data, base, count * 6)>>,
                  do: {id, first, n}

              ids = Enum.map(records, &elem(&1, 0))

              unless ids == Enum.sort(Enum.uniq(ids)) and
                       Enum.all?(ids, &(&1 < length(font.widths))),
                     do: invalid("COLR base glyph IDs must be sorted, unique and in maxp")

              layer_records =
                for <<id::16, color::16 <- read(data, layers, layer_count * 4)>>, do: {id, color}

              state = %{
                data: data,
                colors: colors,
                current: tuple_size(colors) - 1,
                count: length(font.widths),
                bases: %{},
                layers: {},
                store: nil,
                mapping: 0,
                clips: %{}
              }

              state = if version == 1, do: version_one(state, font), else: state

              graph =
                case Map.fetch(state.bases, glyph) do
                  {:ok, offset} ->
                    paint(state, offset, %{})

                  :error ->
                    case Enum.find(records, &(elem(&1, 0) == glyph)) do
                      nil ->
                        nil

                      {_, first, n} ->
                        unless first + n <= layer_count,
                          do: invalid("COLR v0 layers exceed their array")

                        {:layers,
                         Enum.map(Enum.slice(layer_records, first, n), fn {id, color} ->
                           glyph_id(state, id)
                           {:glyph, id, {:solid, color(state, color, 1)}}
                         end)}
                    end
                end

              {:ok,
               if(graph == nil,
                 do: nil,
                 else: %{graph: graph, clip: Map.get(state.clips, glyph), colors: colors}
               )}
          end
        end
      catch
        {:colr_error, error} -> error
      end
    end)
  end

  @doc "Selects an admitted CPAL palette for SVG glyph color variables."
  @spec prepare_palette(binary(), term()) :: {:ok, tuple()} | {:error, {atom(), map()}}
  def prepare_palette(data, selection) do
    FontValidator.with_budget(fn ->
      try do
        {:ok, select_palette(palettes(data), selection)}
      catch
        {:colr_error, error} -> error
      end
    end)
  end

  defp palettes(data) do
    <<version::16, entries::16, count::16, records::16, offset::32>> = read(data, 0, 12)

    unless version in [0, 1] and entries > 0 and count > 0,
      do: invalid("CPAL version or palette counts are invalid")

    work(records + count * entries)
    indices = for <<n::16 <- read(data, 12, count * 2)>>, do: n

    colors =
      for <<b, g, r, a <- read(data, offset, records * 4)>>,
        do: {r / 255, g / 255, b / 255, a / 255}

    types =
      if version == 1 do
        <<types::32, labels::32, entry_labels::32>> = read(data, 12 + count * 2, 12)
        if labels != 0, do: read(data, labels, count * 2)
        if entry_labels != 0, do: read(data, entry_labels, entries * 2)

        if types == 0,
          do: List.duplicate(0, count),
          else: for(<<n::32 <- read(data, types, count * 4)>>, do: n)
      else
        List.duplicate(0, count)
      end

    Enum.zip(indices, types)
    |> Enum.map(fn {index, type} ->
      unless index + entries <= records and (type &&& bnot(3)) == 0,
        do: invalid("CPAL palette range or reserved suitability bits are invalid")

      %{colors: Enum.slice(colors, index, entries) |> List.to_tuple(), type: type}
    end)
  end

  defp select_palette(palettes, selection) do
    base = if is_map(selection), do: selection.base, else: selection

    palette =
      case base do
        index when is_integer(index) and index >= 0 -> Enum.at(palettes, index) || hd(palettes)
        :light -> Enum.find(palettes, hd(palettes), &((&1.type &&& 1) != 0))
        :dark -> Enum.find(palettes, hd(palettes), &((&1.type &&& 2) != 0))
        _ -> hd(palettes)
      end

    if is_map(selection) do
      work(map_size(selection.overrides))

      Enum.reduce(selection.overrides, palette.colors, fn {index, color}, colors ->
        if index >= 0 and index < tuple_size(colors),
          do: put_elem(colors, index, color),
          else: colors
      end)
    else
      palette.colors
    end
  end

  defp version_one(state, font) do
    data = state.data
    <<base::32, layers::32, clips::32, mapping::32, store::32>> = read(data, 14, 20)
    <<count::32>> = read(data, base, 4)
    work(count)

    records =
      for <<id::16, offset::32 <- read(data, base + 4, count * 6)>>, do: {id, base + offset}

    ids = Enum.map(records, &elem(&1, 0))

    unless ids == Enum.sort(Enum.uniq(ids)),
      do: invalid("COLR v1 base glyph IDs must be sorted and unique")

    Enum.each(ids, &glyph_id(state, &1))

    layer_offsets =
      if layers == 0,
        do: {},
        else:
          (
            <<count::32>> = read(data, layers, 4)
            work(count)

            for(<<offset::32 <- read(data, layers + 4, count * 4)>>, do: layers + offset)
            |> List.to_tuple()
          )

    store =
      if store == 0,
        do: nil,
        else:
          unwrap(
            VariationStoreValidator.prepare(
              data,
              store,
              Map.get(font, :normalized_coordinates, [])
            )
          )

    state = %{
      state
      | bases: Map.new(records),
        layers: layer_offsets,
        store: store,
        mapping: mapping
    }

    clip_records =
      if clips == 0,
        do: %{},
        else:
          (
            <<format, count::32>> = read(data, clips, 5)
            unless format == 1, do: invalid("COLR clip list format must be one")
            work(count)

            records =
              for <<first::16, last::16, offset::24 <- read(data, clips + 5, count * 7)>>,
                do: {first, last, clips + offset}

            {values, _} =
              Enum.reduce(records, {%{}, -1}, fn {first, last, offset}, {values, previous} ->
                unless first > previous and first <= last and last < state.count,
                  do: invalid("COLR clip ranges overlap or exceed maxp")

                <<format, a::signed-16, b::signed-16, c::signed-16, d::signed-16>> =
                  read(data, offset, 9)

                unless format in [1, 2], do: invalid("COLR clip box format is invalid")

                values4 =
                  if format == 2,
                    do: varied(state, offset + 9, [a, b, c, d], [1, 1, 1, 1]),
                    else: [a, b, c, d]

                [a, b, c, d] = values4
                unless a <= c and b <= d, do: invalid("COLR clip bounds are inverted")
                work(last - first + 1)
                {Enum.reduce(first..last, values, &Map.put(&2, &1, {a, b, c, d})), last}
              end)

            values
          )

    %{state | clips: clip_records}
  end

  @spec paint(map(), non_neg_integer(), map()) :: tuple() | :empty
  defp paint(state, offset, seen) do
    work(1)
    FontValidator.check(:max_font_glyph_depth, map_size(seen) + 1)
    if Map.has_key?(seen, offset), do: invalid("COLR paint graph contains a cycle")
    seen = Map.put(seen, offset, true)
    <<format>> = read(state.data, offset, 1)

    case format do
      1 ->
        <<n, first::32>> = read(state.data, offset + 1, 5)

        unless first + n <= tuple_size(state.layers),
          do: invalid("COLR layer slice exceeds LayerList")

        work(n)

        {:layers, for(i <- 0..(n - 1)//1, do: paint(state, elem(state.layers, first + i), seen))}

      solid when solid in [2, 3] ->
        <<index::16, alpha::signed-16>> = read(state.data, offset + 1, 4)

        [alpha] =
          if solid == 3, do: varied(state, offset + 5, [alpha], [16384]), else: [alpha / 16384]

        {:solid, color(state, index, alpha)}

      gradient when gradient in 4..9 ->
        <<line::24>> = read(state.data, offset + 1, 3)
        variable? = rem(gradient, 2) == 1
        base = gradient - rem(gradient, 2)
        size = if base == 8, do: 4, else: 6
        raw = read(state.data, offset + 4, size * 2)
        values = for <<n::signed-16 <- raw>>, do: n

        values =
          if base == 6,
            do: List.update_at(List.update_at(values, 2, &(&1 &&& 65535)), 5, &(&1 &&& 65535)),
            else: values

        divisors = if base == 8, do: [1, 1, 16384, 16384], else: List.duplicate(1, size)

        values =
          if variable?,
            do: varied(state, offset + 4 + size * 2, values, divisors),
            else: Enum.zip_with(values, divisors, &(&1 / &2))

        {:gradient, base, values, color_line(state, offset + line, variable?)}

      10 ->
        <<child::24, id::16>> = read(state.data, offset + 1, 5)
        glyph_id(state, id)
        {:glyph, id, paint(state, forward(offset, child), seen)}

      11 ->
        <<id::16>> = read(state.data, offset + 1, 2)

        paint(
          state,
          Map.get(state.bases, id) || invalid("COLR paint references a missing color glyph"),
          seen
        )

      transform when transform in 12..31 ->
        <<child::24>> = read(state.data, offset + 1, 3)

        {:transform, transform(state, offset, transform),
         paint(state, forward(offset, child), seen)}

      32 ->
        <<source::24, mode, backdrop::24>> = read(state.data, offset + 1, 7)

        {:composite, if(mode in 0..27, do: mode, else: 0),
         paint(state, forward(offset, source), seen),
         paint(state, forward(offset, backdrop), seen)}

      _ ->
        :empty
    end
  end

  defp transform(state, offset, format) do
    variable? = rem(format, 2) == 1
    base = format - rem(format, 2)

    {start, count, divisors} =
      case base do
        12 ->
          <<relative::24>> = read(state.data, offset + 4, 3)
          {forward(offset, relative), 6, List.duplicate(65536, 6)}

        14 ->
          {offset + 4, 2, [1, 1]}

        16 ->
          {offset + 4, 2, [16384, 16384]}

        18 ->
          {offset + 4, 4, [16384, 16384, 1, 1]}

        20 ->
          {offset + 4, 1, [16384]}

        22 ->
          {offset + 4, 3, [16384, 1, 1]}

        24 ->
          {offset + 4, 1, [16384]}

        26 ->
          {offset + 4, 3, [16384, 1, 1]}

        28 ->
          {offset + 4, 2, [16384, 16384]}

        30 ->
          {offset + 4, 4, [16384, 16384, 1, 1]}
      end

    width = if base == 12, do: 4, else: 2
    raw = read(state.data, start, count * width)

    values =
      if width == 4,
        do: for(<<n::signed-32 <- raw>>, do: n),
        else: for(<<n::signed-16 <- raw>>, do: n)

    values =
      if variable?,
        do: varied(state, start + count * width, values, divisors),
        else: Enum.zip_with(values, divisors, &(&1 / &2))

    {matrix, center} =
      case {base, values} do
        {12, matrix} ->
          {matrix, {0, 0}}

        {14, [x, y]} ->
          {[1, 0, 0, 1, x, y], {0, 0}}

        {16, [x, y]} ->
          {[x, 0, 0, y, 0, 0], {0, 0}}

        {18, [x, y, cx, cy]} ->
          {[x, 0, 0, y, 0, 0], {cx, cy}}

        {20, [scale]} ->
          {[scale, 0, 0, scale, 0, 0], {0, 0}}

        {22, [scale, cx, cy]} ->
          {[scale, 0, 0, scale, 0, 0], {cx, cy}}

        {rotation, [angle | center]} when rotation in [24, 26] ->
          radians = angle * :math.pi()
          c = :math.cos(radians)
          s = :math.sin(radians)
          {[c, s, -s, c, 0, 0], if(center == [], do: {0, 0}, else: List.to_tuple(center))}

        {skew, [x, y | center]} when skew in [28, 30] ->
          unless abs(:math.cos(x * :math.pi())) > 1.0e-12 and
                   abs(:math.cos(y * :math.pi())) > 1.0e-12,
                 do: invalid("COLR skew is singular")

          {[1, :math.tan(y * :math.pi()), -:math.tan(x * :math.pi()), 1, 0, 0],
           if(center == [], do: {0, 0}, else: List.to_tuple(center))}
      end

    [a, b, c, d, e, f] = matrix
    {cx, cy} = center
    matrix = [a, b, c, d, e + cx - a * cx - c * cy, f + cy - b * cx - d * cy]
    Enum.each(matrix, &FontValidator.check(:max_font_numeric_magnitude, abs(&1)))
    matrix
  end

  defp color_line(state, offset, variable?) do
    <<extend, count::16>> = read(state.data, offset, 3)

    unless extend in [0, 1, 2] and count > 0,
      do: invalid("COLR color line needs stops and a valid extend mode")

    work(count)
    width = if variable?, do: 10, else: 6

    stops =
      for i <- 0..(count - 1) do
        at = offset + 3 + i * width
        <<position::signed-16, index::16, alpha::signed-16>> = read(state.data, at, 6)

        [position, alpha] =
          if variable?,
            do: varied(state, at + 6, [position, alpha], [16384, 16384]),
            else: [position / 16384, alpha / 16384]

        {position, color(state, index, alpha)}
      end

    positions = Enum.map(stops, &elem(&1, 0))
    unless positions == Enum.sort(positions), do: invalid("COLR gradient stops must be ordered")
    {extend, stops}
  end

  defp varied(state, offset, values, divisors) do
    <<base::32>> = read(state.data, offset, 4)
    unless state.store != nil, do: invalid("variable COLR paint requires an ItemVariationStore")

    deltas =
      if base == 0xFFFFFFFF,
        do: List.duplicate(0, length(values)),
        else:
          (if state.mapping == 0 do
             for i <- base..(base + length(values) - 1),
                 do: unwrap(VariationStoreValidator.lookup(state.store, i >>> 16, i &&& 65535))
           else
             unwrap(
               VariationStoreValidator.prepare_map(
                 state.data,
                 state.mapping,
                 state.store,
                 base + length(values),
                 true
               )
             )
             |> Enum.drop(base)
           end)

    Enum.zip([values, deltas, divisors])
    |> Enum.map(fn {value, delta, divisor} -> (value + delta) / divisor end)
  end

  defp color(state, index, alpha) do
    index = if index == 65535, do: state.current, else: index

    unless index >= 0 and index < tuple_size(state.colors),
      do: invalid("COLR palette entry is outside CPAL")

    {r, g, b, a} = elem(state.colors, index)
    {r, g, b, a * min(1, max(0, alpha))}
  end

  defp glyph_id(state, id),
    do: if(id >= state.count, do: invalid("COLR outline glyph ID exceeds maxp"), else: :ok)

  defp forward(offset, relative),
    do:
      if(relative == 0,
        do: invalid("COLR child offsets must be nonzero forward offsets"),
        else: offset + relative
      )

  defp read(data, offset, size) do
    if offset < 0 or size < 0 or offset > byte_size(data) - size,
      do: invalid("COLR/CPAL table is truncated or references bytes outside its owner")

    binary_part(data, offset, size)
  end

  defp work(count), do: FontValidator.reserve(:max_font_color_work, count)

  defp unwrap(result) do
    case result do
      {:ok, value} -> value
      error -> throw({:colr_error, error})
    end
  end

  defp invalid(message),
    do:
      throw(
        {:colr_error,
         Diagnostics.error(:font, :invalid_font, message,
           module: __MODULE__,
           operation: :prepare_color_font,
           source: "COLR/CPAL"
         )}
      )
end
