defmodule NativeElixirPdfUtilities.Validators.GvarValidator do
  @moduledoc false
  import Bitwise
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.{FontValidator, VariationStoreValidator}

  @doc "Validates and applies gvar tuple deltas, including contour interpolation and four metric phantom points."
  @spec prepare(binary() | nil, tuple(), [{number(), number(), number(), number()}], [number()]) ::
          {:ok, [map()]} | {:error, {atom(), map()}}
  def prepare(data, glyphs, phantoms, coordinates) do
    FontValidator.with_budget(fn ->
      try do
        coordinates =
          case NativeElixirPdfUtilities.Validators.FontVariationValidator.prepare_normalized_coordinates(
                 coordinates
               ) do
            {:ok, coordinates} -> coordinates
            error -> throw({:invalid_gvar, error})
          end

        count = tuple_size(glyphs)

        unless length(phantoms) == count,
          do: invalid("gvar phantom metric count differs from glyph count")

        if data == nil do
          {:ok,
           Enum.zip(Tuple.to_list(glyphs), phantoms)
           |> Enum.map(fn {glyph, phantom} -> %{glyph: glyph, phantoms: phantom} end)}
        else
          <<major::16, minor::16, axes::16, shared_count::16, shared_offset::32, glyph_count::16,
            flags::16, data_offset::32>> = read(data, 0, 20)

          unless {major, minor} == {1, 0} and axes == length(coordinates) and count == glyph_count and
                   flags in [0, 1] and shared_count <= 4096,
                 do: invalid("gvar header version, axis count, glyph count or flags are invalid")

          width = if flags == 0, do: 2, else: 4

          offsets =
            for <<n::unsigned-integer-size(^width * 8) <- read(data, 20, (count + 1) * width)>>,
              do: n * if(flags == 0, do: 2, else: 1)

          unless hd(offsets) == 0 and offsets == Enum.sort(offsets) and
                   data_offset >= 20 + (count + 1) * width and
                   data_offset + List.last(offsets) <= byte_size(data),
                 do: invalid("gvar glyph data offsets are invalid")

          FontValidator.reserve(:max_work, shared_count * axes)
          shared_raw = read(data, shared_offset, shared_count * axes * 2)
          shared = for <<n::signed-16 <- shared_raw>>, do: n / 16384

          unless Enum.all?(shared, &(&1 >= -1 and &1 <= 1)),
            do: invalid("gvar shared tuple coordinates exceed normalized bounds")

          shared = if axes == 0, do: {}, else: Enum.chunk_every(shared, axes) |> List.to_tuple()

          results =
            Enum.zip(Enum.chunk_every(offsets, 2, 1, :discard), phantoms)
            |> Enum.with_index()
            |> Enum.map(fn {{[first, last], phantom}, id} ->
              glyph = elem(glyphs, id)

              points =
                if glyph.contours < 0 do
                  Enum.map(glyph.components, fn component ->
                    {x, y} = component.args
                    {x, y}
                  end)
                else
                  Enum.map(glyph.points, fn {x, y, _} -> {x, y} end)
                end

              {left, right, top, bottom} = phantom
              source_points = points ++ [{left, 0}, {right, 0}, {0, top}, {0, bottom}]

              deltas =
                tuples(
                  read(data, data_offset + first, last - first),
                  source_points,
                  glyph.ends,
                  coordinates,
                  shared,
                  glyph.contours < 0
                )

              updated =
                Enum.zip(source_points, deltas)
                |> Enum.map(fn {{x, y}, {dx, dy}} -> {x + dx, y + dy} end)

              {actual, [{left, _}, {right, _}, {_, top}, {_, bottom}]} =
                Enum.split(updated, length(points))

              glyph =
                if glyph.contours < 0 do
                  components =
                    Enum.zip(glyph.components, actual)
                    |> Enum.map(fn {component, {x, y}} ->
                      # OpenType gvar: deltas have no effect on components
                      # attached by point indices; those indices remain fixed.
                      if component.xy?,
                        do: %{component | args: {ot_round(x), ot_round(y)}},
                        else: component
                    end)

                  %{glyph | components: components}
                else
                  points =
                    Enum.zip(glyph.points, actual)
                    |> Enum.map(fn {{_, _, flag}, {x, y}} -> {ot_round(x), ot_round(y), flag} end)

                  %{glyph | points: points}
                end

              %{glyph: glyph, phantoms: {left, right, top, bottom}}
            end)

          {:ok, results}
        end
      catch
        {:invalid_gvar, error} -> error
      end
    end)
  end

  defp tuples(data, points, ends, coordinates, shared, composite?) do
    total = length(points)
    FontValidator.reserve(:max_work, total)
    zero = List.duplicate({0, 0}, total)

    if data == <<>> do
      zero
    else
      <<count::16, offset::16>> = read(data, 0, 4)

      unless (count &&& 0x7000) == 0 and offset >= 4 and offset <= byte_size(data),
        do: invalid("gvar tuple count flags or serialized-data offset are invalid")

      {headers, header_end} =
        Enum.map_reduce(0..((count &&& 4095) - 1)//1, 4, fn _, pos ->
          <<size::16, index::16>> = read(data, pos, 4)
          if (index &&& 4096) != 0, do: invalid("gvar tuple index uses a reserved bit")

          {peak, pos} =
            if (index &&& 32768) != 0 do
              coordinate_tuple(data, pos + 4, length(coordinates))
            else
              id = index &&& 4095

              unless id < tuple_size(shared),
                do: invalid("gvar tuple references an unavailable shared peak")

              {elem(shared, id), pos + 4}
            end

          {region, pos} =
            if (index &&& 16384) != 0 do
              {start, pos} = coordinate_tuple(data, pos, length(coordinates))
              {finish, pos} = coordinate_tuple(data, pos, length(coordinates))
              {Enum.zip([start, peak, finish]), pos}
            else
              {Enum.map(peak, fn value -> {min(value, 0), value, max(value, 0)} end), pos}
            end

          unless Enum.all?(region, fn {a, b, c} -> a <= b and b <= c end),
            do: invalid("gvar tuple support bounds are reversed")

          {%{
             size: size,
             private?: (index &&& 8192) != 0,
             scalar: VariationStoreValidator.scalar(region, coordinates)
           }, pos}
        end)

      unless header_end <= offset,
        do: invalid("gvar tuple headers overlap their serialized deltas")

      payload = read(data, offset, byte_size(data) - offset)

      {shared_points, payload} =
        if (count &&& 32768) != 0, do: point_numbers(payload, total), else: {:all, payload}

      {result, payload} =
        Enum.reduce(headers, {zero, payload}, fn header, {result, bytes} ->
          {tuple_data, bytes} = split(bytes, header.size)

          {numbers, tuple_data} =
            if header.private?,
              do: point_numbers(tuple_data, total),
              else: {shared_points, tuple_data}

          numbers = if numbers == :all, do: Enum.to_list(0..(total - 1)), else: numbers
          {xs, tuple_data} = deltas(tuple_data, length(numbers), [])
          {ys, tuple_data} = deltas(tuple_data, length(numbers), [])
          unless tuple_data == <<>>, do: invalid("gvar tuple delta payload has trailing bytes")
          sparse = Enum.zip([numbers, xs, ys]) |> Map.new(fn {id, x, y} -> {id, {x, y}} end)

          deltas =
            if composite? do
              for id <- 0..(total - 1), do: Map.get(sparse, id, {0, 0})
            else
              interpolate(points, ends, sparse)
            end

          FontValidator.reserve(:max_work, total)

          result =
            Enum.zip(result, deltas)
            |> Enum.map(fn {{x, y}, {dx, dy}} ->
              {x + dx * header.scalar, y + dy * header.scalar}
            end)

          {result, bytes}
        end)

      unless byte_size(payload) <= 1 and payload == :binary.copy(<<0>>, byte_size(payload)),
        do: invalid("gvar glyph variation data has trailing bytes")

      result
    end
  end

  defp coordinate_tuple(data, pos, count) do
    values = for <<n::signed-16 <- read(data, pos, count * 2)>>, do: n / 16384

    unless Enum.all?(values, &(&1 >= -1 and &1 <= 1)),
      do: invalid("gvar tuple coordinates exceed normalized bounds")

    {values, pos + count * 2}
  end

  defp point_numbers(data, total) do
    {raw, data} = split(data, 1)
    <<first>> = raw

    {count, data} =
      if (first &&& 128) != 0 do
        {raw, data} = split(data, 1)
        <<second>> = raw
        {((first &&& 127) <<< 8) + second, data}
      else
        {first, data}
      end

    if count == 0 do
      {:all, data}
    else
      unless count <= total, do: invalid("gvar packed point count exceeds glyph points")
      point_runs(data, count, 0, [], total)
    end
  end

  defp point_runs(data, left, current, values, total) do
    if left == 0 do
      {Enum.reverse(values), data}
    else
      {raw, data} = split(data, 1)
      <<flag>> = raw
      n = (flag &&& 127) + 1
      width = if (flag &&& 128) != 0, do: 2, else: 1
      if n > left, do: invalid("gvar point run exceeds its point count")
      {raw, data} = split(data, n * width)

      {deltas, {current, _first?}} =
        Enum.map_reduce(
          for(<<n::unsigned-integer-size(^width * 8) <- raw>>, do: n),
          {current, values == []},
          fn delta, {current, first?} ->
            next = current + delta

            unless next < total and (first? or delta > 0),
              do: invalid("gvar packed point index is invalid")

            {next, {next, false}}
          end
        )

      all = Enum.reverse(deltas, values)

      point_runs(data, left - n, current, all, total)
    end
  end

  defp deltas(data, left, values) do
    if left == 0 do
      {Enum.reverse(values), data}
    else
      {raw, data} = split(data, 1)
      <<flag>> = raw
      count = (flag &&& 63) + 1
      if count > left, do: invalid("gvar delta run exceeds its point count")

      {run, data} =
        case flag &&& 192 do
          128 ->
            {List.duplicate(0, count), data}

          encoded ->
            width =
              case encoded do
                0 -> 1
                64 -> 2
                192 -> 4
              end

            {raw, data} = split(data, count * width)
            {for(<<n::signed-integer-size(^width * 8) <- raw>>, do: n), data}
        end

      deltas(data, left - count, Enum.reverse(run, values))
    end
  end

  defp interpolate(points, ends, sparse) do
    total = length(points)
    ends = ends ++ Enum.to_list((total - 4)..(total - 1))
    points = List.to_tuple(points)

    {result, _} =
      Enum.map_reduce(ends, 0, fn finish, start ->
        ids = Enum.to_list(start..finish)
        explicit = Enum.filter(ids, &Map.has_key?(sparse, &1))

        result =
          case explicit do
            [] ->
              List.duplicate({0, 0}, length(ids))

            [only] ->
              List.duplicate(Map.fetch!(sparse, only), length(ids))

            _ ->
              references = explicit ++ [hd(explicit) + finish - start + 1]
              pairs = Enum.chunk_every(references, 2, 1, :discard)

              mapped =
                Enum.reduce(pairs, sparse, fn [a, b], mapped ->
                  b_id = if b > finish, do: b - (finish - start + 1), else: b
                  p1 = elem(points, a)
                  p2 = elem(points, b_id)
                  d1 = Map.fetch!(sparse, a)
                  d2 = Map.fetch!(sparse, b_id)

                  Enum.reduce((a + 1)..(b - 1)//1, mapped, fn index, mapped ->
                    id = if index > finish, do: index - (finish - start + 1), else: index
                    p = elem(points, id)

                    dx =
                      interpolate_axis(
                        elem(p, 0),
                        elem(p1, 0),
                        elem(p2, 0),
                        elem(d1, 0),
                        elem(d2, 0)
                      )

                    dy =
                      interpolate_axis(
                        elem(p, 1),
                        elem(p1, 1),
                        elem(p2, 1),
                        elem(d1, 1),
                        elem(d2, 1)
                      )

                    Map.put(mapped, id, {dx, dy})
                  end)
                end)

              Enum.map(ids, &Map.fetch!(mapped, &1))
          end

        {result, finish + 1}
      end)

    List.flatten(result)
  end

  defp interpolate_axis(value, a, b, da, db) do
    {a, b, da, db} = if a > b, do: {b, a, db, da}, else: {a, b, da, db}

    cond do
      a == b -> if da == db, do: da, else: 0
      value <= a -> da
      value >= b -> db
      true -> da + (value - a) * (db - da) / (b - a)
    end
  end

  @doc "Rounds a finite OpenType variation result to the nearest integer, with halves toward positive infinity."
  @spec ot_round(number()) :: integer()
  def ot_round(value), do: floor(value + 0.5)

  defp read(data, offset, size) do
    FontValidator.reserve(:max_work, max(size, 1))

    unless offset >= 0 and size >= 0 and offset + size <= byte_size(data),
      do: invalid("gvar byte range is outside its owning table")

    binary_part(data, offset, size)
  end

  defp split(data, size) do
    value = read(data, 0, size)
    {value, binary_part(data, size, byte_size(data) - size)}
  end

  defp invalid(message), do: throw({:invalid_gvar, failure(message)})

  defp failure(message),
    do:
      Diagnostics.error(:font, :invalid_font, message,
        module: __MODULE__,
        operation: :instantiate_font,
        source: "OpenType gvar"
      )
end
