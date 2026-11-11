defmodule NativeElixirPdfUtilities.Validators.TrueTypeGlyphValidator do
  @moduledoc false
  import Bitwise
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.FontValidator

  @doc "Decodes a TrueType glyph's points, contour endpoints, instructions and composite records under the shared work allowance."
  @spec prepare(term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(data, count) do
    FontValidator.with_budget(fn ->
      try do
        unless is_binary(data) and is_integer(count) and count > 0 and count <= 65535,
          do: invalid("glyph decoding requires bytes and a valid maxp glyph count")

        FontValidator.reserve(:max_font_variation_work, byte_size(data) + 1)

        empty = %{
          contours: 0,
          points: [],
          ends: [],
          instructions: <<>>,
          components: [],
          bbox: {0, 0, 0, 0}
        }

        if data == <<>> do
          {:ok, empty}
        else
          {header, bytes} = read(data, 10)
          <<contours::signed-16, a::signed-16, b::signed-16, c::signed-16, d::signed-16>> = header

          unless contours >= -1 and a <= c and b <= d,
            do: invalid("glyph contour count or bounds are invalid")

          glyph = %{empty | contours: contours, bbox: {a, b, c, d}}

          {glyph, bytes} =
            if contours < 0 do
              {components, instructions?, bytes} = components(bytes, count, [], false)

              {instructions, bytes} =
                if instructions?, do: instructions(bytes), else: {<<>>, bytes}

              {%{glyph | components: components, instructions: instructions}, bytes}
            else
              {raw, bytes} = read(bytes, contours * 2)
              ends = for <<n::16 <- raw>>, do: n

              unless ends == Enum.sort(Enum.uniq(ends)),
                do: invalid("glyph contour endpoints must be strictly increasing")

              total = if ends == [], do: 0, else: List.last(ends) + 1

              {instructions, bytes} =
                if bytes == <<>> and contours == 0, do: {<<>>, bytes}, else: instructions(bytes)

              {flags, bytes} = flags(bytes, total, [])
              {xs, bytes} = coordinates(bytes, flags, 2, 16, 0, [])
              {ys, bytes} = coordinates(bytes, flags, 4, 32, 0, [])
              points = Enum.zip([xs, ys, flags]) |> Enum.map(fn {x, y, f} -> {x, y, f &&& 65} end)
              {%{glyph | points: points, ends: ends, instructions: instructions}, bytes}
            end

          unless byte_size(bytes) <= 3 and bytes == :binary.copy(<<0>>, byte_size(bytes)),
            do: invalid("glyph has unowned trailing bytes")

          {:ok, glyph}
        end
      catch
        {:invalid_glyph, error} -> error
      end
    end)
  end

  defp flags(bytes, left, values) do
    if left == 0 do
      {Enum.reverse(values), bytes}
    else
      {raw, bytes} = read(bytes, 1)
      <<flag>> = raw
      unless (flag &&& 128) == 0, do: invalid("glyph point flag uses a reserved bit")

      {repeat, bytes} =
        if (flag &&& 8) != 0 do
          {raw, bytes} = read(bytes, 1)
          <<n>> = raw
          {n + 1, bytes}
        else
          {1, bytes}
        end

      if repeat > left, do: invalid("glyph flag repetition exceeds its point count")
      FontValidator.reserve(:max_font_variation_work, repeat)
      flags(bytes, left - repeat, List.duplicate(flag, repeat) ++ values)
    end
  end

  defp coordinates(bytes, flags, short, same, current, values) do
    case flags do
      [] ->
        {Enum.reverse(values), bytes}

      [flag | rest] ->
        {delta, bytes} =
          cond do
            (flag &&& short) != 0 ->
              {raw, bytes} = read(bytes, 1)
              <<n>> = raw
              {if((flag &&& same) == 0, do: -n, else: n), bytes}

            (flag &&& same) != 0 ->
              {0, bytes}

            true ->
              {raw, bytes} = read(bytes, 2)
              <<n::signed-16>> = raw
              {n, bytes}
          end

        value = current + delta
        unless value in -32768..32767, do: invalid("glyph coordinates exceed signed int16")
        coordinates(bytes, rest, short, same, value, [value | values])
    end
  end

  defp components(bytes, count, values, instructions?) do
    {raw, bytes} = read(bytes, 4)
    <<flags::16, id::16>> = raw

    unless id < count and (flags &&& 0xE000) == 0 and
             Enum.count([8, 64, 128], &((flags &&& &1) != 0)) <= 1,
           do: invalid("composite glyph index or transform flags are invalid")

    words? = (flags &&& 1) != 0
    xy? = (flags &&& 2) != 0
    {arguments, bytes} = read(bytes, if(words?, do: 4, else: 2))

    args =
      case {words?, xy?} do
        {true, true} ->
          <<x::signed-16, y::signed-16>> = arguments
          {x, y}

        {true, false} ->
          <<x::16, y::16>> = arguments
          {x, y}

        {false, true} ->
          <<x::signed-8, y::signed-8>> = arguments
          {x, y}

        {false, false} ->
          <<x, y>> = arguments
          {x, y}
      end

    size =
      cond do
        (flags &&& 8) != 0 -> 2
        (flags &&& 64) != 0 -> 4
        (flags &&& 128) != 0 -> 8
        true -> 0
      end

    {transform, bytes} = read(bytes, size)

    matrix =
      case transform do
        <<a::signed-16>> ->
          {a / 16384, 0, 0, a / 16384}

        <<a::signed-16, d::signed-16>> ->
          {a / 16384, 0, 0, d / 16384}

        <<a::signed-16, b::signed-16, c::signed-16, d::signed-16>> ->
          {a / 16384, b / 16384, c / 16384, d / 16384}

        <<>> ->
          {1, 0, 0, 1}
      end

    value = %{id: id, flags: flags, args: args, xy?: xy?, matrix: matrix, transform: transform}
    values = [value | values]
    instructions? = instructions? or (flags &&& 256) != 0

    if (flags &&& 32) != 0,
      do: components(bytes, count, values, instructions?),
      else: {Enum.reverse(values), instructions?, bytes}
  end

  defp instructions(bytes) do
    {raw, bytes} = read(bytes, 2)
    <<length::16>> = raw
    read(bytes, length)
  end

  defp read(bytes, size) do
    FontValidator.reserve(:max_font_variation_work, max(size, 1))

    if byte_size(bytes) < size,
      do: invalid("glyph point, component or instruction stream is truncated")

    <<value::binary-size(^size), rest::binary>> = bytes
    {value, rest}
  end

  defp invalid(message) do
    throw(
      {:invalid_glyph,
       Diagnostics.error(:font, :invalid_font, message,
         module: __MODULE__,
         operation: :decode_true_type_glyph,
         source: "TrueType glyf"
       )}
    )
  end

  @doc "Resolves admitted TrueType composite points, attachment indices and bounds with shared dependency limits."
  @spec resolve(non_neg_integer(), tuple(), map(), MapSet.t(), non_neg_integer()) ::
          {map(), map()}
  @spec resolve(non_neg_integer(), tuple(), map(), MapSet.t(), non_neg_integer(), boolean()) ::
          {map(), map()}
  def resolve(id, glyphs, cache, active, depth, quantize \\ true) do
    FontValidator.check(:max_font_glyph_depth, depth + 1)

    case Map.fetch(cache, id) do
      {:ok, glyph} ->
        FontValidator.check(:max_font_glyph_depth, depth + glyph.component_depth)
        {glyph, cache}

      :error ->
        if MapSet.member?(active, id),
          do: invalid("composite glyph contains a dependency cycle")

        glyph = elem(glyphs, id)

        {points, ends, cache} =
          if glyph.contours < 0 do
            Enum.reduce(glyph.components, {[], [], cache}, fn component, {points, ends, cache} ->
              {child, cache} =
                resolve(component.id, glyphs, cache, MapSet.put(active, id), depth + 1, quantize)

              unless length(points) + length(child.points) <= 65536,
                do: invalid("instantiated composite exceeds TrueType's point-index fields")

              {a, b, c, d} = component.matrix

              transformed =
                Enum.map(child.points, fn {x, y, flag} -> {a * x + c * y, b * x + d * y, flag} end)

              {dx, dy} =
                if component.xy? do
                  {x, y} = component.args

                  if (component.flags &&& 2048) != 0,
                    do: {a * x + c * y, b * x + d * y},
                    else: {x, y}
                else
                  {parent_index, child_index} = component.args

                  unless parent_index < length(points) and child_index < length(transformed),
                    do: invalid("composite attachment point index is outside its contours")

                  {px, py, _} = Enum.at(points, parent_index)
                  {cx, cy, _} = Enum.at(transformed, child_index)
                  {px - cx, py - cy}
                end

              FontValidator.reserve(
                :max_font_variation_work,
                length(points) + length(ends) + length(transformed)
              )

              shifted =
                Enum.map(transformed, fn {x, y, flag} ->
                  x = x + dx
                  y = y + dy
                  FontValidator.check(:max_font_outline_coordinate, abs(x))
                  FontValidator.check(:max_font_outline_coordinate, abs(y))

                  if quantize,
                    do:
                      {NativeElixirPdfUtilities.Validators.GvarValidator.ot_round(x),
                       NativeElixirPdfUtilities.Validators.GvarValidator.ot_round(y), flag},
                    else: {x, y, flag}
                end)

              {points ++ shifted, ends ++ Enum.map(child.ends, &(&1 + length(points))), cache}
            end)
          else
            {glyph.points, glyph.ends, cache}
          end

        bbox =
          if points == [],
            do: {0, 0, 0, 0},
            else:
              Enum.reduce(points, {32767, 32767, -32768, -32768}, fn {x, y, _}, {a, b, c, d} ->
                {min(a, x), min(b, y), max(c, x), max(d, y)}
              end)

        glyph = %{glyph | bbox: bbox}
        # Keep composites as components for emission; their flattened points are
        # retained in the cache so parents can resolve point attachments.
        component_depth =
          1 +
            Enum.reduce(glyph.components, 0, fn component, value ->
              max(value, Map.fetch!(cache, component.id).component_depth)
            end)

        FontValidator.check(:max_font_glyph_depth, depth + component_depth)

        glyph =
          %{glyph | points: points, ends: ends} |> Map.put(:component_depth, component_depth)

        {glyph, Map.put(cache, id, glyph)}
    end
  end
end
