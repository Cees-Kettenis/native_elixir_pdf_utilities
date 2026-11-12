defmodule NativeElixirPdfUtilities.Validators.OpenTypeDataValidator do
  @moduledoc false
  import Bitwise
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.FontValidator

  @doc "Reads a bounded range under the owning font-table work allowance."
  @spec read(map(), integer(), non_neg_integer()) :: binary()
  def read(context, offset, size) do
    if offset < 0 or size < 0 or offset + size > byte_size(context.data),
      do: invalid(context, "truncated table or out-of-bounds offset", offset)

    FontValidator.reserve(Map.get(context, :work_limit, :max_work), size)
    binary_part(context.data, offset, size)
  end

  @doc "Reads one unsigned OpenType word."
  @spec u16(map(), non_neg_integer()) :: non_neg_integer()
  def u16(context, offset) do
    <<value::16>> = read(context, offset, 2)
    value
  end

  @doc "Reads one signed OpenType word."
  @spec s16(map(), non_neg_integer()) :: integer()
  def s16(context, offset) do
    <<value::signed-16>> = read(context, offset, 2)
    value
  end

  @doc "Reads one unsigned OpenType long."
  @spec u32(map(), non_neg_integer()) :: non_neg_integer()
  def u32(context, offset) do
    <<value::32>> = read(context, offset, 4)
    value
  end

  @doc "Reads a fixed array of unsigned words."
  @spec words(map(), non_neg_integer(), non_neg_integer()) :: [non_neg_integer()]
  def words(context, offset, count),
    do: for(<<value::16 <- read(context, offset, count * 2)>>, do: value)

  @doc "Admits a non-null relative offset after its parent's fixed fields and record array."
  @spec offset(map(), non_neg_integer(), non_neg_integer(), pos_integer()) :: non_neg_integer()
  def offset(context, parent, value, minimum) do
    if value < minimum or parent + value >= byte_size(context.data),
      do: invalid(context, "relative offset overlaps its header or leaves the table", parent)

    parent + value
  end

  @doc "Checks a glyph index against the admitted maxp count."
  @spec glyph(map(), integer(), non_neg_integer()) :: non_neg_integer()
  def glyph(context, value, offset) do
    if value < 0 or value >= context.glyph_count,
      do: invalid(context, "glyph outside maxp glyph count", offset)

    value
  end

  @doc "Admits sorted Coverage formats 1 and 2, retaining their consecutive indices."
  @spec coverage(map(), non_neg_integer()) :: [non_neg_integer()]
  def coverage(context, base) do
    <<format::16, count::16>> = read(context, base, 4)

    case format do
      1 ->
        values = words(context, base + 4, count)
        ordered(context, values, "unordered coverage glyphs", base)

        if Enum.any?(values, &(&1 >= context.glyph_count)),
          do: invalid(context, "coverage glyph outside maxp glyph count", base)

        values

      2 ->
        raw = read(context, base + 4, count * 6)

        {ranges, _, _} =
          Enum.reduce(
            for(<<a::16, b::16, index::16 <- raw>>, do: {a, b, index}),
            {[], -1, 0},
            fn {first, last, index}, {ranges, previous, expected} ->
              if first > last or first <= previous or last >= context.glyph_count,
                do: invalid(context, "invalid coverage range", base)

              if index != expected, do: invalid(context, "nonconsecutive coverage indices", base)

              FontValidator.reserve(
                Map.get(context, :work_limit, :max_work),
                last - first + 1
              )

              {[Enum.to_list(first..last) | ranges], last, expected + last - first + 1}
            end
          )

        ranges |> Enum.reverse() |> List.flatten()

      _ ->
        invalid(context, "unsupported coverage format", base)
    end
  end

  @doc "Admits ClassDef formats 1 and 2 with an explicit exclusive class bound."
  @spec classes(map(), non_neg_integer(), pos_integer()) :: %{
          non_neg_integer() => non_neg_integer()
        }
  def classes(context, base, class_count \\ 65_536) do
    format = u16(context, base)

    records =
      case format do
        1 ->
          <<first::16, count::16>> = read(context, base + 2, 4)
          FontValidator.reserve(Map.get(context, :work_limit, :max_work), count)

          words(context, base + 6, count)
          |> Enum.with_index(first)
          |> Enum.map(fn {value, glyph} -> {glyph, value} end)

        2 ->
          count = u16(context, base + 2)
          raw = read(context, base + 4, count * 6)

          {ranges, _} =
            Enum.reduce(for(<<a::16, b::16, c::16 <- raw>>, do: {a, b, c}), {[], -1}, fn {first,
                                                                                          last,
                                                                                          value},
                                                                                         {ranges,
                                                                                          previous} ->
              if first <= previous, do: invalid(context, "unordered class ranges", base)

              if first > last or last >= context.glyph_count,
                do: invalid(context, "invalid class range", base)

              FontValidator.reserve(
                Map.get(context, :work_limit, :max_work),
                last - first + 1
              )

              {[Enum.map(first..last, &{&1, value}) | ranges], last}
            end)

          ranges |> Enum.reverse() |> List.flatten()

        _ ->
          invalid(context, "unsupported class definition format", base)
      end

    if class_count <= 0 or
         Enum.any?(records, fn {glyph, value} ->
           glyph >= context.glyph_count or value >= class_count
         end),
       do: invalid(context, "class outside declared glyph or class counts", base)

    Map.new(records)
  end

  @doc "Admits packed pixel Device deltas or an ItemVariationStore VariationIndex."
  @spec device(map(), non_neg_integer(), non_neg_integer()) :: map() | nil
  def device(context, parent, offset) do
    if offset == 0 do
      nil
    else
      base = parent + offset
      <<first::16, last::16, format::16>> = read(context, base, 6)

      case format do
        0x8000 ->
          case Map.get(context, :gdef) do
            %{store: store} ->
              case NativeElixirPdfUtilities.Validators.VariationStoreValidator.lookup(
                     store,
                     first,
                     last
                   ) do
                {:ok, delta} ->
                  %{variation_index: {first, last}, delta: delta}

                _ ->
                  invalid(
                    context,
                    "variation device references an unavailable item variation delta",
                    base
                  )
              end

            nil ->
              %{variation_index: {first, last}}
          end

        format when format in [1, 2, 3] ->
          if first > last, do: invalid(context, "invalid device size range", base)
          bits = 1 <<< format
          count = last - first + 1
          bytes = div(count * bits + 15, 16) * 2
          raw = read(context, base + 6, bytes)
          FontValidator.reserve(Map.get(context, :work_limit, :max_work), count)
          deltas = for <<delta::signed-size(^bits) <- raw>>, do: delta
          %{start_size: first, end_size: last, deltas: Enum.take(deltas, count)}

        _ ->
          invalid(context, "unsupported device format", base)
      end
    end
  end

  @doc "Evaluates an admitted pixel device or variation delta in font design units."
  @spec device_adjustment(map() | nil, number() | nil, pos_integer()) :: number()
  def device_adjustment(device, ppem, units) do
    case device do
      nil ->
        0

      %{delta: delta} ->
        delta

      %{start_size: first, end_size: last, deltas: deltas} ->
        pixels = if ppem != nil, do: round(ppem)
        # Grid fitting uses positive integral pixel sizes, a format invariant.
        # Subpixel sizes rounded to zero have no device correction.
        if pixels != nil and pixels > 0 and pixels >= first and pixels <= last,
          do: Enum.at(deltas, pixels - first) * units / ppem,
          else: 0
    end
  end

  @doc "Admits a GPOS Anchor with unhinted design coordinates and optional contour/device metadata."
  @spec anchor(map(), non_neg_integer(), non_neg_integer()) :: map() | nil
  def anchor(context, parent, offset) do
    if offset == 0 do
      nil
    else
      base = parent + offset
      <<format::16, x::signed-16, y::signed-16>> = read(context, base, 6)

      case format do
        1 ->
          %{x: x, y: y, point: nil, x_device: nil, y_device: nil}

        2 ->
          %{x: x, y: y, point: u16(context, base + 6), x_device: nil, y_device: nil}

        3 ->
          <<dx::16, dy::16>> = read(context, base + 6, 4)

          %{
            x: x,
            y: y,
            point: nil,
            x_device: device(context, base, dx),
            y_device: device(context, base, dy)
          }

        _ ->
          invalid(context, "unsupported anchor format", base)
      end
    end
  end

  @doc "Admits one GPOS ValueRecord and all referenced device tables."
  @spec value(map(), non_neg_integer(), non_neg_integer(), non_neg_integer()) ::
          {map(), non_neg_integer()}
  def value(context, parent, offset, format) do
    if format > 255, do: invalid(context, "ValueRecord has reserved format bits", offset)
    fields = [:x_placement, :y_placement, :x_advance, :y_advance]
    initial = %{x_placement: 0, y_placement: 0, x_advance: 0, y_advance: 0, devices: %{}}

    case format do
      0 ->
        {initial, offset}

      4 ->
        {%{initial | x_advance: s16(context, offset)}, offset + 2}

      _ ->
        Enum.reduce(0..7, {initial, offset}, fn bit, {result, offset} ->
          if (format &&& 1 <<< bit) == 0 do
            {result, offset}
          else
            field = Enum.at(fields, rem(bit, 4))

            result =
              if bit < 4,
                do: Map.put(result, field, s16(context, offset)),
                else:
                  Map.update!(
                    result,
                    :devices,
                    &Map.put(&1, field, device(context, parent, u16(context, offset)))
                  )

            {result, offset + 2}
          end
        end)
    end
  end

  @doc "Checks strictly increasing glyph or range keys without silently overwriting duplicates."
  @spec ordered(map(), [integer()], binary(), non_neg_integer()) :: :ok
  def ordered(context, values, message, offset) do
    Enum.reduce(values, -1, fn value, previous ->
      if value <= previous, do: invalid(context, message, offset)
      value
    end)

    :ok
  end

  @doc "Stops malformed table admission with the shared diagnostic contract."
  @spec invalid(map(), binary(), integer()) :: no_return()
  def invalid(context, message, offset) do
    throw(
      {:invalid_open_type,
       Diagnostics.error(:font, :invalid_font, message,
         operation: Map.get(context, :operation, :prepare_opentype_layout),
         module: Map.get(context, :module, __MODULE__),
         source: "#{context.tag} byte #{offset}"
       )}
    )
  end
end
