defmodule NativeElixirPdfUtilities.Validators.VariationStoreValidator do
  @moduledoc false
  import Bitwise
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.{FontValidator, FontVariationValidator}

  @doc "Validates an OpenType ItemVariationStore and evaluates its rows at normalized coordinates."
  @spec prepare(term(), term(), term()) :: {:ok, tuple()} | {:error, {atom(), map()}}
  def prepare(data, offset, coordinates) do
    case prepare_with_extent(data, offset, coordinates) do
      {:ok, store, _finish} -> {:ok, store}
      error -> error
    end
  end

  @doc "Evaluates an item variation store and returns the end of all referenced bytes for ownership validation."
  @spec prepare_with_extent(term(), term(), term()) ::
          {:ok, tuple(), non_neg_integer()} | {:error, {atom(), map()}}
  def prepare_with_extent(data, offset, coordinates) do
    FontValidator.with_budget(fn ->
      try do
        unless is_binary(data) and is_integer(offset) and offset >= 0,
          do: invalid("variation store requires bytes, an offset and normalized coordinates")

        coordinates =
          case FontVariationValidator.prepare_normalized_coordinates(coordinates) do
            {:ok, coordinates} -> coordinates
            error -> throw({:invalid_variation, error})
          end

        header = read(data, offset, 8)
        <<format::16, region_offset::32, count::16>> = header
        unless format == 1, do: invalid("ItemVariationStore format must be one")
        FontValidator.reserve(:max_work, count)
        offsets = for <<value::32 <- read(data, offset + 8, count * 4)>>, do: value
        region_header = read(data, offset + region_offset, 4)
        <<axis_count::16, region_count::16>> = region_header

        unless axis_count == length(coordinates),
          do: invalid("variation region axis count differs from fvar")

        FontValidator.reserve(:max_work, axis_count * region_count)
        raw = read(data, offset + region_offset + 4, axis_count * region_count * 6)

        triples =
          for <<a::signed-16, b::signed-16, c::signed-16 <- raw>>,
            do: {a / 16384, b / 16384, c / 16384}

        unless Enum.all?(triples, fn {a, b, c} -> a >= -1 and c <= 1 and a <= b and b <= c end),
          do: invalid("variation region has invalid normalized bounds")

        regions =
          if axis_count == 0,
            do: List.duplicate(1, region_count),
            else: Enum.chunk_every(triples, axis_count) |> Enum.map(&scalar(&1, coordinates))

        regions = List.to_tuple(regions)

        stores =
          Enum.map(offsets, fn start ->
            base = offset + start
            <<items::16, encoded_words::16, indices_count::16>> = read(data, base, 6)
            words = encoded_words &&& 32767
            long? = (encoded_words &&& 32768) != 0

            unless words <= indices_count,
              do: invalid("variation delta word count exceeds its region count")

            indices = for <<i::16 <- read(data, base + 6, indices_count * 2)>>, do: i

            unless Enum.all?(indices, &(&1 < region_count)),
              do: invalid("variation delta references an unavailable region")

            scalars = Enum.map(indices, &elem(regions, &1))

            size =
              if long?,
                do: words * 4 + (indices_count - words) * 2,
                else: words * 2 + indices_count - words

            FontValidator.reserve(:max_work, items * indices_count)
            raw = read(data, base + 6 + indices_count * 2, items * size)

            {values, <<>>} =
              Enum.map_reduce(0..(items - 1)//1, raw, fn _, bytes ->
                {row, rest} = split(bytes, size)
                {wide, narrow} = split(row, words * if(long?, do: 4, else: 2))

                values =
                  if long? do
                    for(<<v::signed-32 <- wide>>, do: v) ++ for <<v::signed-16 <- narrow>>, do: v
                  else
                    for(<<v::signed-16 <- wide>>, do: v) ++ for <<v::signed-8 <- narrow>>, do: v
                  end

                {Enum.zip(values, scalars)
                 |> Enum.reduce(0, fn {v, s}, total -> total + v * s end), rest}
              end)

            %{
              scalars: scalars,
              values: List.to_tuple(values),
              word_count: encoded_words,
              finish: base + 6 + indices_count * 2 + items * size
            }
          end)

        finish =
          Enum.reduce(
            stores,
            max(
              offset + 8 + count * 4,
              offset + region_offset + 4 + axis_count * region_count * 6
            ),
            &max(&1.finish, &2)
          )

        {:ok, List.to_tuple(stores), finish}
      catch
        {:invalid_variation, error} -> error
      end
    end)
  end

  @doc "Validates a delta-set map and resolves each glyph's evaluated delta, repeating its final map entry as specified."
  @spec prepare_map(binary(), non_neg_integer(), tuple(), non_neg_integer(), boolean()) ::
          {:ok, [number()] | nil} | {:error, {atom(), map()}}
  def prepare_map(data, offset, store, count, implicit?) do
    FontValidator.with_budget(fn ->
      try do
        unless is_binary(data) and is_integer(offset) and offset >= 0 and is_tuple(store) and
                 is_integer(count) and count >= 0 and is_boolean(implicit?),
               do:
                 invalid(
                   "delta-set maps require bytes, a nonnegative offset/count and an admitted variation store"
                 )

        FontValidator.reserve(:max_work, count)

        if offset == 0 and not implicit? do
          {:ok, nil}
        else
          indices =
            if offset == 0 do
              for id <- 0..(count - 1)//1, do: {0, id}
            else
              <<format, entry>> = read(data, offset, 2)

              unless format in [0, 1] and (entry &&& 192) == 0,
                do: invalid("delta-set map format or reserved entry bits are invalid")

              width = (entry >>> 4) + 1
              bits = (entry &&& 15) + 1

              unless bits <= width * 8,
                do: invalid("delta-set inner-index width exceeds its entry size")

              count_width = if format == 0, do: 2, else: 4
              entries = :binary.decode_unsigned(read(data, offset + 2, count_width))
              if entries == 0, do: invalid("delta-set map has no entries")
              FontValidator.reserve(:max_work, entries)
              raw = read(data, offset + 2 + count_width, entries * width)

              values =
                for <<value::unsigned-integer-size(^width * 8) <- raw>>,
                  do: {value >>> bits, value &&& (1 <<< bits) - 1}

              Enum.each(values, fn {outer, inner} -> lookup!(store, outer, inner) end)
              values
            end

          index_tuple = List.to_tuple(indices)

          values =
            for id <- 0..(count - 1)//1 do
              {outer, inner} = elem(index_tuple, min(id, tuple_size(index_tuple) - 1))
              lookup!(store, outer, inner)
            end

          {:ok, values}
        end
      catch
        {:invalid_variation, error} -> error
      end
    end)
  end

  @doc "Resolves an admitted outer/inner variation index, including the no-variation sentinel."
  @spec lookup(tuple(), non_neg_integer(), non_neg_integer()) ::
          {:ok, number()} | {:error, {atom(), map()}}
  def lookup(store, outer, inner) do
    try do
      {:ok, lookup!(store, outer, inner)}
    catch
      {:invalid_variation, error} -> error
    end
  end

  defp lookup!(store, outer, inner) do
    if {outer, inner} == {65535, 65535} do
      0
    else
      unless is_tuple(store) and is_integer(outer) and outer >= 0 and outer < tuple_size(store),
        do: invalid("variation outer index is unavailable")

      row = elem(store, outer).values

      unless is_integer(inner) and inner >= 0 and inner < tuple_size(row),
        do: invalid("variation inner index is unavailable")

      elem(row, inner)
    end
  end

  @doc "Evaluates CFF2 blend operands using the selected variation data table's region scalars."
  @spec blend([number()], tuple(), non_neg_integer()) ::
          {:ok, [number()]} | {:error, {atom(), map()}}
  def blend(values, store, index) do
    FontValidator.with_budget(fn ->
      try do
        unless is_integer(index) and index >= 0 and index < tuple_size(store),
          do: invalid("CFF2 blend selects an unavailable variation data table")

        {operands, last} = Enum.split(values, -1)

        count =
          case last do
            [n] when is_number(n) and n >= 0 and n == trunc(n) -> trunc(n)
            _ -> invalid("CFF2 blend requires an integer result count")
          end

        scalars = elem(store, index).scalars
        required = count * (length(scalars) + 1)

        unless length(operands) >= required,
          do: invalid("CFF2 blend has insufficient base values and region deltas")

        FontValidator.reserve(:max_work, required)
        {prefix, selected} = Enum.split(operands, length(operands) - required)
        {bases, deltas} = Enum.split(selected, count)

        rows =
          if count == 0 or scalars == [],
            do: [],
            else: Enum.chunk_every(deltas, length(scalars), length(scalars), [])

        rows = if scalars == [], do: List.duplicate([], count), else: rows

        blended =
          Enum.zip(bases, rows)
          |> Enum.map(fn {base, row} ->
            Enum.zip(row, scalars)
            |> Enum.reduce(base, fn {delta, scalar}, value -> value + delta * scalar end)
          end)

        {:ok, prefix ++ blended}
      catch
        {:invalid_variation, error} -> error
      end
    end)
  end

  @doc "Computes the product of OpenType axis support scalars for admitted normalized region triples."
  @spec scalar([{number(), number(), number()}], [number()]) :: number()
  def scalar(region, coordinates) do
    Enum.zip(region, coordinates)
    |> Enum.reduce(1, fn {{start, peak, finish}, value}, product ->
      factor =
        cond do
          peak == 0 or start > peak or peak > finish or (start < 0 and finish > 0) -> 1
          value == peak -> 1
          value <= start or value >= finish -> 0
          value < peak -> (value - start) / (peak - start)
          true -> (finish - value) / (finish - peak)
        end

      product * factor
    end)
  end

  defp read(data, offset, size) do
    FontValidator.reserve(:max_work, max(size, 1))

    unless offset >= 0 and size >= 0 and offset + size <= byte_size(data),
      do: invalid("variation store range is outside its owning table")

    binary_part(data, offset, size)
  end

  defp split(data, size) do
    <<first::binary-size(^size), rest::binary>> = data
    {first, rest}
  end

  defp invalid(message),
    do:
      throw(
        {:invalid_variation,
         Diagnostics.error(:font, :invalid_font, message,
           module: __MODULE__,
           operation: :prepare_variations,
           source: "OpenType variation store"
         )}
      )
end
