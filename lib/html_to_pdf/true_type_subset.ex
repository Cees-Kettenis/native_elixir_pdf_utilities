defmodule NativeElixirPdfUtilities.HtmlToPdf.TrueTypeSubset do
  @moduledoc false

  import Bitwise

  alias NativeElixirPdfUtilities.Validators.TrueTypeSubsetValidator

  @doc false
  @spec subset(map(), [non_neg_integer()]) ::
          {:ok, binary()} | {:error, {atom(), map()}}
  def subset(font, glyph_ids) do
    case TrueTypeSubsetValidator.prepare(font, glyph_ids) do
      {:ok, :keep_full} ->
        {:ok, font.data}

      {:ok, plan} ->
        {glyf, loca} = glyph_tables(plan)

        tables =
          plan.tables
          |> Enum.reject(&(elem(&1, 0) == "DSIG"))
          |> Enum.map(fn
            {"glyf", _data} -> {"glyf", glyf}
            {"loca", _data} -> {"loca", loca}
            {"head", data} -> {"head", put_u32(data, 8, 0)}
            table -> table
          end)

        {:ok, rebuild(plan.scaler, tables)}

      {:error, _reason} = error ->
        error
    end
  end

  defp glyph_tables(plan) do
    {glyph_chunks, offsets, position} =
      Enum.reduce(0..(plan.glyph_count - 1), {[], [], 0}, fn glyph_id,
                                                             {chunks, offsets, position} ->
        start = elem(plan.offsets, glyph_id)
        finish = elem(plan.offsets, glyph_id + 1)

        glyph =
          if MapSet.member?(plan.retained, glyph_id),
            do: binary_part(plan.glyf, start, finish - start),
            else: <<>>

        padding = rem(2 - rem(byte_size(glyph), 2), 2)

        {[:binary.copy(<<0>>, padding), glyph | chunks], [position | offsets],
         position + byte_size(glyph) + padding}
      end)

    offsets = Enum.reverse([position | offsets])

    loca =
      case plan.location_format do
        0 -> for offset <- offsets, into: <<>>, do: <<div(offset, 2)::16>>
        1 -> for offset <- offsets, into: <<>>, do: <<offset::32>>
      end

    {glyph_chunks |> Enum.reverse() |> IO.iodata_to_binary(), loca}
  end

  defp rebuild(scaler, tables) do
    count = length(tables)
    selector = :math.log2(count) |> floor()
    search_range = (1 <<< selector) * 16
    range_shift = count * 16 - search_range
    first_offset = 12 + 16 * count

    {records, payloads, _next_offset} =
      Enum.reduce(tables, {[], [], first_offset}, fn {tag, data}, {records, payloads, offset} ->
        padding = rem(4 - rem(byte_size(data), 4), 4)
        record = <<tag::binary-size(4), checksum(data)::32, offset::32, byte_size(data)::32>>

        {[record | records], [:binary.copy(<<0>>, padding), data | payloads],
         offset + byte_size(data) + padding}
      end)

    directory =
      <<scaler::32, count::16, search_range::16, selector::16, range_shift::16>> <>
        IO.iodata_to_binary(Enum.reverse(records))

    font = IO.iodata_to_binary([directory, Enum.reverse(payloads)])
    head_offset = first_offset + table_payload_offset(tables, "head")
    adjustment_offset = head_offset + 8
    adjustment = 0xB1B0_AFBA - checksum(font) &&& 0xFFFF_FFFF
    <<before::binary-size(^adjustment_offset), _old::32, after_adjustment::binary>> = font
    <<before::binary, adjustment::32, after_adjustment::binary>>
  end

  defp table_payload_offset(tables, tag) do
    tables
    |> Enum.take_while(&(elem(&1, 0) != tag))
    |> Enum.reduce(0, fn {_name, data}, total ->
      total + byte_size(data) + rem(4 - rem(byte_size(data), 4), 4)
    end)
  end

  defp checksum(data) do
    padding = rem(4 - rem(byte_size(data), 4), 4)

    for <<word::32 <- data <> :binary.copy(<<0>>, padding)>>, reduce: 0 do
      total -> total + word &&& 0xFFFF_FFFF
    end
  end

  defp put_u32(data, offset, value) do
    <<before::binary-size(^offset), _old::32, after_value::binary>> = data
    <<before::binary, value::32, after_value::binary>>
  end
end
