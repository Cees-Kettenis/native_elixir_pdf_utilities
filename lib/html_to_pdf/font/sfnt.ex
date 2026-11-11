defmodule NativeElixirPdfUtilities.HtmlToPdf.Font.Sfnt do
  @moduledoc false

  import Bitwise

  @doc "Rebuilds a validated sfnt table set with aligned tables and the whole-font checksum."
  @spec rebuild(non_neg_integer(), [{binary(), binary()}]) :: binary()
  def rebuild(scaler, tables) do
    count = length(tables)
    selector = :math.log2(count) |> floor()
    search_range = (1 <<< selector) * 16
    range_shift = count * 16 - search_range
    first_offset = 12 + 16 * count

    {records, payloads, _next_offset, table_checksum} =
      Enum.reduce(tables, {[], [], first_offset, 0}, fn {tag, data},
                                                        {records, payloads, offset, total} ->
        padding = rem(4 - rem(byte_size(data), 4), 4)
        table_checksum = checksum(data)
        record = <<tag::binary-size(4), table_checksum::32, offset::32, byte_size(data)::32>>

        {[record | records], [:binary.copy(<<0>>, padding), data | payloads],
         offset + byte_size(data) + padding, total + table_checksum}
      end)

    directory =
      <<scaler::32, count::16, search_range::16, selector::16, range_shift::16>> <>
        IO.iodata_to_binary(Enum.reverse(records))

    font = IO.iodata_to_binary([directory, Enum.reverse(payloads)])
    head_offset = first_offset + table_payload_offset(tables, "head")
    adjustment_offset = head_offset + 8
    # Tables start on word boundaries, so their existing checksums also give
    # the payload checksum without scanning the assembled font a second time.
    adjustment = 0xB1B0_AFBA - checksum(directory) - table_checksum &&& 0xFFFF_FFFF
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

  @doc "Returns the OpenType checksum of bytes padded to a complete 32-bit word."
  @spec checksum(binary()) :: non_neg_integer()
  def checksum(data) do
    checksum_words(data, 0)
  end

  defp checksum_words(data, total) do
    case data do
      <<a::32, b::32, c::32, d::32, e::32, f::32, g::32, h::32, i::32, j::32, k::32, l::32, m::32,
        n::32, o::32, p::32, remaining::binary>> ->
        checksum_words(
          remaining,
          total + a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p
        )

      <<first::32, second::32, third::32, fourth::32, remaining::binary>> ->
        checksum_words(remaining, total + first + second + third + fourth)

      <<word::32, remaining::binary>> ->
        checksum_words(remaining, total + word)

      <<>> ->
        total &&& 0xFFFF_FFFF

      remaining ->
        # OpenType checksums pad the final word with zero bytes on the right.
        word = :binary.decode_unsigned(remaining) <<< ((4 - byte_size(remaining)) * 8)
        total + word &&& 0xFFFF_FFFF
    end
  end
end
