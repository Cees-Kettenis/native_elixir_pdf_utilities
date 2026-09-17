defmodule NativeElixirPdfUtilities.Validators.PngValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics

  @doc false
  @spec prepare(binary()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(chunks) do
    read_chunks(chunks, %{idat: [], phase: :header, palette: false, transparency: nil})
  end

  defp read_chunks(chunks, state) do
    case chunks do
      <<length::32, type::binary-size(4), data::binary-size(length), crc::32, rest::binary>> ->
        cond do
          not Regex.match?(~r/\A[A-Za-z]{2}[A-Z][A-Za-z]\z/, type) ->
            error("chunk type must contain letters with an uppercase reserved character")

          :erlang.crc32(:erlang.crc32(type), data) != crc ->
            error("#{type} chunk CRC does not match its data")

          state.phase == :header and type != "IHDR" ->
            error("IHDR must be the first chunk")

          true ->
            prepare_chunk(type, data, rest, state)
        end

      _ ->
        error("PNG has a truncated chunk or is missing IEND")
    end
  end

  defp prepare_chunk(type, data, rest, state) do
    case {type, state.phase} do
      {"IHDR", :header} ->
        case data do
          <<width::32, height::32, 8, color_type, 0, 0, 0>>
          when width in 1..0x7FFFFFFF and height in 1..0x7FFFFFFF and color_type in [2, 6] ->
            # PNG dimensions are fixed to positive signed 31-bit integers by the format.
            read_chunks(
              rest,
              Map.merge(state, %{
                width_px: width,
                height_px: height,
                color_type: color_type,
                phase: :before_data
              })
            )

          _ ->
            error("IHDR must describe a supported non-interlaced 8-bit RGB or RGBA image")
        end

      {"IHDR", _} ->
        error("IHDR must occur exactly once")

      {"PLTE", :before_data} ->
        if not state.palette and is_nil(state.transparency) and byte_size(data) in 3..768 and
             rem(byte_size(data), 3) == 0 do
          read_chunks(rest, %{state | palette: true})
        else
          error("PLTE must be unique, precede tRNS, and contain 1 to 256 RGB entries")
        end

      {"PLTE", _} ->
        error("PLTE must precede IDAT")

      {"tRNS", :before_data} ->
        if is_nil(state.transparency) do
          case {state.color_type, data} do
            {2, <<red::16, green::16, blue::16>>}
            when red <= 255 and green <= 255 and blue <= 255 ->
              read_chunks(rest, %{state | transparency: {red, green, blue}})

            _ ->
              error("tRNS must contain a valid transparent color for an 8-bit RGB image")
          end
        else
          error("tRNS must occur at most once")
        end

      {"tRNS", _} ->
        error("tRNS must precede IDAT")

      {"IDAT", phase} when phase in [:before_data, :data] ->
        read_chunks(rest, %{state | phase: :data, idat: [data | state.idat]})

      {"IDAT", _} ->
        error("IDAT chunks must be consecutive")

      {"IEND", phase} when phase in [:data, :after_data] ->
        if data == "" and rest == "" do
          {:ok, state}
        else
          error("IEND must be empty and the final chunk")
        end

      {"IEND", _} ->
        error("PNG requires IDAT before IEND")

      {<<first, _::binary>>, _} when first in ?A..?Z ->
        error("unsupported critical PNG chunk #{type}")

      _ ->
        phase = if state.phase == :data, do: :after_data, else: state.phase
        read_chunks(rest, %{state | phase: phase})
    end
  end

  defp error(message) do
    Diagnostics.error(:style, :invalid_document, message, source: "PNG")
  end
end
