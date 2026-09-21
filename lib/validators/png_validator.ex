defmodule NativeElixirPdfUtilities.Validators.PngValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc false
  @spec prepare(binary()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(chunks) do
    read_chunks(chunks, %{idat: [], phase: :header, palette: nil, transparency: nil})
  end

  @doc false
  @spec prepare_decode(binary(), HtmlValidator.image_budget(), boolean()) ::
          {:ok, map(), binary()} | {:error, {atom(), map()}}
  def prepare_decode(png, budget, reserve?) do
    metadata =
      case png do
        <<137, 80, 78, 71, 13, 10, 26, 10, chunks::binary>> -> prepare(chunks)
        _ -> error("PNG signature is missing or invalid")
      end

    with {:ok, parsed} <- metadata do
      channels = %{0 => 1, 2 => 3, 3 => 1, 4 => 2, 6 => 4}[parsed.color_type]
      depth = parsed.bit_depth
      output_depth = max(depth, 8)
      colors = if parsed.color_type in [0, 4], do: 1, else: 3
      alpha? = parsed.color_type in [4, 6] or parsed.transparency != nil
      stride = (colors + if(alpha?, do: 1, else: 0)) * div(output_depth, 8)

      # Adam7 starts and strides are fixed by the PNG format, not resource limits.
      geometry =
        if parsed.interlace == 0,
          do: [{0, 0, 1, 1}],
          else: [
            {0, 0, 8, 8},
            {4, 0, 8, 8},
            {0, 4, 4, 8},
            {2, 0, 4, 4},
            {0, 2, 2, 4},
            {1, 0, 2, 2},
            {0, 1, 1, 2}
          ]

      passes =
        Enum.map(geometry, fn {x, y, dx, dy} ->
          width = div(max(parsed.width_px - x, 0) + dx - 1, dx)
          height = if width == 0, do: 0, else: div(max(parsed.height_px - y, 0) + dy - 1, dy)

          %{
            x: x,
            y: y,
            dx: dx,
            dy: dy,
            width: width,
            height: height,
            row_bytes: div(width * channels * depth + 7, 8)
          }
        end)

      inflated_bytes = Enum.reduce(passes, 0, &(&2 + &1.height * (&1.row_bytes + 1)))
      output_bytes = parsed.width_px * parsed.height_px * stride
      largest_row = passes |> Enum.map(& &1.row_bytes) |> Enum.max()
      rows = Enum.reduce(passes, 0, &(&2 + &1.height))
      working_bytes = 3 * inflated_bytes + 3 * output_bytes + 64 * largest_row + 64 * rows

      parsed =
        Map.merge(parsed, %{
          passes: passes,
          output_depth: output_depth,
          colors: colors,
          alpha?: alpha?,
          stride: stride,
          filter_stride: max(div(channels * depth + 7, 8), 1)
        })

      with :ok <- validate_working_bytes(working_bytes),
           :ok <-
             if(reserve?,
               do:
                 HtmlValidator.reserve_decoded_image(
                   budget,
                   parsed.width_px,
                   parsed.height_px,
                   stride
                 ),
               else: :ok
             ),
           {:ok, inflated} <-
             inflate(parsed.idat |> Enum.reverse() |> IO.iodata_to_binary(), inflated_bytes),
           :ok <- validate_filters(inflated, passes) do
        {:ok, parsed, inflated}
      end
    end
  end

  @doc false
  @spec validate_palette_samples(binary(), non_neg_integer(), map()) ::
          :ok | {:error, {atom(), map()}}
  def validate_palette_samples(row, width, parsed) do
    if parsed.color_type == 3 do
      depth = parsed.bit_depth
      <<samples::bitstring-size(^width * ^depth), _::bitstring>> = row
      maximum = tuple_size(parsed.palette)

      if Enum.any?(for(<<index::size(^depth) <- samples>>, do: index), &(&1 >= maximum)),
        do: error("pixel index exceeds the PNG palette"),
        else: :ok
    else
      :ok
    end
  end

  defp validate_working_bytes(bytes) do
    maximum = Limits.get(:max_png_working_bytes)

    if bytes > maximum do
      Diagnostics.error(
        :limits,
        :resource_limit_exceeded,
        "PNG decoding work buffers exceed the #{maximum}-byte allowance",
        source: "PNG"
      )
    else
      :ok
    end
  end

  defp inflate(data, expected_size) do
    zlib = :zlib.open()

    try do
      :ok = :zlib.inflateInit(zlib)
      inflate_chunks(zlib, data, expected_size, 0, [])
    rescue
      ErlangError -> error("PNG compressed data is invalid or truncated")
    after
      :zlib.close(zlib)
    end
  end

  defp inflate_chunks(zlib, data, expected, size, chunks) do
    {status, output} = :zlib.safeInflate(zlib, data)
    size = size + IO.iodata_length(output)

    cond do
      size > expected ->
        error("PNG decompressed data exceeds the expected scanline size")

      status == :finished and size == expected ->
        :ok = :zlib.inflateEnd(zlib)
        {:ok, [output | chunks] |> Enum.reverse() |> IO.iodata_to_binary()}

      status == :finished ->
        error("PNG decompressed data is shorter than the expected scanlines")

      true ->
        inflate_chunks(zlib, <<>>, expected, size, [output | chunks])
    end
  end

  defp validate_filters(data, passes) do
    Enum.reduce_while(passes, {:ok, data}, fn pass, {:ok, data} ->
      size = pass.height * (pass.row_bytes + 1)
      <<rows::binary-size(^size), rest::binary>> = data
      row_size = pass.row_bytes

      if Enum.any?(for(<<filter, _::binary-size(^row_size) <- rows>>, do: filter), &(&1 > 4)) do
        {:halt, error("PNG scanline filter must be between 0 and 4")}
      else
        {:cont, {:ok, rest}}
      end
    end)
    |> case do
      {:ok, _} -> :ok
      error -> error
    end
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
          <<width::32, height::32, depth, color_type, 0, 0, interlace>>
          when width in 1..0x7FFFFFFF and height in 1..0x7FFFFFFF and interlace in [0, 1] ->
            # Positive signed 31-bit dimensions and these sample combinations are PNG invariants.
            depths = %{
              0 => [1, 2, 4, 8, 16],
              2 => [8, 16],
              3 => [1, 2, 4, 8],
              4 => [8, 16],
              6 => [8, 16]
            }

            if depth in Map.get(depths, color_type, []) do
              read_chunks(
                rest,
                Map.merge(state, %{
                  width_px: width,
                  height_px: height,
                  bit_depth: depth,
                  color_type: color_type,
                  interlace: interlace,
                  phase: :before_data
                })
              )
            else
              error("IHDR has an invalid color type and sample depth combination")
            end

          _ ->
            error(
              "IHDR must have valid dimensions, compression, filtering, and interlace methods"
            )
        end

      {"IHDR", _} ->
        error("IHDR must occur exactly once")

      {"PLTE", :before_data} ->
        if is_nil(state.palette) and is_nil(state.transparency) and
             state.color_type in [2, 3, 6] and byte_size(data) in 3..768 and
             rem(byte_size(data), 3) == 0 and
             (state.color_type != 3 or div(byte_size(data), 3) <= 2 ** state.bit_depth) do
          palette = for <<pixel::binary-size(3) <- data>>, do: pixel
          read_chunks(rest, %{state | palette: List.to_tuple(palette)})
        else
          error("PLTE must be unique, precede tRNS, and contain 1 to 256 RGB entries")
        end

      {"PLTE", _} ->
        error("PLTE must precede IDAT")

      {"tRNS", :before_data} ->
        if is_nil(state.transparency) do
          case {state.color_type, data} do
            {2, <<red::16, green::16, blue::16>>}
            when red < Bitwise.bsl(1, state.bit_depth) and green < Bitwise.bsl(1, state.bit_depth) and
                   blue < Bitwise.bsl(1, state.bit_depth) ->
              read_chunks(rest, %{state | transparency: {red, green, blue}})

            {0, <<gray::16>>} when gray < Bitwise.bsl(1, state.bit_depth) ->
              read_chunks(rest, %{state | transparency: gray})

            {3, alpha}
            when is_tuple(state.palette) and byte_size(alpha) > 0 and
                   byte_size(alpha) <= tuple_size(state.palette) ->
              read_chunks(rest, %{state | transparency: alpha})

            _ ->
              error("tRNS must match the color type, sample depth, and preceding palette")
          end
        else
          error("tRNS must occur at most once")
        end

      {"tRNS", _} ->
        error("tRNS must precede IDAT")

      {"IDAT", phase} when phase in [:before_data, :data] ->
        if state.color_type == 3 and is_nil(state.palette) do
          error("indexed PNG requires PLTE before IDAT")
        else
          read_chunks(rest, %{state | phase: :data, idat: [data | state.idat]})
        end

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
