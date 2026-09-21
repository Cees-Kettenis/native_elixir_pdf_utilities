defmodule NativeElixirPdfUtilities.HtmlToPdf.PngDecoder do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.PngValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc false
  @spec decode(binary(), HtmlValidator.image_budget(), boolean()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def decode(png, budget, reserve?) do
    with {:ok, parsed, inflated} <- PngValidator.prepare_decode(png, budget, reserve?),
         {:ok, passes} <- decode_passes(inflated, parsed) do
      pixels =
        case parsed.interlace do
          0 -> passes |> hd() |> Map.fetch!(:rows) |> Tuple.to_list() |> IO.iodata_to_binary()
          1 -> interlace_pixels(passes, parsed)
        end

      color_bytes = parsed.colors * div(parsed.output_depth, 8)
      alpha_bytes = div(parsed.output_depth, 8)

      {data, alpha} =
        if parsed.alpha? do
          color =
            for <<color::binary-size(^color_bytes), _::binary-size(^alpha_bytes) <- pixels>>,
              into: <<>>,
              do: color

          mask =
            for <<_::binary-size(^color_bytes), alpha::binary-size(^alpha_bytes) <- pixels>>,
              into: <<>>,
              do: alpha

          mask = if mask == :binary.copy(<<255>>, byte_size(mask)), do: nil, else: mask
          {color, mask}
        else
          {pixels, nil}
        end

      image = %{
        format: :png,
        data: data,
        width_px: parsed.width_px,
        height_px: parsed.height_px,
        width: parsed.width_px * 0.75,
        height: parsed.height_px * 0.75,
        color_space: if(parsed.colors == 1, do: :device_gray, else: :device_rgb),
        bits_per_component: parsed.output_depth
      }

      {:ok, if(is_nil(alpha), do: image, else: Map.put(image, :alpha_data, alpha))}
    end
  end

  defp decode_passes(data, parsed) do
    Enum.reduce_while(parsed.passes, {:ok, [], data}, fn pass, {:ok, passes, data} ->
      case decode_rows(data, pass, parsed, pass.height, "", []) do
        {:ok, rows, rest} ->
          {:cont, {:ok, [Map.put(pass, :rows, List.to_tuple(rows)) | passes], rest}}

        error ->
          {:halt, error}
      end
    end)
    |> case do
      {:ok, passes, _rest} -> {:ok, Enum.reverse(passes)}
      error -> error
    end
  end

  defp decode_rows(data, pass, parsed, remaining, previous, rows) do
    case remaining do
      0 ->
        {:ok, Enum.reverse(rows), data}

      _ ->
        size = pass.row_bytes
        <<filter, row::binary-size(^size), rest::binary>> = data
        row = unfilter_row(filter, row, previous, parsed.filter_stride)

        with :ok <- PngValidator.validate_palette_samples(row, pass.width, parsed) do
          pixels = expand_row(row, pass.width, parsed)
          decode_rows(rest, pass, parsed, remaining - 1, row, [pixels | rows])
        end
    end
  end

  defp expand_row(row, width, parsed) do
    depth = parsed.bit_depth
    out = parsed.output_depth
    maximum = Bitwise.bsl(1, out) - 1
    transparent = parsed.transparency

    case parsed.color_type do
      0 ->
        <<samples::bitstring-size(^width * ^depth), _::bitstring>> = row

        for <<gray::size(^depth) <- samples>>, into: <<>> do
          scaled = div(gray * maximum, Bitwise.bsl(1, depth) - 1)

          if parsed.alpha?,
            do: <<scaled::size(out), if(gray == transparent, do: 0, else: maximum)::size(out)>>,
            else: <<scaled::size(out)>>
        end

      2 ->
        if parsed.alpha? do
          for <<r::size(^depth), g::size(^depth), b::size(^depth) <- row>>, into: <<>> do
            alpha = if {r, g, b} == transparent, do: 0, else: maximum
            <<r::size(out), g::size(out), b::size(out), alpha::size(out)>>
          end
        else
          row
        end

      3 ->
        <<samples::bitstring-size(^width * ^depth), _::bitstring>> = row

        for <<index::size(^depth) <- samples>>, into: <<>> do
          color = elem(parsed.palette, index)

          if parsed.alpha? do
            alpha =
              if index < byte_size(transparent), do: :binary.at(transparent, index), else: 255

            color <> <<alpha>>
          else
            color
          end
        end

      type when type in [4, 6] ->
        row
    end
  end

  defp interlace_pixels(passes, parsed) do
    passes = List.to_tuple(passes)
    stride = parsed.stride

    for y <- 0..(parsed.height_px - 1), into: <<>> do
      for x <- 0..(parsed.width_px - 1), into: <<>> do
        index =
          cond do
            rem(y, 2) == 1 -> 6
            rem(x, 2) == 1 -> 5
            rem(y, 4) == 2 -> 4
            rem(x, 4) == 2 -> 3
            rem(y, 8) == 4 -> 2
            rem(x, 8) == 4 -> 1
            true -> 0
          end

        pass = elem(passes, index)
        row = elem(pass.rows, div(y - pass.y, pass.dy))
        binary_part(row, div(x - pass.x, pass.dx) * stride, stride)
      end
    end
  end

  defp unfilter_row(filter, row, previous, bytes_per_pixel) do
    case filter do
      0 ->
        row

      _ ->
        previous =
          case previous do
            "" -> :binary.copy(<<0>>, byte_size(row))
            previous -> previous
          end

        unfilter_bytes(filter, row, previous, bytes_per_pixel, 0, [], [], [])
    end
  end

  defp unfilter_bytes(
         filter,
         row,
         previous,
         bytes_per_pixel,
         index,
         left_window,
         up_left_window,
         acc
       ) do
    case {row, previous} do
      {"", ""} ->
        acc
        |> Enum.reverse()
        |> :binary.list_to_bin()

      {<<byte, row_rest::binary>>, <<up, previous_rest::binary>>} ->
        left = if index >= bytes_per_pixel, do: hd(left_window), else: 0
        up_left = if index >= bytes_per_pixel, do: hd(up_left_window), else: 0

        predictor =
          case filter do
            1 -> left
            2 -> up
            3 -> div(left + up, 2)
            4 -> paeth(left, up, up_left)
          end

        decoded = rem(byte + predictor, 256)

        unfilter_bytes(
          filter,
          row_rest,
          previous_rest,
          bytes_per_pixel,
          index + 1,
          window_push(left_window, decoded, bytes_per_pixel),
          window_push(up_left_window, up, bytes_per_pixel),
          [decoded | acc]
        )
    end
  end

  defp window_push(window, byte, bytes_per_pixel) do
    case length(window) < bytes_per_pixel do
      true -> window ++ [byte]
      false -> tl(window) ++ [byte]
    end
  end

  defp paeth(left, up, up_left) do
    estimate = left + up - up_left
    left_distance = abs(estimate - left)
    up_distance = abs(estimate - up)
    up_left_distance = abs(estimate - up_left)

    cond do
      left_distance <= up_distance and left_distance <= up_left_distance -> left
      up_distance <= up_left_distance -> up
      true -> up_left
    end
  end
end
