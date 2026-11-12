defmodule NativeElixirPdfUtilities.Validators.ColrGradientValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.{FontValidator, HtmlValidator, SvgValidator}

  @doc "Rasterizes an admitted COLR two-circle radial gradient, including negative and shrinking radii, using the OpenType sampling equation."
  @spec prepare_radial(
          [number()],
          non_neg_integer(),
          [{number(), tuple()}],
          tuple(),
          pos_integer(),
          number()
        ) :: {:ok, binary()} | {:error, {atom(), map()}}
  def prepare_radial(values, extend, stops, {left, bottom, right, top}, units, size) do
    FontValidator.with_budget(fn ->
      width = max(1, ceil((right - left) * size / units / 0.75 * Limits.get(:svg_raster_scale)))
      height = max(1, ceil((top - bottom) * size / units / 0.75 * Limits.get(:svg_raster_scale)))
      budget = HtmlValidator.new_image_budget()

      with :ok <- SvgValidator.validate_raster_dimensions(width, height, budget) do
        FontValidator.reserve(:max_work, width * height * (length(stops) + 1))

        rows =
          for row <- 0..(height - 1) do
            [
              <<0>>,
              for column <- 0..(width - 1), into: <<>> do
                x = left + (column + 0.5) * (right - left) / width
                y = top - (row + 0.5) * (top - bottom) / height

                rgba =
                  case radial_parameter(values, x, y) do
                    nil -> {0, 0, 0, 0}
                    value -> sample(stops, value, extend)
                  end

                {r, g, b, a} = rgba
                <<round(r * 255), round(g * 255), round(b * 255), round(a * 255)>>
              end
            ]
          end

        png =
          IO.iodata_to_binary([
            <<137, 80, 78, 71, 13, 10, 26, 10>>,
            chunk("IHDR", <<width::32, height::32, 8, 6, 0, 0, 0>>),
            chunk("IDAT", :zlib.compress(IO.iodata_to_binary(rows))),
            chunk("IEND", <<>>)
          ])

        FontValidator.check(:max_decoded_bytes, byte_size(png))
        {:ok, png}
      end
    end)
  end

  @doc "Resolves the largest valid OpenType radial parameter for a design point, or nil outside the cone."
  @spec radial_parameter([number()], number(), number()) :: number() | nil
  def radial_parameter([x0, y0, r0, x1, y1, r1], x, y) do
    dx = x1 - x0
    dy = y1 - y0
    dr = r1 - r0
    qx = x - x0
    qy = y - y0
    a = dx * dx + dy * dy - dr * dr
    b = -2 * (qx * dx + qy * dy + r0 * dr)
    c = qx * qx + qy * qy - r0 * r0

    roots =
      cond do
        a == 0 and b == 0 ->
          []

        a == 0 ->
          [-c / b]

        b * b - 4 * a * c < 0 ->
          []

        true ->
          root = :math.sqrt(b * b - 4 * a * c)
          # Use the stable quadratic form to avoid cancellation near the tip.
          q = -0.5 * (b + if(b < 0, do: -root, else: root))
          if q == 0, do: [-b / (2 * a)], else: [q / a, c / q]
      end

    roots |> Enum.filter(&(r0 + dr * &1 > 0)) |> Enum.max(fn -> nil end)
  end

  @doc "Samples an admitted COLR color line with its extend mode and premultiplied sRGB interpolation."
  @spec sample([{number(), tuple()}], number(), non_neg_integer()) :: tuple()
  def sample(stops, value, extend) do
    {first, first_color} = hd(stops)
    {last, last_color} = List.last(stops)

    if first == last do
      if extend == 0, do: if(value < first, do: first_color, else: last_color), else: {0, 0, 0, 0}
    else
      value =
        case extend do
          0 ->
            min(last, max(first, value))

          1 ->
            first +
              (value - first - Float.floor((value - first) / (last - first)) * (last - first))

          2 ->
            period = (value - first) / (last - first)
            whole = Float.floor(period)
            fraction = period - whole

            first +
              if(rem(trunc(whole), 2) == 0, do: fraction, else: 1 - fraction) * (last - first)
        end

      # At a shared offset, the last stop at that offset owns the outgoing
      # color. This preserves hard transitions and duplicate color stops.
      pairs = Enum.chunk_every(stops, 2, 1, :discard)

      case Enum.find(pairs, fn [{a, _}, {b, _}] -> value >= a and value < b end) do
        nil ->
          last_color

        [{a, {r, g, b, alpha}}, {b_offset, {rr, gg, bb, aa}}] ->
          t = (value - a) / (b_offset - a)
          result_alpha = alpha * (1 - t) + aa * t

          if result_alpha == 0,
            do: {0, 0, 0, 0},
            else:
              {(r * alpha * (1 - t) + rr * aa * t) / result_alpha,
               (g * alpha * (1 - t) + gg * aa * t) / result_alpha,
               (b * alpha * (1 - t) + bb * aa * t) / result_alpha, result_alpha}
      end
    end
  end

  defp chunk(type, data),
    do: <<byte_size(data)::32, type::binary, data::binary, :erlang.crc32(type <> data)::32>>
end
