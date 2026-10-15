defmodule NativeElixirPdfUtilities.HtmlToPdf.CssColor do
  @moduledoc false

  @doc "Converts validator-admitted absolute color channels to clipped sRGB."
  @spec to_srgb(String.t(), [number()]) :: [float()]
  def to_srgb(space, [a, b, c] = channels) do
    result =
      case space do
        space when space in ~w(rgb rgba srgb) ->
          channels

        space when space in ~w(hsl hsla) ->
          hsl(a, b, c)

        "hwb" ->
          if b + c >= 1,
            do: List.duplicate(b / (b + c), 3),
            else: Enum.map(hsl(a, 1, 0.5), &(b + &1 * (1 - b - c)))

        "srgb-linear" ->
          Enum.map(channels, &encode/1)

        space when space in ~w(display-p3 display-p3-linear) ->
          linear =
            if space == "display-p3-linear", do: channels, else: Enum.map(channels, &decode/1)

          linear
          |> matrix([
            [1.2249402, -0.22494018, 0],
            [-0.042056955, 1.0420569, 0],
            [-0.019637555, -0.07863604, 1.0982736]
          ])
          |> Enum.map(&encode/1)

        "a98-rgb" ->
          channels
          |> Enum.map(&signed_power(&1, 2.2))
          |> matrix([
            [66_942_405 / 47_872_228, -19_070_177 / 47_872_228, 0],
            [0, 1, 0],
            [0, -11_512_411 / 268_173_353, 279_685_764 / 268_173_353]
          ])
          |> Enum.map(&encode/1)

        "prophoto-rgb" ->
          channels
          |> Enum.map(fn n -> if abs(n) <= 16 / 512, do: n / 16, else: signed_power(n, 1.8) end)
          |> matrix([
            [2.0343676, -0.7276345, -0.30673307],
            [-0.22882679, 1.2317533, -0.002926598],
            [-0.008558424, -0.1532682, 1.1618266]
          ])
          |> Enum.map(&encode/1)

        "rec2020" ->
          channels
          |> Enum.map(fn n ->
            if abs(n) < 0.081242858,
              do: n / 4.5,
              else:
                signed_power((abs(n) + 0.0992968268) / 1.0992968268, 1 / 0.45) *
                  if(n < 0, do: -1, else: 1)
          end)
          |> matrix([
            [
              2_785_571_537 / 1_677_558_947,
              -985_802_650 / 1_677_558_947,
              -122_209_940 / 1_677_558_947
            ],
            [
              -4_638_020_506 / 37_238_079_773,
              42_187_016_744 / 37_238_079_773,
              -310_916_465 / 37_238_079_773
            ],
            [
              -97_469_024 / 5_369_968_309,
              -3_780_738_464 / 37_589_778_163,
              42_052_799_795 / 37_589_778_163
            ]
          ])
          |> Enum.map(&encode/1)

        "xyz-d50" ->
          channels
          |> matrix([
            [3.134136, -1.617386, -0.49066222],
            [-0.97879547, 1.9162544, 0.033442874],
            [0.07195539, -0.22897676, 1.4053861]
          ])
          |> Enum.map(&encode/1)

        space when space in ~w(xyz xyz-d65) ->
          channels
          |> matrix([
            [12831 / 3959, -329 / 214, -1974 / 3959],
            [-851_781 / 878_810, 1_648_619 / 878_810, 36519 / 878_810],
            [705 / 12673, -2585 / 12673, 705 / 667]
          ])
          |> Enum.map(&encode/1)

        space when space in ~w(lab lch) ->
          [l, a, b] =
            if space == "lch",
              do: [
                a,
                max(b, 0) * :math.cos(c * :math.pi() / 180),
                max(b, 0) * :math.sin(c * :math.pi() / 180)
              ],
              else: channels

          f = (min(max(l, 0), 100) + 16) / 116

          [f + a / 500, f, f - b / 200]
          |> Enum.map(fn n -> if n > 6 / 29, do: n * n * n, else: (116 * n - 16) * 27 / 24389 end)
          |> matrix([
            [3.0222337, -1.617386, -0.40484765],
            [-0.94384825, 1.9162544, 0.027593868],
            [0.06938627, -0.22897676, 1.1595905]
          ])
          |> Enum.map(&encode/1)

        space when space in ~w(oklab oklch) ->
          channels =
            if space == "oklch",
              do: [
                a,
                max(b, 0) * :math.cos(c * :math.pi() / 180),
                max(b, 0) * :math.sin(c * :math.pi() / 180)
              ],
              else: channels

          channels
          |> matrix([
            [1, 0.39633778, 0.21580376],
            [1, -0.105561346, -0.06385417],
            [1, -0.08948418, -1.2914855]
          ])
          |> Enum.map(&(&1 * &1 * &1))
          |> matrix([
            [4.0767417, -3.3077116, 0.23096994],
            [-1.268438, 2.6097574, -0.34131938],
            [-0.0041960863, -0.7034186, 1.7076147]
          ])
          |> Enum.map(&encode/1)
      end

    Enum.map(result, &(&1 |> max(0.0) |> min(1.0)))
  end

  defp hsl(hue, saturation, lightness) do
    saturation = max(saturation, 0)
    lightness = lightness |> max(0) |> min(1)
    amplitude = saturation * min(lightness, 1 - lightness)

    Enum.map([0, 8, 4], fn offset ->
      value = offset + hue / 30
      k = value - Float.floor(value / 12) * 12
      lightness - amplitude * max(-1, min(1, min(k - 3, 9 - k)))
    end)
  end

  defp matrix([a, b, c], rows), do: Enum.map(rows, fn [x, y, z] -> a * x + b * y + c * z end)
  defp signed_power(n, power), do: :math.pow(abs(n), power) * if(n < 0, do: -1, else: 1)

  defp decode(n),
    do:
      if(abs(n) <= 0.04045,
        do: n / 12.92,
        else: signed_power((abs(n) + 0.055) / 1.055, 2.4) * if(n < 0, do: -1, else: 1)
      )

  defp encode(n),
    do:
      if(abs(n) <= 0.0031308,
        do: n * 12.92,
        else: (1.055 * :math.pow(abs(n), 1 / 2.4) - 0.055) * if(n < 0, do: -1, else: 1)
      )
end
