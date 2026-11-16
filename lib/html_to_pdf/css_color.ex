defmodule NativeElixirPdfUtilities.HtmlToPdf.CssColor do
  @moduledoc "Color conversion and premultiplied interpolation before the selected sRGB output boundary."

  # CSS Color 4, 12 November 2026, section 19. The matrices describe color
  # coordinate systems, not resource ceilings. D50 is adapted with Bradford.
  @matrices %{
    "srgb" => [
      [0.4123907992659595, 0.357584339383878, 0.1804807884018343],
      [0.2126390058715104, 0.715168678767756, 0.0721923153607337],
      [0.0193308187155918, 0.1191947797946259, 0.9505321522496607]
    ],
    "display-p3" => [
      [0.4865709486482162, 0.2656676931690931, 0.1982172852343625],
      [0.2289745640697488, 0.6917385218365064, 0.079286914093745],
      [0.0, 0.0451133818589026, 1.043944368900976]
    ],
    "a98-rgb" => [
      [0.5766690429101305, 0.1855582379065463, 0.1882286462349947],
      [0.2973449752505361, 0.6273635662554661, 0.0752914584939979],
      [0.0270313613864123, 0.0706888525358272, 0.9913375368376388]
    ],
    "rec2020" => [
      [0.6369580483012914, 0.1446169035862083, 0.1688809751641721],
      [0.2627002120112671, 0.6779980715188708, 0.059301716469862],
      [0.0, 0.0280726930490874, 1.060985057710791]
    ],
    "prophoto-rgb" => [
      [0.7977666449006423, 0.1351812974005331, 0.0313477341283922],
      [0.2880748288194013, 0.711835234241873, 0.0000899369387256],
      [0.0, 0.0, 0.8251046025104601]
    ]
  }
  @inverse Map.new(@matrices, fn {space, [[a, b, c], [d, e, f], [g, h, i]]} ->
             determinant = a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)

             {space,
              Enum.map(
                [
                  [e * i - f * h, c * h - b * i, b * f - c * e],
                  [f * g - d * i, a * i - c * g, c * d - a * f],
                  [d * h - e * g, b * g - a * h, a * e - b * d]
                ],
                fn row -> Enum.map(row, &(&1 / determinant)) end
              )}
           end)
  @d50_to_d65 [
    [0.955473421488075, -0.0230984549487647, 0.0632592432005707],
    [-0.0283697093338637, 1.0099953980813041, 0.0210414411919173],
    [0.0123140148644819, -0.0205076492988989, 1.330365926242124]
  ]
  @d65_to_d50 [
    [1.0479297925449969, 0.0229468706016097, -0.0501922662892052],
    [0.0296278087700559, 0.9904344267538799, -0.0170737990634188],
    [-0.0092430406462045, 0.0150551914902982, 0.7518742814281371]
  ]
  @d50 [0.9642956764295677, 1.0, 0.8251046025104601]

  @doc "Converts admitted channels to sRGB, clipping only at the output boundary."
  @spec to_srgb(String.t(), [number()]) :: [float()]
  def to_srgb(space, channels) do
    values =
      case space do
        space when space in ~w(rgb rgba srgb) -> channels
        space when space in ~w(hsl hsla) -> hsl(channels)
        "hwb" -> hwb(channels)
        _ -> convert(space, channels, "srgb")
      end

    Enum.map(values, &min(max(&1, 0.0), 1.0))
  end

  @doc "Converts between admitted CSS spaces without clipping intermediate channels."
  @spec convert(String.t(), [number()], String.t()) :: [number()]
  def convert(source, channels, target) do
    source = normalize_space(source)
    target = normalize_space(target)
    if source == target, do: channels, else: channels |> to_xyz(source) |> from_xyz(target)
  end

  @doc "Interpolates prepared colors, preserving missing channels and premultiplying non-hue channels by alpha."
  @spec interpolate(map(), map(), number(), String.t(), atom()) :: map()
  def interpolate(first, last, fraction, space, hue_method \\ :shorter) do
    first |> interpolation_segment(last, space, hue_method) |> sample_interpolation(fraction)
  end

  @doc "Prepares one color interpolation segment once for repeated gradient samples."
  @spec interpolation_segment(map(), map(), String.t(), atom()) :: map()
  def interpolation_segment(first, last, space, hue_method) do
    a = interpolation_channels(first, space)
    b = interpolation_channels(last, space)
    alpha_a = if first.alpha == :none, do: last.alpha, else: first.alpha
    alpha_b = if last.alpha == :none, do: first.alpha, else: last.alpha
    alpha_a = if alpha_a == :none, do: 0.0, else: alpha_a
    alpha_b = if alpha_b == :none, do: 0.0, else: alpha_b
    hue = if space in ~w(hsl hwb), do: 0, else: if(space in ~w(lch oklch), do: 2)

    pairs =
      Enum.zip(a, b)
      |> Enum.with_index()
      |> Enum.map(fn {{a, b}, index} ->
        a = if a == :none, do: b, else: a
        b = if b == :none, do: a, else: b
        a = if a == :none, do: 0.0, else: a
        b = if b == :none, do: 0.0, else: b

        cond do
          index == hue ->
            a = a - Float.floor(a / 360) * 360
            b = b - Float.floor(b / 360) * 360
            difference = b - a

            {a, b} =
              case hue_method do
                :shorter when difference > 180 -> {a + 360, b}
                :shorter when difference < -180 -> {a, b + 360}
                :longer when difference > 0 and difference < 180 -> {a + 360, b}
                :longer when difference <= 0 and difference > -180 -> {a, b + 360}
                :increasing when b < a -> {a, b + 360}
                :decreasing when a < b -> {a + 360, b}
                _ -> {a, b}
              end

            {a, b, true}

          true ->
            {a * alpha_a, b * alpha_b, false}
        end
      end)

    %{space: space, pairs: pairs, alpha_a: alpha_a, alpha_b: alpha_b}
  end

  @doc "Samples an already prepared premultiplied color segment."
  @spec sample_interpolation(map(), number()) :: map()
  def sample_interpolation(segment, fraction) do
    alpha = segment.alpha_a * (1 - fraction) + segment.alpha_b * fraction

    channels =
      Enum.map(segment.pairs, fn {a, b, hue?} ->
        value = a * (1 - fraction) + b * fraction

        cond do
          hue? -> value
          alpha == 0 -> 0.0
          true -> value / alpha
        end
      end)

    %{space: segment.space, channels: channels, alpha: alpha}
  end

  defp interpolation_channels(color, space) do
    channels = Enum.map(color.channels, fn n -> if n == :none, do: 0.0, else: n end)
    converted = convert(color.space, channels, space)
    # Analogous coordinates retain missingness when converting spaces.
    categories = fn selected ->
      case selected do
        s when s in ~w(lab oklab) -> [:lightness, :a, :b]
        s when s in ~w(lch oklch) -> [:lightness, :chroma, :hue]
        s when s in ~w(hsl hsla) -> [:hue, :saturation, :lightness]
        "hwb" -> [:hue, :whiteness, :blackness]
        _ -> [:red, :green, :blue]
      end
    end

    missing =
      Enum.zip(categories.(color.space), color.channels)
      |> Enum.filter(&(elem(&1, 1) == :none))
      |> Enum.map(&elem(&1, 0))

    converted =
      Enum.zip(categories.(space), converted)
      |> Enum.map(fn {category, value} -> if category in missing, do: :none, else: value end)

    case {space, converted} do
      {s, [_, c, _]} when s in ~w(lch oklch) and is_number(c) and abs(c) < 0.000004 ->
        List.replace_at(converted, 2, :none)

      {"hsl", [_, s, _]} when is_number(s) and abs(s) < 0.000004 ->
        List.replace_at(converted, 0, :none)

      {"hwb", [_, w, b]} when is_number(w) and is_number(b) and w + b >= 1 ->
        List.replace_at(converted, 0, :none)

      _ ->
        converted
    end
  end

  defp normalize_space(space) do
    case space do
      s when s in ~w(rgb rgba) -> "srgb"
      "hsla" -> "hsl"
      "xyz" -> "xyz-d65"
      s -> s
    end
  end

  defp to_xyz(channels, space) do
    case space do
      "hsl" ->
        to_xyz(hsl(channels), "srgb")

      "hwb" ->
        to_xyz(hwb(channels), "srgb")

      s when s in ~w(lch oklch) ->
        [l, c, h] = channels

        to_xyz(
          [
            l,
            max(c, 0) * :math.cos(h * :math.pi() / 180),
            max(c, 0) * :math.sin(h * :math.pi() / 180)
          ],
          if(s == "lch", do: "lab", else: "oklab")
        )

      "lab" ->
        [l, a, b] = channels
        f = (min(max(l, 0), 100) + 16) / 116

        [f + a / 500, f, f - b / 200]
        |> Enum.map(fn n -> if n > 6 / 29, do: n * n * n, else: (116 * n - 16) * 27 / 24389 end)
        |> Enum.zip_with(@d50, &(&1 * &2))
        |> matrix(@d50_to_d65)

      "oklab" ->
        channels
        |> matrix([
          [1, 0.3963377773761749, 0.2158037573099136],
          [1, -0.1055613458156586, -0.0638541728258133],
          [1, -0.0894841775298119, -1.2914855480194092]
        ])
        |> Enum.map(&(&1 * &1 * &1))
        |> matrix([
          [1.2268798758459243, -0.5578149944602171, 0.2813910456659647],
          [-0.0405757452148008, 1.112286803280317, -0.0717110580655164],
          [-0.0763729366746601, -0.4214933324022432, 1.5869240198367816]
        ])

      "xyz-d65" ->
        channels

      "xyz-d50" ->
        matrix(channels, @d50_to_d65)

      s ->
        base = String.replace_suffix(s, "-linear", "")

        values =
          if String.ends_with?(s, "-linear"),
            do: channels,
            else: Enum.map(channels, &linearize(&1, base))

        values = matrix(values, Map.fetch!(@matrices, base))
        if base == "prophoto-rgb", do: matrix(values, @d50_to_d65), else: values
    end
  end

  defp from_xyz(channels, space) do
    case space do
      "hsl" ->
        rgb_to_hsl(from_xyz(channels, "srgb"))

      "hwb" ->
        rgb = from_xyz(channels, "srgb")
        [h, _, _] = rgb_to_hsl(rgb)
        [h, Enum.min(rgb), 1 - Enum.max(rgb)]

      s when s in ~w(lch oklch) ->
        [l, a, b] = from_xyz(channels, if(s == "lch", do: "lab", else: "oklab"))
        h = :math.atan2(b, a) * 180 / :math.pi()
        [l, :math.sqrt(a * a + b * b), if(h < 0, do: h + 360, else: h)]

      "lab" ->
        [x, y, z] =
          channels
          |> matrix(@d65_to_d50)
          |> Enum.zip_with(@d50, &(&1 / &2))
          |> Enum.map(fn n ->
            if n > 216 / 24389, do: :math.pow(n, 1 / 3), else: (24389 / 27 * n + 16) / 116
          end)

        [116 * y - 16, 500 * (x - y), 200 * (y - z)]

      "oklab" ->
        channels
        |> matrix([
          [0.819022437996703, 0.3619062600528904, -0.1288737815209879],
          [0.0329836539323885, 0.9292868615863434, 0.0361446663506424],
          [0.0481771893596242, 0.2642395317527308, 0.6335478284694309]
        ])
        |> Enum.map(&signed_power(&1, 1 / 3))
        |> matrix([
          [0.210454268309314, 0.7936177747023054, -0.0040720430116193],
          [1.9779985324311684, -2.4285922420485799, 0.450593709617411],
          [0.0259040424655478, 0.7827717124575296, -0.8086757549230774]
        ])

      "xyz-d65" ->
        channels

      "xyz-d50" ->
        matrix(channels, @d65_to_d50)

      s ->
        base = String.replace_suffix(s, "-linear", "")
        values = if base == "prophoto-rgb", do: matrix(channels, @d65_to_d50), else: channels
        values = matrix(values, Map.fetch!(@inverse, base))
        if String.ends_with?(s, "-linear"), do: values, else: Enum.map(values, &encode(&1, base))
    end
  end

  defp linearize(n, space) do
    case space do
      s when s in ~w(srgb display-p3) ->
        if abs(n) <= 0.04045,
          do: n / 12.92,
          else: signed_power((abs(n) + 0.055) / 1.055, 2.4) * sign(n)

      "a98-rgb" ->
        signed_power(n, 563 / 256)

      "prophoto-rgb" ->
        if abs(n) <= 16 / 512, do: n / 16, else: signed_power(n, 1.8)

      "rec2020" ->
        if abs(n) < 0.0812428582986351,
          do: n / 4.5,
          else: signed_power((abs(n) + 0.099296826809442) / 1.099296826809442, 1 / 0.45) * sign(n)
    end
  end

  defp encode(n, space) do
    case space do
      s when s in ~w(srgb display-p3) ->
        if abs(n) <= 0.0031308,
          do: n * 12.92,
          else: (1.055 * :math.pow(abs(n), 1 / 2.4) - 0.055) * sign(n)

      "a98-rgb" ->
        signed_power(n, 256 / 563)

      "prophoto-rgb" ->
        if abs(n) <= 1 / 512, do: n * 16, else: signed_power(n, 1 / 1.8)

      "rec2020" ->
        if abs(n) < 0.018053968510807,
          do: n * 4.5,
          else: (1.099296826809442 * :math.pow(abs(n), 0.45) - 0.099296826809442) * sign(n)
    end
  end

  defp hsl([h, s, l]) do
    s = min(max(s, 0), 1)
    l = min(max(l, 0), 1)
    amplitude = s * min(l, 1 - l)

    Enum.map([0, 8, 4], fn offset ->
      value = offset + h / 30
      k = value - Float.floor(value / 12) * 12
      l - amplitude * max(-1, min(1, min(k - 3, 9 - k)))
    end)
  end

  defp hwb([h, w, b]) do
    w = max(w, 0)
    b = max(b, 0)

    if w + b >= 1,
      do: List.duplicate(w / (w + b), 3),
      else: Enum.map(hsl([h, 1, 0.5]), &(w + &1 * (1 - w - b)))
  end

  defp rgb_to_hsl([r, g, b]) do
    high = max(r, max(g, b))
    low = min(r, min(g, b))
    l = (high + low) / 2
    d = high - low

    if d == 0 do
      [0.0, 0.0, l]
    else
      s = if l == 0 or l == 1, do: 0.0, else: d / (1 - abs(2 * l - 1))

      h =
        cond do
          high == r -> (g - b) / d
          high == g -> (b - r) / d + 2
          true -> (r - g) / d + 4
        end

      h = h * 60
      [if(h < 0, do: h + 360, else: h), s, l]
    end
  end

  defp matrix([a, b, c], rows), do: Enum.map(rows, fn [x, y, z] -> a * x + b * y + c * z end)
  defp signed_power(n, power), do: :math.pow(abs(n), power) * sign(n)
  defp sign(n), do: if(n < 0, do: -1, else: 1)
end
