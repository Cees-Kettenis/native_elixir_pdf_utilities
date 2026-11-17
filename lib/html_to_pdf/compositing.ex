defmodule NativeElixirPdfUtilities.HtmlToPdf.Compositing do
  @moduledoc "Encoded-sRGB static compositing for projected CSS paint planes."

  @doc "Composites admitted unpremultiplied RGBA channels in [0,1] using the canonical PDF/CSS blend vocabulary."
  @spec over(tuple(), tuple(), atom()) :: tuple()
  def over({r, g, b, a} = source, {br, bg, bb, ba} = backdrop, mode) do
    cond do
      a == 0 ->
        backdrop

      ba == 0 ->
        source

      true ->
        {cr, cg, cb} = blend({br, bg, bb}, {r, g, b}, mode)
        alpha = a + ba * (1 - a)
        component = fn s, b, c -> (a * ((1 - ba) * s + ba * c) + (1 - a) * ba * b) / alpha end
        {component.(r, br, cr), component.(g, bg, cg), component.(b, bb, cb), alpha}
    end
  end

  defp blend(backdrop, source, mode) do
    case mode do
      :Hue ->
        set_lum(set_sat(source, sat(backdrop)), lum(backdrop))

      :Saturation ->
        set_lum(set_sat(backdrop, sat(source)), lum(backdrop))

      :Color ->
        set_lum(source, lum(backdrop))

      :Luminosity ->
        set_lum(backdrop, lum(source))

      _ ->
        for {b, s} <- Enum.zip(Tuple.to_list(backdrop), Tuple.to_list(source)) do
          case mode do
            :Normal ->
              s

            :Multiply ->
              b * s

            :Screen ->
              b + s - b * s

            :Overlay ->
              if b <= 0.5, do: 2 * b * s, else: 1 - 2 * (1 - b) * (1 - s)

            :Darken ->
              min(b, s)

            :Lighten ->
              max(b, s)

            :ColorDodge ->
              cond do
                b == 0 -> 0.0
                s == 1 -> 1.0
                true -> min(1.0, b / (1 - s))
              end

            :ColorBurn ->
              cond do
                b == 1 -> 1.0
                s == 0 -> 0.0
                true -> 1 - min(1.0, (1 - b) / s)
              end

            :HardLight ->
              if s <= 0.5, do: 2 * b * s, else: 1 - 2 * (1 - b) * (1 - s)

            :SoftLight ->
              if s <= 0.5 do
                b - (1 - 2 * s) * b * (1 - b)
              else
                d = if b <= 0.25, do: ((16 * b - 12) * b + 4) * b, else: :math.sqrt(b)
                b + (2 * s - 1) * (d - b)
              end

            :Difference ->
              abs(b - s)

            :Exclusion ->
              b + s - 2 * b * s
          end
        end
        |> List.to_tuple()
    end
  end

  defp lum({r, g, b}), do: 0.3 * r + 0.59 * g + 0.11 * b
  defp sat(rgb), do: Enum.max(Tuple.to_list(rgb)) - Enum.min(Tuple.to_list(rgb))

  defp set_lum(rgb, target) do
    delta = target - lum(rgb)
    values = rgb |> Tuple.to_list() |> Enum.map(&(&1 + delta))
    low = Enum.min(values)
    high = Enum.max(values)

    values =
      if low < 0,
        do: Enum.map(values, &(target + (&1 - target) * target / (target - low))),
        else: values

    values =
      if high > 1,
        do: Enum.map(values, &(target + (&1 - target) * (1 - target) / (high - target))),
        else: values

    List.to_tuple(values)
  end

  defp set_sat(rgb, target) do
    channels = Tuple.to_list(rgb)
    minimum = Enum.min(channels)
    maximum = Enum.max(channels)

    channels
    |> Enum.map(fn value ->
      if maximum > minimum, do: (value - minimum) * target / (maximum - minimum), else: 0.0
    end)
    |> List.to_tuple()
  end
end
