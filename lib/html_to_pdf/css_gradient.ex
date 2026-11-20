defmodule NativeElixirPdfUtilities.HtmlToPdf.CssGradient do
  @moduledoc "Static CSS gradient geometry and bounded raster sampling."
  alias NativeElixirPdfUtilities.HtmlToPdf.{CssColor, RenderCache}

  alias NativeElixirPdfUtilities.Validators.{
    CssGradientValidator,
    CssLengthValidator,
    CssPositionValidator
  }

  # IEEE 754 binary64 machine epsilon absorbs roundoff when a repeated hard
  # stop is reached through division. This is a numeric invariant, not a budget.
  @epsilon 2.220446049250313e-16

  @doc "Renders an admitted gradient at its final image dimensions using the shared raster budgets."
  @spec rasterize(map(), number(), number(), map()) :: {:ok, map()} | {:error, term()}
  def rasterize(gradient, width, height, style) do
    with {:ok, plan} <- prepare(gradient, width, height, style) do
      RenderCache.fetch_stylesheet({__MODULE__, plan, width, height}, fn ->
        with {:ok, {w, h}} <-
               CssGradientValidator.raster_plan(width, height, length(gradient.stops)) do
          pixels =
            for row <- 0..(h - 1) do
              for column <- 0..(w - 1) do
                {r, g, b, a} = sample(plan, (column + 0.5) * width / w, (row + 0.5) * height / h)
                <<round(r * 255), round(g * 255), round(b * 255), round(a * 255)>>
              end
            end
            |> IO.iodata_to_binary()

          data = for <<r, g, b, _ <- pixels>>, into: <<>>, do: <<r, g, b>>
          alpha = for <<_, _, _, a <- pixels>>, into: <<>>, do: <<a>>

          image = %{
            format: :png,
            data: data,
            width_px: w,
            height_px: h,
            width: width,
            height: height,
            color_space: :device_rgb,
            bits_per_component: 8,
            decoded_stride: w * 4
          }

          image =
            if alpha == :binary.copy(<<255>>, w * h),
              do: image,
              else: Map.put(image, :alpha_data, alpha)

          {:ok, image}
        end
      end)
    end
  end

  @doc "Prepares source-defined gradient coordinates and interpolation segments independently of pixel sampling."
  @spec prepare(map(), number(), number(), map()) :: {:ok, map()} | {:error, term()}
  def prepare(gradient, width, height, style) do
    with {:ok, geometry, basis} <- geometry(gradient, width, height, style),
         {:ok, stops} <- CssGradientValidator.used_stops(gradient, basis, style) do
      first = hd(stops)
      last = List.last(stops)
      collapsed? = gradient.repeating? and first.position == last.position
      space = if collapsed?, do: "srgb", else: gradient.space

      {segments, _, _} =
        Enum.reduce(stops, {[], nil, nil}, fn stop, {segments, left, hint} ->
          case stop do
            %{hint: position} ->
              {segments, left, position}

            %{color: color, position: position} ->
              if left == nil do
                {segments, stop, nil}
              else
                segment = %{
                  start: left.position,
                  stop: position,
                  colors:
                    CssColor.interpolation_segment(
                      left.color,
                      color,
                      space,
                      gradient.hue
                    ),
                  hint: hint
                }

                {[segment | segments], stop, nil}
              end
          end
        end)

      {:ok,
       %{
         geometry: geometry,
         repeating?: gradient.repeating?,
         segments: Enum.reverse(segments),
         first: first.position,
         last: last.position,
         first_color:
           CssColor.interpolation_segment(first.color, first.color, gradient.space, gradient.hue),
         last_color:
           CssColor.interpolation_segment(last.color, last.color, gradient.space, gradient.hue),
         average: if(collapsed?, do: average_color(segments))
       }}
    end
  end

  @doc "Samples a prepared gradient at a point measured from its top-left corner."
  @spec sample(map(), number(), number()) :: {number(), number(), number(), number()}
  def sample(plan, x, y) do
    plan.average || sample_gradient(plan, x, y)
  end

  defp sample_gradient(plan, x, y) do
    value =
      case plan.geometry do
        {:linear, dx, dy, start, length} ->
          if length == 0, do: 1.0, else: (x * dx + y * dy - start) / length

        {:radial, cx, cy, rx, ry} ->
          if rx == 0 or ry == 0,
            do: 1.0e30,
            else: :math.sqrt(:math.pow((x - cx) / rx, 2) + :math.pow((y - cy) / ry, 2))

        {:conic, cx, cy, from} ->
          t = (:math.atan2(x - cx, cy - y) - from) / (2 * :math.pi())
          t - Float.floor(t)
      end

    value =
      if plan.repeating? and plan.last > plan.first do
        t = (value - plan.first) / (plan.last - plan.first)
        plan.first + (t - Float.floor(t)) * (plan.last - plan.first)
      else
        value
      end

    color =
      cond do
        value < plan.first ->
          CssColor.sample_interpolation(plan.first_color, 0.0)

        value >= plan.last ->
          CssColor.sample_interpolation(plan.last_color, 1.0)

        true ->
          segment =
            Enum.find(plan.segments, &(&1.stop > value + 4 * @epsilon * max(abs(value), 1.0))) ||
              List.last(plan.segments)

          t =
            if segment.stop == segment.start,
              do: 1.0,
              else: (value - segment.start) / (segment.stop - segment.start)

          t =
            if segment.hint == nil or segment.stop == segment.start do
              t
            else
              middle =
                min(
                  max((segment.hint - segment.start) / (segment.stop - segment.start), 0.0),
                  1.0
                )

              cond do
                middle == 0 -> 1.0
                middle == 1 -> 0.0
                true -> :math.pow(t, :math.log(0.5) / :math.log(middle))
              end
            end

          CssColor.sample_interpolation(segment.colors, t)
      end

    [r, g, b] = CssColor.to_srgb(color.space, color.channels)
    {r, g, b, min(max(color.alpha, 0.0), 1.0)}
  end

  # CSS Images 3 §3.3 gives each adjacent pair equal weight for a zero period.
  # The prepared sRGB midpoint already includes the pair's premultiplied alpha.
  defp average_color(segments) do
    {red, green, blue, total_alpha} =
      Enum.reduce(segments, {0.0, 0.0, 0.0, 0.0}, fn segment, {r, g, b, a} ->
        %{channels: [x, y, z], alpha: alpha} = CssColor.sample_interpolation(segment.colors, 0.5)
        {r + x * alpha, g + y * alpha, b + z * alpha, a + alpha}
      end)

    if total_alpha == 0 do
      {0.0, 0.0, 0.0, 0.0}
    else
      [r, g, b] =
        CssColor.to_srgb("srgb", [red / total_alpha, green / total_alpha, blue / total_alpha])

      {r, g, b, total_alpha / length(segments)}
    end
  end

  defp geometry(gradient, width, height, style) do
    case gradient.type do
      :linear ->
        {dx, dy} =
          case gradient.geometry.direction do
            {:angle, angle} ->
              {:math.sin(angle), -:math.cos(angle)}

            {:sides, sides} ->
              x =
                cond do
                  "left" in sides -> -1
                  "right" in sides -> 1
                  true -> 0
                end

              y =
                cond do
                  "top" in sides -> -1
                  "bottom" in sides -> 1
                  true -> 0
                end

              if x != 0 and y != 0 do
                diagonal = :math.sqrt(width * width + height * height)
                {x * height / diagonal, y * width / diagonal}
              else
                {x, y}
              end
          end

        line = abs(width * dx) + abs(height * dy)
        start = width / 2 * dx + height / 2 * dy - line / 2
        {:ok, {:linear, dx, dy, start, line}, line}

      type ->
        {horizontal, vertical} = gradient.geometry.position

        with {:ok, cx} <- CssPositionValidator.resolve(horizontal, width, style),
             {:ok, cy} <- CssPositionValidator.resolve(vertical, height, style) do
          if type == :conic do
            {:ok, {:conic, cx, cy, gradient.geometry.from}, 1.0}
          else
            case gradient.geometry.radius do
              [one] ->
                {:ok, {:radial, cx, cy, one, one}, one}

              [a, b] ->
                with {:ok, rx} <- CssLengthValidator.resolve(a, width, style),
                     {:ok, ry} <- CssLengthValidator.resolve(b, height, style),
                     do: {:ok, {:radial, cx, cy, rx, ry}, rx}

              keyword ->
                xs = [abs(cx), abs(width - cx)]
                ys = [abs(cy), abs(height - cy)]
                closest? = keyword in [:closest_side, :closest_corner]
                choose = if closest?, do: &Enum.min/1, else: &Enum.max/1

                {rx, ry} =
                  if gradient.geometry.shape == :circle do
                    radius =
                      if keyword in [:closest_side, :farthest_side],
                        do: choose.(xs ++ ys),
                        else: choose.(for x <- xs, y <- ys, do: :math.sqrt(x * x + y * y))

                    {radius, radius}
                  else
                    rx = choose.(xs)
                    ry = choose.(ys)

                    if keyword in [:closest_corner, :farthest_corner] and rx > 0 and ry > 0 do
                      factor =
                        choose.(
                          for x <- xs,
                              y <- ys,
                              do: :math.sqrt(:math.pow(x / rx, 2) + :math.pow(y / ry, 2))
                        )

                      {rx * factor, ry * factor}
                    else
                      {rx, ry}
                    end
                  end

                {:ok, {:radial, cx, cy, rx, ry}, rx}
            end
          end
        end
    end
  end
end
