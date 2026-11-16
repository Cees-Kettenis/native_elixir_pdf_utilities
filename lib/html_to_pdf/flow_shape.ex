defmodule NativeElixirPdfUtilities.HtmlToPdf.FlowShape do
  @moduledoc "Used CSS shape geometry for line-band exclusions."
  alias NativeElixirPdfUtilities.Validators.{
    CssLengthValidator,
    CssPositionValidator,
    HtmlValidator
  }

  # CSS layout coordinates use 1/64 CSS-pixel precision. This is a geometric
  # approximation invariant, not a configurable resource ceiling.
  @precision 0.75 / 64

  @doc "Prepares a float's authored shape in its actual reference and margin boxes."
  @spec prepare_float(map(), number()) :: map()
  def prepare_float(float, containing_width) do
    style = float.style

    clip = %{
      x: float.left,
      top: float.top,
      width: max(float.right - float.left, 0.0),
      height: max(float.top - float.bottom, 0.0)
    }

    shape = Map.get(style, :shape_outside, :none)

    margin =
      if shape == :none,
        do: 0.0,
        else: length_value(Map.get(style, :shape_margin, 0.0), containing_width, style)

    geometry =
      case shape do
        :none ->
          {:rect, clip}

        :image ->
          image_geometry(
            Map.fetch!(style, :shape_image),
            clip,
            Map.get(style, :shape_image_threshold, 0.0)
          )

        shape ->
          reference = reference_box(float, shape.reference)
          geometry(shape, reference, style)
      end

    %{geometry: geometry, margin: margin, clip: clip}
  end

  @doc "Prepares a selected inside shape against a definite content rectangle."
  @spec prepare_inside(map(), number(), number(), number(), number() | nil) :: map() | nil
  def prepare_inside(style, x, top, width, height) do
    case {Map.get(style, :shape_inside, :auto), height} do
      {%{reference: _} = shape, height} when is_number(height) ->
        zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
        border = Map.get(style, :border_widths, zero)
        padding = Map.get(style, :padding, zero)
        margin = Map.get(style, :margin, zero)

        box = %{
          x: x - border.left - padding.left,
          top: top + border.top + padding.top,
          width: width + border.left + border.right + padding.left + padding.right,
          height: height + border.top + border.bottom + padding.top + padding.bottom
        }

        float = %{
          border: box,
          style: style,
          left: box.x - margin.left,
          right: box.x + box.width + margin.right,
          top: box.top + margin.top,
          bottom: box.top - box.height - margin.bottom
        }

        %{
          geometry: geometry(shape, reference_box(float, shape.reference), style),
          clip: %{x: x, top: top, width: width, height: height}
        }

      _ ->
        nil
    end
  end

  defp reference_box(float, reference) do
    style = float.style
    border = Map.get(style, :border_widths, %{top: 0, right: 0, bottom: 0, left: 0})
    padding = Map.get(style, :padding, %{top: 0, right: 0, bottom: 0, left: 0})
    box = float.border

    case reference do
      :margin_box ->
        %{
          x: float.left,
          top: float.top,
          width: max(float.right - float.left, 0.0),
          height: max(float.top - float.bottom, 0.0)
        }

      :border_box ->
        box

      _ ->
        edge =
          if reference == :content_box,
            do:
              Map.new(border, fn {side, value} -> {side, value + Map.fetch!(padding, side)} end),
            else: border

        %{
          x: box.x + edge.left,
          top: box.top - edge.top,
          width: max(box.width - edge.left - edge.right, 0.0),
          height: max(box.height - edge.top - edge.bottom, 0.0)
        }
    end
  end

  @doc "Resolves validated basic shapes in a used reference rectangle."
  @spec geometry(map(), map(), map()) :: tuple()
  def geometry(shape, box, style) do
    case shape do
      %{type: :box, reference: reference} ->
        zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
        border = Map.get(style, :border_widths, zero)

        insets =
          case reference do
            :margin_box ->
              Map.new(Map.get(style, :margin, zero), fn {edge, value} -> {edge, -value} end)

            :padding_box ->
              border

            :content_box ->
              Map.new(border, fn {edge, value} ->
                {edge, value + Map.get(Map.get(style, :padding, zero), edge, 0.0)}
              end)

            :border_box ->
              zero
          end

        host = %{
          x: box.x - insets.left,
          y: box.top - box.height - insets.bottom,
          width: box.width + insets.left + insets.right,
          height: box.height + insets.top + insets.bottom,
          border_radius: Map.get(style, :border_radius, 0.0)
        }

        clip = NativeElixirPdfUtilities.HtmlToPdf.PageGeometry.inset_rounded_clip(host, insets)

        radii =
          case Map.get(clip, :border_radii) do
            nil ->
              List.duplicate({clip.border_radius, clip.border_radius}, 4)

            corners ->
              Enum.map(
                [:top_left, :top_right, :bottom_right, :bottom_left],
                &Map.fetch!(corners, &1)
              )
          end

        {:polygon, rounded_points(box, radii), :nonzero}

      %{type: type, radii: radii, position: {px, py}} when type in [:circle, :ellipse] ->
        cx = position_value(px, box.width, style)
        cy = position_value(py, box.height, style)

        {rx, ry} =
          case {type, radii} do
            {:circle, [radius]} ->
              radius =
                case radius do
                  :closest_side ->
                    Enum.min([abs(cx), abs(box.width - cx), abs(cy), abs(box.height - cy)])

                  :farthest_side ->
                    Enum.max([abs(cx), abs(box.width - cx), abs(cy), abs(box.height - cy)])

                  radius ->
                    length_value(
                      radius,
                      :math.sqrt((box.width * box.width + box.height * box.height) / 2),
                      style
                    )
                end

              {radius, radius}

            {:ellipse, [rx, ry]} ->
              resolve_radius = fn radius, position, size ->
                case radius do
                  :closest_side -> min(abs(position), abs(size - position))
                  :farthest_side -> max(abs(position), abs(size - position))
                  radius -> length_value(radius, size, style)
                end
              end

              {resolve_radius.(rx, cx, box.width), resolve_radius.(ry, cy, box.height)}
          end

        {:ellipse, box.x + cx, box.top - cy, rx, ry}

      %{type: :inset, offsets: offsets, rx: rx, ry: ry} ->
        left = length_value(offsets.left, box.width, style)
        right = length_value(offsets.right, box.width, style)
        top = length_value(offsets.top, box.height, style)
        bottom = length_value(offsets.bottom, box.height, style)

        sx =
          if left + right > box.width and left + right > 0,
            do: box.width / (left + right),
            else: 1.0

        sy =
          if top + bottom > box.height and top + bottom > 0,
            do: box.height / (top + bottom),
            else: 1.0

        inset = %{
          x: box.x + left * sx,
          top: box.top - top * sy,
          width: max(box.width - (left + right) * sx, 0.0),
          height: max(box.height - (top + bottom) * sy, 0.0)
        }

        corners =
          Enum.map([:top, :right, :bottom, :left], fn corner ->
            {length_value(Map.fetch!(rx, corner), inset.width, style),
             length_value(Map.fetch!(ry, corner), inset.height, style)}
          end)

        {:polygon, rounded_points(inset, corners), :nonzero}

      %{type: :polygon, points: points, rule: rule} ->
        {:polygon,
         Enum.map(points, fn {x, y} ->
           {box.x + length_value(x, box.width, style),
            box.top - length_value(y, box.height, style)}
         end), rule}
    end
  end

  defp length_value(value, basis, style) do
    case CssLengthValidator.resolve(value, basis, style) do
      {:ok, value} -> value
      error -> throw({:render_resource_limit, error})
    end
  end

  defp position_value(value, basis, style) do
    case CssPositionValidator.resolve(value, basis, style) do
      {:ok, value} -> value
      error -> throw({:render_resource_limit, error})
    end
  end

  defp rounded_points(box, radii) do
    [{tlx, tly}, {trx, try_}, {brx, bry}, {blx, bly}] = radii

    scale =
      Enum.min([
        1.0,
        if(tlx + trx > 0, do: box.width / (tlx + trx), else: 1.0),
        if(blx + brx > 0, do: box.width / (blx + brx), else: 1.0),
        if(tly + bly > 0, do: box.height / (tly + bly), else: 1.0),
        if(try_ + bry > 0, do: box.height / (try_ + bry), else: 1.0)
      ])

    radii = Enum.map(radii, fn {x, y} -> {x * scale, y * scale} end)

    centers = [
      {box.x + tlx * scale, box.top - tly * scale, :math.pi() / 2},
      {box.x + box.width - trx * scale, box.top - try_ * scale, 0.0},
      {box.x + box.width - brx * scale, box.top - box.height + bry * scale, -:math.pi() / 2},
      {box.x + blx * scale, box.top - box.height + bly * scale, -:math.pi()}
    ]

    Enum.zip(radii, centers)
    |> Enum.flat_map(fn {{rx, ry}, {cx, cy, start}} ->
      count = arc_count(max(rx, ry), :math.pi() / 2)

      Enum.map(0..count, fn index ->
        angle = start + :math.pi() / 2 * (1 - index / count)
        {cx + rx * :math.cos(angle), cy + ry * :math.sin(angle)}
      end)
    end)
  end

  defp arc_count(radius, angle) do
    count =
      if radius <= @precision,
        do: 1,
        else: max(ceil(angle / (2 * :math.acos(max(1 - @precision / radius, -1.0)))), 1)

    HtmlValidator.reserve_render_resource(:max_work, count, :layout)
    HtmlValidator.reserve_render_resource(:max_entries, count, :layout)
    count
  end

  defp image_geometry(image, box, threshold) do
    width = image.width_px
    height = image.height_px
    HtmlValidator.reserve_render_resource(:max_work, width * height, :layout)

    rows =
      case Map.get(image, :alpha_data) do
        nil ->
          if threshold < 1, do: Enum.map(0..(height - 1), &{&1, 0, width}), else: []

        alpha ->
          bits = image.bits_per_component
          maximum = Bitwise.bsl(1, bits) - 1
          bytes = div(width * bits, 8)
          samples = for <<row::binary-size(^bytes) <- alpha>>, do: row

          for {row, y} <- Enum.with_index(samples), reduce: [] do
            rows ->
              visible =
                for {value, x} <-
                      for(<<sample::size(^bits) <- row>>, do: sample) |> Enum.with_index(),
                    value / maximum > threshold,
                    do: x

              if visible == [], do: rows, else: [{y, hd(visible), List.last(visible) + 1} | rows]
          end
      end

    {:image, box, width, height, rows}
  end

  @doc "Returns the horizontal shape extent intersecting a line band, clipped to the float margin box."
  @spec interval(map(), number(), number()) :: {number(), number()} | nil
  def interval(shape, top, bottom) do
    clip = shape.clip
    top = min(top, clip.top)
    bottom = max(bottom, clip.top - clip.height)

    if top <= bottom do
      nil
    else
      case geometry_interval(shape.geometry, shape.margin, top, bottom) do
        nil ->
          nil

        {left, right} ->
          left = max(left, clip.x)
          right = min(right, clip.x + clip.width)
          if right >= left, do: {left, right}
      end
    end
  end

  defp geometry_interval(geometry, margin, top, bottom) do
    case geometry do
      {:rect, box} ->
        rectangle_interval(box, margin, top, bottom)

      {:ellipse, cx, cy, radius, radius} ->
        ellipse_interval(cx, cy, radius + margin, radius + margin, top, bottom)

      {:ellipse, cx, cy, rx, ry} when margin == 0 ->
        ellipse_interval(cx, cy, rx, ry, top, bottom)

      {:ellipse, cx, cy, rx, ry} ->
        count = arc_count(max(rx, ry), 2 * :math.pi())

        points =
          Enum.map(0..(count - 1), fn i ->
            angle = 2 * :math.pi() * i / count
            {cx + rx * :math.cos(angle), cy + ry * :math.sin(angle)}
          end)

        polygon_interval(points, margin, top, bottom)

      {:polygon, points, _rule} ->
        polygon_interval(points, margin, top, bottom)

      {:image, box, width, height, rows} ->
        Enum.reduce(rows, nil, fn {y, left, right}, result ->
          row = %{
            x: box.x + box.width * left / width,
            top: box.top - box.height * y / height,
            width: box.width * (right - left) / width,
            height: box.height / height
          }

          merge_interval(result, rectangle_interval(row, margin, top, bottom))
        end)
    end
  end

  @doc "Finds a complete line interval inside the selected shape and its content box."
  @spec inside_interval(map(), number(), number()) :: {number(), number()} | nil
  def inside_interval(shape, top, bottom) do
    clip = shape.clip

    if bottom >= clip.top or top <= clip.top - clip.height do
      {clip.x, clip.x + clip.width}
    else
      top = min(top, clip.top)
      bottom = max(bottom, clip.top - clip.height)

      interval =
        case shape.geometry do
          {:ellipse, cx, cy, rx, ry} ->
            y = if abs(top - cy) > abs(bottom - cy), do: top, else: bottom
            ellipse_interval(cx, cy, rx, ry, y, y)

          {:polygon, points, rule} ->
            ys =
              [top - 1.0e-7, bottom + 1.0e-7] ++
                Enum.flat_map(points, fn {_, y} ->
                  if y > bottom and y < top, do: [y - 1.0e-7, y + 1.0e-7], else: []
                end)

            ys
            |> Enum.reduce([{clip.x, clip.x + clip.width}], fn y, intervals ->
              next = scanline_intervals(points, rule, y)

              HtmlValidator.reserve_render_resource(
                :max_work,
                length(next) * length(intervals),
                :layout
              )

              for {a, b} <- intervals,
                  {c, d} <- next,
                  min(b, d) > max(a, c),
                  do: {max(a, c), min(b, d)}
            end)
            |> Enum.max_by(fn {left, right} -> right - left end, fn -> nil end)
        end

      case interval do
        nil ->
          nil

        {left, right} ->
          left = max(left, clip.x)
          right = min(right, clip.x + clip.width)
          if right > left, do: {left, right}
      end
    end
  end

  defp scanline_intervals(points, rule, y) do
    intersections =
      points
      |> Enum.zip(tl(points) ++ [hd(points)])
      |> Enum.flat_map(fn {{ax, ay}, {bx, by}} ->
        if ay != by and y >= min(ay, by) and y < max(ay, by),
          do: [{ax + (bx - ax) * (y - ay) / (by - ay), if(by > ay, do: 1, else: -1)}],
          else: []
      end)
      |> Enum.sort()

    {intervals, _, _} =
      Enum.reduce(intersections, {[], nil, 0}, fn {x, winding}, {intervals, previous, state} ->
        filled? = if rule == :evenodd, do: rem(abs(state), 2) == 1, else: state != 0

        intervals =
          if filled? and previous != nil and x > previous,
            do: [{previous, x} | intervals],
            else: intervals

        {intervals, x, state + winding}
      end)

    Enum.reverse(intervals)
  end

  @doc "Returns line-band transition heights for an already prepared exclusion shape."
  @spec transition_heights(map(), number()) :: [number()]
  def transition_heights(shape, height) do
    margin = shape.margin

    points =
      case shape.geometry do
        {:rect, box} ->
          [box.top, box.top - box.height]

        {:ellipse, _, cy, _, ry} ->
          [cy + ry + margin, cy, cy - ry - margin]

        {:polygon, points, _} ->
          Enum.flat_map(points, fn {_, y} -> [y + margin, y - margin] end)

        {:image, box, _pixel_width, pixel_height, rows} ->
          Enum.flat_map(rows, fn {row, _, _} ->
            [
              box.top - row * box.height / pixel_height + margin,
              box.top - (row + 1) * box.height / pixel_height - margin
            ]
          end)
      end

    HtmlValidator.reserve_render_resource(:max_work, length(points), :layout)
    Enum.flat_map(points, &[&1, &1 + height])
  end

  @doc "Finds the earliest fitting band between geometry transitions at CSS coordinate precision."
  @spec first_fitting_height(number(), [number()], function(), number()) :: number()
  def first_fitting_height(top, knots, fits?, fallback) do
    Enum.reduce_while(knots, top, fn candidate, previous ->
      HtmlValidator.reserve_render_resource(:max_work, 1, :layout)

      if fits?.(candidate),
        do: {:halt, {:fit, bisect_inside(previous, candidate, fits?)}},
        else: {:cont, candidate}
    end)
    |> case do
      {:fit, y} -> y
      _ -> fallback
    end
  end

  @doc "Finds the first fitting inside band, with overflow continuing below the content rectangle."
  @spec fit_inside(map(), number(), number(), number()) :: number()
  def fit_inside(shape, top, height, minimum) do
    bottom = shape.clip.top - shape.clip.height

    fits? = fn y ->
      HtmlValidator.reserve_render_resource(:max_work, 1, :layout)

      case inside_interval(shape, y, y - height) do
        {left, right} -> right - left + @precision >= minimum
        nil -> false
      end
    end

    if top <= bottom or fits?.(top) do
      top
    else
      knots =
        case shape.geometry do
          {:ellipse, _, cy, _, ry} -> [cy + ry, cy + height / 2, cy - ry + height]
          {:polygon, points, _} -> Enum.flat_map(points, fn {_, y} -> [y, y + height] end)
        end

      knots =
        [bottom | knots]
        |> Enum.filter(&(&1 < top and &1 >= bottom))
        |> Enum.uniq()
        |> Enum.sort(:desc)

      first_fitting_height(top, knots, fits?, bottom)
    end
  end

  defp bisect_inside(high, low, fits?) do
    if high - low <= @precision do
      low
    else
      middle = (high + low) / 2

      if fits?.(middle),
        do: bisect_inside(high, middle, fits?),
        else: bisect_inside(middle, low, fits?)
    end
  end

  defp rectangle_interval(box, margin, top, bottom) do
    dy = max(max(box.top - box.height - top, bottom - box.top), 0.0)

    if dy <= margin and (margin > 0 or (box.top > bottom and box.top - box.height < top)) do
      dx = :math.sqrt(max(margin * margin - dy * dy, 0.0))
      {box.x - dx, box.x + box.width + dx}
    end
  end

  defp ellipse_interval(cx, cy, rx, ry, top, bottom) do
    dy = abs(cy - max(bottom, min(cy, top)))

    cond do
      ry == 0 ->
        if dy == 0, do: {cx - rx, cx + rx}

      dy <= ry ->
        dx = rx * :math.sqrt(max(1 - dy * dy / (ry * ry), 0.0))
        {cx - dx, cx + dx}

      true ->
        nil
    end
  end

  defp polygon_interval(points, margin, top, bottom) do
    edges = Enum.zip(points, tl(points) ++ [hd(points)])
    HtmlValidator.reserve_render_resource(:max_work, length(edges), :layout)
    base = edge_interval(edges, top, bottom)

    if margin == 0 do
      base
    else
      Enum.reduce(edges, base, fn {{ax, ay} = a, {bx, by} = b}, interval ->
        size = :math.sqrt((bx - ax) * (bx - ax) + (by - ay) * (by - ay))

        interval =
          interval
          |> merge_interval(ellipse_interval(ax, ay, margin, margin, top, bottom))
          |> merge_interval(ellipse_interval(bx, by, margin, margin, top, bottom))

        if size == 0 do
          interval
        else
          nx = -(by - ay) / size * margin
          ny = (bx - ax) / size * margin

          corners = [
            {elem(a, 0) + nx, elem(a, 1) + ny},
            {elem(b, 0) + nx, elem(b, 1) + ny},
            {elem(b, 0) - nx, elem(b, 1) - ny},
            {elem(a, 0) - nx, elem(a, 1) - ny}
          ]

          merge_interval(
            interval,
            edge_interval(Enum.zip(corners, tl(corners) ++ [hd(corners)]), top, bottom)
          )
        end
      end)
    end
  end

  defp edge_interval(edges, top, bottom) do
    xs =
      Enum.flat_map(edges, fn {{ax, ay}, {bx, by}} ->
        endpoints = if ay >= bottom and ay <= top, do: [ax], else: []
        endpoints = if by >= bottom and by <= top, do: [bx | endpoints], else: endpoints

        if ay == by do
          endpoints
        else
          intersections =
            for y <- [top, bottom],
                y >= min(ay, by) and y <= max(ay, by),
                do: ax + (bx - ax) * (y - ay) / (by - ay)

          endpoints ++ intersections
        end
      end)

    if xs != [], do: {Enum.min(xs), Enum.max(xs)}
  end

  defp merge_interval(first, second) do
    case {first, second} do
      {nil, second} -> second
      {first, nil} -> first
      {{a, b}, {c, d}} -> {min(a, c), max(b, d)}
    end
  end
end
