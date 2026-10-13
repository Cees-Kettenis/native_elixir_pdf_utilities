defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgClipGeometry do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Builds a closed nonzero contour from validated projected polygon corners."
  @spec polygon([{number(), number()}]) :: tuple()
  def polygon(points), do: {:path, 0, Enum.zip(points, tl(points) ++ [hd(points)])}

  @doc "Flattens actual native Bezier contours to the configured point precision."
  @spec flatten([{integer(), [number()]}], integer(), number()) :: tuple()
  def flatten(commands, rule, precision) do
    {edges, current, start} =
      Enum.reduce(commands, {[], nil, nil}, fn {operation, coordinates},
                                               {edges, current, start} ->
        case {operation, coordinates} do
          {0, [x, y]} ->
            edges = if current && current != start, do: [{current, start} | edges], else: edges
            {edges, {x, y}, {x, y}}

          {1, [x, y]} ->
            {[{current, {x, y}} | edges], {x, y}, start}

          {2, [ax, ay, x, y]} ->
            points = curve([current, {ax, ay}, {x, y}], precision)
            {Enum.reverse(Enum.zip([current | points], points), edges), {x, y}, start}

          {3, [ax, ay, bx, by, x, y]} ->
            points = curve([current, {ax, ay}, {bx, by}, {x, y}], precision)
            {Enum.reverse(Enum.zip([current | points], points), edges), {x, y}, start}

          {4, []} ->
            {[{current, start} | edges], start, start}
        end
      end)

    edges = if current && current != start, do: [{current, start} | edges], else: edges
    {:path, rule, Enum.reject(edges, fn {a, b} -> a == b end)}
  end

  defp curve(points, precision) do
    HtmlValidator.reserve_render_resource(:max_svg_clip_projection_work, 1, :svg)
    {ax, ay} = hd(points)
    {bx, by} = List.last(points)
    squared = (bx - ax) * (bx - ax) + (by - ay) * (by - ay)

    flat? =
      Enum.all?(points, fn {x, y} ->
        projection =
          if squared == 0,
            do: 0,
            else: min(1, max(0, ((x - ax) * (bx - ax) + (y - ay) * (by - ay)) / squared))

        dx = x - ax - projection * (bx - ax)
        dy = y - ay - projection * (by - ay)
        dx * dx + dy * dy <= precision * precision
      end)

    if flat? do
      [List.last(points)]
    else
      levels =
        Stream.iterate(points, fn points ->
          Enum.zip_with(points, tl(points), fn {x, y}, {a, b} -> {(x + a) / 2, (y + b) / 2} end)
        end)
        |> Enum.take(length(points))

      left = Enum.map(levels, &hd/1)
      right = levels |> Enum.map(&List.last/1) |> Enum.reverse()
      curve(left, precision) ++ curve(right, precision)
    end
  end

  @doc "Returns resolved contour edges for a validated union or intersection."
  @spec edges(tuple()) :: [tuple()]
  def edges(expression) do
    case expression do
      {:path, _, edges} -> edges
      {_, children} -> Enum.flat_map(children, &edges/1)
    end
  end

  @doc "Restricts arrangement edges to the actual link bounds without changing winding contours."
  @spec clip_edges([tuple()], tuple()) :: [tuple()]
  def clip_edges(edges, {left, bottom, right, top}) do
    Enum.flat_map(edges, fn {{x, y}, {a, b}} ->
      interval =
        Enum.reduce([{x, a - x, left, right}, {y, b - y, bottom, top}], {0, 1}, fn {origin, delta,
                                                                                    low, high},
                                                                                   interval ->
          case interval do
            nil ->
              nil

            {start, finish} ->
              if delta == 0 do
                if origin >= low and origin <= high, do: interval, else: nil
              else
                first = (low - origin) / delta
                last = (high - origin) / delta
                start = max(start, min(first, last))
                finish = min(finish, max(first, last))
                if finish > start, do: {start, finish}, else: nil
              end
          end
        end)

      case interval do
        nil ->
          []

        {start, finish} ->
          [
            {{x + start * (a - x), y + start * (b - y)},
             {x + finish * (a - x), y + finish * (b - y)}}
          ]
      end
    end)
  end

  @doc "Decomposes the exact line arrangement into convex PDF link quadrilaterals."
  @spec project(tuple(), [tuple()]) :: [[number()]]
  def project(expression, edges) do
    predicate_work = length(edges(expression))
    ys = Enum.flat_map(edges, fn {{_, y}, {_, b}} -> [y, b] end)

    intersections =
      for {first, index} <- Enum.with_index(edges),
          second <- Enum.drop(edges, index + 1),
          y = crossing(first, second),
          y != nil,
          do: y

    ys = Enum.sort(Enum.uniq(ys ++ intersections))

    ys
    |> Enum.zip(tl(ys))
    |> Enum.filter(fn {bottom, top} -> top > bottom end)
    |> Enum.flat_map(fn {bottom, top} ->
      HtmlValidator.reserve_render_resource(
        :max_svg_clip_projection_work,
        predicate_work + length(edges),
        :svg
      )

      middle = (bottom + top) / 2
      {scan, scan_work} = scan_expression(expression, middle)

      boundaries =
        edges
        |> Enum.filter(fn {{_, y}, {_, b}} -> middle > min(y, b) and middle < max(y, b) end)
        |> Enum.map(fn edge -> {x_at(edge, middle), edge} end)
        |> Enum.sort_by(&elem(&1, 0))

      boundaries
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.flat_map(fn [{left, a}, {right, b}] ->
        HtmlValidator.reserve_render_resource(:max_svg_clip_projection_work, scan_work, :svg)

        if right > left and contains?(scan, (left + right) / 2) do
          [al, ar, bl, br] = [x_at(a, top), x_at(b, top), x_at(a, bottom), x_at(b, bottom)]
          [[al, top, ar, top, bl, bottom, br, bottom]]
        else
          []
        end
      end)
    end)
  end

  # Slabs split at every edge crossing, so their left/right boundaries cannot
  # swap within a returned quadrilateral. Holes remain excluded by winding.
  defp x_at({{x, y}, {a, b}}, height), do: x + (height - y) * (a - x) / (b - y)

  defp crossing({{x, y}, {a, b}}, {{u, v}, {c, d}}) do
    denominator = (a - x) * (d - v) - (b - y) * (c - u)

    if denominator == 0 do
      nil
    else
      t = ((u - x) * (d - v) - (v - y) * (c - u)) / denominator
      s = ((u - x) * (b - y) - (v - y) * (a - x)) / denominator
      if t > 0 and t < 1 and s > 0 and s < 1, do: y + t * (b - y), else: nil
    end
  end

  # At one slab height, winding depends only on crossing X positions. Prepare
  # those once for all cells instead of revisiting every curve chord per cell.
  defp scan_expression(expression, y) do
    case expression do
      {:path, rule, edges} ->
        crossings =
          for {{_, a}, {_, b}} = edge <- edges,
              y >= min(a, b) and y < max(a, b),
              do: {x_at(edge, y), if(b > a, do: 1, else: -1)}

        {{:scan, rule, crossings}, length(crossings) + 1}

      {kind, children} ->
        {children, work} =
          Enum.map_reduce(children, 1, fn child, work ->
            {scan, cost} = scan_expression(child, y)
            {scan, work + cost}
          end)

        {{kind, children}, work}
    end
  end

  defp contains?(expression, x) do
    case expression do
      {:union, children} ->
        Enum.any?(children, &contains?(&1, x))

      {:intersection, children} ->
        Enum.all?(children, &contains?(&1, x))

      {:scan, rule, crossings} ->
        winding =
          Enum.reduce(crossings, 0, fn {position, direction}, total ->
            if position > x, do: total + direction, else: total
          end)

        if rule == 1, do: rem(abs(winding), 2) == 1, else: winding != 0
    end
  end

  @doc "Checks that a validated quadrilateral has a nonzero convex perimeter."
  @spec convex?([{number(), number()}]) :: boolean()
  def convex?(points) do
    cross =
      (points ++ Enum.take(points, 2))
      |> Enum.chunk_every(3, 1, :discard)
      |> Enum.map(fn [{x, y}, {a, b}, {c, d}] -> (a - x) * (d - b) - (b - y) * (c - a) end)

    Enum.any?(cross, &(&1 != 0)) and
      (Enum.all?(cross, &(&1 >= 0)) or Enum.all?(cross, &(&1 <= 0)))
  end
end
