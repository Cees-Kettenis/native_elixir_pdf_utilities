defmodule NativeElixirPdfUtilities.HtmlToPdf.ClipGeometry do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Builds a closed nonzero contour from validated projected polygon corners."
  @spec polygon([{number(), number()}]) :: tuple()
  def polygon(points), do: {:contour, Enum.zip(points, tl(points) ++ [hd(points)])}

  @doc "Subdivides canonical HTML box cubics to the configured PDF point precision."
  @spec flatten_box([{integer(), [number()]}], number()) :: tuple()
  def flatten_box(commands, precision) do
    {edges, current, start} =
      Enum.reduce(commands, {[], nil, nil}, fn {operation, coordinates},
                                               {edges, current, start} ->
        case {operation, coordinates} do
          {0, [x, y]} ->
            edges = if current && current != start, do: [{current, start} | edges], else: edges
            {edges, {x, y}, {x, y}}

          {1, [x, y]} ->
            {[{current, {x, y}} | edges], {x, y}, start}

          {3, [ax, ay, bx, by, x, y]} ->
            points = curve([current, {ax, ay}, {bx, by}, {x, y}], precision)
            {Enum.reverse(Enum.zip([current | points], points), edges), {x, y}, start}

          {4, []} ->
            {[{current, start} | edges], start, start}
        end
      end)

    edges = if current && current != start, do: [{current, start} | edges], else: edges
    {:contour, Enum.reject(edges, fn {a, b} -> a == b end)}
  end

  defp curve(points, precision) do
    HtmlValidator.reserve_render_resource(:max_clip_projection_work, 1, :layout)
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

  @doc "Returns resolved contour edges for a validated box-clip intersection."
  @spec edges(tuple()) :: [tuple()]
  def edges(expression) do
    case expression do
      {:contour, edges} -> edges
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
    ys = Enum.flat_map(edges, fn {{_, y}, {_, b}} -> [y, b] end)

    HtmlValidator.reserve_render_resource(
      :max_clip_projection_work,
      length(edges) * ceil(:math.log2(max(length(edges), 1))),
      :layout
    )

    # Keep only edges with overlapping vertical ranges. Original source indexes
    # retain the arithmetic order of each crossing, including exact reversals.
    {_, intersections} =
      edges
      |> Enum.with_index()
      |> Enum.sort_by(fn {{{_, y}, {_, b}}, _} -> min(y, b) end)
      |> Enum.reduce({[], []}, fn {{{_, y}, {_, b}} = edge, index}, {active, ys} ->
        HtmlValidator.reserve_render_resource(
          :max_clip_projection_work,
          length(active) + 1,
          :layout
        )

        low = min(y, b)
        active = Enum.filter(active, fn {{{_, a}, {_, b}}, _} -> max(a, b) >= low end)
        HtmlValidator.reserve_render_resource(:max_clip_projection_work, length(active), :layout)

        ys =
          Enum.reduce(active, ys, fn {other, previous}, ys ->
            y = if previous < index, do: crossing(other, edge), else: crossing(edge, other)
            if y == nil, do: ys, else: [y | ys]
          end)

        {[{edge, index} | active], ys}
      end)

    ys = ys ++ intersections

    HtmlValidator.reserve_render_resource(
      :max_clip_projection_work,
      length(ys) * (1 + ceil(:math.log2(max(length(ys), 1)))),
      :layout
    )

    ys = Enum.sort(Enum.uniq(ys))

    {indexed, _count, contours} = scan_paths(expression, 0)

    HtmlValidator.reserve_render_resource(
      :max_clip_projection_work,
      (length(contours) + length(edges)) *
        (1 + ceil(:math.log2(max(length(contours) + length(edges), 1)))),
      :layout
    )

    contours =
      contours
      |> Enum.with_index()
      |> Enum.map(fn {entry, index} -> {index, entry} end)
      |> Enum.sort_by(fn {_, {_, {{_, y}, {_, b}}}} -> min(y, b) end)

    boundaries =
      edges
      |> Enum.with_index()
      |> Enum.map(fn {edge, index} -> {index, {nil, edge}} end)
      |> Enum.sort_by(fn {_, {_, {{_, y}, {_, b}}}} -> min(y, b) end)

    {_, _, quads} =
      ys
      |> Enum.zip(tl(ys))
      |> Enum.filter(fn {bottom, top} -> top > bottom end)
      |> Enum.reduce({{contours, %{}}, {boundaries, %{}}, []}, fn {bottom, top},
                                                                  {contours, boundaries, quads} ->
        middle = (bottom + top) / 2
        contours = active_edges(contours, middle)
        boundaries = active_edges(boundaries, middle)
        {_, active_contours} = contours
        {_, active_boundaries} = boundaries

        HtmlValidator.reserve_render_resource(
          :max_clip_projection_work,
          map_size(active_contours) + map_size(active_boundaries),
          :layout
        )

        crossings =
          Enum.reduce(active_contours, %{}, fn {_, {path, {{_, y}, {_, b}} = edge}}, crossings ->
            crossing = {x_at(edge, middle), if(b > y, do: 1, else: -1)}
            Map.update(crossings, path, [crossing], &[crossing | &1])
          end)

        {scan, scan_work} = scan_expression(indexed, crossings)

        HtmlValidator.reserve_render_resource(
          :max_clip_projection_work,
          map_size(active_boundaries) * ceil(:math.log2(max(map_size(active_boundaries), 1))),
          :layout
        )

        cells =
          active_boundaries
          |> Enum.map(fn {_, {_, edge}} -> {x_at(edge, middle), edge} end)
          |> Enum.sort_by(&elem(&1, 0))
          |> Enum.chunk_every(2, 1, :discard)
          |> Enum.flat_map(fn [{left, a}, {right, b}] ->
            HtmlValidator.reserve_render_resource(:max_clip_projection_work, scan_work, :layout)

            if right > left and contains?(scan, (left + right) / 2) do
              [al, ar, bl, br] = [x_at(a, top), x_at(b, top), x_at(a, bottom), x_at(b, bottom)]
              [[al, top, ar, top, bl, bottom, br, bottom]]
            else
              []
            end
          end)

        {contours, boundaries, Enum.reverse(cells, quads)}
      end)

    Enum.reverse(quads)
  end

  # Index HTML box contours once so each slab can keep their winding while
  # visiting only the edges that cross its height, including contours outside
  # the link rectangle that still determine its real winding.
  defp scan_paths(expression, index) do
    case expression do
      {:contour, edges} ->
        {{:scan_path, index}, index + 1, Enum.map(edges, &{index, &1})}

      {kind, children} ->
        {children, next} =
          Enum.map_reduce(children, index, fn child, index ->
            {child, next, edges} = scan_paths(child, index)
            {{child, edges}, next}
          end)

        {{kind, Enum.map(children, &elem(&1, 0))}, next, Enum.flat_map(children, &elem(&1, 1))}
    end
  end

  defp active_edges({pending, active}, y) do
    {new, pending} =
      Enum.split_while(pending, fn {_, {_, {{_, a}, {_, b}}}} -> min(a, b) <= y end)

    HtmlValidator.reserve_render_resource(
      :max_clip_projection_work,
      2 * (map_size(active) + length(new)) + 1,
      :layout
    )

    active =
      Enum.reduce(new, active, fn {index, entry}, active -> Map.put(active, index, entry) end)

    active = Map.reject(active, fn {_, {_, {{_, a}, {_, b}}}} -> max(a, b) <= y end)
    {pending, active}
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
  defp scan_expression(expression, crossings) do
    case expression do
      {:scan_path, index} ->
        points = Map.get(crossings, index, [])
        {{:scan, points}, length(points) + 1}

      {kind, children} ->
        {children, work} =
          Enum.map_reduce(children, 1, fn child, work ->
            {scan, cost} = scan_expression(child, crossings)
            {scan, work + cost}
          end)

        {{kind, children}, work}
    end
  end

  defp contains?(expression, x) do
    case expression do
      {:intersection, children} ->
        Enum.all?(children, &contains?(&1, x))

      {:scan, crossings} ->
        winding =
          Enum.reduce(crossings, 0, fn {position, direction}, total ->
            if position > x, do: total + direction, else: total
          end)

        winding != 0
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

  @doc "Tests an actual point against a prepared clip expression, retaining contour boundary points."
  @spec contains_point?(tuple(), number(), number()) :: boolean()
  def contains_point?(expression, x, y) do
    case expression do
      {:contour, edges} ->
        HtmlValidator.reserve_render_resource(
          :max_clip_projection_work,
          length(edges) + 1,
          :pagination
        )

        boundary =
          Enum.any?(edges, fn {{ax, ay}, {bx, by}} ->
            (bx - ax) * (y - ay) == (by - ay) * (x - ax) and
              x >= min(ax, bx) and x <= max(ax, bx) and y >= min(ay, by) and y <= max(ay, by)
          end)

        if boundary do
          true
        else
          {indexed, _count, contours} = scan_paths(expression, 0)

          HtmlValidator.reserve_render_resource(
            :max_clip_projection_work,
            length(contours) * 2,
            :pagination
          )

          crossings =
            Enum.reduce(contours, %{}, fn {path, {{_, a}, {_, b}} = edge}, crossings ->
              if min(a, b) <= y and y < max(a, b) do
                crossing = {x_at(edge, y), if(b > a, do: 1, else: -1)}
                Map.update(crossings, path, [crossing], &[crossing | &1])
              else
                crossings
              end
            end)

          {scan, work} = scan_expression(indexed, crossings)
          HtmlValidator.reserve_render_resource(:max_clip_projection_work, work, :pagination)
          contains?(scan, x)
        end

      {:intersection, children} ->
        Enum.all?(children, &contains_point?(&1, x, y))
    end
  end
end
