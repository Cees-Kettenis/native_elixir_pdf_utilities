defmodule NativeElixirPdfUtilities.HtmlToPdf.PathGeometry do
  @moduledoc "Static SVG/CSS path construction and bounded arc-length geometry."
  alias NativeElixirPdfUtilities.HtmlToPdf.{ClipGeometry, DrawingGeometry}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Converts canonically parsed SVG commands to absolute moves, lines, cubics and closes."
  @spec commands([{byte(), [number()]}]) :: [{integer(), [number()]}]
  def commands(operations) do
    {commands, _, _, _, _} =
      Enum.reduce(operations, {[], {0.0, 0.0}, {0.0, 0.0}, nil, nil}, fn {command, values},
                                                                         {output,
                                                                          {x, y} = current, start,
                                                                          cubic, quadratic} ->
        HtmlValidator.reserve_render_resource(:max_work, 1, :layout)
        relative = command in ?a..?z
        command = if relative, do: command - 32, else: command
        point = fn px, py -> if relative, do: {x + px, y + py}, else: {px, py} end

        {pieces, last, start, cubic, quadratic} =
          case {command, values} do
            {?M, [px, py]} ->
              target = point.(px, py)
              {[{0, Tuple.to_list(target)}], target, target, nil, nil}

            {?L, [px, py]} ->
              target = point.(px, py)
              {[{1, Tuple.to_list(target)}], target, start, nil, nil}

            {?H, [px]} ->
              target = {if(relative, do: x + px, else: px), y}
              {[{1, Tuple.to_list(target)}], target, start, nil, nil}

            {?V, [py]} ->
              target = {x, if(relative, do: y + py, else: py)}
              {[{1, Tuple.to_list(target)}], target, start, nil, nil}

            {?C, [ax, ay, bx, by, px, py]} ->
              a = point.(ax, ay)
              b = point.(bx, by)
              target = point.(px, py)

              {[{3, Tuple.to_list(a) ++ Tuple.to_list(b) ++ Tuple.to_list(target)}], target,
               start, b, nil}

            {?S, [bx, by, px, py]} ->
              a =
                case cubic do
                  nil -> current
                  {cx, cy} -> {2 * x - cx, 2 * y - cy}
                end

              b = point.(bx, by)
              target = point.(px, py)

              {[{3, Tuple.to_list(a) ++ Tuple.to_list(b) ++ Tuple.to_list(target)}], target,
               start, b, nil}

            {kind, values} when kind in [?Q, ?T] ->
              {control, target} =
                case values do
                  [ax, ay, px, py] ->
                    {point.(ax, ay), point.(px, py)}

                  [px, py] ->
                    {case quadratic do
                       nil -> current
                       {qx, qy} -> {2 * x - qx, 2 * y - qy}
                     end, point.(px, py)}
                end

              {qx, qy} = control
              {px, py} = target

              {[
                 {3,
                  [
                    x + 2 / 3 * (qx - x),
                    y + 2 / 3 * (qy - y),
                    px + 2 / 3 * (qx - px),
                    py + 2 / 3 * (qy - py),
                    px,
                    py
                  ]}
               ], target, start, nil, control}

            {?A, [rx, ry, rotation, large, sweep, px, py]} ->
              target = point.(px, py)

              {arc(current, target, abs(rx), abs(ry), rotation, large, sweep), target, start, nil,
               nil}

            {?Z, []} ->
              {[{4, []}], start, start, nil, nil}
          end

        {Enum.reverse(pieces, output), last, start, cubic, quadratic}
      end)

    Enum.reverse(commands)
  end

  defp arc({x, y}, {px, py}, rx, ry, rotation, large, sweep) do
    cond do
      x == px and y == py ->
        []

      rx == 0 or ry == 0 ->
        [{1, [px, py]}]

      true ->
        angle = rotation * :math.pi() / 180
        cos = :math.cos(angle)
        sin = :math.sin(angle)
        dx = (x - px) / 2
        dy = (y - py) / 2
        ux = cos * dx + sin * dy
        uy = -sin * dx + cos * dy
        radius_scale = max(1.0, :math.sqrt(ux * ux / (rx * rx) + uy * uy / (ry * ry)))
        rx = rx * radius_scale
        ry = ry * radius_scale
        numerator = max(0.0, rx * rx * ry * ry - rx * rx * uy * uy - ry * ry * ux * ux)
        denominator = rx * rx * uy * uy + ry * ry * ux * ux

        factor =
          if denominator == 0,
            do: 0.0,
            else: :math.sqrt(numerator / denominator) * if(large == sweep, do: -1, else: 1)

        cx = factor * rx * uy / ry
        cy = -factor * ry * ux / rx
        center_x = cos * cx - sin * cy + (x + px) / 2
        center_y = sin * cx + cos * cy + (y + py) / 2
        ax = (ux - cx) / rx
        ay = (uy - cy) / ry
        bx = (-ux - cx) / rx
        by = (-uy - cy) / ry
        start = :math.atan2(ay, ax)
        delta = :math.atan2(ax * by - ay * bx, ax * bx + ay * by)

        delta =
          cond do
            sweep == 0 and delta > 0 -> delta - 2 * :math.pi()
            sweep == 1 and delta < 0 -> delta + 2 * :math.pi()
            true -> delta
          end

        # Cubic arc segments span at most one quarter turn. This is a geometric
        # construction rule; subdivision work uses the existing shared budgets.
        count = max(1, ceil(abs(delta) / (:math.pi() / 2)))

        project = fn a, b ->
          {center_x + cos * rx * a - sin * ry * b, center_y + sin * rx * a + cos * ry * b}
        end

        for index <- 0..(count - 1) do
          first = start + delta * index / count
          last = start + delta * (index + 1) / count
          alpha = 4 / 3 * :math.tan((last - first) / 4)

          a =
            project.(
              :math.cos(first) - alpha * :math.sin(first),
              :math.sin(first) + alpha * :math.cos(first)
            )

          b =
            project.(
              :math.cos(last) + alpha * :math.sin(last),
              :math.sin(last) - alpha * :math.cos(last)
            )

          target =
            if index == count - 1, do: {px, py}, else: project.(:math.cos(last), :math.sin(last))

          {3, Tuple.to_list(a) ++ Tuple.to_list(b) ++ Tuple.to_list(target)}
        end
    end
  end

  @doc "Transforms admitted path coordinates through the shared affine point geometry."
  @spec transform(list(), tuple()) :: list()
  def transform(commands, matrix) do
    Enum.map(commands, fn {operation, coords} ->
      {operation,
       coords
       |> Enum.chunk_every(2)
       |> Enum.flat_map(fn [x, y] ->
         DrawingGeometry.transform_point(x, y, matrix) |> Tuple.to_list()
       end)}
    end)
  end

  @doc "Prepares ordered open/closed path edges at the shared clipping precision for static motion placement."
  @spec edges(list()) :: [{{number(), number()}, {number(), number()}}]
  def edges(commands) do
    precision = 1 / NativeElixirPdfUtilities.Limits.get(:clip_curve_precision)

    {edges, _, _} =
      Enum.reduce(commands, {[], nil, nil}, fn {op, values}, {edges, current, start} ->
        HtmlValidator.reserve_render_resource(:max_work, 1, :layout)

        case {op, values} do
          {0, [x, y]} ->
            {edges, {x, y}, {x, y}}

          {1, [x, y]} ->
            {[{current, {x, y}} | edges], {x, y}, start}

          {3, values} ->
            [x, y] = Enum.take(values, -2)
            target = {x, y}

            {:contour, curve} =
              ClipGeometry.flatten_box([{0, Tuple.to_list(current)}, {3, values}], precision)

            curve = Enum.reject(curve, &(&1 == {target, current}))
            {curve ++ edges, target, start}

          {4, []} ->
            {[{current, start} | edges], start, start}
        end
      end)

    Enum.reverse(edges) |> Enum.reject(fn {a, b} -> a == b end)
  end

  @doc "Measures and samples a finite path, returning its point and tangent with CSS endpoint extrapolation."
  @spec sample(list(), number(), boolean()) :: %{point: tuple(), angle: float(), length: number()}
  def sample(commands, distance, closed?) do
    edges = edges(commands)

    measured =
      Enum.map(edges, fn {{ax, ay}, {bx, by}} = edge ->
        {edge, :math.sqrt((bx - ax) * (bx - ax) + (by - ay) * (by - ay))}
      end)

    length = Enum.reduce(measured, 0.0, fn {_, length}, total -> total + length end)

    distance =
      if closed? and length > 0, do: distance - floor(distance / length) * length, else: distance

    case measured do
      [] ->
        point =
          case commands do
            [{0, [x, y]} | _] -> {x, y}
            [] -> {0.0, 0.0}
          end

        %{point: point, angle: 0.0, length: length}

      _ ->
        {{ax, ay}, {bx, by}, local} =
          Enum.reduce_while(measured, distance, fn {{{ax, ay}, {bx, by}}, length}, remaining ->
            if remaining <= length,
              do: {:halt, {ax, ay, bx, by, remaining / length}},
              else: {:cont, remaining - length}
          end)
          |> case do
            {ax, ay, bx, by, ratio} ->
              {{ax, ay}, {bx, by}, ratio}

            remaining ->
              {{{ax, ay}, {bx, by}}, last} = List.last(measured)
              {{ax, ay}, {bx, by}, 1 + remaining / last}
          end

        %{
          point: {ax + (bx - ax) * local, ay + (by - ay) * local},
          angle: :math.atan2(by - ay, bx - ax),
          length: length
        }
    end
  end
end
