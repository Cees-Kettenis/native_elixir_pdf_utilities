defmodule NativeElixirPdfUtilities.HtmlToPdf.CssBorderArtwork do
  @moduledoc "Source CSS border contour measurement and finite initial dash artwork."
  alias NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Measures validated rounded contours using the source Skia conic subdivision and float32 distance accumulation."
  @spec measure(map()) :: map()
  def measure(contour) do
    x = contour.x
    y = contour.y
    right = x + contour.width
    bottom = y + contour.height
    {blx, bly} = contour.radii.bottom_left
    {brx, bry} = contour.radii.bottom_right
    {trx, try_} = contour.radii.top_right
    {tlx, tly} = contour.radii.top_left

    primitives = [
      %{type: :line, from: {x + blx, y}, to: {right - brx, y}},
      %{
        type: :conic,
        from: {right - brx, y},
        control: {right, y},
        to: {right, y + bry},
        center: {right - brx, y + bry},
        radii: {brx, bry}
      },
      %{type: :line, from: {right, y + bry}, to: {right, bottom - try_}},
      %{
        type: :conic,
        from: {right, bottom - try_},
        control: {right, bottom},
        to: {right - trx, bottom},
        center: {right - trx, bottom - try_},
        radii: {trx, try_}
      },
      %{type: :line, from: {right - trx, bottom}, to: {x + tlx, bottom}},
      %{
        type: :conic,
        from: {x + tlx, bottom},
        control: {x, bottom},
        to: {x, bottom - tly},
        center: {x + tlx, bottom - tly},
        radii: {tlx, tly}
      },
      %{type: :line, from: {x, bottom - tly}, to: {x, y + bly}},
      %{
        type: :conic,
        from: {x, y + bly},
        control: {x, y},
        to: {x + blx, y},
        center: {x + blx, y + bly},
        radii: {blx, bly}
      }
    ]

    {segments, length} =
      primitives
      |> Enum.with_index()
      |> Enum.map_reduce(0.0, fn {primitive, index}, distance ->
        spans =
          if primitive.type == :line,
            do: [{0.0, primitive.from, 1.0, primitive.to}],
            else: conic_spans(primitive, 0.0, primitive.from, 1.0, primitive.to, 0)

        Enum.map_reduce(spans, distance, fn {t0, {ax, ay}, t1, {bx, by}}, distance ->
          next =
            source_float(
              distance + source_float(:math.sqrt((bx - ax) * (bx - ax) + (by - ay) * (by - ay)))
            )

          {%{primitive: primitive, index: index, from: distance, to: next, t0: t0, t1: t1}, next}
        end)
      end)

    %{length: length, segments: List.flatten(segments) |> Enum.filter(&(&1.to > &1.from))}
  end

  @doc "Returns the source closed-path dash length, gap and cap for validated border thickness and contour length."
  @spec pattern(:dashed | :dotted, number(), number()) :: map()
  def pattern(style, width, length) do
    dashed? = style == :dashed or width <= 3
    dash = if style == :dashed, do: width * if(width >= 3, do: 2, else: 3), else: width
    nominal = if style == :dashed, do: width * if(width >= 3, do: 1, else: 2), else: width

    result =
      cond do
        not dashed? and length < width * 2 ->
          %{dash: 0.0, gap: width * 2, cap: :round}

        dashed? and length <= dash * 2 ->
          %{dash: length * 1.0, gap: 0.0, cap: :butt}

        dashed? and length <= 2 * (dash + nominal) ->
          factor = length / (2 * (dash + nominal))
          %{dash: dash * factor, gap: nominal * factor, cap: :butt}

        style == :dashed or not dashed? ->
          count = max(Float.floor(length / (dash + nominal)), 1)
          first = (length - count * dash) / count
          second = (length - (count + 1) * dash) / (count + 1)

          gap =
            if second <= 0 or abs(first - nominal) < abs(second - nominal),
              do: first,
              else: second

          if dashed?,
            do: %{dash: dash, gap: gap, cap: :butt},
            else: %{dash: 0.0, gap: gap + width - 0.01, cap: :round}

        true ->
          %{dash: dash, gap: nominal, cap: :butt}
      end

    Map.new(result, fn {key, value} ->
      {key, if(is_number(value), do: source_float(value), else: value)}
    end)
  end

  @doc "Builds actual source dash paths or round-dot centers along the measured validated contour."
  @spec paths(map(), map()) :: [
          {:stroke, [{integer(), [number()]}]} | {:dot, {number(), number()}}
        ]
  def paths(measure, pattern) do
    period = pattern.dash + pattern.gap

    if period <= 0 or measure.length <= 0 do
      []
    else
      # The number of emitted pieces is governed by the existing shared layout
      # and projection work budgets, not a separate painter ceiling.
      Stream.iterate(0.0, &source_float(&1 + period))
      |> Stream.take_while(&(&1 < measure.length))
      |> Enum.map(fn start ->
        HtmlValidator.reserve_render_resource(
          :max_clip_projection_work,
          1 + length(measure.segments),
          :writer
        )

        if pattern.cap == :round do
          segment = Enum.find(measure.segments, &(&1.to >= start))
          {:dot, segment_point(segment, start)}
        else
          stop = min(start + pattern.dash, measure.length)
          pieces = Enum.filter(measure.segments, &(&1.from < stop and &1.to > start))

          commands =
            Enum.flat_map(pieces, fn segment ->
              first = segment_point(segment, max(start, segment.from))
              last = segment_point(segment, min(stop, segment.to))

              case segment.primitive.type do
                :line -> [{0, Tuple.to_list(first)}, {1, Tuple.to_list(last)}]
                :conic -> arc_commands(segment.primitive, first, last)
              end
            end)

          commands =
            commands
            |> Enum.reduce([], fn command, acc ->
              case {command, acc} do
                {{0, _}, [_ | _]} -> acc
                _ -> [command | acc]
              end
            end)
            |> Enum.reverse()

          {:stroke, commands}
        end
      end)
    end
  end

  # These fixed .5 CSS-pixel and eight-subdivision rules describe Skia's
  # contour sampling, not configurable renderer resource ceilings. Each
  # subdivision separately consumes the shared projection work budget.
  defp conic_spans(primitive, t0, p0, t1, p1, depth) do
    HtmlValidator.reserve_render_resource(:max_clip_projection_work, 1, :writer)
    middle = (t0 + t1) / 2
    {mx, my} = conic_point(primitive, middle)
    {ax, ay} = p0
    {bx, by} = p1

    if depth < 8 and max(abs(mx - (ax + bx) / 2), abs(my - (ay + by) / 2)) > 0.5 do
      conic_spans(primitive, t0, p0, middle, {mx, my}, depth + 1) ++
        conic_spans(primitive, middle, {mx, my}, t1, p1, depth + 1)
    else
      [{t0, p0, t1, p1}]
    end
  end

  defp segment_point(segment, distance) do
    t =
      segment.t0 +
        (segment.t1 - segment.t0) * (distance - segment.from) / (segment.to - segment.from)

    if segment.primitive.type == :conic do
      conic_point(segment.primitive, t)
    else
      {ax, ay} = segment.primitive.from
      {bx, by} = segment.primitive.to
      {source_float(ax + (bx - ax) * t), source_float(ay + (by - ay) * t)}
    end
  end

  defp conic_point(primitive, t) do
    {ax, ay} = primitive.from
    {bx, by} = primitive.control
    {cx, cy} = primitive.to
    weight = source_float(:math.sqrt(0.5))
    u = 1 - t
    denominator = u * u + 2 * weight * u * t + t * t

    {source_float((u * u * ax + 2 * weight * u * t * bx + t * t * cx) / denominator),
     source_float((u * u * ay + 2 * weight * u * t * by + t * t * cy) / denominator)}
  end

  defp arc_commands(primitive, {ax, ay} = first, {bx, by} = last) do
    {cx, cy} = primitive.center
    {rx, ry} = primitive.radii

    if rx == 0 or ry == 0 do
      [{0, Tuple.to_list(first)}, {1, Tuple.to_list(last)}]
    else
      start = :math.atan2((ay - cy) / ry, (ax - cx) / rx)
      stop = :math.atan2((by - cy) / ry, (bx - cx) / rx)
      stop = if stop < start, do: stop + 2 * :math.pi(), else: stop
      # Reuse the shared quarter-circle control coefficient for subarcs.
      {_, [coefficient | _]} = Enum.at(DrawingGeometry.rounded_rect_commands(0, 0, 2, 2, 1), 2)
      factor = (coefficient - 1) * :math.tan((stop - start) / 4) / :math.tan(:math.pi() / 8)

      [
        {0, Tuple.to_list(first)},
        {3,
         [
           ax - factor * rx * :math.sin(start),
           ay + factor * ry * :math.cos(start),
           bx + factor * rx * :math.sin(stop),
           by - factor * ry * :math.cos(stop),
           bx,
           by
         ]}
      ]
    end
  end

  defp source_float(value) do
    <<rounded::float-size(32)>> = <<value * 1.0::float-size(32)>>
    rounded
  end
end
