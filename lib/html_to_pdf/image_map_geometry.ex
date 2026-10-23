defmodule NativeElixirPdfUtilities.HtmlToPdf.ImageMapGeometry do
  @moduledoc "Geometry for validator-prepared image maps, with ordered masks, even-odd polygons and clipped PDF quadrilaterals."
  alias NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, ImageMapValidator}

  @doc false
  @spec regions(map(), map(), map()) :: [map()]
  def regions(model, viewport, image) do
    clip =
      DrawingGeometry.intersect_clip(
        {viewport.x, viewport.y, viewport.x + viewport.width, viewport.y + viewport.height},
        {image.x, image.y, image.x + image.width, image.y + image.height}
      )

    clip =
      DrawingGeometry.intersect_clip(clip, DrawingGeometry.box_clip_bounds(Map.get(image, :clip)))

    {left, bottom, right, top} = clip

    if right <= left or top <= bottom do
      []
    else
      polygons =
        Enum.map(model.areas, fn area ->
          polygon = polygon(area.shape, viewport.width / 0.75, viewport.height / 0.75)

          polygon =
            Enum.map(polygon, fn {x, y} ->
              {viewport.x + x * 0.75, viewport.y + viewport.height - y * 0.75}
            end)

          {area, clip_polygon(polygon, clip)}
        end)

      edges = Enum.flat_map(polygons, fn {_area, points} -> edges(points) end)
      count = length(edges)

      HtmlValidator.reserve_render_resource(
        :max_image_map_geometry_work,
        div(count * max(count - 1, 0), 2),
        :layout
      )

      intersections = intersections(edges)

      levels =
        ([bottom, top] ++
           Enum.flat_map(edges, fn {{_, ay}, {_, by}} -> [ay, by] end) ++ intersections)
        |> Enum.uniq()
        |> Enum.sort()

      HtmlValidator.reserve_render_resource(
        :max_image_map_geometry_work,
        length(levels) * count * max(length(polygons), 1),
        :layout
      )

      quads =
        levels
        |> Enum.chunk_every(2, 1, :discard)
        |> Enum.reduce(%{}, fn [low, high], result ->
          middle = (low + high) / 2

          active =
            edges
            |> Enum.filter(fn {{_, ay}, {_, by}} ->
              min(ay, by) < middle and middle < max(ay, by)
            end)
            |> Enum.sort_by(&edge_x(&1, middle))

          active
          |> Enum.chunk_every(2, 1, :discard)
          |> Enum.reduce(result, fn [left_edge, right_edge], result ->
            x = (edge_x(left_edge, middle) + edge_x(right_edge, middle)) / 2

            if edge_x(right_edge, middle) <= edge_x(left_edge, middle) do
              result
            else
              owner = Enum.find(polygons, fn {_area, points} -> inside?(points, x, middle) end)

              case owner do
                {area, _} when area.destination != nil ->
                  quad = [
                    {edge_x(left_edge, low), low},
                    {edge_x(right_edge, low), low},
                    {edge_x(right_edge, high), high},
                    {edge_x(left_edge, high), high}
                  ]

                  Map.update(result, area.identity, {area, [quad]}, fn {area, existing} ->
                    {area, [quad | existing]}
                  end)

                _ ->
                  result
              end
            end
          end)
        end)

      Enum.flat_map(model.areas, fn area ->
        case Map.get(quads, area.identity) do
          nil ->
            []

          {area, quads} ->
            {left, bottom, right, top} = bounds(quads)

            box = %{
              type: HtmlValidator.reserve_layout_box(:link_region),
              role: :image_map,
              x: left,
              y: bottom,
              width: right - left,
              height: top - bottom,
              link_quads: Enum.reverse(quads),
              image_map_area: area.identity,
              image_map: model.identity
            }

            case area.destination do
              {:uri, url} -> [Map.put(box, :link_url, url)]
              destination -> [Map.put(box, :link_destination, destination)]
            end
        end
      end)
    end
  end

  defp polygon(shape, width, height) do
    case shape do
      :empty ->
        []

      :default ->
        [{0, 0}, {width, 0}, {width, height}, {0, height}]

      {:rect, left, top, right, bottom} ->
        [{left, top}, {right, top}, {right, bottom}, {left, bottom}]

      {:poly, points} ->
        points

      {:circle, x, y, radius} ->
        count = ImageMapValidator.circle_segments(radius)

        for index <- 0..(count - 1) do
          angle = index * 2 * :math.pi() / count
          {x + radius * :math.cos(angle), y + radius * :math.sin(angle)}
        end
    end
  end

  defp edges(points) do
    case points do
      [] -> []
      [_] -> []
      _ -> Enum.zip(points, tl(points) ++ [hd(points)])
    end
  end

  defp edge_x({{ax, ay}, {bx, by}}, y), do: ax + (bx - ax) * (y - ay) / (by - ay)

  defp inside?(points, x, y) do
    rem(
      Enum.count(edges(points), fn {{ax, ay}, {bx, by}} ->
        ay > y != by > y and x < (bx - ax) * (y - ay) / (by - ay) + ax
      end),
      2
    ) == 1
  end

  defp intersections(edges) do
    case edges do
      [] ->
        []

      [{{ax, ay}, {bx, by}} | rest] ->
        crossings =
          Enum.flat_map(rest, fn {{cx, cy}, {dx, dy}} ->
            denominator = (bx - ax) * (dy - cy) - (by - ay) * (dx - cx)

            if denominator == 0 do
              []
            else
              first = ((cx - ax) * (dy - cy) - (cy - ay) * (dx - cx)) / denominator
              second = ((cx - ax) * (by - ay) - (cy - ay) * (bx - ax)) / denominator

              if first > 0 and first < 1 and second > 0 and second < 1,
                do: [ay + first * (by - ay)],
                else: []
            end
          end)

        crossings ++ intersections(rest)
    end
  end

  @doc false
  @spec clip_quads([list()], tuple()) :: [list()]
  def clip_quads(quads, clip) do
    Enum.flat_map(quads, fn quad ->
      case clip_polygon(quad, clip) do
        [] ->
          []

        [_, _] ->
          []

        [a, b, c] ->
          [[a, b, c, a]]

        [_, _, _, _] = points ->
          [points]

        [first | rest] ->
          rest
          |> Enum.chunk_every(2, 1, :discard)
          |> Enum.map(fn [a, b] -> [first, a, b, first] end)
      end
    end)
  end

  defp clip_polygon(points, {left, bottom, right, top}) do
    Enum.reduce(
      [{:x, left, :minimum}, {:x, right, :maximum}, {:y, bottom, :minimum}, {:y, top, :maximum}],
      points,
      fn {axis, boundary, side}, polygon ->
        Enum.flat_map(edges(polygon), fn {a, b} ->
          index = if axis == :x, do: 0, else: 1
          a_value = elem(a, index)
          b_value = elem(b, index)
          a_in = if side == :minimum, do: a_value >= boundary, else: a_value <= boundary
          b_in = if side == :minimum, do: b_value >= boundary, else: b_value <= boundary

          crossing =
            if a_in != b_in do
              ratio = (boundary - a_value) / (b_value - a_value)

              {elem(a, 0) + ratio * (elem(b, 0) - elem(a, 0)),
               elem(a, 1) + ratio * (elem(b, 1) - elem(a, 1))}
            end

          case {a_in, b_in} do
            {true, true} -> [b]
            {true, false} -> [crossing]
            {false, true} -> [crossing, b]
            _ -> []
          end
        end)
      end
    )
  end

  @doc false
  @spec bounds([list()]) :: tuple()
  def bounds(quads) do
    points = List.flatten(quads)
    xs = Enum.map(points, &elem(&1, 0))
    ys = Enum.map(points, &elem(&1, 1))
    {Enum.min(xs), Enum.min(ys), Enum.max(xs), Enum.max(ys)}
  end
end
