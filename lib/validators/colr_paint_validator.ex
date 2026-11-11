defmodule NativeElixirPdfUtilities.Validators.ColrPaintValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{AffineGeometry, DrawingGeometry}

  alias NativeElixirPdfUtilities.Validators.{
    FontOutlineValidator,
    FontValidator,
    ColrGradientValidator
  }

  @doc "Prepares bounded static COLR artwork as a self-contained SVG at the selected glyph bounds."
  @spec prepare(map(), map(), number()) ::
          {:ok, binary(), tuple(), map()} | {:error, {atom(), map()}}
  def prepare(font, paint, size \\ 12) do
    FontValidator.with_budget(fn ->
      try do
        with {:ok, glyph_bounds} <- FontValidator.prepare_glyph_bounds(font) do
          bounds = intersect(graph_bounds(paint.graph, glyph_bounds), paint.clip)

          if bounds == nil,
            do: invalid("COLR paint graph is unbounded; supply glyph clips or a clip box")

          {a, b, c, d} = bounds

          if c <= a or d <= b do
            {:ok, "", {a, b, c, d}, %{mode: :inline, resources: %{}}}
          else
            state = %{font: font, bounds: glyph_bounds, next: 0, size: size, resources: %{}}
            {art, state} = svg(paint.graph, bounds, state)

            source =
              IO.iodata_to_binary([
                "<svg xmlns='http://www.w3.org/2000/svg' viewBox='",
                numbers([a, -d, c - a, d - b]),
                "' width='",
                number(c - a),
                "' height='",
                number(d - b),
                "'><g transform='scale(1,-1)'>",
                art,
                "</g></svg>"
              ])

            FontValidator.check(:max_font_decoded_bytes, byte_size(source))
            {:ok, source, bounds, %{mode: :inline, resources: state.resources}}
          end
        end
      catch
        {:colr_paint_error, error} -> error
      end
    end)
  end

  defp graph_bounds(graph, bounds) do
    case graph do
      :empty ->
        {0, 0, 0, 0}

      {:solid, _} ->
        nil

      {:gradient, _, _, _} ->
        nil

      {:glyph, id, child} ->
        intersect(elem(bounds, id), graph_bounds(child, bounds))

      {:layers, children} ->
        Enum.reduce(children, {0, 0, 0, 0}, fn child, box ->
          union(box, graph_bounds(child, bounds))
        end)

      {:transform, matrix, child} ->
        case graph_bounds(child, bounds) do
          nil -> nil
          box -> transformed(box, List.to_tuple(matrix))
        end

      {:composite, mode, source, backdrop} ->
        case mode do
          0 ->
            {0, 0, 0, 0}

          1 ->
            graph_bounds(source, bounds)

          2 ->
            graph_bounds(backdrop, bounds)

          intersection when intersection in [5, 6] ->
            intersect(graph_bounds(source, bounds), graph_bounds(backdrop, bounds))

          7 ->
            graph_bounds(source, bounds)

          8 ->
            graph_bounds(backdrop, bounds)

          9 ->
            graph_bounds(backdrop, bounds)

          10 ->
            graph_bounds(source, bounds)

          _ ->
            union(graph_bounds(source, bounds), graph_bounds(backdrop, bounds))
        end
    end
  end

  defp svg(graph, box, state) do
    FontValidator.reserve(:max_font_color_work, 1)
    {a, b, c, d} = box

    case graph do
      :empty ->
        {[], state}

      {:solid, color} ->
        {[
           "<rect x='",
           number(a),
           "' y='",
           number(b),
           "' width='",
           number(max(c - a, 0)),
           "' height='",
           number(max(d - b, 0)),
           "' ",
           fill(color),
           "/>"
         ], state}

      {:glyph, id, child} ->
        path =
          case FontOutlineValidator.prepare_path(state.font, id) do
            {:ok, path} -> path
            error -> throw({:colr_paint_error, error})
          end

        name = "clip#{state.next}"
        state = %{state | next: state.next + 1}
        {art, state} = svg(child, intersect(box, elem(state.bounds, id)), state)

        {[
           "<defs><clipPath id='",
           name,
           "'><path d='",
           path,
           "'/></clipPath></defs><g clip-path='url(#",
           name,
           ")'>",
           art,
           "</g>"
         ], state}

      {:layers, children} ->
        Enum.map_reduce(children, state, fn child, state -> svg(child, box, state) end)

      {:transform, matrix, child} ->
        case AffineGeometry.inverse(List.to_tuple(matrix)) do
          :error ->
            {[], state}

          {:ok, inverse} ->
            {art, state} = svg(child, transformed(box, inverse), state)
            {["<g transform='matrix(", numbers(matrix), ")'>", art, "</g>"], state}
        end

      {:gradient, kind, values, {extend, stops}} ->
        gradient(kind, values, extend, stops, box, state)

      {:composite, mode, source, backdrop} ->
        {source, state} = svg(source, box, state)
        {backdrop, state} = svg(backdrop, box, state)

        case mode do
          0 ->
            {[], state}

          1 ->
            {source, state}

          2 ->
            {backdrop, state}

          3 ->
            {[backdrop, source], state}

          4 ->
            {[source, backdrop], state}

          _ ->
            index = state.next
            state = %{state | next: index + 1}
            filter = "composite#{index}"
            src = "source#{index}"
            dest = "backdrop#{index}"

            operation =
              case mode do
                porter when porter in [5, 6, 7, 8, 9, 10, 11] ->
                  {operator, in1, in2} =
                    case porter do
                      5 -> {"in", "s", "d"}
                      6 -> {"in", "d", "s"}
                      7 -> {"out", "s", "d"}
                      8 -> {"out", "d", "s"}
                      9 -> {"atop", "s", "d"}
                      10 -> {"atop", "d", "s"}
                      11 -> {"xor", "s", "d"}
                    end

                  ["<feComposite in='", in1, "' in2='", in2, "' operator='", operator, "'/>"]

                12 ->
                  "<feComposite in='s' in2='d' operator='arithmetic' k1='0' k2='1' k3='1' k4='0'/>"

                blend ->
                  name =
                    Enum.at(
                      ~w(screen overlay darken lighten color-dodge color-burn hard-light soft-light difference exclusion multiply hue saturation color luminosity),
                      blend - 13
                    )

                  ["<feBlend in='s' in2='d' mode='", name, "'/>"]
              end

            # feImage local references render each complete subgraph into the
            # same user-space filter canvas before Porter-Duff composition.
            {[
               "<defs><g id='",
               src,
               "'>",
               source,
               "</g><g id='",
               dest,
               "'>",
               backdrop,
               "</g><filter id='",
               filter,
               "' filterUnits='userSpaceOnUse' primitiveUnits='userSpaceOnUse' x='",
               number(a),
               "' y='",
               number(b),
               "' width='",
               number(max(c - a, 0)),
               "' height='",
               number(max(d - b, 0)),
               "' color-interpolation-filters='sRGB'><feImage href='#",
               src,
               "' result='s'/><feImage href='#",
               dest,
               "' result='d'/>",
               operation,
               "</filter></defs><rect x='",
               number(a),
               "' y='",
               number(b),
               "' width='",
               number(max(c - a, 0)),
               "' height='",
               number(max(d - b, 0)),
               "' filter='url(#",
               filter,
               ")'/>"
             ], state}
        end
    end
  end

  defp gradient(kind, values, extend, stops, box, state) do
    {a, b, c, d} = box
    first = elem(hd(stops), 0)
    last = elem(List.last(stops), 0)

    if first == last do
      cond do
        extend != 0 ->
          {[], state}

        length(stops) == 1 ->
          svg({:solid, elem(hd(stops), 1)}, box, state)

        true ->
          gradient(
            kind,
            values,
            extend,
            [{first - 1, elem(hd(stops), 1)} | stops] ++ [{last + 1, elem(List.last(stops), 1)}],
            box,
            state
          )
      end
    else
      name = "gradient#{state.next}"
      state = %{state | next: state.next + 1}

      stop_xml =
        Enum.map(stops, fn {offset, {r, g, b, alpha}} ->
          [
            "<stop offset='",
            number((offset - first) / (last - first)),
            "' stop-color='rgb(",
            numbers([r * 255, g * 255, b * 255]),
            ")' stop-opacity='",
            number(alpha),
            "'/>"
          ]
        end)

      spread = Enum.at(["pad", "repeat", "reflect"], extend)

      case kind do
        4 ->
          [x0, y0, x1, y1, x2, y2] = values
          nx = -(y2 - y0)
          ny = x2 - x0
          denominator = nx * nx + ny * ny
          projection = (x1 - x0) * nx + (y1 - y0) * ny

          if denominator == 0 or projection == 0 do
            {[], state}
          else
            vx = nx * projection / denominator
            vy = ny * projection / denominator

            {[
               "<defs><linearGradient id='",
               name,
               "' gradientUnits='userSpaceOnUse' spreadMethod='",
               spread,
               "' x1='",
               number(x0 + vx * first),
               "' y1='",
               number(y0 + vy * first),
               "' x2='",
               number(x0 + vx * last),
               "' y2='",
               number(y0 + vy * last),
               "'>",
               stop_xml,
               "</linearGradient></defs>",
               gradient_rectangle(box, name)
             ], state}
          end

        6 ->
          case ColrGradientValidator.prepare_radial(
                 values,
                 extend,
                 stops,
                 box,
                 state.font.units_per_em,
                 state.size
               ) do
            {:ok, png} ->
              key = "svg-resource:radial#{state.next}"

              state = %{
                state
                | next: state.next + 1,
                  resources: Map.put(state.resources, key, %{data: png, media_type: "image/png"})
              }

              {[
                 "<image x='",
                 number(a),
                 "' y='",
                 number(b),
                 "' width='",
                 number(c - a),
                 "' height='",
                 number(d - b),
                 "' preserveAspectRatio='none' transform='translate(0 ",
                 number(b + d),
                 ") scale(1,-1)' href='",
                 key,
                 "'/>"
               ], state}

            error ->
              throw({:colr_paint_error, error})
          end

        8 ->
          [cx, cy, start, finish] = values
          start = (start + 1) * :math.pi()
          finish = (finish + 1) * :math.pi()
          segments = Limits.get(:font_color_sweep_segments)
          FontValidator.reserve(:max_font_color_work, segments * (length(stops) + 1))

          radius =
            Enum.max(
              for {x, y} <- [{a, b}, {a, d}, {c, b}, {c, d}],
                  do: :math.sqrt((x - cx) * (x - cx) + (y - cy) * (y - cy))
            ) * 1.01

          wedges =
            if finish == start and extend != 0,
              do: [],
              else:
                (for i <- 0..(segments - 1) do
                   angle0 = i * 2 * :math.pi() / segments
                   angle1 = (i + 1) * 2 * :math.pi() / segments

                   t =
                     if finish == start,
                       do: if((angle0 + angle1) / 2 < start, do: -1.0, else: 2.0),
                       else: ((angle0 + angle1) / 2 - start) / (finish - start)

                   color = ColrGradientValidator.sample(stops, t, extend)

                   [
                     "<path d='M",
                     numbers([cx, cy]),
                     " L",
                     numbers([cx + :math.cos(angle0) * radius, cy + :math.sin(angle0) * radius]),
                     " L",
                     numbers([cx + :math.cos(angle1) * radius, cy + :math.sin(angle1) * radius]),
                     " Z' ",
                     fill(color),
                     "/>"
                   ]
                 end)

          {wedges, state}
      end
    end
  end

  defp gradient_rectangle({a, b, c, d}, name),
    do: [
      "<rect x='",
      number(a),
      "' y='",
      number(b),
      "' width='",
      number(max(c - a, 0)),
      "' height='",
      number(max(d - b, 0)),
      "' fill='url(#",
      name,
      ")'/>"
    ]

  defp fill({r, g, b, a}),
    do: ["fill='rgb(", numbers([r * 255, g * 255, b * 255]), ")' fill-opacity='", number(a), "'"]

  defp numbers(values), do: Enum.map_join(values, " ", &number/1)

  defp number(value),
    do: if(is_integer(value), do: Integer.to_string(value), else: Float.to_string(value * 1.0))

  defp transformed({a, b, c, d}, matrix) do
    points =
      Enum.map([{a, b}, {a, d}, {c, b}, {c, d}], fn {x, y} ->
        DrawingGeometry.transform_point(x, y, matrix)
      end)

    xs = Enum.map(points, &elem(&1, 0))
    ys = Enum.map(points, &elem(&1, 1))
    {Enum.min(xs), Enum.min(ys), Enum.max(xs), Enum.max(ys)}
  end

  defp union(a, b) do
    case {a, b} do
      {nil, _} -> nil
      {_, nil} -> nil
      {{a, b, c, d}, {e, f, g, h}} -> {min(a, e), min(b, f), max(c, g), max(d, h)}
    end
  end

  defp intersect(a, b) do
    case {a, b} do
      {nil, box} -> box
      {box, nil} -> box
      {{a, b, c, d}, {e, f, g, h}} -> {max(a, e), max(b, f), min(c, g), min(d, h)}
    end
  end

  defp invalid(message),
    do:
      throw(
        {:colr_paint_error,
         Diagnostics.error(:font, :invalid_font, message,
           module: __MODULE__,
           operation: :paint_color_font,
           source: "COLR paint"
         )}
      )
end
