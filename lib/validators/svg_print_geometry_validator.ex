defmodule NativeElixirPdfUtilities.Validators.SvgPrintGeometryValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, SvgStyleValidator}

  @extensions ~w(marker-segment marker-pattern marker-knockout-left marker-knockout-right stroke-alignment stroke-dashadjust stroke-dashcorner)
  @knockout ~w(marker-knockout-left marker-knockout-right)
  @path_geometry ~w(x y x1 y1 x2 y2 width height rx ry points)
  @shape_effects ~w(id transform opacity filter mask clip-path visibility style)

  @doc "Checks whether the SVG source contains a print geometry property name."
  @spec selected?(String.t()) :: boolean()
  def selected?(source), do: String.contains?(source, @extensions)

  @doc "Validates the value of a recognized print geometry property and returns its original text."
  @spec prepare(String.t(), String.t()) :: {:ok, String.t()} | :error
  def prepare(property, source) do
    parsed =
      case property do
        "marker-segment" -> SvgStyleValidator.parse("marker-mid", source)
        "marker-pattern" -> pattern(source)
        property when property in @knockout -> length_value(source)
        "stroke-alignment" -> if source in ~w(center inside outside), do: {:ok, source}
        "stroke-dashadjust" -> adjustment(source)
        "stroke-dashcorner" -> if source == "none", do: {:ok, :none}, else: gap(source)
      end

    case parsed do
      {:ok, _} -> {:ok, source}
      _ -> :error
    end
  end

  @doc "Rewrites supported stroke and marker geometry in the SVG tree and reports unsupported geometry."
  @spec normalize(map()) :: {:ok, map()} | {:error, {:invalid_document, Diagnostics.diagnostic()}}
  def normalize(root) do
    definitions =
      root
      |> NativeElixirPdfUtilities.Validators.SvgSceneValidator.tree_nodes()
      |> Enum.reduce(%{}, fn {node, _}, defs ->
        case node do
          %{tag: "marker", attributes: %{"id" => id} = attrs} ->
            left = length_value(Map.get(attrs, "marker-knockout-left", "0"))
            right = length_value(Map.get(attrs, "marker-knockout-right", "0"))

            if Map.take(attrs, @knockout) == %{} do
              defs
            else
              Map.put(defs, id, {left, right, attrs})
            end

          _ ->
            defs
        end
      end)

    rewrite(root, %{}, definitions)
  end

  defp rewrite(node, inherited, definitions) do
    attrs = node.attributes

    unsupported_style? =
      selected?(Map.get(attrs, "style", "")) or
        (node.tag == "style" and Enum.any?(node.children, &selected?(Map.get(&1, :text, ""))))

    extensions = Map.merge(inherited, Map.take(attrs, @extensions -- @knockout))

    with false <- unsupported_style?,
         {:ok, children} <-
           Enum.reduce_while(node.children, {:ok, []}, fn
             %{type: :element} = child, {:ok, children} ->
               case rewrite(child, extensions, definitions) do
                 {:ok, child} -> {:cont, {:ok, [child | children]}}
                 error -> {:halt, error}
               end

             child, {:ok, children} ->
               {:cont, {:ok, [child | children]}}
           end) do
      node = %{node | attributes: Map.drop(attrs, @extensions), children: Enum.reverse(children)}

      case node.tag do
        "rect" ->
          with {:ok, node} <-
                 align_rectangle(node, Map.get(extensions, "stroke-alignment", "center")),
               do:
                 stroke_and_markers(node, Map.delete(extensions, "stroke-alignment"), definitions)

        tag when tag in ~w(line polyline) ->
          stroke_and_markers(node, extensions, definitions)

        tag when tag in ~w(circle ellipse polygon path text) ->
          if active?(extensions), do: unsupported(tag), else: {:ok, node}

        _ ->
          {:ok, node}
      end
    else
      true -> unsupported("standalone SVG CSS print geometry")
      error -> error
    end
  end

  defp active?(extensions) do
    Enum.any?(extensions, fn {property, value} ->
      value != "none" and {property, value} != {"stroke-alignment", "center"}
    end)
  end

  defp align_rectangle(node, side) do
    case side do
      "center" -> {:ok, node}
      side when side in ["inside", "outside"] -> paint_rectangle_stroke(node, side)
      _ -> unsupported("stroke-alignment")
    end
  end

  defp paint_rectangle_stroke(node, side) do
    attrs = node.attributes

    if Map.get(attrs, "stroke", "none") == "none" do
      {:ok, node}
    else
      with true <- Map.get(attrs, "stroke-linejoin", "miter") == "miter",
           true <- Map.get(attrs, "stroke-dasharray", "none") == "none",
           true <-
             Map.get(attrs, "rx", "0") in ["0", "0.0"] and
               Map.get(attrs, "ry", "0") in ["0", "0.0"],
           {:ok, x} <- length_value(Map.get(attrs, "x", "0")),
           {:ok, y} <- length_value(Map.get(attrs, "y", "0")),
           {:ok, width} <- length_value(Map.get(attrs, "width", "0")),
           {:ok, height} <- length_value(Map.get(attrs, "height", "0")),
           {:ok, stroke} <- gap(Map.get(attrs, "stroke-width", "1")) do
        {outer, inner} =
          if side == "inside" do
            {{x, y, width, height},
             {x + stroke, y + stroke, width - 2 * stroke, height - 2 * stroke}}
          else
            {{x - stroke, y - stroke, width + 2 * stroke, height + 2 * stroke},
             {x, y, width, height}}
          end

        ring =
          shape(node, "path", %{
            "d" => box_path(outer) <> box_path(inner),
            "fill" => attrs["stroke"],
            "fill-rule" => "evenodd",
            "fill-opacity" => Map.get(attrs, "stroke-opacity", "1"),
            "stroke" => "none"
          })

        fill = child(node, %{node.attributes | "stroke" => "none"})
        {:ok, group(node, if(width > 0 and height > 0, do: [fill, ring], else: []))}
      else
        _ -> unsupported("stroke-alignment")
      end
    end
  end

  defp box_path({x, y, width, height}) do
    if width > 0 and height > 0,
      do: "M#{n(x)} #{n(y)}h#{n(width)}v#{n(height)}h#{n(-width)}Z",
      else: ""
  end

  defp stroke_and_markers(node, extensions, definitions) do
    knockout? =
      Enum.any?(
        ~w(marker-start marker-end),
        &Map.has_key?(definitions, marker_id(node.attributes[&1]))
      )

    if active?(extensions) or knockout? do
      with {:ok, points} <- points(node),
           [{first, last, distance} | _] = segments <- segments(points),
           true <- Map.get(extensions, "stroke-alignment", "center") == "center",
           {:ok, marker_paint} <- marker_shapes(node, segments, extensions),
           {:ok, painted} <-
             stroke_shapes(
               node,
               {first, last, distance},
               segments,
               extensions,
               definitions,
               knockout?
             ) do
        {:ok, group(node, painted ++ marker_paint)}
      else
        {:error, _} = error -> error
        _ -> unsupported(node.tag)
      end
    else
      {:ok, node}
    end
  end

  defp marker_shapes(node, segments, extensions) do
    with {:ok, marker} <-
           SvgStyleValidator.parse("marker-mid", Map.get(extensions, "marker-segment", "none")),
         {:ok, pattern} <- pattern(Map.get(extensions, "marker-pattern", "none")) do
      total = Enum.reduce(segments, 0.0, fn {_, _, distance}, sum -> sum + distance end)

      with {:ok, placements} <- repeat_markers(pattern, total) do
        middle =
          if marker == :none do
            []
          else
            Enum.map(segments, fn {from, to, distance} ->
              marker_shape(node, {from, to, distance}, distance / 2, marker)
            end)
          end

        {repeating, _} =
          Enum.map_reduce(placements, {segments, 0.0}, fn {distance, marker},
                                                          {remaining, start} ->
            {[segment | _] = remaining, start} = seek_segment(remaining, start, distance)
            {marker_shape(node, segment, distance - start, marker), {remaining, start}}
          end)

        {:ok, middle ++ repeating}
      end
    else
      _ -> unsupported("marker-segment or marker-pattern")
    end
  end

  defp seek_segment(segments, start, distance) do
    case segments do
      [{_, _, length} | [_ | _] = rest] when start + length <= distance ->
        seek_segment(rest, start + length, distance)

      _ ->
        {segments, start}
    end
  end

  defp repeat_markers(pattern, total) do
    if pattern == [], do: {:ok, []}, else: repeat_marker_pattern(pattern, total)
  end

  defp repeat_marker_pattern(pattern, total) do
    {markers, period} =
      Enum.reduce(pattern, {[], 0.0}, fn
        {:gap, gap}, {marks, shift} -> {marks, shift + gap}
        {:percent, ratio}, {marks, shift} -> {marks, shift + ratio * total}
        {:fragment, _} = marker, {marks, shift} -> {[{shift, marker} | marks], shift}
        :none, acc -> acc
      end)

    if period > 0 do
      limit = Limits.get(:max_entries)

      if period < total / (limit + 1),
        do: HtmlValidator.check_render_resource(:max_entries, limit + 1, :style)

      cycles = floor(total / period)
      HtmlValidator.reserve_render_resource(:max_entries, (cycles + 1) * length(markers), :style)

      bytes_per_cycle =
        Enum.reduce(markers, 0, fn {_, {:fragment, id}}, bytes ->
          bytes + byte_size(id) + 128
        end)

      HtmlValidator.check_render_resource(
        :max_input_bytes,
        bytes_per_cycle * (cycles + 1),
        :style
      )

      markers = Enum.reverse(markers)

      {:ok,
       for(
         cycle <- 0..cycles,
         {offset, marker} <- markers,
         distance = cycle * period + offset,
         distance <= total,
         do: {distance, marker}
       )}
    else
      unsupported("marker-pattern needs a positive repeat interval")
    end
  end

  defp marker_shape(node, {from, to, distance}, offset, {:fragment, id}) do
    {x, y} = point_on(from, to, offset / distance)
    {ux, uy} = point_on(from, to, 1 / distance)
    {fx, fy} = from

    shape(node, "path", %{
      "d" => "M#{n(x)} #{n(y)}l#{n(ux - fx)} #{n(uy - fy)}",
      "fill" => "none",
      "stroke" => "none",
      "stroke-width" => Map.get(node.attributes, "stroke-width", "1"),
      "marker-start" => "url(##{id})"
    })
  end

  defp stroke_shapes(node, {from, to, distance}, segments, extensions, definitions, knockout?) do
    attrs = node.attributes
    corner = Map.get(extensions, "stroke-dashcorner", "none")
    adjust = Map.get(extensions, "stroke-dashadjust", "none")

    cond do
      not knockout? and corner == "none" and adjust == "none" ->
        {:ok, [child(node, attrs)]}

      length(segments) != 1 or Map.get(attrs, "stroke-linecap", "butt") != "butt" ->
        unsupported("stroke-dashcorner, stroke-dashadjust or marker-knockout")

      knockout? and
          (corner != "none" or adjust != "none" or
             Map.get(attrs, "stroke-dasharray", "none") != "none") ->
        unsupported("marker knockout with dashed stroke")

      knockout? ->
        with {:ok, start} <- knockout_extent(definitions, attrs["marker-start"], 1),
             {:ok, finish} <- knockout_extent(definitions, attrs["marker-end"], 0) do
          stroke =
            piece(node, from, to, min(distance, start), max(0, distance - finish), distance, %{})

          markers = child(node, Map.put(attrs, "stroke", "none"))
          {:ok, stroke ++ [markers]}
        end

      true ->
        with {:ok, corner} <- if(corner == "none", do: {:ok, 0}, else: gap(corner)),
             {:ok, adjustment} <- adjustment(adjust),
             {:ok, dashes} <- dash_values(Map.get(attrs, "stroke-dasharray", "none")) do
          corner = min(corner, distance / 2)
          length = distance - 2 * corner
          dash_source = adjusted_dashes(dashes, length, adjustment)

          if corner == 0 do
            {:ok, [child(node, Map.put(attrs, "stroke-dasharray", dash_source))]}
          else
            begin = piece(node, from, to, 0, corner, distance, %{"stroke-dasharray" => "none"})

            middle =
              piece(node, from, to, corner, distance - corner, distance, %{
                "stroke-dasharray" => dash_source
              })

            finish =
              piece(node, from, to, distance - corner, distance, distance, %{
                "stroke-dasharray" => "none"
              })

            markers = child(node, Map.put(attrs, "stroke", "none"))
            {:ok, begin ++ middle ++ finish ++ [markers]}
          end
        else
          _ -> unsupported("stroke-dashcorner or stroke-dashadjust")
        end
    end
  end

  defp knockout_extent(definitions, reference, side) do
    case Map.get(definitions, marker_id(reference)) do
      nil ->
        {:ok, 0}

      {{:ok, left}, {:ok, right}, %{"markerUnits" => "userSpaceOnUse"} = attrs}
      when not is_map_key(attrs, "viewBox") and left <= 0 and right >= 0 ->
        {:ok, if(side == 1, do: right, else: -left)}

      _ ->
        unsupported("marker-knockout")
    end
  end

  defp marker_id(reference) do
    case SvgStyleValidator.parse("marker-start", reference) do
      {:ok, {:fragment, id}} -> id
      _ -> nil
    end
  end

  defp piece(node, from, to, start, finish, distance, paint) do
    if start < finish do
      stroke_piece(node, from, to, start, finish, distance, paint)
    else
      []
    end
  end

  defp stroke_piece(node, from, to, start, finish, distance, paint) do
    {x1, y1} = point_on(from, to, start / distance)
    {x2, y2} = point_on(from, to, finish / distance)

    attributes =
      node.attributes
      |> Map.take(
        ~w(stroke stroke-width stroke-opacity stroke-linecap stroke-dasharray stroke-dashoffset)
      )
      |> Map.merge(paint)
      |> Map.merge(%{"x1" => n(x1), "y1" => n(y1), "x2" => n(x2), "y2" => n(y2)})

    [shape(node, "line", attributes)]
  end

  defp dash_values(source) do
    if source == "none", do: {:ok, []}, else: parse_dash_values(source)
  end

  defp parse_dash_values(source) do
    values = source |> String.split(~r/[\s,]+/, trim: true) |> Enum.map(&gap/1)

    if values != [] and Enum.all?(values, &match?({:ok, _}, &1)) do
      values = Enum.map(values, &elem(&1, 1))
      {:ok, if(rem(length(values), 2) == 1, do: values ++ values, else: values)}
    else
      :error
    end
  end

  defp adjusted_dashes(values, length, adjustment) do
    if values == [], do: "none", else: scale_dashes(values, length, adjustment)
  end

  defp scale_dashes(values, length, adjustment) do
    {values, _} =
      Enum.map_reduce(values, 0, fn value, index -> {{value, rem(index, 2)}, index + 1} end)

    {direction, selected} = adjustment

    {variable, fixed} =
      Enum.split_with(values, fn {_, kind} -> selected == :both or selected == kind end)

    variable = Enum.reduce(variable, 0.0, fn {value, _}, sum -> sum + value end)
    fixed = Enum.reduce(fixed, 0.0, fn {value, _}, sum -> sum + value end)
    period = variable + fixed

    multiplier =
      if period > 0 and variable > 0 and length > 0 and direction != :none do
        repeats =
          if direction == :stretch, do: floor(length / period), else: ceil(length / period)

        scale = if repeats > 0, do: (length / repeats - fixed) / variable, else: 1.0
        if scale >= 0 and scale <= 2, do: scale, else: 1.0
      else
        1.0
      end

    Enum.map_join(values, " ", fn {value, kind} ->
      n(if selected == :both or selected == kind, do: value * multiplier, else: value)
    end)
  end

  defp adjustment(source) do
    case String.split(source) do
      ["none"] -> {:ok, {:none, :both}}
      [mode] when mode in ~w(stretch compress) -> {:ok, {direction(mode), :both}}
      [mode, "dashes"] when mode in ~w(stretch compress) -> {:ok, {direction(mode), 0}}
      [mode, "gaps"] when mode in ~w(stretch compress) -> {:ok, {direction(mode), 1}}
      _ -> :error
    end
  end

  defp direction(mode), do: if(mode == "stretch", do: :stretch, else: :compress)

  defp pattern(source) do
    if source == "none", do: {:ok, []}, else: marker_pattern(source)
  end

  defp marker_pattern(source) do
    tokens = HtmlValidator.split_css_tokens(source)

    Enum.reduce_while(tokens, {:ok, []}, fn token, {:ok, result} ->
      case SvgStyleValidator.parse("marker-mid", token) do
        {:ok, marker} ->
          {:cont, {:ok, [marker | result]}}

        :error ->
          case HtmlValidator.parse_css_number(token) do
            {value, "%"} when value >= 0 ->
              {:cont, {:ok, [{:percent, value / 100} | result]}}

            {value, unit} when unit in ["", "px"] and value >= 0 ->
              {:cont, {:ok, [{:gap, value} | result]}}

            _ ->
              {:halt, :error}
          end
      end
    end)
    |> case do
      {:ok, values} ->
        if Enum.any?(values, &match?({:fragment, _}, &1)),
          do: {:ok, Enum.reverse(values)},
          else: :error

      :error ->
        :error
    end
  end

  defp points(node) do
    case node do
      %{tag: "line", attributes: attrs} -> line_points(attrs)
      %{tag: "polyline", attributes: attrs} -> polyline_points(attrs)
      _ -> :error
    end
  end

  defp line_points(attrs) do
    with [{:ok, x1}, {:ok, y1}, {:ok, x2}, {:ok, y2}] <-
           Enum.map(~w(x1 y1 x2 y2), &length_value(Map.get(attrs, &1, "0"))) do
      {:ok, [{x1, y1}, {x2, y2}]}
    else
      _ -> :error
    end
  end

  defp polyline_points(attrs) do
    coords =
      attrs
      |> Map.get("points", "")
      |> String.split(~r/[\s,]+/, trim: true)
      |> Enum.map(&length_value/1)

    if Enum.all?(coords, &match?({:ok, _}, &1)) and rem(length(coords), 2) == 0 do
      {:ok, for([{:ok, x}, {:ok, y}] <- Enum.chunk_every(coords, 2), do: {x, y})}
    else
      :error
    end
  end

  defp segments(points) do
    points
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.flat_map(fn
      [same, same] ->
        []

      [{x1, y1} = from, {x2, y2} = to] ->
        [{from, to, :math.sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2)}]
    end)
  end

  defp point_on({x1, y1}, {x2, y2}, ratio),
    do: {x1 + (x2 - x1) * ratio, y1 + (y2 - y1) * ratio}

  defp length_value(source) do
    case HtmlValidator.parse_css_number(source) do
      {value, unit} when unit in ["", "px"] -> {:ok, value}
      _ -> :error
    end
  end

  defp gap(source) do
    case length_value(source) do
      {:ok, value} when value >= 0 -> {:ok, value}
      _ -> :error
    end
  end

  defp group(node, children) do
    node = Map.put(node, :xml_name, "g")

    %{
      node
      | tag: "g",
        children: children,
        attributes:
          Map.drop(node.attributes, @path_geometry ++ ~w(d marker-start marker-mid marker-end))
    }
  end

  defp child(node, attrs), do: %{node | attributes: Map.drop(attrs, @shape_effects)}

  defp shape(node, tag, attrs),
    do: %{node | tag: tag, attributes: attrs, children: []} |> Map.put(:xml_name, tag)

  defp n(value), do: :erlang.float_to_binary(value, [{:decimals, 10}, :compact])

  defp unsupported(source) do
    Diagnostics.error(
      :style,
      :invalid_document,
      "SVG print geometry requires the supported straight-segment or unrounded rectangular form",
      source: source
    )
  end
end
