defmodule NativeElixirPdfUtilities.Validators.CssMotionValidator do
  @moduledoc "Static offset properties using shared CSS positions and canonical path geometry."
  alias NativeElixirPdfUtilities.HtmlToPdf.{PathGeometry, AffineGeometry}

  alias NativeElixirPdfUtilities.Validators.{
    CssClipValidator,
    CssMathValidator,
    CssPositionValidator,
    CssSyntaxValidator,
    CssValueValidator,
    HtmlValidator
  }

  @defaults %{
    css_offset_path: :none,
    css_offset_position: :normal,
    css_offset_anchor: :auto,
    css_offset_distance: 0.0,
    css_offset_rotate: {true, 0.0}
  }
  @fields %{
    "offset-path" => :css_offset_path,
    "offset-position" => :css_offset_position,
    "offset-anchor" => :css_offset_anchor,
    "offset-distance" => :css_offset_distance,
    "offset-rotate" => :css_offset_rotate
  }

  @doc "Returns initial static offset values."
  @spec defaults() :: map()
  def defaults, do: @defaults

  @doc "Identifies an offset path establishing a transform scope."
  @spec active?(map()) :: boolean()
  def active?(style) do
    case style do
      %{css_offset_path: path} -> path != :none
      _ -> false
    end
  end

  @doc "Prepares one static offset longhand using shared numeric and shape validators."
  @spec prepare(map(), binary(), binary()) :: {:ok, map()} | :error | {:error, term()}
  def prepare(style, property, source) do
    source = String.trim(source)

    result =
      case property do
        "offset" ->
          shorthand(style, source)

        "offset-path" ->
          components =
            CssSyntaxValidator.components(source) |> Enum.reject(&(String.trim(&1.text) == ""))

          case components do
            [%{kind: :block, opener: "ray(", closed: true, children: children}] ->
              ray(Enum.map_join(children, & &1.text), style)

            _ ->
              with {:ok, clip} <- CssClipValidator.prepare(style, source) do
                shape = clip.css_clip_path

                shape =
                  if is_map(shape) and shape.type in [:circle, :ellipse],
                    do:
                      Map.put(
                        shape,
                        :offset_implicit_position,
                        "at" not in HtmlValidator.split_css_tokens(
                          source
                          |> String.replace("(", " ")
                          |> String.replace(")", " ")
                        )
                      ),
                    else: shape

                {:ok, shape}
              end
          end

        "offset-distance" ->
          CssPositionValidator.prepare_math(source)

        property when property in ["offset-position", "offset-anchor"] ->
          if source in if(property == "offset-position", do: ["normal", "auto"], else: ["auto"]),
            do: {:ok, String.to_existing_atom(source)},
            else:
              CssValueValidator.parse_two_axis_position(
                source,
                Map.get(style, :font_size, 12),
                false
              )

        "offset-rotate" ->
          tokens = HtmlValidator.split_css_tokens(source)

          {automatic, tokens} =
            case Enum.filter(tokens, &(&1 in ["auto", "reverse"])) do
              [] -> {false, tokens}
              [keyword] -> {keyword, Enum.reject(tokens, &(&1 == keyword))}
              _ -> {nil, []}
            end

          with true <- automatic != nil,
               {:ok, angle} <-
                 (case tokens do
                    [] when automatic != false -> {:ok, 0.0}
                    [angle] -> CssMathValidator.angle(angle)
                    _ -> :error
                  end) do
            {:ok,
             {automatic != false, angle + if(automatic == "reverse", do: :math.pi(), else: 0.0)}}
          else
            _ -> :error
          end
      end

    if property == "offset",
      do: result,
      else:
        with(
          {:ok, value} <- result,
          do: {:ok, Map.put(style, Map.fetch!(@fields, property), value)}
        )
  end

  # Keep the shorthand's path token opaque; its strings and commas belong to
  # the shared path/shape grammar, not the offset token assignment.
  defp shorthand(style, source) do
    parts = CssSyntaxValidator.split(source, "/")

    with [main | anchor] when length(anchor) <= 1 <- parts,
         tokens = HtmlValidator.split_css_tokens(main),
         {position, rest} =
           Enum.split_while(tokens, fn token ->
             token != "none" and token not in ~w(border-box padding-box content-box margin-box) and
               not match?({:ok, _, "(" <> _}, HtmlValidator.parse_css_identifier(token))
           end),
         {:ok, used} <-
           if(position == [],
             do: {:ok, Map.merge(style, @defaults)},
             else:
               prepare(Map.merge(style, @defaults), "offset-position", Enum.join(position, " "))
           ),
         {:ok, used} <-
           (case rest do
              [] ->
                {:ok, used}

              [path | tail] ->
                {boxes, tail} =
                  Enum.split_while(
                    tail,
                    &(&1 in ~w(border-box padding-box content-box margin-box))
                  )

                with {:ok, used} <- prepare(used, "offset-path", Enum.join([path | boxes], " ")) do
                  {distance, rotation} =
                    Enum.split_with(
                      tail,
                      &(match?({:ok, _}, CssPositionValidator.prepare_math(&1)) and
                          not match?({:ok, _}, CssMathValidator.angle(&1)))
                    )

                  with true <- length(distance) <= 1,
                       {:ok, used} <-
                         if(distance == [],
                           do: {:ok, used},
                           else: prepare(used, "offset-distance", hd(distance))
                         ),
                       do:
                         if(rotation == [],
                           do: {:ok, used},
                           else: prepare(used, "offset-rotate", Enum.join(rotation, " "))
                         ),
                       else: (_ -> :error)
                end
            end),
         {:ok, used} <-
           if(anchor == [], do: {:ok, used}, else: prepare(used, "offset-anchor", hd(anchor))) do
      {:ok, used}
    else
      _ -> :error
    end
  end

  defp ray(source, style) do
    tokens = HtmlValidator.split_css_tokens(source)
    {tokens, position_tokens} = Enum.split_while(tokens, &(&1 != "at"))

    position =
      case position_tokens do
        [] -> nil
        ["at" | values] -> Enum.join(values, " ")
      end

    sizes =
      Enum.filter(
        tokens,
        &(&1 in ~w(closest-side farthest-side closest-corner farthest-corner sides))
      )

    angles = Enum.reject(tokens, &(&1 in ["contain" | sizes]))

    with true <- length(sizes) <= 1 and Enum.count(tokens, &(&1 == "contain")) <= 1,
         [angle] <- angles,
         {:ok, angle} <- CssMathValidator.angle(angle),
         {:ok, position} <-
           if(position == nil,
             do: {:ok, nil},
             else:
               CssValueValidator.parse_two_axis_position(
                 position,
                 Map.get(style, :font_size, 12),
                 false
               )
           ) do
      {:ok,
       %{
         type: :ray,
         angle: angle,
         size: List.first(sizes) || "closest-side",
         contain: "contain" in tokens,
         position: position
       }}
    else
      _ -> :error
    end
  end

  @doc "Freezes declaring font units while retaining deferred path and box percentages."
  @spec compute_style(map()) :: {:ok, map()} | :error | {:error, term()}
  def compute_style(style) do
    case style do
      style
      when not is_map_key(style, :css_offset_position) and
             not is_map_key(style, :css_offset_anchor) and
             not is_map_key(style, :css_offset_distance) ->
        {:ok, style}

      _ ->
        Enum.reduce_while(
          [:css_offset_position, :css_offset_anchor, :css_offset_distance],
          {:ok, style},
          fn key, {:ok, style} ->
            case Map.fetch(style, key) do
              {:ok, value} ->
                case CssPositionValidator.compute_position(value, style) do
                  {:ok, value} -> {:cont, {:ok, Map.put(style, key, value)}}
                  error -> {:halt, error}
                end

              :error ->
                {:cont, {:ok, style}}
            end
          end
        )
    end
  end

  @doc "Validates computed static offset facts at the styled-tree boundary."
  @spec valid_style?(map()) :: boolean()
  def valid_style?(style) do
    case style do
      style
      when is_map(style) and not is_map_key(style, :css_offset_path) and
             not is_map_key(style, :css_offset_distance) and
             not is_map_key(style, :css_offset_position) and
             not is_map_key(style, :css_offset_anchor) and
             not is_map_key(style, :css_offset_rotate) ->
        true

      _ ->
        if Enum.all?(@defaults, fn {key, value} -> Map.get(style, key, value) == value end) do
          true
        else
          path = Map.get(style, :css_offset_path, :none)

          valid_path =
            case path do
              %{type: :ray, angle: angle, size: size, contain: contain, position: position} ->
                is_number(angle) and
                  abs(angle) <= NativeElixirPdfUtilities.Limits.get(:max_numeric_magnitude) and
                  size in ~w(closest-side farthest-side closest-corner farthest-corner sides) and
                  is_boolean(contain) and valid_position?(position, [nil])

              _ ->
                CssClipValidator.valid_style?(%{css_clip_path: path})
            end

          valid_path and
            valid_position?(Map.get(style, :css_offset_position, :normal), [:normal, :auto]) and
            valid_position?(Map.get(style, :css_offset_anchor, :auto), [:auto]) and
            CssPositionValidator.valid_prepared?(
              Map.get(style, :css_offset_distance, @defaults.css_offset_distance)
            ) and
            case Map.get(style, :css_offset_rotate, {true, 0.0}) do
              {auto, angle} ->
                is_boolean(auto) and is_number(angle) and
                  abs(angle) <= NativeElixirPdfUtilities.Limits.get(:max_numeric_magnitude)

              _ ->
                false
            end
        end
    end
  end

  defp valid_position?(value, keywords) do
    value in keywords or
      case value do
        {x, y} ->
          CssPositionValidator.valid_prepared?(x) and CssPositionValidator.valid_prepared?(y)

        _ ->
          false
      end
  end

  @doc "Resolves a motion matrix in native page coordinates around the selected element reference box."
  @spec resolve(map(), map()) :: {:ok, tuple()} | :error | {:error, term()}
  def resolve(style, bounds) do
    if active?(style) do
      reference =
        Map.get(style, :_offset_reference, %{
          x: 0.0,
          top: bounds.top,
          width: Map.get(style, :_position_viewport, %{width: bounds.width}).width,
          height: Map.get(style, :_containing_height, bounds.height) || bounds.height
        })

      box_style = Map.merge(style, Map.get(reference, :offset_box_style, %{}))

      position_box =
        CssClipValidator.reference_box(
          reference,
          box_style,
          Map.get(reference, :offset_position_box, :border_box)
        )

      start = Map.get(style, :css_offset_position, :normal)

      with {:ok, start} <-
             (case start do
                :normal ->
                  {:ok, nil}

                :auto ->
                  {:ok, {bounds.x, bounds.top}}

                {x, y} ->
                  with {:ok, x} <- CssPositionValidator.resolve(x, position_box.width, style),
                       {:ok, y} <- CssPositionValidator.resolve(y, position_box.height, style),
                       do: {:ok, {position_box.x + x, position_box.top - y}}
              end),
           {:ok, commands, closed} <- path(box_style, reference, bounds, start),
           measured = PathGeometry.sample(commands, 0, closed),
           {:ok, distance} <-
             CssPositionValidator.resolve(
               Map.get(style, :css_offset_distance, @defaults.css_offset_distance),
               measured.length,
               style
             ),
           distance =
             (case style.css_offset_path do
                %{type: :ray, contain: false} -> distance
                _ -> if(closed, do: distance, else: min(measured.length, max(0.0, distance)))
              end),
           sample = PathGeometry.sample(commands, distance, closed),
           origin = Map.get(style, :transform_origin, {{:percent, 0.5}, {:percent, 0.5}}),
           anchor = Map.get(style, :css_offset_anchor, :auto),
           anchor = if(anchor == :auto, do: {elem(origin, 0), elem(origin, 1)}, else: anchor),
           {:ok, ax} <- CssPositionValidator.resolve(elem(anchor, 0), bounds.width, style),
           {:ok, ay} <- CssPositionValidator.resolve(elem(anchor, 1), bounds.height, style) do
        {auto, extra} = Map.get(style, :css_offset_rotate, {true, 0.0})
        angle = if(auto, do: sample.angle, else: 0.0) - extra
        {px, py} = sample.point
        c = :math.cos(angle)
        s = :math.sin(angle)
        ax = bounds.x + ax
        ay = bounds.top - ay
        {:ok, {c, s, -s, c, px - c * ax + s * ay, py - s * ax - c * ay}}
      end
    else
      {:ok, {1.0, 0.0, 0.0, 1.0, 0.0, 0.0}}
    end
  rescue
    ArithmeticError ->
      NativeElixirPdfUtilities.Validators.CssProjectionValidator.invalid(
        "Offset path arithmetic exceeds finite geometry; reduce path coordinates or distance"
      )
  end

  defp path(style, reference, bounds, start) do
    case style.css_offset_path do
      %{type: :ray} = ray ->
        with {:ok, point} <-
               if(ray.position == nil,
                 do:
                   {:ok,
                    start ||
                      {reference.x + reference.width / 2, reference.top - reference.height / 2}},
                 else:
                   with(
                     {:ok, x} <-
                       CssPositionValidator.resolve(elem(ray.position, 0), reference.width, style),
                     {:ok, y} <-
                       CssPositionValidator.resolve(
                         elem(ray.position, 1),
                         reference.height,
                         style
                       ),
                     do: {:ok, {reference.x + x, reference.top - y}}
                   )
               ) do
          {x, y} = point

          sides = [
            abs(x - reference.x),
            abs(reference.x + reference.width - x),
            abs(y - reference.top),
            abs(reference.top - reference.height - y)
          ]

          corners =
            for cx <- [reference.x, reference.x + reference.width],
                cy <- [reference.top, reference.top - reference.height],
                do: :math.sqrt((x - cx) * (x - cx) + (y - cy) * (y - cy))

          dx = :math.sin(ray.angle)
          dy = :math.cos(ray.angle)

          length =
            case ray.size do
              "closest-side" ->
                Enum.min(sides)

              "farthest-side" ->
                Enum.max(sides)

              "closest-corner" ->
                Enum.min(corners)

              "farthest-corner" ->
                Enum.max(corners)

              "sides" ->
                if x <= reference.x or x >= reference.x + reference.width or y >= reference.top or
                     y <= reference.top - reference.height do
                  0.0
                else
                  Enum.flat_map(
                    [
                      {dx, reference.x - x, reference.x + reference.width - x},
                      {dy, reference.top - reference.height - y, reference.top - y}
                    ],
                    fn {d, a, b} ->
                      if d == 0, do: [], else: Enum.filter([a / d, b / d], &(&1 >= 0))
                    end
                  )
                  |> Enum.min()
                end
            end

          length =
            if ray.contain,
              do: max(0, length - max(bounds.width, bounds.height) / 2),
              else: length

          {:ok, [{0, [x, y]}, {1, [x + dx * length, y + dy * length]}], false}
        end

      %{type: type} = shape when type in [:circle, :ellipse] ->
        reference = CssClipValidator.reference_box(reference, style, shape.reference)

        shape =
          if start != nil and Map.get(shape, :offset_implicit_position, false) do
            {x, y} = start
            Map.put(shape, :position, {x - reference.x, reference.top - y})
          else
            shape
          end

        {:ellipse, cx, cy, rx, ry} =
          NativeElixirPdfUtilities.HtmlToPdf.FlowShape.geometry(shape, reference, style)

        commands =
          PathGeometry.commands([
            {?M, [cx + rx, cy]},
            {?A, [rx, ry, 0, 0, 0, cx - rx, cy]},
            {?A, [rx, ry, 0, 0, 0, cx + rx, cy]},
            {?Z, []}
          ])

        {:ok, commands, true}

      %{type: type} = shape when type in [:box, :inset] ->
        reference = CssClipValidator.reference_box(reference, style, shape.reference)

        {:polygon, points, _} =
          NativeElixirPdfUtilities.HtmlToPdf.FlowShape.geometry(shape, reference, style)

        highest = points |> Enum.map(&elem(&1, 1)) |> Enum.max()

        {_, index} =
          points
          |> Enum.with_index()
          |> Enum.filter(fn {{_, y}, _} -> y == highest end)
          |> Enum.min_by(fn {{x, _}, _} -> x end)

        {first, last} = Enum.split(points, index)
        [{x, y} | rest] = last ++ first
        {:ok, [{0, [x, y]}] ++ Enum.map(rest, fn {x, y} -> {1, [x, y]} end) ++ [{4, []}], true}

      shape ->
        with {:ok, clip} <-
               CssClipValidator.resolve(Map.put(style, :css_clip_path, shape), reference) do
          commands = clip.path_commands

          commands =
            case shape do
              %{type: :path} ->
                {x, y} = start || {reference.x, reference.top}
                PathGeometry.transform(commands, {1, 0, 0, 1, x - reference.x, y - reference.top})

              _ ->
                commands
            end

          {:ok, commands, match?({4, []}, List.last(commands))}
        end
    end
  end

  @doc "Inserts the offset transform between individual properties and the transform list."
  @spec operations(map(), map(), list(), non_neg_integer()) ::
          {:ok, list()} | :error | {:error, term()}
  def operations(style, bounds, operations, count) do
    if active?(style) do
      origin = Map.get(style, :transform_origin, {{:percent, 0.5}, {:percent, 0.5}})

      with {:ok, matrix} <- resolve(style, bounds),
           {:ok, x} <- CssPositionValidator.resolve(elem(origin, 0), bounds.width, style),
           {:ok, y} <- CssPositionValidator.resolve(elem(origin, 1), bounds.height, style) do
        x = bounds.x + x
        y = bounds.top - y

        local =
          AffineGeometry.multiply(
            {1, 0, 0, 1, -x, -y},
            AffineGeometry.multiply(matrix, {1, 0, 0, 1, x, y})
          )

        {a, b, c, d, e, f} = local
        {before, after_ops} = Enum.split(operations, count)
        {:ok, before ++ [{"matrix", [a, -b, -c, d, e / 0.75, -f / 0.75]}] ++ after_ops}
      end
    else
      {:ok, operations}
    end
  end
end
