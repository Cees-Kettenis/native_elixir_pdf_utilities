defmodule NativeElixirPdfUtilities.HtmlToPdf.ProjectionScene do
  @moduledoc "Bounded static CSS scene flattening with perspective-correct paint and depth."
  alias NativeElixirPdfUtilities.HtmlToPdf.{
    ProjectionGeometry,
    CssEffectScene,
    DrawingGeometry,
    Compositing,
    ImageMapGeometry
  }

  alias NativeElixirPdfUtilities.Validators.{
    CssProjectionValidator,
    CssEffectsValidator,
    WriterValidator,
    ColorFontValidator,
    HtmlValidator,
    ClipGeometryValidator
  }

  @doc "Flattens one validated projective scene, retaining logical text and projected annotations."
  @spec prepare(map(), tuple(), keyword()) :: {:ok, map()} | {:error, term()}
  def prepare(box, size, opts) do
    with :ok <- CssProjectionValidator.validate_descriptor(box.css_projection),
         :ok <- CssEffectsValidator.validate_artwork([box], opts),
         prefix = CssEffectScene.common_scopes(box.boxes),
         {:ok, planes} <- group(box, ProjectionGeometry.identity(), size, opts, prefix),
         {:ok, paint} <- composite(planes, {0, 0, elem(size, 0), elem(size, 1)}, size, prefix),
         {:ok, logical} <-
           logical(
             [box],
             ProjectionGeometry.identity(),
             size,
             {0, 0, elem(size, 0), elem(size, 1)},
             false,
             ProjectionGeometry.identity(),
             nil
           ) do
      {:ok,
       %{box | transform: {1, 0, 0, 1, 0, 0}, boxes: paint ++ logical}
       |> Map.drop([:css_projection, :css_effect, :clip])}
    end
  rescue
    ArithmeticError ->
      CssProjectionValidator.invalid(
        "Projected scene arithmetic is not finite; reduce perspective extremes or intermediate transforms"
      )
  end

  defp group(box, parent, size, opts, prefix) do
    with {:ok, {own, child}} <- CssProjectionValidator.compose_group(parent, box, elem(size, 1)) do
      projection =
        Map.get(box, :css_projection, %{
          preserve: false,
          backface: :visible,
          perspective: ProjectionGeometry.identity()
        })

      hidden = projection.backface == :hidden and ProjectionGeometry.backface?(own)

      child_size =
        if Map.get(box, :role) in [:css_transform_plane, :overflow_clip],
          do: size,
          else: {box.width, box.height}

      cond do
        hidden ->
          {:ok, []}

        projection.preserve ->
          collect(box.boxes, child, child_size, opts, prefix)

        not CssProjectionValidator.scoped_boxes?(box.boxes) ->
          source = box |> Map.drop([:css_projection]) |> Map.put(:transform, {1, 0, 0, 1, 0, 0})

          with {:ok, [%{boxes: [source]}]} <-
                 CssEffectScene.prepare([%{size: child_size, boxes: [source]}], opts),
               {:ok, plane} <- texture([source], own, child_size, prefix),
               do: {:ok, plane}

        true ->
          local_prefix = CssEffectScene.common_scopes(box.boxes)

          with {:ok, children} <-
                 collect(box.boxes, projection.perspective, child_size, opts, local_prefix, true),
               {:ok, extent} <- extent(children),
               {:ok, paint} <- composite(children, extent, child_size, local_prefix, false),
               source =
                 box
                 |> Map.drop([:css_projection])
                 |> Map.put(:transform, {1, 0, 0, 1, 0, 0})
                 |> Map.put(:boxes, paint),
               {:ok, [%{boxes: [source]}]} <-
                 CssEffectScene.prepare([%{size: child_size, boxes: [source]}], opts),
               {:ok, plane} <- texture([source], own, child_size, prefix),
               do: {:ok, plane}
      end
    end
  end

  # Contiguous source instructions share a texture; retained children remain separate depth planes.
  defp collect(boxes, matrix, size, opts, prefix, flatten_children \\ false) do
    boxes
    |> Enum.chunk_by(&(Map.get(&1, :type) == :drawing_group))
    |> Enum.reduce_while({:ok, []}, fn run, {:ok, acc} ->
      result =
        if hd(run).type == :drawing_group do
          Enum.reduce_while(run, {:ok, []}, fn box, {:ok, planes} ->
            with {:ok, added} <- group(box, matrix, size, opts, prefix),
                 {:ok, added} <-
                   if(
                     flatten_children and
                       Map.get(box, :css_projection, %{preserve: false}).preserve,
                     do:
                       with(
                         {:ok, extent} <- extent(added),
                         {:ok, paint} <- composite(added, extent, size, prefix),
                         do: texture(paint, ProjectionGeometry.identity(), size, prefix)
                       ),
                     else: {:ok, added}
                   ) do
              {:cont, {:ok, planes ++ added}}
            else
              error -> {:halt, error}
            end
          end)
        else
          texture(run, matrix, size, prefix)
        end

      case result do
        {:ok, planes} -> {:cont, {:ok, acc ++ planes}}
        error -> {:halt, error}
      end
    end)
  end

  defp texture(boxes, matrix, size, prefix) do
    with {:ok, context} <- WriterValidator.prepare_paint_plane(boxes, size),
         {:ok, context} <- ColorFontValidator.prepare(context, []),
         normalized = hd(context.pages).boxes,
         {:ok, rectangles} <- DrawingGeometry.placed_ink_rectangles(normalized) do
      rectangles = Enum.filter(rectangles, fn {l, b, r, t} -> r > l and t > b end)

      if rectangles == [] or ProjectionGeometry.inverse_plane(matrix) == :empty do
        {:ok, []}
      else
        {left, bottom, right, top} = union(rectangles)

        region = %{
          x: left - 0.75,
          y: bottom - 0.75,
          width: right - left + 1.5,
          height: top - bottom + 1.5
        }

        with {:ok, image} <-
               CssEffectScene.render(
                 normalized,
                 region,
                 [],
                 %{definitions: "", color: {0, 0, 0}, compositing_prefix: prefix},
                 size
               ) do
          scopes = CssEffectScene.common_scopes(normalized) |> Enum.drop(length(prefix))

          mode =
            case scopes do
              [] -> :Normal
              scopes -> List.last(scopes).mix_blend_mode
            end

          {:ok,
           [
             %{
               matrix: matrix,
               inverse: ProjectionGeometry.inverse_plane(matrix),
               source: image,
               mode: mode
             }
           ]}
        end
      end
    end
  end

  # A flat intermediate may overflow its source box. Bound it before allocating,
  # and reject an unbounded eye-plane crossing instead of choosing a magic extent.
  defp extent(planes) do
    Enum.reduce_while(planes, {:ok, []}, fn plane, {:ok, points} ->
      s = plane.source

      corners =
        for {x, y} <- [
              {s.x, s.y},
              {s.x + s.width, s.y},
              {s.x + s.width, s.y + s.height},
              {s.x, s.y + s.height}
            ],
            do: ProjectionGeometry.point(plane.matrix, x, y)

      if Enum.all?(corners, fn {_, _, _, w} -> w > 0 end),
        do: {:cont, {:ok, points ++ Enum.map(corners, fn {x, y, _, w} -> {x / w, y / w} end)}},
        else:
          {:halt,
           CssProjectionValidator.invalid(
             "An intermediate flattened scene crosses the eye plane; move the perspective camera or flatten at the physical page boundary"
           )}
    end)
    |> case do
      {:ok, []} -> {:ok, {0, 0, 0, 0}}
      {:ok, points} -> {:ok, ImageMapGeometry.bounds([points])}
      error -> error
    end
  end

  defp composite(planes, viewport, _size, prefix, depth_order \\ true) do
    planes =
      Enum.flat_map(planes, fn plane ->
        polygon = ProjectionGeometry.polygon(plane.matrix, plane.source, viewport)
        if length(polygon) < 3, do: [], else: [Map.put(plane, :polygon, polygon)]
      end)

    if planes == [] do
      {:ok, []}
    else
      {l, b, r, t} = ImageMapGeometry.bounds(Enum.map(planes, & &1.polygon))

      region = %{
        x: floor(l / 0.75) * 0.75,
        y: floor(b / 0.75) * 0.75,
        width: (ceil(r / 0.75) - floor(l / 0.75)) * 0.75,
        height: (ceil(t / 0.75) - floor(b / 0.75)) * 0.75
      }

      with {:ok, {width, height}} <- CssProjectionValidator.canvas(region, length(planes)) do
        pixels =
          for row <- 0..(height - 1), col <- 0..(width - 1), into: <<>> do
            x = region.x + (col + 0.5) * region.width / width
            y = region.y + region.height - (row + 0.5) * region.height / height

            samples =
              Enum.flat_map(planes, fn plane ->
                case ProjectionGeometry.source_point(plane.matrix, plane.inverse, x, y) do
                  :hidden ->
                    []

                  {u, v, z} ->
                    rgba = sample(plane.source, u, v)
                    if elem(rgba, 3) == 0, do: [], else: [{z, rgba, plane.mode}]
                end
              end)

            {red, green, blue, alpha} =
              samples
              |> then(fn samples ->
                if depth_order, do: Enum.sort_by(samples, &elem(&1, 0)), else: samples
              end)
              |> Enum.reduce({0.0, 0.0, 0.0, 0.0}, fn {_, rgba, mode}, acc ->
                Compositing.over(rgba, acc, mode)
              end)

            <<round(red * 255), round(green * 255), round(blue * 255), round(alpha * 255)>>
          end

        rgb = for <<r, g, b, _ <- pixels>>, into: <<>>, do: <<r, g, b>>
        alpha = for <<_, _, _, a <- pixels>>, into: <<>>, do: <<a>>

        image = %{
          format: :png,
          width_px: width,
          height_px: height,
          width: width * 0.75,
          height: height * 0.75,
          bits_per_component: 8,
          color_space: :device_rgb,
          data: rgb,
          alpha_data: alpha,
          interpolate: true
        }

        {:ok,
         [
           Map.merge(region, %{
             type: HtmlValidator.reserve_layout_box(:image),
             image: image,
             css_source_positioned: true,
             css_compositing_scopes: prefix
           })
         ]}
      end
    end
  end

  @doc "Bilinearly samples one admitted source texture in premultiplied alpha."
  @spec sample(map(), number(), number()) :: tuple()
  def sample(source, x, y) do
    image = source.image
    u = (x - source.x) * image.width_px / source.width - 0.5
    v = (source.y + source.height - y) * image.height_px / source.height - 0.5
    left = floor(u)
    top = floor(v)

    rgba =
      for {col, wx} <- [{left, 1 - u + left}, {left + 1, u - left}],
          {row, wy} <- [{top, 1 - v + top}, {top + 1, v - top}],
          reduce: {0.0, 0.0, 0.0, 0.0} do
        {r, g, b, a} ->
          if col >= 0 and row >= 0 and col < image.width_px and row < image.height_px do
            index = row * image.width_px + col

            alpha =
              case Map.get(image, :alpha_data) do
                nil -> 1.0
                mask -> :binary.at(mask, index) / 255
              end

            weight = wx * wy * alpha

            {r + :binary.at(image.data, index * 3) / 255 * weight,
             g + :binary.at(image.data, index * 3 + 1) / 255 * weight,
             b + :binary.at(image.data, index * 3 + 2) / 255 * weight, a + weight}
          else
            {r, g, b, a}
          end
      end

    case rgba do
      {_, _, _, alpha} when alpha == 0 ->
        {0.0, 0.0, 0.0, 0.0}

      {r, g, b, a} ->
        {min(1.0, max(0.0, r / a)), min(1.0, max(0.0, g / a)), min(1.0, max(0.0, b / a)),
         min(1.0, max(0.0, a))}
    end
  end

  defp logical(boxes, matrix, size, viewport, hidden, orientation, clip) do
    Enum.reduce_while(boxes, {:ok, []}, fn box, {:ok, acc} ->
      result =
        case box do
          %{type: :drawing_group, boxes: children} ->
            {own, child} = ProjectionGeometry.group_matrices(matrix, box, elem(size, 1))

            {facing, oriented_child} =
              ProjectionGeometry.group_matrices(orientation, box, elem(size, 1))

            projection =
              Map.get(box, :css_projection, %{
                backface: :visible,
                preserve: false,
                perspective: ProjectionGeometry.identity()
              })

            back =
              Map.get(box, :css_projection, %{backface: :visible}).backface == :hidden and
                ProjectionGeometry.backface?(facing)

            orientation = if projection.preserve, do: oriented_child, else: projection.perspective
            local_clip = projected_clip(DrawingGeometry.local_clip(box), own, viewport)
            child_clip = {:intersection, Enum.reject([clip, local_clip], &is_nil/1)}
            logical(children, child, size, viewport, hidden or back, orientation, child_clip)

          %{type: type} when type in [:actual_text_start, :actual_text_end] ->
            {:ok, if(hidden, do: [], else: [box])}

          _ ->
            logical_leaf(box, matrix, size, viewport, hidden, clip)
        end

      case result do
        {:ok, added} -> {:cont, {:ok, acc ++ added}}
        error -> {:halt, error}
      end
    end)
  end

  defp logical_leaf(box, matrix, size, viewport, hidden, clip) do
    if hidden do
      {:ok, []}
    else
      with {:ok, context} <- WriterValidator.prepare_paint_plane([box], size) do
        normalized =
          if ProjectionGeometry.inverse_plane(matrix) == :empty,
            do: [],
            else: hd(context.pages).boxes

        Enum.reduce_while(normalized, {:ok, []}, fn source, {:ok, acc} ->
          links =
            if Map.has_key?(source, :link_url) or Map.has_key?(source, :link_destination) do
              source_polygons =
                case Map.get(source, :drawing_link_quad_points) do
                  points when is_list(points) ->
                    points
                    |> Enum.chunk_every(8)
                    |> Enum.map(fn [x1, y1, x2, y2, x3, y3, x4, y4] ->
                      [{x1, y1}, {x2, y2}, {x4, y4}, {x3, y3}]
                    end)

                  _ ->
                    case Map.get(source, :link_quads) do
                      polygons when is_list(polygons) ->
                        polygons

                      _ ->
                        {x, y} =
                          if source.type == :text,
                            do: DrawingGeometry.text_position(source, elem(size, 1)),
                            else: {source.x, source.y}

                        height =
                          if source.type == :text,
                            do: Map.get(source, :line_height, source.font_size * 1.2),
                            else: source.height

                        width = Map.get(source, :annotation_width, source.width)
                        [[{x, y}, {x + width, y}, {x + width, y + height}, {x, y + height}]]
                    end
                end

              polygons =
                Enum.map(
                  source_polygons,
                  &ProjectionGeometry.project_polygon(matrix, &1, viewport)
                )
                |> Enum.filter(&(length(&1) >= 3))

              if polygons == [] do
                {:ok, []}
              else
                {l, b, r, t} = viewport

                edges =
                  Enum.flat_map(polygons, fn polygon ->
                    NativeElixirPdfUtilities.HtmlToPdf.ClipGeometry.polygon(polygon)
                    |> NativeElixirPdfUtilities.HtmlToPdf.ClipGeometry.edges()
                  end)

                source_clip = projected_clip(Map.get(source, :clip), matrix, viewport)

                expression =
                  {:intersection, Enum.reject([{:contour, edges}, clip, source_clip], &is_nil/1)}

                with {:ok, quads} <-
                       ClipGeometryValidator.project(
                         expression,
                         [{l, b}, {r, b}, {r, t}, {l, t}],
                         viewport
                       ) do
                  links =
                    Enum.map(quads, fn [x1, y1, x2, y2, x3, y3, x4, y4] ->
                      points = [x1, y1, x2, y2, x3, y3, x4, y4]

                      {left, bottom, right, top} =
                        ImageMapGeometry.bounds([[{x1, y1}, {x2, y2}, {x3, y3}, {x4, y4}]])

                      Map.merge(Map.take(source, [:link_url, :link_destination]), %{
                        type: :link_region,
                        x: left,
                        y: bottom,
                        width: right - left,
                        height: top - bottom,
                        link_quad_points: points
                      })
                    end)

                  {:ok, links}
                end
              end
            else
              {:ok, []}
            end

          case links do
            {:ok, links} ->
              text =
                cond do
                  source.type in [:actual_text_start, :actual_text_end] ->
                    [source]

                  source.type == :text ->
                    {px, py, _, w} = ProjectionGeometry.point(matrix, source.x, source.y)

                    if w > 0 do
                      # The local Jacobian retains searchable glyph positions on the projected plane.
                      {xx, xy, _, wx} = ProjectionGeometry.point(matrix, source.x + 1, source.y)
                      {yx, yy, _, wy} = ProjectionGeometry.point(matrix, source.x, source.y + 1)

                      if wx > 0 and wy > 0 do
                        a = xx / wx - px / w
                        b = xy / wx - py / w
                        c = yx / wy - px / w
                        d = yy / wy - py / w

                        affine =
                          {a, b, c, d, px / w - a * source.x - c * source.y,
                           py / w - b * source.x - d * source.y}

                        invisible =
                          source
                          |> Map.drop([
                            :link_url,
                            :link_destination,
                            :drawing_annotation_rect,
                            :drawing_link_quad_points,
                            :drawing_page_height
                          ])
                          |> Map.put(:color, {0.0, 0.0, 0.0, 0.0})

                        [
                          %{
                            type: :drawing_group,
                            x: 0.0,
                            y: 0.0,
                            width: elem(size, 0),
                            height: elem(size, 1),
                            transform: affine,
                            boxes: [invisible]
                          }
                        ]
                      else
                        []
                      end
                    else
                      []
                    end

                  true ->
                    []
                end

              {:cont, {:ok, acc ++ links ++ text}}

            error ->
              {:halt, error}
          end
        end)
        |> case do
          {:ok, additions} ->
            # A fully hidden text run must not leave extractable ActualText alone.
            if Enum.any?(additions, &(&1.type == :drawing_group)),
              do: {:ok, additions},
              else:
                {:ok,
                 Enum.reject(additions, &(&1.type in [:actual_text_start, :actual_text_end]))}

          error ->
            error
        end
      end
    end
  end

  defp projected_clip(clip, matrix, viewport) do
    case DrawingGeometry.box_clip_expression(clip, {1, 0, 0, 1, 0, 0}) do
      nil -> nil
      expression -> projected_expression(expression, matrix, viewport)
    end
  end

  defp projected_expression(expression, matrix, viewport) do
    case expression do
      {:intersection, children} ->
        {:intersection, Enum.map(children, &projected_expression(&1, matrix, viewport))}

      {kind, edges} when kind in [:contour, :evenodd] ->
        points = Enum.map(edges, &elem(&1, 0))
        polygon = ProjectionGeometry.project_polygon(matrix, points, viewport)

        if length(polygon) < 3 do
          {kind, []}
        else
          {:contour, projected} = NativeElixirPdfUtilities.HtmlToPdf.ClipGeometry.polygon(polygon)
          {kind, projected}
        end
    end
  end

  defp union([first | rest]),
    do:
      Enum.reduce(rest, first, fn {l, b, r, t}, {a, c, d, e} ->
        {min(l, a), min(b, c), max(r, d), max(t, e)}
      end)
end
