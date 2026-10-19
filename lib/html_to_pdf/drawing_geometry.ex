defmodule NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry do
  @moduledoc """
  Shared drawing coordinates for validated writer boxes and logical annotations.

  Pixel snapping happens in the local viewport before applying drawing-group
  transforms. These calculations consume validator-prepared geometry.
  """

  # CSS fixes one pixel at 0.75 PDF points. The raster bias is one unit of the
  # writer's fixed four-decimal PDF precision, not a configurable resource limit.
  @css_pixel_points 0.75
  @raster_boundary_bias 0.0001

  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry

  @doc "Translates validated native boxes, including glyph-group children in the parent coordinate space."
  @spec translate_boxes([map()], number(), number()) :: [map()]
  def translate_boxes(boxes, delta_x, delta_y) do
    Enum.map(boxes, fn box ->
      box =
        case box do
          %{type: :glyph_group, boxes: children} ->
            %{box | boxes: translate_boxes(children, delta_x, delta_y)}

          _ ->
            box
        end

      translate_box(box, delta_x, delta_y)
    end)
  end

  @doc "Translates validated artwork and annotations while keeping drawing-group children and clips local."
  @spec translate_box(map(), number(), number()) :: map()
  def translate_box(box, delta_x, delta_y) do
    box =
      case box do
        %{type: :drawing_group, role: :overflow_clip, boxes: children} ->
          %{box | boxes: translate_boxes(children, delta_x, delta_y)}

        %{type: :drawing_group, transform: {a, b, c, d, e, f}} ->
          %{box | transform: {a, b, c, d, e + delta_x, f + delta_y}}

        _ ->
          box
      end

    box =
      Enum.reduce([{:x, delta_x}, {:y, delta_y}], box, fn {key, delta}, shifted ->
        case Map.get(shifted, key) do
          value when is_number(value) -> Map.put(shifted, key, value + delta)
          _ -> shifted
        end
      end)

    box =
      case Map.get(box, :clip) do
        %{x: x, y: y} = clip when box.type != :drawing_group or box.role == :overflow_clip ->
          clip = %{clip | x: x + delta_x, y: y + delta_y}

          clip =
            case Map.get(clip, :inner_clip) do
              nil ->
                clip

              inner ->
                Map.put(clip, :inner_clip, %{inner | x: inner.x + delta_x, y: inner.y + delta_y})
            end

          Map.put(box, :clip, clip)

        _ ->
          box
      end

    box =
      case Map.get(box, :link_quads) do
        nil ->
          box

        quads ->
          Map.put(
            box,
            :link_quads,
            Enum.map(quads, fn quad ->
              Enum.map(quad, fn {x, y} -> {x + delta_x, y + delta_y} end)
            end)
          )
      end

    box =
      case Map.get(box, :link_quad_points) do
        points when is_list(points) ->
          points =
            points
            |> Enum.chunk_every(2)
            |> Enum.flat_map(fn [x, y] -> [x + delta_x, y + delta_y] end)

          Map.put(box, :link_quad_points, points)

        _ ->
          box
      end

    box =
      case Map.get(box, :css_transform_border_box) do
        nil ->
          box

        bounds ->
          Map.put(box, :css_transform_border_box, %{
            bounds
            | x: bounds.x + delta_x,
              top: bounds.top + delta_y
          })
      end

    box =
      case Map.get(box, :css_transform_scopes) do
        nil ->
          box

        scopes ->
          Map.put(
            box,
            :css_transform_scopes,
            Enum.map(scopes, fn scope ->
              bounds = %{
                scope.border_box
                | x: scope.border_box.x + delta_x,
                  top: scope.border_box.top + delta_y
              }

              matrix =
                case scope.matrix do
                  :empty ->
                    :empty

                  {a, b, c, d, e, f} ->
                    {a, b, c, d, e + delta_x - a * delta_x - c * delta_y,
                     f + delta_y - b * delta_x - d * delta_y}
                end

              %{scope | matrix: matrix, border_box: bounds}
            end)
          )
      end

    box =
      case Map.get(box, :inline_positioned_anchor) do
        nil ->
          box

        anchor ->
          Map.put(box, :inline_positioned_anchor, %{anchor | source_y: anchor.source_y + delta_y})
      end

    case Map.get(box, :link_rect) do
      %{x: x, y: y} = region ->
        Map.put(box, :link_rect, %{region | x: x + delta_x, y: y + delta_y})

      _ ->
        box
    end
  end

  @doc "Clips validated native content to an actual containing viewport, retaining local group transforms."
  @spec clip_boxes([map()], map()) :: [map()]
  def clip_boxes(boxes, clip) do
    Enum.flat_map(boxes, fn box ->
      case box do
        %{type: type} when type in [:drawing_group, :glyph_group] ->
          [overflow_group(box, clip)]

        _ ->
          {left, bottom, right, top} =
            intersect_clip(
              {clip.x, clip.y, clip.x + clip.width, clip.y + clip.height},
              box_clip_bounds(Map.get(box, :clip))
            )

          existing = Map.get(box, :clip)

          prepared = %{
            x: left,
            y: bottom,
            width: max(right - left, 0.0),
            height: max(top - bottom, 0.0)
          }

          rounded =
            case existing do
              %{inner_clip: %{border_radii: _} = inner} ->
                inner

              %{border_radii: _} ->
                Map.delete(existing, :inner_clip)

              %{inner_clip: %{border_radius: radius} = inner} when radius > 0 ->
                inner

              %{border_radius: radius} when radius > 0 ->
                Map.delete(existing, :inner_clip)

              _ ->
                if Map.get(clip, :border_radius, 0) > 0 or Map.has_key?(clip, :border_radii),
                  do: Map.delete(clip, :inner_clip),
                  else: nil
            end

          prepared =
            if rounded == nil, do: prepared, else: Map.put(prepared, :inner_clip, rounded)

          cond do
            prepared.width <= 0 or prepared.height <= 0 ->
              [overflow_group(box, Map.delete(prepared, :inner_clip))]

            is_map(existing) and
              (Map.get(existing, :border_radius, 0) > 0 or
                 Map.has_key?(existing, :border_radii) or Map.has_key?(existing, :inner_clip)) and
                (Map.get(clip, :border_radius, 0) > 0 or Map.has_key?(clip, :border_radii) or
                   Map.has_key?(clip, :inner_clip)) ->
              [overflow_group(box, clip)]

            true ->
              [Map.put(box, :clip, prepared)]
          end
      end
    end)
  end

  defp overflow_group(box, clip) do
    %{
      type: NativeElixirPdfUtilities.Validators.HtmlValidator.reserve_layout_box(:drawing_group),
      role: :overflow_clip,
      x: clip.x,
      y: clip.y,
      width: clip.width,
      height: clip.height,
      transform: {1, 0, 0, 1, 0, 0},
      clip: clip,
      boxes: [box]
    }
    |> Map.merge(
      Map.take(box, [
        :flow_id,
        :out_of_flow,
        :position_anchor,
        :break_inside,
        :css_transform_scopes
      ])
    )
  end

  @doc "Returns the HTML source print canvas for validated page or viewport geometry."
  @spec css_print_canvas(number()) :: %{
          point_scale: float(),
          css_scale: float(),
          css_pixels_per_point: float(),
          height: number()
        }
  def css_print_canvas(height) do
    # The recorded source printer uses a 300-dpi device canvas and a float32
    # device-to-point matrix. These are paint-unit conversions, not resource
    # ceilings. Retaining the source matrix preserves selected-font and image
    # sampling through the same coordinate frame without moving source boxes.
    <<point_scale::float-size(32)>> = <<72.0 / 300::float-size(32)>>

    %{
      point_scale: point_scale,
      css_scale: 300 / 96,
      css_pixels_per_point: 1 / @css_pixel_points,
      height: height
    }
  end

  @doc false
  @spec text_position(map(), number()) :: {number(), number()}
  def text_position(box, height) do
    case Map.get(box, :snap_to_css_pixel_grid, false) do
      true ->
        baseline =
          case box do
            %{baseline_snap_depth: depth} -> text_baseline(box.y, height, depth)
            _ -> height - snap_css_pixel(height - box.y)
          end

        case Map.get(box, :font_face) do
          %{type: :embedded} -> {box.x, baseline}
          _ -> {box.x, baseline + @raster_boundary_bias}
        end

      false ->
        {box.x, box.y}
    end
  end

  @doc false
  @spec text_baseline(number(), number(), number()) :: number()
  def text_baseline(y, height, depth) do
    if depth == 0 do
      height - snap_css_pixel(height - y)
    else
      height - snap_css_pixel(height - y - depth) -
        Float.floor(depth / @css_pixel_points * 64) / 64 * @css_pixel_points
    end
  end

  @doc "Returns the effective local clip, including the existing zero-area group rule."
  @spec local_clip(map(), number() | nil) :: map() | nil
  def local_clip(group, parent_height \\ nil) do
    cond do
      group.width == 0 or group.height == 0 ->
        %{x: 0, y: 0, width: 0, height: 0}

      Map.get(group, :role) == :overflow_clip and is_number(parent_height) ->
        clip = group.clip

        {x, y} = rounded_viewport_origin(clip.x, clip.y, clip.height, parent_height)

        # A CSS scrollport clip retains the source print matrix's edge ties.
        {x, y} =
          if Map.get(group, :css_scrollport, false),
            do: {x - @raster_boundary_bias, y + @raster_boundary_bias},
            else: {x, y}

        shifted = %{clip | x: x, y: y}

        case Map.get(clip, :inner_clip) do
          nil ->
            shifted

          inner ->
            Map.put(shifted, :inner_clip, %{
              inner
              | x: inner.x + x - clip.x,
                y: inner.y + y - clip.y
            })
        end

      true ->
        Map.get(group, :clip)
    end
  end

  @doc false
  @spec snap_css_pixel(number()) :: float()
  def snap_css_pixel(value) do
    PageGeometry.snap_css_pixel(value)
  end

  @doc "Returns the intersection of a validated box's outer and optional inner clip rectangles."
  @spec box_clip_bounds(map() | nil) :: tuple() | nil
  def box_clip_bounds(clip) do
    case clip do
      nil ->
        nil

      %{x: x, y: y, width: width, height: height} ->
        outer = {x, y, x + width, y + height}

        case Map.get(clip, :inner_clip) do
          nil ->
            outer

          inner ->
            intersect_clip(
              outer,
              {inner.x, inner.y, inner.x + inner.width, inner.y + inner.height}
            )
        end
    end
  end

  @doc "Transforms each validated box clip boundary before intersecting its outer and inner rectangles."
  @spec transform_box_clip(map() | nil, tuple()) :: tuple() | nil
  def transform_box_clip(clip, transform) do
    case clip do
      nil ->
        nil

      clip ->
        intersect_clip(
          transform_clip(clip, transform),
          transform_clip(Map.get(clip, :inner_clip), transform)
        )
    end
  end

  @doc false
  @spec local_group_transform(map(), number() | nil) :: tuple()
  def local_group_transform(group, parent_height) do
    {a, b, c, d, e, f} = group.transform

    {e, f} =
      if Map.get(group, :role) == :embedded_viewport_host and is_number(parent_height) and
           b == 0 and c == 0 and a > 0 and d > 0 do
        # Blink rounds the replaced-content paint location on entry to a child
        # view. Fractional parent paint offsets do not propagate into that view.
        # This is CSS-pixel origin rounding, separate from PDF CTM precision.
        rounded_viewport_origin(e, f, d * group.height, parent_height)
      else
        {e, f}
      end

    [a, b, c, d, e, f] = Enum.map([a, b, c, d, e, f], &Float.round(&1 * 1.0, 4))
    {a, b, c, d, e, f}
  end

  defp rounded_viewport_origin(x, y, height, parent_height) do
    {snap_css_pixel(x), parent_height - snap_css_pixel(parent_height - y - height) - height}
  end

  @doc false
  @spec compose_group(map(), tuple() | map() | nil) :: tuple() | map()
  def compose_group(group, composition) do
    {parent, inherited_clip, parent_height} =
      composition_parts(composition || {1, 1, 0, 0, nil, nil})

    {a, b, c, d, e, f} = local_group_transform(group, parent_height)
    {pa, pb, pc, pd, pe, pf} = affine_matrix(parent)

    affine =
      {pa * a + pc * b, pb * a + pd * b, pa * c + pc * d, pb * c + pd * d, pa * e + pc * f + pe,
       pb * e + pd * f + pf}

    {a, b, c, d, e, f} = affine

    transform =
      if not is_map(composition) and b == 0 and c == 0 and a > 0 and d > 0,
        do: {a, d, e, f},
        else: affine

    clip =
      intersect_clip(
        inherited_clip,
        transform_box_clip(local_clip(group, parent_height), transform)
      )

    # Overflow clipping adds no viewport or coordinate origin. Preserve the
    # containing paint frame for pixel rounding and descendant annotations.
    height =
      if Map.get(group, :role) == :overflow_clip and is_number(parent_height),
        do: parent_height,
        else: group.height

    case transform do
      {sx, sy, tx, ty} -> {sx, sy, tx, ty, clip, height}
      affine -> %{transform: affine, clip: clip, height: height}
    end
  end

  @doc "Returns the cumulative matrix, exact clip and local paint height of validated composition."
  @spec composition_parts(tuple() | map()) :: {tuple(), term(), number() | nil}
  def composition_parts(composition) do
    case composition do
      {sx, sy, tx, ty, clip, height} -> {{sx, sy, tx, ty}, clip, height}
      %{transform: matrix, clip: clip, height: height} -> {matrix, clip, height}
    end
  end

  @doc "Expresses a validated axis or affine transform as a PDF six-value matrix."
  @spec affine_matrix(tuple()) :: tuple()
  def affine_matrix(transform) do
    case transform do
      {sx, sy, tx, ty} -> {sx, 0, 0, sy, tx, ty}
      {_, _, _, _, _, _} -> transform
    end
  end

  @doc false
  @spec transform_point(number(), number(), tuple()) :: {number(), number()}
  def transform_point(x, y, transform) do
    [x, y] = Enum.map([x, y], &Float.round(&1 * 1.0, 4))
    {a, b, c, d, e, f} = affine_matrix(transform)
    {a * x + c * y + e, b * x + d * y + f}
  end

  @doc false
  @spec transform_clip(map() | nil, tuple()) :: tuple() | nil
  def transform_clip(clip, transform) do
    case clip do
      nil ->
        nil

      %{x: x, y: y, width: width, height: height} ->
        [x, y, width, height] = Enum.map([x, y, width, height], &Float.round(&1 * 1.0, 4))

        if Map.get(clip, :border_radius, 0) > 0 or Map.has_key?(clip, :border_radii) do
          {:intersection, [{:contour, edges}]} =
            box_clip_expression(Map.delete(clip, :inner_clip), transform)

          {:polygon, edges |> Enum.reverse() |> Enum.map(&elem(&1, 0))}
        else
          case transform do
            {sx, sy, tx, ty} ->
              {sx * x + tx, sy * y + ty, sx * (x + width) + tx, sy * (y + height) + ty}

            {_, _, _, _, _, _} ->
              {:polygon,
               Enum.map([{x, y}, {x + width, y}, {x + width, y + height}, {x, y + height}], fn
                 {x, y} -> transform_point(x, y, transform)
               end)}
          end
        end
    end
  end

  @doc false
  @spec intersect_clip(tuple() | nil, tuple() | nil) :: tuple() | nil
  def intersect_clip(first, second) do
    case {first, second} do
      {nil, clip} ->
        clip

      {clip, nil} ->
        clip

      {{left, bottom, right, top}, {x1, y1, x2, y2}} ->
        {max(left, x1), max(bottom, y1), min(right, x2), min(top, y2)}

      {first, second} ->
        {:polygon, clip_polygon(clip_polygon_points(first), second)}
    end
  end

  @doc "Returns the bounds of a validated rectangle or exact convex clip polygon."
  @spec clip_bounds(term()) :: tuple() | nil
  def clip_bounds(clip) do
    case clip do
      nil ->
        nil

      {:polygon, []} ->
        {0, 0, 0, 0}

      {:polygon, points} ->
        {Enum.min(Enum.map(points, &elem(&1, 0))), Enum.min(Enum.map(points, &elem(&1, 1))),
         Enum.max(Enum.map(points, &elem(&1, 0))), Enum.max(Enum.map(points, &elem(&1, 1)))}

      {_, _, _, _} ->
        clip
    end
  end

  @doc "Returns a canonical geometric clip expression for validated composition."
  @spec clip_expression(term()) :: tuple() | nil
  def clip_expression(clip) do
    case clip do
      nil ->
        nil

      _ ->
        case clip_polygon_points(clip) do
          [] -> {:contour, []}
          points -> NativeElixirPdfUtilities.HtmlToPdf.ClipGeometry.polygon(points)
        end
    end
  end

  @doc "Returns the actual cubic commands shared by rounded rectangle paint and clip projection."
  @spec rounded_rect_commands(number(), number(), number(), number(), number()) :: [
          {integer(), [number()]}
        ]
  def rounded_rect_commands(x, y, width, height, radius) do
    right = x + width
    top = y + height
    control = radius * 0.552_284_7498

    [
      {0, [x + radius, y]},
      {1, [right - radius, y]},
      {3, [right - radius + control, y, right, y + radius - control, right, y + radius]},
      {1, [right, top - radius]},
      {3, [right, top - radius + control, right - radius + control, top, right - radius, top]},
      {1, [x + radius, top]},
      {3, [x + radius - control, top, x, top - radius + control, x, top - radius]},
      {1, [x, y + radius]},
      {3, [x, y + radius - control, x + radius - control, y, x + radius, y]},
      {4, []}
    ]
  end

  @doc "Projects prepared outer and inner box clips with the same rounded curves used by PDF paint."
  @spec box_clip_expression(map() | nil, tuple()) :: tuple() | nil
  def box_clip_expression(clip, transform) do
    case clip do
      nil ->
        nil

      _ ->
        expressions =
          [clip, Map.get(clip, :inner_clip)]
          |> Enum.reject(&is_nil/1)
          |> Enum.map(fn clip ->
            if Map.get(clip, :border_radius, 0) > 0 or Map.has_key?(clip, :border_radii) do
              commands =
                case Map.get(clip, :border_radii) do
                  nil ->
                    rounded_rect_commands(
                      clip.x,
                      clip.y,
                      clip.width,
                      clip.height,
                      clip.border_radius
                    )

                  radii ->
                    elliptical_rect_commands(clip.x, clip.y, clip.width, clip.height, radii, 4)
                end
                |> Enum.map(fn {operation, coordinates} ->
                  coordinates =
                    coordinates
                    |> Enum.chunk_every(2)
                    |> Enum.flat_map(fn [x, y] ->
                      {x, y} = transform_point(x, y, transform)
                      [x, y]
                    end)

                  {operation, coordinates}
                end)

              NativeElixirPdfUtilities.HtmlToPdf.ClipGeometry.flatten_box(
                commands,
                1 / NativeElixirPdfUtilities.Limits.get(:clip_curve_precision)
              )
            else
              clip_expression(transform_clip(clip, transform))
            end
          end)

        {:intersection, expressions}
    end
  end

  @doc "Tests a validated point against the actual convex clip, including boundary points."
  @spec clip_contains?(term(), number(), number()) :: boolean()
  def clip_contains?(clip, x, y) do
    case clip do
      nil ->
        true

      %{bounds: bounds, expression: expression} ->
        clip_contains?(bounds, x, y) and
          NativeElixirPdfUtilities.HtmlToPdf.ClipGeometry.contains_point?(expression, x, y)

      {:polygon, points} ->
        crosses =
          Enum.zip(points, Enum.drop(points, 1) ++ Enum.take(points, 1))
          |> Enum.map(fn {{ax, ay}, {bx, by}} -> (bx - ax) * (y - ay) - (by - ay) * (x - ax) end)

        length(points) >= 3 and Enum.any?(crosses, &(&1 != 0)) and
          (Enum.all?(crosses, &(&1 >= 0)) or Enum.all?(crosses, &(&1 <= 0)))

      {left, bottom, right, top} ->
        right > left and top > bottom and x >= left and x <= right and y >= bottom and y <= top
    end
  end

  @doc "Clips validated polygon corners against an exact convex containing clip."
  @spec clip_polygon([{number(), number()}], term()) :: [{number(), number()}]
  def clip_polygon(points, clip) do
    boundary = clip_polygon_points(clip)

    NativeElixirPdfUtilities.Validators.HtmlValidator.reserve_render_resource(
      :max_clip_projection_work,
      length(points) * length(boundary),
      :writer
    )

    edges = Enum.zip(boundary, Enum.drop(boundary, 1) ++ Enum.take(boundary, 1))
    area = Enum.reduce(edges, 0, fn {{x, y}, {a, b}}, sum -> sum + x * b - a * y end)

    if length(boundary) < 3 or area == 0 do
      []
    else
      Enum.reduce(edges, points, fn {{ax, ay}, {bx, by}}, polygon ->
        Enum.zip(polygon, Enum.drop(polygon, 1) ++ Enum.take(polygon, 1))
        |> Enum.flat_map(fn {{x, y}, {u, v} = second} ->
          first_cross = ((bx - ax) * (y - ay) - (by - ay) * (x - ax)) * area
          second_cross = ((bx - ax) * (v - ay) - (by - ay) * (u - ax)) * area
          first_in = first_cross >= 0
          second_in = second_cross >= 0

          crossing =
            if first_in != second_in do
              ratio = first_cross / (first_cross - second_cross)
              {x + ratio * (u - x), y + ratio * (v - y)}
            end

          case {first_in, second_in} do
            {true, true} -> [second]
            {true, false} -> [crossing]
            {false, true} -> [crossing, second]
            _ -> []
          end
        end)
        |> Enum.uniq()
      end)
    end
  end

  defp clip_polygon_points(clip) do
    case clip do
      {:polygon, points} ->
        points

      {left, bottom, right, top} when right > left and top > bottom ->
        [{left, bottom}, {right, bottom}, {right, top}, {left, top}]

      _ ->
        []
    end
  end

  @doc "Builds validated elliptical contours using the source printer's float32 conic subdivision."
  @spec elliptical_rect_commands(number(), number(), number(), number(), map(), 4 | 8) :: [
          {integer(), [number()]}
        ]
  def elliptical_rect_commands(x, y, width, height, radii, subdivisions) do
    x = conic_float(x)
    y = conic_float(y)
    right = conic_float(x + conic_float(width))
    top = conic_float(y + conic_float(height))
    {blx, bly} = radii.bottom_left
    {brx, bry} = radii.bottom_right
    {trx, try_} = radii.top_right
    {tlx, tly} = radii.top_left
    left_bottom = conic_float(x + conic_float(blx))
    bottom_left = conic_float(y + conic_float(bly))
    right_bottom = conic_float(right - conic_float(brx))
    bottom_right = conic_float(y + conic_float(bry))
    right_top = conic_float(right - conic_float(trx))
    top_right = conic_float(top - conic_float(try_))
    left_top = conic_float(x + conic_float(tlx))
    top_left = conic_float(top - conic_float(tly))
    weight = conic_float(:math.sqrt(0.5))
    level = if subdivisions == 4, do: 2, else: 3

    [
      {0, [left_bottom, y]},
      {1, [right_bottom, y]},
      conic_cubic_commands({right_bottom, y}, {right, y}, {right, bottom_right}, weight, level),
      {1, [right, top_right]},
      conic_cubic_commands({right, top_right}, {right, top}, {right_top, top}, weight, level),
      {1, [left_top, top]},
      conic_cubic_commands({left_top, top}, {x, top}, {x, top_left}, weight, level),
      {1, [x, bottom_left]},
      conic_cubic_commands({x, bottom_left}, {x, y}, {left_bottom, y}, weight, level),
      {4, []}
    ]
    |> List.flatten()
  end

  # Adapted from SkConic::chop/subdivide and SkConvertQuadToCubic in Skia:
  # https://github.com/google/skia/blob/main/src/core/SkGeometry.cpp
  # The four/eight-piece counts are fixed source PDF contour rules. Every
  # scalar operation rounds to float32, as in Skia's float2 calculations.
  defp conic_cubic_commands({ax, ay} = a, {qx, qy}, {bx, by} = b, weight, level) do
    if level == 0 do
      scale = conic_float(2 / 3)

      [
        {3,
         [
           conic_float(ax + conic_float(conic_float(qx - ax) * scale)),
           conic_float(ay + conic_float(conic_float(qy - ay) * scale)),
           conic_float(bx + conic_float(conic_float(qx - bx) * scale)),
           conic_float(by + conic_float(conic_float(qy - by) * scale)),
           bx,
           by
         ]}
      ]
    else
      scale = conic_float(1 / conic_float(1 + weight))
      weighted_scale = conic_float(weight * scale)

      [{lx, ly}, {mx, my}, {rx, ry}] =
        Enum.zip([ax, ay], Enum.zip([qx, qy], [bx, by]))
        |> Enum.map(fn {a, {q, b}} ->
          t0 = conic_float(a * scale)
          t1 = conic_float(q * weighted_scale)
          t2 = conic_float(b * scale)

          {
            conic_float(t0 + t1),
            conic_float(conic_float(conic_float(0.5 * t0) + t1) + conic_float(0.5 * t2)),
            conic_float(t1 + t2)
          }
        end)
        |> then(fn [{lx, mx, rx}, {ly, my, ry}] -> [{lx, ly}, {mx, my}, {rx, ry}] end)

      # Each validated quarter ellipse has its control at an endpoint's Y.
      # It is therefore monotonic before chopping. Preserve Skia's corrections
      # for float rounding that can push a chopped point past its endpoints.
      my =
        if conic_float(conic_float(ay - my) * conic_float(by - my)) <= 0 do
          my
        else
          if abs(conic_float(my - ay)) < abs(conic_float(my - by)), do: ay, else: by
        end

      ly = if conic_float(conic_float(ay - ly) * conic_float(my - ly)) <= 0, do: ly, else: ay
      ry = if conic_float(conic_float(my - ry) * conic_float(by - ry)) <= 0, do: ry, else: by

      new_weight = conic_float(:math.sqrt(conic_float(0.5 + conic_float(weight * 0.5))))
      middle = {mx, my}

      conic_cubic_commands(a, {lx, ly}, middle, new_weight, level - 1) ++
        conic_cubic_commands(middle, {rx, ry}, b, new_weight, level - 1)
    end
  end

  defp conic_float(value) do
    <<rounded::float-size(32)>> = <<value * 1.0::float-size(32)>>
    rounded
  end
end
