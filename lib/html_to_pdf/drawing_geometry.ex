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
        %{x: x, y: y} = clip when box.type != :drawing_group ->
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
        %{type: :drawing_group} ->
          children = translate_boxes([box], -clip.x, -clip.y)

          [
            %{
              type:
                NativeElixirPdfUtilities.Validators.HtmlValidator.reserve_layout_box(
                  :drawing_group
                ),
              role: :overflow_clip,
              x: clip.x,
              y: clip.y,
              width: clip.width,
              height: clip.height,
              transform: {1, 0, 0, 1, clip.x, clip.y},
              clip: %{x: 0.0, y: 0.0, width: clip.width, height: clip.height},
              boxes: children
            }
            |> Map.merge(Map.take(box, [:flow_id, :out_of_flow, :position_anchor, :break_inside]))
          ]

        %{type: :glyph_group, boxes: children} ->
          [%{box | boxes: clip_boxes(children, clip)}]

        _ ->
          {left, bottom, right, top} =
            intersect_clip(
              {clip.x, clip.y, clip.x + clip.width, clip.y + clip.height},
              box_clip_bounds(Map.get(box, :clip))
            )

          if right > left and top > bottom do
            existing = Map.get(box, :clip)
            prepared = %{x: left, y: bottom, width: right - left, height: top - bottom}

            rounded =
              case existing do
                %{inner_clip: %{border_radius: radius} = inner} when radius > 0 -> inner
                %{border_radius: radius} when radius > 0 -> Map.delete(existing, :inner_clip)
                _ -> nil
              end

            prepared =
              if rounded == nil, do: prepared, else: Map.put(prepared, :inner_clip, rounded)

            [Map.put(box, :clip, prepared)]
          else
            []
          end
      end
    end)
  end

  @doc false
  @spec text_position(map(), number()) :: {number(), number()}
  def text_position(box, height) do
    case Map.get(box, :snap_to_css_pixel_grid, false) do
      true ->
        x = if Map.has_key?(box, :glyph_ids), do: box.x - @raster_boundary_bias, else: box.x

        baseline =
          case box do
            %{baseline_snap_depth: depth} -> text_baseline(box.y, height, depth)
            _ -> height - snap_css_pixel(height - box.y)
          end

        {x, baseline + @raster_boundary_bias}

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
  @spec local_clip(map()) :: map() | nil
  def local_clip(group) do
    if group.width == 0 or group.height == 0,
      do: %{x: 0, y: 0, width: 0, height: 0},
      else: Map.get(group, :clip)
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
      if Map.get(group, :role) == :embedded_viewport_host and is_number(parent_height) do
        # Blink rounds the replaced-content paint location on entry to a child
        # view. Fractional parent paint offsets do not propagate into that view.
        # This is CSS-pixel origin rounding, separate from PDF CTM precision.
        top = f + d * group.height

        {snap_css_pixel(e),
         parent_height - snap_css_pixel(parent_height - top) - d * group.height}
      else
        {e, f}
      end

    [a, b, c, d, e, f] = Enum.map([a, b, c, d, e, f], &Float.round(&1 * 1.0, 4))
    {a, b, c, d, e, f}
  end

  @doc false
  @spec compose_group(map(), tuple() | nil) :: tuple()
  def compose_group(group, composition) do
    {sx, sy, tx, ty, inherited_clip, parent_height} = composition || {1, 1, 0, 0, nil, nil}
    {a, _b, _c, d, e, f} = local_group_transform(group, parent_height)
    transform = {sx * a, sy * d, sx * e + tx, sy * f + ty}
    clip = intersect_clip(inherited_clip, transform_clip(local_clip(group), transform))
    {sx, sy, tx, ty} = transform
    {sx, sy, tx, ty, clip, group.height}
  end

  @doc false
  @spec transform_point(number(), number(), tuple()) :: {number(), number()}
  def transform_point(x, y, {sx, sy, tx, ty}) do
    [x, y] = Enum.map([x, y], &Float.round(&1 * 1.0, 4))
    {sx * x + tx, sy * y + ty}
  end

  @doc false
  @spec transform_clip(map() | nil, tuple()) :: tuple() | nil
  def transform_clip(clip, {sx, sy, tx, ty}) do
    case clip do
      nil ->
        nil

      %{x: x, y: y, width: width, height: height} ->
        [x, y, width, height] = Enum.map([x, y, width, height], &Float.round(&1 * 1.0, 4))
        {sx * x + tx, sy * y + ty, sx * (x + width) + tx, sy * (y + height) + ty}
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
    end
  end
end
