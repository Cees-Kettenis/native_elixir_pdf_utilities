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
          Map.put(box, :clip, %{clip | x: x + delta_x, y: y + delta_y})

        _ ->
          box
      end

    case Map.get(box, :link_rect) do
      %{x: x, y: y} = region ->
        Map.put(box, :link_rect, %{region | x: x + delta_x, y: y + delta_y})

      _ ->
        box
    end
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
    Float.floor(value / @css_pixel_points + 0.5) * @css_pixel_points
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
