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

  @doc false
  @spec snap_css_pixel(number()) :: float()
  def snap_css_pixel(value) do
    Float.floor(value / @css_pixel_points + 0.5) * @css_pixel_points
  end
end
