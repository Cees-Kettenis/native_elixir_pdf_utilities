defmodule NativeElixirPdfUtilities.HtmlToPdf.CssBorderImage do
  @moduledoc "Nine-part border image geometry using clipped source images and shared paint budgets."
  alias NativeElixirPdfUtilities.HtmlToPdf.CssImageValue

  alias NativeElixirPdfUtilities.Validators.{
    CssBorderImageValidator,
    CssLengthValidator,
    HtmlValidator
  }

  @doc "Paints independent corner, edge and optional center patches without changing layout dimensions."
  @spec boxes(map(), map(), :border | :mask) :: [map()]
  @spec boxes(map(), map()) :: [map()]
  def boxes(style, rect, mode \\ :border) do
    fields = Map.merge(CssBorderImageValidator.defaults(), style)

    outset =
      Map.new(fields.border_image_outset, fn {side, value} ->
        {side, extent(value, rect.border_widths[side], nil, style)}
      end)

    area = %{
      x: rect.x - outset.left,
      y: rect.y - outset.bottom,
      width: rect.width + outset.left + outset.right,
      height: rect.height + outset.top + outset.bottom
    }

    with {:ok, image} <-
           CssImageValue.rasterize(fields.border_image_source, area.width, area.height, style) do
      intrinsic? = fields.border_image_source.kind == :decoded

      vector? =
        not intrinsic? or
          (Map.has_key?(image, :svg_viewport) and image.width > 0 and image.height > 0)

      {sw, sh} =
        if vector?,
          do: {image.width / 0.75, image.height / 0.75},
          else: {image.width_px, image.height_px}

      {sw, sh} =
        if Map.get(image, :image_orientation, 1) in 5..8 and not vector?,
          do: {sh, sw},
          else: {sw, sh}

      # Slices stretch the already admitted SVG canvas, without establishing
      # a new SVG viewport for every individual patch.
      image = Map.delete(image, :svg_viewport)

      slices =
        Map.new(fields.border_image_slice.edges, fn {side, value} ->
          axis = if side in [:top, :bottom], do: sh, else: sw

          n =
            case value do
              {:percent, ratio} -> ratio * axis
              {:slice, n} -> n
            end

          {side, min(n, axis)}
        end)

      widths =
        Map.new(fields.border_image_width, fn {side, value} ->
          basis = if side in [:top, :bottom], do: area.height, else: area.width

          value =
            if value == :auto do
              if intrinsic? do
                natural_scale =
                  if side in [:left, :right], do: image.width / sw, else: image.height / sh

                slices[side] * natural_scale
              else
                rect.border_widths[side]
              end
            else
              value
            end

          {side, extent(value, rect.border_widths[side], basis, style)}
        end)

      factor =
        Enum.reduce(
          [{area.width, widths.left + widths.right}, {area.height, widths.top + widths.bottom}],
          1.0,
          fn {size, sum}, factor -> if sum > 0, do: min(factor, size / sum), else: factor end
        )

      widths = Map.new(widths, fn {side, n} -> {side, n * factor} end)

      xs = [
        {0, slices.left},
        {slices.left, max(sw - slices.left - slices.right, 0)},
        {sw - slices.right, slices.right}
      ]

      ys = [
        {0, slices.top},
        {slices.top, max(sh - slices.top - slices.bottom, 0)},
        {sh - slices.bottom, slices.bottom}
      ]

      dx = [area.x, area.x + widths.left, area.x + area.width - widths.right, area.x + area.width]

      dy = [
        area.y + area.height,
        area.y + area.height - widths.top,
        area.y + widths.bottom,
        area.y
      ]

      {repeat_x, repeat_y} = fields.border_image_repeat

      for row <- 0..2,
          col <- 0..2,
          mode == :mask or fields.border_image_slice.fill or row != 1 or col != 1,
          reduce: [] do
        boxes ->
          {sx, source_width} = Enum.at(xs, col)
          {sy, source_height} = Enum.at(ys, row)

          destination = %{
            x: Enum.at(dx, col),
            y: Enum.at(dy, row + 1),
            width: Enum.at(dx, col + 1) - Enum.at(dx, col),
            height: Enum.at(dy, row) - Enum.at(dy, row + 1)
          }

          if mode == :mask and row == 1 and col == 1 and not fields.border_image_slice.fill do
            [
              Map.merge(rect, %{
                x: destination.x,
                y: destination.y,
                width: destination.width,
                height: destination.height,
                fill_color: {1.0, 1.0, 1.0},
                stroke_color: nil,
                stroke_width: 0.0,
                border_radius: 0.0,
                border_widths: %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
              })
              | boxes
            ]
          else
            if source_width > 0 and source_height > 0 and destination.width > 0 and
                 destination.height > 0 do
              # Edges retain the scale of their perpendicular border width. The
              # middle uses the top/left scale, then the opposite edge, then 1.
              horizontal_scale =
                if row == 1 do
                  cond do
                    slices.top > 0 and widths.top > 0 -> widths.top / slices.top
                    slices.bottom > 0 and widths.bottom > 0 -> widths.bottom / slices.bottom
                    true -> 0.75
                  end
                else
                  destination.height / source_height
                end

              vertical_scale =
                if col == 1 do
                  cond do
                    slices.left > 0 and widths.left > 0 -> widths.left / slices.left
                    slices.right > 0 and widths.right > 0 -> widths.right / slices.right
                    true -> 0.75
                  end
                else
                  destination.width / source_width
                end

              xplan =
                tiles(
                  destination.x,
                  destination.width,
                  source_width * horizontal_scale,
                  if(col == 1, do: repeat_x, else: :stretch)
                )

              yplan =
                tiles(
                  destination.y,
                  destination.height,
                  source_height * vertical_scale,
                  if(row == 1, do: repeat_y, else: :stretch)
                )

              HtmlValidator.check_render_resource(
                :max_entries,
                xplan.count * yplan.count,
                :layout
              )

              patches =
                for xi <- 0..(xplan.count - 1),
                    yi <- 0..(yplan.count - 1),
                    xplan.count > 0 and yplan.count > 0 do
                  x = xplan.start + xi * xplan.step
                  y = yplan.start + yi * yplan.step
                  scale_x = xplan.size / source_width
                  scale_y = yplan.size / source_height

                  %{
                    type: HtmlValidator.reserve_layout_box(:image),
                    paint_layer: :container_background,
                    role: :border_image,
                    css_paint_role: :border_image,
                    image: image,
                    x: x - sx * scale_x,
                    y: y - (sh - sy - source_height) * scale_y,
                    width: sw * scale_x,
                    height: sh * scale_y,
                    clip: %{
                      x: max(x, destination.x),
                      y: max(y, destination.y),
                      width:
                        max(
                          min(x + xplan.size, destination.x + destination.width) -
                            max(x, destination.x),
                          0.0
                        ),
                      height:
                        max(
                          min(y + yplan.size, destination.y + destination.height) -
                            max(y, destination.y),
                          0.0
                        )
                    }
                  }
                end

              boxes ++ patches
            else
              boxes
            end
          end
      end
    else
      error -> throw({:render_resource_limit, error})
    end
  end

  defp extent(value, border, basis, style) do
    case value do
      {:multiple, n} ->
        n * border

      value ->
        case CssLengthValidator.resolve(value, basis, style) do
          {:ok, n} -> n
          error -> throw({:render_resource_limit, error})
        end
    end
  end

  defp tiles(origin, extent, natural, repeat) do
    case repeat do
      :stretch ->
        %{start: origin, step: extent, size: extent, count: 1}

      :round ->
        count = max(round(extent / natural), 1)
        %{start: origin, step: extent / count, size: extent / count, count: count}

      :space ->
        count = floor(extent / natural)
        gap = if count > 1, do: (extent - count * natural) / (count - 1), else: 0

        %{
          start: origin + if(count <= 1, do: (extent - natural) / 2, else: 0),
          step: natural + gap,
          size: natural,
          count: max(count, 1)
        }

      :repeat ->
        count = ceil(extent / natural)

        %{
          start: origin + (extent - count * natural) / 2,
          step: natural,
          size: natural,
          count: count
        }
    end
  end
end
