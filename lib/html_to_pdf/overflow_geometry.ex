defmodule NativeElixirPdfUtilities.HtmlToPdf.OverflowGeometry do
  @moduledoc "Clips each computed overflow axis without moving artwork or inventing an infinite viewport."
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, PageGeometry}
  alias NativeElixirPdfUtilities.Validators.{CssOverflowValidator, FontValidator}

  @doc "Returns the overflow clip using allocated box edges and actual ink on any visible axis."
  @spec clip([map()], map(), map()) :: map() | nil
  def clip(boxes, style, padding_box) do
    case style do
      style
      when not is_map_key(style, :overflow_x) and not is_map_key(style, :overflow_y) and
             not is_map_key(style, :contain) ->
        if Map.get(style, :overflow, :visible) == :visible,
          do: nil,
          else: used_clip(boxes, style, padding_box)

      _ ->
        used_clip(boxes, style, padding_box)
    end
  end

  defp used_clip(boxes, style, padding_box) do
    {x, y} = CssOverflowValidator.axes(style)
    {x, y} = if :paint in Map.get(style, :contain, []), do: {:clip, :clip}, else: {x, y}

    if x == :visible and y == :visible do
      nil
    else
      {reference, amount} = Map.get(style, :overflow_clip_margin, {:padding_box, 0.0})
      zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}

      adjustment =
        case reference do
          :content_box ->
            Map.new(Map.get(style, :padding, zero), fn {side, value} -> {side, -value} end)

          :border_box ->
            Map.get(style, :border_widths, zero)

          :padding_box ->
            zero
        end

      left = padding_box.x - if(x == :clip, do: amount + adjustment.left, else: 0.0)

      right =
        padding_box.x + padding_box.width +
          if(x == :clip, do: amount + adjustment.right, else: 0.0)

      bottom = padding_box.y - if(y == :clip, do: amount + adjustment.bottom, else: 0.0)

      top =
        padding_box.y + padding_box.height +
          if(y == :clip, do: amount + adjustment.top, else: 0.0)

      {left, bottom, right, top} =
        if x == :visible or y == :visible do
          {ink_left, ink_bottom, ink_right, ink_top} =
            ink_bounds(boxes, {left, bottom, right, top})

          {if(x == :visible, do: ink_left, else: left),
           if(y == :visible, do: ink_bottom, else: bottom),
           if(x == :visible, do: ink_right, else: right),
           if(y == :visible, do: ink_top, else: top)}
        else
          {left, bottom, right, top}
        end

      %{x: left, y: bottom, width: max(right - left, 0.0), height: max(top - bottom, 0.0)}
    end
  end

  @doc "Unions actual font outlines and transformed paint extents in the containing coordinate space."
  @spec ink_bounds([map()], {number(), number(), number(), number()}) :: tuple()
  def ink_bounds(boxes, seed) do
    Enum.reduce(boxes, seed, fn box, {left, bottom, right, top} ->
      {box_top, box_bottom} = PageGeometry.box_vertical_bounds(box)
      bounds = {box.x, box_bottom, box.x + Map.get(box, :width, 0.0), box_top}

      bounds =
        case box do
          %{type: :text} ->
            case FontValidator.prepare_text_ink_rectangle(box) do
              {:ok, nil} -> bounds
              {:ok, rectangle} -> rectangle
              {:error, _} = error -> throw({:render_resource_limit, error})
            end

          %{type: :glyph_group, boxes: children} ->
            ink_bounds(children, bounds)

          %{type: :drawing_group, boxes: children, transform: matrix} ->
            local = ink_bounds(children, {0.0, 0.0, 0.0, 0.0})

            DrawingGeometry.transform_clip(rectangle(local), matrix)
            |> DrawingGeometry.clip_bounds()

          %{type: :rect} ->
            stroke = Map.get(box, :stroke_width, 0.0) / 2
            {x1, y1, x2, y2} = bounds
            {x1 - stroke, y1 - stroke, x2 + stroke, y2 + stroke}

          _ ->
            bounds
        end

      bounds =
        Enum.reduce(Enum.reverse(Map.get(box, :css_transform_scopes, [])), bounds, fn scope,
                                                                                      bounds ->
          DrawingGeometry.transform_clip(rectangle(bounds), scope.matrix)
          |> DrawingGeometry.clip_bounds()
        end)

      {x1, y1, x2, y2} = bounds
      # Cover the source pixel containing an outline edge, including fractional
      # text baseline snapping; this is a paint-unit conversion, not a ceiling.
      {min(left, Float.floor(x1 / 0.75) * 0.75), min(bottom, Float.floor(y1 / 0.75) * 0.75),
       max(right, Float.ceil(x2 / 0.75) * 0.75), max(top, Float.ceil(y2 / 0.75) * 0.75)}
    end)
  end

  defp rectangle({left, bottom, right, top}) do
    %{x: left, y: bottom, width: right - left, height: top - bottom}
  end
end
