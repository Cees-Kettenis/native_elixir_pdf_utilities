defmodule NativeElixirPdfUtilities.HtmlToPdf.CssBackground do
  @moduledoc "Per-layer CSS background geometry and final-page fixed image painting."
  alias NativeElixirPdfUtilities.HtmlToPdf.{CssImageValue, DrawingGeometry, Layout, PageGeometry}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, CssPositionValidator}
  @zero %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}

  @fixed_context {__MODULE__, :fixed_backgrounds}

  @doc "Scopes fixed background detection and resolves child viewport artwork before embedding."
  @spec with_layout(function(), term()) :: term()
  def with_layout(callback, viewport) do
    previous = Process.get(@fixed_context)
    Process.delete(@fixed_context)

    try do
      case callback.() do
        {:ok, layout} = result ->
          if Process.get(@fixed_context, false) do
            if viewport == nil do
              {:ok, Map.put(layout, :css_fixed_backgrounds, true)}
            else
              [page] = resolve_fixed_pages([%{size: layout.page_size, boxes: layout.boxes}])
              {:ok, %{layout | boxes: page.boxes}}
            end
          else
            result
          end

        result ->
          result
      end
    after
      if previous == nil,
        do: Process.delete(@fixed_context),
        else: Process.put(@fixed_context, previous)
    end
  end

  @doc "Identifies a background needing independent layer geometry."
  @spec layered?(map()) :: boolean()
  def layered?(style) do
    case style do
      style
      when not is_map_key(style, :background_images) and
             not is_map_key(style, :background_positions) and
             not is_map_key(style, :background_sizes) and
             not is_map_key(style, :background_repeats) and
             not is_map_key(style, :background_origins) and
             not is_map_key(style, :background_clips) and
             not is_map_key(style, :background_attachments) ->
        false

      _ ->
        Map.has_key?(style, :background_images) or Map.has_key?(style, :background_positions) or
          Map.has_key?(style, :background_sizes) or Map.has_key?(style, :background_repeats) or
          Map.get(style, :background_origins, [:padding_box]) != [:padding_box] or
          Map.get(style, :background_clips, [:border_box]) != [:border_box] or
          Map.get(style, :background_attachments, [:scroll]) != [:scroll]
    end
  end

  @doc "Paints the color below independent image layers and keeps physical borders above them."
  @spec boxes(map(), map()) :: [map()]
  def boxes(style, rect) do
    images =
      Map.get_lazy(style, :background_images, fn ->
        case Map.get(style, :background_image) do
          nil -> [nil]
          image -> [%{kind: :decoded, image: image}]
        end
      end)

    layers =
      Enum.with_index(images)
      |> Enum.map(fn {image, index} ->
        %{
          image: image,
          position:
            cycle(
              style,
              :background_positions,
              [Map.get(style, :background_position, {{:percent, 0.0}, {:percent, 0.0}})],
              index
            ),
          size:
            cycle(
              style,
              :background_sizes,
              [Map.get(style, :background_size, {:auto, :auto})],
              index
            ),
          repeat:
            cycle(
              style,
              :background_repeats,
              [Map.get(style, :background_repeat, :repeat)],
              index
            ),
          origin: cycle(style, :background_origins, [:padding_box], index),
          clip: cycle(style, :background_clips, [:border_box], index),
          attachment: cycle(style, :background_attachments, [:scroll], index)
        }
      end)

    color =
      case Map.get(style, :background_color) do
        nil ->
          []

        _ ->
          clip =
            clip(rect, style, List.last(layers).clip)

          [
            %{rect | stroke_width: 0.0, stroke_color: nil, border_widths: @zero}
            |> Map.put(:clip, clip)
          ]
      end

    artwork =
      layers
      |> Enum.reverse()
      |> Enum.flat_map(fn layer ->
        cond do
          layer.image == nil ->
            []

          layer.attachment == :fixed ->
            Process.put(@fixed_context, true)

            marker =
              rect
              |> Map.put(:fill_color, {0.0, 0.0, 0.0, 0.0})
              |> Map.put(:stroke_width, 0.0)
              |> Map.put(:stroke_color, nil)
              |> Map.put(:border_widths, @zero)
              |> Map.delete(:form_control)
              |> Map.put(:clip, clip(rect, style, layer.clip))

            [
              Map.put(marker, :css_fixed_background, %{
                layer: %{layer | attachment: :scroll},
                style: style
              })
            ]

          true ->
            paint_layer(layer, rect, style, rect)
        end
      end)

    borders =
      if Enum.any?(Map.values(rect.border_widths), &(&1 > 0)) and rect.stroke_color != nil,
        do: [%{rect | fill_color: nil}],
        else: []

    carrier =
      if color == [] and borders == [] and Map.get(rect, :form_control) != nil,
        do: [%{rect | fill_color: {0.0, 0.0, 0.0, 0.0}}],
        else: []

    color ++ artwork ++ borders ++ carrier
  end

  @doc "Paints image layers for an inline source strip without duplicating its separate fill and border surfaces."
  @spec images(map(), number(), number(), number(), number()) :: [map()]
  def images(style, x, y, width, height) do
    rect = %{
      type: :rect,
      paint_layer: :container_background,
      snap_to_css_pixel_grid: true,
      x: x,
      y: y,
      width: width,
      height: height,
      fill_color: nil,
      stroke_color: nil,
      stroke_width: 0.0,
      border_widths: Map.get(style, :border_widths, @zero),
      border_radius: Map.get(style, :border_radius, 0.0)
    }

    rect =
      case NativeElixirPdfUtilities.Validators.CssBorderRadiusValidator.used_radii(
             style,
             width,
             height
           ) do
        {:ok, nil} -> rect
        {:ok, radii} -> Map.put(rect, :border_radii, radii)
        error -> throw({:render_resource_limit, error})
      end

    boxes(Map.put(style, :background_color, nil), rect)
  end

  @doc "Repaints fixed layers in each final page viewport before transforms and effects are materialized."
  @spec resolve_fixed_pages([map()]) :: [map()]
  def resolve_fixed_pages(pages) do
    Enum.map(pages, fn page -> %{page | boxes: fixed_boxes(page.boxes, page.size)} end)
  end

  defp fixed_boxes(boxes, {width, height} = page_size) do
    Enum.flat_map(boxes, fn box ->
      case box do
        %{css_fixed_background: %{layer: layer, style: style}} ->
          viewport = %{x: 0.0, y: 0.0, width: width, height: height, border_radius: 0.0}

          original_clip = Map.get(box, :clip, Map.take(box, [:x, :y, :width, :height]))

          clip =
            DrawingGeometry.intersect_clip(
              DrawingGeometry.box_clip_bounds(original_clip),
              DrawingGeometry.box_clip_bounds(box)
            )
            |> DrawingGeometry.intersect_clip({0.0, 0.0, width, height})

          case clip do
            {left, bottom, right, top} when right > left and top > bottom ->
              target = %{
                x: left,
                y: bottom,
                width: right - left,
                height: top - bottom,
                border_radius: 0.0
              }

              metadata =
                Map.drop(box, [
                  :type,
                  :x,
                  :y,
                  :width,
                  :height,
                  :css_fixed_background,
                  :clip,
                  :fill_color,
                  :stroke_color,
                  :stroke_width,
                  :border_widths,
                  :border_colors,
                  :border_styles,
                  :border_radius
                ])

              paint_layer(
                layer,
                viewport,
                Map.put(style, :border_widths, @zero) |> Map.put(:padding, @zero),
                target
              )
              |> Enum.map(fn tile ->
                tile
                |> Map.merge(metadata)
                |> Map.put(
                  :clip,
                  Map.put(original_clip, :inner_clip, Map.take(target, [:x, :y, :width, :height]))
                )
              end)

            _ ->
              []
          end

        %{boxes: children} ->
          [%{box | boxes: fixed_boxes(children, page_size)}]

        _ ->
          [box]
      end
    end)
  end

  defp cycle(style, key, fallback, index) do
    values = Map.get(style, key, fallback)
    Enum.at(values, rem(index, length(values)))
  end

  defp clip(rect, style, box) do
    border = Map.get(style, :border_widths, @zero)
    padding = Map.get(style, :padding, @zero)

    insets =
      case box do
        :border_box ->
          @zero

        :padding_box ->
          border

        :content_box ->
          Map.new(border, fn {side, value} -> {side, value + Map.fetch!(padding, side)} end)
      end

    PageGeometry.inset_rounded_clip(rect, insets)
  end

  defp paint_layer(layer, rect, style, target) do
    area = clip(rect, style, layer.origin)
    boundary = clip(target, style, layer.clip)

    natural =
      case layer.image do
        %{kind: :decoded, image: image} -> {image.width, image.height, true}
        _ -> {area.width, area.height, false}
      end

    {tile_width, tile_height} = image_size(layer.size, natural, area, style)

    {repeat_x, repeat_y} =
      case layer.repeat do
        :repeat -> {:repeat, :repeat}
        :repeat_x -> {:repeat, :no_repeat}
        :repeat_y -> {:no_repeat, :repeat}
        :no_repeat -> {:no_repeat, :no_repeat}
        pair -> pair
      end

    repeat =
      cond do
        repeat_x == :no_repeat and repeat_y == :no_repeat -> :no_repeat
        repeat_x == :no_repeat -> :repeat_y
        repeat_y == :no_repeat -> :repeat_x
        true -> :repeat
      end

    if tile_width > 0 and tile_height > 0 and area.width > 0 and area.height > 0 do
      case HtmlValidator.validate_background_image_tile_dimensions(
             tile_width,
             tile_height,
             boundary.width,
             boundary.height,
             repeat
           ) do
        :ok -> :ok
        error -> throw({:render_resource_limit, error})
      end

      rounded_width =
        if repeat_x == :round,
          do: area.width / max(round(area.width / tile_width), 1),
          else: tile_width

      rounded_height =
        if repeat_y == :round,
          do: area.height / max(round(area.height / tile_height), 1),
          else: tile_height

      {tile_width, tile_height} =
        case {layer.size, elem(natural, 2)} do
          {{_, :auto}, true} when repeat_x == :round and repeat_y != :round ->
            {rounded_width, tile_height * rounded_width / tile_width}

          {{:auto, _}, true} when repeat_y == :round and repeat_x != :round ->
            {tile_width * rounded_height / tile_height, rounded_height}

          _ ->
            {rounded_width, rounded_height}
        end

      {horizontal, vertical} = layer.position

      {offset_x, offset_y} =
        with {:ok, x} <- CssPositionValidator.resolve(horizontal, area.width - tile_width, style),
             {:ok, y} <- CssPositionValidator.resolve(vertical, area.height - tile_height, style) do
          {x, y}
        else
          error -> throw({:render_resource_limit, error})
        end

      xs =
        axis_plan(
          area.x + offset_x,
          tile_width,
          area.x,
          area.width,
          boundary.x,
          boundary.width,
          repeat_x
        )

      # Plan in top-down coordinates so spacing and edge positions match CSS.
      ys =
        axis_plan(
          -area.y - area.height + offset_y,
          tile_height,
          -area.y - area.height,
          area.height,
          -boundary.y - boundary.height,
          boundary.height,
          repeat_y
        )

      if xs.count == 0 or ys.count == 0 do
        []
      else
        case HtmlValidator.validate_background_image_tile_count(xs.count * ys.count) do
          :ok -> :ok
          error -> throw({:render_resource_limit, error})
        end

        case CssImageValue.rasterize(layer.image, tile_width, tile_height, style) do
          {:ok, image} ->
            for ix <- 0..(xs.count - 1), iy <- 0..(ys.count - 1) do
              %{
                type: HtmlValidator.reserve_layout_box(:image),
                paint_layer: :container_background,
                snap_to_css_pixel_grid: true,
                x: xs.first + ix * xs.step,
                y: -(ys.first + iy * ys.step) - tile_height,
                width: tile_width,
                height: tile_height,
                image: image,
                clip: boundary
              }
            end

          error ->
            throw({:render_resource_limit, error})
        end
      end
    else
      []
    end
  end

  defp image_size(size, {nw, nh, intrinsic?}, area, style) do
    case size do
      mode when mode in [:cover, :contain] ->
        if intrinsic? and nw > 0 and nh > 0 do
          scale =
            if mode == :cover,
              do: max(area.width / nw, area.height / nh),
              else: min(area.width / nw, area.height / nh)

          {nw * scale, nh * scale}
        else
          {area.width, area.height}
        end

      {width, height} ->
        w =
          if width == :auto, do: :auto, else: Layout.resolve_size_value(width, area.width, style)

        h =
          if height == :auto,
            do: :auto,
            else: Layout.resolve_size_value(height, area.height, style)

        case {w, h} do
          {:auto, :auto} -> {nw, nh}
          {w, :auto} -> {w, if(intrinsic? and nw > 0, do: w * nh / nw, else: area.height)}
          {:auto, h} -> {if(intrinsic? and nh > 0, do: h * nw / nh, else: area.width), h}
          pair -> pair
        end
    end
  end

  defp axis_plan(initial, tile, origin, extent, minimum, span, repeat) do
    case repeat do
      :no_repeat ->
        %{first: initial, step: tile, count: 1}

      :space ->
        count = floor(extent / tile)

        if count >= 2 do
          step = (extent - tile) / (count - 1)
          first = origin - Float.ceil((origin - minimum) / step) * step
          first = if first + tile <= minimum, do: first + step, else: first
          %{first: first, step: step, count: max(ceil((minimum + span - first) / step), 0)}
        else
          %{first: initial, step: tile, count: 1}
        end

      _ ->
        first = initial - Float.ceil((initial - minimum) / tile) * tile
        %{first: first, step: tile, count: max(ceil((minimum + span - first) / tile), 1)}
    end
  end
end
