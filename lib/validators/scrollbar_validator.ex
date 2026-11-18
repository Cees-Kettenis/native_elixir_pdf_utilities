defmodule NativeElixirPdfUtilities.Validators.ScrollbarValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Parses static scrollbar appearance and gutter policy."
  @spec declaration(map(), String.t(), String.t()) :: {:ok, map()} | :error | {:error, term()}
  def declaration(style, property, source) do
    terms = HtmlValidator.split_css_tokens(source)

    case {property, Enum.map(terms, &String.downcase(&1, :ascii))} do
      {"scrollbar-width", [value]} when value in ~w(auto thin none) ->
        {:ok, Map.put(style, :scrollbar_width, String.to_existing_atom(value))}

      {"scrollbar-gutter", ["auto"]} ->
        {:ok, Map.put(style, :scrollbar_gutter, :auto)}

      {"scrollbar-gutter", ["stable"]} ->
        {:ok, Map.put(style, :scrollbar_gutter, :stable)}

      {"scrollbar-gutter", values}
      when values in [["stable", "both-edges"], ["both-edges", "stable"]] ->
        {:ok, Map.put(style, :scrollbar_gutter, :both_edges)}

      {"scrollbar-color", ["auto"]} ->
        {:ok, Map.put(style, :scrollbar_color, :auto)}

      {"scrollbar-color", [_, _]} ->
        [thumb, track] = terms

        with {:ok, thumb} <- HtmlValidator.parse_css_color(thumb, style),
             {:ok, track} <- HtmlValidator.parse_css_color(track, style) do
          colors =
            Enum.map([thumb, track], fn color ->
              if color == :transparent, do: {0, 0, 0, 0.0}, else: color
            end)

          {:ok, Map.put(style, :scrollbar_color, List.to_tuple(colors))}
        end

      _ ->
        :error
    end
  end

  @doc "Returns the pinned Chromium classic and thin scrollbar thicknesses in points."
  @spec thickness(map()) :: number()
  def thickness(style) do
    case Map.get(style, :scrollbar_width, :auto) do
      :none -> 0.0
      :thin -> 7.5
      :auto -> 11.25
    end
  end

  @doc false
  @spec prepare(term()) :: {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare(geometry) do
    case geometry do
      %{
        vertical: vertical,
        horizontal: horizontal,
        viewport_width: width,
        viewport_height: height,
        client_width: client_width,
        client_height: client_height,
        scroll_width: scroll_width,
        scroll_height: scroll_height
      }
      when is_boolean(vertical) and is_boolean(horizontal) ->
        offset = Map.get(geometry, :scroll_offset, %{x: 0.0, y: 0.0})
        thickness = Map.get(geometry, :scrollbar_thickness, 11.25)
        gutter = Map.get(geometry, :gutter_width, if(vertical, do: thickness, else: 0.0))
        left = Map.get(geometry, :client_left, 0.0)

        valid? =
          case offset do
            %{x: x, y: y} ->
              map_size(offset) == 2 and
                Enum.all?(
                  Map.keys(geometry),
                  &(&1 in [
                      :vertical,
                      :horizontal,
                      :viewport_width,
                      :viewport_height,
                      :client_width,
                      :client_height,
                      :scroll_width,
                      :scroll_height,
                      :scroll_offset,
                      :scrollbar_thickness,
                      :scrollbar_color,
                      :gutter_width,
                      :client_left
                    ])
                ) and
                Enum.all?(
                  [width, height, client_width, client_height, scroll_width, scroll_height, x, y],
                  fn number ->
                    case HtmlValidator.normalize_geometry_number(number) do
                      {:ok, value} -> value >= 0
                      _ -> false
                    end
                  end
                ) and thickness in [0.0, 7.5, 11.25] and
                left in [0.0, thickness] and gutter in [0.0, thickness, thickness * 2] and
                valid_colors?(Map.get(geometry, :scrollbar_color, :auto)) and
                client_width == max(width - gutter, 0) and
                client_height == max(height - if(horizontal, do: thickness, else: 0), 0) and
                scroll_width >= client_width and scroll_height >= client_height and
                x <= max(scroll_width - client_width, 0) and
                y <= max(scroll_height - client_height, 0)

            _ ->
              false
          end

        if valid? do
          parts =
            geometry
            |> Map.merge(%{
              scroll_offset: offset,
              scrollbar_thickness: thickness,
              client_left: left
            })
            |> parts()
            |> Enum.map(fn part ->
              case Map.get(geometry, :scrollbar_color, :auto) do
                {thumb, track} ->
                  Map.put(part, :color, if(part.kind == :track, do: track, else: thumb))

                :auto ->
                  part
              end
            end)

          # Paint rectangles may extend outside a tiny host before clipping. Their
          # coordinates still share the existing geometry budget and finite bounds.
          case Enum.all?(parts, fn part ->
                 coordinates = Map.take(part, [:x, :y, :width, :height]) |> Map.values()
                 clip = Map.get(part, :paint_clip)

                 clip_coordinates =
                   if clip,
                     do: Map.values(clip) ++ [clip.x + clip.width, clip.y + clip.height],
                     else: []

                 Enum.all?(
                   coordinates ++
                     [part.x + part.width, part.y + part.height, part.border_radius] ++
                     clip_coordinates,
                   &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
                 )
               end) do
            true ->
              {:ok, parts}

            false ->
              Diagnostics.error(
                :layout,
                :resource_limit_exceeded,
                "scrollbar artwork exceeds max_numeric_magnitude; reduce the viewport dimensions",
                operation: :prepare_scrollbar,
                module: __MODULE__,
                source: "scrollbar geometry"
              )
          end
        else
          invalid_geometry()
        end

      _ ->
        invalid_geometry()
    end
  end

  @doc "Settles automatic popover gutters from laid-out content and the previous axis state."
  @spec popover_geometry([map()], map(), map(), map() | nil) ::
          {:paint | :relayout, map()}
  def popover_geometry(boxes, content, padding, previous) do
    alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, PageGeometry}

    vertical = previous != nil and previous.vertical
    horizontal = previous != nil and previous.horizontal
    viewport_width = content.width + padding.left + padding.right
    viewport_height = content.height + padding.top + padding.bottom
    origin_x = content.x - padding.left

    {right, bottom} =
      Enum.reduce(boxes, {content.x, content.top}, fn box, {right, bottom} ->
        if box.type == :page_break do
          {right, bottom}
        else
          geometry =
            if box.type == :image,
              do: Map.merge(box, Map.get(box, :clip, %{})),
              else: box

          {_top, box_bottom} = PageGeometry.box_vertical_bounds(geometry)

          {max(right, geometry.x + geometry.width), min(bottom, box_bottom)}
        end
      end)

    scroll_width = max(right - origin_x + padding.right, 0.0)

    scroll_height =
      max(content.content_height, content.top - bottom) + padding.top + padding.bottom

    # There are only two axes. Freezing an appearing gutter permits at most
    # two relayouts, including when narrowing content creates more line boxes.
    next_horizontal =
      horizontal or
        DrawingGeometry.snap_css_pixel(scroll_width) >
          viewport_width - if(vertical, do: 11.25, else: 0.0)

    next_vertical =
      vertical or
        DrawingGeometry.snap_css_pixel(scroll_height) >
          viewport_height - if(horizontal, do: 11.25, else: 0.0)

    client_width = max(viewport_width - if(next_vertical, do: 11.25, else: 0.0), 0.0)
    client_height = max(viewport_height - if(next_horizontal, do: 11.25, else: 0.0), 0.0)

    geometry = %{
      viewport_width: viewport_width,
      viewport_height: viewport_height,
      client_width: client_width,
      client_height: client_height,
      scroll_width: max(scroll_width, client_width),
      scroll_height: max(scroll_height, client_height),
      vertical: next_vertical,
      horizontal: next_horizontal,
      scroll_offset: %{x: 0.0, y: 0.0}
    }

    if next_horizontal == horizontal and next_vertical == vertical,
      do: {:paint, geometry},
      else: {:relayout, geometry}
  end

  defp valid_colors?(colors) do
    case colors do
      :auto ->
        true

      {thumb, track} ->
        NativeElixirPdfUtilities.Validators.WriterValidator.valid_color?(thumb) and
          NativeElixirPdfUtilities.Validators.WriterValidator.valid_color?(track)

      _ ->
        false
    end
  end

  defp invalid_geometry do
    Diagnostics.error(
      :layout,
      :invalid_layout,
      "scrollbar geometry requires finite nonnegative viewport, client and scrolling dimensions, axis states and offsets within the scrolling range",
      operation: :prepare_scrollbar,
      module: __MODULE__,
      source: "scrollbar geometry"
    )
  end

  defp parts(geometry) do
    # Fixed Chromium 152 Fluent classic/light profile, not resource ceilings:
    # 15px gutters, 18px buttons, 9px thumbs, 17px minimum thumb length.
    # See ui/native_theme/native_theme_fluent.{h,cc} and Blink's matching theme.
    axes =
      if(geometry.vertical, do: [:vertical], else: []) ++
        if(geometry.horizontal, do: [:horizontal], else: [])

    thickness = geometry.scrollbar_thickness
    parts = Enum.flat_map(axes, &scrollbar_parts(&1, geometry))

    if geometry.vertical and geometry.horizontal do
      parts ++
        [
          %{
            kind: :track,
            x: max(geometry.viewport_width - thickness, 0),
            y: geometry.viewport_height - geometry.client_height - thickness,
            width: thickness,
            height: thickness,
            border_radius: 0.0
          }
        ]
    else
      parts
    end
  end

  defp scrollbar_parts(axis, geometry) do
    thickness = geometry.scrollbar_thickness
    thin? = thickness != 11.25

    {x, y, width, height, span, total} =
      case axis do
        :vertical ->
          {geometry.viewport_width - thickness, geometry.viewport_height - geometry.client_height,
           thickness, geometry.client_height, geometry.client_height, geometry.scroll_height}

        :horizontal ->
          {geometry.client_left, geometry.viewport_height - geometry.client_height - thickness,
           geometry.client_width, thickness, geometry.client_width, geometry.scroll_width}
      end

    if span > 0 do
      span_pixels = round(span / 0.75)

      button =
        cond do
          thin? -> 0
          span_pixels < 36 -> div(span_pixels, 2)
          true -> 18
        end

      track = max(span_pixels - 2 * button, 0)
      length = min(track, max(round(span_pixels / max(total / 0.75, 1) * track), 17))
      parts = [%{kind: :track, x: x, y: y, width: width, height: height, border_radius: 0.0}]

      parts =
        if button > 0 do
          parts ++
            Enum.map([:back, :forward], &scrollbar_arrow(axis, geometry, span_pixels, button, &1))
        else
          parts
        end

      if total > span and length > 0 and geometry.client_width > 0 and geometry.client_height > 0 do
        offset =
          if axis == :vertical, do: geometry.scroll_offset.y, else: geometry.scroll_offset.x

        position = trunc(offset / max(total - span, 0.75) * max(track - length, 0))

        {thumb_x, thumb_y, thumb_width, thumb_height} =
          case axis do
            :vertical ->
              {x + 2.25, y + (span_pixels - button - length - position) * 0.75, thickness - 4.5,
               length * 0.75}

            :horizontal ->
              {x + (button + position) * 0.75, y + 2.25, length * 0.75, thickness - 4.5}
          end

        parts ++
          [
            %{
              kind: :thumb,
              x: thumb_x,
              y: thumb_y,
              width: thumb_width,
              height: thumb_height,
              border_radius: min(thumb_width, thumb_height) / 2
            }
          ]
      else
        parts
      end
    else
      []
    end
  end

  defp scrollbar_arrow(axis, geometry, span, button, end_part) do
    # NativeThemeFluent::GetArrowRect and NativeThemeBase::PathForArrow's
    # font-independent triangle fallback. Linux has no Segoe Fluent Icons.
    scale = max(15, button) / 18
    side = ceil(9 * scale)
    side = side + rem(min(15, button) - side, 2)
    offset = round(if(end_part == :back, do: -scale, else: scale))
    start = if(end_part == :back, do: 0, else: span - button)

    {left, top} =
      case axis do
        :vertical ->
          {geometry.viewport_width / 0.75 - 15 + Float.floor((15 - min(15, side)) / 2),
           start + Float.floor((button - min(button, side)) / 2) + offset}

        :horizontal ->
          {geometry.client_left / 0.75 + start + Float.floor((button - min(button, side)) / 2) +
             offset, geometry.viewport_height / 0.75 - 15 + Float.floor((15 - min(15, side)) / 2)}
      end

    # GetArrowRect clamps its centered square to the actual button rectangle
    # before PathForArrow derives the triangle from that clamped dimension.
    long_side = min(button, side)
    short_side = min(15, side)
    arrow = div(long_side, 2) + 1

    {left, top, width, height, direction} =
      case axis do
        :vertical ->
          bottom = top + long_side - div(arrow, 2) + 1
          top = if(end_part == :back, do: bottom - arrow, else: 2 * top + long_side - bottom)
          {left, top, short_side, arrow, if(end_part == :back, do: :up, else: :down)}

        :horizontal ->
          edge = left + div(arrow, 2)
          left = if(end_part == :forward, do: edge, else: 2 * left + long_side - edge - arrow)
          {left, top, arrow, short_side, if(end_part == :back, do: :left, else: :right)}
      end

    paint_clip =
      case axis do
        :vertical ->
          %{
            x: geometry.viewport_width - 11.25,
            y: geometry.viewport_height - (start + button) * 0.75,
            width: 11.25,
            height: button * 0.75
          }

        :horizontal ->
          %{x: geometry.client_left + start * 0.75, y: 0.0, width: button * 0.75, height: 11.25}
      end

    %{
      kind: :arrow,
      paint_clip: paint_clip,
      direction: direction,
      x: left * 0.75,
      y: geometry.viewport_height - (top + height) * 0.75,
      width: width * 0.75,
      height: height * 0.75,
      border_radius: 0.0
    }
  end
end
