defmodule NativeElixirPdfUtilities.HtmlToPdf.PageGeometry do
  @moduledoc """
  Shared page-option cascade helpers and vertical layout-box geometry.

  Page-size and margin convenience APIs delegate admission to PageGeometryValidator.
  This module applies canonical geometry to drawing and page-option cascades.
  """

  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.PageGeometryValidator

  # Fixed CSS length invariant: one pixel is 1/96 inch, or 0.75 PDF points.
  @css_pixel_points 0.75

  @doc "Rounds page margins to the renderer's CSS pixel grid."
  @spec snap_css_pixel_margins(margins()) :: margins()
  def snap_css_pixel_margins(margins) do
    Map.new(margins, fn {side, value} ->
      {side, round(value / @css_pixel_points) * @css_pixel_points}
    end)
  end

  @doc "Returns the whole CSS-pixel source height of a validated print fragmentainer; its physical PDF clip retains the original fractional height."
  @spec css_fragmentainer_source_height(number()) :: float()
  def css_fragmentainer_source_height(height),
    do: Float.ceil(height / @css_pixel_points) * @css_pixel_points

  @doc false
  @spec snap_box_to_css_pixel_grid(map()) :: map()
  def snap_box_to_css_pixel_grid(box) do
    case Map.get(box, :snap_to_css_pixel_grid, false) do
      true ->
        left = snap_css_pixel(box.x)
        bottom = snap_css_pixel(box.y)
        right = snap_css_pixel(box.x + box.width)
        top = snap_css_pixel(box.y + box.height)

        %{box | x: left, y: bottom, width: max(right - left, 0.0), height: max(top - bottom, 0.0)}

      false ->
        box
    end
  end

  @doc false
  @spec snap_css_pixel(number()) :: float()
  def snap_css_pixel(value) do
    Float.floor(value / @css_pixel_points + 0.5) * @css_pixel_points
  end

  @doc "Returns the content or padding edge clip after subtracting each edge from a validated rounded host."
  @spec inset_rounded_clip(map(), map()) :: map()
  def inset_rounded_clip(host, insets) do
    radius = min(host.border_radius, min(host.width, host.height) / 2)

    clip = %{
      x: host.x + insets.left,
      y: host.y + insets.bottom,
      width: max(host.width - insets.left - insets.right, 0.0),
      height: max(host.height - insets.top - insets.bottom, 0.0),
      border_radius: max(radius - insets.left, 0.0)
    }

    case Map.get(host, :border_radii) do
      nil ->
        if length(Enum.uniq(Map.values(insets))) == 1 do
          clip
        else
          # CSS edge curves retain Blink's fixed 1/64 CSS-pixel LayoutUnit
          # precision before the enclosing rectangle is snapped for paint.
          insets =
            Map.new(insets, fn {side, extent} ->
              {side, Float.floor(extent / @css_pixel_points * 64) / 64 * @css_pixel_points}
            end)

          Map.put(clip, :border_radii, %{
            bottom_left: {max(radius - insets.left, 0.0), max(radius - insets.bottom, 0.0)},
            bottom_right: {max(radius - insets.right, 0.0), max(radius - insets.bottom, 0.0)},
            top_right: {max(radius - insets.right, 0.0), max(radius - insets.top, 0.0)},
            top_left: {max(radius - insets.left, 0.0), max(radius - insets.top, 0.0)}
          })
        end

      radii ->
        corners = %{
          top_left: {:left, :top},
          top_right: {:right, :top},
          bottom_right: {:right, :bottom},
          bottom_left: {:left, :bottom}
        }

        radii =
          Map.new(radii, fn {corner, {x, y}} ->
            {horizontal, vertical} = Map.fetch!(corners, corner)

            {corner,
             {max(x - Map.fetch!(insets, horizontal), 0.0),
              max(y - Map.fetch!(insets, vertical), 0.0)}}
          end)

        clip |> Map.put(:border_radius, 0.0) |> Map.put(:border_radii, radii)
    end
  end

  @doc false
  @spec rounded_border_contours(map(), number()) :: [map()] | nil
  def rounded_border_contours(box, page_height) do
    solid? =
      case Map.get(box, :border_styles) do
        nil ->
          true

        styles ->
          Enum.all?(Map.values(styles), fn style ->
            style == :solid or
              (Map.get(box, :role) == :temporal_host and style in [:inset, :outset])
          end)
      end

    if Map.get(box, :role) in [:native_listbox, :css_menu, :temporal_host] and
         box.border_radius > 0 and
         length(Enum.uniq(Map.values(Map.get(box, :border_widths, %{all: box.stroke_width})))) ==
           1 and
         solid? do
      inner = %{
        box
        | x: box.x + box.stroke_width,
          y: box.y + box.stroke_width,
          width: max(box.width - 2 * box.stroke_width, 0.0),
          height: max(box.height - 2 * box.stroke_width, 0.0),
          border_radius: max(box.border_radius - box.stroke_width, 0.0)
      }

      snapped = snap_box_to_css_pixel_grid(%{inner | y: page_height - inner.y - inner.height})
      inner = %{snapped | y: page_height - snapped.y - snapped.height}

      # Preserve the writer's existing print-matrix edge-tie adjustment.
      # Appearance bounds include precisely the same painted outer contour.
      Enum.map([box, inner], &%{&1 | x: &1.x - 0.0001, y: &1.y + 0.0001})
    end
  end

  @doc "Returns pixel-snapped outer, inner and center contours for validated per-edge curved CSS borders."
  @spec curved_border_geometry(map(), number()) :: map()
  def curved_border_geometry(box, page_height) do
    scale = @css_pixel_points

    outer = %{
      x: box.x / scale,
      y: (page_height - box.y - box.height) / scale,
      width: box.width / scale,
      height: box.height / scale,
      radius: min(box.border_radius / scale, min(box.width, box.height) / (2 * scale))
    }

    widths =
      Map.get(box, :border_widths, %{
        top: box.stroke_width,
        right: box.stroke_width,
        bottom: box.stroke_width,
        left: box.stroke_width
      })
      |> Map.new(fn {side, extent} -> {side, extent / scale} end)

    insets = [
      Map.new(widths, fn {side, _} -> {side, 0} end),
      widths,
      # Chromium CenterOutsets constructs integer edge widths before snapping.
      Map.new(widths, fn {side, extent} -> {side, trunc(extent * 0.5)} end)
    ]

    contours =
      Enum.map(insets, fn inset ->
        x = Float.floor(outer.x + inset.left + 0.5)
        y = Float.floor(outer.y + inset.top + 0.5)
        width = max(Float.floor(outer.x + outer.width - inset.right + 0.5) - x, 0)
        height = max(Float.floor(outer.y + outer.height - inset.bottom + 0.5) - y, 0)

        radii =
          case Map.get(box, :border_radii) do
            nil ->
              %{
                bottom_left:
                  {max(outer.radius - inset.left, 0), max(outer.radius - inset.top, 0)},
                bottom_right:
                  {max(outer.radius - inset.right, 0), max(outer.radius - inset.top, 0)},
                top_right:
                  {max(outer.radius - inset.right, 0), max(outer.radius - inset.bottom, 0)},
                top_left: {max(outer.radius - inset.left, 0), max(outer.radius - inset.bottom, 0)}
              }

            radii ->
              source = %{
                bottom_left: {:top_left, :left, :top},
                bottom_right: {:top_right, :right, :top},
                top_right: {:bottom_right, :right, :bottom},
                top_left: {:bottom_left, :left, :bottom}
              }

              Map.new(source, fn {corner, {original, horizontal, vertical}} ->
                {x, y} = Map.fetch!(radii, original)

                {corner,
                 {max(x / scale - Map.fetch!(inset, horizontal), 0.0),
                  max(y / scale - Map.fetch!(inset, vertical), 0.0)}}
              end)
          end

        %{
          x: x,
          y: y,
          width: width,
          height: height,
          radius: max(outer.radius - inset.top, 0),
          radii: radii
        }
      end)

    [outer, inner, center] = contours
    tl = {outer.x, outer.y}
    tr = {outer.x + outer.width, outer.y}
    bl = {outer.x, outer.y + outer.height}
    br = {outer.x + outer.width, outer.y + outer.height}

    # Each side's miter rays continue through the curved corners.
    # An isolated group already clips outside edge ties.
    bias = if Map.has_key?(box, :css_border_surface), do: 0, else: 0.0001

    sides =
      [
        {:top, tl, tr, {widths.left, widths.top}, {-widths.right, widths.top}, outer.height / 2,
         outer.width, widths.left + widths.right},
        {:right, tr, br, {-widths.right, widths.top}, {-widths.right, -widths.bottom},
         outer.width / 2, outer.height, widths.top + widths.bottom},
        {:bottom, br, bl, {-widths.right, -widths.bottom}, {widths.left, -widths.bottom},
         outer.height / 2, outer.width, widths.left + widths.right},
        {:left, bl, tl, {widths.left, -widths.bottom}, {widths.left, widths.top}, outer.width / 2,
         outer.height, widths.top + widths.bottom}
      ]
      |> Map.new(fn {side, {ax, ay}, {bx, by}, {dx, dy}, {ex, ey}, depth, extent, adjacent} ->
        fraction = if widths[side] > 0, do: depth / widths[side], else: 0.0
        fraction = if adjacent > 0, do: min(fraction, extent / adjacent), else: fraction

        # Keep hard side clips on the interior side of source raster ties.
        {ax, ay, bx, by} =
          case side do
            :top -> {ax, ay + bias, bx, by + bias}
            :right -> {ax - bias, ay, bx - bias, by}
            :bottom -> {ax, ay - bias, bx, by - bias}
            :left -> {ax + bias, ay, bx + bias, by}
          end

        {side,
         Enum.uniq([
           {ax, ay},
           {bx, by},
           {bx + ex * fraction, by + ey * fraction},
           {ax + dx * fraction, ay + dy * fraction}
         ])}
      end)

    %{
      outer: outer,
      inner: inner,
      center: center,
      width: box.stroke_width / scale,
      widths: widths,
      sides: sides,
      page_height: page_height / scale
    }
  end

  @type page_size_name :: PageGeometryValidator.page_size_name()
  @type orientation :: PageGeometryValidator.orientation()
  @type page_size_input :: PageGeometryValidator.page_size_input()
  @type margins :: PageGeometryValidator.margins()
  @type margin_input :: PageGeometryValidator.margin_input()

  @doc """
  Resolves a renderer page-size value to a positive `{width, height}` point tuple.
  """
  @spec normalize_page_size(term()) :: {:ok, {float(), float()}} | {:error, :invalid_page_size}
  defdelegate normalize_page_size(page_size), to: PageGeometryValidator

  @doc """
  Resolves a nonnegative renderer margin into top, right, bottom, and left points.
  """
  @spec normalize_margins(term()) :: {:ok, margins()} | {:error, :invalid_margin}
  defdelegate normalize_margins(margin), to: PageGeometryValidator

  @doc """
  Returns a scalar for uniform margins and the four-sided map for asymmetric margins.
  """
  @spec compact_margins(margins()) :: float() | margins()
  def compact_margins(%{top: top, right: right, bottom: bottom, left: left} = margins) do
    case top == right and top == bottom and top == left do
      true -> top
      false -> margins
    end
  end

  @doc """
  Merges a later page-margin declaration over an earlier declaration.
  """
  @spec merge_margin_defaults(term(), term()) :: term()
  def merge_margin_defaults(previous, override) do
    case override do
      override when is_map(override) ->
        previous_margins =
          case previous do
            previous when is_map(previous) ->
              previous

            nil ->
              %{}

            previous ->
              case normalize_margins(previous) do
                {:ok, margins} -> margins
                {:error, :invalid_margin} -> %{}
              end
          end

        Map.merge(previous_margins, override)

      override ->
        override
    end
  end

  @doc """
  Merges extracted page options while cascading partial margin longhands by side.
  """
  @spec merge_page_options(keyword(), keyword()) :: keyword()
  def merge_page_options(previous, override) do
    merged = Keyword.merge(previous, override)

    case Keyword.fetch(override, :margin) do
      {:ok, override_margin} ->
        margin =
          previous
          |> Keyword.get(:margin)
          |> merge_margin_defaults(override_margin)

        Keyword.put(merged, :margin, margin)

      :error ->
        merged
    end
  end

  @doc """
  Converts an applicable CSS page-margin value to a renderer margin value.
  """
  @spec css_margin_option(String.t()) :: number() | String.t() | margins() | nil
  defdelegate css_margin_option(value), to: PageGeometryValidator

  @doc """
  Converts an applicable CSS page-size descriptor to a renderer page-size value.
  """
  @spec css_page_size_option(String.t()) :: page_size_input() | nil
  defdelegate css_page_size_option(value), to: PageGeometryValidator

  @doc """
  Returns whether a four-sided margin leaves positive printable page geometry.
  """
  @spec valid_printable_area?({number(), number()}, margins()) :: boolean()
  def valid_printable_area?(page_size, margins) do
    HtmlValidator.validate_printable_area(page_size, margins) == :ok
  end

  @doc """
  Returns the vertical `{top, bottom}` bounds for a drawing or nonpainting flow marker.

  Text bounds account for both the font ascent and the full line height so all
  rendering stages agree about the vertical space occupied by a line.
  """
  @spec box_vertical_bounds(term()) :: {number(), number()}
  def box_vertical_bounds(box) do
    bounds =
      case box do
        %{type: :drawing_group, role: :writing_mode, y: y, writing_extent: %{height: height}} ->
          {y + height, y}

        %{type: type, y: y, height: height}
        when type in [
               :rect,
               :image,
               :responsive_image,
               :link_region,
               :flow_marker,
               :glyph_group,
               :drawing_group
             ] and
               is_number(y) and
               is_number(height) ->
          {y + height, y}

        %{type: :text, y: y, line_baseline_depth: depth, line_box_height: height}
        when is_number(y) and is_number(depth) and is_number(height) ->
          {y + depth, y + depth - height}

        %{type: :text, y: y, font_size: font_size, line_height: line_height}
        when is_number(y) and is_number(font_size) and is_number(line_height) ->
          {y + font_size, y + font_size - line_height}

        %{type: :text, y: y, font_size: font_size}
        when is_number(y) and is_number(font_size) ->
          {y + font_size, y}

        %{type: :text, y: y, line_height: line_height}
        when is_number(y) and is_number(line_height) ->
          {y + line_height, y}

        %{type: :page_break, y: y} when is_number(y) ->
          {y, y}

        _ ->
          {0.0, 0.0}
      end

    case box do
      %{css_parent_clips: clips} ->
        Enum.reduce(clips, bounds, fn {_id, clip}, {top, bottom} ->
          top = min(top, clip.y + clip.height)
          {top, min(top, max(bottom, clip.y))}
        end)

      _ ->
        bounds
    end
  end

  @doc "Resolves the variable dimension of three page-margin boxes using CSS Paged Media flex factors."
  @spec margin_axis([map()], number()) :: [number()]
  def margin_axis([a, b, c], available) do
    [a, b, c] =
      boxes =
      Enum.map([a, b, c], fn box ->
        Map.merge(
          %{min: 0.0, max: 0.0, fixed: nil, generated: true, minimum: 0.0, maximum: :infinity},
          box
        )
      end)

    widths =
      if not b.generated do
        [left, right] = margin_pair(a, c, available)
        [left, 0.0, right]
      else
        middle =
          if b.fixed == nil do
            ac = %{
              min: 2 * max(a.min, c.min),
              max: 2 * max(a.max, c.max),
              fixed: if(a.fixed != nil and c.fixed != nil, do: 2 * max(a.fixed, c.fixed))
            }

            margin_pair(b, ac, available) |> hd()
          else
            b.fixed
          end

        [
          a.fixed || max((available - middle) / 2, 0.0),
          middle,
          c.fixed || max((available - middle) / 2, 0.0)
        ]
      end

    constrained =
      Enum.zip_with(boxes, widths, fn box, width ->
        cap = if box.maximum == :infinity, do: width, else: min(width, box.maximum)
        value = max(cap, box.minimum)
        if value == width, do: box, else: Map.put(box, :fixed, value)
      end)

    if constrained == boxes, do: widths, else: margin_axis(constrained, available)
  end

  defp margin_pair(a, b, available) do
    cond do
      a.fixed != nil and b.fixed != nil ->
        [a.fixed, b.fixed]

      a.fixed != nil ->
        [a.fixed, max(available - a.fixed, 0.0)]

      b.fixed != nil ->
        [max(available - b.fixed, 0.0), b.fixed]

      true ->
        {left, right, fa, fb} =
          cond do
            a.max + b.max < available -> {a.max, b.max, a.max, b.max}
            a.min + b.min < available -> {a.min, b.min, a.max - a.min, b.max - b.min}
            true -> {a.min, b.min, a.min, b.min}
          end

        {fa, fb} = if fa + fb == 0, do: {1.0, 1.0}, else: {fa, fb}
        free = available - left - right
        [max(left + free * fa / (fa + fb), 0.0), max(right + free * fb / (fa + fb), 0.0)]
    end
  end
end
