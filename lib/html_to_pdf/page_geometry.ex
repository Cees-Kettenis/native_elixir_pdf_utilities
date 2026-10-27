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

        radii = %{
          bottom_left: {max(outer.radius - inset.left, 0), max(outer.radius - inset.top, 0)},
          bottom_right: {max(outer.radius - inset.right, 0), max(outer.radius - inset.top, 0)},
          top_right: {max(outer.radius - inset.right, 0), max(outer.radius - inset.bottom, 0)},
          top_left: {max(outer.radius - inset.left, 0), max(outer.radius - inset.bottom, 0)}
        }

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
    case box do
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
  end
end
