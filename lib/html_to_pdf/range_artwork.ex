defmodule NativeElixirPdfUtilities.HtmlToPdf.RangeArtwork do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry

  @doc false
  @spec boxes(map(), number(), number(), number(), number()) :: [map()]
  def boxes(style, x, y, width, height) do
    if width <= 0 or height <= 0 do
      []
    else
      # Fixed Linux Chromium NativeThemeBase geometry in CSS points. Native
      # thumb dimensions do not scale with caller font size or control height.
      disabled = Map.get(style, :_range_disabled, false)

      accent =
        if disabled,
          do: {203 / 255, 203 / 255, 203 / 255},
          else: Map.get(style, :accent_color, {0.0, 117 / 255, 1.0})

      track_height = min(6.0, height)
      track_y = y + (height - track_height) / 2
      border = style.border_widths
      padding = style.padding
      content_width = max(width - border.left - border.right - padding.left - padding.right, 0.0)

      content_height =
        max(height - border.top - border.bottom - padding.top - padding.bottom, 0.0)

      offset = trunc((content_width - 12.0) * style._range_state.position / 0.75 * 64) / 64 * 0.75
      thumb_left = border.left + padding.left + offset
      thumb_top = border.top + padding.top + (content_height - 12.0) / 2

      base = %{
        type: HtmlValidator.reserve_layout_box(:rect),
        role: :range_track,
        x: x + min(0.75, width / 2),
        y: track_y,
        width: max(width - 1.5, 0.0),
        height: track_height,
        fill_color: {239 / 255, 239 / 255, 239 / 255, if(disabled, do: 77 / 255, else: 1.0)},
        stroke_color: nil,
        stroke_width: 0.0,
        border_radius: track_height / 2,
        form_owner: Map.get(style, :_form_owner),
        snap_to_css_pixel_grid: false
      }

      fill = %{
        base
        | type: HtmlValidator.reserve_layout_box(:rect),
          role: :range_value,
          fill_color: accent
      }

      border = %{
        base
        | type: HtmlValidator.reserve_layout_box(:rect),
          role: :range_border,
          fill_color: nil,
          stroke_color:
            {118 / 255, 118 / 255, 118 / 255, if(disabled, do: 77 / 255, else: 128 / 255)},
          stroke_width: 0.75
      }

      thumb = %{
        base
        | type: HtmlValidator.reserve_layout_box(:rect),
          role: :range_thumb,
          x: x + thumb_left + 0.375,
          y: y + height - thumb_top - 12.0 + 0.375,
          width: 11.25,
          height: 11.25,
          border_radius: 6.0,
          fill_color: accent
      }

      parts =
        if style.appearance == :none do
          # html.css gives the thumb appearance:auto independently of its host.
          [thumb]
        else
          if base.width > 0 do
            if thumb_left + 3.0 > 0, do: [base, fill, border, thumb], else: [base, border, thumb]
          else
            [thumb]
          end
        end

      Enum.map(
        parts,
        &Map.put(&1, :range_frame, {&1.x - x, &1.y - y, width, height, thumb_left, thumb_top})
      )
    end
  end

  @doc false
  @spec snap_box(map(), number()) :: map()
  def snap_box(box, page_height) do
    {dx, dy, width, height, thumb_left, thumb_top} = box.range_frame
    frame_x = box.x - dx
    frame_y = box.y - dy
    left = PageGeometry.snap_css_pixel(frame_x)
    right = PageGeometry.snap_css_pixel(frame_x + width)
    top = page_height - PageGeometry.snap_css_pixel(page_height - frame_y - height)
    bottom = page_height - PageGeometry.snap_css_pixel(page_height - frame_y)
    width = max(right - left, 0.0)
    height = max(top - bottom, 0.0)
    track_height = min(6.0, height)
    track_y = bottom + (height - track_height) / 2
    thumb_left = PageGeometry.snap_css_pixel(thumb_left)
    thumb_top = PageGeometry.snap_css_pixel(thumb_top)

    case box.role do
      :range_thumb ->
        %{
          box
          | x: left + thumb_left + 0.375,
            y: top - thumb_top - 11.625,
            width: 11.25,
            height: 11.25,
            border_radius: 5.625
        }

      role ->
        box = %{
          box
          | x: left + min(0.75, width / 2),
            y: track_y,
            width: max(width - 1.5, 0.0),
            height: track_height,
            border_radius: track_height / 2
        }

        if role == :range_value do
          # The value extent is an internal paint clip, independent of the
          # validated overflow clip attached by layout or a containing viewport.
          Map.put(box, :range_fill_clip, %{
            x: left,
            y: track_y,
            width: max(thumb_left + 3.0, 0.0),
            height: track_height
          })
        else
          box
        end
    end
  end
end
