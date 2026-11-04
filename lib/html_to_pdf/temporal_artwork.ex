defmodule NativeElixirPdfUtilities.HtmlToPdf.TemporalArtwork do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, Font, PageGeometry}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @calendar_path Path.expand("../../priv/html_to_pdf/temporal/calendar_icon.svg", __DIR__)
  @time_path Path.expand("../../priv/html_to_pdf/temporal/time_icon.svg", __DIR__)
  @external_resource @calendar_path
  @external_resource @time_path
  @calendar_svg @calendar_path |> File.read!() |> String.replace("WindowText", "#000000")
  @time_svg @time_path |> File.read!() |> String.replace("WindowText", "#000000")

  @doc false
  @spec picker_source(String.t()) :: binary()
  def picker_source(kind), do: if(kind == "time", do: @time_svg, else: @calendar_svg)

  @doc false
  @spec boxes(map(), number(), number(), number(), number()) :: [map()]
  def boxes(style, x, y, width, height) do
    display = style._temporal_display
    border = style.border_widths
    padding = style.padding
    content_x = x + border.left + padding.left
    content_y = y + border.bottom + padding.bottom
    content_width = max(width - border.left - border.right - padding.left - padding.right, 0.0)
    content_height = max(height - border.top - border.bottom - padding.top - padding.bottom, 0.0)
    editor_width = max(content_width - display.indicator_width, 0.0)
    # Flex alignment centers the complete fields wrapper, including its fixed
    # one CSS-pixel top and bottom padding. Authored height can be smaller.
    editor_top = content_y + (content_height + display.editor_height) / 2
    baseline = editor_top - 0.75 - display.baseline_depth

    clip = %{
      x: content_x,
      y: content_y,
      width: editor_width,
      height: content_height,
      border_radius: 0.0
    }

    {glyphs, _offset} =
      Enum.map_reduce(display.parts, 0.0, fn part, offset ->
        box = %{
          type: HtmlValidator.reserve_layout_box(:text),
          role: :temporal_field,
          text: part.text,
          x: content_x + offset + part.padding,
          y: baseline,
          width: max(part.width - 2 * part.padding, 0.0),
          font: Font.pdf_name(style.font_face),
          font_face: style.font_face,
          font_size: style.font_size,
          line_height: style.line_height,
          letter_spacing: style.letter_spacing,
          color: part.color,
          snap_to_css_pixel_grid: true,
          form_owner: Map.get(style, :_form_owner),
          clip: clip
        }

        {box, offset + part.width}
      end)

    glyph_group = %{
      type: HtmlValidator.reserve_layout_box(:glyph_group),
      actual_text: display.value,
      role: :temporal_value,
      x: content_x,
      y: content_y,
      width: editor_width,
      height: content_height,
      form_owner: Map.get(style, :_form_owner),
      boxes: glyphs
    }

    picker =
      case Map.get(style, :_temporal_picker_image) do
        nil ->
          []

        image ->
          content_size = display.indicator_content_size

          rendered_height =
            if display.kind == "time", do: content_size, else: content_size * 15 / 16

          [
            %{
              type: HtmlValidator.reserve_layout_box(:image),
              role: :temporal_picker,
              image: image,
              x:
                content_x + content_width - display.indicator_width + display.indicator_margin +
                  display.indicator_padding,
              y: content_y + (content_height - content_size) / 2 + content_size - rendered_height,
              width: content_size,
              height: rendered_height,
              snap_to_css_pixel_grid: true,
              form_owner: Map.get(style, :_form_owner),
              clip: %{x: x, y: y, width: width, height: height, border_radius: 0.0}
            }
          ]
      end

    # PDF scalar appearances validate ordinary text paint independently. Static
    # documents use the existing ActualText group for the canonical HTML value.
    value =
      if display.forms_mode == :interactive and Map.get(style, :_form_owner),
        do: glyphs,
        else: [glyph_group]

    if style.border_radius > 0 do
      DrawingGeometry.clip_boxes(
        value ++ picker,
        PageGeometry.inset_rounded_clip(
          %{x: x, y: y, width: width, height: height, border_radius: style.border_radius},
          border
        )
      )
    else
      value ++ picker
    end
  end
end
