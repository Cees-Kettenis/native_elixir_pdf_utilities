defmodule NativeElixirPdfUtilities.HtmlToPdf.FileArtwork do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, HtmlFileValidator}

  @doc false
  @spec boxes(map(), number(), number(), number(), number()) :: [map()]
  def boxes(style, x, y, width, height) do
    display = style._file_display
    border = style.border_widths
    padding = style.padding
    content_x = x + border.left + padding.left
    content_y = y + border.bottom + padding.bottom
    content_width = max(width - border.left - border.right - padding.left - padding.right, 0.0)
    content_height = max(height - border.top - border.bottom - padding.top - padding.bottom, 0.0)
    baseline = content_y + content_height - display.baseline
    button_y = baseline - (display.button_height - display.button_baseline)

    clip = %{
      x: content_x,
      y: content_y,
      width: content_width,
      height: content_height,
      border_radius: 0.0
    }

    alpha = if display.disabled, do: 0.3, else: 1.0
    stroke = {118 / 255, 118 / 255, 118 / 255, alpha}

    button = %{
      type: HtmlValidator.reserve_layout_box(:rect),
      role: :native_file_button,
      x: content_x,
      y: button_y,
      width: display.button_width,
      height: display.button_height,
      border_radius: 1.875,
      fill_color: {239 / 255, 239 / 255, 239 / 255, alpha},
      stroke_width: 0.75,
      stroke_color: stroke,
      border_widths: %{top: 1.5, right: 1.5, bottom: 1.5, left: 1.5},
      border_styles: %{top: :solid, right: :solid, bottom: :solid, left: :solid},
      border_colors: %{top: stroke, right: stroke, bottom: stroke, left: stroke},
      snap_to_css_pixel_grid: true,
      clip: clip
    }

    filename_x = content_x + display.button_width + 3.0

    {filename_parts, _} =
      HtmlFileValidator.filename_parts(style, content_width - display.button_width - 3.0)
      |> Enum.map_reduce(filename_x, fn part, offset ->
        {{part.text, style.font_face, offset, style.color, style.letter_spacing},
         offset + part.width}
      end)

    texts =
      Enum.map(
        [
          {display.button_text, display.button_face, content_x + 6.0,
           if(display.disabled, do: {16 / 255, 16 / 255, 16 / 255, 0.3}, else: {0, 0, 0}), 0.0}
          | filename_parts
        ],
        fn {text, face, text_x, color, spacing} ->
          %{
            type: HtmlValidator.reserve_layout_box(:text),
            role: :file_label,
            text: text,
            x: text_x,
            y: baseline,
            width: Font.text_width(text, face, style.font_size, spacing),
            font: Font.pdf_name(face),
            font_face: face,
            font_size: style.font_size,
            line_height: display.label_height,
            letter_spacing: spacing,
            color: color,
            snap_to_css_pixel_grid: true,
            clip: clip
          }
        end
      )

    [button_text | filename_texts] = texts

    status =
      if length(filename_texts) > 1 do
        [
          %{
            type: HtmlValidator.reserve_layout_box(:glyph_group),
            role: :file_status,
            actual_text: Enum.map_join(filename_texts, & &1.text),
            x: filename_x,
            y: content_y,
            width: max(content_width - display.button_width - 3.0, 0.0),
            height: content_height,
            boxes: filename_texts
          }
        ]
      else
        filename_texts
      end

    if style.font_size == 0, do: [button], else: [button, button_text | status]
  end
end
