defmodule NativeElixirPdfUtilities.HtmlToPdf.TextShadow do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, Visibility}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Paints text shadows from back to front without adding logical text or link owners."
  @spec boxes(map(), map(), [map()], map()) :: [map()]
  def boxes(box, style, decorations, metadata) do
    style
    |> Map.get(:text_shadow, [])
    |> Enum.reverse()
    |> Enum.flat_map(fn shadow ->
      color = if shadow.color == :current_color, do: style.color, else: shadow.color

      if color == :transparent or box.text == "" or box.font_size == 0 do
        []
      else
        if shadow.blur == 0 do
          # Glyph groups already provide an empty ActualText paint scope. Reuse
          # it so shadows retain exactly the selected shaping and source font.
          text =
            box
            |> Map.drop([
              :link_url,
              :link_destination,
              :_text_line,
              :_css_actual_text,
              :_math_group,
              :_math_actual_text,
              :_hyphenation_source,
              :_hyphenation_range
            ])
            |> Map.put(:color, color)
            |> Map.put(:role, :text_shadow)
            |> Map.put(:text_baseline_offset, -shadow.y)
            |> DrawingGeometry.translate_box(shadow.x, -shadow.y)

          HtmlValidator.reserve_layout_box(:text)

          group =
            %{
              type: HtmlValidator.reserve_layout_box(:glyph_group),
              actual_text: "",
              boxes: [text],
              role: :text_shadow,
              x: box.x + shadow.x,
              y: box.y - shadow.y,
              width: box.annotation_width,
              height: Map.get(box, :line_height, box.font_size)
            }
            |> Visibility.mark(style)
            |> Map.merge(metadata)

          lines =
            Enum.map(decorations, fn line ->
              HtmlValidator.reserve_layout_box(:rect)

              line
              |> Map.put(:fill_color, if(line.fill_color == nil, do: nil, else: color))
              |> Map.put(:stroke_color, color)
              |> Map.put(:role, :text_shadow)
              |> Map.update!(:text_baseline_offset, &(&1 - shadow.y))
              |> then(fn line ->
                if Map.has_key?(line, :border_colors),
                  do:
                    Map.update!(line, :border_colors, fn colors ->
                      Map.new(colors, fn {side, _} -> {side, color} end)
                    end),
                  else: line
              end)
              |> DrawingGeometry.translate_box(shadow.x, -shadow.y)
            end)

          lines ++ [group]
        else
          NativeElixirPdfUtilities.HtmlToPdf.TextShadowBlur.boxes(
            box,
            style,
            shadow,
            color,
            decorations,
            metadata
          )
        end
      end
    end)
  end
end
