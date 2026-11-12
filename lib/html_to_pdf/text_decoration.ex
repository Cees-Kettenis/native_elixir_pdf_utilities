defmodule NativeElixirPdfUtilities.HtmlToPdf.TextDecoration do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.{CssValueValidator, HtmlValidator}

  @doc "Keeps each wavy layer's phase continuous through its positioned inline fragments."
  @spec origins([map()], number(), (map() -> number())) :: map()
  def origins(runs, start, advance) do
    if Enum.any?(runs, fn run ->
         Enum.any?(Map.get(run.style, :_text_decorations, []), &(Map.get(&1, :style) == :wavy))
       end) do
      {origins, _} =
        Enum.reduce(runs, {%{}, start}, fn run, {origins, cursor} ->
          x = Map.get(run, :_bidi_x, cursor)

          origins =
            run.style
            |> Map.get(:_text_decorations, [])
            |> Enum.with_index()
            |> Enum.reduce(origins, fn {decoration, index}, origins ->
              if Map.get(decoration, :style) == :wavy,
                do: Map.update(origins, wave_key(decoration, index), x, &min(&1, x)),
                else: origins
            end)

          {origins, x + advance.(run)}
        end)

      origins
    else
      %{}
    end
  end

  defp wave_key(decoration, index),
    do:
      {index, Map.take(decoration, [:lines, :color, :thickness, :underline, :overline, :strike])}

  @doc "Paints admitted line decorations using their originating font metrics and source geometry."
  @spec boxes(map(), map(), map()) :: [map()]
  def boxes(box, style, metadata) do
    decorations = Map.get(style, :_text_decorations, [])

    ink =
      if Map.get(box, :text, "") != "" and
           Enum.any?(decorations, fn decoration ->
             Map.get(decoration, :skip_ink, :auto) != :none and
               Enum.any?(decoration.lines, &(&1 in [:underline, :overline]))
           end) do
        case NativeElixirPdfUtilities.HtmlToPdf.TextInk.measure(
               [
                 %{
                   text: box.text,
                   box: box,
                   style:
                     Map.merge(
                       style,
                       Map.take(box, [:font_face, :font_size, :letter_spacing, :word_spacing])
                     )
                 }
               ],
               true
             ) do
          {:ok, geometry} -> geometry.glyphs
          error -> throw({:render_resource_limit, error})
        end
      else
        []
      end

    style
    |> Map.get(:_text_decorations, [])
    |> Enum.with_index()
    |> Enum.flat_map(fn {decoration, index} ->
      if box.font_size == 0 or decoration.thickness == 0 or box.annotation_width <= 0 or
           decoration.color == :transparent do
        []
      else
        Enum.flat_map(decoration.lines, fn line ->
          position =
            case line do
              :underline -> decoration.underline - decoration.thickness
              :overline -> decoration.overline
              :line_through -> decoration.strike - decoration.thickness / 2
            end

          rect = %{
            type: HtmlValidator.reserve_layout_box(:rect),
            x: box.x,
            y: box.y + position,
            width: box.annotation_width,
            height: decoration.thickness,
            fill_color: decoration.color,
            stroke_color: decoration.color,
            stroke_width: 0.0,
            border_radius: 0.0,
            role: :text_decoration,
            text_baseline_offset: position,
            snap_to_css_pixel_grid: true
          }

          rect =
            case Map.get(decoration, :style, :solid) do
              pattern when pattern in [:dotted, :dashed] ->
                Map.merge(rect, %{
                  fill_color: nil,
                  border_widths: CssValueValidator.edges(0.0, 0.0, decoration.thickness, 0.0),
                  border_styles: CssValueValidator.edges(:none, :none, pattern, :none),
                  border_colors: CssValueValidator.edges(decoration.color)
                })

              :wavy ->
                wave =
                  NativeElixirPdfUtilities.HtmlToPdf.TextDecorationWave.geometry(
                    decoration.thickness
                  )

                gap = decoration.thickness + 0.75
                top = box.y + position + decoration.thickness

                center =
                  case line do
                    :underline -> top - gap
                    :overline -> top + gap
                    :line_through -> top
                  end

                origin =
                  metadata
                  |> Map.get(:_decoration_origins, %{})
                  |> Map.get(wave_key(decoration, index), box.x)

                Map.merge(rect, %{
                  shape: :wave,
                  fill_color: nil,
                  stroke_width: decoration.thickness,
                  height: wave.height,
                  y: center - wave.height / 2 - 0.375,
                  text_baseline_offset: center - wave.height / 2 - 0.375 - box.y,
                  wave_offset: origin - box.x
                })

              _ ->
                rect
            end

          offsets =
            if Map.get(decoration, :style, :solid) == :double do
              gap = decoration.thickness + 0.75

              case line do
                :underline -> [0.0, -gap]
                :overline -> [0.0, gap]
                :line_through -> [0.0, -Float.floor(gap / 0.75) * 0.75]
              end
            else
              [0.0]
            end

          Enum.with_index(offsets)
          |> Enum.flat_map(fn {offset, index} ->
            if index > 0, do: HtmlValidator.reserve_layout_box(:rect)

            rect = %{
              rect
              | y: rect.y + offset,
                text_baseline_offset: rect.text_baseline_offset + offset
            }

            rect =
              rect
              |> NativeElixirPdfUtilities.HtmlToPdf.Visibility.mark(style)
              |> Map.merge(metadata)

            if line == :line_through or Map.get(decoration, :skip_ink, :auto) == :none do
              [rect]
            else
              skip_ink(rect, box, ink, Map.get(decoration, :skip_ink, :auto))
            end
          end)
        end)
      end
    end)
  end

  # Project the actual contour portion inside the decoration band, then
  # subtract its horizontal span and a small UA gap from the stroke.
  defp skip_ink(rect, box, glyphs, mode) do
    gap = min(rect.height, 3.0)
    bottom = rect.y - box.y
    top = bottom + rect.height

    intervals =
      glyphs
      |> Enum.flat_map(fn glyph ->
        ideographic? =
          Enum.any?(glyph.codepoints, fn cp ->
            elem(NativeElixirPdfUtilities.HtmlToPdf.UnicodeTextData.properties(cp), 0) in ~w(Hani Hira Kana Hang Yiii)
          end)

        if mode == :auto and ideographic? do
          []
        else
          points =
            Enum.flat_map(glyph.edges, fn {{ax, ay}, {bx, by}} ->
              if max(ay, by) >= bottom and min(ay, by) <= top do
                if ay == by do
                  [ax, bx]
                else
                  low = max(min(ay, by), bottom)
                  high = min(max(ay, by), top)

                  [
                    ax + (low - ay) / (by - ay) * (bx - ax),
                    ax + (high - ay) / (by - ay) * (bx - ax)
                  ]
                end
              else
                []
              end
            end)

          if points == [],
            do: [],
            else: [{box.x + Enum.min(points) - gap, box.x + Enum.max(points) + gap}]
        end
      end)
      |> Enum.sort()

    {segments, cursor} =
      Enum.reduce(intervals, {[], rect.x}, fn {left, right}, {segments, cursor} ->
        segments =
          if left > cursor,
            do: [{cursor, min(left, rect.x + rect.width)} | segments],
            else: segments

        {segments, max(cursor, right)}
      end)

    segments =
      if cursor < rect.x + rect.width,
        do: [{cursor, rect.x + rect.width} | segments],
        else: segments

    segments
    |> Enum.reverse()
    |> Enum.filter(fn {left, right} -> right > left and left < rect.x + rect.width end)
    |> Enum.with_index(fn {left, right}, index ->
      if index > 0, do: HtmlValidator.reserve_layout_box(:rect)
      rect = %{rect | x: left, width: right - left}

      if Map.get(rect, :shape) == :wave,
        do: Map.update!(rect, :wave_offset, &(&1 + box.x - left)),
        else: rect
    end)
  end
end
