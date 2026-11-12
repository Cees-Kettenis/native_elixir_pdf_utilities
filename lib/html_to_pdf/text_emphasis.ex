defmodule NativeElixirPdfUtilities.HtmlToPdf.TextEmphasis do
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeGrapheme
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, FontFallback, ShapedTextPaint, Visibility}

  alias NativeElixirPdfUtilities.Validators.{
    CssFontControlsValidator,
    CssTextEmphasisValidator,
    FontValidator,
    HtmlValidator
  }

  @doc "Resolves an admitted emphasis mark through the existing font fallback and metrics."
  @spec metrics(map()) :: map() | nil
  def metrics(style) do
    value =
      if Map.get(style, :font_size, 0) == 0,
        do: :none,
        else: Map.get(style, :text_emphasis_style, :none)

    case value do
      value when value in [:none, {:custom, ""}] ->
        nil

      value ->
        mode = Map.get(style, :_writing_plane, Map.get(style, :writing_mode, :horizontal_tb))

        FontValidator.memo(
          {:emphasis_metrics, style.font_face, style.font_size, value, mode,
           Map.get(style, :text_orientation), Map.get(style, :text_emphasis_position)},
          fn ->
            vertical? =
              mode in [:vertical_rl, :vertical_lr] and
                Map.get(style, :text_orientation, :mixed) != :sideways

            mark =
              case value do
                {:custom, text} ->
                  text

                {fill, shape} ->
                  shape =
                    if shape == :auto, do: if(vertical?, do: :sesame, else: :circle), else: shape

                  Map.fetch!(
                    %{
                      dot: {0x2022, 0x25E6},
                      circle: {0x25CF, 0x25CB},
                      double_circle: {0x25C9, 0x25CE},
                      triangle: {0x25B2, 0x25B3},
                      sesame: {0xFE45, 0xFE46}
                    },
                    shape
                  )
                  |> elem(if(fill == :filled, do: 0, else: 1))
                  |> then(&<<&1::utf8>>)
              end

            size = style.font_size / 2

            mark_style =
              style
              |> Map.put(:writing_mode, if(vertical?, do: mode, else: :horizontal_tb))
              |> Map.put(:font_size, size)
              |> Map.put(:line_height, size)
              |> Map.put(:letter_spacing, 0.0)
              |> Map.put(:word_spacing, 0.0)
              |> Map.put(:text_orientation, :upright)
              |> Map.put(:text_combine_upright, :none)
              |> Map.update(:font_controls, %{:"variant-east-asian" => ["ruby"]}, fn controls ->
                controls = CssFontControlsValidator.effective(controls)

                variants =
                  case Map.get(controls, :"variant-east-asian", :normal) do
                    :normal -> ["ruby"]
                    values -> Enum.uniq(["ruby" | values])
                  end

                Map.put(controls, :"variant-east-asian", variants)
              end)

            with {:ok, %{children: [%{style: resolved}]}} <-
                   FontFallback.resolve(%{
                     type: :document,
                     children: [%{type: :text, text: mark, style: mark_style}]
                   }),
                 resolved =
                   if(vertical?,
                     do:
                       Map.update!(resolved, :font_face, fn face ->
                         Map.update!(face, :_vertical_text, &Map.put(&1, :emphasis, true))
                       end),
                     else: resolved
                   ),
                 {:ok, metrics} <- FontValidator.prepare_text_metrics(resolved.font_face),
                 {:ok, ink} <-
                   NativeElixirPdfUtilities.HtmlToPdf.TextInk.measure(
                     [%{text: mark, style: resolved}],
                     false
                   ) do
              face = resolved.font_face

              {left, right} =
                case ink.points do
                  [] ->
                    {0.0, 0.0}

                  points ->
                    {Enum.min(Enum.map(points, &elem(&1, 0))),
                     Enum.max(Enum.map(points, &elem(&1, 0)))}
                end

              typo = metrics.typographic_height || %{ascent: face.ascent, descent: -face.descent}
              extent = typo.ascent + typo.descent
              parent = Font.line_metrics(style.font_face)

              parent_scale =
                Font.used_size(style.font_face, style.font_size) / parent.units_per_em

              {:ok, parent_metrics} = FontValidator.prepare_text_metrics(style.font_face)

              parent_typo =
                parent_metrics.typographic_height ||
                  %{ascent: parent.ascent, descent: -parent.descent}

              parent_extent = parent_typo.ascent + parent_typo.descent
              parent_size = Font.line_size(style.font_face, style.font_size)
              parent_ascent = round(parent.ascent * parent_scale / 0.75) * 0.75
              parent_descent = round(-parent.descent * parent_scale / 0.75) * 0.75
              # Trim whole pixels of internal leading around the normalized
              # em box, matching the shared ruby/emphasis annotation metrics.
              parent_ascent =
                parent_ascent -
                  Float.floor(
                    max(parent_ascent - parent_size * parent_typo.ascent / parent_extent, 0.0) /
                      0.75
                  ) * 0.75

              parent_descent =
                parent_descent -
                  Float.floor(
                    max(parent_descent - parent_size * parent_typo.descent / parent_extent, 0.0) /
                      0.75
                  ) * 0.75

              {block, side} = Map.get(style, :text_emphasis_position, {:over, :right})

              over? =
                if vertical?,
                  do:
                    if(mode == :vertical_rl,
                      do: side == :right,
                      else: side == :left
                    ),
                  else: block == :over

              offset =
                if over?,
                  do: Float.ceil((parent_ascent + size * typo.descent / extent) / 0.75) * 0.75,
                  else: -Float.ceil((parent_descent + size * typo.ascent / extent) / 0.75) * 0.75

              %{
                text: mark,
                font: face,
                size: size,
                center: (left + right) / 2,
                offset: offset,
                over?: over?,
                height: size,
                ascent: parent_ascent + if(over?, do: size, else: 0.0),
                descent: parent_descent + if(over?, do: 0.0, else: size)
              }
            else
              error -> throw({:render_resource_limit, error})
            end
          end
        )
    end
  end

  @doc "Paints one source-free mark per eligible grapheme, sharing shaped advances through ligatures."
  @spec boxes(map(), map(), map()) :: [map()]
  def boxes(box, style, metadata) do
    mark = metrics(style)

    color =
      case Map.get(style, :text_emphasis_color, :current_color) do
        :current_color -> style.color
        color -> color
      end

    if mark == nil or color == :transparent or box.text == "" or box.font_size == 0 do
      []
    else
      case ShapedTextPaint.plan(box) do
        {:ok, plan} ->
          scalars =
            Enum.reduce(plan.glyphs, %{}, fn glyph, scalars ->
              count = max(glyph.scalar_end - glyph.scalar_start, 1)
              HtmlValidator.reserve_render_resource(:max_work, count, :layout)
              cursor = glyph.paint_x - glyph.x_offset

              Enum.reduce(0..(count - 1), scalars, fn part, scalars ->
                physical = if rem(glyph.bidi_level, 2) == 1, do: count - part - 1, else: part

                bounds =
                  {cursor + glyph.paint_advance * physical / count,
                   cursor + glyph.paint_advance * (physical + 1) / count}

                Map.update(scalars, glyph.scalar_start + part, bounds, fn {left, right} ->
                  {min(left, elem(bounds, 0)), max(right, elem(bounds, 1))}
                end)
              end)
            end)

          {marks, _scalar} =
            Enum.map_reduce(UnicodeGrapheme.graphemes(box.text), 0, fn grapheme, scalar ->
              codepoints = String.to_charlist(grapheme)
              count = length(codepoints)

              boundaries =
                Enum.map(scalar..(scalar + count - 1), &Map.get(scalars, &1, {0.0, 0.0}))

              left = Enum.min(Enum.map(boundaries, &elem(&1, 0)))
              right = Enum.max(Enum.map(boundaries, &elem(&1, 1)))
              center = (left + right) / 2

              text =
                if CssTextEmphasisValidator.receives?(hd(codepoints)) do
                  [
                    %{
                      type: HtmlValidator.reserve_layout_box(:text),
                      text: mark.text,
                      font: Font.pdf_name(mark.font),
                      font_face: mark.font,
                      font_size: mark.size,
                      color: color,
                      x: box.x + center - mark.center,
                      y: box.y + mark.offset,
                      width: 0.0,
                      role: :text_emphasis,
                      text_baseline_offset: mark.offset,
                      snap_to_css_pixel_grid: true
                    }
                    |> Map.merge(metadata)
                    |> Visibility.mark(style)
                  ]
                else
                  []
                end

              {text, scalar + count}
            end)

          marks = List.flatten(marks)

          if marks == [] do
            []
          else
            [
              %{
                type: HtmlValidator.reserve_layout_box(:glyph_group),
                actual_text: "",
                boxes: marks,
                x: box.x,
                y: box.y,
                width: box.annotation_width,
                height: style.line_height,
                role: :text_emphasis
              }
              |> Map.merge(metadata)
              |> Visibility.mark(style)
            ]
          end

        error ->
          throw({:render_resource_limit, error})
      end
    end
  end
end
