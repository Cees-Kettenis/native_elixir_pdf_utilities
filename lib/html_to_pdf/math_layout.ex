defmodule NativeElixirPdfUtilities.HtmlToPdf.MathLayout do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.FontValidator
  alias NativeElixirPdfUtilities.Validators.MathLayoutValidator

  @doc """
  Measures a styled MathML token/row tree after validator preparation.

  Coordinates are points relative to the alphabetic baseline, with positive y
  upwards. Layout and ink extents are separate. Drawing items retain the selected
  face, glyph ID and original Unicode for the writer's ActualText contract.
  """
  @spec measure(term(), map()) :: {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def measure(node, context \\ %{}) do
    with_context(fn ->
      with {:ok, prepared} <- MathLayoutValidator.prepare_element(node, context) do
        {:ok, measure_prepared(prepared)}
      end
    end)
  end

  defp measure_prepared(prepared) do
    measured =
      case prepared do
        %{kind: :glyph, glyph: glyph, style: style} ->
          measured = glyph_geometry(glyph)
          %{measured | glyphs: Enum.map(measured.glyphs, &Map.put(&1, :color, style.color))}

        %{kind: :space, width: width, ascent: ascent, descent: descent} ->
          %{
            width: width,
            ascent: ascent,
            descent: descent,
            italic_correction: 0.0,
            top_accent_attachment: nil,
            glyphs: []
          }

        %{kind: :flow, children: children, baseline_depth: baseline, line_height: height} ->
          children = Enum.map(children, &measure_prepared/1)

          glyphs =
            children
            |> Enum.with_index()
            |> Enum.flat_map(fn {line, index} ->
              Enum.map(line.glyphs, &%{&1 | y: &1.y - index * height})
            end)

          items =
            children
            |> Enum.with_index()
            |> Enum.flat_map(fn {line, index} ->
              Enum.map(line.drawing_items, &%{&1 | y: &1.y - index * height})
            end)

          %{
            width: Enum.max(Enum.map(children, & &1.width), fn -> 0.0 end),
            ascent: if(children == [], do: 0.0, else: baseline),
            descent: if(children == [], do: 0.0, else: height * length(children) - baseline),
            ink_ascent: Enum.max(Enum.map(children, & &1.ink_ascent), fn -> 0.0 end),
            ink_descent:
              children
              |> Enum.with_index()
              |> Enum.map(fn {line, index} -> line.ink_descent + index * height end)
              |> Enum.max(fn -> 0.0 end),
            italic_correction: 0.0,
            top_accent_attachment: nil,
            glyphs: glyphs,
            drawing_items: items
          }

        %{children: children, kind: kind} ->
          children = Enum.map(children, &measure_prepared/1)
          # A token is a text run. Italic row spacing applies between boxes, not
          # between letters of a multi-character token.
          children =
            if kind == :token and length(children) != 1,
              do: Enum.map(children, &%{&1 | italic_correction: 0.0}),
              else: children

          children =
            if kind == :token do
              Enum.zip(children, prepared.children)
              |> Enum.map(fn {child, prepared} ->
                adjustment = Map.get(prepared, :kerning_before, 0.0)

                %{
                  child
                  | width: child.width + adjustment,
                    glyphs: Enum.map(child.glyphs, &%{&1 | x: &1.x + adjustment})
                }
                |> Map.put(
                  :drawing_items,
                  Enum.map(child.drawing_items, &%{&1 | x: &1.x + adjustment})
                )
              end)
            else
              children
            end

          case {kind, children} do
            {:token, [child]} ->
              child

            _ ->
              {:ok, measured} = row(children)

              {items, _, _} =
                Enum.reduce(children, {[], 0.0, 0.0}, fn child, {items, offset, previous} ->
                  slanted = child.italic_correction != 0
                  offset = if slanted, do: offset, else: offset + previous
                  positioned = Enum.map(child.drawing_items, &%{&1 | x: &1.x + offset})

                  {items ++ positioned, offset + child.width,
                   if(slanted, do: child.italic_correction, else: 0.0)}
                end)

              measured
              |> Map.put(
                :italic_correction,
                if(kind == :token, do: 0.0, else: measured.italic_correction)
              )
              |> Map.put(:actual_text, Enum.map_join(children, & &1.actual_text))
              |> Map.put(:drawing_items, items)
              |> Map.put(
                :ink_ascent,
                Enum.max(Enum.map(children, & &1.ink_ascent), fn -> 0.0 end)
              )
              |> Map.put(
                :ink_descent,
                Enum.max(Enum.map(children, & &1.ink_descent), fn -> 0.0 end)
              )
          end
      end

    measured =
      measured
      |> Map.put(:ink_ascent, Map.get(measured, :ink_ascent, measured.ascent))
      |> Map.put(:ink_descent, Map.get(measured, :ink_descent, measured.descent))
      |> Map.put(:min_content_width, measured.width)
      |> Map.put(:max_content_width, measured.width)
      |> Map.put(
        :drawing_items,
        Map.get(measured, :drawing_items, Enum.map(measured.glyphs, &Map.put(&1, :kind, :glyph)))
      )
      |> Map.put(
        :actual_text,
        Map.get(
          prepared,
          :actual_text,
          Map.get(measured, :actual_text, Enum.map_join(measured.glyphs, & &1.text))
        )
      )

    if Map.get(prepared, :root, false),
      do: measured,
      else: decorate_geometry(measured, prepared.style)
  end

  defp decorate_geometry(measured, style) do
    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    margin = Map.get(style, :margin, zero)
    padding = Map.get(style, :padding, zero)
    border = Map.get(style, :border_widths, zero)
    left = margin.left + padding.left + border.left
    right = margin.right + padding.right + border.right
    above = margin.top + padding.top + border.top
    below = margin.bottom + padding.bottom + border.bottom

    content_width =
      case Map.get(style, :width) do
        width when is_number(width) -> width
        _ -> measured.width
      end

    content_height =
      case Map.get(style, :height) do
        height when is_number(height) -> height
        _ -> measured.ascent + measured.descent
      end

    background =
      if Map.get(style, :background_color) not in [nil, :transparent] or
           Enum.any?(Map.values(border), &(&1 > 0)) do
        [
          %{
            kind: :background,
            x: margin.left,
            y: -content_height + measured.ascent - padding.bottom - border.bottom,
            width: content_width + padding.left + padding.right + border.left + border.right,
            height: content_height + padding.top + padding.bottom + border.top + border.bottom,
            style: style
          }
        ]
      else
        []
      end

    measured
    |> Map.put(:width, content_width + left + right)
    |> Map.put(:ascent, measured.ascent + above)
    |> Map.put(:descent, content_height - measured.ascent + below)
    |> Map.put(:glyphs, Enum.map(measured.glyphs, &%{&1 | x: &1.x + left}))
    |> Map.put(
      :drawing_items,
      background ++ Enum.map(measured.drawing_items, &%{&1 | x: &1.x + left})
    )
    |> Map.put(:min_content_width, content_width + left + right)
    |> Map.put(:max_content_width, content_width + left + right)
  end

  @doc false
  @spec glyph(term(), term(), term(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def glyph(text, font, size, opts \\ []) do
    with_context(fn ->
      with {:ok, glyph} <- MathLayoutValidator.prepare_glyph(text, font, size, opts) do
        {:ok, glyph_geometry(glyph)}
      end
    end)
  end

  @doc false
  @spec stretch(term(), term(), term(), term(), term(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def stretch(text, font, size, axis, target, opts \\ []) do
    with_context(fn ->
      with {:ok, prepared} <-
             MathLayoutValidator.prepare_stretch(text, font, size, axis, target, opts) do
        case prepared do
          %{kind: :glyph, glyph: glyph} ->
            {:ok, glyph_geometry(glyph)}

          %{kind: :assembly, parts: parts} ->
            {glyphs, _offset} =
              Enum.map_reduce(parts, 0.0, fn part, offset ->
                measured = glyph_geometry(part.metric)
                [glyph] = measured.glyphs

                glyph =
                  if axis == :horizontal,
                    do: %{glyph | x: offset},
                    else: %{glyph | y: offset + measured.descent}

                {glyph,
                 offset + part.full_advance * size / prepared.source.metrics.units_per_em -
                   prepared.overlap}
              end)

            {width, ascent, descent} =
              if axis == :vertical do
                {Enum.max(
                   Enum.map(
                     prepared.metric_parts,
                     &(&1.metric.advance * size / &1.metric.units_per_em)
                   )
                 ), prepared.stretch_size, 0.0}
              else
                {prepared.stretch_size,
                 Enum.max(
                   Enum.map(prepared.metric_parts, fn part ->
                     elem(part.metric.bounds, 3) * size / part.metric.units_per_em
                   end)
                 ),
                 Enum.max(
                   Enum.map(prepared.metric_parts, fn part ->
                     -elem(part.metric.bounds, 1) * size / part.metric.units_per_em
                   end)
                 )}
              end

            {value, pixels} = prepared.italic
            italic = value * size / prepared.source.metrics.units_per_em + pixels

            {:ok,
             %{
               width: width,
               ascent: ascent,
               descent: descent,
               italic_correction: italic,
               top_accent_attachment: nil,
               glyphs: glyphs
             }}
        end
      end
    end)
  end

  defp glyph_geometry(glyph) do
    scale = glyph.font_size / glyph.units_per_em
    {_xmin, ymin, _xmax, ymax} = glyph.bounds
    ascent = ymax * scale
    descent = -ymin * scale
    width = glyph.advance * scale

    italic =
      case glyph.italic do
        nil -> 0.0
        {value, pixels} -> value * scale + pixels
      end

    accent =
      case glyph.accent do
        nil -> nil
        {value, pixels} -> value * scale + pixels
      end

    %{
      width: width,
      ascent: ascent,
      descent: descent,
      italic_correction: italic,
      top_accent_attachment: accent,
      glyphs: [
        %{
          x: 0.0,
          y: 0.0,
          text: glyph.text,
          glyph_id: glyph.glyph_id,
          font_face: glyph.font_face,
          font_size: glyph.font_size,
          width: width,
          height: ascent + descent
        }
      ]
    }
  end

  @doc false
  @spec row(term()) :: {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def row(children) do
    with_context(fn ->
      with :ok <- MathLayoutValidator.validate_row(children) do
        {glyphs, offset, previous} =
          Enum.reduce(children, {[], 0.0, 0.0}, fn child, {glyphs, offset, previous} ->
            slanted? = child.italic_correction != 0
            offset = if slanted?, do: offset, else: offset + previous
            positioned = Enum.map(child.glyphs, &%{&1 | x: &1.x + offset})

            {Enum.reverse(positioned, glyphs), offset + child.width,
             if(slanted?, do: child.italic_correction, else: 0.0)}
          end)

        {:ok,
         %{
           width: offset + previous,
           ascent: Enum.max(Enum.map(children, & &1.ascent), fn -> 0.0 end),
           descent: Enum.max(Enum.map(children, & &1.descent), fn -> 0.0 end),
           italic_correction: previous,
           glyphs: Enum.reverse(glyphs)
         }}
      end
    end)
  end

  @doc false
  @spec with_context((-> result)) :: result | {:error, {atom(), Diagnostics.diagnostic()}}
        when result: term()
  def with_context(fun) do
    result =
      HtmlValidator.with_render_budget(fn ->
        Font.with_measurement_cache(fn -> FontValidator.with_budget(fun) end)
      end)

    case result do
      {:error, {reason, diagnostic}} ->
        {:error, {reason, Map.put(diagnostic, :operation, :measure_math)}}

      result ->
        result
    end
  end
end
