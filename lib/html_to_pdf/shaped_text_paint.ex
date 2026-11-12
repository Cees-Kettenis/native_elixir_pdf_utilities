defmodule NativeElixirPdfUtilities.HtmlToPdf.ShapedTextPaint do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, Font, UnicodeBidi, UnicodeTextData}

  alias NativeElixirPdfUtilities.Validators.{
    CssTextSpacingValidator,
    FontValidator,
    HtmlValidator,
    TextShapingValidator
  }

  @doc "Expands already admitted text into positioned glyphs while retaining logical extraction."
  @spec prepare([map()]) :: {:ok, [map()], map()} | {:error, {atom(), map()}}
  def prepare(pages) do
    FontValidator.with_budget(fn ->
      planned? =
        Enum.any?(pages, fn page ->
          Enum.any?(page.boxes, fn box ->
            case box do
              %{type: :text} = box
              when not is_map_key(box, :font_face) and
                     not is_map_key(box, :bidi_direction) and
                     not is_map_key(box, :bidi_levels) and
                     not is_map_key(box, :_text_line) ->
                false

              %{type: :text} ->
                requires_plan?(box) or Map.get(box, :_text_line) != nil

              _ ->
                false
            end
          end)
        end)

      if planned? do
        Enum.reduce_while(pages, {:ok, [], %{}}, fn page, {:ok, pages, faces} ->
          height = Map.get(page, :paint_frame, %{height: elem(page.size, 1)}).height

          result =
            Enum.reduce_while(page.boxes, {:ok, [], faces, 0, nil}, fn box,
                                                                       {:ok, boxes, faces, depth,
                                                                        previous_line} ->
              case box.type do
                :actual_text_start ->
                  {:cont, {:ok, [[box] | boxes], faces, depth + 1, previous_line}}

                :actual_text_end ->
                  {:cont, {:ok, [[box] | boxes], faces, depth - 1, previous_line}}

                :text ->
                  face = Map.get(box, :font_face, %{})
                  required? = requires_plan?(box)

                  if required? do
                    current_line = Map.get(box, :_text_line)

                    break_join? =
                      Map.get(box, :_overflow_marker, false) or
                        current_line == nil or current_line != previous_line

                    case glyphs(box, height, depth, break_join?) do
                      {:ok, glyphs} ->
                        faces =
                          if Enum.any?(glyphs, &Map.has_key?(&1, :glyph_ids)),
                            do: Map.put(faces, face.id, face),
                            else: faces

                        {:cont,
                         {:ok, [glyphs | boxes], faces, depth,
                          next_source_line(box, previous_line, current_line)}}

                      error ->
                        {:halt, error}
                    end
                  else
                    case {Map.get(box, :_text_line), Map.get(box, :_overflow_marker, false)} do
                      {nil, _} ->
                        {:cont,
                         {:ok, [[box] | boxes], faces, depth,
                          next_source_line(box, previous_line, nil)}}

                      {_, true} ->
                        {:cont,
                         {:ok, [[box] | boxes], faces, depth,
                          next_source_line(box, previous_line, nil)}}

                      {line, false} ->
                        painted = actual_text(box, [box], depth, line != previous_line)

                        {:cont,
                         {:ok, [painted | boxes], faces, depth,
                          next_source_line(box, previous_line, line)}}
                    end
                  end

                _ ->
                  {:cont, {:ok, [[box] | boxes], faces, depth, previous_line}}
              end
            end)

          case result do
            {:ok, boxes, faces, _, _} ->
              {:cont,
               {:ok, [%{page | boxes: boxes |> Enum.reverse() |> List.flatten()} | pages], faces}}

            error ->
              {:halt, error}
          end
        end)
        |> case do
          {:ok, pages, faces} -> {:ok, Enum.reverse(pages), faces}
          error -> error
        end
      else
        {:ok, pages, %{}}
      end
    end)
  end

  # An ordinary run already uses the writer's bounded CSS advance plan.
  # Reuse this dispatch for whole-page admission and individual expansion.
  defp requires_plan?(box) do
    face = Map.get(box, :font_face, %{})

    not Map.has_key?(box, :glyph_ids) and box.font_size > 0 and
      (Map.get(box, :bidi_direction) == :rtl or Map.has_key?(box, :bidi_levels) or
         TextShapingValidator.required?(face, box.text) or
         (Map.get(face, :type) == :embedded and
            (Map.get(box, :word_spacing, 0) != 0 or Map.get(box, :_justification_spacing, 0) != 0)))
  end

  # Paint-only shadow glyphs cannot interrupt the logical line's join state.
  defp next_source_line(box, previous, current) do
    if Map.get(box, :role) in [:text_shadow, :text_emphasis], do: previous, else: current
  end

  @doc "Returns an admitted text box's visual glyph positions relative to its baseline origin."
  @spec plan(map()) :: {:ok, map()} | {:error, {atom(), map()}}
  def plan(box) do
    direction = Map.get(box, :bidi_direction, :auto)

    resolved =
      case Map.fetch(box, :bidi_levels) do
        {:ok, levels} -> {:ok, %{levels: levels, order: UnicodeBidi.reorder_levels(levels)}}
        :error -> UnicodeBidi.resolve(box.text, direction)
      end

    with {:ok, bidi} <- resolved do
      characters =
        Map.get(box, :_shaping_characters) ||
          Enum.map(String.codepoints(box.text), &%{displayed: &1, fragments: []})

      characters =
        Enum.zip(characters, bidi.levels)
        |> Enum.map(fn {character, level} ->
          <<codepoint::utf8>> = character.displayed
          mirrored = if rem(level, 2) == 1, do: UnicodeTextData.mirror(codepoint), else: codepoint
          %{character | displayed: <<mirrored::utf8>>}
        end)

      options =
        if Map.has_key?(box, :bidi_levels) and bidi.levels != [],
          do: [direction: if(rem(hd(bidi.levels), 2) == 1, do: :rtl, else: :ltr)],
          else: if(direction in [:ltr, :rtl], do: [direction: direction], else: [])

      with {:ok, shaped} <-
             Font.css_glyph_positions(
               box.text,
               box.font_face,
               box.font_size,
               Map.get(box, :letter_spacing, 0),
               Keyword.put(options, :characters, characters)
             ),
           :ok <- TextShapingValidator.validate_visible_glyphs(shaped.glyphs) do
        ranks = bidi.order |> Enum.with_index() |> Map.new()
        levels = List.to_tuple(bidi.levels)

        glyphs =
          shaped.glyphs
          |> Enum.map_reduce(0.0, fn glyph, y ->
            {%{glyph | y_offset: glyph.y_offset + y}, y + glyph.y_advance}
          end)
          |> elem(0)
          |> Enum.with_index()
          |> Enum.reject(fn {glyph, _} -> glyph.ignorable? end)

        glyphs =
          Enum.sort_by(glyphs, fn {glyph, index} ->
            level = elem(levels, glyph.scalar_start)

            {Map.get(ranks, glyph.scalar_start, 0),
             if(rem(level, 2) == 1, do: -index, else: index)}
          end)

        spacing = Map.get(box, :letter_spacing, 0)
        justification = Map.get(box, :_justification_spacing, 0)
        boundaries = MapSet.new(Map.get(box, :_justification_boundaries, []))

        {positioned, {advance, _}} =
          Enum.map_reduce(glyphs, {0.0, nil}, fn {glyph, _index}, {x, previous} ->
            x =
              x +
                if(previous != nil and previous != glyph.cluster_start,
                  do:
                    spacing +
                      if(MapSet.member?(boundaries, max(previous, glyph.cluster_start)),
                        do: justification,
                        else: 0.0
                      ),
                  else: 0
                )

            word_spacing =
              Enum.count(glyph.codepoints, &CssTextSpacingValidator.word_separator?/1) *
                Map.get(box, :word_spacing, 0)

            advance = glyph.x_advance + word_spacing

            paint_y =
              glyph.y_offset +
                if(Map.get(glyph, :vertical_center?, false),
                  do:
                    Map.get(box, :line_baseline_depth, box.font_size) -
                      Map.get(box, :line_box_height, box.font_size) / 2,
                  else: 0
                )

            positioned =
              Map.merge(glyph, %{
                paint_x: x + glyph.x_offset + word_spacing / 2,
                paint_y: paint_y,
                paint_advance: advance,
                bidi_level: elem(levels, glyph.scalar_start)
              })

            {positioned, {x + advance, glyph.cluster_start}}
          end)

        {:ok, %{shaped: shaped, bidi: bidi, glyphs: positioned, advance: advance}}
      end
    end
  end

  defp glyphs(box, height, depth, break_join?) do
    with {:ok, paint} <- plan(box) do
      if unchanged_glyphs?(box, paint.shaped, paint.bidi) do
        boxes =
          if not Map.get(box, :_overflow_marker, false) and
               (Map.has_key?(box, :bidi_levels) or Map.has_key?(box, :_text_line)),
             do: actual_text(box, [box], depth, break_join?),
             else: [box]

        {:ok, boxes}
      else
        {left, baseline} =
          DrawingGeometry.text_position(box, Map.get(box, :drawing_page_height, height))

        boxes =
          Enum.map(paint.glyphs, fn glyph ->
            advance = glyph.paint_advance
            text = <<hd(glyph.codepoints)::utf8>>
            fragments = glyph.fragments

            font =
              NativeElixirPdfUtilities.Validators.EmojiPresentationValidator.prepare(
                box.font_face,
                List.to_string(glyph.codepoints)
              )

            source = %{
              source_fragments: Enum.filter(fragments, &Map.has_key?(&1, :owner)),
              field_scalar_fragments: Enum.filter(fragments, &Map.has_key?(&1, :scalar_owner)),
              text: text,
              displayed_unicode: hd(glyph.codepoints),
              glyph_ids: [glyph.glyph]
            }

            child =
              box
              |> Map.drop([
                :_shaping_characters,
                :_css_actual_text,
                :bidi_levels,
                :bidi_direction,
                :_justification_boundaries
              ])
              |> Map.merge(%{
                text: text,
                font_face: font,
                glyph_ids: [glyph.glyph],
                x: left + glyph.paint_x,
                y: baseline + glyph.paint_y,
                width: advance,
                annotation_width: advance,
                letter_spacing: 0,
                word_spacing: 0,
                _justification_spacing: 0,
                snap_to_css_pixel_grid: false,
                css_source_positioned: true,
                native_glyph_runs: if(fragments == [], do: [], else: [source])
              })

            child =
              if Map.has_key?(glyph, :glyph_basis),
                do: Map.put(child, :glyph_basis, glyph.glyph_basis),
                else: child

            child
          end)

        TextShapingValidator.validate_paint_positions(boxes)
        boxes = batch_boxes(boxes)
        {:ok, actual_text(box, boxes, depth, break_join?)}
      end
    end
  end

  defp actual_text(box, boxes, depth, break_join?) do
    if depth > 0,
      do: boxes,
      else:
        [%{type: :actual_text_start, text: box.text, break_join?: break_join?}] ++
          boxes ++ [%{type: :actual_text_end}]
  end

  defp unchanged_glyphs?(box, shaped, bidi) do
    codepoints = String.to_charlist(box.text)

    Map.get(box, :word_spacing, 0) == 0 and Map.get(box, :_justification_spacing, 0) == 0 and
      bidi.order == Enum.to_list(0..(length(codepoints) - 1)//1) and
      length(codepoints) == length(shaped.glyphs) and
      Enum.zip(codepoints, shaped.glyphs)
      |> Enum.all?(fn {codepoint, glyph} ->
        glyph.codepoints == [codepoint] and glyph.glyph == Map.get(box.font_face.cmap, codepoint) and
          glyph.x_offset == 0 and glyph.y_offset == 0 and glyph.y_advance == 0 and
          not Map.has_key?(glyph, :glyph_basis) and not glyph.ignorable?
      end)
  end

  defp batch_boxes(boxes) do
    boxes
    |> Enum.chunk_while(
      [],
      fn box, chunk ->
        case chunk do
          [] ->
            {:cont, [box]}

          [previous | _] ->
            if not Map.has_key?(box, :glyph_basis) and not Map.has_key?(previous, :glyph_basis) and
                 box.x == previous.x + previous.width and box.y == previous.y and
                 Map.get(box.font_face, :emoji_presentation) ==
                   Map.get(previous.font_face, :emoji_presentation),
               do: {:cont, [box | chunk]},
               else: {:cont, Enum.reverse(chunk), [box]}
        end
      end,
      fn chunk -> if chunk == [], do: {:cont, []}, else: {:cont, Enum.reverse(chunk), []} end
    )
    |> Enum.map(fn chunk ->
      first = hd(chunk)
      width = Enum.reduce(chunk, 0, &(&1.width + &2))
      HtmlValidator.reserve_layout_box(:text)

      Map.merge(first, %{
        text: Enum.map_join(chunk, & &1.text),
        glyph_ids: Enum.flat_map(chunk, & &1.glyph_ids),
        glyph_advances: Enum.map(chunk, & &1.width),
        width: width,
        annotation_width: width,
        native_glyph_runs: Enum.flat_map(chunk, & &1.native_glyph_runs)
      })
    end)
  end
end
