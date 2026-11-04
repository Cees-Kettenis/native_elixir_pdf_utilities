defmodule NativeElixirPdfUtilities.Validators.FormAppearanceValidator do
  @moduledoc "Canonical closed source editor geometry and embedded font programs for ordinary field appearances."
  import Bitwise
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    Font,
    FontCache,
    FontFallback,
    Layout,
    FormAppearanceComposition
  }

  alias NativeElixirPdfUtilities.Validators.{
    FontValidator,
    FormValidator,
    FormInspectionValidator,
    HtmlFormValidator,
    HtmlValidator,
    ListBoxAppearanceValidator,
    WriterValidator
  }

  @styles %{
    white_space: [:normal, :nowrap, :pre, :pre_wrap, :pre_line, :break_spaces],
    text_align: [:left, :right, :center, :justify, :start, :end],
    word_break: [:normal, :break_all, :keep_all, :break_word],
    overflow_wrap: [:normal, :break_word, :anywhere],
    line_break: [:auto, :normal, :strict, :loose, :anywhere, :break_word]
  }

  @doc "Captures genuine resolved editor font and geometry before field projection, including an initially empty editor."
  @spec capture(map(), map()) :: map() | nil
  def capture(field, control) do
    case {field.type, Map.get(control, :editor_style)} do
      {type, %{font_face: %{type: :embedded} = primary} = style}
      when type in [:text, :choice] and style.font_size > 0 and style.line_height > 0 ->
        {width, height} = field.appearance.size
        padding = style.padding
        border = style.border_widths
        clip_width = max(width - padding.left - padding.right - border.left - border.right, 0.0)
        clip_height = max(height - padding.top - padding.bottom - border.top - border.bottom, 0.0)

        clip_width =
          if Map.has_key?(style, :_number_state),
            do: max(clip_width - 11.25, 0.0),
            else: clip_width

        offset = HtmlFormValidator.initial_control_content_offset(style, clip_height)

        origin =
          Map.get(field.appearance, :paint_frame, %{x: 0, y: 0, width: width, height: height})

        listbox = Enum.find_value(field.appearance.boxes, &Map.get(&1, :listbox_rows))
        leaves = text_leaves(field.appearance.boxes)

        fonts =
          ([primary | Enum.map(leaves, & &1.font_face)] ++
             if(listbox, do: listbox.fonts, else: []))
          |> Enum.filter(&match?(%{type: :embedded, data: data} when is_binary(data), &1))
          |> Enum.uniq_by(& &1.id)
          |> Enum.map(&Map.take(&1, [:family, :data, :weight, :style]))

        clip = %{
          x: border.left + padding.left + origin.x,
          y: border.bottom + padding.bottom + origin.y,
          width: clip_width,
          height: clip_height
        }

        top = clip.y + clip.height - offset
        # The existing painter supplies the actual used clip and baseline for
        # nonempty editors. Empty editors use the same canonical baseline rule.
        {clip, top} =
          case leaves do
            [first | _] ->
              source_clip = Map.get(first, :clip, clip)
              observed = Map.merge(style, Map.take(first, [:font_face, :font_size, :line_height]))
              {source_clip, first.y + HtmlValidator.text_baseline_depth(observed)}

            [] ->
              case listbox && Enum.find_value(field.appearance.boxes, &Map.get(&1, :clip)) do
                nil -> {clip, top}
                observed -> {observed, observed.y + observed.height - offset}
              end
          end

        %{
          size: {width, height},
          origin: origin,
          x: clip.x,
          top: top,
          clip: clip,
          style:
            Map.take(style, [
              :font_size,
              :color,
              :letter_spacing,
              :line_height,
              :text_align,
              :white_space,
              :word_break,
              :overflow_wrap,
              :line_break
            ]),
          fonts: fonts,
          listbox: listbox && Map.take(listbox, [:rows]),
          projections: []
        }

      _ ->
        nil
    end
  end

  @doc "Retains the same genuine editor frame under canonical field projection; fonts and source geometry stay source-local."
  @spec project_field(map(), tuple(), [[tuple()]], tuple()) :: map()
  def project_field(field, matrix, polygons, bounds) do
    case Map.get(field, :source_editor) do
      nil ->
        field

      editor ->
        Map.put(field, :source_editor, %{
          editor
          | projections:
              editor.projections ++
                [
                  %{matrix: matrix, polygons: polygons, bounds: bounds}
                ]
        })
    end
  end

  @doc "Serializes only source producer editor facts and approved full font bytes into ordinary PDF objects."
  @spec encode_controls([map()], pos_integer()) ::
          {:ok, [map()], [tuple()], pos_integer()} | {:error, {atom(), map()}}
  def encode_controls(controls, next) do
    FormInspectionValidator.with_active_scope(fn ->
      Enum.reduce_while(controls, {:ok, [], [], next, %{}}, fn control,
                                                               {:ok, controls, objects, next,
                                                                fonts} ->
        case Map.get(control, :source_editor) do
          editor when is_map(editor) ->
            with {:ok, _} <- validate(editor),
                 {:ok, descriptors, added, next, fonts} <-
                   Enum.reduce_while(editor.fonts, {:ok, [], [], next, fonts}, fn face,
                                                                                  {:ok,
                                                                                   descriptors,
                                                                                   added, next,
                                                                                   fonts} ->
                     # These bytes already belong to the validated editor. An
                     # exact binary key reuses its font object across controls
                     # without hashing the full font again for each field.
                     source = face.data

                     {ref, next, added, fonts} =
                       case Map.fetch(fonts, source) do
                         {:ok, ref} ->
                           {ref, next, added, fonts}

                         :error ->
                           ref = {:ref, {next, 0}}
                           compressed = FontCache.compressed(face.data)

                           stream =
                             if WriterValidator.use_compressed_stream?(
                                  byte_size(face.data),
                                  byte_size(compressed),
                                  1_024
                                ),
                                do: {:stream, %{"Filter" => {:name, "FlateDecode"}}, compressed},
                                else: {:stream, %{}, face.data}

                           {ref, next + 1,
                            [
                              {next, 0, stream}
                              | added
                            ], Map.put(fonts, source, ref)}
                       end

                     dictionary = %{
                       "Family" => {:string, face.family},
                       "Weight" => face.weight,
                       "Style" => {:name, Atom.to_string(face.style)},
                       "Data" => ref
                     }

                     {:cont, {:ok, [dictionary | descriptors], added, next, fonts}}
                   end),
                 dictionary = %{
                   "Type" => {:name, "NEPUSourceEditor"},
                   "Version" => 1,
                   "Size" => Tuple.to_list(editor.size),
                   "Origin" => encode_rect(editor.origin),
                   "X" => editor.x,
                   "Top" => editor.top,
                   "Clip" => encode_rect(editor.clip),
                   "FontSize" => editor.style.font_size,
                   "LineHeight" => editor.style.line_height,
                   "LetterSpacing" => editor.style.letter_spacing,
                   "Color" => Tuple.to_list(editor.style.color),
                   "Style" =>
                     Map.new(@styles, fn {key, _} ->
                       {Atom.to_string(key),
                        {:name, Atom.to_string(Map.get(editor.style, key, default_style(key)))}}
                     end),
                   "Fonts" => Enum.reverse(descriptors),
                   "Projections" =>
                     Enum.map(editor.projections, fn projection ->
                       %{
                         "Matrix" => Tuple.to_list(projection.matrix),
                         "Bounds" => Tuple.to_list(projection.bounds),
                         "Polygons" =>
                           Enum.map(
                             projection.polygons,
                             &Enum.flat_map(&1, fn {x, y} -> [x, y] end)
                           )
                       }
                     end)
                 },
                 dictionary =
                   if(Map.get(editor, :listbox),
                     do:
                       Map.put(
                         dictionary,
                         "ListBox",
                         ListBoxAppearanceValidator.encode(editor.listbox)
                       ),
                     else: dictionary
                   ) do
              {:cont,
               {:ok, [Map.put(control, :source_editor_pdf, dictionary) | controls],
                added ++ objects, next, fonts}}
            else
              error -> {:halt, error}
            end

          _ ->
            {:cont, {:ok, [control | controls], objects, next, fonts}}
        end
      end)
      |> case do
        {:ok, controls, objects, next, _fonts} -> {:ok, Enum.reverse(controls), objects, next}
        error -> error
      end
    end)
  end

  @doc "Loads a persisted source editor through canonical font/style/geometry validation, without font discovery or resolution."
  @spec prepare(map(), term()) :: {:ok, map() | nil} | {:error, {atom(), map()}}
  def prepare(document, value) do
    FormInspectionValidator.with_scope(document, fn -> prepare_editor(document, value) end)
  end

  defp prepare_editor(document, value) do
    case value do
      nil ->
        {:ok, nil}

      %{
        "Type" => {:name, "NEPUSourceEditor"},
        "Version" => 1,
        "Size" => [width, height],
        "Origin" => origin,
        "X" => x,
        "Top" => top,
        "Clip" => clip,
        "FontSize" => size,
        "LineHeight" => line,
        "LetterSpacing" => spacing,
        "Color" => color,
        "Style" => style,
        "Fonts" => fonts,
        "Projections" => projections
      }
      when is_map(style) and is_list(fonts) and is_list(projections) and is_list(color) ->
        with true <-
               Enum.all?(
                 Map.keys(value),
                 &(&1 in [
                     "Type",
                     "Version",
                     "Size",
                     "Origin",
                     "X",
                     "Top",
                     "Clip",
                     "FontSize",
                     "LineHeight",
                     "LetterSpacing",
                     "Color",
                     "Style",
                     "Fonts",
                     "Projections",
                     "ListBox"
                   ])
               ) and
                 Enum.all?(
                   Map.keys(style),
                   &(&1 in Enum.map(Map.keys(@styles), fn key -> Atom.to_string(key) end))
                 ) and
                 length(fonts) <= Limits.get(:max_font_count) and
                 length(projections) <= Limits.get(:max_pdf_form_depth),
             {:ok, listbox} <- ListBoxAppearanceValidator.decode(Map.get(value, "ListBox")),
             :ok <-
               FormInspectionValidator.reserve(
                 :max_pdf_form_preparation_entries,
                 length(fonts) + length(projections),
                 "NEPUSourceEditor"
               ),
             {:ok, fonts} <-
               collect(fonts, fn font ->
                 case font do
                   %{
                     "Family" => {:string, family},
                     "Weight" => weight,
                     "Style" => {:name, style},
                     "Data" => source
                   }
                   when style in ["normal", "italic"] ->
                     with true <-
                            Enum.all?(
                              Map.keys(font),
                              &(&1 in ["Family", "Weight", "Style", "Data"])
                            ),
                          true <- is_binary(family),
                          :ok <-
                            FormInspectionValidator.reserve(
                              :max_pdf_form_metadata_bytes,
                              byte_size(family),
                              "Fonts"
                            ),
                          {:ok, bytes} <- FormInspectionValidator.decoded_font(document, source) do
                       {:ok,
                        %{
                          family: family,
                          weight: weight,
                          style: if(style == "normal", do: :normal, else: :italic),
                          data: bytes
                        }}
                     else
                       {:error, _} = error ->
                         error

                       _ ->
                         invalid(
                           "source editor fonts require only closed embedded byte descriptors"
                         )
                     end

                   _ ->
                     invalid(
                       "source editor font must have embedded byte data and canonical family/weight/style"
                     )
                 end
               end),
             {:ok, style} <-
               Enum.reduce_while(@styles, {:ok, %{}}, fn {key, values}, {:ok, result} ->
                 parsed =
                   case Map.get(style, Atom.to_string(key)) do
                     {:name, name} -> Enum.find(values, &(Atom.to_string(&1) == name))
                     _ -> nil
                   end

                 if parsed,
                   do: {:cont, {:ok, Map.put(result, key, parsed)}},
                   else: {:halt, invalid("source editor has an unsupported compiled text style")}
               end),
             {:ok, clip} <- decode_rect(clip),
             {:ok, origin} <- decode_rect(origin),
             {:ok, projections} <-
               collect(projections, fn projection ->
                 case projection do
                   %{
                     "Matrix" => [a, b, c, d, e, f],
                     "Bounds" => [l, bottom, r, t],
                     "Polygons" => polygons
                   }
                   when is_list(polygons) ->
                     with true <-
                            Enum.all?(
                              Map.keys(projection),
                              &(&1 in ["Matrix", "Bounds", "Polygons"])
                            ),
                          true <- length(polygons) <= Limits.get(:max_layout_boxes),
                          :ok <-
                            FormInspectionValidator.reserve(
                              :max_pdf_form_preparation_entries,
                              length(polygons),
                              "Projections"
                            ),
                          {:ok, polygons} <-
                            collect(polygons, fn polygon ->
                              if is_list(polygon) and length(polygon) >= 6 and
                                   length(polygon) <= 2 * Limits.get(:max_layout_boxes) and
                                   rem(length(polygon), 2) == 0 do
                                with :ok <-
                                       FormInspectionValidator.reserve(
                                         :max_pdf_form_preparation_entries,
                                         div(length(polygon), 2),
                                         "Polygons"
                                       ) do
                                  {:ok,
                                   Enum.chunk_every(polygon, 2) |> Enum.map(&List.to_tuple/1)}
                                end
                              else
                                invalid(
                                  "source editor projection polygons require actual coordinate pairs"
                                )
                              end
                            end) do
                       {:ok,
                        %{
                          matrix: {a, b, c, d, e, f},
                          bounds: {l, bottom, r, t},
                          polygons: polygons
                        }}
                     else
                       {:error, _} = error ->
                         error

                       _ ->
                         invalid("source editor projection exceeds its closed coordinate grammar")
                     end

                   _ ->
                     invalid(
                       "source editor projection requires actual matrix, bounds and clip polygons"
                     )
                 end
               end) do
          validate(%{
            size: {width, height},
            origin: origin,
            x: x,
            top: top,
            clip: clip,
            style:
              Map.merge(style, %{
                font_size: size,
                line_height: line,
                letter_spacing: spacing,
                color: List.to_tuple(color)
              }),
            fonts: fonts,
            listbox: listbox,
            projections: projections
          })
        else
          {:error, _} = error ->
            error

          _ ->
            invalid("appearance template exceeds its canonical resource shape or depth")
        end

      _ ->
        invalid("source editor requires a supported closed source paint template")
    end
  end

  @doc "Validates exact editor paint facts and selected full byte font programs before source glyph layout."
  @spec validate(term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def validate(editor) do
    FormInspectionValidator.with_active_scope(fn -> validate_editor(editor) end)
  end

  defp validate_editor(editor) do
    case editor do
      %{
        size: {width, height},
        x: x,
        top: top,
        clip: clip,
        origin: origin,
        style:
          %{font_size: size, line_height: line, letter_spacing: spacing, color: color} = style,
        fonts: fonts,
        projections: projections
      }
      when is_map(clip) and is_map(origin) and is_list(projections) and is_list(fonts) ->
        with {:ok, listbox} <- ListBoxAppearanceValidator.validate(Map.get(editor, :listbox)),
             {:ok, {^width, ^height}} <- HtmlValidator.prepare_viewport_dimensions(width, height),
             true <-
               width > 0 and height > 0 and is_number(size) and size > 0 and
                 is_number(line) and line > 0,
             :ok <- WriterValidator.validate_clip_rectangle(origin),
             :ok <- WriterValidator.validate_clip_rectangle(clip),
             :ok <-
               WriterValidator.validate_pdf_geometry([
                 x,
                 top,
                 size,
                 line,
                 spacing,
                 width,
                 height,
                 0
               ]),
             {:ok, loaded} <-
               FormInspectionValidator.loaded_fonts(fonts, fn ->
                 FontValidator.memo({__MODULE__, :byte_faces, fonts}, fn ->
                   Font.load_byte_faces(fonts)
                 end)
               end),
             :ok <- ListBoxAppearanceValidator.validate_fonts(listbox, loaded),
             true <-
               Enum.all?(@styles, fn {key, values} ->
                 Map.get(style, key, default_style(key)) in values
               end),
             {:ok, _} <-
               WriterValidator.prepare(
                 [
                   %{
                     size: {origin.width, origin.height},
                     boxes: [
                       %{
                         type: :text,
                         text: "",
                         font: Font.pdf_name(hd(loaded)),
                         font_face: hd(loaded),
                         font_size: size,
                         color: color,
                         x: x,
                         y:
                           top -
                             HtmlValidator.text_baseline_depth(
                               Map.put(style, :font_face, hd(loaded))
                             ),
                         line_height: line,
                         letter_spacing: spacing,
                         clip: clip
                       }
                     ]
                   }
                 ],
                 []
               ),
             true <- length(projections) <= Limits.get(:max_pdf_form_depth),
             :ok <-
               Enum.reduce_while(projections, :ok, fn projection, :ok ->
                 result =
                   case projection do
                     %{bounds: {l, b, r, t}, matrix: {a, c, d, e, f, g}, polygons: polygons}
                     when is_list(polygons) ->
                       with :ok <- WriterValidator.validate_pdf_geometry([a, c, d, e, f, g, 0, 0]),
                            :ok <- WriterValidator.validate_pdf_geometry([l, b, r, t, l, b, r, t]),
                            true <- r > l and t > b,
                            true <-
                              polygons != [] and length(polygons) <= Limits.get(:max_layout_boxes),
                            true <-
                              Enum.all?(polygons, fn polygon ->
                                is_list(polygon) and length(polygon) >= 3 and
                                  length(polygon) <= Limits.get(:max_layout_boxes) and
                                  Enum.all?(polygon, fn point ->
                                    case point do
                                      {x, y} ->
                                        match?(
                                          :ok,
                                          WriterValidator.validate_pdf_geometry([
                                            x,
                                            y,
                                            x,
                                            y,
                                            x,
                                            y,
                                            x,
                                            y
                                          ])
                                        )

                                      _ ->
                                        false
                                    end
                                  end)
                              end) do
                         :ok
                       else
                         {:error, _} = error ->
                           error

                         _ ->
                           invalid(
                             "source editor projection requires positive actual bounds and finite clip polygons"
                           )
                       end

                     _ ->
                       invalid(
                         "source editor projection requires its canonical matrix and clip geometry"
                       )
                   end

                 case result do
                   :ok -> {:cont, :ok}
                   error -> {:halt, error}
                 end
               end) do
          {:ok, editor |> Map.put(:loaded_fonts, loaded) |> Map.put(:listbox, listbox)}
        else
          {:error, _} = error ->
            error

          _ ->
            invalid("source editor requires positive geometry and supported selected byte fonts")
        end

      _ ->
        invalid("source editor paint template is malformed")
    end
  end

  @doc "Validates the closed resolved text input before the shared inline painter measures glyphs."
  @spec prepare_paint([map()], map(), map()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_paint(nodes, style, editor) do
    with {:ok, editor} <- validate(editor),
         true <- is_list(nodes) and is_map(style),
         true <- Enum.all?(nodes, &match?(%{type: :text, text: text} when is_binary(text), &1)),
         :ok <-
           HtmlValidator.validate_font_fallback_input(%{type: :document, children: nodes}, :error),
         :ok <-
           HtmlValidator.validate_font_fallback_input(
             %{type: :document, children: [%{type: :text, text: "", style: style}]},
             :error
           ) do
      {:ok, editor}
    else
      {:error, _} = error -> error
      _ -> invalid("source editor painter requires only canonically resolved scalar text")
    end
  end

  @doc "Prepares genuine source text with the original byte fonts, editor size/color/padding and shared layout/clip rules."
  @spec appearance(map(), map(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def appearance(field, editor, value) do
    if field.type == :choice and (field.flags &&& 131_072) == 0 and Map.get(editor, :listbox) do
      with {:ok, editor} <- validate(editor),
           {:ok, frame} <- ListBoxAppearanceValidator.appearance(field, editor, value) do
        Enum.reduce_while(editor.projections, {:ok, frame}, fn projection, {:ok, frame} ->
          case FormAppearanceComposition.project_appearance(
                 frame,
                 projection.matrix,
                 projection.polygons,
                 projection.bounds
               ) do
            {:ok, projected} ->
              {:cont, {:ok, Map.put(projected, :choice_top_index, frame.choice_top_index)}}

            error ->
              {:halt, error}
          end
        end)
      end
    else
      text_appearance(field, editor, value)
    end
  end

  defp text_appearance(field, editor, value) do
    with {:ok, editor} <- validate(editor),
         {:ok, text} <-
           (case field.type do
              :text ->
                if((field.flags &&& 8192) != 0,
                  do: FormValidator.password_display_text(value),
                  else: {:ok, value}
                )

              :choice ->
                {:ok,
                 Enum.filter(field.choices, &(&1.value in List.wrap(value)))
                 |> Enum.map_join("\n", & &1.label)}
            end),
         primary = hd(editor.loaded_fonts),
         registry = %{
           embedded: editor.loaded_fonts,
           fallback: editor.loaded_fonts,
           system_font_discovery: false
         },
         style =
           editor.style
           |> Map.merge(%{
             font_face: primary,
             font_families: [primary.family],
             font_weight: primary.weight,
             font_style: primary.style,
             _font_registry: registry
           }),
         {:ok, prepared} <-
           FontFallback.resolve(
             %{type: :document, children: [%{type: :text, text: text, style: style}]},
             :error
           ),
         {:ok, frame} <- Layout.paint_source_editor(prepared.children, style, editor),
         {:ok, frame} <-
           Enum.reduce_while(editor.projections, {:ok, frame}, fn projection, {:ok, frame} ->
             case FormAppearanceComposition.project_appearance(
                    frame,
                    projection.matrix,
                    projection.polygons,
                    projection.bounds
                  ) do
               {:ok, frame} -> {:cont, {:ok, frame}}
               error -> {:halt, error}
             end
           end) do
      {:ok, frame}
    else
      {:error, {_reason, _diagnostic}} = error ->
        appearance_failure(error, field)
    end
  catch
    {:render_resource_limit, error} -> appearance_failure(error, field)
  end

  defp appearance_failure({:error, {reason, diagnostic}}, field) do
    {:error,
     {reason,
      Diagnostics.with_context(diagnostic,
        source: field.name,
        module: __MODULE__,
        operation: :prepare_form_appearance
      )}}
  end

  @doc false
  @spec text_leaves([map()]) :: [map()]
  def text_leaves(boxes) do
    Enum.flat_map(boxes, fn box ->
      case box do
        %{type: :text, font_face: _} -> [box]
        %{boxes: children} -> text_leaves(children)
        _ -> []
      end
    end)
  end

  defp encode_rect(rect) do
    %{"X" => rect.x, "Y" => rect.y, "Width" => rect.width, "Height" => rect.height}
    |> then(fn value ->
      value =
        if Map.has_key?(rect, :border_radius),
          do: Map.put(value, "Radius", rect.border_radius),
          else: value

      case Map.get(rect, :inner_clip) do
        nil -> value
        inner -> Map.put(value, "Inner", encode_rect(inner))
      end
    end)
  end

  defp decode_rect(value, inner? \\ false) do
    case value do
      %{"X" => x, "Y" => y, "Width" => width, "Height" => height} ->
        with true <-
               Enum.all?(
                 Map.keys(value),
                 &(&1 in ["X", "Y", "Width", "Height", "Radius", "Inner"])
               ),
             true <- not inner? or not Map.has_key?(value, "Inner"),
             {:ok, inner} <-
               if(Map.has_key?(value, "Inner"),
                 do: decode_rect(value["Inner"], true),
                 else: {:ok, nil}
               ),
             rect = %{x: x, y: y, width: width, height: height},
             rect =
               if(Map.has_key?(value, "Radius"),
                 do: Map.put(rect, :border_radius, value["Radius"]),
                 else: rect
               ),
             rect = if(inner, do: Map.put(rect, :inner_clip, inner), else: rect),
             :ok <- WriterValidator.validate_clip_rectangle(rect) do
          {:ok, rect}
        else
          {:error, _} = error ->
            error

          _ ->
            invalid(
              "source editor clip requires one bounded rectangle and at most one rounded inner clip"
            )
        end

      _ ->
        invalid("source editor clip requires its exact compiled rectangle")
    end
  end

  defp collect(values, convert) do
    Enum.reduce_while(values, {:ok, []}, fn value, {:ok, result} ->
      case convert.(value) do
        {:ok, value} -> {:cont, {:ok, [value | result]}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, result} -> {:ok, Enum.reverse(result)}
      error -> error
    end
  end

  defp default_style(key) do
    case key do
      :white_space -> :normal
      :text_align -> :left
      :line_break -> :auto
      _ -> :normal
    end
  end

  defp invalid(message),
    do:
      Diagnostics.error(:forms, :unsupported_form, message,
        module: __MODULE__,
        operation: :prepare_form_appearance
      )
end
