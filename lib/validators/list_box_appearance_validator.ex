defmodule NativeElixirPdfUtilities.Validators.ListBoxAppearanceValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, FontFallback, Layout, ScrollbarPainter}

  alias NativeElixirPdfUtilities.Validators.{
    FormAppearanceValidator,
    FormInspectionValidator,
    HtmlValidator,
    PdfValidator,
    ScrollbarValidator,
    WriterValidator
  }

  @doc false
  @spec capture([map()], map(), map()) :: map()
  def capture(boxes, style, clip) do
    choice_values = List.to_tuple(style._select_state.choices)
    choices = Map.new(style._select_state.choices, &{&1.id, &1.index})
    selected = MapSet.new(style._select_state.selected_indices)
    leaves = FormAppearanceValidator.text_leaves(boxes)
    texts = Enum.group_by(leaves, &Map.get(&1, :initial_option_id))

    rows =
      Enum.flat_map(boxes, fn box ->
        index = Map.get(choices, Map.get(box, :initial_option_id))

        cond do
          box.type == :rect and not is_nil(index) ->
            row_texts = Map.get(texts, box.initial_option_id, [])
            first = List.first(row_texts)
            face = if first, do: first.font_face, else: style.font_face

            text_style =
              Map.merge(
                style,
                if(first,
                  do: Map.take(first, [:font_size, :line_height, :letter_spacing, :color]),
                  else: %{}
                )
              )

            top =
              if first,
                do:
                  first.y +
                    HtmlValidator.text_baseline_depth(Map.put(text_style, :font_face, face)),
                else: box.y + box.height

            [
              %{
                index: index,
                label: elem(choice_values, index).label,
                x: box.x - clip.x,
                top: clip.y + clip.height - box.y - box.height,
                width: box.width,
                height: box.height,
                text_x: if(first, do: first.x - box.x, else: 0.0),
                text_top: box.y + box.height - top,
                font_size: text_style.font_size,
                line_height: text_style.line_height,
                letter_spacing: text_style.letter_spacing,
                color: text_style.color,
                background:
                  if(
                    MapSet.member?(selected, index) and
                      box.fill_color == {206 / 255, 206 / 255, 206 / 255},
                    do: nil,
                    else: box.fill_color
                  ),
                selection: {206 / 255, 206 / 255, 206 / 255},
                family: face.family,
                weight: face.weight,
                style: face.style
              }
            ]

          box.type == :text and box.text != "" and box.width > 0 and
              is_nil(Map.get(box, :initial_option_id)) ->
            face = box.font_face

            [
              %{
                index: nil,
                label: box.text,
                x: box.x - clip.x,
                top: clip.y + clip.height - box.y - HtmlValidator.text_baseline_depth(box),
                width: box.width,
                height: box.line_height,
                text_x: 0.0,
                text_top: 0.0,
                font_size: box.font_size,
                line_height: box.line_height,
                letter_spacing: Map.get(box, :letter_spacing, 0.0),
                color: box.color,
                background: nil,
                selection: nil,
                family: face.family,
                weight: face.weight,
                style: face.style
              }
            ]

          true ->
            []
        end
      end)

    %{rows: rows, fonts: Enum.map(leaves, & &1.font_face)}
  end

  @doc false
  @spec encode(map()) :: [map()]
  def encode(model) do
    Enum.map(model.rows, &encode_row/1)
  end

  defp encode_row(row) do
    %{
      "Index" => row.index,
      "Label" => {:string, row.label},
      "Geometry" =>
        Enum.map(
          [
            :x,
            :top,
            :width,
            :height,
            :text_x,
            :text_top,
            :font_size,
            :line_height,
            :letter_spacing
          ],
          &Map.fetch!(row, &1)
        ),
      "Color" => Tuple.to_list(row.color),
      "Background" => encode_color(Map.get(row, :background)),
      "Selection" => encode_color(Map.get(row, :selection, {206 / 255, 206 / 255, 206 / 255})),
      "Family" => {:string, row.family},
      "Weight" => row.weight,
      "Style" => {:name, Atom.to_string(row.style)}
    }
  end

  defp encode_color(color) do
    if is_tuple(color), do: Tuple.to_list(color), else: color
  end

  defp decode_color(color) do
    case color do
      color when color in [nil, :null] ->
        {:ok, nil}

      color when is_list(color) and length(color) in [3, 4] ->
        color = List.to_tuple(color)
        if WriterValidator.valid_color?(color), do: {:ok, color}, else: invalid()

      _ ->
        invalid()
    end
  end

  @doc false
  @spec validate(term()) :: {:ok, map() | nil} | {:error, {atom(), map()}}
  def validate(model) do
    case model do
      nil ->
        {:ok, nil}

      %{rows: rows} when is_list(rows) and map_size(model) == 1 ->
        FormInspectionValidator.with_scope(%{objects: %{}}, fn ->
          Enum.reduce_while(rows, {:ok, []}, fn row, {:ok, acc} ->
            keys = [
              :index,
              :label,
              :x,
              :top,
              :width,
              :height,
              :text_x,
              :text_top,
              :font_size,
              :line_height,
              :letter_spacing,
              :color,
              :family,
              :weight,
              :style
            ]

            if is_map(row) and
                 Enum.all?(Map.keys(row), &(&1 in (keys ++ [:background, :selection]))) and
                 Enum.all?(keys, &Map.has_key?(row, &1)) and is_tuple(row.color) and
                 is_atom(row.style) do
              case decode([encode_row(row)]) do
                {:ok, %{rows: [parsed]}} -> {:cont, {:ok, [parsed | acc]}}
                error -> {:halt, error}
              end
            else
              {:halt, invalid()}
            end
          end)
          |> case do
            {:ok, rows} -> {:ok, %{rows: Enum.reverse(rows)}}
            error -> error
          end
        end)

      _ ->
        invalid()
    end
  end

  @doc false
  @spec decode(term()) :: {:ok, map() | nil} | {:error, {atom(), map()}}
  def decode(value) do
    case value do
      nil ->
        {:ok, nil}

      rows when is_list(rows) ->
        with :ok <-
               FormInspectionValidator.reserve(
                 :max_pdf_form_preparation_entries,
                 length(rows),
                 "ListBox"
               ) do
          Enum.reduce_while(rows, {:ok, []}, fn row, {:ok, acc} ->
            case row do
              %{
                "Index" => index,
                "Label" => {:string, label},
                "Geometry" =>
                  [x, top, width, height, text_x, text_top, size, line, spacing] = geometry,
                "Color" => color,
                "Family" => {:string, family},
                "Weight" => weight,
                "Style" => {:name, font_style}
              }
              when (is_nil(index) or index == :null or (is_integer(index) and index >= 0)) and
                     is_binary(label) and is_binary(family) and is_list(color) and
                     length(color) in [3, 4] ->
                with true <-
                       Enum.all?(
                         Map.keys(row),
                         &(&1 in ~w(Index Label Geometry Color Family Weight Style Background Selection))
                       ) and font_style in ["normal", "italic"] and
                         PdfValidator.valid_number?(weight),
                     true <- String.valid?(label) and String.valid?(family),
                     {:ok, background} <- decode_color(Map.get(row, "Background")),
                     {:ok, selection} <-
                       decode_color(Map.get(row, "Selection", [206 / 255, 206 / 255, 206 / 255])),
                     true <- Enum.all?(geometry, &PdfValidator.valid_number?/1),
                     true <-
                       width > 0 and height > 0 and size > 0 and line > 0 and
                         WriterValidator.valid_color?(List.to_tuple(color)),
                     # Count the row's scalar paint facts too. This same scope
                     # spans producer controls, so retained row metadata cannot
                     # multiply unchecked before a later reader sees the PDF.
                     :ok <-
                       FormInspectionValidator.reserve(
                         :max_pdf_form_preparation_entries,
                         length(geometry) + length(color) +
                           if(background, do: tuple_size(background), else: 0) +
                           if(selection, do: tuple_size(selection), else: 0) + 5,
                         "ListBox"
                       ),
                     :ok <-
                       FormInspectionValidator.reserve(
                         :max_pdf_form_metadata_bytes,
                         byte_size(label) + byte_size(family),
                         "ListBox"
                       ) do
                  parsed = %{
                    index: if(index == :null, do: nil, else: index),
                    label: label,
                    x: x,
                    top: top,
                    width: width,
                    height: height,
                    text_x: text_x,
                    text_top: text_top,
                    font_size: size,
                    line_height: line,
                    letter_spacing: spacing,
                    color: List.to_tuple(color),
                    background: background,
                    selection: selection,
                    family: family,
                    weight: weight,
                    style: if(font_style == "normal", do: :normal, else: :italic)
                  }

                  {:cont, {:ok, [parsed | acc]}}
                else
                  {:error, _} = error -> {:halt, error}
                  _ -> {:halt, invalid()}
                end

              _ ->
                {:halt, invalid()}
            end
          end)
          |> case do
            {:ok, rows} -> {:ok, %{rows: Enum.reverse(rows)}}
            error -> error
          end
        end

      _ ->
        invalid()
    end
  end

  @doc false
  @spec validate_fonts(map() | nil, [map()]) :: :ok | {:error, {atom(), map()}}
  def validate_fonts(model, fonts) do
    if is_nil(model) do
      :ok
    else
      identities = MapSet.new(fonts, &{&1.family, &1.weight * 1.0, &1.style})

      if Enum.all?(
           model.rows,
           &MapSet.member?(identities, {&1.family, &1.weight * 1.0, &1.style})
         ), do: :ok, else: invalid()
    end
  end

  @doc false
  @spec prepare_editor(map(), map(), map(), boolean()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_editor(field, widget, font, source?) do
    [left, bottom, right, top] = widget.rect
    width = right - left
    height = top - bottom

    style = %{
      font_size: 12.0,
      line_height: 14.4,
      letter_spacing: 0.0,
      color: {0, 0, 0},
      white_space: :pre,
      text_align: :left,
      word_break: :normal,
      overflow_wrap: :normal,
      line_break: :auto
    }

    editor =
      if source? and widget.source_editor,
        do: widget.source_editor,
        else: %{
          size: {width, height},
          origin: %{x: 0.0, y: 0.0, width: width, height: height},
          x: 2.0,
          top: height - 2.0,
          clip: %{x: 2.0, y: 2.0, width: max(width - 4, 0.0), height: max(height - 4, 0.0)},
          style: style,
          fonts: [Map.take(font, [:family, :weight, :style, :data])],
          loaded_fonts: [font],
          projections: []
        }

    model = Map.get(editor, :listbox)
    row_font = if source?, do: hd(editor.loaded_fonts), else: font

    model =
      if model,
        do: model,
        else: %{
          rows:
            Enum.with_index(field.choices)
            |> Enum.map(fn {choice, index} ->
              %{
                index: index,
                label: choice.label,
                x: 0.0,
                top: index * (editor.style.line_height + 0.75),
                width: editor.clip.width,
                height: editor.style.line_height + 0.75,
                text_x: 0.0,
                text_top: 0.0,
                font_size: editor.style.font_size,
                line_height: editor.style.line_height,
                letter_spacing: editor.style.letter_spacing,
                color: editor.style.color,
                family: row_font.family,
                weight: row_font.weight,
                style: row_font.style
              }
            end)
        }

    if source? do
      {:ok, Map.put(editor, :listbox, model)}
    else
      rows =
        Enum.map(model.rows, fn row ->
          size =
            min(
              12.0,
              min(
                max(row.height - 0.75, 0.0) / 1.2,
                max(row.width - row.text_x, 0.0) /
                  max(Font.text_width(row.label, font, 1.0), 0.001)
              )
            )

          Map.merge(row, %{
            font_size: size,
            line_height: size * 1.2,
            letter_spacing: 0.0,
            color: {0, 0, 0},
            family: font.family,
            weight: font.weight,
            style: font.style,
            text_top: 0.0,
            text_x:
              case Map.get(field.effective, "Q", 0) do
                1 -> max((row.width - Font.text_width(row.label, font, size)) / 2, 0.0)
                2 -> max(row.width - Font.text_width(row.label, font, size), 0.0)
                _ -> row.text_x
              end
          })
        end)

      if editor.clip.width > 0 and editor.clip.height > 0 and Enum.all?(rows, &(&1.font_size > 0)),
        do: {:ok, Map.put(editor, :listbox, %{rows: rows})},
        else: invalid()
    end
  end

  @doc false
  @spec appearance(map(), map(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def appearance(field, editor, value) do
    rows = editor.listbox.rows
    selected = MapSet.new(List.wrap(value))
    choices = List.to_tuple(field.choices)
    top_index = Map.get(field.dictionary, "TI", 0)

    valid_indices? =
      Enum.all?(rows, fn row -> is_nil(row.index) or row.index < tuple_size(choices) end)

    with true <- is_integer(top_index) and top_index >= 0 and valid_indices? do
      total = Enum.reduce(rows, 0.0, &max(&1.top + &1.height, &2))
      initial = Enum.find(rows, &(&1.index == top_index))
      initial_offset = if initial, do: max(initial.top, 0.0), else: 0.0

      chosen =
        Enum.find(rows, fn row ->
          not is_nil(row.index) and MapSet.member?(selected, elem(choices, row.index).value)
        end)

      offset =
        cond do
          chosen && chosen.top < initial_offset ->
            chosen.top

          chosen && chosen.top + chosen.height > initial_offset + editor.clip.height ->
            chosen.top + chosen.height - editor.clip.height

          true ->
            initial_offset
        end

      offset =
        min(max(round(max(offset, 0.0) / 0.75) * 0.75, 0.0), max(total - editor.clip.height, 0.0))

      visible =
        Enum.filter(rows, &(&1.top + &1.height > offset and &1.top < offset + editor.clip.height))

      first_index = Enum.find_value(visible, 0, & &1.index)

      Enum.reduce_while(visible, {:ok, []}, fn row, {:ok, acc} ->
        face =
          Enum.find(
            editor.loaded_fonts,
            &(&1.family == row.family and &1.weight == row.weight and &1.style == row.style)
          )

        if face do
          style =
            Map.merge(
              editor.style,
              Map.take(row, [:font_size, :line_height, :letter_spacing, :color])
            )
            |> Map.merge(%{
              font_face: face,
              font_families: [face.family],
              font_weight: face.weight,
              font_style: face.style,
              white_space: :pre,
              text_align: :left,
              _font_registry: %{
                embedded: editor.loaded_fonts,
                fallback: editor.loaded_fonts,
                system_font_discovery: false
              }
            })

          label = if is_nil(row.index), do: row.label, else: elem(choices, row.index).label

          child = %{
            editor
            | x: editor.clip.x + row.x + row.text_x,
              listbox: nil,
              top: editor.clip.y + editor.clip.height - row.top - row.text_top + offset,
              style: style,
              clip: %{
                editor.clip
                | width:
                    max(min(row.width - row.text_x, editor.clip.width - row.x - row.text_x), 0.0)
              }
          }

          with {:ok, resolved} <-
                 FontFallback.resolve(
                   %{type: :document, children: [%{type: :text, text: label, style: style}]},
                   :error
                 ),
               {:ok, frame} <- Layout.paint_source_editor(resolved.children, style, child) do
            fills =
              [Map.get(row, :background)] ++
                if(
                  not is_nil(row.index) and
                    MapSet.member?(selected, elem(choices, row.index).value),
                  do: [Map.get(row, :selection, {206 / 255, 206 / 255, 206 / 255})],
                  else: []
                )

            highlight =
              Enum.flat_map(fills, fn fill ->
                if fill,
                  do: [
                    %{
                      type: :rect,
                      x: editor.clip.x + row.x,
                      y: editor.clip.y + editor.clip.height - row.top - row.height + offset,
                      width: row.width,
                      height: row.height,
                      fill_color: fill,
                      stroke_color: nil,
                      stroke_width: 0.0,
                      border_radius: 0.0,
                      clip: editor.clip,
                      snap_to_css_pixel_grid: true
                    }
                  ],
                  else: []
              end)

            {:cont, {:ok, [highlight ++ frame.boxes | acc]}}
          else
            error -> {:halt, error}
          end
        else
          {:halt, invalid()}
        end
      end)
      |> case do
        {:ok, groups} ->
          geometry = %{
            vertical: true,
            horizontal: false,
            viewport_width: editor.clip.width + 11.25,
            viewport_height: editor.clip.height,
            client_width: editor.clip.width,
            client_height: editor.clip.height,
            scroll_width: editor.clip.width,
            scroll_height: max(total, editor.clip.height),
            scroll_offset: %{x: 0.0, y: offset}
          }

          with {:ok, parts} <- ScrollbarValidator.prepare(geometry) do
            scrollbar =
              ScrollbarPainter.paint(parts)
              |> NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.translate_boxes(
                editor.clip.x,
                editor.clip.y
              )
              |> Enum.map(&Map.put(&1, :snap_to_css_pixel_grid, true))

            frame = %{
              size: editor.size,
              boxes: Enum.reverse(groups) |> List.flatten(),
              choice_top_index: first_index
            }

            frame = %{frame | boxes: frame.boxes ++ scrollbar}

            frame =
              if editor.origin.x == 0 and editor.origin.y == 0 and
                   {editor.origin.width, editor.origin.height} == editor.size,
                 do: frame,
                 else: Map.put(frame, :paint_frame, editor.origin)

            with {:ok, _} <- WriterValidator.prepare_appearances([frame], 1), do: {:ok, frame}
          end

        error ->
          error
      end
    else
      _ -> invalid()
    end
  end

  defp invalid do
    Diagnostics.error(
      :forms,
      :unsupported_form,
      "list box requires bounded row geometry, valid option indices and embedded row fonts",
      module: __MODULE__
    )
  end
end
