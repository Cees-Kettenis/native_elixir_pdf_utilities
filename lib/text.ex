defmodule NativeElixirPdfUtilities.Text do
  @moduledoc """
  Strict native extraction of embedded Unicode text from PDF documents.

  Extraction resolves the PDF page tree, resources, content streams, Form
  XObjects, and the active font at every text operation. It succeeds only when
  every shown text string has a reliable Unicode mapping; it never guesses from
  an embedded font program or merges CMaps from unrelated fonts.

  In string extraction, marked-content `ActualText` replaces enclosed decoded
  text operands or painted image graphics once. Positioned extraction preserves
  physical text operands by default; set `actual_text: true` for replacement
  spans. Image replacements use their actual image transform and
  region geometry; they do not recover per-glyph coordinates or a font resource.
  Nested replacements use the outermost string, and empty replacements suppress
  their pieces. Physical operands still undergo font and geometry validation.

  PDFs store positioned text operations, not semantic rows, columns, or tables.
  `extract_spans/2` exposes those decoded operations for callers that need to
  interpret document-specific structure. `extract/2` remains a string
  projection: `layout: true` reconstructs approximate visual lines, while
  `layout: false` projects text-show execution order.

  Extraction does not perform OCR. Successfully decoded text can be recovered
  even when its rendering mode does not paint the text, but clipping paths,
  transparency, occlusion, and other causes of visual visibility are not
  evaluated.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.TextValidator

  @graphics_state_fields [
    :ctm,
    :font,
    :font_size,
    :char_spacing,
    :word_spacing,
    :horizontal_scale,
    :leading,
    :rise,
    :render_mode
  ]

  @typedoc "Options for reconstructed string extraction."
  @type extract_option :: {:layout, boolean()}
  @typedoc "Options for positioned span extraction."
  @type span_option :: {:order, :source | :visual} | {:actual_text, boolean()}
  @typedoc "A PDF text rendering mode from 0 through 7."
  @type render_mode :: 0..7
  @typedoc "A six-value PDF affine matrix in `[a, b, c, d, e, f]` order."
  @type matrix :: [float()]

  @typedoc """
  A decoded text-showing operand and its positioned extraction context.

  `source_index` is zero-based within a page and follows emitted-span execution
  order. With `actual_text: true`, an `ActualText` group emits one span in place
  of its text operands. Page content streams are traversed in `/Contents` order, and Form
  XObjects are traversed where their `Do` operator occurs. Reusing a Form
  therefore produces new spans with new indexes.

  `x`, `y`, `end_x`, and `end_y` describe the text baseline in normalized
  display coordinates. The origin is the top-left of the rotated MediaBox, X
  increases rightward, Y increases downward, and page rotation and Form CTMs
  are applied. These points are not glyph bounding boxes. The end point uses
  the PDF font widths available to the extractor and can be approximate when a
  font omits explicit metrics. Replacement spans use the first operand's start
  and context, and the last operand's end. They combine painting and clipping
  flags across all pieces. These endpoints are not the group's bounding box.

  `text_matrix` is the PDF text matrix at the start of the operand. `ctm` is the
  active PDF current transformation matrix, including Form transforms. Neither
  matrix includes the final MediaBox/page-rotation normalization represented by
  the baseline coordinates.

  `font_resource` is the active PDF resource name, not a guaranteed font family
  or PostScript name. `font_size` is the `Tf` text-space size, not a calculated
  display-space height.

  For text operands, `paints_text?` and `adds_to_clip_path?` are derived from `render_mode`.
  Image replacements use the image Do transform, with `font_resource: nil` and
  `font_size` equal to the image-region height. `image_resource` identifies that
  image, and the image region cannot claim the original glyph coordinates.
  They describe the requested PDF text rendering operation; they do not claim
  that the text is visually visible or clipped. `joins_previous?` identifies a
  text-showing operand that immediately continues the preceding operand without
  an intervening text-positioning, text-object, content-form, or graphics-matrix
  boundary. This follows PDF text-operator execution rather than inferring
  continuity from display coordinates.
  Replacement spans preserve adjacency across painting wrappers. Explicit text
  positioning between groups ends it; internal positioning does not split a
  group's replacement text.
  """
  @type text_span :: %{
          text: String.t(),
          source_index: non_neg_integer(),
          x: float(),
          y: float(),
          end_x: float(),
          end_y: float(),
          font_resource: String.t() | nil,
          font_size: float(),
          text_matrix: matrix(),
          ctm: matrix(),
          render_mode: render_mode(),
          paints_text?: boolean(),
          adds_to_clip_path?: boolean(),
          joins_previous?: boolean()
        }

  @typedoc """
  Positioned text for one resolved PDF page.

  `media_box` is `[left, bottom, right, top]` in PDF default user-space units.
  `rotation` is the effective inherited page rotation normalized to 0, 90, 180,
  or 270 degrees. Every resolved page is returned, including pages whose
  `spans` list is empty.
  """
  @type text_page :: %{
          number: pos_integer(),
          media_box: [number()],
          rotation: 0 | 90 | 180 | 270,
          spans: [text_span()]
        }

  @typedoc "A page-preserving positioned-text extraction result."
  @type text_document :: %{
          page_count: non_neg_integer(),
          pages: [text_page()]
        }
  @type error_reason ::
          :operation_worker_failed
          | :encrypted_pdf
          | :invalid_options
          | :invalid_path
          | :invalid_pdf_input
          | :no_extractable_text
          | :resource_limit_exceeded
          | :unsupported_pdf_feature
          | :unsupported_text_encoding
          | File.posix()

  @doc """
  Extracts reliably decodable embedded text from a PDF binary.

  Set `layout: true` (the default) to group spans into approximate visual lines.
  With `layout: false`, spans retain their content-stream order. Extraction fails
  rather than returning partial text when a shown text operation cannot be
  decoded with the active font.
  """
  @spec extract(binary(), [extract_option()]) ::
          {:ok, String.t()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def extract(pdf_binary, opts \\ []) do
    NativeElixirPdfUtilities.Operation.run(__MODULE__, :extract, fn ->
      with {:ok, request} <- TextValidator.validate_request(pdf_binary, opts, :extract),
           {:ok, pdf_context} <- Reader.read_validated(request.pdf),
           {:ok, text_context} <- TextValidator.prepare(pdf_context),
           {:ok, pages} <- extract_pages(text_context, true) do
        visible_pages =
          pages
          |> Enum.map(fn page ->
            {page.number, page.rotation, Enum.filter(page.spans, & &1.paints_text?)}
          end)
          |> Enum.reject(fn {_page_number, _rotation, spans} -> spans == [] end)

        case visible_pages do
          [] ->
            error(:text_extraction, :no_extractable_text, "PDF contains no extractable text")

          pages ->
            project_pages(pages, request.options.layout)
        end
      else
        {:error, _} = extraction_error -> text_error(extraction_error, :extract)
      end
    end)
  end

  @doc """
  Extracts positioned, reliably decoded text spans from a PDF binary.

  The result preserves every resolved page and every non-empty decoded text
  operand, including operands enclosed by `ActualText`. Set `actual_text: true`
  to emit one replacement span per group instead; an empty replacement then
  suppresses its operands. This includes rendering
  modes 3 (neither painted nor added
  to the clipping path) and 7 (added to the clipping path without painting).
  This preservation guarantee applies to decoded text operations, not to OCR,
  undecodable fonts, glyph outlines, semantic tables, or evaluated visual
  visibility.

  Spans use source execution order by default. Set `order: :visual` to return a
  best-effort display ordering using the same line grouping as `extract/2`;
  `source_index` remains unchanged so source order can always be restored.

  A valid PDF with no decodable text returns an `:ok` document containing empty
  page span lists. Extraction remains strict and returns a structured error
  rather than a partial document when a shown string cannot be decoded.
  """
  @spec extract_spans(binary(), [span_option()]) ::
          {:ok, text_document()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def extract_spans(pdf_binary, opts \\ []) do
    NativeElixirPdfUtilities.Operation.run(__MODULE__, :extract_spans, fn ->
      with {:ok, request} <- TextValidator.validate_request(pdf_binary, opts, :extract_spans),
           {:ok, pdf_context} <- Reader.read_validated(request.pdf),
           {:ok, text_context} <- TextValidator.prepare(pdf_context),
           {:ok, pages} <- extract_pages(text_context, request.options.actual_text) do
        pages =
          if request.options.order == :visual do
            Enum.map(pages, fn page ->
              %{page | spans: visual_spans(page.spans, page.rotation)}
            end)
          else
            pages
          end

        {:ok, %{page_count: length(pages), pages: pages}}
      else
        {:error, _} = extraction_error -> text_error(extraction_error, :extract_spans)
      end
    end)
  end

  @doc """
  Reads a PDF file and extracts reliably decodable embedded text from it.
  """
  @spec extract_file(String.t(), [extract_option()]) ::
          {:ok, String.t()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def extract_file(path, opts \\ []) do
    NativeElixirPdfUtilities.Operation.run(__MODULE__, :extract_file, fn ->
      extract_file_with(path, opts, :extract_file, &extract/2)
    end)
  end

  @doc """
  Reads a PDF file and extracts its page-preserving positioned text spans.

  This has the same ordering, geometry, preservation, and strict diagnostic
  behavior as `extract_spans/2`. File extraction errors include the source path.
  """
  @spec extract_file_spans(String.t(), [span_option()]) ::
          {:ok, text_document()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def extract_file_spans(path, opts \\ []) do
    NativeElixirPdfUtilities.Operation.run(__MODULE__, :extract_file_spans, fn ->
      extract_file_with(path, opts, :extract_file_spans, &extract_spans/2)
    end)
  end

  defp extract_file_with(path, opts, operation, extractor) do
    case TextValidator.validate_path(path, operation) do
      {:ok, path} ->
        case NativeElixirPdfUtilities.FileReader.read(path, Limits.get(:max_input_bytes)) do
          {:ok, pdf_binary} ->
            case extractor.(pdf_binary, opts) do
              {:ok, result} ->
                {:ok, result}

              {:error, {reason, diagnostic}} ->
                {:error,
                 {reason, diagnostic |> Map.put(:source, path) |> Map.put(:operation, operation)}}
            end

          {:error, {reason, diagnostic}} ->
            {:error,
             {reason,
              Diagnostics.with_context(diagnostic,
                operation: operation,
                module: __MODULE__,
                source: path
              )}}

          {:error, reason} ->
            error(:file, reason, "file read failed: #{reason}", operation: :read, source: path)
        end

      {:error, _} = path_error ->
        text_error(path_error, operation)
    end
  end

  defp extract_pages(text_context, actual_text) do
    text_context.pages
    |> Enum.reduce_while({:ok, []}, fn page, {:ok, pages} ->
      case extract_page(page, actual_text) do
        {:ok, page} -> {:cont, {:ok, [page | pages]}}
        {:error, _} = extraction_error -> {:halt, extraction_error}
      end
    end)
    |> case do
      {:ok, pages} -> {:ok, Enum.reverse(pages)}
      error -> error
    end
  end

  defp extract_page(page, actual_text) do
    initial_state = text_state(page, actual_text)

    Enum.reduce_while(page.contents, {:ok, initial_state, []}, fn operations,
                                                                  {:ok, state, spans} ->
      case interpret(operations, state, page.number) do
        {:ok, state, new_spans} ->
          {:cont, {:ok, state, Enum.reverse(new_spans, spans)}}

        {:error, {reason, diagnostic}} ->
          {:halt, {:error, {reason, with_debug_details(diagnostic, page: page.number)}}}
      end
    end)
    |> case do
      {:ok, _state, spans} ->
        {:ok,
         %{
           number: page.number,
           media_box: page.media_box,
           rotation: page.rotation,
           spans: Enum.reverse(spans)
         }}

      error ->
        error
    end
  end

  defp interpret(operations, state, page_number) do
    Enum.reduce_while(operations, {:ok, state, []}, fn operation, {:ok, state, spans} ->
      case apply_operator(operation, state, spans, page_number) do
        {:ok, state, spans} -> {:cont, {:ok, state, spans}}
        {:error, _} = extraction_error -> {:halt, extraction_error}
      end
    end)
    |> case do
      {:ok, state, spans} -> {:ok, state, Enum.reverse(spans)}
      {:error, _} = extraction_error -> extraction_error
    end
  end

  defp apply_operator(operation, state, spans, page) do
    operator = operation.operator
    operands = operation.operands

    case {operator, operands} do
      {operator, _operands} when operator in ["BMC", "BDC"] ->
        text = Map.get(operation, :actual_text)
        owns_replacement? = state.actual_text? and is_binary(text) and is_nil(state.replacement)

        replacement =
          if owns_replacement?,
            do: %{
              text: text,
              first: nil,
              last: nil,
              follows_replacement?: state.previous_replacement?
            },
            else: state.replacement

        {:ok,
         %{
           state
           | marked_content: [owns_replacement? | state.marked_content],
             replacement: replacement
         }, spans}

      {"EMC", []} ->
        [owns_replacement? | marked_content] = state.marked_content
        state = %{state | marked_content: marked_content}

        case {owns_replacement?, state.replacement} do
          {true, %{text: text, first: first, last: last} = replacement}
          when text != "" and not is_nil(first) ->
            span = %{
              first
              | text: text,
                end_x: last.end_x,
                end_y: last.end_y,
                joins_previous?: first.joins_previous? or replacement.follows_replacement?
            }

            {:ok,
             %{
               state
               | replacement: nil,
                 next_source_index: state.next_source_index + 1,
                 previous_replacement?: true
             }, [span | spans]}

          {true, _replacement} ->
            {:ok, %{state | replacement: nil}, spans}

          {false, _replacement} ->
            {:ok, state, spans}
        end

      {"q", []} ->
        saved_state = Map.take(state, @graphics_state_fields)
        {:ok, %{state | stack: [saved_state | state.stack]}, spans}

      {"Q", []} ->
        [saved_state | stack] = state.stack

        restored_state =
          state
          |> Map.merge(saved_state)
          |> Map.put(:stack, stack)
          |> break_text_join()

        {:ok, restored_state, spans}

      {"cm", matrix} ->
        with {:ok, ctm} <- TextValidator.compose_matrix(matrix, state.ctm, page, "cm") do
          {:ok, state |> Map.put(:ctm, ctm) |> break_text_join(), spans}
        end

      {"BT", []} ->
        {:ok,
         break_text_join(%{
           state
           | in_text?: true,
             text_matrix: identity(),
             line_matrix: identity()
         }), spans}

      {"ET", []} ->
        {:ok, break_text_join(%{state | in_text?: false}), spans}

      {"Tf", [{:name, _font_name}, size]} ->
        {:ok, %{state | font: operation.font, font_size: size}, spans}

      {"gs", [{:name, _name}]} ->
        case operation do
          %{font: font, font_size: size} ->
            {:ok, %{state | font: font, font_size: size}, spans}

          _ ->
            {:ok, state, spans}
        end

      {"Tm", matrix} ->
        {:ok, break_text_join(%{state | text_matrix: matrix, line_matrix: matrix}, :position),
         spans}

      {operator, [tx, ty]} when operator in ["Td", "TD"] ->
        with {:ok, line_matrix} <-
               TextValidator.translate_matrix(state.line_matrix, tx, ty, page, operator) do
          state = %{
            state
            | line_matrix: line_matrix,
              text_matrix: line_matrix,
              leading: if(operator == "TD", do: -ty, else: state.leading)
          }

          {:ok, break_text_join(state, :position), spans}
        end

      {"T*", []} ->
        with {:ok, line_matrix} <-
               TextValidator.translate_matrix(state.line_matrix, 0.0, -state.leading, page, "T*") do
          {:ok,
           break_text_join(
             %{state | line_matrix: line_matrix, text_matrix: line_matrix},
             :position
           ), spans}
        end

      {"TL", [leading]} ->
        {:ok, %{state | leading: leading}, spans}

      {"Tc", [value]} ->
        {:ok, %{state | char_spacing: value}, spans}

      {"Tw", [value]} ->
        {:ok, %{state | word_spacing: value}, spans}

      {"Tz", [value]} ->
        {:ok, %{state | horizontal_scale: value}, spans}

      {"Tr", [mode]} ->
        {:ok, %{state | render_mode: mode}, spans}

      {"Ts", [value]} ->
        {:ok, %{state | rise: value}, spans}

      {"Tj", [_string]} ->
        show(state, spans, operation.decoded, page)

      {"TJ", [{:array, _values}]} ->
        show_array(operation.prepared_values, state, spans, page)

      {"'", [_string]} ->
        with {:ok, state, spans} <-
               apply_operator(%{operator: "T*", operands: []}, state, spans, page) do
          show(state, spans, operation.decoded, page)
        end

      {"\"", [word_spacing, char_spacing, _string]} ->
        state = %{state | word_spacing: word_spacing, char_spacing: char_spacing}

        with {:ok, state, spans} <-
               apply_operator(%{operator: "T*", operands: []}, state, spans, page) do
          show(state, spans, operation.decoded, page)
        end

      {"Do", [{:name, _name}]} ->
        state = break_text_join(state)

        with {:ok, state, spans} <- execute_form(operation.form, state, spans, page) do
          {:ok, break_text_join(state), spans}
        end

      _ ->
        {:ok, state, spans}
    end
  end

  defp break_text_join(state, boundary \\ :paint) do
    # Painting wrappers can split one logical replacement sequence. Explicit
    # positioning outside an active replacement ends that semantic adjacency.
    %{
      state
      | join_next_span?: false,
        previous_replacement?:
          state.previous_replacement? and
            (boundary != :position or not is_nil(state.replacement))
    }
  end

  defp show(state, spans, decoded, page) do
    add_span(state, spans, decoded, page, state.join_next_span?)
  end

  defp show_array(values, state, spans, page) do
    values
    |> Enum.reduce_while(
      {:ok, state, spans, state.join_next_span?},
      fn value, {:ok, state, spans, joins_previous?} ->
        case value do
          {:adjustment, value} ->
            case TextValidator.adjust_text_matrix(state, value, page) do
              {:ok, matrix} ->
                {:cont, {:ok, %{state | text_matrix: matrix}, spans, joins_previous?}}

              {:error, _} = geometry_error ->
                {:halt, geometry_error}
            end

          {:text, decoded} ->
            case add_span(state, spans, decoded, page, joins_previous?) do
              {:ok, state, spans} ->
                {:cont, {:ok, state, spans, joins_previous? or decoded.text != ""}}

              {:error, _} = span_error ->
                {:halt, span_error}
            end
        end
      end
    )
    |> case do
      {:ok, state, spans, _shown?} -> {:ok, state, spans}
      {:error, _} = array_error -> array_error
    end
  end

  defp add_span(state, spans, decoded, page, join_previous?) do
    if decoded.text == "" do
      {:ok, state, spans}
    else
      with :ok <- TextValidator.validate_next_span(state.shown_span_count, page),
           {:ok, geometry} <- TextValidator.span_geometry(state, decoded, page) do
        span = %{
          text: decoded.text,
          source_index: state.next_source_index,
          x: geometry.x,
          y: geometry.y,
          end_x: geometry.end_x,
          end_y: geometry.end_y,
          font_resource: state.font.name,
          font_size: state.font_size,
          text_matrix: state.text_matrix,
          ctm: state.ctm,
          render_mode: state.render_mode,
          paints_text?: state.render_mode not in [3, 7],
          adds_to_clip_path?: state.render_mode in 4..7,
          joins_previous?: join_previous?
        }

        case state.replacement do
          nil ->
            {:ok,
             %{
               state
               | next_source_index: state.next_source_index + 1,
                 text_matrix: geometry.next_text_matrix,
                 shown_span_count: state.shown_span_count + 1,
                 join_next_span?: true,
                 previous_replacement?: false
             }, [span | spans]}

          replacement ->
            first =
              case replacement.first do
                nil ->
                  span

                first ->
                  %{
                    first
                    | paints_text?: first.paints_text? or span.paints_text?,
                      adds_to_clip_path?: first.adds_to_clip_path? or span.adds_to_clip_path?
                  }
              end

            {:ok,
             %{
               state
               | replacement: %{replacement | first: first, last: span},
                 text_matrix: geometry.next_text_matrix,
                 shown_span_count: state.shown_span_count + 1,
                 join_next_span?: true
             }, spans}
        end
      end
    end
  end

  defp execute_form(form, state, spans, page) do
    case form do
      nil ->
        {:ok, state, spans}

      %{image: image_name} = image ->
        case state.replacement do
          nil ->
            {:ok, state, spans}

          %{text: ""} ->
            {:ok, state, spans}

          replacement ->
            with :ok <- TextValidator.validate_next_span(state.shown_span_count, page),
                 {:ok, geometry} <- TextValidator.image_span_geometry(state, image, page) do
              if geometry.paints? do
                span = %{
                  text: replacement.text,
                  source_index: state.next_source_index,
                  x: geometry.x,
                  y: geometry.y,
                  end_x: geometry.end_x,
                  end_y: geometry.end_y,
                  font_resource: nil,
                  font_size: geometry.height,
                  text_matrix: [1.0, 0.0, 0.0, 1.0, 0.0, 0.0],
                  ctm: state.ctm,
                  render_mode: 0,
                  paints_text?: true,
                  adds_to_clip_path?: false,
                  joins_previous?: false,
                  image_resource: image_name
                }

                first = Map.put(replacement.first || span, :paints_text?, true)

                {:ok,
                 %{
                   state
                   | replacement: %{replacement | first: first, last: span},
                     shown_span_count: state.shown_span_count + 1
                 }, spans}
              else
                {:ok, state, spans}
              end
            end
        end

      form ->
        with {:ok, ctm} <-
               TextValidator.compose_matrix(form.matrix, state.ctm, page, "Do Form Matrix") do
          child_state = %{
            state
            | ctm: ctm,
              stack: [],
              marked_content: [],
              in_text?: false,
              join_next_span?: false
          }

          with {:ok, child_state, child_spans} <- interpret(form.instructions, child_state, page) do
            {:ok,
             %{
               state
               | next_source_index: child_state.next_source_index,
                 shown_span_count: child_state.shown_span_count,
                 replacement: child_state.replacement,
                 previous_replacement?: child_state.previous_replacement?
             }, Enum.reverse(child_spans, spans)}
          end
        end
    end
  end

  defp project_pages(pages, layout?) do
    separator = if layout?, do: "\f", else: "\n"

    pages
    |> Enum.reduce_while({:ok, [], 0, 0}, fn {number, rotation, spans},
                                             {:ok, rendered, bytes, whitespace} ->
      {parts, page_bytes, spaces} =
        if layout? do
          layout_page_projection(spans, rotation)
        else
          parts = plain_page(spans)
          {parts, IO.iodata_length(parts), 0}
        end

      total = bytes + page_bytes + if(rendered == [], do: 0, else: byte_size(separator))
      whitespace = whitespace + spaces

      with :ok <- TextValidator.validate_layout_whitespace([{number, whitespace}]),
           :ok <- TextValidator.validate_output_size(total, number) do
        page = if layout?, do: render_layout_page(parts), else: parts
        {:cont, {:ok, [page | rendered], total, whitespace}}
      else
        error -> {:halt, text_error(error, :extract)}
      end
    end)
    |> case do
      {:ok, rendered, _, _} ->
        {:ok, rendered |> Enum.reverse() |> Enum.intersperse(separator) |> IO.iodata_to_binary()}

      error ->
        error
    end
  end

  defp plain_page(spans) do
    spans
    |> Enum.map_reduce(nil, fn span, previous_source_index ->
      joins_visible_previous? =
        span.joins_previous? and previous_source_index == span.source_index - 1

      separator = if is_nil(previous_source_index) or joins_visible_previous?, do: "", else: " "
      {[separator, span.text], span.source_index}
    end)
    |> elem(0)
  end

  defp layout_page_projection(spans, rotation) do
    lines =
      Enum.map(visual_lines(spans, rotation), fn {line, min_x} ->
        project_line(line, min_x)
      end)

    # Each line separator contributes one UTF-8 byte to the returned page.
    bytes = Enum.reduce(lines, length(lines) - 1, &(&1.bytes + &2))
    {lines, bytes, Enum.sum(Enum.map(lines, & &1.whitespace_bytes))}
  end

  defp render_layout_page(lines) do
    lines
    |> Enum.map(fn line ->
      Enum.map(line.parts, fn {spaces, text} -> [String.duplicate(" ", spaces), text] end)
    end)
    |> Enum.intersperse("\n")
  end

  defp visual_spans(spans, rotation) do
    Enum.flat_map(visual_lines(spans, rotation), fn {line, _min_x} ->
      Enum.map(line, & &1.span)
    end)
  end

  defp visual_lines(spans, rotation) do
    groups = Enum.group_by(spans, &baseline_turn(&1, rotation))

    lines =
      Enum.flat_map(groups, fn {turn, spans} ->
        # Separate text objects can form a column of lines with a common start.
        # Isolated diagram labels otherwise retain display-coordinate order.
        turn =
          if turn != 0 and not Enum.any?(spans, & &1.joins_previous?) and
               not aligned_line_starts?(spans, turn), do: 0, else: turn

        positioned =
          Enum.map(spans, fn span ->
            {x, y} = reading_position(span.x, span.y, turn)
            {end_x, _} = reading_position(span.end_x, span.end_y, turn)
            %{x: x, y: y, end_x: end_x, span: span}
          end)

        min_x = positioned |> Enum.min_by(& &1.x) |> Map.fetch!(:x)

        positioned
        |> Enum.reduce([], &collect_run/2)
        |> Enum.sort_by(&{&1.y, &1.x, &1.index})
        |> group_lines([])
        |> Enum.map(fn {_y, runs} ->
          line = runs |> Enum.sort_by(&{&1.x, &1.index}) |> Enum.flat_map(&Enum.reverse(&1.parts))
          {line, min_x}
        end)
      end)

    if map_size(groups) <= 1 do
      lines
    else
      # Place differently oriented lines on the page's predominant reading axes.
      {turn, _} =
        Enum.max_by(groups, fn {_turn, items} -> {length(items), -hd(items).source_index} end)

      Enum.sort_by(lines, fn {[%{span: first} | _], _min_x} ->
        {x, y} = reading_position(first.x, first.y, turn)
        {y, x, first.source_index}
      end)
    end
  end

  defp aligned_line_starts?([first | rest], turn) do
    {start, baseline} = reading_position(first.x, first.y, turn)

    Enum.reduce_while(rest, false, fn span, separate ->
      {x, y} = reading_position(span.x, span.y, turn)

      if abs(x - start) <= 1.5,
        do: {:cont, separate or abs(y - baseline) > 1.5},
        else: {:halt, false}
    end)
  end

  # The first column of the text-to-page transform gives the baseline even
  # for zero-width operands. Display coordinates already include page rotation.
  defp baseline_turn(span, rotation) do
    [a, b | _] = span.text_matrix
    [xx, xy, yx, yy | _] = span.ctm
    dx = a * xx + b * yx
    dy = -(a * xy + b * yy)

    turn =
      cond do
        abs(dy) > abs(dx) -> if dy > 0, do: 1, else: 3
        dx < 0 -> 2
        true -> 0
      end

    rem(turn + div(rotation, 90), 4)
  end

  defp reading_position(x, y, turn) do
    case turn do
      0 -> {x, y}
      1 -> {y, -x}
      2 -> {-x, -y}
      3 -> {-y, x}
    end
  end

  defp collect_run(position, runs) do
    case runs do
      [%{parts: [previous | _]} = run | rest]
      when position.span.joins_previous? and
             position.span.source_index == previous.span.source_index + 1 and
             abs(position.y - run.y) <= 1.5 ->
        [%{run | parts: [position | run.parts]} | rest]

      _ ->
        [
          %{x: position.x, y: position.y, index: position.span.source_index, parts: [position]}
          | runs
        ]
    end
  end

  defp group_lines(runs, lines) do
    case {runs, lines} do
      {[], _} ->
        Enum.reverse(lines)

      {[run | rest], [{y, current} | previous]} when abs(run.y - y) <= 1.5 ->
        group_lines(rest, [{y, [run | current]} | previous])

      {[run | rest], _} ->
        group_lines(rest, [{run.y, [run]} | lines])
    end
  end

  defp project_line(spans, min_x) do
    {parts, _current_x, _first?} =
      Enum.reduce(spans, {[], min_x, true}, fn position, {parts, current_x, first?} ->
        space_width = max(position.span.font_size * 0.25, 4.0)
        gap = position.x - current_x

        spaces =
          cond do
            first? -> max(round((position.x - min_x) / space_width), 0)
            gap > space_width * 0.35 -> max(round(gap / space_width), 1)
            true -> 0
          end

        {[{spaces, position.span.text} | parts], max(position.end_x, position.x), false}
      end)

    parts = trim_line_end(parts)

    {bytes, whitespace_bytes} =
      Enum.reduce(parts, {0, 0}, fn {spaces, text}, {bytes, whitespace} ->
        {bytes + spaces + byte_size(text), whitespace + spaces}
      end)

    %{parts: parts, bytes: bytes, whitespace_bytes: whitespace_bytes}
  end

  # Trim the reversed parts before expanding any coordinate-derived spaces.
  defp trim_line_end(parts) do
    case parts do
      [] ->
        []

      [{spaces, text} | rest] ->
        case String.trim_trailing(text) do
          "" -> trim_line_end(rest)
          trimmed -> Enum.reverse(rest, [{spaces, trimmed}])
        end
    end
  end

  defp text_state(page, actual_text) do
    %{
      ctm: identity(),
      text_matrix: identity(),
      line_matrix: identity(),
      font: nil,
      font_size: 0.0,
      char_spacing: 0.0,
      word_spacing: 0.0,
      horizontal_scale: 100.0,
      leading: 0.0,
      rise: 0.0,
      render_mode: 0,
      in_text?: false,
      join_next_span?: false,
      stack: [],
      next_source_index: 0,
      shown_span_count: 0,
      marked_content: [],
      actual_text?: actual_text,
      replacement: nil,
      previous_replacement?: false,
      rotation: page.rotation,
      page: %{media_box: page.media_box, rotation: page.rotation}
    }
  end

  defp identity, do: [1.0, 0.0, 0.0, 1.0, 0.0, 0.0]

  defp text_error({:error, {reason, diagnostic}}, operation) do
    {:error,
     {reason, diagnostic |> Map.put(:operation, operation) |> Map.put(:module, __MODULE__)}}
  end

  defp error(stage, reason, message, details \\ []) do
    {pdf_details, diagnostic_options} = Keyword.split(details, [:page])

    {:error, {reason, diagnostic}} =
      Diagnostics.error(
        stage,
        reason,
        message,
        Keyword.merge([operation: :extract, module: __MODULE__], diagnostic_options)
      )

    {:error, {reason, with_debug_details(diagnostic, pdf_details)}}
  end

  defp with_debug_details(diagnostic, details) do
    message =
      Enum.reduce(details, diagnostic.message, fn detail, message ->
        case detail do
          {:page, page} ->
            "#{message}; page #{page}"
        end
      end)

    Map.put(diagnostic, :message, message)
  end
end
