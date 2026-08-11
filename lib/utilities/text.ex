defmodule NativeElixirPdfUtilities.Text do
  @moduledoc """
  Strict native extraction of embedded Unicode text from PDF documents.

  Extraction resolves the PDF page tree, resources, content streams, Form
  XObjects, and the active font at every text operation. It succeeds only when
  every shown text string has a reliable Unicode mapping; it never guesses from
  an embedded font program or merges CMaps from unrelated fonts.

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
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Pdf.TextEncoding
  alias NativeElixirPdfUtilities.Validators.TextValidator
  import Bitwise

  @max_cmap_bytes 1_000_000
  @max_cmap_entries 100_000
  @max_cid 65_535
  @max_cid_width_entries 65_536
  @max_form_depth 20
  @max_text_spans 25_000
  @extract_option_keys [:layout]
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
  @type span_option :: {:order, :source | :visual}
  @typedoc "A PDF text rendering mode from 0 through 7."
  @type render_mode :: 0..7
  @typedoc "A six-value PDF affine matrix in `[a, b, c, d, e, f]` order."
  @type matrix :: [float()]

  @typedoc """
  A decoded text-showing operand and its positioned extraction context.

  `source_index` is zero-based within a page and follows text-show execution
  order. Page content streams are traversed in `/Contents` order, and Form
  XObjects are traversed where their `Do` operator occurs. Reusing a Form
  therefore produces new spans with new indexes.

  `x`, `y`, `end_x`, and `end_y` describe the text baseline in normalized
  display coordinates. The origin is the top-left of the rotated MediaBox, X
  increases rightward, Y increases downward, and page rotation and Form CTMs
  are applied. These points are not glyph bounding boxes. The end point uses
  the PDF font widths available to the extractor and can be approximate when a
  font omits explicit metrics.

  `text_matrix` is the PDF text matrix at the start of the operand. `ctm` is the
  active PDF current transformation matrix, including Form transforms. Neither
  matrix includes the final MediaBox/page-rotation normalization represented by
  the baseline coordinates.

  `font_resource` is the active PDF resource name, not a guaranteed font family
  or PostScript name. `font_size` is the `Tf` text-space size, not a calculated
  display-space height.

  `paints_text?` and `adds_to_clip_path?` are derived only from `render_mode`.
  They describe the requested PDF text rendering operation; they do not claim
  that the text is visually visible or clipped. `joins_previous?` identifies a
  text-showing operand that immediately continues the preceding operand without
  an intervening text-positioning, text-object, content-form, or graphics-matrix
  boundary. This follows PDF text-operator execution rather than inferring
  continuity from display coordinates.
  """
  @type text_span :: %{
          text: String.t(),
          source_index: non_neg_integer(),
          x: float(),
          y: float(),
          end_x: float(),
          end_y: float(),
          font_resource: String.t(),
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
          :encrypted_pdf
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
    cond do
      not is_binary(pdf_binary) ->
        error(:input, :invalid_pdf_input, "PDF input must be a binary")

      not Keyword.keyword?(opts) ->
        error(:options, :invalid_options, "extract options must be a keyword list")

      Enum.any?(Keyword.keys(opts), &(&1 not in @extract_option_keys)) ->
        unknown =
          opts
          |> Keyword.keys()
          |> Enum.reject(&(&1 in @extract_option_keys))
          |> Enum.uniq()
          |> Enum.sort()

        error(
          :options,
          :invalid_options,
          "extract options contain unsupported keys: #{inspect(unknown)}"
        )

      not is_boolean(Keyword.get(opts, :layout, true)) ->
        error(:options, :invalid_options, "layout option must be a boolean")

      true ->
        with {:ok, pdf_context} <- Reader.read_validated(pdf_binary),
             {:ok, text_context} <- TextValidator.prepare(pdf_context),
             {:ok, pages} <- extract_pages(text_context) do
          visible_pages =
            pages
            |> Enum.map(fn page -> Enum.filter(page.spans, & &1.paints_text?) end)
            |> Enum.reject(&(&1 == []))

          case visible_pages do
            [] ->
              error(:text_extraction, :no_extractable_text, "PDF contains no extractable text")

            pages ->
              text =
                if Keyword.get(opts, :layout, true) do
                  pages |> Enum.map(&layout_page/1) |> Enum.join("\f")
                else
                  pages |> Enum.map(&plain_page/1) |> Enum.join("\n")
                end

              {:ok, text}
          end
        else
          {:error, _} = reader_error -> text_error(reader_error, :extract)
        end
    end
  end

  @doc """
  Extracts positioned, reliably decoded text spans from a PDF binary.

  The result preserves every resolved page and every non-empty text operand
  that can be decoded, including rendering modes 3 (neither painted nor added
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
    cond do
      not is_binary(pdf_binary) ->
        error(:input, :invalid_pdf_input, "PDF input must be a binary", operation: :extract_spans)

      not Keyword.keyword?(opts) ->
        error(:options, :invalid_options, "extract span options must be a keyword list",
          operation: :extract_spans
        )

      Enum.any?(Keyword.keys(opts), &(&1 != :order)) ->
        error(:options, :invalid_options, "extract span options contain an unknown option",
          operation: :extract_spans
        )

      Keyword.get(opts, :order, :source) not in [:source, :visual] ->
        error(:options, :invalid_options, "span order must be :source or :visual",
          operation: :extract_spans
        )

      true ->
        with {:ok, pdf_context} <- Reader.read_validated(pdf_binary),
             {:ok, text_context} <- TextValidator.prepare(pdf_context),
             {:ok, pages} <- extract_pages(text_context) do
          pages =
            if Keyword.get(opts, :order, :source) == :visual do
              Enum.map(pages, fn page -> %{page | spans: visual_spans(page.spans)} end)
            else
              pages
            end

          {:ok, %{page_count: length(pages), pages: pages}}
        else
          {:error, _} = extraction_error -> text_error(extraction_error, :extract_spans)
        end
    end
  end

  @doc """
  Reads a PDF file and extracts reliably decodable embedded text from it.
  """
  @spec extract_file(String.t(), [extract_option()]) ::
          {:ok, String.t()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def extract_file(path, opts \\ []) do
    extract_file_with(path, opts, :extract_file, &extract/2)
  end

  @doc """
  Reads a PDF file and extracts its page-preserving positioned text spans.

  This has the same ordering, geometry, preservation, and strict diagnostic
  behavior as `extract_spans/2`. File extraction errors include the source path.
  """
  @spec extract_file_spans(String.t(), [span_option()]) ::
          {:ok, text_document()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def extract_file_spans(path, opts \\ []) do
    extract_file_with(path, opts, :extract_file_spans, &extract_spans/2)
  end

  defp extract_file_with(path, opts, operation, extractor) do
    case path do
      path when is_binary(path) ->
        case File.read(path) do
          {:ok, pdf_binary} ->
            case extractor.(pdf_binary, opts) do
              {:ok, result} ->
                {:ok, result}

              {:error, {reason, diagnostic}} ->
                {:error,
                 {reason, diagnostic |> Map.put(:source, path) |> Map.put(:operation, operation)}}
            end

          {:error, reason} ->
            error(:file, reason, "file read failed: #{reason}", operation: :read, source: path)
        end

      _ ->
        error(:file, :invalid_path, "path must be a string", operation: operation)
    end
  end

  defp extract_pages(text_context) do
    text_context.pages
    |> Enum.reduce_while({:ok, []}, fn page, {:ok, pages} ->
      case extract_page(text_context.document, page) do
        {:ok, page} -> {:cont, {:ok, [page | pages]}}
        {:error, _} = extraction_error -> {:halt, extraction_error}
      end
    end)
    |> case do
      {:ok, pages} -> {:ok, Enum.reverse(pages)}
      error -> error
    end
  end

  defp extract_page(document, page) do
    initial_state = text_state(page)

    Enum.reduce_while(page.contents, {:ok, initial_state, []}, fn operations,
                                                                  {:ok, state, spans} ->
      case interpret(operations, document, page.resources, state, page.number, 0) do
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

  defp interpret(operations, document, resources, state, page_number, depth) do
    Enum.reduce_while(operations, {:ok, state, []}, fn operation, {:ok, state, spans} ->
      case apply_operator(
             operation.operator,
             operation.operands,
             state,
             spans,
             document,
             resources,
             page_number,
             depth
           ) do
        {:ok, state, spans} -> {:cont, {:ok, state, spans}}
        {:error, _} = extraction_error -> {:halt, extraction_error}
      end
    end)
    |> case do
      {:ok, state, spans} -> {:ok, state, Enum.reverse(spans)}
      {:error, _} = extraction_error -> extraction_error
    end
  end

  defp apply_operator(operator, operands, state, spans, document, resources, page, depth) do
    case {operator, operands} do
      {"q", []} ->
        saved_state = Map.take(state, @graphics_state_fields)
        {:ok, %{state | stack: [saved_state | state.stack]}, spans}

      {"Q", []} ->
        case state.stack do
          [saved_state | stack] ->
            restored_state =
              state
              |> Map.merge(saved_state)
              |> Map.put(:stack, stack)
              |> break_text_join()

            {:ok, restored_state, spans}

          [] ->
            error(:content, :invalid_pdf_input, "Q has no matching q", page: page)
        end

      {"cm", matrix} ->
        {:ok,
         state
         |> Map.put(:ctm, multiply(matrix, state.ctm))
         |> break_text_join(), spans}

      {"BT", []} ->
        {:ok,
         %{
           state
           | in_text?: true,
             text_matrix: identity(),
             line_matrix: identity(),
             join_next_span?: false
         }, spans}

      {"ET", []} ->
        {:ok, %{state | in_text?: false, join_next_span?: false}, spans}

      {"Tf", [{:name, font_name}, size]} ->
        with {:ok, font} <- resolve_font(document, resources, font_name, page) do
          {:ok, %{state | font: font, font_size: size}, spans}
        end

      {"Tm", matrix} ->
        {:ok, %{state | text_matrix: matrix, line_matrix: matrix, join_next_span?: false}, spans}

      {operator, [tx, ty]} when operator in ["Td", "TD"] ->
        line_matrix = translate(state.line_matrix, tx, ty)

        state = %{
          state
          | line_matrix: line_matrix,
            text_matrix: line_matrix,
            leading: if(operator == "TD", do: -ty, else: state.leading),
            join_next_span?: false
        }

        {:ok, state, spans}

      {"T*", []} ->
        line_matrix = translate(state.line_matrix, 0.0, -state.leading)

        {:ok,
         %{state | line_matrix: line_matrix, text_matrix: line_matrix, join_next_span?: false},
         spans}

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

      {"Tj", [string]} ->
        show(state, spans, string, page)

      {"TJ", [{:array, values}]} ->
        if state.in_text? do
          show_array(values, state, spans, page)
        else
          error(:content, :invalid_pdf_input, "TJ appears outside a text object", page: page)
        end

      {"'", [string]} ->
        with {:ok, state, spans} <- apply_operator("T*", [], state, spans, nil, nil, page, 0) do
          show(state, spans, string, page)
        end

      {"\"", [word_spacing, char_spacing, string]} ->
        state = %{state | word_spacing: word_spacing, char_spacing: char_spacing}

        with {:ok, state, spans} <- apply_operator("T*", [], state, spans, nil, nil, page, 0) do
          show(state, spans, string, page)
        end

      {"Do", [{:name, name}]} ->
        state = break_text_join(state)

        case execute_form(name, state, spans, document, resources, page, depth) do
          {:ok, state, spans} -> {:ok, break_text_join(state), spans}
          {:error, _} = form_error -> form_error
        end

      _ ->
        {:ok, state, spans}
    end
  end

  defp break_text_join(state) do
    Map.put(state, :join_next_span?, false)
  end

  defp show(state, spans, string, page) do
    if state.in_text? do
      with {:ok, decoded} <- decode_string(string, state.font, page),
           {:ok, state, spans} <-
             add_span(state, spans, decoded, page, state.join_next_span?) do
        {:ok, state, spans}
      end
    else
      error(:content, :invalid_pdf_input, "text-showing operator appears outside a text object",
        page: page
      )
    end
  end

  defp show_array(values, state, spans, page) do
    values
    |> Enum.reduce_while(
      {:ok, state, spans, state.join_next_span?},
      fn value, {:ok, state, spans, joins_previous?} ->
        case TextValidator.number(value) do
          {:ok, value} ->
            adjustment = -value / 1000.0 * state.font_size * state.horizontal_scale / 100.0

            {:cont,
             {:ok, %{state | text_matrix: translate(state.text_matrix, adjustment, 0.0)}, spans,
              joins_previous?}}

          :error ->
            case decode_string(value, state.font, page) do
              {:ok, decoded} ->
                case add_span(state, spans, decoded, page, joins_previous?) do
                  {:ok, state, spans} ->
                    {:cont, {:ok, state, spans, joins_previous? or decoded.text != ""}}

                  {:error, _} = span_error ->
                    {:halt, span_error}
                end

              {:error, _} = decoding_error ->
                {:halt, decoding_error}
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
    cond do
      decoded.text == "" ->
        {:ok, state, spans}

      state.next_source_index >= @max_text_spans ->
        error(:limits, :resource_limit_exceeded, "text span count exceeds the limit", page: page)

      true ->
        next_state = advance_text(state, decoded)

        [_, _, _, _, x, y] =
          state.text_matrix |> translate(0.0, state.rise) |> multiply(state.ctm)

        [_, _, _, _, end_x, end_y] =
          next_state.text_matrix |> translate(0.0, state.rise) |> multiply(state.ctm)

        {x, y} = display_position(x, y, state.page)
        {end_x, end_y} = display_position(end_x, end_y, state.page)

        span = %{
          text: decoded.text,
          source_index: state.next_source_index,
          x: x,
          y: y,
          end_x: end_x,
          end_y: end_y,
          font_resource: state.font.name,
          font_size: state.font_size,
          text_matrix: state.text_matrix,
          ctm: state.ctm,
          render_mode: state.render_mode,
          paints_text?: state.render_mode not in [3, 7],
          adds_to_clip_path?: state.render_mode in 4..7,
          joins_previous?: join_previous?
        }

        {:ok,
         %{
           next_state
           | next_source_index: state.next_source_index + 1,
             join_next_span?: true
         }, [span | spans]}
    end
  end

  defp advance_text(state, decoded) do
    width_codes = Map.get(decoded, :width_codes, decoded.codes)
    glyph_width = Enum.reduce(width_codes, 0, &(font_width(state.font, &1) + &2))
    glyph_count = length(decoded.codes)
    spaces = Enum.count(decoded.codes, &(&1 == 32))

    width =
      (glyph_width / 1000.0 * state.font_size + state.char_spacing * glyph_count +
         state.word_spacing * spaces) *
        state.horizontal_scale / 100.0

    %{state | text_matrix: translate(state.text_matrix, width, 0.0)}
  end

  defp execute_form(name, state, spans, document, resources, page, depth) do
    if depth >= @max_form_depth do
      error(:limits, :resource_limit_exceeded, "Form XObject nesting exceeds the limit",
        page: page
      )
    else
      with {:ok, resources} <- Reader.dictionary(document, resources),
           {:ok, xobjects} <- Reader.dictionary(document, Map.get(resources, "XObject")),
           {:ok, xobject_ref} <- required_value(xobjects, name, "XObject", page),
           {:ok, xobject} <- Reader.dictionary(document, xobject_ref) do
        if name?(Map.get(xobject, "Subtype"), "Form") do
          with {:ok, stream} <- Reader.decoded_stream(document, xobject_ref),
               {:ok, operations} <- TextValidator.instructions(stream, page),
               {:ok, form_matrix} <- matrix_value(Map.get(xobject, "Matrix"), page),
               {:ok, child_state, child_spans} <-
                 interpret(
                   operations,
                   document,
                   Map.get(xobject, "Resources", resources),
                   %{
                     state
                     | ctm: multiply(form_matrix, state.ctm),
                       stack: []
                   },
                   page,
                   depth + 1
                 ) do
            {:ok, %{state | next_source_index: child_state.next_source_index},
             Enum.reverse(child_spans, spans)}
          end
        else
          {:ok, state, spans}
        end
      else
        {:error, _} = form_error -> form_error
      end
    end
  end

  defp resolve_font(document, resources, font_name, page) do
    with {:ok, resources} <- Reader.dictionary(document, resources),
         {:ok, fonts} <- Reader.dictionary(document, Map.get(resources, "Font")),
         {:ok, font_ref} <- required_value(fonts, font_name, "font", page),
         {:ok, font} <- Reader.dictionary(document, font_ref) do
      cmap =
        case Map.get(font, "ToUnicode") do
          nil ->
            {:ok, nil}

          cmap_ref ->
            with {:ok, stream} <- Reader.decoded_stream(document, cmap_ref),
                 do: parse_cmap(stream, page, font_name)
        end

      with {:ok, cmap} <- cmap,
           {:ok, cid_encoding} <- type0_cid_encoding(document, font, page, font_name),
           {:ok, widths, default_width} <- font_metrics(document, font) do
        {:ok,
         %{
           name: font_name,
           dictionary: font,
           cmap: cmap,
           cid_encoding: cid_encoding,
           document: document,
           widths: widths,
           default_width: default_width
         }}
      end
    else
      {:error, {reason, diagnostic}} ->
        {:error, {reason, with_debug_details(diagnostic, page: page, font: font_name)}}
    end
  end

  defp font_metrics(document, font) do
    if name?(Map.get(font, "Subtype"), "Type0") do
      type0_font_metrics(document, font)
    else
      simple_font_metrics(document, font)
    end
  end

  defp type0_cid_encoding(document, font, page, font_name) do
    case name?(Map.get(font, "Subtype"), "Type0") do
      true ->
        case Map.get(font, "Encoding") do
          {:name, "Identity-H"} ->
            {:ok, :identity}

          {:name, _name} ->
            error(
              :cmap,
              :unsupported_text_encoding,
              "predefined Type0 Encoding CMaps other than Identity-H are unsupported",
              page: page,
              font: font_name
            )

          nil ->
            error(:font, :invalid_pdf_input, "Type0 font Encoding entry is missing",
              page: page,
              font: font_name
            )

          encoding ->
            with {:ok, dictionary} <- Reader.dictionary(document, encoding),
                 {:ok, stream} <- Reader.decoded_stream(document, encoding) do
              case {Map.get(dictionary, "UseCMap"), Map.get(dictionary, "WMode", 0)} do
                {nil, 0} ->
                  parse_cid_cmap(stream, page, font_name)

                {nil, _vertical} ->
                  error(
                    :cmap,
                    :unsupported_text_encoding,
                    "vertical Type0 Encoding CMaps are unsupported",
                    page: page,
                    font: font_name
                  )

                {_use_cmap, _writing_mode} ->
                  error(
                    :cmap,
                    :unsupported_text_encoding,
                    "Type0 Encoding UseCMap inheritance is unsupported",
                    page: page,
                    font: font_name
                  )
              end
            end
        end

      false ->
        {:ok, nil}
    end
  end

  defp simple_font_metrics(document, font) do
    first_char = Map.get(font, "FirstChar", 0)

    with true <- is_integer(first_char) and first_char >= 0,
         {:ok, widths} <- Reader.resolve(document, Map.get(font, "Widths")),
         true <- is_nil(widths) or (is_list(widths) and Enum.all?(widths, &is_number/1)),
         {:ok, default_width} <- simple_default_width(document, font) do
      width_map =
        case widths do
          nil ->
            %{}

          widths ->
            widths
            |> Enum.with_index(first_char)
            |> Map.new(fn {width, code} -> {code, width} end)
        end

      {:ok, width_map, default_width}
    else
      false -> error(:font, :invalid_pdf_input, "simple font width metrics are malformed")
      {:error, _} = metric_error -> metric_error
    end
  end

  defp simple_default_width(document, font) do
    case Map.get(font, "FontDescriptor") do
      nil ->
        {:ok, 500}

      descriptor ->
        with {:ok, descriptor} <- Reader.dictionary(document, descriptor) do
          case Map.get(descriptor, "MissingWidth", 500) do
            width when is_number(width) -> {:ok, width}
            _ -> error(:font, :invalid_pdf_input, "font MissingWidth is malformed")
          end
        end
    end
  end

  defp type0_font_metrics(document, font) do
    with {:ok, descendants} <- Reader.resolve(document, Map.get(font, "DescendantFonts")),
         [descendant | _] <- descendants,
         {:ok, descendant} <- Reader.dictionary(document, descendant),
         default_width when is_number(default_width) <- Map.get(descendant, "DW", 1000),
         {:ok, widths} <- Reader.resolve(document, Map.get(descendant, "W")),
         {:ok, widths} <- cid_widths(widths) do
      {:ok, widths, default_width}
    else
      {:error, _} = metric_error -> metric_error
      _ -> error(:font, :invalid_pdf_input, "Type0 descendant font metrics are malformed")
    end
  end

  defp cid_widths(values) do
    case values do
      nil ->
        {:ok, %{}}

      values when is_list(values) ->
        parse_cid_widths(values, %{}, 0)

      _ ->
        error(:font, :invalid_pdf_input, "CID font W array is malformed")
    end
  end

  defp parse_cid_widths(values, widths, entry_count) do
    case values do
      [] ->
        {:ok, widths}

      [first, listed | rest] when is_integer(first) and first >= 0 and is_list(listed) ->
        count = length(listed)

        with true <- Enum.all?(listed, &is_number/1),
             :ok <- validate_cid_width_entries(first, count, entry_count) do
          listed_widths =
            Map.new(Enum.with_index(listed, first), fn {width, code} -> {code, width} end)

          parse_cid_widths(rest, Map.merge(widths, listed_widths), entry_count + count)
        else
          false -> error(:font, :invalid_pdf_input, "CID font listed widths are malformed")
          {:error, _} = width_error -> width_error
        end

      [first, last, width | rest]
      when is_integer(first) and first >= 0 and is_integer(last) and last >= first and
             is_number(width) ->
        count = last - first + 1

        with :ok <- validate_cid_width_entries(first, count, entry_count) do
          range_widths = Map.new(first..last, &{&1, width})
          parse_cid_widths(rest, Map.merge(widths, range_widths), entry_count + count)
        end

      _ ->
        error(:font, :invalid_pdf_input, "CID font W array is malformed")
    end
  end

  defp validate_cid_width_entries(first, count, entry_count) do
    cond do
      entry_count + count > @max_cid_width_entries ->
        error(:font, :resource_limit_exceeded, "CID font width entry count exceeds the limit")

      first > @max_cid or (count > 0 and first + count - 1 > @max_cid) ->
        error(:font, :invalid_pdf_input, "CID font W array contains an out-of-range CID")

      true ->
        :ok
    end
  end

  defp font_width(font, code) do
    Map.get(font.widths, code, font.default_width)
  end

  defp decode_string(string, font, page) do
    {_kind, bytes} = string

    cond do
      is_nil(font) ->
        error(
          :text_encoding,
          :unsupported_text_encoding,
          "text is shown without an active font",
          page: page
        )

      font.cmap ->
        with {:ok, decoded} <- decode_cmap(bytes, font.cmap, page, font.name) do
          {:ok, Map.put(decoded, :width_codes, width_codes(font, decoded))}
        end

      name?(Map.get(font.dictionary, "Subtype"), "Type0") ->
        error(:text_encoding, :unsupported_text_encoding, "Type0 font has no ToUnicode CMap",
          page: page,
          font: font.name
        )

      true ->
        decode_simple_font(bytes, font, page)
    end
  end

  defp decode_simple_font(bytes, font, page) do
    with {:ok, encoding, differences} <- font_encoding(font.document, font.dictionary),
         {:ok, decoded} <- decode_simple_bytes(bytes, encoding, differences) do
      {:ok, decoded}
    else
      {:error, {reason, diagnostic}} ->
        {:error, {reason, with_debug_details(diagnostic, page: page, font: font.name)}}
    end
  end

  defp font_encoding(document, font) do
    case Map.get(font, "Encoding") do
      nil ->
        with {:ok, encoding} <- default_font_encoding(font) do
          {:ok, encoding, %{}}
        end

      {:name, name} ->
        if TextEncoding.supported?(name) do
          {:ok, name, %{}}
        else
          error(:text_encoding, :unsupported_text_encoding, "simple font encoding is unsupported")
        end

      encoding_ref ->
        with {:ok, encoding} <- Reader.dictionary(document, encoding_ref) do
          base =
            case Map.get(encoding, "BaseEncoding") do
              {:name, name} -> {:ok, name}
              nil -> default_font_encoding(font)
              _ -> :error
            end

          with {:ok, base} <- base,
               true <- TextEncoding.supported?(base),
               {:ok, differences} <- differences(Map.get(encoding, "Differences")) do
            {:ok, base, differences}
          else
            false ->
              error(
                :text_encoding,
                :unsupported_text_encoding,
                "simple font base encoding is unsupported"
              )

            :error ->
              error(:text_encoding, :invalid_pdf_input, "simple font Encoding is malformed")

            {:error, _} = encoding_error ->
              encoding_error
          end
        end
    end
  end

  defp default_font_encoding(font) do
    case Map.get(font, "BaseFont") do
      {:name, base_font} when is_binary(base_font) ->
        if String.valid?(base_font) do
          base_font = base_font |> String.split("+") |> List.last()

          case base_font do
            "Symbol" ->
              {:ok, "SymbolEncoding"}

            "ZapfDingbats" ->
              {:ok, "ZapfDingbatsEncoding"}

            base_font
            when base_font in [
                   "Times-Roman",
                   "Times-Bold",
                   "Times-Italic",
                   "Times-BoldItalic",
                   "Helvetica",
                   "Helvetica-Bold",
                   "Helvetica-Oblique",
                   "Helvetica-BoldOblique",
                   "Courier",
                   "Courier-Bold",
                   "Courier-Oblique",
                   "Courier-BoldOblique"
                 ] ->
              {:ok, "StandardEncoding"}

            _ ->
              error(
                :text_encoding,
                :unsupported_text_encoding,
                "custom simple font has no reliable Unicode encoding"
              )
          end
        else
          error(:text_encoding, :invalid_pdf_input, "font BaseFont name is malformed")
        end

      _ ->
        error(
          :text_encoding,
          :unsupported_text_encoding,
          "simple font has no declared or standard base encoding"
        )
    end
  end

  defp differences(values) do
    case values do
      nil ->
        {:ok, %{}}

      values when is_list(values) ->
        values
        |> Enum.reduce_while({:ok, %{}, nil}, fn value, {:ok, mappings, code} ->
          case value do
            value when is_integer(value) and value in 0..255 ->
              {:cont, {:ok, mappings, value}}

            {:name, glyph} when is_integer(code) and code in 0..255 ->
              {:cont, {:ok, Map.put(mappings, code, glyph), code + 1}}

            _ ->
              {:halt,
               error(:text_encoding, :invalid_pdf_input, "font Differences array is malformed")}
          end
        end)
        |> case do
          {:ok, mappings, _code} -> {:ok, mappings}
          {:error, _} = differences_error -> differences_error
        end

      _ ->
        error(:text_encoding, :invalid_pdf_input, "font Differences entry is malformed")
    end
  end

  defp decode_simple_bytes(bytes, encoding, differences) do
    bytes
    |> :binary.bin_to_list()
    |> Enum.reduce_while({:ok, []}, fn code, {:ok, characters} ->
      case TextEncoding.character(encoding, code, differences) do
        {:ok, character} ->
          {:cont, {:ok, [character | characters]}}

        :error ->
          {:halt,
           error(
             :text_encoding,
             :unsupported_text_encoding,
             "font encoding cannot be converted to Unicode"
           )}
      end
    end)
    |> case do
      {:ok, characters} ->
        {:ok,
         %{text: characters |> Enum.reverse() |> Enum.join(), codes: :binary.bin_to_list(bytes)}}

      decoding_error ->
        decoding_error
    end
  end

  defp width_codes(font, decoded) do
    case font.cid_encoding do
      :identity ->
        Enum.map(decoded.source_codes, &:binary.decode_unsigned/1)

      %{mappings: mappings, notdef: notdef} ->
        Enum.map(decoded.source_codes, fn source ->
          Map.get(mappings, source, Map.get(notdef, source, 0))
        end)

      nil ->
        decoded.codes
    end
  end

  defp parse_cmap(stream, page, font) do
    cond do
      byte_size(stream) > @max_cmap_bytes ->
        error(:cmap, :resource_limit_exceeded, "ToUnicode CMap exceeds the byte limit",
          page: page,
          font: font
        )

      Regex.match?(~r/\/\S+\s+usecmap\b/, stream) ->
        error(
          :cmap,
          :unsupported_text_encoding,
          "ToUnicode usecmap inheritance is unsupported",
          page: page,
          font: font
        )

      true ->
        with {:ok, codespaces} <- parse_codespaces(stream, "ToUnicode"),
             {:ok, bfchar} <- parse_bfchar(stream),
             {:ok, bfrange} <- parse_bfrange(stream, map_size(bfchar)),
             mappings = Map.merge(bfrange, bfchar),
             true <- map_size(mappings) <= @max_cmap_entries,
             true <- map_size(mappings) > 0 do
          {:ok, %{codespaces: codespaces, mappings: mappings}}
        else
          false ->
            error(
              :cmap,
              :unsupported_text_encoding,
              "ToUnicode CMap has no usable Unicode mappings",
              page: page,
              font: font
            )

          {:error, _} = error ->
            error
        end
    end
  end

  defp parse_cid_cmap(stream, page, font) do
    cond do
      byte_size(stream) > @max_cmap_bytes ->
        error(:cmap, :resource_limit_exceeded, "Type0 Encoding CMap exceeds the byte limit",
          page: page,
          font: font
        )

      Regex.match?(~r/\/\S+\s+usecmap\b/, stream) ->
        error(
          :cmap,
          :unsupported_text_encoding,
          "Type0 Encoding usecmap inheritance is unsupported",
          page: page,
          font: font
        )

      Regex.match?(~r/\/WMode\s+1\s+def\b/, stream) ->
        error(
          :cmap,
          :unsupported_text_encoding,
          "vertical Type0 Encoding CMaps are unsupported",
          page: page,
          font: font
        )

      true ->
        with {:ok, codespaces} <- parse_codespaces(stream, "Type0 Encoding"),
             {:ok, cidchar} <- parse_cid_char(stream, "cidchar", 0),
             {:ok, cidrange} <-
               parse_cid_range(stream, "cidrange", map_size(cidchar), :sequential),
             mappings = Map.merge(cidrange, cidchar),
             {:ok, notdefchar} <-
               parse_cid_char(stream, "notdefchar", map_size(mappings)),
             {:ok, notdefrange} <-
               parse_cid_range(
                 stream,
                 "notdefrange",
                 map_size(mappings) + map_size(notdefchar),
                 :constant
               ),
             notdef = Map.merge(notdefrange, notdefchar) do
          {:ok, %{codespaces: codespaces, mappings: mappings, notdef: notdef}}
        else
          :limit ->
            error(
              :cmap,
              :resource_limit_exceeded,
              "Type0 Encoding CMap entry count exceeds the limit",
              page: page,
              font: font
            )

          :error ->
            error(:cmap, :invalid_pdf_input, "Type0 Encoding CMap mappings are malformed",
              page: page,
              font: font
            )

          {:error, _} = cmap_error ->
            cmap_error
        end
    end
  end

  defp parse_codespaces(stream, label) do
    sections = Regex.scan(~r/(\d+)\s+begincodespacerange\s*(.*?)\s*endcodespacerange/s, stream)

    sections
    |> Enum.reduce_while({:ok, []}, fn [_, count, section], {:ok, values} ->
      entries = Regex.scan(~r/<([0-9A-Fa-f]+)>\s*<([0-9A-Fa-f]+)>/, section)

      valid? =
        String.to_integer(count) == length(entries) and
          cmap_section_consumed?(section, ~r/<[0-9A-Fa-f]+>\s*<[0-9A-Fa-f]+>/)

      if valid? do
        parsed =
          Enum.map(entries, fn [_, first, last] -> {hex_bytes(first), hex_bytes(last)} end)

        if Enum.all?(parsed, fn {first, last} ->
             is_binary(first) and byte_size(first) == byte_size(last) and first <= last
           end) do
          {:cont, {:ok, values ++ parsed}}
        else
          {:halt, error(:cmap, :invalid_pdf_input, "#{label} codespace range is malformed")}
        end
      else
        {:halt, error(:cmap, :invalid_pdf_input, "#{label} codespace count is malformed")}
      end
    end)
    |> case do
      {:ok, []} -> error(:cmap, :invalid_pdf_input, "#{label} codespace range is missing")
      result -> result
    end
  end

  defp parse_cid_char(stream, operator, existing) do
    pattern = Regex.compile!("(\\d+)\\s+begin#{operator}\\s*(.*?)\\s*end#{operator}", "s")

    Regex.scan(pattern, stream)
    |> Enum.reduce_while({:ok, %{}}, fn [_, count, section], {:ok, mappings} ->
      entries = Regex.scan(~r/<([0-9A-Fa-f]+)>\s+(\d+)/, section)

      valid? =
        String.to_integer(count) == length(entries) and
          cmap_section_consumed?(section, ~r/<[0-9A-Fa-f]+>\s+\d+/)

      case valid? do
        true ->
          parsed =
            Enum.map(entries, fn [_, source, cid] ->
              {hex_bytes(source), String.to_integer(cid)}
            end)

          case Enum.all?(parsed, fn {source, cid} ->
                 is_binary(source) and cid in 0..65_535
               end) do
            true ->
              mappings = Map.merge(mappings, Map.new(parsed))

              case existing + map_size(mappings) <= @max_cmap_entries do
                true -> {:cont, {:ok, mappings}}
                false -> {:halt, :limit}
              end

            false ->
              {:halt, :error}
          end

        false ->
          {:halt, :error}
      end
    end)
  end

  defp parse_cid_range(stream, operator, existing, mapping_type) do
    pattern = Regex.compile!("(\\d+)\\s+begin#{operator}\\s*(.*?)\\s+end#{operator}", "s")

    Regex.scan(pattern, stream)
    |> Enum.reduce_while({:ok, %{}}, fn [_, declared_count, section], {:ok, mappings} ->
      entries = Regex.scan(~r/<([0-9A-Fa-f]+)>\s*<([0-9A-Fa-f]+)>\s*(\d+)/, section)

      valid? =
        String.to_integer(declared_count) == length(entries) and
          cmap_section_consumed?(section, ~r/<[0-9A-Fa-f]+>\s*<[0-9A-Fa-f]+>\s*\d+/)

      case valid? do
        true ->
          Enum.reduce_while(entries, {:ok, mappings}, fn [_, first, last, cid], {:ok, mappings} ->
            first = hex_bytes(first)
            last = hex_bytes(last)
            cid = String.to_integer(cid)

            case is_binary(first) and is_binary(last) and
                   byte_size(first) == byte_size(last) and first <= last do
              true ->
                count = :binary.decode_unsigned(last) - :binary.decode_unsigned(first) + 1

                cond do
                  cid > 65_535 or (mapping_type == :sequential and cid + count - 1 > 65_535) ->
                    {:halt, :error}

                  existing + map_size(mappings) + count > @max_cmap_entries ->
                    {:halt, :limit}

                  true ->
                    range =
                      0..(count - 1)
                      |> Map.new(fn offset ->
                        mapped_cid =
                          case mapping_type do
                            :sequential -> cid + offset
                            :constant -> cid
                          end

                        {increment_binary(first, offset), mapped_cid}
                      end)

                    {:cont, {:ok, Map.merge(mappings, range)}}
                end

              false ->
                {:halt, :error}
            end
          end)
          |> case do
            {:ok, mappings} -> {:cont, {:ok, mappings}}
            failure -> {:halt, failure}
          end

        false ->
          {:halt, :error}
      end
    end)
  end

  defp parse_bfchar(stream) do
    Regex.scan(~r/(\d+)\s+beginbfchar\s*(.*?)\s*endbfchar/s, stream)
    |> Enum.reduce_while({:ok, %{}}, fn [_, count, section], {:ok, mappings} ->
      entries = Regex.scan(~r/<([0-9A-Fa-f]+)>\s*<([0-9A-Fa-f]+)>/, section)

      if String.to_integer(count) == length(entries) and
           cmap_section_consumed?(section, ~r/<[0-9A-Fa-f]+>\s*<[0-9A-Fa-f]+>/) do
        Enum.reduce_while(entries, {:ok, mappings}, fn [_, source, target], {:ok, mappings} ->
          with source when is_binary(source) <- hex_bytes(source),
               {:ok, target} <- utf16(hex_bytes(target)) do
            {:cont, {:ok, Map.put(mappings, source, target)}}
          else
            _ ->
              {:halt, error(:cmap, :invalid_pdf_input, "ToUnicode bfchar mapping is malformed")}
          end
        end)
        |> case do
          {:ok, mappings} -> {:cont, {:ok, mappings}}
          {:error, _} = bfchar_error -> {:halt, bfchar_error}
        end
      else
        {:halt, error(:cmap, :invalid_pdf_input, "ToUnicode bfchar count is malformed")}
      end
    end)
  end

  defp parse_bfrange(stream, existing) do
    Regex.scan(~r/(\d+)\s+beginbfrange\s*(.*?)\s*endbfrange/s, stream)
    |> Enum.reduce_while({:ok, %{}}, fn [_, declared_count, section], {:ok, mappings} ->
      entries =
        Regex.scan(
          ~r/<([0-9A-Fa-f]+)>\s*<([0-9A-Fa-f]+)>\s*(\[[^\]]*\]|<[0-9A-Fa-f]+>)/,
          section
        )

      declared_count = String.to_integer(declared_count)

      section_valid? =
        declared_count == length(entries) and
          cmap_section_consumed?(
            section,
            ~r/<[0-9A-Fa-f]+>\s*<[0-9A-Fa-f]+>\s*(?:\[[^\]]*\]|<[0-9A-Fa-f]+>)/
          )

      if section_valid? do
        entries
        |> Enum.reduce_while({:ok, mappings}, fn [_, first, last, target], {:ok, mappings} ->
          with first when is_binary(first) <- hex_bytes(first),
               last when is_binary(last) and byte_size(last) == byte_size(first) <-
                 hex_bytes(last),
               true <- first <= last,
               count <- :binary.decode_unsigned(last) - :binary.decode_unsigned(first) + 1,
               true <- count + existing + map_size(mappings) <= @max_cmap_entries,
               {:ok, entries} <- bfrange_entries(first, count, target) do
            {:cont, {:ok, Map.merge(mappings, entries)}}
          else
            _ ->
              {:halt, error(:cmap, :invalid_pdf_input, "ToUnicode bfrange mapping is malformed")}
          end
        end)
        |> case do
          {:ok, mappings} -> {:cont, {:ok, mappings}}
          {:error, _} = bfrange_error -> {:halt, bfrange_error}
        end
      else
        {:halt, error(:cmap, :invalid_pdf_input, "ToUnicode bfrange count is malformed")}
      end
    end)
  end

  defp cmap_section_consumed?(section, entry_pattern) do
    section
    |> then(&Regex.replace(entry_pattern, &1, ""))
    |> String.replace(~r/%[^\r\n]*/, "")
    |> String.trim()
    |> then(&(&1 == ""))
  end

  defp bfrange_entries(first, count, target) do
    case target do
      "[" <> array ->
        targets = for [_, target] <- Regex.scan(~r/<([0-9A-Fa-f]+)>/, array), do: target

        if length(targets) == count do
          Enum.with_index(targets)
          |> Enum.reduce_while({:ok, %{}}, fn {target, offset}, {:ok, mappings} ->
            case utf16(hex_bytes(target)) do
              {:ok, target} ->
                {:cont, {:ok, Map.put(mappings, increment_binary(first, offset), target)}}

              error ->
                {:halt, error}
            end
          end)
        else
          error(:cmap, :invalid_pdf_input, "ToUnicode bfrange array length is invalid")
        end

      target ->
        target = target |> String.trim_leading("<") |> String.trim_trailing(">") |> hex_bytes()

        if is_binary(target) do
          0..(count - 1)
          |> Enum.reduce_while({:ok, %{}}, fn offset, {:ok, mappings} ->
            with source when is_binary(source) <- increment_binary(first, offset),
                 destination when is_binary(destination) <- increment_binary(target, offset),
                 {:ok, text} <- utf16(destination) do
              {:cont, {:ok, Map.put(mappings, source, text)}}
            else
              _ ->
                {:halt,
                 error(:cmap, :invalid_pdf_input, "ToUnicode bfrange destination overflows")}
            end
          end)
        else
          error(:cmap, :invalid_pdf_input, "ToUnicode bfrange destination is malformed")
        end
    end
  end

  defp decode_cmap(bytes, cmap, page, font) do
    decode_cmap_bytes(bytes, cmap, page, font, [], [], [])
  end

  defp decode_cmap_bytes(bytes, cmap, page, font, text_acc, codes, source_codes) do
    case bytes do
      <<>> ->
        {:ok,
         %{
           text: text_acc |> Enum.reverse() |> Enum.join(),
           codes: Enum.reverse(codes),
           source_codes: Enum.reverse(source_codes)
         }}

      bytes ->
        candidate =
          cmap.codespaces
          |> Enum.map(fn {first, _last} -> byte_size(first) end)
          |> Enum.uniq()
          |> Enum.sort(:desc)
          |> Enum.find(fn size ->
            byte_size(bytes) >= size and
              (cmap.codespaces == [] or
                 Enum.any?(cmap.codespaces, fn {first, last} ->
                   byte_size(first) == size and binary_part(bytes, 0, size) >= first and
                     binary_part(bytes, 0, size) <= last
                 end))
          end)

        if candidate do
          <<code::binary-size(^candidate), rest::binary>> = bytes

          case Map.get(cmap.mappings, code) do
            nil ->
              error(
                :text_encoding,
                :unsupported_text_encoding,
                "ToUnicode CMap has no mapping for a shown character code",
                page: page,
                font: font
              )

            mapped_text ->
              decode_cmap_bytes(
                rest,
                cmap,
                page,
                font,
                [mapped_text | text_acc],
                [:binary.decode_unsigned(code) | codes],
                [code | source_codes]
              )
          end
        else
          error(
            :text_encoding,
            :unsupported_text_encoding,
            "shown character code is outside the ToUnicode codespace",
            page: page,
            font: font
          )
        end
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
    |> IO.iodata_to_binary()
  end

  defp layout_page(spans) do
    min_x = spans |> Enum.map(& &1.x) |> Enum.min()

    spans
    |> Enum.sort_by(&{&1.y, &1.x})
    |> group_lines([])
    |> Enum.map(fn line -> line.spans |> Enum.sort_by(& &1.x) |> render_line(min_x) end)
    |> Enum.join("\n")
  end

  defp visual_spans(spans) do
    spans
    |> Enum.sort_by(&{&1.y, &1.x})
    |> group_lines([])
    |> Enum.flat_map(fn line -> Enum.sort_by(line.spans, & &1.x) end)
  end

  defp group_lines(spans, lines) do
    case spans do
      [] ->
        Enum.reverse(lines)

      [span | rest] ->
        case lines do
          [%{y: y, font_size: font_size, spans: line_spans} = line | previous]
          when abs(span.y - y) <= 1.5 ->
            updated = %{
              line
              | spans: [span | line_spans],
                font_size: max(font_size, span.font_size)
            }

            group_lines(rest, [updated | previous])

          _ ->
            group_lines(
              rest,
              [%{y: span.y, font_size: span.font_size, spans: [span]} | lines]
            )
        end
    end
  end

  defp render_line(spans, min_x) do
    spans
    |> Enum.reduce({[], min_x, true}, fn span, {parts, current_x, first?} ->
      space_width = max(span.font_size * 0.25, 4.0)
      gap = span.x - current_x

      spaces =
        cond do
          first? -> max(round((span.x - min_x) / space_width), 0)
          gap > space_width * 0.35 -> max(round(gap / space_width), 1)
          true -> 0
        end

      {[span.text, String.duplicate(" ", spaces) | parts], max(span.end_x, span.x), false}
    end)
    |> elem(0)
    |> Enum.reverse()
    |> IO.iodata_to_binary()
    |> String.trim_trailing()
  end

  defp text_state(page) do
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
      rotation: page.rotation,
      page: %{media_box: page.media_box, rotation: page.rotation}
    }
  end

  defp display_position(x, y, page) do
    [left, bottom, right, top] = page.media_box

    case page.rotation do
      0 -> {x - left, top - y}
      90 -> {y - bottom, x - left}
      180 -> {right - x, y - bottom}
      270 -> {top - y, right - x}
    end
  end

  defp identity, do: [1.0, 0.0, 0.0, 1.0, 0.0, 0.0]

  defp matrix_value(values, page) do
    case values do
      nil ->
        {:ok, identity()}

      values when is_list(values) ->
        if length(values) == 6 and Enum.all?(values, &is_number/1) do
          {:ok, Enum.map(values, &(&1 * 1.0))}
        else
          error(:content, :invalid_pdf_input, "Form XObject Matrix is malformed", page: page)
        end

      _ ->
        error(:content, :invalid_pdf_input, "Form XObject Matrix is malformed", page: page)
    end
  end

  defp translate(matrix, x, y), do: multiply([1.0, 0.0, 0.0, 1.0, x, y], matrix)

  defp multiply([a, b, c, d, e, f], [a2, b2, c2, d2, e2, f2]),
    do: [
      a * a2 + b * c2,
      a * b2 + b * d2,
      c * a2 + d * c2,
      c * b2 + d * d2,
      e * a2 + f * c2 + e2,
      e * b2 + f * d2 + f2
    ]

  defp required_value(dictionary, key, label, page) do
    case Map.get(dictionary, key) do
      nil ->
        error(:resources, :invalid_pdf_input, "#{label} resource #{key} is missing", page: page)

      value ->
        {:ok, value}
    end
  end

  defp name?(value, expected) do
    case value do
      {:name, value} -> value == expected
      _ -> false
    end
  end

  defp hex_bytes(hex) do
    case Base.decode16(hex, case: :mixed) do
      {:ok, bytes} -> bytes
      :error -> nil
    end
  end

  defp utf16(bytes) do
    case bytes do
      nil ->
        :error

      bytes ->
        case :unicode.characters_to_binary(bytes, {:utf16, :big}, :utf8) do
          value when is_binary(value) -> {:ok, value}
          _ -> :error
        end
    end
  end

  defp increment_binary(binary, offset) do
    size = bit_size(binary)
    value = :binary.decode_unsigned(binary) + offset

    if value < 1 <<< size do
      <<value::unsigned-big-size(size)>>
    else
      nil
    end
  end

  defp text_error({:error, {reason, diagnostic}}, operation),
    do:
      {:error,
       {reason, diagnostic |> Map.put(:operation, operation) |> Map.put(:module, __MODULE__)}}

  defp error(stage, reason, message, details \\ []) do
    {pdf_details, diagnostic_options} = Keyword.split(details, [:page, :font])

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

          {:font, font} ->
            "#{message}; font #{font}"
        end
      end)

    Map.put(diagnostic, :message, message)
  end
end
