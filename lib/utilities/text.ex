defmodule NativeElixirPdfUtilities.Text do
  @moduledoc """
  Best-effort embedded text extraction for classic PDFs.

  This module is native Elixir and does not perform OCR. It reads PDF content streams,
  decodes supported Flate-compressed streams, applies ToUnicode CMaps where present,
  and reconstructs approximate page layout from text matrix coordinates.
  """

  alias NativeElixirPdfUtilities.Tokenizer
  alias NativeElixirPdfUtilities.Diagnostics

  @type extract_option :: {:layout, boolean()}
  @type error_reason ::
          :empty_pdf_text | :invalid_options | :invalid_pdf_input | :invalid_path | File.posix()
  @type object_record :: %{obj: integer(), gen: integer(), tokens: [Tokenizer.token()]}
  @type text_chunk :: %{x: float(), y: float(), text: String.t()}

  @doc """
  Extracts embedded text from a PDF binary.

  Options:

    * `:layout` - when true, approximate visible text layout using spaces and line breaks.
      This is currently the only output mode and defaults to true.
  """
  @spec extract(binary(), [extract_option()]) ::
          {:ok, String.t()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def extract(pdf_binary, opts \\ []) do
    case pdf_binary do
      pdf_binary when is_binary(pdf_binary) ->
        case Keyword.keyword?(opts) do
          true ->
            layout? = Keyword.get(opts, :layout, true)
            objects = parse_objects(pdf_binary)

            cmap_by_ref = cmap_by_ref(objects)
            font_cmap_by_name = font_cmap_by_name(objects, cmap_by_ref)
            fallback_cmap = merge_cmaps(Map.values(cmap_by_ref))

            pages =
              objects
              |> decoded_content_streams()
              |> Enum.map(&extract_page_chunks(&1, font_cmap_by_name, fallback_cmap))
              |> Enum.reject(&(&1 == []))

            case pages do
              [] ->
                Diagnostics.error(
                  :text_extraction,
                  :empty_pdf_text,
                  "PDF contains no extractable text",
                  operation: :extract,
                  module: __MODULE__
                )

              pages ->
                text =
                  if layout? do
                    pages |> Enum.map(&layout_page/1) |> Enum.join("\f")
                  else
                    pages |> Enum.map(&plain_page/1) |> Enum.join("\n")
                  end

                {:ok, text}
            end

          false ->
            Diagnostics.error(
              :options,
              :invalid_options,
              "extract options must be a keyword list",
              operation: :extract,
              module: __MODULE__
            )
        end

      _ ->
        Diagnostics.error(:text_extraction, :invalid_pdf_input, "PDF input must be a binary",
          operation: :extract,
          module: __MODULE__
        )
    end
  end

  @doc """
  Reads a PDF file and extracts embedded text from it.
  """
  @spec extract_file(String.t(), [extract_option()]) ::
          {:ok, String.t()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def extract_file(path, opts \\ []) do
    case path do
      path when is_binary(path) ->
        case File.read(path) do
          {:ok, pdf_binary} ->
            case extract(pdf_binary, opts) do
              {:ok, text} ->
                {:ok, text}

              {:error, {reason, detail}} ->
                {:error,
                 {reason,
                  Diagnostics.with_context(detail,
                    operation: :extract_file,
                    module: __MODULE__,
                    source: path
                  )}}
            end

          {:error, reason} ->
            Diagnostics.error(:file, reason, "file read failed: #{reason}",
              operation: :read,
              module: __MODULE__,
              source: path
            )
        end

      _ ->
        Diagnostics.error(:file, :invalid_path, "path must be a string",
          operation: :extract_file,
          module: __MODULE__
        )
    end
  end

  defp parse_objects(pdf_binary) do
    pdf_binary
    |> Tokenizer.new()
    |> Tokenizer.tokenize_all()
    |> do_parse_objects([])
  end

  defp do_parse_objects(tokens, acc) do
    case tokens do
      [] ->
        Enum.reverse(acc)

      [{:int, obj}, {:int, gen}, :obj | rest] ->
        {body, rest} = take_until_endobj(rest, [])
        do_parse_objects(rest, [%{obj: obj, gen: gen, tokens: body} | acc])

      [_token | rest] ->
        do_parse_objects(rest, acc)
    end
  end

  defp take_until_endobj(tokens, acc) do
    case tokens do
      [] -> {Enum.reverse(acc), []}
      [:endobj | rest] -> {Enum.reverse(acc), rest}
      [token | rest] -> take_until_endobj(rest, [token | acc])
    end
  end

  defp cmap_by_ref(objects) do
    objects
    |> Enum.flat_map(fn object ->
      object.tokens
      |> stream_data()
      |> case do
        nil ->
          []

        stream ->
          decoded = decode_stream(stream, object.tokens)

          if String.contains?(decoded, "begincmap") do
            [{{object.obj, object.gen}, parse_cmap(decoded)}]
          else
            []
          end
      end
    end)
    |> Map.new()
  end

  defp font_cmap_by_name(objects, cmap_by_ref) do
    font_ref_to_cmap_ref =
      objects
      |> Enum.flat_map(fn object ->
        case find_ref_after_name(object.tokens, "ToUnicode") do
          nil -> []
          cmap_ref -> [{{object.obj, object.gen}, cmap_ref}]
        end
      end)
      |> Map.new()

    objects
    |> Enum.flat_map(fn object ->
      object.tokens
      |> font_resource_refs()
      |> Enum.flat_map(fn {font_name, font_ref} ->
        with cmap_ref when not is_nil(cmap_ref) <- Map.get(font_ref_to_cmap_ref, font_ref),
             cmap when is_map(cmap) <- Map.get(cmap_by_ref, cmap_ref) do
          [{font_name, cmap}]
        else
          _ -> []
        end
      end)
    end)
    |> Map.new()
  end

  defp decoded_content_streams(objects) do
    objects
    |> Enum.flat_map(fn object ->
      case stream_data(object.tokens) do
        nil ->
          []

        stream ->
          decoded = decode_stream(stream, object.tokens)

          cond do
            String.contains?(decoded, "begincmap") ->
              []

            String.contains?(decoded, "BT") and
                (String.contains?(decoded, "Tj") or String.contains?(decoded, "TJ")) ->
              [decoded]

            true ->
              []
          end
      end
    end)
  end

  defp stream_data(tokens) do
    Enum.find_value(tokens, fn
      {:stream_data, data} -> data
      _ -> nil
    end)
  end

  defp decode_stream(data, tokens) do
    if has_flate_filter?(tokens) do
      try do
        :zlib.uncompress(data)
      rescue
        _ -> data
      end
    else
      data
    end
  end

  defp has_flate_filter?(tokens) do
    tokens
    |> Enum.take_while(&(&1 != :stream))
    |> Enum.any?(&(&1 == {:name, "FlateDecode"}))
  end

  defp parse_cmap(cmap_text) do
    bfchar =
      cmap_text
      |> cmap_sections("beginbfchar", "endbfchar")
      |> Enum.flat_map(&Regex.scan(~r/<([0-9A-Fa-f]+)>\s*<([0-9A-Fa-f]+)>/, &1))
      |> Map.new(fn [_, source, target] ->
        {hex_to_integer(source), unicode_hex_to_string(target)}
      end)

    bfrange =
      cmap_text
      |> cmap_sections("beginbfrange", "endbfrange")
      |> Enum.flat_map(
        &Regex.scan(~r/<([0-9A-Fa-f]+)>\s*<([0-9A-Fa-f]+)>\s*<([0-9A-Fa-f]+)>/, &1)
      )
      |> Enum.reduce(%{}, fn [_, first, last, target], acc ->
        first = hex_to_integer(first)
        last = hex_to_integer(last)
        target = hex_to_integer(target)

        first..last
        |> Enum.with_index()
        |> Enum.reduce(acc, fn {source, index}, range_acc ->
          Map.put(range_acc, source, codepoint_to_string(target + index))
        end)
      end)

    Map.merge(bfrange, bfchar)
  end

  defp cmap_sections(cmap_text, opening, closing) do
    regex = Regex.compile!("(?s)#{opening}\\s+(.*?)\\s+#{closing}")

    regex
    |> Regex.scan(cmap_text)
    |> Enum.map(fn [_, section] -> section end)
  end

  defp find_ref_after_name(tokens, name) do
    tokens
    |> Enum.chunk_every(4, 1, :discard)
    |> Enum.find_value(fn
      [{:name, ^name}, {:int, obj}, {:int, gen}, :R] -> {obj, gen}
      _ -> nil
    end)
  end

  defp font_resource_refs(tokens) do
    tokens
    |> Enum.chunk_every(4, 1, :discard)
    |> Enum.flat_map(fn
      [{:name, font_name}, {:int, obj}, {:int, gen}, :R] ->
        if String.match?(font_name, ~r/^F\d+$/), do: [{font_name, {obj, gen}}], else: []

      _ ->
        []
    end)
  end

  defp merge_cmaps(cmaps) do
    Enum.reduce(cmaps, %{}, fn cmap, acc -> Map.merge(acc, cmap) end)
  end

  defp extract_page_chunks(content_stream, font_cmap_by_name, fallback_cmap) do
    content_stream
    |> Tokenizer.new()
    |> Tokenizer.tokenize_all()
    |> Enum.reduce({[], [], %{x: 0.0, y: 0.0, font: nil}}, fn token, {chunks, operands, state} ->
      case token do
        {:op, op} ->
          handle_operator(op, operands, state, chunks, font_cmap_by_name, fallback_cmap)

        _ ->
          {chunks, [token | operands], state}
      end
    end)
    |> elem(0)
    |> Enum.reverse()
    |> Enum.reject(&(String.trim(&1.text) == ""))
  end

  defp handle_operator(op, operands, state, chunks, font_cmap_by_name, fallback_cmap) do
    case op do
      "Tf" ->
        font =
          operands
          |> Enum.find_value(fn
            {:name, font_name} -> font_name
            _ -> nil
          end)

        {chunks, [], %{state | font: font || state.font}}

      "Tm" ->
        numbers = operands |> Enum.reverse() |> Enum.flat_map(&number_value/1)

        case numbers do
          [_a, _b, _c, _d, x, y | _] -> {chunks, [], %{state | x: x, y: y}}
          _ -> {chunks, [], state}
        end

      op when op in ["Td", "TD"] ->
        numbers = operands |> Enum.reverse() |> Enum.flat_map(&number_value/1)

        case numbers do
          [tx, ty | _] -> {chunks, [], %{state | x: state.x + tx, y: state.y + ty}}
          _ -> {chunks, [], state}
        end

      "T*" ->
        {chunks, [], %{state | y: state.y + 12.0}}

      "Tj" ->
        text =
          operands
          |> List.first()
          |> decode_text_token(cmap_for_font(state.font, font_cmap_by_name, fallback_cmap))

        add_chunk(chunks, state, text)

      "TJ" ->
        cmap = cmap_for_font(state.font, font_cmap_by_name, fallback_cmap)

        text =
          operands
          |> Enum.reverse()
          |> Enum.filter(fn
            {:string, _} -> true
            {:hex_string, _} -> true
            _ -> false
          end)
          |> Enum.map(&decode_text_token(&1, cmap))
          |> Enum.join("")

        add_chunk(chunks, state, text)

      op when op in ["'", "\""] ->
        handle_operator(
          "Tj",
          operands,
          %{state | y: state.y + 12.0},
          chunks,
          font_cmap_by_name,
          fallback_cmap
        )

      _ ->
        {chunks, [], state}
    end
  end

  defp add_chunk(chunks, state, text) do
    chunk = %{x: state.x, y: state.y, text: text}
    {[chunk | chunks], [], state}
  end

  defp cmap_for_font(font, font_cmap_by_name, fallback_cmap) do
    Map.get(font_cmap_by_name, font, fallback_cmap)
  end

  defp decode_text_token(token, cmap) do
    case token do
      {:string, text} -> decode_encoded_text(text, cmap)
      {:hex_string, text} -> decode_encoded_text(text, cmap)
      _ -> ""
    end
  end

  defp decode_encoded_text(text, cmap) do
    cond do
      byte_size(text) >= 2 and rem(byte_size(text), 2) == 0 ->
        text
        |> for_each_16bit_code()
        |> Enum.map(fn code -> Map.get(cmap, code, fallback_code_to_string(code)) end)
        |> Enum.join("")

      true ->
        text
    end
  end

  defp for_each_16bit_code(text) do
    for <<code::16 <- text>>, do: code
  end

  defp fallback_code_to_string(code) do
    if code >= 32 and code <= 126 do
      <<code>>
    else
      ""
    end
  end

  defp layout_page(chunks) do
    page_min_x =
      chunks
      |> Enum.map(& &1.x)
      |> Enum.min()

    chunks
    |> Enum.group_by(fn chunk -> round(chunk.y / 6.0) * 6 end)
    |> Enum.sort_by(fn {y, _chunks} -> y end)
    |> Enum.map(fn {_y, line_chunks} ->
      line_chunks
      |> Enum.sort_by(& &1.x)
      |> render_line(page_min_x)
    end)
    |> Enum.join("\n")
  end

  defp plain_page(chunks) do
    chunks
    |> Enum.map(& &1.text)
    |> Enum.join(" ")
  end

  defp render_line(chunks, page_min_x) do
    chunks
    |> Enum.reduce("", fn chunk, acc ->
      column = chunk.x |> Kernel.-(page_min_x) |> Kernel./(4.6) |> round() |> max(0)
      padding = max(column - String.length(acc), 1)
      acc <> String.duplicate(" ", padding) <> chunk.text
    end)
    |> String.trim_trailing()
  end

  defp number_value(token) do
    case token do
      {:int, value} -> [value * 1.0]
      {:real, value} -> [value]
      _ -> []
    end
  end

  defp hex_to_integer(hex) do
    {value, ""} = Integer.parse(hex, 16)
    value
  end

  defp unicode_hex_to_string(hex) do
    hex
    |> hex_to_integer()
    |> codepoint_to_string()
  end

  defp codepoint_to_string(codepoint) do
    try do
      <<codepoint::utf8>>
    rescue
      _ -> ""
    end
  end
end
