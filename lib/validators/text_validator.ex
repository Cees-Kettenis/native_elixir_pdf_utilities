defmodule NativeElixirPdfUtilities.Validators.TextValidator do
  @moduledoc """
  Text-extraction validation and preparation for parsed PDF documents.

  The validator consumes a shared PDF validation context, validates effective
  page geometry and content-stream references, decodes the streams once, and
  converts content syntax into operation instructions. The extractor consumes
  those prepared values instead of reopening page dictionaries or tokenizing
  content independently.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Tokenizer
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  @validated_operators ~w(q Q cm BT ET Tf Tm Td TD T* TL Tc Tw Tz Tr Ts Tj TJ ' " Do)

  @typedoc "A validated content instruction with operands in source order."
  @type instruction :: %{required(:operator) => binary(), required(:operands) => [term()]}

  @typedoc "One page prepared for strict text extraction."
  @type page_context :: %{
          required(:number) => pos_integer(),
          required(:ref) => PdfValidator.ref(),
          required(:resources) => PdfValidator.value(),
          required(:media_box) => [number()],
          required(:rotation) => integer(),
          required(:contents) => [[instruction()]]
        }

  @typedoc "A text-specific context prepared from the shared PDF context."
  @type context :: %{
          required(:document) => PdfValidator.document(),
          required(:pages) => [page_context()]
        }

  @doc """
  Prepares all resolved pages and their content streams for text extraction.
  """
  @spec prepare(PdfValidator.context()) ::
          {:ok, context()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare(pdf_context) do
    case pdf_context do
      %{document: document, pages: pages} ->
        pages
        |> Enum.with_index(1)
        |> Enum.reduce_while({:ok, []}, fn {page, page_number}, {:ok, prepared_pages} ->
          case prepare_page(document, page, page_number) do
            {:ok, prepared} -> {:cont, {:ok, [prepared | prepared_pages]}}
            {:error, _} = page_error -> {:halt, page_error}
          end
        end)
        |> case do
          {:ok, prepared_pages} ->
            {:ok, %{document: document, pages: Enum.reverse(prepared_pages)}}

          {:error, _} = preparation_error ->
            preparation_error
        end

      _ ->
        error(:text_validation, :invalid_pdf_input, "shared PDF validation context is malformed")
    end
  end

  @doc """
  Validates and tokenizes one decoded PDF content stream into instructions.
  """
  @spec instructions(binary(), pos_integer()) ::
          {:ok, [instruction()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def instructions(content, page_number) do
    case is_binary(content) and is_integer(page_number) and page_number > 0 do
      true ->
        tokens = Tokenizer.new(content) |> Tokenizer.tokenize_all()

        case Enum.any?(tokens, &match?({:error, _}, &1)) do
          true ->
            error(:content, :invalid_pdf_input, "content stream contains invalid syntax",
              page: page_number
            )

          false ->
            tokens
            |> Enum.reduce_while({:ok, [], []}, fn token, {:ok, operations, operands} ->
              case token do
                :lbracket ->
                  {:cont, {:ok, operations, [:array_start | operands]}}

                :rbracket ->
                  case close_array(operands) do
                    {:ok, operands} -> {:cont, {:ok, operations, operands}}
                    :error -> {:halt, content_error("content array is unbalanced", page_number)}
                  end

                {:op, operator} ->
                  case prepare_instruction(operator, Enum.reverse(operands), page_number) do
                    {:ok, operation} -> {:cont, {:ok, [operation | operations], []}}
                    {:error, _} = instruction_error -> {:halt, instruction_error}
                  end

                token ->
                  {:cont, {:ok, operations, [token | operands]}}
              end
            end)
            |> case do
              {:ok, operations, []} ->
                {:ok, Enum.reverse(operations)}

              {:ok, _operations, _operands} ->
                content_error("content stream has dangling operands", page_number)

              {:error, _} = content_error ->
                content_error
            end
        end

      false ->
        error(:content, :invalid_pdf_input, "content stream input is malformed")
    end
  end

  @doc """
  Validates balanced graphics-state and text-object scopes across instruction streams.

  The instruction lists are treated as one logical sequence so page scopes may
  cross `/Contents` stream boundaries. Form XObjects should pass their single
  instruction list separately because their scopes are independent.
  """
  @spec validate_scopes([[instruction()]], pos_integer()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_scopes(contents, page_number) do
    case is_list(contents) and Enum.all?(contents, &is_list/1) and is_integer(page_number) and
           page_number > 0 do
      true ->
        contents
        |> Enum.reduce_while({:ok, {0, false}}, fn instructions, {:ok, scope} ->
          case validate_scope_instructions(instructions, scope, page_number) do
            {:ok, scope} -> {:cont, {:ok, scope}}
            {:error, _} = scope_error -> {:halt, scope_error}
          end
        end)
        |> case do
          {:ok, {0, false}} ->
            :ok

          {:ok, {_graphics_depth, true}} ->
            content_error("BT has no matching ET", page_number)

          {:ok, {_graphics_depth, false}} ->
            content_error("q has no matching Q", page_number)

          {:error, _} = scope_error ->
            scope_error
        end

      false ->
        error(:content, :invalid_pdf_input, "content scope input is malformed")
    end
  end

  @doc """
  Converts one PDF numeric token into its semantic number.
  """
  @spec number(term()) :: {:ok, number()} | :error
  def number(value) do
    case value do
      {:int, value} -> {:ok, value}
      {:real, value} -> {:ok, value}
      _ -> :error
    end
  end

  @doc """
  Converts an exact-length list of PDF numeric tokens into floating-point values.
  """
  @spec numbers([term()], non_neg_integer()) :: {:ok, [float()]} | :error
  def numbers(values, count) do
    converted = Enum.map(values, &number/1)

    case length(converted) == count and Enum.all?(converted, &match?({:ok, _}, &1)) do
      true -> {:ok, Enum.map(converted, fn {:ok, value} -> value * 1.0 end)}
      false -> :error
    end
  end

  defp prepare_instruction(operator, operands, page_number) do
    normalized =
      case {operator, operands} do
        {operator, []} when operator in ["q", "Q", "BT", "ET", "T*"] ->
          {:ok, []}

        {operator, operands} when operator in ["cm", "Tm"] ->
          numbers(operands, 6)

        {"Tf", [{:name, font_name}, size]} ->
          case number(size) do
            {:ok, size} -> {:ok, [{:name, font_name}, size * 1.0]}
            :error -> :error
          end

        {operator, operands} when operator in ["Td", "TD"] ->
          numbers(operands, 2)

        {operator, operands} when operator in ["TL", "Tc", "Tw", "Tz", "Ts"] ->
          numbers(operands, 1)

        {"Tr", operands} ->
          case numbers(operands, 1) do
            {:ok, [mode]} when mode >= 0 and mode <= 7 and trunc(mode) == mode ->
              {:ok, [trunc(mode)]}

            _ ->
              :error
          end

        {"Tj", [string]} ->
          if string_token?(string), do: {:ok, [string]}, else: :error

        {"TJ", [{:array, values}]} ->
          if Enum.all?(values, &(string_token?(&1) or match?({:ok, _}, number(&1)))) do
            {:ok, [{:array, values}]}
          else
            :error
          end

        {"'", [string]} ->
          if string_token?(string), do: {:ok, [string]}, else: :error

        {"\"", [word_spacing, char_spacing, string]} ->
          with {:ok, word_spacing} <- number(word_spacing),
               {:ok, char_spacing} <- number(char_spacing),
               true <- string_token?(string) do
            {:ok, [word_spacing * 1.0, char_spacing * 1.0, string]}
          else
            _ -> :error
          end

        {"Do", [{:name, name}]} ->
          {:ok, [{:name, name}]}

        {operator, _operands} ->
          if operator in @validated_operators, do: :error, else: {:ok, operands}
      end

    case normalized do
      {:ok, operands} -> {:ok, %{operator: operator, operands: operands}}
      :error -> content_error("#{operator} has invalid operands", page_number)
    end
  end

  defp string_token?(value) do
    case value do
      {kind, bytes} when kind in [:string, :hex_string] -> is_binary(bytes)
      _ -> false
    end
  end

  defp prepare_page(document, page, page_number) do
    with {:ok, media_box} <- page_rectangle(document, page.media_box, page_number),
         {:ok, rotation} <- page_rotation(document, page.rotate, page_number),
         {:ok, content_refs} <- content_references(page.dictionary, page.ref),
         {:ok, contents} <- prepare_contents(document, content_refs, page_number) do
      {:ok,
       %{
         number: page_number,
         ref: page.ref,
         resources: page.resources,
         media_box: media_box,
         rotation: rotation,
         contents: contents
       }}
    end
  end

  defp page_rectangle(document, value, page_number) do
    case PdfValidator.number_array(document, value, 4) do
      {:ok, [left, bottom, right, top] = media_box}
      when right > left and top > bottom ->
        {:ok, media_box}

      _ ->
        error(:page_tree, :invalid_pdf_input, "page MediaBox or Rotate value is malformed",
          page: page_number
        )
    end
  end

  defp page_rotation(document, value, page_number) do
    value = value || 0

    case PdfValidator.resolve(document, value) do
      {:ok, rotation} when is_integer(rotation) and rem(rotation, 90) == 0 ->
        {:ok, Integer.mod(rotation, 360)}

      _ ->
        error(:page_tree, :invalid_pdf_input, "page MediaBox or Rotate value is malformed",
          page: page_number
        )
    end
  end

  defp content_references(dictionary, page_ref) do
    case Map.get(dictionary, "Contents") do
      nil ->
        {:ok, []}

      {:ref, _} = content_ref ->
        {:ok, [content_ref]}

      content_refs when is_list(content_refs) ->
        case Enum.all?(content_refs, &match?({:ref, _}, &1)) do
          true ->
            {:ok, content_refs}

          false ->
            error(:content, :invalid_pdf_input, "Contents array contains a non-stream reference")
        end

      _ ->
        error(:content, :invalid_pdf_input, "page Contents is malformed", object: page_ref)
    end
  end

  defp prepare_contents(document, content_refs, page_number) do
    Enum.reduce_while(content_refs, {:ok, []}, fn content_ref, {:ok, contents} ->
      with {:ok, content} <- Reader.decoded_stream(document, content_ref),
           {:ok, operations} <- instructions(content, page_number) do
        {:cont, {:ok, [operations | contents]}}
      else
        {:error, {reason, diagnostic}} ->
          diagnostic = append_details(diagnostic, page: page_number)
          {:halt, {:error, {reason, diagnostic}}}
      end
    end)
    |> case do
      {:ok, contents} ->
        contents = Enum.reverse(contents)

        case validate_scopes(contents, page_number) do
          :ok -> {:ok, contents}
          {:error, _} = scope_error -> scope_error
        end

      {:error, _} = content_error ->
        content_error
    end
  end

  defp validate_scope_instructions(instructions, scope, page_number) do
    Enum.reduce_while(instructions, {:ok, scope}, fn instruction, {:ok, scope} ->
      case {instruction, scope} do
        {%{operator: "q"}, {graphics_depth, in_text?}} ->
          {:cont, {:ok, {graphics_depth + 1, in_text?}}}

        {%{operator: "Q"}, {graphics_depth, in_text?}} when graphics_depth > 0 ->
          {:cont, {:ok, {graphics_depth - 1, in_text?}}}

        {%{operator: "Q"}, {0, _in_text?}} ->
          {:halt, content_error("Q has no matching q", page_number)}

        {%{operator: "BT"}, {graphics_depth, false}} ->
          {:cont, {:ok, {graphics_depth, true}}}

        {%{operator: "BT"}, {_graphics_depth, true}} ->
          {:halt, content_error("BT appears inside a text object", page_number)}

        {%{operator: "ET"}, {graphics_depth, true}} ->
          {:cont, {:ok, {graphics_depth, false}}}

        {%{operator: "ET"}, {_graphics_depth, false}} ->
          {:halt, content_error("ET has no matching BT", page_number)}

        {%{operator: operator}, _scope} when is_binary(operator) ->
          {:cont, {:ok, scope}}

        _ ->
          {:halt, content_error("content scope instruction is malformed", page_number)}
      end
    end)
  end

  defp close_array(operands) do
    {values, rest} = Enum.split_while(operands, &(&1 != :array_start))

    case rest do
      [] -> :error
      [_array_start | rest] -> {:ok, [{:array, Enum.reverse(values)} | rest]}
    end
  end

  defp content_error(message, page_number) do
    error(:content, :invalid_pdf_input, message, page: page_number)
  end

  defp error(stage, reason, message, details \\ []) do
    {pdf_details, diagnostic_options} = Keyword.split(details, [:object, :page])

    {:error, {reason, diagnostic}} =
      Diagnostics.error(
        stage,
        reason,
        message,
        Keyword.merge(
          [operation: :extract, module: __MODULE__],
          diagnostic_options
        )
      )

    {:error, {reason, append_details(diagnostic, pdf_details)}}
  end

  defp append_details(diagnostic, details) do
    message =
      Enum.reduce(details, diagnostic.message, fn detail, message ->
        case detail do
          {:object, {object, generation}} -> "#{message}; object #{object} #{generation}"
          {:page, page} -> "#{message}; page #{page}"
        end
      end)

    Map.put(diagnostic, :message, message)
  end
end
