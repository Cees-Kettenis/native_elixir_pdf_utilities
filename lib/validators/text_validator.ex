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
  alias NativeElixirPdfUtilities.Validators.TextResourceValidator

  @validated_operators ~w(q Q cm gs BT ET Tf Tm Td TD T* TL Tc Tw Tz Tr Ts Tj TJ ' " Do)
  @text_showing_operators ~w(Tj TJ ' ")
  @max_decoded_content_bytes 50_000_000
  @max_parsed_instructions 100_000
  @max_stream_uses 100_000
  @max_instruction_uses 1_000_000
  @max_form_expansions 10_000

  @typedoc "A validated content instruction with operands in source order."
  @type instruction :: %{required(:operator) => binary(), required(:operands) => [term()]}

  @typedoc "One page prepared for strict text extraction."
  @type page_context :: %{
          required(:number) => pos_integer(),
          required(:ref) => PdfValidator.ref(),
          required(:media_box) => [number()],
          required(:rotation) => integer(),
          required(:contents) => [[instruction()]]
        }

  @typedoc false
  @type preparation_context :: %{
          required(:stream_refs) => %{optional(PdfValidator.value()) => PdfValidator.ref()},
          required(:decoded_streams) => %{optional(PdfValidator.ref()) => binary()},
          required(:instructions) => %{
            optional(PdfValidator.ref()) => [instruction()]
          },
          required(:decoded_bytes) => non_neg_integer(),
          required(:parsed_instructions) => non_neg_integer(),
          required(:stream_uses) => non_neg_integer(),
          required(:instruction_uses) => non_neg_integer(),
          required(:form_expansions) => non_neg_integer()
        }

  @typedoc false
  @type preparation_stats :: %{
          required(:unique_streams) => non_neg_integer(),
          required(:decoded_bytes) => non_neg_integer(),
          required(:parsed_instructions) => non_neg_integer(),
          required(:stream_uses) => non_neg_integer(),
          required(:instruction_uses) => non_neg_integer(),
          required(:form_expansions) => non_neg_integer()
        }

  @typedoc "A text-specific context prepared from the shared PDF context."
  @type context :: %{
          required(:document) => PdfValidator.document(),
          required(:pages) => [page_context()],
          required(:preparation_stats) => preparation_stats()
        }

  @typedoc "A validated public text-extraction request."
  @type request :: %{
          required(:pdf) => binary(),
          required(:options) => %{optional(:layout) => boolean(), optional(:order) => atom()}
        }

  @doc """
  Validates and normalizes a public text-extraction request.
  """
  @spec validate_request(term(), term(), :extract | :extract_spans) ::
          {:ok, request()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_request(pdf, opts, operation) do
    case {pdf, opts, operation} do
      {pdf, opts, :extract} when is_binary(pdf) and is_list(opts) ->
        case Keyword.keyword?(opts) do
          true ->
            unknown = opts |> Keyword.keys() |> Enum.reject(&(&1 == :layout)) |> Enum.uniq()
            layout = Keyword.get(opts, :layout, true)

            cond do
              unknown != [] ->
                error(
                  :options,
                  :invalid_options,
                  "extract options contain unsupported keys: #{inspect(Enum.sort(unknown))}",
                  operation: operation
                )

              not is_boolean(layout) ->
                error(:options, :invalid_options, "layout option must be a boolean",
                  operation: operation
                )

              true ->
                {:ok, %{pdf: pdf, options: %{layout: layout}}}
            end

          false ->
            error(:options, :invalid_options, "extract options must be a keyword list",
              operation: operation
            )
        end

      {pdf, opts, :extract_spans} when is_binary(pdf) and is_list(opts) ->
        case Keyword.keyword?(opts) do
          true ->
            unknown = opts |> Keyword.keys() |> Enum.reject(&(&1 == :order)) |> Enum.uniq()
            order = Keyword.get(opts, :order, :source)

            cond do
              unknown != [] ->
                error(
                  :options,
                  :invalid_options,
                  "extract span options contain an unknown option",
                  operation: operation
                )

              order not in [:source, :visual] ->
                error(:options, :invalid_options, "span order must be :source or :visual",
                  operation: operation
                )

              true ->
                {:ok, %{pdf: pdf, options: %{order: order}}}
            end

          false ->
            error(:options, :invalid_options, "extract span options must be a keyword list",
              operation: operation
            )
        end

      {_pdf, _opts, operation} when operation in [:extract, :extract_spans] ->
        error(:input, :invalid_pdf_input, "PDF input must be a binary", operation: operation)

      _ ->
        error(:input, :invalid_pdf_input, "text extraction request is malformed")
    end
  end

  @doc """
  Validates a public file-extraction path before file access.
  """
  @spec validate_path(term(), atom()) ::
          {:ok, String.t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_path(path, operation) do
    case {path, operation} do
      {path, operation} when is_binary(path) and is_atom(operation) ->
        {:ok, path}

      {_path, operation} when is_atom(operation) ->
        error(:file, :invalid_path, "path must be a string", operation: operation)

      _ ->
        error(:file, :invalid_path, "file extraction request is malformed")
    end
  end

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
        |> Enum.reduce_while(
          {:ok, [], new_preparation_context()},
          fn {page, page_number}, {:ok, prepared_pages, preparation_context} ->
            case prepare_page(document, page, page_number, preparation_context) do
              {:ok, prepared, preparation_context} ->
                {:cont, {:ok, [prepared | prepared_pages], preparation_context}}

              {:error, _} = page_error ->
                {:halt, page_error}
            end
          end
        )
        |> case do
          {:ok, prepared_pages, preparation_context} ->
            {:ok,
             %{
               document: document,
               pages: Enum.reverse(prepared_pages),
               preparation_stats: preparation_stats(preparation_context)
             }}

          {:error, _} = preparation_error ->
            preparation_error
        end

      _ ->
        error(:text_validation, :invalid_pdf_input, "shared PDF validation context is malformed")
    end
  end

  @doc false
  @spec new_preparation_context() :: preparation_context()
  def new_preparation_context do
    %{
      stream_refs: %{},
      decoded_streams: %{},
      instructions: %{},
      decoded_bytes: 0,
      parsed_instructions: 0,
      stream_uses: 0,
      instruction_uses: 0,
      form_expansions: 0
    }
  end

  @doc false
  @spec prepare_content_stream(
          PdfValidator.document(),
          PdfValidator.value(),
          pos_integer(),
          preparation_context()
        ) ::
          {:ok, [instruction()], preparation_context()}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_content_stream(document, value, page_number, preparation_context) do
    with {:ok, stream_ref, content, preparation_context} <-
           cached_stream(document, value, page_number, preparation_context),
         {:ok, operations, preparation_context} <-
           cached_instructions(
             stream_ref,
             content,
             preparation_context,
             page_number
           ) do
      {:ok, operations, preparation_context}
    end
  end

  @doc false
  @spec decoded_stream(
          PdfValidator.document(),
          PdfValidator.value(),
          pos_integer(),
          preparation_context()
        ) ::
          {:ok, binary(), preparation_context()}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def decoded_stream(document, value, page_number, preparation_context) do
    with {:ok, _stream_ref, content, preparation_context} <-
           cached_stream(document, value, page_number, preparation_context) do
      {:ok, content, preparation_context}
    end
  end

  @doc false
  @spec charge_form_expansion(preparation_context(), pos_integer()) ::
          {:ok, preparation_context()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def charge_form_expansion(preparation_context, page_number) do
    form_expansions = preparation_context.form_expansions + 1

    case form_expansions <= @max_form_expansions do
      true ->
        {:ok, %{preparation_context | form_expansions: form_expansions}}

      false ->
        resource_limit_error("Form XObject expansion count exceeds the limit", page_number)
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

        {operator, [{:name, name}]} when operator in ["gs", "Do"] ->
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

  defp prepare_page(document, page, page_number, preparation_context) do
    with {:ok, media_box} <- page_rectangle(document, page.media_box, page_number),
         {:ok, rotation} <- page_rotation(document, page.rotate, page_number),
         {:ok, content_refs} <- content_references(page.dictionary, page.ref),
         {:ok, contents, preparation_context} <-
           prepare_contents(document, content_refs, page_number, preparation_context),
         {:ok, contents, preparation_context} <-
           TextResourceValidator.prepare_contents(
             document,
             page.resources,
             contents,
             page_number,
             preparation_context
           ) do
      {:ok,
       %{
         number: page_number,
         ref: page.ref,
         media_box: media_box,
         rotation: rotation,
         contents: contents
       }, preparation_context}
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

  defp prepare_contents(document, content_refs, page_number, preparation_context) do
    Enum.reduce_while(
      content_refs,
      {:ok, [], preparation_context},
      fn content_ref, {:ok, contents, preparation_context} ->
        case prepare_content_stream(document, content_ref, page_number, preparation_context) do
          {:ok, operations, preparation_context} ->
            {:cont, {:ok, [operations | contents], preparation_context}}

          {:error, {reason, diagnostic}} ->
            diagnostic = append_details(diagnostic, page: page_number)
            {:halt, {:error, {reason, diagnostic}}}
        end
      end
    )
    |> case do
      {:ok, contents, preparation_context} ->
        contents = Enum.reverse(contents)

        case validate_scopes(contents, page_number) do
          :ok -> {:ok, contents, preparation_context}
          {:error, _} = scope_error -> scope_error
        end

      {:error, _} = content_error ->
        content_error
    end
  end

  defp cached_decoded_stream(stream_context, preparation_context, page_number) do
    case Map.fetch(preparation_context.decoded_streams, stream_context.ref) do
      {:ok, content} ->
        {:ok, content, preparation_context}

      :error ->
        with {:ok, content} <- Reader.decode_prepared_stream(stream_context),
             {:ok, preparation_context} <-
               cache_decoded_stream(
                 preparation_context,
                 stream_context.ref,
                 content,
                 page_number
               ) do
          {:ok, content, preparation_context}
        end
    end
  end

  defp cached_stream(document, value, page_number, preparation_context) do
    with {:ok, preparation_context} <- charge_stream_use(preparation_context, page_number) do
      case Map.fetch(preparation_context.stream_refs, value) do
        {:ok, stream_ref} ->
          {:ok, stream_ref, Map.fetch!(preparation_context.decoded_streams, stream_ref),
           preparation_context}

        :error ->
          with {:ok, stream_context} <-
                 PdfValidator.prepare_decoded_stream(document, value,
                   operation: :read,
                   module: Reader
                 ),
               {:ok, content, preparation_context} <-
                 cached_decoded_stream(stream_context, preparation_context, page_number) do
            preparation_context = %{
              preparation_context
              | stream_refs: Map.put(preparation_context.stream_refs, value, stream_context.ref)
            }

            {:ok, stream_context.ref, content, preparation_context}
          end
      end
    end
  end

  defp cache_decoded_stream(preparation_context, stream_ref, content, page_number) do
    decoded_bytes = preparation_context.decoded_bytes + byte_size(content)

    case decoded_bytes <= @max_decoded_content_bytes do
      true ->
        {:ok,
         %{
           preparation_context
           | decoded_streams: Map.put(preparation_context.decoded_streams, stream_ref, content),
             decoded_bytes: decoded_bytes
         }}

      false ->
        resource_limit_error("aggregate decoded content bytes exceed the limit", page_number)
    end
  end

  defp cached_instructions(stream_ref, content, preparation_context, page_number) do
    case Map.fetch(preparation_context.instructions, stream_ref) do
      {:ok, operations} ->
        with {:ok, preparation_context} <-
               charge_instruction_uses(preparation_context, length(operations), page_number) do
          {:ok, operations, preparation_context}
        end

      :error ->
        with {:ok, operations} <- instructions(content, page_number),
             {:ok, preparation_context} <-
               cache_instructions(preparation_context, stream_ref, operations, page_number),
             {:ok, preparation_context} <-
               charge_instruction_uses(preparation_context, length(operations), page_number) do
          {:ok, operations, preparation_context}
        end
    end
  end

  defp cache_instructions(preparation_context, stream_ref, operations, page_number) do
    parsed_instructions = preparation_context.parsed_instructions + length(operations)

    case parsed_instructions <= @max_parsed_instructions do
      true ->
        {:ok,
         %{
           preparation_context
           | instructions: Map.put(preparation_context.instructions, stream_ref, operations),
             parsed_instructions: parsed_instructions
         }}

      false ->
        resource_limit_error("parsed content instruction count exceeds the limit", page_number)
    end
  end

  defp charge_stream_use(preparation_context, page_number) do
    stream_uses = preparation_context.stream_uses + 1

    case stream_uses <= @max_stream_uses do
      true ->
        {:ok, %{preparation_context | stream_uses: stream_uses}}

      false ->
        resource_limit_error("content stream reference count exceeds the limit", page_number)
    end
  end

  defp charge_instruction_uses(preparation_context, count, page_number) do
    instruction_uses = preparation_context.instruction_uses + count

    case instruction_uses <= @max_instruction_uses do
      true -> {:ok, %{preparation_context | instruction_uses: instruction_uses}}
      false -> resource_limit_error("content instruction work exceeds the limit", page_number)
    end
  end

  defp preparation_stats(preparation_context) do
    %{
      unique_streams: map_size(preparation_context.decoded_streams),
      decoded_bytes: preparation_context.decoded_bytes,
      parsed_instructions: preparation_context.parsed_instructions,
      stream_uses: preparation_context.stream_uses,
      instruction_uses: preparation_context.instruction_uses,
      form_expansions: preparation_context.form_expansions
    }
  end

  defp resource_limit_error(message, page_number) do
    error(:limits, :resource_limit_exceeded, message, page: page_number)
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

        {%{operator: operator}, {graphics_depth, in_text?}}
        when operator in @text_showing_operators and in_text? ->
          {:cont, {:ok, {graphics_depth, in_text?}}}

        {%{operator: operator}, {_graphics_depth, false}}
        when operator in @text_showing_operators ->
          {:halt, content_error("#{operator} appears outside a text object", page_number)}

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
          [operation: Keyword.get(details, :operation, :extract), module: __MODULE__],
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
