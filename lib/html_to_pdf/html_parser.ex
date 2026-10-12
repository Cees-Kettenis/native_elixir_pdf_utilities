defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlParser do
  @moduledoc """
  Strict HTML parser for the native HTML-to-PDF renderer.

  Supports a strict subset of document-oriented HTML: structural
  html/head/body/style tags, paragraphs, headings, inline emphasis/color
  containers, div containers for flex/grid layouts, lists, links, tables,
  images, static form controls, and CSS-targeting attributes, including
  `data-*` and `aria-*` metadata used by attribute selectors and generated
  content. Unsupported or malformed markup returns an error instead of
  guessing at browser behavior.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlEntities
  alias NativeElixirPdfUtilities.Validators.ForeignContentValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @type processing_instruction_node :: %{
          type: :processing_instruction,
          target: String.t(),
          data: String.t(),
          node_id: reference(),
          source: %{text: String.t(), line: pos_integer(), column: pos_integer()}
        }
  @type text_node :: %{type: :text, text: String.t()}
  @type element_node :: %{
          required(:type) => :element,
          required(:tag) => String.t(),
          required(:attributes) => map(),
          required(:children) => [text_node() | element_node() | processing_instruction_node()],
          optional(:namespace) => :html | :svg | :mathml,
          optional(:attribute_namespaces) => map(),
          optional(:content) => %{
            type: :document_fragment,
            children: [text_node() | element_node() | processing_instruction_node()]
          }
        }
  @type dom_tree :: %{
          type: :document,
          children: [text_node() | element_node() | processing_instruction_node()]
        }

  @doc """
  Parses an HTML binary into a renderer DOM tree.

  Processing instructions retain case-sensitive targets and literal data. They
  occupy authored tree positions without painting or acting as element siblings.
  Their `node_id` references identify individual nodes, and `source` preserves
  the original token and its line and column before newline preprocessing.
  """
  @spec parse(term()) ::
          {:ok, dom_tree()}
          | {:error,
             :invalid_encoding | :invalid_html | :unsupported_html | :resource_limit_exceeded}
  def parse(html) do
    case parse_detailed(html) do
      {:ok, dom} -> {:ok, dom}
      {:error, {reason, _detail}} -> {:error, reason}
    end
  end

  @doc """
  Parses an HTML binary and returns source-location details when parsing fails.
  """
  @spec parse_detailed(term()) ::
          {:ok, dom_tree()}
          | {:error,
             {:invalid_encoding | :invalid_html | :unsupported_html | :resource_limit_exceeded,
              map()}}
  def parse_detailed(html), do: parse_input(html, :document)

  @doc """
  Parses a fragment in a supported HTML, SVG, or MathML context without adding a wrapper.

  For example, the `"tr"` context accepts table cells, and `"select"` accepts
  options. Context names are case-insensitive. Raw-text, RCDATA and plaintext contexts
  consume the entire input as text. `"svg"` and `"math"` select foreign roots;
  maps such as `%{namespace: :mathml, tag: "annotation-xml",
  attributes: %{"encoding" => "text/html"}}` describe foreign integration contexts.
  Foreign elements retain `:namespace` and qualified `:attribute_namespaces`.
  Unsupported contexts return an error.
  """
  @spec parse_fragment(term(), term()) ::
          {:ok, dom_tree()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def parse_fragment(html, context), do: parse_fragment_detailed(html, context)

  @doc """
  Parses a fragment with the same source diagnostics and resource limits as `parse_detailed/1`.
  """
  @spec parse_fragment_detailed(term(), term()) ::
          {:ok, dom_tree()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def parse_fragment_detailed(html, context) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, context} <- HtmlValidator.validate_html_fragment_context(context) do
        parse_input(html, context)
      end
    end)
  end

  @doc """
  Parses a complete rendering document, constructing omitted document and table groups.

  Returns an `html` element containing `head` and `body`, and constructs `tbody`
  and `colgroup` elements around authored rows and columns. `parse_detailed/1`
  retains its existing wrapper-free tree representation.
  """
  @spec parse_document_detailed(term()) ::
          {:ok, dom_tree()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def parse_document_detailed(html) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, dom} <- parse_input(html, :render_document) do
        HtmlValidator.construct_html_document(dom)
      end
    end)
  end

  defp parse_input(html, context) do
    HtmlValidator.with_render_budget(fn ->
      case HtmlValidator.validate_html_source(html) do
        {:ok, html} ->
          HtmlValidator.reserve_render_resource(
            :max_aggregate_html_source_bytes,
            byte_size(html),
            :html
          )

          # Normalize the input stream before tokenization, not decoded references.
          # Keep the caller's bytes for diagnostics and resource accounting.
          input =
            if String.contains?(html, "\r"), do: String.replace(html, ~r/\r\n?/, "\n"), else: html

          case parse_document(input, context, {html, byte_size(input)}) do
            {:ok, dom} ->
              {:ok, dom}

            {:error, {:html_source, source, remaining_bytes}} ->
              offset = byte_size(input) - byte_size(source) - remaining_bytes
              {source, line, column} = original_source_span(html, offset, byte_size(source))

              Diagnostics.error(
                :html,
                :unsupported_html,
                html_error_message(line, source),
                source: source,
                line: line,
                column: column
              )

            {:error, {:html_diagnostic, reason, diagnostic, remaining_bytes, token}} ->
              offset = byte_size(input) - byte_size(token) - remaining_bytes

              {source, line, column} =
                original_source_span(html, offset, byte_size(token))

              source = if diagnostic.source == token, do: source, else: diagnostic.source

              Diagnostics.error(diagnostic.stage, reason, diagnostic.message,
                source: source,
                module: Map.get(diagnostic, :module),
                line: line,
                column: column
              )
          end

        {:error, {reason, diagnostic}} ->
          {:error, {reason, diagnostic}}
      end
    end)
  end

  defp parse_document(html, context, source_context) do
    case HtmlValidator.html_text_mode(context) != :data do
      true ->
        HtmlValidator.reserve_render_resource(:max_html_nodes, 1, :html)

        text =
          if HtmlValidator.html_text_mode(context) != :rcdata,
            do: html,
            else: HtmlEntities.decode(html, :text)

        children = if text == "", do: [], else: [%{type: :text, text: text}]
        {:ok, %{type: :document, children: children}}

      false ->
        parse_markup(html, context, source_context)
    end
  end

  defp parse_markup(html, context, source_context) do
    {context, ancestors} =
      if context == :render_document, do: {:document, [:render_document]}, else: {context, []}

    html =
      case html do
        <<239, 187, 191, remaining::binary>> -> remaining
        html -> html
      end

    with {:ok, children, {:source, ""}} <-
           parse_children({:source, html}, context, nil, [], ancestors, source_context) do
      {:ok, %{type: :document, children: children}}
    end
  end

  defp next_token(stream, context) do
    case stream do
      {:pending, token, remaining} ->
        {:ok, token, remaining}

      {:source, ""} ->
        :eof

      {:source, html} ->
        read_token(html, context)
    end
  end

  defp read_token(html, context) do
    HtmlValidator.reserve_render_resource(:max_html_nodes, 1, :html)

    cond do
      HtmlValidator.html_text_mode(context) != :data ->
        scan_text_content(html, context)

      String.starts_with?(html, "<?") ->
        {token, remaining} =
          case :binary.match(html, ">") do
            {index, 1} ->
              length = index + 1
              <<token::binary-size(^length), remaining::binary>> = html
              {token, remaining}

            :nomatch ->
              {html, ""}
          end

        {:ok, token, {:source, remaining}}

      String.starts_with?(html, "<!--") ->
        {token, remaining} =
          case :binary.match(html, "-->", scope: {4, byte_size(html) - 4}) do
            {index, 3} ->
              length = index + 3
              <<token::binary-size(^length), remaining::binary>> = html
              {token, remaining}

            :nomatch ->
              {html, ""}
          end

        case HtmlValidator.validate_html_comment(token) do
          :ok ->
            {:ok, token, {:source, remaining}}

          {:error, {reason, diagnostic}} ->
            {:error, {:html_diagnostic, reason, diagnostic, byte_size(remaining), token}}
        end

      is_tuple(context) and String.starts_with?(html, "<![CDATA[") ->
        case :binary.match(html, "]]>", scope: {9, byte_size(html) - 9}) do
          {index, 3} ->
            length = index + 3
            <<token::binary-size(^length), remaining::binary>> = html
            {:ok, token, {:source, remaining}}

          :nomatch ->
            {:error, {:html_source, html, 0}}
        end

      String.starts_with?(html, "<") ->
        with {:ok, token, remaining} <- take_tag_token(html) do
          {:ok, token, {:source, remaining}}
        end

      true ->
        case :binary.match(html, "<") do
          {index, 1} ->
            <<text::binary-size(^index), remaining::binary>> = html
            {:ok, text, {:source, remaining}}

          :nomatch ->
            {:ok, html, {:source, ""}}
        end
    end
  end

  defp take_tag_token(html) do
    case tag_token_length(html, nil, 0, :name) do
      {:ok, length} ->
        <<token::binary-size(^length), remaining::binary>> = html
        {:ok, token, remaining}

      :error ->
        {:error, {:html_source, html, 0}}
    end
  end

  defp tag_token_length(html, quote, length, state) do
    case html do
      <<>> ->
        :error

      <<character, remaining::binary>> ->
        cond do
          state == :attributes and is_nil(quote) and character in [?", ?'] ->
            tag_token_length(remaining, character, length + 1, state)

          character == quote ->
            tag_token_length(remaining, nil, length + 1, state)

          is_nil(quote) and character == ?> ->
            {:ok, length + 1}

          true ->
            tag_token_length(
              remaining,
              quote,
              length + 1,
              if(state == :name and character in [32, 9, 10, 12, 13],
                do: :attributes,
                else: state
              )
            )
        end
    end
  end

  defp scan_text_content(html, tag) do
    case HtmlValidator.html_text_end_match(html, tag) do
      {0, _length} ->
        with {:ok, closing, remaining} <- take_tag_token(html) do
          {:ok, closing, {:source, remaining}}
        end

      {index, _length} ->
        <<text::binary-size(^index), remaining::binary>> = html
        {:ok, text, {:source, remaining}}

      nil ->
        {:ok, html, {:source, ""}}
    end
  end

  defp parse_children(tokens, context, closing_tag, children, ancestors, source_context) do
    case next_token(tokens, context) do
      :eof when is_nil(closing_tag) ->
        {:ok, Enum.reverse(children), {:source, ""}}

      :eof ->
        case HtmlValidator.html_eof_end_tag_allowed?(
               context,
               closing_tag,
               Enum.reverse(children),
               ancestors
             ) do
          true -> {:ok, Enum.reverse(children), {:source, ""}}
          false -> {:error, :unsupported_html}
        end

      {:ok, token, remaining} ->
        parse_child(token, remaining, context, closing_tag, children, ancestors, source_context)

      {:error, _reason} = error ->
        error
    end
  end

  defp stream_source({:source, source}), do: source

  defp parse_child(token, remaining, context, closing_tag, children, ancestors, source_context) do
    cond do
      HtmlValidator.html_text_mode(context) != :data ->
        parse_text_content_child(
          token,
          remaining,
          context,
          closing_tag,
          children,
          ancestors,
          source_context
        )

      String.starts_with?(token, "<?") ->
        case HtmlValidator.validate_html_processing_instruction(token) do
          {:ok, node} ->
            {original, input_size} = source_context
            offset = input_size - byte_size(token) - byte_size(stream_source(remaining))
            # Source identity scans are bounded cumulatively, including inert fragments.
            HtmlValidator.reserve_render_resource(
              :max_html_tree_work,
              2 * byte_size(original),
              :html
            )

            {source, line, column} = original_source_span(original, offset, byte_size(token))

            node =
              node
              |> Map.put(:node_id, make_ref())
              |> Map.put(:source, %{text: source, line: line, column: column})

            parse_children(
              remaining,
              context,
              closing_tag,
              [node | children],
              ancestors,
              source_context
            )

          {:error, {reason, diagnostic}} ->
            {:error,
             {:html_diagnostic, reason, diagnostic, byte_size(stream_source(remaining)), token}}
        end

      String.starts_with?(token, "<!--") ->
        parse_children(remaining, context, closing_tag, children, ancestors, source_context)

      is_tuple(context) and String.starts_with?(token, "<![CDATA[") ->
        text = binary_part(token, 9, byte_size(token) - 12)

        parse_children(
          remaining,
          context,
          closing_tag,
          [%{type: :text, text: text} | children],
          ancestors,
          source_context
        )

      Regex.match?(~r/^<!doctype(?=[ \t\n\f\r>])/i, token) ->
        case HtmlValidator.validate_html_doctype(token, context, children) do
          :ok ->
            parse_children(
              remaining,
              :after_doctype,
              closing_tag,
              children,
              ancestors,
              source_context
            )

          {:error, {reason, diagnostic}} ->
            {:error,
             {:html_diagnostic, reason, diagnostic, byte_size(stream_source(remaining)), token}}
        end

      String.starts_with?(token, "</") ->
        case parse_closing_tag(token, context, ancestors) do
          {:ok, tag} ->
            cond do
              tag == closing_tag ->
                {:ok, Enum.reverse(children), remaining}

              HtmlValidator.omitted_html_end_tag?(closing_tag, {:end, tag}, ancestors) ->
                {:ok, Enum.reverse(children), {:pending, token, remaining}}

              true ->
                case HtmlValidator.html_implied_start_closure(context, tag, children) do
                  nil ->
                    {:error, {:html_source, token, byte_size(stream_source(remaining))}}

                  {:group, kind, group_children, previous} ->
                    group = HtmlValidator.construct_html_group(kind, group_children)

                    parse_children(
                      remaining,
                      context,
                      closing_tag,
                      [group | previous],
                      ancestors,
                      source_context
                    )

                  next_context ->
                    parse_children(
                      remaining,
                      next_context,
                      closing_tag,
                      children,
                      ancestors,
                      source_context
                    )
                end
            end

          _ ->
            {:error, {:html_source, token, byte_size(stream_source(remaining))}}
        end

      String.starts_with?(token, "<") ->
        with {:ok, prepared} <- parse_opening_tag(token, context),
             tag <- prepared.tag,
             attributes <- prepared.attributes,
             :ok <-
               NativeElixirPdfUtilities.Validators.HtmlMetadataValidator.validate_attributes(
                 prepared,
                 token
               ),
             :ok <- HtmlValidator.validate_html_element_attributes(tag, attributes),
             false <- HtmlValidator.omitted_html_end_tag?(closing_tag, {:start, tag}, ancestors),
             true <- ForeignContentValidator.valid_child?(context, prepared, ancestors),
             {:ok, element_children, rest} <-
               element_children(
                 ForeignContentValidator.child_context(prepared),
                 if(prepared.namespace == :svg, do: String.downcase(tag), else: tag),
                 remaining,
                 prepared.self_closing?,
                 HtmlValidator.html_parent_stack(
                   context,
                   tag,
                   ancestors
                 ),
                 source_context
               ),
             true <-
               ForeignContentValidator.valid_element?(
                 prepared,
                 element_children,
                 context,
                 ancestors
               ) do
          element_children =
            HtmlValidator.normalize_html_element_children(
              tag,
              element_children,
              stream_source(remaining)
            )

          element_children =
            if tag == "table" and :render_document in ancestors,
              do: HtmlValidator.construct_html_table_groups(element_children),
              else: element_children

          element = %{
            type: :element,
            tag: tag,
            attributes: attributes,
            children: element_children
          }

          element =
            if prepared.namespace != :html or is_tuple(context) do
              element
              |> Map.put(:namespace, prepared.namespace)
              |> Map.put(:attribute_namespaces, prepared.attribute_namespaces)
            else
              element
            end

          element =
            if tag == "template" and prepared.namespace == :html,
              do: HtmlValidator.construct_html_template(element),
              else: element

          parse_children(
            rest,
            context,
            closing_tag,
            [element | children],
            ancestors,
            source_context
          )
        else
          true ->
            {:ok, Enum.reverse(children), {:pending, token, remaining}}

          {:error, {:html_source, source}} ->
            {:error, {:html_source, source, byte_size(stream_source(remaining))}}

          {:error, {reason, diagnostic}} when is_map(diagnostic) ->
            diagnostic = Diagnostics.with_context(diagnostic, source: token)

            {:error,
             {:html_diagnostic, reason, diagnostic, byte_size(stream_source(remaining)), token}}

          {:error, detail} = error when is_tuple(detail) ->
            error

          {:error, :unsupported_html} ->
            source = token <> stream_source(remaining)
            {:error, {:html_source, source, 0}}

          false ->
            {:error, {:html_source, token, byte_size(stream_source(remaining))}}
        end

      true ->
        case HtmlValidator.html_text_action(context, token) do
          :ignore ->
            parse_children(remaining, context, closing_tag, children, ancestors, source_context)

          :invalid ->
            {:error,
             {:html_source, String.trim_leading(token), byte_size(stream_source(remaining))}}

          :text ->
            parse_children(
              remaining,
              context,
              closing_tag,
              [%{type: :text, text: HtmlEntities.decode(token, :text)} | children],
              ancestors,
              source_context
            )
        end
    end
  end

  defp parse_text_content_child(
         token,
         remaining,
         context,
         closing_tag,
         children,
         ancestors,
         source_context
       ) do
    case HtmlValidator.html_text_token_action(context, token) do
      :end ->
        {:ok, Enum.reverse(children), remaining}

      :invalid ->
        {:error, {:html_source, token, byte_size(stream_source(remaining))}}

      :text ->
        text =
          case HtmlValidator.html_text_mode(context) != :rcdata do
            true -> token
            false -> HtmlEntities.decode(token, :text)
          end

        parse_children(
          remaining,
          context,
          closing_tag,
          [%{type: :text, text: text} | children],
          ancestors,
          source_context
        )
    end
  end

  defp parse_opening_tag(token, context \\ :document) do
    captures =
      Regex.named_captures(
        ~r/^<(?<tag>[a-zA-Z][^ \t\n\f\r\x00\/>]*)(?<attributes>[\s\S]*)>$/u,
        token
      )

    case captures do
      %{"tag" => tag, "attributes" => attributes} ->
        tag = String.downcase(tag, :ascii)

        case ForeignContentValidator.prepare_start_tag(tag, attributes, context) do
          {:ok, prepared} -> {:ok, prepared}
          {:error, :unsupported_html} -> {:error, {:html_source, token}}
          error -> error
        end

      _ ->
        {:error, :unsupported_html}
    end
  end

  defp parse_closing_tag(token, context, ancestors) do
    case Regex.named_captures(~r/^<\/(?<tag>[a-zA-Z][^ \t\n\f\r\x00\/>]*)[ \t\n\f\r]*>$/u, token) do
      %{"tag" => tag} ->
        tag = String.downcase(tag, :ascii)

        case ForeignContentValidator.supported_end_tag?(tag, context, ancestors) do
          true -> {:ok, tag}
          false -> {:error, :unsupported_html}
        end

      _ ->
        {:error, :unsupported_html}
    end
  end

  defp element_children(context, tag, remaining, self_closing?, ancestors, source_context) do
    case self_closing? do
      true ->
        {:ok, [], remaining}

      false ->
        HtmlValidator.within_html_element(fn ->
          parse_children(remaining, context, tag, [], ancestors, source_context)
        end)
    end
  end

  defp unsupported_tag_source(source) do
    case Regex.run(~r/^<\/?[ \t\n\f\r]*([a-zA-Z][^ \t\n\f\r\x00\/>]*)/u, source) do
      [_prefix, tag] ->
        case HtmlValidator.supported_html_tag?(String.downcase(tag, :ascii)) do
          true -> nil
          false -> source
        end

      nil ->
        nil
    end
  end

  defp invalid_attribute_source(source) do
    case Regex.run(~r/^<[a-zA-Z][a-zA-Z0-9]*(?:[^<>"']|"[^"]*"|'[^']*')*>/u, source) do
      [token] ->
        case parse_opening_tag(token) do
          {:ok, _prepared} -> nil
          _ -> token
        end

      nil ->
        nil
    end
  end

  defp html_error_message(line, source) do
    cond do
      unsupported_tag_source(source) ->
        ~s(line #{line}: HTML tag "#{source}" is unsupported)

      invalid_attribute_source(source) ->
        ~s(line #{line}: HTML tag "#{source}" has invalid, duplicate, or unsupported attributes)

      true ->
        ~s(line #{line}: HTML source "#{source}" is unsupported or malformed)
    end
  end

  # Translate normalized byte boundaries for diagnostics and PI source identity.
  # CRLF is the only
  # preprocessing sequence that changes byte length; lone CR changes lines only.
  defp original_source_span(html, offset, length) do
    crlf_positions = :binary.matches(html, "\r\n")

    [first, last] =
      Enum.map([offset, offset + length], fn boundary ->
        Enum.reduce_while(crlf_positions, boundary, fn {index, 2}, original ->
          if index < original, do: {:cont, original + 1}, else: {:halt, original}
        end)
      end)

    prefix = binary_part(html, 0, first)
    lines = String.split(prefix, ~r/\r\n|\r|\n/, trim: false)
    line = length(lines)
    column = String.length(List.last(lines) || "") + 1
    {binary_part(html, first, last - first), line, column}
  end
end
