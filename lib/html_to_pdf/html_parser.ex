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
  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlTree
  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlEntities
  alias NativeElixirPdfUtilities.Validators.ForeignContentValidator
  alias NativeElixirPdfUtilities.Validators.TemplatePatchValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.TemplateCandidateValidator

  # This fixed byte alphabet follows UTF-8 encoding, not a configurable ceiling.
  @non_ascii_bytes for byte <- 128..255, do: <<byte>>

  @type processing_instruction_node :: %{
          type: :processing_instruction,
          target: String.t(),
          data: String.t(),
          node_id: reference(),
          document_id: reference(),
          tree_scope_id: reference(),
          parent_id: reference() | nil,
          source: %{text: String.t(), line: pos_integer(), column: pos_integer()}
        }
  @type text_node :: %{
          type: :text,
          text: String.t(),
          node_id: reference(),
          document_id: reference(),
          tree_scope_id: reference(),
          parent_id: reference() | nil,
          source: %{text: String.t(), line: pos_integer(), column: pos_integer()}
        }
  @type element_node :: %{
          required(:type) => :element,
          required(:tag) => String.t(),
          required(:attributes) => map(),
          required(:children) => [text_node() | element_node() | processing_instruction_node()],
          required(:node_id) => reference(),
          required(:document_id) => reference(),
          required(:tree_scope_id) => reference(),
          required(:parent_id) => reference() | nil,
          required(:source) => %{text: String.t(), line: pos_integer(), column: pos_integer()},
          optional(:namespace) => :html | :svg | :mathml,
          optional(:attribute_namespaces) => map(),
          optional(:content) => %{
            type: :document_fragment,
            node_id: reference(),
            document_id: reference(),
            tree_scope_id: reference(),
            parent_id: nil,
            source: %{text: String.t(), line: pos_integer(), column: pos_integer()},
            children: [text_node() | element_node() | processing_instruction_node()]
          }
        }
  @type dom_tree :: %{
          type: :document,
          node_id: reference(),
          document_id: reference(),
          tree_scope_id: reference(),
          parent_id: nil,
          source: %{text: String.t(), line: pos_integer(), column: pos_integer()},
          children: [text_node() | element_node() | processing_instruction_node()]
        }
  @type fragment_tree :: %{
          type: :document_fragment,
          node_id: reference(),
          document_id: reference(),
          tree_scope_id: reference(),
          parent_id: nil,
          source: %{text: String.t(), line: pos_integer(), column: pos_integer()},
          children: [text_node() | element_node() | processing_instruction_node()]
        }

  @doc """
  Parses an HTML binary into a renderer DOM tree.

  Processing instructions retain case-sensitive targets and literal data. They
  occupy authored tree positions without painting or acting as element siblings.
  Every node has a unique logical `node_id`, an authored `document_id`, a
  `tree_scope_id` and `parent_id`. Document and fragment roots have no parent;
  template fragments establish separate tree scopes in the same authored document.
  These identities carry no resource permissions. `source` contains exactly
  `text`, `line` and `column`, preserving original bytes before newline
  preprocessing, with positive line numbers and Unicode grapheme columns.
  Document source text is the complete input. Elements retain their opening
  token, processing instructions their complete token, and text nodes their
  tokenizer span before reference decoding or initial newline suppression.
  Template fragments retain the owning template's opening token. Implied
  elements have empty source text at line 1, column 1. Text tokens retain their
  boundaries across comments; CDATA spans include the original delimiters.
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
  Parses a canonical fragment with live insertion parents in the supplied context.

  The root is a `document_fragment` with its own tree scope. Omitted `tbody`
  and `colgroup` parents exist before their authored children are parsed, both
  in a table context and in nested tables. HTML, SVG and MathML contexts use
  the same strict validation and tokenizer rules as `parse_fragment_detailed/2`.
  Original source bytes and authored logical identities are retained. This
  operation grants no resource permissions and does not enable template patches.
  The existing fragment API retains its wrapper-free document projection.
  """
  @spec parse_fragment_tree_detailed(term(), term()) ::
          {:ok, fragment_tree()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def parse_fragment_tree_detailed(html, context) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, context} <- HtmlValidator.validate_html_fragment_context(context) do
        parse_input(html, {:fragment_tree, context})
      end
    end)
  end

  @doc """
  Parses a complete rendering document, constructing omitted document and table groups.

  Returns an `html` element containing `head` and `body`, and constructs `tbody`
  and `colgroup` elements before their authored children are parsed, so live
  insertion locations include these implied parents. `parse_detailed/1`
  retains its existing wrapper-free tree representation.
  """
  @spec parse_document_detailed(term()) ::
          {:ok, dom_tree()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def parse_document_detailed(html) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, dom} <- parse_input(html, :render_document),
           :ok <- HtmlValidator.validate_constructed_html_document(dom) do
        {:ok, dom}
      end
    end)
  end

  defp parse_input(html, context) do
    HtmlValidator.with_render_budget(fn ->
      TemplatePatchValidator.with_parser_invocation(html, fn html ->
        HtmlValidator.reserve_render_resource(
          :max_aggregate_html_source_bytes,
          byte_size(html),
          :html
        )

        {input, source_context} = source_stream(html)

        case parse_document(input, context, source_context) do
          {:ok, dom} -> {:ok, dom}
          error -> parse_error(error, source_context)
        end
      end)
    end)
  end

  defp source_stream(html) do
    # Normalize the input stream before tokenization, not decoded references.
    # Keep the caller's bytes for diagnostics and resource accounting.
    HtmlValidator.reserve_render_resource(:max_html_tree_work, 3 * byte_size(html), :html)
    input = HtmlValidator.preprocess_html(html)
    crlf = :binary.matches(html, "\r\n")

    {column_positions, column_extras} =
      unicode_columns(html, 0, 0, [], [], :binary.compile_pattern(@non_ascii_bytes))

    source_context = %{
      original: html,
      column_positions: column_positions,
      column_extras: column_extras,
      size: byte_size(input),
      crlf:
        crlf
        |> Enum.with_index()
        |> Enum.map(fn {{position, 2}, index} -> position - index end)
        |> List.to_tuple(),
      lines:
        [
          0
          | Enum.map(:binary.matches(html, ["\r\n", "\r", "\n"]), fn {position, length} ->
              position + length
            end)
        ]
        |> List.to_tuple()
    }

    {input, source_context}
  end

  defp parse_error(error, source_context) do
    case error do
      {:error, {:html_source, source, remaining_bytes}} ->
        offset = source_context.size - byte_size(source) - remaining_bytes

        {source, line, column} =
          original_source_span(source_context, offset, byte_size(source))

        Diagnostics.error(
          :html,
          :unsupported_html,
          html_error_message(line, source),
          source: source,
          line: line,
          column: column
        )

      {:error, {:html_diagnostic, reason, diagnostic, remaining_bytes, token}} ->
        offset = source_context.size - byte_size(token) - remaining_bytes

        {source, line, column} =
          original_source_span(source_context, offset, byte_size(token))

        source = if diagnostic.source == token, do: source, else: diagnostic.source

        Diagnostics.error(diagnostic.stage, reason, diagnostic.message,
          source: source,
          module: Map.get(diagnostic, :module),
          operation: Map.get(diagnostic, :operation),
          line: line,
          column: column
        )

      {:error, {_reason, _diagnostic}} = error ->
        error

      _ ->
        source =
          Map.get(source_context, :candidate_source, %{
            text: source_context.original,
            line: 1,
            column: 1
          })

        Diagnostics.error(
          :html,
          :unsupported_html,
          "candidate source cannot be parsed completely in this tokenizer context; original input remains retained",
          module: __MODULE__,
          operation: :parse,
          source: source.text,
          line: source.line,
          column: source.column
        )
    end
  end

  defp parse_document(html, context, source_context) do
    {context, root_type, ancestors} =
      case context do
        {:fragment_tree, context} -> {context, :document_fragment, [:fragment_tree]}
        :render_document -> {:document, :document, [:render_document]}
        context -> {context, :document, []}
      end

    tree = HtmlTree.new(%{text: source_context.original, line: 1, column: 1}, root_type)

    case HtmlValidator.html_text_mode(context) != :data do
      true ->
        HtmlValidator.reserve_render_resource(:max_html_nodes, 1, :html)

        text =
          if HtmlValidator.html_text_mode(context) != :rcdata,
            do: html,
            else: HtmlEntities.decode(html, :text)

        tree =
          if text == "" do
            tree
          else
            {tree, _id} =
              HtmlTree.insert(tree, tree.root, %{
                type: :text,
                text: text,
                source: token_source(source_context, html, {:source, ""})
              })

            tree
          end

        {:ok, HtmlTree.fetch(tree, tree.root)}

      false ->
        html =
          case html do
            <<239, 187, 191, remaining::binary>> -> remaining
            html -> html
          end

        with {:ok, tree, {:source, ""}} <-
               parse_children(
                 {:source, html},
                 context,
                 nil,
                 ancestors,
                 source_context,
                 tree,
                 tree.root
               ) do
          {:ok, HtmlTree.fetch(tree, tree.root)}
        end
    end
  end

  @doc false
  @spec capture_template_candidate(term(), HtmlTree.t()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def capture_template_candidate(handle, tree) do
    TemplatePatchValidator.with_candidate_input(handle, tree, fn capture ->
      {input, source_context} = source_stream(capture.source)
      plan = capture.plan
      source_context = Map.put(source_context, :candidate_source, plan.source)

      normalized_start =
        capture.source
        |> binary_part(0, plan.source_offset + byte_size(plan.source.text))
        |> HtmlValidator.preprocess_html()
        |> byte_size()

      remaining =
        {:source, binary_part(input, normalized_start, byte_size(input) - normalized_start)}

      element = %{
        type: :element,
        tag: "template",
        attributes: capture.opening.attributes,
        children: [],
        source: plan.source
      }

      HtmlValidator.reserve_render_resource(:max_html_nodes, 1, :html)
      {original, host} = HtmlTree.insert(tree, plan.original.parent, element)
      {original, fragment} = HtmlTree.open_fragment(original, host, plan.source)

      result =
        element_children(
          "template",
          "template",
          remaining,
          false,
          HtmlValidator.html_parent_stack(
            plan.tokenizer_context,
            "template",
            plan.tokenizer_ancestors
          ),
          source_context,
          original,
          fragment
        )

      case result do
        {:ok, original, {:source, rest}} ->
          {original, _} = HtmlTree.close(original, fragment)
          {original, _} = HtmlTree.close(original, host)
          normalized_finish = source_context.size - byte_size(rest)

          finish =
            normalized_finish +
              boundary_index(
                source_context.crlf,
                normalized_finish,
                0,
                tuple_size(source_context.crlf)
              )

          with {:ok, provenance} <-
                 TemplateCandidateValidator.prepare_original(original, finish, plan),
               {:ok, transaction} <- HtmlTree.begin_content_patch(tree, plan.target) do
            bounded = binary_part(input, normalized_start, normalized_finish - normalized_start)
            prospective_context = %{source_context | size: normalized_finish}
            prospective_context = Map.put(prospective_context, :insertion, plan.target)
            ancestors = capture.prospective_ancestors

            prospective =
              parse_children(
                {:source, bounded},
                plan.insertion.context,
                "template",
                ancestors,
                prospective_context,
                transaction.tree,
                plan.target.parent
              )

            case prospective do
              {:ok, projected, {:source, ""}} ->
                previous = MapSet.new(transaction.tree.nodes[plan.target.parent].children)

                roots =
                  projected.nodes[plan.target.parent].children
                  |> Enum.reverse()
                  |> Enum.reject(&MapSet.member?(previous, &1))

                with :ok <-
                       NativeElixirPdfUtilities.Validators.HtmlTreeValidator.validate_patch_adoption(
                         projected,
                         plan.target,
                         roots
                       ),
                     {:ok, projected} <-
                       HtmlTree.finish_content_patch(%{
                         transaction
                         | tree: projected,
                           inserted: roots
                       }) do
                  {:ok,
                   %{
                     original: original,
                     projected: projected,
                     host: host,
                     fragment: fragment,
                     inserted_roots: roots,
                     finish: finish,
                     provenance: provenance
                   }}
                end

              error ->
                parse_error(error, prospective_context)
            end
          end

        error ->
          parse_error(error, source_context)
      end
    end)
  end

  @doc false
  @spec read_source_token(String.t(), term(), [term()]) ::
          {:ok, String.t(), String.t(), term()} | {:error, term()}
  def read_source_token(html, context, ancestors) do
    with {:ok, token, {:source, remaining}} <- read_token(html, context) do
      classification =
        cond do
          HtmlValidator.html_text_mode(context) != :data and
              HtmlValidator.html_text_token_action(context, token) == :text ->
            {:ok, :text}

          String.starts_with?(token, "</") ->
            with {:ok, tag} <- parse_closing_tag(token, context, ancestors),
                 do: {:ok, {:end, tag}}

          is_tuple(context) and String.starts_with?(token, "<![CDATA[") ->
            {:ok, :text}

          String.starts_with?(token, "<!") or String.starts_with?(token, "<?") ->
            {:ok, :other}

          String.starts_with?(token, "<") ->
            with {:ok, prepared} <- parse_opening_tag(token, context),
                 do: {:ok, {:start, prepared}}

          true ->
            {:ok, :text}
        end

      with {:ok, kind} <- classification, do: {:ok, token, remaining, kind}
    end
  end

  defp next_token(stream, context) do
    case stream do
      {:pending, token, remaining} ->
        {:ok, token, remaining}

      {:source, ""} ->
        :eof

      {:source, html} ->
        HtmlValidator.reserve_render_resource(:max_html_nodes, 1, :html)
        read_token(html, context)
    end
  end

  defp read_token(html, context) do
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

  defp parse_children(tokens, context, closing_tag, ancestors, source_context, tree, parent) do
    next = next_token(tokens, context)

    action =
      if (context in [:document, :after_doctype, "html", "head", "table"] and
            :render_document in ancestors) or
           (context == "table" and :fragment_tree in ancestors) do
        token =
          case next do
            {:ok, token, _remaining} -> token
            _ -> :eof
          end

        previous =
          if context in [:document, :after_doctype, "html"],
            do: HtmlTree.last_element_tag(tree, parent),
            else: nil

        HtmlValidator.html_insertion_action(
          context,
          token,
          previous,
          tree.nodes[parent].source.text == ""
        )
      else
        :process
      end

    case {action, next} do
      {{:error, {reason, diagnostic}}, {:ok, token, remaining}} ->
        {:error,
         {:html_diagnostic, reason, diagnostic, byte_size(stream_source(remaining)), token}}

      {{:imply, tag}, next} when next == :eof or elem(next, 0) == :ok ->
        pending =
          case next do
            :eof -> {:source, ""}
            {:ok, token, remaining} -> {:pending, token, remaining}
          end

        group =
          HtmlValidator.construct_html_group(tag, [])
          |> Map.put(:source, %{text: "", line: 1, column: 1})

        {tree, id} = attach_node(tree, parent, group, source_context)

        # The implied parent exists before reprocessing the authored token.
        # Recursion tracks its insertion mode, while HtmlTree owns its address.
        with {:ok, tree, rest} <-
               HtmlValidator.within_html_element(fn ->
                 parse_children(
                   pending,
                   tag,
                   tag,
                   [context | ancestors],
                   source_context,
                   tree,
                   id
                 )
               end) do
          {tree, _node} = HtmlTree.close(tree, id)
          parse_children(rest, context, closing_tag, ancestors, source_context, tree, parent)
        end

      {:close, {:ok, token, remaining}} ->
        {:ok, tree, {:pending, token, remaining}}

      {_, :eof} when is_nil(closing_tag) ->
        {:ok, tree, {:source, ""}}

      {_, :eof} ->
        case HtmlValidator.html_eof_end_tag_allowed?(
               context,
               closing_tag,
               HtmlTree.children(tree, parent),
               ancestors
             ) do
          true -> {:ok, tree, {:source, ""}}
          false -> {:error, :unsupported_html}
        end

      {_, {:ok, token, remaining}} ->
        parse_child(
          token,
          remaining,
          context,
          closing_tag,
          ancestors,
          source_context,
          tree,
          parent
        )

      {_, {:error, _reason} = error} ->
        error
    end
  end

  defp stream_source({:source, source}), do: source

  defp parse_child(
         token,
         remaining,
         context,
         closing_tag,
         ancestors,
         source_context,
         tree,
         parent
       ) do
    cond do
      HtmlValidator.html_text_mode(context) != :data ->
        case HtmlValidator.html_text_token_action(context, token) do
          :end ->
            {:ok, tree, remaining}

          :invalid ->
            {:error, {:html_source, token, byte_size(stream_source(remaining))}}

          :text ->
            text =
              if HtmlValidator.html_text_mode(context) != :rcdata,
                do: token,
                else: HtmlEntities.decode(token, :text)

            {tree, id} =
              attach_node(
                tree,
                parent,
                %{
                  type: :text,
                  text: text,
                  source: token_source(source_context, token, remaining)
                },
                source_context
              )

            {tree, _node} = HtmlTree.close(tree, id)

            parse_children(
              remaining,
              context,
              closing_tag,
              ancestors,
              source_context,
              tree,
              parent
            )
        end

      String.starts_with?(token, "<?") ->
        case HtmlValidator.validate_html_processing_instruction(token) do
          {:ok, node} ->
            {tree, id} =
              attach_node(
                tree,
                parent,
                Map.put(node, :source, token_source(source_context, token, remaining)),
                source_context
              )

            {tree, _node} = HtmlTree.close(tree, id)

            parse_children(
              remaining,
              context,
              closing_tag,
              ancestors,
              source_context,
              tree,
              parent
            )

          {:error, {reason, diagnostic}} ->
            {:error,
             {:html_diagnostic, reason, diagnostic, byte_size(stream_source(remaining)), token}}
        end

      String.starts_with?(token, "<!--") ->
        parse_children(remaining, context, closing_tag, ancestors, source_context, tree, parent)

      is_tuple(context) and String.starts_with?(token, "<![CDATA[") ->
        text = binary_part(token, 9, byte_size(token) - 12)

        {tree, id} =
          attach_node(
            tree,
            parent,
            %{
              type: :text,
              text: text,
              source: token_source(source_context, token, remaining)
            },
            source_context
          )

        {tree, _node} = HtmlTree.close(tree, id)
        parse_children(remaining, context, closing_tag, ancestors, source_context, tree, parent)

      Regex.match?(~r/^<!doctype(?=[ \t\n\f\r>])/i, token) ->
        case HtmlValidator.validate_html_doctype(
               token,
               context,
               Enum.reverse(HtmlTree.children(tree, parent))
             ) do
          :ok ->
            parse_children(
              remaining,
              :after_doctype,
              closing_tag,
              ancestors,
              source_context,
              tree,
              parent
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
                {:ok, tree, remaining}

              HtmlValidator.omitted_html_end_tag?(closing_tag, {:end, tag}, ancestors) ->
                {:ok, tree, {:pending, token, remaining}}

              true ->
                case HtmlValidator.html_implied_start_closure(
                       context,
                       tag,
                       Enum.reverse(HtmlTree.children(tree, parent))
                     ) do
                  nil ->
                    {:error, {:html_source, token, byte_size(stream_source(remaining))}}

                  {:group, kind, group_children, previous} ->
                    group = HtmlValidator.construct_html_group(kind, group_children)

                    tree =
                      HtmlTree.replace_children(tree, parent, Enum.reverse(previous) ++ [group])

                    parse_children(
                      remaining,
                      context,
                      closing_tag,
                      ancestors,
                      source_context,
                      tree,
                      parent
                    )

                  next_context ->
                    parse_children(
                      remaining,
                      next_context,
                      closing_tag,
                      ancestors,
                      source_context,
                      tree,
                      parent
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
             source = token_source(source_context, token, remaining),
             planning =
               (if tag == "template" and prepared.namespace == :html do
                  offset =
                    source_context.size - byte_size(token) - byte_size(stream_source(remaining))

                  {tree, parent,
                   %{
                     source: source,
                     offset:
                       offset +
                         boundary_index(
                           source_context.crlf,
                           offset,
                           0,
                           tuple_size(source_context.crlf)
                         ),
                     tokenizer_context: context,
                     ancestors: ancestors
                   }}
                else
                  nil
                end),
             :ok <- HtmlValidator.validate_html_element_attributes(tag, attributes, planning),
             false <- HtmlValidator.omitted_html_end_tag?(closing_tag, {:start, tag}, ancestors),
             true <- ForeignContentValidator.valid_child?(context, prepared, ancestors) do
          element = %{
            type: :element,
            tag: tag,
            attributes: attributes,
            children: [],
            source: source
          }

          element =
            if prepared.namespace != :html or is_tuple(context),
              do:
                element
                |> Map.put(:namespace, prepared.namespace)
                |> Map.put(:attribute_namespaces, prepared.attribute_namespaces),
              else: element

          # Attach the opening element now. Recursive calls only track tokenizer
          # context; the canonical table owns open and closed tree relationships.
          {tree, id} = attach_node(tree, parent, element, source_context)

          {tree, child_parent} =
            if tag == "template" and prepared.namespace == :html,
              do: HtmlTree.open_fragment(tree, id, element.source),
              else: {tree, id}

          with {:ok, tree, rest} <-
                 element_children(
                   ForeignContentValidator.child_context(prepared),
                   if(prepared.namespace == :svg, do: String.downcase(tag), else: tag),
                   remaining,
                   prepared.self_closing?,
                   HtmlValidator.html_parent_stack(context, tag, ancestors),
                   source_context,
                   tree,
                   child_parent
                 ),
               element_children <- HtmlTree.children(tree, child_parent),
               true <-
                 ForeignContentValidator.valid_element?(
                   prepared,
                   element_children,
                   context,
                   ancestors
                 ) do
            normalized =
              HtmlValidator.normalize_html_element_children(
                tag,
                element_children,
                stream_source(remaining)
              )

            tree =
              if normalized == element_children,
                do: tree,
                else: HtmlTree.replace_children(tree, child_parent, normalized)

            {tree, _node} = HtmlTree.close(tree, child_parent)
            {tree, _node} = if child_parent == id, do: {tree, nil}, else: HtmlTree.close(tree, id)
            parse_children(rest, context, closing_tag, ancestors, source_context, tree, parent)
          else
            false ->
              {:error, {:html_source, token, byte_size(stream_source(remaining))}}

            {:error, :unsupported_html} ->
              {:error, {:html_source, token <> stream_source(remaining), 0}}

            error ->
              error
          end
        else
          true ->
            {:ok, tree, {:pending, token, remaining}}

          {:error, {:html_source, source}} ->
            {:error, {:html_source, source, byte_size(stream_source(remaining))}}

          {:error, {reason, diagnostic}} when is_map(diagnostic) ->
            diagnostic = Diagnostics.with_context(diagnostic, source: token)

            {:error,
             {:html_diagnostic, reason, diagnostic, byte_size(stream_source(remaining)), token}}

          {:error, :unsupported_html} ->
            {:error, {:html_source, token <> stream_source(remaining), 0}}

          false ->
            {:error, {:html_source, token, byte_size(stream_source(remaining))}}
        end

      true ->
        case HtmlValidator.html_text_action(context, token) do
          :ignore ->
            parse_children(
              remaining,
              context,
              closing_tag,
              ancestors,
              source_context,
              tree,
              parent
            )

          :invalid ->
            {:error,
             {:html_source, String.trim_leading(token), byte_size(stream_source(remaining))}}

          :text ->
            {tree, id} =
              attach_node(
                tree,
                parent,
                %{
                  type: :text,
                  text: HtmlEntities.decode(token, :text),
                  source: token_source(source_context, token, remaining)
                },
                source_context
              )

            {tree, _node} = HtmlTree.close(tree, id)

            parse_children(
              remaining,
              context,
              closing_tag,
              ancestors,
              source_context,
              tree,
              parent
            )
        end
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

  defp element_children(
         context,
         tag,
         remaining,
         self_closing?,
         ancestors,
         source_context,
         tree,
         parent
       ) do
    case self_closing? do
      true ->
        {:ok, tree, remaining}

      false ->
        HtmlValidator.within_html_element(fn ->
          parse_children(remaining, context, tag, ancestors, source_context, tree, parent)
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

  defp attach_node(tree, parent, node, source_context) do
    before =
      case Map.get(source_context, :insertion) do
        %{parent: ^parent, end: finish} -> finish
        _ -> nil
      end

    HtmlTree.insert(tree, parent, node, before)
  end

  defp token_source(context, token, remaining) do
    offset = context.size - byte_size(token) - byte_size(stream_source(remaining))
    {text, line, column} = original_source_span(context, offset, byte_size(token))
    %{text: text, line: line, column: column}
  end

  # Precomputed newline and grapheme indexes preserve byte spans and columns
  # without rescanning the document or line prefix for each captured token.
  defp original_source_span(context, offset, length) do
    first = offset + boundary_index(context.crlf, offset, 0, tuple_size(context.crlf))

    last =
      offset + length + boundary_index(context.crlf, offset + length, 0, tuple_size(context.crlf))

    line = boundary_index(context.lines, first + 1, 0, tuple_size(context.lines))
    line_start = elem(context.lines, line - 1)

    HtmlValidator.reserve_render_resource(
      :max_html_tree_work,
      last - first,
      :html
    )

    extras =
      Enum.map([first, line_start], fn position ->
        index =
          boundary_index(
            context.column_positions,
            position + 1,
            0,
            tuple_size(context.column_positions)
          )

        if index == 0, do: 0, else: elem(context.column_extras, index - 1)
      end)

    [first_extra, line_extra] = extras
    column = first - line_start - first_extra + line_extra + 1
    {binary_part(context.original, first, last - first), line, column}
  end

  # Grapheme byte-width differences let source columns use binary searches,
  # including combining marks, without rescanning each token's full line prefix.
  defp unicode_columns(html, offset, extra, positions, extras, pattern) do
    case :binary.match(html, pattern) do
      :nomatch ->
        {positions |> Enum.reverse() |> List.to_tuple(),
         extras |> Enum.reverse() |> List.to_tuple()}

      {index, 1} ->
        # Include the preceding ASCII character in case a combining mark belongs
        # to it. All other ASCII bytes can be skipped in one binary scan.
        skipped = max(index - 1, 0)
        relevant = binary_part(html, skipped, byte_size(html) - skipped)
        {grapheme, remaining} = String.next_grapheme(relevant)
        size = byte_size(grapheme)
        offset = offset + skipped + size

        if size == 1 do
          unicode_columns(remaining, offset, extra, positions, extras, pattern)
        else
          unicode_columns(
            remaining,
            offset,
            extra + size - 1,
            [offset | positions],
            [extra + size - 1 | extras],
            pattern
          )
        end
    end
  end

  defp boundary_index(positions, boundary, first, last, work \\ 0) do
    if first == last do
      # Source size is validated before indexing. Account for the bounded binary
      # search once rather than updating the shared budget at every comparison.
      HtmlValidator.reserve_render_resource(:max_html_tree_work, work + 1, :html)
      first
    else
      middle = div(first + last, 2)

      if elem(positions, middle) < boundary,
        do: boundary_index(positions, boundary, middle + 1, last, work + 1),
        else: boundary_index(positions, boundary, first, middle, work + 1)
    end
  end
end
