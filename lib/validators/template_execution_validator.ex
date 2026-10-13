defmodule NativeElixirPdfUtilities.Validators.TemplateExecutionValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.{Diagnostics, HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, HtmlTree, ResourceContext, Style}

  alias NativeElixirPdfUtilities.Validators.{
    FontValidator,
    HtmlValidator,
    ResourceValidator,
    TemplatePatchValidator
  }

  @key {__MODULE__, :execution}
  @payload [
    :type,
    :tag,
    :namespace,
    :attributes,
    :attribute_namespaces,
    :attribute_order,
    :text,
    :target,
    :data,
    :source
  ]
  @shadow ~w(shadowrootmode shadowrootdelegatesfocus shadowrootserializable shadowrootslotassignment shadowrootclonable shadowrootcustomelementregistry)

  @doc false
  @spec prepare_source(term(), term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_source(source, opts, image_budget) do
    with :ok <-
           HtmlValidator.validate_render_request(
             source,
             opts,
             NativeElixirPdfUtilities.HtmlToPdf.Font.normalize_options(opts)
           ),
         :ok <- ResourceValidator.validate_preparation_budget(image_budget) do
      previous = Process.get(@key)

      frame = %{
        source: source,
        opts: opts,
        resource: ResourceContext.current(),
        limits: Limits.effective(),
        font_budget: Process.get({FontValidator, :budget}).budget_id,
        mode: :original,
        parser: nil,
        original: nil,
        indexed: %{},
        actions: %{},
        busy: false,
        source_styles: %{},
        source_provenance: nil,
        source_view: nil,
        capture: nil,
        fragment_views: [],
        declaration_contexts: [],
        css_budget: HtmlValidator.new_css_budget()
      }

      Process.put(@key, frame)

      try do
        with {:ok, original} <- HtmlParser.parse_document_detailed(source) do
          nodes = authored_nodes(original, true)

          templates =
            Enum.filter(nodes, &match?(%{tag: "template", attributes: %{"for" => _}}, &1))

          if templates == [] do
            prepare_runtime_view(original, opts, image_budget)
          else
            indexed =
              Enum.reduce(nodes, %{}, fn node, index ->
                HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)

                key =
                  {node.source.line, node.source.column, node.type, Map.get(node, :tag),
                   node.parent_id}

                Map.update(index, key, [node], &(&1 ++ [node]))
              end)

            Process.put(@key, %{
              Process.get(@key)
              | original: HtmlTree.from_document(original),
                indexed: indexed
            })

            with {:ok, entries} <- Style.prepare_stylesheets(original, opts),
                 {:ok, source_contexts} <-
                   Style.with_source_style_contexts(
                     original,
                     Enum.map(entries, & &1.css),
                     Process.get(@key).css_budget,
                     fn node, _, context ->
                       if context.status == :resolved do
                         current = Process.get(@key)
                         HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)

                         Process.put(@key, %{
                           current
                           | source_styles:
                               Map.put(current.source_styles, node.node_id, context.style)
                         })
                       end

                       :ok
                     end,
                     opts
                   ),
                 _ =
                   Process.put(@key, %{
                     Process.get(@key)
                     | source_provenance: source_contexts.source_provenance,
                       declaration_contexts:
                         source_contexts.source_obligations.declaration_contexts
                   }),
                 :ok <- prepare_original_fragment_contexts(nodes, opts),
                 :ok <- complete_declaration_contexts(Process.get(@key).declaration_contexts),
                 :ok <- validate_original_runtime_contexts(nodes),
                 {:ok, prepared} <- prepare_runtime_view(original, opts, image_budget),
                 :ok <- validate_glyph_coverage(original, prepared.styled_tree),
                 :ok <- prepare_original_fragments(opts, image_budget),
                 :ok <- validate_live() do
              Process.put(@key, %{Process.get(@key) | mode: :execute, parser: nil})

              with {:ok, dom} <- HtmlParser.parse_document_detailed(source),
                   :ok <- validate_live(),
                   {:ok, prepared} <- prepare_runtime_view(dom, opts, image_budget),
                   :ok <- validate_live() do
                {:ok, prepared}
              end
            end
          end
        end
      after
        if previous, do: Process.put(@key, previous), else: Process.delete(@key)
      end
    end
  end

  @doc false
  @spec with_parser_scope(reference(), String.t(), (-> term())) :: term()
  def with_parser_scope(id, source, consumer) do
    case Process.get(@key) do
      %{parser: nil, source: ^source, busy: false} = frame ->
        Process.put(@key, %{frame | parser: id})

        try do
          consumer.()
        after
          Process.put(@key, %{Process.get(@key) | parser: nil})
        end

      _ ->
        consumer.()
    end
  end

  @doc false
  @spec inert_source_view?(map(), term()) :: boolean()
  def inert_source_view?(attributes, context) do
    case Process.get(@key) do
      %{mode: mode, source: source} = frame ->
        not Enum.any?(@shadow, &Map.has_key?(attributes, &1)) and
          (context == nil or
             (mode == :original and
                match?(
                  %{id: id, source: ^source} when id == frame.parser,
                  Process.get({TemplatePatchValidator, :invocation})
                )) or
             (mode == :execute and frame.busy and is_map(frame.capture) and
                match?(
                  %{id: id, source: ^source} when id == frame.parser,
                  Process.get({TemplatePatchValidator, :invocation})
                ) and
                Process.get({TemplatePatchValidator, :candidate_capture}) == frame.capture and
                case context do
                  {tree, _parent, origin} ->
                    tree.root == frame.capture.tree.root and
                      origin.offset > frame.capture.offset and
                      (origin.tokenizer_context == "template" or "template" in origin.ancestors) and
                      validate_live() == :ok

                  _ ->
                    false
                end))

      _ ->
        false
    end
  end

  @doc false
  @spec source_charge(String.t()) :: non_neg_integer()
  def source_charge(source) do
    case Process.get(@key) do
      %{mode: :execute, source: ^source, parser: parser, busy: false} when not is_nil(parser) ->
        if match?(%{id: ^parser}, Process.get({TemplatePatchValidator, :invocation})),
          do: 0,
          else: byte_size(source)

      _ ->
        byte_size(source)
    end
  end

  @doc false
  @spec parser_tree(map(), atom()) :: HtmlTree.t()
  def parser_tree(source, type) do
    case Process.get(@key) do
      %{mode: :execute, original: original, parser: parser, busy: false}
      when not is_nil(parser) ->
        if match?(%{id: ^parser}, Process.get({TemplatePatchValidator, :invocation})) do
          root = original.nodes[original.root]

          %HtmlTree{
            root: root.node_id,
            nodes: %{root.node_id => root |> Map.delete(:projection) |> Map.put(:children, [])}
          }
        else
          HtmlTree.new(source, type)
        end

      _ ->
        HtmlTree.new(source, type)
    end
  end

  @doc false
  @spec original_node_id(map(), reference()) :: reference() | nil
  def original_node_id(node, parent) do
    case Process.get(@key) do
      %{mode: :execute, parser: parser, indexed: indexed} = frame when not is_nil(parser) ->
        key = {node.source.line, node.source.column, node.type, Map.get(node, :tag), parent}

        indexed =
          if match?(%{id: ^parser}, Process.get({TemplatePatchValidator, :invocation})),
            do: indexed,
            else: %{}

        case Map.get(indexed, key, []) do
          [original | rest] ->
            HtmlValidator.reserve_render_resource(
              :max_html_tree_work,
              byte_size(node.source.text) + 1,
              :html
            )

            if Map.take(node, @payload) == Map.take(original, @payload) do
              Process.put(@key, %{frame | indexed: Map.put(indexed, key, rest)})
              original.node_id
            end

          [] ->
            nil
        end

      _ ->
        nil
    end
  end

  @doc false
  @spec original_fragment_id(reference()) :: reference() | nil
  def original_fragment_id(host) do
    case Process.get(@key) do
      %{mode: :execute, original: original} ->
        case Map.get(original.nodes, host) do
          %{content: fragment} -> fragment
          _ -> nil
        end

      _ ->
        nil
    end
  end

  @doc false
  @spec original_prefix_provenance(term(), term()) ::
          :not_owned | {:ok, map()} | {:error, {atom(), map()}}
  def original_prefix_provenance(tree, finish) do
    case Process.get(@key) do
      %{
        mode: :execute,
        busy: true,
        parser: parser,
        original: original,
        source_provenance: provenance
      }
      when not is_nil(provenance) ->
        if match?(%{id: ^parser}, Process.get({TemplatePatchValidator, :invocation})) do
          project_original_source(tree, finish, original.root)
        else
          :not_owned
        end

      _ ->
        :not_owned
    end
  end

  @doc false
  @spec original_view_provenance(term()) :: :not_owned | {:ok, map()} | {:error, {atom(), map()}}
  def original_view_provenance(dom) do
    case Process.get(@key) do
      %{
        mode: :original,
        busy: true,
        source_view: %{root: root, fragment: fragment},
        original: original
      } ->
        if is_map(dom) and dom[:node_id] == root do
          with :ok <-
                 NativeElixirPdfUtilities.Validators.HtmlTreeValidator.validate_transport(dom),
               tree = HtmlTree.from_document(dom),
               {:ok, provenance} <-
                 project_original_source(
                   tree,
                   byte_size(original.nodes[original.root].source.text),
                   root
                 ) do
            contexts =
              Map.new(provenance.insertion_contexts, fn {id, reason} ->
                {id, if(original.nodes[id].tree_scope_id == fragment, do: nil, else: reason)}
              end)

            embedded =
              Enum.map(provenance.embedded, fn entry ->
                entry = %{entry | node: HtmlTree.fetch(tree, entry.node.node_id)}

                if entry.node.tree_scope_id == fragment,
                  do: Map.put(entry, :insertion_context, nil),
                  else: entry
              end)

            {:ok, %{provenance | insertion_contexts: contexts, embedded: embedded}}
          end
        else
          :not_owned
        end

      _ ->
        :not_owned
    end
  end

  # Both prospective prefixes and detached views preserve exact authored payloads
  # while keeping real placement separate from original lexical ownership.
  defp project_original_source(tree, finish, expected_root) do
    %{original: original, source_provenance: provenance} = Process.get(@key)

    with :ok <- validate_live(),
         {:ok, _prefix} <-
           NativeElixirPdfUtilities.Validators.HtmlCssSourceValidator.validate_source_prefix(
             original.nodes[original.root].source.text,
             finish
           ),
         :ok <-
           if(
             is_struct(tree, HtmlTree) and is_map(tree.nodes) and
               tree.root == expected_root and Map.has_key?(tree.nodes, tree.root),
             do: :ok,
             else:
               Diagnostics.error(
                 :html,
                 :invalid_document,
                 "candidate source projection requires the captured logical tree",
                 module: __MODULE__,
                 operation: :template_candidate_projection,
                 source: "source_prefix"
               )
           ) do
      HtmlValidator.reserve_render_resource(
        :max_html_tree_work,
        map_size(tree.nodes),
        :html
      )

      bindings =
        Enum.reduce_while(tree.nodes, {:ok, %{}}, fn {id, node}, {:ok, bindings} ->
          metadata =
            if is_map(node) and node[:node_id] == id and is_reference(id) and
                 is_reference(node[:document_id]) do
              NativeElixirPdfUtilities.Validators.HtmlTreeValidator.validate_node_metadata(
                Map.delete(node, :projection)
              )
            else
              Diagnostics.error(
                :html,
                :invalid_document,
                "candidate source projection requires canonical stored node identities",
                module: __MODULE__,
                operation: :template_candidate_projection,
                source: "source_prefix"
              )
            end

          if metadata != :ok do
            {:halt, metadata}
          else
            HtmlValidator.reserve_render_resource(
              :max_html_tree_work,
              byte_size(node.source.text) + 1,
              :html
            )

            authored = Map.get(original.nodes, id)
            span = Map.get(provenance.token_bindings, id)

            cond do
              node.type in [:document, :document_fragment] or node.source.text == "" ->
                {:cont, {:ok, bindings}}

              id == node.node_id && authored && span &&
                span.offset + byte_size(span.raw) <= finish &&
                node.document_id == authored.document_id &&
                  Map.take(node, @payload) == Map.take(authored, @payload) ->
                {:cont,
                 {:ok,
                  Map.put(bindings, id, %{
                    original:
                      Map.take(authored, [
                        :node_id,
                        :document_id,
                        :parent_id,
                        :tree_scope_id,
                        :source
                      ]),
                    placement:
                      Map.take(node, [
                        :node_id,
                        :document_id,
                        :parent_id,
                        :tree_scope_id,
                        :source
                      ])
                  })}}

              true ->
                {:halt,
                 Diagnostics.error(
                   :html,
                   :invalid_document,
                   "candidate prefix cannot retain the protected original token identity; lexical ownership and current placement remain distinct",
                   module: __MODULE__,
                   operation: :template_candidate_projection,
                   source: node.source.text,
                   line: node.source.line,
                   column: node.source.column
                 )}
            end
          end
        end)

      with {:ok, bindings} <- bindings do
        ids = Map.keys(bindings)

        {:ok,
         %{
           provenance
           | token_bindings: Map.take(provenance.token_bindings, ids),
             inline: Map.take(provenance.inline, ids),
             records: Map.take(provenance.records, ids),
             insertion_contexts: Map.take(provenance.insertion_contexts, ids),
             embedded: Enum.filter(provenance.embedded, &Map.has_key?(bindings, &1.node.node_id))
         }
         |> Map.put(:prefix_placements, bindings)}
      end
    end
  end

  @doc false
  @spec authorize(term(), HtmlTree.t(), map()) :: :ok | :not_owned | {:error, {atom(), map()}}
  def authorize(handle, tree, context) do
    frame = Process.get(@key)
    invocation = Process.get({TemplatePatchValidator, :invocation})

    if is_map(frame) and frame.mode == :execute and not frame.busy and
         is_map(invocation) and invocation.id == frame.parser do
      with :ok <- validate_live() do
        if context.target == nil do
          :ok
        else
          capture = %{
            handle: handle,
            tree: tree,
            invocation: invocation.id,
            offset: context.source_offset
          }

          Process.put(@key, %{frame | busy: true, capture: capture})

          try do
            TemplatePatchValidator.with_candidate_projection(handle, tree, fn captured ->
              TemplatePatchValidator.with_candidate_evidence(captured, tree, fn candidate ->
                prospective = HtmlTree.fetch(candidate.prospective, candidate.prospective.root)

                with {:ok, _prepared} <-
                       prepare_runtime_view(
                         prospective,
                         frame.opts,
                         frame.resource.image_budget
                       ),
                     :ok <- validate_live() do
                  current = Process.get(@key)

                  action = %{
                    tree: tree,
                    prospective: candidate.prospective,
                    finish: candidate.captured_finish
                  }

                  Process.put(@key, %{
                    current
                    | actions: Map.put(current.actions, context.source_offset, action)
                  })

                  :ok
                end
              end)
            end)
          after
            Process.put(@key, %{Process.get(@key) | busy: frame.busy, capture: frame.capture})
          end
        end
      end
    else
      :not_owned
    end
  end

  @doc false
  @spec take_action(HtmlTree.t(), term()) :: :keep | {:replace, HtmlTree.t(), non_neg_integer()}
  def take_action(tree, planning) do
    case {Process.get(@key), planning} do
      {%{mode: :execute, actions: actions} = frame, {_, _, %{offset: offset}}} ->
        case Map.pop(actions, offset) do
          {%{tree: ^tree, prospective: projected, finish: finish}, remaining} ->
            Process.put(@key, %{frame | actions: remaining})
            {:replace, projected, finish}

          _ ->
            :keep
        end

      _ ->
        :keep
    end
  end

  # This names the complete authored-declaration obligation, rather than treating
  # successful active styling as policy for inactive or unmatched declarations.
  defp complete_declaration_contexts(records) do
    case Enum.find(records, &(&1.status == :unresolved)) do
      nil ->
        :ok

      record ->
        Diagnostics.error(
          :css,
          :unsupported_html,
          "template replacement requires an original context for #{record.reason}; this authored declaration remains unresolved",
          module: __MODULE__,
          operation: :parse,
          source: record.source.text,
          line: record.source.line,
          column: record.source.column
        )
    end
  end

  defp prepare_original_fragment_contexts(nodes, opts) do
    Enum.reduce_while(nodes, :ok, fn node, :ok ->
      case Map.get(node, :content) do
        %{type: :document_fragment} = fragment ->
          result =
            with {:ok, normalized} <-
                   HtmlValidator.construct_html_document(%{fragment | type: :document}) do
              view = normalized |> HtmlTree.from_document() |> then(&HtmlTree.fetch(&1, &1.root))
              frame = Process.get(@key)

              Process.put(@key, %{
                frame
                | busy: true,
                  source_view: %{root: view.node_id, fragment: fragment.node_id}
              })

              try do
                with {:ok, entries} <- Style.prepare_stylesheets(view, opts),
                     {:ok, contexts} <-
                       Style.with_source_style_contexts(
                         view,
                         Enum.map(entries, & &1.css),
                         frame.css_budget,
                         fn source_node, _, context ->
                           if context.status == :resolved do
                             current = Process.get(@key)
                             HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)

                             Process.put(@key, %{
                               current
                               | source_styles:
                                   Map.put(
                                     current.source_styles,
                                     source_node.node_id,
                                     context.style
                                   )
                             })
                           end

                           :ok
                         end,
                         opts
                       ),
                     :ok <-
                       complete_declaration_contexts(
                         Enum.reject(
                           contexts.source_obligations.declaration_contexts,
                           &(&1.status == :unresolved and &1.reason == :template_insertion_context)
                         )
                       ) do
                  current = Process.get(@key)

                  checked =
                    Enum.reduce(contexts.source_obligations.declaration_contexts, %{}, fn record,
                                                                                          checked ->
                      HtmlValidator.reserve_render_resource(
                        :max_css_work,
                        byte_size(record.source.text) + 1,
                        :css
                      )

                      if record.status == :checked_in_original_context and
                           record.source[:provenance] == :original_html and
                           is_map(record.source[:owner]) and
                           record.source.owner.tree_scope_id == fragment.node_id do
                        Map.put(checked, {record.source, record.declaration}, record)
                      else
                        checked
                      end
                    end)

                  records =
                    Enum.map(current.declaration_contexts, fn record ->
                      HtmlValidator.reserve_render_resource(
                        :max_css_work,
                        byte_size(record.source.text) + 1,
                        :css
                      )

                      if record.status == :unresolved and
                           record.source[:provenance] == :original_html and
                           is_map(record.source[:owner]) and
                           record.source.owner.tree_scope_id == fragment.node_id do
                        match = Map.get(checked, {record.source, record.declaration})

                        if match,
                          do:
                            %{
                              record
                              | status: :checked_in_original_context,
                                contexts: match.contexts
                            }
                            |> Map.delete(:reason),
                          else: record
                      else
                        record
                      end
                    end)

                  HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)

                  Process.put(@key, %{
                    current
                    | declaration_contexts: records,
                      fragment_views: [view | current.fragment_views]
                  })

                  :ok
                end
              after
                current = Process.get(@key)
                Process.put(@key, %{current | busy: frame.busy, source_view: frame.source_view})
              end
            end

          case result do
            :ok -> {:cont, :ok}
            error -> {:halt, error}
          end

        _ ->
          {:cont, :ok}
      end
    end)
  end

  defp prepare_original_fragments(opts, image_budget) do
    Enum.reduce_while(Enum.reverse(Process.get(@key).fragment_views), :ok, fn view, :ok ->
      result =
        with {:ok, prepared} <- prepare_runtime_view(view, opts, image_budget),
             :ok <- validate_glyph_coverage(view, prepared.styled_tree),
             do: :ok

      case result do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  # Ordinary preparation skips hidden/conditional owners. Generated control text
  # has no authored text child, so both glyph and resource ownership need this gate.
  defp validate_original_runtime_contexts(nodes) do
    frame = Process.get(@key)

    Enum.reduce_while(nodes, :ok, fn node, :ok ->
      style = Map.get(frame.source_styles, node.node_id, %{})

      resource? =
        Map.get(node, :tag) in ~w(img iframe frame object embed image use) or
          Map.has_key?(style, :_image_input_state) or
          Enum.any?([:background_image_source, :list_style_image_source], &Map.get(style, &1))

      control? = Map.has_key?(style, :form_kind) and style.form_kind != :hidden

      hidden? =
        if resource? or control? do
          Stream.unfold(node.node_id, fn id ->
            case Map.get(frame.original.nodes, id) do
              nil ->
                nil

              owner ->
                HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
                {owner, owner.parent_id}
            end
          end)
          |> Enum.any?(fn owner ->
            Map.get(Map.get(frame.source_styles, owner.node_id, %{}), :display) == :none or
              HtmlValidator.html_hidden_state(Map.get(owner, :attributes, %{})) != :not_hidden or
              (Map.get(owner, :tag) == "details" and not Map.has_key?(owner.attributes, "open"))
          end)
        else
          false
        end

      conditional? =
        if resource? or control? do
          Stream.unfold(node.parent_id, fn id ->
            case Map.get(frame.original.nodes, id) do
              nil ->
                nil

              owner ->
                HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
                {owner, owner.parent_id}
            end
          end)
          |> Enum.any?(
            &(Map.get(&1, :tag) in ~w(object embed iframe select datalist option optgroup))
          )
        else
          false
        end

      deferred? = NativeElixirPdfUtilities.Validators.ResponsiveImageValidator.allows_auto?(node)

      detached_base? =
        Map.get(node, :tag) == "base" and
          Map.get(frame.original.nodes[node.tree_scope_id], :type) == :document_fragment

      if detached_base? do
        {:halt,
         Diagnostics.error(
           :html,
           :unsupported_html,
           "template replacement requires the original detached document URL context before activating this base element",
           module: __MODULE__,
           operation: :parse,
           source: node.source.text,
           line: node.source.line,
           column: node.source.column
         )}
      else
        if resource? and (hidden? or conditional? or deferred?) do
          {:halt,
           Diagnostics.error(
             :asset,
             :unsupported_html,
             "template replacement cannot complete an original hidden, conditional or final-width resource context; authorization and contents remain pending",
             module: __MODULE__,
             operation: :parse,
             source: node.source.text,
             line: node.source.line,
             column: node.source.column
           )}
        else
          if control? and (hidden? or conditional?) do
            {:halt,
             Diagnostics.error(
               :font,
               :unsupported_html,
               "template replacement has an unprepared original control context; selected glyph and runtime obligations remain pending",
               module: __MODULE__,
               operation: :parse,
               source: node.source.text,
               line: node.source.line,
               column: node.source.column
             )}
          else
            {:cont, :ok}
          end
        end
      end
    end)
  end

  defp validate_glyph_coverage(original, styled) do
    actual = Enum.group_by(authored_nodes(styled, false), &Map.get(&1, :node_id))

    authored_nodes(original, false)
    |> Enum.reduce_while(:ok, fn node, :ok ->
      if node.type == :text and String.trim(node.text) != "" do
        case Map.get(actual, node.node_id) do
          nil ->
            {:halt,
             Diagnostics.error(
               :font,
               :unsupported_html,
               "template replacement has an unprepared original text context; selected glyph and runtime obligations remain pending",
               module: __MODULE__,
               operation: :parse,
               source: node.source.text,
               line: node.source.line,
               column: node.source.column
             )}

          runs ->
            result =
              Enum.reduce_while(runs, :ok, fn run, :ok ->
                case NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator.validate_prepared_source(
                       run,
                       node,
                       :font
                     ) do
                  :ok -> {:cont, :ok}
                  error -> {:halt, error}
                end
              end)

            case result do
              :ok -> {:cont, :ok}
              error -> {:halt, error}
            end
        end
      else
        {:cont, :ok}
      end
    end)
  end

  defp authored_nodes(root, fragments?) do
    Stream.unfold([root], fn
      [] ->
        nil

      [node | rest] ->
        HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)

        children =
          if not fragments? and Map.get(node, :tag) in ~w(style script title),
            do: [],
            else: Map.get(node, :children, [])

        children =
          if fragments? and is_map(Map.get(node, :content)),
            do: children ++ [node.content],
            else: children

        {node, children ++ rest}
    end)
    |> Enum.to_list()
  end

  # User resource callbacks may reenter public parsers. Ordinary preparation
  # cannot lend the surrounding execution parser's source/action authority.
  defp prepare_runtime_view(dom, opts, budget) do
    frame = Process.get(@key)
    Process.put(@key, %{frame | busy: true})

    try do
      with {:ok, prepared} <- HtmlToPdf.prepare_document(dom, opts, budget) do
        if frame.original do
          with {:ok, writer} <-
                 NativeElixirPdfUtilities.Validators.WriterValidator.prepare(
                   prepared.pages,
                   prepared.options
                 ),
               {:ok, _, _} <-
                 NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare(
                   writer.pages,
                   prepared.options
                 ) do
            {:ok, prepared}
          end
        else
          {:ok, prepared}
        end
      end
    after
      Process.put(@key, %{Process.get(@key) | busy: frame.busy})
    end
  end

  defp validate_live do
    frame = Process.get(@key)

    with :ok <- ResourceValidator.validate_context(frame.resource) do
      if frame.resource == ResourceContext.current() and frame.limits == Limits.effective() and
           frame.font_budget == Process.get({FontValidator, :budget}).budget_id do
        :ok
      else
        Diagnostics.error(
          :html,
          :invalid_document,
          "template execution source scope, preparation configuration or enclosing font budget changed",
          module: __MODULE__,
          operation: :parse,
          source: "template execution"
        )
      end
    end
  end
end
