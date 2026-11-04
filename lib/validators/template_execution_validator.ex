defmodule NativeElixirPdfUtilities.Validators.TemplateExecutionValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.{Diagnostics, HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, HtmlTree, ResourceContext, Style}

  alias NativeElixirPdfUtilities.Validators.{
    FontValidator,
    HtmlCompositionValidator,
    HtmlValidator,
    ResourceValidator,
    TemplatePatchValidator,
    TemplateRendererContextValidator
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

  @doc false
  @spec prepare_source(term(), term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_source(source, opts, image_budget) do
    NativeElixirPdfUtilities.Validators.HtmlRecoveryValidator.with_render_options(opts, fn ->
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
          fragment_provenances: %{},
          declaration_contexts: [],
          css_budget: HtmlValidator.new_css_budget()
        }

        Process.put(@key, frame)

        try do
          with {:ok, original} <- HtmlParser.parse_document_detailed(source) do
            # Parsed HTML tag names preserve their authored ASCII letters. If the
            # source has no template spelling, no execution template can exist.
            nodes =
              if String.contains?(String.downcase(source, :ascii), "template"),
                do: authored_nodes(original, true),
                else: []

            templates =
              Enum.filter(nodes, fn node ->
                Map.get(node, :tag) == "template" and
                  HtmlCompositionValidator.template_mode(node.attributes) == :replacement
              end)

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
                       Enum.flat_map(entries, fn entry ->
                         if entry.source_kind == :configured, do: [entry.css], else: []
                       end),
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
                   :ok <-
                     prepare_original_font_sources(
                       source_contexts.font_source_entries,
                       entries,
                       opts
                     ),
                   :ok <- prepare_original_fragment_contexts(nodes, opts),
                   {:ok, prepared} <- prepare_runtime_view(original, opts, image_budget),
                   :ok <- validate_glyph_coverage(original, prepared),
                   :ok <- prepare_original_fragments(opts, image_budget),
                   :ok <- complete_declaration_contexts(Process.get(@key).declaration_contexts),
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
    end)
  end

  @doc false
  @spec validate_original_runtime_scope(term(), term(), term()) ::
          :ok | {:error, {atom(), map()}}
  def validate_original_runtime_scope(dom, provenance, budget) do
    case Process.get(@key) do
      %{mode: :original, busy: true, original: original} = frame when not is_nil(original) ->
        views = [HtmlTree.fetch(original, original.root) | frame.fragment_views]
        expected = Enum.find(views, fn view -> is_map(dom) and view.node_id == dom[:node_id] end)

        source =
          if expected,
            do: Map.get(frame.fragment_provenances, expected.node_id, frame.source_provenance)

        preparation = Process.get({TemplateRendererContextValidator, :preparation})

        with :ok <- validate_live(),
             true <-
               is_map(expected) and expected == dom and source == provenance and
                 frame.css_budget == budget and
                 (is_nil(preparation) or preparation.resource_context != frame.resource) do
          :ok
        else
          {:error, {_reason, _diagnostic}} = error ->
            error

          _ ->
            Diagnostics.error(
              :html,
              :invalid_document,
              "original runtime observation requires its own issued resource scope, exact original view, provenance and shared CSS budget",
              module: __MODULE__,
              operation: :original_runtime_source,
              source: "original runtime scope"
            )
        end

      _ ->
        Diagnostics.error(
          :html,
          :invalid_document,
          "original runtime source observations require an active original preparation invocation",
          module: __MODULE__,
          operation: :original_runtime_source,
          source: "original runtime scope"
        )
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
        HtmlCompositionValidator.template_mode(attributes) == :replacement and
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
    case {Process.get(@key), Process.get({TemplatePatchValidator, :invocation})} do
      {%{mode: :execute, source: ^source, parser: parser, busy: false}, %{id: id}}
      when not is_nil(parser) and parser == id ->
        0

      _ ->
        byte_size(source)
    end
  end

  @doc false
  @spec parser_tree(map(), atom()) :: HtmlTree.t()
  def parser_tree(source, type) do
    case {Process.get(@key), Process.get({TemplatePatchValidator, :invocation})} do
      {%{mode: :execute, original: original, parser: parser, busy: false}, %{id: id}}
      when not is_nil(parser) and parser == id ->
        root = original.nodes[original.root]

        %HtmlTree{
          root: root.node_id,
          nodes: %{root.node_id => root |> Map.delete(:projection) |> Map.put(:children, [])}
        }

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

  # Applied declarations need genuine original execution. Unapplied stylesheet
  # declarations instead retain their canonical symbolic source preparation.
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
                         Enum.flat_map(entries, fn entry ->
                           if entry.source_kind == :configured, do: [entry.css], else: []
                         end),
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
                       prepare_original_font_sources(contexts.font_source_entries, entries, opts) do
                  current = Process.get(@key)

                  checked =
                    Enum.reduce(contexts.source_obligations.declaration_contexts, %{}, fn record,
                                                                                          checked ->
                      HtmlValidator.reserve_render_resource(
                        :max_css_work,
                        byte_size(record.source.text) + 1,
                        :css
                      )

                      if record.status in [:checked_in_original_context, :checked_symbolically] and
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
                            Map.merge(
                              record,
                              Map.take(match, [:status, :contexts, :source_value, :selection])
                            )
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
                      fragment_views: [view | current.fragment_views],
                      fragment_provenances:
                        Map.put(
                          current.fragment_provenances,
                          view.node_id,
                          contexts.source_provenance
                        )
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

  defp prepare_original_font_sources(source_entries, entries, opts) do
    configured = Enum.filter(entries, &(Map.get(&1, :source_kind) == :configured))
    count = length(configured)
    HtmlValidator.reserve_render_resource(:max_css_work, count + length(source_entries), :font)

    sources =
      source_entries
      |> Enum.with_index()
      |> Enum.map(fn {source, index} ->
        if index < count do
          # Pair by the original loader's ordering, never by equal CSS bytes.
          Map.merge(Enum.at(configured, index), source)
        else
          Map.merge(
            %{base_url: Keyword.get(opts, :base_url), trusted_local_resources: false},
            source
          )
        end
      end)

    with {:ok, selected} <-
           NativeElixirPdfUtilities.Validators.CssFontFaceValidator.prepare_original_resources(
             sources
           ) do
      if selected == [] do
        :ok
      else
        frame = Process.get(@key)
        Process.put(@key, %{frame | busy: true})

        try do
          with {:ok, url_context} <-
                 NativeElixirPdfUtilities.Validators.HtmlUrlValidator.document_context(
                   HtmlTree.fetch(frame.original, frame.original.root),
                   opts
                 ) do
            opts = Keyword.put(opts, :__document_url_context__, url_context)

            Enum.reduce_while(selected, :ok, fn entry, :ok ->
              result =
                NativeElixirPdfUtilities.Validators.CssFontFaceValidator.with_original_resource(
                  entry,
                  fn ->
                    with :ok <- validate_live(),
                         {:ok, _original_registry} <- Style.prepare_font_resources([entry], opts),
                         do: validate_live()
                  end
                )

              case result do
                :ok -> {:cont, :ok}
                error -> {:halt, error}
              end
            end)
          end
        after
          Process.put(@key, %{Process.get(@key) | busy: frame.busy})
        end
      end
    end
  end

  defp prepare_original_fragments(opts, image_budget) do
    Enum.reduce_while(Enum.reverse(Process.get(@key).fragment_views), :ok, fn view, :ok ->
      result =
        with {:ok, prepared} <- prepare_runtime_view(view, opts, image_budget),
             :ok <- validate_glyph_coverage(view, prepared),
             do: :ok

      case result do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  @doc "Returns whether ordinary original rendering left authored owners unvisited."
  @spec source_completion_required?(map(), map()) :: boolean()
  def source_completion_required?(dom, prepared) do
    actual = MapSet.new(authored_nodes(prepared.styled_tree, false), &Map.get(&1, :node_id))

    Enum.any?(authored_nodes(dom, false), fn node ->
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
      style = Map.get(Process.get(@key).source_styles, node.node_id, %{})
      text? = node.type == :text and String.trim(node.text) != ""

      resource? =
        Map.get(node, :tag) in ~w(img iframe frame object embed) or
          Map.has_key?(style, :_image_input_state) or
          Enum.any?([:background_image_source, :list_style_image_source], &Map.get(style, &1))

      control? = Map.has_key?(style, :form_kind) and style.form_kind != :hidden

      dormant_box? =
        node.type == :element and Map.get(style, :display) == :none and
          Map.get(node, :tag) not in ~w(base style script title meta link template source) and
          Map.get(style, :form_kind) != :hidden

      dormant_box? or (text? and not MapSet.member?(actual, node.node_id)) or
        ((resource? or control?) and
           (not MapSet.member?(actual, node.node_id) or Map.get(style, :display) == :none))
    end)
  end

  defp validate_glyph_coverage(original, prepared) do
    views = [
      prepared.styled_tree
      | Enum.map(Map.get(prepared, :original_source_views, []), & &1.styled_tree)
    ]

    actual =
      Enum.group_by(Enum.flat_map(views, &authored_nodes(&1, false)), &Map.get(&1, :node_id))

    math_runs = Map.get(prepared, :original_math_glyph_runs, %{})
    source_tree = HtmlTree.from_document(original)

    authored_nodes(original, false)
    |> Enum.reduce_while(:ok, fn node, :ok ->
      # SVG descendants are converter image content. Their canonical source,
      # fonts and pixels were prepared with the image, without PDF glyph runs.
      image_text? =
        if node.type == :text do
          Stream.unfold(node.parent_id, fn id ->
            case Map.get(source_tree.nodes, id) do
              %{parent_id: parent_id} = parent ->
                HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :font)
                {parent, parent_id}

              _ ->
                nil
            end
          end)
          |> Enum.any?(&(Map.get(&1, :namespace) == :svg and Map.get(&1, :tag) == "svg"))
        else
          false
        end

      if node.type == :text and String.trim(node.text) != "" and not image_text? do
        case Map.get(actual, node.node_id) do
          nil ->
            covered =
              math_runs
              |> Map.get(node.node_id, [])
              |> Enum.map(& &1.source_offset)
              |> MapSet.new()

            required =
              node.text
              |> String.codepoints()
              |> Enum.with_index()
              |> Enum.reject(fn {text, _} -> text in [" ", "\t", "\r", "\n", "\f"] end)
              |> Enum.map(&elem(&1, 1))
              |> MapSet.new()

            if MapSet.size(covered) > 0 and MapSet.subset?(required, covered) do
              {:cont, :ok}
            else
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
            end

          runs ->
            # These runs were issued from this exact original node during the
            # guarded preparation. A lost identity here is an internal invariant
            # failure; caller-authored identities are rejected before issuance.
            Enum.each(runs, fn run ->
              :ok =
                NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator.validate_prepared_source(
                  run,
                  node,
                  :font
                )
            end)

            {:cont, :ok}
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
      preparation =
        if frame.mode == :original and frame.source_provenance do
          provenance = Map.get(frame.fragment_provenances, dom.node_id, frame.source_provenance)

          with {:ok, prepared, cascades} <-
                 NativeElixirPdfUtilities.Validators.TemplateRendererContextValidator.with_original_source_execution(
                   dom,
                   provenance,
                   frame.css_budget,
                   fn -> HtmlToPdf.prepare_document(dom, opts, budget) end
                 ) do
            observe_original_runtime_cascades(cascades)
            {:ok, prepared}
          end
        else
          HtmlToPdf.prepare_document(dom, opts, budget)
        end

      with {:ok, prepared} <- preparation do
        if frame.original do
          Enum.reduce_while(
            [prepared | Map.get(prepared, :original_source_views, [])],
            {:ok, prepared},
            fn view, result ->
              with {:ok, writer} <-
                     NativeElixirPdfUtilities.Validators.WriterValidator.prepare(
                       view.pages,
                       view.options
                     ),
                   {:ok, _, _} <-
                     NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare(
                       writer.pages,
                       view.options
                     ) do
                {:cont, result}
              else
                error -> {:halt, error}
              end
            end
          )
        else
          {:ok, prepared}
        end
      end
    after
      Process.put(@key, %{Process.get(@key) | busy: frame.busy})
    end
  end

  # Only successful original preparation reaches this merge. Prospective and
  # callback executions cannot turn their observations into original checks.
  defp observe_original_runtime_cascades(cascades) do
    frame = Process.get(@key)

    checked =
      cascades
      |> Map.values()
      |> List.flatten()
      |> Enum.reduce(%{}, fn declaration, checked ->
        HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)

        if is_map(declaration.source) do
          key = {declaration.source, declaration.declaration}
          node = declaration.context

          context = %{
            owner: Map.take(node, [:node_id, :document_id, :tree_scope_id, :parent_id]),
            source: node.source,
            tag: node.tag,
            pseudo: declaration.pseudo,
            execution: :original_runtime,
            clone_origin: declaration.clone_origin
          }

          Map.update(checked, key, [context], &[context | &1])
        else
          checked
        end
      end)

    records =
      Enum.map(frame.declaration_contexts, fn record ->
        HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)
        contexts = Map.get(checked, {record.source, record.declaration}, [])

        if contexts != [] do
          record
          |> Map.put(:status, :checked_in_original_context)
          |> Map.put(:contexts, Enum.uniq(record.contexts ++ contexts))
          |> Map.delete(:reason)
          |> Map.delete(:selection)
        else
          record
        end
      end)

    Process.put(@key, %{frame | declaration_contexts: records})
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
