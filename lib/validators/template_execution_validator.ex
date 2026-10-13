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
        source_styles: %{}
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
                     HtmlValidator.new_css_budget(),
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
                 :ok <- complete_declaration_contexts(source_contexts),
                 :ok <- validate_original_resource_contexts(nodes),
                 {:ok, prepared} <- prepare_runtime_view(original, opts, image_budget),
                 :ok <- validate_glyph_coverage(original, prepared.styled_tree),
                 :ok <- prepare_original_fragments(nodes, opts, image_budget),
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
                )))

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
          Process.put(@key, %{frame | busy: true})

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
            Process.put(@key, %{Process.get(@key) | busy: false})
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
  defp complete_declaration_contexts(result) do
    case Enum.find(result.source_obligations.declaration_contexts, &(&1.status == :unresolved)) do
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

  defp prepare_original_fragments(nodes, opts, image_budget) do
    Enum.reduce_while(nodes, :ok, fn node, :ok ->
      case Map.get(node, :content) do
        %{type: :document_fragment} = fragment ->
          # Validation executes an explicit detached view, retaining the original
          # fragment scope and child identities. It does not inherit host styles.
          result =
            with {:ok, normalized} <-
                   HtmlValidator.construct_html_document(%{fragment | type: :document}),
                 tree = HtmlTree.from_document(normalized),
                 view = HtmlTree.fetch(tree, tree.root),
                 {:ok, prepared} <- prepare_runtime_view(view, opts, image_budget),
                 :ok <- validate_glyph_coverage(view, prepared.styled_tree),
                 do: :ok

          case result do
            :ok -> {:cont, :ok}
            error -> {:halt, error}
          end

        _ ->
          {:cont, :ok}
      end
    end)
  end

  # Ordinary preparation skips hidden resource boxes. A skipped reference cannot
  # certify the original resource obligation merely because the final tree omits it.
  defp validate_original_resource_contexts(nodes) do
    frame = Process.get(@key)

    Enum.reduce_while(nodes, :ok, fn node, :ok ->
      style = Map.get(frame.source_styles, node.node_id, %{})

      resource? =
        Map.get(node, :tag) in ~w(img iframe frame object embed image use) or
          Enum.any?([:background_image_source, :list_style_image_source], &Map.get(style, &1))

      hidden? =
        if resource? do
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
        if resource? do
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
        if hidden? or conditional? or deferred? do
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
          {:cont, :ok}
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
          if Map.get(node, :tag) in ~w(style script title),
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
