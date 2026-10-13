defmodule NativeElixirPdfUtilities.Validators.TemplatePatchValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlTree, HtmlParser, ResourceContext}

  alias NativeElixirPdfUtilities.Validators.{
    HtmlCssSourceValidator,
    HtmlMetadataValidator,
    HtmlTreeValidator,
    HtmlValidator,
    FontValidator,
    ResourceValidator,
    TemplateRendererContextValidator,
    TemplateCandidateValidator
  }

  @composition_attributes ~w(for shadowrootmode shadowrootdelegatesfocus shadowrootserializable shadowrootslotassignment shadowrootclonable shadowrootcustomelementregistry)
  @pending_checks [
    :original_source_policy,
    :insertion_content_model,
    :insertion_style_context,
    :selected_fonts_and_metrics,
    :page_and_candidate_contexts,
    :runtime_layout_and_text_decorations,
    :active_resource_authorization_and_contents
  ]

  @doc false
  @spec with_parser_invocation(term(), (String.t() -> term())) :: term()
  def with_parser_invocation(source, consumer) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, source} <- HtmlValidator.validate_html_source(source) do
        key = {__MODULE__, :invocation}
        prior = Process.get(key)
        invocation = %{id: make_ref(), source: source, handles: %{}}
        Process.put(key, invocation)

        try do
          NativeElixirPdfUtilities.Validators.TemplateExecutionValidator.with_parser_scope(
            invocation.id,
            source,
            fn -> consumer.(source) end
          )
        after
          if prior, do: Process.put(key, prior), else: Process.delete(key)
        end
      end
    end)
  end

  @doc false
  @spec validate_attributes(map(), nil | {HtmlTree.t(), reference(), map()}) ::
          :ok | {:error, {atom(), map()}}
  def validate_attributes(attributes, context \\ nil) do
    composition = Enum.find(@composition_attributes, &Map.has_key?(attributes, &1))

    cond do
      is_nil(composition) ->
        :ok

      composition != "for" and not Map.has_key?(attributes, "for") ->
        NativeElixirPdfUtilities.Validators.HtmlCompositionValidator.validate_attributes(
          attributes
        )

      composition == "for" and
          NativeElixirPdfUtilities.Validators.TemplateExecutionValidator.inert_source_view?(
            attributes,
            context
          ) ->
        :ok

      context == nil or composition != "for" or
          Enum.any?(tl(@composition_attributes), &Map.has_key?(attributes, &1)) ->
        Diagnostics.error(
          :html,
          :unsupported_html,
          "template replacement and declarative shadow attributes require validated parser composition contexts",
          module: __MODULE__
        )

      true ->
        {tree, parent, origin} = context

        with_insertion_context(tree, parent, attributes["for"], origin, fn handle ->
          with_planned_insertion(handle, tree, fn _context -> authorize_mutation(handle, tree) end)
        end)
    end
  end

  @doc false
  @spec with_insertion_context(HtmlTree.t(), reference(), String.t(), map(), (reference() ->
                                                                                term())) :: term()
  def with_insertion_context(tree, parent, name, origin, consumer) do
    HtmlValidator.with_render_budget(fn ->
      invocation = Process.get({__MODULE__, :invocation})

      HtmlValidator.reserve_render_resource(
        :max_html_tree_work,
        map_size(tree.nodes) * (Limits.get(:max_html_depth) + 1) +
          byte_size(tree.nodes[tree.root].source.text),
        :html
      )

      with :ok <- require_invocation(invocation, tree),
           mapped =
             HtmlCssSourceValidator.map_span(
               HtmlCssSourceValidator.local(invocation.source),
               origin.offset,
               byte_size(origin.source.text)
             ),
           :ok <-
             if(Map.take(mapped, [:text, :line, :column]) == origin.source,
               do: :ok,
               else:
                 plan_error("template source does not match its exact original token location")
             ),
           :ok <- validate_opening_token(name, origin),
           :ok <- HtmlTreeValidator.validate_transport(HtmlTree.fetch(tree, tree.root)),
           {:ok, target} <- HtmlTreeValidator.prepare_content_patch(tree, parent, name) do
        original_parent =
          case tree.nodes[parent] do
            %{type: :element, tag: "template", content: content} = node ->
              if Map.get(node, :namespace, :html) == :html, do: content, else: parent

            _ ->
              parent
          end

        insertion_parent = if target, do: target.parent, else: original_parent
        original = insertion_context(tree, original_parent, origin.tokenizer_context)
        insertion = insertion_context(tree, insertion_parent, origin.tokenizer_context)

        context = %{
          name: name,
          source: origin.source,
          source_offset: origin.offset,
          target: target,
          original: original,
          insertion: insertion,
          tokenizer_context: origin.tokenizer_context,
          tokenizer_ancestors: origin.ancestors,
          pending_checks: @pending_checks
        }

        handle = make_ref()

        entry = %{
          invocation: invocation.id,
          kind: :insertion_plan,
          tree: tree,
          context: context,
          consumed: false,
          limits: Limits.effective(),
          counters: Process.get({HtmlValidator, :render_budget})
        }

        current = Process.get({__MODULE__, :invocation})

        Process.put({__MODULE__, :invocation}, %{
          current
          | handles: Map.put(current.handles, handle, entry)
        })

        try do
          consumer.(handle)
        after
          current = Process.get({__MODULE__, :invocation})

          Process.put({__MODULE__, :invocation}, %{
            current
            | handles: Map.delete(current.handles, handle)
          })
        end
      end
    end)
  end

  @doc false
  @spec with_planned_insertion(term(), HtmlTree.t(), (map() -> term())) :: term()
  def with_planned_insertion(handle, tree, consumer) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, invocation, entry} <- live_entry(handle, tree),
           :ok <- require_plan_kind(entry),
           :ok <-
             if(not entry.consumed,
               do: :ok,
               else: plan_error("template insertion plan has already been consumed")
             ) do
        entry = %{entry | consumed: true}

        Process.put({__MODULE__, :invocation}, %{
          invocation
          | handles: Map.put(invocation.handles, handle, entry)
        })

        consumer.(entry.context)
      end
    end)
  end

  @doc false
  @spec authorize_mutation(term(), HtmlTree.t()) :: :ok | {:error, {atom(), map()}}
  def authorize_mutation(handle, tree) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, _invocation, entry} <- live_entry(handle, tree),
           :ok <- require_plan_kind(entry),
           :ok <-
             if(entry.consumed,
               do: :ok,
               else:
                 plan_error(
                   "template insertion plan must be consumed before mutation authorization"
                 )
             ) do
        context = entry.context

        location =
          case context.target do
            nil ->
              "inert fallback, no earlier matching marker"

            target ->
              "matched earlier #{tree.nodes[target.parent][:tag] || tree.nodes[target.parent].type} range with #{length(target.removed)} original subtree roots"
          end

        case NativeElixirPdfUtilities.Validators.TemplateExecutionValidator.authorize(
               handle,
               tree,
               context
             ) do
          :not_owned ->
            Diagnostics.error(
              :html,
              :unsupported_html,
              "template-for #{inspect(context.name)} prepared #{location}; validated parser composition and source removal are blocked pending #{Enum.join(context.pending_checks, ", ")}. Remove the for attribute to keep an ordinary inert template; no original source has been discarded",
              module: __MODULE__,
              operation: :parse,
              source: context.source.text,
              line: context.source.line,
              column: context.source.column
            )

          result ->
            result
        end
      end
    end)
  end

  @doc false
  @spec with_candidate_input(term(), HtmlTree.t(), (map() -> term())) :: term()
  def with_candidate_input(handle, tree, consumer) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, invocation, entry} <- live_entry(handle, tree),
           :ok <- require_plan_kind(entry),
           :ok <-
             if(entry.consumed,
               do: :ok,
               else: plan_error("candidate capture requires a consumed insertion plan")
             ),
           {:ok, _, "", {:start, prepared}} <-
             HtmlParser.read_source_token(
               HtmlValidator.preprocess_html(entry.context.source.text),
               entry.context.tokenizer_context,
               entry.context.tokenizer_ancestors
             ),
           :ok <-
             (case HtmlValidator.validate_html_element_attributes(
                     "template",
                     Map.delete(prepared.attributes, "for")
                   ) do
                :ok ->
                  :ok

                {:error, {reason, diagnostic}} ->
                  {:error,
                   {reason,
                    Diagnostics.with_context(diagnostic,
                      source: entry.context.source.text,
                      line: entry.context.source.line,
                      column: entry.context.source.column
                    )}}
              end) do
        ancestors =
          entry.context.insertion.ancestors
          |> tl()
          |> Enum.map(fn id ->
            HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
            node = tree.nodes[id]
            if node.type == :element, do: Map.get(node, :tag), else: :fragment_tree
          end)
          |> Kernel.++([:fragment_tree, "template"])

        # The marker describes this exact consumed parse, never a caller option.
        key = {__MODULE__, :candidate_capture}
        previous = Process.get(key)

        Process.put(key, %{
          handle: handle,
          tree: tree,
          invocation: invocation.id,
          offset: entry.context.source_offset
        })

        try do
          consumer.(%{
            source: invocation.source,
            plan: entry.context,
            opening: prepared,
            prospective_ancestors: ancestors
          })
        after
          if previous, do: Process.put(key, previous), else: Process.delete(key)
        end
      end
    end)
  end

  @doc false
  @spec with_candidate_projection(term(), HtmlTree.t(), (reference() -> term())) :: term()
  def with_candidate_projection(handle, tree, consumer) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, invocation, plan} <- live_entry(handle, tree),
           :ok <- require_plan_kind(plan),
           {:ok, candidate} <- HtmlParser.capture_template_candidate(handle, tree),
           {:ok, context} <- TemplateCandidateValidator.prepare(candidate, tree, plan.context) do
        key = {__MODULE__, :invocation}
        current = Process.get(key)
        candidate_handle = make_ref()

        entry = %{
          invocation: invocation.id,
          kind: :candidate_projection,
          tree: tree,
          plan: handle,
          context: context,
          consumed: false,
          limits: Limits.effective(),
          counters: Process.get({HtmlValidator, :render_budget})
        }

        Process.put(key, %{current | handles: Map.put(current.handles, candidate_handle, entry)})
        frame_key = {__MODULE__, :candidate_projection}
        previous = Process.get(frame_key)
        Process.put(frame_key, candidate_handle)

        try do
          consumer.(candidate_handle)
        after
          current = Process.get(key)
          Process.put(key, %{current | handles: Map.delete(current.handles, candidate_handle)})
          if previous, do: Process.put(frame_key, previous), else: Process.delete(frame_key)
        end
      end
    end)
  end

  @doc false
  @spec with_candidate_evidence(term(), HtmlTree.t(), (map() -> term())) :: term()
  def with_candidate_evidence(handle, tree, consumer) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, invocation, entry} <- candidate_entry(handle, tree, false) do
        Process.put({__MODULE__, :invocation}, %{
          invocation
          | handles: Map.put(invocation.handles, handle, %{entry | consumed: true})
        })

        consumer.(entry.context)
      end
    end)
  end

  @doc false
  @spec with_consumed_candidate_frame(term(), HtmlTree.t(), (map() -> term())) :: term()
  def with_consumed_candidate_frame(handle, tree, consumer) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, _invocation, entry} <- candidate_entry(handle, tree, true),
           do: consumer.(entry.context)
    end)
  end

  defp candidate_entry(handle, tree, consumed?) do
    with {:ok, invocation, entry} <- live_entry(handle, tree),
         :ok <-
           if(entry.kind == :candidate_projection,
             do: :ok,
             else:
               plan_error("candidate evidence requires its invocation-issued projection handle")
           ),
         :ok <-
           if(entry.consumed == consumed?,
             do: :ok,
             else:
               plan_error(
                 if(consumed?,
                   do: "source projection requires a consumed candidate frame",
                   else: "candidate projection evidence has already been consumed"
                 )
               )
           ),
         :ok <-
           if(Process.get({__MODULE__, :candidate_projection}) == handle,
             do: :ok,
             else: plan_error("candidate evidence belongs to a different active capture")
           ),
         {:ok, _invocation, plan} <- live_entry(entry.plan, tree),
         :ok <- require_plan_kind(plan) do
      {:ok, invocation, entry}
    end
  end

  @doc false
  @spec with_renderer_policy_context(term(), HtmlTree.t(), term(), (reference() -> term())) ::
          term()
  def with_renderer_policy_context(handle, tree, opts, consumer) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, _invocation, plan} <- live_entry(handle, tree),
           :ok <- require_plan_kind(plan),
           :ok <-
             if(plan.consumed,
               do: :ok,
               else: plan_error("renderer preparation requires a consumed insertion plan")
             ) do
        issue_renderer_policy(
          handle,
          tree,
          nil,
          fn resource ->
            TemplateRendererContextValidator.prepare(tree, plan.context, opts, resource)
          end,
          consumer
        )
      end
    end)
  end

  @doc false
  @spec with_prospective_renderer_policy_context(term(), HtmlTree.t(), term(), (reference() ->
                                                                                  term())) ::
          term()
  def with_prospective_renderer_policy_context(handle, tree, opts, consumer) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, _invocation, captured} <- candidate_entry(handle, tree, false),
           {:ok, _invocation, plan} <- live_entry(captured.plan, tree) do
        with_candidate_evidence(handle, tree, fn _candidate ->
          issue_renderer_policy(
            captured.plan,
            tree,
            handle,
            fn resource ->
              TemplateRendererContextValidator.prepare_candidate(
                handle,
                tree,
                plan.context,
                opts,
                resource
              )
            end,
            consumer
          )
        end)
      end
    end)
  end

  defp issue_renderer_policy(handle, tree, candidate, prepare, consumer) do
    resource_context = ResourceContext.current()
    limits = Limits.effective()
    source = Process.get({__MODULE__, :invocation}).handles[handle].context.source

    with :ok <- ResourceValidator.validate_context(resource_context) do
      FontValidator.with_budget(fn ->
        font_identity = Process.get({FontValidator, :budget}).budget_id

        with {:ok, evidence} <- prepare.(resource_context),
             :ok <-
               if(
                 Limits.effective() == limits and ResourceContext.current() == resource_context and
                   Process.get({FontValidator, :budget}).budget_id == font_identity,
                 do: :ok,
                 else:
                   plan_error(
                     "renderer preparation configuration, scope or enclosing font budget changed during execution",
                     source
                   )
               ) do
          key = {__MODULE__, :invocation}
          invocation = Process.get(key)
          policy_handle = make_ref()

          entry = %{
            invocation: invocation.id,
            kind: :renderer_policy,
            tree: tree,
            plan: handle,
            candidate: candidate,
            context: evidence,
            consumed: false,
            limits: limits,
            resource_context: resource_context,
            font_budget: Process.get({FontValidator, :budget}),
            image_counters:
              for(index <- 1..3, do: :atomics.get(resource_context.image_budget, index)),
            counters: Process.get({HtmlValidator, :render_budget})
          }

          Process.put(key, %{
            invocation
            | handles: Map.put(invocation.handles, policy_handle, entry)
          })

          policy_key = {__MODULE__, :renderer_policy}
          previous_policy = Process.get(policy_key)
          Process.put(policy_key, policy_handle)

          try do
            consumer.(policy_handle)
          after
            current = Process.get(key)
            Process.put(key, %{current | handles: Map.delete(current.handles, policy_handle)})

            if previous_policy,
              do: Process.put(policy_key, previous_policy),
              else: Process.delete(policy_key)
          end
        end
      end)
    end
  end

  @doc false
  @spec with_renderer_policy_evidence(term(), HtmlTree.t(), (map() -> term())) :: term()
  def with_renderer_policy_evidence(handle, tree, consumer) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, invocation, entry} <- live_entry(handle, tree),
           :ok <-
             if(entry.kind == :renderer_policy,
               do: :ok,
               else: plan_error("renderer evidence requires its issued policy handle")
             ),
           :ok <-
             if(not entry.consumed,
               do: :ok,
               else: plan_error("renderer policy evidence has already been consumed")
             ),
           :ok <-
             if(Process.get({__MODULE__, :renderer_policy}) == handle,
               do: :ok,
               else:
                 plan_error("renderer policy evidence belongs to a different active preparation")
             ),
           {:ok, _current, plan} <- live_entry(entry.plan, tree),
           :ok <- require_plan_kind(plan),
           :ok <-
             if(entry.candidate,
               do: with_consumed_candidate_frame(entry.candidate, tree, fn _ -> :ok end),
               else: :ok
             ),
           :ok <- ResourceValidator.validate_context(ResourceContext.current()),
           :ok <- validate_policy_budgets(entry) do
        Process.put({__MODULE__, :invocation}, %{
          invocation
          | handles: Map.put(invocation.handles, handle, %{entry | consumed: true})
        })

        consumer.(entry.context)
      end
    end)
  end

  defp validate_policy_budgets(entry) do
    font_budget = Process.get({FontValidator, :budget})
    current = ResourceContext.current()

    font_valid? =
      is_map(font_budget) and Map.get(font_budget, :budget_id) == entry.font_budget.budget_id and
        MapSet.subset?(entry.font_budget.sources, font_budget.sources) and
        MapSet.subset?(entry.font_budget.faces, font_budget.faces) and
        Enum.all?(entry.font_budget, fn {key, value} ->
          not is_number(value) or Map.get(font_budget, key, 0) >= value
        end)

    image_valid? =
      current == entry.resource_context and
        Enum.all?(Enum.with_index(entry.image_counters, 1), fn {used, index} ->
          :atomics.get(current.image_budget, index) >= used
        end)

    if font_valid? and image_valid?,
      do: :ok,
      else: plan_error("renderer policy context or retained font/image budgets changed")
  end

  defp require_plan_kind(entry) do
    if entry.kind == :insertion_plan,
      do: :ok,
      else: plan_error("operation requires its issued insertion plan")
  end

  defp validate_opening_token(name, origin) do
    normalized = HtmlValidator.preprocess_html(origin.source.text)

    case HtmlParser.read_source_token(normalized, origin.tokenizer_context, origin.ancestors) do
      {:ok, ^normalized, "",
       {:start, %{tag: "template", namespace: :html, attributes: %{"for" => ^name}}}} ->
        :ok

      _ ->
        plan_error(
          "template insertion planning requires the exact original HTML template opening token and for value"
        )
    end
  end

  defp require_invocation(invocation, tree) do
    if is_map(invocation) and tree.nodes[tree.root].source.text == invocation.source,
      do: :ok,
      else:
        plan_error(
          "template insertion planning requires the current parser invocation and exact original source"
        )
  end

  defp live_entry(handle, tree) do
    HtmlValidator.reserve_render_resource(
      :max_html_tree_work,
      map_size(tree.nodes) * (Limits.get(:max_html_depth) + 1) +
        byte_size(tree.nodes[tree.root].source.text),
      :html
    )

    invocation = Process.get({__MODULE__, :invocation})
    entry = if invocation && is_reference(handle), do: Map.get(invocation.handles, handle)
    budget = Process.get({HtmlValidator, :render_budget})

    counters_valid? =
      is_map(budget) and is_map(entry) and
        Enum.all?(entry.counters, fn {limit, used} ->
          limit == :max_html_depth or Map.get(budget, limit, 0) >= used
        end)

    if entry && entry.invocation == invocation.id && entry.tree == tree && counters_valid? &&
         entry.limits == Limits.effective(),
       do: {:ok, invocation, entry},
       else:
         plan_error(
           "template insertion plan is expired, forged, belongs to another invocation, or its original tree/budget/configuration changed"
         )
  end

  defp insertion_context(tree, parent, fallback) do
    ancestors = ancestor_ids(tree, parent, [])
    node = tree.nodes[parent]
    scope = node.tree_scope_id

    tokenizer_context =
      case node do
        %{type: :element, tag: tag} ->
          if Map.get(node, :namespace, :html) == :html,
            do: tag,
            else: {:foreign, node.namespace, tag, node.attributes}

        _ ->
          fallback
      end

    %{
      parent: parent,
      ancestors: ancestors,
      document: node.document_id,
      scope: scope,
      namespace: Map.get(node, :namespace, :html),
      context: tokenizer_context,
      siblings: Enum.reverse(Map.get(node, :children, [])),
      stylesheets: style_ids(tree, scope),
      stylesheet_selection: :requires_renderer_media_context
    }
  end

  defp ancestor_ids(tree, id, ancestors) do
    HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
    node = tree.nodes[id]
    ancestors = [id | ancestors]

    if node.parent_id,
      do: ancestor_ids(tree, node.parent_id, ancestors),
      else: Enum.reverse(ancestors)
  end

  defp style_ids(tree, id) do
    HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
    node = tree.nodes[id]

    own =
      if node[:tag] == "style" and HtmlMetadataValidator.stylesheet_type?(node.attributes),
        do: [id],
        else: []

    children =
      if node[:tag] == "template", do: [], else: Map.get(node, :children, []) |> Enum.reverse()

    own ++ Enum.flat_map(children, &style_ids(tree, &1))
  end

  defp plan_error(message, source \\ nil) do
    context =
      if source, do: [source: source.text, line: source.line, column: source.column], else: []

    Diagnostics.error(
      :html,
      :invalid_document,
      message,
      [module: __MODULE__, operation: :parse] ++ context
    )
  end
end
