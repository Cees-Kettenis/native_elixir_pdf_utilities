defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlSelectedContent do
  @moduledoc """
  Clones actual selected option child subtrees into initial selected-content nodes.

  Selectedness comes from `HtmlFormValidator`. Authored source and logical trees
  remain unchanged. Clones have new identities and explicit original-node links;
  they inherit the destination document, tree scope and placement, not the
  option's style or any additional resource authorization.
  """

  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlComposition

  alias NativeElixirPdfUtilities.Validators.{
    HtmlCompositionValidator,
    HtmlFormValidator,
    HtmlSelectedContentValidator,
    HtmlValidator
  }

  @doc "Builds initial selected-content clones from a validated authored document."
  @spec compose(term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def compose(dom) do
    HtmlValidator.with_render_budget(fn ->
      with :ok <- HtmlCompositionValidator.validate_input(dom),
           {:ok, composition} <- HtmlComposition.compose_validated(dom),
           nodes =
             HtmlFormValidator.prepare_composed_initial_nodes(
               dom,
               composition.roots,
               composition.composed.children,
               %{}
             ),
           {:ok, composition, nodes} <- populate_composition_validated(composition, nodes, dom) do
        {:ok, %{composition | composed: %{composition.composed | children: nodes}}}
      end
    end)
  end

  @doc false
  @spec populate_validated([map()], map()) :: {:ok, [map()]} | {:error, {atom(), map()}}
  def populate_validated(nodes, logical) do
    composition = %{roots: %{}, assignments: %{}}

    with {:ok, _composition, nodes} <- populate_composition_validated(composition, nodes, logical),
         do: {:ok, nodes}
  end

  @doc false
  @spec populate_composition_validated(map(), [map()], map()) ::
          {:ok, map(), [map()]} | {:error, {atom(), map()}}
  def populate_composition_validated(composition, nodes, logical) do
    if HtmlSelectedContentValidator.required?(nodes) do
      index = HtmlSelectedContentValidator.logical_index(logical)
      :ok = HtmlSelectedContentValidator.reserve_authored_nodes(index)
      prepared = HtmlSelectedContentValidator.prepared_index(nodes)

      state =
        Map.merge(composition, %{
          generated_hosts: %{},
          clone_logical_nodes: %{},
          pending_selects: MapSet.new()
        })

      {nodes, state} = populate(nodes, index, prepared, [], 0, state, :all)

      with {:ok, nodes, state} <- HtmlComposition.compose_generated_roots(nodes, state),
           {:ok, nodes, state} <- complete_pending_selects(nodes, logical, index, state) do
        {:ok, Map.drop(state, [:generated_hosts, :pending_selects]), nodes}
      end
    else
      {:ok, composition, nodes}
    end
  end

  # Cloned shadow trees can contain their own select controls. Their initial
  # selected-content updates run after real attachment and form preparation.
  defp complete_pending_selects(nodes, document, logical, state) do
    nodes =
      HtmlFormValidator.prepare_composed_initial_nodes(
        document,
        state.roots,
        nodes,
        state.clone_logical_nodes
      )

    if MapSet.size(state.pending_selects) == 0 do
      {:ok, nodes, state}
    else
      pending = state.pending_selects
      state = %{state | pending_selects: MapSet.new()}
      index = Map.merge(logical, state.clone_logical_nodes)
      prepared = HtmlSelectedContentValidator.prepared_index(nodes)
      {nodes, state} = populate(nodes, index, prepared, [], 0, state, pending)

      with {:ok, nodes, state} <- HtmlComposition.compose_generated_roots(nodes, state),
           do: complete_pending_selects(nodes, document, logical, state)
    end
  end

  defp populate(nodes, logical, prepared, ancestors, depth, state, targets) do
    HtmlValidator.check_render_resource(:max_html_depth, depth, :html)

    Enum.map_reduce(nodes, state, fn node, state ->
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)

      case node do
        %{type: :element, tag: "selectedcontent", children: children} ->
          plan =
            HtmlSelectedContentValidator.selected_content_plan(node, ancestors, logical, prepared)

          update? = plan.update and (targets == :all or MapSet.member?(targets, plan.select_id))

          {children, state} =
            if update? do
              if plan.option,
                do:
                  clone_children(
                    plan.option.children,
                    node,
                    node._document_order,
                    depth + 1,
                    logical,
                    state,
                    false
                  ),
                else: {[], state}
            else
              if targets == :all,
                do: {children, state},
                else:
                  populate(
                    children,
                    logical,
                    prepared,
                    [node | ancestors],
                    depth + 1,
                    state,
                    targets
                  )
            end

          node =
            node
            |> Map.put(:children, children)
            |> Map.put(:_selected_content_disabled, plan.disabled)
            |> Map.put(
              :_selected_content_option_id,
              if(plan.option, do: Map.get(plan.option, :node_id))
            )

          state =
            if is_reference(Map.get(node, :node_id)) and
                 (update? or Map.has_key?(state.clone_logical_nodes, node.node_id)),
               do: %{
                 state
                 | clone_logical_nodes:
                     Map.put(
                       state.clone_logical_nodes,
                       Map.get(node, :node_id, Map.get(node, :clone_id)),
                       node
                     )
               },
               else: state

          {node, state}

        %{type: :element, children: children} ->
          {children, state} =
            populate(children, logical, prepared, [node | ancestors], depth + 1, state, targets)

          {%{node | children: children}, state}

        _ ->
          {node, state}
      end
    end)
  end

  # Each created node has a new identity. Its payload and source link are checked
  # against the actual original before either logical registration or styling.
  defp clone_children(children, owner, path, depth, logical, state, inert?) do
    HtmlValidator.check_render_resource(:max_html_depth, depth, :html)

    children
    |> Enum.with_index()
    |> Enum.map_reduce(state, fn {source, index}, state ->
      node = clone_identity(source, owner, false)

      case node do
        %{type: :element} ->
          node =
            node
            |> Map.put(:_selector_id, make_ref())
            |> Map.put(:_document_order, path ++ [index])

          plan = HtmlSelectedContentValidator.clone_shadow_plan(source, logical, inert?)

          {children, state} =
            clone_children(
              plan.children,
              node,
              node._document_order,
              depth + 1,
              logical,
              state,
              inert?
            )

          node = %{node | children: children}

          {node, state} =
            case Map.get(source, :content) do
              %{type: :document_fragment} = fragment ->
                {fragment, state} =
                  clone_fragment(
                    fragment,
                    node,
                    node._document_order,
                    depth + 1,
                    logical,
                    state,
                    true
                  )

                {Map.put(node, :content, fragment), state}

              _ ->
                {node, state}
            end

          state = %{
            state
            | clone_logical_nodes:
                Map.put(
                  state.clone_logical_nodes,
                  Map.get(node, :node_id, Map.get(node, :clone_id)),
                  node
                )
          }

          state =
            if node.tag == "select",
              do: %{
                state
                | pending_selects:
                    MapSet.put(
                      state.pending_selects,
                      Map.get(node, :node_id, Map.get(node, :clone_id))
                    )
              },
              else: state

          {node, state} =
            if plan.root do
              root = Map.put(plan.root, :shadow_host_id, node.node_id)

              {root, state} =
                clone_fragment(root, node, node._document_order, depth + 1, logical, state, false)

              root =
                root
                |> Map.put(:host_id, Map.get(node, :node_id, Map.get(node, :clone_id)))
                |> Map.put(:host_tree_scope_id, Map.get(node, :tree_scope_id))
                |> Map.put(:order, map_size(state.roots))

              state = %{
                state
                | roots:
                    Map.put(state.roots, Map.get(root, :node_id, Map.get(root, :clone_id)), root),
                  generated_hosts:
                    Map.put(
                      state.generated_hosts,
                      Map.get(node, :node_id, Map.get(node, :clone_id)),
                      root
                    )
              }

              {node, state}
            else
              {node, state}
            end

          {node, state}

        _ ->
          {node,
           %{
             state
             | clone_logical_nodes:
                 Map.put(
                   state.clone_logical_nodes,
                   Map.get(node, :node_id, Map.get(node, :clone_id)),
                   node
                 )
           }}
      end
    end)
  end

  # Both ordinary template content and clonable shadow roots clone a real
  # fragment. Inert content retains its scope without executing declarations.
  defp clone_fragment(source, owner, path, depth, logical, state, inert?) do
    fragment = clone_identity(source, owner, true)

    fragment =
      if inert?,
        do: fragment,
        else:
          fragment
          |> Map.put(:shadow_scope_id, Map.get(fragment, :node_id, Map.get(fragment, :clone_id)))
          |> Map.put(:shadow_host_id, Map.get(owner, :node_id, Map.get(owner, :clone_id)))

    {children, state} =
      clone_children(source.children, fragment, path, depth, logical, state, inert?)

    fragment = %{fragment | children: children}

    {fragment,
     %{
       state
       | clone_logical_nodes:
           Map.put(
             state.clone_logical_nodes,
             Map.get(fragment, :node_id, Map.get(fragment, :clone_id)),
             fragment
           )
     }}
  end

  defp clone_identity(source, owner, fragment?) do
    HtmlValidator.reserve_render_resource(:max_html_nodes, 1, :html)
    HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
    id = make_ref()

    node =
      source
      |> Map.drop([
        :assigned_slot_id,
        :assigned_slot,
        :assigned_shadow_scope_id,
        :assigned_shadow_host_id,
        :_initial_form_state,
        :_initial_selected,
        :_initial_option_disabled,
        :_initial_option_label,
        :shadow_scope_id,
        :shadow_host_id,
        :shadow_root_id
      ])
      |> Map.put(:cloned_from_node_id, Map.get(source, :node_id))
      |> Map.put(:clone_id, id)
      |> Map.put(:node_id, id)
      |> Map.put(:composed_parent_id, if(fragment?, do: nil, else: Map.get(owner, :node_id)))

    node =
      if Map.has_key?(owner, :document_id) do
        node
        |> Map.put(:document_id, owner.document_id)
        |> Map.put(:tree_scope_id, if(fragment?, do: id, else: owner.tree_scope_id))
        |> Map.put(:parent_id, if(fragment?, do: nil, else: owner.node_id))
      else
        Map.drop(node, [:node_id, :document_id, :tree_scope_id, :parent_id])
      end

    node = Map.merge(node, Map.take(owner, [:shadow_scope_id, :shadow_host_id]))

    case HtmlSelectedContentValidator.validate_clone_origin(node, source) do
      :ok -> node
      {:error, _} = error -> throw({:render_resource_limit, error})
    end
  end
end
