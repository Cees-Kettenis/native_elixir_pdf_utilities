defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlComposition do
  @moduledoc """
  Builds an initial composed projection from authored declarative shadow trees.

  The logical document is retained unchanged. Assigned nodes keep their original
  identity, tree scope, parent and source. Projection ancestry is recorded in
  `composed_parent_id`; it never grants resource permissions. Manual roots have
  no initial assignments, since no script or `assign()` call has executed.
  """

  alias NativeElixirPdfUtilities.Validators.{HtmlCompositionValidator, HtmlValidator}

  @doc "Returns the unchanged logical tree and its initial shadow/slot rendering projection."
  @spec compose(term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def compose(dom) do
    HtmlValidator.with_render_budget(fn ->
      with :ok <- HtmlCompositionValidator.validate_input(dom), do: compose_validated(dom)
    end)
  end

  @doc false
  @spec compose_validated(map()) :: {:ok, map()} | {:error, {atom(), map()}}
  def compose_validated(dom) do
    # HtmlValidator has already accepted the authored tree. This execution
    # entry avoids a second full identity/semantic pass during normal styling.
    with {:ok, children, state} <-
           if(HtmlCompositionValidator.composition_required?(dom.children),
             do:
               compose_nodes(
                 dom.children,
                 collect_initial_roots(dom.children, %{roots: %{}, assignments: %{}}, 0),
                 nil,
                 0
               ),
             else: {:ok, dom.children, %{roots: %{}, assignments: %{}}}
           ) do
      {:ok,
       %{
         logical: dom,
         composed: %{dom | children: children},
         roots: state.roots,
         assignments: state.assignments
       }}
    end
  end

  # Attachment precedes flat projection. Unassigned light descendants still
  # have real shadow roots and logical form/ID associations of their own.
  defp collect_initial_roots(nodes, state, depth) do
    HtmlValidator.check_render_resource(:max_html_depth, depth, :html)

    Enum.reduce(nodes, state, fn node, state ->
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)

      case node do
        %{type: :element, tag: "template"} ->
          state

        %{type: :element, children: children} ->
          case HtmlCompositionValidator.initial_shadow_root(node) do
            nil ->
              collect_initial_roots(children, state, depth + 1)

            {template, configuration} ->
              root =
                Map.merge(template.content, configuration)
                |> Map.put(:host_id, Map.get(node, :node_id))
                |> Map.put(:host_tree_scope_id, Map.get(node, :tree_scope_id))
                |> Map.put(:depth, depth + 1)
                |> Map.put(:order, map_size(state.roots))

              state = %{state | roots: Map.put(state.roots, Map.get(root, :node_id), root)}

              state =
                collect_initial_roots(Enum.reject(children, &(&1 == template)), state, depth + 1)

              collect_initial_roots(root.children, state, depth + 1)
          end

        _ ->
          state
      end
    end)
  end

  # Each branch is a different rendering tree. Ordinary inert template content
  # is never visited here, and nested hosts start their own assignment search.
  @doc false
  @spec compose_generated_roots([map()], map()) ::
          {:ok, [map()], map()} | {:error, {atom(), map()}}
  def compose_generated_roots(nodes, state), do: compose_nodes(nodes, state, nil, 0, :generated)

  defp compose_nodes(nodes, state, parent, depth, mode \\ :authored) do
    HtmlValidator.check_render_resource(:max_html_depth, depth, :html)

    Enum.reduce_while(nodes, {:ok, [], state}, fn node, {:ok, output, state} ->
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
      node = Map.put(node, :composed_parent_id, parent)

      result =
        case node do
          %{type: :element, children: children} ->
            shadow =
              case {mode, Map.get(state, :generated_hosts, %{})[Map.get(node, :node_id)]} do
                {:generated, root} when is_map(root) ->
                  {:generated, root,
                   Map.take(root, [
                     :mode,
                     :slot_assignment,
                     :delegates_focus,
                     :clonable,
                     :serializable
                   ])}

                {:generated, _} ->
                  nil

                {:authored, _} ->
                  case HtmlCompositionValidator.initial_shadow_root(node) do
                    nil -> nil
                    {child, configuration} -> {:authored, child.content, configuration, child}
                  end
              end

            case shadow do
              nil ->
                with {:ok, children, state} <-
                       compose_nodes(children, state, Map.get(node, :node_id), depth + 1, mode) do
                  {:ok, %{node | children: children}, state}
                end

              shadow ->
                {root, configuration, light} =
                  case shadow do
                    {:authored, root, configuration, template} ->
                      {root, configuration, Enum.reject(children, &(&1 == template))}

                    {:generated, root, configuration} ->
                      {root, configuration, children}
                  end

                host_id = Map.get(node, :node_id)
                root_id = Map.get(root, :node_id)

                root = Map.get(state.roots, root_id, root)

                root =
                  Map.merge(root, configuration)
                  |> Map.put(:host_id, host_id)
                  |> Map.put(:host_tree_scope_id, Map.get(node, :tree_scope_id))
                  |> Map.put(:depth, depth + 1)
                  |> Map.put_new(:order, map_size(state.roots))

                state = %{state | roots: Map.put(state.roots, root_id, root)}

                state =
                  if mode == :generated,
                    do: Map.update!(state, :generated_hosts, &Map.delete(&1, host_id)),
                    else: state

                with {distributed, state, _claimed} <-
                       distribute(
                         root.children,
                         light,
                         configuration.slot_assignment,
                         state,
                         MapSet.new(),
                         root_id,
                         host_id,
                         depth + 1
                       ),
                     :ok <- HtmlCompositionValidator.validate_active_children(distributed),
                     {:ok, children, state} <-
                       compose_nodes(distributed, state, host_id, depth + 1, mode) do
                  {:ok, node |> Map.put(:children, children) |> Map.put(:shadow_root_id, root_id),
                   state}
                end
            end

          _ ->
            {:ok, node, state}
        end

      case result do
        {:ok, node, state} -> {:cont, {:ok, [node | output], state}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, output, state} -> {:ok, Enum.reverse(output), state}
      error -> error
    end
  end

  defp distribute(nodes, light, assignment, state, claimed, scope, host, depth) do
    HtmlValidator.check_render_resource(:max_html_depth, depth, :html)

    Enum.reduce(nodes, {[], state, claimed}, fn node, {output, state, claimed} ->
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)

      {node, state, claimed} =
        case {HtmlCompositionValidator.slot_element?(node), node} do
          {_, %{tree_scope_id: node_scope}} when node_scope != scope ->
            {node, state, claimed}

          {true, %{type: :element, tag: "slot", attributes: attributes, children: fallback}} ->
            name = Map.get(attributes, "name", "")
            first? = not MapSet.member?(claimed, name)
            claimed = MapSet.put(claimed, name)

            assigned =
              if assignment == :named and first? do
                HtmlValidator.reserve_render_resource(:max_html_tree_work, length(light), :html)

                Enum.filter(light, fn child ->
                  case child do
                    %{type: :text} ->
                      name == ""

                    %{type: :element, attributes: attributes} ->
                      Map.get(attributes, "slot", "") == name

                    _ ->
                      false
                  end
                end)
              else
                []
              end

            children =
              if assigned == [],
                do: fallback,
                else:
                  Enum.map(assigned, fn child ->
                    child
                    |> Map.put(:assigned_slot_id, Map.get(node, :node_id))
                    |> Map.put(:assigned_shadow_scope_id, scope)
                    |> Map.put(:assigned_shadow_host_id, host)
                    |> Map.put(:assigned_slot, %{node | children: []})
                  end)

            state = %{
              state
              | assignments:
                  Map.put(
                    state.assignments,
                    Map.get(node, :node_id),
                    Enum.map(assigned, &Map.get(&1, :node_id))
                  )
            }

            # Assigned light subtrees belong to their authored scope, and their
            # own slots are not slots of the receiving shadow tree.
            {children, state, claimed} =
              if assigned == [],
                do:
                  distribute(children, light, assignment, state, claimed, scope, host, depth + 1),
                else: {children, state, claimed}

            {%{node | children: children}, state, claimed}

          {_, %{type: :element, tag: "template"}} ->
            {node, state, claimed}

          {_, %{type: :element, children: children}} ->
            {children, state, claimed} =
              distribute(children, light, assignment, state, claimed, scope, host, depth + 1)

            {%{node | children: children}, state, claimed}

          _ ->
            {node, state, claimed}
        end

      node =
        if not Map.has_key?(node, :tree_scope_id) or node.tree_scope_id == scope,
          do: node |> Map.put(:shadow_scope_id, scope) |> Map.put(:shadow_host_id, host),
          else: node

      {[node | output], state, claimed}
    end)
    |> then(fn {output, state, claimed} -> {Enum.reverse(output), state, claimed} end)
  end
end
