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
           nodes = HtmlFormValidator.prepare_initial_nodes(composition.composed.children),
           {:ok, nodes} <- populate_validated(nodes, dom) do
        {:ok, %{composition | composed: %{composition.composed | children: nodes}}}
      end
    end)
  end

  @doc false
  @spec populate_validated([map()], map()) :: {:ok, [map()]} | {:error, {atom(), map()}}
  def populate_validated(nodes, logical) do
    if HtmlSelectedContentValidator.required?(nodes) do
      index = HtmlSelectedContentValidator.logical_index(logical)
      :ok = HtmlSelectedContentValidator.reserve_authored_nodes(index)
      prepared = HtmlSelectedContentValidator.prepared_index(nodes)
      nodes = populate(nodes, index, prepared, [], 0)
      # Recompute control/selector metadata for the actual clone placement.
      # This keeps copied noninteractive hidden controls and ID associations
      # under the same form authority as their authored counterparts.
      {:ok, HtmlFormValidator.prepare_initial_nodes(nodes)}
    else
      {:ok, nodes}
    end
  end

  defp populate(nodes, logical, prepared, ancestors, depth) do
    HtmlValidator.check_render_resource(:max_html_depth, depth, :html)

    Enum.map(nodes, fn node ->
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)

      case node do
        %{type: :element, tag: "selectedcontent", children: children} ->
          plan =
            HtmlSelectedContentValidator.selected_content_plan(node, ancestors, logical, prepared)

          children =
            case plan do
              %{update: true, option: nil} ->
                []

              %{update: true, option: option} ->
                clone_children(option.children, node, node._document_order, depth + 1)

              _ ->
                children
            end

          node
          |> Map.put(:children, children)
          |> Map.put(:_selected_content_disabled, plan.disabled)
          |> Map.put(
            :_selected_content_option_id,
            if(plan.option, do: Map.get(plan.option, :node_id))
          )

        %{type: :element, children: children} ->
          %{node | children: populate(children, logical, prepared, [node | ancestors], depth + 1)}

        _ ->
          node
      end
    end)
  end

  # New logical identities describe actual clones. The source span and original
  # node link describe their provenance, independently of their new placement.
  defp clone_children(children, owner, path, depth) do
    HtmlValidator.check_render_resource(:max_html_depth, depth, :html)

    children
    |> Enum.with_index()
    |> Enum.map(fn {source, index} ->
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
          :shadow_host_id
        ])

      node =
        node
        |> Map.put(:cloned_from_node_id, Map.get(source, :node_id))
        |> Map.put(:clone_id, id)
        |> Map.put(:composed_parent_id, Map.get(owner, :node_id))

      node =
        if Map.has_key?(owner, :document_id) do
          node
          |> Map.put(:node_id, id)
          |> Map.put(:document_id, owner.document_id)
          |> Map.put(:tree_scope_id, owner.tree_scope_id)
          |> Map.put(:parent_id, owner.node_id)
        else
          Map.drop(node, [:node_id, :document_id, :tree_scope_id, :parent_id])
        end

      node = Map.merge(node, Map.take(owner, [:shadow_scope_id, :shadow_host_id]))

      case node do
        %{type: :element, children: nested} ->
          node =
            node
            |> Map.put(:_selector_id, make_ref())
            |> Map.put(:_document_order, path ++ [index])

          children = clone_children(nested, node, node._document_order, depth + 1)
          node = %{node | children: children}

          node =
            case Map.get(source, :content) do
              %{type: :document_fragment, children: children} = original ->
                HtmlValidator.reserve_render_resource(:max_html_nodes, 1, :html)
                HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
                fragment_id = make_ref()

                fragment =
                  original
                  |> Map.put(:cloned_from_node_id, Map.get(original, :node_id))
                  |> Map.put(:clone_id, fragment_id)

                fragment =
                  if Map.has_key?(node, :document_id),
                    do:
                      Map.merge(fragment, %{
                        node_id: fragment_id,
                        document_id: node.document_id,
                        tree_scope_id: fragment_id,
                        parent_id: nil
                      }),
                    else: Map.drop(fragment, [:node_id, :document_id, :tree_scope_id, :parent_id])

                fragment = %{
                  fragment
                  | children: clone_children(children, fragment, node._document_order, depth + 1)
                }

                Map.put(node, :content, fragment)

              _ ->
                node
            end

          if node.tag == "selectedcontent",
            do: Map.put(node, :_selected_content_disabled, true),
            else: node

        _ ->
          node
      end
    end)
  end
end
