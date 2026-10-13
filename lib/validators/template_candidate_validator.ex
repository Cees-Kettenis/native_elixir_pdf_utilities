defmodule NativeElixirPdfUtilities.Validators.TemplateCandidateValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlTree}

  alias NativeElixirPdfUtilities.Validators.{
    HtmlCssSourceValidator,
    HtmlTreeValidator,
    HtmlValidator
  }

  @identity [
    :node_id,
    :document_id,
    :tree_scope_id,
    :parent_id,
    :source,
    :tag,
    :attributes,
    :namespace
  ]

  @doc false
  @spec prepare_original(HtmlTree.t(), non_neg_integer(), map()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_original(tree, finish, plan) do
    with {:ok, provenance} <-
           HtmlCssSourceValidator.prepare_prefix(HtmlTree.fetch(tree, tree.root), finish),
         :ok <-
           if(plan.target,
             do: :ok,
             else:
               candidate_error(
                 "candidate stays inert because no earlier target exists; prospective preparation remains unresolved",
                 plan.source
               )
           ) do
      {:ok, provenance}
    end
  end

  @doc false
  @spec prepare(map(), HtmlTree.t(), map()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare(candidate, prefix, plan) do
    original = candidate.original
    projected = candidate.projected

    HtmlValidator.reserve_render_resource(
      :max_html_tree_work,
      map_size(original.nodes) + map_size(projected.nodes),
      :html
    )

    authored =
      Enum.reduce(original.nodes, %{}, fn {id, node}, indexed ->
        source = node.source

        if not Map.has_key?(prefix.nodes, id) and source.text != "" do
          Map.put(indexed, {source.line, source.column, node.type}, node)
        else
          indexed
        end
      end)

    remapping =
      Enum.reduce_while(projected.nodes, {:ok, %{}, %{}}, fn {id, node}, {:ok, ids, bindings} ->
        source = node.source

        if Map.has_key?(prefix.nodes, id) or source.text == "" do
          {:cont, {:ok, Map.put(ids, id, id), bindings}}
        else
          original_node = Map.get(authored, {source.line, source.column, node.type})

          HtmlValidator.reserve_render_resource(
            :max_html_tree_work,
            byte_size(source.text),
            :html
          )

          if original_node && original_node.source == source do
            {:cont,
             {:ok, Map.put(ids, id, original_node.node_id),
              Map.put(bindings, original_node.node_id, Map.take(original_node, @identity))}}
          else
            {:halt,
             candidate_error(
               "prospective tokenizer context cannot preserve this exact authored token identity; original source remains retained",
               source
             )}
          end
        end
      end)

    with {:ok, ids, authored_bindings} <- remapping do
      nodes =
        Map.new(projected.nodes, fn {id, node} ->
          HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)

          node =
            node
            |> Map.delete(:projection)
            |> Map.put(:node_id, Map.fetch!(ids, id))
            |> Map.put(:parent_id, Map.get(ids, node.parent_id))
            |> Map.put(:tree_scope_id, Map.fetch!(ids, node.tree_scope_id))

          node =
            if Map.has_key?(node, :children) do
              HtmlValidator.reserve_render_resource(
                :max_html_tree_work,
                length(node.children),
                :html
              )

              Map.update!(node, :children, &Enum.map(&1, fn id -> Map.fetch!(ids, id) end))
            else
              node
            end

          node =
            if node[:content], do: Map.update!(node, :content, &Map.fetch!(ids, &1)), else: node

          {node.node_id, node}
        end)

      HtmlValidator.reserve_render_resource(:max_html_tree_work, map_size(projected.hosts), :html)

      hosts =
        Map.new(projected.hosts, fn {fragment, host} ->
          {Map.fetch!(ids, fragment), Map.fetch!(ids, host)}
        end)

      projected = %{projected | nodes: nodes, hosts: hosts}
      dom = HtmlTree.fetch(projected, projected.root)

      with :ok <- HtmlTreeValidator.validate_transport(dom),
           {:ok, _controls} <-
             HtmlValidator.prepare_style_input(dom, [], Font.normalize_options([])) do
        bindings =
          Map.new(authored_bindings, fn {id, original_binding} ->
            HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)

            {id,
             %{original: original_binding, prospective: Map.take(projected.nodes[id], @identity)}}
          end)

        {:ok,
         %{
           original: original,
           prospective: projected,
           original_host: candidate.host,
           original_fragment: candidate.fragment,
           inserted_roots: Enum.map(candidate.inserted_roots, &Map.fetch!(ids, &1)),
           source_bindings: bindings,
           captured_finish: candidate.finish,
           authored_prefix: candidate.provenance,
           original_scope: plan.original,
           prospective_scope: plan.insertion,
           content_model_policy: :validated_native_parse_in_observed_target_context,
           source_policy: :requires_complete_original_source_validation,
           style_policy: :requires_actual_prospective_cascade_preparation,
           font_policy: :requires_actual_prospective_glyph_preparation,
           resource_policy: :requires_actual_prospective_authorized_outcomes,
           pending_checks: plan.pending_checks
         }}
      end
    end
  end

  defp candidate_error(message, source) do
    Diagnostics.error(:html, :unsupported_html, message,
      module: __MODULE__,
      operation: :template_candidate_projection,
      source: source.text,
      line: source.line,
      column: source.column
    )
  end
end
