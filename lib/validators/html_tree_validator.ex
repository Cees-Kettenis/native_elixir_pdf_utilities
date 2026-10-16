defmodule NativeElixirPdfUtilities.Validators.HtmlTreeValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlTree
  alias NativeElixirPdfUtilities.Validators.ProcessingInstructionValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @identity_keys [:node_id, :document_id, :tree_scope_id, :parent_id]

  @doc false
  @spec validate_node_metadata(term()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_node_metadata(node) do
    HtmlValidator.with_render_budget(fn ->
      canonical? =
        is_map(node) and
          Enum.any?([:document_id, :tree_scope_id, :parent_id], &Map.has_key?(node, &1))

      valid? =
        is_map(node) and not Map.has_key?(node, :projection) and
          (not canonical? or
             (Enum.all?(@identity_keys, &Map.has_key?(node, &1)) and
                is_reference(node.node_id) and is_reference(node.document_id) and
                is_reference(node.tree_scope_id) and
                (is_nil(node.parent_id) or is_reference(node.parent_id))))

      valid? = valid? and (not canonical? or Map.has_key?(node, :source))

      valid? =
        valid? and
          case {canonical?, Map.get(node, :type), Map.fetch(node, :source)} do
            {false, :processing_instruction, {:ok, %{text: text}}} when is_binary(text) ->
              HtmlValidator.reserve_render_resource(:max_html_tree_work, byte_size(text), :html)
              true

            {false, :processing_instruction, _} ->
              true

            {_, _, :error} ->
              true

            {_, _, {:ok, %{text: text, line: line, column: column} = source}}
            when is_binary(text) and is_integer(line) and line > 0 and is_integer(column) and
                   column > 0 ->
              HtmlValidator.reserve_render_resource(:max_html_tree_work, byte_size(text), :html)
              map_size(source) == 3 and String.valid?(text, :fast_ascii)

            _ ->
              false
          end

      if valid?,
        do: HtmlValidator.validate_attribute_order(node),
        else:
          metadata_error(
            node,
            "canonical HTML identity requires reference node/document/scope IDs, a parent reference or nil, and original UTF-8 source text with positive line and column; private tree projections cannot be supplied"
          )
    end)
  end

  @doc false
  @spec validate_transport(term()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_transport(dom) do
    HtmlValidator.with_render_budget(fn ->
      case dom do
        %{type: type, children: children}
        when type in [:document, :document_fragment] and is_list(children) ->
          canonical? = Map.has_key?(dom, :document_id)
          document = Map.get(dom, :document_id)
          scope = Map.get(dom, :node_id)

          case walk(dom, canonical?, document, scope, nil, MapSet.new(), 0) do
            {:ok, _seen} -> :ok
            error -> error
          end

        _ ->
          :ok
      end
    end)
  end

  # This result is an internal plan over the parser's live address table. It is
  # neither caller transport, permission nor evidence of removed-source policy.
  @type content_patch_target :: %{
          scope: reference(),
          parent: reference(),
          start: reference(),
          end: reference() | nil,
          removed: [reference()]
        }

  @doc false
  @spec prepare_content_patch(HtmlTree.t(), reference(), String.t()) ::
          {:ok, content_patch_target() | nil} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_content_patch(tree, insertion_parent, name) do
    HtmlValidator.with_render_budget(fn ->
      if name == "" do
        {:ok, nil}
      else
        HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
        owner = Map.fetch!(tree.nodes, insertion_parent)

        scope =
          case {Map.get(owner, :namespace, :html), owner} do
            {:html, %{type: :element, tag: "template", content: content}} ->
              content

            {:html, %{type: :element, tag: "body", parent_id: parent}} when not is_nil(parent) ->
              HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)

              case Map.fetch!(tree.nodes, parent) do
                %{type: :element, tag: "html"} -> parent
                _ -> insertion_parent
              end

            _ ->
              insertion_parent
          end

        with {:ok, start} <- find_patch_marker(tree, scope, name) do
          case start do
            nil ->
              {:ok, nil}

            start ->
              marker = Map.fetch!(tree.nodes, start)
              parent = marker.parent_id

              {end_marker, removed} =
                if marker.target == "marker" do
                  {start, []}
                else
                  patch_range(tree, parent, start)
                end

              {:ok,
               %{scope: scope, parent: parent, start: start, end: end_marker, removed: removed}}
          end
        end
      end
    end)
  end

  @doc false
  @spec validate_patch_boundaries(HtmlTree.t(), map(), boolean()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_patch_boundaries(tree, target, opening?) do
    HtmlValidator.with_render_budget(fn ->
      parent = Map.get(tree.nodes, target.parent)
      start = Map.get(tree.nodes, target.start)
      finish = if target.end, do: Map.get(tree.nodes, target.end)

      HtmlValidator.reserve_render_resource(
        :max_html_tree_work,
        if(is_map(parent), do: 2 * length(parent.children), else: 1),
        :html
      )

      owned? =
        is_map(parent) and
          match?(
            %{type: :processing_instruction, target: marker} when marker in ["marker", "start"],
            start
          ) and start.parent_id == target.parent and
          target.start in parent.children and
          (target.end == nil or
             (is_map(finish) and
                (target.end == target.start or
                   match?(%{type: :processing_instruction, target: "end"}, finish)) and
                finish.parent_id == target.parent and
                target.end in parent.children))

      range_matches? =
        if owned? and opening? do
          if start.target == "marker" do
            target.end == target.start and target.removed == []
          else
            patch_range(tree, target.parent, target.start) == {target.end, target.removed}
          end
        else
          not opening?
        end

      if owned? and range_matches?,
        do: :ok,
        else:
          Diagnostics.error(
            :html,
            :invalid_document,
            "content patch boundaries or removed range no longer belong to the prepared insertion parent",
            module: __MODULE__,
            operation: :content_patch
          )
    end)
  end

  @doc false
  @spec validate_patch_adoption(HtmlTree.t(), map(), [reference()]) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_patch_adoption(tree, target, ids) do
    HtmlValidator.with_render_budget(fn ->
      with :ok <- validate_patch_boundaries(tree, target, false) do
        HtmlValidator.reserve_render_resource(:max_html_tree_work, length(ids), :html)

        valid? =
          Enum.uniq(ids) == ids and
            Enum.all?(ids, fn id ->
              node = Map.get(tree.nodes, id)

              is_map(node) and node.document_id == tree.nodes[tree.root].document_id and
                node.type not in [:document, :document_fragment] and
                id not in [target.start, target.end] and
                adoptable_subtree?(tree, id, target.parent, node.document_id, 0)
            end)

        if valid?,
          do: :ok,
          else:
            Diagnostics.error(
              :html,
              :invalid_document,
              "patch adoption requires distinct authored nodes from this document, excluding boundaries and insertion-parent ancestors",
              module: __MODULE__,
              operation: :content_patch
            )
      end
    end)
  end

  defp adoptable_subtree?(tree, id, parent, document, depth) do
    HtmlValidator.check_render_resource(:max_html_depth, depth, :html)
    HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
    node = Map.get(tree.nodes, id)

    if is_map(node) and id != parent and node.document_id == document do
      children = Map.get(node, :children, []) ++ if(node[:content], do: [node.content], else: [])
      Enum.all?(children, &adoptable_subtree?(tree, &1, parent, document, depth + 1))
    else
      false
    end
  end

  # Children are stored backwards. Search only ordinary descendants in authored
  # tree order; template content is a different tree, never a light descendant.
  defp find_patch_marker(tree, parent, name) do
    children = Map.fetch!(tree.nodes, parent) |> Map.get(:children, [])
    HtmlValidator.reserve_render_resource(:max_html_tree_work, length(children), :html)

    children
    |> Enum.reverse()
    |> Enum.reduce_while({:ok, nil}, fn id, _result ->
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
      node = Map.fetch!(tree.nodes, id)

      result =
        case node do
          %{type: :processing_instruction, target: target} when target in ["marker", "start"] ->
            with {:ok, marker_name} <-
                   ProcessingInstructionValidator.attribute(Map.delete(node, :projection), "name") do
              {:ok, if(marker_name == name, do: id, else: nil)}
            end

          _ ->
            find_patch_marker(tree, id, name)
        end

      case result do
        {:ok, nil} -> {:cont, result}
        _ -> {:halt, result}
      end
    end)
  end

  # Range matching uses siblings only. Every start target increments nesting,
  # irrespective of its name or whether its pseudo-attribute data is malformed.
  defp patch_range(tree, parent, start) do
    siblings = Map.fetch!(tree.nodes, parent).children
    HtmlValidator.reserve_render_resource(:max_html_tree_work, 3 * length(siblings), :html)
    {later, [^start | _earlier]} = Enum.split_while(siblings, &(&1 != start))

    later
    |> Enum.reverse()
    |> Enum.reduce_while({nil, [], 0}, fn id, {nil, removed, nesting} ->
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)

      case Map.fetch!(tree.nodes, id) do
        %{type: :processing_instruction, target: "end"} when nesting == 0 ->
          {:halt, {id, removed, nesting}}

        %{type: :processing_instruction, target: "end"} ->
          {:cont, {nil, [id | removed], nesting - 1}}

        %{type: :processing_instruction, target: "start"} ->
          {:cont, {nil, [id | removed], nesting + 1}}

        _ ->
          {:cont, {nil, [id | removed], nesting}}
      end
    end)
    |> then(fn {end_marker, removed, _nesting} -> {end_marker, Enum.reverse(removed)} end)
  end

  defp walk(node, canonical?, document, scope, parent, seen, depth) do
    HtmlValidator.check_render_resource(:max_html_depth, depth, :html)
    HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
    fragment? = Map.get(node, :type) == :document_fragment
    scope = if fragment?, do: Map.get(node, :node_id), else: scope
    parent = if fragment?, do: nil, else: parent

    with :ok <- validate_node_metadata(node) do
      consistent? =
        if canonical? do
          Map.get(node, :document_id) == document and
            Map.get(node, :tree_scope_id) == scope and
            Map.get(node, :parent_id) == parent and
            is_reference(Map.get(node, :node_id)) and
            not MapSet.member?(seen, node.node_id)
        else
          not Enum.any?([:document_id, :tree_scope_id, :parent_id], &Map.has_key?(node, &1)) and
            (not Map.has_key?(node, :node_id) or
               (Map.get(node, :type) == :processing_instruction and
                  not MapSet.member?(seen, node.node_id)))
        end

      if consistent? do
        seen = if Map.has_key?(node, :node_id), do: MapSet.put(seen, node.node_id), else: seen
        children = Map.get(node, :children, [])
        children = if is_list(children), do: children, else: []

        descendants =
          case Map.get(node, :content) do
            fragment when is_map(fragment) -> children ++ [fragment]
            _ -> children
          end

        Enum.reduce_while(descendants, {:ok, seen}, fn child, {:ok, seen} ->
          result =
            if is_map(child),
              do:
                walk(
                  child,
                  canonical?,
                  document,
                  scope,
                  Map.get(node, :node_id),
                  seen,
                  depth +
                    if(Map.get(node, :type) in [:document, :document_fragment], do: 0, else: 1)
                ),
              else:
                if(canonical?,
                  do:
                    metadata_error(
                      node,
                      "canonical HTML identity requires map nodes throughout the tree"
                    ),
                  else: {:ok, seen}
                )

          case result do
            {:ok, _seen} -> {:cont, result}
            error -> {:halt, error}
          end
        end)
      else
        metadata_error(
          node,
          "HTML identity transport must cover the complete tree with unique node IDs and matching parent, document and tree-scope relationships; do not mix canonical and metadata-free nodes"
        )
      end
    end
  end

  defp metadata_error(node, message) do
    source = if is_map(node), do: Map.get(node, :source), else: nil

    context =
      if is_map(source),
        do:
          Map.take(source, [:line, :column])
          |> Map.to_list()
          |> Enum.filter(fn {_key, value} -> is_integer(value) and value > 0 end),
        else: []

    source = if is_map(source), do: Map.get(source, :text), else: nil

    if is_binary(source) do
      # Invalid metadata can bypass the ordinary source checks. Bound diagnostic
      # UTF-8 scanning and invalid-binary inspection before either operation.
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 2 * byte_size(source), :html)
    end

    source =
      cond do
        is_nil(source) -> nil
        is_binary(source) and String.valid?(source, :fast_ascii) -> source
        true -> inspect(source)
      end

    Diagnostics.error(
      :style,
      :invalid_document,
      message,
      [module: __MODULE__, operation: :compute, source: source] ++ context
    )
  end
end
