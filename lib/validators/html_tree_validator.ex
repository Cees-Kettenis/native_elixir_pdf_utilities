defmodule NativeElixirPdfUtilities.Validators.HtmlTreeValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
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
              map_size(source) == 3 and String.valid?(text)

            _ ->
              false
          end

      if valid?,
        do: :ok,
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
        %{type: :document, children: children} when is_list(children) ->
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
        is_binary(source) and String.valid?(source) -> source
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
