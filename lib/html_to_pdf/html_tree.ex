defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlTree do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.HtmlTreeValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  # The parser owns this table. Addresses are logical identities, never permissions.
  # Children are stored backwards so attaching a token costs constant list work.
  @type t :: %__MODULE__{
          root: reference(),
          nodes: %{reference() => map()},
          hosts: %{reference() => reference()}
        }
  defstruct [:root, nodes: %{}, hosts: %{}]

  @doc false
  @spec new(map(), :document | :document_fragment) :: t()
  def new(source, type \\ :document) do
    id = make_ref()
    document = make_ref()

    root = %{
      type: type,
      node_id: id,
      document_id: document,
      tree_scope_id: id,
      parent_id: nil,
      source: source,
      children: []
    }

    %__MODULE__{root: id, nodes: %{id => root}}
  end

  @doc false
  @spec from_document(map()) :: t()
  def from_document(document) do
    tree = %__MODULE__{
      root: document.node_id,
      nodes: %{document.node_id => %{document | children: []}}
    }

    replace_children(tree, tree.root, document.children, true)
  end

  @doc false
  @spec insert(t(), reference(), map(), reference() | nil, reference() | nil) ::
          {t(), reference()}
  def insert(tree, parent, node, before \\ nil, original_id \\ nil) do
    tree = invalidate(tree, parent)
    owner = Map.fetch!(tree.nodes, parent)
    id = original_id || make_ref()

    node =
      node
      |> Map.put(:node_id, id)
      |> Map.put(:document_id, owner.document_id)
      |> Map.put(:tree_scope_id, owner.tree_scope_id)
      |> Map.put(:parent_id, parent)

    children =
      case before do
        nil ->
          [id | owner.children]

        before ->
          HtmlValidator.reserve_render_resource(
            :max_html_tree_work,
            length(owner.children),
            :html
          )

          {after_nodes, [^before | earlier]} = Enum.split_while(owner.children, &(&1 != before))
          after_nodes ++ [before, id | earlier]
      end

    nodes = tree.nodes |> Map.put(id, node) |> Map.put(parent, %{owner | children: children})
    {%{tree | nodes: nodes}, id}
  end

  @doc false
  @spec open_fragment(t(), reference(), map(), reference() | nil) :: {t(), reference()}
  def open_fragment(tree, template, source, original_id \\ nil) do
    tree = invalidate(tree, template)
    owner = Map.fetch!(tree.nodes, template)
    content = HtmlValidator.construct_html_template(%{owner | children: []}).content
    id = original_id || make_ref()

    fragment =
      Map.merge(content, %{
        type: :document_fragment,
        node_id: id,
        document_id: owner.document_id,
        tree_scope_id: id,
        parent_id: nil,
        source: source,
        children: []
      })

    nodes =
      tree.nodes
      |> Map.put(id, fragment)
      |> Map.put(template, Map.put(owner, :content, id))

    {%{tree | nodes: nodes, hosts: Map.put(tree.hosts, id, template)}, id}
  end

  @doc false
  @spec fetch(t(), reference()) :: map()
  def fetch(tree, id) do
    HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
    node = Map.fetch!(tree.nodes, id)

    case Map.fetch(node, :projection) do
      {:ok, projection} ->
        projection

      :error ->
        node =
          if Map.has_key?(node, :children),
            do: Map.put(node, :children, children(tree, id)),
            else: node

        case Map.fetch(node, :content) do
          {:ok, content} -> Map.put(node, :content, fetch(tree, content))
          :error -> node
        end
    end
  end

  @doc false
  @spec close(t(), reference()) :: {t(), map()}
  def close(tree, id) do
    node = fetch(tree, id)
    {%{tree | nodes: Map.update!(tree.nodes, id, &Map.put(&1, :projection, node))}, node}
  end

  @doc false
  @spec children(t(), reference()) :: [map()]
  def children(tree, parent) do
    tree.nodes
    |> Map.fetch!(parent)
    |> Map.fetch!(:children)
    |> Enum.reverse()
    |> Enum.map(&fetch(tree, &1))
  end

  @doc false
  @spec last_element_tag(t(), reference()) :: String.t() | nil
  def last_element_tag(tree, parent) do
    tree.nodes
    |> Map.fetch!(parent)
    |> Map.fetch!(:children)
    |> Enum.reduce_while(nil, fn id, _tag ->
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)

      case Map.fetch!(tree.nodes, id) do
        %{type: :element, tag: tag} -> {:halt, tag}
        _ -> {:cont, nil}
      end
    end)
  end

  @doc false
  @spec replace_children(t(), reference(), [map()], boolean()) :: t()
  def replace_children(tree, parent, children, recursive? \\ false) do
    tree = invalidate(tree, parent)
    owner = Map.fetch!(tree.nodes, parent)

    {tree, ids} =
      Enum.reduce(children, {tree, []}, fn node, {tree, ids} ->
        {tree, id} = store_subtree(tree, node, parent, owner.tree_scope_id, recursive?)
        {tree, [id | ids]}
      end)

    %{tree | nodes: Map.put(tree.nodes, parent, %{owner | children: ids})}
  end

  @doc false
  @spec begin_content_patch(t(), map()) :: {:ok, map()} | {:error, {atom(), map()}}
  def begin_content_patch(tree, target) do
    HtmlValidator.with_render_budget(fn ->
      with :ok <- HtmlTreeValidator.validate_patch_boundaries(tree, target, true) do
        original = tree
        tree = detach_nodes(tree, target.parent, target.removed)
        {:ok, %{tree: tree, original: original, target: target, inserted: []}}
      end
    end)
  end

  @doc false
  @spec adopt_content_patch(map(), [reference()]) :: {:ok, map()} | {:error, {atom(), map()}}
  def adopt_content_patch(transaction, ids) do
    HtmlValidator.with_render_budget(fn ->
      %{tree: tree, target: target} = transaction

      with :ok <- HtmlTreeValidator.validate_patch_adoption(tree, target, ids) do
        tree =
          Enum.reduce(ids, tree, fn id, tree ->
            case tree.nodes[id].parent_id do
              nil -> tree
              parent -> detach_nodes(tree, parent, [id])
            end
          end)

        tree = invalidate(tree, target.parent)
        owner = tree.nodes[target.parent]

        {adopted, tree} =
          Enum.map_reduce(ids, tree, fn id, tree ->
            node = fetch(tree, id)
            {tree, id} = store_subtree(tree, node, target.parent, owner.tree_scope_id, true)
            {id, tree}
          end)

        children =
          case target.end do
            nil ->
              Enum.reverse(adopted) ++ owner.children

            finish ->
              HtmlValidator.reserve_render_resource(
                :max_html_tree_work,
                length(owner.children),
                :html
              )

              {later, [^finish | earlier]} = Enum.split_while(owner.children, &(&1 != finish))
              later ++ [finish | Enum.reverse(adopted)] ++ earlier
          end

        tree = %{tree | nodes: Map.put(tree.nodes, target.parent, %{owner | children: children})}

        HtmlValidator.reserve_render_resource(
          :max_html_tree_work,
          length(transaction.inserted) + length(ids),
          :html
        )

        {:ok, %{transaction | tree: tree, inserted: transaction.inserted ++ ids}}
      end
    end)
  end

  @doc false
  @spec finish_content_patch(map()) :: {:ok, t()} | {:error, {atom(), map()}}
  def finish_content_patch(%{tree: tree, target: target}) do
    HtmlValidator.with_render_budget(fn ->
      with :ok <- HtmlTreeValidator.validate_patch_boundaries(tree, target, false) do
        boundaries = Enum.uniq([target.start, target.end]) |> Enum.reject(&is_nil/1)
        {:ok, detach_nodes(tree, target.parent, boundaries)}
      end
    end)
  end

  defp detach_nodes(tree, parent, ids) do
    tree = invalidate(tree, parent)
    owner = tree.nodes[parent]

    HtmlValidator.reserve_render_resource(
      :max_html_tree_work,
      length(owner.children) + length(ids),
      :html
    )

    removed = MapSet.new(ids)

    nodes =
      Enum.reduce(ids, tree.nodes, fn id, nodes ->
        Map.update!(nodes, id, fn node ->
          node |> Map.put(:parent_id, nil) |> Map.delete(:projection)
        end)
      end)

    children = Enum.reject(owner.children, &MapSet.member?(removed, &1))
    %{tree | nodes: Map.put(nodes, parent, %{owner | children: children})}
  end

  # Normalization can add implicit groups or edit a text token. Import those
  # projections without changing an existing logical identity or fragment root.
  defp store_subtree(tree, node, parent, scope, recursive?) do
    HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
    id = Map.get_lazy(node, :node_id, &make_ref/0)
    document = Map.fetch!(tree.nodes, tree.root).document_id
    scope = if node.type == :document_fragment, do: id, else: scope
    parent = if node.type == :document_fragment, do: nil, else: parent

    existing = Map.get(tree.nodes, id)

    stored =
      node
      |> Map.put(:node_id, id)
      |> Map.put(:document_id, document)
      |> Map.put(:tree_scope_id, scope)
      |> Map.put(:parent_id, parent)
      |> Map.put_new(:source, %{text: "", line: 1, column: 1})

    tree = %{tree | nodes: Map.put(tree.nodes, id, stored)}

    tree =
      case {existing, recursive?, Map.fetch(node, :children)} do
        {existing, false, {:ok, _}} when is_map(existing) ->
          %{tree | nodes: Map.put(tree.nodes, id, Map.put(stored, :children, existing.children))}

        {_, _, {:ok, children}} ->
          replace_children(tree, id, children, recursive?)

        {_, _, :error} ->
          tree
      end

    tree =
      case {existing, recursive?, Map.fetch(node, :content)} do
        {%{content: content}, false, _} ->
          %{tree | nodes: Map.update!(tree.nodes, id, &Map.put(&1, :content, content))}

        {_, _, {:ok, fragment}} ->
          {tree, content} = store_subtree(tree, fragment, nil, nil, recursive?)

          %{
            tree
            | nodes: Map.update!(tree.nodes, id, &Map.put(&1, :content, content)),
              hosts: Map.put(tree.hosts, content, id)
          }

        {_, _, :error} ->
          tree
      end

    {tree, id}
  end

  # A mutation of an earlier closed subtree must invalidate cached ancestors.
  # Fragment ownership is internal. Its public parent remains nil.
  defp invalidate(tree, id, work \\ 0) do
    node = Map.fetch!(tree.nodes, id)

    tree =
      if Map.has_key?(node, :projection),
        do: %{tree | nodes: Map.put(tree.nodes, id, Map.delete(node, :projection))},
        else: tree

    parent = node.parent_id || Map.get(tree.hosts, id)

    if parent == nil do
      # Parser depth is bounded before insertion. Charge the complete ancestor
      # walk once; an uncached node never stops traversal to a cached ancestor.
      HtmlValidator.reserve_render_resource(:max_html_tree_work, work + 1, :html)
      tree
    else
      invalidate(tree, parent, work + 1)
    end
  end
end
