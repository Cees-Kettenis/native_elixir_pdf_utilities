defmodule NativeElixirPdfUtilities.HtmlToPdf.CssSelector do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.RenderCache
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, LanguageValidator}

  @doc "Returns stable generated owner identities for selected validated pseudo-elements."
  @spec pseudo_owners([map()], map(), [map()], map(), map()) :: [map()]
  def pseudo_owners(selectors, node, ancestors, context, style \\ %{}) do
    alias NativeElixirPdfUtilities.Validators.CssPseudoValidator

    selectors
    |> Enum.flat_map(fn selector ->
      pseudo = List.last(selector.parts).pseudo_element

      if pseudo != nil and CssPseudoValidator.owner?(pseudo, node, style) and
           matches_rule?(
             %{selector | parts: Enum.reverse(selector.parts)},
             node,
             ancestors,
             context
           ),
         do: [
           %{
             owner: owner_identity(node, pseudo),
             pseudo: pseudo,
             paint_ready: CssPseudoValidator.paint_ready?(pseudo)
           }
         ],
         else: []
    end)
    |> Enum.uniq()
  end

  @doc "Returns the originating element and pseudo-element's generated identity."
  @spec owner_identity(map(), atom() | tuple()) :: tuple()
  def owner_identity(node, pseudo), do: {Map.fetch!(node, :_selector_id), pseudo}

  @doc "Retains authored relationships and prepared control states for tree selectors."
  @spec prepare_context(map(), map(), [map()], map()) :: map()
  def prepare_context(context, dom, children, composition) do
    alias NativeElixirPdfUtilities.Validators.HtmlSelectedContentValidator

    logical =
      Map.merge(
        HtmlSelectedContentValidator.logical_index(dom),
        Map.get(composition, :clone_logical_nodes, %{})
      )

    prepared = HtmlSelectedContentValidator.prepared_index(children)

    logical =
      Map.new(logical, fn {id, original} ->
        node =
          Map.merge(original, Map.get(prepared, id, %{}))
          |> Map.put(:children, Map.get(original, :children, []))

        {id, node}
      end)

    Map.merge(context, %{
      logical_nodes: logical,
      root_children: children,
      consumed_shadow_roots: composition.roots
    })
  end

  @doc "Matches a compiled rule in its author tree, including shadow host and part boundaries."
  @spec matches_rule?(map(), map(), [map()], map()) :: boolean()
  def matches_rule?(selector, node, ancestors, context),
    do: match?({:ok, _}, match_rule(selector, node, ancestors, context))

  @doc "Matches a validated scoped rule and returns its innermost scope proximity."
  @spec match_rule(map(), map(), [map()], map()) :: {:ok, non_neg_integer() | nil} | false
  def match_rule(selector, node, ancestors, context) do
    scope = Map.get(selector, :shadow_scope_id)
    host = Map.get(selector, :shadow_host_id)
    boundary = Map.get(hd(selector.parts), :shadow_selector)
    context = Map.merge(context, %{shadow_scope_id: scope, shadow_host_id: host})

    case boundary do
      {:part, names} ->
        Enum.reduce(part_hosts(node, names, scope, context), false, fn host, best ->
          scoped_match =
            scoped_match(
              selector,
              host,
              logical_ancestors(host, context),
              Map.put(context, :part_target, node)
            )

          closer_match(best, scoped_match)
        end)

      _ ->
        applicable =
          case {scope, boundary} do
            {nil, nil} ->
              not Map.has_key?(node, :shadow_scope_id)

            {nil, _} ->
              false

            {_, {kind, _}} when kind in [:host, :host_context] ->
              Map.get(node, :node_id) == host

            {scope, {:slotted, _}} ->
              Map.get(node, :assigned_shadow_scope_id) == scope

            {scope, nil} ->
              Map.get(node, :tree_scope_id) == scope or
                ((Map.get(node, :node_id) == host and
                    Map.get(hd(selector.parts), :host_eligible, false)) or
                   (Map.get(selector, :scopes, []) != [] and
                      Map.get(hd(selector.parts), :scope_eligible, false)))
          end

        local =
          Enum.filter(ancestors, fn ancestor ->
            if is_nil(scope),
              do: not Map.has_key?(ancestor, :shadow_scope_id),
              else:
                Map.get(ancestor, :tree_scope_id) == scope or Map.get(ancestor, :node_id) == host
          end)

        local =
          if match?({:slotted, _}, boundary),
            do: Enum.reject(local, &(Map.get(&1, :node_id) == Map.get(node, :assigned_slot_id))),
            else: local

        if applicable, do: scoped_match(selector, node, local, context), else: false
    end
  end

  defp scoped_match(selector, node, ancestors, context) do
    scopes = Map.get(selector, :scopes, [])

    if scopes == [] do
      if matches?(selector.parts, node, ancestors, context), do: {:ok, nil}, else: false
    else
      subject =
        case Map.get(hd(selector.parts), :shadow_selector) do
          {:slotted, _} -> Map.get(node, :assigned_slot, node)
          _ -> node
        end

      parents =
        if identity(subject) == identity(node),
          do: ancestors,
          else: logical_ancestors(subject, context)

      chain = [subject | parents]

      states =
        Enum.reduce(scopes, [%{context: context, root_index: length(chain) - 1}], fn scope,
                                                                                     states ->
          Enum.flat_map(states, fn state ->
            chain
            |> Enum.with_index()
            |> Enum.flat_map(fn {root, index} ->
              HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)
              root_parents = Enum.drop(chain, index + 1)

              root_match =
                if is_nil(scope.roots) do
                  owner = Map.get(selector, :implicit_scope_root_id)
                  owner_node = Map.get(Map.get(context, :logical_nodes, %{}), owner)

                  implicit =
                    cond do
                      is_map(owner_node) and owner_node.type == :element -> owner
                      not is_nil(Map.get(context, :shadow_host_id)) -> context.shadow_host_id
                      true -> identity(List.last(chain))
                    end

                  identity(root) == implicit
                else
                  Enum.any?(
                    scope.roots,
                    &matches?(Enum.reverse(&1.parts), root, root_parents, state.context)
                  )
                end

              local = Map.put(state.context, :scope_root, identity(root))

              limited =
                if is_nil(scope.limits),
                  do: false,
                  else:
                    chain
                    |> Enum.take(index + 1)
                    |> Enum.with_index()
                    |> Enum.any?(fn {candidate, position} ->
                      Enum.any?(
                        scope.limits,
                        &matches?(
                          Enum.reverse(&1.parts),
                          candidate,
                          Enum.drop(chain, position + 1),
                          local
                        )
                      )
                    end)

              if index <= state.root_index and root_match and not limited,
                do: [%{context: local, root_index: index}],
                else: []
            end)
          end)
        end)

      Enum.reduce(states, false, fn state, best ->
        result =
          if matches?(selector.parts, node, ancestors, state.context),
            do: {:ok, state.root_index},
            else: false

        closer_match(best, result)
      end)
    end
  end

  defp closer_match(left, right) do
    case {left, right} do
      {false, result} -> result
      {result, false} -> result
      {{:ok, left}, {:ok, right}} -> {:ok, min(left, right)}
    end
  end

  defp part_hosts(node, names, scope, context) do
    exposed_part_hosts(
      node,
      HtmlValidator.html_class_tokens(Map.get(node.attributes, "part", "")),
      names,
      scope,
      context
    )
  end

  defp exposed_part_hosts(node, parts, names, scope, context) do
    HtmlValidator.reserve_render_resource(:max_css_work, length(parts) + length(names) + 1, :css)

    case Map.get(Map.get(context, :consumed_shadow_roots, %{}), Map.get(node, :tree_scope_id)) do
      nil ->
        []

      root ->
        host = Map.fetch!(context.logical_nodes, root.host_id)
        parent_scope = Map.get(host, :tree_scope_id)

        if parent_scope == scope or
             (is_nil(scope) and not Map.has_key?(context.consumed_shadow_roots, parent_scope)) do
          if MapSet.subset?(MapSet.new(names), MapSet.new(parts)), do: [host], else: []
        else
          exported =
            NativeElixirPdfUtilities.Validators.CssSelectorValidator.exported_parts(
              Map.get(host.attributes, "exportparts", ""),
              parts
            )

          if exported == [],
            do: [],
            else: exposed_part_hosts(host, exported, names, scope, context)
        end
    end
  end

  defp logical_ancestors(node, context) do
    case Map.get(context.logical_nodes, Map.get(node, :parent_id)) do
      %{type: :element} = parent ->
        [parent | logical_ancestors(parent, context)]

      %{type: :document_fragment, node_id: id} ->
        case Map.get(Map.get(context, :consumed_shadow_roots, %{}), id) do
          nil ->
            []

          root ->
            host = Map.fetch!(context.logical_nodes, root.host_id)
            [host | logical_ancestors(host, context)]
        end

      _ ->
        []
    end
  end

  @doc "Matches validated selector parts in right-to-left order against the source tree."
  @spec matches?([map()], map(), [map()], map()) :: boolean()
  def matches?(parts, node, ancestors, context) do
    case parts do
      [part] ->
        matches_simple_selector?(part, node, ancestors, context)

      [part, left_part | remaining] ->
        case matches_simple_selector?(part, node, ancestors, context) do
          true ->
            matches_ancestor_selector?(
              part.combinator,
              left_part,
              remaining,
              node,
              ancestors,
              context
            )

          false ->
            false
        end
    end
  end

  defp matches_ancestor_selector?(combinator, left_part, remaining, node, ancestors, context) do
    case {combinator, ancestors} do
      {kind, _} when kind in [:adjacent, :sibling] ->
        preceding =
          siblings(node, ancestors, context)
          |> Enum.take_while(&(identity(&1) != identity(node)))
          |> Enum.reverse()

        preceding = if kind == :adjacent, do: Enum.take(preceding, 1), else: preceding
        Enum.any?(preceding, &matches?([left_part | remaining], &1, ancestors, context))

      {:child, [parent | rest]} ->
        matches?([left_part | remaining], parent, rest, context)

      {:descendant, ancestors} ->
        ancestors
        |> Enum.with_index()
        |> Enum.any?(fn {ancestor, index} ->
          rest = Enum.drop(ancestors, index + 1)
          matches?([left_part | remaining], ancestor, rest, context)
        end)

      _ ->
        false
    end
  end

  defp matches_simple_selector?(part, node, ancestors, context) do
    HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)

    target =
      case Map.get(part, :shadow_selector) do
        {:slotted, _} -> Map.get(node, :assigned_slot, node)
        _ -> node
      end

    attributes = Map.get(target, :attributes, %{})

    classes =
      Map.get_lazy(target, :_selector_classes, fn ->
        target
        |> Map.get(:attributes, %{})
        |> Map.get("class", "")
        |> HtmlValidator.html_class_tokens()
        |> MapSet.new()
      end)

    expected_tag =
      if Map.get(node, :namespace, :html) == :html,
        do: part.tag,
        else: Map.get(part, :source_tag, part.tag)

    tag_matches? = is_nil(part.tag) or Map.get(target, :tag) == expected_tag

    namespace_matches? =
      case Map.get(part, :namespace, :any) do
        :any -> true
        expected -> namespace_uri(Map.get(target, :namespace, :html)) == expected
      end

    id_matches? =
      (is_nil(part.id) or Map.get(attributes, "id") == part.id) and
        Enum.all?(Map.get(part, :extra_ids, []), &(Map.get(attributes, "id") == &1))

    classes_match? = Enum.all?(part.classes, &MapSet.member?(classes, &1))

    scope_eligible =
      Map.get(context, :scope_root) == identity(node) and
        not is_nil(Map.get(context, :scope_root)) and Map.get(part, :scope_eligible, false)

    # Resolve context only for compounds whose positive requirements match.
    # An impossible compound needs neither sibling positions nor negation work.
    shadow_matches? =
      case Map.get(part, :shadow_selector) do
        {:host, compound} ->
          Map.get(node, :node_id) == Map.get(context, :shadow_host_id) and
            (is_nil(compound) or
               matches_simple_selector?(
                 compound,
                 node,
                 ancestors,
                 Map.put(context, :shadow_argument, true)
               ))

        {:host_context, compound} ->
          Map.get(node, :node_id) == Map.get(context, :shadow_host_id) and
            Enum.any?([node | logical_ancestors(node, context)], fn candidate ->
              matches_simple_selector?(
                compound,
                candidate,
                logical_ancestors(candidate, context),
                Map.put(context, :shadow_argument, true)
              )
            end)

        {:part, _} ->
          Map.has_key?(context, :part_target) and
            (not Map.has_key?(part, :shadow_tail) or
               matches_simple_selector?(
                 part.shadow_tail,
                 context.part_target,
                 [],
                 Map.put(context, :part_argument, true)
               ))

        {:slotted, compound} ->
          Map.get(node, :assigned_shadow_scope_id) == Map.get(context, :shadow_scope_id) and
            matches_simple_selector?(
              compound,
              node,
              ancestors,
              Map.put(context, :shadow_argument, true)
            )

        nil ->
          Map.get(context, :shadow_argument, false) or is_nil(Map.get(context, :shadow_scope_id)) or
            Map.get(node, :node_id) != Map.get(context, :shadow_host_id) or
            Map.get(part, :host_eligible, false) or scope_eligible
      end

    featureless =
      Map.get(context, :shadow_host_id) != nil and
        Map.get(node, :node_id) == Map.get(context, :shadow_host_id) and
        not Map.get(context, :shadow_argument, false)

    (not Map.get(context, :part_argument, false) or Map.get(part, :part_eligible, false)) and
      (not featureless or Map.get(part, :host_eligible, false) or scope_eligible) and
      shadow_matches? and
      namespace_matches? and tag_matches? and id_matches? and classes_match? and
      Enum.all?(part.attributes, &attribute_matches?(&1, target)) and
      Enum.all?(part.pseudo_classes, &pseudo_class_matches?(&1, target, ancestors, context)) and
      Enum.all?(part.negations, &(not matches_simple_selector?(&1, target, ancestors, context)))
  end

  defp pseudo_class_matches?(pseudo_class, node, ancestors, context) do
    case pseudo_class do
      {kind, selectors} when kind in [:is, :where, :not] ->
        matched =
          Enum.any?(selectors, &matches?(Enum.reverse(&1.parts), node, ancestors, context))

        if kind == :not, do: not matched, else: matched

      {:has, selectors} ->
        Enum.any?(selectors, fn selector ->
          relative_matches?(tl(selector.parts), [{node, ancestors}], context)
        end)

      {kind, index, selectors} when kind in [:nth_child, :nth_last_child] ->
        filtered =
          Enum.filter(siblings(node, ancestors, context), fn sibling ->
            Enum.any?(selectors, &matches?(Enum.reverse(&1.parts), sibling, ancestors, context))
          end)

        position = Enum.find_index(filtered, &(identity(&1) == identity(node)))

        if is_nil(position),
          do: false,
          else:
            nth_matches?(
              index,
              if(kind == :nth_child, do: position + 1, else: length(filtered) - position)
            )

      {:dir, direction} ->
        parents =
          if Map.has_key?(context, :logical_nodes),
            do: logical_ancestors(node, context),
            else: ancestors

        original = Map.get(Map.get(context, :logical_nodes, %{}), identity(node), node)

        NativeElixirPdfUtilities.Validators.HtmlSelectorStateValidator.direction(
          original,
          parents
        ) == direction

      :empty ->
        node
        |> source_children(context)
        |> Enum.all?(fn child ->
          case child do
            %{type: :element} -> false
            %{type: :text, text: text} -> Regex.match?(~r/\A[ \t\n\r\f]*\z/, text)
            _ -> true
          end
        end)

      :scope ->
        case Map.get(context, :scope_root) do
          nil -> Map.get(node, :tag) == "html" and ancestors == []
          root -> identity(node) == root
        end

      state
      when state in [
             :any_link,
             :link,
             :visited,
             :local_link,
             :open,
             :unchecked,
             :defined,
             :modal,
             :autofill,
             :placeholder_shown,
             :default,
             :in_range,
             :out_of_range,
             :user_valid,
             :user_invalid,
             :hover,
             :active,
             :focus,
             :focus_visible,
             :focus_within,
             :target,
             :target_within
           ] ->
        NativeElixirPdfUtilities.Validators.HtmlSelectorStateValidator.matches?(
          state,
          node,
          ancestors,
          context
        )

      {:lang, ranges} ->
        language = LanguageValidator.resolved_language(node, ancestors)
        Enum.any?(ranges, &LanguageValidator.matches?(&1, language))

      state
      when state in [
             :enabled,
             :disabled,
             :checked,
             :required,
             :optional,
             :valid,
             :invalid,
             :popover_open,
             :indeterminate
           ] ->
        HtmlValidator.control_state_matches?(state, node)

      state when state in [:read_only, :read_write] ->
        HtmlValidator.editing_state_matches?(state, node, ancestors, context.cache)

      :root ->
        Map.get(node, :tag) == "html" and ancestors == []

      kind
      when kind in [
             :first_child,
             :last_child,
             :first_of_type,
             :last_of_type,
             :only_child,
             :only_of_type
           ] ->
        case Map.fetch(
               selector_positions(ancestors, context, node),
               Map.get(node, :node_id, node._selector_id)
             ) do
          {:ok, {position, count, type_position, type_count}} ->
            case kind do
              :first_child -> position == 1
              :last_child -> position == count
              :first_of_type -> type_position == 1
              :last_of_type -> type_position == type_count
              :only_child -> count == 1
              :only_of_type -> type_count == 1
            end

          :error ->
            # The virtual body supplying fragment defaults is not a source node.
            false
        end

      {kind, index} when kind in [:nth_child, :nth_last_child, :nth_of_type, :nth_last_of_type] ->
        case Map.fetch(
               selector_positions(ancestors, context, node),
               Map.get(node, :node_id, node._selector_id)
             ) do
          {:ok, {position, count, type_position, type_count}} ->
            position =
              case kind do
                :nth_child -> position
                :nth_last_child -> count - position + 1
                :nth_of_type -> type_position
                :nth_last_of_type -> type_count - type_position + 1
              end

            nth_matches?(index, position)

          :error ->
            false
        end
    end
  end

  defp nth_matches?(index, position) do
    case index do
      :odd -> rem(position, 2) == 1
      :even -> rem(position, 2) == 0
      {0, offset} -> position == offset
      {step, offset} -> rem(position - offset, step) == 0 and div(position - offset, step) >= 0
      index -> position == index
    end
  end

  defp relative_matches?(parts, candidates, context) do
    case parts do
      [] ->
        candidates != []

      [part | remaining] ->
        Enum.any?(candidates, fn {node, ancestors} ->
          next =
            case part.combinator do
              :child ->
                Enum.map(elements(node, context), &{&1, [node | ancestors]})

              :descendant ->
                descendants(node, ancestors, context)

              kind when kind in [:adjacent, :sibling] ->
                following =
                  siblings(node, ancestors, context)
                  |> Enum.drop_while(&(identity(&1) != identity(node)))
                  |> Enum.drop(1)

                following = if kind == :adjacent, do: Enum.take(following, 1), else: following
                Enum.map(following, &{&1, ancestors})
            end

          next =
            Enum.filter(next, fn {candidate, parents} ->
              matches_simple_selector?(part, candidate, parents, context)
            end)

          relative_matches?(remaining, next, context)
        end)
    end
  end

  defp descendants(node, ancestors, context) do
    Enum.flat_map(elements(node, context), fn child ->
      [{child, [node | ancestors]} | descendants(child, [node | ancestors], context)]
    end)
  end

  defp identity(node), do: Map.get(node, :node_id, Map.get(node, :_selector_id))

  defp source_children(node, context) do
    original = Map.get(Map.get(context, :logical_nodes, %{}), identity(node), node)

    original =
      if Map.get(context, :shadow_host_id) != nil and
           Map.get(node, :node_id) == Map.get(context, :shadow_host_id),
         do:
           Map.get(
             Map.get(context, :consumed_shadow_roots, %{}),
             Map.get(context, :shadow_scope_id),
             original
           ),
         else: original

    Map.get(original, :children, [])
    |> Enum.reject(fn child ->
      Map.get(child, :tag) == "template" and
        Map.has_key?(
          Map.get(context, :consumed_shadow_roots, %{}),
          get_in(child, [:content, :node_id])
        )
    end)
    |> Enum.map(fn child ->
      Map.get(Map.get(context, :logical_nodes, %{}), identity(child), child)
    end)
  end

  defp elements(node, context) do
    children = source_children(node, context)
    HtmlValidator.reserve_render_resource(:max_css_work, length(children), :css)

    Enum.filter(children, &match?(%{type: :element}, &1))
  end

  defp siblings(node, ancestors, context),
    do: elements(selector_parent(node, ancestors, context), context)

  defp selector_parent(node, ancestors, context) do
    case Map.get(context, :logical_nodes, %{})[Map.get(node, :parent_id)] do
      parent when is_map(parent) ->
        parent

      _ ->
        case ancestors do
          [parent | _] -> parent
          [] -> %{_selector_id: context.root_selector_id, children: context.root_children}
        end
    end
  end

  defp attribute_matches?(attribute, node) do
    case attribute do
      {:namespace, namespace, selector} ->
        local = elem(selector, 1)

        Enum.any?(node.attributes, fn {name, value} ->
          uri = Map.get(Map.get(node, :attribute_namespaces, %{}), name)

          expected =
            if Map.get(node, :namespace, :html) == :html and is_nil(uri),
              do: String.downcase(local, :ascii),
              else: local

          actual_name = if is_nil(uri), do: name, else: List.last(String.split(name, ":"))

          if actual_name == expected and
               (namespace == :any or uri == if(namespace == :none, do: nil, else: namespace)) do
            HtmlValidator.attribute_selector_matches?(
              put_elem(selector, 1, expected),
              %{node | attributes: %{expected => value}}
              |> Map.put(:attribute_namespaces, %{expected => uri})
            )
          else
            false
          end
        end)

      _ ->
        HtmlValidator.attribute_selector_matches?(attribute, node)
    end
  end

  defp namespace_uri(namespace) do
    case namespace do
      :html -> "http://www.w3.org/1999/xhtml"
      :svg -> "http://www.w3.org/2000/svg"
      :mathml -> "http://www.w3.org/1998/Math/MathML"
      value -> value
    end
  end

  # The stylesheet cache lives only within this render. Fresh selector identities
  # prevent reuse across style computations, documents or changed child lists.
  defp selector_positions(ancestors, context, node) do
    parent = selector_parent(node, ancestors, context)

    key = Map.get(parent, :node_id, Map.get(parent, :_selector_id))

    RenderCache.fetch_stylesheet({:selector_positions, context.cache, key}, fn ->
      elements =
        Enum.filter(parent.children, fn child ->
          match?(%{type: :element}, child) and
            not (Map.get(child, :tag) == "template" and
                   Map.has_key?(
                     Map.get(context, :consumed_shadow_roots, %{}),
                     get_in(child, [:content, :node_id])
                   ))
        end)

      HtmlValidator.reserve_render_resource(:max_css_work, length(elements), :css)
      type_counts = Enum.frequencies_by(elements, &{Map.get(&1, :namespace, :html), &1.tag})
      count = length(elements)

      {positions, _seen} =
        elements
        |> Enum.with_index(1)
        |> Enum.reduce({%{}, %{}}, fn {child, index}, {positions, seen} ->
          type = {Map.get(child, :namespace, :html), child.tag}
          type_position = Map.get(seen, type, 0) + 1
          value = {index, count, type_position, Map.fetch!(type_counts, type)}

          {Map.put(positions, Map.get(child, :node_id, Map.get(child, :_selector_id)), value),
           Map.put(seen, type, type_position)}
        end)

      positions
    end)
  end
end
