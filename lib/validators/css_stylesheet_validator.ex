defmodule NativeElixirPdfUtilities.Validators.CssStylesheetValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.RenderCache

  alias NativeElixirPdfUtilities.Validators.{
    CssSyntaxValidator,
    CssSelectorValidator,
    HtmlValidator
  }

  @doc "Prepares nested style rules, layer declarations and scoped rules with authored byte spans."
  @spec prepare(String.t(), map()) :: {:ok, map()}
  def prepare(source, namespaces \\ %{}) do
    RenderCache.fetch_stylesheet(
      {__MODULE__, source, namespaces},
      fn ->
        state = %{rules: [], layers: [], registrations: []}

        context = %{
          parent: nil,
          parsed_selectors: nil,
          scope_top: false,
          condition_active: true,
          scope_language: false,
          layer: [],
          scopes: [],
          namespaces: namespaces,
          origin: 0,
          depth: 0
        }

        state = flatten(source, context, state)
        state = Map.new(state, fn {key, values} -> {key, Enum.reverse(values)} end)
        HtmlValidator.reserve_render_resource(:max_css_rules, length(state.rules), :css)
        {:ok, state}
      end,
      fn {:ok, state} ->
        HtmlValidator.reserve_render_resource(:max_css_rules, length(state.rules), :css)
        :ok
      end
    )
  end

  defp flatten(source, context, state) do
    HtmlValidator.check_render_resource(:max_css_syntax_depth, context.depth, :css)
    items = CssSyntaxValidator.block_items(source, context.depth == 0)

    {state, pending} =
      Enum.reduce(items, {state, []}, fn item, {state, pending} ->
        if item.kind == :declaration do
          {state, [item | pending]}
        else
          state = emit_declarations(pending, source, context, state)
          state = flatten_rule(item, context, state)
          {state, []}
        end
      end)

    emit_declarations(pending, source, context, state)
  end

  defp emit_declarations(items, source, context, state) do
    if items == [] or is_nil(context.parent) do
      state
    else
      [first | _] = items = Enum.reverse(items)
      last = List.last(items)
      length = last.offset + byte_size(last.source) - first.offset
      body = binary_part(source, first.offset, length)

      rule = %{
        selectors: context.parent,
        declarations: body,
        offset: context.origin + first.offset,
        body_offset: context.origin + first.offset,
        source: body,
        block: true,
        closed: true,
        selector_offset: context.selector_offset,
        selector_length: context.selector_length,
        layer: context.layer,
        scopes: context.scopes,
        condition_active: context.condition_active,
        resolved_selectors: true,
        parsed_selectors: context.parsed_selectors
      }

      %{state | rules: [rule | state.rules]}
    end
  end

  defp flatten_rule(rule, context, state) do
    prelude = HtmlValidator.trim_css_whitespace(rule.selectors)

    rule_at =
      context.origin + rule.offset + byte_size(rule.selectors) -
        byte_size(String.trim_leading(rule.selectors))

    body_at = context.origin + rule.offset + byte_size(rule.selectors) + 1
    child = %{context | origin: body_at, depth: context.depth + 1}

    case at_rule(prelude) do
      {"layer", body} ->
        case layer_names(body, context.layer, rule.block, rule_at) do
          {:ok, paths} ->
            events =
              Enum.map(
                paths,
                &%{
                  path: &1,
                  offset: rule_at,
                  condition_active: context.condition_active
                }
              )

            state = %{state | layers: Enum.reverse(events) ++ state.layers}

            if rule.block,
              do: flatten(rule.declarations, %{child | layer: hd(paths)}, state),
              else: state

          :error ->
            state
        end

      {"scope", body} when rule.block ->
        case scope(body, context) do
          {:ok, scope} ->
            flatten(
              rule.declarations,
              %{
                child
                | scopes: context.scopes ++ [scope],
                  scope_top: true,
                  scope_language: context.scope_language or scope.language_dependent,
                  parent: if(context.parent, do: ":where(:scope)", else: nil),
                  parsed_selectors:
                    if(context.parent,
                      do: elem(CssSelectorValidator.parse(":where(:scope)"), 1),
                      else: nil
                    )
              },
              state
            )

          :error ->
            state
        end

      {"supports", body} when rule.block ->
        active =
          context.condition_active and
            NativeElixirPdfUtilities.Validators.CssSupportsValidator.matches?(body)

        flatten(rule.declarations, %{child | condition_active: active}, state)

      {"property", body} when rule.block and is_nil(context.parent) ->
        registration = %{
          name: body,
          declarations: rule.declarations,
          offset: rule_at,
          condition_active: context.condition_active
        }

        %{state | registrations: [registration | state.registrations]}

      {_unknown, _} ->
        state

      nil ->
        selector =
          cond do
            context.scope_top -> scoped_selector(prelude)
            context.parent -> nested_selector(prelude, context.parent)
            true -> elem(replace_nesting(prelude, ":where(:scope)"), 0)
          end

        case CssSelectorValidator.parse(selector, %{namespaces: context.namespaces}) do
          {:ok, parsed_selectors} ->
            parsed_selectors =
              if context.scope_language,
                do: Enum.map(parsed_selectors, &Map.put(&1, :language_dependent, true)),
                else: parsed_selectors

            leading = byte_size(rule.selectors) - byte_size(String.trim_leading(rule.selectors))

            child =
              Map.merge(child, %{
                parent: selector,
                scope_top: false,
                parsed_selectors: parsed_selectors,
                selector_offset: context.origin + rule.offset + leading,
                selector_length: byte_size(prelude)
              })

            state = flatten(rule.declarations, child, state)
            # Empty authored rules still retain their source owner and ordering.
            if HtmlValidator.trim_css_whitespace(rule.declarations) == "" do
              empty =
                Map.merge(rule, %{
                  selectors: selector,
                  declarations: "",
                  body_offset: body_at,
                  offset: context.origin + rule.offset,
                  layer: context.layer,
                  scopes: context.scopes,
                  selector_offset: child.selector_offset,
                  selector_length: child.selector_length,
                  condition_active: context.condition_active,
                  resolved_selectors: true,
                  parsed_selectors: parsed_selectors
                })

              %{state | rules: [empty | state.rules]}
            else
              state
            end

          _ ->
            state
        end
    end
  end

  defp at_rule(source) do
    case source do
      <<"@", tail::binary>> ->
        case HtmlValidator.parse_css_identifier(tail) do
          {:ok, name, body} ->
            {String.downcase(name, :ascii), HtmlValidator.trim_css_whitespace(body)}

          _ ->
            {"", ""}
        end

      _ ->
        nil
    end
  end

  defp layer_names(source, parent, block, offset) do
    if source == "" and block do
      {:ok, [parent ++ [{:anonymous, offset}]]}
    else
      names = CssSyntaxValidator.split(source, ",")

      parsed =
        Enum.map(names, fn name ->
          components = CssSyntaxValidator.split(name, ".")

          Enum.reduce_while(components, {:ok, parent}, fn component, {:ok, path} ->
            case HtmlValidator.parse_css_identifier(HtmlValidator.trim_css_whitespace(component)) do
              {:ok, name, ""} ->
                if String.downcase(name, :ascii) in ~w(initial inherit unset revert revert-layer default),
                  do: {:halt, :error},
                  else: {:cont, {:ok, path ++ [name]}}

              _ ->
                {:halt, :error}
            end
          end)
        end)

      if (not block or length(names) == 1) and Enum.all?(parsed, &match?({:ok, _}, &1)),
        do: {:ok, Enum.map(parsed, &elem(&1, 1))},
        else: :error
    end
  end

  defp scope(source, context) do
    values =
      source
      |> CssSyntaxValidator.components()
      |> Enum.reject(&(&1.kind == :comment or Regex.match?(~r/\A[ \t\n\r\f]+\z/, &1.text)))

    parsed =
      case values do
        [] ->
          {:ok, nil, nil}

        [%{kind: :block, opener: "("} = root] ->
          {:ok, root, nil}

        [
          %{kind: :block, opener: "("} = root,
          %{text: keyword},
          %{kind: :block, opener: "("} = limit
        ] ->
          if String.downcase(keyword, :ascii) == "to", do: {:ok, root, limit}, else: :error

        [%{text: keyword}, %{kind: :block, opener: "("} = limit] ->
          if String.downcase(keyword, :ascii) == "to", do: {:ok, nil, limit}, else: :error

        _ ->
          :error
      end

    case parsed do
      {:ok, root, limit} ->
        parsed =
          Enum.with_index([root, limit], fn block, index ->
            if is_nil(block) do
              {:ok, nil}
            else
              body = binary_part(block.text, 1, block.length - if(block.closed, do: 2, else: 1))

              body =
                if index == 1,
                  do: scoped_selector(body),
                  else:
                    if(context.parent,
                      do: nested_selector(body, context.parent, false),
                      else: elem(replace_nesting(body, ":where(:scope)"), 0)
                    )

              case CssSelectorValidator.parse(body, %{namespaces: context.namespaces}) do
                {:ok, selectors} = result ->
                  if Enum.any?(selectors, fn selector ->
                       Enum.any?(
                         selector.parts,
                         &(not is_nil(&1.pseudo_element) or
                             match?(
                               {kind, _} when kind in [:part, :slotted],
                               Map.get(&1, :shadow_selector)
                             ))
                       )
                     end), do: :error, else: result

                result ->
                  result
              end
            end
          end)

        case parsed do
          [{:ok, roots}, {:ok, limits}] ->
            {:ok,
             %{
               roots: roots,
               limits: limits,
               language_dependent:
                 Enum.any?(
                   List.wrap(roots) ++ List.wrap(limits),
                   &Map.get(&1, :language_dependent, false)
                 )
             }}

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  defp scoped_selector(source) do
    source
    |> CssSyntaxValidator.split(",")
    |> Enum.map_join(",", fn branch ->
      branch = HtmlValidator.trim_css_whitespace(branch)
      {branch, nesting} = replace_nesting(branch, ":where(:scope)")
      # Explicit :scope and nesting selectors may position the root anywhere in
      # a complex selector. Other branches have an implicit descendant anchor.
      scoped = nesting or contains_scope?(branch)

      if scoped and not String.starts_with?(branch, [">", "+", "~"]),
        do: branch,
        else: ":where(:scope) " <> branch
    end)
  end

  defp contains_scope?(source) do
    {found, _} =
      source
      |> CssSyntaxValidator.components()
      |> Enum.reduce({false, false}, fn component, {found, colon} ->
        case component do
          %{kind: :block, opener: opener} when opener != "[" ->
            body =
              binary_part(
                component.text,
                byte_size(opener),
                component.length - byte_size(opener) - if(component.closed, do: 1, else: 0)
              )

            {found or contains_scope?(body), false}

          %{kind: :token, text: ":"} ->
            {found, true}

          %{kind: :token, text: text} ->
            scope =
              case HtmlValidator.parse_css_identifier(text) do
                {:ok, identifier, ""} -> String.downcase(identifier, :ascii) == "scope"
                _ -> false
              end

            {found or (colon and scope), false}

          _ ->
            {found, false}
        end
      end)

    found
  end

  defp nested_selector(source, parent, implicit \\ true) do
    source
    |> CssSyntaxValidator.split(",")
    |> Enum.map_join(",", fn branch ->
      {branch, found} = replace_nesting(branch, ":is(" <> parent <> ")")
      if found or not implicit, do: branch, else: ":is(" <> parent <> ") " <> branch
    end)
  end

  defp replace_nesting(source, replacement) do
    source
    |> CssSyntaxValidator.components()
    |> Enum.map_reduce(false, fn component, found ->
      case component do
        %{kind: :token, text: "&"} ->
          {replacement, true}

        %{kind: :block, opener: opener} when opener != "[" ->
          length = component.length - byte_size(opener) - if(component.closed, do: 1, else: 0)
          body = binary_part(component.text, byte_size(opener), length)
          {body, replaced} = replace_nesting(body, replacement)

          closing =
            if component.closed,
              do: binary_part(component.text, component.length - 1, 1),
              else: ""

          {opener <> body <> closing, found or replaced}

        _ ->
          {component.text, found}
      end
    end)
    |> then(fn {parts, found} -> {Enum.join(parts), found} end)
  end
end
