defmodule NativeElixirPdfUtilities.Validators.CssSelectorValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, CssSyntaxValidator}

  @state_atoms %{
    "empty" => :empty,
    "unchecked" => :unchecked,
    "defined" => :defined,
    "modal" => :modal,
    "autofill" => :autofill,
    "scope" => :scope,
    "any-link" => :any_link,
    "link" => :link,
    "visited" => :visited,
    "local-link" => :local_link,
    "open" => :open,
    "placeholder-shown" => :placeholder_shown,
    "default" => :default,
    "in-range" => :in_range,
    "out-of-range" => :out_of_range,
    "user-valid" => :user_valid,
    "user-invalid" => :user_invalid,
    "hover" => :hover,
    "active" => :active,
    "focus" => :focus,
    "focus-visible" => :focus_visible,
    "focus-within" => :focus_within,
    "target" => :target,
    "target-within" => :target_within
  }
  @initial_states Map.keys(@state_atoms)

  @doc "Validates a selector list and computes its specificity."
  @spec parse(String.t(), map()) :: {:ok, [map()]} | {:error, :invalid_css}
  def parse(selector_source, context \\ %{}) do
    selector_source
    |> HtmlValidator.normalize_css_comments(:syntax)
    |> split_selector_list()
    |> Enum.reduce_while({:ok, []}, fn selector, {:ok, acc} ->
      case parse_selector(HtmlValidator.trim_css_whitespace(selector), context) do
        {:ok, parsed} -> {:cont, {:ok, [parsed | acc]}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
    |> then(fn result ->
      case result do
        {:ok, items} -> {:ok, Enum.reverse(items)}
        error -> error
      end
    end)
  end

  @doc "Selects valid exported part mappings from the HTML exportparts attribute."
  @spec exported_parts(String.t(), [String.t()]) :: [String.t()]
  def exported_parts(source, parts) do
    HtmlValidator.reserve_render_resource(:max_work, byte_size(source) + length(parts), :css)
    parts = MapSet.new(parts)

    source
    |> String.split(",")
    |> Enum.flat_map(fn mapping ->
      case String.split(mapping, ":") |> Enum.map(&HtmlValidator.trim_ascii_whitespace/1) do
        [name] ->
          if MapSet.member?(parts, name), do: [name], else: []

        [internal, external] ->
          if MapSet.member?(parts, internal) and external != "" and
               HtmlValidator.html_class_tokens(external) == [external], do: [external], else: []

        _ ->
          []
      end
    end)
  end

  defp split_selector_list(source), do: CssSyntaxValidator.split(source, ",")

  defp parse_selector(selector, context) do
    HtmlValidator.check_render_resource(
      :max_depth,
      Map.get(context, :depth, 0),
      :css
    )

    HtmlValidator.reserve_render_resource(:max_work, byte_size(selector), :css)

    tokens =
      selector
      |> CssSyntaxValidator.components()
      |> Enum.reduce({[], []}, fn component, {tokens, current} ->
        cond do
          component.kind == :token and component.text in [">", "+", "~"] ->
            {[component.text | push_selector_token(tokens, current)], []}

          component.kind == :token and Regex.match?(~r/\A[ \t\n\r\f]+\z/, component.text) ->
            {push_selector_token(tokens, current), []}

          true ->
            {tokens, [component.text | current]}
        end
      end)
      |> then(fn {tokens, current} ->
        tokens |> push_selector_token(current) |> Enum.reverse()
      end)

    if tokens == [], do: {:error, :invalid_css}, else: selector_tokens_to_parts(tokens, context)
  end

  defp push_selector_token(tokens, current) do
    case current |> Enum.reverse() |> Enum.join() |> HtmlValidator.trim_css_whitespace() do
      "" -> tokens
      token -> [token | tokens]
    end
  end

  defp selector_tokens_to_parts(tokens, context) do
    parsed =
      Enum.reduce_while(tokens, {:ok, [], nil, false}, fn token,
                                                          {:ok, parts, pending_combinator,
                                                           language?} ->
        cond do
          token in [">", "+", "~"] and (parts == [] or not is_nil(pending_combinator)) ->
            {:halt, {:error, :invalid_css}}

          token in [">", "+", "~"] ->
            {:cont,
             {:ok, parts, Map.fetch!(%{">" => :child, "+" => :adjacent, "~" => :sibling}, token),
              language?}}

          true ->
            case parse_simple_selector(token, context) do
              {:ok, part} ->
                combinator =
                  case parts do
                    [] -> nil
                    _ -> pending_combinator || :descendant
                  end

                part = Map.put(part, :combinator, combinator)

                part =
                  if Map.get(context, :part_argument, false) and combinator != nil,
                    do: Map.put(part, :part_eligible, false),
                    else: part

                {:cont,
                 {:ok, [part | parts], nil,
                  language? or Map.get(part, :language_dependent, false)}}

              {:error, reason} ->
                {:halt, {:error, reason}}
            end
        end
      end)

    case parsed do
      {:ok, _parts, pending_combinator, _language?} when not is_nil(pending_combinator) ->
        {:error, :invalid_css}

      {:ok, parts, nil, language?} ->
        parts = Enum.reverse(parts)

        case Enum.drop(parts, -1) |> Enum.any?(&(not is_nil(&1.pseudo_element))) do
          true ->
            {:error, :invalid_css}

          false ->
            selector = %{parts: parts, specificity: specificity(parts)}

            selector =
              if Enum.any?(parts, fn part ->
                   part.combinator in [:adjacent, :sibling] or
                     Map.has_key?(part, :shadow_selector) or
                     Enum.any?([part | part.negations], fn compound ->
                       Enum.any?(compound.pseudo_classes, fn pseudo ->
                         is_tuple(pseudo) or
                           pseudo in [
                             :empty,
                             :first_child,
                             :last_child,
                             :first_of_type,
                             :last_of_type,
                             :only_child,
                             :only_of_type
                           ]
                       end)
                     end)
                 end), do: Map.put(selector, :tree_dependent, true), else: selector

            {:ok, if(language?, do: Map.put(selector, :language_dependent, true), else: selector)}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp parse_simple_selector(selector, context) do
    HtmlValidator.check_render_resource(
      :max_depth,
      Map.get(context, :depth, 0),
      :css
    )

    parsed = qualified_name(selector, context)

    case parsed do
      {:ok, namespace, tag, modifiers} ->
        part = %{
          tag: if(is_nil(tag), do: nil, else: String.downcase(tag, :ascii)),
          id: nil,
          classes: [],
          attributes: [],
          pseudo_classes: [],
          negations: [],
          pseudo_element: nil,
          combinator: nil
        }

        part =
          if is_binary(tag) and tag != part.tag, do: Map.put(part, :source_tag, tag), else: part

        part = if namespace == :any, do: part, else: Map.put(part, :namespace, namespace)

        part =
          if is_nil(tag) and String.starts_with?(selector, ["*", "|"]),
            do: Map.put(part, :explicit_type, true),
            else: part

        case parse_selector_modifiers(modifiers, part, context) do
          {:ok, part} ->
            part =
              if featureless_eligible?(part, :host_eligible),
                do: Map.put(part, :host_eligible, true),
                else: part

            part =
              if Map.get(context, :part_argument, false),
                do: Map.put(part, :part_eligible, part_eligible?(part)),
                else: part

            part =
              if featureless_eligible?(part, :scope_eligible),
                do: Map.put(part, :scope_eligible, true),
                else: part

            {:ok,
             %{
               part
               | classes: Enum.reverse(part.classes),
                 attributes: Enum.reverse(part.attributes),
                 pseudo_classes: Enum.reverse(part.pseudo_classes),
                 negations: Enum.reverse(part.negations)
             }}

          error ->
            error
        end

      :error ->
        {:error, :invalid_css}
    end
  end

  # Element-backed parts expose states, but not their internal tree or element features.
  defp part_eligible?(part) do
    is_nil(part.tag) and is_nil(part.id) and part.classes == [] and part.attributes == [] and
      not Map.get(part, :explicit_type, false) and not Map.has_key?(part, :shadow_selector) and
      Enum.all?(part.pseudo_classes, fn pseudo ->
        case pseudo do
          {kind, _}
          when kind in [:has, :nth_child, :nth_last_child, :nth_of_type, :nth_last_of_type] ->
            false

          {_kind, _index, _selectors} ->
            false

          kind
          when kind in [
                 :empty,
                 :root,
                 :scope,
                 :first_child,
                 :last_child,
                 :first_of_type,
                 :last_of_type,
                 :only_child,
                 :only_of_type
               ] ->
            false

          _ ->
            true
        end
      end)
  end

  defp featureless_eligible?(part, key) do
    logical = fn selectors ->
      Enum.any?(selectors, &Map.get(List.last(&1.parts), key, false))
    end

    positive =
      (key == :host_eligible and
         match?({kind, _} when kind in [:host, :host_context], Map.get(part, :shadow_selector))) or
        (key == :scope_eligible and :scope in part.pseudo_classes) or
        Enum.any?(part.pseudo_classes, fn pseudo ->
          case pseudo do
            {kind, selectors} when kind in [:is, :where, :not] -> logical.(selectors)
            _ -> false
          end
        end) or
        Enum.any?(part.negations, &Map.get(&1, key, false))

    positive and is_nil(part.tag) and is_nil(part.id) and part.classes == [] and
      part.attributes == [] and not Map.get(part, :explicit_type, false) and
      Enum.all?(part.negations, &Map.get(&1, key, false)) and
      Enum.all?(part.pseudo_classes, fn pseudo ->
        case pseudo do
          {:has, _} -> true
          :scope -> key == :scope_eligible
          {kind, selectors} when kind in [:is, :where, :not] -> logical.(selectors)
          _ -> false
        end
      end)
  end

  defp qualified_name(source, context) do
    parsed =
      case source do
        <<"*", rest::binary>> -> {:ok, "*", rest}
        <<"|", _::binary>> -> {:ok, "", source}
        <<prefix, _::binary>> when prefix in [?#, ?., ?[, ?:] -> {:ok, nil, source}
        _ -> HtmlValidator.parse_css_identifier(source)
      end

    namespaces = Map.get(context, :namespaces, %{})

    case parsed do
      {:ok, tag, <<"|=", _::binary>> = rest} ->
        {:ok, :any, tag, rest}

      {:ok, prefix, <<"|", local::binary>>} ->
        namespace =
          case prefix do
            "*" -> :any
            "" -> nil
            _ -> Map.get(namespaces, prefix, :unknown)
          end

        name =
          case local do
            <<"*", rest::binary>> -> {:ok, nil, rest}
            _ -> HtmlValidator.parse_css_identifier(local)
          end

        case {namespace, name} do
          {:unknown, _} -> :error
          {namespace, {:ok, tag, rest}} -> {:ok, namespace, tag, rest}
          _ -> :error
        end

      {:ok, tag, rest} ->
        namespace =
          if is_nil(tag) and Map.get(context, :logical_argument, false),
            do: :any,
            else: Map.get(namespaces, "", :any)

        {:ok, namespace, if(tag == "*", do: nil, else: tag), rest}

      :error ->
        :error
    end
  end

  defp parse_selector_modifiers(source, part, context) do
    HtmlValidator.reserve_render_resource(:max_work, byte_size(source), :css)

    case source do
      "" ->
        {:ok, part}

      <<prefix, name::binary>> when prefix in [?#, ?.] ->
        case HtmlValidator.parse_css_identifier(name) do
          {:ok, name, rest} ->
            case {prefix, part.id} do
              {?#, nil} ->
                parse_selector_modifiers(rest, %{part | id: name}, context)

              {?#, _} ->
                parse_selector_modifiers(
                  rest,
                  Map.update(part, :extra_ids, [name], &[name | &1]),
                  context
                )

              {?., _} ->
                parse_selector_modifiers(rest, %{part | classes: [name | part.classes]}, context)
            end

          :error ->
            {:error, :invalid_css}
        end

      <<"[", body::binary>> ->
        with {:ok, namespace, name, rest} <- attribute_name(body, context),
             {:ok, attribute, rest} <-
               HtmlValidator.parse_css_attribute_selector("[" <> name <> rest) do
          attribute = if namespace == nil, do: attribute, else: {:namespace, namespace, attribute}

          parse_selector_modifiers(
            rest,
            %{part | attributes: [attribute | part.attributes]},
            context
          )
        else
          _ -> {:error, :invalid_css}
        end

      <<":", name::binary>> ->
        double = String.starts_with?(name, ":")
        name = if double, do: binary_part(name, 1, byte_size(name) - 1), else: name

        case HtmlValidator.parse_css_identifier(name) do
          {:ok, keyword, rest} ->
            keyword = String.downcase(keyword, :ascii)

            cond do
              double and keyword in ["slotted", "part"] ->
                shadow_function(source, keyword, part, context)

              double or keyword in ["before", "after", "first-line", "first-letter"] ->
                case NativeElixirPdfUtilities.Validators.CssPseudoValidator.parse(keyword, rest) do
                  {:ok, kind} -> {:ok, %{part | pseudo_element: kind}}
                  :error -> {:error, :invalid_css}
                end

              keyword in ["host", "host-context"] ->
                shadow_function(source, keyword, part, context)

              keyword in ["is", "where", "not", "has"] ->
                logical_function(source, keyword, part, context)

              keyword in ["nth-child", "nth-last-child", "nth-of-type", "nth-last-of-type"] ->
                nth_function(source, keyword, part, context)

              keyword == "dir" ->
                case CssSyntaxValidator.function(source, keyword) do
                  {:ok, argument, rest} ->
                    case HtmlValidator.parse_css_identifier(
                           HtmlValidator.trim_css_whitespace(argument)
                         ) do
                      {:ok, direction, ""} ->
                        add_pseudo(
                          rest,
                          part,
                          {:dir, String.downcase(direction, :ascii)},
                          context
                        )

                      _ ->
                        {:error, :invalid_css}
                    end

                  _ ->
                    {:error, :invalid_css}
                end

              keyword == "lang" ->
                case HtmlValidator.parse_css_language_selector(source) do
                  {:ok, pseudo, rest} ->
                    add_pseudo(rest, Map.put(part, :language_dependent, true), pseudo, context)

                  _ ->
                    {:error, :invalid_css}
                end

              keyword in @initial_states ->
                add_pseudo(rest, part, Map.fetch!(@state_atoms, keyword), context)

              true ->
                case HtmlValidator.parse_css_simple_pseudo_class(source) do
                  {:ok, pseudo, rest} -> add_pseudo(rest, part, pseudo, context)
                  _ -> {:error, :invalid_css}
                end
            end

          _ ->
            {:error, :invalid_css}
        end

      _ ->
        {:error, :invalid_css}
    end
  end

  defp attribute_name(source, context) do
    source = HtmlValidator.trim_ascii_whitespace(source)

    case qualified_name(source, Map.put(context, :logical_argument, true)) do
      {:ok, namespace, name, rest} when is_binary(name) ->
        # Bare attributes never inherit the stylesheet's default namespace.
        explicit =
          String.starts_with?(source, ["|", "*|"]) or
            case HtmlValidator.parse_css_identifier(source) do
              {:ok, _, <<"|", next, _::binary>>} -> next != ?=
              _ -> false
            end

        local =
          case source do
            <<"*|", local::binary>> ->
              local

            <<"|", local::binary>> ->
              local

            _ ->
              case HtmlValidator.parse_css_identifier(source) do
                {:ok, _, <<"|", local::binary>>} when explicit -> local
                _ -> source
              end
          end

        raw_name = binary_part(local, 0, byte_size(local) - byte_size(rest))
        {:ok, if(explicit, do: namespace || :none, else: nil), raw_name, rest}

      _ ->
        :error
    end
  end

  defp add_pseudo(rest, part, pseudo, context) do
    parse_selector_modifiers(
      rest,
      %{part | pseudo_classes: [pseudo | part.pseudo_classes]},
      context
    )
  end

  defp nested_context(context) do
    Map.update(context, :depth, 1, &(&1 + 1)) |> Map.put(:logical_argument, true)
  end

  defp logical_function(source, keyword, part, context) do
    with {:ok, body, rest} <- CssSyntaxValidator.function(source, keyword),
         true <- keyword != "has" or not Map.get(context, :in_has, false) do
      nested = nested_context(context)
      nested = if keyword == "has", do: Map.put(nested, :in_has, true), else: nested

      result =
        body
        |> split_selector_list()
        |> Enum.reduce_while({:ok, []}, fn item, {:ok, selectors} ->
          item = HtmlValidator.trim_css_whitespace(item)

          item =
            if keyword == "has",
              do:
                "*" <> if(String.starts_with?(item, [">", "+", "~"]), do: item, else: " " <> item),
              else: item

          parsed =
            if keyword == "has" and item == "* ",
              do: {:error, :invalid_css},
              else: parse_selector(item, nested)

          case parsed do
            {:ok, selector} ->
              if Enum.any?(
                   selector.parts,
                   &(not is_nil(&1.pseudo_element) or
                       match?(
                         {kind, _} when kind in [:slotted, :part],
                         Map.get(&1, :shadow_selector)
                       ))
                 ),
                 do:
                   if(keyword in ["is", "where"],
                     do: {:cont, {:ok, selectors}},
                     else: {:halt, {:error, :invalid_css}}
                   ),
                 else: {:cont, {:ok, [selector | selectors]}}

            _ ->
              if keyword in ["is", "where"],
                do: {:cont, {:ok, selectors}},
                else: {:halt, {:error, :invalid_css}}
          end
        end)

      case result do
        {:ok, selectors} ->
          selectors = Enum.reverse(selectors)

          part =
            if Enum.any?(selectors, &Map.get(&1, :language_dependent, false)),
              do: Map.put(part, :language_dependent, true),
              else: part

          # Retain the established representation for a single compound :not().
          case {keyword, selectors} do
            {"not", [%{parts: [compound]}]} ->
              parse_selector_modifiers(
                rest,
                %{part | negations: [compound | part.negations]},
                context
              )

            _ ->
              add_pseudo(
                rest,
                part,
                {Map.fetch!(
                   %{"is" => :is, "where" => :where, "not" => :not, "has" => :has},
                   keyword
                 ), selectors},
                context
              )
          end

        error ->
          error
      end
    else
      _ -> {:error, :invalid_css}
    end
  end

  defp nth_function(source, keyword, part, context) do
    with {:ok, body, rest} <- CssSyntaxValidator.function(source, keyword) do
      separator =
        body
        |> CssSyntaxValidator.components()
        |> Enum.find(fn component ->
          component.kind == :token and
            case HtmlValidator.parse_css_identifier(component.text) do
              {:ok, name, ""} -> String.downcase(name, :ascii) == "of"
              _ -> false
            end
        end)

      {expression, filter} =
        if separator do
          ending = separator.offset + separator.length

          {binary_part(body, 0, separator.offset),
           [binary_part(body, ending, byte_size(body) - ending)]}
        else
          {body, []}
        end

      with {:ok, {kind, index}, ""} <-
             HtmlValidator.parse_css_nth_pseudo_class(":" <> keyword <> "(" <> expression <> ")") do
        case filter do
          [] ->
            add_pseudo(rest, part, {kind, index}, context)

          [filter] when kind in [:nth_child, :nth_last_child] ->
            with {:ok, selectors} <- parse(filter, nested_context(context)),
                 false <-
                   Enum.any?(selectors, fn selector ->
                     Enum.any?(selector.parts, &(not is_nil(&1.pseudo_element)))
                   end) do
              part =
                if Enum.any?(selectors, &Map.get(&1, :language_dependent, false)),
                  do: Map.put(part, :language_dependent, true),
                  else: part

              add_pseudo(rest, part, {kind, index, selectors}, context)
            else
              _ -> {:error, :invalid_css}
            end

          _ ->
            {:error, :invalid_css}
        end
      else
        _ -> {:error, :invalid_css}
      end
    else
      _ -> {:error, :invalid_css}
    end
  end

  defp shadow_function(source, keyword, part, context) do
    source =
      if String.starts_with?(source, "::"),
        do: binary_part(source, 1, byte_size(source) - 1),
        else: source

    {:ok, _name, tail} =
      HtmlValidator.parse_css_identifier(binary_part(source, 1, byte_size(source) - 1))

    parsed =
      if keyword == "host" and not String.starts_with?(tail, "("),
        do: {:ok, nil, tail},
        else: CssSyntaxValidator.function(source, keyword)

    case parsed do
      {:ok, nil, rest} ->
        parse_selector_modifiers(rest, Map.put(part, :shadow_selector, {:host, nil}), context)

      {:ok, body, rest} when keyword == "part" ->
        names =
          body
          |> CssSyntaxValidator.components()
          |> Enum.reject(&(&1.kind == :comment or Regex.match?(~r/\A[ \t\n\r\f]+\z/, &1.text)))
          |> Enum.map(& &1.text)

        if names != [] and
             Enum.all?(names, &match?({:ok, _, ""}, HtmlValidator.parse_css_identifier(&1))) do
          names =
            Enum.map(names, fn name ->
              {:ok, decoded, ""} = HtmlValidator.parse_css_identifier(name)
              decoded
            end)

          shadow_tail(rest, Map.put(part, :shadow_selector, {:part, names}), context)
        else
          {:error, :invalid_css}
        end

      {:ok, body, rest} ->
        case parse_simple_selector(
               HtmlValidator.trim_css_whitespace(body),
               nested_context(context)
             ) do
          {:ok, %{pseudo_element: nil} = compound} ->
            part =
              if Map.get(compound, :language_dependent, false),
                do: Map.put(part, :language_dependent, true),
                else: part

            if Map.has_key?(compound, :shadow_selector),
              do: {:error, :invalid_css},
              else:
                shadow_tail(
                  rest,
                  Map.put(
                    part,
                    :shadow_selector,
                    {Map.fetch!(
                       %{"host" => :host, "host-context" => :host_context, "slotted" => :slotted},
                       keyword
                     ), compound}
                  ),
                  context
                )

          _ ->
            {:error, :invalid_css}
        end

      _ ->
        {:error, :invalid_css}
    end
  end

  defp shadow_tail(source, part, context) do
    case {Map.get(part, :shadow_selector), source} do
      {{:part, _}, <<":", _::binary>>} ->
        with {:ok, tail} <-
               parse_simple_selector(
                 source,
                 Map.put(nested_context(context), :part_argument, true)
               ),
             true <-
               is_nil(tail.id) and tail.classes == [] and tail.attributes == [] and
                 not Map.has_key?(tail, :shadow_selector) do
          part = Map.put(part, :shadow_tail, %{tail | pseudo_element: nil})

          part =
            if Map.get(tail, :language_dependent, false),
              do: Map.put(part, :language_dependent, true),
              else: part

          {:ok, %{part | pseudo_element: tail.pseudo_element}}
        else
          _ -> {:error, :invalid_css}
        end

      {{kind, _}, rest} when kind in [:part, :slotted] ->
        if rest == "" or String.starts_with?(rest, "::"),
          do: parse_selector_modifiers(rest, part, context),
          else: {:error, :invalid_css}

      _ ->
        parse_selector_modifiers(source, part, context)
    end
  end

  defp specificity(parts) do
    Enum.reduce(parts, {0, 0, 0}, fn part, total ->
      own =
        {if(part.id, do: 1, else: 0) + length(Map.get(part, :extra_ids, [])),
         length(part.classes) + length(part.attributes),
         if(part.tag, do: 1, else: 0) +
           NativeElixirPdfUtilities.Validators.CssPseudoValidator.specificity(part.pseudo_element)}

      shadow =
        case Map.get(part, :shadow_selector) do
          {:host, nil} ->
            {0, 1, 0}

          {kind, compound} when kind in [:host, :host_context] ->
            add_specificity({0, 1, 0}, specificity([compound]))

          {:slotted, compound} ->
            add_specificity({0, 0, 1}, specificity([compound]))

          {:part, _} ->
            {0, 0, 1}

          nil ->
            {0, 0, 0}
        end

      pseudos =
        Enum.reduce(part.pseudo_classes, {0, 0, 0}, fn pseudo, sum ->
          contribution =
            case pseudo do
              {:where, _} ->
                {0, 0, 0}

              {kind, selectors} when kind in [:is, :not, :has] ->
                maximum_specificity(selectors)

              {_kind, _index, selectors} ->
                add_specificity({0, 1, 0}, maximum_specificity(selectors))

              _ ->
                {0, 1, 0}
            end

          add_specificity(sum, contribution)
        end)

      total
      |> add_specificity(own)
      |> add_specificity(shadow)
      |> add_specificity(specificity(List.wrap(Map.get(part, :shadow_tail))))
      |> add_specificity(pseudos)
      |> add_specificity(specificity(part.negations))
    end)
  end

  defp maximum_specificity(selectors),
    do: Enum.reduce(selectors, {0, 0, 0}, &max(&1.specificity, &2))

  defp add_specificity({a, b, c}, {x, y, z}), do: {a + x, b + y, c + z}
end
