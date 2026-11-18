defmodule NativeElixirPdfUtilities.Validators.CssSubstitutionValidator do
  @moduledoc "Static typed attributes and environment substitution without resource authority."
  alias NativeElixirPdfUtilities.Validators.{
    CssMathValidator,
    CssConditionalValueValidator,
    CssFunctionValidator,
    CssStaticNumericValidator,
    CssValueValidator,
    CssCascadeValidator,
    CssPropertyRegistrationValidator,
    CssReferenceValidator,
    CssSyntaxValidator,
    HtmlValidator
  }

  @static ~w(if first-valid inherit sibling-index sibling-count ident cycle random-item progress calc-mix transform-mix calc-interpolate color-interpolate transform-interpolate interpolate random)
  @substitutions ~w(attr env control-value) ++ @static

  @doc "Recognizes element/environment substitutions, including escaped function names."
  @spec contains?(String.t()) :: boolean()
  def contains?(value) do
    :binary.match(value, "(") != :nomatch and
      Regex.match?(
        ~r/(?:attr|env|control-value|if|first-valid|inherit|sibling-|ident|cycle|random|progress|(?:calc|transform)-mix|interpolate|--|\\)/i,
        value
      ) and
      component_contains?(CssSyntaxValidator.components(value))
  end

  defp component_contains?(components) do
    Enum.any?(components, fn component ->
      case component do
        %{kind: :block, opener: opener, children: children} ->
          case HtmlValidator.parse_css_identifier(opener) do
            {:ok, name, "("} ->
              String.downcase(name, :ascii) in @substitutions or String.starts_with?(name, "--") or
                component_contains?(children)

            _ ->
              component_contains?(children)
          end

        _ ->
          false
      end
    end)
  end

  @doc "Substitutes selected static values; missing typed values are invalid at computed-value time."
  @spec resolve(String.t() | nil, map()) ::
          {:ok, String.t() | nil} | :error | {:error, {atom(), map()}}
  def resolve(value, context) do
    if is_binary(value) and contains?(value) do
      with {:ok, value} <- substitute(value, context, 0),
           do: {:ok, HtmlValidator.normalize_css_comments(value)}
    else
      {:ok, value}
    end
  end

  @doc "Resolves arbitrary substitutions and variables with one shared expansion budget."
  @spec resolve_value(String.t(), map(), :atomics.atomics_ref(), term()) ::
          {:ok, String.t()} | :error | {:error, {atom(), map()}}
  def resolve_value(value, context, budget, declaration \\ nil) do
    if contains?(value) or (is_map(declaration) and is_map_key(declaration, :css_locals)) do
      with {:ok, context} <- declaration_context(context, budget, declaration),
           do: resolve_nested(value, context, 0)
    else
      with {:ok, resolved} <-
             HtmlValidator.resolve_css_variables(
               value,
               Map.get(context, :_custom_properties, %{}),
               %{},
               budget
             ) do
        if resolved != value and contains?(resolved) do
          with {:ok, context} <- declaration_context(context, budget, declaration),
               do: resolve_nested(resolved, context, 0)
        else
          {:ok, resolved}
        end
      end
    end
  end

  defp declaration_context(context, budget, declaration) do
    context =
      case CssValueValidator.declaration_pair(declaration) do
        {property, _} ->
          context = Map.put(context, :_css_property, property)

          if is_map(declaration) do
            Map.merge(context, %{
              _css_definition_scope: Map.get(declaration, :definition_scope),
              _css_media_aliases: Map.get(declaration, :css_media_aliases, %{}),
              _css_locals: Map.get(declaration, :css_locals, %{})
            })
          else
            context
          end

        nil ->
          context
      end

    context = Map.put(context, :_css_budget, budget)

    if Map.get(context, :_css_locals, %{}) == %{},
      do: {:ok, context},
      else: prepare_locals(context, budget)
  end

  @doc "Binds custom-property attributes to their declaring element before inheritance."
  @spec prepare_properties(map(), map(), :atomics.atomics_ref()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def prepare_properties(properties, context, budget) do
    selected =
      if Enum.any?(properties, fn {_, value} -> is_binary(value) and contains?(value) end),
        do: HtmlValidator.invalidate_custom_property_cycles(properties),
        else: {:ok, properties}

    with {:ok, properties} <- selected do
      context =
        context |> Map.put(:_custom_properties, properties) |> Map.put(:_css_budget, budget)

      Enum.reduce_while(properties, {:ok, properties}, fn {name, value}, {:ok, prepared} ->
        declaration = get_in(context, [:_css_custom_declarations, name])

        result =
          if declaration && elem(declaration.declaration, 1) == value,
            do: resolve_value(value, context, budget, declaration),
            else: resolve(value, context)

        case result do
          {:ok, ^value} -> {:cont, {:ok, prepared}}
          {:ok, value} -> {:cont, {:ok, Map.put(prepared, name, value)}}
          :error -> {:cont, {:ok, Map.put(prepared, name, nil)}}
          error -> {:halt, error}
        end
      end)
    end
  end

  @doc "Finalizes substitutions selected by a variable fallback before custom-property inheritance."
  @spec resolve_custom_value(String.t() | nil, map(), :atomics.atomics_ref()) ::
          {:ok, String.t() | nil} | {:error, {atom(), map()}}
  def resolve_custom_value(value, context, budget) do
    case resolve(value, Map.put(context, :_css_budget, budget)) do
      :error -> {:ok, nil}
      result -> result
    end
  end

  defp substitute(value, context, depth) do
    HtmlValidator.check_render_resource(:max_depth, depth, :css)
    HtmlValidator.check_render_resource(:max_input_bytes, byte_size(value), :css)

    value
    |> CssSyntaxValidator.components()
    |> Enum.reduce_while({:ok, []}, fn component, {:ok, result} ->
      resolved =
        case component do
          %{kind: :block, opener: opener, closed: true} ->
            body =
              binary_part(
                component.text,
                byte_size(opener),
                component.length - byte_size(opener) - 1
              )

            name =
              case HtmlValidator.parse_css_identifier(opener) do
                {:ok, "--" <> _ = name, "("} -> name
                {:ok, name, "("} -> String.downcase(name, :ascii)
                _ -> nil
              end

            case name do
              "attr" ->
                attribute(body, context, depth + 1)

              "env" ->
                environment(body, context, depth + 1)

              "control-value" ->
                control_value(body, context)

              "--" <> _ ->
                indexed =
                  Map.update(
                    context,
                    :_css_random_index,
                    [component.offset],
                    &[component.offset | &1]
                  )

                with {:ok, resolved} <- call(name, body, indexed, depth + 1),
                     do: {:ok, "/**/" <> resolved <> "/**/"}

              name when name in @static ->
                indexed =
                  Map.update(
                    context,
                    :_css_random_index,
                    [component.offset],
                    &[component.offset | &1]
                  )

                with {:ok, resolved} <-
                       static(
                         name,
                         body,
                         indexed,
                         depth + 1,
                         component.text == String.trim(value)
                       ),
                     do: {:ok, "/**/" <> resolved <> "/**/"}

              "var" ->
                if Map.get(context, :_css_nested, false) do
                  [name | fallback] = CssSyntaxValidator.split_trimmed(body, ",")

                  with {:ok, "--" <> _ = name, ""} <- HtmlValidator.parse_css_identifier(name) do
                    budget = Map.get_lazy(context, :_css_budget, &HtmlValidator.new_css_budget/0)

                    resolved =
                      HtmlValidator.resolve_css_variables(
                        "var(" <> name <> ")",
                        Map.get(context, :_custom_properties, %{}),
                        %{},
                        budget
                      )

                    case resolved do
                      {:ok, value} ->
                        resolve_nested(value, context, depth + 1)

                      :error when fallback != [] ->
                        resolve_nested(Enum.join(fallback, ","), context, depth + 1)

                      error ->
                        error
                    end
                  else
                    _ -> :error
                  end
                  |> case do
                    {:ok, value} -> {:ok, "/**/" <> value <> "/**/"}
                    error -> error
                  end
                else
                  {:ok, component.text}
                end

              _ ->
                with {:ok, body} <- substitute(body, context, depth + 1),
                     do:
                       {:ok,
                        opener <> body <> binary_part(component.text, component.length - 1, 1)}
            end

          _ ->
            {:ok, component.text}
        end

      case resolved do
        {:ok, text} -> {:cont, {:ok, [text | result]}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, result} ->
        value = result |> Enum.reverse() |> IO.iodata_to_binary()
        HtmlValidator.check_render_resource(:max_input_bytes, byte_size(value), :css)
        {:ok, value}

      error ->
        error
    end
  end

  defp prepare_locals(context, budget) do
    resolved =
      Enum.reduce_while(context._css_locals, {:ok, %{}}, fn {key, value}, {:ok, locals} ->
        case value do
          {:argument, type, source, captured} ->
            result =
              with {:ok, caller} <-
                     if(captured == %{},
                       do: {:ok, context},
                       else: prepare_locals(%{context | _css_locals: captured}, budget)
                     ),
                   {:ok, value} <-
                     resolve_nested(source, Map.put(caller, :_css_budget, budget), 0),
                   {:ok, value} <- CssFunctionValidator.reference_free(value),
                   do: CssFunctionValidator.check_type(value, type)

            case result do
              {:ok, value} -> {:cont, {:ok, Map.put(locals, key, value)}}
              :error -> {:cont, {:ok, Map.put(locals, key, nil)}}
              error -> {:halt, error}
            end

          value ->
            {:cont, {:ok, Map.put(locals, key, value)}}
        end
      end)

    with {:ok, locals} <- resolved,
         values = Map.merge(Map.get(context, :_custom_properties, %{}), locals),
         {:ok, properties} <- HtmlValidator.compute_custom_properties(values, budget) do
      {:ok, Map.put(context, :_custom_properties, properties)}
    end
  end

  defp resolve_nested(source, context, depth) do
    budget = Map.get_lazy(context, :_css_budget, &HtmlValidator.new_css_budget/0)
    properties = Map.get(context, :_custom_properties, %{})

    with {:ok, preprocessed} <- substitute(source, Map.put(context, :_css_nested, true), depth),
         {:ok, resolved} <-
           HtmlValidator.resolve_css_variables(preprocessed, properties, %{}, budget) do
      {:ok, String.trim(HtmlValidator.normalize_css_comments(resolved))}
    end
  end

  defp call(name, source, context, depth) do
    scope = Map.get(context, :_css_definition_scope)
    functions = Map.get(context, :_css_functions, %{})
    function = scoped_function(functions, scope, name, Map.get(context, :_css_scope_parents, %{}))
    stack = Map.get(context, :_css_call_stack, [])
    signature = {if(function, do: Map.get(function, :shadow_scope_id), else: scope), name}

    if function && signature not in stack do
      external = Map.get(context, :_custom_properties, %{})

      with {:ok, arguments} <-
             CssFunctionValidator.bind(function.parameters, source, fn value, type ->
               with {:ok, value} <- resolve_nested(value, context, depth),
                    do: CssFunctionValidator.check_type(value, type)
             end),
           {:ok, locals} <-
             CssFunctionValidator.body(
               function.body,
               Map.get(context, :_position_viewport),
               function.media_aliases
             ),
           {result, locals} when is_binary(result) <- Map.pop(locals, "result") do
        locals =
          Map.new(locals, fn {key, value} ->
            value =
              case CssCascadeValidator.keyword(value) do
                "initial" -> Map.get(arguments, key)
                "inherit" -> Map.get(external, key)
                _ -> value
              end

            {key, value}
          end)

        function_context =
          Map.merge(context, %{
            _css_call_stack: [signature | stack],
            _css_definition_scope: Map.get(function, :shadow_scope_id),
            _css_media_aliases: function.media_aliases,
            _css_property: "result",
            _css_function_type: function.type,
            _cascade_inherited: %{_custom_properties: external},
            _custom_properties: external |> Map.merge(arguments) |> Map.merge(locals)
          })

        with {:ok, value} <- resolve_nested(result, function_context, depth),
             {:ok, value} <- CssFunctionValidator.check_type(value, function.type),
             do: CssFunctionValidator.reference_free(value)
      else
        {:error, _} = error -> error
        _ -> :error
      end
    else
      :error
    end
  end

  defp scoped_function(functions, scope, name, parents) do
    case Map.get(functions, {scope, name}) do
      nil when not is_nil(scope) ->
        scoped_function(functions, Map.get(parents, scope), name, parents)

      definition ->
        definition
    end
  end

  defp visible_scope?(target, definition, parents) do
    cond do
      target == definition -> true
      is_nil(definition) -> false
      true -> visible_scope?(target, Map.get(parents, definition), parents)
    end
  end

  defp static(name, source, context, depth, whole?) do
    case name do
      "if" ->
        with {:ok, branches} <- CssConditionalValueValidator.branches(source) do
          Enum.reduce_while(branches, {:ok, ""}, fn {test, branch}, acc ->
            case resolve_nested(test, context, depth) do
              {:ok, test} ->
                properties =
                  Map.delete(
                    Map.get(context, :_custom_properties, %{}),
                    Map.get(context, :_css_property)
                  )

                if String.downcase(test, :ascii) == "else" or
                     CssConditionalValueValidator.matches?(
                       test,
                       Map.get(context, :_position_viewport),
                       Map.get(context, :_css_media_aliases, %{}),
                       properties
                     ),
                   do: {:halt, resolve_nested(branch, context, depth)},
                   else: {:cont, acc}

              {:error, _} = error ->
                {:halt, error}

              :error ->
                {:cont, acc}
            end
          end)
        end

      "first-valid" when whole? ->
        Enum.reduce_while(CssSyntaxValidator.split_trimmed(source, ","), :error, fn branch,
                                                                                    :error ->
          with {:ok, branch} <- resolve_nested(branch, context, depth),
               {:ok, _} <- property_value(branch, context) do
            {:halt, {:ok, branch}}
          else
            {:error, {:resource_limit_exceeded, _}} = error -> {:halt, error}
            _ -> {:cont, :error}
          end
        end)

      "inherit" ->
        inherited(source, context, depth)

      name when name in ["sibling-index", "sibling-count"] ->
        if String.trim(source) == "" do
          {index, count} =
            if visible_scope?(
                 Map.get(context, :_css_node_scope),
                 Map.get(context, :_css_definition_scope),
                 Map.get(context, :_css_scope_parents, %{})
               ),
               do: Map.get(context, :_css_siblings, {1, 1}),
               else: {0, 0}

          {:ok, Integer.to_string(if(name == "sibling-index", do: index, else: count))}
        else
          :error
        end

      "ident" ->
        with {:ok, value} <- resolve_nested(source, context, depth),
             do: CssStaticNumericValidator.identifier(value)

      "cycle" when whole? ->
        cycle(source, context, depth)

      name when name in ["first-valid", "cycle"] ->
        :error

      "random-item" ->
        case CssSyntaxValidator.split_trimmed(source, ",") do
          [key, _ | _] = arguments ->
            with {:ok, key} <- resolve_nested(key, context, depth),
                 {:ok, fraction} <- CssStaticNumericValidator.random_base(key, context),
                 choices = tl(arguments),
                 chosen = Enum.at(choices, trunc(fraction * length(choices))),
                 do: resolve_nested(chosen, context, depth)

          _ ->
            :error
        end

      _ ->
        with {:ok, value} <- resolve_nested(source, context, depth),
             do: CssStaticNumericValidator.evaluate(name, value, context, whole?)
    end
  end

  defp property_value(source, context) do
    case Map.get(context, :_css_property) do
      nil ->
        :error

      "--" <> _ ->
        {:ok, source}

      "result" ->
        CssFunctionValidator.check_type(source, Map.get(context, :_css_function_type, :any))

      property ->
        case CssValueValidator.validate_resolved(context, property, source) do
          {:error, :invalid_document} -> :error
          result -> result
        end
    end
  end

  defp inherited(source, context, depth) do
    [header | fallback] = CssSyntaxValidator.split_trimmed(source, ",")
    inherited = get_in(context, [:_cascade_inherited, :_custom_properties]) || %{}

    with {:ok, name} <- resolve_nested(header, context, depth),
         {:ok, "--" <> _ = name, ""} <- HtmlValidator.parse_css_identifier(name) do
      case Map.get(inherited, name) do
        value when is_binary(value) -> {:ok, value}
        _ when fallback != [] -> resolve_nested(Enum.join(fallback, ","), context, depth)
        _ -> :error
      end
    else
      {:error, _} = error -> error
      _ -> :error
    end
  end

  defp cycle(source, context, depth) do
    property = Map.get(context, :_css_property)

    if is_binary(property) and Map.has_key?(context, :_cascade_inherited) and
         not Map.get(context, :_css_in_cycle, false) do
      inherited = context._cascade_inherited
      context = Map.put(context, :_css_in_cycle, true)

      CssSyntaxValidator.split_trimmed(source, ",")
      |> Enum.reduce_while({:ok, []}, fn item, {:ok, options} ->
        with {:ok, value} <- resolve_nested(item, context, depth),
             {:ok, computed} <- property_value(value, context) do
          equal =
            if String.starts_with?(property, "--") do
              Map.get(Map.get(inherited, :_custom_properties, %{}), property) == computed
            else
              targets = CssCascadeValidator.targets(property, context)
              CssCascadeValidator.project(inherited, computed, targets) == inherited
            end

          {:cont, {:ok, [{value, equal} | options]}}
        else
          error -> {:halt, error}
        end
      end)
      |> case do
        {:ok, options} ->
          options = Enum.reverse(options)
          first = options |> hd() |> elem(0)

          case Enum.drop_while(options, fn {_, equal} -> not equal end) do
            [_, {next, _} | _] -> {:ok, next}
            _ -> {:ok, first}
          end

        error ->
          error
      end
    else
      :error
    end
  end

  defp attribute(body, context, depth) do
    [header | fallback] = HtmlValidator.split_css_comma(body, true)

    with {:ok, name, type} <- attribute_header(header) do
      name =
        if Map.get(context, :_css_attribute_case, :html) == :html,
          do: String.downcase(name, :ascii),
          else: name

      value = Map.get(Map.get(context, :_css_attributes, %{}), name)
      parsed = if is_binary(value), do: typed_attribute(value, type), else: :error

      result =
        case parsed do
          {:ok, _} ->
            parsed

          :error ->
            cond do
              fallback != [] -> substitute(Enum.join(fallback, ","), context, depth)
              type == :raw_string -> {:ok, "\"\""}
              true -> :error
            end
        end

      # Every supported URL value remains a token or function. Attribute results
      # cannot contain one, even through a universal type or a nested fallback.
      # Strings stay opaque; the renderer has no string-to-URL conversion function.
      with {:ok, value} <- result,
           {:ok, value} <-
             HtmlValidator.resolve_css_variables(
               value,
               Map.get(context, :_custom_properties, %{}),
               %{},
               Map.get_lazy(context, :_css_budget, &HtmlValidator.new_css_budget/0)
             ),
           {:ok, value} <- substitute(value, context, depth),
           {:ok, []} <- CssReferenceValidator.scan(value) do
        {:ok, "/**/" <> value <> "/**/"}
      else
        {:error, {:resource_limit_exceeded, _}} = error -> error
        _ -> :error
      end
    else
      _ -> :error
    end
  end

  defp attribute_header(header) do
    case HtmlValidator.split_css_tokens(header) do
      [name | types] when length(types) <= 1 ->
        with {:ok, name, ""} <- HtmlValidator.parse_css_identifier(name),
             {:ok, type} <- attribute_type(List.first(types) || "raw-string"),
             do: {:ok, name, type}

      _ ->
        :error
    end
  end

  defp attribute_type(source) do
    case HtmlValidator.parse_css_identifier(source) do
      {:ok, name, ""} ->
        case String.downcase(name, :ascii) do
          "raw-string" -> {:ok, :raw_string}
          "number" -> {:ok, :number}
          unit -> attribute_unit(unit)
        end

      {:ok, name, "(" <> rest} ->
        if String.downcase(name, :ascii) == "type" and String.ends_with?(rest, ")") do
          source = binary_part(rest, 0, byte_size(rest) - 1)

          with {:ok, syntax} <- CssPropertyRegistrationValidator.syntax(quote_string(source)),
               false <- contains_url_type?(syntax),
               do: {:ok, {:syntax, syntax}},
               else: (_ -> :error)
        else
          :error
        end

      _ ->
        if source == "%", do: attribute_unit(source), else: :error
    end
  end

  defp attribute_unit(unit) do
    with {:ok, _, dimension} <-
           CssMathValidator.evaluate("1" <> unit, %{type_check: true, width: 1, height: 1}),
         true <-
           dimension in [
             %{length: 1},
             %{angle: 1},
             %{time: 1},
             %{frequency: 1},
             %{resolution: 1},
             %{percentage: 1}
           ],
         do: {:ok, {:unit, unit}},
         else: (_ -> :error)
  end

  defp typed_attribute(value, type) do
    case type do
      :raw_string ->
        {:ok, quote_string(value)}

      :number ->
        with {:ok, _} <- CssMathValidator.prepare_scalar(String.trim(value), :number),
             do: {:ok, String.trim(value)}

      {:unit, unit} ->
        with {number, ""} <- HtmlValidator.parse_css_number(String.trim(value)),
             do: {:ok, to_string(number) <> unit},
             else: (_ -> :error)

      {:syntax, syntax} ->
        if CssPropertyRegistrationValidator.matches_syntax?(syntax, String.trim(value)),
          do:
            {:ok,
             value
             |> CssSyntaxValidator.components()
             |> Enum.map_join(&CssSyntaxValidator.serialize/1)},
          else: :error
    end
  end

  defp contains_url_type?(syntax) do
    syntax != :any and Enum.any?(syntax, fn {_, type, _} -> type in ["url", "image"] end)
  end

  defp control_value(body, context) do
    value = Map.get(context, :_css_control_value, "")

    case String.trim(body) do
      type when type in ["", "<string>"] ->
        {:ok, quote_string(value)}

      "<number>" ->
        case HtmlValidator.parse_css_number(value) do
          {_number, ""} -> {:ok, "/**/" <> value <> "/**/"}
          _ -> :error
        end

      _ ->
        :error
    end
  end

  defp environment(body, context, depth) do
    [header | fallback] = HtmlValidator.split_css_comma(body, true)

    with {:ok, name, indices} <- environment_header(header) do
      cond do
        indices == [] and
            name in ~w(safe-area-inset-top safe-area-inset-right safe-area-inset-bottom safe-area-inset-left safe-area-max-inset-top safe-area-max-inset-right safe-area-max-inset-bottom safe-area-max-inset-left) ->
          {:ok, "0px"}

        indices == [] and name == "preferred-text-scale" ->
          {:ok, "1"}

        fallback != [] ->
          substitute(Enum.join(fallback, ","), context, depth)

        true ->
          :error
      end
    else
      _ -> :error
    end
  end

  defp environment_header(header) do
    case HtmlValidator.split_css_tokens(header) do
      [name | indices] ->
        with {:ok, name, ""} <- HtmlValidator.parse_css_identifier(name),
             nil <- NativeElixirPdfUtilities.Validators.CssCascadeValidator.keyword(name),
             false <- String.downcase(name, :ascii) == "default",
             true <-
               Enum.all?(indices, fn value ->
                 Regex.match?(~r/\A[+]?[0-9]+\z/, value) and
                   match?({_, ""}, HtmlValidator.parse_css_number(value))
               end),
             do: {:ok, name, indices},
             else: (_ -> :error)

      _ ->
        :error
    end
  end

  @doc "Checks arbitrary-substitution syntax without an element, measured values or resource authorization."
  @spec valid_syntax?(String.t()) :: boolean()
  def valid_syntax?(value) do
    components = CssSyntaxValidator.components(value)
    not CssSyntaxValidator.invalid_value?(components) and valid_components?(components)
  end

  defp valid_components?(components) do
    Enum.all?(components, fn component ->
      case component do
        %{kind: :block, opener: opener, closed: true, children: children} ->
          body =
            binary_part(
              component.text,
              byte_size(opener),
              component.length - byte_size(opener) - 1
            )

          [header | _] = HtmlValidator.split_css_comma(body, true)

          valid =
            case HtmlValidator.parse_css_identifier(opener) do
              {:ok, name, "("} ->
                case String.downcase(name, :ascii) do
                  "attr" -> match?({:ok, _, _}, attribute_header(header))
                  "env" -> match?({:ok, _, _}, environment_header(header))
                  "control-value" -> String.trim(body) in ["", "<string>", "<number>"]
                  "if" -> match?({:ok, _}, CssConditionalValueValidator.branches(body))
                  name when name in ["sibling-index", "sibling-count"] -> String.trim(body) == ""
                  _ -> true
                end

              _ ->
                true
            end

          valid and valid_components?(children)

        %{kind: :block} ->
          false

        _ ->
          true
      end
    end)
  end

  defp quote_string(value) do
    "\"" <>
      (value
       |> String.replace("\\", "\\\\")
       |> String.replace("\"", "\\\"")
       |> String.replace("\n", "\\a ")
       |> String.replace("\r", "\\d ")
       |> String.replace("\u0000", "�")) <> "\""
  end
end
