defmodule NativeElixirPdfUtilities.Validators.CssSubstitutionValidator do
  @moduledoc "Static typed attributes and environment substitution without resource authority."
  alias NativeElixirPdfUtilities.Validators.{
    CssMathValidator,
    CssPropertyRegistrationValidator,
    CssReferenceValidator,
    CssSyntaxValidator,
    HtmlValidator
  }

  @doc "Recognizes element/environment substitutions, including escaped function names."
  @spec contains?(String.t()) :: boolean()
  def contains?(value) do
    :binary.match(value, "(") != :nomatch and
      Regex.match?(~r/(?:attr|env|\\)/i, value) and
      component_contains?(CssSyntaxValidator.components(value))
  end

  defp component_contains?(components) do
    Enum.any?(components, fn component ->
      case component do
        %{kind: :block, opener: opener, children: children} ->
          case HtmlValidator.parse_css_identifier(opener) do
            {:ok, name, "("} ->
              String.downcase(name, :ascii) in ["attr", "env"] or component_contains?(children)

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
  @spec resolve_value(String.t(), map(), :atomics.atomics_ref()) ::
          {:ok, String.t()} | :error | {:error, {atom(), map()}}
  def resolve_value(value, context, budget) do
    substituted? = contains?(value)

    prepared =
      if substituted?,
        do: resolve(value, Map.put(context, :_css_budget, budget)),
        else: {:ok, value}

    with {:ok, source} <- prepared,
         {:ok, resolved} <-
           HtmlValidator.resolve_css_variables(
             source,
             Map.get(context, :_custom_properties, %{}),
             %{},
             budget
           ) do
      if resolved == source,
        do: {:ok, resolved},
        else: resolve(resolved, Map.put(context, :_css_budget, budget))
    end
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
        case resolve(value, context) do
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
                {:ok, name, "("} -> String.downcase(name, :ascii)
                _ -> nil
              end

            case name do
              "attr" ->
                attribute(body, context, depth + 1)

              "env" ->
                environment(body, context, depth + 1)

              # Variable fallbacks are conditional. The shared variable resolver selects them first.
              "var" ->
                {:ok, component.text}

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
