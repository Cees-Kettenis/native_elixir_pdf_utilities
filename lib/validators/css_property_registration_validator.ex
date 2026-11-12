defmodule NativeElixirPdfUtilities.Validators.CssPropertyRegistrationValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    CssSyntaxValidator,
    CssMathValidator,
    CssPositionValidator,
    CssTransformValidator,
    CssCascadeValidator
  }

  @types ~w(length number percentage length-percentage color string image url integer angle time resolution
    transform-function transform-list custom-ident)
  @font_units ~w(em ex ch cap ic lh rem rex rch rcap ric rlh)
  @viewport_units ~w(vw vh vi vb vmin vmax svw svh svi svb svmin svmax
    lvw lvh lvi lvb lvmin lvmax dvw dvh dvi dvb dvmin dvmax)

  @doc "Validates declarative registrations; an invalid registration is ignored as a whole."
  @spec prepare([map()]) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(records) do
    Enum.reduce_while(records, {:ok, %{}}, fn record, {:ok, registrations} ->
      descriptors =
        record.declarations
        |> CssSyntaxValidator.declarations()
        |> Enum.reduce(%{}, fn item, descriptors ->
          case item.declaration do
            {"syntax", value} ->
              case syntax(value) do
                {:ok, syntax} -> Map.put(descriptors, :syntax, syntax)
                :error -> descriptors
              end

            {"inherits", value} ->
              case HtmlValidator.parse_css_identifier(value) do
                {:ok, value, ""} ->
                  case String.downcase(value, :ascii) do
                    "true" -> Map.put(descriptors, :inherits, true)
                    "false" -> Map.put(descriptors, :inherits, false)
                    _ -> descriptors
                  end

                _ ->
                  descriptors
              end

            {"initial-value", value} ->
              Map.put(descriptors, :initial, value)

            _ ->
              descriptors
          end
        end)

      name = HtmlValidator.parse_css_identifier(HtmlValidator.trim_css_whitespace(record.name))

      case {name, descriptors} do
        {{:ok, "--" <> suffix = name, ""}, %{syntax: syntax, inherits: _}} when suffix != "" ->
          initial = Map.get(descriptors, :initial)
          {units, _} = if is_binary(initial), do: dependencies(initial), else: {[], false}
          viewport_initial = Enum.any?(units, &(&1 in @viewport_units)) and syntax != :any
          registration = Map.merge(descriptors, %{name: name, initial: initial})

          result =
            cond do
              syntax == :any and is_nil(initial) ->
                {:ok, nil}

              is_nil(initial) ->
                :error

              true ->
                compute(registration, initial, Map.put(initial_context(), :independent, true))
            end

          case result do
            {:ok, computed} ->
              registration =
                if viewport_initial,
                  do: Map.put(registration, :initial_tokens, initial),
                  else: %{registration | initial: computed}

              {:cont, {:ok, Map.put(registrations, name, registration)}}

            {:error, _} = error ->
              {:halt, error}

            :error ->
              {:cont, {:ok, registrations}}
          end

        _ ->
          {:cont, {:ok, registrations}}
      end
    end)
  end

  @doc "Parses the shared CSS typed-value syntax used by registrations and attribute references."
  @spec syntax(String.t()) :: {:ok, term()} | :error
  def syntax(value) do
    case CssSyntaxValidator.components(value) do
      [%{kind: :string, text: value}] ->
        source = value |> HtmlValidator.decode_css_string() |> HtmlValidator.trim_css_whitespace()

        if source == "*" do
          {:ok, :any}
        else
          alternatives =
            String.split(source, "|")
            |> Enum.map(fn component ->
              component = HtmlValidator.trim_css_whitespace(component)

              {component, multiplier} =
                if String.ends_with?(component, ["+", "#"]),
                  do:
                    {binary_part(component, 0, byte_size(component) - 1), String.last(component)},
                  else: {component, nil}

              case component do
                <<"<", rest::binary>> ->
                  type = String.trim_trailing(rest, ">")

                  if String.ends_with?(rest, ">") and type in @types and
                       not (type == "transform-list" and not is_nil(multiplier)),
                     do: {:type, type, multiplier},
                     else: nil

                _ ->
                  case HtmlValidator.parse_css_identifier(component) do
                    {:ok, identifier, ""} ->
                      if CssCascadeValidator.keyword(identifier) ||
                           String.downcase(identifier, :ascii) == "default",
                         do: nil,
                         else: {:literal, identifier, multiplier}

                    _ ->
                      nil
                  end
              end
            end)

          if alternatives != [] and Enum.all?(alternatives, &(&1 != nil)),
            do: {:ok, alternatives},
            else: :error
        end

      _ ->
        :error
    end
  end

  @doc "Checks a typed value without resolving relative geometry or acquiring resources."
  @spec matches_syntax?(term(), String.t()) :: boolean()
  def matches_syntax?(syntax, value) do
    not CssSyntaxValidator.invalid_value?(CssSyntaxValidator.components(value)) and
      match?(
        {:ok, _},
        compute(%{syntax: syntax}, value, %{syntax_only: true, registration_phase: :provisional})
      )
  end

  @doc "Computes a registered value through its declared type using the actual element's metrics."
  @spec compute(map(), String.t() | nil, map()) ::
          {:ok, String.t() | nil} | :error | {:error, {atom(), map()}}
  def compute(registration, value, context) do
    case compute_selected(registration, value, context) do
      {:ok, value, _selected_type} -> {:ok, value}
      result -> result
    end
  end

  # Keep the selected alternative so dependency analysis uses the same type grammar.
  defp compute_selected(registration, value, context) do
    cond do
      is_nil(value) ->
        :error

      registration.syntax == :any ->
        {:ok, value, :any}

      true ->
        Enum.reduce_while(registration.syntax, :error, fn {kind, type, multiplier}, :error ->
          values =
            case multiplier do
              "+" -> HtmlValidator.split_css_tokens(value)
              "#" -> HtmlValidator.split_css_comma(value, true)
              nil -> [value]
            end

          result =
            Enum.reduce_while(values, {:ok, []}, fn value, {:ok, values} ->
              case compute_component(
                     kind,
                     type,
                     HtmlValidator.trim_css_whitespace(value),
                     context
                   ) do
                {:ok, value} -> {:cont, {:ok, [value | values]}}
                result -> {:halt, result}
              end
            end)

          case result do
            {:ok, values} when values != [] ->
              {:halt,
               {:ok,
                values |> Enum.reverse() |> Enum.join(if(multiplier == "#", do: ", ", else: " ")),
                {kind, type, multiplier}}}

            {:error, _} = error ->
              {:halt, error}

            _ ->
              {:cont, :error}
          end
        end)
    end
  end

  @doc "Installs registered defaults and inheritance before authored custom declarations."
  @spec baseline(map()) :: {:ok, map()} | {:error, {atom(), map()}}
  def baseline(context) do
    inherited =
      Map.get(context, :_cascade_inherited, context) |> Map.get(:_custom_properties, %{})

    Enum.reduce_while(Map.get(context, :_property_registrations, %{}), {:ok, context}, fn
      {name, registration}, {:ok, context} ->
        initial =
          if Map.has_key?(registration, :initial_tokens),
            do: compute(registration, registration.initial_tokens, context),
            else: {:ok, registration.initial}

        case initial do
          {:ok, initial} ->
            registration = %{registration | initial: initial}
            value = if registration.inherits, do: Map.get(inherited, name, initial), else: initial

            {:cont,
             {:ok,
              context
              |> Map.update!(:_custom_properties, &Map.put(&1, name, value))
              |> Map.update!(:_property_registrations, &Map.put(&1, name, registration))}}

          error ->
            {:halt, error}
        end
    end)
  end

  @doc "Resolves typed and untyped custom properties through the shared memoized expansion graph."
  @spec resolve(map(), map(), [map()], :atomics.atomics_ref(), atom()) ::
          {:ok, map(), map()} | {:error, {atom(), map()}}
  def resolve(properties, context, declarations, budget, phase) do
    with {:ok, properties} <-
           NativeElixirPdfUtilities.Validators.CssSubstitutionValidator.prepare_properties(
             properties,
             context,
             budget
           ) do
      registrations = Map.get(context, :_property_registrations, %{})

      if registrations == %{} do
        with {:ok, values} <-
               HtmlValidator.compute_custom_properties(properties, budget, fn _name, value ->
                 NativeElixirPdfUtilities.Validators.CssSubstitutionValidator.resolve_custom_value(
                   value,
                   Map.put(context, :_custom_properties, properties),
                   budget
                 )
               end),
             do: {:ok, values, properties}
      else
        context = Map.put(context, :registration_phase, phase)

        transform = fn name, value ->
          with {:ok, value} <-
                 NativeElixirPdfUtilities.Validators.CssSubstitutionValidator.resolve_custom_value(
                   value,
                   Map.put(context, :_custom_properties, properties),
                   budget
                 ),
               do: compute_registered(name, value, registrations, context)
        end

        with {:ok, values} <-
               HtmlValidator.compute_custom_properties(properties, budget, transform),
             {:ok, invalid, metric_context} <-
               if(phase == :provisional,
                 do:
                   dependency_cycles(
                     properties,
                     values,
                     registrations,
                     declarations,
                     context,
                     budget
                   ),
                 else: {:ok, MapSet.new(), context}
               ) do
          if MapSet.size(invalid) == 0 and metric_context == context do
            {:ok, values, properties}
          else
            properties =
              Map.new(properties, fn {name, value} ->
                {name, if(MapSet.member?(invalid, name), do: nil, else: value)}
              end)

            with {:ok, values} <-
                   HtmlValidator.compute_custom_properties(
                     properties,
                     budget,
                     &compute_registered(&1, &2, registrations, metric_context)
                   ),
                 do: {:ok, values, properties}
          end
        end
      end
    end
  end

  defp compute_registered(name, value, registrations, context) do
    case Map.get(registrations, name) do
      nil ->
        {:ok, value}

      registration ->
        fallback =
          if registration.inherits,
            do:
              Map.get(
                Map.get(Map.get(context, :_cascade_inherited, %{}), :_custom_properties, %{}),
                name,
                registration.initial
              ),
            else: registration.initial

        case compute(registration, value, context) do
          :error -> {:ok, fallback}
          result -> result
        end
    end
  end

  defp dependency_cycles(properties, values, registrations, declarations, context, budget) do
    with {:ok, dependent_height, metric_context} <-
           line_height_dependency(declarations, values, context, budget) do
      graph = :digraph.new([:private])

      try do
        Enum.each(
          Map.keys(properties) ++ [:font_size, :line_height, :color],
          &:digraph.add_vertex(graph, &1)
        )

        Enum.each(properties, fn {name, value} ->
          refs =
            if is_nil(value),
              do: [],
              else: elem(HtmlValidator.parse_css_variable_references(value), 1)

          Enum.each(refs, fn reference ->
            Enum.each(reference.dependencies, fn dependency ->
              HtmlValidator.reserve_render_resource(:max_work, 1, :css)

              if Map.has_key?(properties, dependency),
                do: :digraph.add_edge(graph, name, dependency)
            end)
          end)

          if Map.has_key?(registrations, name) and registrations[name].syntax != :any and
               is_binary(values[name]) do
            {units, color} = dependencies(values[name])

            if Enum.any?(units, &(&1 in ~w(em ex ch cap ic))),
              do: :digraph.add_edge(graph, name, :font_size)

            if Enum.any?(units, &(&1 == "lh")), do: :digraph.add_edge(graph, name, :line_height)

            if Map.get(context, :_root_element, false) and
                 Enum.any?(units, &(&1 in ~w(rem rex rch rcap ric))),
               do: :digraph.add_edge(graph, name, :font_size)

            if color do
              {:ok, _value, selected_type} =
                compute_selected(
                  registrations[name],
                  values[name],
                  Map.put(context, :registration_phase, :provisional)
                )

              if match?({:type, "color", _}, selected_type),
                do: :digraph.add_edge(graph, name, :color)
            end
          end
        end)

        Enum.each(declarations, fn declaration ->
          {property, value} =
            NativeElixirPdfUtilities.Validators.CssValueValidator.declaration_pair(declaration)

          target =
            case property do
              "font-size" -> :font_size
              "font" -> :font_size
              "line-height" -> :line_height
              "color" -> :color
              _ -> nil
            end

          if not is_nil(target) and Map.get(declaration, :cascade_targets, [property]) != [] do
            {:ok, refs} = HtmlValidator.parse_css_variable_references(value)

            for reference <- refs,
                dependency <- reference.dependencies,
                do: :digraph.add_edge(graph, target, dependency)
          end
        end)

        if dependent_height, do: :digraph.add_edge(graph, :line_height, :font_size)

        graph
        |> :digraph_utils.cyclic_strong_components()
        |> List.flatten()
        |> Enum.filter(&is_binary/1)
        |> MapSet.new()
        |> then(&{:ok, &1, metric_context})
      after
        :digraph.delete(graph)
      end
    end
  end

  defp line_height_dependency(declarations, values, context, budget) do
    line_height =
      Enum.find(declarations, fn declaration ->
        pair = NativeElixirPdfUtilities.Validators.CssValueValidator.declaration_pair(declaration)

        match?({property, _} when property in ["line-height", "font"], pair) and
          "line-height" in Map.get(declaration, :cascade_targets, ["line-height"])
      end)

    if line_height do
      {property, value} =
        NativeElixirPdfUtilities.Validators.CssValueValidator.declaration_pair(line_height)

      resolved = HtmlValidator.resolve_css_variables(value, values, %{}, budget)
      resolved = if resolved == :error, do: {:ok, "unset"}, else: resolved

      resolved =
        if property == "font" do
          with {:ok, value} <- resolved do
            case NativeElixirPdfUtilities.Validators.CssValueValidator.font_line_height(value) do
              {:error, :invalid_document} -> {:ok, "normal"}
              result -> result
            end
          end
        else
          resolved
        end

      with {:ok, value} <- resolved do
        case NativeElixirPdfUtilities.Validators.CssValueValidator.validate_resolved(
               context,
               "line-height",
               value
             ) do
          {:ok, metric_context} ->
            height = Map.get(metric_context, :line_height)

            height =
              if match?({:rem, _}, height) and not Map.get(context, :_root_element, false),
                do: elem(height, 1) * context._root_font_size,
                else: height

            dependent =
              Map.get(metric_context, :line_height_normal, false) or
                is_number(Map.get(metric_context, :line_height_multiplier)) or
                not is_number(height)

            {:ok, dependent,
             if(dependent, do: context, else: Map.put(metric_context, :line_height, height))}

          _ ->
            {:ok, true, context}
        end
      end
    else
      {:ok, true, context}
    end
  end

  defp dependencies(value) do
    value
    |> CssSyntaxValidator.components()
    |> Enum.reduce({[], false}, fn component, {units, color} ->
      case component do
        %{kind: :block, opener: opener} when opener != "[" ->
          body =
            binary_part(
              component.text,
              byte_size(opener),
              component.length - byte_size(opener) - if(component.closed, do: 1, else: 0)
            )

          {nested, nested_color} = dependencies(body)
          {nested ++ units, color or nested_color}

        %{kind: :token, text: text} ->
          case HtmlValidator.parse_css_number(text) do
            {_, unit} ->
              {[HtmlValidator.normalize_css_unit(unit) | units], color}

            :error ->
              {units,
               color or
                 match?(
                   {:ok, "currentcolor", ""},
                   HtmlValidator.parse_css_identifier(String.downcase(text, :ascii))
                 )}
          end

        _ ->
          {units, color}
      end
    end)
  end

  defp compute_component(kind, type, value, context) do
    {units, color_dependency} = dependencies(value)

    relative =
      kind == :type and
        (Enum.any?(units, &(&1 in @font_units)) or (type == "color" and color_dependency))

    deferred =
      relative and
        (color_dependency or
           Enum.any?(units, fn unit ->
             unit != "lh" or not is_number(Map.get(context, :line_height)) or
               Map.get(context, :line_height_normal, false) or
               is_number(Map.get(context, :line_height_multiplier))
           end))

    context =
      Map.put(
        context,
        :registration_type_check,
        Map.get(context, :syntax_only, false) or
          (deferred and Map.get(context, :registration_phase) == :provisional)
      )

    unavailable_metrics =
      (is_nil(Map.get(context, :font_face)) or not is_number(Map.get(context, :line_height))) and
        Enum.any?(
          units,
          &(&1 in ~w(ex ch cap ic rex rch rcap ric rlh) or
              (&1 == "lh" and Map.get(context, :line_height_normal, false)))
        )

    cond do
      Map.get(context, :independent, false) and relative ->
        :error

      Enum.any?(units, &(&1 in @viewport_units)) and
        not Map.get(context, :syntax_only, false) and
        not Map.get(context, :independent, false) and
          not (is_number(get_in(context, [:_position_viewport, :width])) and
                   is_number(get_in(context, [:_position_viewport, :height]))) ->
        Diagnostics.error(
          :css,
          :not_implemented,
          "registered custom-property units require the original viewport dimensions",
          module: __MODULE__,
          source: value
        )

      unavailable_metrics and Map.get(context, :registration_phase) != :provisional ->
        Diagnostics.error(
          :css,
          :not_implemented,
          "registered custom-property units require selected font metrics in the original context",
          module: __MODULE__,
          source: value
        )

      true ->
        result =
          case {kind, type} do
            {:literal, expected} ->
              case HtmlValidator.parse_css_identifier(value) do
                {:ok, ^expected, ""} -> {:ok, value}
                _ -> :error
              end

            {:type, "custom-ident"} ->
              case HtmlValidator.parse_css_identifier(value) do
                {:ok, identifier, ""} ->
                  if CssCascadeValidator.keyword(identifier) ||
                       String.downcase(identifier, :ascii) == "default",
                     do: :error,
                     else: {:ok, value}

                _ ->
                  :error
              end

            {:type, "color"} ->
              case HtmlValidator.parse_css_color(value, Map.get(context, :color, {0, 0, 0})) do
                {:ok, :transparent} ->
                  {:ok, "rgba(0,0,0,0)"}

                {:ok, color} ->
                  [r, g, b | alpha] = Tuple.to_list(color)
                  {:ok, "rgba(#{r * 255},#{g * 255},#{b * 255},#{List.first(alpha) || 1})"}

                _ ->
                  :error
              end

            {:type, "string"} ->
              values =
                value
                |> CssSyntaxValidator.components()
                |> Enum.reject(
                  &(&1.kind == :comment or Regex.match?(~r/\A[ \t\r\n\f]+\z/, &1.text))
                )

              case values do
                [%{kind: :string} = string] -> {:ok, CssSyntaxValidator.serialize(string)}
                _ -> :error
              end

            {:type, image} when image in ["image", "url"] ->
              case {CssSyntaxValidator.components(value),
                    HtmlValidator.parse_background_image_reference(value)} do
                {[%{kind: kind}], {:ok, source}}
                when kind in [:url, :block] and is_binary(source) ->
                  {:ok, value}

                _ ->
                  if Regex.match?(
                       ~r/\A(?:repeating-)?(?:linear|radial|conic)-gradient\(/i,
                       value
                     ),
                     do:
                       Diagnostics.error(
                         :css,
                         :not_implemented,
                         "registered gradient images require the CSS image value implementation",
                         module: __MODULE__,
                         source: value
                       ),
                     else: :error
              end

            {:type, transform} when transform in ["transform-function", "transform-list"] ->
              values = HtmlValidator.split_css_tokens(value)

              if values != [] and (transform == "transform-list" or length(values) == 1) do
                case CssTransformValidator.prepare(value) do
                  {:ok, _} -> canonical_lengths(value, context)
                  result -> result
                end
              else
                :error
              end

            {:type, "length-percentage"} ->
              with {:ok, _} <-
                     if(match?({zero, ""} when zero == 0, HtmlValidator.parse_css_number(value)),
                       do: {:ok, :zero},
                       else: CssMathValidator.length_dependencies(value)
                     ) do
                case CssMathValidator.evaluate(value, environment(context)) do
                  {:ok, number, %{length: 1}} when is_number(number) -> {:ok, "#{number}px"}
                  {:ok, number, %{percentage: 1}} when is_number(number) -> {:ok, "#{number}%"}
                  {:ok, number, type} when number == 0 and map_size(type) == 0 -> {:ok, "0px"}
                  _ -> canonical_lengths(value, context)
                end
              end

            {:type, scalar} when scalar in ["number", "integer"] ->
              with {:ok, number} <-
                     CssMathValidator.scalar(
                       value,
                       if(scalar == "integer", do: :integer, else: :number),
                       environment(context)
                     ),
                   do: {:ok, to_string(number)}

            {:type, numeric} ->
              case CssMathValidator.evaluate(value, environment(context)) do
                {:ok, number, type} when is_number(number) ->
                  case {numeric, type} do
                    {"length", %{length: 1}} ->
                      {:ok, "#{number}px"}

                    {"length", empty} when number == 0 and map_size(empty) == 0 ->
                      {:ok, "0px"}

                    {"percentage", %{percentage: 1}} ->
                      {:ok, "#{number}%"}

                    {"angle", %{angle: 1}} ->
                      {:ok, "#{number}rad"}

                    {"time", %{time: 1}} ->
                      {:ok, "#{number}s"}

                    {"resolution", %{resolution: 1}} when number >= 0 ->
                      {:ok, "#{number}dppx"}

                    _ ->
                      :error
                  end

                result ->
                  result
              end
          end

        if match?({:ok, _}, result) and deferred and
             Map.get(context, :registration_phase) == :provisional, do: {:ok, value}, else: result
    end
  end

  defp environment(context) do
    cond do
      Map.get(context, :registration_type_check, false) ->
        # This only validates dimensions. Deferred tokens are computed again with actual metrics.
        %{
          font_size: 1.0,
          root_font_size: 1.0,
          width: 1.0,
          height: 1.0,
          source_metrics: %{ex: 1.0, ch: 1.0, cap: 1.0, ic: 1.0, lh: 1.0},
          root_metrics: %{ex: 1.0, ch: 1.0, cap: 1.0, ic: 1.0, lh: 1.0},
          type_check: true,
          _depth_limit: :max_depth
        }

      not is_nil(Map.get(context, :font_face)) and is_number(Map.get(context, :font_size)) and
          is_number(Map.get(context, :line_height)) ->
        {:ok, environment} = CssPositionValidator.math_environment(context, 0)
        Map.delete(environment, :percentage_basis)

      true ->
        %{
          font_size: if(is_number(Map.get(context, :font_size)), do: context.font_size / 0.75),
          root_font_size:
            if(is_number(Map.get(context, :_root_font_size)), do: context._root_font_size / 0.75),
          # Independent initial tokens are type checked and retained until a real viewport exists.
          width:
            if(Map.get(context, :independent, false),
              do: 1.0,
              else: get_in(context, [:_position_viewport, :width])
            ),
          height:
            if(Map.get(context, :independent, false),
              do: 1.0,
              else: get_in(context, [:_position_viewport, :height])
            ),
          source_metrics: %{
            lh: if(is_number(Map.get(context, :line_height)), do: context.line_height / 0.75)
          },
          root_metrics: %{
            lh:
              if(is_number(Map.get(context, :_root_line_height)),
                do: context._root_line_height / 0.75
              )
          },
          _depth_limit: :max_depth
        }
    end
  end

  defp canonical_lengths(value, context) do
    # The owning type grammar has already accepted the complete value. Rewrite
    # its lengths recursively; arithmetic failures use the shared budget throw.
    output =
      value
      |> CssSyntaxValidator.components()
      |> Enum.map_join(fn component ->
        case component do
          %{kind: :block, opener: opener} ->
            body =
              binary_part(
                component.text,
                byte_size(opener),
                component.length - byte_size(opener) - if(component.closed, do: 1, else: 0)
              )

            {:ok, body} = canonical_lengths(body, context)

            opener <>
              body <>
              if(component.closed,
                do: binary_part(component.text, component.length - 1, 1),
                else: ""
              )

          %{kind: :token, text: text} ->
            case CssMathValidator.evaluate(text, environment(context)) do
              {:ok, number, %{length: 1}} when is_number(number) -> "#{number}px"
              _ -> text
            end

          _ ->
            component.text
        end
      end)

    {:ok, output}
  end

  defp initial_context, do: %{color: {0, 0, 0}, font_size: 12.0, _root_font_size: 12.0}
end
