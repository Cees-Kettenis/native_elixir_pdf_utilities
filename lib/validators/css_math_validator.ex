defmodule NativeElixirPdfUtilities.Validators.CssMathValidator do
  @moduledoc "Shared bounded CSS calculations with dimensional arithmetic and the Values 4 numeric functions."
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, SourceSizeValidator}

  @length_units ~w(pt px in cm mm pc q em rem ex rex cap rcap ch rch ic ric lh rlh vw vh vi vb vmin vmax svw svh svi svb svmin svmax lvw lvh lvi lvb lvmin lvmax dvw dvh dvi dvb dvmin dvmax cqw cqh cqi cqb cqmin cqmax)
  @absolute_length_units ~w(pt px in cm mm pc q)
  @type numeric :: number() | :infinity | :negative_infinity | :nan
  @type dimension :: %{optional(atom()) => integer()}

  @doc """
  Evaluates a CSS scalar or math function in an already prepared length environment.

  Finite arithmetic overflow aborts through the shared render-budget diagnostic
  path. Explicit CSS infinity and NaN retain their calculation semantics.
  """
  @spec evaluate(String.t(), map()) :: {:ok, numeric(), dimension()} | :error
  def evaluate(value, environment) do
    environment = Map.put_new(environment, :_source_expression, value)
    value = HtmlValidator.normalize_css_comments(value)
    value = value |> HtmlValidator.trim_css_whitespace() |> String.downcase(:ascii)

    if length(component_terms(value, true)) == 1 and not String.starts_with?(value, "("),
      do: expression(value, environment, 0),
      else: :error
  rescue
    ArithmeticError ->
      throw(
        {:render_resource_limit,
         Diagnostics.error(
           :style,
           :resource_limit_exceeded,
           "CSS calculation arithmetic exceeds the finite numeric range; reduce intermediate magnitudes or increase tiny nonzero divisors and step intervals",
           operation: :evaluate_css_math,
           module: __MODULE__,
           source: value
         )}
      )
  end

  @doc "Evaluates an absolute color component using the pinned scalar calculation grammar without font or viewport authority."
  @spec evaluate_color_component(String.t()) :: {:ok, numeric(), dimension()} | :error
  def evaluate_color_component(value) do
    value
    |> NativeElixirPdfUtilities.HtmlToPdf.CssParser.strip_comments()
    |> evaluate(%{width: 0, height: 0, color_components: true})
  end

  @doc "Parses a CSS number or integer, rounding calculated integers toward positive infinity at ties."
  @spec scalar(String.t(), :number | :integer) :: {:ok, number()} | :error
  @spec scalar(String.t(), :number | :integer, map()) :: {:ok, number()} | :error
  def scalar(value, category \\ :number, environment \\ %{}) do
    value = value |> HtmlValidator.normalize_css_comments() |> HtmlValidator.trim_css_whitespace()

    case HtmlValidator.parse_css_number(value) do
      {number, ""} ->
        if category == :number or Regex.match?(~r/\A[+-]?[0-9]+\z/, value),
          do: {:ok, if(category == :integer, do: trunc(number), else: number)},
          else: :error

      _ ->
        with {:ok, _name, "(" <> _} <- HtmlValidator.parse_css_identifier(value),
             {:ok, number, type} when type == %{} <- evaluate(value, environment),
             number = if(number == :nan, do: 0.0, else: number),
             true <- is_number(number) do
          HtmlValidator.check_render_resource(:max_numeric_magnitude, abs(number), :style)

          # floor(x + 0.5) loses large odd integers when x + 0.5 rounds in binary64.
          rounded =
            if category == :integer,
              do: floor(number) + if(number - floor(number) >= 0.5, do: 1, else: 0),
              else: number

          {:ok, rounded}
        else
          _ -> :error
        end
    end
  end

  @doc "Applies a property's numeric range, rejecting out-of-range literals and clamping valid calculations."
  @spec scalar_in_range(String.t(), :number | :integer, number(), number() | nil) ::
          {:ok, number()} | :error
  @spec scalar_in_range(String.t(), :number | :integer, number(), number() | nil, map()) ::
          {:ok, number()} | :error
  def scalar_in_range(value, category, minimum, maximum \\ nil, environment \\ %{}) do
    with {:ok, number} <- scalar(value, category, environment) do
      if number >= minimum and (maximum == nil or number <= maximum) do
        {:ok, number}
      else
        value =
          value |> HtmlValidator.normalize_css_comments() |> HtmlValidator.trim_css_whitespace()

        if match?({:ok, _, "(" <> _}, HtmlValidator.parse_css_identifier(value)),
          do: {:ok, max(minimum, if(maximum == nil, do: number, else: min(maximum, number)))},
          else: :error
      end
    end
  end

  @typedoc "A scalar whose dimensional products need the selected element's metrics."
  @type scalar_calculation ::
          {:css_scalar_math, {String.t(), :number | :integer, number() | nil, number() | nil}}

  @doc "Prepares scalars without substituting an initial font for context-dependent dimensional products."
  @spec prepare_scalar(String.t(), :number | :integer, number() | nil, number() | nil) ::
          {:ok, number() | scalar_calculation()} | :error | {:error, Diagnostics.detailed_error()}
  def prepare_scalar(value, category, minimum \\ nil, maximum \\ nil) do
    result =
      if is_tuple(HtmlValidator.parse_css_number(String.trim(value))) and
           not String.contains?(value, "("),
         do: {:ok, %{units: []}},
         else: scalar_dependencies(value)

    with {:ok, dependencies} <- result do
      if Enum.any?(dependencies.units, &(&1 not in @absolute_length_units)) do
        {:ok, {:css_scalar_math, {value, category, minimum, maximum}}}
      else
        if minimum == nil,
          do: scalar(value, category),
          else: scalar_in_range(value, category, minimum, maximum)
      end
    end
  end

  @doc "Computes a prepared scalar using actual selected/root font and print viewport metrics."
  @spec resolve_scalar(term(), map()) ::
          {:ok, number()} | :error | {:error, Diagnostics.detailed_error()}
  def resolve_scalar(value, style) do
    case value do
      {:css_scalar_math, {source, category, minimum, maximum}} ->
        with {:ok, environment} <-
               NativeElixirPdfUtilities.Validators.CssPositionValidator.math_environment(
                 style,
                 0.0
               ) do
          environment = Map.delete(environment, :percentage_basis)

          result =
            if minimum == nil,
              do: scalar(source, category, environment),
              else: scalar_in_range(source, category, minimum, maximum, environment)

          case result do
            :error ->
              Diagnostics.error(
                :style,
                :not_implemented,
                "CSS scalar calculation requires measured units in the original element context",
                module: __MODULE__,
                operation: :resolve_css_math,
                source: source
              )

            result ->
              result
          end
        end

      _ ->
        {:ok, value}
    end
  end

  @doc "Finalizes validated scalar properties and counters after element font selection."
  @spec resolve_scalars(map()) :: {:ok, map()} | :error | {:error, Diagnostics.detailed_error()}
  def resolve_scalars(style) do
    result =
      Enum.reduce_while(
        [:z_index, :orphans, :widows, :order, :flex_grow, :flex_shrink],
        {:ok, style},
        fn property, {:ok, result} ->
          case Map.get(result, property) do
            {:css_scalar_math, _} = value ->
              case resolve_scalar(value, result) do
                {:ok, number} -> {:cont, {:ok, Map.put(result, property, number)}}
                error -> {:halt, error}
              end

            _ ->
              {:cont, {:ok, result}}
          end
        end
      )

    with {:ok, style} <- result do
      Enum.reduce_while(
        [:counter_reset, :counter_increment, :counter_set],
        {:ok, style},
        fn property, {:ok, style} ->
          values = Map.get(style, property, [])

          result =
            Enum.reduce_while(values, {:ok, []}, fn {name, value}, {:ok, values} ->
              case resolve_scalar(value, style) do
                {:ok, value} -> {:cont, {:ok, [{name, value} | values]}}
                error -> {:halt, error}
              end
            end)

          case result do
            {:ok, reversed} ->
              resolved = Enum.reverse(reversed)

              {:cont,
               {:ok, if(resolved == values, do: style, else: Map.put(style, property, resolved))}}

            error ->
              {:halt, error}
          end
        end
      )
    end
  end

  @typedoc "A length-percentage source with validated grammar and measured point scales."
  @type length_calculation ::
          {:css_calculation,
           {String.t(), %{String.t() => number()}}
           | {String.t(), %{String.t() => number()}, :css_pixel}}

  @doc "Prepares a length-percentage calculation while deferring its percentage basis."
  @spec prepare_length(term(), term()) ::
          {:ok, length_calculation()} | :error | {:error, Diagnostics.detailed_error()}
  @spec prepare_length(term(), term(), :point | :css_pixel) ::
          {:ok, length_calculation()} | :error | {:error, Diagnostics.detailed_error()}
  def prepare_length(value, scales, unit \\ :point) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, calculation, _dependencies} <- prepare_measured_length(value, scales, unit),
           do: {:ok, calculation}
    end)
  end

  defp prepare_measured_length(value, scales, unit) do
    if is_map(scales),
      do: HtmlValidator.check_render_resource(:max_work, map_size(scales), :style)

    valid? =
      unit in [:point, :css_pixel] and is_binary(value) and String.valid?(value) and
        is_map(scales) and
        Enum.all?(scales, fn {unit, scale} ->
          is_binary(unit) and is_number(scale) and scale >= 0 and
            scale <= Limits.get(:max_numeric_magnitude)
        end)

    cond do
      unit == :point and is_binary(value) and
          byte_size(value) > Limits.get(:max_numeric_token_bytes) ->
        length_failure(
          :resource_limit_exceeded,
          "CSS calculation exceeds max_numeric_token_bytes",
          value
        )

      valid? ->
        with {:ok, dependencies} <- length_dependencies(value) do
          case Enum.find(dependencies.units, &(not Map.has_key?(scales, &1))) do
            nil ->
              if unit == :point or
                   (valid_unit_scales?(scales) and
                      Enum.sort(Map.keys(scales)) == Enum.sort(dependencies.units)) do
                calculation =
                  {:css_calculation,
                   if(unit == :point, do: {value, scales}, else: {value, scales, :css_pixel})}

                {:ok, calculation, dependencies}
              else
                length_failure(
                  :invalid_document,
                  "Captured CSS pixel scales must match the referenced length units",
                  value
                )
              end

            unit ->
              length_failure(
                :invalid_document,
                "CSS calculation in #{unit} requires measured unit geometry",
                value
              )
          end
        end

      true ->
        length_failure(
          :invalid_document,
          "CSS calculation requires text and measured unit scales",
          nil
        )
    end
  end

  @doc "Collects units through the shared grammar after validating a length-percentage type."
  @spec referenced_length_units(term()) ::
          {:ok, [String.t()]} | :error | {:error, Diagnostics.detailed_error()}
  def referenced_length_units(value) do
    case length_dependencies(value) do
      {:ok, dependencies} -> {:ok, dependencies.units}
      result -> result
    end
  end

  @doc "Collects unit and percentage dependencies through the shared length calculation grammar."
  @spec length_dependencies(term()) ::
          {:ok, %{units: [String.t()], percentage?: boolean()}}
          | :error
          | {:error, Diagnostics.detailed_error()}
  @spec length_dependencies(term(), map()) ::
          {:ok, map()} | :error | {:error, Diagnostics.detailed_error()}
  def length_dependencies(value, context \\ %{}) do
    calculation_dependencies(value, %{length: 1}, context)
  end

  @doc "Collects length dependencies while validating a scalar result without acquiring fonts or a viewport."
  @spec scalar_dependencies(term()) ::
          {:ok, map()} | :error | {:error, Diagnostics.detailed_error()}
  def scalar_dependencies(value), do: calculation_dependencies(value, %{}, %{})

  defp calculation_dependencies(value, expected_type, context) do
    cond do
      not (is_binary(value) and String.valid?(value)) ->
        length_failure(
          :invalid_document,
          "CSS calculation requires text and measured unit scales",
          nil
        )

      true ->
        HtmlValidator.with_render_budget(fn ->
          collector = make_ref()
          Process.put(collector, [])

          try do
            HtmlValidator.reserve_render_resource(:max_work, byte_size(value), :css)

            environment =
              Map.merge(context, %{
                type_check: true,
                percentage_basis: 1.0,
                unit_scales: Map.new(@length_units, &{&1, 1.0}),
                _depth_limit: :max_depth,
                _unit_collector: collector
              })

            environment =
              if expected_type == %{},
                do: Map.delete(environment, :percentage_basis),
                else: environment

            case evaluate(value, environment) do
              {:ok, _, ^expected_type} ->
                dependencies = collector |> Process.get() |> Enum.reverse() |> Enum.uniq()
                {:ok, %{units: List.delete(dependencies, "%"), percentage?: "%" in dependencies}}

              _ ->
                :error
            end
          catch
            {:render_resource_limit,
             {:error, {:resource_limit_exceeded, %{operation: :prepare_css_calculation}}} = error} ->
              error
          after
            Process.delete(collector)
          end
        end)
    end
  end

  @doc "Evaluates a prepared length against its actual point-valued percentage basis."
  @spec resolve_length(term(), term()) ::
          {:ok, number()} | {:error, Diagnostics.detailed_error()}
  @spec resolve_length(term(), term(), :source | :property) ::
          {:ok, number()} | {:error, Diagnostics.detailed_error()}
  def resolve_length(calculation, basis, policy \\ :source) do
    prepared =
      case calculation do
        {:css_calculation, {source, scales}} -> {source, scales, 1.0}
        {:css_calculation, {source, scales, :css_pixel}} -> {source, scales, 0.75}
        _ -> nil
      end

    case prepared do
      {value, scales, factor}
      when is_binary(value) and is_map(scales) and is_number(basis) and
             (basis >= 0 or policy == :property) ->
        HtmlValidator.with_render_budget(fn ->
          if abs(basis) > Limits.get(:max_numeric_magnitude) do
            length_failure(
              :resource_limit_exceeded,
              "CSS calculation exceeds max_numeric_magnitude",
              value
            )
          else
            environment = %{
              unit_scales: scales,
              percentage_basis: basis / factor,
              _depth_limit: :max_depth,
              _bounded_length: policy == :source
            }

            case evaluate(value, environment) do
              {:ok, number, %{length: 1}} when is_number(number) ->
                result = number * factor

                HtmlValidator.check_render_resource(
                  :max_numeric_magnitude,
                  abs(result),
                  :layout
                )

                {:ok, result}

              {:ok, :nan, %{length: 1}} when policy == :property ->
                {:ok, 0.0}

              {:ok, special, %{length: 1}}
              when policy == :property and special in [:infinity, :negative_infinity] ->
                length_failure(
                  :resource_limit_exceeded,
                  "CSS calculation exceeds the finite layout numeric range",
                  value
                )

              {:ok, _, %{length: 1}} ->
                length_failure(
                  :invalid_document,
                  "CSS calculation divides by zero or produces a non-finite length",
                  value
                )

              _ ->
                length_failure(
                  :invalid_document,
                  "CSS calculation requires a validated expression and percentage basis",
                  value
                )
            end
          end
        end)

      _ ->
        length_failure(
          :invalid_document,
          "CSS calculation requires a validated expression and percentage basis",
          nil
        )
    end
  end

  @doc "Captures referenced length units from actual selected/root fonts and the original print viewport."
  @spec capture_unit_scales([String.t()], map()) ::
          {:ok, map()} | :error | {:error, Diagnostics.detailed_error()}
  def capture_unit_scales(units, style) do
    if units == [] do
      {:ok, %{}}
    else
      environment =
        cond do
          Map.has_key?(style, :_css_unit_scales) ->
            {:ok, %{unit_scales: style._css_unit_scales}}

          Enum.all?(units, &(&1 in @absolute_length_units)) ->
            {:ok, %{}}

          Enum.all?(units, &(&1 not in ~w(ex rex cap rcap ch rch ic ric lh rlh))) ->
            viewport = Map.get(style, :_position_viewport) || %{}
            size = Map.get(style, :font_size)
            root_size = Map.get(style, :_root_font_size, size)

            {:ok,
             %{
               font_size: if(is_number(size), do: size / 0.75),
               root_font_size: if(is_number(root_size), do: root_size / 0.75),
               width: Map.get(viewport, :width),
               height: Map.get(viewport, :height),
               writing_mode: Map.get(style, :writing_mode, :horizontal_tb)
             }}

          true ->
            NativeElixirPdfUtilities.Validators.CssPositionValidator.math_environment(style, 0.0)
        end

      with {:ok, environment} <- environment do
        Enum.reduce_while(units, {:ok, %{}}, fn unit, {:ok, scales} ->
          case evaluate("1" <> unit, environment) do
            {:ok, scale, %{length: 1}} when is_number(scale) and scale >= 0 ->
              {:cont, {:ok, Map.put(scales, unit, scale)}}

            _ ->
              {:halt,
               Diagnostics.error(
                 :style,
                 :not_implemented,
                 "CSS unit #{unit} requires measured geometry in the original element context",
                 module: __MODULE__,
                 operation: :compute_css_units,
                 source: "1" <> unit
               )}
          end
        end)
      end
    end
  end

  @doc "Freezes length-unit scales while leaving the percentage basis for the consuming formatting box."
  @spec compute_length(String.t(), map()) ::
          {:ok, length_calculation()} | :error | {:error, Diagnostics.detailed_error()}
  def compute_length(source, style) do
    with {:ok, dependencies} <- length_dependencies(source),
         {:ok, scales} <- capture_unit_scales(dependencies.units, style),
         do: {:ok, {:css_calculation, {source, scales, :css_pixel}}}
  end

  @doc "Checks a captured unit dictionary without acquiring a font or other resource."
  @spec valid_unit_scales?(term()) :: boolean()
  def valid_unit_scales?(scales) do
    # A dictionary has at most one entry per grammar unit, not a resource ceiling.
    is_map(scales) and map_size(scales) <= length(@length_units) and
      Enum.all?(scales, fn {unit, scale} ->
        unit in @length_units and is_number(scale) and scale >= 0 and
          scale <= Limits.get(:max_numeric_magnitude)
      end)
  end

  @doc "Checks captured length metadata through its original grammar and exact referenced units."
  @spec valid_prepared_length?(term()) :: boolean()
  def valid_prepared_length?(value), do: match?({:ok, _}, validate_prepared_length(value))

  @doc "Validates measured calculation metadata and returns its genuine unit and percentage dependencies."
  @spec validate_prepared_length(term()) ::
          {:ok, map()} | :error | {:error, Diagnostics.detailed_error()}
  def validate_prepared_length(value) do
    case value do
      {:css_calculation, {source, scales, :css_pixel}} ->
        with {:ok, ^value, dependencies} <- prepare_measured_length(source, scales, :css_pixel),
             do: {:ok, dependencies}

      {:css_calculation, {source, scales}} ->
        with {:ok, ^value, dependencies} <- prepare_measured_length(source, scales, :point),
             do: {:ok, dependencies}

      _ ->
        :error
    end
  end

  defp length_failure(reason, message, source) do
    Diagnostics.error(:layout, reason, message,
      operation: :prepare_css_calculation,
      module: __MODULE__,
      source: source
    )
  end

  @doc false
  @spec component_terms(String.t(), boolean() | :ranges) :: [String.t()]
  def component_terms(value, products?) do
    case component_tokens(value, products?) do
      {:ok, terms} -> terms
      :invalid -> []
    end
  end

  @doc "Tokenizes balanced CSS components while keeping strings and escapes opaque."
  @spec component_tokens(String.t(), boolean() | :ranges) :: {:ok, [String.t()]} | :invalid
  def component_tokens(value, products?) do
    {terms, current, depth, valid?} =
      value
      |> HtmlValidator.css_component_units()
      |> Enum.reduce({[], [], 0, true}, fn char, {terms, current, depth, valid?} ->
        cond do
          char in ["\"", "'"] ->
            {terms, [char | current], depth, false}

          char == "(" ->
            {terms, [char | current], depth + 1, valid?}

          char == ")" ->
            {terms, [char | current], depth - 1, valid? and depth > 0}

          depth == 0 and
              ((products? == true and char in ["*", "/"]) or
                 (products? == :ranges and char in ["<", "<=", ">", ">=", "="])) ->
            terms =
              if current == [],
                do: terms,
                else: [current |> Enum.reverse() |> Enum.join() | terms]

            {[char | terms], [], depth, valid?}

          char in [" ", "\t", "\n", "\r", "\f"] and depth == 0 ->
            if current == [],
              do: {terms, [], depth, valid?},
              else: {[current |> Enum.reverse() |> Enum.join() | terms], [], depth, valid?}

          true ->
            {terms, [char | current], depth, valid?}
        end
      end)

    terms = if current == [], do: terms, else: [current |> Enum.reverse() |> Enum.join() | terms]
    if valid? and depth == 0, do: {:ok, Enum.reverse(terms)}, else: :invalid
  end

  defp expression(value, environment, depth) do
    result = expression_value(value, environment, depth)

    case result do
      {:ok, number, _} when is_number(number) ->
        if Map.get(environment, :_bounded_length, false),
          do: HtmlValidator.check_render_resource(:max_numeric_magnitude, abs(number), :layout)

        result

      _ ->
        result
    end
  end

  defp expression_value(value, environment, depth) do
    HtmlValidator.check_render_resource(
      Map.get(environment, :_depth_limit, :max_depth),
      depth,
      :style
    )

    terms = component_terms(value, true)
    HtmlValidator.reserve_render_resource(:max_work, length(terms), :style)

    additive =
      terms
      |> Enum.with_index()
      |> Enum.filter(fn {term, _} -> term in ["+", "-"] end)
      |> List.last()

    product =
      terms
      |> Enum.with_index()
      |> Enum.filter(fn {term, _} -> term in ["*", "/"] end)
      |> List.last()

    case additive || product do
      {operator, index} ->
        with true <- index > 0 and index < length(terms) - 1,
             {:ok, left, left_type} <-
               expression(terms |> Enum.take(index) |> Enum.join(" "), environment, depth + 1),
             {:ok, right, right_type} <-
               expression(terms |> Enum.drop(index + 1) |> Enum.join(" "), environment, depth + 1) do
          case operator do
            additive when additive in ["+", "-"] ->
              if left_type == right_type,
                do: {:ok, arithmetic(operator, left, right), left_type},
                else: :error

            product ->
              # The pinned color grammar allows scalar factors and scalar divisors.
              # Check each authored operation before general dimensional cancellation.
              invalid_color_product? =
                Map.get(environment, :color_components, false) and
                  ((product == "*" and left_type != %{} and right_type != %{}) or
                     (product == "/" and right_type != %{}))

              if invalid_color_product? do
                :error
              else
                right_type =
                  if product == "/",
                    do: Map.new(right_type, fn {key, exponent} -> {key, -exponent} end),
                    else: right_type

                type =
                  Map.merge(left_type, right_type, fn _key, left, right -> left + right end)
                  |> Map.reject(fn {_key, exponent} -> exponent == 0 end)

                {:ok, arithmetic(operator, left, right), type}
              end
          end
        else
          _ -> :error
        end

      nil ->
        case HtmlValidator.parse_css_identifier(value) do
          {:ok, function, "(" <> rest} ->
            function = String.downcase(function, :ascii)

            if String.ends_with?(rest, ")") do
              arguments =
                String.slice(rest, 0, String.length(rest) - 1) |> HtmlValidator.split_css_comma()

              strategy_name =
                case List.first(arguments) do
                  nil ->
                    nil

                  argument ->
                    case HtmlValidator.parse_css_identifier(argument) do
                      {:ok, name, ""} -> String.downcase(name, :ascii)
                      _ -> nil
                    end
                end

              {strategy, arguments} =
                case {function, strategy_name, arguments} do
                  {"round", strategy, [_ | rest]}
                  when strategy in ["nearest", "up", "down", "to-zero"] ->
                    {strategy, rest}

                  _ ->
                    {"nearest", arguments}
                end

              parsed =
                Enum.map(arguments, fn argument ->
                  if function == "clamp" and argument == "none",
                    do: :none,
                    else: expression(argument, environment, depth + 1)
                end)

              function(function, parsed, strategy)
            else
              :error
            end

          _ ->
            if String.starts_with?(value, "(") and String.ends_with?(value, ")"),
              do:
                expression(
                  String.slice(value, 1, String.length(value) - 2),
                  environment,
                  depth + 1
                ),
              else: terminal(value, environment)
        end
    end
  end

  defp terminal(value, environment) do
    identifier =
      case HtmlValidator.parse_css_identifier(value) do
        {:ok, name, rest} -> {:ok, String.downcase(name, :ascii), rest}
        _ -> :error
      end

    case identifier do
      {:ok, "size", ""} ->
        case Map.get(environment, :_intrinsic_basis) do
          value when is_number(value) -> {:ok, value, %{length: 1}}
          _ -> :error
        end

      {:ok, "pi", ""} ->
        {:ok, :math.pi(), %{}}

      {:ok, "e", ""} ->
        {:ok, :math.exp(1), %{}}

      {:ok, "infinity", ""} ->
        {:ok, :infinity, %{}}

      {:ok, "-infinity", ""} ->
        {:ok, :negative_infinity, %{}}

      {:ok, "nan", ""} ->
        {:ok, :nan, %{}}

      _ ->
        parsed_number =
          case HtmlValidator.parse_css_number(value) do
            :error ->
              if Map.get(environment, :color_components, false) and
                   Regex.match?(~r/\A[+-]?(?:\d+(?:\.\d+)?|\.\d+)[eE]\+?\d{3,}\z/, value) do
                {:ok, mantissa, exponent} =
                  case Regex.run(~r/\A(.+)[eE]\+?(\d+)\z/, value) do
                    [_, mantissa, exponent] -> {:ok, mantissa, String.to_integer(exponent)}
                  end

                case HtmlValidator.parse_css_number(mantissa) do
                  {number, ""} when number != 0 and exponent > 308 ->
                    {if(number < 0,
                       do: -Limits.get(:max_numeric_magnitude),
                       else: Limits.get(:max_numeric_magnitude)
                     ), ""}

                  _ ->
                    :error
                end
              else
                :error
              end

            parsed ->
              parsed
          end

        if is_tuple(parsed_number) and
             byte_size(value) > Limits.get(:max_numeric_token_bytes) do
          throw(
            {:render_resource_limit,
             length_failure(
               :resource_limit_exceeded,
               "CSS numeric token exceeds max_numeric_token_bytes",
               Map.get(environment, :_source_expression, value)
             )}
          )
        end

        case parsed_number do
          {number, raw_unit} ->
            unit = HtmlValidator.normalize_css_unit(raw_unit)

            if Map.get(environment, :color_components, false) and
                 unit not in ["", "%", "deg", "grad", "rad", "turn"] do
              :error
            else
              case Map.get(environment, :_unit_collector) do
                nil ->
                  :ok

                collector ->
                  if unit in ["%" | @length_units],
                    do: Process.put(collector, [unit | Process.get(collector)])
              end

              environment =
                if not Map.has_key?(environment, :unit_scales) and
                     unit in ~w(ex rex ch rch cap rcap ic ric lh rlh) and
                     not Map.get(environment, :type_check, false) and
                     not Map.has_key?(environment, :source_metrics) do
                  case SourceSizeValidator.prepare_context(environment, ["1" <> unit], []) do
                    {:ok, context} -> context
                    {:error, _} = error -> throw({:render_resource_limit, error})
                  end
                else
                  environment
                end

              {scale, type} =
                case unit do
                  unit when unit in ~w(vw svw lvw dvw vi svi lvi dvi) ->
                    axis =
                      if String.ends_with?(unit, "vi") and
                           Map.get(environment, :writing_mode, :horizontal_tb) != :horizontal_tb,
                         do: :height,
                         else: :width

                    extent = Map.get(environment, axis, 0.0)
                    {if(is_number(extent), do: extent / 100), :length}

                  unit when unit in ~w(vh svh lvh dvh vb svb lvb dvb) ->
                    axis =
                      if String.ends_with?(unit, "vb") and
                           Map.get(environment, :writing_mode, :horizontal_tb) != :horizontal_tb,
                         do: :width,
                         else: :height

                    extent = Map.get(environment, axis, 0.0)
                    {if(is_number(extent), do: extent / 100), :length}

                  unit when unit in ~w(vmin svmin lvmin dvmin) ->
                    {if(
                       is_number(Map.get(environment, :width, 0.0)) and
                         is_number(Map.get(environment, :height, 0.0)),
                       do:
                         min(
                           Map.get(environment, :width, 0.0),
                           Map.get(environment, :height, 0.0)
                         ) / 100
                     ), :length}

                  unit when unit in ~w(vmax svmax lvmax dvmax) ->
                    {if(
                       is_number(Map.get(environment, :width, 0.0)) and
                         is_number(Map.get(environment, :height, 0.0)),
                       do:
                         max(
                           Map.get(environment, :width, 0.0),
                           Map.get(environment, :height, 0.0)
                         ) / 100
                     ), :length}

                  unit when unit in ~w(em rem) ->
                    {Map.get(
                       environment,
                       if(unit == "em", do: :font_size, else: :root_font_size),
                       16.0
                     ), :length}

                  unit when unit in ~w(ex rex ch rch cap rcap ic ric lh rlh) ->
                    key =
                      Map.fetch!(
                        %{"ex" => :ex, "ch" => :ch, "cap" => :cap, "ic" => :ic, "lh" => :lh},
                        String.trim_leading(unit, "r")
                      )

                    metrics =
                      if String.starts_with?(unit, "r"),
                        do:
                          Map.get(
                            environment,
                            :root_metrics,
                            Map.get(environment, :source_metrics, %{})
                          ),
                        else: Map.get(environment, :source_metrics, %{})

                    {if(Map.get(environment, :type_check, false),
                       do: 1.0,
                       else: Map.get(metrics, key)
                     ), :length}

                  "px" ->
                    {1.0, :length}

                  "pt" ->
                    {1 / 0.75, :length}

                  "pc" ->
                    {16.0, :length}

                  "in" ->
                    {96.0, :length}

                  "cm" ->
                    {96 / 2.54, :length}

                  "mm" ->
                    {96 / 25.4, :length}

                  "q" ->
                    {96 / 101.6, :length}

                  "deg" ->
                    {:math.pi() / 180, :angle}

                  "grad" ->
                    {:math.pi() / 200, :angle}

                  "turn" ->
                    {2 * :math.pi(), :angle}

                  "rad" ->
                    {1.0, :angle}

                  "s" ->
                    {1.0, :time}

                  "ms" ->
                    {0.001, :time}

                  "hz" ->
                    {1.0, :frequency}

                  "khz" ->
                    {1000.0, :frequency}

                  "dppx" ->
                    {1.0, :resolution}

                  "x" ->
                    {1.0, :resolution}

                  "dpi" ->
                    {1 / 96, :resolution}

                  "dpcm" ->
                    {2.54 / 96, :resolution}

                  "fr" ->
                    {1.0, :flex}

                  "%" ->
                    case Map.fetch(environment, :percentage_basis) do
                      {:ok, basis} ->
                        {basis / 100.0, Map.get(environment, :percentage_dimension, :length)}

                      :error ->
                        {1.0, :percentage}
                    end

                  "" ->
                    {1.0, :number}

                  _ ->
                    {nil, :unknown}
                end

              {scale, type} =
                if Map.has_key?(environment, :unit_scales) and unit in @length_units,
                  do: {Map.get(environment.unit_scales, unit), :length},
                  else: {scale, type}

              with true <- is_number(scale),
                   {:ok, value} <- HtmlValidator.normalize_geometry_number(number * scale) do
                {:ok, value, if(type == :number, do: %{}, else: %{type => 1})}
              else
                _ -> :error
              end
            end

          _ ->
            :error
        end
    end
  end

  defp function(name, arguments, strategy) do
    case {name, arguments} do
      {"clamp", [low, {:ok, preferred, type}, high]} ->
        low = if low == :none, do: {:ok, :negative_infinity, type}, else: low
        high = if high == :none, do: {:ok, :infinity, type}, else: high

        case {low, high} do
          {{:ok, low, ^type}, {:ok, high, ^type}} ->
            {:ok, extreme(:max, [low, extreme(:min, [preferred, high])]), type}

          _ ->
            :error
        end

      _ ->
        if arguments != [] and Enum.all?(arguments, &match?({:ok, _, _}, &1)) do
          values = Enum.map(arguments, &elem(&1, 1))
          types = Enum.map(arguments, &elem(&1, 2))
          consistent? = length(Enum.uniq(types)) == 1
          type = hd(types)

          case {name, values} do
            {"calc", [value]} ->
              {:ok, value, type}

            {"min", _} when consistent? ->
              {:ok, extreme(:min, values), type}

            {"max", _} when consistent? ->
              {:ok, extreme(:max, values), type}

            {"abs", [value]} ->
              {:ok, absolute(value), type}

            {"sign", [value]} ->
              {:ok, sign(value), %{}}

            {"hypot", _} when consistent? ->
              {:ok, hypot(values), type}

            {name, [value]} when name in ["sin", "cos", "tan"] and type in [%{}, %{angle: 1}] ->
              {:ok, transcendental(name, [value]), %{}}

            {name, [value]} when name in ["asin", "acos", "atan"] and type == %{} ->
              {:ok, transcendental(name, [value]), %{angle: 1}}

            {"atan2", [y, x]} when consistent? ->
              {:ok, transcendental("atan2", [y, x]), %{angle: 1}}

            {name, values}
            when name in ["pow", "sqrt", "log", "exp"] and consistent? and type == %{} ->
              case {name, length(values)} do
                {"pow", 2} -> {:ok, transcendental(name, values), %{}}
                {"log", count} when count in [1, 2] -> {:ok, transcendental(name, values), %{}}
                {name, 1} when name in ["sqrt", "exp"] -> {:ok, transcendental(name, values), %{}}
                _ -> :error
              end

            {"round", [value]} when type == %{} ->
              {:ok, stepped("round", value, 1.0, strategy), type}

            {name, [value, interval]} when name in ["round", "mod", "rem"] and consistent? ->
              {:ok, stepped(name, value, interval, strategy), type}

            _ ->
              :error
          end
        else
          :error
        end
    end
  end

  defp arithmetic(operator, left, right) do
    case {operator, left, right} do
      {_, :nan, _} ->
        :nan

      {_, _, :nan} ->
        :nan

      {"-", left, right} ->
        arithmetic("+", left, negate(right))

      {"+", left, right} when is_number(left) and is_number(right) ->
        left + right

      {"+", left, right} ->
        if left in [:infinity, :negative_infinity] and right == negate(left),
          do: :nan,
          else: if(is_number(left), do: right, else: left)

      {"*", left, right} when is_number(left) and is_number(right) ->
        left * right

      {"*", left, right} ->
        if left == 0 or right == 0, do: :nan, else: infinity(sign(left) * sign(right))

      {"/", left, right} when is_number(left) and is_number(right) and right != 0 ->
        left / right

      {"/", left, right} when is_number(left) and is_number(right) ->
        if left == 0, do: :nan, else: infinity(sign(left))

      {"/", left, right} when is_number(left) ->
        if right == :negative_infinity, do: -0.0, else: 0.0

      {"/", left, right} when is_number(right) ->
        infinity(sign(left) * if(right < 0, do: -1, else: 1))

      {"/", _, _} ->
        :nan
    end
  end

  defp negate(value) do
    case value do
      :infinity -> :negative_infinity
      :negative_infinity -> :infinity
      number -> -number
    end
  end

  defp sign(value) do
    case value do
      :nan -> :nan
      :infinity -> 1.0
      :negative_infinity -> -1.0
      number when number > 0 -> 1.0
      number when number < 0 -> -1.0
      number -> number
    end
  end

  defp infinity(sign), do: if(sign < 0, do: :negative_infinity, else: :infinity)

  defp absolute(value) do
    case value do
      :negative_infinity -> :infinity
      :nan -> :nan
      :infinity -> :infinity
      value -> abs(value)
    end
  end

  defp extreme(operation, values) do
    cond do
      :nan in values ->
        :nan

      operation == :min and :negative_infinity in values ->
        :negative_infinity

      operation == :max and :infinity in values ->
        :infinity

      true ->
        finite = Enum.filter(values, &is_number/1)

        if finite == [],
          do: hd(values),
          else: if(operation == :min, do: Enum.min(finite), else: Enum.max(finite))
    end
  end

  defp hypot(values) do
    cond do
      :nan in values ->
        :nan

      Enum.any?(values, &(&1 in [:infinity, :negative_infinity])) ->
        :infinity

      true ->
        scale = values |> Enum.map(&abs/1) |> Enum.max()

        if scale == 0,
          do: 0.0,
          else:
            :math.sqrt(
              Enum.reduce(values, 0.0, fn value, sum -> sum + value / scale * (value / scale) end)
            ) * scale
    end
  end

  defp stepped(name, value, interval, strategy) do
    interval = if name == "round", do: absolute(interval), else: interval

    cond do
      value == :nan or interval == :nan or interval == 0 ->
        :nan

      not is_number(value) ->
        if name == "round" and is_number(interval), do: value, else: :nan

      not is_number(interval) ->
        case {name, strategy} do
          {"mod", _} -> if sign(value) == sign(interval), do: value, else: :nan
          {"round", "up"} -> if value > 0, do: :infinity, else: 0.0
          {"round", "down"} -> if value < 0, do: :negative_infinity, else: 0.0
          _ -> if name == "rem", do: value, else: 0.0
        end

      name == "mod" ->
        value - Float.floor(value / interval) * interval

      name == "rem" ->
        value - trunc(value / interval) * interval

      true ->
        multiple =
          case strategy do
            "up" -> Float.ceil(value / interval)
            "down" -> Float.floor(value / interval)
            "to-zero" -> trunc(value / interval)
            _ -> Float.floor(value / interval + 0.5)
          end

        multiple * interval
    end
  end

  defp transcendental(name, values) do
    cond do
      :nan in values ->
        :nan

      name in ["sin", "cos", "tan"] and not is_number(hd(values)) ->
        :nan

      name == "sqrt" and hd(values) == :infinity ->
        :infinity

      name == "sqrt" and (hd(values) == :negative_infinity or hd(values) < 0) ->
        :nan

      name == "exp" and hd(values) == :infinity ->
        :infinity

      name == "exp" and hd(values) == :negative_infinity ->
        0.0

      name in ["asin", "acos"] and (not is_number(hd(values)) or abs(hd(values)) > 1) ->
        :nan

      name == "atan" and hd(values) == :infinity ->
        :math.pi() / 2

      name == "atan" and hd(values) == :negative_infinity ->
        -:math.pi() / 2

      name == "log" ->
        [value | base] = values
        base = if base == [], do: :math.exp(1), else: hd(base)

        cond do
          (is_number(value) and value < 0) or value == :negative_infinity or
            (is_number(base) and (base <= 0 or base == 1)) or base == :negative_infinity ->
            :nan

          value == 0 ->
            :negative_infinity

          value == :infinity ->
            arithmetic(
              "/",
              :infinity,
              if(base == :infinity, do: :infinity, else: :math.log(base))
            )

          base == :infinity ->
            0.0

          true ->
            :math.log(value) / :math.log(base)
        end

      name == "pow" ->
        [base, exponent] = values

        cond do
          exponent == 0 ->
            1.0

          exponent in [:infinity, :negative_infinity] ->
            absolute = absolute(base)

            cond do
              absolute == 1 -> :nan
              (absolute > 1 or absolute == :infinity) == (exponent == :infinity) -> :infinity
              true -> 0.0
            end

          base in [:infinity, :negative_infinity] ->
            if exponent < 0,
              do: 0.0,
              else:
                if(
                  base == :negative_infinity and trunc(exponent) == exponent and
                    rem(trunc(exponent), 2) != 0,
                  do: :negative_infinity,
                  else: :infinity
                )

          base == 0 and exponent < 0 ->
            :infinity

          base < 0 and trunc(exponent) != exponent ->
            :nan

          true ->
            try do
              :math.pow(base, exponent)
            rescue
              ArithmeticError -> :infinity
            end
        end

      name == "atan2" ->
        [y, x] = values

        {y, x} =
          if not is_number(y) or not is_number(x) do
            {if(is_number(y), do: 0.0, else: sign(y)), if(is_number(x), do: 0.0, else: sign(x))}
          else
            {y, x}
          end

        :math.atan2(y, x)

      true ->
        value = hd(values)

        try do
          case name do
            "sin" -> :math.sin(value)
            "cos" -> :math.cos(value)
            "tan" -> :math.tan(value)
            "asin" -> :math.asin(value)
            "acos" -> :math.acos(value)
            "atan" -> :math.atan(value)
            "sqrt" -> :math.sqrt(value)
            "exp" -> :math.exp(value)
          end
        rescue
          ArithmeticError -> :infinity
        end
    end
  end
end
