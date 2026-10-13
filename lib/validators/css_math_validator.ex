defmodule NativeElixirPdfUtilities.Validators.CssMathValidator do
  @moduledoc "Shared bounded CSS calculations with dimensional arithmetic and the Values 4 numeric functions."
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, SourceSizeValidator}
  @type numeric :: number() | :infinity | :negative_infinity | :nan
  @type dimension :: %{optional(atom()) => integer()}

  @doc "Evaluates a CSS scalar or math function in an already prepared length environment."
  @spec evaluate(String.t(), map()) :: {:ok, numeric(), dimension()} | :error
  def evaluate(value, environment) do
    value = value |> HtmlValidator.trim_css_whitespace() |> String.downcase(:ascii)

    if length(component_terms(value, true)) == 1 and not String.starts_with?(value, "("),
      do: expression(value, environment, 0),
      else: :error
  end

  @doc "Evaluates an absolute color component using the pinned scalar calculation grammar without font or viewport authority."
  @spec evaluate_color_component(String.t()) :: {:ok, numeric(), dimension()} | :error
  def evaluate_color_component(value) do
    value
    |> NativeElixirPdfUtilities.HtmlToPdf.CssParser.strip_comments()
    |> evaluate(%{width: 0, height: 0, color_components: true})
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
    HtmlValidator.check_render_resource(:max_media_query_depth, depth, :style)
    terms = component_terms(value, true)
    HtmlValidator.reserve_render_resource(:max_css_work, length(terms), :style)

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
               expression(terms |> Enum.drop(index + 1) |> Enum.join(" "), environment, depth + 1),
             true <-
               not Map.get(environment, :color_components, false) or
                 operator in ["+", "-"] or
                 (operator == "*" and (left_type == %{} or right_type == %{})) or
                 (operator == "/" and right_type == %{}) do
          case operator do
            additive when additive in ["+", "-"] ->
              if left_type == right_type,
                do: {:ok, arithmetic(operator, left, right), left_type},
                else: :error

            product ->
              right_type =
                if product == "/",
                  do: Map.new(right_type, fn {key, exponent} -> {key, -exponent} end),
                  else: right_type

              type =
                Map.merge(left_type, right_type, fn _key, left, right -> left + right end)
                |> Map.reject(fn {_key, exponent} -> exponent == 0 end)

              {:ok, arithmetic(operator, left, right), type}
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
        case HtmlValidator.parse_css_number(value) do
          {number, raw_unit} ->
            unit =
              case HtmlValidator.parse_css_identifier(raw_unit) do
                {:ok, unit, ""} -> String.downcase(unit, :ascii)
                _ -> raw_unit
              end

            if Map.get(environment, :color_components, false) and
                 unit not in ["", "%", "deg", "grad", "rad", "turn"] do
              :error
            else
              environment =
                if unit in ~w(ex rex ch rch cap rcap ic ric lh rlh) and
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
                    {environment.width / 100, :length}

                  unit when unit in ~w(vh svh lvh dvh vb svb lvb dvb) ->
                    {environment.height / 100, :length}

                  unit when unit in ~w(vmin svmin lvmin dvmin) ->
                    {min(environment.width, environment.height) / 100, :length}

                  unit when unit in ~w(vmax svmax lvmax dvmax) ->
                    {max(environment.width, environment.height) / 100, :length}

                  unit when unit in ~w(em rem) ->
                    {Map.get(
                       environment,
                       if(unit == "em", do: :font_size, else: :root_font_size),
                       16.0
                     ), :length}

                  unit when unit in ~w(ex rex ch rch cap rcap ic ric lh rlh) ->
                    key = unit |> String.trim_leading("r") |> String.to_existing_atom()

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

                  "dpi" ->
                    {1 / 96, :resolution}

                  "dpcm" ->
                    {2.54 / 96, :resolution}

                  "fr" ->
                    {1.0, :flex}

                  "%" ->
                    case Map.fetch(environment, :percentage_basis) do
                      {:ok, basis} -> {basis / 100.0, :length}
                      :error -> {1.0, :percentage}
                    end

                  "" ->
                    {1.0, :number}

                  _ ->
                    {nil, :unknown}
                end

              with true <- is_number(scale),
                   {:ok, value} <- HtmlValidator.normalize_geometry_number(number * scale),
                   do: {:ok, value, if(type == :number, do: %{}, else: %{type => 1})}
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
      :nan -> :nan
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
