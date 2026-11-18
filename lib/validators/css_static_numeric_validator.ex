defmodule NativeElixirPdfUtilities.Validators.CssStaticNumericValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.Validators.{
    CssMathValidator,
    CssSyntaxValidator,
    CssTransformValidator,
    HtmlValidator
  }

  @doc "Computes the supported static Values 5 numeric, mix and interpolation notations."
  @spec evaluate(String.t(), String.t(), map(), boolean()) ::
          {:ok, String.t()} | :error | {:error, term()}
  def evaluate(name, source, context, whole?) do
    args = CssSyntaxValidator.split_trimmed(source, ",")

    case {name, args} do
      {"progress", [value, start, finish]} ->
        {value, unclamped?} =
          case HtmlValidator.parse_css_identifier(value) do
            {:ok, "no-clamp", rest} when rest != "" -> {String.trim(rest), true}
            _ -> {value, false}
          end

        with {:ok, [{value, type}, {start, type}, {finish, type}]} <-
               all([value, start, finish], &quantity(&1, context)) do
          cond do
            start == finish and not unclamped? -> {:ok, "0"}
            start == finish and value == start -> {:ok, "0"}
            start == finish and value < start -> {:ok, "calc(-infinity)"}
            start == finish -> {:ok, "calc(infinity)"}
            unclamped? -> {:ok, number((value - start) / (finish - start))}
            true -> {:ok, number(clamp((value - start) / (finish - start)))}
          end
        else
          {:error, _} = error -> error
          _ -> :error
        end

      {"random", args} ->
        random(args, context)

      {name, args} when name in ["calc-mix", "transform-mix"] ->
        family = if name == "calc-mix", do: :number, else: :transform

        with {:ok, items} <- all(args, &weight(&1, context, family)),
             do: mix(normalize_weights(items), family, context)

      {name, [input, _ | _] = args}
      when name in [
             "calc-interpolate",
             "color-interpolate",
             "transform-interpolate",
             "interpolate"
           ] ->
        if name != "interpolate" or whole? do
          family =
            case name do
              "calc-interpolate" -> :number
              "color-interpolate" -> :color
              "transform-interpolate" -> :transform
              "interpolate" -> :property
            end

          interpolate(input, tl(args), family, context)
        else
          :error
        end

      _ ->
        :error
    end
  rescue
    ArithmeticError ->
      Diagnostics.error(
        :css,
        :resource_limit_exceeded,
        "static CSS arithmetic exceeds the finite numeric range",
        module: __MODULE__,
        operation: :evaluate_css_math,
        source: name <> "(" <> source <> ")"
      )
  end

  @doc "Constructs a single identifier from the computed components of ident()."
  @spec identifier(String.t()) :: {:ok, String.t()} | :error
  def identifier(source) do
    with {:ok, tokens} <- all(HtmlValidator.split_css_tokens(source), &identifier_piece/1),
         value when value != "" <- Enum.join(tokens) do
      case HtmlValidator.parse_css_identifier(value) do
        {:ok, ^value, ""} ->
          {:ok, value}

        _ ->
          escaped =
            value
            |> String.to_charlist()
            |> Enum.map_join(fn codepoint ->
              <<92>> <> Integer.to_string(codepoint, 16) <> " "
            end)

          {:ok, escaped}
      end
    else
      _ -> :error
    end
  end

  defp identifier_piece(source) do
    case CssSyntaxValidator.components(source) do
      [%{kind: :string, text: string}] ->
        {:ok, HtmlValidator.decode_css_string(string)}

      _ ->
        case HtmlValidator.parse_css_identifier(source) do
          {:ok, identifier, ""} ->
            {:ok, identifier}

          _ ->
            with {:ok, value} <- CssMathValidator.scalar(source, :integer),
                 do: {:ok, Integer.to_string(value)}
        end
    end
  end

  defp random(args, context) do
    {key, values} =
      case args do
        [first | rest] when length(rest) in [2, 3] ->
          if random_key?(first), do: {first, rest}, else: {"auto", args}

        _ ->
          {"auto", args}
      end

    with {:ok, base} <- random_base(key, context),
         {:ok, [{minimum, type}, {maximum, type} | step]} <-
           all(values, &quantity(&1, context)) do
      maximum = max(minimum, maximum)

      case step do
        [] ->
          quantity_source(minimum + base * (maximum - minimum), type)

        [{step, ^type}] when step > 0 ->
          count = trunc((maximum - minimum) / step)
          epsilon = step / 1000
          count = if minimum + (count + 1) * step - maximum <= epsilon, do: count + 1, else: count
          index = trunc(base * (count + 1))
          value = minimum + index * step
          value = if index == count and abs(value - maximum) <= epsilon, do: maximum, else: value
          quantity_source(value, type)

        _ ->
          :error
      end
    else
      {:error, _} = error -> error
      _ -> :error
    end
  end

  defp random_key?(first) do
    case HtmlValidator.parse_css_identifier(first) do
      {:ok, "--" <> _, _} ->
        true

      {:ok, name, _} ->
        String.downcase(name, :ascii) in ~w(auto fixed element-scoped property-scoped property-index-scoped) or
          String.starts_with?(name, "ua-")

      _ ->
        false
    end
  end

  @doc "Resolves fixed random bases and per-document cache keys without mutable evaluation order."
  @spec random_base(String.t(), map()) :: {:ok, float()} | :error
  def random_base(source, context) do
    tokens = HtmlValidator.split_css_tokens(source)

    case tokens do
      ["fixed", value] ->
        with {:ok, value} <- CssMathValidator.scalar(value),
             true <- value >= 0 and value <= 1,
             do: {:ok, min(0.9999999999999999, value * 1.0)},
             else: (_ -> :error)

      _ ->
        tokens =
          if tokens == ["auto"],
            do: ["element-scoped", "property-index-scoped"],
            else: tokens

        parsed =
          Enum.reduce_while(tokens, %{}, fn token, acc ->
            with {:ok, name, ""} <- HtmlValidator.parse_css_identifier(token),
                 {:ok, key, value} <- cache_component(name, context),
                 false <- Map.has_key?(acc, key) do
              {:cont, Map.put(acc, key, value)}
            else
              _ -> {:halt, :error}
            end
          end)

        if is_map(parsed) and map_size(parsed) > 0 do
          document = Map.get(context, :_css_random_document, :local)

          <<bits::unsigned-53, _::bitstring>> =
            :crypto.hash(:sha256, :erlang.term_to_binary({document, parsed}))

          {:ok, bits / 9_007_199_254_740_992}
        else
          :error
        end
    end
  end

  defp cache_component(name, context) do
    case name do
      "element-scoped" ->
        {:ok, :element, {Map.get(context, :_css_element), Map.get(context, :_css_call_stack)}}

      "property-scoped" ->
        {:ok, :slot, Map.get(context, :_css_property)}

      "property-index-scoped" ->
        {:ok, :slot, {Map.get(context, :_css_property), Map.get(context, :_css_random_index)}}

      "--" <> suffix when suffix != "" ->
        {:ok, :name, name}

      "ua-" <> suffix when suffix != "" ->
        {:ok, :slot, name}

      _ ->
        :error
    end
  end

  defp weight(source, context, family) do
    tokens = HtmlValidator.split_css_tokens(source)

    {value, weight} =
      case Enum.split(tokens, -1) do
        {[_ | _] = value, [last]} ->
          case quantity(last, context) do
            {:ok, value_weight, %{percentage: 1}} ->
              {Enum.join(value, " "), value_weight}

            _ ->
              if family == :transform, do: leading_weight(tokens, context), else: {source, nil}
          end

        _ ->
          {source, nil}
      end

    if is_nil(weight) or (weight >= 0 and weight <= 100),
      do: {:ok, {value, weight}},
      else: :error
  end

  defp leading_weight([first | rest] = tokens, context) do
    case quantity(first, context) do
      {:ok, value, %{percentage: 1}} -> {Enum.join(rest, " "), value}
      _ -> {Enum.join(tokens, " "), nil}
    end
  end

  defp normalize_weights(items) do
    {sum, omitted} =
      Enum.reduce(items, {0, 0}, fn {_value, weight}, {sum, omitted} ->
        if is_nil(weight), do: {sum, omitted + 1}, else: {sum + weight, omitted}
      end)

    share = if omitted == 0, do: 0, else: max(0, 100 - sum) / omitted
    Enum.map(items, fn {value, weight} -> {value, (weight || share) / max(100, sum)} end)
  end

  defp mix(items, family, context) do
    case family do
      :number ->
        with {:ok, values} <-
               all(items, fn {source, weight} ->
                 with {:ok, value, type} <- quantity(source, context),
                      do: {:ok, {value * weight, type}}
               end) do
          quantities = Enum.group_by(values, &elem(&1, 1), &elem(&1, 0))

          case quantities do
            %{%{length: 1} => lengths, %{percentage: 1} => percentages}
            when map_size(quantities) == 2 ->
              {:ok,
               "calc(" <>
                 number(Enum.sum(lengths)) <> "px + " <> number(Enum.sum(percentages)) <> "%)"}

            _ when map_size(quantities) == 1 ->
              [{type, amounts}] = Map.to_list(quantities)
              quantity_source(Enum.sum(amounts), type)

            _ ->
              :error
          end
        end

      :transform ->
        with {:ok, values} <-
               all(items, fn {source, weight} -> transform_pair("none", source, weight) end) do
          values = Enum.reject(values, &(&1 == "none"))
          {:ok, if(values == [], do: "none", else: Enum.join(values, " "))}
        end
    end
  end

  defp interpolate(input, source, family, context) do
    with {:ok, position, type, map_easing, segment_easing, space} <- one_input(input, context),
         {:ok, stops} <- stops(source, context),
         {:ok, position, stops} <- normalize_stops(position, type, stops),
         {:ok, position} <- ease(map_easing, position),
         {left, left_value, _} <-
           Enum.find(Enum.reverse(stops), hd(stops), &(elem(&1, 0) <= position)),
         {right, right_value, easing} <-
           Enum.find(stops, List.last(stops), &(elem(&1, 0) > position)),
         amount = if(right > left, do: clamp((position - left) / (right - left)), else: 0.0),
         {:ok, amount} <- ease(easing || segment_easing, amount) do
      interpolate_pair(family, left_value, right_value, amount, space, context)
    else
      {:error, _} = error -> error
      _ -> :error
    end
  end

  defp one_input(input, context) do
    case HtmlValidator.split_css_tokens(input) do
      [source | options] ->
        with {:ok, position, type} <- quantity(source, context),
             {:ok, {map_easing, easing, space}} <- options(options, {nil, nil, nil}),
             do: {:ok, position, type, map_easing, easing, space}

      _ ->
        :error
    end
  end

  defp options(options, current) do
    case {options, current} do
      {[], _} ->
        {:ok, current}

      {["by", easing | rest], {nil, segment, space}} ->
        options(rest, {easing, segment, space})

      {["in", space, path, "hue" | rest], {map, segment, nil}}
      when path in ~w(shorter longer increasing decreasing) ->
        options(rest, {map, segment, space <> " " <> path <> " hue"})

      {["in", space | rest], {map, segment, nil}} ->
        options(rest, {map, segment, space})

      {[easing | rest], {map, nil, space}} ->
        options(rest, {map, easing, space})

      _ ->
        :error
    end
  end

  defp stops(source, context) do
    Enum.reduce_while(source, {:ok, [], nil}, fn entry, {:ok, stops, easing} ->
      case CssSyntaxValidator.split_trimmed(entry, ":") do
        [position, value] ->
          with [_ | _] = positions <- HtmlValidator.split_css_tokens(position),
               true <- length(positions) <= 2,
               {:ok, positions} <-
                 all(positions, fn source ->
                   case quantity(source, context) do
                     {:ok, number, type} -> {:ok, {number, type, value, easing}}
                     error -> error
                   end
                 end) do
            {:cont, {:ok, Enum.reverse(positions, stops), nil}}
          else
            error -> {:halt, error}
          end

        [option] when not is_nil(option) and is_nil(easing) and stops != [] ->
          {:cont, {:ok, stops, option}}

        _ ->
          {:halt, :error}
      end
    end)
    |> case do
      {:ok, [_ | _] = stops, nil} -> {:ok, Enum.reverse(stops)}
      {:error, _} = error -> error
      _ -> :error
    end
  end

  defp normalize_stops(input, type, stops) do
    relative = [%{}, %{percentage: 1}]

    absolute =
      for {position, type, _, _} <- stops, type not in relative, do: {position, type}

    case absolute do
      [] ->
        if type in relative,
          do: {:ok, relative_position(input, type), bounded_stops(stops, 0, 1)},
          else: :error

      [{start, dimension} | _] ->
        {finish, _} = List.last(absolute)

        if (type in relative or type == dimension) and
             Enum.all?(absolute, &(elem(&1, 1) == dimension)) and finish > start do
          input =
            if type in relative,
              do: relative_position(input, type),
              else: (input - start) / (finish - start)

          {:ok, input, bounded_stops(stops, start, finish)}
        else
          :error
        end
    end
  end

  defp relative_position(number, type), do: if(type == %{}, do: number, else: number / 100)

  defp bounded_stops(stops, start, finish) do
    stops
    |> Enum.map_reduce(nil, fn {position, type, value, easing}, previous ->
      position =
        if type in [%{}, %{percentage: 1}],
          do: relative_position(position, type),
          else: (position - start) / (finish - start)

      clamped = if previous, do: max(position, previous), else: position
      {{clamped, value, easing}, clamped}
    end)
    |> elem(0)
  end

  defp interpolate_pair(family, left, right, amount, space, context) do
    family =
      if family == :property do
        case Map.get(context, :_css_property) do
          "transform" ->
            :transform

          "color" ->
            :color

          <<_::binary>> = property ->
            if String.ends_with?(property, "-color"), do: :color, else: :discrete_or_number

          _ ->
            :discrete_or_number
        end
      else
        family
      end

    case family do
      :color ->
        space = space || "oklab"

        {:ok,
         "color-mix(in " <>
           space <> ", " <> left <> " " <> number((1 - amount) * 100) <> "%, " <> right <> ")"}

      :transform ->
        transform_pair(left, right, amount)

      family when family in [:number, :discrete_or_number] ->
        case {quantity(left, context), quantity(right, context)} do
          {{:ok, a, type}, {:ok, b, type}} -> quantity_source(a + (b - a) * amount, type)
          {{:error, _} = error, _} -> error
          {_, {:error, _} = error} -> error
          _ when family == :discrete_or_number -> {:ok, if(amount < 0.5, do: left, else: right)}
          _ -> :error
        end
    end
  end

  defp transform_pair(left, right, amount) do
    with {:ok, left} <- CssTransformValidator.prepare(left),
         {:ok, right} <- CssTransformValidator.prepare(right) do
      left = if left == :none, do: [], else: left
      right = if right == :none, do: [], else: right
      count = max(length(left), length(right))

      pairs =
        Enum.zip(
          left ++ List.duplicate(nil, count - length(left)),
          right ++ List.duplicate(nil, count - length(right))
        )

      with {:ok, operations} <- all(pairs, &transform_operation(&1, amount)) do
        {:ok, if(operations == [], do: "none", else: Enum.join(operations, " "))}
      end
    end
  end

  defp transform_operation(pair, amount) do
    case pair do
      {nil, operation} ->
        transform_operation({identity(operation), operation}, amount)

      {operation, nil} ->
        transform_operation({operation, identity(operation)}, amount)

      {{name, left}, {name, right}}
      when name not in ~w(perspective matrix matrix3d rotate3d) and length(left) == length(right) ->
        arguments =
          Enum.zip_with(left, right, fn {role, a}, {_, b} ->
            {a, b} =
              if role in [:length_x, :length_y, :length_z] do
                {if(a == "0", do: "0px", else: a), if(b == "0", do: "0px", else: b)}
              else
                {a, b}
              end

            "calc((" <>
              a <> ") * " <> number(1 - amount) <> " + (" <> b <> ") * " <> number(amount) <> ")"
          end)

        {:ok, name <> "(" <> Enum.join(arguments, ",") <> ")"}

      _ ->
        :error
    end
  end

  defp identity({name, arguments}) do
    {name,
     Enum.map(arguments, fn {role, _} ->
       value =
         case role do
           :scale -> "1"
           :angle -> "0deg"
           role when role in [:length_x, :length_y, :length_z] -> "0px"
           _ -> "0"
         end

       {role, value}
     end)}
  end

  @easing_keywords %{
    "step-start" => "steps(1, jump-start)",
    "step-end" => "steps(1, jump-end)",
    "ease" => "cubic-bezier(.25,.1,.25,1)",
    "ease-in" => "cubic-bezier(.42,0,1,1)",
    "ease-out" => "cubic-bezier(0,0,.58,1)",
    "ease-in-out" => "cubic-bezier(.42,0,.58,1)"
  }

  defp ease(easing, amount) do
    if easing in [nil, "linear"] do
      {:ok, amount}
    else
      source = Map.get(@easing_keywords, easing, easing)

      with {:ok, name, "(" <> tail} <- HtmlValidator.parse_css_identifier(source),
           true <- String.ends_with?(tail, ")") do
        values = CssSyntaxValidator.split_trimmed(String.slice(tail, 0..-2//1), ",")

        case name do
          "cubic-bezier" ->
            with {:ok, [x1, y1, x2, y2]} <- all(values, &CssMathValidator.scalar/1),
                 true <- x1 >= 0 and x1 <= 1 and x2 >= 0 and x2 <= 1 do
              t = invert_bezier(clamp(amount), x1, x2, 0.0, 1.0, 32)
              {:ok, bezier(t, y1, y2)}
            else
              _ -> :error
            end

          "steps" ->
            steps(values, amount)

          _ ->
            :error
        end
      else
        _ -> :error
      end
    end
  end

  defp steps(values, amount) do
    values = if length(values) == 1, do: values ++ ["jump-end"], else: values

    with [source, position] <- values,
         true <- Regex.match?(~r/\A\+?[0-9]+\z/, source),
         {count, ""} when count > 0 <- HtmlValidator.parse_css_number(source) do
      step = floor(amount * count)

      case position do
        position when position in ["jump-start", "start"] -> {:ok, clamp((step + 1) / count)}
        position when position in ["jump-end", "end"] -> {:ok, clamp(step / count)}
        "jump-none" when count > 1 -> {:ok, clamp(step / (count - 1))}
        "jump-both" -> {:ok, clamp((step + 1) / (count + 1))}
        _ -> :error
      end
    else
      _ -> :error
    end
  end

  defp bezier(t, first, second) do
    rest = 1 - t
    3 * rest * rest * t * first + 3 * rest * t * t * second + t * t * t
  end

  defp invert_bezier(x, first, second, low, high, depth) do
    mid = (low + high) / 2

    cond do
      depth == 0 -> mid
      bezier(mid, first, second) < x -> invert_bezier(x, first, second, mid, high, depth - 1)
      true -> invert_bezier(x, first, second, low, mid, depth - 1)
    end
  end

  defp quantity(value, context) do
    css = "calc(" <> value <> ")"

    with {:ok, %{units: units}} <- CssMathValidator.typed_dependencies(css),
         {:ok, scales} <- CssMathValidator.capture_unit_scales(units, context),
         {:ok, amount, type} when is_number(amount) <-
           CssMathValidator.evaluate(css, %{unit_scales: scales}) do
      {:ok, amount, type}
    else
      {:error, _} = error -> error
      _ -> :error
    end
  end

  defp quantity_source(value, type) do
    unit =
      case type do
        %{} when map_size(type) == 0 -> ""
        %{percentage: 1} when map_size(type) == 1 -> "%"
        %{length: 1} when map_size(type) == 1 -> "px"
        %{angle: 1} when map_size(type) == 1 -> "rad"
        %{time: 1} when map_size(type) == 1 -> "s"
        %{frequency: 1} when map_size(type) == 1 -> "Hz"
        %{resolution: 1} when map_size(type) == 1 -> "dppx"
        %{flex: 1} when map_size(type) == 1 -> "fr"
        _ -> nil
      end

    if is_binary(unit), do: {:ok, number(value) <> unit}, else: :error
  end

  defp all(values, fun) do
    Enum.reduce_while(values, {:ok, []}, fn value, {:ok, acc} ->
      case fun.(value) do
        {:ok, value} -> {:cont, {:ok, [value | acc]}}
        {:ok, value, type} -> {:cont, {:ok, [{value, type} | acc]}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, values} -> {:ok, Enum.reverse(values)}
      error -> error
    end
  end

  defp clamp(value), do: max(0.0, min(1.0, value))
  defp number(value), do: :erlang.float_to_binary(value * 1.0, [:short])
end
