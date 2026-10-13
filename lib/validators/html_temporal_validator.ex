defmodule NativeElixirPdfUtilities.Validators.HtmlTemporalValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  # HTML calendar units and the ISO week epoch are fixed format rules.
  @epoch_days :calendar.date_to_gregorian_days({1970, 1, 1})
  @day_milliseconds 86_400_000
  @kinds ~w(date month week time datetime-local)

  @doc false
  @spec temporal_type?(term()) :: boolean()
  def temporal_type?(kind), do: kind in @kinds

  @doc false
  @spec parse(String.t(), term()) :: {:ok, map()} | :error
  def parse(kind, source) do
    if is_binary(source) do
      HtmlValidator.check_render_resource(:max_html_numeric_token_bytes, byte_size(source), :html)

      case kind do
        kind when kind in ["date", "month"] ->
          pattern =
            if kind == "date",
              do: ~r/\A([0-9]{4,})-([0-9]{2})-([0-9]{2})\z/,
              else: ~r/\A([0-9]{4,})-([0-9]{2})\z/

          case Regex.run(pattern, source, capture: :all_but_first) do
            [year, month | day] ->
              year = String.to_integer(year)
              month = String.to_integer(month)
              day = if day == [], do: 1, else: String.to_integer(hd(day))

              if year > 0 and :calendar.valid_date({year, month, day}) do
                number =
                  if kind == "month",
                    do: (year - 1970) * 12 + month - 1,
                    else:
                      (:calendar.date_to_gregorian_days({year, month, day}) - @epoch_days) *
                        @day_milliseconds

                {:ok,
                 %{
                   value: source,
                   number: number,
                   components: %{year: year, month: month, day: day}
                 }}
              else
                :error
              end

            _ ->
              :error
          end

        "week" ->
          case Regex.run(~r/\A([0-9]{4,})-W([0-9]{2})\z/, source, capture: :all_but_first) do
            [year, week] ->
              year = String.to_integer(year)
              week = String.to_integer(week)

              if year > 0 and week > 0 and
                   week <= elem(:calendar.iso_week_number({year, 12, 28}), 1) do
                january_four = :calendar.date_to_gregorian_days({year, 1, 4})

                monday =
                  january_four - :calendar.day_of_the_week({year, 1, 4}) + 1 + (week - 1) * 7

                {:ok,
                 %{
                   value: source,
                   number: (monday - @epoch_days) * @day_milliseconds,
                   components: %{year: year, week: week}
                 }}
              else
                :error
              end

            _ ->
              :error
          end

        "time" ->
          case Regex.run(~r/\A([0-9]{2}):([0-9]{2})(?::([0-9]{2})(?:\.([0-9]{1,3}))?)?\z/, source,
                 capture: :all_but_first
               ) do
            [hour, minute | tail] ->
              hour = String.to_integer(hour)
              minute = String.to_integer(minute)

              second =
                case tail do
                  [] -> 0
                  [value | _] -> String.to_integer(value)
                end

              fraction =
                case tail do
                  [_, value] -> value
                  _ -> ""
                end

              millisecond =
                if fraction == "",
                  do: 0,
                  else: String.to_integer(String.pad_trailing(fraction, 3, "0"))

              if hour < 24 and minute < 60 and second < 60 do
                {:ok,
                 %{
                   value: source,
                   number: ((hour * 60 + minute) * 60 + second) * 1000 + millisecond,
                   components: %{
                     hour: hour,
                     minute: minute,
                     second: second,
                     millisecond: millisecond
                   }
                 }}
              else
                :error
              end

            _ ->
              :error
          end

        "datetime-local" ->
          with [date, time] <-
                 Regex.run(~r/\A([^ T]+)[ T]([^ T]+)\z/, source, capture: :all_but_first),
               {:ok, date} <- parse("date", date),
               {:ok, time} <- parse("time", time) do
            # Local datetime sanitation keeps the date and shortest time, using
            # T as the separator. Time inputs themselves keep authored precision.
            normalized = String.slice(time.value, 0, 5)

            normalized =
              if time.components.second != 0 or time.components.millisecond != 0,
                do:
                  normalized <>
                    ":" <> String.pad_leading(Integer.to_string(time.components.second), 2, "0"),
                else: normalized

            normalized =
              if time.components.millisecond != 0,
                do:
                  normalized <>
                    "." <>
                    (time.components.millisecond
                     |> Integer.to_string()
                     |> String.pad_leading(3, "0")
                     |> String.trim_trailing("0")),
                else: normalized

            {:ok,
             %{
               value: date.value <> "T" <> normalized,
               number: date.number + time.number,
               components: Map.merge(date.components, time.components)
             }}
          else
            _ -> :error
          end

        _ ->
          :error
      end
    else
      :error
    end
  end

  @doc false
  @spec prepare_initial(map()) :: map()
  def prepare_initial(attributes) do
    kind = HtmlValidator.input_type(Map.get(attributes, "type", "text"))

    parsed =
      Map.new(~w(value min max), fn name ->
        value =
          case parse(kind, Map.get(attributes, name)) do
            {:ok, value} -> value
            :error -> nil
          end

        {name, value}
      end)

    {scale, default} =
      case kind do
        "date" -> {@day_milliseconds, 1}
        "month" -> {1, 1}
        "week" -> {7 * @day_milliseconds, 1}
        _ -> {1000, 60}
      end

    step =
      case HtmlValidator.parse_html_float(Map.get(attributes, "step"), :decimal) do
        {:ok, %{value: value} = step} when value > 0 -> step
        _ -> nil
      end

    step =
      cond do
        String.downcase(Map.get(attributes, "step", ""), :ascii) == "any" -> :any
        step -> step
        true -> %{value: default * 1.0, coefficient: default, exponent: 0}
      end

    number = if parsed["value"], do: parsed["value"].number
    minimum = if parsed["min"], do: parsed["min"].number
    maximum = if parsed["max"], do: parsed["max"].number
    base = minimum || number || if(kind == "week", do: -3 * @day_milliseconds, else: 0)

    reversed =
      kind == "time" and not is_nil(minimum) and not is_nil(maximum) and maximum < minimum

    excluded = reversed and not is_nil(number) and number > maximum and number < minimum

    mismatch =
      if step == :any or is_nil(number) do
        false
      else
        # Scale the parsed finite number's shortest representation. The shared
        # fixed-precision token metadata can truncate leading fractional zeroes
        # in authored decimal strings, even when their float remains positive.
        {:ok, step} =
          HtmlValidator.parse_html_float(:erlang.float_to_binary(step.value, [:short]), :decimal)

        {numerator, denominator} =
          if step.exponent < 0,
            do: {step.coefficient * scale, Integer.pow(10, -step.exponent)},
            else: {step.coefficient * scale * Integer.pow(10, step.exponent), 1}

        rem((number - base) * denominator, numerator) != 0
      end

    value = if parsed["value"], do: parsed["value"].value, else: ""

    %{
      value: value,
      display: value,
      readonly: Map.has_key?(attributes, "readonly"),
      required: Map.has_key?(attributes, "required"),
      temporal: %{
        kind: kind,
        value: number,
        components: if(parsed["value"], do: parsed["value"].components, else: nil),
        min: minimum,
        max: maximum,
        step: if(step == :any, do: :any, else: step.value),
        step_scale: scale,
        step_base: base,
        step_mismatch: mismatch,
        reversed_range: reversed,
        range_underflow:
          excluded or
            (not reversed and not is_nil(number) and not is_nil(minimum) and number < minimum),
        range_overflow:
          excluded or
            (not reversed and not is_nil(number) and not is_nil(maximum) and number > maximum)
      }
    }
  end

  @doc false
  @spec valid_initial?(term(), term()) :: boolean()
  def valid_initial?(state, attributes) do
    is_map(state) and is_map(attributes) and
      Enum.all?(attributes, fn {key, value} ->
        is_binary(key) and String.valid?(key) and is_binary(value) and String.valid?(value)
      end) and
      temporal_type?(HtmlValidator.input_type(Map.get(attributes, "type", "text"))) and
      Map.take(state, [:value, :display, :readonly, :required, :temporal]) ==
        prepare_initial(attributes)
  end

  @doc false
  @spec validate_render_style(map(), map()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_render_style(node, style) do
    case node do
      %{tag: "input", attributes: attributes} ->
        kind = HtmlValidator.input_type(Map.get(attributes, "type", "text"))

        if Map.get(style, :display) != :none and temporal_type?(kind) do
          Diagnostics.error(
            :style,
            :unsupported_html,
            "Native temporal picker display is not yet supported; use display:none for preserved initial state or supply a static image of the control",
            module: __MODULE__,
            operation: :compute,
            source: kind,
            line: get_in(node, [:source, :line]),
            column: get_in(node, [:source, :column])
          )
        else
          :ok
        end

      _ ->
        :ok
    end
  end

  @doc false
  @spec validate_initial_layout(map()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_initial_layout(node) do
    case node do
      %{style: %{display: :none}} ->
        :ok

      %{style: style} when is_map(style) ->
        control = Map.get(style, :_form_control)

        temporal? =
          Map.has_key?(style, :_temporal_state) or
            (is_map(control) and is_map(Map.get(control, :attributes)) and
               is_binary(Map.get(control.attributes, "type", "text")) and
               temporal_type?(
                 HtmlValidator.input_type(Map.get(control.attributes, "type", "text"))
               ))

        if temporal? do
          valid? =
            case control do
              %{initial_state: state, attributes: attributes} ->
                valid_initial?(state, attributes) and
                  Map.get(style, :_temporal_state) == Map.get(state, :temporal)

              _ ->
                false
            end

          if valid? do
            Diagnostics.error(
              :layout,
              :unsupported_html,
              "Native temporal picker display is not yet supported; supply a static image of the control",
              module: __MODULE__,
              operation: :layout,
              source: control.initial_state.temporal.kind
            )
          else
            Diagnostics.error(
              :layout,
              :invalid_layout,
              "Temporal layout requires canonical initial temporal state; compute styles before layout",
              module: __MODULE__,
              operation: :layout,
              source: "temporal"
            )
          end
        else
          :ok
        end

      _ ->
        :ok
    end
  end
end
