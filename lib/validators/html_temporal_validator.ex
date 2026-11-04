defmodule NativeElixirPdfUtilities.Validators.HtmlTemporalValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    HtmlFormValidator,
    LanguageValidator,
    WriterValidator
  }

  alias NativeElixirPdfUtilities.HtmlToPdf.Font

  # HTML calendar units and the ISO week epoch are fixed format rules.
  @epoch_days :calendar.date_to_gregorian_days({1970, 1, 1})
  @day_milliseconds 86_400_000
  @kinds ~w(date month week time datetime-local)
  @native_types @kinds

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
  @spec prepare_render_style(map(), map(), [map()], keyword()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_render_style(node, style, ancestors, opts) do
    case node do
      %{tag: "input", _initial_form_state: state} ->
        attributes =
          NativeElixirPdfUtilities.Validators.HtmlFormValidator.initial_control_attributes(node)

        kind = HtmlValidator.input_type(Map.get(attributes, "type", "text"))

        cond do
          style.display == :none or Map.get(style, :_html_content_skipped, false) or
              not temporal_type?(kind) ->
            {:ok, style}

          LanguageValidator.resolved_language(node, ancestors) not in [
            [],
            [""],
            ["en"],
            ["en", "us"]
          ] ->
            Diagnostics.error(
              :style,
              :unsupported_html,
              "Native temporal display currently supports the English US field format; use lang=en-US or supply static artwork for another locale",
              module: __MODULE__,
              operation: :compute,
              source: kind
            )

          true ->
            with {:ok, display} <-
                   prepare_display(
                     style,
                     state,
                     attributes,
                     Keyword.get(opts, :forms, :interactive)
                   ),
                 {:ok, width} <-
                   HtmlFormValidator.validate_initial_control_dimension(
                     display.intrinsic_width,
                     :width
                   ),
                 {:ok, height} <-
                   HtmlFormValidator.validate_initial_control_dimension(
                     display.intrinsic_height,
                     :height
                   ) do
              style =
                style
                |> Map.put(:_temporal_display, display)
                |> Map.put(:_temporal_disabled, state.disabled)

              style =
                if style.width in [nil, :auto], do: Map.put(style, :width, width), else: style

              style =
                if style.height in [nil, :auto], do: Map.put(style, :height, height), else: style

              {:ok, style}
            end
        end

      _ ->
        {:ok, style}
    end
  end

  # Chromium DateComponents uses the ECMAScript Date range and a maximum
  # year of 275760. These are fixed native-widget format invariants, not Limits.
  @native_max_year 275_760
  @native_max_milliseconds 8_640_000_000_000_000

  @doc false
  @spec prepare_display(map(), map(), map(), :static | :interactive) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_display(style, state, attributes, forms_mode) do
    kind = state.temporal.kind

    bounds =
      Enum.map(~w(min max), fn name ->
        case parse(kind, Map.get(attributes, name)) do
          {:ok, parsed} -> parsed
          :error -> nil
        end
      end)

    components = state.temporal.components

    native_range? =
      Enum.all?(
        [state.temporal.value | Enum.map(bounds, &if(&1, do: &1.number))],
        fn value ->
          is_nil(value) or kind == "time" or
            value <=
              if(kind == "month",
                do: (@native_max_year - 1970) * 12 + 8,
                else: @native_max_milliseconds
              )
        end
      )

    cond do
      kind not in @native_types ->
        unsupported_display(:style, :compute, kind)

      match?(%{type: :built_in}, Map.get(style, :font_face)) ->
        Diagnostics.error(
          :style,
          :unsupported_html,
          "Native temporal fields require selected physical font metrics; supply an embedded font instead of a built-in PDF font with estimated advances",
          module: __MODULE__,
          operation: :compute,
          source: kind
        )

      not native_range? ->
        Diagnostics.error(
          :style,
          :unsupported_html,
          "Temporal initial display exceeds Chromium's fixed native date range; supply static artwork for this valid calendar value",
          module: __MODULE__,
          operation: :compute,
          source: kind
        )

      true ->
        parts =
          case kind do
            "time" ->
              time_parts(state)

            "datetime-local" ->
              calendar_parts(kind, components, bounds) ++ [{", ", [], 0.0}] ++ time_parts(state)

            _ ->
              calendar_parts(kind, components, bounds)
          end

        proof = %{
          type: :text,
          x: 0,
          y: 0,
          text:
            Enum.map_join(parts, fn {text, candidates, _padding} ->
              Enum.join([text | candidates])
            end),
          font:
            case Map.get(style, :font_face) do
              %{type: :embedded, id: id} = font when is_binary(id) -> Font.pdf_name(font)
              _ -> nil
            end,
          font_face: Map.get(style, :font_face),
          font_size: Map.get(style, :font_size),
          color: Map.get(style, :color)
        }

        with true <- forms_mode in [:static, :interactive],
             {:ok, font_size} <-
               HtmlValidator.normalize_geometry_number(Map.get(style, :font_size)),
             {:ok, line_height} <-
               HtmlValidator.normalize_geometry_number(Map.get(style, :line_height)),
             {:ok, letter_spacing} <-
               HtmlValidator.normalize_geometry_number(Map.get(style, :letter_spacing)),
             true <- font_size >= 0 and line_height >= 0,
             {:ok, _} <- WriterValidator.prepare([%{size: {100, 100}, boxes: [proof]}], []) do
          # Blink's shaped font size uses 1/64 CSS-pixel precision, as the
          # existing writer does before compensating PDF glyph advances.
          shaped_size = Float.floor(font_size / 0.75 * 64) / 64 * 0.75

          parts =
            Enum.map(parts, fn {text, candidates, padding} ->
              widths =
                Enum.map([text | candidates], fn value ->
                  max(0.0, Font.text_width(value, style.font_face, shaped_size, letter_spacing))
                  |> then(&(Float.ceil(&1 / 0.75 * 64) / 64 * 0.75))
                end)

              %{
                text: text,
                padding: padding,
                color: style.color,
                width: hd(widths) + 2 * padding,
                maximum_width: Enum.max(widths) + 2 * padding
              }
            end)

          # Native indicator em geometry enters Blink's LayoutUnit before flex
          # sizing. Retaining its 1/64-pixel fraction avoids later baseline ties.
          content_size =
            Float.floor(font_size / 0.75 * if(kind == "time", do: 1.05, else: 1.0) * 64) / 64 *
              0.75

          indicator_padding = if kind == "time", do: 2.25, else: 1.5
          indicator_margin = if kind == "time", do: 6.0, else: 0.0
          indicator_width = content_size + 2 * indicator_padding + indicator_margin
          editor_height = line_height + 1.5

          intrinsic_width =
            Float.ceil(Enum.sum(Enum.map(parts, & &1.maximum_width)) / 0.75) * 0.75 +
              indicator_width

          intrinsic_height = max(editor_height, content_size + 2 * indicator_padding)

          {:ok,
           %{
             kind: kind,
             value: state.value,
             forms_mode: forms_mode,
             parts: parts,
             intrinsic_width: intrinsic_width,
             intrinsic_height: intrinsic_height,
             editor_height: editor_height,
             baseline_depth: HtmlValidator.text_baseline_depth(style),
             indicator_width: indicator_width,
             indicator_padding: indicator_padding,
             indicator_margin: indicator_margin,
             indicator_content_size: content_size,
             picker_visible: not state.disabled and not state.readonly and font_size > 0
           }}
        else
          {:error, {reason, diagnostic}} ->
            {:error, {reason, diagnostic}}

          _ ->
            Diagnostics.error(
              :style,
              :invalid_document,
              "Temporal display requires bounded font geometry and a selected font containing every displayed field",
              module: __MODULE__,
              operation: :compute,
              source: kind
            )
        end
    end
  end

  # Native English-US fields are separate padded elements, not ISO strings.
  # Symbolic fields size every label, even when bounds constrain their value.
  @month_names ~w(January February March April May June July August September October November December)
  defp calendar_parts(kind, components, bounds) do
    [minimum, maximum] = bounds
    minimum_year = if minimum, do: minimum.components.year, else: 1
    maximum_year = if maximum, do: maximum.components.year, else: @native_max_year
    year_max = Integer.to_string(max(minimum_year, maximum_year)) |> String.pad_leading(4, "0")

    placeholder = if kind in ["month", "week"], do: "----", else: "yyyy"
    year = display_component(components, :year, 4, placeholder)
    year = {year, [placeholder, year_max], 0.75}

    case kind do
      "month" ->
        month = if components, do: Enum.at(@month_names, components.month - 1), else: "---------"
        [{month, ["---------" | @month_names], 0.75}, {" ", [], 0.0}, year]

      "week" ->
        week = display_component(components, :week, 2, "--")
        [{"Week ", [], 0.0}, {week, ["--", "53"], 0.75}, {", ", [], 0.0}, year]

      _ ->
        [
          {display_component(components, :month, 2, "mm"), ["mm", "12"], 0.75},
          {"/", [], 0.0},
          {display_component(components, :day, 2, "dd"), ["dd", "31"], 0.75},
          {"/", [], 0.0},
          year
        ]
    end
  end

  defp time_parts(state) do
    components = state.temporal.components

    second? =
      rem(state.temporal.value || 0, 60_000) != 0 or
        rem(state.temporal.min || 0, 60_000) != 0 or
        (state.temporal.step != :any and :math.fmod(state.temporal.step, 60.0) != 0)

    millisecond? =
      rem(state.temporal.value || 0, 1000) != 0 or
        rem(state.temporal.min || 0, 1000) != 0 or
        (state.temporal.step != :any and :math.fmod(state.temporal.step, 1.0) != 0)

    hour =
      if components do
        adjusted = %{
          components
          | hour: if(rem(components.hour, 12) == 0, do: 12, else: rem(components.hour, 12))
        }

        display_component(adjusted, :hour, 2, "--")
      else
        "--"
      end

    parts = [
      {hour, ["--", "12"], 0.75},
      {":", [], 0.0},
      {display_component(components, :minute, 2, "--"), ["--", "59"], 0.75}
    ]

    parts =
      if second?,
        do:
          parts ++
            [
              {":", [], 0.0},
              {display_component(components, :second, 2, "--"), ["--", "59"], 0.75}
            ],
        else: parts

    parts =
      if millisecond?,
        do:
          parts ++
            [
              {".", [], 0.0},
              {display_component(components, :millisecond, 3, "---"), ["---", "999"], 0.75}
            ],
        else: parts

    ampm = if components, do: if(components.hour < 12, do: "AM", else: "PM"), else: "--"
    parts ++ [{" ", [], 0.0}, {ampm, ["--", "AM", "PM"], 0.75}]
  end

  defp display_component(components, key, digits, placeholder) do
    if components,
      do: Integer.to_string(Map.fetch!(components, key)) |> String.pad_leading(digits, "0"),
      else: placeholder
  end

  defp unsupported_display(stage, operation, kind) do
    Diagnostics.error(
      stage,
      :unsupported_html,
      "Native temporal picker display is not yet supported for this type; use display:none for preserved initial state or supply a static image of the control",
      module: __MODULE__,
      operation: operation,
      source: kind
    )
  end

  @doc false
  @spec validate_initial_layout(map()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_initial_layout(node) do
    case node do
      %{style: %{display: :none}} ->
        :ok

      %{style: style} when is_map(style) ->
        control = Map.get(style, :_form_control, Map.get(style, :_temporal_control))

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
                valid_initial?(state, attributes) and is_boolean(Map.get(state, :disabled)) and
                  Map.get(style, :_temporal_state) == Map.get(state, :temporal)

              _ ->
                false
            end

          if valid? do
            state = control.initial_state

            cond do
              not is_map(Map.get(style, :_temporal_display)) ->
                Diagnostics.error(
                  :layout,
                  :invalid_layout,
                  "Temporal layout requires validator-prepared segmented display; compute styles before layout",
                  module: __MODULE__,
                  operation: :layout,
                  source: "temporal"
                )

              true ->
                case prepare_display(
                       style,
                       state,
                       control.attributes,
                       Map.get(style._temporal_display, :forms_mode)
                     ) do
                  {:ok, display} ->
                    image = Map.get(style, :_temporal_picker_image)

                    if display == style._temporal_display and
                         Map.get(style, :_temporal_disabled) == state.disabled and
                         not is_nil(image) == display.picker_visible do
                      if display.picker_visible do
                        # The existing writer validator owns decoded image frames
                        # and resource bounds, including directly supplied styles.
                        case WriterValidator.prepare(
                               [
                                 %{
                                   size: {100, 100},
                                   boxes: [
                                     %{
                                       type: :image,
                                       image: image,
                                       x: 0,
                                       y: 0,
                                       width: display.indicator_content_size,
                                       height: display.indicator_content_size
                                     }
                                   ]
                                 }
                               ],
                               []
                             ) do
                          {:ok, _} -> :ok
                          error -> error
                        end
                      else
                        :ok
                      end
                    else
                      Diagnostics.error(
                        :layout,
                        :invalid_layout,
                        "Temporal layout requires canonical segmented display, picker resources and disabled state; compute styles before layout",
                        module: __MODULE__,
                        operation: :layout,
                        source: "temporal"
                      )
                    end

                  error ->
                    error
                end
            end
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
