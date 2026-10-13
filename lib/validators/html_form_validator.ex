defmodule NativeElixirPdfUtilities.Validators.HtmlFormValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.HtmlTemporalValidator

  @initial_context {__MODULE__, :initial_context}

  @doc false
  @spec prepare_initial_nodes([map()]) :: [map()]
  def prepare_initial_nodes(children) do
    previous = Process.get(@initial_context)

    Process.put(@initial_context, %{
      ids: [],
      controls: [],
      labels: [],
      label_targets: %{}
    })

    try do
      nodes = HtmlValidator.assign_render_metadata(children)
      collect_initial_metadata(nodes)
      context = Process.get(@initial_context)

      if context.controls == [] and context.labels == [] do
        nodes
      else
        ids =
          context.ids
          |> Enum.reverse()
          |> Enum.reduce(%{}, fn {id, item}, ids ->
            Map.put_new(ids, id, item)
          end)

        controls =
          context.controls
          |> Enum.reverse()
          |> Enum.map(fn item ->
            owner =
              if item.tag in ~w(progress meter) do
                nil
              else
                case Map.fetch(item.attributes, "form") do
                  {:ok, reference} ->
                    case Map.get(ids, reference) do
                      %{tag: "form", namespace: :html, ref: ref} -> ref
                      _ -> nil
                    end

                  :error ->
                    item.ancestor_owner
                end
              end

            Map.put(item, :owner, owner)
          end)

        winners =
          Enum.reduce(controls, %{}, fn item, winners ->
            name = Map.get(item.attributes, "name")

            if item.input_type == "radio" and name not in [nil, ""] and
                 Map.has_key?(item.attributes, "checked") do
              Map.put(winners, {item.owner, name}, item.ref)
            else
              winners
            end
          end)

        {states, selected_options, disabled_options} =
          Enum.reduce(controls, {%{}, %{}, MapSet.new()}, fn item,
                                                             {states, selected_options,
                                                              disabled_options} ->
            name = Map.get(item.attributes, "name")

            checked =
              if item.input_type == "radio" and name not in [nil, ""],
                do: Map.get(winners, {item.owner, name}) == item.ref,
                else: Map.has_key?(item.attributes, "checked")

            state = %{owner: item.owner, checked: checked, disabled: item.disabled}

            state =
              if item.input_type == "radio" do
                indeterminate =
                  if name in [nil, ""],
                    do: not checked,
                    else: not Map.has_key?(winners, {item.owner, name})

                Map.put(state, :indeterminate, indeterminate)
              else
                state
              end

            state =
              case {item.tag, item.input_type} do
                {"textarea", _} ->
                  value = item.textarea_value

                  placeholder =
                    Map.get(item.attributes, "placeholder", "") |> String.replace(~r/\r\n?/, "\n")

                  Map.merge(state, %{
                    value: value,
                    cols: initial_positive_dimension(item.attributes, "cols", 20),
                    rows: initial_positive_dimension(item.attributes, "rows", 2),
                    wrap:
                      case String.downcase(Map.get(item.attributes, "wrap", ""), :ascii) do
                        "hard" -> :hard
                        "off" -> :off
                        _ -> :soft
                      end,
                    placeholder: placeholder,
                    placeholder_shown: value == "" and placeholder != ""
                  })

                {"input", type} when type in ~w(text search tel url email password) ->
                  prepared = HtmlValidator.prepare_input_attributes(item.attributes)
                  value = Map.get(prepared, "value", "")

                  placeholder =
                    :binary.replace(
                      Map.get(item.attributes, "placeholder", ""),
                      ["\r", "\n"],
                      "",
                      [:global]
                    )

                  Map.merge(state, %{
                    value: value,
                    size: initial_positive_dimension(item.attributes, "size", 20),
                    placeholder: placeholder,
                    placeholder_shown: value == "" and placeholder != ""
                  })

                {"input", "number"} ->
                  Map.merge(state, prepare_initial_number(item.attributes))

                {"input", "range"} ->
                  Map.merge(state, prepare_initial_range(item.attributes))

                {"input", type} when type in ~w(date month week time datetime-local) ->
                  Map.merge(state, HtmlTemporalValidator.prepare_initial(item.attributes))

                {"input", type} when type in ~w(button reset submit) ->
                  prepared = HtmlValidator.prepare_input_attributes(item.attributes)
                  Map.put(state, :value, Map.fetch!(prepared, "value"))

                _ ->
                  state
              end

            state =
              if item.input_type == "radio" do
                indeterminate =
                  if name in [nil, ""],
                    do: not checked,
                    else: not Map.has_key?(winners, {item.owner, name})

                Map.put(state, :indeterminate, indeterminate)
              else
                state
              end

            {state, selected_options, disabled_options} =
              if item.tag == "select" do
                selection = initial_selection(item.options, item.attributes)

                selected_options =
                  Enum.reduce(selection.choices, selected_options, &Map.put(&2, &1.id, &1))

                disabled_options =
                  Enum.reduce(selection.disabled_ids, disabled_options, &MapSet.put(&2, &1))

                {Map.put(state, :selection, selection), selected_options, disabled_options}
              else
                {state, selected_options, disabled_options}
              end

            {Map.put(states, item.ref, state), selected_options, disabled_options}
          end)

        labels =
          Map.new(context.labels, fn label ->
            target =
              case Map.fetch(label.attributes, "for") do
                {:ok, id} ->
                  case Map.get(ids, id) do
                    nil -> nil
                    item -> if labelable_initial_element?(item), do: item.ref, else: nil
                  end

                :error ->
                  Map.get(context.label_targets, label.ref)
              end

            {label.ref, target}
          end)

        attach_initial_state(nodes, states, labels, selected_options, disabled_options)
      end
    after
      if is_nil(previous),
        do: Process.delete(@initial_context),
        else: Process.put(@initial_context, previous)
    end
  end

  @doc false
  @spec prepare_initial_range(map()) :: map()
  def prepare_initial_range(attributes) do
    parsed =
      Map.new(~w(value min max step), fn name ->
        value =
          case HtmlValidator.parse_html_float(Map.get(attributes, name), :decimal) do
            {:ok, decimal} -> decimal
            :error -> nil
          end

        {name, value}
      end)

    # HTML defaults and the shared parser's decimal precision are fixed format
    # rules. Integer arithmetic keeps decimal step ties exact, including negatives.
    exponent =
      Enum.reduce(parsed, 0, fn {_name, value}, acc ->
        if value, do: min(acc, value.exponent), else: acc
      end) - 1

    integers =
      Map.new(parsed, fn {name, value} ->
        integer =
          if value do
            sign = if value.negative, do: -1, else: 1

            if value.value == 0.0,
              do: 0,
              else: sign * value.coefficient * Integer.pow(10, value.exponent - exponent)
          end

        {name, integer}
      end)

    unit = Integer.pow(10, -exponent)
    minimum = integers["min"] || 0
    maximum = integers["max"] || 100 * unit
    base = integers["min"] || integers["value"] || 0

    step =
      cond do
        String.downcase(Map.get(attributes, "step", ""), :ascii) == "any" -> :any
        integers["step"] && integers["step"] > 0 -> integers["step"]
        true -> unit
      end

    default = if maximum < minimum, do: minimum, else: minimum + div(maximum - minimum, 2)

    proposed =
      if HtmlValidator.valid_html_float?(Map.get(attributes, "value")),
        do: integers["value"],
        else: default

    value = max(proposed, minimum)
    value = if maximum >= minimum, do: min(value, maximum), else: value

    value =
      if step == :any do
        value
      else
        lower = base + Integer.floor_div(value - base, step) * step
        upper = lower + step

        candidates =
          Enum.filter([lower, upper], fn candidate ->
            candidate >= minimum and (maximum < minimum or candidate <= maximum) and
              match?(
                {:ok, _},
                HtmlValidator.parse_html_float(range_decimal_string(candidate, exponent))
              )
          end)

        Enum.min_by(candidates, &{abs(&1 - value), -&1}, fn -> value end)
      end

    lexical = range_decimal_string(value, exponent)
    {:ok, numeric} = HtmlValidator.parse_html_float(lexical)
    # HTML's best representation serializes the resulting finite double.
    # Reuse the shared parsed token rather than another exponent grammar.
    {:ok, shortest} =
      HtmlValidator.parse_html_float(:erlang.float_to_binary(numeric, [:short]), :decimal)

    lexical =
      range_decimal_string(
        if(shortest.negative, do: -shortest.coefficient, else: shortest.coefficient),
        shortest.exponent
      )

    %{
      value: lexical,
      display: lexical,
      readonly: false,
      required: false,
      range: %{
        value: numeric,
        min: if(parsed["min"], do: parsed["min"].value, else: 0.0),
        max: if(parsed["max"], do: parsed["max"].value, else: 100.0),
        step:
          if(step == :any,
            do: :any,
            else: elem(HtmlValidator.parse_html_float(range_decimal_string(step, exponent)), 1)
          ),
        step_base:
          if(parsed["min"],
            do: parsed["min"].value,
            else: if(parsed["value"], do: parsed["value"].value, else: 0.0)
          ),
        position:
          if(maximum <= minimum,
            do: 0.0,
            else: div((value - minimum) * Integer.pow(10, 18), maximum - minimum) / 1.0e18
          )
      }
    }
  end

  defp range_decimal_string(integer, exponent) do
    if integer == 0 do
      "0"
    else
      digits = Integer.to_string(abs(integer))
      length = byte_size(digits)
      trimmed = String.trim_trailing(digits, "0")
      exponent = exponent + length - byte_size(trimmed)
      point = byte_size(trimmed) + exponent
      sign = if integer < 0, do: "-", else: ""

      sign <>
        cond do
          point > 21 or point <= -6 ->
            String.first(trimmed) <>
              if(byte_size(trimmed) > 1, do: "." <> String.slice(trimmed, 1..-1//1), else: "") <>
              "e" <> if(point > 0, do: "+", else: "") <> Integer.to_string(point - 1)

          point <= 0 ->
            "0." <> String.duplicate("0", -point) <> trimmed

          exponent >= 0 ->
            trimmed <> String.duplicate("0", exponent)

          true ->
            String.slice(trimmed, 0, point) <> "." <> String.slice(trimmed, point..-1//1)
        end
    end
  end

  @doc false
  @spec valid_initial_range?(term(), term()) :: boolean()
  def valid_initial_range?(state, attributes) do
    is_map(state) and is_map(attributes) and
      Enum.all?(attributes, fn {key, value} ->
        is_binary(key) and String.valid?(key) and is_binary(value) and String.valid?(value)
      end) and
      Map.take(state, [:value, :display, :readonly, :required, :range]) ==
        prepare_initial_range(attributes)
  end

  @doc false
  @spec validate_initial_range_layout(map()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_initial_range_layout(node) do
    valid? =
      case node do
        %{style: %{display: :none}} ->
          true

        %{style: %{_range_state: range} = style} ->
          case Map.get(style, :_form_control, Map.get(style, :_range_control)) do
            %{tag: "input", attributes: attributes, initial_state: state} ->
              HtmlValidator.input_type(Map.get(attributes, "type", "text")) == "range" and
                valid_initial_range?(state, attributes) and state.range == range and
                is_boolean(Map.get(state, :disabled)) and
                Map.get(style, :_range_disabled) == state.disabled

            _ ->
              false
          end

        %{style: style} when is_map(style) ->
          case Map.get(style, :_form_control, Map.get(style, :_range_control)) do
            %{tag: "input", attributes: attributes} ->
              HtmlValidator.input_type(Map.get(attributes, "type", "text")) != "range"

            _ ->
              true
          end

        _ ->
          true
      end

    if valid? do
      :ok
    else
      Diagnostics.error(
        :layout,
        :invalid_layout,
        "range layout requires canonical initial Range state; compute styles before layout",
        module: __MODULE__,
        operation: :layout,
        source: "range"
      )
    end
  end

  @doc false
  @spec validate_initial_range_attributes(map()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_initial_range_attributes(node) do
    if Map.get(node, :namespace, :html) == :html and node.tag == "input" and
         HtmlValidator.input_type(Map.get(node.attributes, "type", "text")) == "range" and
         Map.has_key?(node.attributes, "list") do
      Diagnostics.error(
        :html,
        :unsupported_html,
        "Range datalist tick artwork is not yet supported; remove list or supply a static image of the control",
        module: __MODULE__,
        operation: :compute,
        source: "range/list"
      )
    else
      :ok
    end
  end

  @doc false
  @spec prepare_initial_number(map()) :: map()
  def prepare_initial_number(attributes) do
    authored = Map.get(attributes, "value", "")
    value = if HtmlValidator.valid_html_float?(authored), do: authored, else: ""

    numbers =
      Map.new(~w(value min max step), fn name ->
        number =
          case HtmlValidator.parse_html_float(Map.get(attributes, name)) do
            {:ok, number} -> number
            :error -> nil
          end

        {name, number}
      end)

    step =
      cond do
        String.downcase(Map.get(attributes, "step", ""), :ascii) == "any" -> :any
        is_number(numbers["step"]) and numbers["step"] > 0 -> numbers["step"]
        true -> 1.0
      end

    decoration? =
      step != :any and
        Enum.all?(~w(min max), &HtmlValidator.valid_html_float?(Map.get(attributes, &1)))

    columns =
      if decoration? do
        dimensions =
          Enum.map(~w(min max step), fn name ->
            source = Map.get(attributes, name, "1")
            source = if HtmlValidator.valid_html_float?(source), do: source, else: "1"
            {:ok, number} = HtmlValidator.parse_html_float(source, :decimal)
            digits = number.coefficient_digits
            sign = if number.negative, do: 1, else: 0

            cond do
              number.exponent >= 0 ->
                {sign + digits, 0}

              number.exponent + digits > 0 ->
                {sign + number.exponent + digits, -number.exponent}

              true ->
                {sign + 1, -number.exponent}
            end
          end)

        {before, after_decimal} =
          Enum.reduce(dimensions, {0, 0}, fn {before, after_decimal}, {max_before, max_after} ->
            {max(before, max_before), max(after_decimal, max_after)}
          end)

        before + after_decimal + if(after_decimal > 0, do: 1, else: 0)
      else
        20
      end

    placeholder =
      :binary.replace(Map.get(attributes, "placeholder", ""), ["\r", "\n"], "", [:global])

    numeric_value = if value == "", do: nil, else: numbers["value"]

    %{
      value: value,
      display: value,
      placeholder: placeholder,
      placeholder_shown: value == "" and placeholder != "",
      readonly: Map.has_key?(attributes, "readonly"),
      required: Map.has_key?(attributes, "required"),
      number: %{
        value: numeric_value,
        min: numbers["min"],
        max: numbers["max"],
        step: step,
        step_base: numbers["min"] || numbers["value"] || 0.0,
        range_underflow:
          not is_nil(numeric_value) and not is_nil(numbers["min"]) and
            numeric_value < numbers["min"],
        range_overflow:
          not is_nil(numeric_value) and not is_nil(numbers["max"]) and
            numeric_value > numbers["max"],
        columns: columns,
        decoration: decoration?
      }
    }
  end

  @doc false
  @spec valid_initial_number?(term(), term()) :: boolean()
  def valid_initial_number?(state, attributes) do
    is_map(state) and is_map(attributes) and
      Enum.all?(attributes, fn {key, value} ->
        is_binary(key) and String.valid?(key) and is_binary(value) and String.valid?(value)
      end) and
      Map.take(state, [
        :value,
        :display,
        :placeholder,
        :placeholder_shown,
        :readonly,
        :required,
        :number
      ]) ==
        prepare_initial_number(attributes)
  end

  @doc false
  @spec validate_initial_number_layout(map()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_initial_number_layout(node) do
    case node do
      %{style: %{display: :none}} ->
        # Initial DOM preparation still validates/sanitizes hidden controls.
        # No painting or field transport consumes their styled metadata.
        :ok

      %{style: %{_number_state: number} = style} ->
        valid? =
          case Map.get(style, :_form_control) do
            %{tag: "input", attributes: attributes, initial_state: state} ->
              valid_initial_number?(state, attributes) and
                HtmlValidator.input_type(Map.get(attributes, "type", "text")) == "number" and
                is_boolean(Map.get(style, :_native_menu)) and Map.get(state, :number) == number

            _ ->
              false
          end

        if valid?,
          do: :ok,
          else:
            Diagnostics.error(
              :layout,
              :invalid_layout,
              "number layout requires canonical initial number state and prepared appearance; compute styles before layout",
              module: __MODULE__,
              operation: :layout,
              source: "number"
            )

      _ ->
        :ok
    end
  end

  @doc false
  @spec prepare_initial_number_style(map(), [{String.t(), term()}]) :: map()
  defdelegate prepare_initial_number_style(style, declarations),
    to: __MODULE__,
    as: :prepare_initial_textfield_style

  @doc false
  @spec prepare_initial_textfield_style(map(), [{String.t(), term()}]) :: map()
  def prepare_initial_textfield_style(style, declarations) do
    native? =
      style.appearance == :auto and
        not Enum.any?(declarations, fn {property, _value} ->
          property in ["background", "background-color", "background-image", "box-shadow"] or
            (String.starts_with?(property, "border") and property != "border-radius")
        end)

    # NativeThemeBase TextField and MenuList share this fixed initial rounded
    # frame and ScopedRasterFlags stroke. Existing rect transport owns its paint.
    style = Map.put(style, :_native_menu, native?)

    if native?,
      do:
        Map.put(
          style,
          :border_styles,
          Map.new(style.border_styles, fn {side, _} -> {side, :solid} end)
        ),
      else: style
  end

  @doc false
  @spec prepare_initial_temporal_style(map(), [{String.t(), term()}]) :: map()
  def prepare_initial_temporal_style(style, declarations) do
    # Rounded corners defer TextField background/border paint to CSS. Preserve
    # the UA inset border instead of the native one-pixel frame in that case.
    style =
      if style.border_radius > 0,
        do: Map.put(style, :_native_menu, false),
        else: prepare_initial_textfield_style(style, declarations)

    # The UA font shorthand resets inherited line height. An authored temporal
    # line-height or font shorthand still wins in the ordinary cascade.
    if Enum.any?(declarations, fn {property, _} -> property in ["line-height", "font"] end),
      do: style,
      else:
        style
        |> Map.put(:line_height_normal, true)
        |> Map.delete(:line_height_multiplier)
        |> Map.delete(:line_height_explicit)
  end

  @doc false
  @spec number_uses_normal_line_height?(map()) :: boolean()
  def number_uses_normal_line_height?(style) do
    # TextControlInnerEditorElement compares the host's integer CSS-pixel
    # computed line height, and resets undersized/taller fixed-height editors.
    computed = trunc(style.line_height / 0.75) * 0.75

    style.font_size >= computed or
      case Map.get(style, :height) do
        {:percent, _} -> true
        height when is_number(height) -> height > computed
        _ -> false
      end
  end

  @doc false
  @spec initial_number_content_offset(map(), number()) :: float()
  def initial_number_content_offset(style, height) do
    # AlignBlockContent uses unsafe center for input controls. LayoutUnit's
    # fixed 1/64 CSS-pixel division truncates toward zero, including overflow.
    trunc((height - style.line_height) / 0.75 * 64 / 2) / 64 * 0.75
  end

  @doc false
  @spec readonly_input_type?(String.t()) :: boolean()
  def readonly_input_type?(type),
    do:
      HtmlValidator.text_input_type?(type) or type == "number" or
        HtmlTemporalValidator.temporal_type?(type)

  @doc false
  @spec valid_initial_input?(String.t(), map(), [term()]) :: boolean()
  def valid_initial_input?(type, attributes, children) do
    children == [] and
      (not Map.has_key?(attributes, "checked") or type in ["checkbox", "radio"]) and
      (not Map.has_key?(attributes, "readonly") or HtmlValidator.readonly_input_type?(type)) and
      (not Map.has_key?(attributes, "multiple") or type == "email") and
      (not Map.has_key?(attributes, "size") or HtmlValidator.text_input_type?(type)) and
      (not Map.has_key?(attributes, "placeholder") or
         (HtmlValidator.readonly_input_type?(type) and
            not HtmlTemporalValidator.temporal_type?(type))) and
      (not Map.has_key?(attributes, "list") or type == "range") and
      (not Enum.any?(~w(min max step), &Map.has_key?(attributes, &1)) or
         type in ["number", "range"] or HtmlTemporalValidator.temporal_type?(type)) and
      (type != "range" or
         not Enum.any?(
           ~w(accept alpha alt colorspace dirname formaction formenctype formmethod formnovalidate formtarget height maxlength minlength pattern required popovertarget popovertargetaction src width),
           &Map.has_key?(attributes, &1)
         ))
  end

  defp collect_initial_metadata(
         nodes,
         form_owner \\ nil,
         label_ref \\ nil,
         ancestor_disabled? \\ false
       ) do
    Enum.each(nodes, fn node ->
      case node do
        %{type: :element, children: children} ->
          html? = Map.get(node, :namespace, :html) == :html

          if node.tag in ~w(input select textarea button label fieldset progress meter) or
               Map.has_key?(node.attributes, "id") do
            register_initial_node(node, form_owner, label_ref, ancestor_disabled?)
          end

          next_owner =
            if node.tag == "form" and Map.get(node, :namespace, :html) == :html,
              do: node._selector_id,
              else: form_owner

          next_label =
            if node.tag == "label" and Map.get(node, :namespace, :html) == :html,
              do: node._selector_id,
              else: label_ref

          if html? and node.tag == "fieldset" and Map.has_key?(node.attributes, "disabled") do
            # Only this fieldset's own disabling is exempted. An outer disabled
            # fieldset still disables controls inside a nested fieldset's legend.
            first_legend =
              Enum.find(children, fn child ->
                Map.get(child, :tag) == "legend" and Map.get(child, :namespace, :html) == :html
              end)

            Enum.each(children, fn child ->
              exempt? = first_legend != nil and child == first_legend

              collect_initial_metadata(
                [child],
                next_owner,
                next_label,
                ancestor_disabled? or not exempt?
              )
            end)
          else
            collect_initial_metadata(children, next_owner, next_label, ancestor_disabled?)
          end

        _ ->
          :ok
      end
    end)
  end

  # Metadata is already assigned. Collect only the ID/control facts needed
  # to resolve forward references, label targets and initial radio groups.
  defp register_initial_node(node, form_owner, label_ref, ancestor_disabled?) do
    attributes = node.attributes
    namespace = Map.get(node, :namespace, :html)

    relevant? =
      namespace == :html and
        node.tag in ~w(input select textarea button label fieldset progress meter)

    if relevant? or Map.get(attributes, "id", "") != "" do
      context = Process.get(@initial_context)

      input_type =
        if node.tag == "input" and namespace == :html,
          do: HtmlValidator.input_type(Map.get(attributes, "type", "text")),
          else: nil

      item = %{
        ref: node._selector_id,
        tag: node.tag,
        namespace: namespace,
        attributes: attributes,
        input_type: input_type,
        order: node._document_order,
        ancestor_owner: form_owner,
        disabled: ancestor_disabled? or Map.has_key?(attributes, "disabled")
      }

      item =
        if node.tag == "textarea" and namespace == :html,
          do:
            Map.put(
              item,
              :textarea_value,
              Enum.map_join(node.children, &Map.get(&1, :text, ""))
              |> String.replace(~r/\r\n?/, "\n")
            ),
          else: item

      item =
        if node.tag == "select" and namespace == :html,
          do: Map.put(item, :options, HtmlValidator.select_options(node.children)),
          else: item

      context =
        case Map.get(attributes, "id", "") do
          "" -> context
          id -> %{context | ids: [{id, item} | context.ids]}
        end

      context =
        case {namespace, node.tag} do
          {:html, "label"} ->
            %{context | labels: [item | context.labels]}

          {:html, tag} when tag in ~w(input select textarea button fieldset progress meter) ->
            %{context | controls: [item | context.controls]}

          _ ->
            context
        end

      context =
        if not is_nil(label_ref) and labelable_initial_element?(item),
          do: %{context | label_targets: Map.put_new(context.label_targets, label_ref, item.ref)},
          else: context

      Process.put(@initial_context, context)
    end
  end

  defp labelable_initial_element?(item) do
    item.namespace == :html and
      (item.tag in ~w(select textarea button progress meter) or
         (item.tag == "input" and item.input_type != "hidden"))
  end

  @doc false
  @spec validate_initial_control_dimension(number(), :width | :height) ::
          {:ok, float()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_initial_control_dimension(extent, property) do
    case HtmlValidator.normalize_geometry_number(extent) do
      {:ok, dimension} when dimension >= 0 ->
        {:ok, dimension}

      {:ok, _negative} ->
        Diagnostics.error(
          :style,
          :invalid_document,
          "initial control #{property} must be nonnegative; check the prepared control dimensions and font metrics",
          operation: :compute,
          module: __MODULE__,
          source: "initial control #{property}"
        )

      _ ->
        Diagnostics.error(
          :style,
          :resource_limit_exceeded,
          "initial control #{property} exceeds max_css_numeric_magnitude; reduce the size, cols, rows or font size, or specify a smaller CSS dimension",
          operation: :compute,
          module: __MODULE__,
          source: "initial control #{property}"
        )
    end
  end

  # HTML positive dimension attributes use integer prefixes and element-specific
  # defaults. Valid authored extents share the existing geometry budget.
  defp initial_positive_dimension(attributes, name, default) do
    case HtmlValidator.parse_html_integer(Map.get(attributes, name)) do
      {:ok, value} when value > 0 ->
        try do
          HtmlValidator.check_render_resource(:max_css_numeric_magnitude, value, :html)
          value
        catch
          {:render_resource_limit, {:error, {reason, diagnostic}}} ->
            throw(
              {:render_resource_limit,
               {:error,
                {reason,
                 Diagnostics.with_context(diagnostic,
                   source: name <> "=" <> Map.fetch!(attributes, name)
                 )}}}
            )
        end

      _ ->
        default
    end
  end

  # HTML selectedness is independent of whether an option produces a painted row.
  # PDF indices are zero-based in this complete option list, never the visible rows.
  defp initial_selection(options, attributes) do
    multiple? = Map.has_key?(attributes, "multiple")

    display_size =
      case HtmlValidator.parse_html_integer(Map.get(attributes, "size"), :nonnegative) do
        {:ok, size} when size >= 0 ->
          HtmlValidator.check_render_resource(:max_css_numeric_magnitude, size, :html)
          size

        _ ->
          if multiple?, do: 4, else: 1
      end

    authored = Enum.filter(options, &Map.has_key?(&1.attributes, "selected"))

    selected =
      cond do
        multiple? ->
          authored

        authored != [] ->
          [List.last(authored)]

        display_size == 1 ->
          Enum.take(Enum.filter(options, &(not &1._initial_option_disabled)), 1)

        true ->
          []
      end

    selected_ids = Enum.map(selected, & &1._selector_id)
    selected_set = MapSet.new(selected_ids)

    choices =
      options
      |> Enum.with_index()
      |> Enum.map(fn {option, index} ->
        text =
          option.children
          |> Enum.map_join(&Map.get(&1, :text, ""))
          |> HtmlValidator.trim_ascii_whitespace()
          |> String.replace(~r/[ \t\n\f\r]+/u, " ")

        label =
          case Map.get(option.attributes, "label", "") do
            "" -> text
            label -> label
          end

        %{
          id: option._selector_id,
          index: index,
          value: Map.get(option.attributes, "value", text),
          label: label,
          disabled: option._initial_option_disabled,
          selected: MapSet.member?(selected_set, option._selector_id)
        }
      end)

    selected_choices = Enum.filter(choices, & &1.selected)
    first = List.first(selected_choices)
    listbox? = if multiple?, do: display_size != 1, else: display_size > 1

    display_label =
      if multiple? and not listbox? and length(selected_choices) != 1,
        do: "#{length(selected_choices)} selected",
        else: if(is_nil(first), do: "", else: first.label)

    %{
      id: if(is_nil(first), do: nil, else: first.id),
      selected_ids: selected_ids,
      selected_indices: Enum.map(selected_choices, & &1.index),
      values: Enum.map(selected_choices, & &1.value),
      value: if(is_nil(first), do: "", else: first.value),
      children: [%{type: :text, text: display_label}],
      choices: choices,
      disabled_ids:
        for(option <- options, option._initial_option_disabled, do: option._selector_id),
      multiple: multiple?,
      display_size: display_size,
      listbox: listbox?,
      rows: if(display_size == 0, do: 4, else: display_size)
    }
  end

  defp attach_initial_state(
         nodes,
         states,
         labels,
         selected_options,
         disabled_options,
         parent \\ nil
       ) do
    Enum.map(nodes, fn node ->
      case node do
        %{type: :element, children: children} ->
          node =
            case Map.fetch(states, node._selector_id) do
              {:ok, state} -> Map.put(node, :_initial_form_state, state)
              :error -> Map.delete(node, :_initial_form_state)
            end

          node =
            if node.tag in ~w(progress meter) and Map.get(node, :namespace, :html) == :html do
              node
              |> Map.delete(:_initial_form_state)
              |> Map.put(
                :_initial_gauge_state,
                NativeElixirPdfUtilities.Validators.HtmlGaugeValidator.prepare(
                  node.tag,
                  node.attributes
                )
              )
            else
              Map.delete(node, :_initial_gauge_state)
            end

          node =
            if node.tag == "legend" and Map.get(node, :namespace, :html) == :html and
                 match?(%{tag: "fieldset", _initial_form_state: _}, parent) do
              Map.put(node, :_initial_legend_form_owner, parent._initial_form_state.owner)
            else
              Map.delete(node, :_initial_legend_form_owner)
            end

          node =
            case Map.fetch(labels, node._selector_id) do
              {:ok, target} -> Map.put(node, :_initial_label_control, target)
              :error -> Map.delete(node, :_initial_label_control)
            end

          node =
            if node.tag == "option" and Map.get(node, :namespace, :html) == :html do
              node
              |> Map.put(
                :_initial_selected,
                Map.get(selected_options, node._selector_id, %{selected: false}).selected
              )
              |> Map.put(
                :_initial_option_label,
                Map.get(selected_options, node._selector_id, %{label: nil}).label
              )
              |> Map.put(
                :_initial_option_disabled,
                MapSet.member?(disabled_options, node._selector_id)
              )
            else
              Map.delete(node, :_initial_selected)
            end

          Map.put(
            node,
            :children,
            attach_initial_state(
              children,
              states,
              labels,
              selected_options,
              disabled_options,
              node
            )
          )

        _ ->
          node
      end
    end)
  end

  # Transport may be supplied directly to PdfWriter. Verify the canonical identities
  # and values agree, without consulting authored attributes or painted rows.
  defp valid_initial_selection?(selection) do
    case selection do
      %{
        choices: choices,
        value: value,
        values: values,
        selected_ids: ids,
        selected_indices: indices,
        multiple: multiple,
        listbox: listbox,
        display_size: display_size,
        rows: rows,
        id: id
      }
      when is_list(choices) and is_binary(value) and is_list(values) and is_list(ids) and
             is_list(indices) and is_boolean(multiple) and is_boolean(listbox) and
             is_integer(display_size) and display_size >= 0 and is_integer(rows) ->
        valid_choices? =
          choices
          |> Enum.with_index()
          |> Enum.all?(fn
            {%{
               id: id,
               index: index,
               value: value,
               label: label,
               selected: selected,
               disabled: disabled
             }, expected_index} ->
              is_reference(id) and index == expected_index and is_binary(value) and
                String.valid?(value) and is_binary(label) and String.valid?(label) and
                is_boolean(selected) and is_boolean(disabled)

            _ ->
              false
          end)

        if valid_choices? do
          selected = Enum.filter(choices, & &1.selected)
          first = List.first(selected)

          display_size <= NativeElixirPdfUtilities.Limits.get(:max_css_numeric_magnitude) and
            rows == if(display_size == 0, do: 4, else: display_size) and
            listbox == if(multiple, do: display_size != 1, else: display_size > 1) and
            ids == Enum.map(selected, & &1.id) and
            indices == Enum.map(selected, & &1.index) and
            values == Enum.map(selected, & &1.value) and
            value == if(is_nil(first), do: "", else: first.value) and
            id == if(is_nil(first), do: nil, else: first.id) and
            (multiple or length(selected) <= 1) and
            length(Enum.uniq_by(choices, & &1.id)) == length(choices)
        else
          false
        end

      _ ->
        false
    end
  end

  @doc false
  @spec prepare([map()], keyword()) :: {:ok, [map()], [map()]} | {:error, {atom(), map()}}
  def prepare(pages, opts) do
    case Keyword.get(opts, :forms, :interactive) do
      mode when mode in [:static, :interactive] ->
        with :ok <- validate_initial_controls(pages) do
          controls =
            pages
            |> Enum.with_index(1)
            |> Enum.flat_map(fn {page, number} ->
              page.boxes
              |> Enum.filter(&is_map(Map.get(&1, :form_control)))
              |> Enum.uniq_by(& &1.form_control.id)
              |> Enum.sort_by(& &1.form_control.order)
              |> Enum.with_index(1)
              |> Enum.map(fn {box, index} ->
                control = box.form_control

                type =
                  case control.tag do
                    "input" ->
                      type = HtmlValidator.input_type(Map.get(control.attributes, "type", "text"))

                      if HtmlValidator.readonly_input_type?(type) or type == "range",
                        do: "text",
                        else: type

                    "textarea" ->
                      "text"

                    "select" ->
                      "choice"
                  end

                %{box: box, control: control, type: type, page: number, index: index}
              end)
            end)

          explicit =
            controls
            |> Enum.map(&Map.get(&1.control.attributes, "name"))
            |> Enum.reject(&(&1 in [nil, ""]))
            |> MapSet.new()

          cond do
            mode == :static ->
              {:ok, pages, []}

            Enum.any?(controls, fn item ->
              Map.has_key?(item.control.initial_state, :range) and
                  not item.control.initial_state.disabled
            end) ->
              error(
                :unsupported_form,
                "Mutable HTML Range cannot be represented by the existing PDF field types without slider actions; use forms: :static for initial Range artwork",
                source: "range",
                operation: :write_pdf,
                module: __MODULE__
              )

            Enum.any?(controls, &(Map.get(&1.control, :temporal_display_mode) == :static)) ->
              error(
                :unsupported_form,
                "Temporal artwork was prepared for static forms; recompute styles with forms: :interactive before readonly scalar transport",
                source: "temporal",
                operation: :write_pdf,
                module: __MODULE__
              )

            Enum.any?(controls, fn item ->
              Map.has_key?(item.control.initial_state, :temporal) and
                not item.control.initial_state.disabled and
                  not item.control.initial_state.readonly
            end) ->
              error(
                :unsupported_form,
                "Mutable temporal controls require typed editing and picker actions unavailable in PDF text fields; use forms: :static for supplied initial artwork",
                source: "temporal",
                operation: :write_pdf,
                module: __MODULE__
              )

            length(controls) * 2 > Limits.get(:max_appearance_widgets) ->
              error(:resource_limit_exceeded, "HTML controls exceed the appearance count limit")

            length(controls) > Limits.get(:max_pdf_form_fields) ->
              error(:resource_limit_exceeded, "HTML controls exceed max_pdf_form_fields")

            length(Enum.uniq_by(controls, & &1.control.id)) != length(controls) ->
              error(:unsupported_form, "a control cannot span pages or repeat in page furniture")

            true ->
              # HTML radios keep per-element :required state, but PDF widgets in
              # one field share the group's submission requirement.
              required_radio_groups =
                Enum.reduce(controls, MapSet.new(), fn item, groups ->
                  name = Map.get(item.control.attributes, "name")

                  if item.type == "radio" and name not in [nil, ""] and
                       HtmlValidator.control_state_matches?(:required, item.control) do
                    MapSet.put(groups, {item.control.initial_state.owner, name})
                  else
                    groups
                  end
                end)

              controls
              |> Enum.reduce_while({:ok, [], explicit}, fn item, {:ok, fields, names} ->
                attributes = item.control.attributes

                name =
                  case Map.get(attributes, "name") do
                    name when is_binary(name) and name != "" ->
                      name

                    _ ->
                      base = "#{String.upcase(item.type)}_#{item.page}_#{item.index}"

                      Stream.iterate(0, &(&1 + 1))
                      |> Enum.find_value(fn suffix ->
                        candidate = if suffix == 0, do: base, else: "#{base}_#{suffix}"
                        if MapSet.member?(names, candidate), do: nil, else: candidate
                      end)
                  end

                choices =
                  if item.type == "choice" do
                    item.control.initial_state.selection.choices
                  else
                    []
                  end

                choice =
                  if item.type == "choice",
                    do:
                      NativeElixirPdfUtilities.Validators.FormValidator.initial_choice_field(
                        item.control.initial_state.selection
                      ),
                    else: %{flags: 0, value: nil, indices: [], listbox: false}

                value =
                  case item.type do
                    "text" ->
                      item.control.initial_state.value

                    "checkbox" ->
                      item.control.initial_state.checked

                    "radio" ->
                      if item.control.initial_state.checked,
                        do: Map.get(attributes, "value", "on"),
                        else: nil

                    "choice" ->
                      choice.value
                  end

                page = Enum.at(pages, item.page - 1)
                {page_width, page_height} = page.size

                original_page_appearance? =
                  item.type == "choice" or
                    Map.has_key?(item.control.initial_state, :temporal) or
                    (item.control.tag == "input" and
                       HtmlValidator.input_type(Map.get(attributes, "type", "text")) in [
                         "number",
                         "range"
                       ])

                box =
                  if original_page_appearance? do
                    snapped =
                      PageGeometry.snap_box_to_css_pixel_grid(%{
                        item.box
                        | y: page_height - item.box.y - item.box.height
                      })

                    snapped = %{snapped | y: page_height - snapped.y - snapped.height}

                    case PageGeometry.rounded_border_contours(snapped, page_height) do
                      [outer, _inner] -> outer
                      nil -> snapped
                    end
                  else
                    item.box
                  end

                appearance_boxes =
                  page.boxes
                  |> Enum.filter(&(Map.get(&1, :form_owner) == item.control.id))
                  |> Enum.map(fn box ->
                    if original_page_appearance? do
                      box
                    else
                      box
                      |> Map.put(:x, box.x - item.box.x)
                      |> Map.put(:y, box.y - item.box.y)
                      |> Map.delete(:form_owner)
                      |> then(fn box ->
                        case Map.get(box, :clip) do
                          nil ->
                            box

                          clip ->
                            clip = %{clip | x: clip.x - item.box.x, y: clip.y - item.box.y}

                            clip =
                              case Map.get(clip, :inner_clip) do
                                nil ->
                                  clip

                                inner ->
                                  Map.put(clip, :inner_clip, %{
                                    inner
                                    | x: inner.x - item.box.x,
                                      y: inner.y - item.box.y
                                  })
                              end

                            Map.put(box, :clip, clip)
                        end
                      end)
                    end
                  end)

                appearance = %{size: {box.width, box.height}, boxes: appearance_boxes}

                appearance =
                  if original_page_appearance?,
                    do:
                      Map.put(appearance, :paint_frame, %{
                        x: box.x,
                        y: box.y,
                        width: page_width,
                        height: page_height
                      }),
                    else: appearance

                visible_options =
                  MapSet.new(Enum.map(appearance_boxes, &Map.get(&1, :initial_option_id)))

                top_index =
                  Enum.find_value(choices, 0, fn choice ->
                    if MapSet.member?(visible_options, choice.id), do: choice.index
                  end)

                locked? =
                  item.control.initial_state.disabled or
                    Map.get(
                      item.control.initial_state,
                      :readonly,
                      Map.has_key?(attributes, "readonly")
                    )

                required? =
                  if item.type == "radio" and Map.get(attributes, "name") not in [nil, ""] do
                    MapSet.member?(
                      required_radio_groups,
                      {item.control.initial_state.owner, name}
                    )
                  else
                    HtmlValidator.control_state_matches?(:required, item.control)
                  end

                field = %{
                  name: name,
                  html_owner: item.control.initial_state.owner,
                  type: String.to_existing_atom(item.type),
                  value: value,
                  choices: choices,
                  selected_indices: choice.indices,
                  choice_listbox: choice.listbox,
                  top_index: top_index,
                  export: Map.get(attributes, "value", "on"),
                  page: item.page,
                  rect: [box.x, box.y, box.x + box.width, box.y + box.height],
                  flags:
                    choice.flags + if(locked?, do: 1, else: 0) +
                      if(required? and not locked?, do: 2, else: 0) +
                      if(item.control.tag == "textarea", do: 4096, else: 0) +
                      if(
                        item.control.tag == "input" and
                          HtmlValidator.input_type(Map.get(attributes, "type", "text")) ==
                            "password",
                        do: 8192,
                        else: 0
                      ),
                  appearance: appearance
                }

                range_appearance =
                  validate_initial_range_appearance(
                    item.control,
                    box,
                    appearance_boxes,
                    page_height
                  )

                cond do
                  range_appearance != :ok ->
                    {:halt, range_appearance}

                  byte_size(name) > Limits.get(:max_pdf_form_text_bytes) ->
                    {:halt,
                     error(
                       :resource_limit_exceeded,
                       "HTML field name exceeds max_pdf_form_text_bytes"
                     )}

                  String.contains?(name, [".", <<0>>]) ->
                    {:halt,
                     error(:invalid_form, "HTML field names cannot contain a period or NUL")}

                  Enum.any?(
                    fields,
                    &(&1.name == name and (&1.type != :radio or field.type != :radio))
                  ) ->
                    {:halt,
                     error(
                       :invalid_form,
                       "duplicate HTML name #{inspect(name)}; only radio groups may share a name"
                     )}

                  field.type == :radio and
                      Enum.any?(fields, &(&1.name == name and &1.html_owner != field.html_owner)) ->
                    {:halt,
                     error(
                       :unsupported_form,
                       "radio name #{inspect(name)} belongs to different HTML form owners; " <>
                         "use unique names or forms: :static instead of merging independent groups",
                       source: name
                     )}

                  field.type == :radio and
                      Enum.any?(fields, &(&1.name == name and &1.flags != field.flags)) ->
                    {:halt,
                     error(
                       :unsupported_form,
                       "radio group #{inspect(name)} mixes enabled and disabled controls; " <>
                         "make all members enabled or all disabled, or use forms: :static",
                       source: name
                     )}

                  field.type == :radio and field.export in ["", "Off"] ->
                    {:halt, error(:invalid_form, "radio export values cannot be empty or Off")}

                  field.type == :choice and
                      length(Enum.uniq_by(choices, & &1.value)) != length(choices) ->
                    {:halt,
                     error(
                       :invalid_form,
                       "select options require unique export values for interactive PDF choices; use distinct value attributes or forms: :static"
                     )}

                  field.type == :radio and
                      Enum.any?(
                        fields,
                        &(&1.name == name and
                              &1.export == field.export)
                      ) ->
                    {:halt,
                     error(
                       :invalid_form,
                       "radio groups require unique export values"
                     )}

                  true ->
                    {:cont, {:ok, [field | fields], MapSet.put(names, name)}}
                end
              end)
              |> case do
                {:ok, fields, _} ->
                  cleaned =
                    Enum.map(pages, fn page ->
                      %{
                        page
                        | boxes: Enum.reject(page.boxes, &(not is_nil(Map.get(&1, :form_owner))))
                      }
                    end)

                  bytes =
                    Enum.reduce(fields, 0, fn field, total ->
                      Enum.reduce(field.appearance.boxes, total, fn box, total ->
                        total + byte_size(Map.get(box, :text, ""))
                      end)
                    end)

                  if bytes <= Limits.get(:max_appearance_text_bytes),
                    do:
                      {:ok, cleaned,
                       fields |> Enum.reverse() |> Enum.map(&Map.delete(&1, :html_owner))},
                    else:
                      error(
                        :resource_limit_exceeded,
                        "HTML control appearance text exceeds the aggregate limit"
                      )

                failure ->
                  failure
              end
          end
        end

      _ ->
        error(:invalid_options, "forms must be :interactive or :static")
    end
  end

  defp validate_initial_controls(pages) do
    invalid? =
      Enum.any?(pages, fn page ->
        Enum.any?(page.boxes, fn box ->
          case Map.get(box, :form_control) do
            nil ->
              false

            %{
              id: _id,
              order: order,
              tag: tag,
              attributes: attributes,
              children: children,
              initial_state: %{checked: checked, owner: owner, disabled: disabled} = state
            }
            when tag in ["input", "textarea", "select"] and is_list(order) and is_map(attributes) and
                   is_list(children) ->
              selection_valid? =
                tag != "select" or valid_initial_selection?(Map.get(state, :selection))

              attributes_valid? =
                Enum.all?(attributes, fn {key, value} ->
                  is_binary(key) and String.valid?(key) and is_binary(value) and
                    String.valid?(value)
                end)

              number_valid? =
                attributes_valid? and
                  (tag != "input" or
                     HtmlValidator.input_type(Map.get(attributes, "type", "text")) != "number" or
                     valid_initial_number?(state, attributes))

              range_valid? =
                attributes_valid? and
                  (tag != "input" or
                     HtmlValidator.input_type(Map.get(attributes, "type", "text")) != "range" or
                     valid_initial_range?(state, attributes))

              temporal_valid? =
                attributes_valid? and
                  (tag != "input" or
                     not HtmlTemporalValidator.temporal_type?(
                       HtmlValidator.input_type(Map.get(attributes, "type", "text"))
                     ) or HtmlTemporalValidator.valid_initial?(state, attributes))

              not number_valid? or not range_valid? or not temporal_valid? or
                not is_boolean(checked) or
                not is_boolean(disabled) or
                not (is_nil(owner) or is_reference(owner)) or not selection_valid? or
                not attributes_valid? or
                ((tag == "textarea" or
                    (tag == "input" and
                       HtmlValidator.text_input_type?(
                         HtmlValidator.input_type(Map.get(attributes, "type", "text"))
                       ))) and
                   (not is_binary(Map.get(state, :value)) or
                      not String.valid?(Map.get(state, :value)))) or
                not HtmlValidator.valid_form_element?(tag, attributes, children) or
                (tag == "input" and
                   (HtmlValidator.button_input_type?(
                      HtmlValidator.input_type(Map.get(attributes, "type", "text"))
                    ) or
                      HtmlValidator.input_type(Map.get(attributes, "type", "text")) == "hidden"))

            _ ->
              true
          end
        end)
      end)

    if invalid? do
      error(
        :invalid_form,
        "HTML control requires validated initial checked, disabled and owner state, an applicable UTF-8 initial text value, and selection state with UTF-8 option values and labels",
        source: "initial_state",
        operation: :write_pdf,
        module: __MODULE__
      )
    else
      :ok
    end
  end

  # Existing PDF appearance BBoxes clip at the widget. A fixed native thumb
  # outside that rectangle cannot retain the initial source artwork safely.
  defp validate_initial_range_appearance(control, widget, boxes, page_height) do
    if Map.has_key?(control.initial_state, :range) do
      Enum.reduce_while(boxes, :ok, fn box, status ->
        if Map.get(box, :role) == :range_thumb do
          if NativeElixirPdfUtilities.Validators.WriterValidator.valid_range_frame?(box) do
            painted = NativeElixirPdfUtilities.HtmlToPdf.RangeArtwork.snap_box(box, page_height)

            if painted.x >= widget.x and painted.y >= widget.y and
                 painted.x + painted.width <= widget.x + widget.width and
                 painted.y + painted.height <= widget.y + widget.height do
              {:cont, status}
            else
              {:halt,
               error(
                 :unsupported_form,
                 "Disabled Range thumb paints outside the PDF widget rectangle; use forms: :static to preserve overflowing initial artwork",
                 module: __MODULE__,
                 operation: :write_pdf,
                 source: "range"
               )}
            end
          else
            {:halt,
             error(:invalid_form, "Range appearance requires a validated native thumb frame",
               module: __MODULE__,
               operation: :write_pdf,
               source: "range"
             )}
          end
        else
          {:cont, status}
        end
      end)
    else
      :ok
    end
  end

  defp error(reason, message, opts \\ []), do: Diagnostics.error(:forms, reason, message, opts)
end
