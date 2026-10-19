defmodule NativeElixirPdfUtilities.Validators.HtmlFormValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, PageGeometry}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.HtmlTemporalValidator

  @initial_context {__MODULE__, :initial_context}

  @doc false
  @spec prepare_composed_initial_nodes(map(), map(), [map()], map()) :: [map()]
  def prepare_composed_initial_nodes(logical, roots, projection, clone_logical_nodes) do
    if map_size(roots) == 0 and map_size(clone_logical_nodes) == 0 do
      prepare_initial_nodes(projection)
    else
      root_children =
        roots
        |> Enum.sort_by(fn {_scope, root} -> root.order end)
        |> Enum.flat_map(fn {_scope, root} -> root.children end)

      # The clone registry contains actual logical children, not distributed
      # slot children. Prefer its latest record when a selectedcontent changes.
      extra_roots =
        clone_logical_nodes
        |> Map.values()
        |> Enum.filter(fn node ->
          not Map.has_key?(clone_logical_nodes, Map.get(node, :parent_id))
        end)
        |> Enum.sort_by(&Map.get(&1, :_document_order, []))

      {forest, _seen} =
        logical_form_forest(
          logical.children ++ root_children ++ extra_roots,
          clone_logical_nodes,
          MapSet.new(),
          0
        )

      prepared = prepare_initial_nodes(forest)
      facts = initial_node_facts(prepared, %{})

      HtmlValidator.assign_render_metadata(projection)
      |> transfer_initial_node_facts(facts)
      |> then(
        &NativeElixirPdfUtilities.Validators.HtmlMetadataValidator.prepare_composed_nodes(
          logical,
          &1,
          clone_logical_nodes
        )
      )
    end
  end

  # A node may appear in both a root's child list and the clone registry. Walk
  # each real logical identity once, charging the shared traversal budget.
  defp logical_form_forest(nodes, registry, seen, depth) do
    HtmlValidator.check_render_resource(:max_html_depth, depth, :html)

    {reversed, seen} =
      Enum.reduce(nodes, {[], seen}, fn source, {output, seen} ->
        HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
        id = Map.get(source, :node_id)

        if id != nil and MapSet.member?(seen, id) do
          {output, seen}
        else
          node = Map.get(registry, id, source)
          seen = if id == nil, do: seen, else: MapSet.put(seen, id)

          case node do
            %{children: children} ->
              {children, seen} = logical_form_forest(children, registry, seen, depth + 1)
              {[%{node | children: children} | output], seen}

            _ ->
              {[node | output], seen}
          end
        end
      end)

    {Enum.reverse(reversed), seen}
  end

  defp initial_node_facts(nodes, facts) do
    Enum.reduce(nodes, facts, fn node, facts ->
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)

      facts =
        case Map.get(node, :node_id) do
          nil -> facts
          id -> Map.put(facts, id, node)
        end

      initial_node_facts(Map.get(node, :children, []), facts)
    end)
  end

  # Keep composed inertness, translation, headings and painting order. Only
  # logical identities and the form validator's own initial facts are copied.
  defp transfer_initial_node_facts(nodes, facts) do
    Enum.map(nodes, fn node ->
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)

      node =
        case Map.get(facts, Map.get(node, :node_id)) do
          nil ->
            node

          logical ->
            keys = [
              :_selector_id,
              :_initial_form_state,
              :_initial_gauge_state,
              :_initial_legend_form_owner,
              :_initial_label_control,
              :_initial_selected,
              :_initial_option_label,
              :_initial_option_disabled
            ]

            node = node |> Map.drop(keys) |> Map.merge(Map.take(logical, keys))

            if Map.has_key?(logical, :_initial_form_state),
              do: Map.put(node, :_initial_form_children, logical.children),
              else: node
        end

      case node do
        %{children: children} ->
          %{node | children: transfer_initial_node_facts(children, facts)}

        _ ->
          node
      end
    end)
  end

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
            Map.put_new(ids, {item.scope, id}, item)
          end)

        controls =
          context.controls
          |> Enum.reverse()
          |> Enum.map(fn item ->
            owner =
              if item.tag in ~w(form progress meter datalist) do
                nil
              else
                case Map.fetch(item.attributes, "form") do
                  {:ok, reference} ->
                    case Map.get(ids, {item.scope, reference}) do
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
              Map.put(winners, {item.scope, item.owner, name}, item.ref)
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
                do: Map.get(winners, {item.scope, item.owner, name}) == item.ref,
                else: Map.has_key?(item.attributes, "checked")

            state = %{owner: item.owner, checked: checked, disabled: item.disabled}

            state =
              if item.tag == "input",
                do:
                  Map.merge(state, %{
                    readonly: Map.has_key?(item.attributes, "readonly"),
                    required: Map.has_key?(item.attributes, "required")
                  }),
                else: state

            state =
              if item.tag == "input" and Map.has_key?(item.attributes, "list") do
                reference = Map.fetch!(item.attributes, "list")

                HtmlValidator.reserve_render_resource(
                  :max_html_tree_work,
                  byte_size(reference),
                  :style
                )

                association =
                  case Map.get(ids, {item.scope, reference}) do
                    %{tag: "datalist", namespace: :html} = source ->
                      Map.take(source, [:ref, :source_id, :scope]) |> Map.put(:id, reference)

                    _ ->
                      nil
                  end

                Map.put(state, :datalist, association)
              else
                state
              end

            state =
              if item.input_type == "radio" do
                indeterminate =
                  if name in [nil, ""],
                    do: not checked,
                    else: not Map.has_key?(winners, {item.scope, item.owner, name})

                Map.put(state, :indeterminate, indeterminate)
              else
                state
              end

            state =
              case {item.tag, item.input_type} do
                {"output", _} ->
                  references = Map.get(item.attributes, "for", "")

                  HtmlValidator.reserve_render_resource(
                    :max_html_tree_work,
                    byte_size(references),
                    :style
                  )

                  tokens = references |> HtmlValidator.html_class_tokens() |> Enum.uniq()

                  HtmlValidator.reserve_render_resource(
                    :max_html_tree_work,
                    length(tokens),
                    :style
                  )

                  sources =
                    Enum.map(tokens, fn id ->
                      case Map.get(ids, {item.scope, id}) do
                        nil -> %{id: id, ref: nil, source_id: nil, scope: item.scope}
                        source -> Map.take(source, [:ref, :source_id, :scope]) |> Map.put(:id, id)
                      end
                    end)

                  Map.merge(state, %{
                    value: item.output_value,
                    default_value: item.output_value,
                    default_value_override: nil,
                    disabled: false,
                    source_tokens: tokens,
                    sources: sources
                  })

                {"datalist", _} ->
                  HtmlValidator.reserve_render_resource(
                    :max_html_tree_work,
                    2 * length(item.datalist_options),
                    :style
                  )

                  Map.merge(state, %{
                    owner: nil,
                    disabled: false,
                    options: item.datalist_options,
                    suggestions:
                      Enum.filter(item.datalist_options, &(not &1.disabled and &1.value != ""))
                  })

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

                {"input", "color"} ->
                  {:ok, color} = prepare_initial_color(item.attributes)
                  Map.merge(state, color)

                {"input", "image"} ->
                  Map.merge(state, %{
                    value: Map.get(item.attributes, "value", ""),
                    image_input: %{
                      src: Map.get(item.attributes, "src", ""),
                      alt: Map.get(item.attributes, "alt", "")
                    }
                  })

                {"input", "file"} ->
                  Map.merge(state, %{
                    value: "",
                    files: [],
                    multiple: Map.has_key?(item.attributes, "multiple"),
                    required: Map.has_key?(item.attributes, "required"),
                    accept: Map.get(item.attributes, "accept", "")
                  })

                {"input", "number"} ->
                  Map.merge(state, prepare_initial_number(item.attributes))

                {"input", "range"} ->
                  options =
                    case Map.get(state, :datalist) do
                      %{id: id} ->
                        ids[{item.scope, id}].datalist_options
                        |> Enum.filter(&(not &1.disabled and &1.value != ""))
                        |> Enum.map(& &1.value)

                      _ ->
                        []
                    end

                  Map.merge(state, prepare_initial_range(item.attributes, options))

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
                    else: not Map.has_key?(winners, {item.scope, item.owner, name})

                Map.put(state, :indeterminate, indeterminate)
              else
                state
              end

            disabled_options =
              if item.tag == "datalist" do
                Enum.reduce(item.datalist_options, disabled_options, fn option, disabled ->
                  if option.disabled, do: MapSet.put(disabled, option.id), else: disabled
                end)
              else
                disabled_options
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

        states =
          NativeElixirPdfUtilities.Validators.HtmlConstraintValidator.prepare_initial_states(
            states,
            controls
          )

        labels =
          Map.new(context.labels, fn label ->
            target =
              case Map.fetch(label.attributes, "for") do
                {:ok, id} ->
                  case Map.get(ids, {label.scope, id}) do
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
  @spec prepare_initial_color(map()) :: {:ok, map()}
  def prepare_initial_color(attributes) do
    value = Map.get(attributes, "value", "")
    HtmlValidator.reserve_render_resource(:max_html_tree_work, 2 * byte_size(value), :html)

    parsed =
      NativeElixirPdfUtilities.Validators.CssColorValidator.parse_initial_input_color(value)

    channels =
      case parsed do
        {:ok, color} when is_tuple(color) ->
          color |> Tuple.to_list() |> Enum.take(3) |> Enum.map(&round(&1 * 255))

        _ ->
          [0, 0, 0]
      end

    # Pinned Chromium152 has InputTypeColorEnhancements disabled. Alpha and
    # colorspace remain authored metadata; initial CSS colors become opaque
    # 8-bit sRGB even when these attributes are present.
    rgb = channels |> Enum.map(&(&1 / 255)) |> List.to_tuple()
    canonical = "#" <> Base.encode16(:erlang.list_to_binary(channels), case: :lower)
    {:ok, %{value: canonical, color_input: %{value: canonical, rgb: rgb}}}
  end

  @doc false
  @spec validate_initial_special_attributes(map()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_initial_special_attributes(attributes) do
    if HtmlValidator.input_type(Map.get(attributes, "type", "text")) == "color" do
      {:ok, _} = prepare_initial_color(attributes)
      :ok
    else
      :ok
    end
  end

  @doc "Resolves automatic checkbox/radio dimensions using the pinned native-theme footprint."
  @spec prepare_initial_toggle_style(map(), map()) :: {:ok, map()}
  def prepare_initial_toggle_style(node, style) do
    attributes = initial_control_attributes(node)
    type = HtmlValidator.input_type(Map.get(attributes, "type", "text"))

    if Map.get(node, :namespace, :html) == :html and node.tag == "input" and
         type in ~w(checkbox radio) and Map.get(style, :appearance, :auto) == :auto do
      # Chromium's native checkbox/radio theme has a 13 CSS-pixel footprint;
      # this is a pinned UA default, not a resource ceiling or font-relative size.
      style =
        Enum.reduce([:width, :height], style, fn key, result ->
          if Map.get(result, key) in [nil, :auto], do: Map.put(result, key, 9.75), else: result
        end)

      # A native replaced toggle has no scrolling content box. Use its existing
      # native paint path instead of making its fallback glyph a scrollport.
      style =
        if Map.get(style, :_popover, false) and Map.get(style, :overflow) == :auto,
          do: Map.put(style, :overflow, :visible),
          else: style

      {:ok, style}
    else
      {:ok, style}
    end
  end

  @doc "Resolves skipped initial input intrinsic dimensions without preparing internal artwork."
  @spec prepare_skipped_input_style(map(), map(), [{String.t(), String.t()}]) :: map()
  def prepare_skipped_input_style(node, style, declarations) do
    if node.tag == "input" and Map.get(node, :namespace, :html) == :html and
         Map.get(style, :content_visibility) == :hidden and
         Map.get(style, :display) not in [:inline, :contents, :none] do
      attributes = initial_control_attributes(node)
      kind = HtmlValidator.input_type(Map.get(attributes, "type", "text"))
      properties = MapSet.new(declarations, &elem(&1, 0))

      style =
        case kind do
          kind when kind in ~w(date month week time datetime-local) ->
            Enum.reduce([:width, :height], style, fn key, result ->
              if result[key] in [nil, :auto], do: Map.put(result, key, 0.0), else: result
            end)

          "range" ->
            if style.height in [nil, :auto] or not MapSet.member?(properties, "height"),
              do: Map.put(style, :height, 0.0),
              else: style

          "file" ->
            style =
              if style.width in [nil, :auto] do
                size = Float.floor(style.font_size / 0.75 * 64) / 64 * 0.75

                width =
                  Float.ceil(
                    34 *
                      NativeElixirPdfUtilities.HtmlToPdf.Font.text_width(
                        "0",
                        style.font_face,
                        size,
                        style.letter_spacing
                      ) / 0.75
                  ) * 0.75

                HtmlValidator.check_render_resource(:max_css_numeric_magnitude, width, :style)
                Map.put(style, :width, width)
              else
                style
              end

            if style.height in [nil, :auto], do: Map.put(style, :height, 0.0), else: style

          "image" ->
            {:ok, hints} =
              HtmlValidator.html_dimension_hints(
                attributes,
                [{"width", :width}, {"height", :height}],
                "input/image"
              )

            Enum.reduce([:width, :height], style, fn key, result ->
              if result[key] in [nil, :auto] or
                   (not MapSet.member?(properties, Atom.to_string(key)) and
                      not Map.has_key?(hints, key)),
                 do: Map.put(result, key, 0.0),
                 else: result
            end)
            |> Map.drop([:image, :_responsive_image])
            |> Map.put(
              :display,
              if(style.display == :image,
                do:
                  if(Map.get(style, :image_display) == :inline, do: :inline_block, else: :block),
                else: style.display
              )
            )

          _ ->
            style
        end

      if kind in ~w(color range file image date month week time datetime-local) do
        style = Map.drop(style, [:_temporal_state, :_image_input_state])

        if kind in ~w(color range) do
          Map.put(style, if(kind == "color", do: :_color_control, else: :_range_control), %{
            tag: "input",
            attributes: attributes,
            initial_state: node._initial_form_state
          })
        else
          Map.put(style, :form_kind, :skipped_input)
        end
      else
        style
      end
    else
      style
    end
  end

  @doc false
  @spec prepare_special_input_style(map(), map(), keyword()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_special_input_style(node, style, opts) do
    state = Map.get(node, :_initial_form_state, %{})
    type = HtmlValidator.input_type(Map.get(initial_control_attributes(node), "type", "text"))

    cond do
      Map.get(node, :namespace, :html) != :html or node.tag != "input" or style.display == :none ->
        {:ok, style}

      Map.get(style, :_html_content_skipped, false) ->
        {:ok, style}

      type == "image" and Keyword.get(opts, :forms, :interactive) == :interactive and
        not state.disabled and not Map.has_key?(node, :_html_inert) ->
        Diagnostics.error(
          :forms,
          :unsupported_form,
          "Image-input submit coordinates require native actions unavailable in PDF controls; use forms: :static for the authorized initial image",
          module: __MODULE__,
          operation: :compute,
          source: "input/image"
        )

      type == "color" ->
        # Preserve authored percentages and calculations for the shared used-size
        # resolver. Automatic dimensions retain the native control's footprint.
        style =
          Enum.reduce([width: 9.0, height: 10.5], style, fn {axis, intrinsic}, style ->
            if Map.get(style, axis) in [nil, :auto],
              do: Map.put(style, axis, intrinsic),
              else: style
          end)

        {:ok, style}

      true ->
        {:ok, style}
    end
  end

  @doc false
  @spec validate_initial_color_layout(map()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_initial_color_layout(node) do
    case node do
      %{style: %{display: :none}} ->
        :ok

      %{style: %{_color_state: color} = style} ->
        valid? =
          case Map.get(style, :_form_control) || Map.get(style, :_color_control) do
            %{
              tag: "input",
              attributes: attrs,
              initial_state: %{value: value, disabled: disabled} = state
            }
            when is_map(attrs) and is_binary(value) and is_boolean(disabled) ->
              valid_attributes? =
                Enum.all?(attrs, fn {key, value} ->
                  is_binary(key) and String.valid?(key) and is_binary(value) and
                    String.valid?(value)
                end)

              case if(valid_attributes?, do: prepare_initial_color(attrs), else: :error) do
                {:ok, expected} ->
                  HtmlValidator.input_type(Map.get(attrs, "type", "text")) == "color" and
                    color == expected.color_input and state.value == expected.value and
                    Map.get(state, :color_input) == color and
                    Map.get(style, :_color_disabled) == state.disabled

                _ ->
                  false
              end

            _ ->
              false
          end

        if valid?,
          do: :ok,
          else:
            Diagnostics.error(
              :layout,
              :invalid_layout,
              "Color layout requires canonical initial value, swatch and disabled state; compute styles before layout",
              module: __MODULE__,
              operation: :layout,
              source: "color"
            )

      %{style: style} when is_map(style) ->
        case Map.get(style, :_form_control) || Map.get(style, :_color_control) do
          %{tag: "input", attributes: attributes} ->
            if not is_map(attributes) or
                 HtmlValidator.input_type(Map.get(attributes, "type", "text")) == "color" do
              Diagnostics.error(
                :layout,
                :invalid_layout,
                "Color layout requires canonical swatch state; compute styles before layout",
                module: __MODULE__,
                operation: :layout,
                source: "color"
              )
            else
              :ok
            end

          _ ->
            :ok
        end

      _ ->
        :ok
    end
  end

  @doc false
  @spec prepare_initial_range(map()) :: map()
  @spec prepare_initial_range(map(), [String.t()]) :: map()
  def prepare_initial_range(attributes, options \\ []) do
    HtmlValidator.reserve_render_resource(:max_html_tree_work, length(options), :html)

    option_numbers =
      Enum.map(options, fn value ->
        if HtmlValidator.valid_html_float?(value) do
          {:ok, decimal} = HtmlValidator.parse_html_float(value, :decimal)
          decimal
        end
      end)

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
      Enum.reduce(Map.values(parsed) ++ option_numbers, 0, fn value, acc ->
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
        options: options,
        ticks:
          Enum.flat_map(option_numbers, fn decimal ->
            if decimal && maximum > minimum do
              tick =
                if(decimal.negative, do: -1, else: 1) * decimal.coefficient *
                  Integer.pow(10, decimal.exponent - exponent)

              if tick >= minimum and tick <= maximum and
                   (step == :any or rem(tick - base, step) == 0) do
                [
                  %{
                    value: decimal.value,
                    position:
                      div((tick - minimum) * Integer.pow(10, 18), maximum - minimum) / 1.0e18
                  }
                ]
              else
                []
              end
            else
              []
            end
          end),
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
      is_map(Map.get(state, :range)) and is_list(Map.get(state.range, :options)) and
      HtmlValidator.check_render_resource(:max_html_tree_work, length(state.range.options), :html) ==
        :ok and
      Enum.all?(state.range.options, &(is_binary(&1) and String.valid?(&1))) and
      Map.take(state, [:value, :display, :readonly, :required, :range]) ==
        prepare_initial_range(attributes, state.range.options)
  rescue
    ArgumentError -> false
  end

  # Tick candidates come from an actual same-tree datalist association. The
  # numeric preparation helper also serves standalone canonical-value callers;
  # layout and transport must retain the source association it was supplied.
  defp valid_initial_range_association?(state, attributes) do
    case state.range.options do
      [] ->
        true

      _ ->
        case Map.get(state, :datalist) do
          %{id: id, ref: ref, scope: scope} ->
            Map.get(attributes, "list") == id and is_reference(ref) and is_tuple(scope)

          _ ->
            false
        end
    end
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
                valid_initial_range?(state, attributes) and
                valid_initial_range_association?(state, attributes) and state.range == range and
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
  @spec prepare_initial_number_style(map(), [
          NativeElixirPdfUtilities.HtmlToPdf.CssParser.declaration()
        ]) :: map()
  defdelegate prepare_initial_number_style(style, declarations),
    to: __MODULE__,
    as: :prepare_initial_textfield_style

  @doc false
  @spec prepare_initial_textfield_style(map(), [
          NativeElixirPdfUtilities.HtmlToPdf.CssParser.declaration()
        ]) :: map()
  def prepare_initial_textfield_style(style, declarations) do
    native? =
      style.appearance == :auto and
        not Enum.any?(declarations, fn declaration ->
          property = elem(declaration, 0)

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
  @spec prepare_initial_temporal_style(map(), [
          NativeElixirPdfUtilities.HtmlToPdf.CssParser.declaration()
        ]) :: map()
  def prepare_initial_temporal_style(style, declarations) do
    # Rounded corners defer TextField background/border paint to CSS. Preserve
    # the UA inset border instead of the native one-pixel frame in that case.
    style =
      if style.border_radius > 0,
        do: Map.put(style, :_native_menu, false),
        else: prepare_initial_textfield_style(style, declarations)

    # The UA font shorthand resets inherited line height. An authored temporal
    # line-height or font shorthand still wins in the ordinary cascade.
    if Enum.any?(declarations, fn declaration ->
         elem(declaration, 0) in ["line-height", "font"]
       end),
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

  @doc "Returns the initial single-line input/menu editor's used vertical offset; multiline textarea content remains top aligned."
  @spec initial_control_content_offset(map(), number()) :: float()
  def initial_control_content_offset(style, height) do
    if Map.get(style, :form_kind) in [:text, :select],
      do: initial_number_content_offset(style, height),
      else: 0.0
  end

  @doc "Returns the fixed Linux initial textarea resize strokes in CSS pixels, before actual control placement."
  @spec initial_resizer_strokes(map(), number(), number()) :: [map()]
  def initial_resizer_strokes(style, width, height) do
    if width > 0 and height > 0 and Map.get(style, :form_kind) == :textarea and
         Map.get(style, :resize) != :none and
         Map.get(style, :overflow) in [:auto, :scroll, :hidden] do
      # Chromium's 15px CornerRect uses integer division for the half and
      # three-quarter endpoints. ScopedRasterFlags turns the 1px stroke into
      # a PDF hairline and scales its 153/255 alpha by the .75 print matrix.
      for {left, size} <- [{8, 7}, {4, 3}],
          {shift, color} <- [{0, {0, 0, 0, 0.45}}, {1, {1, 1, 1, 0.45}}] do
        %{left: left - shift, bottom: 1, size: size - shift, color: color}
      end
    else
      []
    end
  end

  @doc false
  @spec readonly_input_type?(String.t()) :: boolean()
  def readonly_input_type?(type),
    do:
      HtmlValidator.text_input_type?(type) or type == "number" or
        HtmlTemporalValidator.temporal_type?(type)

  @doc "Whether an input attribute participates in the effective HTML input state."
  @spec input_attribute_applies?(String.t(), String.t()) :: boolean()
  def input_attribute_applies?(type, name) do
    # This fixed applicability table follows HTML input states, not resource limits.
    case name do
      "checked" ->
        type in ~w(checkbox radio)

      "readonly" ->
        readonly_input_type?(type)

      "multiple" ->
        type in ~w(email file)

      name when name in ~w(size maxlength minlength pattern) ->
        HtmlValidator.text_input_type?(type)

      "placeholder" ->
        HtmlValidator.text_input_type?(type) or type == "number"

      "list" ->
        type in ~w(text search tel url email number range color date month week time datetime-local)

      name when name in ~w(min max step) ->
        type in ~w(number range) or HtmlTemporalValidator.temporal_type?(type)

      name when name in ~w(src alt width height) ->
        type == "image"

      name when name in ~w(alpha colorspace) ->
        type == "color"

      name when name in ~w(accept capture) ->
        type == "file"

      "required" ->
        readonly_input_type?(type) or type in ~w(checkbox radio file)

      "autocomplete" ->
        type in ~w(hidden text search tel url email password number range color date month week time datetime-local)

      "dirname" ->
        HtmlValidator.text_input_type?(type) or type in ~w(hidden submit)

      name when name in ~w(formaction formenctype formmethod formnovalidate formtarget) ->
        type in ~w(submit image)

      name when name in ~w(popovertarget popovertargetaction) ->
        type in ~w(submit image reset button)

      _ ->
        true
    end
  end

  @doc "Prepares applicable input semantics without changing the authored attributes retained on the node."
  @spec applicable_input_attributes(map(), map()) :: map()
  def applicable_input_attributes(attributes, namespaces \\ %{}) do
    HtmlValidator.reserve_render_resource(:max_html_tree_work, map_size(attributes), :html)
    attributes = Map.reject(attributes, fn {name, _} -> not is_nil(Map.get(namespaces, name)) end)
    type = HtmlValidator.input_type(Map.get(attributes, "type", "text"))
    Map.reject(attributes, fn {name, _} -> not input_attribute_applies?(type, name) end)
  end

  @doc "Returns unqualified control attributes and effective input applicability for validated initial state."
  @spec initial_control_attributes(map()) :: map()
  def initial_control_attributes(node) do
    attributes = Map.get(node, :attributes, %{})
    namespaces = Map.get(node, :attribute_namespaces, %{})

    cond do
      Map.get(node, :namespace, :html) != :html ->
        attributes

      Map.get(node, :tag) == "input" ->
        applicable_input_attributes(attributes, namespaces)

      true ->
        Map.reject(attributes, fn {name, _} ->
          not is_nil(Map.get(namespaces, name)) or
            (name in ~w(checked disabled readonly multiple required selected alpha) and
               not HtmlValidator.valid_boolean_form_attribute?(Map.get(node, :tag), name))
        end)
    end
  end

  @doc false
  @spec valid_initial_input?(String.t(), map(), [term()]) :: boolean()
  def valid_initial_input?(_type, _attributes, children), do: children == []

  defp collect_initial_metadata(
         nodes,
         form_owner \\ nil,
         label_ref \\ nil,
         ancestor_disabled? \\ false,
         in_datalist? \\ false,
         parent_scope \\ nil,
         fieldsets \\ []
       ) do
    Enum.each(nodes, fn node ->
      case node do
        %{type: :element, children: children} ->
          html? = Map.get(node, :namespace, :html) == :html
          scope = {Map.get(node, :document_id), Map.get(node, :tree_scope_id)}

          {form_owner, label_ref, ancestor_disabled?, in_datalist?, fieldsets} =
            if parent_scope != nil and scope != parent_scope,
              do: {nil, nil, false, false, []},
              else: {form_owner, label_ref, ancestor_disabled?, in_datalist?, fieldsets}

          if node.tag in ~w(form input select textarea button label fieldset progress meter output datalist) or
               Map.has_key?(node.attributes, "id") do
            register_initial_node(
              node,
              form_owner,
              label_ref,
              ancestor_disabled?,
              in_datalist?,
              fieldsets
            )
          end

          next_owner =
            if node.tag == "form" and Map.get(node, :namespace, :html) == :html,
              do: node._selector_id,
              else: form_owner

          next_label =
            if node.tag == "label" and Map.get(node, :namespace, :html) == :html,
              do: node._selector_id,
              else: label_ref

          next_datalist = in_datalist? or (html? and node.tag == "datalist")

          next_fieldsets =
            if html? and node.tag == "fieldset",
              do: [node._selector_id | fieldsets],
              else: fieldsets

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
                ancestor_disabled? or not exempt?,
                next_datalist,
                scope,
                next_fieldsets
              )
            end)
          else
            collect_initial_metadata(
              children,
              next_owner,
              next_label,
              ancestor_disabled?,
              next_datalist,
              scope,
              next_fieldsets
            )
          end

        _ ->
          :ok
      end
    end)
  end

  # Metadata is already assigned. Collect only the ID/control facts needed
  # to resolve forward references, label targets and initial radio groups.
  defp register_initial_node(
         node,
         form_owner,
         label_ref,
         ancestor_disabled?,
         in_datalist?,
         fieldsets
       ) do
    attributes = initial_control_attributes(node)
    namespace = Map.get(node, :namespace, :html)

    relevant? =
      namespace == :html and
        node.tag in ~w(form input select textarea button label fieldset progress meter output datalist)

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
        scope: {Map.get(node, :document_id), Map.get(node, :tree_scope_id)},
        source_id: Map.get(node, :node_id),
        input_type: input_type,
        order: node._document_order,
        ancestor_owner: form_owner,
        fieldsets: fieldsets,
        in_datalist: in_datalist?,
        placeholder_option: nil,
        disabled:
          node.tag not in ~w(form datalist) and
            (ancestor_disabled? or Map.has_key?(attributes, "disabled"))
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
        case {namespace, node.tag} do
          {:html, "output"} ->
            Map.put(item, :output_value, HtmlValidator.descendant_text_content(node.children))

          {:html, "datalist"} ->
            Map.put(item, :datalist_options, initial_datalist_options(node.children))

          _ ->
            item
        end

      item =
        if node.tag == "select" and namespace == :html do
          options = HtmlValidator.select_options(node.children)
          first = List.first(options)

          placeholder =
            if first != nil and
                 Enum.any?(node.children, &(Map.get(&1, :_selector_id) == first._selector_id)) and
                 Map.get(
                   first.attributes,
                   "value",
                   HtmlValidator.initial_option_text(first.children)
                 ) == "", do: first._selector_id, else: nil

          item |> Map.put(:options, options) |> Map.put(:placeholder_option, placeholder)
        else
          item
        end

      context =
        case Map.get(attributes, "id", "") do
          "" -> context
          id -> %{context | ids: [{id, item} | context.ids]}
        end

      context =
        case {namespace, node.tag} do
          {:html, "label"} ->
            %{context | labels: [item | context.labels]}

          {:html, tag}
          when tag in ~w(form input select textarea button fieldset progress meter output datalist) ->
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

  # Datalist collects descendant options, including a legacy fallback select.
  # Its suggestions do not acquire selection or change an input's authored value.
  defp initial_datalist_options(children, ancestor_disabled? \\ false) do
    Enum.flat_map(children, fn node ->
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :style)

      case node do
        %{type: :element, tag: "option", children: nested} ->
          if Map.get(node, :namespace, :html) == :html do
            attributes = initial_control_attributes(node)
            text = HtmlValidator.initial_option_text(nested)

            [
              %{
                id: node._selector_id,
                source_id: Map.get(node, :node_id),
                value: Map.get(attributes, "value", text),
                label: initial_option_label(attributes, text),
                disabled: ancestor_disabled? or Map.has_key?(attributes, "disabled")
              }
            ]
          else
            []
          end

        %{type: :element, tag: tag, children: nested} ->
          attributes = initial_control_attributes(node)

          disabled? =
            ancestor_disabled? or
              (tag == "optgroup" and Map.get(node, :namespace, :html) == :html and
                 Map.has_key?(attributes, "disabled"))

          initial_datalist_options(nested, disabled?)

        _ ->
          []
      end
    end)
  end

  # Both suggestion sources and visible options use the HTML label fallback.
  defp initial_option_label(attributes, text) do
    case Map.get(attributes, "label", "") do
      "" -> text
      label -> label
    end
  end

  @doc false
  @spec prepare_initial_auxiliary_style(map(), map()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_initial_auxiliary_style(node, style) do
    state = Map.get(node, :_initial_form_state, %{})

    cond do
      Map.get(node, :namespace, :html) != :html ->
        {:ok, style}

      node.tag == "datalist" ->
        {:ok, Map.put(style, :_datalist_state, state)}

      node.tag == "output" ->
        {:ok, Map.put(style, :_output_state, state)}

      true ->
        {:ok, style}
    end
  end

  @doc "Determines native select-row metrics from an option's validated HTML ancestor chain."
  @spec native_select_option?([map()]) :: boolean()
  def native_select_option?(ancestors) do
    HtmlValidator.reserve_render_resource(:max_html_tree_work, length(ancestors), :html)

    Enum.any?(ancestors, fn ancestor ->
      Map.get(ancestor, :tag) == "select" and Map.get(ancestor, :namespace, :html) == :html
    end)
  end

  @doc "Applies popover defaults while retaining a native control's intrinsic dimensions."
  @spec prepare_popover_defaults(map(), map()) :: map()
  def prepare_popover_defaults(defaults, popover_defaults) do
    merged = Map.merge(defaults, popover_defaults)

    merged =
      if Map.get(defaults, :form_kind) in [:checkbox, :radio] do
        # The native toggle UA rules reset border/padding more specifically
        # than the global popover rule. Preserve their established appearance.
        merged
        |> Map.merge(
          Map.take(defaults, [:padding, :border_widths, :border_styles, :background_color])
        )
        |> Map.delete(:_popover_default_padding)
      else
        merged
      end

    if Map.has_key?(defaults, :form_kind),
      do: Map.put(merged, :_popover_intrinsic_dimensions, Map.take(defaults, [:width, :height])),
      else: merged
  end

  @doc "Resolves native popover fit-content dimensions through the existing control sizing rules."
  @spec resolve_popover_dimensions(map()) :: map()
  def resolve_popover_dimensions(style) do
    case Map.pop(style, :_popover_intrinsic_dimensions) do
      {nil, style} ->
        style

      {intrinsic, style} ->
        Enum.reduce([:width, :height], style, fn dimension, style ->
          if Map.get(style, dimension) == :fit_content,
            do: Map.put(style, dimension, Map.get(intrinsic, dimension, :auto)),
            else: style
        end)
    end
  end

  @doc "Keeps native HTML controls as atomic inline boxes after the actual CSS cascade."
  @spec prepare_initial_control_box(map(), map()) :: map()
  def prepare_initial_control_box(node, style) do
    native? =
      Map.get(node, :namespace, :html) == :html and
        Map.get(node, :tag) in ~w(input textarea select button) and
        Map.get(style, :form_kind) not in [nil, :hidden, :base_select] and
        not Map.has_key?(style, :_image_input_state)

    if native? and style.display == :inline,
      do: Map.put(style, :display, :inline_block),
      else: style
  end

  @doc "Keeps normalized control fragments as root blocks while full HTML ancestry uses native inline flow."
  @spec initial_control_display(map(), atom(), [map()]) :: atom()
  def initial_control_display(node, display, ancestors) do
    ordinary? =
      Map.get(node, :namespace, :html) == :html and
        (node.tag in ~w(textarea select button) or
           (node.tag == "input" and
              HtmlValidator.input_type(Map.get(initial_control_attributes(node), "type", "text")) in ~w(text search tel url email password checkbox radio button reset submit)))

    if display == :inline_block and ancestors == [] and ordinary?, do: :block, else: display
  end

  defp labelable_initial_element?(item) do
    item.namespace == :html and
      (item.tag in ~w(select textarea button progress meter output) or
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

    authored = Enum.filter(options, &Map.has_key?(initial_control_attributes(&1), "selected"))

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
        attributes = initial_control_attributes(option)
        text = HtmlValidator.initial_option_text(option.children)

        %{
          id: option._selector_id,
          index: index,
          value: Map.get(attributes, "value", text),
          label: initial_option_label(attributes, text),
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
              attributes = initial_control_attributes(node)

              label =
                case Map.fetch(selected_options, node._selector_id) do
                  {:ok, choice} ->
                    choice.label

                  :error ->
                    initial_option_label(
                      attributes,
                      HtmlValidator.initial_option_text(node.children)
                    )
                end

              node
              |> Map.put(
                :_initial_selected,
                Map.get(selected_options, node._selector_id, %{
                  selected: Map.has_key?(attributes, "selected")
                }).selected
              )
              |> Map.put(
                :_initial_option_label,
                label
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

  @doc "Validates the shared static or interactive forms mode for rendering and field preparation."
  @spec prepare_mode(term(), :forms | :options) ::
          {:ok, :static | :interactive} | {:error, Diagnostics.detailed_error()}
  def prepare_mode(mode, stage \\ :forms) do
    case mode do
      mode when mode in [:static, :interactive] ->
        {:ok, mode}

      _ ->
        Diagnostics.error(stage, :invalid_options, "forms must be :interactive or :static",
          module: __MODULE__,
          operation: :prepare_forms_mode,
          source: "forms"
        )
    end
  end

  @doc "Runs nested native layout with its validated forms mode, restoring the enclosing document afterward."
  @spec with_layout_mode(term(), (-> term())) :: term()
  def with_layout_mode(mode, callback) do
    with {:ok, mode} <- prepare_mode(mode), true <- is_function(callback, 0) do
      key = {__MODULE__, :layout_forms_mode}
      previous = Process.get(key)
      Process.put(key, mode)

      try do
        callback.()
      after
        if previous == nil, do: Process.delete(key), else: Process.put(key, previous)
      end
    else
      false ->
        Diagnostics.error(
          :forms,
          :invalid_options,
          "native forms layout requires a zero-argument callback",
          module: __MODULE__,
          operation: :with_layout_mode,
          source: "forms"
        )

      failure ->
        failure
    end
  end

  @doc false
  @spec layout_mode() :: :static | :interactive
  def layout_mode, do: Process.get({__MODULE__, :layout_forms_mode}, :interactive)

  @doc "Prepares child controls in an issued document scope, retaining their actual HTML owner and control identity for final composition."
  @spec prepare_scoped([map()], keyword(), NativeElixirPdfUtilities.HtmlToPdf.ResourceContext.t()) ::
          {:ok, [map()], [map()]} | {:error, {atom(), map()}}
  def prepare_scoped(pages, opts, context) do
    with :ok <- NativeElixirPdfUtilities.Validators.ResourceValidator.validate_context(context) do
      prepare(pages, Keyword.put(opts, :__retain_html_identity__, true))
    end
  end

  @doc false
  @spec prepare([map()], keyword()) :: {:ok, [map()], [map()]} | {:error, {atom(), map()}}
  def prepare(pages, opts) do
    case prepare_mode(Keyword.get(opts, :forms, :interactive)) do
      {:ok, mode} ->
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

                      if HtmlValidator.readonly_input_type?(type) or type in ["range", "color"],
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

          # Rounded dashed native menus and disabled native selects share their
          # real control surface. Composite its background/arrow/border once.
          select_groups =
            controls
            |> Enum.filter(
              &(&1.control.tag == "select" and
                  Map.get(&1.box, :role) in [:native_menu, :css_menu, :native_listbox] and
                  (&1.control.initial_state.disabled or is_map(Map.get(&1.box, :native_menu_clip))))
            )
            |> Map.new(fn item ->
              opacity = %{
                owner: item.control.id,
                alpha: if(item.control.initial_state.disabled, do: 0.7, else: 1.0)
              }

              opacity =
                case Map.get(item.box, :native_menu_clip) do
                  nil -> opacity
                  clip -> Map.put(opacity, :clip, clip)
                end

              {item.control.id, opacity}
            end)

          pages =
            Enum.map(pages, fn page ->
              %{
                page
                | boxes:
                    Enum.map(page.boxes, fn box ->
                      case Map.get(select_groups, Map.get(box, :form_owner)) do
                        nil -> box
                        group -> Map.put(box, :initial_select_opacity, group)
                      end
                    end)
              }
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

            Enum.any?(
              controls,
              &(Map.has_key?(&1.control.initial_state, :color_input) and
                    not &1.control.initial_state.disabled)
            ) ->
              error(
                :unsupported_form,
                "Mutable color picker actions are unavailable in PDF text fields; use forms: :static for initial swatch artwork",
                source: "color",
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

            Enum.any?(controls, fn item ->
              not is_nil(Map.get(item.control.initial_state, :datalist)) and
                not item.control.initial_state.disabled and
                  not Map.get(
                    item.control.initial_state,
                    :readonly,
                    Map.has_key?(item.control.attributes, "readonly")
                  )
            end) ->
              error(
                :unsupported_form,
                "Mutable datalist suggestions require selection actions unavailable in PDF text fields; use readonly or disabled scalar transport",
                source: "input/list",
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
              HtmlValidator.reserve_render_resource(
                :max_pdf_form_fields,
                length(controls),
                :forms
              )

              HtmlValidator.reserve_render_resource(
                :max_appearance_widgets,
                length(controls) * 2,
                :forms
              )

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

                      available_field_name(base, names)
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
                    Map.has_key?(item.control.initial_state, :color_input) or
                    (item.control.tag == "input" and
                       HtmlValidator.input_type(Map.get(attributes, "type", "text")) in [
                         "number",
                         "range"
                       ])

                source_page_appearance? =
                  original_page_appearance? or
                    Keyword.get(opts, :__retain_html_identity__, false)

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
                  Enum.filter(page.boxes, &(Map.get(&1, :form_owner) == item.control.id))

                appearance_boxes =
                  if source_page_appearance? do
                    appearance_boxes
                  else
                    appearance_boxes
                    |> DrawingGeometry.translate_boxes(-box.x, -box.y)
                    |> Enum.map(&Map.delete(&1, :form_owner))
                  end

                appearance = %{
                  size: {box.width, box.height},
                  boxes: appearance_boxes
                }

                appearance =
                  if source_page_appearance? do
                    Map.put(appearance, :paint_frame, %{
                      x: box.x,
                      y: box.y,
                      width: page_width,
                      height: page_height
                    })
                  else
                    appearance
                  end

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
                  html_control_id: item.control.id,
                  html_name_explicit: Map.get(attributes, "name") not in [nil, ""],
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

                field =
                  Map.put(
                    field,
                    :source_editor,
                    NativeElixirPdfUtilities.Validators.FormAppearanceValidator.capture(
                      field,
                      item.control
                    )
                  )

                range_appearance =
                  validate_initial_range_appearance(
                    item.control,
                    box,
                    appearance_boxes,
                    page_height
                  )

                name_validation = validate_field_identity(field, fields)

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

                  name_validation != :ok ->
                    {:halt, name_validation}

                  field.type == :radio and field.export in ["", "Off"] ->
                    {:halt, error(:invalid_form, "radio export values cannot be empty or Off")}

                  field.type == :choice and
                      length(Enum.uniq_by(choices, & &1.value)) != length(choices) ->
                    {:halt,
                     error(
                       :invalid_form,
                       "select options require unique export values for interactive PDF choices; use distinct value attributes or forms: :static"
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
                        | boxes:
                            Enum.flat_map(page.boxes, fn box ->
                              if is_nil(Map.get(box, :form_owner)) do
                                [box]
                              else
                                case {Map.get(box, :html_targets, []),
                                      Map.get(box, :css_transform_scopes, [])} do
                                  {[], []} ->
                                    []

                                  _targets ->
                                    [
                                      box
                                      |> Map.take([:x, :y, :html_targets, :css_transform_scopes])
                                      |> Map.merge(%{
                                        type: HtmlValidator.reserve_layout_box(:flow_marker),
                                        width: 0.0,
                                        height: 0.0
                                      })
                                    ]
                                end
                              end
                            end)
                      }
                    end)

                  bytes =
                    Enum.reduce(fields, 0, fn field, total ->
                      Enum.reduce(field.appearance.boxes, total, fn box, total ->
                        total + byte_size(Map.get(box, :text, ""))
                      end)
                    end)

                  HtmlValidator.reserve_render_resource(:max_appearance_text_bytes, bytes, :forms)

                  {:ok, cleaned,
                   fields
                   |> Enum.reverse()
                   |> Enum.map(fn field ->
                     if Keyword.get(opts, :__retain_html_identity__, false),
                       do: field,
                       else: Map.drop(field, [:html_owner, :html_control_id, :html_name_explicit])
                   end)}

                failure ->
                  failure
              end
          end
        end

      failure ->
        failure
    end
  catch
    {:render_resource_limit, failure} -> failure
  end

  @doc "Combines canonical writer-normalized child fields with root fields, retaining separate document names and shared widget/appearance ceilings."
  @spec merge_composition([map()], [map()], keyword()) ::
          {:ok, [map()]} | {:error, {atom(), map()}}
  def merge_composition(pages, root_fields, opts) do
    records =
      pages
      |> Enum.with_index(1)
      |> Enum.flat_map(fn {page, number} ->
        page.boxes
        |> Enum.flat_map(fn box ->
          case Map.get(box, :embedded_widgets) do
            %{fields: [_ | _]} = record ->
              [
                %{
                  record
                  | fields:
                      Enum.map(record.fields, fn field ->
                        field
                        |> Map.put(:page, number)
                        |> Map.put(
                          :html_slice_identity,
                          {record.document_id, record.source_token, field.html_control_id}
                        )
                      end)
                }
              ]

            _ ->
              []
          end
        end)
      end)

    context = NativeElixirPdfUtilities.HtmlToPdf.ResourceContext.current()
    root_document = if context == nil, do: nil, else: context.document_id
    {same_document, records} = Enum.split_with(records, &(&1.document_id == root_document))

    root_fields =
      normalize_required_group_flags(root_fields ++ Enum.flat_map(same_document, & &1.fields))
      |> normalize_generated_names()

    count = length(root_fields) + Enum.reduce(records, 0, &(length(&1.fields) + &2))

    identities =
      Enum.reduce_while(root_fields, {:ok, []}, fn field, {:ok, previous} ->
        case validate_field_identity(field, previous) do
          :ok -> {:cont, {:ok, [field | previous]}}
          failure -> {:halt, failure}
        end
      end)

    cond do
      identities |> elem(0) == :error ->
        identities

      (records != [] or same_document != []) and
          Keyword.get(opts, :forms, :interactive) == :static ->
        error(
          :unsupported_form,
          "interactive child fields require surfaces prepared with forms: :interactive; recompute static child artwork for forms: :static"
        )

      count > Limits.get(:max_pdf_form_fields) or count * 2 > Limits.get(:max_appearance_widgets) ->
        error(
          :resource_limit_exceeded,
          "composed HTML controls exceed the shared field or appearance count limit"
        )

      length(Enum.uniq_by(records ++ same_document, & &1.token)) !=
          length(records) + length(same_document) ->
        error(
          :unsupported_form,
          "a child control document cannot repeat across pages or page furniture"
        )

      true ->
        document_order = records |> Enum.map(& &1.document_id) |> Enum.uniq()
        documents = Enum.group_by(records, & &1.document_id)
        names = MapSet.new(Enum.map(root_fields, & &1.name))

        Enum.reduce_while(document_order, {:ok, [root_fields], names}, fn id,
                                                                          {:ok, chunks, names} ->
          own =
            documents[id]
            |> Enum.flat_map(& &1.fields)
            |> normalize_required_group_flags()
            |> normalize_generated_names()

          name_work = Enum.reduce(own, 0, &(byte_size(&1.name) + &2))

          prefix =
            Stream.iterate(1, &(&1 + 1))
            |> Enum.find_value(fn index ->
              prefix = "CHILD_#{index}_"

              HtmlValidator.reserve_render_resource(
                :max_html_tree_work,
                name_work + byte_size(prefix) * length(own),
                :forms
              )

              if Enum.all?(own, &(not MapSet.member?(names, prefix <> &1.name))), do: prefix
            end)

          cond do
            Enum.any?(
              own,
              &(byte_size(prefix) + byte_size(&1.name) > Limits.get(:max_pdf_form_text_bytes))
            ) ->
              {:halt,
               error(
                 :resource_limit_exceeded,
                 "child PDF field name exceeds max_pdf_form_text_bytes"
               )}

            own
            |> Enum.group_by(& &1.html_control_id)
            |> Enum.any?(fn {_id, fields} ->
              length(Enum.uniq_by(fields, & &1.html_slice_identity)) != 1
            end) ->
              {:halt,
               error(:unsupported_form, "a child control cannot repeat in composed artwork")}

            true ->
              identities =
                Enum.reduce_while(own, {:ok, []}, fn field, {:ok, previous} ->
                  case validate_field_identity(field, previous) do
                    :ok -> {:cont, {:ok, [field | previous]}}
                    failure -> {:halt, failure}
                  end
                end)

              if elem(identities, 0) == :error do
                {:halt, identities}
              else
                own = Enum.map(own, &Map.put(&1, :name, prefix <> &1.name))
                names = Enum.reduce(own, names, &MapSet.put(&2, &1.name))
                {:cont, {:ok, [own | chunks], names}}
              end
          end
        end)
        |> case do
          {:ok, chunks, _names} -> {:ok, chunks |> Enum.reverse() |> List.flatten()}
          failure -> failure
        end
    end
  catch
    {:render_resource_limit, failure} -> failure
  end

  # Canonical name and radio ownership rules apply before and after native
  # document composition. Fields retain the HTML identities until this point.
  defp validate_field_identity(field, previous) do
    members =
      Enum.filter(previous, fn member ->
        member.name == field.name and
          (Map.get(field, :html_slice_identity) == nil or
             Map.get(member, :html_slice_identity) != field.html_slice_identity)
      end)

    cond do
      members != [] and (field.type != :radio or Enum.any?(members, &(&1.type != :radio))) ->
        error(
          :invalid_form,
          "duplicate HTML name #{inspect(field.name)}; only radio groups may share a name"
        )

      field.type == :radio and Enum.any?(members, &(&1.html_owner != field.html_owner)) ->
        error(
          :unsupported_form,
          "radio name #{inspect(field.name)} belongs to different HTML form owners; " <>
            "use unique names or forms: :static instead of merging independent groups",
          source: field.name
        )

      field.type == :radio and Enum.any?(members, &(&1.flags != field.flags)) ->
        error(
          :unsupported_form,
          "radio group #{inspect(field.name)} mixes enabled and disabled controls; " <>
            "make all members enabled or all disabled, or use forms: :static",
          source: field.name
        )

      field.type == :radio and Enum.any?(members, &(&1.export == field.export)) ->
        error(:invalid_form, "radio groups require unique export values")

      true ->
        :ok
    end
  end

  defp available_field_name(base, names) do
    Stream.iterate(0, &(&1 + 1))
    |> Enum.find_value(fn suffix ->
      candidate = if suffix == 0, do: base, else: "#{base}_#{suffix}"
      if MapSet.member?(names, candidate), do: nil, else: candidate
    end)
  end

  defp normalize_generated_names(fields) do
    names =
      fields |> Enum.filter(&Map.get(&1, :html_name_explicit, true)) |> MapSet.new(& &1.name)

    fields
    |> Enum.map_reduce({names, %{}}, fn field, {names, slices} ->
      identity = Map.get(field, :html_slice_identity)

      if Map.get(field, :html_name_explicit, true) do
        {field, {names, slices}}
      else
        name = Map.get(slices, identity) || available_field_name(field.name, names)
        slices = if identity == nil, do: slices, else: Map.put(slices, identity, name)
        {Map.put(field, :name, name), {MapSet.put(names, name), slices}}
      end
    end)
    |> elem(0)
  end

  defp normalize_required_group_flags(fields) do
    required =
      fields
      |> Enum.filter(&(&1.type == :radio and Bitwise.band(&1.flags, 2) != 0))
      |> MapSet.new(&{&1.name, &1.html_owner})

    Enum.map(fields, fn field ->
      if field.type == :radio and MapSet.member?(required, {field.name, field.html_owner}),
        do: Map.update!(field, :flags, &Bitwise.bor(&1, 2)),
        else: field
    end)
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
                     (valid_initial_range?(state, attributes) and
                        valid_initial_range_association?(state, attributes)))

              temporal_valid? =
                attributes_valid? and
                  (tag != "input" or
                     not HtmlTemporalValidator.temporal_type?(
                       HtmlValidator.input_type(Map.get(attributes, "type", "text"))
                     ) or HtmlTemporalValidator.valid_initial?(state, attributes))

              color_valid? =
                attributes_valid? and
                  (tag != "input" or
                     HtmlValidator.input_type(Map.get(attributes, "type", "text")) != "color" or
                     case prepare_initial_color(attributes) do
                       {:ok, expected} -> Map.take(state, [:value, :color_input]) == expected
                     end)

              not color_valid? or not number_valid? or not range_valid? or not temporal_valid? or
                not NativeElixirPdfUtilities.Validators.HtmlConstraintValidator.valid_transported_state?(
                  state
                ) or
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
                not HtmlValidator.valid_form_element?(tag, attributes, children, :initial_dom) or
                (tag == "input" and
                   (HtmlValidator.button_input_type?(
                      HtmlValidator.input_type(Map.get(attributes, "type", "text"))
                    ) or
                      HtmlValidator.input_type(Map.get(attributes, "type", "text")) in [
                        "hidden",
                        "image",
                        "file"
                      ]))

            _ ->
              true
          end
        end)
      end)

    if invalid? do
      error(
        :invalid_form,
        "HTML control requires validated initial checked, disabled and owner state, an applicable UTF-8 initial text value, and selection state with UTF-8 option values and labels; supplied constraint flags require coherent boolean candidacy and validity",
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
        if Map.get(box, :role) in [:range_thumb, :range_tick] do
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
                 "Disabled Range artwork paints outside the PDF widget rectangle; use forms: :static to preserve overflowing initial artwork",
                 module: __MODULE__,
                 operation: :write_pdf,
                 source: "range"
               )}
            end
          else
            {:halt,
             error(:invalid_form, "Range appearance requires a validated native artwork frame",
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
