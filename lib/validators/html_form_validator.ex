defmodule NativeElixirPdfUtilities.Validators.HtmlFormValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

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
          Enum.reduce(controls, {%{}, MapSet.new(), MapSet.new()}, fn item,
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
                selection = initial_selection(item.options)

                selected_options =
                  if is_nil(selection.id),
                    do: selected_options,
                    else: MapSet.put(selected_options, selection.id)

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

  # The single-selection reset algorithm retains the last authored selection.
  # Without one, a normal dropdown selects its first enabled option.
  defp initial_selection(options) do
    selected =
      Enum.reduce(options, nil, fn option, selected ->
        if Map.has_key?(option.attributes, "selected"), do: option, else: selected
      end) || Enum.find(options, &(not &1._initial_option_disabled))

    choices =
      Enum.map(options, fn option ->
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
          value: Map.get(option.attributes, "value", text),
          label: label,
          selected: not is_nil(selected) and option._selector_id == selected._selector_id
        }
      end)

    value =
      Enum.find_value(choices, fn choice -> if choice.selected, do: {:selected, choice.value} end)

    selected_label =
      Enum.find_value(choices, "", fn choice -> if choice.selected, do: choice.label end)

    %{
      id: if(is_nil(selected), do: nil, else: selected._selector_id),
      children: [%{type: :text, text: selected_label}],
      choices: choices,
      disabled_ids:
        for(option <- options, option._initial_option_disabled, do: option._selector_id),
      value: if(is_nil(value), do: nil, else: elem(value, 1))
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
              |> Map.put(:_initial_selected, MapSet.member?(selected_options, node._selector_id))
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
                      if HtmlValidator.text_input_type?(type), do: "text", else: type

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
                      item.control.initial_state.selection.value
                  end

                box = item.box
                page = Enum.at(pages, item.page - 1)

                appearance_boxes =
                  page.boxes
                  |> Enum.filter(&(Map.get(&1, :form_owner) == item.control.id))
                  |> Enum.map(fn box ->
                    box
                    |> Map.put(:x, box.x - item.box.x)
                    |> Map.put(:y, box.y - item.box.y)
                    |> Map.delete(:form_owner)
                    |> then(fn box ->
                      case Map.get(box, :clip) do
                        nil ->
                          box

                        clip ->
                          Map.put(box, :clip, %{
                            clip
                            | x: clip.x - item.box.x,
                              y: clip.y - item.box.y
                          })
                      end
                    end)
                  end)

                locked? =
                  item.control.initial_state.disabled or Map.has_key?(attributes, "readonly")

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
                  export: Map.get(attributes, "value", "on"),
                  page: item.page,
                  rect: [box.x, box.y, box.x + box.width, box.y + box.height],
                  flags:
                    if(locked?, do: 1, else: 0) +
                      if(required? and not locked?, do: 2, else: 0) +
                      if(item.control.tag == "textarea", do: 4096, else: 0) +
                      if(
                        item.control.tag == "input" and
                          HtmlValidator.input_type(Map.get(attributes, "type", "text")) ==
                            "password",
                        do: 8192,
                        else: 0
                      ),
                  appearance: %{size: {box.width, box.height}, boxes: appearance_boxes}
                }

                cond do
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
                    {:halt, error(:invalid_form, "select options require unique export values")}

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
                if tag == "select" do
                  case Map.get(state, :selection) do
                    %{choices: choices, value: value} when is_list(choices) ->
                      (is_nil(value) or (is_binary(value) and String.valid?(value))) and
                        Enum.all?(choices, fn
                          %{value: value, label: label, selected: selected} ->
                            is_binary(value) and String.valid?(value) and is_binary(label) and
                              String.valid?(label) and is_boolean(selected)

                          _ ->
                            false
                        end)

                    _ ->
                      false
                  end
                else
                  true
                end

              not is_boolean(checked) or not is_boolean(disabled) or
                not (is_nil(owner) or is_reference(owner)) or not selection_valid? or
                not Enum.all?(attributes, fn {key, value} ->
                  is_binary(key) and String.valid?(key) and is_binary(value) and
                    String.valid?(value)
                end) or
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

  defp error(reason, message, opts \\ []), do: Diagnostics.error(:forms, reason, message, opts)
end
