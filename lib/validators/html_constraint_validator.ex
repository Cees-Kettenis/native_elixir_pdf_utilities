defmodule NativeElixirPdfUtilities.Validators.HtmlConstraintValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  # HTML's authored email grammar has ASCII local/domain characters and a
  # fixed63-byte domain-label bound. User-edit IDNA conversion is a distinct
  # operation and does not sanitize initial authored values.
  @email_address ~r/\A[A-Za-z0-9.!#$%&'*+\/=?^_`{|}~-]+@[A-Za-z0-9](?:[A-Za-z0-9-]{0,61}[A-Za-z0-9])?(?:\.[A-Za-z0-9](?:[A-Za-z0-9-]{0,61}[A-Za-z0-9])?)*\z/

  @doc false
  @spec prepare_initial_states(map(), [map()]) :: map()
  def prepare_initial_states(states, controls) do
    HtmlValidator.reserve_render_resource(:max_html_tree_work, 4 * length(controls), :html)

    radio_groups =
      Enum.reduce(controls, %{}, fn item, groups ->
        name = Map.get(item.attributes, "name", "")

        if item.input_type == "radio" and name != "" do
          key = {item.scope, item.owner, name}
          state = Map.fetch!(states, item.ref)

          Map.update(
            groups,
            key,
            %{required: Map.has_key?(item.attributes, "required"), checked: state.checked},
            fn group ->
              %{
                required: group.required or Map.has_key?(item.attributes, "required"),
                checked: group.checked or state.checked
              }
            end
          )
        else
          groups
        end
      end)

    states =
      Enum.reduce(controls, states, fn item, result ->
        if item.tag in ~w(input textarea select button output fieldset) do
          state = Map.fetch!(result, item.ref)
          readonly = Map.get(state, :readonly, Map.has_key?(item.attributes, "readonly"))
          required = Map.has_key?(item.attributes, "required")
          type = item.input_type
          button_type = String.downcase(Map.get(item.attributes, "type", "submit"), :ascii)

          barred =
            state.disabled or item.in_datalist or
              item.tag in ~w(output fieldset) or
              (item.tag == "input" and type in ~w(hidden image reset button)) or
              (item.tag == "button" and button_type in ~w(reset button)) or
              (readonly and (item.tag == "textarea" or HtmlValidator.readonly_input_type?(type)))

          value_missing =
            cond do
              item.tag == "select" and required ->
                selection = state.selection
                selected = Enum.filter(selection.choices, & &1.selected)

                selected == [] or
                  (not selection.multiple and selection.display_size == 1 and
                     length(selected) == 1 and hd(selected).id == item.placeholder_option)

              item.tag == "input" and type == "radio" ->
                case Map.get(
                       radio_groups,
                       {item.scope, item.owner, Map.get(item.attributes, "name", "")}
                     ) do
                  %{required: true, checked: false} -> true
                  _ -> false
                end

              item.tag == "input" and type == "checkbox" ->
                required and not state.checked

              item.tag == "input" and type == "file" ->
                required and state.files == []

              required and (item.tag == "textarea" or HtmlValidator.readonly_input_type?(type)) ->
                not state.disabled and not readonly and Map.get(state, :value, "") == ""

              true ->
                false
            end

          numeric = Map.get(state, :temporal) || Map.get(state, :number) || %{}

          numeric_flags = %{
            range_underflow: Map.get(numeric, :range_underflow, false),
            range_overflow: Map.get(numeric, :range_overflow, false),
            step_mismatch:
              if(Map.has_key?(state, :number),
                do: number_step_mismatch(state.number),
                else: Map.get(numeric, :step_mismatch, false)
              )
          }

          # Initial authored values have never been user-edited. Length flags and
          # badInput therefore stay false, including invalid number sanitation.
          email_mismatch =
            if item.tag == "input" and type == "email" and state.value != "" do
              HtmlValidator.reserve_render_resource(
                :max_html_tree_work,
                byte_size(state.value),
                :html
              )

              values =
                if Map.has_key?(item.attributes, "multiple"),
                  do: String.split(state.value, ","),
                  else: [state.value]

              Enum.any?(values, &(not Regex.match?(@email_address, &1)))
            else
              false
            end

          type_mismatch =
            if item.tag == "input" and type == "url" and state.value != "" do
              case NativeElixirPdfUtilities.Validators.HtmlUrlValidator.valid_initial_input_url?(
                     state.value
                   ) do
                {:ok, valid} ->
                  not valid

                {:error, {reason, diagnostic}} ->
                  throw(
                    {:render_resource_limit,
                     {:error,
                      {reason,
                       NativeElixirPdfUtilities.Diagnostics.with_context(diagnostic,
                         source: "input/url"
                       )}}}
                  )
              end
            else
              email_mismatch
            end

          pattern_mismatch =
            if item.tag == "input" and HtmlValidator.text_input_type?(type) and
                 Map.has_key?(item.attributes, "pattern") and state.value != "" do
              values =
                if type == "email" and Map.has_key?(item.attributes, "multiple"),
                  do: String.split(state.value, ","),
                  else: [state.value]

              case NativeElixirPdfUtilities.Validators.HtmlPatternValidator.initial_pattern(
                     Map.fetch!(item.attributes, "pattern"),
                     values
                   ) do
                {:ok, %{mismatch: mismatch}} ->
                  mismatch

                {:error, {reason, diagnostic}} ->
                  throw(
                    {:render_resource_limit,
                     {:error,
                      {reason,
                       NativeElixirPdfUtilities.Diagnostics.with_context(diagnostic,
                         source: "input/pattern"
                       )}}}
                  )
              end
            else
              false
            end

          checks =
            Map.merge(numeric_flags, %{
              value_missing: value_missing,
              type_mismatch: type_mismatch,
              pattern_mismatch: pattern_mismatch,
              too_long: false,
              too_short: false,
              bad_input: false,
              custom_error: false
            })

          constraint = %{
            will_validate: not barred,
            checks: checks,
            valid: not Enum.any?(checks, fn {_check, value} -> value end)
          }

          Map.put(result, item.ref, Map.put(state, :constraint_validation, constraint))
        else
          result
        end
      end)

    # Forms aggregate their owned candidates, including external controls.
    # Fieldsets aggregate same-tree descendants even when another form owns
    # them. A barred control's own invalid flags never invalidate either.
    invalid_aggregates =
      Enum.reduce(controls, MapSet.new(), fn item, invalid ->
        case Map.get(Map.fetch!(states, item.ref), :constraint_validation) do
          %{will_validate: true, valid: false} ->
            ancestors = Map.get(item, :fieldsets, [])
            HtmlValidator.reserve_render_resource(:max_html_tree_work, length(ancestors), :html)
            invalid = if item.owner == nil, do: invalid, else: MapSet.put(invalid, item.owner)
            Enum.reduce(ancestors, invalid, &MapSet.put(&2, &1))

          _ ->
            invalid
        end
      end)

    Enum.reduce(controls, states, fn item, states ->
      if item.tag in ~w(form fieldset) do
        Map.update!(
          states,
          item.ref,
          &Map.put(&1, :aggregate_valid, not MapSet.member?(invalid_aggregates, item.ref))
        )
      else
        states
      end
    end)
  end

  @doc false
  @spec matches_initial_state?(:valid | :invalid, map()) :: boolean()
  def matches_initial_state?(pseudo, node) do
    state = Map.get(node, :_initial_form_state, Map.get(node, :initial_state, %{}))

    valid =
      if Map.get(node, :namespace, :html) == :html do
        case Map.get(node, :tag) do
          tag when tag in ~w(form fieldset) ->
            Map.get(state, :aggregate_valid)

          tag when tag in ~w(input select textarea button) ->
            case Map.get(state, :constraint_validation) do
              %{will_validate: true, valid: valid} when is_boolean(valid) -> valid
              _ -> nil
            end

          _ ->
            nil
        end
      end

    is_boolean(valid) and
      case pseudo do
        :valid -> valid
        :invalid -> not valid
      end
  end

  @doc false
  @spec valid_transported_state?(map()) :: boolean()
  def valid_transported_state?(state) do
    aggregate_coherent? =
      not Map.has_key?(state, :aggregate_valid) or is_boolean(state.aggregate_valid)

    aggregate_coherent? and
      case Map.fetch(state, :constraint_validation) do
        # Existing caller-supplied control boxes predate this derived metadata.
        :error ->
          true

        {:ok, %{will_validate: candidate, checks: checks, valid: valid}}
        when is_boolean(candidate) and is_boolean(valid) and is_map(checks) ->
          keys =
            ~w(value_missing type_mismatch pattern_mismatch too_long too_short range_underflow range_overflow step_mismatch bad_input custom_error)a

          map_size(checks) == length(keys) and
            Enum.all?(keys, &is_boolean(Map.get(checks, &1))) and
            valid == not Enum.any?(checks, fn {_check, flag} -> flag end)

        _ ->
          false
      end
  end

  # Chromium's numeric StepRange tolerates single-precision rounding error and
  # stops comparing beyond IEEE754's53-bit mantissa. These are fixed format
  # rules, not tunable resource ceilings. Decimal alignment avoids overflow
  # when two individually finite authored values have an infinite difference.
  defp number_step_mismatch(number) do
    if number.value == nil or number.step == :any do
      false
    else
      parsed =
        Enum.map([number.value, number.step_base, number.step], fn value ->
          {:ok, decimal} =
            HtmlValidator.parse_html_float(
              :erlang.float_to_binary(value * 1.0, [:short]),
              :decimal
            )

          decimal
        end)

      exponent = parsed |> Enum.map(& &1.exponent) |> Enum.min()

      aligned =
        Enum.map(parsed, fn decimal ->
          power = decimal.exponent - exponent

          HtmlValidator.reserve_render_resource(
            :max_html_tree_work,
            power + decimal.coefficient_digits,
            :html
          )

          if(decimal.negative, do: -1, else: 1) * decimal.coefficient * Integer.pow(10, power)
        end)

      [value, base, step] = aligned
      difference = abs(value - base)

      if difference > step * Integer.pow(2, 53) do
        false
      else
        remainder = rem(difference, step)
        min(remainder, step - remainder) * Integer.pow(2, 24) > step
      end
    end
  end
end
