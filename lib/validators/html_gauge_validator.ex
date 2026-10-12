defmodule NativeElixirPdfUtilities.Validators.HtmlGaugeValidator do
  @moduledoc """
  Prepares the initial static progress and meter state from the pinned HTML Standard.

  Consumers use `_initial_gauge_state` without reparsing authored attributes. Missing
  progress value means indeterminate; malformed present values mean determinate zero.
  Meter points are resolved in specification order and classify the actual value as
  optimum, suboptimal or even less good. Gauges are labelable, not form-associated,
  and never create PDF fields. Their authored fallback tree remains in the DOM.
  """

  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc false
  @spec prepare_style(map()) :: map()
  def prepare_style(style) do
    author_border =
      Enum.any?(Map.get(style, :border_styles, %{}), fn {_side, border} ->
        border not in [:none, :hidden]
      end)

    devolved =
      style._gauge.kind == :progress and
        (Map.has_key?(style, :background_color) or Map.has_key?(style, :background_image_source) or
           author_border)

    Map.put(style, :_gauge_native, style.appearance == :auto and not devolved)
  end

  @doc false
  @spec valid_prepared?(term()) :: boolean()
  def valid_prepared?(state) do
    case state do
      %{kind: kind} when kind in [:progress, :meter] ->
        names =
          if kind == :progress,
            do: [:min, :max, :value, :position],
            else: [:min, :max, :value, :position, :low, :high, :optimum]

        if Enum.all?(names, &is_float(Map.get(state, &1))) and
             (kind == :meter or is_boolean(Map.get(state, :determinate))) do
          attributes =
            Map.new(names -- [:position], &{Atom.to_string(&1), to_string(Map.fetch!(state, &1))})

          attributes =
            if kind == :progress and not state.determinate,
              do: Map.delete(attributes, "value"),
              else: attributes

          state == prepare(Atom.to_string(kind), attributes)
        else
          false
        end

      _ ->
        false
    end
  end

  @doc false
  @spec prepare(String.t(), map()) :: map()
  def prepare(tag, attributes) do
    numbers =
      Map.new(~w(min max value low high optimum), fn name ->
        {name, HtmlValidator.parse_html_float(Map.get(attributes, name))}
      end)

    case tag do
      "progress" ->
        maximum =
          case numbers["max"] do
            {:ok, number} when number > 0 -> number
            _ -> 1.0
          end

        value =
          case numbers["value"] do
            {:ok, number} -> min(max(number, 0.0), maximum)
            _ -> 0.0
          end

        determinate = Map.has_key?(attributes, "value")

        %{
          kind: :progress,
          determinate: determinate,
          min: 0.0,
          max: maximum,
          value: value,
          position: if(determinate, do: value / maximum, else: -1.0)
        }

      "meter" ->
        minimum = resolved_number(numbers["min"], 0.0)
        maximum = max(resolved_number(numbers["max"], 1.0), minimum)
        value = min(max(resolved_number(numbers["value"], 0.0), minimum), maximum)
        low = min(max(resolved_number(numbers["low"], minimum), minimum), maximum)
        high = min(max(resolved_number(numbers["high"], maximum), low), maximum)
        # Halving before addition avoids overflow for finite extreme endpoints.
        optimum =
          min(
            max(resolved_number(numbers["optimum"], minimum / 2 + maximum / 2), minimum),
            maximum
          )

        region =
          cond do
            optimum < low and value <= low -> :optimum
            optimum < low and value <= high -> :suboptimal
            optimum < low -> :even_less_good
            optimum > high and value >= high -> :optimum
            optimum > high and value >= low -> :suboptimal
            optimum > high -> :even_less_good
            value >= low and value <= high -> :optimum
            true -> :suboptimal
          end

        position =
          if maximum == minimum do
            0.0
          else
            # Scale first to keep the subtraction finite for the full IEEE range.
            scale = max(abs(minimum), abs(maximum))
            (value / scale - minimum / scale) / (maximum / scale - minimum / scale)
          end

        %{
          kind: :meter,
          min: minimum,
          max: maximum,
          value: value,
          low: low,
          high: high,
          optimum: optimum,
          region: region,
          position: position
        }
    end
  end

  # Each point has a different specification default but shares parse-error recovery.
  defp resolved_number(result, default) do
    case result do
      {:ok, number} -> number
      :error -> default
    end
  end
end
