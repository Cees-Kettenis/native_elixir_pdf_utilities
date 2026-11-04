defmodule NativeElixirPdfUtilities.Validators.MathFractionValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, MathLayoutValidator}

  # Core 3.3.2 resolves font values and authored thickness before measurement.
  @doc "Resolves selected-font fraction shifts, gaps and authored rule thickness."
  @spec prepare(term(), term()) :: {:ok, map()} | {:error, Diagnostics.detailed_error()}
  def prepare(row, attributes) do
    case {row, attributes} do
      {%{kind: :row, children: [_, _], style: %{font_face: font, font_size: size} = style},
       attributes}
      when is_map(font) and is_number(size) and size >= 0 and is_map(attributes) ->
        math_style = Map.get(style, :math_style, :normal)

        if math_style in [:normal, :compact] and is_number(style.font_size) and
             style.font_size <= Limits.get(:max_css_numeric_magnitude) do
          with {:ok, metrics} <- Font.math_metrics(style.font_face) do
            names =
              if math_style == :compact do
                [
                  :axis_height,
                  :fraction_rule_thickness,
                  :fraction_numerator_shift_up,
                  :fraction_numerator_gap_min,
                  :fraction_denominator_shift_down,
                  :fraction_denominator_gap_min,
                  :stack_top_shift_up,
                  :stack_bottom_shift_down,
                  :stack_gap_min
                ]
              else
                [
                  :axis_height,
                  :fraction_rule_thickness,
                  :fraction_numerator_display_style_shift_up,
                  :fraction_num_display_style_gap_min,
                  :fraction_denominator_display_style_shift_down,
                  :fraction_denom_display_style_gap_min,
                  :stack_top_display_style_shift_up,
                  :stack_bottom_display_style_shift_down,
                  :stack_display_style_gap_min
                ]
              end

            values = MathLayoutValidator.resolve_constants(metrics, names, style.font_size)

            with {:ok,
                  [
                    axis,
                    default,
                    numerator_shift,
                    numerator_gap,
                    denominator_shift,
                    denominator_gap,
                    stack_top,
                    stack_bottom,
                    stack_gap
                  ]} <- values do
              default = max(default, 0.0)

              thickness =
                case Map.get(attributes, "linethickness") do
                  nil ->
                    {:ok, default}

                  value ->
                    case HtmlValidator.parse_css_number(value) do
                      {number, "%"} ->
                        {:ok, number * default / 100}

                      _ ->
                        case MathLayoutValidator.math_length(value, style) do
                          {:ok, {:rem, ratio}} ->
                            {:ok, ratio * Map.get(style, :_root_font_size, style.font_size)}

                          {:ok, number} when is_number(number) ->
                            {:ok, number}

                          {:error, {_reason, _diagnostic}} = error ->
                            error

                          _ ->
                            {:ok, default}
                        end
                    end
                end

              with {:ok, thickness} <- thickness do
                if thickness > 0,
                  do: HtmlValidator.reserve_render_resource(:max_layout_boxes, 1, :layout)

                {:ok,
                 row
                 |> Map.put(:kind, :fraction)
                 |> Map.put(:axis, axis)
                 |> Map.put(:rule_thickness, max(thickness, 0.0))
                 |> Map.put(:numerator_shift, numerator_shift)
                 |> Map.put(:numerator_gap, numerator_gap)
                 |> Map.put(:denominator_shift, denominator_shift)
                 |> Map.put(:denominator_gap, denominator_gap)
                 |> Map.put(:stack_top, stack_top)
                 |> Map.put(:stack_bottom, stack_bottom)
                 |> Map.put(:stack_gap, stack_gap)}
              end
            end
          end
        else
          failure("fractions require a bounded font size and normal or compact math-style")
        end

      _ ->
        failure(
          "fractions require a prepared numerator and denominator and a styled selected font"
        )
    end
  end

  defp failure(message) do
    Diagnostics.error(:layout, :invalid_document, message,
      operation: :measure_math,
      module: __MODULE__,
      source: "mfrac"
    )
  end
end
