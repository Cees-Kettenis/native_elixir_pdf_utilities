defmodule NativeElixirPdfUtilities.Validators.MathRadicalValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, MathLayoutValidator}

  # Core 3.3.3 resolves the selected face's values before geometry consumes them.
  @doc "Resolves selected-font radical clearance, rule and degree constants."
  @spec prepare(term(), term(), term()) ::
          {:ok, map()} | {:error, Diagnostics.detailed_error()}
  def prepare(base, index, style) do
    case {base, index, style} do
      {base, index, %{font_face: font, font_size: size} = style}
      when is_map(base) and (is_nil(index) or is_map(index)) and is_map(font) and
             is_number(size) and size >= 0 ->
        mode = Map.get(style, :math_style, :normal)

        if mode in [:normal, :compact] and
             style.font_size <= Limits.get(:max_css_numeric_magnitude) do
          with {:ok, metrics} <- Font.math_metrics(style.font_face) do
            gap =
              if mode == :compact,
                do: :radical_vertical_gap,
                else: :radical_display_style_vertical_gap

            names = [
              gap,
              :radical_rule_thickness,
              :radical_extra_ascender,
              :radical_kern_before_degree,
              :radical_kern_after_degree
            ]

            values = MathLayoutValidator.resolve_constants(metrics, names, style.font_size)

            with {:ok, [gap, thickness, extra, before_degree, after_degree]} <- values do
              if thickness > 0,
                do: HtmlValidator.reserve_render_resource(:max_layout_boxes, 1, :layout)

              {:ok,
               %{
                 kind: :radical,
                 base: base,
                 index: index,
                 style: style,
                 gap: gap,
                 rule_thickness: max(thickness, 0.0),
                 extra_ascender: extra,
                 kern_before: max(before_degree, 0.0),
                 kern_after: after_degree,
                 degree_raise: metrics.constants.radical_degree_bottom_raise_percent / 100
               }}
            end
          end
        else
          failure("radicals require a bounded font size and normal or compact math-style")
        end

      _ ->
        failure("radicals require a prepared base, optional degree and a styled selected font")
    end
  end

  @doc false
  @spec prepare_stretch(term(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_stretch(prepared, measured) do
    case {prepared, measured} do
      {%{
         kind: :radical,
         style: %{font_face: font, font_size: size},
         gap: gap,
         rule_thickness: thickness
       }, %{ink_ascent: ascent, ink_descent: descent}}
      when is_number(ascent) and is_number(descent) and is_number(gap) and is_number(thickness) ->
        target = max(thickness + gap + ascent + descent, 0.0)

        if target > Limits.get(:max_css_numeric_magnitude) do
          failure("radical target exceeds shared numeric bounds")
        else
          with {:ok, base} <- MathLayoutValidator.prepare_glyph("√", font, size, []) do
            construction = Map.get(base.source.metrics.variants.vertical, base.glyph_id)

            if construction == nil do
              {:ok,
               %{
                 shape: %{kind: :glyph, glyph: base},
                 intrinsic_width: base.advance * size / base.units_per_em
               }}
            else
              with {:ok, shape} <-
                     MathLayoutValidator.prepare_stretch("√", font, size, :vertical, target, []) do
                width = MathLayoutValidator.preferred_vertical_width(base, construction)
                {:ok, %{shape: shape, intrinsic_width: width}}
              end
            end
          end
        end

      _ ->
        failure(
          "radical stretching requires validator-prepared radical state and measured base ink"
        )
    end
  end

  defp failure(message) do
    Diagnostics.error(:layout, :invalid_document, message,
      operation: :measure_math,
      module: __MODULE__,
      source: "msqrt/mroot"
    )
  end
end
