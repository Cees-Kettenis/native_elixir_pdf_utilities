defmodule NativeElixirPdfUtilities.Validators.SourceSizeValidator do
  @moduledoc "Resolves font-relative source-size units from the initial document font."

  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Validators.{FontValidator, HtmlValidator}

  @metric_unit ~r/(?:[0-9.](?:ex|rex|ch|rch|cap|rcap|ic|ric|lh|rlh)\b)/i

  @doc "Adds actual initial-font metrics only when source-size or media values require them."
  @spec prepare_context(map(), [String.t()], keyword()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def prepare_context(context, values, opts) do
    if not Map.has_key?(context, :source_metrics) and
         Enum.any?(values, &Regex.match?(@metric_unit, &1)) do
      opts = Keyword.merge(Map.get(context, :source_font_options, []), opts)

      font_result =
        case Keyword.get(opts, :__source_font__) do
          nil ->
            with {:ok, registry} <- Font.load_registry(opts),
                 {:ok, _, font} <-
                   Font.resolve(
                     Keyword.get(opts, :default_font, "DejaVu Sans"),
                     400,
                     :normal,
                     registry
                   ),
                 do: {:ok, font}

          font ->
            {:ok, font}
        end

      with {:ok, font} <- font_result,
           {:ok, bounds} <- FontValidator.prepare_glyph_bounds(font) do
        # CSS source-size/media font units use UA initial values, never authored image font-size.
        initial_pixels = 16.0
        unit_scale = initial_pixels / font.units_per_em

        x_height =
          case FontValidator.prepare_x_height(font) do
            {:ok, ratio} -> initial_pixels * ratio
            {:error, {:unsupported_glyph, _}} -> initial_pixels / 2
            {:error, _} = error -> throw({:render_resource_limit, error})
          end

        cap_height =
          case Map.get(font.cmap, ?H) do
            glyph when is_integer(glyph) and glyph > 0 and glyph < tuple_size(bounds) ->
              elem(elem(bounds, glyph), 3) * unit_scale

            _ ->
              font.ascent * unit_scale
          end

        # CSS mandates a one-em fallback when the water ideograph is unavailable.
        ideographic =
          if Map.get(font.cmap, ?水, 0) == 0,
            do: initial_pixels,
            else: Font.text_width("水", font, initial_pixels)

        metrics = %{
          ex: x_height,
          ch:
            if(Map.get(font.cmap, ?0, 0) == 0,
              do: initial_pixels / 2,
              else: Font.text_width("0", font, initial_pixels)
            ),
          cap: cap_height,
          ic: ideographic,
          lh: HtmlValidator.normal_line_height(%{font_face: font, font_size: 12.0}) / 0.75
        }

        Enum.each(metrics, fn {_unit, value} ->
          HtmlValidator.check_render_resource(:max_css_numeric_magnitude, value, :style)
        end)

        {:ok, Map.put(context, :source_metrics, metrics)}
      end
    else
      {:ok, context}
    end
  end
end
