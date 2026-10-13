defmodule NativeElixirPdfUtilities.Validators.SourceSizeValidator do
  @moduledoc "Resolves initial source-size and selected element font metrics through one calculation."

  alias NativeElixirPdfUtilities.Diagnostics
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
           {:ok, metrics} <-
             prepare_font_metrics(
               font,
               12.0,
               HtmlValidator.normal_line_height(%{font_face: font, font_size: 12.0})
             ) do
        {:ok, Map.put(context, :source_metrics, metrics)}
      end
    else
      {:ok, context}
    end
  end

  @doc "Measures CSS pixel font units from a selected font, size and used line height in points."
  @spec prepare_font_metrics(map(), number(), number()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def prepare_font_metrics(font, font_size, line_height) do
    with {:ok, font_size} <- HtmlValidator.normalize_geometry_number(font_size),
         true <- font_size >= 0,
         {:ok, line_height} <- HtmlValidator.normalize_geometry_number(line_height),
         true <- line_height >= 0,
         {:ok, bounds} <- FontValidator.prepare_glyph_bounds(font) do
      pixels = font_size / 0.75
      unit_scale = pixels / font.units_per_em

      x_height =
        case FontValidator.prepare_x_height(font) do
          {:ok, ratio} -> pixels * ratio
          {:error, {:unsupported_glyph, _}} -> pixels / 2
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
          do: pixels,
          else: Font.text_width("水", font, pixels)

      metrics = %{
        ex: x_height,
        ch:
          if(Map.get(font.cmap, ?0, 0) == 0,
            do: pixels / 2,
            else: Font.text_width("0", font, pixels)
          ),
        cap: cap_height,
        ic: ideographic,
        lh: line_height / 0.75
      }

      Enum.each(metrics, fn {_unit, value} ->
        HtmlValidator.check_render_resource(:max_css_numeric_magnitude, value, :style)
      end)

      {:ok, metrics}
    else
      {:error, {_reason, _diagnostic}} = error ->
        error

      _ ->
        Diagnostics.error(
          :style,
          :invalid_document,
          "font metric sizes and used line height must be finite nonnegative points",
          module: __MODULE__,
          source: "font-relative CSS units"
        )
    end
  end
end
