defmodule NativeElixirPdfUtilities.Validators.FontMetricValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.{FontValidator, HtmlValidator}

  @fields %{
    "size-adjust" => :size_adjust,
    "ascent-override" => :ascent_override,
    "descent-override" => :descent_override,
    "line-gap-override" => :line_gap_override
  }

  @doc "Returns the font-face metric descriptors and their canonical option fields."
  @spec fields() :: map()
  def fields, do: @fields

  @doc "Prepares font-face metric percentages. Numeric option values use the same percent units as CSS."
  @spec prepare_descriptor(String.t(), term()) :: {:ok, map()} | :error
  def prepare_descriptor(property, value) do
    with {:ok, key} <- Map.fetch(@fields, property) do
      normalized =
        case value do
          :normal ->
            :normal

          n when is_number(n) ->
            {:percent, n}

          text when is_binary(text) ->
            case String.downcase(String.trim(text), :ascii) do
              "normal" ->
                :normal

              number ->
                case HtmlValidator.parse_css_number(number) do
                  {n, "%"} -> {:percent, n}
                  _ -> :invalid
                end
            end

          _ ->
            :invalid
        end

      case normalized do
        :normal ->
          if property == "size-adjust", do: :error, else: {:ok, %{key => :normal}}

        {:percent, n} when n >= 0 ->
          {:ok, %{key => if(n == trunc(n), do: trunc(n), else: n)}}

        _ ->
          :error
      end
    else
      _ -> :error
    end
  end

  @doc "Normalizes metric options through the same descriptor rules as CSS font faces."
  @spec prepare_descriptors(map()) :: {:ok, map()} | :error | {:error, {atom(), map()}}
  def prepare_descriptors(config) do
    FontValidator.with_budget(fn ->
      Enum.reduce_while(@fields, {:ok, %{}}, fn {property, key}, {:ok, prepared} ->
        supplied =
          case Map.fetch(config, key) do
            :error -> Map.fetch(config, String.replace(property, "-", "_"))
            found -> found
          end

        case supplied do
          {:ok, value} ->
            case prepare_descriptor(property, value) do
              {:ok, field} ->
                Enum.each(field, fn {_key, value} ->
                  if is_number(value), do: FontValidator.check(:max_font_numeric_magnitude, value)
                end)

                {:cont, {:ok, Map.merge(prepared, field)}}

              :error ->
                {:halt, :error}
            end

          :error ->
            {:cont, {:ok, prepared}}
        end
      end)
    end)
  end

  @doc "Resolves a from-font aspect against the first selected face, preserving it for fallback faces."
  @spec prepare_reference(map(), map()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_reference(font, style) do
    controls = Map.get(style, :font_controls, %{})

    case Map.get(controls, :"size-adjust") do
      {metric, :from_font} ->
        with {:ok, aspect} <- aspect(font, metric) do
          value = if aspect == nil, do: :none, else: {metric, aspect}
          {:ok, Map.put(style, :font_controls, Map.put(controls, :"size-adjust", value))}
        end

      _ ->
        {:ok, style}
    end
  end

  @doc "Applies face metrics and the used-size adjustment without changing CSS computed font size."
  @spec prepare(map(), map()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(font, style) do
    descriptors = Map.get(font, :metric_descriptors, %{})
    base = Map.get(descriptors, :size_adjust, 100) / 100
    adjustment = Map.get(Map.get(style, :font_controls, %{}), :"size-adjust", :none)

    result =
      case adjustment do
        {metric, target} when is_number(target) ->
          with {:ok, aspect} <- aspect(font, metric) do
            {:ok, if(aspect == nil or aspect == 0, do: base, else: target / aspect)}
          end

        _ ->
          {:ok, base}
      end

    with {:ok, scale} <- result do
      units = font.units_per_em

      metrics =
        Enum.reduce(
          [
            {:ascent_override, :ascent, 1},
            {:descent_override, :descent, -1},
            {:line_gap_override, :line_gap, 1}
          ],
          Map.take(font, [:ascent, :descent, :line_gap, :units_per_em]),
          fn {descriptor, field, sign}, metrics ->
            case Map.get(descriptors, descriptor, :normal) do
              :normal -> metrics
              percent -> Map.put(metrics, field, sign * units * percent / 100)
            end
          end
        )

      if scale == 1 and metrics == Map.take(font, [:ascent, :descent, :line_gap, :units_per_em]) do
        {:ok, font}
      else
        fingerprint =
          :crypto.hash(:sha256, :erlang.term_to_binary({scale, metrics}))
          |> binary_part(0, 6)
          |> Base.encode16(case: :lower)

        {:ok,
         font
         |> Map.put(:metric_scale, scale)
         |> Map.put(:layout_metrics, metrics)
         |> Map.update!(:id, &(&1 <> "-metrics-" <> fingerprint))}
      end
    end
  end

  @doc "Returns the physical aspect used by CSS size adjustment, or nil when unavailable."
  @spec aspect(map(), String.t()) :: {:ok, number() | nil} | {:error, {atom(), map()}}
  def aspect(font, metric) do
    {codepoint, dimension, fallback} =
      case metric do
        "ex-height" -> {?x, :height, nil}
        "cap-height" -> {?H, :height, nil}
        "ch-width" -> {?0, :width, 0.5}
        "ic-width" -> {?水, :width, 1}
        "ic-height" -> {?水, :height, 1}
      end

    preferred =
      if metric in ["ex-height", "cap-height"] do
        case FontValidator.prepare_text_metrics(font) do
          {:ok, metrics} ->
            Map.get(metrics, if(metric == "ex-height", do: :x_height, else: :cap_height))

          _ ->
            nil
        end
      end

    glyph = Map.get(font.cmap, codepoint, 0)

    cond do
      preferred != nil ->
        {:ok, preferred / font.units_per_em}

      glyph == 0 ->
        {:ok, fallback}

      true ->
        case dimension do
          :width ->
            widths = NativeElixirPdfUtilities.HtmlToPdf.Font.glyph_widths(font, glyph)
            {:ok, elem(widths, glyph) / font.units_per_em}

          :height ->
            with {:ok, bounds} <- FontValidator.prepare_glyph_bounds(font) do
              {_left, bottom, _right, top} = elem(bounds, glyph)
              value = if metric == "ic-height", do: top - bottom, else: top
              {:ok, if(value > 0, do: value / font.units_per_em, else: fallback)}
            end
        end
    end
  end

  @doc "Computes bounded used text size after admitted font metric adjustment."
  @spec used_size(map(), number()) :: number()
  def used_size(font, size) do
    size = layout_size(font, size)

    case Map.get(font, :glyph_scale) do
      nil -> size
      scale -> bounded_size(scale, size)
    end
  end

  @doc "Returns the used line-strut size independently of synthetic variant glyph scaling."
  @spec layout_size(map(), number()) :: number()
  def layout_size(font, size) do
    case Map.get(font, :metric_scale) do
      nil ->
        size

      0 ->
        0

      scale ->
        bounded_size(scale, size)
    end
  end

  @doc "Returns the baseline translation for a selected synthetic script face."
  @spec baseline_offset(map(), number()) :: number()
  def baseline_offset(font, size) do
    case Map.get(font, :baseline_shift) do
      nil -> 0
      0 -> 0
      shift -> bounded_size(shift, layout_size(font, size))
    end
  end

  defp bounded_size(scale, size) do
    limit = NativeElixirPdfUtilities.Limits.get(:max_pdf_numeric_magnitude)
    magnitude = abs(scale)

    value =
      if magnitude > 1 and abs(size) > limit / magnitude,
        do: limit + 1,
        else: abs(size) * magnitude

    HtmlValidator.check_render_resource(:max_pdf_numeric_magnitude, value, :writer)
    size * scale
  end

  @doc "Validates optional used-size and line metric metadata at the writer boundary."
  @spec validate_paint(map()) :: :ok | {:error, {atom(), map()}}
  def validate_paint(font) do
    scale = Map.get(font, :metric_scale, 1)
    glyph_scale = Map.get(font, :glyph_scale, 1)
    shift = Map.get(font, :baseline_shift, 0)
    metrics = Map.get(font, :layout_metrics)

    valid_metrics =
      metrics == nil or
        (is_map(metrics) and
           Enum.all?(
             [:ascent, :descent, :line_gap, :units_per_em],
             &is_number(Map.get(metrics, &1))
           ) and metrics.units_per_em > 0 and metrics.ascent >= 0 and metrics.descent <= 0)

    if is_number(scale) and scale >= 0 and
         scale <= NativeElixirPdfUtilities.Limits.get(:max_pdf_numeric_magnitude) and
         valid_metrics and is_number(glyph_scale) and glyph_scale >= 0 and
         glyph_scale <= NativeElixirPdfUtilities.Limits.get(:max_pdf_numeric_magnitude) and
         is_number(shift) and
         abs(shift) <= NativeElixirPdfUtilities.Limits.get(:max_pdf_numeric_magnitude) do
      :ok
    else
      NativeElixirPdfUtilities.Diagnostics.error(
        :font,
        :invalid_font,
        "font metric scale must be finite and nonnegative, with valid line metrics",
        module: __MODULE__,
        operation: :write_pdf,
        source: Map.get(font, :family, "font")
      )
    end
  end
end
