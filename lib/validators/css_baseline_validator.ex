defmodule NativeElixirPdfUtilities.Validators.CssBaselineValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, TextShaper}

  alias NativeElixirPdfUtilities.Validators.{
    CssLengthValidator,
    FontBaselineValidator,
    FontValidator,
    HtmlValidator
  }

  @metrics %{
    "baseline" => :baseline,
    "text-top" => :text_top,
    "text-bottom" => :text_bottom,
    "alphabetic" => :alphabetic,
    "ideographic" => :ideographic,
    "middle" => :middle,
    "central" => :central,
    "mathematical" => :mathematical,
    "hanging" => :hanging
  }
  @metric_values Map.values(@metrics)
  @dominant_values [:auto | Map.values(Map.delete(@metrics, "baseline"))]
  @properties ~w(vertical-align baseline-source alignment-baseline baseline-shift dominant-baseline)

  @doc "Lists CSS Inline 3 baseline longhands and their vertical-align shorthand."
  @spec properties() :: [String.t()]
  def properties, do: @properties

  @doc "Returns the CSS Inline 3 initial baseline values."
  @spec defaults() :: map()
  def defaults,
    do: %{
      baseline_source: :auto,
      alignment_baseline: :baseline,
      baseline_shift: 0.0,
      dominant_baseline: :auto
    }

  @doc "Captures existing HTML alignment hints before applying author longhands."
  @spec initialize(map()) :: map()
  def initialize(style) do
    if Map.has_key?(style, :alignment_baseline) do
      style
    else
      {alignment, shift} =
        case :maps.get(:vertical_align, style, :baseline) do
          :middle -> {:middle, 0.0}
          :text_top -> {:text_top, 0.0}
          :text_bottom -> {:text_bottom, 0.0}
          value when value in [:sub, :super, :top, :bottom] -> {:baseline, value}
          value when is_number(value) -> {:baseline, value}
          _ -> {:baseline, 0.0}
        end

      style
      |> Map.put(:alignment_baseline, alignment)
      |> Map.put(:baseline_shift, shift)
      |> Map.put_new(:baseline_source, :auto)
      |> Map.put_new(:dominant_baseline, :auto)
    end
  end

  @doc "Admits baseline controls using the shared bounded CSS length grammar."
  @spec prepare(map(), String.t(), String.t()) ::
          {:ok, map()} | :error | {:error, {atom(), map()}}
  def prepare(style, property, value) do
    tokens = HtmlValidator.split_css_tokens(String.downcase(String.trim(value), :ascii))

    case {property, tokens} do
      {"baseline-source", [value]} when value in ["auto", "first", "last"] ->
        {:ok, Map.put(initialize(style), :baseline_source, String.to_existing_atom(value))}

      {"dominant-baseline", [value]} ->
        metric = if value == "auto", do: :auto, else: Map.get(@metrics, value)

        if metric not in [nil, :baseline],
          do: {:ok, Map.put(style, :dominant_baseline, metric)},
          else: :error

      {"alignment-baseline", [value]} ->
        case Map.fetch(@metrics, value) do
          {:ok, metric} ->
            {:ok, synchronize(Map.put(initialize(style), :alignment_baseline, metric))}

          :error ->
            :error
        end

      {"baseline-shift", [value]} ->
        with {:ok, shift} <- shift(value),
             do: {:ok, synchronize(Map.put(initialize(style), :baseline_shift, shift))}

      {"vertical-align", [_ | _]} ->
        initial = %{baseline_source: :auto, alignment_baseline: :baseline, baseline_shift: 0.0}

        Enum.reduce_while(tokens, {:ok, initial, MapSet.new()}, fn token, {:ok, parsed, seen} ->
          part =
            cond do
              token in ["first", "last"] ->
                {:ok, :baseline_source, String.to_existing_atom(token)}

              Map.has_key?(@metrics, token) ->
                {:ok, :alignment_baseline, Map.fetch!(@metrics, token)}

              true ->
                with {:ok, value} <- shift(token), do: {:ok, :baseline_shift, value}
            end

          case part do
            {:ok, key, value} ->
              if MapSet.member?(seen, key),
                do: {:halt, :error},
                else: {:cont, {:ok, Map.put(parsed, key, value), MapSet.put(seen, key)}}

            error ->
              {:halt, error}
          end
        end)
        |> case do
          {:ok, parsed, _} -> {:ok, synchronize(Map.merge(style, parsed))}
          error -> error
        end

      _ ->
        :error
    end
  end

  defp shift(value) do
    case value do
      "sub" ->
        {:ok, :sub}

      "super" ->
        {:ok, :super}

      "top" ->
        {:ok, :top}

      "center" ->
        {:ok, :center}

      "bottom" ->
        {:ok, :bottom}

      _ ->
        case CssLengthValidator.prepare(value, :margin) do
          {:ok, :auto} -> :error
          result -> result
        end
    end
  end

  @doc "Projects winning baseline longhands onto the existing inline alignment representation."
  @spec synchronize(map()) :: map()
  def synchronize(style) do
    alignment = :maps.get(:alignment_baseline, style, :baseline)
    shift = :maps.get(:baseline_shift, style, 0.0)

    value =
      cond do
        shift in [:top, :center, :bottom] -> shift
        alignment == :baseline and shift in [:sub, :super] -> shift
        alignment == :baseline and is_number(shift) -> if(shift == 0, do: :baseline, else: shift)
        alignment == :middle and shift == 0 -> :middle
        true -> {:baseline_controls, alignment, shift}
      end

    Map.put(style, :vertical_align, value)
  end

  @doc "Resolves parent-relative shifts and font baseline positions before inline layout."
  @spec resolve(map(), map()) :: {:ok, map()} | :error | {:error, {atom(), map()}}
  @spec resolve(map(), map(), String.t() | (-> String.t())) ::
          {:ok, map()} | :error | {:error, {atom(), map()}}
  def resolve(style, parent, text \\ "") do
    FontValidator.with_budget(fn ->
      alignment = :maps.get(:vertical_align, style, :baseline)

      alignment =
        if (alignment in [:baseline, :sub, :super] or is_number(alignment)) and
             :maps.get(:dominant_baseline, parent, :auto) != :auto,
           do:
             {:baseline_controls, :baseline, if(alignment == :baseline, do: 0.0, else: alignment)},
           else: alignment

      case alignment do
        value when value in [:sub, :super] ->
          {:ok, Map.put(style, :vertical_align, script_shift(value, parent))}

        {:baseline_controls, metric, shift} ->
          metric = if metric == :baseline, do: baseline_metric(parent, metric), else: metric
          text = if is_function(text, 0), do: text.(), else: text

          with {:ok, itemization} <-
                 NativeElixirPdfUtilities.Validators.TextItemizationValidator.prepare(
                   text,
                   :maps.get(:_text_language, style, "und")
                 ),
               script =
                 Enum.find_value(itemization.runs, "Latn", fn run ->
                   if run.script not in ~w(Zyyy Zinh Zzzz), do: run.script
                 end),
               {:ok, offset} <- resolved_shift(shift, style, parent),
               {:ok, parent_anchor} <- anchor(parent, metric, script),
               {:ok, child_anchor} <-
                 (case metric do
                    :text_top ->
                      {:ok, HtmlValidator.text_baseline_depth(style)}

                    :text_bottom ->
                      {:ok, HtmlValidator.text_baseline_depth(style) - style.line_height}

                    _ ->
                      anchor(style, metric, script)
                  end) do
            value = parent_anchor - child_anchor + offset
            {:ok, Map.put(style, :vertical_align, value)}
          end

        _ ->
          {:ok, style}
      end
    end)
  end

  @doc "Recognizes the admitted inline alignment representation, including legacy UA values."
  @spec valid_alignment?(term()) :: boolean()
  def valid_alignment?(value) do
    case value do
      value
      when value in [
             :baseline,
             :sub,
             :super,
             :top,
             :middle,
             :center,
             :bottom,
             :text_top,
             :text_bottom
           ] ->
        true

      {:baseline_controls, metric, shift} ->
        validate_style(%{alignment_baseline: metric, baseline_shift: shift}, "vertical-align") ==
          :ok

      number when is_number(number) ->
        match?({:ok, _}, HtmlValidator.normalize_geometry_number(number))

      _ ->
        false
    end
  end

  defp resolved_shift(shift, style, parent) do
    case shift do
      value when value in [:sub, :super] ->
        {:ok, script_shift(value, parent)}

      _ ->
        CssLengthValidator.resolve(shift, style.line_height, style)
    end
  end

  defp script_shift(value, parent) do
    # Blink stores these parent-relative UA shifts in fixed 1/64 CSS pixels.
    pixels = trunc(parent.font_size / 0.75 * 64)

    if value == :sub,
      do: -(div(pixels, 5) + 64) / 64 * 0.75,
      else: (div(pixels, 3) + 64) / 64 * 0.75
  end

  @doc "Resolves baseline shift lengths after font selection and line-height computation."
  @spec compute_style(map()) :: {:ok, map()} | :error | {:error, {atom(), map()}}
  def compute_style(style) do
    case Map.fetch(style, :baseline_shift) do
      {:ok, shift} when shift in [:sub, :super, :top, :center, :bottom] ->
        {:ok, style}

      {:ok, shift} ->
        with {:ok, value} <-
               CssLengthValidator.resolve(
                 shift,
                 Map.get(style, :line_height, :maps.get(:font_size, style, nil)),
                 style
               ) do
          {:ok,
           if(value == shift,
             do: style,
             else: synchronize(Map.put(style, :baseline_shift, value))
           )}
        end

      :error ->
        {:ok, style}
    end
  end

  @doc "Admits the legacy vertical-align parser through the same shorthand grammar."
  @spec legacy(String.t(), number() | :source) ::
          {:ok, term()} | :error | {:error, {atom(), map()}}
  def legacy(value, font_size) do
    with {:ok, style} <- prepare(%{}, "vertical-align", value) do
      if font_size == :source do
        case style.vertical_align do
          {:baseline_controls, :baseline, _shift} ->
            HtmlValidator.prepare_css_length_source(value, :margin)

          _ ->
            {:ok, style.vertical_align}
        end
      else
        with {:ok, style} <-
               compute_style(
                 Map.merge(style, %{
                   font_size: font_size,
                   line_height: font_size,
                   _root_font_size: font_size
                 })
               ),
             do: {:ok, style.vertical_align}
      end
    end
  end

  defp baseline_metric(style, metric) do
    metric = if metric == :baseline, do: :maps.get(:dominant_baseline, style, :auto), else: metric

    metric =
      if metric == :auto,
        do:
          if(
            :maps.get(:writing_mode, style, :horizontal_tb) in [
              :horizontal_tb,
              :sideways_rl,
              :sideways_lr
            ] or
              :maps.get(:text_orientation, style, nil) == :sideways,
            do: :alphabetic,
            else: :central
          ),
        else: metric

    if metric == :middle and :maps.get(:text_orientation, style, nil) == :upright,
      do: :central,
      else: metric
  end

  # BASE positions are relative to the font's Roman baseline, which remains the
  # internal PDF text origin. CSS Inline 3 Appendix A supplies missing metrics.
  defp anchor(style, metric, script) do
    metric = baseline_metric(style, metric)

    cond do
      metric == :alphabetic ->
        {:ok, 0.0}

      metric in [:text_top, :text_bottom] ->
        font = style.font_face
        metrics = Font.line_metrics(font)

        value =
          if metric == :text_top,
            do: :maps.get(:ascent, metrics, 1000),
            else: :maps.get(:descent, metrics, 0)

        size = Font.line_size(font, style.font_size)
        {:ok, round(value / :maps.get(:units_per_em, metrics, 1000) * size / 0.75) * 0.75}

      true ->
        font = style.font_face
        vertical? = :maps.get(:writing_mode, style, :horizontal_tb) != :horizontal_tb
        scripts = TextShaper.script_tags(script) ++ ["DFLT"]
        ppem = if style.font_size > 0, do: style.font_size / 0.75

        with {:ok, selected} <-
               FontBaselineValidator.coordinates(
                 font,
                 if(vertical?, do: :vertical, else: :horizontal),
                 scripts,
                 ppem
               ) do
          values = if selected == nil, do: %{}, else: selected.values
          units = :maps.get(:units_per_em, font, 1000)
          size = Font.used_size(font, style.font_size)
          roman = Map.get(values, "romn", 0.0)
          metrics = Font.line_metrics(font)
          ascent = Map.get(metrics, :ascent, units)
          descent = :maps.get(:descent, metrics, 0)
          extent = max(ascent - descent, 1)

          under =
            Map.get(
              values,
              "ideo",
              Map.get(values, "idtp", descent / extent * units + units) - units
            )

          over = Map.get(values, "idtp", under + units)

          fallback =
            case metric do
              :ideographic -> under
              :central -> (under + over) / 2
              :hanging -> units * 0.6
              :middle -> units * 0.25
              :mathematical -> units * 0.5
            end

          tag =
            case metric do
              :ideographic -> "ideo"
              :hanging -> "hang"
              :mathematical -> "math"
              _ -> nil
            end

          position =
            cond do
              Map.has_key?(values, tag) ->
                {:ok, Map.fetch!(values, tag)}

              metric == :mathematical ->
                mathematical_baseline(font, ppem, units)

              metric == :middle and size > 0 ->
                case HtmlValidator.text_x_height(style) do
                  height when is_number(height) -> {:ok, height / 2 / size * units}
                  error -> error
                end

              true ->
                {:ok, fallback}
            end

          with {:ok, position} <- position,
               do: {:ok, (position - roman) / units * size}
        end
    end
  end

  # Prefer the font's mathematical axis. CSS Inline 3 Appendix A permits the
  # minus sign's ink center when MATH is absent; a half-em is the last fallback.
  defp mathematical_baseline(font, ppem, units) do
    case font do
      %{type: :embedded, data: data} ->
        with {:ok, _, tables} <-
               FontValidator.prepare_table_directory(data) do
          if Map.has_key?(tables, "MATH") do
            with {:ok, metrics} <- Font.math_metrics(font) do
              record = metrics.constants.axis_height

              delta =
                NativeElixirPdfUtilities.Validators.OpenTypeDataValidator.device_adjustment(
                  record.device,
                  ppem,
                  units
                )

              {:ok, record.value + delta}
            end
          else
            case Map.get(font.cmap, 0x2212, 0) do
              0 ->
                {:ok, units / 2}

              glyph ->
                with {:ok, bounds} <- Font.glyph_bounds(font) do
                  {_left, bottom, _right, top} = elem(bounds, glyph)
                  {:ok, (bottom + top) / 2}
                end
            end
          end
        end

      _ ->
        {:ok, units / 2}
    end
  end

  @doc "Checks prepared baseline controls before layout consumes styled input."
  @spec validate_style(map(), String.t()) :: :ok | {:error, {atom(), map()}}
  def validate_style(style, source) do
    metric = :maps.get(:alignment_baseline, style, :baseline)
    dominant = :maps.get(:dominant_baseline, style, :auto)
    shift = :maps.get(:baseline_shift, style, 0.0)

    if valid_alignment?(:maps.get(:vertical_align, style, :baseline)) and
         :maps.get(:baseline_source, style, :auto) in [:auto, :first, :last] and
         metric in @metric_values and
         dominant in @dominant_values and
         (shift in [:sub, :super, :top, :center, :bottom] or
            CssLengthValidator.valid_edge?(shift, :margin)) do
      :ok
    else
      Diagnostics.error(
        :layout,
        :invalid_layout,
        "baseline controls require a registered metric, first/last/auto source and a bounded shift; recompute styles",
        operation: :layout,
        module: __MODULE__,
        source: source
      )
    end
  end
end
