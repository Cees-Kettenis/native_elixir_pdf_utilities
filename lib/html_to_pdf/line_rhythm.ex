defmodule NativeElixirPdfUtilities.HtmlToPdf.LineRhythm do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @grid {__MODULE__, :grid}

  @doc false
  @spec with_grid(map(), number(), (-> term())) :: term()
  def with_grid(style, top, callback) do
    step = Map.fetch!(style, :line_height)
    depth = HtmlValidator.text_baseline_depth(style)
    baseline = top - depth
    with_phase(%{top: top, baseline: baseline, step: step}, callback)
  end

  @doc false
  @spec reset_origin((-> term())) :: term()
  def reset_origin(callback) do
    case Process.get(@grid) do
      nil -> callback.()
      grid -> with_phase(%{grid | top: 0.0, baseline: grid.baseline - grid.top}, callback)
    end
  end

  defp with_phase(grid, callback) do
    previous = Process.get(@grid)
    Process.put(@grid, grid)

    try do
      callback.()
    after
      if previous == nil, do: Process.delete(@grid), else: Process.put(@grid, previous)
    end
  end

  @doc false
  @spec round_line(number(), number(), number()) :: {number(), number()}
  def round_line(height, depth, step) do
    adjusted = round_size(height, step, :up)
    {adjusted, depth + (adjusted - height) / 2}
  end

  @doc false
  @spec line_shift(atom(), number(), number(), number()) :: number()
  def line_shift(policy, top, height, depth) do
    case Process.get(@grid) do
      %{step: step} = grid when step > 0 ->
        case policy do
          :baseline ->
            distance(top - depth, grid.baseline, step)

          :contain ->
            span = round_size(height, step, :up)
            distance(top, grid.top - (span - height) / 2, step)
        end

      _ ->
        0.0
    end
  end

  @doc false
  @spec box_shift(atom(), number(), number(), [map()]) :: number()
  def box_shift(policy, top, bottom, boxes) do
    case Process.get(@grid) do
      %{step: step} = grid when step > 0 ->
        case policy do
          :block_start ->
            distance(top, grid.top, step)

          :block_end ->
            distance(bottom, grid.top, step)

          :center ->
            distance((top + bottom) / 2, grid.top - step / 2, step)

          side when side in [:baseline, :last_baseline] ->
            baselines = if side == :baseline, do: boxes, else: Enum.reverse(boxes)

            point =
              Enum.find_value(baselines, fn
                %{type: :text, y: y} -> y
                _ -> nil
              end)

            if point == nil, do: 0.0, else: distance(point, grid.baseline, step)
        end

      _ ->
        0.0
    end
  end

  @doc false
  @spec align_column([map()], number(), atom()) :: [map()]
  def align_column(boxes, top, policy) do
    with %{step: step} = grid when step > 0 <- Process.get(@grid),
         %{y: baseline, line_baseline_depth: depth, line_box_height: height} <-
           Enum.find(
             boxes,
             &match?(%{type: :text, line_baseline_depth: _, line_box_height: _}, &1)
           ) do
      shift =
        case policy do
          :baseline ->
            distance(baseline, top - (grid.top - grid.baseline), step)

          :contain ->
            span = round_size(height, step, :up)
            distance(baseline + depth, top - (span - height) / 2, step)
        end

      NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.translate_boxes(boxes, 0.0, -shift)
    else
      _ -> boxes
    end
  end

  defp distance(point, grid, step) do
    units = (grid - point) / step
    max(point - (grid - Float.ceil(units - 1.0e-7) * step), 0.0)
  end

  @doc false
  @spec block_step(map(), number(), number()) :: {number(), number(), number()}
  def block_step(style, height, margins) do
    step = style.block_step_size

    extra =
      max(
        round_size(height + margins, step, Map.get(style, :block_step_round, :up)) - margins,
        0.0
      ) - height

    lead =
      case Map.get(style, :block_step_align, :auto) do
        :start -> 0.0
        :end -> extra
        _ -> extra / 2
      end

    case Map.get(style, :block_step_insert, :margin_box) do
      :margin_box -> {0.0, lead, extra}
      :padding_box -> {extra, lead, 0.0}
      :content_box -> {extra, 0.0, 0.0}
    end
  end

  defp round_size(value, step, direction) do
    quotient = value / step

    units =
      case direction do
        :up -> Float.ceil(quotient - 1.0e-7)
        :down -> Float.floor(quotient + 1.0e-7)
        :nearest -> Float.floor(quotient + 0.5)
      end

    units * step
  end
end
