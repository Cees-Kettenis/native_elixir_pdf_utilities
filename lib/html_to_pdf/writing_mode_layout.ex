defmodule NativeElixirPdfUtilities.HtmlToPdf.WritingModeLayout do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry

  alias NativeElixirPdfUtilities.Validators.{
    CssIntrinsicSizingValidator,
    CssLengthValidator,
    CssSizingValidator,
    HtmlValidator
  }

  @viewport {__MODULE__, :viewport}

  @doc "Shares the initial containing block with orthogonal descendants and restores nested viewports."
  @spec with_viewport(tuple(), (-> term())) :: term()
  def with_viewport(size, callback) do
    previous = Process.get(@viewport)
    Process.put(@viewport, size)

    try do
      callback.()
    after
      if previous == nil, do: Process.delete(@viewport), else: Process.put(@viewport, previous)
    end
  end

  @doc "Lays out an orthogonal formatting context with the ordinary logical line, box and table algorithms."
  @spec layout(map(), number(), number(), number(), function(), function()) ::
          {:ok, [map()], number()} | {:error, term()}
  def layout(block, x, top, available, callback, intrinsic) do
    parent = Map.get(block.style, :_writing_parent, :horizontal_tb)
    source = Map.get(block, :_writing_source, block)
    mode = Map.get(source.style, :writing_mode, :horizontal_tb)
    logical = logical_tree(source, mode)

    display =
      case logical.style.display do
        display when display in [:inline, :inline_block] -> :block
        :inline_flex -> :flex
        :inline_grid -> :grid
        display -> display
      end

    logical = put_in(logical.style.display, display)
    {viewport_width, viewport_height} = Process.get(@viewport, {available, available})
    inline_basis = if mode == :horizontal_tb, do: viewport_width, else: viewport_height

    value = Map.get(logical.style, :width, :auto)

    resolved =
      case value do
        {:css_size_math, _} -> CssSizingValidator.resolve_math(value, inline_basis, logical.style)
        _ -> CssLengthValidator.resolve(value, inline_basis, logical.style)
      end

    inline =
      case resolved do
        {:ok, value} when is_number(value) ->
          value

        {:ok, {:percent, ratio}} ->
          inline_basis * ratio

        {:ok, value} ->
          minimum = intrinsic.(logical, :min)
          preferred = intrinsic.(logical, :max)
          automatic = max(minimum, min(preferred, inline_basis))

          case value do
            {:css_intrinsic_size, _} ->
              case CssIntrinsicSizingValidator.resolve(
                     value,
                     inline_basis,
                     %{min_content: minimum, max_content: preferred, auto: automatic},
                     logical.style
                   ) do
                {:ok, size} -> size
                error -> throw({:render_resource_limit, error})
              end

            _ ->
              automatic
          end

        error ->
          throw({:render_resource_limit, error})
      end

    inline = max(inline, 0)
    logical = Map.update!(logical, :style, &Map.put(&1, :width, inline))

    edges =
      Enum.reduce([:padding, :border_widths], 0.0, fn key, sum ->
        edge = Map.get(logical.style, key, %{})
        sum + Map.get(edge, :left, 0) + Map.get(edge, :right, 0)
      end)

    margin = Map.get(logical.style, :margin, %{})

    inline =
      inline + if(Map.get(logical.style, :box_sizing) == :border_box, do: 0, else: edges) +
        Map.get(margin, :left, 0) + Map.get(margin, :right, 0)

    with {:ok, boxes, bottom} <- callback.(logical, 0.0, 0.0, inline) do
      block_size = max(-bottom, 0.0)
      boxes = DrawingGeometry.translate_boxes(boxes, 0, block_size)
      {pa, pb, pc, pd} = basis(parent)
      {ca, cb, cc, cd} = basis(mode)
      # The writing bases are orthogonal, so their inverses are transposes.
      {a, b, c, d} = {pa * ca + pb * cb, pc * ca + pd * cb, pa * cc + pb * cd, pc * cc + pd * cd}
      xs = [0, a * inline, c * block_size, a * inline + c * block_size]
      ys = [0, b * inline, d * block_size, b * inline + d * block_size]
      width = Enum.max(xs) - Enum.min(xs)
      height = Enum.max(ys) - Enum.min(ys)
      e = x - Enum.min(xs)
      f = top - Enum.max(ys)
      paint_height = Float.ceil(block_size / 0.75) * 0.75

      group = %{
        type: HtmlValidator.reserve_layout_box(:drawing_group),
        role: :writing_mode,
        x: x,
        y: top - height,
        width: inline,
        height: paint_height,
        transform: {a, b, c, d, e, f},
        boxes: boxes,
        writing_extent: %{width: width, height: height}
      }

      boxes =
        NativeElixirPdfUtilities.Validators.CssTransformValidator.mark_border_box(
          block.style,
          [group],
          %{x: x, top: top, width: width, height: height}
        )

      {:ok, boxes, top - height}
    end
  end

  defp basis(mode) do
    case mode do
      :horizontal_tb -> {1, 0, 0, 1}
      mode when mode in [:vertical_rl, :sideways_rl] -> {0, -1, 1, 0}
      :vertical_lr -> {0, -1, -1, 0}
      :sideways_lr -> {0, 1, -1, 0}
    end
  end

  defp logical_tree(node, mode) do
    HtmlValidator.reserve_render_resource(:max_work, 1, :layout)
    style = Map.get(node, :style, %{})
    actual = Map.get(style, :writing_mode, :horizontal_tb)

    if actual != mode do
      node
      |> Map.put(:_writing_source, node)
      |> Map.put(
        :style,
        logical_style(style, mode)
        |> Map.put(:_writing_parent, mode)
        |> Map.put(:_writing_orthogonal, true)
      )
    else
      style = logical_style(style, mode)

      style =
        if mode == :horizontal_tb,
          do: style,
          else:
            Map.update!(
              style,
              :font_face,
              &Map.put(&1, :_vertical_text, %{
                mode: mode,
                orientation: Map.get(style, :text_orientation, :mixed),
                combine: Map.get(style, :text_combine_upright, :none)
              })
            )

      node
      |> Map.put(:style, style)
      |> Map.update(:children, [], &Enum.map(&1, fn child -> logical_tree(child, mode) end))
    end
  end

  defp logical_style(style, mode) do
    pairs =
      if mode == :horizontal_tb,
        do: [],
        else: [
          width: :height,
          height: :width,
          min_width: :min_height,
          min_height: :min_width,
          max_width: :max_height,
          max_height: :max_width
        ]

    result =
      Enum.reduce(pairs, Map.drop(style, Keyword.keys(pairs)), fn {target, source}, result ->
        if Map.has_key?(style, source), do: Map.put(result, target, style[source]), else: result
      end)

    sides =
      case mode do
        :horizontal_tb -> [:top, :right, :bottom, :left]
        mode when mode in [:vertical_rl, :sideways_rl] -> [:right, :bottom, :left, :top]
        :vertical_lr -> [:left, :bottom, :right, :top]
        :sideways_lr -> [:left, :top, :right, :bottom]
      end

    result =
      Enum.reduce(
        [:margin, :padding, :border_widths, :border_colors, :border_styles, :offsets],
        result,
        fn key, result ->
          case Map.get(style, key) do
            %{} = edges ->
              Map.put(
                result,
                key,
                Map.new(Enum.zip([:top, :right, :bottom, :left], sides), fn {target, source} ->
                  {target, Map.get(edges, source)}
                end)
              )

            _ ->
              result
          end
        end
      )

    result =
      if Map.get(style, :text_orientation) == :upright and mode in [:vertical_rl, :vertical_lr],
        do: result |> Map.put(:direction, :ltr) |> Map.put(:_bidi_contexts, []),
        else: result

    result |> Map.put(:writing_mode, :horizontal_tb) |> Map.put(:_writing_plane, mode)
  end
end
