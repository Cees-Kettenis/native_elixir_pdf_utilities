defmodule NativeElixirPdfUtilities.HtmlToPdf.FloatLayout do
  @moduledoc "Formatting-context float placement, clearance and line exclusions."
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, Layout, FlowShape}

  alias NativeElixirPdfUtilities.Validators.{
    CssDisplayValidator,
    CssLengthValidator,
    HtmlValidator
  }

  @context {__MODULE__, :context}

  @doc "Scopes float ownership to a formatting context and restores its parent on exit."
  @spec with_context((-> term())) :: term()
  def with_context(callback) do
    previous = Process.get(@context)
    Process.put(@context, %{floats: [], last_top: nil})

    try do
      callback.()
    after
      if previous == nil, do: Process.delete(@context), else: Process.put(@context, previous)
    end
  end

  @doc "Identifies an in-flow float after position and display have been computed."
  @spec floating?(map()) :: boolean()
  def floating?(style) do
    case style do
      style when not is_map_key(style, :float) ->
        false

      _ ->
        Map.get(style, :float, :none) != :none and
          Map.get(style, :position, :static) not in [:absolute, :fixed] and
          Map.get(style, :display) not in [:none, :contents] and
          not Map.get(style, :_float_placed, false)
    end
  end

  @doc "Places a float or ordinary block using existing box measurement and painting."
  @spec layout(map(), number(), number(), number(), function(), function()) :: term()
  def layout(block, x, top, width, render, intrinsic) do
    cond do
      floating?(block.style) ->
        place_float(block, x, top, width, render, intrinsic)

      true ->
        cleared = clear_top(Map.get(block.style, :clear, :none), block.style, top)

        if CssDisplayValidator.independent?(block.style) do
          independent_block(block, x, cleared, width, render)
        else
          render.(block, x, cleared, width)
        end
    end
  end

  defp place_float(block, x, top, width, render, intrinsic) do
    original_top = top

    style =
      case CssLengthValidator.resolve_edges(block.style, width) do
        {:ok, style} -> style
        error -> throw({:render_resource_limit, error})
      end

    margin = Map.get(style, :margin, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0})
    edge = Map.get(style, :padding, %{left: 0.0, right: 0.0})
    border = Map.get(style, :border_widths, %{left: 0.0, right: 0.0})
    decorations = edge.left + edge.right + border.left + border.right
    available = max(width - margin.left - margin.right - decorations, 0.0)
    {minimum, maximum} = intrinsic.(%{block | style: style}, available)

    content =
      Layout.resolved_content_size(style, :width, width, max(minimum, min(maximum, available)))

    border_width = content + decorations
    outer_width = border_width + margin.left + margin.right

    top =
      if Map.get(style, :_float_preceding_width, 0.0) + outer_width > width,
        do: top - Map.get(style, :_float_line_height, style.line_height),
        else: top

    style =
      style
      |> Map.put(:_float_placed, true)
      |> Map.put(
        :width,
        if(Map.get(style, :box_sizing) == :border_box, do: border_width, else: content)
      )

    result = with_context(fn -> render.(%{block | style: style}, 0.0, 0.0, outer_width) end)

    case result do
      {:ok, boxes, bottom} ->
        height = max(-bottom, 0.0)
        context = Process.get(@context, %{floats: [], last_top: nil})
        top = clear_top(Map.get(style, :clear, :none), style, top)
        top = if context.last_top == nil, do: top, else: min(top, context.last_top)
        {left, right, top} = available_band(context.floats, x, top, width, height, outer_width)
        side = physical_side(Map.fetch!(style, :float), style)
        left = if side == :right, do: right - outer_width, else: left

        float = %{
          left: left,
          right: left + outer_width,
          top: top,
          bottom: top - height,
          side: side,
          style: style,
          border: %{
            x: left + margin.left,
            top: top - margin.top,
            width: border_width,
            height: max(height - margin.top - margin.bottom, 0.0)
          }
        }

        float = Map.put(float, :shape, FlowShape.prepare_float(float, width))
        HtmlValidator.reserve_render_resource(:max_entries, 1, :layout)
        Process.put(@context, %{floats: [float | context.floats], last_top: top})
        boxes = DrawingGeometry.translate_boxes(boxes, left, top)
        {:ok, boxes, original_top}

      error ->
        error
    end
  end

  defp independent_block(block, x, top, width, render) do
    floats = Process.get(@context, %{floats: []}).floats

    if floats == [] do
      with_context(fn -> render.(block, x, top, max(width, 0.0)) end)
    else
      margin = Map.get(block.style, :margin, %{left: 0.0, right: 0.0})
      padding = Map.get(block.style, :padding, %{left: 0.0, right: 0.0})
      border = Map.get(block.style, :border_widths, %{left: 0.0, right: 0.0})
      preferred = Layout.resolved_content_size(block.style, :width, width, nil)

      minimum =
        if is_number(preferred),
          do:
            preferred + margin.left + margin.right + padding.left + padding.right + border.left +
              border.right,
          else: 0.0

      place_independent(
        block,
        %{floats: floats, x: x, width: width, minimum: minimum},
        top,
        0.0,
        render
      )
    end
  end

  defp place_independent(block, context, top, height, render) do
    {left, right, top} =
      available_band(context.floats, context.x, top, context.width, height, context.minimum)

    with {:ok, boxes, bottom} <-
           with_context(fn -> render.(block, left, top, max(right - left, 0.0)) end) do
      height = max(height, top - bottom)

      band =
        available_band(context.floats, context.x, top, context.width, height, context.minimum)

      if band == {left, right, top},
        do: {:ok, boxes, bottom},
        else: place_independent(block, context, elem(band, 2), height, render)
    end
  end

  @doc "Separates floats from an inline tree while retaining each insertion's styled source prefix."
  @spec extract_inline([map()]) :: {[map()], [map()]}
  def extract_inline(children) do
    HtmlValidator.reserve_render_resource(:max_work, length(children), :layout)

    {normal, floats} =
      Enum.reduce(children, {[], []}, fn child, {normal, floats} ->
        cond do
          floating?(Map.get(child, :style, %{})) ->
            {normal, [%{node: child, prefix: Enum.reverse(normal)} | floats]}

          Map.get(child, :type) == :element and get_in(child, [:style, :display]) == :inline ->
            {nested, found} = extract_inline(child.children)

            found =
              Enum.map(found, fn entry ->
                %{entry | prefix: Enum.reverse(normal) ++ [%{child | children: entry.prefix}]}
              end)

            {[%{child | children: nested} | normal], Enum.reverse(found, floats)}

          true ->
            {[child | normal], floats}
        end
      end)

    {Enum.reverse(normal), Enum.reverse(floats)}
  end

  defp available_band(floats, x, top, width, height, minimum, kind \\ :box) do
    HtmlValidator.reserve_render_resource(:max_work, length(floats) + 1, :layout)

    active =
      Enum.flat_map(floats, fn float ->
        interval =
          if kind == :shape,
            do: FlowShape.interval(float.shape, top, top - max(height, 0.000001)),
            else:
              if(float.top > top - max(height, 0.000001) and float.bottom < top,
                do: {float.left, float.right}
              )

        case interval do
          nil -> []
          {left, right} -> [{float, left, right}]
        end
      end)

    left =
      Enum.reduce(active, x, fn {f, _, right}, left ->
        if f.side == :left, do: max(left, right), else: left
      end)

    right =
      Enum.reduce(active, x + width, fn {f, left, _}, right ->
        if f.side == :right, do: min(right, left), else: right
      end)

    {left, right} =
      Enum.reduce(active, {left, right}, fn
        {%{side: side}, _, _}, band when side in [:left, :right] ->
          band

        {%{side: side}, start, stop}, band ->
          NativeElixirPdfUtilities.HtmlToPdf.ExclusionLayout.available(side, band, start, stop)
      end)

    if right - left < minimum and active != [] do
      next = active |> Enum.map(fn {float, _, _} -> float.bottom end) |> Enum.max()

      if kind == :shape do
        fits? = fn y ->
          {left, right, _} = available_band(floats, x, y, width, height, 0.0, kind)
          right - left >= minimum
        end

        knots =
          floats
          |> Enum.flat_map(&FlowShape.transition_heights(&1.shape, height))
          |> then(&[next | &1])
          |> Enum.filter(&(&1 < top and &1 >= next))
          |> Enum.uniq()
          |> Enum.sort(:desc)

        boundary = FlowShape.first_fitting_height(top, knots, fits?, next)
        # Search line opportunities in CSS-pixel descent steps, retaining the
        # analytical horizontal intersection at the selected line position.
        boundary = max(top - Float.ceil((top - boundary) / 0.75) * 0.75, next)
        available_band(floats, x, boundary, width, height, minimum, kind)
      else
        available_band(floats, x, next, width, height, minimum, kind)
      end
    else
      {left, right, top}
    end
  end

  @doc "Resolves clearance against floats owned by the current formatting context."
  @spec clear_top(atom(), map(), number()) :: number()
  def clear_top(clear, style, top) do
    if clear == :none do
      top
    else
      side = physical_side(clear, style)

      Enum.reduce(Process.get(@context, %{floats: []}).floats, top, fn float, y ->
        if clear == :both or side == float.side, do: min(y, float.bottom), else: y
      end)
    end
  end

  defp physical_side(value, style) do
    case {value, Map.get(style, :direction, :ltr)} do
      {:inline_start, :rtl} -> :right
      {:inline_end, :rtl} -> :left
      {:inline_start, _} -> :left
      {:inline_end, _} -> :right
      _ -> value
    end
  end

  @doc "Includes contained floats in an independent formatting context's automatic height."
  @spec content_height(map(), number(), number()) :: number()
  def content_height(style, top, height) do
    if CssDisplayValidator.independent?(style) or Map.get(style, :_root_element, false) do
      max(height, top - bottom(top))
    else
      height
    end
  end

  @doc "Returns the lowest float margin edge or the supplied empty-context fallback."
  @spec bottom(number()) :: number()
  def bottom(default) do
    Enum.reduce(Process.get(@context, %{floats: []}).floats, default, &min(&1.bottom, &2))
  end

  @doc "Adds a nonpainting exclusion marker using the actual line-measurement callback."
  @spec inherit_exclusions([map()], map(), number(), number(), number(), function()) :: [map()]
  def inherit_exclusions(runs, style, x, top, width, measure) do
    context = Process.get(@context)
    exclusions = NativeElixirPdfUtilities.HtmlToPdf.ExclusionLayout.active(style)

    case {context, exclusions} do
      {context, []}
      when (context == nil or :erlang.map_get(:floats, context) == []) and
             (not is_map_key(style, :shape_inside) or
                :erlang.map_get(:shape_inside, style) == :auto) ->
        runs

      {context, exclusions} ->
        floats = if context == nil, do: exclusions, else: context.floats ++ exclusions

        inside =
          if Map.get(style, :shape_inside, :auto) == :auto do
            nil
          else
            height =
              Layout.resolved_content_size(
                style,
                :height,
                Map.get(style, :_containing_height),
                nil
              )

            FlowShape.prepare_inside(style, x, top, width, height)
          end

        if runs != [] and (floats != [] or inside != nil) do
          [
            marker(%{
              floats: floats,
              inside: inside,
              style: style,
              x: x,
              top: top,
              width: width,
              measure: measure
            })
            | runs
          ]
        else
          runs
        end
    end
  end

  defp marker(context) do
    {left, right, top} =
      available_band(
        context.floats,
        context.x,
        context.top,
        context.width,
        Map.get(context, :line_height, context.style.line_height),
        0.000001,
        :shape
      )

    {left, right} =
      if context.inside == nil do
        {left, right}
      else
        case FlowShape.inside_interval(
               context.inside,
               top,
               top - Map.get(context, :line_height, context.style.line_height)
             ) do
          nil -> {context.x, context.x}
          {a, b} -> {max(left, a), min(right, b)}
        end
      end

    before = max(left - context.x, 0.0)
    after_width = max(context.x + context.width - right, 0.0)

    %{
      type: :inline_positioning_marker,
      text: "",
      style: context.style,
      _inline_padding_width: before + after_width,
      inline_positioning_contexts: [],
      _float_exclusion: %{context | top: top},
      _float_line: %{left: before, right: after_width, clearance: context.top - top}
    }
  end

  @doc "Carries float exclusions to the next line using the previous line's measured height."
  @spec next_line([map()]) :: [map()]
  def next_line(line) do
    context =
      case line do
        [%{type: :inline_positioning_marker} | _] ->
          Enum.find_value(line, &Map.get(&1, :_float_exclusion))

        _ ->
          nil
      end

    case context do
      nil ->
        []

      context ->
        height = context.measure.(line)
        [marker(%{context | top: context.top - height})]
    end
  end

  @doc "Moves an empty line below a float when its first indivisible item cannot fit beside it."
  @spec fit_line([map()], number(), number()) :: [map()]
  def fit_line(line, minimum, height) do
    reservation =
      case line do
        [%{type: :inline_positioning_marker} | _] ->
          Enum.find(line, &Map.has_key?(&1, :_float_exclusion))

        _ ->
          nil
      end

    case reservation do
      nil ->
        line

      reservation ->
        context = reservation._float_exclusion

        extra =
          Enum.reduce(line, 0.0, fn run, width ->
            if Map.has_key?(run, :_float_exclusion),
              do: width,
              else: width + Map.get(run, :_inline_padding_width, 0.0)
          end)

        {_, _, top} =
          available_band(
            context.floats,
            context.x,
            context.top,
            context.width,
            height,
            min(minimum + extra, context.width),
            :shape
          )

        top =
          if context.inside == nil,
            do: top,
            else:
              FlowShape.fit_inside(
                context.inside,
                top,
                height,
                min(minimum + extra, context.width)
              )

        if top < context.top or
             height != Map.get(context, :line_height, context.style.line_height) do
          updated = marker(context |> Map.put(:top, top) |> Map.put(:line_height, height))

          updated =
            put_in(
              updated._float_line.clearance,
              reservation._float_line.clearance + context.top - top +
                updated._float_line.clearance
            )

          Enum.map(line, fn run -> if run == reservation, do: updated, else: run end)
        else
          line
        end
    end
  end

  @doc "Separates fitting reservations from the line's paintable interval."
  @spec paint_line([map()], number(), number()) :: {[map()], number(), number(), number()}
  def paint_line(runs, x, width) do
    # Fitting reservations precede paintable content, alongside indentation
    # and initial-letter markers. Ordinary lines need no metadata search.
    reservation =
      case runs do
        [%{type: :inline_positioning_marker} | _] ->
          Enum.find_value(runs, &Map.get(&1, :_float_line))

        _ ->
          nil
      end

    case reservation do
      nil ->
        {runs, x, width, 0.0}

      line ->
        runs =
          Enum.map(runs, fn run ->
            if Map.has_key?(run, :_float_line), do: %{run | _inline_padding_width: 0.0}, else: run
          end)

        {runs, x + line.left, max(width - line.left - line.right, 0.0), line.clearance}
    end
  end
end
