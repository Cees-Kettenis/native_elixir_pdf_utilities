defmodule NativeElixirPdfUtilities.HtmlToPdf.StickyGeometry do
  @moduledoc "Initial printed sticky geometry, resolved before fragmentation without scrolling or repetition."
  alias NativeElixirPdfUtilities.Validators.{
    CssPositionValidator,
    CssTransformValidator,
    CssOverflowValidator
  }

  @key {__MODULE__, :pending}

  @doc "Scopes pending sticky geometry to the current document, including independent child viewports."
  @spec with_layout((-> term())) :: term()
  def with_layout(operation) do
    previous = Process.put(@key, false)

    try do
      case operation.() do
        {:ok, layout} = result ->
          if Process.get(@key) do
            {width, height} = layout.page_size
            margins = layout.margins

            viewport = %{
              x: margins.left,
              y: margins.bottom,
              width: width - margins.left - margins.right,
              height: height - margins.top - margins.bottom
            }

            boxes = layout.boxes |> assign(viewport, viewport) |> resolve()
            {:ok, %{layout | boxes: boxes}}
          else
            result
          end

        other ->
          other
      end
    after
      if previous == nil, do: Process.delete(@key), else: Process.put(@key, previous)
    end
  end

  @doc "Retains a sticky border box in the existing transform and containing-clip coordinate contract."
  @spec attach([map()], map(), map()) :: [map()]
  def attach(boxes, style, bounds) do
    if Map.get(style, :position, :static) == :sticky do
      Process.put(@key, true)
      {:ok, transform} = CssTransformValidator.prepare("translate(0pt,0pt)")

      {:ok, scope} =
        CssTransformValidator.prepare_scope(Map.put(style, :css_transform, transform), bounds)

      data = %{style: style, containing: nil, scrollport: nil}
      scope = Map.put(scope, :_sticky_position, data)

      Enum.map(boxes, fn box ->
        inner =
          Enum.map(Map.get(box, :css_transform_scopes, []), fn child ->
            case child do
              %{_sticky_position: data} ->
                data =
                  Enum.reduce([:containing, :scrollport], data, fn field, data ->
                    case Map.get(data, field) do
                      nil ->
                        data

                      rect ->
                        Map.put(
                          data,
                          field,
                          Map.update(rect, :ancestors, [scope.id], &[scope.id | &1])
                        )
                    end
                  end)

                %{child | _sticky_position: data}

              _ ->
                child
            end
          end)

        Map.put(box, :css_transform_scopes, [scope | inner])
      end)
    else
      boxes
    end
  end

  @doc "Records the nearest block content box and, independently, the nearest scroll-container padding box."
  @spec content([map()], map(), map()) :: [map()]
  def content(boxes, style, padding_box) do
    if Process.get(@key, false) do
      padding = Map.get(style, :padding, %{left: 0, right: 0, top: 0, bottom: 0})

      content = %{
        x: padding_box.x + padding.left,
        y: padding_box.y + padding.bottom,
        width: max(padding_box.width - padding.left - padding.right, 0),
        height: max(padding_box.height - padding.top - padding.bottom, 0)
      }

      scrollport = if CssOverflowValidator.independent?(style), do: padding_box
      assign(boxes, content, scrollport)
    else
      boxes
    end
  end

  @doc "Fills still-unresolved containing and scrollport owners without replacing nearer owners."
  @spec assign([map()], map(), map() | nil) :: [map()]
  def assign(boxes, containing, scrollport) do
    Enum.map(boxes, fn box ->
      case Map.get(box, :css_transform_scopes) do
        nil ->
          box

        scopes ->
          Map.put(
            box,
            :css_transform_scopes,
            Enum.map(scopes, fn scope ->
              case scope do
                %{_sticky_position: data} ->
                  %{
                    scope
                    | _sticky_position: %{
                        data
                        | containing: data.containing || Map.put(containing, :ancestors, []),
                          scrollport:
                            data.scrollport ||
                              if(scrollport, do: Map.put(scrollport, :ancestors, []))
                      }
                  }

                _ ->
                  scope
              end
            end)
          )
      end
    end)
  end

  @doc "Resolves each retained sticky owner once using the initial, unscrolled scrollport."
  @spec resolve([map()]) :: [map()]
  def resolve(boxes) do
    {boxes, _} =
      Enum.map_reduce(boxes, %{}, fn box, resolved ->
        {scopes, resolved, _ancestors} =
          Enum.reduce(Map.get(box, :css_transform_scopes, []), {[], resolved, []}, fn scope,
                                                                                      {scopes,
                                                                                       resolved,
                                                                                       ancestors} ->
            case scope do
              %{_sticky_position: data} ->
                {scope, delta} =
                  Map.get_lazy(resolved, scope.id, fn ->
                    accumulated = fn ids ->
                      Enum.reduce(ids, {0.0, 0.0}, fn id, {x, y} ->
                        {_scope, {dx, dy}} = Map.fetch!(resolved, id)
                        {x + dx, y + dy}
                      end)
                    end

                    {dx, dy} = accumulated.(ancestors)
                    bounds = scope.border_box

                    border = %{
                      x: bounds.x + dx,
                      y: bounds.top - bounds.height + dy,
                      width: bounds.width,
                      height: bounds.height
                    }

                    translate = fn rect ->
                      {x, y} = accumulated.(Map.get(rect, :ancestors, []))
                      %{rect | x: rect.x + x, y: rect.y + y}
                    end

                    containing = translate.(data.containing)
                    port = translate.(data.scrollport)
                    {dx, dy} = offset(data.style, border, containing, port)

                    {:ok, transform} =
                      CssTransformValidator.prepare("translate(#{dx}pt,#{-dy}pt)")

                    {:ok, prepared} =
                      CssTransformValidator.prepare_scope(
                        Map.put(data.style, :css_transform, transform),
                        bounds
                      )

                    {%{scope | matrix: prepared.matrix, style: prepared.style}
                     |> Map.delete(:_sticky_position), {dx, dy}}
                  end)

                resolved = Map.put(resolved, scope.id, {scope, delta})
                {[scope | scopes], resolved, [scope.id | ancestors]}

              _ ->
                {[scope | scopes], resolved, ancestors}
            end
          end)

        box =
          if scopes == [],
            do: box,
            else: Map.put(box, :css_transform_scopes, Enum.reverse(scopes))

        {box, resolved}
      end)

    boxes
  end

  @doc "Computes sticky translation from border edges and the constrained position box, leaving flow unchanged."
  @spec offset(map(), map(), map(), map()) :: {number(), number()}
  def offset(style, border, containing, scrollport) do
    offsets = Map.get(style, :offsets, %{left: :auto, right: :auto, top: :auto, bottom: :auto})
    margin = Map.get(style, :margin, %{left: 0, right: 0, top: 0, bottom: 0})

    Enum.map([:width, :height], fn axis ->
      {start, finish, position} =
        if axis == :width, do: {:left, :right, :x}, else: {:bottom, :top, :y}

      resolve = fn side ->
        case Map.fetch!(offsets, side) do
          :auto ->
            nil

          value ->
            {:ok, value} =
              CssPositionValidator.resolve(value, Map.fetch!(scrollport, axis), style)

            value
        end
      end

      start_inset = resolve.(start)
      end_inset = resolve.(finish)
      extent = Map.fetch!(border, axis)
      port_start = Map.fetch!(scrollport, position)
      port_end = port_start + Map.fetch!(scrollport, axis)
      start_edge = Map.fetch!(border, position)
      end_edge = start_edge + extent
      low = start_inset || 0.0
      high = end_inset || 0.0
      # Reduce the flow-relative end inset if the sticky box is larger than its view rectangle.
      axes = NativeElixirPdfUtilities.Validators.CssLogicalValidator.axes(style)
      ending = if axes.inline_size == axis, do: axes.inline_end, else: axes.block_end
      start_is_end? = start == ending

      {low, high} =
        if start_is_end?,
          do: {min(low, Map.fetch!(scrollport, axis) - high - extent), high},
          else: {low, min(high, Map.fetch!(scrollport, axis) - low - extent)}

      delta = if start_inset != nil, do: max(port_start + low - start_edge, 0.0), else: 0.0
      delta = if end_inset != nil, do: min(delta, port_end - high - end_edge), else: delta
      cb_start = Map.fetch!(containing, position)
      cb_end = cb_start + Map.fetch!(containing, axis)
      start_margin = min(Map.fetch!(margin, start), start_edge - cb_start)
      end_margin = min(Map.fetch!(margin, finish), cb_end - end_edge)
      min(max(delta, cb_start + start_margin - start_edge), cb_end - end_margin - end_edge)
    end)
    |> List.to_tuple()
  end
end
