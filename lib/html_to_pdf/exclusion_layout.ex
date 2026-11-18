defmodule NativeElixirPdfUtilities.HtmlToPdf.ExclusionLayout do
  @moduledoc false

  alias NativeElixirPdfUtilities.HtmlToPdf.Layout
  alias NativeElixirPdfUtilities.Validators.{CssLengthValidator, HtmlValidator}

  @scope {__MODULE__, :exclusions}
  @zero %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}

  @doc false
  @spec with_scope([map()], map(), number(), number(), number(), (-> term())) :: term()
  def with_scope(children, style, x, top, width, callback) do
    positions = for %{style: %{wrap_flow: flow} = style} <- children, flow != :auto, do: style

    case positions do
      [] -> callback.()
      _ -> with_rectangles(positions, bounds(style, x, top, width), callback)
    end
  end

  defp with_rectangles(positions, bounds, callback) do
    exclusions =
      Enum.map(positions, fn style ->
        case rectangle(style, bounds) do
          {:ok, rect} ->
            rect

          :error ->
            error =
              NativeElixirPdfUtilities.Diagnostics.error(
                :layout,
                :not_implemented,
                "a print exclusion needs definite size and an inset on each axis",
                module: __MODULE__,
                source: "wrap-flow"
              )

            throw({:render_resource_limit, error})
        end
      end)

    previous = Process.get(@scope, [])
    Process.put(@scope, exclusions ++ previous)

    try do
      callback.()
    after
      if previous == [], do: Process.delete(@scope), else: Process.put(@scope, previous)
    end
  end

  @doc false
  @spec active(map()) :: [map()]
  def active(style) do
    if :maps.get(:wrap_through, style, :wrap) == :none, do: [], else: Process.get(@scope, [])
  end

  defp bounds(style, x, top, width) do
    padding = Map.get(style, :padding, @zero)
    height = Layout.resolve_content_size(style, :height, Map.get(style, :_containing_height), nil)

    %{
      x: x - padding.left,
      top: top + padding.top,
      width: width + padding.left + padding.right,
      height: if(height != nil, do: height + padding.top + padding.bottom),
      direction: Map.get(style, :direction, :ltr)
    }
  end

  defp rectangle(style, bounds) do
    with {:ok, style} <- CssLengthValidator.resolve_edges(style, bounds.width),
         width when is_number(width) <-
           Layout.resolve_content_size(style, :width, bounds.width, nil),
         height when is_number(height) <-
           Layout.resolve_content_size(style, :height, bounds.height, nil),
         offsets = Map.get(style, :offsets, @zero),
         margin = Map.get(style, :margin, @zero),
         padding = Map.get(style, :padding, @zero),
         border = Map.get(style, :border_widths, @zero),
         width = width + padding.left + padding.right + border.left + border.right,
         height = height + padding.top + padding.bottom + border.top + border.bottom,
         x when is_number(x) <-
           offset(offsets.left, offsets.right, bounds.width, width + margin.left + margin.right),
         y when is_number(y) <-
           offset(offsets.top, offsets.bottom, bounds.height, height + margin.top + margin.bottom) do
      left = bounds.x + x + margin.left
      top = bounds.top - y - margin.top

      side =
        case {style.wrap_flow, bounds.direction} do
          {:start, :rtl} -> :allow_right
          {:start, _} -> :allow_left
          {:end, :rtl} -> :allow_left
          {:end, _} -> :allow_right
          {flow, _} -> flow
        end

      rect = %{x: left, top: top, width: width, height: height}
      shape = %{geometry: {:rect, rect}, margin: 0.0, clip: rect}
      HtmlValidator.reserve_render_resource(:max_entries, 1, :layout)

      {:ok,
       %{
         side: side,
         left: left,
         right: left + width,
         top: top,
         bottom: top - height,
         shape: shape
       }}
    else
      _ -> :error
    end
  end

  defp offset(start, stop, basis, size) do
    case resolve(start, basis) do
      nil ->
        with true <- is_number(basis),
             stop when is_number(stop) <- resolve(stop, basis),
             do: basis - size - stop

      start ->
        start
    end
  end

  defp resolve(value, basis) do
    case value do
      {:percent, proportion} when is_number(basis) -> proportion * basis
      value when is_number(value) -> value
      _ -> nil
    end
  end

  @doc false
  @spec available(atom(), {number(), number()}, number(), number()) :: {number(), number()}
  def available(side, {left, right}, start, stop) do
    if stop <= left or start >= right do
      {left, right}
    else
      first = {left, max(min(right, start), left)}
      last = {min(max(left, stop), right), right}
      first_size = elem(first, 1) - elem(first, 0)
      last_size = elem(last, 1) - elem(last, 0)

      case side do
        :allow_left -> first
        :allow_right -> last
        :clear -> {left, left}
        :minimum -> if first_size <= last_size, do: first, else: last
        _ -> if first_size >= last_size, do: first, else: last
      end
    end
  end
end
