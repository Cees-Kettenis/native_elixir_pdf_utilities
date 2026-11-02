defmodule NativeElixirPdfUtilities.HtmlToPdf.Stacking do
  @moduledoc false

  @doc "Attaches paint ancestry without changing the order used by pagination."
  @spec attach([map()], map()) :: [map()]
  def attach(boxes, style) do
    boxes =
      case Map.get(style, :_stacking_order) do
        nil ->
          boxes

        order ->
          z = Map.get(style, :z_index, :auto)
          isolated? = is_integer(z) or Map.get(style, :css_transform, :none) != :none
          level = if is_integer(z), do: z, else: 0
          entry = {order, level}

          Enum.map(boxes, fn box ->
            path = Map.get(box, :_stacking_path, [])

            background_owner = Map.get(box, :_stacking_background_order)

            ancestor_background? =
              is_list(background_owner) and background_owner != order and
                Enum.take(order, length(background_owner)) == background_owner

            cond do
              ancestor_background? -> box
              List.first(path) == entry -> box
              isolated? or path == [] -> Map.put(box, :_stacking_path, [entry | path])
              true -> box
            end
          end)
      end

    style
    |> Map.get(:_stacking_contexts, [])
    |> Enum.reverse()
    |> Enum.reduce(boxes, fn {order, level}, boxes ->
      attach(boxes, %{_stacking_order: order, z_index: level})
    end)
  end

  @doc "Orders placed paint within each stacking context, preserving source order for ties."
  @spec order([map()]) :: [map()]
  def order(boxes) do
    if Enum.any?(boxes, fn box ->
         Map.has_key?(box, :_stacking_path) or
           Map.has_key?(box, :_stacking_root_background) or Map.has_key?(box, :boxes)
       end) do
      boxes
      |> Enum.with_index()
      |> Enum.sort_by(fn {box, index} ->
        path = Map.get(box, :_stacking_path, [])

        ancestry =
          Enum.map(path, fn {source, z} ->
            layer =
              cond do
                z < 0 -> 1
                z == 0 -> 4
                true -> 5
              end

            {layer, z, source}
          end)

        own_background? =
          case List.last(path) do
            {source, _} ->
              Map.get(box, :_stacking_background_order) == source

            nil ->
              Map.get(box, :position_anchor) == :canvas or
                Map.get(box, :_stacking_root_background, false)
          end

        ancestry ++ [{if(own_background?, do: 0, else: 2), 0, index}]
      end)
      |> Enum.map(fn {box, _index} ->
        # A placed page can be finalized again when furniture is composed. Consume
        # the ancestry so transform groups retain their already resolved paint order.
        box =
          Map.drop(box, [:_stacking_path, :_stacking_background_order, :_stacking_root_background])

        case box do
          %{boxes: children} -> %{box | boxes: order(children)}
          _ -> box
        end
      end)
    else
      boxes
    end
  end
end
