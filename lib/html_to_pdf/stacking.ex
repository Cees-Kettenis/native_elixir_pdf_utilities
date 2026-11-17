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
          isolated? = NativeElixirPdfUtilities.Validators.CssStackingValidator.context?(style)
          level = NativeElixirPdfUtilities.Validators.CssStackingValidator.level(style)
          entry = {order, level}

          scope =
            if NativeElixirPdfUtilities.Validators.CssCompositingValidator.paint_group?(
                 style,
                 boxes
               ),
               do: %{
                 id: order,
                 opacity: Map.get(style, :opacity, 1.0),
                 mix_blend_mode: Map.get(style, :mix_blend_mode, :Normal)
               }

          Enum.map(boxes, fn box ->
            path = Map.get(box, :_stacking_path, [])

            background_owner = Map.get(box, :_stacking_background_order)

            ancestor_background? =
              is_list(background_owner) and background_owner != order and
                Enum.take(order, length(background_owner)) == background_owner

            if ancestor_background? do
              box
            else
              box =
                cond do
                  List.first(path) == entry -> box
                  isolated? or path == [] -> Map.put(box, :_stacking_path, [entry | path])
                  true -> box
                end

              scopes = Map.get(box, :css_compositing_scopes, [])

              if scope != nil and List.first(scopes) != scope,
                do: Map.put(box, :css_compositing_scopes, [scope | scopes]),
                else: box
            end
          end)
      end

    boxes =
      style
      |> Map.get(:_stacking_contexts, [])
      |> Enum.reverse()
      |> Enum.reduce(boxes, fn {order, level}, boxes ->
        attach(boxes, %{_stacking_order: order, z_index: level, _stacking_item: true})
      end)

    Enum.reduce(Enum.reverse(Map.get(style, :_css_compositing_contexts, [])), boxes, fn scope,
                                                                                        boxes ->
      attach(boxes, %{
        _stacking_order: scope.id,
        opacity: scope.opacity,
        mix_blend_mode: scope.mix_blend_mode,
        isolation: :isolate
      })
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

        ancestry ++
          [
            {cond do
               Map.get(box, :css_paint_role) == :css_outline -> 3
               own_background? -> 0
               true -> 2
             end, 0, index}
          ]
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
