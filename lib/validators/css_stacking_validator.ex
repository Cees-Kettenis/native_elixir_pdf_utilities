defmodule NativeElixirPdfUtilities.Validators.CssStackingValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}

  @doc "Checks the retained source order of a positioned element."
  @spec valid_source_order?(term()) :: boolean()
  def valid_source_order?(order) do
    is_list(order) and length(order) <= Limits.get(:max_html_depth) and
      Enum.all?(order, &(is_integer(&1) and &1 >= 0))
  end

  @doc "Checks paint ancestry on already validated drawing boxes before ordering them."
  @spec validate_boxes([map()]) :: :ok | {:error, {atom(), map()}}
  def validate_boxes(boxes) do
    Enum.reduce_while(boxes, :ok, fn box, :ok ->
      case box do
        box
        when not is_map_key(box, :_stacking_path) and
               not is_map_key(box, :_stacking_background_order) and
               not is_map_key(box, :_stacking_root_background) ->
          case box do
            %{boxes: children} ->
              case validate_boxes(children) do
                :ok -> {:cont, :ok}
                error -> {:halt, error}
              end

            _ ->
              {:cont, :ok}
          end

        _ ->
          path = Map.get(box, :_stacking_path, [])
          background = Map.get(box, :_stacking_background_order)

          valid =
            is_list(path) and length(path) <= Limits.get(:max_html_depth) and
              Enum.all?(path, fn entry ->
                case entry do
                  {order, z} -> valid_source_order?(order) and is_integer(z)
                  _ -> false
                end
              end) and
              (background == nil or valid_source_order?(background)) and
              is_boolean(Map.get(box, :_stacking_root_background, false))

          result =
            cond do
              not valid ->
                Diagnostics.error(
                  :pagination,
                  :invalid_layout,
                  "CSS stacking requires bounded source-order paths and integer stack levels; recompute layout",
                  source: "CSS stacking",
                  module: __MODULE__
                )

              Map.has_key?(box, :boxes) ->
                validate_boxes(box.boxes)

              true ->
                :ok
            end

          case result do
            :ok -> {:cont, :ok}
            error -> {:halt, error}
          end
      end
    end)
  end
end
