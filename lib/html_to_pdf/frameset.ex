defmodule NativeElixirPdfUtilities.HtmlToPdf.Frameset do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.EmbeddedViewport
  alias NativeElixirPdfUtilities.Validators.{FramesetValidator, HtmlValidator}

  @doc "Composes a prepared recursive frameset grid from real isolated child viewports."
  @spec layout(term(), term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def layout(node, width, height) do
    HtmlValidator.with_render_budget(fn -> layout_grid(node, width, height, 1) end)
  catch
    {:render_resource_limit, error} -> error
  end

  defp layout_grid(node, width, height, depth) do
    with {:ok, grid} <- FramesetValidator.prepare_scene(node, width, height, depth) do
      columns = List.to_tuple(grid.columns)
      rows = List.to_tuple(grid.rows)
      column_count = tuple_size(columns)
      cells = column_count * tuple_size(rows)

      {column_offsets, _} =
        Enum.map_reduce(grid.columns, 0.0, fn value, offset ->
          {offset, offset + value + grid.gap}
        end)

      {row_offsets, _} =
        Enum.map_reduce(grid.rows, 0.0, fn value, offset ->
          {offset, offset + value + grid.gap}
        end)

      column_offsets = List.to_tuple(column_offsets)
      row_offsets = List.to_tuple(row_offsets)

      node.children
      |> Enum.with_index()
      |> Enum.reduce_while({:ok, []}, fn {child, index}, {:ok, boxes} ->
        column = rem(index, column_count)
        row = div(index, column_count)
        cell_width = if(index < cells, do: elem(columns, column), else: 0.0)
        cell_height = if(index < cells, do: elem(rows, row), else: 0.0)

        result =
          cond do
            Map.has_key?(child.style, :_frameset) ->
              layout_grid(child, cell_width, cell_height, depth + 1)

            true ->
              EmbeddedViewport.layout(child.style.embedded_document, cell_width, cell_height)
          end

        case result do
          {:ok, group} ->
            group =
              if(child.tag == "frame", do: Map.put(group, :role, :frameset_viewport), else: group)

            x = if(index < cells, do: elem(column_offsets, column), else: 0.0)
            y = if(index < cells, do: height - elem(row_offsets, row) - cell_height, else: 0.0)

            group =
              group |> Map.put(:x, x) |> Map.put(:y, y) |> Map.put(:transform, {1, 0, 0, 1, x, y})

            # A zero-area cell still ran its complete child validation/layout,
            # but has no paint or representable link destinations to transport.
            {:cont,
             {:ok, if(cell_width > 0 and cell_height > 0, do: [group | boxes], else: boxes)}}

          error ->
            {:halt, error}
        end
      end)
      |> case do
        {:ok, boxes} ->
          borders =
            Enum.map(grid.borders, fn part ->
              Map.merge(part, %{
                type: HtmlValidator.reserve_layout_box(:rect),
                stroke_color: nil,
                stroke_width: 0.0,
                border_radius: 0.0,
                role: :frameset_border,
                snap_to_css_pixel_grid: true
              })
            end)

          {:ok,
           %{
             type: HtmlValidator.reserve_layout_box(:drawing_group),
             x: 0.0,
             y: 0.0,
             width: width,
             height: height,
             transform: {1, 0, 0, 1, 0, 0},
             clip: %{x: 0.0, y: 0.0, width: width, height: height},
             boxes: Enum.reverse(boxes) ++ borders
           }}

        error ->
          error
      end
    end
  end
end
