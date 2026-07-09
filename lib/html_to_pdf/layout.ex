defmodule NativeElixirPdfUtilities.HtmlToPdf.Layout do
  @moduledoc """
  Layout engine for the native HTML-to-PDF renderer.

  Lays out block text elements, inline text runs, box styling, lists, link
  annotation bounds, deterministic tables, a documented flexbox/grid subset,
  image boxes, embedded-font text metrics, and pagination metadata.
  """

  alias NativeElixirPdfUtilities.HtmlToPdf.Font

  @type box :: map()
  @type layout_tree :: %{
          type: :layout,
          page_size: term(),
          margin: term(),
          boxes: [box()]
        }
  @type render_option :: NativeElixirPdfUtilities.HtmlToPdf.render_option()

  @doc """
  Converts a styled document tree into a layout tree.
  """
  @spec layout(term(), term()) ::
          {:ok, layout_tree()} | {:error, :invalid_layout | :invalid_margin | :invalid_page_size}
  def layout(styled_tree, opts \\ []) do
    case {styled_tree, opts} do
      {%{type: :document, children: children}, opts} when is_list(children) and is_list(opts) ->
        with {:ok, page_size} <- page_size(Keyword.get(opts, :page_size, :a4)),
             {:ok, margin} <- margin(Keyword.get(opts, :margin, 0)),
             {:ok, boxes} <- layout_blocks(children, page_size, margin) do
          {page_width, page_height} = page_size

          {:ok,
           %{
             type: :layout,
             page_size: page_size,
             margin: margin,
             boxes: boxes,
             content_width: page_width - margin * 2,
             content_height: page_height - margin * 2
           }}
        end

      _ ->
        {:error, :invalid_layout}
    end
  end

  @spec layout_blocks([term()], {number(), number()}, number()) ::
          {:ok, [box()]} | {:error, :invalid_layout}
  defp layout_blocks(children, page_size, margin) do
    {page_width, page_height} = page_size

    result =
      Enum.reduce(children, {:ok, [], page_height - margin}, fn child, acc ->
        case acc do
          {:ok, boxes, y} ->
            case layout_block(child, margin, y, page_width - margin * 2) do
              {:ok, block_boxes, next_y} -> {:ok, boxes ++ block_boxes, next_y}
              {:error, reason} -> {:error, reason}
            end

          {:error, reason} ->
            {:error, reason}
        end
      end)

    case result do
      {:ok, boxes, _y} -> {:ok, boxes}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec layout_block(term(), number(), number(), number()) ::
          {:ok, [box()], number()} | {:error, :invalid_layout}
  defp layout_block(block, x, y, width) do
    case block do
      %{type: :element, style: %{display: :none}} ->
        {:ok, [], y}

      %{type: :element, style: %{display: :block} = style, children: children}
      when is_list(children) ->
        with line_height when is_number(line_height) <- Map.get(style, :line_height),
             font_size when is_number(font_size) <- Map.get(style, :font_size) do
          margin =
            Map.get(style, :margin, edges(0.0, 0.0, Map.get(style, :margin_after, 0.0), 0.0))

          padding = Map.get(style, :padding, edges(0.0))
          border_widths = Map.get(style, :border_widths, edges(0.0))
          box_x = x + margin.left
          box_top = y - margin.top
          available_box_width = width - margin.left - margin.right

          content_width =
            resolved_content_size(
              style,
              :width,
              width_available_size(style, available_box_width),
              available_box_width - horizontal_box_size(style)
            )

          box_width =
            content_width + border_widths.left + padding.left + padding.right +
              border_widths.right

          content_x = box_x + border_widths.left + padding.left

          content_top = box_top - border_widths.top - padding.top

          flow_metadata =
            style
            |> break_metadata()
            |> Map.put(:flow_id, {:block, box_x, box_top})

          case layout_block_content(
                 children,
                 style,
                 content_x,
                 content_top,
                 content_width,
                 flow_metadata
               ) do
            {:ok, content_boxes, content_height} ->
              content_box_height = resolved_content_size(style, :height, nil, content_height)

              box_height =
                border_widths.top + padding.top + content_box_height + padding.bottom +
                  border_widths.bottom

              background_box =
                style
                |> background_box(box_x, box_top - box_height, box_width, box_height)
                |> tag_boxes(flow_metadata)

              next_y = box_top - box_height - margin.bottom

              page_break_box =
                %{
                  type: :page_break,
                  x: box_x,
                  y: box_top,
                  width: box_width,
                  height: 0.0
                }
                |> Map.merge(Map.put(flow_metadata, :flow_id, {:page_break, box_x, box_top}))

              boxes =
                case {background_box ++ content_boxes, flow_metadata} do
                  {[], %{break_before: :page}} ->
                    [page_break_box]

                  {[], %{break_after: :page}} ->
                    [page_break_box]

                  {boxes, _metadata} ->
                    boxes
                end

              {:ok, boxes, next_y}

            {:error, reason} ->
              {:error, reason}
          end
        else
          _ -> {:error, :invalid_layout}
        end

      %{type: :element, style: %{display: :image} = style} ->
        layout_image(style, x, y, width)

      %{type: :element, style: %{display: :list} = style, children: children}
      when is_list(children) ->
        layout_list(style, children, x, y, width)

      %{type: :element, style: %{display: :table} = style, children: children}
      when is_list(children) ->
        layout_table(style, children, x, y, width)

      %{type: :element, style: %{display: display} = style, children: children}
      when display in [:flex, :inline_flex] and is_list(children) ->
        layout_flex(style, children, x, y, width)

      %{type: :element, style: %{display: display} = style, children: children}
      when display in [:grid, :inline_grid] and is_list(children) ->
        layout_grid(style, children, x, y, width)

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp layout_block_content(children, style, x, y, width, metadata) do
    case inline_runs(children) do
      {:ok, runs} ->
        {boxes, content_height} = inline_text_boxes(runs, style, x, y, width, metadata)
        {:ok, boxes, content_height}

      {:error, _reason} ->
        with {:ok, boxes, next_y} <- layout_block_flow(children, style, x, y, width) do
          {:ok, boxes, y - next_y}
        end
    end
  end

  defp layout_block_flow(children, style, x, y, width) do
    result =
      Enum.reduce_while(children, {:ok, [], y}, fn child, {:ok, boxes, current_y} ->
        case layout_flow_child(child, style, x, current_y, width) do
          {:ok, child_boxes, next_y} -> {:cont, {:ok, boxes ++ child_boxes, next_y}}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)

    case result do
      {:ok, boxes, next_y} -> {:ok, boxes, next_y}
      {:error, reason} -> {:error, reason}
    end
  end

  defp layout_flow_child(child, style, x, y, width) do
    case child do
      %{type: :text, text: text} when is_binary(text) ->
        case String.trim(text) do
          "" ->
            {:ok, [], y}

          _ ->
            with {:ok, runs} <- inline_runs([child]) do
              {boxes, content_height} = inline_text_boxes(runs, style, x, y, width, %{})
              {:ok, boxes, y - content_height}
            end
        end

      %{type: :element, style: %{display: display}} when display in [:inline, :line_break] ->
        with {:ok, runs} <- inline_runs([child]) do
          {boxes, content_height} = inline_text_boxes(runs, style, x, y, width, %{})
          {:ok, boxes, y - content_height}
        end

      _ ->
        layout_block(child, x, y, width)
    end
  end

  defp layout_image(style, x, y, width) do
    margin = Map.get(style, :margin, edges(0.0, 0.0, Map.get(style, :margin_after, 0.0), 0.0))
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))
    available_content_width = width - margin.left - margin.right - horizontal_box_size(style)
    {content_width, content_height} = image_content_size(style, available_content_width, nil)
    box_x = x + margin.left
    box_top = y - margin.top

    box_width =
      content_width + border_widths.left + padding.left + padding.right + border_widths.right

    box_height =
      border_widths.top + padding.top + content_height + padding.bottom + border_widths.bottom

    flow_metadata =
      style
      |> break_metadata()
      |> Map.put(:flow_id, {:image, box_x, box_top})

    background_box =
      style
      |> background_box(box_x, box_top - box_height, box_width, box_height)
      |> tag_boxes(flow_metadata)

    image_box =
      %{
        type: :image,
        x: box_x + border_widths.left + padding.left,
        y: box_top - border_widths.top - padding.top - content_height,
        width: content_width,
        height: content_height,
        image: Map.fetch!(style, :image)
      }
      |> Map.merge(flow_metadata)

    next_y = box_top - box_height - margin.bottom
    {:ok, background_box ++ [image_box], next_y}
  end

  defp image_content_size(style) do
    image_content_size(style, nil, nil)
  end

  defp image_content_size(style, available_width, available_height) do
    image = Map.fetch!(style, :image)
    natural_width = Map.fetch!(image, :width)
    natural_height = Map.fetch!(image, :height)
    ratio = Map.get(style, :aspect_ratio, natural_width / natural_height)
    width = resolved_content_size(style, :width, available_width, nil)
    height = resolved_content_size(style, :height, available_height, nil)

    {content_width, content_height} =
      case {width, height} do
        {width, height} when is_number(width) and is_number(height) ->
          {width, height}

        {width, _height} when is_number(width) ->
          {width, width / ratio}

        {_width, height} when is_number(height) ->
          {height * ratio, height}

        _ ->
          {natural_width, natural_height}
      end

    apply_image_size_constraints(
      style,
      content_width,
      content_height,
      ratio,
      available_width,
      available_height,
      is_number(width) and is_number(height)
    )
  end

  defp layout_grid(style, children, x, y, width) do
    margin = Map.get(style, :margin, edges(0.0, 0.0, Map.get(style, :margin_after, 0.0), 0.0))
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))
    available_box_width = width - margin.left - margin.right

    content_width =
      resolved_content_size(
        style,
        :width,
        width_available_size(style, available_box_width),
        available_box_width - horizontal_box_size(style)
      )

    box_width =
      content_width + border_widths.left + padding.left + padding.right + border_widths.right

    box_x = x + margin.left
    box_top = y - margin.top
    content_x = box_x + border_widths.left + padding.left
    content_top = box_top - border_widths.top - padding.top

    with {:ok, items} <- grid_items(children) do
      placed_items = place_grid_items(items, style)
      column_count = grid_axis_count(placed_items, style, :column)
      row_count = grid_axis_count(placed_items, style, :row)
      column_tracks = grid_tracks(style, :column, column_count)
      row_tracks = grid_tracks(style, :row, row_count)
      column_intrinsics = grid_column_intrinsics(placed_items, column_count)

      column_sizes =
        resolve_grid_columns(
          column_tracks,
          column_intrinsics,
          content_width,
          grid_column_gap(style)
        )

      placed_items =
        grid_items_with_resolved_heights(
          placed_items,
          style,
          column_sizes,
          grid_column_gap(style)
        )

      row_intrinsics = grid_row_intrinsics(placed_items, row_count)
      available_height = resolved_content_size(style, :height, nil, nil)

      row_sizes =
        resolve_grid_rows(
          row_tracks,
          row_intrinsics,
          grid_row_gap(style),
          available_height,
          Map.get(style, :align_content, :stretch)
        )

      content_height = available_height || grid_tracks_size(row_sizes, grid_row_gap(style))

      flow_metadata =
        style
        |> break_metadata()
        |> Map.put(:flow_id, {:grid, box_x, box_top})

      box_height =
        border_widths.top + padding.top + content_height + padding.bottom +
          border_widths.bottom

      background_box =
        style
        |> background_box(box_x, box_top - box_height, box_width, box_height)
        |> tag_boxes(flow_metadata)

      with {:ok, item_boxes} <-
             grid_item_boxes(
               placed_items,
               style,
               content_x,
               content_top,
               content_width,
               content_height,
               column_sizes,
               row_sizes,
               flow_metadata.flow_id
             ) do
        next_y = box_top - box_height - margin.bottom
        {:ok, background_box ++ item_boxes, next_y}
      end
    end
  end

  defp grid_items(children) do
    children
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn {child, index}, {:ok, acc} ->
      case grid_item(child, index) do
        {:ok, nil} -> {:cont, {:ok, acc}}
        {:ok, item} -> {:cont, {:ok, acc ++ [item]}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp grid_item(child, index) do
    case child do
      %{type: :text, text: text} when is_binary(text) ->
        case String.trim(text) do
          "" ->
            {:ok, nil}

          _ ->
            style = child.style |> text_style() |> Map.put(:display, :inline)
            {:ok, build_grid_item(style, [%{text: text, style: child.style}], index)}
        end

      %{type: :element, style: %{display: :none}} ->
        {:ok, nil}

      %{type: :element, style: %{display: :image} = style} ->
        {:ok, build_grid_image_item(style, index)}

      %{type: :element, style: style, children: children} when is_list(children) ->
        case inline_runs(children) do
          {:ok, runs} -> {:ok, build_grid_item(style, runs, index)}
          {:error, _reason} -> build_grid_block_item(style, children, index)
        end

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp build_grid_item(style, runs, index) do
    margin = Map.get(style, :margin, edges(0.0))
    text_width = Enum.reduce(runs, 0.0, fn run, acc -> acc + text_width(run.text, run.style) end)
    line_height = Map.get(style, :line_height, 14.4)

    content_width = resolved_content_size(style, :width, nil, text_width)
    content_height = resolved_content_size(style, :height, nil, line_height)

    %{
      index: index,
      style: style,
      runs: runs,
      intrinsic_width: content_width + horizontal_box_size(style),
      intrinsic_height: content_height + vertical_box_size(style),
      margin: margin,
      row_start: Map.get(style, :grid_row_start, :auto),
      row_end: Map.get(style, :grid_row_end, :auto),
      column_start: Map.get(style, :grid_column_start, :auto),
      column_end: Map.get(style, :grid_column_end, :auto)
    }
  end

  defp build_grid_block_item(style, children, index) do
    margin = Map.get(style, :margin, edges(0.0))
    intrinsic_width = flex_block_intrinsic_width(children)
    content_width = resolved_content_size(style, :width, nil, intrinsic_width)

    case layout_container_content_height(style, children, max(content_width, 0.0)) do
      {:ok, content_height} ->
        content_height = resolved_content_size(style, :height, nil, content_height)

        {:ok,
         %{
           index: index,
           style: style,
           children: children,
           intrinsic_width: content_width + horizontal_box_size(style),
           intrinsic_height: content_height + vertical_box_size(style),
           margin: margin,
           row_start: Map.get(style, :grid_row_start, :auto),
           row_end: Map.get(style, :grid_row_end, :auto),
           column_start: Map.get(style, :grid_column_start, :auto),
           column_end: Map.get(style, :grid_column_end, :auto)
         }}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp build_grid_image_item(style, index) do
    margin = Map.get(style, :margin, edges(0.0))
    {content_width, content_height} = image_content_size(style)

    %{
      index: index,
      style: style,
      image: Map.fetch!(style, :image),
      intrinsic_width: content_width + horizontal_box_size(style),
      intrinsic_height: content_height + vertical_box_size(style),
      margin: margin,
      row_start: Map.get(style, :grid_row_start, :auto),
      row_end: Map.get(style, :grid_row_end, :auto),
      column_start: Map.get(style, :grid_column_start, :auto),
      column_end: Map.get(style, :grid_column_end, :auto)
    }
  end

  defp place_grid_items(items, style) do
    column_count = style |> Map.get(:grid_template_columns, []) |> length() |> max(1)

    {_occupied, placed} =
      Enum.reduce(items, {MapSet.new(), []}, fn item, {occupied, acc} ->
        placement = grid_item_placement(item, column_count, occupied)

        occupied =
          placement.row_start..(placement.row_end - 1)
          |> Enum.reduce(occupied, fn row, row_acc ->
            placement.column_start..(placement.column_end - 1)
            |> Enum.reduce(row_acc, &MapSet.put(&2, {row, &1}))
          end)

        {occupied, acc ++ [Map.merge(item, placement)]}
      end)

    placed
  end

  defp grid_item_placement(item, column_count, occupied) do
    column_start = grid_line_start(item.column_start)
    row_start = grid_line_start(item.row_start)
    column_span = grid_axis_span(item.column_start, item.column_end)
    row_span = grid_axis_span(item.row_start, item.row_end)

    case {row_start, column_start} do
      {row_start, column_start} when is_integer(row_start) and is_integer(column_start) ->
        grid_placement(row_start, column_start, row_span, column_span)

      {row_start, :auto} when is_integer(row_start) ->
        column_start = first_free_grid_column(occupied, row_start, column_count, column_span)
        grid_placement(row_start, column_start, row_span, column_span)

      {:auto, column_start} when is_integer(column_start) ->
        row_start = first_free_grid_row(occupied, column_start, row_span, column_span)
        grid_placement(row_start, column_start, row_span, column_span)

      _ ->
        {row_start, column_start} =
          first_free_grid_cell(occupied, column_count, row_span, column_span)

        grid_placement(row_start, column_start, row_span, column_span)
    end
  end

  defp grid_placement(row_start, column_start, row_span, column_span) do
    %{
      row_start: row_start,
      row_end: row_start + row_span,
      column_start: column_start,
      column_end: column_start + column_span
    }
  end

  defp grid_line_start(line) do
    case line do
      line when is_integer(line) -> line
      _ -> :auto
    end
  end

  defp grid_axis_span(start_line, end_line) do
    case {start_line, end_line} do
      {_start_line, {:span, span}} ->
        span

      {start_line, end_line} when is_integer(start_line) and is_integer(end_line) ->
        max(end_line - start_line, 1)

      {{:span, span}, _end_line} ->
        span

      _ ->
        1
    end
  end

  defp first_free_grid_column(occupied, row_start, column_count, column_span) do
    1..column_count
    |> Enum.find(&grid_cells_free?(occupied, row_start, &1, 1, column_span))
    |> case do
      nil -> column_count + 1
      column -> column
    end
  end

  defp first_free_grid_row(occupied, column_start, row_span, column_span) do
    1
    |> Stream.iterate(&(&1 + 1))
    |> Enum.find(&grid_cells_free?(occupied, &1, column_start, row_span, column_span))
  end

  defp first_free_grid_cell(occupied, column_count, row_span, column_span) do
    1
    |> Stream.iterate(&(&1 + 1))
    |> Enum.reduce_while(nil, fn row, _acc ->
      column =
        1..column_count
        |> Enum.find(&grid_cells_free?(occupied, row, &1, row_span, column_span))

      case column do
        nil -> {:cont, nil}
        column -> {:halt, {row, column}}
      end
    end)
  end

  defp grid_cells_free?(occupied, row_start, column_start, row_span, column_span) do
    row_start..(row_start + row_span - 1)
    |> Enum.all?(fn row ->
      column_start..(column_start + column_span - 1)
      |> Enum.all?(&(!MapSet.member?(occupied, {row, &1})))
    end)
  end

  defp grid_axis_count(items, style, axis) do
    explicit_count =
      case axis do
        :column -> style |> Map.get(:grid_template_columns, []) |> length()
        :row -> style |> Map.get(:grid_template_rows, []) |> length()
      end

    item_count =
      items
      |> Enum.map(fn item ->
        case axis do
          :column -> item.column_end - 1
          :row -> item.row_end - 1
        end
      end)
      |> Enum.max(fn -> 0 end)

    max(max(explicit_count, item_count), 1)
  end

  defp grid_tracks(style, axis, count) do
    {template, auto_track} =
      case axis do
        :column ->
          {Map.get(style, :grid_template_columns, []), Map.get(style, :grid_auto_columns, :auto)}

        :row ->
          {Map.get(style, :grid_template_rows, []), Map.get(style, :grid_auto_rows, :auto)}
      end

    missing = max(count - length(template), 0)
    template ++ List.duplicate(auto_track, missing)
  end

  defp grid_column_intrinsics(items, column_count) do
    1..column_count
    |> Enum.map(fn column ->
      items
      |> Enum.filter(&(&1.column_start == column and &1.column_end == column + 1))
      |> Enum.map(&(&1.intrinsic_width + &1.margin.left + &1.margin.right))
      |> Enum.max(fn -> 0.0 end)
    end)
  end

  defp resolve_grid_columns(tracks, column_intrinsics, available_size, gap) do
    gap_total = gap * max(length(tracks) - 1, 0)

    fixed_size =
      tracks
      |> Enum.with_index()
      |> Enum.reduce(0.0, fn {track, index}, acc ->
        case track do
          {:length, length} -> acc + length
          :auto -> acc + Enum.at(column_intrinsics, index, 0.0)
          {:fr, _fraction} -> acc
        end
      end)

    total_fraction =
      Enum.reduce(tracks, 0.0, fn track, acc ->
        case track do
          {:fr, fraction} -> acc + fraction
          _ -> acc
        end
      end)

    remaining = max(available_size - fixed_size - gap_total, 0.0)

    tracks
    |> Enum.with_index()
    |> Enum.map(fn {track, index} ->
      case track do
        {:length, length} -> length
        {:fr, fraction} when total_fraction > 0 -> remaining * fraction / total_fraction
        {:fr, _fraction} -> 0.0
        :auto -> Enum.at(column_intrinsics, index, 0.0)
      end
    end)
  end

  defp grid_row_intrinsics(items, row_count) do
    1..row_count
    |> Enum.map(fn row ->
      items
      |> Enum.filter(&(&1.row_start == row and &1.row_end == row + 1))
      |> Enum.map(&(&1.intrinsic_height + &1.margin.top + &1.margin.bottom))
      |> Enum.max(fn -> 0.0 end)
    end)
  end

  defp grid_items_with_resolved_heights(items, container_style, column_sizes, column_gap) do
    Enum.map(
      items,
      &grid_item_with_resolved_height(&1, container_style, column_sizes, column_gap)
    )
  end

  defp grid_item_with_resolved_height(item, container_style, column_sizes, column_gap) do
    area_width = grid_track_span(column_sizes, column_gap, item.column_start, item.column_end)
    justify = grid_item_justify(item, container_style)
    box_width = grid_aligned_box_size(item, :width, justify, area_width)
    content_width = max(box_width - horizontal_box_size(item.style), 0.0)

    content_height = grid_item_content_height(item, content_width)
    Map.put(item, :intrinsic_height, content_height + vertical_box_size(item.style))
  end

  defp grid_item_content_height(item, content_width) do
    cond do
      Map.has_key?(item, :runs) ->
        line_height = Map.get(item.style, :line_height, 14.4)

        resolved_content_size(
          item.style,
          :height,
          nil,
          inline_content_height(item.runs, content_width, line_height)
        )

      Map.has_key?(item, :children) ->
        {:ok, content_height} =
          layout_container_content_height(item.style, item.children, content_width)

        resolved_content_size(item.style, :height, nil, content_height)

      Map.has_key?(item, :image) ->
        {_content_width, content_height} = image_content_size(item.style, content_width, nil)
        content_height
    end
  end

  defp resolve_grid_rows(tracks, row_intrinsics, gap, available_height, align_content) do
    fraction_total =
      Enum.reduce(tracks, 0.0, fn track, acc ->
        case track do
          {:fr, fraction} -> acc + fraction
          _ -> acc
        end
      end)

    fixed_size =
      tracks
      |> Enum.with_index()
      |> Enum.reduce(0.0, fn {track, index}, acc ->
        intrinsic = Enum.at(row_intrinsics, index, 0.0)

        case track do
          {:length, length} -> acc + length
          :auto -> acc + intrinsic
          {:fr, _fraction} -> acc
        end
      end)

    gap_total = gap * max(length(tracks) - 1, 0)
    remaining = max((available_height || fixed_size) - fixed_size - gap_total, 0.0)

    row_sizes =
      tracks
      |> Enum.with_index()
      |> Enum.map(fn {track, index} ->
        intrinsic = Enum.at(row_intrinsics, index, 0.0)

        case track do
          {:length, length} ->
            length

          :auto ->
            intrinsic

          {:fr, fraction} when fraction_total > 0 ->
            max(intrinsic, remaining * fraction / fraction_total)

          {:fr, _fraction} ->
            intrinsic
        end
      end)

    stretch_grid_rows(row_sizes, tracks, gap, available_height, align_content)
  end

  defp stretch_grid_rows(row_sizes, tracks, gap, available_height, align_content) do
    case {align_content, available_height} do
      {:stretch, available_height} when is_number(available_height) ->
        stretchable_indexes =
          tracks
          |> Enum.with_index()
          |> Enum.flat_map(fn
            {:auto, index} -> [index]
            {_track, _index} -> []
          end)

        free_space = max(available_height - grid_tracks_size(row_sizes, gap), 0.0)

        cond do
          stretchable_indexes == [] ->
            row_sizes

          free_space == 0.0 ->
            row_sizes

          true ->
            extra = free_space / length(stretchable_indexes)

            Enum.with_index(row_sizes, fn size, index ->
              case index in stretchable_indexes do
                true -> size + extra
                false -> size
              end
            end)
        end

      _ ->
        row_sizes
    end
  end

  defp grid_tracks_size(sizes, gap) do
    Enum.sum(sizes) + gap * max(length(sizes) - 1, 0)
  end

  defp grid_item_boxes(
         items,
         style,
         x,
         y,
         content_width,
         content_height,
         column_sizes,
         row_sizes,
         container_flow_id
       ) do
    column_gap = grid_column_gap(style)
    row_gap = grid_row_gap(style)
    grid_width = grid_tracks_size(column_sizes, column_gap)
    grid_height = grid_tracks_size(row_sizes, row_gap)

    {content_x, content_gap} =
      grid_content_distribution(
        style,
        :justify_content,
        content_width,
        grid_width,
        column_gap,
        length(column_sizes)
      )

    {content_y, row_gap} =
      grid_content_distribution(
        style,
        :align_content,
        content_height,
        grid_height,
        row_gap,
        length(row_sizes)
      )

    Enum.reduce_while(items, {:ok, []}, fn item, {:ok, acc} ->
      item =
        position_grid_item(
          item,
          style,
          x + content_x,
          y - content_y,
          column_sizes,
          row_sizes,
          content_gap,
          row_gap
        )

      {:ok, boxes} = flex_item_boxes(item, :row)
      tagged_boxes = tag_boxes(boxes, %{flow_id: container_flow_id})
      {:cont, {:ok, acc ++ tagged_boxes}}
    end)
  end

  defp position_grid_item(
         item,
         container_style,
         x,
         y,
         column_sizes,
         row_sizes,
         column_gap,
         row_gap
       ) do
    area_x = x + grid_track_offset(column_sizes, column_gap, item.column_start)
    area_y = y - grid_track_offset(row_sizes, row_gap, item.row_start)
    area_width = grid_track_span(column_sizes, column_gap, item.column_start, item.column_end)
    area_height = grid_track_span(row_sizes, row_gap, item.row_start, item.row_end)
    justify = grid_item_justify(item, container_style)
    align = grid_item_align(item, container_style)
    box_width = grid_aligned_box_size(item, :width, justify, area_width)
    box_height = grid_aligned_box_size(item, :height, align, area_height)
    x_offset = grid_axis_position(justify, box_width, area_width)
    y_offset = grid_axis_position(align, box_height, area_height)

    Map.merge(item, %{
      x: area_x + x_offset + item.margin.left,
      y: area_y - y_offset - item.margin.top,
      box_width: box_width,
      box_height: box_height
    })
  end

  defp grid_content_distribution(style, property, container_size, grid_size, gap, count) do
    free_space = max(container_size - grid_size, 0.0)

    case Map.get(style, property, :flex_start) do
      :flex_end ->
        {free_space, gap}

      :center ->
        {free_space / 2, gap}

      :space_between when count > 1 ->
        {0.0, gap + free_space / (count - 1)}

      :space_around when count > 0 ->
        distributed_gap = gap + free_space / count
        {distributed_gap / 2, distributed_gap}

      :space_evenly when count > 0 ->
        distributed_gap = gap + free_space / (count + 1)
        {distributed_gap, distributed_gap}

      _ ->
        {0.0, gap}
    end
  end

  defp grid_track_offset(sizes, gap, start_line) do
    sizes
    |> Enum.take(start_line - 1)
    |> Enum.sum()
    |> Kernel.+(gap * max(start_line - 1, 0))
  end

  defp grid_track_span(sizes, gap, start_line, end_line) do
    span_count = max(end_line - start_line, 1)

    sizes
    |> Enum.drop(start_line - 1)
    |> Enum.take(span_count)
    |> Enum.sum()
    |> Kernel.+(gap * max(span_count - 1, 0))
  end

  defp grid_aligned_box_size(item, axis, align, area_size) do
    margin =
      case axis do
        :width -> item.margin.left + item.margin.right
        :height -> item.margin.top + item.margin.bottom
      end

    intrinsic =
      case axis do
        :width -> item.intrinsic_width
        :height -> item.intrinsic_height
      end

    case align do
      :stretch -> max(area_size - margin, 0.0)
      _ -> min(intrinsic, max(area_size - margin, 0.0))
    end
  end

  defp grid_axis_position(align, box_size, area_size) do
    case align do
      :flex_end -> max(area_size - box_size, 0.0)
      :center -> max((area_size - box_size) / 2, 0.0)
      _ -> 0.0
    end
  end

  defp grid_item_align(item, container_style) do
    case Map.get(item.style, :align_self, :auto) do
      :auto -> Map.get(container_style, :align_items, :stretch)
      align -> align
    end
  end

  defp grid_item_justify(item, container_style) do
    case Map.get(item.style, :justify_self, :auto) do
      :auto -> Map.get(container_style, :justify_items, :stretch)
      justify -> justify
    end
  end

  defp grid_column_gap(style) do
    Map.get(style, :column_gap, 0.0)
  end

  defp grid_row_gap(style) do
    Map.get(style, :row_gap, 0.0)
  end

  defp layout_flex(style, children, x, y, width) do
    margin = Map.get(style, :margin, edges(0.0, 0.0, Map.get(style, :margin_after, 0.0), 0.0))
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))
    available_box_width = width - margin.left - margin.right

    content_width =
      resolved_content_size(
        style,
        :width,
        width_available_size(style, available_box_width),
        available_box_width - horizontal_box_size(style)
      )

    box_width =
      content_width + border_widths.left + padding.left + padding.right + border_widths.right

    box_x = x + margin.left
    box_top = y - margin.top
    content_x = box_x + border_widths.left + padding.left
    content_top = box_top - border_widths.top - padding.top

    main_axis = flex_main_axis(style)

    with {:ok, items} <-
           flex_items(
             children,
             main_axis,
             content_width,
             flex_available_cross(main_axis, content_width)
           ) do
      case items do
        [] ->
          box_height = border_widths.top + padding.top + padding.bottom + border_widths.bottom

          background_box =
            background_box(style, box_x, box_top - box_height, box_width, box_height)

          {:ok, tag_boxes(background_box, break_metadata(style)),
           box_top - box_height - margin.bottom}

        items ->
          lines = flex_lines(items, style, content_width)
          content_height = flex_content_height(lines, style)

          box_height =
            border_widths.top + padding.top + content_height + padding.bottom +
              border_widths.bottom

          flow_metadata =
            style
            |> break_metadata()
            |> Map.put(:flow_id, {:flex, box_x, box_top})

          background_box =
            style
            |> background_box(box_x, box_top - box_height, box_width, box_height)
            |> tag_boxes(flow_metadata)

          with {:ok, item_boxes} <-
                 flex_line_boxes(
                   lines,
                   style,
                   content_x,
                   content_top,
                   content_width,
                   content_height,
                   flow_metadata.flow_id
                 ) do
            next_y = box_top - box_height - margin.bottom
            {:ok, background_box ++ item_boxes, next_y}
          end
      end
    end
  end

  defp flex_items(children, main_axis, available_main, available_cross) do
    children
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn {child, index}, {:ok, acc} ->
      case flex_item(child, index, main_axis, available_main, available_cross) do
        {:ok, nil} -> {:cont, {:ok, acc}}
        {:ok, item} -> {:cont, {:ok, acc ++ [item]}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
    |> case do
      {:ok, items} -> {:ok, Enum.sort_by(items, &{&1.order, &1.index})}
      {:error, reason} -> {:error, reason}
    end
  end

  defp flex_item(child, index, main_axis, available_main, available_cross) do
    case child do
      %{type: :text, text: text} when is_binary(text) ->
        case text |> collapse_inline_whitespace() |> String.trim() do
          "" ->
            {:ok, nil}

          text ->
            style = child.style |> text_style() |> Map.put(:display, :inline)
            runs = [%{text: text, style: child.style}]
            {:ok, build_flex_item(style, runs, index, main_axis, available_main, available_cross)}
        end

      %{type: :element, style: %{display: :none}} ->
        {:ok, nil}

      %{type: :element, style: %{display: :line_break}} ->
        {:ok, nil}

      %{type: :element, style: %{display: :image} = style} ->
        {:ok, build_flex_image_item(style, index, main_axis, available_main, available_cross)}

      %{type: :element, style: style, children: children} when is_list(children) ->
        case inline_runs(children) do
          {:ok, runs} ->
            {:ok, build_flex_item(style, runs, index, main_axis, available_main, available_cross)}

          {:error, _reason} ->
            build_flex_block_item(
              style,
              children,
              index,
              main_axis,
              available_main,
              available_cross
            )
        end

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp build_flex_item(style, runs, index, main_axis, available_main, available_cross) do
    margin = Map.get(style, :margin, edges(0.0))
    text_width = Enum.reduce(runs, 0.0, fn run, acc -> acc + text_width(run.text, run.style) end)
    line_height = Map.get(style, :line_height, 14.4)

    {content_main, content_cross} =
      case main_axis do
        :row ->
          content_main = flex_basis(style, :width, available_main, text_width)
          content_height = inline_content_height(runs, content_main, line_height)
          {content_main, resolved_content_size(style, :height, nil, content_height)}

        :column ->
          {flex_basis(style, :height, nil, line_height),
           resolved_content_size(style, :width, available_cross, text_width)}
      end

    main_box = content_main + flex_main_box_size(style, main_axis)
    cross_box = content_cross + flex_cross_box_size(style, main_axis)

    %{
      index: index,
      style: style,
      runs: runs,
      order: Map.get(style, :order, 0),
      flex_grow: Map.get(style, :flex_grow, 0.0),
      flex_shrink: Map.get(style, :flex_shrink, 1.0),
      main_axis: main_axis,
      main_box: main_box,
      cross_box: cross_box,
      outer_main: main_box + flex_main_margin_size(margin, main_axis),
      outer_cross: cross_box + flex_cross_margin_size(margin, main_axis),
      margin: margin
    }
  end

  defp build_flex_block_item(style, children, index, main_axis, available_main, available_cross) do
    margin = Map.get(style, :margin, edges(0.0))
    intrinsic_width = flex_block_intrinsic_width(children)

    content_width =
      case main_axis do
        :row ->
          flex_basis(style, :width, available_main, min(intrinsic_width, available_main))

        :column ->
          default_width = max(available_cross - flex_cross_box_size(style, :column), 0.0)

          resolved_content_size(style, :width, available_cross, default_width)
      end

    case layout_container_content_height(style, children, max(content_width, 0.0)) do
      {:ok, content_height} ->
        {content_main, content_cross} =
          case main_axis do
            :row ->
              {content_width, resolved_content_size(style, :height, nil, content_height)}

            :column ->
              {flex_basis(style, :height, nil, content_height), content_width}
          end

        main_box = content_main + flex_main_box_size(style, main_axis)
        cross_box = content_cross + flex_cross_box_size(style, main_axis)

        {:ok,
         %{
           index: index,
           style: style,
           children: children,
           order: Map.get(style, :order, 0),
           flex_grow: Map.get(style, :flex_grow, 0.0),
           flex_shrink: Map.get(style, :flex_shrink, 1.0),
           main_axis: main_axis,
           main_box: main_box,
           cross_box: cross_box,
           outer_main: main_box + flex_main_margin_size(margin, main_axis),
           outer_cross: cross_box + flex_cross_margin_size(margin, main_axis),
           margin: margin
         }}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp layout_container_content_height(style, children, content_width) do
    case Map.get(style, :display) do
      :table ->
        nested_style = Map.put(style, :margin, edges(0.0))
        available_width = content_width + horizontal_box_size(style)

        with {:ok, _boxes, next_y} <-
               layout_table(nested_style, children, 0.0, 0.0, available_width) do
          {:ok, max(0.0 - next_y - vertical_box_size(style), 0.0)}
        end

      display when display in [:grid, :inline_grid] ->
        nested_style = Map.put(style, :margin, edges(0.0))
        available_width = content_width + horizontal_box_size(style)

        with {:ok, _boxes, next_y} <-
               layout_grid(nested_style, children, 0.0, 0.0, available_width) do
          {:ok, max(0.0 - next_y - vertical_box_size(style), 0.0)}
        end

      display when display in [:flex, :inline_flex] ->
        nested_style = Map.put(style, :margin, edges(0.0))
        available_width = content_width + horizontal_box_size(style)

        with {:ok, _boxes, next_y} <-
               layout_flex(nested_style, children, 0.0, 0.0, available_width) do
          {:ok, max(0.0 - next_y - vertical_box_size(style), 0.0)}
        end

      _ ->
        with {:ok, _boxes, content_height} <-
               layout_block_content(children, style, 0.0, 0.0, content_width, %{}) do
          {:ok, content_height}
        end
    end
  end

  defp build_flex_image_item(style, index, main_axis, available_main, available_cross) do
    margin = Map.get(style, :margin, edges(0.0))

    {available_width, available_height} =
      case main_axis do
        :row -> {available_main, available_cross}
        :column -> {available_cross, available_main}
      end

    {content_width, content_height} = image_content_size(style, available_width, available_height)

    {content_main, content_cross} =
      case main_axis do
        :row -> {content_width, content_height}
        :column -> {content_height, content_width}
      end

    main_box = content_main + flex_main_box_size(style, main_axis)
    cross_box = content_cross + flex_cross_box_size(style, main_axis)

    %{
      index: index,
      style: style,
      image: Map.fetch!(style, :image),
      order: Map.get(style, :order, 0),
      flex_grow: Map.get(style, :flex_grow, 0.0),
      flex_shrink: Map.get(style, :flex_shrink, 1.0),
      main_axis: main_axis,
      main_box: main_box,
      cross_box: cross_box,
      outer_main: main_box + flex_main_margin_size(margin, main_axis),
      outer_cross: cross_box + flex_cross_margin_size(margin, main_axis),
      margin: margin
    }
  end

  defp flex_basis(style, size_property, available_size, intrinsic_size) do
    case Map.get(style, :flex_basis, :auto) do
      :auto -> resolved_content_size(style, size_property, available_size, intrinsic_size)
      basis when is_number(basis) -> basis
    end
  end

  defp flex_lines(items, style, content_width) do
    main_axis = flex_main_axis(style)
    gap = flex_main_gap(style)
    available_main = flex_available_main(style, main_axis, content_width, items, gap)
    wrap = Map.get(style, :flex_wrap, :nowrap)

    lines =
      Enum.reduce(items, [], fn item, lines ->
        append_flex_item_to_lines(lines, item, wrap, available_main, gap)
      end)

    lines
    |> Enum.map(&resolve_flex_line(&1, available_main, gap))
  end

  defp append_flex_item_to_lines([], item, _wrap, _available_main, _gap) do
    [%{items: [item], base_main: item.outer_main}]
  end

  defp append_flex_item_to_lines(lines, item, wrap, available_main, gap) do
    [line | previous] = Enum.reverse(lines)
    next_base = line.base_main + gap + item.outer_main

    case wrap == :wrap and line.items != [] and next_base > available_main do
      true ->
        Enum.reverse(previous) ++ [line, %{items: [item], base_main: item.outer_main}]

      false ->
        Enum.reverse(previous) ++ [%{line | items: line.items ++ [item], base_main: next_base}]
    end
  end

  defp resolve_flex_line(line, available_main, gap) do
    item_gap_total = gap * max(length(line.items) - 1, 0)
    base_without_gap = Enum.reduce(line.items, 0.0, &(&1.outer_main + &2))

    free_space = available_main - base_without_gap - item_gap_total
    items = resolve_flex_item_sizes(line.items, free_space)
    outer_main = Enum.reduce(items, 0.0, &(&1.outer_main + &2)) + item_gap_total
    cross = items |> Enum.map(& &1.outer_cross) |> Enum.max(fn -> 0.0 end)

    %{items: items, main: outer_main, cross: cross}
  end

  defp resolve_flex_item_sizes(items, free_space) do
    cond do
      free_space > 0 ->
        total_grow = Enum.reduce(items, 0.0, &(&1.flex_grow + &2))

        case total_grow > 0 do
          true ->
            Enum.map(items, fn item ->
              extra = free_space * item.flex_grow / total_grow
              resize_flex_item(item, item.main_box + extra)
            end)

          false ->
            items
        end

      free_space < 0 ->
        scaled_shrink = Enum.reduce(items, 0.0, &(&1.flex_shrink * &1.main_box + &2))

        case scaled_shrink > 0 do
          true ->
            Enum.map(items, fn item ->
              reduction = abs(free_space) * item.flex_shrink * item.main_box / scaled_shrink
              resize_flex_item(item, max(item.main_box - reduction, 0.0))
            end)

          false ->
            items
        end

      true ->
        items
    end
  end

  defp resize_flex_item(item, main_box) do
    outer_main = main_box + flex_main_margin_size(item.margin, item.main_axis)
    %{item | main_box: main_box, outer_main: outer_main}
  end

  defp flex_line_boxes(lines, style, x, y, content_width, content_height, container_flow_id) do
    main_axis = flex_main_axis(style)
    cross_gap = flex_cross_gap(style)

    {lines, _cross_offset} =
      Enum.reduce(lines, {[], 0.0}, fn line, {acc, cross_offset} ->
        line_cross = flex_line_cross(line, style, length(lines))

        positioned =
          flex_position_line(
            line,
            style,
            x,
            y,
            content_width,
            content_height,
            cross_offset,
            line_cross
          )

        {acc ++ positioned, cross_offset + line_cross + cross_gap}
      end)

    Enum.reduce_while(lines, {:ok, []}, fn item, {:ok, acc} ->
      {:ok, boxes} = flex_item_boxes(item, main_axis)
      tagged_boxes = tag_boxes(boxes, %{flow_id: container_flow_id})
      {:cont, {:ok, acc ++ tagged_boxes}}
    end)
  end

  defp flex_position_line(
         line,
         style,
         x,
         y,
         content_width,
         content_height,
         cross_offset,
         line_cross
       ) do
    main_axis = flex_main_axis(style)
    available_main = flex_position_available_main(main_axis, content_width, content_height)
    gap = flex_main_gap(style)
    items = flex_direction_items(line.items, style)
    {main_offset, item_gap} = flex_main_distribution(style, line, available_main, gap)

    items
    |> Enum.reduce({[], main_offset}, fn item, {acc, main_offset} ->
      item =
        position_flex_item(
          item,
          style,
          x,
          y,
          content_width,
          cross_offset,
          line_cross,
          main_offset
        )

      {acc ++ [item], main_offset + item.outer_main + item_gap}
    end)
    |> elem(0)
  end

  defp position_flex_item(item, style, x, y, content_width, cross_offset, line_cross, main_offset) do
    main_axis = flex_main_axis(style)
    align = flex_item_align(item, style)
    cross_position = flex_cross_position(align, item.outer_cross, line_cross)

    case main_axis do
      :row ->
        box_width = item.main_box
        box_height = flex_aligned_cross_box(item, align, line_cross, :row)

        Map.merge(item, %{
          x: x + main_offset + item.margin.left,
          y: y - cross_offset - cross_position - item.margin.top,
          box_width: box_width,
          box_height: box_height
        })

      :column ->
        box_width = flex_aligned_cross_box(item, align, content_width, :column)
        box_height = item.main_box

        Map.merge(item, %{
          x: x + cross_position + item.margin.left,
          y: y - main_offset - item.margin.top,
          box_width: box_width,
          box_height: box_height
        })
    end
  end

  defp flex_item_boxes(item, main_axis) do
    style = item.style
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))

    case Map.get(item, :image) do
      nil ->
        case Map.get(item, :children) do
          nil -> flex_text_item_boxes(item, main_axis, style, padding, border_widths)
          children -> flex_block_item_boxes(item, style, padding, border_widths, children)
        end

      image ->
        flex_image_item_boxes(item, main_axis, style, padding, border_widths, image)
    end
  end

  defp flex_text_item_boxes(item, main_axis, style, padding, border_widths) do
    content_x = item.x + border_widths.left + padding.left

    content_width =
      item.box_width - border_widths.left - padding.left - padding.right - border_widths.right

    content_top = item.y - border_widths.top - padding.top

    background_boxes =
      background_box(style, item.x, item.y - item.box_height, item.box_width, item.box_height)

    {text_boxes, _content_height} =
      inline_text_boxes(item.runs, style, content_x, content_top, max(content_width, 0.0), %{})

    {:ok,
     case main_axis do
       :row -> background_boxes ++ text_boxes
       :column -> background_boxes ++ text_boxes
     end}
  end

  defp flex_block_item_boxes(item, style, padding, border_widths, children) do
    case Map.get(style, :display) do
      :table ->
        nested_style = nested_flex_item_style(style, item)

        with {:ok, boxes, _next_y} <-
               layout_table(nested_style, children, item.x, item.y, item.box_width) do
          {:ok, boxes}
        end

      display when display in [:grid, :inline_grid] ->
        nested_style = nested_flex_item_style(style, item)

        with {:ok, boxes, _next_y} <-
               layout_grid(nested_style, children, item.x, item.y, item.box_width) do
          {:ok, boxes}
        end

      display when display in [:flex, :inline_flex] ->
        nested_style = nested_flex_item_style(style, item)

        with {:ok, boxes, _next_y} <-
               layout_flex(nested_style, children, item.x, item.y, item.box_width) do
          {:ok, boxes}
        end

      _ ->
        flex_plain_block_item_boxes(item, style, padding, border_widths, children)
    end
  end

  defp nested_flex_item_style(style, item) do
    style
    |> Map.put(:margin, edges(0.0))
    |> Map.put(:height, max(item.box_height - vertical_box_size(style), 0.0))
  end

  defp flex_plain_block_item_boxes(item, style, padding, border_widths, children) do
    content_x = item.x + border_widths.left + padding.left

    content_width =
      item.box_width - border_widths.left - padding.left - padding.right - border_widths.right

    content_top = item.y - border_widths.top - padding.top

    background_boxes =
      background_box(style, item.x, item.y - item.box_height, item.box_width, item.box_height)

    {:ok, content_boxes, _content_height} =
      layout_block_content(children, style, content_x, content_top, max(content_width, 0.0), %{})

    {:ok, background_boxes ++ content_boxes}
  end

  defp flex_image_item_boxes(item, main_axis, style, padding, border_widths, image) do
    content_x = item.x + border_widths.left + padding.left

    content_width =
      item.box_width - border_widths.left - padding.left - padding.right - border_widths.right

    content_height =
      item.box_height - border_widths.top - padding.top - padding.bottom - border_widths.bottom

    image_box = %{
      type: :image,
      x: content_x,
      y: item.y - border_widths.top - padding.top - max(content_height, 0.0),
      width: max(content_width, 0.0),
      height: max(content_height, 0.0),
      image: image
    }

    background_boxes =
      background_box(style, item.x, item.y - item.box_height, item.box_width, item.box_height)

    {:ok,
     case main_axis do
       :row -> background_boxes ++ [image_box]
       :column -> background_boxes ++ [image_box]
     end}
  end

  defp flex_content_height(lines, style) do
    main_axis = flex_main_axis(style)
    height = resolved_content_size(style, :height, nil, nil)

    case {main_axis, height} do
      {:row, height} when is_number(height) ->
        height

      {:row, _height} ->
        cross_gap = flex_cross_gap(style)
        Enum.reduce(lines, 0.0, &(&1.cross + &2)) + cross_gap * max(length(lines) - 1, 0)

      {:column, height} when is_number(height) ->
        height

      {:column, _height} ->
        [line] = lines
        line.main
    end
  end

  defp flex_line_cross(line, style, line_count) do
    height = resolved_content_size(style, :height, nil, nil)

    case {flex_main_axis(style), height, line_count} do
      {:row, height, 1} when is_number(height) -> height
      {:row, _height, _line_count} -> line.cross
      {:column, _height, _line_count} -> resolved_content_size(style, :width, nil, line.cross)
    end
  end

  defp flex_position_available_main(main_axis, content_width, content_height) do
    case main_axis do
      :row -> content_width
      :column -> content_height
    end
  end

  defp flex_main_distribution(style, line, available_main, gap) do
    free_space = max(available_main - line.main, 0.0)
    count = length(line.items)

    case Map.get(style, :justify_content, :flex_start) do
      :flex_end ->
        {free_space, gap}

      :center ->
        {free_space / 2, gap}

      :space_between when count > 1 ->
        {0.0, gap + free_space / (count - 1)}

      :space_around when count > 0 ->
        item_gap = gap + free_space / count
        {item_gap / 2, item_gap}

      :space_evenly when count > 0 ->
        item_gap = gap + free_space / (count + 1)
        {item_gap, item_gap}

      _ ->
        {0.0, gap}
    end
  end

  defp flex_cross_position(align, item_outer_cross, line_cross) do
    case align do
      :flex_end -> max(line_cross - item_outer_cross, 0.0)
      :center -> max((line_cross - item_outer_cross) / 2, 0.0)
      _ -> 0.0
    end
  end

  defp flex_aligned_cross_box(item, align, line_cross, main_axis) do
    case {align, main_axis} do
      {:stretch, :row} ->
        max(line_cross - item.margin.top - item.margin.bottom, 0.0)

      {:stretch, :column} ->
        max(line_cross - item.margin.left - item.margin.right, 0.0)

      {_align, :row} ->
        item.cross_box

      {_align, :column} ->
        item.cross_box
    end
  end

  defp flex_item_align(item, container_style) do
    case Map.get(item.style, :align_self, :auto) do
      :auto -> Map.get(container_style, :align_items, :stretch)
      align -> align
    end
  end

  defp flex_direction_items(items, style) do
    case Map.get(style, :flex_direction, :row) do
      direction when direction in [:row_reverse, :column_reverse] -> Enum.reverse(items)
      _ -> items
    end
  end

  defp flex_available_main(style, main_axis, content_width, items, gap) do
    case main_axis do
      :row ->
        content_width

      :column ->
        case resolved_content_size(style, :height, nil, nil) do
          height when is_number(height) ->
            height

          _ ->
            Enum.reduce(items, 0.0, &(&1.outer_main + &2)) + gap * max(length(items) - 1, 0)
        end
    end
  end

  defp flex_available_cross(main_axis, content_width) do
    case main_axis do
      :column -> content_width
      :row -> nil
    end
  end

  defp flex_main_axis(style) do
    case Map.get(style, :flex_direction, :row) do
      direction when direction in [:column, :column_reverse] -> :column
      _ -> :row
    end
  end

  defp flex_main_gap(style) do
    case flex_main_axis(style) do
      :row -> Map.get(style, :column_gap, 0.0)
      :column -> Map.get(style, :row_gap, 0.0)
    end
  end

  defp flex_cross_gap(style) do
    case flex_main_axis(style) do
      :row -> Map.get(style, :row_gap, 0.0)
      :column -> Map.get(style, :column_gap, 0.0)
    end
  end

  defp flex_main_box_size(style, main_axis) do
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))

    case main_axis do
      :row -> padding.left + padding.right + border_widths.left + border_widths.right
      :column -> padding.top + padding.bottom + border_widths.top + border_widths.bottom
    end
  end

  defp flex_cross_box_size(style, main_axis) do
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))

    case main_axis do
      :row -> padding.top + padding.bottom + border_widths.top + border_widths.bottom
      :column -> padding.left + padding.right + border_widths.left + border_widths.right
    end
  end

  defp flex_main_margin_size(margin, main_axis) do
    case main_axis do
      :row -> margin.left + margin.right
      :column -> margin.top + margin.bottom
    end
  end

  defp flex_cross_margin_size(margin, main_axis) do
    case main_axis do
      :row -> margin.top + margin.bottom
      :column -> margin.left + margin.right
    end
  end

  defp flex_block_intrinsic_width(children) do
    Enum.reduce(children, 0.0, fn child, width ->
      max(width, flex_child_intrinsic_width(child))
    end)
  end

  defp flex_child_intrinsic_width(child) do
    case child do
      %{type: :text, text: text, style: style} when is_binary(text) ->
        text |> String.trim() |> text_width(style)

      %{type: :element, style: %{display: :none}} ->
        0.0

      %{type: :element, style: %{display: :image} = style} ->
        {width, _height} = image_content_size(style)
        width + horizontal_box_size(style)

      %{type: :element, style: style, children: children} when is_list(children) ->
        case inline_runs(children) do
          {:ok, runs} ->
            runs
            |> Enum.reduce(0.0, fn run, width -> width + text_width(run.text, run.style) end)
            |> Kernel.+(horizontal_box_size(style))

          {:error, _reason} ->
            flex_block_intrinsic_width(children) + horizontal_box_size(style)
        end

      _ ->
        0.0
    end
  end

  defp width_available_size(style, available_box_width) do
    case Map.get(style, :box_sizing, :content_box) do
      :border_box -> available_box_width
      _ -> available_box_width - horizontal_box_size(style)
    end
  end

  defp resolved_size(style, property, available_size, default) do
    size =
      case Map.get(style, property) do
        {:min, sizes} when is_list(sizes) ->
          sizes
          |> Enum.map(&resolve_size_value(&1, available_size))
          |> Enum.reject(&is_nil/1)
          |> Enum.min(fn -> default end)

        {:percent, ratio} when is_number(available_size) ->
          max(available_size * ratio, 0.0)

        value when is_number(value) ->
          value

        _ ->
          default
      end

    apply_size_constraints(style, property, size, available_size)
  end

  defp resolved_content_size(style, property, available_size, default) do
    size = resolved_size(style, property, available_size, default)

    case {Map.get(style, :box_sizing, :content_box), property, size} do
      {:border_box, :width, size} when is_number(size) ->
        max(size - horizontal_box_size(style), 0.0)

      {:border_box, :height, size} when is_number(size) ->
        max(size - vertical_box_size(style), 0.0)

      _ ->
        size
    end
  end

  defp resolve_size_value(value, available_size) do
    case value do
      {:percent, ratio} when is_number(available_size) -> max(available_size * ratio, 0.0)
      value when is_number(value) -> value
      _ -> nil
    end
  end

  defp apply_size_constraints(style, property, size, available_size) do
    case {property, size} do
      {:width, size} when is_number(size) ->
        size
        |> apply_min_size(style, :min_width, available_size)
        |> apply_max_size(style, :max_width, available_size)

      {:height, size} when is_number(size) ->
        size
        |> apply_min_size(style, :min_height, available_size)
        |> apply_max_size(style, :max_height, available_size)

      _ ->
        size
    end
  end

  defp apply_min_size(size, style, property, available_size) do
    case resolved_constraint_size(style, property, available_size) do
      constraint when is_number(constraint) -> max(size, constraint)
      _ -> size
    end
  end

  defp apply_max_size(size, style, property, available_size) do
    case resolved_constraint_size(style, property, available_size) do
      constraint when is_number(constraint) -> min(size, constraint)
      _ -> size
    end
  end

  defp resolved_constraint_size(style, property, available_size) do
    style
    |> Map.get(property)
    |> resolve_size_value(available_size)
  end

  defp apply_image_size_constraints(
         style,
         width,
         height,
         ratio,
         available_width,
         available_height,
         explicit_dimensions?
       ) do
    case explicit_dimensions? do
      true ->
        {
          apply_size_constraints(style, :width, width, available_width),
          apply_size_constraints(style, :height, height, available_height)
        }

      false ->
        {width, height}
        |> scale_image_down_to_max(style, ratio, available_width, available_height)
        |> scale_image_up_to_min(style, ratio, available_width, available_height)
    end
  end

  defp scale_image_down_to_max({width, height}, style, ratio, available_width, available_height) do
    max_width = resolved_constraint_size(style, :max_width, available_width)
    max_height = resolved_constraint_size(style, :max_height, available_height)

    scale =
      [max_image_scale(width, max_width), max_image_scale(height, max_height)]
      |> Enum.reject(&is_nil/1)
      |> Enum.min(fn -> 1.0 end)
      |> min(1.0)

    {width * scale, width * scale / ratio}
  end

  defp scale_image_up_to_min({width, height}, style, ratio, available_width, available_height) do
    min_width = resolved_constraint_size(style, :min_width, available_width)
    min_height = resolved_constraint_size(style, :min_height, available_height)

    scale =
      [min_image_scale(width, min_width), min_image_scale(height, min_height)]
      |> Enum.reject(&is_nil/1)
      |> Enum.max(fn -> 1.0 end)
      |> max(1.0)

    {width * scale, width * scale / ratio}
  end

  defp max_image_scale(size, constraint) do
    case {size, constraint} do
      {size, constraint} when is_number(size) and size > 0 and is_number(constraint) ->
        constraint / size

      _ ->
        nil
    end
  end

  defp min_image_scale(size, constraint) do
    case {size, constraint} do
      {size, constraint} when is_number(size) and size > 0 and is_number(constraint) ->
        constraint / size

      _ ->
        nil
    end
  end

  defp horizontal_box_size(style) do
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))
    padding.left + padding.right + border_widths.left + border_widths.right
  end

  defp vertical_box_size(style) do
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))
    padding.top + padding.bottom + border_widths.top + border_widths.bottom
  end

  defp layout_table(style, children, x, y, width) do
    margin = Map.get(style, :margin, edges(0.0, 0.0, Map.get(style, :margin_after, 0.0), 0.0))
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))
    box_x = x + margin.left
    box_top = y - margin.top
    available_box_width = width - margin.left - margin.right
    collapsed? = Map.get(style, :border_collapse, :separate) == :collapse

    content_width =
      resolved_content_size(
        style,
        :width,
        width_available_size(style, available_box_width),
        available_box_width - horizontal_box_size(style)
      )

    box_width =
      case collapsed? do
        true ->
          content_width

        false ->
          content_width + border_widths.left + padding.left + padding.right +
            border_widths.right
      end

    content_x =
      case collapsed? do
        true -> box_x + padding.left
        false -> box_x + border_widths.left + padding.left
      end

    content_top =
      case collapsed? do
        true -> box_top - padding.top
        false -> box_top - border_widths.top - padding.top
      end

    table_id = {:table, content_x, content_top}
    table_metadata = break_metadata(style)

    with {:ok, caption_boxes, rows_top} <-
           layout_table_caption(children, content_x, content_top, content_width, table_id),
         {:ok, rows} <- table_rows(children),
         {:ok, row_boxes, content_bottom} <-
           layout_table_rows(
             rows,
             content_x,
             rows_top,
             content_width,
             table_id,
             Map.get(style, :border_collapse, :separate)
           ) do
      content_height = content_top - content_bottom

      box_height =
        case collapsed? do
          true ->
            padding.top + content_height + padding.bottom

          false ->
            border_widths.top + padding.top + content_height + padding.bottom +
              border_widths.bottom
        end

      table_boxes =
        table_background_boxes(
          style,
          box_x,
          box_top - box_height,
          box_width,
          box_height,
          rows_top,
          content_bottom,
          Map.get(style, :border_collapse, :separate),
          Map.merge(table_metadata, %{flow_id: table_id, table_id: table_id})
        )

      next_y = box_top - box_height - margin.bottom
      {:ok, table_boxes.before ++ caption_boxes ++ row_boxes ++ table_boxes.after, next_y}
    end
  end

  defp table_background_boxes(
         style,
         x,
         y,
         width,
         height,
         rows_top,
         content_bottom,
         border_collapse,
         metadata
       ) do
    case border_collapse do
      :collapse ->
        row_grid_height = rows_top - content_bottom

        background =
          style
          |> Map.put(:border_widths, edges(0.0))
          |> background_box(x, content_bottom, width, row_grid_height)
          |> tag_boxes(metadata)

        border =
          style
          |> Map.put(:background_color, nil)
          |> background_box(x, content_bottom, width, row_grid_height)
          |> tag_boxes(metadata)

        %{before: background, after: border}

      _ ->
        boxes =
          style
          |> background_box(x, y, width, height)
          |> tag_boxes(metadata)

        %{before: boxes, after: []}
    end
  end

  defp layout_table_caption(children, x, y, width, table_id) do
    case Enum.find(children, &match?(%{style: %{display: :table_caption}}, &1)) do
      nil ->
        {:ok, [], y}

      %{style: style, children: caption_children} when is_list(caption_children) ->
        with {:ok, runs} <- inline_runs(caption_children) do
          margin =
            Map.get(style, :margin, edges(0.0, 0.0, Map.get(style, :margin_after, 0.0), 0.0))

          padding = Map.get(style, :padding, edges(0.0))
          border_widths = Map.get(style, :border_widths, edges(0.0))
          box_x = x + margin.left
          box_top = y - margin.top
          box_width = width - margin.left - margin.right

          content_x = box_x + border_widths.left + padding.left

          content_width =
            box_width - border_widths.left - padding.left - padding.right - border_widths.right

          content_top = box_top - border_widths.top - padding.top

          flow_metadata = %{flow_id: {:table_caption, table_id}, table_id: table_id}

          {text_boxes, content_height} =
            inline_text_boxes(runs, style, content_x, content_top, content_width, flow_metadata)

          box_height =
            border_widths.top + padding.top + content_height + padding.bottom +
              border_widths.bottom

          background_box =
            style
            |> background_box(box_x, box_top - box_height, box_width, box_height)
            |> tag_boxes(flow_metadata)

          {:ok, background_box ++ text_boxes, box_top - box_height - margin.bottom}
        end

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp table_rows(children) do
    result =
      Enum.reduce_while(children, {:ok, []}, fn child, {:ok, rows} ->
        case table_child_rows(child) do
          {:ok, table_rows} -> {:cont, {:ok, rows ++ table_rows}}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)

    case result do
      {:ok, rows} when rows != [] -> {:ok, rows}
      _ -> {:error, :invalid_layout}
    end
  end

  defp table_child_rows(child) do
    case child do
      %{style: %{display: display}} when display in [:none, :table_caption] ->
        {:ok, []}

      %{style: %{display: :table_row}, children: cells} when is_list(cells) ->
        case visible_table_cells(cells) do
          [] ->
            {:ok, []}

          visible_cells ->
            {:ok, [%{row: Map.put(child, :children, visible_cells), section: :body}]}
        end

      %{style: %{display: :table_row_group}, children: group_rows} when is_list(group_rows) ->
        section = child.style |> Map.get(:table_section, :body)

        Enum.reduce_while(group_rows, {:ok, []}, fn group_row, {:ok, rows} ->
          case table_group_row(group_row, section) do
            {:ok, table_rows} -> {:cont, {:ok, rows ++ table_rows}}
            {:error, reason} -> {:halt, {:error, reason}}
          end
        end)

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp table_group_row(row, section) do
    case row do
      %{style: %{display: :none}} ->
        {:ok, []}

      %{style: %{display: :table_row}, children: cells} when is_list(cells) ->
        case visible_table_cells(cells) do
          [] ->
            {:ok, []}

          visible_cells ->
            {:ok, [%{row: Map.put(row, :children, visible_cells), section: section}]}
        end

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp visible_table_cells(cells) do
    Enum.reject(cells, &match?(%{style: %{display: :none}}, &1))
  end

  defp layout_table_rows(rows, x, y, width, table_id, border_collapse) do
    column_count =
      rows
      |> Enum.map(fn %{row: %{children: cells}} -> table_column_count(cells) end)
      |> Enum.max()

    column_widths = table_column_widths(rows, column_count, width)

    result =
      rows
      |> Enum.with_index()
      |> Enum.reduce_while({:ok, [], y}, fn {%{row: row, section: section}, index},
                                            {:ok, boxes, current_y} ->
        case layout_table_row(
               row,
               section,
               table_id,
               index,
               x,
               current_y,
               column_widths,
               width,
               border_collapse,
               index == length(rows) - 1
             ) do
          {:ok, row_boxes, next_y} -> {:cont, {:ok, boxes ++ row_boxes, next_y}}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)

    case result do
      {:ok, boxes, next_y} -> {:ok, boxes, next_y}
      {:error, reason} -> {:error, reason}
    end
  end

  defp layout_table_row(
         %{style: %{display: :table_row} = style, children: cells},
         section,
         table_id,
         index,
         x,
         y,
         column_widths,
         table_width,
         border_collapse,
         last_row?
       )
       when is_list(cells) do
    case Enum.all?(cells, &match?(%{style: %{display: :table_cell}}, &1)) do
      true ->
        with {:ok, row_height} <- table_row_height(cells, column_widths, table_width, style) do
          row_metadata =
            table_id
            |> table_row_metadata(section, index)
            |> Map.merge(break_metadata(style))

          {boxes, consumed_columns} =
            Enum.reduce(cells, {[], 0}, fn cell, {acc, index} ->
              colspan = table_cell_colspan(cell)
              cell_x = x + (column_widths |> Enum.take(index) |> Enum.sum())
              cell_width = table_cell_width(cells, column_widths, index, colspan, table_width)
              last_cell? = index + colspan >= length(column_widths)

              {:ok, cell_boxes} =
                layout_table_cell(
                  cell,
                  cell_x,
                  y,
                  cell_width,
                  row_height,
                  row_metadata,
                  border_collapse,
                  last_cell?,
                  last_row?
                )

              {acc ++ cell_boxes, index + colspan}
            end)

          boxes =
            boxes ++
              trailing_collapsed_table_border(
                cells,
                x,
                y,
                row_height,
                column_widths,
                consumed_columns,
                border_collapse,
                last_row?,
                row_metadata
              )

          {background_boxes, content_boxes} =
            Enum.split_with(boxes, &(Map.get(&1, :role) == :table_cell_background))

          {border_boxes, content_boxes} =
            Enum.split_with(content_boxes, &(Map.get(&1, :role) == :table_border))

          {:ok, background_boxes ++ border_boxes ++ content_boxes, y - row_height}
        else
          {:error, reason} -> {:error, reason}
        end

      false ->
        {:error, :invalid_layout}
    end
  end

  defp layout_table_cell(
         %{style: %{display: :table_cell} = style, children: children},
         x,
         y,
         width,
         height,
         row_metadata,
         border_collapse,
         last_cell?,
         last_row?
       )
       when is_list(children) do
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))
    content_x = x + border_widths.left + padding.left

    content_width =
      width - border_widths.left - padding.left - padding.right - border_widths.right

    content_top = y - border_widths.top - padding.top

    with {:ok, content_boxes, content_bottom} <-
           layout_table_cell_content(children, style, content_x, content_top, content_width) do
      content_height = content_top - content_bottom

      content_area_height =
        height - border_widths.top - padding.top - padding.bottom - border_widths.bottom

      vertical_offset = table_cell_vertical_offset(style, content_area_height, content_height)

      content_boxes =
        Enum.map(content_boxes, fn box ->
          Map.update!(box, :y, &(&1 - vertical_offset))
        end)

      cell_box =
        table_cell_background_box(style, x, y - height, width, height, border_collapse)
        |> tag_boxes(row_metadata)
        |> Enum.map(&Map.put(&1, :role, :table_cell_background))

      border_box =
        table_cell_border_box(
          style,
          x,
          y - height,
          width,
          height,
          border_collapse,
          last_cell?,
          last_row?
        )
        |> tag_boxes(row_metadata)
        |> Enum.map(&Map.put(&1, :role, :table_border))

      {:ok, cell_box ++ border_box ++ tag_boxes(content_boxes, row_metadata)}
    end
  end

  defp table_cell_background_box(style, x, y, width, height, border_collapse) do
    case border_collapse do
      :collapse ->
        style
        |> Map.put(:border_widths, edges(0.0))
        |> background_box(x, y, width, height)
        |> Enum.map(fn box ->
          box
          |> Map.put(:stroke_width, 0.0)
          |> Map.put(:border_widths, edges(0.0))
        end)

      _ ->
        background_box(style, x, y, width, height)
    end
  end

  defp table_cell_border_box(
         style,
         x,
         y,
         width,
         height,
         border_collapse,
         last_cell?,
         last_row?
       ) do
    case border_collapse do
      :collapse ->
        border_widths = Map.get(style, :border_widths, edges(0.0))

        collapsed_widths = %{
          top: border_widths.top,
          right: if(last_cell?, do: border_widths.right, else: 0.0),
          bottom: if(last_row?, do: border_widths.bottom, else: 0.0),
          left: border_widths.left
        }

        stroke_width = collapsed_widths |> Map.values() |> Enum.max()

        case stroke_width > 0 do
          true ->
            [
              %{
                type: :rect,
                x: x,
                y: y,
                width: width,
                height: height,
                fill_color: nil,
                stroke_color: Map.get(style, :border_color, {0, 0, 0}),
                stroke_width: stroke_width,
                border_widths: collapsed_widths,
                border_radius: 0.0
              }
            ]

          false ->
            []
        end

      _ ->
        []
    end
  end

  defp trailing_collapsed_table_border(
         cells,
         x,
         y,
         row_height,
         column_widths,
         consumed_columns,
         border_collapse,
         last_row?,
         row_metadata
       ) do
    case {border_collapse, length(cells) > 1 and consumed_columns < length(column_widths)} do
      {:collapse, true} ->
        style =
          cells
          |> List.last()
          |> Map.fetch!(:style)

        cell_x = x + (column_widths |> Enum.take(consumed_columns) |> Enum.sum())

        cell_width =
          column_widths
          |> Enum.drop(consumed_columns)
          |> Enum.sum()

        style
        |> table_cell_border_box(
          cell_x,
          y - row_height,
          cell_width,
          row_height,
          :collapse,
          true,
          last_row?
        )
        |> tag_boxes(row_metadata)
        |> Enum.map(&Map.put(&1, :role, :table_border))

      _ ->
        []
    end
  end

  defp table_cell_vertical_offset(style, content_area_height, content_height) do
    extra_space = max(content_area_height - content_height, 0.0)

    case Map.get(style, :vertical_align, :middle) do
      :bottom -> extra_space
      :middle -> extra_space / 2
      _ -> 0.0
    end
  end

  defp table_column_count(cells) do
    Enum.reduce(cells, 0, &(&2 + table_cell_colspan(&1)))
  end

  defp table_column_widths(rows, column_count, table_width) do
    preferred =
      Enum.reduce(rows, List.duplicate(nil, column_count), fn %{row: %{children: cells}},
                                                              widths ->
        {next_widths, _index} =
          Enum.reduce(cells, {widths, 0}, fn cell, {acc, index} ->
            colspan = table_cell_colspan(cell)

            acc =
              case table_cell_preferred_width(cell, table_width) do
                nil ->
                  acc

                preferred_width ->
                  share = preferred_width / colspan

                  Enum.reduce(index..(index + colspan - 1), acc, fn column, column_acc ->
                    List.update_at(column_acc, column, fn
                      nil -> share
                      width -> max(width, share)
                    end)
                  end)
              end

            {acc, index + colspan}
          end)

        next_widths
      end)

    minimum = table_minimum_column_widths(rows, column_count)
    fixed_total = preferred |> Enum.reject(&is_nil/1) |> Enum.sum()
    flexible_count = Enum.count(preferred, &is_nil/1)

    cond do
      fixed_total > table_width and fixed_total > 0 and flexible_count > 0 ->
        Enum.map(preferred, fn
          nil -> 0.0
          width -> width / fixed_total * table_width
        end)

      fixed_total > table_width and fixed_total > 0 ->
        preferred
        |> Enum.with_index()
        |> Enum.map(fn {width, index} ->
          max(width, Enum.at(minimum, index))
        end)
        |> shrink_columns_to_width(minimum, table_width)

      flexible_count > 0 ->
        fixed_widths =
          preferred
          |> Enum.with_index()
          |> Enum.map(fn
            {nil, _index} -> nil
            {width, index} -> max(width, Enum.at(minimum, index))
          end)

        fixed_total = fixed_widths |> Enum.reject(&is_nil/1) |> Enum.sum()
        flexible_width = max((table_width - fixed_total) / flexible_count, 0.0)

        fixed_widths
        |> Enum.with_index()
        |> Enum.map(fn
          {nil, index} -> max(flexible_width, Enum.at(minimum, index))
          {width, _index} -> width
        end)
        |> shrink_columns_to_width(minimum, table_width)

      true ->
        Enum.map(preferred, &(&1 / fixed_total * table_width))
    end
  end

  defp table_minimum_column_widths(rows, column_count) do
    Enum.reduce(rows, List.duplicate(0.0, column_count), fn %{row: %{children: cells}}, widths ->
      {next_widths, _index} =
        Enum.reduce(cells, {widths, 0}, fn cell, {acc, index} ->
          colspan = table_cell_colspan(cell)

          acc =
            case {colspan, table_cell_minimum_width(cell)} do
              {1, min_width} when min_width > 0 ->
                List.update_at(acc, index, &max(&1, min_width))

              _ ->
                acc
            end

          {acc, index + colspan}
        end)

      next_widths
    end)
  end

  defp table_cell_minimum_width(cell) do
    case cell do
      %{style: %{display: :table_cell} = style, children: children} when is_list(children) ->
        case inline_runs(children) do
          {:ok, runs} ->
            padding = Map.get(style, :padding, edges(0.0))
            border_widths = Map.get(style, :border_widths, edges(0.0))

            content_width =
              runs
              |> Enum.flat_map(fn run ->
                run.text
                |> inline_wrap_tokens(run.style)
                |> Enum.map(&%{text: String.trim(&1), style: run.style})
              end)
              |> Enum.reject(&(&1.text == ""))
              |> Enum.map(&text_width(&1.text, &1.style))
              |> Enum.max(fn -> 0.0 end)

            content_width + padding.left + padding.right + border_widths.left +
              border_widths.right

          {:error, _reason} ->
            0.0
        end

      _ ->
        0.0
    end
  end

  defp shrink_columns_to_width(widths, minimum, table_width) do
    total_width = Enum.sum(widths)

    case total_width > table_width and total_width > 0 do
      true ->
        shrinkable =
          widths
          |> Enum.zip(minimum)
          |> Enum.map(fn {width, min_width} -> max(width - min_width, 0.0) end)

        shrinkable_total = Enum.sum(shrinkable)
        overflow = total_width - table_width

        case {shrinkable_total > 0, overflow <= shrinkable_total} do
          {true, true} ->
            shrink_largest_columns(widths, minimum, overflow)

          _ ->
            Enum.map(widths, &(&1 / total_width * table_width))
        end

      false ->
        widths
    end
  end

  defp shrink_largest_columns(widths, minimum, overflow) do
    shrink_order =
      widths
      |> Enum.zip(minimum)
      |> Enum.with_index()
      |> Enum.map(fn {{width, min_width}, index} ->
        {index, max(width - min_width, 0.0)}
      end)
      |> Enum.sort_by(fn {_index, shrinkable_width} -> shrinkable_width end, :desc)

    {shrunk_widths, _remaining_overflow} =
      Enum.reduce(shrink_order, {widths, overflow}, fn {index, shrinkable_width},
                                                       {acc, remaining_overflow} ->
        reduction = min(shrinkable_width, remaining_overflow)

        acc =
          List.update_at(acc, index, fn width ->
            width - reduction
          end)

        {acc, remaining_overflow - reduction}
      end)

    shrunk_widths
  end

  defp table_cell_preferred_width(cell, table_width) do
    case cell do
      %{style: %{width: {:percent, ratio}}} when is_number(ratio) ->
        table_width * ratio

      %{style: %{width: width}} when is_number(width) and width > 0 ->
        min(width, table_width)

      _ ->
        nil
    end
  end

  defp table_cell_width(cells, column_widths, index, colspan, table_width) do
    case cells do
      [_single_cell] -> table_width
      _ -> column_widths |> Enum.slice(index, colspan) |> Enum.sum()
    end
  end

  defp table_row_height(cells, column_widths, table_width, style) do
    cells
    |> Enum.reduce_while({:ok, [], 0}, fn cell, {:ok, heights, index} ->
      colspan = table_cell_colspan(cell)
      cell_width = table_cell_width(cells, column_widths, index, colspan, table_width)

      case table_cell_height(cell, cell_width) do
        {:ok, height} -> {:cont, {:ok, heights ++ [height], index + colspan}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
    |> case do
      {:ok, heights, _index} when heights != [] ->
        {:ok, max(Enum.max(heights), table_row_declared_height(style))}

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp table_row_declared_height(style) do
    resolved_content_size(style, :height, nil, nil) || 0.0
  end

  defp table_cell_height(cell, width) do
    case cell do
      %{style: %{display: :table_cell} = style, children: children} when is_list(children) ->
        padding = Map.get(style, :padding, edges(0.0))
        border_widths = Map.get(style, :border_widths, edges(0.0))

        content_width =
          width - border_widths.left - padding.left - padding.right - border_widths.right

        with {:ok, _boxes, content_bottom} <-
               layout_table_cell_content(children, style, 0.0, 0.0, content_width) do
          content_height = 0.0 - content_bottom

          content_box_height =
            border_widths.top + padding.top + content_height + padding.bottom +
              border_widths.bottom

          {:ok, max(content_box_height, table_cell_declared_height(style))}
        end

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp table_cell_colspan(cell) do
    case cell do
      %{style: %{colspan: colspan}} when is_integer(colspan) and colspan >= 1 -> colspan
      _ -> 1
    end
  end

  defp table_cell_declared_height(style) do
    [
      resolved_content_size(style, :height, nil, nil),
      resolved_content_size(style, :min_height, nil, nil)
    ]
    |> Enum.filter(&is_number/1)
    |> Enum.max(fn -> 0.0 end)
  end

  defp layout_table_cell_content(children, style, x, y, width) do
    case inline_runs(children) do
      {:ok, runs} ->
        {boxes, content_height} = inline_text_boxes(runs, style, x, y, width, %{})
        {:ok, boxes, y - content_height}

      {:error, _reason} ->
        layout_table_cell_blocks(children, style, x, y, width)
    end
  end

  defp layout_table_cell_blocks(children, style, x, y, width) do
    result =
      Enum.reduce_while(children, {:ok, [], y}, fn child, {:ok, boxes, current_y} ->
        case layout_table_cell_block(child, style, x, current_y, width) do
          {:ok, child_boxes, next_y} -> {:cont, {:ok, boxes ++ child_boxes, next_y}}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)

    case result do
      {:ok, boxes, next_y} -> {:ok, boxes, next_y}
      {:error, reason} -> {:error, reason}
    end
  end

  defp layout_table_cell_block(child, style, x, y, width) do
    case child do
      %{type: :text, text: text} when is_binary(text) ->
        case String.trim(text) do
          "" ->
            {:ok, [], y}

          _ ->
            with {:ok, runs} <- inline_runs([child]) do
              {boxes, content_height} = inline_text_boxes(runs, style, x, y, width, %{})
              {:ok, boxes, y - content_height}
            end
        end

      %{type: :element, style: %{display: display}} when display in [:inline, :line_break] ->
        with {:ok, runs} <- inline_runs([child]) do
          {boxes, content_height} = inline_text_boxes(runs, style, x, y, width, %{})
          {:ok, boxes, y - content_height}
        end

      _ ->
        layout_block(child, x, y, width)
    end
  end

  defp table_row_metadata(table_id, section, index) do
    metadata = %{
      flow_id: {:table_row, table_id, index},
      table_id: table_id,
      table_section: section
    }

    case section do
      :head -> Map.put(metadata, :repeat_table_header, true)
      _ -> metadata
    end
  end

  defp aligned_text_x(runs, style, x, width) do
    text_width =
      Enum.reduce(runs, 0.0, fn run, acc ->
        acc + text_width(run.text, run.style)
      end)

    case Map.get(style, :text_align, :left) do
      :center -> x + max((width - text_width) / 2, 0.0)
      :right -> x + max(width - text_width, 0.0)
      _ -> x
    end
  end

  defp layout_list(style, children, x, y, width) do
    margin = Map.get(style, :margin, edges(0.0, 0.0, Map.get(style, :margin_after, 0.0), 0.0))
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))
    box_x = x + margin.left
    box_top = y - margin.top
    box_width = width - margin.left - margin.right
    content_x = box_x + border_widths.left + padding.left
    content_top = box_top - border_widths.top - padding.top

    content_width =
      box_width - border_widths.left - padding.left - padding.right - border_widths.right

    case layout_list_items(
           children,
           Map.fetch!(style, :list_marker_type),
           content_x,
           content_top,
           content_width
         ) do
      {:ok, item_boxes, content_bottom} ->
        content_height = content_top - content_bottom

        box_height =
          border_widths.top + padding.top + content_height + padding.bottom + border_widths.bottom

        background_box = background_box(style, box_x, box_top - box_height, box_width, box_height)
        next_y = box_top - box_height - margin.bottom
        {:ok, background_box ++ item_boxes, next_y}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp layout_list_items(children, marker_type, x, y, width) do
    result =
      children
      |> Enum.with_index(1)
      |> Enum.reduce_while({:ok, [], y}, fn {child, index}, {:ok, boxes, current_y} ->
        case layout_list_item(child, marker_type, index, x, current_y, width) do
          {:ok, item_boxes, next_y} -> {:cont, {:ok, boxes ++ item_boxes, next_y}}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)

    case result do
      {:ok, boxes, next_y} -> {:ok, boxes, next_y}
      {:error, reason} -> {:error, reason}
    end
  end

  defp layout_list_item(item, marker_type, index, x, y, width) do
    case item do
      %{type: :element, style: %{display: :list_item} = style, children: children}
      when is_list(children) ->
        with {:ok, runs} <- inline_runs(children) do
          marker_gap = 18.0
          marker = list_marker(marker_type, index)
          marker_style = text_style(style)
          baseline_y = y - Map.fetch!(style, :font_size)
          text_x = x + marker_gap
          text_width = width - marker_gap
          flow_metadata = %{flow_id: {:list_item, x, y, index}}

          marker_box =
            %{text: marker, style: marker_style}
            |> text_box(x, baseline_y, marker_gap)
            |> Map.merge(flow_metadata)

          {text_boxes, content_height} =
            inline_text_boxes(runs, style, text_x, y, text_width, flow_metadata)

          margin =
            Map.get(style, :margin, edges(0.0, 0.0, Map.get(style, :margin_after, 0.0), 0.0))

          next_y = y - content_height - margin.bottom
          {:ok, [marker_box] ++ text_boxes, next_y}
        end

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp inline_text_boxes(runs, style, x, y, width, metadata) do
    line_height = Map.fetch!(style, :line_height)
    font_size = Map.fetch!(style, :font_size)
    lines = inline_lines(runs, width)

    boxes =
      lines
      |> Enum.with_index()
      |> Enum.flat_map(fn {line_runs, index} ->
        baseline_y = y - line_height * index - font_size
        start_x = aligned_text_x(line_runs, style, x, width)

        {line_boxes, _next_x} =
          Enum.reduce(line_runs, {[], start_x}, fn run, {acc, current_x} ->
            box = run |> text_box(current_x, baseline_y, width) |> Map.merge(metadata)
            {acc ++ [box], current_x + text_width(run.text, run.style)}
          end)

        line_boxes
      end)

    {boxes, inline_content_height(runs, lines, line_height)}
  end

  defp inline_content_height(runs, width, line_height) when is_number(width) do
    runs
    |> inline_lines(width)
    |> then(&inline_content_height(runs, &1, line_height))
  end

  defp inline_content_height(runs, lines, line_height) when is_list(lines) do
    case runs do
      [] -> 0.0
      _ -> line_height * max(length(lines), 1)
    end
  end

  defp inline_lines(runs, width) do
    Enum.reduce(runs, [[]], fn run, lines ->
      run.text
      |> String.split("\n", trim: false)
      |> Enum.with_index()
      |> Enum.reduce(lines, fn {part, index}, acc ->
        acc =
          case part do
            "" -> acc
            _ -> append_wrapped_inline_text(acc, %{run | text: part}, width)
          end

        case index < length(String.split(run.text, "\n", trim: false)) - 1 do
          true -> acc ++ [[]]
          false -> acc
        end
      end)
    end)
  end

  defp append_wrapped_inline_text(lines, run, width) do
    case width do
      width when is_number(width) and width > 0 ->
        merge_key = System.unique_integer([:positive])

        run.text
        |> inline_wrap_tokens(run.style)
        |> Enum.reduce(lines, fn token, acc ->
          append_inline_token(
            acc,
            run |> Map.put(:text, token) |> Map.put(:merge_key, merge_key),
            width
          )
        end)

      _ ->
        List.update_at(lines, length(lines) - 1, &(&1 ++ [run]))
    end
  end

  defp inline_wrap_tokens(text, style) do
    case Map.get(style, :line_break, :normal) do
      :anywhere ->
        String.graphemes(text)

      _ ->
        ~r/\S+\s*|\s+/u
        |> Regex.scan(text)
        |> Enum.map(&List.first/1)
    end
  end

  defp append_inline_token(lines, run, width) do
    current_line = List.last(lines) || []
    token_width = text_width(run.text, run.style)

    cond do
      String.trim(run.text) == "" and current_line == [] ->
        lines

      token_width > width and Map.get(run.style, :line_break) == :break_word ->
        append_break_word_token(lines, run, width)

      current_line != [] and inline_line_width(current_line) + token_width > width ->
        lines ++ [[%{run | text: String.trim_leading(run.text)}]]

      true ->
        List.update_at(lines, length(lines) - 1, &append_inline_line_run(&1, run))
    end
  end

  defp append_break_word_token(lines, run, width) do
    run.text
    |> String.graphemes()
    |> Enum.reduce(lines, fn grapheme, acc ->
      append_inline_token(acc, %{run | text: grapheme}, width)
    end)
  end

  defp append_inline_line_run(line, run) do
    case List.last(line) do
      %{merge_key: merge_key, text: text} ->
        case merge_key == Map.get(run, :merge_key) do
          true -> List.update_at(line, length(line) - 1, &%{&1 | text: text <> run.text})
          false -> line ++ [run]
        end

      _ ->
        line ++ [run]
    end
  end

  defp inline_line_width(line_runs) do
    Enum.reduce(line_runs, 0.0, fn run, width -> width + text_width(run.text, run.style) end)
  end

  @spec inline_runs([term()]) ::
          {:ok, [%{text: String.t(), style: map()}]} | {:error, :invalid_layout}
  defp inline_runs(children) do
    Enum.reduce_while(children, {:ok, []}, fn child, {:ok, runs} ->
      case append_inline_run(child, runs) do
        {:ok, runs} -> {:cont, {:ok, runs}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp append_inline_run(child, runs) do
    case child do
      %{type: :text, text: text, style: style} when is_binary(text) and is_map(style) ->
        {:ok, runs ++ [%{text: collapse_inline_whitespace(text), style: style}]}

      %{type: :element, style: %{display: :inline}, children: children}
      when is_list(children) ->
        case inline_runs(children) do
          {:ok, child_runs} -> {:ok, runs ++ child_runs}
          {:error, reason} -> {:error, reason}
        end

      %{type: :element, style: %{display: :line_break} = style, children: []} ->
        {:ok, runs ++ [%{text: "\n", style: text_style(style), hard_break: true}]}

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp collapse_inline_whitespace(text) do
    String.replace(text, ~r/\s+/u, " ")
  end

  @spec text_box(%{text: String.t(), style: map()}, number(), number(), number()) :: box()
  defp text_box(run, x, y, width) do
    style = run.style
    font_face = text_font_face(style)

    box = %{
      type: :text,
      text: run.text,
      x: x,
      y: y,
      width: width,
      annotation_width: text_width(run.text, style),
      font: Font.pdf_name(font_face),
      font_face: font_face,
      font_size: Map.fetch!(style, :font_size),
      letter_spacing: Map.get(style, :letter_spacing, 0.0),
      line_height: Map.fetch!(style, :line_height),
      color: Map.fetch!(style, :color)
    }

    case Map.get(style, :link_url) do
      link_url when is_binary(link_url) -> Map.put(box, :link_url, link_url)
      _ -> box
    end
  end

  @spec background_box(map(), number(), number(), number(), number()) :: [box()]
  defp background_box(style, x, y, width, height) do
    border_widths = Map.get(style, :border_widths, edges(0.0))
    stroke_width = Enum.max(Map.values(border_widths))
    fill_color = Map.get(style, :background_color)

    case {fill_color, stroke_width > 0} do
      {nil, false} ->
        []

      _ ->
        [
          %{
            type: :rect,
            x: x,
            y: y,
            width: width,
            height: height,
            fill_color: fill_color,
            stroke_color: Map.get(style, :border_color, {0, 0, 0}),
            stroke_width: stroke_width,
            border_widths: border_widths,
            border_colors:
              Map.get(style, :border_colors, edges(Map.get(style, :border_color, {0, 0, 0}))),
            border_radius: Map.get(style, :border_radius, 0.0)
          }
        ]
    end
  end

  defp tag_boxes(boxes, metadata) do
    Enum.map(boxes, &Map.merge(&1, metadata))
  end

  defp break_metadata(style) do
    %{
      break_before: Map.get(style, :break_before, :auto),
      break_after: Map.get(style, :break_after, :auto)
    }
  end

  @spec text_width(String.t(), map()) :: number()
  defp text_width(text, style) do
    text
    |> Font.text_width(text_font_face(style), Map.fetch!(style, :font_size))
    |> Kernel.+(letter_spacing_width(text, style))
  end

  defp letter_spacing_width(text, style) do
    letter_spacing = Map.get(style, :letter_spacing, 0.0)

    case letter_spacing == 0.0 do
      true -> 0.0
      false -> max(String.length(text) - 1, 0) * letter_spacing
    end
  end

  defp text_font_face(style) do
    case Map.get(style, :font_face) do
      nil ->
        {:ok, _families, font_face} =
          Font.resolve(
            Map.fetch!(style, :font_family),
            Map.fetch!(style, :font_weight),
            Map.fetch!(style, :font_style),
            %{embedded: []}
          )

        font_face

      font_face ->
        font_face
    end
  end

  defp text_style(style) do
    Map.take(style, [
      :color,
      :font_face,
      :font_families,
      :font_family,
      :font_size,
      :font_style,
      :font_weight,
      :letter_spacing,
      :line_height
    ])
  end

  defp list_marker(marker_type, index) do
    case marker_type do
      :decimal -> "#{index}."
      :disc -> "*"
    end
  end

  defp edges(value) do
    edges(value, value, value, value)
  end

  defp edges(top, right, bottom, left) do
    %{top: top, right: right, bottom: bottom, left: left}
  end

  defp page_size(page_size) do
    case page_size do
      :a4 ->
        {:ok, {595.28, 841.89}}

      :letter ->
        {:ok, {612.0, 792.0}}

      {width, height} when is_number(width) and is_number(height) and width > 0 and height > 0 ->
        {:ok, normalize_custom_page_size(width, height)}

      _ ->
        {:error, :invalid_page_size}
    end
  end

  defp normalize_custom_page_size(width, height) do
    case width <= 20 and height <= 20 do
      true -> {width * 72.0, height * 72.0}
      false -> {width * 1.0, height * 1.0}
    end
  end

  defp margin(margin) do
    case margin do
      margin when is_number(margin) and margin >= 0 ->
        {:ok, margin * 1.0}

      margin when is_binary(margin) ->
        case Regex.run(~r/^\s*(\d+(?:\.\d+)?)(pt|px|mm|cm|in)\s*$/u, margin) do
          [_, value, unit] ->
            {number, ""} = Float.parse(value)
            {:ok, number * points_per_unit(unit)}

          _ ->
            {:error, :invalid_margin}
        end

      _ ->
        {:error, :invalid_margin}
    end
  end

  defp points_per_unit(unit) do
    case unit do
      "pt" -> 1.0
      "px" -> 0.75
      "mm" -> 72.0 / 25.4
      "cm" -> 72.0 / 2.54
      "in" -> 72.0
    end
  end
end
