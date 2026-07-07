defmodule NativeElixirPdfUtilities.HtmlToPdf.Layout do
  @moduledoc """
  Layout engine for the native HTML-to-PDF renderer.

  Milestone 11 lays out block text elements, inline text runs, basic block box
  styling, lists, link annotation bounds, deterministic one-page tables, and a
  documented single-line-text flexbox and grid subset with image boxes and
  pagination metadata.
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
             {:ok, margin} <- margin(Keyword.get(opts, :margin, "20mm")),
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
        with {:ok, runs} <- inline_runs(children) do
          margin =
            Map.get(style, :margin, edges(0.0, 0.0, Map.get(style, :margin_after, 0.0), 0.0))

          padding = Map.get(style, :padding, edges(0.0))
          border_widths = Map.get(style, :border_widths, edges(0.0))
          box_x = x + margin.left
          box_top = y - margin.top
          box_width = width - margin.left - margin.right
          line_height = Map.fetch!(style, :line_height)

          box_height =
            border_widths.top + padding.top + line_height + padding.bottom + border_widths.bottom

          baseline_y = box_top - border_widths.top - padding.top - Map.fetch!(style, :font_size)
          content_x = box_x + border_widths.left + padding.left

          content_width =
            box_width - border_widths.left - padding.left - padding.right - border_widths.right

          flow_metadata =
            style
            |> break_metadata()
            |> Map.put(:flow_id, {:block, box_x, box_top})

          background_box =
            style
            |> background_box(box_x, box_top - box_height, box_width, box_height)
            |> tag_boxes(flow_metadata)

          {boxes, _next_x} =
            Enum.reduce(runs, {background_box, content_x}, fn run, {acc, current_x} ->
              box =
                run |> text_box(current_x, baseline_y, content_width) |> Map.merge(flow_metadata)

              {acc ++ [box], current_x + text_width(run.text, run.style)}
            end)

          next_y = box_top - box_height - margin.bottom
          {:ok, boxes, next_y}
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

  defp layout_image(style, x, y, _width) do
    margin = Map.get(style, :margin, edges(0.0, 0.0, Map.get(style, :margin_after, 0.0), 0.0))
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))
    {content_width, content_height} = image_content_size(style)
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
    image = Map.fetch!(style, :image)
    natural_width = Map.fetch!(image, :width)
    natural_height = Map.fetch!(image, :height)

    case {Map.get(style, :width), Map.get(style, :height)} do
      {width, height} when is_number(width) and is_number(height) ->
        {width, height}

      {width, _height} when is_number(width) ->
        {width, width * natural_height / natural_width}

      {_width, height} when is_number(height) ->
        {height * natural_width / natural_height, height}

      _ ->
        {natural_width, natural_height}
    end
  end

  defp layout_grid(style, children, x, y, width) do
    margin = Map.get(style, :margin, edges(0.0, 0.0, Map.get(style, :margin_after, 0.0), 0.0))
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))
    available_box_width = width - margin.left - margin.right
    content_width = Map.get(style, :width, available_box_width - horizontal_box_size(style))

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

      row_intrinsics = grid_row_intrinsics(placed_items, row_count)

      row_sizes =
        resolve_grid_rows(
          row_tracks,
          row_intrinsics,
          grid_row_gap(style),
          Map.get(style, :height)
        )

      content_height = Map.get(style, :height, grid_tracks_size(row_sizes, grid_row_gap(style)))

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

      item_boxes =
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
        )

      next_y = box_top - box_height - margin.bottom
      {:ok, background_box ++ item_boxes, next_y}
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
          {:error, reason} -> {:error, reason}
        end

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp build_grid_item(style, runs, index) do
    margin = Map.get(style, :margin, edges(0.0))
    text_width = Enum.reduce(runs, 0.0, fn run, acc -> acc + text_width(run.text, run.style) end)
    line_height = Map.get(style, :line_height, 14.4)

    content_width = Map.get(style, :width, text_width)
    content_height = Map.get(style, :height, line_height)

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

  defp resolve_grid_rows(tracks, row_intrinsics, gap, available_height) do
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

    items
    |> Enum.flat_map(fn item ->
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

      item
      |> flex_item_boxes(:row)
      |> tag_boxes(%{flow_id: {:grid_item, container_flow_id, item.index}})
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
    justify = Map.get(container_style, :justify_items, :stretch)
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
    content_width = Map.get(style, :width, available_box_width - horizontal_box_size(style))

    box_width =
      content_width + border_widths.left + padding.left + padding.right + border_widths.right

    box_x = x + margin.left
    box_top = y - margin.top
    content_x = box_x + border_widths.left + padding.left
    content_top = box_top - border_widths.top - padding.top

    with {:ok, items} <- flex_items(children, flex_main_axis(style)) do
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

          item_boxes =
            flex_line_boxes(
              lines,
              style,
              content_x,
              content_top,
              content_width,
              content_height,
              flow_metadata.flow_id
            )

          next_y = box_top - box_height - margin.bottom
          {:ok, background_box ++ item_boxes, next_y}
      end
    end
  end

  defp flex_items(children, main_axis) do
    children
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn {child, index}, {:ok, acc} ->
      case flex_item(child, index, main_axis) do
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

  defp flex_item(child, index, main_axis) do
    case child do
      %{type: :text, text: text} when is_binary(text) ->
        case String.trim(text) do
          "" ->
            {:ok, nil}

          _ ->
            style = child.style |> text_style() |> Map.put(:display, :inline)
            runs = [%{text: text, style: child.style}]
            {:ok, build_flex_item(style, runs, index, main_axis)}
        end

      %{type: :element, style: %{display: :none}} ->
        {:ok, nil}

      %{type: :element, style: %{display: :image} = style} ->
        {:ok, build_flex_image_item(style, index, main_axis)}

      %{type: :element, style: style, children: children} when is_list(children) ->
        case inline_runs(children) do
          {:ok, runs} -> {:ok, build_flex_item(style, runs, index, main_axis)}
          {:error, reason} -> {:error, reason}
        end

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp build_flex_item(style, runs, index, main_axis) do
    margin = Map.get(style, :margin, edges(0.0))
    text_width = Enum.reduce(runs, 0.0, fn run, acc -> acc + text_width(run.text, run.style) end)
    line_height = Map.get(style, :line_height, 14.4)

    {content_main, content_cross} =
      case main_axis do
        :row ->
          {flex_basis(style, :width, text_width), Map.get(style, :height, line_height)}

        :column ->
          {flex_basis(style, :height, line_height), Map.get(style, :width, text_width)}
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

  defp build_flex_image_item(style, index, main_axis) do
    margin = Map.get(style, :margin, edges(0.0))
    {content_width, content_height} = image_content_size(style)

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

  defp flex_basis(style, size_property, intrinsic_size) do
    case Map.get(style, :flex_basis, :auto) do
      :auto -> Map.get(style, size_property, intrinsic_size)
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

    available_main =
      flex_resolved_available_main(available_main, base_without_gap, item_gap_total)

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

    lines
    |> Enum.flat_map(fn item ->
      item
      |> flex_item_boxes(main_axis)
      |> tag_boxes(%{flow_id: {:flex_item, container_flow_id, item.index}})
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
        flex_text_item_boxes(item, main_axis, style, padding, border_widths)

      image ->
        flex_image_item_boxes(item, main_axis, style, padding, border_widths, image)
    end
  end

  defp flex_text_item_boxes(item, main_axis, style, padding, border_widths) do
    baseline_y = item.y - border_widths.top - padding.top - Map.fetch!(style, :font_size)
    content_x = item.x + border_widths.left + padding.left

    content_width =
      item.box_width - border_widths.left - padding.left - padding.right - border_widths.right

    start_x = aligned_text_x(item.runs, style, content_x, max(content_width, 0.0))

    background_boxes =
      background_box(style, item.x, item.y - item.box_height, item.box_width, item.box_height)

    {text_boxes, _next_x} =
      Enum.reduce(item.runs, {[], start_x}, fn run, {acc, current_x} ->
        box = text_box(run, current_x, baseline_y, max(content_width, 0.0))
        {acc ++ [box], current_x + text_width(run.text, run.style)}
      end)

    case main_axis do
      :row -> background_boxes ++ text_boxes
      :column -> background_boxes ++ text_boxes
    end
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

    case main_axis do
      :row -> background_boxes ++ [image_box]
      :column -> background_boxes ++ [image_box]
    end
  end

  defp flex_content_height(lines, style) do
    main_axis = flex_main_axis(style)

    case {main_axis, Map.get(style, :height)} do
      {:row, height} when is_number(height) ->
        height

      {:row, _height} ->
        cross_gap = flex_cross_gap(style)
        Enum.reduce(lines, 0.0, &(&1.cross + &2)) + cross_gap * max(length(lines) - 1, 0)

      {:column, height} when is_number(height) ->
        height

      {:column, _height} ->
        case lines do
          [line] -> line.main
          _ -> Enum.map(lines, & &1.main) |> Enum.max(fn -> 0.0 end)
        end
    end
  end

  defp flex_line_cross(line, style, line_count) do
    case {flex_main_axis(style), Map.get(style, :height), line_count} do
      {:row, height, 1} when is_number(height) -> height
      {:row, _height, _line_count} -> line.cross
      {:column, _height, _line_count} -> Map.get(style, :width, line.cross)
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
        case Map.get(style, :height) do
          height when is_number(height) ->
            height

          _ ->
            Enum.reduce(items, 0.0, &(&1.outer_main + &2)) + gap * max(length(items) - 1, 0)
        end
    end
  end

  defp flex_resolved_available_main(available_main, base_without_gap, item_gap_total) do
    max(available_main, base_without_gap + item_gap_total)
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
    box_width = width - margin.left - margin.right
    content_x = box_x + border_widths.left + padding.left
    content_top = box_top - border_widths.top - padding.top

    content_width =
      box_width - border_widths.left - padding.left - padding.right - border_widths.right

    table_id = {:table, content_x, content_top}
    table_metadata = break_metadata(style)

    with {:ok, caption_boxes, rows_top} <-
           layout_table_caption(children, content_x, content_top, content_width, table_id),
         {:ok, rows} <- table_rows(children),
         {:ok, row_boxes, content_bottom} <-
           layout_table_rows(rows, content_x, rows_top, content_width, table_id) do
      content_height = content_top - content_bottom

      box_height =
        border_widths.top + padding.top + content_height + padding.bottom + border_widths.bottom

      table_box =
        style
        |> background_box(box_x, box_top - box_height, box_width, box_height)
        |> tag_boxes(Map.merge(table_metadata, %{flow_id: table_id, table_id: table_id}))

      next_y = box_top - box_height - margin.bottom
      {:ok, table_box ++ caption_boxes ++ row_boxes, next_y}
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
          line_height = Map.fetch!(style, :line_height)

          box_height =
            border_widths.top + padding.top + line_height + padding.bottom + border_widths.bottom

          content_x = box_x + border_widths.left + padding.left

          content_width =
            box_width - border_widths.left - padding.left - padding.right - border_widths.right

          baseline_y = box_top - border_widths.top - padding.top - Map.fetch!(style, :font_size)
          start_x = aligned_text_x(runs, style, content_x, content_width)

          flow_metadata = %{flow_id: {:table_caption, table_id}, table_id: table_id}

          background_box =
            style
            |> background_box(box_x, box_top - box_height, box_width, box_height)
            |> tag_boxes(flow_metadata)

          {text_boxes, _next_x} =
            Enum.reduce(runs, {[], start_x}, fn run, {acc, current_x} ->
              box =
                run |> text_box(current_x, baseline_y, content_width) |> Map.merge(flow_metadata)

              {acc ++ [box], current_x + text_width(run.text, run.style)}
            end)

          {:ok, background_box ++ text_boxes, box_top - box_height - margin.bottom}
        end

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp table_rows(children) do
    result =
      Enum.reduce(children, {:ok, []}, fn child, acc ->
        case acc do
          {:ok, rows} ->
            case child do
              %{style: %{display: :table_caption}} ->
                {:ok, rows}

              %{style: %{display: :table_row}, children: cells} when is_list(cells) ->
                {:ok, rows ++ [%{row: child, section: :body}]}

              %{style: %{display: :table_row_group}, children: group_rows}
              when is_list(group_rows) ->
                case Enum.all?(group_rows, &match?(%{style: %{display: :table_row}}, &1)) do
                  true ->
                    section = child.style |> Map.get(:table_section, :body)
                    table_rows = Enum.map(group_rows, &%{row: &1, section: section})
                    {:ok, rows ++ table_rows}

                  false ->
                    {:error, :invalid_layout}
                end

              _ ->
                {:error, :invalid_layout}
            end

          {:error, reason} ->
            {:error, reason}
        end
      end)

    case result do
      {:ok, rows} when rows != [] -> {:ok, rows}
      _ -> {:error, :invalid_layout}
    end
  end

  defp layout_table_rows(rows, x, y, width, table_id) do
    column_count =
      rows
      |> Enum.map(fn %{row: %{children: cells}} -> length(cells) end)
      |> Enum.max(fn -> 0 end)

    case column_count do
      count when count > 0 ->
        column_width = width / count

        result =
          rows
          |> Enum.with_index()
          |> Enum.reduce({:ok, [], y}, fn {%{row: row, section: section}, index}, acc ->
            case acc do
              {:ok, boxes, current_y} ->
                case layout_table_row(row, section, table_id, index, x, current_y, column_width) do
                  {:ok, row_boxes, next_y} -> {:ok, boxes ++ row_boxes, next_y}
                  {:error, reason} -> {:error, reason}
                end

              {:error, reason} ->
                {:error, reason}
            end
          end)

        case result do
          {:ok, boxes, next_y} -> {:ok, boxes, next_y}
          {:error, reason} -> {:error, reason}
        end

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp layout_table_row(row, section, table_id, index, x, y, column_width) do
    case row do
      %{style: %{display: :table_row}, children: cells} when is_list(cells) ->
        case Enum.all?(cells, &match?(%{style: %{display: :table_cell}}, &1)) do
          true ->
            row_height = cells |> Enum.map(&table_cell_height/1) |> Enum.max()
            row_metadata = table_row_metadata(table_id, section, index)

            result =
              Enum.reduce_while(cells, {:ok, [], 0}, fn cell, {:ok, acc, index} ->
                cell_x = x + column_width * index

                case layout_table_cell(cell, cell_x, y, column_width, row_height, row_metadata) do
                  {:ok, cell_boxes} -> {:cont, {:ok, acc ++ cell_boxes, index + 1}}
                  {:error, reason} -> {:halt, {:error, reason}}
                end
              end)

            case result do
              {:ok, boxes, _index} -> {:ok, boxes, y - row_height}
              {:error, reason} -> {:error, reason}
            end

          false ->
            {:error, :invalid_layout}
        end

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp layout_table_cell(cell, x, y, width, height, row_metadata) do
    case cell do
      %{style: %{display: :table_cell} = style, children: children} when is_list(children) ->
        with {:ok, runs} <- inline_runs(children) do
          padding = Map.get(style, :padding, edges(0.0))
          border_widths = Map.get(style, :border_widths, edges(0.0))
          content_x = x + border_widths.left + padding.left

          content_width =
            width - border_widths.left - padding.left - padding.right - border_widths.right

          baseline_y = y - border_widths.top - padding.top - Map.fetch!(style, :font_size)
          start_x = aligned_text_x(runs, style, content_x, content_width)

          cell_box =
            style
            |> background_box(x, y - height, width, height)
            |> tag_boxes(row_metadata)

          {text_boxes, _next_x} =
            Enum.reduce(runs, {[], start_x}, fn run, {acc, current_x} ->
              box =
                run |> text_box(current_x, baseline_y, content_width) |> Map.merge(row_metadata)

              {acc ++ [box], current_x + text_width(run.text, run.style)}
            end)

          {:ok, cell_box ++ text_boxes}
        end

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp table_cell_height(%{style: style}) do
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))

    border_widths.top + padding.top + Map.fetch!(style, :line_height) + padding.bottom +
      border_widths.bottom
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
      |> Enum.reduce({:ok, [], y}, fn {child, index}, acc ->
        case acc do
          {:ok, boxes, current_y} ->
            case layout_list_item(child, marker_type, index, x, current_y, width) do
              {:ok, item_boxes, next_y} -> {:ok, boxes ++ item_boxes, next_y}
              {:error, reason} -> {:error, reason}
            end

          {:error, reason} ->
            {:error, reason}
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

          {text_boxes, _next_x} =
            Enum.reduce(runs, {[], text_x}, fn run, {acc, current_x} ->
              box = run |> text_box(current_x, baseline_y, text_width) |> Map.merge(flow_metadata)
              {acc ++ [box], current_x + text_width(run.text, run.style)}
            end)

          margin =
            Map.get(style, :margin, edges(0.0, 0.0, Map.get(style, :margin_after, 0.0), 0.0))

          next_y = y - Map.fetch!(style, :line_height) - margin.bottom
          {:ok, [marker_box] ++ text_boxes, next_y}
        end

      _ ->
        {:error, :invalid_layout}
    end
  end

  @spec inline_runs([term()]) ::
          {:ok, [%{text: String.t(), style: map()}]} | {:error, :invalid_layout}
  defp inline_runs(children) do
    Enum.reduce(children, {:ok, []}, fn child, acc ->
      case acc do
        {:ok, runs} ->
          append_inline_run(child, runs)

        {:error, reason} ->
          {:error, reason}
      end
    end)
  end

  defp append_inline_run(child, runs) do
    case child do
      %{type: :text, text: text, style: style} when is_binary(text) and is_map(style) ->
        {:ok, runs ++ [%{text: text, style: style}]}

      %{type: :element, style: %{display: :inline}, children: children}
      when is_list(children) ->
        case inline_runs(children) do
          {:ok, child_runs} -> {:ok, runs ++ child_runs}
          {:error, reason} -> {:error, reason}
        end

      _ ->
        {:error, :invalid_layout}
    end
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
    Font.text_width(text, text_font_face(style), Map.fetch!(style, :font_size))
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
        {:ok, {width * 1.0, height * 1.0}}

      _ ->
        {:error, :invalid_page_size}
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
