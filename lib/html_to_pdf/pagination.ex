defmodule NativeElixirPdfUtilities.HtmlToPdf.Pagination do
  @moduledoc """
  Pagination stage for the native HTML-to-PDF renderer.

  Milestone 7 splits layout output into deterministic PDF pages with page
  margins, automatic page breaks, manual page breaks, unbroken flow units, and
  repeated table headers.
  """

  @type page :: %{size: {float(), float()}, boxes: [term()]}
  @type render_option :: NativeElixirPdfUtilities.HtmlToPdf.render_option()

  @doc """
  Splits a layout tree into PDF pages.
  """
  @spec paginate(term(), [render_option()]) :: {:ok, [page()]} | {:error, :invalid_layout}
  def paginate(layout_tree, opts \\ []) do
    case {layout_tree, opts} do
      {%{type: :layout, page_size: page_size, boxes: boxes}, opts}
      when is_list(boxes) and is_list(opts) ->
        paginate_boxes(layout_tree, page_size, boxes)

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp paginate_boxes(layout_tree, page_size, boxes) do
    margin = Map.get(layout_tree, :margin, 0.0)

    case {valid_page_size?(page_size), is_number(margin) and margin >= 0} do
      {true, true} ->
        groups = flow_groups(boxes)
        headers = repeated_table_headers(groups)
        {:ok, groups_to_pages(groups, headers, page_size, margin)}

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp groups_to_pages(groups, headers, page_size, margin) do
    {_width, page_height} = page_size
    content_top = page_height - margin

    initial_state = %{
      pages: [],
      current_boxes: [],
      current_y: content_top,
      previous_bottom: nil
    }

    final_state =
      Enum.reduce(groups, initial_state, fn group, state ->
        place_group(group, state, headers, page_size, margin)
      end)

    pages =
      final_state.pages
      |> Kernel.++([final_state.current_boxes])
      |> Enum.reject(&(&1 == []))
      |> Enum.map(&%{size: page_size, boxes: &1})

    case pages do
      [] -> [%{size: page_size, boxes: []}]
      pages -> pages
    end
  end

  defp place_group(group, state, headers, page_size, margin) do
    state =
      case Map.get(group, :break_before) do
        :page -> page_break(state, page_size, margin)
        _ -> state
      end

    gap = vertical_gap(state.previous_bottom, group.top)
    target_top = state.current_y - gap
    group_bottom = target_top - group.height

    state =
      case state.current_boxes != [] and group_bottom < margin do
        true ->
          state
          |> page_break(page_size, margin)
          |> repeat_table_header(group, headers)

        false ->
          %{state | current_y: target_top}
      end

    target_top = state.current_y
    shifted_boxes = shift_boxes(group.boxes, target_top - group.top)

    state = %{
      state
      | current_boxes: state.current_boxes ++ shifted_boxes,
        current_y: target_top - group.height,
        previous_bottom: group.bottom
    }

    case Map.get(group, :break_after) do
      :page -> page_break(state, page_size, margin)
      _ -> state
    end
  end

  defp repeat_table_header(state, group, headers) do
    case {Map.get(group, :table_id), Map.get(group, :table_section)} do
      {table_id, section} when not is_nil(table_id) and section != :head ->
        case Map.get(headers, table_id) do
          nil ->
            state

          header ->
            shifted_boxes = shift_boxes(header.boxes, state.current_y - header.top)

            %{
              state
              | current_boxes: state.current_boxes ++ shifted_boxes,
                current_y: state.current_y - header.height,
                previous_bottom: header.bottom
            }
        end

      _ ->
        state
    end
  end

  defp page_break(state, page_size, margin) do
    {_width, page_height} = page_size
    content_top = page_height - margin

    pages =
      case state.current_boxes do
        [] -> state.pages
        boxes -> state.pages ++ [boxes]
      end

    %{state | pages: pages, current_boxes: [], current_y: content_top, previous_bottom: nil}
  end

  defp flow_groups(boxes) do
    boxes
    |> Enum.chunk_while([], &chunk_box/2, &finish_chunk/1)
    |> Enum.map(&flow_group/1)
  end

  defp chunk_box(box, []) do
    {:cont, [box]}
  end

  defp chunk_box(box, [previous | _rest] = chunk) do
    case Map.get(box, :flow_id, box) == Map.get(previous, :flow_id, previous) do
      true -> {:cont, [box | chunk]}
      false -> {:cont, Enum.reverse(chunk), [box]}
    end
  end

  defp finish_chunk([]) do
    {:cont, []}
  end

  defp finish_chunk(chunk) do
    {:cont, Enum.reverse(chunk), []}
  end

  defp flow_group(boxes) do
    bounds = Enum.map(boxes, &box_bounds/1)
    top = bounds |> Enum.map(&elem(&1, 0)) |> Enum.max()
    bottom = bounds |> Enum.map(&elem(&1, 1)) |> Enum.min()
    first = hd(boxes)

    %{
      boxes: boxes,
      top: top,
      bottom: bottom,
      height: top - bottom,
      break_before: Map.get(first, :break_before, :auto),
      break_after: Map.get(first, :break_after, :auto),
      table_id: Map.get(first, :table_id),
      table_section: Map.get(first, :table_section),
      repeat_table_header: Map.get(first, :repeat_table_header, false)
    }
  end

  defp box_bounds(box) do
    case box do
      %{type: :rect, y: y, height: height} when is_number(y) and is_number(height) ->
        {y + height, y}

      %{type: :text, y: y, font_size: font_size} when is_number(y) and is_number(font_size) ->
        {y + font_size, y}

      %{type: :text, y: y, line_height: line_height}
      when is_number(y) and is_number(line_height) ->
        {y + line_height, y}

      _ ->
        {0.0, 0.0}
    end
  end

  defp repeated_table_headers(groups) do
    groups
    |> Enum.filter(&(&1.repeat_table_header == true and not is_nil(&1.table_id)))
    |> Map.new(&{&1.table_id, &1})
  end

  defp shift_boxes(boxes, delta_y) do
    Enum.map(boxes, fn box ->
      case Map.get(box, :y) do
        y when is_number(y) -> Map.put(box, :y, y + delta_y)
        _ -> box
      end
    end)
  end

  defp vertical_gap(nil, _top) do
    0.0
  end

  defp vertical_gap(previous_bottom, top) do
    max(previous_bottom - top, 0.0)
  end

  defp valid_page_size?(page_size) do
    case page_size do
      {width, height} when is_number(width) and is_number(height) and width > 0 and height > 0 ->
        true

      _ ->
        false
    end
  end
end
