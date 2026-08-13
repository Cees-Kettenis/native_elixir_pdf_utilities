defmodule NativeElixirPdfUtilities.HtmlToPdf.Pagination do
  @moduledoc """
  Pagination stage for the native HTML-to-PDF renderer.

  Milestone 7 splits layout output into deterministic PDF pages with page
  margins, automatic page breaks, manual page breaks, unbroken flow units, and
  repeated table headers.
  """

  @type page :: %{size: {float(), float()}, boxes: [term()]}
  @type render_option :: NativeElixirPdfUtilities.HtmlToPdf.render_option()
  @type error_reason :: :invalid_layout

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc """
  Splits a layout tree into PDF pages.
  """
  @spec paginate(term(), [render_option()]) ::
          {:ok, [page()]} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def paginate(layout_tree, opts \\ []) do
    margins =
      case layout_tree do
        layout_tree when is_map(layout_tree) ->
          layout_tree
          |> Map.get(:margins, Map.get(layout_tree, :margin, 0.0))
          |> PageGeometry.normalize_margins()

        _ ->
          {:error, :invalid_margin}
      end

    case HtmlValidator.validate_pagination_input(layout_tree, opts, margins) do
      :ok ->
        {:ok, margins} = margins
        paginate_boxes(layout_tree.page_size, layout_tree.boxes, margins)

      {:error, {reason, diagnostic}} ->
        {:error,
         {reason, Diagnostics.with_context(diagnostic, operation: :paginate, module: __MODULE__)}}
    end
  end

  defp paginate_boxes(page_size, boxes, margins) do
    {_page_width, page_height} = page_size
    content_height = page_height - margins.top - margins.bottom

    groups =
      boxes
      |> flow_groups()
      |> Enum.flat_map(&fragment_flow_group(&1, content_height))

    headers = repeated_table_headers(groups)
    {:ok, groups_to_pages(groups, headers, page_size, margins)}
  end

  defp groups_to_pages(groups, headers, page_size, margins) do
    {_width, page_height} = page_size
    content_top = page_height - margins.top

    initial_state = %{
      pages: [],
      current_boxes: [],
      current_y: content_top,
      previous_bottom: nil
    }

    final_state =
      Enum.reduce(groups, initial_state, fn group, state ->
        place_group(group, state, headers, page_size, margins)
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

  defp place_group(group, state, headers, page_size, margins) do
    state =
      case Map.get(group, :break_before) do
        :page -> page_break(state, page_size, margins)
        _ -> state
      end

    gap = vertical_gap(state.previous_bottom, group.top)
    target_top = target_group_top(state, group, gap)
    group_bottom = target_top - group.height

    state =
      case state.current_boxes != [] and group.height > 0 and group_bottom < margins.bottom do
        true ->
          state
          |> page_break(page_size, margins)
          |> repeat_table_header(group, headers, margins)

        false ->
          %{state | current_y: target_top}
      end

    target_top = state.current_y

    shifted_boxes =
      group.boxes
      |> Enum.reject(&page_break_box?/1)
      |> shift_boxes(target_top - group.top)

    state =
      case shifted_boxes do
        [] ->
          state

        _ ->
          %{
            state
            | current_boxes: state.current_boxes ++ shifted_boxes,
              current_y: target_top - group.height,
              previous_bottom: group.bottom
          }
      end

    case Map.get(group, :break_after) do
      :page -> page_break(state, page_size, margins)
      _ -> state
    end
  end

  defp repeat_table_header(state, group, headers, margins) do
    case {Map.get(group, :table_id), Map.get(group, :table_section)} do
      {table_id, section} when not is_nil(table_id) and section != :head ->
        case Map.get(headers, table_id) do
          nil ->
            state

          header ->
            case state.current_y - header.height - group.height >= margins.bottom do
              true ->
                shifted_boxes = shift_boxes(header.boxes, state.current_y - header.top)

                %{
                  state
                  | current_boxes: state.current_boxes ++ shifted_boxes,
                    current_y: state.current_y - header.height,
                    previous_bottom: header.bottom
                }

              false ->
                state
            end
        end

      _ ->
        state
    end
  end

  defp page_break(state, page_size, margins) do
    {_width, page_height} = page_size
    content_top = page_height - margins.top

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

  defp chunk_box(box, chunk) do
    case chunk do
      [] ->
        {:cont, [box]}

      [previous | _rest] ->
        case Map.get(box, :flow_id, box) == Map.get(previous, :flow_id, previous) do
          true -> {:cont, [box | chunk]}
          false -> {:cont, Enum.reverse(chunk), [box]}
        end
    end
  end

  defp finish_chunk(chunk) do
    case chunk do
      [] -> {:cont, []}
      chunk -> {:cont, Enum.reverse(chunk), []}
    end
  end

  defp flow_group(boxes) do
    bounds = Enum.map(boxes, &PageGeometry.box_vertical_bounds/1)
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
      break_inside: Map.get(first, :break_inside, :auto),
      table_id: Map.get(first, :table_id),
      table_section: Map.get(first, :table_section),
      repeat_table_header: Map.get(first, :repeat_table_header, false)
    }
  end

  defp fragment_flow_group(group, content_height) do
    fragmentable? = Enum.any?(group.boxes, &Map.has_key?(&1, :fragment_id))

    case fragmentable? and
           (group.break_inside == :auto or group.height > content_height) do
      true ->
        group.boxes
        |> Enum.chunk_by(&Map.get(&1, :fragment_id))
        |> Enum.map(&flow_group/1)

      false ->
        [group]
    end
  end

  defp page_break_box?(box) do
    case box do
      %{type: :page_break} -> true
      _ -> false
    end
  end

  defp repeated_table_headers(groups) do
    groups
    |> Enum.filter(&(&1.repeat_table_header == true and not is_nil(&1.table_id)))
    |> Enum.group_by(& &1.table_id)
    |> Map.new(fn {table_id, header_groups} ->
      top = header_groups |> Enum.map(& &1.top) |> Enum.max()
      bottom = header_groups |> Enum.map(& &1.bottom) |> Enum.min()

      header = %{
        boxes: Enum.flat_map(header_groups, & &1.boxes),
        top: top,
        bottom: bottom,
        height: top - bottom
      }

      {table_id, header}
    end)
  end

  defp shift_boxes(boxes, delta_y) do
    Enum.map(boxes, fn box ->
      case Map.get(box, :y) do
        y when is_number(y) -> Map.put(box, :y, y + delta_y)
        _ -> box
      end
    end)
  end

  defp vertical_gap(previous_bottom, top) do
    case previous_bottom do
      nil -> 0.0
      previous_bottom -> max(previous_bottom - top, 0.0)
    end
  end

  defp target_group_top(state, group, gap) do
    target_top = state.current_y - gap

    cond do
      state.current_boxes != [] and group.top > state.current_y ->
        group.top

      state.current_boxes == [] and state.pages == [] and group.top < target_top ->
        group.top

      true ->
        target_top
    end
  end
end
