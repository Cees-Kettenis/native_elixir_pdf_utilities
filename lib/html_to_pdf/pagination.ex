defmodule NativeElixirPdfUtilities.HtmlToPdf.Pagination do
  @moduledoc """
  Pagination stage for the native HTML-to-PDF renderer.

  Milestone 7 splits layout output into deterministic PDF pages with page
  margins, automatic page breaks, manual page breaks, unbroken flow units, and
  repeated table headers.
  """

  @type page :: %{size: {float(), float()}, boxes: [term()]}
  @type render_option :: NativeElixirPdfUtilities.HtmlToPdf.render_option()
  @type error_reason :: :invalid_layout | :unsupported_html | :resource_limit_exceeded

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc """
  Splits a layout tree into PDF pages.
  """
  @spec paginate(term(), [render_option()]) ::
          {:ok, [page()]} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def paginate(layout_tree, opts \\ []) do
    HtmlValidator.with_render_budget(fn ->
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

          paginate_boxes(
            layout_tree.page_size,
            layout_tree.boxes,
            margins,
            Map.get(layout_tree, :link_scope),
            Map.get(layout_tree, :link_expected_targets, []),
            Map.get(layout_tree, :link_document_targets, [])
          )

        {:error, {reason, diagnostic}} ->
          {:error,
           {reason,
            Diagnostics.with_context(diagnostic, operation: :paginate, module: __MODULE__)}}
      end
    end)
  end

  defp paginate_boxes(page_size, boxes, margins, link_scope, expected_targets, document_targets) do
    {_page_width, page_height} = page_size
    content_height = page_height - margins.top - margins.bottom
    pagination_order = make_ref()

    boxes =
      boxes
      |> Enum.with_index()
      |> Enum.map(fn {box, index} -> Map.put(box, pagination_order, index) end)

    {canvas_boxes, boxes} = Enum.split_with(boxes, &(Map.get(&1, :position_anchor) == :canvas))

    {root_positioned_boxes, flow_boxes} =
      Enum.split_with(boxes, &(Map.get(&1, :position_anchor) == :root))

    fieldset_paints = Enum.filter(flow_boxes, &Map.has_key?(&1, :fieldset_paint_id))

    flow_boxes =
      if fieldset_paints == [] do
        flow_boxes
      else
        Enum.map(flow_boxes, fn box ->
          case Map.get(box, :fieldset_paint_id) do
            nil ->
              if Map.has_key?(box, :fieldset_ancestors),
                do: Map.put(box, :fieldset_source_y, box.y),
                else: box

            _ ->
              top = box.y + box.fieldset_bounds.top

              box
              |> Map.put(:type, :flow_marker)
              |> Map.put(:width, 0.0)
              |> Map.put(:height, 0.0)
              |> Map.put(:y, top)
              |> Map.put(:fieldset_source_y, top)
          end
        end)
      end

    groups =
      flow_boxes
      |> flow_groups()
      |> Enum.flat_map(&fragment_flow_group(&1, content_height))

    headers = repeated_table_headers(groups)

    pages =
      groups
      |> keep_table_groups(headers, content_height)
      |> groups_to_pages(headers, page_size, margins)
      |> close_table_fragments()
      |> paint_fieldset_fragments(fieldset_paints, margins, pagination_order)
      |> restore_root_positioned_boxes(root_positioned_boxes, pagination_order)
      |> paint_table_outlines()
      |> Enum.map(fn page ->
        background = Enum.map(canvas_boxes, &Map.delete(&1, pagination_order))
        HtmlValidator.reserve_render_resource(:max_layout_boxes, length(background), :pagination)
        %{page | boxes: background ++ page.boxes}
      end)

    NativeElixirPdfUtilities.Validators.HtmlLinkValidator.resolve_pages(
      pages,
      link_scope,
      expected_targets,
      document_targets
    )
  end

  # Fieldset decoration is sliced at page boundaries, while legend artwork and
  # the top notch occur only in the first fragment. Keep authored backgrounds
  # behind the descendants on every page without making them pagination items.
  defp paint_fieldset_fragments(pages, paints, margins, pagination_order) do
    case paints do
      [] ->
        pages

      _ ->
        membership =
          pages
          |> Enum.with_index()
          |> Enum.reduce(%{}, fn {page, index}, membership ->
            Enum.reduce(page.boxes, membership, fn box, membership ->
              Enum.reduce(Map.get(box, :fieldset_ancestors, []), membership, fn id, membership ->
                Map.update(membership, id, {index, index}, fn {first, _last} -> {first, index} end)
              end)
            end)
          end)

        paints = Enum.group_by(paints, & &1.fieldset_paint_id)

        pages
        |> Enum.with_index()
        |> Enum.map(fn {page, index} ->
          members =
            Enum.reduce(page.boxes, %{}, fn box, members ->
              Enum.reduce(Map.get(box, :fieldset_ancestors, []), members, fn id, members ->
                Map.update(members, id, [box], &[box | &1])
              end)
            end)

          decorations =
            Enum.flat_map(members, fn {id, reversed} ->
              {first_page, last_page} = Map.fetch!(membership, id)
              [first_member | _] = members = Enum.reverse(reversed)
              last_member = List.last(members)
              first_delta = first_member.y - first_member.fieldset_source_y
              last_delta = last_member.y - last_member.fieldset_source_y

              Enum.map(Map.fetch!(paints, id), fn paint ->
                top =
                  if index == first_page,
                    do: paint.y + paint.fieldset_bounds.top + first_delta,
                    else: elem(page.size, 1) - margins.top

                bottom =
                  if index == last_page,
                    do: paint.y + paint.fieldset_bounds.bottom + last_delta,
                    else: margins.bottom

                HtmlValidator.reserve_render_resource(:max_layout_boxes, 1, :pagination)

                case paint.type do
                  :rect ->
                    widths =
                      Map.get(paint, :border_widths, %{
                        top: 0.0,
                        right: 0.0,
                        bottom: 0.0,
                        left: 0.0
                      })

                    widths = %{
                      widths
                      | top: if(index == first_page, do: widths.top, else: 0.0),
                        bottom: if(index == last_page, do: widths.bottom, else: 0.0)
                    }

                    paint = %{
                      paint
                      | y: bottom,
                        height: max(top - bottom, 0.0),
                        border_widths: widths,
                        stroke_width: Enum.max(Map.values(widths))
                    }

                    if index == first_page, do: paint, else: Map.delete(paint, :border_cutout)

                  :image ->
                    delta = first_delta

                    clip = paint.clip

                    paint
                    |> Map.put(:y, paint.y + delta)
                    |> Map.put(:clip, %{clip | y: bottom, height: max(top - bottom, 0.0)})
                end
              end)
            end)

          boxes = Enum.reject(page.boxes, &Map.has_key?(&1, :fieldset_paint_id))
          %{page | boxes: Enum.sort_by(decorations ++ boxes, &Map.fetch!(&1, pagination_order))}
        end)
    end
  end

  defp paint_table_outlines(pages) do
    Enum.map(pages, fn page ->
      outlines =
        page.boxes
        |> Enum.filter(&(Map.get(&1, :role) == :table_outline))
        |> Enum.group_by(& &1.table_paint_id)

      boxes =
        Enum.reduce(outlines, page.boxes, fn {table_id, pieces}, boxes ->
          top = pieces |> Enum.map(&(&1.y + &1.height)) |> Enum.max()
          bottom = pieces |> Enum.map(& &1.y) |> Enum.min()
          outline = %{hd(pieces) | y: bottom, height: top - bottom}

          {borders, boxes} =
            Enum.split_with(boxes, fn box ->
              Map.get(box, :table_paint_id) == table_id and Map.get(box, :role) == :table_border
            end)

          last_piece = List.last(pieces)

          Enum.flat_map(boxes, fn box ->
            cond do
              box == last_piece ->
                [outline | borders]

              Map.get(box, :table_paint_id) == table_id and Map.get(box, :role) == :table_outline ->
                []

              true ->
                [box]
            end
          end)
        end)

      %{page | boxes: boxes}
    end)
  end

  # Interior collapsed borders normally belong to the following row. At a
  # physical page boundary that row is absent, so finish the visible fragment
  # with the bottom border requested by its final cells.
  defp close_table_fragments(pages) do
    Enum.map(pages, fn page ->
      bottoms =
        page.boxes
        |> Enum.filter(&Map.has_key?(&1, :fragment_bottom_border_width))
        |> Enum.reduce(%{}, fn box, acc ->
          Map.update(acc, box.table_id, box.y, &min(&1, box.y))
        end)

      boxes =
        Enum.map(page.boxes, fn box ->
          case box do
            %{fragment_bottom_border_width: width, border_widths: borders} when width > 0 ->
              if box.y == Map.fetch!(bottoms, box.table_id) and borders.bottom == 0 do
                %{
                  box
                  | border_widths: %{borders | bottom: width},
                    stroke_width: max(box.stroke_width, width)
                }
              else
                box
              end

            _ ->
              box
          end
        end)

      %{page | boxes: boxes}
    end)
  end

  defp restore_root_positioned_boxes([first_page | remaining_pages], boxes, pagination_order) do
    root_boxes = Enum.reject(boxes, &page_break_box?/1)

    first_page = %{
      first_page
      | boxes:
          (first_page.boxes ++ root_boxes)
          |> Enum.sort_by(&Map.fetch!(&1, pagination_order))
    }

    Enum.map([first_page | remaining_pages], fn page ->
      %{page | boxes: Enum.map(page.boxes, &Map.delete(&1, pagination_order))}
    end)
  end

  defp groups_to_pages(groups, headers, page_size, margins) do
    {_width, page_height} = page_size
    content_top = page_height - margins.top

    initial_state = %{
      pages: [],
      page_count: 0,
      current_boxes: [],
      current_y: content_top,
      position_delta: 0.0
    }

    final_state =
      Enum.reduce(groups, initial_state, fn group, state ->
        place_group(group, state, headers, page_size, margins)
      end)

    pages =
      final_state.pages
      |> Enum.reverse([final_state.current_boxes])
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
        :page ->
          state
          |> page_break(page_size, margins)
          |> repeat_table_header(group, headers, margins)

        _ ->
          state
      end

    target_top = target_group_top(state, group)
    group_bottom = target_top - Map.get(group, :placement_height, group.height)

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
          %{state | position_delta: target_top - group.top}

        _ ->
          HtmlValidator.check_render_resource(
            :max_rendered_pages,
            state.page_count + 1,
            :pagination
          )

          HtmlValidator.check_render_resource(:max_pdf_pages, state.page_count + 1, :pagination)

          %{
            state
            | current_boxes: state.current_boxes ++ shifted_boxes,
              current_y: target_top - group.height,
              position_delta: target_top - group.top
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
                shifted_boxes =
                  header.boxes
                  |> Enum.map(&Map.drop(&1, [:outline_anchor, :html_targets]))
                  |> shift_boxes(state.current_y - header.top)

                %{
                  state
                  | current_boxes: state.current_boxes ++ shifted_boxes,
                    current_y: state.current_y - header.height
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
        boxes -> [boxes | state.pages]
      end

    %{
      state
      | pages: pages,
        page_count: state.page_count + if(state.current_boxes == [], do: 0, else: 1),
        current_boxes: [],
        current_y: content_top,
        position_delta: nil
    }
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
    in_flow_boxes = Enum.reject(boxes, &(Map.get(&1, :out_of_flow, false) == true))

    bounds =
      case in_flow_boxes do
        [] -> Enum.map(boxes, &PageGeometry.box_vertical_bounds/1)
        in_flow_boxes -> Enum.map(in_flow_boxes, &PageGeometry.box_vertical_bounds/1)
      end

    top = bounds |> Enum.map(&elem(&1, 0)) |> Enum.max()

    bottom =
      case in_flow_boxes do
        [] -> top
        _ -> bounds |> Enum.map(&elem(&1, 1)) |> Enum.min()
      end

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
      repeat_table_header: Map.get(first, :repeat_table_header, false),
      row_group: Map.get(first, :row_group),
      row_group_avoid: Map.get(first, :row_group_avoid, false)
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
        |> then(fn fragments ->
          first = hd(group.boxes)
          orphans = Map.get(first, :orphans, 2)
          widows = Map.get(first, :widows, 2)
          count = length(fragments)

          # A fitting paragraph shorter than both reservations cannot split
          # legally. Larger paragraphs reserve their beginning and final lines.
          fragments
          |> Enum.with_index()
          |> Enum.map(fn {fragment, index} ->
            last_index =
              cond do
                index == 0 and count < orphans + widows -> count - 1
                index == 0 -> min(orphans, count) - 1
                index == max(count - widows, 0) -> count - 1
                true -> index
              end

            # Ordinary lines already carry their extent. Only the first/final
            # multi-line reservations traverse the list, at most twice per flow.
            height =
              if last_index == index,
                do: fragment.height,
                else: fragment.top - Enum.at(fragments, last_index).bottom

            # If a reservation exceeds a complete page, CSS break restrictions
            # must relax to make progress. Ordinary oversized paragraphs still
            # keep every fitting first/final reservation.
            if height <= content_height,
              do: Map.put(fragment, :placement_height, height),
              else: fragment
          end)
        end)

      false ->
        [group]
    end
  end

  # Reserve a fitting semantic row group before placing its first row. Oversized
  # groups remain individual rows, leaving repeated headings room on each page.
  defp keep_table_groups(groups, headers, content_height) do
    groups =
      groups
      |> Enum.chunk_by(fn group ->
        if group.row_group_avoid, do: group.row_group, else: make_ref()
      end)
      |> Enum.flat_map(fn [first | rest] = rows ->
        height = first.top - List.last(rows).bottom
        header_height = Map.get(headers, first.table_id, %{height: 0}).height

        if first.row_group_avoid and height + header_height <= content_height do
          [Map.put(first, :placement_height, height) | rest]
        else
          rows
        end
      end)

    # A table heading must travel with at least its first body row. Looking
    # ahead also preserves a fitting first row group when starting a table.
    groups
    |> Enum.with_index()
    |> Enum.map(fn {group, index} ->
      if group.table_section == :head do
        following = Enum.drop(groups, index + 1)

        {headings, body} =
          Enum.split_while(
            following,
            &(&1.table_id == group.table_id and &1.table_section == :head)
          )

        case body do
          [row | _] when row.table_id == group.table_id ->
            heading_height = group.top - List.last([group | headings]).bottom
            required = heading_height + Map.get(row, :placement_height, row.height)

            required =
              if required <= content_height, do: required, else: heading_height + row.height

            Map.put(group, :placement_height, required)

          _ ->
            group
        end
      else
        group
      end
    end)
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
    HtmlValidator.reserve_render_resource(:max_layout_boxes, length(boxes), :pagination)

    Enum.map(boxes, fn box ->
      box =
        case box do
          %{type: :glyph_group, boxes: children} ->
            %{box | boxes: shift_boxes(children, delta_y)}

          _ ->
            box
        end

      NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.translate_box(box, 0, delta_y)
    end)
  end

  defp target_group_top(state, group) do
    case state.position_delta do
      position_delta when is_number(position_delta) -> group.top + position_delta
      nil -> state.current_y
    end
  end
end
