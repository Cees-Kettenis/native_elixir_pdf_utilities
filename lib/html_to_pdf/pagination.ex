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
            Map.get(layout_tree, :link_document_targets, []),
            opts
          )

        {:error, {reason, diagnostic}} ->
          {:error,
           {reason,
            Diagnostics.with_context(diagnostic, operation: :paginate, module: __MODULE__)}}
      end
    end)
  end

  defp paginate_boxes(
         page_size,
         boxes,
         margins,
         link_scope,
         expected_targets,
         document_targets,
         opts
       ) do
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

    {fieldset_paints, spanning_cells?} =
      Enum.reduce(flow_boxes, {[], false}, fn box, {fieldsets, spanning?} ->
        fieldsets =
          if Map.has_key?(box, :fieldset_paint_id), do: [box | fieldsets], else: fieldsets

        {fieldsets, spanning? or Map.has_key?(box, :table_span_fragment)}
      end)

    fieldset_paints = Enum.reverse(fieldset_paints)

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
      |> paint_spanning_cell_fragments(spanning_cells?, margins)
      |> align_table_fragment_text()
      |> paint_fieldset_fragments(fieldset_paints, margins, pagination_order, opts)
      |> restore_root_positioned_boxes(root_positioned_boxes, pagination_order)
      |> paint_table_outlines()
      |> Enum.map(fn page ->
        background = Enum.map(canvas_boxes, &Map.delete(&1, pagination_order))
        HtmlValidator.reserve_render_resource(:max_layout_boxes, length(background), :pagination)
        %{page | boxes: background ++ page.boxes}
      end)

    with {:ok, pages} <-
           NativeElixirPdfUtilities.Validators.CssTransformValidator.materialize(
             pages,
             Keyword.put(opts, :css_fragmentation_margins, margins)
           ) do
      with {:ok, pages} <-
             NativeElixirPdfUtilities.Validators.HtmlLinkValidator.resolve_pages(
               pages,
               link_scope,
               expected_targets,
               document_targets
             ) do
        {:ok,
         Enum.map(pages, fn page ->
           %{
             page
             | boxes: Enum.reject(page.boxes, &(Map.get(&1, :role) == :explicit_block_space))
           }
         end)}
      end
    end
  end

  # Fieldset decoration is sliced at page boundaries, while legend artwork and
  # the top notch occur only in the first fragment. Keep authored backgrounds
  # behind the descendants on every page without making them pagination items.
  defp paint_fieldset_fragments(pages, paints, margins, pagination_order, opts) do
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

        positioned_sources =
          pages
          |> Enum.with_index()
          |> Enum.reduce(%{}, fn {page, index}, sources ->
            Enum.reduce(page.boxes, sources, fn box, sources ->
              case Map.get(box, :positioned_fragment_owner) do
                nil -> sources
                owner -> Map.update(sources, owner, [{box, index}], &[{box, index} | &1])
              end
            end)
          end)

        positioned_sources =
          prepare_positioned_source_planes(positioned_sources, pages, pagination_order, opts)

        # The source printer advances through physical page content viewports.
        # Suppressed continuation padding changes member positions, but does not
        # shorten this continuous source canvas.
        {source_page_offsets, _extent} =
          Enum.map_reduce(pages, 0.0, fn page, extent ->
            {extent, extent + elem(page.size, 1) - margins.top - margins.bottom}
          end)

        source_page_offsets = List.to_tuple(source_page_offsets)

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
                paint =
                  if index == first_page, do: paint, else: Map.delete(paint, :outline_anchor)

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

                    paint =
                      NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.translate_box(
                        paint,
                        0,
                        first_delta
                      )

                    paint =
                      case Map.get(paint, :html_targets) do
                        nil ->
                          paint

                        targets ->
                          Map.put(
                            paint,
                            :html_targets,
                            Enum.map(targets, fn target ->
                              %{target | dy: target.dy + paint.y - bottom}
                            end)
                          )
                      end

                    paint = %{
                      paint
                      | y: bottom,
                        height: max(top - bottom, 0.0),
                        border_widths: widths,
                        stroke_width: Enum.max(Map.values(widths))
                    }

                    paint =
                      if index == first_page, do: paint, else: Map.delete(paint, :border_cutout)

                    paint

                  :image ->
                    paint =
                      NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.translate_box(
                        paint,
                        0,
                        first_delta
                      )

                    NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.clip_boxes([paint], %{
                      x: paint.clip.x,
                      y: bottom,
                      width: paint.clip.width,
                      height: max(top - bottom, 0.0)
                    })
                    |> hd()
                    |> Map.put(:background_fragment, %{
                      x: paint.clip.x,
                      y: bottom,
                      width: paint.clip.width,
                      height: max(top - bottom, 0.0),
                      first: index == first_page,
                      last: index == last_page,
                      source_origin: %{
                        x: paint.clip.x,
                        top: margins.top - elem(source_page_offsets, index)
                      },
                      raster_viewport: %{
                        x: paint.clip.x,
                        y: margins.bottom,
                        width: paint.clip.width,
                        height: elem(page.size, 1) - margins.top - margins.bottom
                      }
                    })
                end
                |> Map.merge(
                  Map.take(paint, [pagination_order, :fieldset_paint_id, :fieldset_ancestors])
                )
              end)
            end)

          positioned =
            Enum.flat_map(members, fn {owner, _members} ->
              Enum.flat_map(Map.get(positioned_sources, owner, []), fn {source, source_page} ->
                delta = elem(source_page_offsets, index) - elem(source_page_offsets, source_page)

                {:ok, slice} =
                  source
                  |> NativeElixirPdfUtilities.Validators.EmbeddedFormValidator.issue_drawing_slice()
                  |> propagate_error()

                source =
                  NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.translate_box(
                    slice,
                    0,
                    delta
                  )

                HtmlValidator.reserve_render_resource(:max_layout_boxes, 1, :pagination)

                NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.clip_boxes([source], %{
                  x: margins.left,
                  y: margins.bottom,
                  width: elem(page.size, 0) - margins.left - margins.right,
                  height: elem(page.size, 1) - margins.top - margins.bottom
                })
                |> Enum.map(&Map.put(&1, pagination_order, Map.fetch!(source, pagination_order)))
              end)
            end)

          boxes =
            Enum.reject(page.boxes, fn box ->
              Map.has_key?(box, :fieldset_paint_id) or
                Map.has_key?(box, :positioned_fragment_owner)
            end)

          %{
            page
            | boxes:
                Enum.sort_by(
                  decorations ++ boxes ++ positioned,
                  &Map.fetch!(&1, pagination_order)
                )
          }
        end)
    end
  end

  # Prepare a genuine positioned control plane once before making physical
  # page copies. Its descriptor retains source-local fields and APs; only
  # canonical slice tokens and actual page transforms vary between copies.
  defp prepare_positioned_source_planes(sources, pages, pagination_order, opts) do
    alias NativeElixirPdfUtilities.Validators.{EmbeddedFormValidator, HtmlFormValidator}
    context = NativeElixirPdfUtilities.HtmlToPdf.ResourceContext.current()

    Map.new(sources, fn {owner, entries} ->
      groups =
        Enum.group_by(entries, fn {box, _page} ->
          Enum.map(Map.get(box, :css_transform_scopes, []), & &1.id)
        end)

      entries =
        Enum.flat_map(groups, fn {_scopes, entries} ->
          entries =
            Enum.sort_by(entries, fn {box, _page} -> Map.fetch!(box, pagination_order) end)

          boxes = Enum.map(entries, &elem(&1, 0))

          if Enum.any?(boxes, &is_map(Map.get(&1, :form_control))) do
            [{first, index} | _] = entries
            page = Enum.at(pages, index)

            with {:ok, [prepared], fields} <-
                   HtmlFormValidator.prepare_scoped(
                     [%{size: page.size, boxes: boxes}],
                     opts,
                     context
                   ),
                 {:ok, paint} <- EmbeddedFormValidator.source_paint(prepared.boxes),
                 {:ok, descriptor} <-
                   if(fields == [],
                     do: {:ok, nil},
                     else: EmbeddedFormValidator.issue(context, fields)
                   ) do
              group =
                %{
                  type: HtmlValidator.reserve_layout_box(:drawing_group),
                  role: :positioned_source_plane,
                  width: elem(page.size, 0),
                  height: elem(page.size, 1),
                  transform: {1, 0, 0, 1, 0, 0},
                  boxes: paint
                }
                |> Map.merge(
                  Map.take(first, [
                    :css_transform_scopes,
                    :positioned_fragment_owner,
                    :out_of_flow
                  ])
                )
                |> Map.put(pagination_order, Map.fetch!(first, pagination_order))

              group =
                if descriptor == nil,
                  do: group,
                  else: Map.put(group, :embedded_widgets, descriptor)

              [{group, index}]
            else
              {:error, _} = failure -> throw({:render_resource_limit, failure})
            end
          else
            entries
          end
        end)

      {owner, entries}
    end)
  end

  defp paint_spanning_cell_fragments(pages, spanning_cells?, margins) do
    if spanning_cells? do
      alias NativeElixirPdfUtilities.Validators.HtmlTableValidator

      sources =
        pages |> Enum.flat_map(& &1.boxes) |> Enum.filter(&Map.has_key?(&1, :table_span_fragment))

      {pages, consumed} =
        Enum.map_reduce(pages, %{}, fn page, consumed ->
          with {:ok, overflow} <-
                 HtmlTableValidator.prepare_cell_overflow_fragments(
                   sources,
                   page.boxes,
                   consumed,
                   elem(page.size, 1) - margins.top,
                   margins.bottom
                 ),
               {:ok, boxes, next} <-
                 HtmlTableValidator.prepare_cell_page_fragments(
                   overflow ++ page.boxes,
                   consumed,
                   margins.bottom,
                   elem(page.size, 1) - margins.top - margins.bottom
                 ) do
            {%{page | boxes: boxes}, next}
          else
            {:error, _} = failure -> throw({:render_resource_limit, failure})
          end
        end)

      continue_spanning_cell_pages(pages, sources, consumed, margins)
    else
      pages
    end
  end

  # Genuine source flow can outlive the measured table surfaces when an atomic
  # control was deferred. Carry only unpaid native units into actual page clips.
  defp continue_spanning_cell_pages(pages, sources, consumed, margins) do
    alias NativeElixirPdfUtilities.Validators.HtmlTableValidator
    page_size = hd(pages).size

    {:ok, overflow} =
      HtmlTableValidator.prepare_cell_overflow_fragments(
        sources,
        [],
        consumed,
        elem(page_size, 1) - margins.top,
        margins.bottom
      )
      |> propagate_error()

    case overflow do
      [] ->
        pages

      _ ->
        HtmlValidator.check_render_resource(:max_pdf_pages, length(pages) + 1, :pagination)

        {:ok, boxes, next} =
          HtmlTableValidator.prepare_cell_page_fragments(
            overflow,
            consumed,
            margins.bottom,
            elem(page_size, 1) - margins.top - margins.bottom
          )
          |> propagate_error()

        continue_spanning_cell_pages(
          pages ++ [%{size: page_size, boxes: boxes}],
          sources,
          next,
          margins
        )
    end
  end

  # Fragment callbacks retain validator diagnostics through the outer budget.
  defp propagate_error(result) do
    case result do
      {:error, _} = error -> throw({:render_resource_limit, error})
      result -> result
    end
  end

  # Row/cell slicing introduces a local PDF coordinate plane, but text still
  # paints on the original page's CSS pixel grid. Resolve that page position
  # before localizing instead of snapping again against a fractional clip size.
  defp align_table_fragment_text(pages) do
    Enum.map(pages, fn page ->
      boxes =
        Enum.map(page.boxes, fn box ->
          if Map.get(box, :role) == :table_row_fragment or Map.has_key?(box, :table_span_fragment) do
            {a, _b, _c, d, e, f} = box.transform

            %{
              box
              | boxes:
                  align_table_fragment_text_boxes(box.boxes, elem(page.size, 1), {a, d, e, f})
            }
          else
            box
          end
        end)

      %{page | boxes: boxes}
    end)
  end

  defp align_table_fragment_text_boxes(boxes, page_height, {a, d, e, f} = transform) do
    Enum.map(boxes, fn box ->
      case box do
        %{type: :text, snap_to_css_pixel_grid: true} ->
          projected = %{box | x: a * box.x + e, y: d * box.y + f}

          {x, y} =
            NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.text_position(
              projected,
              page_height
            )

          local_x = (x - e) / a
          local_y = (y - f) / d
          # Raster placement moves ink, while authored target origins retain
          # their source position under the same genuine fragment clip.
          targets = Map.get(box, :html_targets, [])

          targets =
            Enum.map(targets, fn target ->
              %{target | dx: target.dx + box.x - local_x, dy: target.dy + box.y - local_y}
            end)

          box
          |> Map.put(:x, local_x)
          |> Map.put(:y, local_y)
          |> Map.put(:html_targets, targets)
          |> Map.put(:snap_to_css_pixel_grid, false)
          |> Map.put(:css_source_positioned, true)

        %{type: :glyph_group, boxes: children} ->
          %{box | boxes: align_table_fragment_text_boxes(children, page_height, transform)}

        _ ->
          box
      end
    end)
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
        reservation = Map.get(group, :block_placement_height)
        content_height = page_height - margins.top - margins.bottom

        group =
          if is_number(reservation) and reservation <= content_height,
            do: Map.put(group, :placement_height, max(reservation, group.height)),
            else: group

        state =
          if group.height == 0 and is_number(reservation) and reservation <= content_height and
               target_group_top(state, group) - reservation < margins.bottom and
               state.current_boxes != [], do: page_break(state, page_size, margins), else: state

        if Enum.all?(group.boxes, &(Map.get(&1, :role) == :explicit_block_space)) do
          consume_block_space(group, state, page_size, margins)
        else
          place_group(group, state, headers, page_size, margins)
        end
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

  # A fixed-height block's remaining empty content is divisible source flow.
  # Consume actual capacity so following content keeps its source offset even
  # when the reservation alone crosses a page boundary.
  defp consume_block_space(group, state, page_size, margins) do
    reserve = Map.get(group, :placement_height, group.height)
    full_height = elem(page_size, 1) - margins.top - margins.bottom

    state =
      if Map.has_key?(group, :placement_height) and reserve <= full_height and
           target_group_top(state, group) - reserve < margins.bottom and
           state.current_boxes != [], do: page_break(state, page_size, margins), else: state

    top = target_group_top(state, group)
    available = max(top - margins.bottom, 0.0)
    taken = min(group.height, available)
    [source] = group.boxes
    [marker] = shift_boxes([source], top - group.top)

    marker = %{
      marker
      | y: top - taken,
        height: taken,
        type: HtmlValidator.reserve_layout_box(:flow_marker)
    }

    marker =
      if Map.has_key?(source, :fieldset_source_y),
        do: Map.put(marker, :fieldset_source_y, group.top - taken),
        else: marker

    HtmlValidator.check_render_resource(:max_rendered_pages, state.page_count + 1, :pagination)
    HtmlValidator.check_render_resource(:max_pdf_pages, state.page_count + 1, :pagination)

    state = %{
      state
      | current_boxes: state.current_boxes ++ [marker],
        current_y: top - taken,
        position_delta: top - group.top
    }

    if taken < group.height do
      remaining = %{
        group
        | top: group.top - taken,
          height: group.height - taken,
          boxes: [%{source | height: source.height - taken}]
      }

      consume_block_space(remaining, page_break(state, page_size, margins), page_size, margins)
    else
      state
    end
  end

  defp place_group(group, state, headers, page_size, margins) do
    content_height = elem(page_size, 1) - margins.top - margins.bottom

    group =
      if group.table_section == :head and Map.get(group, :placement_height, 0.0) > content_height,
        do: Map.delete(group, :placement_height),
        else: group

    target_top = target_group_top(state, group)

    span_continues? =
      if group.table_section in [:body, :foot] and group.break_inside != :avoid and
           Map.get(group, :placement_height, group.height) == group.height and
           group.height > max(target_top - margins.bottom, 0.0) do
        case NativeElixirPdfUtilities.Validators.HtmlTableValidator.prepare_row_continuation_intent(
               group.boxes,
               target_top - group.top,
               margins.bottom
             ) do
          {:ok, continuing?} -> continuing?
          {:error, _} = error -> throw({:render_resource_limit, error})
        end
      else
        false
      end

    if group.table_section in [:body, :foot] and
         (group.height > content_height or span_continues?) do
      state =
        if group.break_before == :page,
          do:
            state
            |> page_break(page_size, margins)
            |> start_table_row_page(%{group | height: 0.0}, headers, margins),
          else: state

      place_table_row(group, group.top, state, headers, page_size, margins)
    else
      place_whole_group(group, state, headers, page_size, margins)
    end
  end

  # Oversized rows consume the remaining fragmentainer instead of first moving
  # their whole height to a fresh page. Every cut comes from canonical native
  # line/atomic geometry, and each continuation reserves the repeated heading.
  defp place_table_row(group, source_top, state, headers, page_size, margins) do
    remaining = source_top - group.bottom
    target_top = target_group_top(state, %{group | top: source_top})
    capacity = max(target_top - margins.bottom, 0.0)

    {:ok, boxes, height} =
      NativeElixirPdfUtilities.Validators.HtmlTableValidator.prepare_row_page_fragment(
        group.boxes,
        source_top,
        group.bottom,
        capacity,
        elem(page_size, 1) - margins.top - margins.bottom
      )
      |> propagate_error()

    cond do
      height == 0 and state.current_boxes != [] and is_number(state.position_delta) ->
        next =
          state
          |> page_break(page_size, margins)
          |> start_table_row_page(
            Map.put(%{group | height: 0.0}, :row_continuing, source_top < group.top),
            headers,
            margins
          )

        place_table_row(group, source_top, next, headers, page_size, margins)

      height == 0 ->
        # An atom taller than a complete fragmentainer stays intact on an empty
        # page, matching the ordinary oversized-atom placement rule.
        {:ok, boxes, _height} =
          NativeElixirPdfUtilities.Validators.HtmlTableValidator.prepare_row_page_fragment(
            group.boxes,
            source_top,
            group.bottom,
            remaining
          )
          |> propagate_error()

        place_whole_group(
          %{group | boxes: boxes, top: source_top, height: remaining, break_before: :auto},
          state,
          headers,
          page_size,
          margins
        )

      true ->
        fragment = %{
          group
          | boxes: boxes,
            top: source_top,
            bottom: source_top - height,
            height: height,
            break_before: :auto,
            break_after: :auto
        }

        fragment = Map.delete(fragment, :placement_height)
        next = place_whole_group(fragment, state, headers, page_size, margins)

        if height >= remaining do
          if group.break_after == :page, do: page_break(next, page_size, margins), else: next
        else
          next =
            next
            |> page_break(page_size, margins)
            |> start_table_row_page(
              Map.put(%{group | height: 0.0}, :row_continuing, true),
              headers,
              margins
            )

          inset =
            if Enum.any?(group.boxes, &Map.has_key?(&1, :table_span_fragment)),
              do: 0.0,
              else: Map.get(hd(boxes), :table_fragment_inset, 0.0)

          next = %{next | current_y: next.current_y - inset}
          source_gap = Map.get(hd(boxes), :table_fragment_source_gap, 0.0)

          place_table_row(
            group,
            source_top - height - source_gap,
            next,
            headers,
            page_size,
            margins
          )
        end
    end
  end

  defp place_whole_group(group, state, headers, page_size, margins) do
    state =
      case Map.get(group, :break_before) do
        :page ->
          state
          |> page_break(page_size, margins)
          |> start_table_row_page(group, headers, margins)

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
          |> start_table_row_page(group, headers, margins)

        false ->
          %{state | current_y: target_top}
      end

    target_top = state.current_y

    target_top =
      case NativeElixirPdfUtilities.Validators.MathPaintValidator.prepare_pagination_top(
             group.boxes,
             group.top,
             target_top,
             page_size,
             elem(page_size, 1) - margins.top
           ) do
        {:ok, top} -> top
        {:error, _} = error -> throw({:render_resource_limit, error})
      end

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

  defp start_table_row_page(state, group, headers, margins) do
    state = repeat_table_header(state, group, headers, margins)

    first_source_row? =
      group.table_section in [:body, :foot] and not Map.get(group, :row_continuing, false) and
        Enum.any?(group.boxes, &match?({:table_row, _table, 0}, Map.get(&1, :flow_id)))

    if first_source_row? do
      case NativeElixirPdfUtilities.Validators.HtmlTableValidator.prepare_row_start_inset(
             group.boxes
           ) do
        {:ok, inset} -> %{state | current_y: state.current_y - inset}
        {:error, _} = error -> throw({:render_resource_limit, error})
      end
    else
      state
    end
  end

  defp repeat_table_header(state, group, headers, margins) do
    case {Map.get(group, :table_id), Map.get(group, :table_section)} do
      {table_id, section}
      when not is_nil(table_id) and section != :head and is_nil(group.table_caption_side) ->
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
      row_group_avoid: Map.get(first, :row_group_avoid, false),
      table_caption_side: Map.get(first, :table_caption_side),
      block_placement_height: Map.get(first, :block_placement_height)
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
