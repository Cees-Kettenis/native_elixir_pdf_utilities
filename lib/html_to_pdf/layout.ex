defmodule NativeElixirPdfUtilities.HtmlToPdf.Layout do
  @moduledoc """
  Layout engine for the native HTML-to-PDF renderer.

  Lays out block text elements, inline text runs, box styling, lists, link
  annotation bounds, deterministic tables, a documented flexbox/grid subset,
  image boxes, embedded-font text metrics, and pagination metadata.
  """

  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @css_pixel_points 0.75
  @line_wrap_tolerance 1.0

  @type box :: map()
  @type layout_tree :: %{
          type: :layout,
          page_size: term(),
          margin: term(),
          margins: PageGeometry.margins(),
          boxes: [box()]
        }
  @type render_option :: NativeElixirPdfUtilities.HtmlToPdf.render_option()

  @doc """
  Converts a styled document tree into a layout tree.
  """
  @spec layout(term(), term()) ::
          {:ok, layout_tree()} | {:error, :invalid_layout | :invalid_margin | :invalid_page_size}
  def layout(styled_tree, opts \\ []) do
    page_size =
      case Keyword.keyword?(opts) do
        true -> PageGeometry.normalize_page_size(Keyword.get(opts, :page_size, :a4))
        false -> {:error, :invalid_page_size}
      end

    margins =
      case Keyword.keyword?(opts) do
        true -> PageGeometry.normalize_margins(Keyword.get(opts, :margin, 0))
        false -> {:error, :invalid_margin}
      end

    case HtmlValidator.validate_layout_input(styled_tree, opts, page_size, margins) do
      :ok ->
        styled_tree = attach_positioned_descendants(styled_tree)
        {:ok, page_size} = page_size
        {:ok, margins} = margins
        children = styled_tree.children
        positioned_children = Map.get(styled_tree, :positioned_children, [])

        with {:ok, boxes} <- layout_blocks(children, page_size, margins),
             {:ok, boxes} <-
               layout_positioned_children(
                 positioned_children,
                 boxes,
                 %{
                   x: margins.left,
                   top: elem(page_size, 1) - margins.top,
                   width: elem(page_size, 0) - margins.left - margins.right,
                   height: elem(page_size, 1) - margins.top - margins.bottom
                 },
                 :root
               ),
             false <- Enum.any?(boxes, &(Map.get(&1, :type) == :layout_error)) do
          {page_width, page_height} = page_size

          {:ok,
           %{
             type: :layout,
             page_size: page_size,
             margin: PageGeometry.compact_margins(margins),
             margins: margins,
             boxes: boxes,
             content_width: page_width - margins.left - margins.right,
             content_height: page_height - margins.top - margins.bottom
           }}
        else
          true -> {:error, :invalid_layout}
          {:error, reason} -> {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp attach_positioned_descendants(%{type: :document, children: children} = document) do
    {children, positioned_children} = collect_positioned_children(children)

    document
    |> Map.put(:children, children)
    |> Map.put(:positioned_children, positioned_children)
  end

  defp collect_positioned_children(children) do
    Enum.reduce(children, {[], []}, fn child, {normal, positioned} ->
      case child do
        %{type: :element, children: nested_children, style: style} = element ->
          {nested_children, nested_positioned} = collect_positioned_children(nested_children)

          element =
            element
            |> Map.put(:children, nested_children)
            |> maybe_attach_positioned_children(nested_positioned)

          case Map.get(style, :position, :static) do
            :absolute -> {normal, positioned ++ [element]}
            :relative -> {normal ++ [element], positioned}
            _ -> {normal ++ [element], positioned ++ nested_positioned}
          end

        child ->
          {normal ++ [child], positioned}
      end
    end)
  end

  defp maybe_attach_positioned_children(element, positioned_children) do
    case {Map.get(element.style, :position, :static), positioned_children} do
      {position, positioned_children} when position in [:relative, :absolute] ->
        Map.put(element, :positioned_children, positioned_children)

      _ ->
        element
    end
  end

  @spec layout_blocks([term()], {number(), number()}, PageGeometry.margins()) ::
          {:ok, [box()]} | {:error, :invalid_layout}
  defp layout_blocks(children, page_size, margins) do
    {page_width, page_height} = page_size

    result =
      Enum.reduce_while(children, {:ok, [], page_height - margins.top}, fn child,
                                                                           {:ok, boxes, y} ->
        case layout_block(
               child,
               margins.left,
               y,
               page_width - margins.left - margins.right
             ) do
          {:ok, block_boxes, next_y} -> {:cont, {:ok, boxes ++ block_boxes, next_y}}
          {:error, reason} -> {:halt, {:error, reason}}
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
    case do_layout_block(block, x, y, width) do
      {:ok, boxes, next_y} ->
        position_container_contents(block, boxes, next_y, x, y, width)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp do_layout_block(block, x, y, width) do
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
            |> Map.put(
              :fragment_lines,
              Map.get(style, :background_color) == nil and
                Enum.all?(Map.values(border_widths), &(&1 <= 0))
            )
            |> Map.put(:flow_id, {:block, box_x, box_top})

          content_metadata =
            flow_metadata
            |> Map.put(:break_before, :auto)
            |> Map.put(:break_after, :auto)

          case layout_block_content(
                 children,
                 style,
                 content_x,
                 content_top,
                 content_width,
                 content_metadata
               ) do
            {:ok, content_boxes, content_height} ->
              content_box_height = resolved_content_size(style, :height, nil, content_height)

              box_height =
                border_widths.top + padding.top + content_box_height + padding.bottom +
                  border_widths.bottom

              background_box =
                style
                |> background_box(box_x, box_top - box_height, box_width, box_height)
                |> tag_boxes(content_metadata)

              flow_marker =
                case {background_box, padding.top} do
                  {[], top_inset} when top_inset > 0 ->
                    [
                      %{
                        type: :rect,
                        x: box_x,
                        y: box_top - top_inset,
                        width: 0.001,
                        height: top_inset,
                        fill_color: {0.0, 0.0, 0.0, 0.0},
                        stroke_color: nil,
                        stroke_width: 0.0,
                        border_widths: edges(0.0),
                        border_colors: edges(nil),
                        border_styles: edges(:none),
                        border_radius: 0.0,
                        paint_layer: :flow_marker
                      }
                      |> Map.merge(content_metadata)
                    ]

                  _ ->
                    []
                end

              next_y = box_top - box_height - margin.bottom

              page_break_box = fn position ->
                %{
                  type: :page_break,
                  x: box_x,
                  y: if(position == :before, do: box_top, else: next_y),
                  width: box_width,
                  height: 0.0
                }
                |> Map.merge(%{
                  break_before: if(position == :before, do: :page, else: :auto),
                  break_after: if(position == :after, do: :page, else: :auto),
                  break_inside: :auto,
                  flow_id: {:page_break, position, box_x, box_top}
                })
              end

              boxes = background_box ++ flow_marker ++ content_boxes

              boxes =
                case Map.get(flow_metadata, :break_before) do
                  :page -> [page_break_box.(:before) | boxes]
                  _ -> boxes
                end

              boxes =
                case Map.get(flow_metadata, :break_after) do
                  :page -> boxes ++ [page_break_box.(:after)]
                  _ -> boxes
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

  defp position_container_contents(block, boxes, next_y, x, y, available_width) do
    style = Map.fetch!(block, :style)
    positioned_children = Map.get(block, :positioned_children, [])

    case positioned_children do
      [] ->
        {:ok, relatively_shifted_boxes(boxes, style, x, y, available_width, next_y), next_y}

      positioned_children ->
        with {:ok, containing_block} <-
               positioned_containing_block(style, x, y, available_width, next_y),
             {:ok, positioned_boxes} <-
               positioned_child_boxes(positioned_children, containing_block) do
          boxes =
            boxes
            |> mark_container_background(style, x, y, available_width, next_y)
            |> stack_positioned_boxes(positioned_boxes, is_integer(Map.get(style, :z_index)))

          {:ok, relatively_shifted_boxes(boxes, style, x, y, available_width, next_y), next_y}
        end
    end
  end

  defp positioned_containing_block(style, x, y, available_width, next_y) do
    display = Map.get(style, :display)

    case display in [:block, :flex, :inline_flex, :grid, :inline_grid] do
      true ->
        margin = Map.get(style, :margin, edges(0.0))
        border_widths = Map.get(style, :border_widths, edges(0.0))
        box_x = x + margin.left
        box_top = y - margin.top
        box_width = positioned_element_border_width(style, available_width)
        box_height = max(box_top - next_y - margin.bottom, 0.0)

        {:ok,
         %{
           x: box_x + border_widths.left,
           top: box_top - border_widths.top,
           width: max(box_width - border_widths.left - border_widths.right, 0.0),
           height: max(box_height - border_widths.top - border_widths.bottom, 0.0)
         }}

      false ->
        {:error, :invalid_layout}
    end
  end

  defp positioned_element_border_width(style, available_width) do
    margin = Map.get(style, :margin, edges(0.0))
    available_box_width = available_width - margin.left - margin.right

    case Map.get(style, :display) do
      :image ->
        {content_width, _content_height} =
          image_content_size(
            style,
            available_box_width - horizontal_box_size(style),
            nil
          )

        content_width + horizontal_box_size(style)

      _ ->
        content_width =
          resolved_content_size(
            style,
            :width,
            width_available_size(style, available_box_width),
            available_box_width - horizontal_box_size(style)
          )

        content_width + horizontal_box_size(style)
    end
  end

  defp layout_positioned_children(positioned_children, normal_boxes, containing_block, anchor) do
    case positioned_children do
      [] ->
        {:ok, normal_boxes}

      positioned_children ->
        with {:ok, positioned_boxes} <-
               positioned_child_boxes(positioned_children, containing_block) do
          positioned_boxes =
            Enum.map(positioned_boxes, fn entry ->
              %{entry | boxes: Enum.map(entry.boxes, &Map.put(&1, :position_anchor, anchor))}
            end)

          {:ok, stack_positioned_boxes(normal_boxes, positioned_boxes, true)}
        end
    end
  end

  defp positioned_child_boxes(positioned_children, containing_block) do
    positioned_children
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn {child, index}, {:ok, entries} ->
      case positioned_child_box(child, containing_block, index) do
        {:ok, entry} -> {:cont, {:ok, entries ++ [entry]}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp positioned_child_box(%{type: :element, style: style} = child, containing_block, index) do
    case Map.get(style, :display) in [:block, :image, :flex, :inline_flex, :grid, :inline_grid] do
      true ->
        offsets = Map.get(style, :offsets, edges(:auto))
        style = stretch_positioned_style(style, offsets, containing_block)
        child = Map.put(child, :style, style)

        case layout_block(child, 0.0, 0.0, containing_block.width) do
          {:ok, boxes, next_y} ->
            margin = Map.get(style, :margin, edges(0.0))
            border_width = positioned_element_border_width(style, containing_block.width)
            border_height = max(0.0 - margin.top - next_y - margin.bottom, 0.0)
            left = resolved_offset(offsets.left, containing_block.width)
            right = resolved_offset(offsets.right, containing_block.width)
            top = resolved_offset(offsets.top, containing_block.height)
            bottom = resolved_offset(offsets.bottom, containing_block.height)

            target_x =
              cond do
                is_number(left) ->
                  containing_block.x + left + margin.left

                is_number(right) ->
                  containing_block.x + containing_block.width - right - margin.right -
                    border_width

                true ->
                  containing_block.x + margin.left
              end

            target_top =
              cond do
                is_number(top) ->
                  containing_block.top - top - margin.top

                is_number(bottom) ->
                  containing_block.top - containing_block.height + bottom + margin.bottom +
                    border_height

                true ->
                  containing_block.top - margin.top
              end

            delta_x = target_x - margin.left
            delta_y = target_top - -margin.top

            boxes =
              boxes
              |> shift_layout_boxes(delta_x, delta_y)
              |> Enum.map(&Map.put(&1, :out_of_flow, true))

            z_index = Map.get(style, :z_index, :auto)
            {:ok, %{z_index: z_index, index: index, boxes: boxes}}

          {:error, reason} ->
            {:error, reason}
        end

      false ->
        {:error, :invalid_layout}
    end
  end

  defp stretch_positioned_style(style, offsets, containing_block) do
    style =
      case {Map.get(style, :width), resolved_offset(offsets.left, containing_block.width),
            resolved_offset(offsets.right, containing_block.width)} do
        {nil, left, right} when is_number(left) and is_number(right) ->
          target =
            max(
              containing_block.width - left - right -
                horizontal_margin_size(style),
              0.0
            )

          Map.put(style, :width, declared_size_for_border_box(style, :width, target))

        _ ->
          style
      end

    case {Map.get(style, :height), resolved_offset(offsets.top, containing_block.height),
          resolved_offset(offsets.bottom, containing_block.height)} do
      {nil, top, bottom} when is_number(top) and is_number(bottom) ->
        target =
          max(
            containing_block.height - top - bottom - vertical_margin_size(style),
            0.0
          )

        Map.put(style, :height, declared_size_for_border_box(style, :height, target))

      _ ->
        style
    end
  end

  defp declared_size_for_border_box(style, property, size) do
    case {Map.get(style, :box_sizing, :content_box), property} do
      {:border_box, _property} -> size
      {_box_sizing, :width} -> max(size - horizontal_box_size(style), 0.0)
      {_box_sizing, :height} -> max(size - vertical_box_size(style), 0.0)
    end
  end

  defp relatively_shifted_boxes(boxes, style, _x, y, available_width, next_y) do
    case Map.get(style, :position, :static) do
      :relative ->
        offsets = Map.get(style, :offsets, edges(:auto))
        margin = Map.get(style, :margin, edges(0.0))
        element_height = max(y - margin.top - next_y - margin.bottom, 0.0)
        left = resolved_offset(offsets.left, available_width)
        right = resolved_offset(offsets.right, available_width)
        top = resolved_offset(offsets.top, element_height)
        bottom = resolved_offset(offsets.bottom, element_height)
        delta_x = if(is_number(left), do: left, else: if(is_number(right), do: -right, else: 0.0))
        delta_y = if(is_number(top), do: -top, else: if(is_number(bottom), do: bottom, else: 0.0))
        shift_layout_boxes(boxes, delta_x, delta_y)

      _ ->
        boxes
    end
  end

  defp resolved_offset(offset, available) do
    case offset do
      :auto -> nil
      {:percent, ratio} -> ratio * available
      offset when is_number(offset) -> offset
      _ -> nil
    end
  end

  defp horizontal_margin_size(style) do
    margin = Map.get(style, :margin, edges(0.0))
    margin.left + margin.right
  end

  defp vertical_margin_size(style) do
    margin = Map.get(style, :margin, edges(0.0))
    margin.top + margin.bottom
  end

  defp stack_positioned_boxes(normal_boxes, positioned_entries, stacking_context?) do
    {background_boxes, content_boxes} =
      Enum.split_while(normal_boxes, &(Map.get(&1, :stacking_background, false) == true))

    sorted =
      Enum.sort_by(positioned_entries, fn entry ->
        z_index = if(is_integer(entry.z_index), do: entry.z_index, else: 0)
        {z_index, entry.index}
      end)

    {negative, nonnegative} =
      Enum.split_with(sorted, &(is_integer(&1.z_index) and &1.z_index < 0))

    negative_boxes = Enum.flat_map(negative, & &1.boxes)
    nonnegative_boxes = Enum.flat_map(nonnegative, & &1.boxes)

    case stacking_context? do
      true -> background_boxes ++ negative_boxes ++ content_boxes ++ nonnegative_boxes
      false -> negative_boxes ++ background_boxes ++ content_boxes ++ nonnegative_boxes
    end
  end

  defp mark_container_background(boxes, style, x, y, available_width, next_y) do
    margin = Map.get(style, :margin, edges(0.0))
    box_x = x + margin.left
    box_top = y - margin.top
    box_width = positioned_element_border_width(style, available_width)
    box_height = max(box_top - next_y - margin.bottom, 0.0)
    box_y = box_top - box_height

    {background, remaining} =
      Enum.split_while(boxes, fn box ->
        background_paints_area?(box, box_x, box_y, box_width, box_height)
      end)

    Enum.map(background, &Map.put(&1, :stacking_background, true)) ++ remaining
  end

  defp background_paints_area?(box, x, y, width, height) do
    case box do
      %{type: :rect, paint_layer: :container_background} ->
        approximately_equal?(box.x, x) and approximately_equal?(box.y, y) and
          approximately_equal?(box.width, width) and approximately_equal?(box.height, height)

      %{type: :image, paint_layer: :container_background, clip: clip} ->
        approximately_equal?(clip.x, x) and approximately_equal?(clip.y, y) and
          approximately_equal?(clip.width, width) and approximately_equal?(clip.height, height)

      _ ->
        false
    end
  end

  defp approximately_equal?(left, right) do
    is_number(left) and is_number(right) and abs(left - right) <= 0.0001
  end

  defp shift_layout_boxes(boxes, delta_x, delta_y) do
    Enum.map(boxes, fn box ->
      box =
        box
        |> maybe_shift_coordinate(:x, delta_x)
        |> maybe_shift_coordinate(:y, delta_y)

      case Map.get(box, :clip) do
        %{x: clip_x, y: clip_y} = clip ->
          Map.put(box, :clip, %{clip | x: clip_x + delta_x, y: clip_y + delta_y})

        _ ->
          box
      end
    end)
  end

  defp maybe_shift_coordinate(box, coordinate, delta) do
    case Map.get(box, coordinate) do
      value when is_number(value) -> Map.put(box, coordinate, value + delta)
      _ -> box
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
      Enum.reduce_while(children, {:ok, [], y, nil}, fn child,
                                                        {:ok, boxes, current_y,
                                                         previous_margin_bottom} ->
        {margin_top, margin_bottom} = flow_child_vertical_margins(child, style)

        collapsed_margin = collapsed_sibling_margin(previous_margin_bottom, margin_top)

        case layout_flow_child(child, style, x, current_y + collapsed_margin, width) do
          {:ok, child_boxes, next_y} ->
            next_margin_bottom =
              following_sibling_margin(
                child_boxes,
                previous_margin_bottom,
                margin_top,
                margin_bottom
              )

            {:cont, {:ok, boxes ++ child_boxes, next_y, next_margin_bottom}}

          {:error, reason} ->
            {:halt, {:error, reason}}
        end
      end)

    case result do
      {:ok, boxes, next_y, _margin_bottom} -> {:ok, boxes, next_y}
      {:error, reason} -> {:error, reason}
    end
  end

  defp flow_child_vertical_margins(child, parent_style) do
    case {Map.get(parent_style, :display), child} do
      {parent_display, %{type: :element, style: %{display: display} = style}}
      when parent_display in [:block, :table_cell] and
             display not in [:none, :inline, :line_break] ->
        margin = Map.get(style, :margin, edges(0.0))
        {margin.top, margin.bottom}

      _ ->
        {0.0, 0.0}
    end
  end

  defp collapsed_sibling_margin(previous_margin_bottom, margin_top) do
    case previous_margin_bottom do
      previous when is_number(previous) -> min(max(previous, 0.0), max(margin_top, 0.0))
      _ -> 0.0
    end
  end

  defp following_sibling_margin(child_boxes, previous_margin_bottom, margin_top, margin_bottom) do
    case child_boxes do
      [] ->
        [previous_margin_bottom, margin_top, margin_bottom]
        |> Enum.reject(&is_nil/1)
        |> Enum.max(fn -> 0.0 end)

      _ ->
        margin_bottom
    end
  end

  defp layout_flow_child(child, style, x, y, width) do
    case child do
      %{type: :text, text: text} when is_binary(text) ->
        text = normalize_inline_whitespace(text, child.style)

        case trim_inline_whitespace(text) do
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
      fitted_image_box(
        style,
        box_x + border_widths.left + padding.left,
        box_top - border_widths.top - padding.top - content_height,
        content_width,
        content_height,
        Map.fetch!(style, :image)
      )
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
        text = normalize_inline_whitespace(text, child.style)

        case trim_inline_whitespace(text) do
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
    end
  end

  defp build_grid_item(style, runs, index) do
    margin = Map.get(style, :margin, edges(0.0))
    text_width = Enum.reduce(runs, 0.0, fn run, acc -> acc + inline_run_width(run, nil) end)
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
    resolve_grid_track_sizes(tracks, column_intrinsics, available_size, gap, :column)
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
    row_sizes =
      resolve_grid_track_sizes(tracks, row_intrinsics, available_height, gap, :row)

    stretch_grid_rows(row_sizes, tracks, gap, available_height, align_content)
  end

  defp resolve_grid_track_sizes(tracks, intrinsics, available_size, gap, axis) do
    track_data =
      tracks
      |> Enum.with_index()
      |> Enum.map(fn {track, index} ->
        intrinsic = Enum.at(intrinsics, index, 0.0)
        fraction = grid_track_fraction(track)

        %{
          fixed_size: grid_fixed_track_size(track, intrinsic),
          fraction: fraction,
          index: index,
          minimum:
            case fraction do
              nil -> 0.0
              _fraction -> grid_flexible_track_minimum(track, intrinsic, axis)
            end
        }
      end)

    fixed_total =
      Enum.reduce(track_data, 0.0, fn data, total ->
        case data.fraction do
          nil -> total + data.fixed_size
          _fraction -> total
        end
      end)

    flexible_tracks = Enum.reject(track_data, &is_nil(&1.fraction))
    minimum_total = Enum.reduce(flexible_tracks, 0.0, &(&1.minimum + &2))
    gap_total = gap * max(length(tracks) - 1, 0)

    track_space =
      case available_size do
        size when is_number(size) -> max(size - gap_total, 0.0)
        _ -> fixed_total + minimum_total
      end

    flexible_sizes =
      distribute_flexible_grid_tracks(flexible_tracks, max(track_space - fixed_total, 0.0))

    Enum.map(track_data, fn data ->
      case data.fraction do
        nil -> data.fixed_size
        _fraction -> Map.fetch!(flexible_sizes, data.index)
      end
    end)
  end

  defp grid_track_fraction(track) do
    case track do
      {:fr, fraction} -> fraction
      {:minmax, _minimum, {:fr, fraction}} -> fraction
      _ -> nil
    end
  end

  defp grid_fixed_track_size(track, intrinsic) do
    case track do
      {:length, length} ->
        length

      :auto ->
        intrinsic

      {:fr, _fraction} ->
        0.0

      {:minmax, minimum, maximum} ->
        minimum_size = grid_minimum_track_size(minimum, intrinsic)

        case maximum do
          {:length, length} -> max(minimum_size, length)
          :auto -> max(minimum_size, intrinsic)
          {:fr, _fraction} -> minimum_size
        end
    end
  end

  defp grid_flexible_track_minimum(track, intrinsic, axis) do
    minimum =
      case track do
        {:fr, _fraction} -> 0.0
        {:minmax, minimum, {:fr, _fraction}} -> grid_minimum_track_size(minimum, intrinsic)
      end

    case axis do
      :row -> max(minimum, intrinsic)
      :column -> minimum
    end
  end

  defp grid_minimum_track_size(minimum, intrinsic) do
    case minimum do
      {:length, length} -> length
      :auto -> intrinsic
    end
  end

  defp distribute_flexible_grid_tracks(tracks, available_size) do
    {zero_fraction_tracks, flexible_tracks} =
      Enum.split_with(tracks, &(&1.fraction <= 0))

    resolved =
      Map.new(zero_fraction_tracks, fn track ->
        {track.index, track.minimum}
      end)

    remaining =
      max(
        available_size -
          Enum.reduce(zero_fraction_tracks, 0.0, &(&1.minimum + &2)),
        0.0
      )

    case flexible_tracks do
      [] ->
        resolved

      flexible_tracks ->
        fraction_total = Enum.reduce(flexible_tracks, 0.0, &(&1.fraction + &2))
        fraction_unit = remaining / fraction_total

        {constrained_tracks, unconstrained_tracks} =
          Enum.split_with(
            flexible_tracks,
            &(fraction_unit * &1.fraction < &1.minimum)
          )

        case constrained_tracks do
          [] ->
            Enum.reduce(unconstrained_tracks, resolved, fn track, sizes ->
              Map.put(sizes, track.index, fraction_unit * track.fraction)
            end)

          constrained_tracks ->
            constrained_size =
              Enum.reduce(constrained_tracks, 0.0, &(&1.minimum + &2))

            constrained_sizes =
              Enum.reduce(constrained_tracks, resolved, fn track, sizes ->
                Map.put(sizes, track.index, track.minimum)
              end)

            Map.merge(
              constrained_sizes,
              distribute_flexible_grid_tracks(
                unconstrained_tracks,
                max(remaining - constrained_size, 0.0)
              )
            )
        end
    end
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
      tagged_boxes = tag_atomic_boxes(boxes, %{flow_id: container_flow_id})
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
        case text |> normalize_inline_whitespace(child.style) |> trim_inline_whitespace() do
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
    end
  end

  defp build_flex_item(style, runs, index, main_axis, available_main, available_cross) do
    margin = Map.get(style, :margin, edges(0.0))

    text_width =
      Enum.reduce(runs, 0.0, fn run, acc -> acc + inline_run_width(run, available_main) end)

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

    constraint_available_main =
      case main_axis do
        :row -> available_main
        :column -> resolved_content_size(style, :height, nil, nil)
      end

    wrap = Map.get(style, :flex_wrap, :nowrap)

    lines =
      Enum.reduce(items, [], fn item, lines ->
        append_flex_item_to_lines(lines, item, wrap, available_main, gap)
      end)

    lines
    |> Enum.map(&resolve_flex_line(&1, available_main, constraint_available_main, gap))
  end

  defp append_flex_item_to_lines(lines, item, wrap, available_main, gap) do
    case lines do
      [] ->
        [%{items: [item], base_main: item.outer_main}]

      lines ->
        [line | previous] = Enum.reverse(lines)
        next_base = line.base_main + gap + item.outer_main

        case wrap == :wrap and line.items != [] and next_base > available_main do
          true ->
            Enum.reverse(previous) ++ [line, %{items: [item], base_main: item.outer_main}]

          false ->
            Enum.reverse(previous) ++
              [%{line | items: line.items ++ [item], base_main: next_base}]
        end
    end
  end

  defp resolve_flex_line(line, available_main, constraint_available_main, gap) do
    item_gap_total = gap * max(length(line.items) - 1, 0)
    base_without_gap = Enum.reduce(line.items, 0.0, &(&1.outer_main + &2))

    free_space = available_main - base_without_gap - item_gap_total
    items = resolve_flex_item_sizes(line.items, free_space, constraint_available_main)
    outer_main = Enum.reduce(items, 0.0, &(&1.outer_main + &2)) + item_gap_total
    cross = items |> Enum.map(& &1.outer_cross) |> Enum.max(fn -> 0.0 end)

    %{items: items, main: outer_main, cross: cross}
  end

  defp resolve_flex_item_sizes(items, free_space, available_main) do
    {items, free_space} =
      Enum.map_reduce(items, free_space, fn item, remaining_space ->
        {minimum, maximum} = flex_main_constraints(item, available_main)
        constrained_main = constrain_flex_main(item.main_box, minimum, maximum)

        item =
          item
          |> resize_flex_item(constrained_main)
          |> Map.merge(%{
            flex_base_main: item.main_box,
            flex_minimum_main: minimum,
            flex_maximum_main: maximum,
            flex_frozen: false
          })

        {item, remaining_space - (constrained_main - item.flex_base_main)}
      end)

    mode =
      cond do
        free_space > 0 -> :grow
        free_space < 0 -> :shrink
        true -> :none
      end

    items
    |> redistribute_flex_space(free_space, mode)
    |> Enum.map(
      &Map.drop(&1, [:flex_base_main, :flex_minimum_main, :flex_maximum_main, :flex_frozen])
    )
  end

  defp redistribute_flex_space(items, free_space, mode) do
    total_weight =
      Enum.reduce(items, 0.0, fn item, total ->
        case item.flex_frozen do
          true -> total
          false -> total + flex_distribution_weight(item, mode)
        end
      end)

    case abs(free_space) <= 1.0e-9 or total_weight <= 0 do
      true ->
        items

      false ->
        {items, consumed_space, frozen_any?} =
          Enum.reduce(items, {[], 0.0, false}, fn item, {resolved, consumed, frozen_any?} ->
            weight = flex_distribution_weight(item, mode)

            case item.flex_frozen or weight <= 0 do
              true ->
                {resolved ++ [item], consumed, frozen_any?}

              false ->
                previous_main = item.main_box
                proposed_main = item.main_box + free_space * weight / total_weight

                constrained_main =
                  constrain_flex_main(
                    proposed_main,
                    item.flex_minimum_main,
                    item.flex_maximum_main
                  )

                case abs(constrained_main - proposed_main) > 1.0e-9 do
                  true ->
                    item =
                      item
                      |> resize_flex_item(constrained_main)
                      |> Map.put(:flex_frozen, true)

                    {
                      resolved ++ [item],
                      consumed + constrained_main - previous_main,
                      true
                    }

                  false ->
                    {resolved ++ [item], consumed, frozen_any?}
                end
            end
          end)

        case frozen_any? do
          true ->
            redistribute_flex_space(items, free_space - consumed_space, mode)

          false ->
            Enum.map(items, fn item ->
              weight = flex_distribution_weight(item, mode)

              case item.flex_frozen or weight <= 0 do
                true ->
                  item

                false ->
                  resize_flex_item(item, item.main_box + free_space * weight / total_weight)
              end
            end)
        end
    end
  end

  defp flex_distribution_weight(item, mode) do
    case mode do
      :grow -> item.flex_grow
      :shrink -> item.flex_shrink * item.flex_base_main
      :none -> 0.0
    end
  end

  defp flex_main_constraints(item, available_main) do
    {minimum_property, maximum_property} =
      case item.main_axis do
        :row -> {:min_width, :max_width}
        :column -> {:min_height, :max_height}
      end

    box_size = flex_main_box_size(item.style, item.main_axis)

    constraint_box_size = fn property, fallback ->
      case resolved_constraint_size(item.style, property, available_main) do
        constraint when is_number(constraint) ->
          case Map.get(item.style, :box_sizing, :content_box) do
            :border_box -> max(constraint, box_size)
            :content_box -> constraint + box_size
          end

        _ ->
          fallback
      end
    end

    minimum = constraint_box_size.(minimum_property, box_size)
    maximum = constraint_box_size.(maximum_property, nil)

    case maximum do
      maximum when is_number(maximum) -> {minimum, max(maximum, minimum)}
      nil -> {minimum, nil}
    end
  end

  defp constrain_flex_main(size, minimum, maximum) do
    size = max(size, minimum)

    case maximum do
      maximum when is_number(maximum) -> min(size, maximum)
      nil -> size
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
      tagged_boxes = tag_atomic_boxes(boxes, %{flow_id: container_flow_id})
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

    image_box =
      fitted_image_box(
        style,
        content_x,
        item.y - border_widths.top - padding.top - max(content_height, 0.0),
        max(content_width, 0.0),
        max(content_height, 0.0),
        image
      )

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

        content_height =
          Enum.reduce(lines, 0.0, &(&1.cross + &2)) +
            cross_gap * max(length(lines) - 1, 0)

        snap_to_pixel_grid? =
          Enum.any?(lines, fn line ->
            length(line.items) > 1 and Enum.all?(line.items, &(not Map.has_key?(&1, :image)))
          end)

        case snap_to_pixel_grid? do
          true -> Float.floor(content_height / @css_pixel_points) * @css_pixel_points
          false -> content_height
        end

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
        text
        |> normalize_inline_whitespace(style)
        |> trim_inline_whitespace()
        |> text_width(style)

      %{type: :element, style: %{display: :none}} ->
        0.0

      %{type: :element, style: %{display: :image} = style} ->
        {width, _height} = image_content_size(style)
        width + horizontal_box_size(style)

      %{type: :element, style: style, children: children} when is_list(children) ->
        case inline_runs(children) do
          {:ok, runs} ->
            runs
            |> Enum.reduce(0.0, fn run, width -> width + inline_run_width(run, nil) end)
            |> Kernel.+(horizontal_box_size(style))

          {:error, _reason} ->
            flex_block_intrinsic_width(children) + horizontal_box_size(style)
        end
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
    case {Map.get(style, :box_sizing, :content_box), property} do
      {:border_box, :width} ->
        box_size = horizontal_box_size(style)
        default = if is_number(default), do: default + box_size, else: default
        size = resolved_size(style, property, available_size, default)
        if is_number(size), do: max(size - box_size, 0.0), else: size

      {:border_box, :height} ->
        box_size = vertical_box_size(style)
        default = if is_number(default), do: default + box_size, else: default
        size = resolved_size(style, property, available_size, default)
        if is_number(size), do: max(size - box_size, 0.0), else: size

      _ ->
        resolved_size(style, property, available_size, default)
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
      case {collapsed?, Map.get(style, :width)} do
        {collapsed?, declared_width} when not is_nil(declared_width) ->
          style
          |> resolved_size(:width, available_box_width, available_box_width)
          |> Kernel.-(if(collapsed?, do: 0.0, else: horizontal_box_size(style)))
          |> max(0.0)

        _ ->
          resolved_content_size(
            style,
            :width,
            width_available_size(style, available_box_width),
            available_box_width - horizontal_box_size(style)
          )
      end

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
             table_columns(children),
             content_x,
             rows_top,
             content_width,
             table_id,
             Map.get(style, :border_collapse, :separate),
             Map.get(style, :border_spacing, {0.0, 0.0}),
             Map.get(style, :table_layout, :auto),
             if(collapsed?,
               do: resolved_size(style, :height, nil, nil),
               else: resolved_content_size(style, :height, nil, nil)
             )
           ) do
      content_height = content_top - content_bottom

      box_height =
        case collapsed? do
          true ->
            border_widths.top + padding.top + content_height + padding.bottom +
              border_widths.bottom

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

  defp table_columns(children) do
    children
    |> Enum.filter(&match?(%{style: %{display: :table_column_group}}, &1))
    |> Enum.flat_map(fn %{style: group_style, children: columns} ->
      group_width = Map.get(group_style, :width)

      case Enum.filter(columns, &match?(%{style: %{display: :table_column}}, &1)) do
        [] ->
          List.duplicate(%{width: group_width}, Map.get(group_style, :span, 1))

        columns ->
          Enum.flat_map(columns, fn %{style: column_style} ->
            column = %{width: Map.get(column_style, :width) || group_width}
            List.duplicate(column, Map.get(column_style, :span, 1))
          end)
      end
    end)
  end

  defp table_child_rows(child) do
    case child do
      %{style: %{display: display}}
      when display in [:none, :table_caption, :table_column_group] ->
        {:ok, []}

      %{style: %{display: :table_row}, children: cells} when is_list(cells) ->
        case visible_table_cells(cells) do
          [] ->
            {:ok, []}

          visible_cells ->
            {:ok,
             [%{row: Map.put(child, :children, visible_cells), section: :body, group_style: nil}]}
        end

      %{style: %{display: :table_row_group}, children: group_rows} when is_list(group_rows) ->
        section = child.style |> Map.get(:table_section, :body)

        Enum.reduce_while(group_rows, {:ok, []}, fn group_row, {:ok, rows} ->
          case table_group_row(group_row, section, child.style) do
            {:ok, table_rows} -> {:cont, {:ok, rows ++ table_rows}}
            {:error, reason} -> {:halt, {:error, reason}}
          end
        end)

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp table_group_row(row, section, group_style) do
    case row do
      %{style: %{display: :none}} ->
        {:ok, []}

      %{style: %{display: :table_row}, children: cells} when is_list(cells) ->
        case visible_table_cells(cells) do
          [] ->
            {:ok, []}

          visible_cells ->
            {:ok,
             [
               %{
                 row: Map.put(row, :children, visible_cells),
                 section: section,
                 group_style: group_style
               }
             ]}
        end

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp visible_table_cells(cells) do
    Enum.reject(cells, &match?(%{style: %{display: :none}}, &1))
  end

  defp layout_table_rows(
         rows,
         columns,
         x,
         y,
         width,
         table_id,
         border_collapse,
         border_spacing,
         table_layout,
         available_height
       ) do
    grid_rows = table_grid(rows)
    column_count = max(table_column_count(grid_rows), length(columns))

    {horizontal_spacing, vertical_spacing} =
      case {border_collapse, border_spacing} do
        {:separate, {horizontal, vertical}}
        when is_number(horizontal) and is_number(vertical) ->
          {
            Float.floor(horizontal / @css_pixel_points) * @css_pixel_points,
            Float.floor(vertical / @css_pixel_points) * @css_pixel_points
          }

        _ ->
          {0.0, 0.0}
      end

    column_grid_width = max(width - horizontal_spacing * (column_count + 1), 0.0)

    column_widths =
      table_column_widths(
        grid_rows,
        columns,
        column_count,
        column_grid_width,
        table_layout,
        border_collapse
      )

    with {:ok, row_heights} <-
           table_row_heights(
             grid_rows,
             column_widths,
             horizontal_spacing,
             vertical_spacing,
             available_height,
             border_collapse
           ) do
      {boxes, next_y} =
        grid_rows
        |> Enum.with_index()
        |> Enum.reduce({[], y - vertical_spacing}, fn {row, index}, {boxes, current_y} ->
          {:ok, row_boxes, next_y} =
            layout_table_row(
              row,
              table_id,
              index,
              x,
              current_y,
              column_widths,
              row_heights,
              border_collapse,
              horizontal_spacing,
              vertical_spacing
            )

          {boxes ++ row_boxes, next_y - vertical_spacing}
        end)

      {:ok, boxes, next_y}
    end
  end

  defp layout_table_row(
         %{
           row: %{style: %{display: :table_row} = style},
           section: section,
           group_style: group_style,
           cells: cells,
           consumed_columns: consumed_columns
         },
         table_id,
         index,
         x,
         y,
         column_widths,
         row_heights,
         border_collapse,
         horizontal_spacing,
         vertical_spacing
       ) do
    row_height = Enum.at(row_heights, index)
    last_row? = index == length(row_heights) - 1

    row_metadata =
      table_id
      |> table_row_metadata(section, index)
      |> Map.merge(break_metadata(style))

    group_background_boxes =
      case group_style do
        nil ->
          []

        group_style ->
          row_width = Enum.sum(column_widths) + horizontal_spacing * (length(column_widths) + 1)

          group_style
          |> Map.put(:border_widths, edges(0.0))
          |> background_box(x, y - row_height, row_width, row_height)
          |> tag_boxes(row_metadata)
          |> Enum.map(&Map.put(&1, :role, :table_row_group_background))
      end

    boxes =
      Enum.reduce(cells, [], fn
        %{cell: cell, column: column, colspan: colspan, rowspan: rowspan}, boxes ->
          cell_x =
            x + horizontal_spacing * (column + 1) +
              (column_widths |> Enum.take(column) |> Enum.sum())

          cell_width =
            table_cell_width(
              column_widths,
              column,
              colspan,
              horizontal_spacing
            )

          cell_height =
            (row_heights |> Enum.slice(index, rowspan) |> Enum.sum()) +
              vertical_spacing * max(rowspan - 1, 0)

          last_cell? = column + colspan >= length(column_widths)
          cell_last_row? = index + rowspan >= length(row_heights)

          {:ok, cell_boxes} =
            layout_table_cell(
              cell,
              cell_x,
              y,
              cell_width,
              cell_height,
              row_metadata,
              border_collapse,
              last_cell?,
              cell_last_row?
            )

          boxes ++ cell_boxes
      end)

    cells_for_border = Enum.map(cells, & &1.cell)

    boxes =
      boxes ++
        trailing_collapsed_table_border(
          cells_for_border,
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
      Enum.split_with(
        boxes,
        &(Map.get(&1, :role) == :table_cell_background and
            Map.get(&1, :paint_ordered, false) == false)
      )

    {border_boxes, content_boxes} =
      Enum.split_with(
        content_boxes,
        &(Map.get(&1, :role) == :table_border and Map.get(&1, :paint_ordered, false) == false)
      )

    paint_boxes =
      Enum.map(background_boxes ++ border_boxes, &Map.put(&1, :paint_ordered, true))

    {:ok, group_background_boxes ++ paint_boxes ++ content_boxes, y - row_height}
  end

  defp table_grid(rows) do
    {grid_rows, _occupied_until} =
      rows
      |> Enum.with_index()
      |> Enum.map_reduce(%{}, fn {%{row: %{children: cells}} = row, row_index}, occupied_until ->
        row_group_length =
          rows
          |> Enum.drop(row_index)
          |> Enum.take_while(&(&1.section == row.section))
          |> length()

        active_columns =
          occupied_until
          |> Enum.filter(fn {_column, end_row} -> end_row > row_index end)
          |> Enum.map(&elem(&1, 0))

        {placed_cells, occupied_until, _next_column} =
          Enum.reduce(cells, {[], occupied_until, 0}, fn cell,
                                                         {placements, occupied, next_column} ->
            colspan = table_cell_colspan(cell)
            rowspan = min(table_cell_rowspan(cell), row_group_length)

            column =
              next_table_cell_column(occupied, row_index, next_column, colspan)

            occupied =
              case rowspan > 1 do
                true ->
                  Enum.reduce(column..(column + colspan - 1), occupied, fn occupied_column, acc ->
                    Map.put(acc, occupied_column, row_index + rowspan)
                  end)

                false ->
                  occupied
              end

            placement = %{
              cell: cell,
              column: column,
              colspan: colspan,
              rowspan: rowspan
            }

            {placements ++ [placement], occupied, column + colspan}
          end)

        covered_columns =
          Enum.map(placed_cells, &(&1.column + &1.colspan - 1)) ++ active_columns

        consumed_columns = Enum.max(covered_columns) + 1

        {Map.merge(
           row,
           %{
             cells: placed_cells,
             active_columns: active_columns,
             consumed_columns: consumed_columns
           }
         ), occupied_until}
      end)

    grid_rows
  end

  defp next_table_cell_column(occupied_until, row_index, candidate, colspan) do
    occupied? =
      Enum.any?(candidate..(candidate + colspan - 1), fn column ->
        Map.get(occupied_until, column, 0) > row_index
      end)

    case occupied? do
      true -> next_table_cell_column(occupied_until, row_index, candidate + 1, colspan)
      false -> candidate
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
       ) do
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))
    content_x = x + border_widths.left + padding.left

    content_width =
      width - border_widths.left - padding.left - padding.right - border_widths.right

    content_top = y - border_widths.top - padding.top

    content_area_height =
      max(
        height - border_widths.top - padding.top - padding.bottom - border_widths.bottom,
        0.0
      )

    with {:ok, content_boxes, content_bottom} <-
           layout_table_cell_content(
             children,
             style,
             content_x,
             content_top,
             content_width,
             content_area_height
           ) do
      content_height = content_top - content_bottom

      vertical_offset = table_cell_vertical_offset(style, content_area_height, content_height)

      content_boxes =
        Enum.map(content_boxes, fn box ->
          Map.update!(box, :y, &(&1 - vertical_offset))
        end)

      cell_box =
        table_cell_background_box(style, x, y - height, width, height, border_collapse)
        |> tag_boxes(row_metadata)
        |> Enum.map(fn box ->
          box
          |> Map.put(:role, :table_cell_background)
        end)

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
        |> Enum.map(fn box ->
          box
          |> Map.put(:role, :table_border)
        end)

      {:ok, cell_box ++ border_box ++ tag_atomic_boxes(content_boxes, row_metadata)}
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
        border_styles = Map.get(style, :border_styles, edges(:solid))

        border_colors =
          Map.get(style, :border_colors, edges(Map.get(style, :border_color, {0, 0, 0})))

        collapsed_widths = %{
          top: border_widths.top,
          right: if(last_cell?, do: border_widths.right, else: 0.0),
          bottom: if(last_row?, do: border_widths.bottom, else: 0.0),
          left: border_widths.left
        }

        stroke_width = collapsed_widths |> Map.values() |> Enum.max()

        case visible_border?(collapsed_widths, border_styles, border_colors) do
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
                border_colors: border_colors,
                border_styles: border_styles,
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
        |> Enum.map(fn box ->
          box
          |> Map.put(:role, :table_border)
        end)

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

  defp table_column_count(rows) do
    rows
    |> Enum.flat_map(& &1.cells)
    |> Enum.map(&(&1.column + &1.colspan))
    |> Enum.max()
  end

  defp table_column_widths(
         rows,
         columns,
         column_count,
         table_width,
         table_layout,
         border_collapse
       ) do
    column_preferred =
      0..(column_count - 1)
      |> Enum.map(fn index ->
        columns
        |> Enum.at(index, %{})
        |> Map.get(:width)
        |> table_column_preferred_width(table_width)
      end)

    width_rows = if table_layout == :fixed, do: Enum.take(rows, 1), else: rows

    percentage_total =
      Enum.reduce(width_rows, 0.0, fn %{cells: cells}, total ->
        Enum.reduce(cells, total, fn
          %{cell: %{style: %{width: {:percent, ratio}}}}, acc ->
            acc + ratio

          _placement, acc ->
            acc
        end)
      end)

    percentage_widths_fill_table? = percentage_total >= 0.9999

    explicitly_sized_columns =
      columns
      |> Enum.with_index()
      |> Enum.reduce(MapSet.new(), fn
        {%{width: {:percent, ratio}}, index}, indexes when is_number(ratio) ->
          MapSet.put(indexes, index)

        {%{width: width}, index}, indexes when is_number(width) ->
          MapSet.put(indexes, index)

        {_column, _index}, indexes ->
          indexes
      end)
      |> then(fn indexes ->
        Enum.reduce(width_rows, indexes, fn %{cells: cells}, indexes ->
          Enum.reduce(cells, indexes, fn
            %{cell: %{style: %{width: {:percent, ratio}}}, column: index, colspan: 1}, acc
            when is_number(ratio) ->
              MapSet.put(acc, index)

            %{cell: %{style: %{width: width}}, column: index, colspan: 1}, acc
            when is_number(width) ->
              MapSet.put(acc, index)

            _placement, acc ->
              acc
          end)
        end)
      end)

    preferred =
      Enum.reduce(width_rows, column_preferred, fn %{cells: cells}, widths ->
        Enum.reduce(cells, widths, fn
          %{cell: cell, column: index, colspan: colspan}, acc ->
            preferred_width =
              case {table_layout, colspan, Map.get(cell.style, :width)} do
                {:auto, colspan, width} when colspan > 1 and width in [nil, :auto] ->
                  nil

                {:auto, 1, width} when width in [nil, :auto] ->
                  case {MapSet.member?(explicitly_sized_columns, index), percentage_total > 0} do
                    {true, _has_percentage_widths?} ->
                      nil

                    {false, true} ->
                      table_cell_preferred_width(
                        cell,
                        table_width,
                        percentage_widths_fill_table?,
                        table_layout,
                        border_collapse
                      )

                    {false, false} ->
                      nil
                  end

                _ ->
                  table_cell_preferred_width(
                    cell,
                    table_width,
                    percentage_widths_fill_table?,
                    table_layout,
                    border_collapse
                  )
              end

            case preferred_width do
              nil ->
                acc

              preferred_width ->
                share = preferred_width / colspan

                Enum.reduce(index..(index + colspan - 1), acc, fn column, column_acc ->
                  List.update_at(column_acc, column, fn
                    nil -> share
                    width when table_layout == :fixed -> width
                    width -> max(width, share)
                  end)
                end)
            end
        end)
      end)

    minimum =
      case table_layout do
        :fixed -> List.duplicate(0.0, column_count)
        _ -> table_minimum_column_widths(rows, column_count)
      end
      |> Enum.with_index()
      |> Enum.map(fn {width, index} ->
        case MapSet.member?(explicitly_sized_columns, index) do
          true -> 0.0
          false -> width
        end
      end)

    fixed_total = preferred |> Enum.reject(&is_nil/1) |> Enum.sum()
    flexible_count = Enum.count(preferred, &is_nil/1)

    cond do
      table_layout == :fixed and fixed_total > table_width and fixed_total > 0 ->
        Enum.map(preferred, fn
          nil -> 0.0
          width -> width / fixed_total * table_width
        end)

      table_layout == :fixed and flexible_count > 0 ->
        flexible_width = max((table_width - fixed_total) / flexible_count, 0.0)
        Enum.map(preferred, fn width -> width || flexible_width end)

      table_layout == :fixed and column_count > 0 ->
        extra_width = max((table_width - fixed_total) / column_count, 0.0)
        Enum.map(preferred, &(&1 + extra_width))

      fixed_total > table_width and fixed_total > 0 and flexible_count > 0 ->
        Enum.map(preferred, fn
          nil -> 0.0
          width -> width / fixed_total * table_width
        end)

      fixed_total > table_width and fixed_total > 0 and percentage_total <= 0 ->
        Enum.map(preferred, &(&1 / fixed_total * table_width))

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

  defp table_column_preferred_width(width, table_width) do
    case width do
      {:percent, ratio} when is_number(ratio) -> table_width * ratio
      width when is_number(width) and width > 0 -> min(width, table_width)
      _ -> nil
    end
  end

  defp table_minimum_column_widths(rows, column_count) do
    Enum.reduce(rows, List.duplicate(0.0, column_count), fn %{cells: cells}, widths ->
      Enum.reduce(cells, widths, fn
        %{cell: cell, column: index, colspan: colspan}, acc ->
          case {colspan, table_cell_minimum_width(cell)} do
            {1, min_width} when min_width > 0 ->
              List.update_at(acc, index, &max(&1, min_width))

            _ ->
              acc
          end
      end)
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
                case run do
                  %{type: :inline_block} ->
                    [%{width: inline_run_width(run, nil)}]

                  _ ->
                    run.text
                    |> inline_wrap_tokens(run.style)
                    |> Enum.map(&%{text: trim_inline_whitespace(&1), style: run.style})
                end
              end)
              |> Enum.flat_map(fn
                %{width: width} -> [width]
                %{text: ""} -> []
                run -> [text_width(run.text, run.style)]
              end)
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

  defp table_cell_preferred_width(
         cell,
         table_width,
         percentage_widths_fill_table?,
         table_layout,
         border_collapse
       ) do
    case cell do
      %{style: %{width: {:percent, ratio}} = style} when is_number(ratio) ->
        table_width * ratio +
          if(percentage_widths_fill_table?,
            do: 0.0,
            else: table_cell_horizontal_box_size(style, border_collapse)
          )

      %{style: %{width: width} = style} when is_number(width) and width > 0 ->
        min(width + horizontal_box_size(style), table_width)

      %{style: %{display: :table_cell} = style, children: children}
      when table_layout == :auto and is_list(children) ->
        case inline_runs(children) do
          {:ok, runs} ->
            runs
            |> Enum.reduce(0.0, fn run, width -> width + inline_run_width(run, nil) end)
            |> Kernel.+(horizontal_box_size(style))
            |> min(table_width)

          {:error, _reason} ->
            nil
        end

      _ ->
        nil
    end
  end

  defp table_cell_horizontal_box_size(style, border_collapse) do
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))

    horizontal_border_size =
      case border_collapse do
        :collapse -> max(border_widths.left, border_widths.right)
        _ -> border_widths.left + border_widths.right
      end

    padding.left + padding.right + horizontal_border_size
  end

  defp table_cell_width(
         column_widths,
         index,
         colspan,
         horizontal_spacing
       ) do
    (column_widths |> Enum.slice(index, colspan) |> Enum.sum()) +
      horizontal_spacing * max(colspan - 1, 0)
  end

  defp table_row_heights(
         rows,
         column_widths,
         horizontal_spacing,
         vertical_spacing,
         available_height,
         border_collapse
       ) do
    initial_heights =
      Enum.reduce_while(rows, {:ok, []}, fn
        %{row: %{style: style}, cells: cells}, {:ok, row_heights} ->
          result =
            cells
            |> Enum.filter(&(&1.rowspan == 1))
            |> Enum.reduce_while({:ok, []}, fn placement, {:ok, cell_heights} ->
              width =
                table_cell_width(
                  column_widths,
                  placement.column,
                  placement.colspan,
                  horizontal_spacing
                )

              case table_cell_height(placement.cell, width, border_collapse) do
                {:ok, height} -> {:cont, {:ok, cell_heights ++ [height]}}
                {:error, reason} -> {:halt, {:error, reason}}
              end
            end)

          case result do
            {:ok, cell_heights} ->
              intrinsic_height = Enum.max(cell_heights, fn -> 0.0 end)

              {:cont,
               {:ok, row_heights ++ [max(intrinsic_height, table_row_declared_height(style))]}}

            {:error, reason} ->
              {:halt, {:error, reason}}
          end
      end)

    adjusted_heights =
      case initial_heights do
        {:ok, heights} ->
          rows
          |> Enum.with_index()
          |> Enum.reduce_while({:ok, heights}, fn {%{cells: cells}, row_index},
                                                  {:ok, row_heights} ->
            result =
              cells
              |> Enum.filter(&(&1.rowspan > 1))
              |> Enum.reduce_while({:ok, row_heights}, fn placement, {:ok, acc} ->
                width =
                  table_cell_width(
                    column_widths,
                    placement.column,
                    placement.colspan,
                    horizontal_spacing
                  )

                case table_cell_height(placement.cell, width, border_collapse) do
                  {:ok, required_height} ->
                    current_height =
                      acc
                      |> Enum.slice(row_index, placement.rowspan)
                      |> Enum.sum()

                    current_height =
                      current_height + vertical_spacing * max(placement.rowspan - 1, 0)

                    extra_height = max(required_height - current_height, 0.0)
                    share = extra_height / placement.rowspan

                    adjusted =
                      acc
                      |> Enum.with_index()
                      |> Enum.map(fn
                        {height, index}
                        when index >= row_index and index < row_index + placement.rowspan ->
                          height + share

                        {height, _index} ->
                          height
                      end)

                    {:cont, {:ok, adjusted}}

                  {:error, reason} ->
                    {:halt, {:error, reason}}
                end
              end)

            case result do
              {:ok, adjusted} -> {:cont, {:ok, adjusted}}
              {:error, reason} -> {:halt, {:error, reason}}
            end
          end)

        {:error, reason} ->
          {:error, reason}
      end

    case adjusted_heights do
      {:ok, heights} ->
        heights =
          case {border_collapse, available_height} do
            {:collapse, available_height} when not is_number(available_height) ->
              outer_bottom_border =
                rows
                |> List.last(%{cells: []})
                |> Map.fetch!(:cells)
                |> Enum.map(fn %{cell: cell} ->
                  cell.style
                  |> Map.get(:border_widths, edges(0.0))
                  |> Map.fetch!(:bottom)
                end)
                |> Enum.max()

              List.update_at(heights, -1, &(&1 + outer_bottom_border))

            _ ->
              heights
          end

        {:ok, stretch_table_row_heights(rows, heights, available_height, vertical_spacing)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp stretch_table_row_heights(rows, heights, available_height, vertical_spacing) do
    case available_height do
      available_height when is_number(available_height) and heights != [] ->
        row_space =
          max(available_height - vertical_spacing * (length(heights) + 1), 0.0)

        extra_height = max(row_space - Enum.sum(heights), 0.0)

        percentage_rows =
          rows
          |> Enum.with_index()
          |> Enum.flat_map(fn
            {%{row: %{style: %{height: {:percent, ratio}}}}, index}
            when is_number(ratio) and ratio > 0 ->
              [{index, ratio}]

            {_row, _index} ->
              []
          end)

        case {extra_height, percentage_rows} do
          {extra_height, _percentage_rows} when extra_height <= 0 ->
            heights

          {extra_height, []} ->
            share = extra_height / length(heights)
            Enum.map(heights, &(&1 + share))

          {extra_height, percentage_rows} ->
            ratio_total =
              Enum.reduce(percentage_rows, 0.0, fn {_index, ratio}, acc -> acc + ratio end)

            Enum.with_index(heights, fn height, index ->
              case List.keyfind(percentage_rows, index, 0) do
                {_index, ratio} -> height + extra_height * ratio / ratio_total
                nil -> height
              end
            end)
        end

      _ ->
        heights
    end
  end

  defp table_row_declared_height(style) do
    resolved_content_size(style, :height, nil, nil) || 0.0
  end

  defp table_cell_height(cell, width, border_collapse) do
    case cell do
      %{style: %{display: :table_cell} = style, children: children} when is_list(children) ->
        padding = Map.get(style, :padding, edges(0.0))
        border_widths = Map.get(style, :border_widths, edges(0.0))

        content_width =
          width - border_widths.left - padding.left - padding.right - border_widths.right

        with {:ok, _boxes, content_bottom} <-
               layout_table_cell_content(children, style, 0.0, 0.0, content_width, nil) do
          content_height = 0.0 - content_bottom

          border_height =
            case border_collapse do
              :collapse -> max(border_widths.top, border_widths.bottom)
              _ -> border_widths.top + border_widths.bottom
            end

          content_box_height = padding.top + content_height + padding.bottom + border_height

          {:ok, max(content_box_height, table_cell_declared_height(style, border_collapse))}
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

  defp table_cell_rowspan(cell) do
    case cell do
      %{style: %{rowspan: rowspan}} when is_integer(rowspan) and rowspan >= 1 -> rowspan
      _ -> 1
    end
  end

  defp table_cell_declared_height(style, border_collapse) do
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))

    box_inset =
      case border_collapse do
        :collapse ->
          padding.top + padding.bottom + max(border_widths.top, border_widths.bottom)

        _ ->
          vertical_box_size(style)
      end

    [
      resolved_content_size(style, :height, nil, nil),
      resolved_content_size(style, :min_height, nil, nil)
    ]
    |> Enum.filter(&is_number/1)
    |> Enum.map(&(&1 + box_inset))
    |> Enum.max(fn -> 0.0 end)
  end

  defp layout_table_cell_content(children, style, x, y, width, available_height) do
    case inline_runs(children) do
      {:ok, runs} ->
        {boxes, content_height} = inline_text_boxes(runs, style, x, y, width, %{})
        {:ok, boxes, y - content_height}

      {:error, _reason} ->
        layout_table_cell_blocks(children, style, x, y, width, available_height)
    end
  end

  defp layout_table_cell_blocks(children, style, x, y, width, available_height) do
    result =
      Enum.reduce_while(children, {:ok, [], y, nil}, fn child,
                                                        {:ok, boxes, current_y,
                                                         previous_margin_bottom} ->
        {margin_top, margin_bottom} = flow_child_vertical_margins(child, style)
        collapsed_margin = collapsed_sibling_margin(previous_margin_bottom, margin_top)

        remaining_height =
          case available_height do
            available_height when is_number(available_height) ->
              max(available_height - (y - current_y), 0.0)

            _ ->
              nil
          end

        case layout_table_cell_block(
               child,
               style,
               x,
               current_y + collapsed_margin,
               width,
               remaining_height
             ) do
          {:ok, child_boxes, next_y} ->
            next_margin_bottom =
              following_sibling_margin(
                child_boxes,
                previous_margin_bottom,
                margin_top,
                margin_bottom
              )

            {:cont, {:ok, boxes ++ child_boxes, next_y, next_margin_bottom}}

          {:error, reason} ->
            {:halt, {:error, reason}}
        end
      end)

    case result do
      {:ok, boxes, next_y, _margin_bottom} -> {:ok, boxes, next_y}
      {:error, reason} -> {:error, reason}
    end
  end

  defp layout_table_cell_block(child, style, x, y, width, available_height) do
    case child do
      %{type: :text, text: text} when is_binary(text) ->
        case trim_inline_whitespace(text) do
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
        child_style = Map.fetch!(child, :style)

        child =
          case resolved_size(child_style, :height, available_height, nil) do
            height when is_number(height) ->
              Map.put(child, :style, Map.put(child_style, :height, height))

            _ ->
              child
          end

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
        acc + inline_run_width(run, width)
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
    available_box_width = width - margin.left - margin.right

    content_width =
      resolved_content_size(
        style,
        :width,
        width_available_size(style, available_box_width),
        available_box_width - horizontal_box_size(style)
      )

    box_width = content_width + horizontal_box_size(style)
    content_x = box_x + border_widths.left + padding.left
    content_top = box_top - border_widths.top - padding.top

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
    lines = inline_lines(runs, width)
    last_line_index = length(lines) - 1

    {boxes, _consumed_height} =
      lines
      |> Enum.with_index()
      |> Enum.reduce({[], 0.0}, fn {line_runs, index}, {boxes, consumed_height} ->
        current_line_height = inline_line_height(line_runs, line_height, width)
        baseline_depth = inline_line_baseline_depth(line_runs, style)
        line_top = y - consumed_height
        baseline_y = line_top - baseline_depth
        start_x = aligned_text_x(line_runs, style, x, width)

        line_metadata =
          case Map.get(metadata, :fragment_lines, false) do
            true ->
              metadata
              |> Map.put(:fragment_id, {:line, Map.fetch!(metadata, :flow_id), index})
              |> Map.put(
                :break_before,
                if(index == 0, do: Map.get(metadata, :break_before, :auto), else: :auto)
              )
              |> Map.put(
                :break_after,
                if(index == last_line_index,
                  do: Map.get(metadata, :break_after, :auto),
                  else: :auto
                )
              )

            false ->
              metadata
          end

        {line_boxes, _next_x} =
          Enum.reduce(line_runs, {[], start_x}, fn run, {acc, current_x} ->
            run_boxes =
              inline_run_boxes(run, current_x, line_top, baseline_y, width, line_metadata)

            {acc ++ run_boxes, current_x + inline_run_width(run, width)}
          end)

        {boxes ++ line_boxes, consumed_height + current_line_height}
      end)

    {boxes, inline_content_height(runs, lines, line_height)}
  end

  defp inline_run_boxes(run, x, line_top, baseline_y, width, metadata) do
    case run do
      %{type: :inline_block, style: style, runs: child_runs} ->
        margin = Map.get(style, :margin, edges(0.0))
        padding = Map.get(style, :padding, edges(0.0))
        border_widths = Map.get(style, :border_widths, edges(0.0))
        content_width = inline_block_content_width(run, width)
        content_height = inline_block_content_height(run, content_width)
        box_width = content_width + horizontal_box_size(style)
        box_height = content_height + vertical_box_size(style)
        block_baseline_depth = border_widths.top + padding.top + Map.fetch!(style, :font_size)
        box_top = line_top - max(line_top - block_baseline_depth - baseline_y, 0.0)
        box_x = x + margin.left
        content_x = box_x + border_widths.left + padding.left
        content_top = box_top - border_widths.top - padding.top

        background =
          style
          |> background_box(box_x, box_top - box_height, box_width, box_height)
          |> Enum.map(&Map.merge(&1, metadata))

        {content_boxes, _height} =
          inline_text_boxes(child_runs, style, content_x, content_top, content_width, metadata)

        background ++ content_boxes

      _ ->
        [run |> text_box(x, baseline_y, width) |> Map.merge(metadata)]
    end
  end

  defp inline_content_height(runs, width_or_lines, line_height) do
    case width_or_lines do
      width when is_number(width) ->
        runs
        |> inline_lines(width)
        |> then(&inline_content_height(runs, &1, line_height))

      lines when is_list(lines) ->
        case runs do
          [] -> 0.0
          _ -> Enum.reduce(lines, 0.0, &(&2 + inline_line_height(&1, line_height, nil)))
        end
    end
  end

  defp inline_line_height(line_runs, line_height, available_width) do
    Enum.reduce(line_runs, line_height, fn run, height ->
      case run do
        %{type: :inline_block, style: style} ->
          margin = Map.get(style, :margin, edges(0.0))
          content_width = inline_block_content_width(run, available_width)

          max(
            height,
            inline_block_content_height(run, content_width) + vertical_box_size(style) +
              margin.top + margin.bottom
          )

        _ ->
          height
      end
    end)
  end

  defp inline_line_baseline_depth(line_runs, parent_style) do
    parent_font_size = Map.fetch!(parent_style, :font_size)

    largest_text_font_size =
      Enum.reduce(line_runs, parent_font_size, fn run, font_size ->
        case run do
          %{type: :inline_block} -> font_size
          _ -> max(font_size, Map.fetch!(run.style, :font_size))
        end
      end)

    text_baseline_depth =
      case largest_text_font_size > parent_font_size do
        true -> max(parent_font_size, Map.fetch!(parent_style, :line_height))
        false -> parent_font_size
      end

    Enum.reduce(line_runs, text_baseline_depth, fn run, depth ->
      case run do
        %{type: :inline_block, style: style} ->
          padding = Map.get(style, :padding, edges(0.0))
          border_widths = Map.get(style, :border_widths, edges(0.0))
          max(depth, border_widths.top + padding.top + Map.fetch!(style, :font_size))

        _ ->
          depth
      end
    end)
  end

  defp inline_lines(runs, width) do
    runs
    |> Enum.reduce([[]], fn run, lines ->
      case run do
        %{type: :inline_block} ->
          append_inline_atomic(lines, run, width)

        _ ->
          parts = String.split(run.text, "\n", trim: false)

          parts
          |> Enum.with_index()
          |> Enum.reduce(lines, fn {part, index}, acc ->
            acc =
              case part do
                "" -> acc
                _ -> append_wrapped_inline_text(acc, %{run | text: part}, width)
              end

            case index < length(parts) - 1 do
              true -> acc ++ [[]]
              false -> acc
            end
          end)
      end
    end)
    |> maybe_drop_trailing_break_line(runs)
    |> Enum.map(&trim_trailing_inline_whitespace/1)
  end

  defp maybe_drop_trailing_break_line(lines, runs) do
    last_meaningful_run =
      runs
      |> Enum.reverse()
      |> Enum.find(fn run ->
        Map.get(run, :hard_break, false) or trim_inline_whitespace(run.text) != ""
      end)

    case {last_meaningful_run, List.last(lines)} do
      {%{hard_break: true}, []} when length(lines) > 1 -> Enum.drop(lines, -1)
      _ -> lines
    end
  end

  defp trim_trailing_inline_whitespace(line_runs) do
    trailing_run_count =
      line_runs
      |> Enum.reverse()
      |> Enum.find_index(fn run ->
        Map.get(run, :type) == :inline_block or trim_inline_whitespace(run.text) != ""
      end)

    case trailing_run_count do
      nil ->
        []

      trailing_run_count ->
        line_runs
        |> Enum.take(length(line_runs) - trailing_run_count)
        |> List.update_at(-1, fn
          %{type: :inline_block} = run -> run
          run -> %{run | text: String.replace(run.text, ~r/[ \t\f\r]+$/u, "")}
        end)
    end
  end

  defp append_inline_atomic(lines, run, width) do
    current_line = List.last(lines) || []

    case current_line != [] and
           inline_line_width(current_line, width) + inline_run_width(run, width) >
             width do
      true -> lines ++ [[run]]
      false -> List.update_at(lines, length(lines) - 1, &(&1 ++ [run]))
    end
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
        ~r/[^ \t\n\f\r]+[ \t\n\f\r]*|[ \t\n\f\r]+/u
        |> Regex.scan(text)
        |> Enum.map(&List.first/1)
    end
  end

  defp append_inline_token(lines, run, width) do
    current_line = List.last(lines) || []
    token_width = text_width(run.text, run.style)

    cond do
      trim_inline_whitespace(run.text) == "" and current_line == [] ->
        lines

      token_width > width and Map.get(run.style, :line_break) == :break_word ->
        append_break_word_token(lines, run, width)

      current_line != [] and
          inline_line_width(current_line, width) + token_width > width + @line_wrap_tolerance ->
        lines ++ [[%{run | text: trim_leading_inline_whitespace(run.text)}]]

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

  defp inline_line_width(line_runs, available_width) do
    Enum.reduce(line_runs, 0.0, fn run, width ->
      width + inline_run_width(run, available_width)
    end)
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
        text = normalize_inline_whitespace(text, style)
        {:ok, append_whitespace_collapsed_run(runs, text, style)}

      %{type: :element, style: %{display: :inline}, children: children}
      when is_list(children) ->
        case inline_runs(children) do
          {:ok, child_runs} ->
            {:ok,
             Enum.reduce(child_runs, runs, fn run, acc ->
               append_whitespace_collapsed_run(acc, run.text, run.style)
             end)}

          {:error, reason} ->
            {:error, reason}
        end

      %{type: :element, style: %{display: :inline_block} = style, children: children}
      when is_list(children) ->
        case inline_runs(children) do
          {:ok, child_runs} ->
            {:ok, runs ++ [%{type: :inline_block, text: "", style: style, runs: child_runs}]}

          {:error, reason} ->
            {:error, reason}
        end

      %{type: :element, style: %{display: :none}} ->
        {:ok, runs}

      %{type: :element, style: %{display: :line_break} = style, children: []} ->
        {:ok, runs ++ [%{text: "\n", style: text_style(style), hard_break: true}]}

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp inline_run_width(run, available_width) do
    case run do
      %{type: :inline_block, style: style} ->
        margin = Map.get(style, :margin, edges(0.0))

        inline_block_content_width(run, available_width) + horizontal_box_size(style) +
          margin.left + margin.right

      _ ->
        text_width(run.text, run.style)
    end
  end

  defp inline_block_content_width(%{style: style, runs: runs}, available_width) do
    intrinsic_width = Enum.reduce(runs, 0.0, &(&2 + inline_run_width(&1, available_width)))
    resolved_content_size(style, :width, available_width, intrinsic_width)
  end

  defp inline_block_content_height(%{style: style, runs: runs}, content_width) do
    line_height = Map.fetch!(style, :line_height)
    intrinsic_height = inline_content_height(runs, content_width, line_height)
    resolved_content_size(style, :height, nil, intrinsic_height)
  end

  defp normalize_inline_whitespace(text, style) do
    normalized_newlines = String.replace(text, ~r/\r\n?|\n/u, "\n")

    case Map.get(style, :white_space, :normal) do
      :pre_line ->
        normalized_newlines
        |> String.split("\n", trim: false)
        |> Enum.map(&String.replace(&1, ~r/[ \t\f]+/u, " "))
        |> Enum.join("\n")

      _ ->
        String.replace(normalized_newlines, ~r/[ \t\n\f]+/u, " ")
    end
  end

  defp append_whitespace_collapsed_run(runs, text, style) do
    text =
      case List.last(runs) do
        %{text: previous_text} ->
          case Regex.match?(~r/[ \t\n\f\r]$/u, previous_text) do
            true -> String.replace(text, ~r/^[ \t\f]+/u, "")
            false -> text
          end

        _ ->
          text
      end

    runs ++ [%{text: text, style: style}]
  end

  defp trim_inline_whitespace(text) do
    String.replace(text, ~r/^[ \t\n\f\r]+|[ \t\n\f\r]+$/u, "")
  end

  defp trim_leading_inline_whitespace(text) do
    String.replace(text, ~r/^[ \t\n\f\r]+/u, "")
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
      snap_to_css_pixel_grid: true,
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
    border_styles = Map.get(style, :border_styles, edges(:solid))

    border_colors =
      Map.get(style, :border_colors, edges(Map.get(style, :border_color, {0, 0, 0})))

    stroke_width = Enum.max(Map.values(border_widths))
    fill_color = Map.get(style, :background_color)

    border_visible? = visible_border?(border_widths, border_styles, border_colors)

    rect = %{
      type: :rect,
      paint_layer: :container_background,
      snap_to_css_pixel_grid: true,
      x: x,
      y: y,
      width: width,
      height: height,
      fill_color: fill_color,
      stroke_color: Map.get(style, :border_color, {0, 0, 0}),
      stroke_width: stroke_width,
      border_widths: border_widths,
      border_colors: border_colors,
      border_styles: border_styles,
      border_radius: Map.get(style, :border_radius, 0.0)
    }

    case Map.get(style, :background_image) do
      nil ->
        case {fill_color, border_visible?} do
          {nil, false} -> []
          _ -> [rect]
        end

      image ->
        fill_boxes =
          case fill_color do
            nil ->
              []

            _ ->
              [
                %{rect | stroke_color: nil, stroke_width: 0.0, border_widths: edges(0.0)}
              ]
          end

        border_boxes =
          case border_visible? do
            true -> [%{rect | fill_color: nil}]
            false -> []
          end

        fill_boxes ++
          background_image_boxes(style, image, x, y, width, height, border_widths) ++
          border_boxes
    end
  end

  defp fitted_image_box(style, viewport_x, viewport_y, viewport_width, viewport_height, image) do
    case Map.get(style, :object_fit, :fill) do
      :fill ->
        %{
          type: :image,
          snap_to_css_pixel_grid: true,
          x: viewport_x,
          y: viewport_y,
          width: viewport_width,
          height: viewport_height,
          image: image
        }

      fit when fit in [:contain, :cover] ->
        natural_width = Map.fetch!(image, :width)
        natural_height = Map.fetch!(image, :height)

        scale =
          case fit do
            :contain -> min(viewport_width / natural_width, viewport_height / natural_height)
            :cover -> max(viewport_width / natural_width, viewport_height / natural_height)
          end

        rendered_width = natural_width * scale
        rendered_height = natural_height * scale

        {horizontal, vertical} =
          Map.get(style, :object_position, {{:percent, 0.5}, {:percent, 0.5}})

        %{
          type: :image,
          snap_to_css_pixel_grid: true,
          x: viewport_x + paint_position(horizontal, viewport_width - rendered_width),
          y:
            viewport_y + viewport_height - rendered_height -
              paint_position(vertical, viewport_height - rendered_height),
          width: rendered_width,
          height: rendered_height,
          image: image,
          clip: %{x: viewport_x, y: viewport_y, width: viewport_width, height: viewport_height}
        }
    end
  end

  defp background_image_boxes(style, image, x, y, width, height, border_widths) do
    area_x = x + border_widths.left
    area_y = y + border_widths.bottom
    area_width = max(width - border_widths.left - border_widths.right, 0.0)
    area_height = max(height - border_widths.top - border_widths.bottom, 0.0)
    {tile_width, tile_height} = background_image_size(style, image, area_width, area_height)
    {horizontal, vertical} = Map.get(style, :background_position)
    initial_x = area_x + paint_position(horizontal, area_width - tile_width)

    initial_y =
      area_y + area_height - tile_height - paint_position(vertical, area_height - tile_height)

    repeat = Map.get(style, :background_repeat, :repeat)

    x_positions =
      tile_positions(initial_x, tile_width, x, x + width, repeat in [:repeat, :repeat_x])

    y_positions =
      tile_positions(initial_y, tile_height, y, y + height, repeat in [:repeat, :repeat_y])

    tile_count = length(x_positions) * length(y_positions)

    case tile_count <= Limits.get(:max_background_image_tiles) do
      true ->
        for tile_x <- x_positions, tile_y <- y_positions do
          %{
            type: :image,
            paint_layer: :container_background,
            snap_to_css_pixel_grid: true,
            x: tile_x,
            y: tile_y,
            width: tile_width,
            height: tile_height,
            image: image,
            clip: %{x: x, y: y, width: width, height: height}
          }
        end

      false ->
        [%{type: :layout_error, reason: :background_image_tile_limit}]
    end
  end

  defp background_image_size(style, image, area_width, area_height) do
    natural_width = Map.fetch!(image, :width)
    natural_height = Map.fetch!(image, :height)

    case Map.get(style, :background_size, {:auto, :auto}) do
      :contain ->
        scale = min(area_width / natural_width, area_height / natural_height)
        {natural_width * scale, natural_height * scale}

      :cover ->
        scale = max(area_width / natural_width, area_height / natural_height)
        {natural_width * scale, natural_height * scale}

      {width, height} ->
        resolved_width = resolve_background_size(width, area_width)
        resolved_height = resolve_background_size(height, area_height)

        case {resolved_width, resolved_height} do
          {:auto, :auto} -> {natural_width, natural_height}
          {width, :auto} -> {width, width * natural_height / natural_width}
          {:auto, height} -> {height * natural_width / natural_height, height}
          {width, height} -> {width, height}
        end
    end
  end

  defp resolve_background_size(size, available) do
    case size do
      :auto -> :auto
      {:percent, ratio} -> available * ratio
      size -> size
    end
  end

  defp paint_position(position, remaining) do
    case position do
      {:percent, ratio} -> remaining * ratio
      length -> length
    end
  end

  defp tile_positions(initial, tile_size, minimum, maximum, repeat?) do
    case repeat? and tile_size > 0 do
      true ->
        first = initial - max(Float.ceil((initial - minimum) / tile_size), 0) * tile_size
        count = max(Float.ceil((maximum - first) / tile_size) |> trunc(), 1)
        Enum.map(0..(count - 1), &(first + &1 * tile_size))

      false ->
        [initial]
    end
  end

  defp visible_border?(border_widths, border_styles, border_colors) do
    Enum.any?([:top, :right, :bottom, :left], fn side ->
      Map.fetch!(border_widths, side) > 0 and
        Map.fetch!(border_styles, side) not in [:none, :hidden] and
        not is_nil(Map.fetch!(border_colors, side))
    end)
  end

  defp tag_boxes(boxes, metadata) do
    Enum.map(boxes, &Map.merge(&1, metadata))
  end

  defp tag_atomic_boxes(boxes, metadata) do
    Enum.map(boxes, fn box ->
      box
      |> Map.drop([:fragment_id, :fragment_lines])
      |> Map.merge(metadata)
    end)
  end

  defp break_metadata(style) do
    %{
      break_before: Map.get(style, :break_before, :auto),
      break_after: Map.get(style, :break_after, :auto),
      break_inside: Map.get(style, :break_inside, :auto)
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
            %{embedded: [], fallback: []}
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
      :line_break,
      :line_height,
      :text_align,
      :white_space
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
end
