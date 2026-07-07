defmodule NativeElixirPdfUtilities.HtmlToPdf.Layout do
  @moduledoc """
  Layout engine for the native HTML-to-PDF renderer.

  Milestone 6 lays out block text elements, inline text runs, basic block
  box styling, lists, link annotation bounds, and deterministic one-page
  tables. Later milestones add richer wrapping, pagination, flexbox, and grid
  layout behavior behind this module.
  """

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

          background_box =
            background_box(style, box_x, box_top - box_height, box_width, box_height)

          {boxes, _next_x} =
            Enum.reduce(runs, {background_box, content_x}, fn run, {acc, current_x} ->
              box = text_box(run, current_x, baseline_y, content_width)
              {acc ++ [box], current_x + text_width(run.text, run.style)}
            end)

          next_y = box_top - box_height - margin.bottom
          {:ok, boxes, next_y}
        end

      %{type: :element, style: %{display: :list} = style, children: children}
      when is_list(children) ->
        layout_list(style, children, x, y, width)

      %{type: :element, style: %{display: :table} = style, children: children}
      when is_list(children) ->
        layout_table(style, children, x, y, width)

      _ ->
        {:error, :invalid_layout}
    end
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

    with {:ok, caption_boxes, rows_top} <-
           layout_table_caption(children, content_x, content_top, content_width),
         {:ok, rows} <- table_rows(children),
         {:ok, row_boxes, content_bottom} <-
           layout_table_rows(rows, content_x, rows_top, content_width) do
      content_height = content_top - content_bottom

      box_height =
        border_widths.top + padding.top + content_height + padding.bottom + border_widths.bottom

      table_box = background_box(style, box_x, box_top - box_height, box_width, box_height)
      next_y = box_top - box_height - margin.bottom
      {:ok, table_box ++ caption_boxes ++ row_boxes, next_y}
    end
  end

  defp layout_table_caption(children, x, y, width) do
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

          background_box =
            background_box(style, box_x, box_top - box_height, box_width, box_height)

          {text_boxes, _next_x} =
            Enum.reduce(runs, {[], start_x}, fn run, {acc, current_x} ->
              box = text_box(run, current_x, baseline_y, content_width)
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
                {:ok, rows ++ [child]}

              %{style: %{display: :table_row_group}, children: group_rows}
              when is_list(group_rows) ->
                case Enum.all?(group_rows, &match?(%{style: %{display: :table_row}}, &1)) do
                  true -> {:ok, rows ++ group_rows}
                  false -> {:error, :invalid_layout}
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

  defp layout_table_rows(rows, x, y, width) do
    column_count =
      rows
      |> Enum.map(fn %{children: cells} -> length(cells) end)
      |> Enum.max(fn -> 0 end)

    case column_count do
      count when count > 0 ->
        column_width = width / count

        result =
          Enum.reduce(rows, {:ok, [], y}, fn row, acc ->
            case acc do
              {:ok, boxes, current_y} ->
                case layout_table_row(row, x, current_y, column_width) do
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

  defp layout_table_row(row, x, y, column_width) do
    case row do
      %{style: %{display: :table_row}, children: cells} when is_list(cells) ->
        case Enum.all?(cells, &match?(%{style: %{display: :table_cell}}, &1)) do
          true ->
            row_height = cells |> Enum.map(&table_cell_height/1) |> Enum.max()

            result =
              Enum.reduce_while(cells, {:ok, [], 0}, fn cell, {:ok, acc, index} ->
                cell_x = x + column_width * index

                case layout_table_cell(cell, cell_x, y, column_width, row_height) do
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

  defp layout_table_cell(cell, x, y, width, height) do
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
          cell_box = background_box(style, x, y - height, width, height)

          {text_boxes, _next_x} =
            Enum.reduce(runs, {[], start_x}, fn run, {acc, current_x} ->
              box = text_box(run, current_x, baseline_y, content_width)
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
          marker_box = text_box(%{text: marker, style: marker_style}, x, baseline_y, marker_gap)

          {text_boxes, _next_x} =
            Enum.reduce(runs, {[], text_x}, fn run, {acc, current_x} ->
              box = text_box(run, current_x, baseline_y, text_width)
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

    box = %{
      type: :text,
      text: run.text,
      x: x,
      y: y,
      width: width,
      annotation_width: text_width(run.text, style),
      font:
        pdf_font(
          Map.fetch!(style, :font_family),
          Map.fetch!(style, :font_weight),
          Map.fetch!(style, :font_style)
        ),
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

  @spec text_width(String.t(), map()) :: number()
  defp text_width(text, style) do
    text
    |> String.length()
    |> Kernel.*(Map.fetch!(style, :font_size))
    |> Kernel.*(0.6)
  end

  defp text_style(style) do
    Map.take(style, [
      :color,
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

  @spec pdf_font(String.t(), number(), atom()) :: String.t()
  defp pdf_font(font_family, font_weight, font_style) do
    case {font_family, font_weight >= 700, font_style} do
      {"Helvetica", true, :italic} -> "Helvetica-BoldOblique"
      {"Helvetica", true, _} -> "Helvetica-Bold"
      {"Helvetica", false, :italic} -> "Helvetica-Oblique"
      {"Helvetica", false, _} -> "Helvetica"
      {font_family, _, _} -> font_family
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
