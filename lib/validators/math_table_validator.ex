defmodule NativeElixirPdfUtilities.Validators.MathTableValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, MathLayout}

  alias NativeElixirPdfUtilities.Validators.{
    FontValidator,
    HtmlTableValidator,
    HtmlValidator,
    MathLayoutValidator
  }

  @doc "Resolves MathML cell default padding using the final selected font metrics."
  @spec resolve_padding(term()) :: {:ok, map()} | {:error, Diagnostics.detailed_error()}
  def resolve_padding(style) do
    case style do
      style when is_map(style) ->
        padding = Map.get(style, :padding, %{})

        deferred? =
          is_map(padding) and
            Enum.any?(padding, fn {_edge, value} ->
              match?({:math_cell_ex, _}, value) or match?({:math_cell_em, _}, value)
            end)

        if deferred? do
          case style do
            %{font_face: font, font_size: size}
            when is_map(font) and is_number(size) and size >= 0 ->
              bounded? =
                size <= Limits.get(:max_css_numeric_magnitude) and
                  Enum.all?(padding, fn {_edge, value} ->
                    case value do
                      {unit, ratio} when unit in [:math_cell_ex, :math_cell_em] ->
                        is_number(ratio) and ratio >= 0 and
                          ratio <= Limits.get(:max_css_numeric_magnitude)

                      number ->
                        is_number(number) and number >= 0 and
                          number <= Limits.get(:max_css_numeric_magnitude)
                    end
                  end)

              if bounded? do
                with {:ok, x_height} <- FontValidator.prepare_x_height(font) do
                  padding =
                    Map.new(padding, fn {edge, value} ->
                      resolved =
                        case value do
                          {:math_cell_ex, ratio} -> ratio * x_height * size
                          {:math_cell_em, ratio} -> ratio * size
                          number -> number
                        end

                      {edge, resolved}
                    end)

                  if Enum.all?(padding, fn {_edge, value} ->
                       value <= Limits.get(:max_css_numeric_magnitude)
                     end),
                     do: {:ok, Map.put(style, :padding, padding)},
                     else:
                       failure("MathML cell default padding exceeds shared numeric bounds", "mtd")
                end
              else
                failure(
                  "MathML cell default padding requires bounded nonnegative lengths and font size",
                  "mtd"
                )
              end

            _ ->
              failure("MathML cell padding requires the final selected font and font size", "mtd")
          end
        else
          {:ok, style}
        end

      _ ->
        failure("MathML cell padding resolution requires a style map", "mtd")
    end
  end

  @doc "Prepares a natural MathML matrix using shared CSS table structure and span validation."
  @spec prepare(term(), map()) :: {:ok, map()} | {:error, Diagnostics.detailed_error()}
  def prepare(node, context) do
    MathLayout.with_context(fn ->
      case {node, context} do
        {%{
           namespace: :mathml,
           tag: "mtable",
           children: children,
           style: %{font_face: font, font_size: size} = style
         }, context}
        when is_list(children) and is_map(font) and is_number(size) and size >= 0 and
               is_map(context) ->
          with :ok <- validate_table_style(style),
               {:ok, projected} <- prepare_nodes(visible_nodes(children)),
               {:ok, table} <- HtmlTableValidator.form(projected),
               {:ok, rows} <- prepare_cells(table.rows, context),
               {:ok, metrics} <- Font.math_metrics(font),
               {:ok, {axis, pixels}} <-
                 MathLayoutValidator.prepare_value(metrics.constants.axis_height, size) do
            FontValidator.reserve(:max_font_math_work, 1)
            axis = axis * size / metrics.units_per_em + pixels
            {:ok, %{kind: :matrix, rows: rows, axis: axis, style: style}}
          end

        _ ->
          failure(
            "matrix preparation requires a styled MathML mtable and preparation context",
            "mtable"
          )
      end
    end)
  end

  # Repeated table whitespace handling keeps authored rows/cells and their source
  # identity intact. Anonymous CSS table repair remains outside this profile.
  defp visible_nodes(nodes) do
    Enum.reject(nodes, fn node ->
      match?(%{style: %{display: :none}}, node) or
        (match?(%{type: :text, text: text} when is_binary(text), node) and
           HtmlValidator.trim_ascii_whitespace(node.text) == "") or
        match?(%{type: :processing_instruction}, node)
    end)
  end

  defp prepare_nodes(nodes) do
    Enum.reduce_while(nodes, {:ok, []}, fn node, {:ok, rows} ->
      result =
        case node do
          %{
            type: :element,
            namespace: :mathml,
            tag: "mtr",
            children: children,
            style: %{display: :table_row} = style
          }
          when is_list(children) ->
            with :ok <- validate_cell_style(style, "mtr"),
                 {:ok, cells} <-
                   Enum.reduce_while(visible_nodes(children), {:ok, []}, fn cell, {:ok, cells} ->
                     case cell do
                       %{
                         type: :element,
                         namespace: :mathml,
                         tag: "mtd",
                         attributes: attributes,
                         children: content,
                         style: %{display: :table_cell} = cell_style
                       }
                       when is_map(attributes) and is_list(content) ->
                         aliases =
                           Map.put(attributes, "colspan", Map.get(attributes, "columnspan"))

                         # An absent alias must stay absent for the shared attribute grammar.
                         aliases =
                           if Map.has_key?(attributes, "columnspan"),
                             do: aliases,
                             else: Map.delete(aliases, "colspan")

                         with :ok <- validate_cell_style(cell_style, "mtd"),
                              {:ok, colspan} <-
                                HtmlValidator.validate_table_span_attribute(aliases, "colspan"),
                              {:ok, rowspan} <-
                                HtmlValidator.validate_table_span_attribute(attributes, "rowspan") do
                           {:cont,
                            {:ok,
                             [
                               %{
                                 cell
                                 | style:
                                     Map.merge(cell_style, %{colspan: colspan, rowspan: rowspan})
                               }
                               | cells
                             ]}}
                         else
                           :error ->
                             {:halt,
                              failure(
                                "MathML cell span attributes require authored strings",
                                "mtd"
                              )}

                           error ->
                             {:halt, error}
                         end

                       _ ->
                         {:halt,
                          failure(
                            "MathML rows require mtd cells; anonymous CSS table repair is pending",
                            "mtr"
                          )}
                     end
                   end) do
              {:ok, %{node | children: Enum.reverse(cells)}}
            end

          _ ->
            failure(
              "MathML matrices require mtr rows; anonymous CSS table repair is pending",
              "mtable"
            )
        end

      case result do
        {:ok, row} -> {:cont, {:ok, [row | rows]}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, rows} -> {:ok, Enum.reverse(rows)}
      error -> error
    end
  end

  defp prepare_cells(rows, context) do
    Enum.reduce_while(rows, {:ok, []}, fn row, {:ok, prepared} ->
      result =
        Enum.reduce_while(row.cells, {:ok, []}, fn placement, {:ok, cells} ->
          if placement.colspan == 1 and placement.rowspan == 1 do
            cell = placement.cell
            # Core mtd owns an anonymous mrow. Its CSS cell padding is placed once
            # by the matrix; authored children retain their complete final styles.
            row_style =
              cell.style
              |> Map.drop([:width, :height, :margin, :padding, :border_widths, :background_color])
              |> Map.put(:display, :block)

            anonymous = %{cell | tag: "mrow", style: row_style}

            case MathLayoutValidator.prepare_element(
                   anonymous,
                   Map.drop(context, [
                     :inline_stretch_size,
                     :block_stretch_size,
                     :operator_context
                   ])
                 ) do
              {:ok, content} ->
                {:cont,
                 {:ok,
                  [
                    %{
                      content: Map.delete(content, :root),
                      style: cell.style,
                      column: placement.column
                    }
                    | cells
                  ]}}

              error ->
                {:halt, error}
            end
          else
            {:halt,
             failure(
               "MathML spanning cell geometry requires the later CSS table sizing batch",
               "mtd"
             )}
          end
        end)

      case result do
        {:ok, cells} -> {:cont, {:ok, [Enum.reverse(cells) | prepared]}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, rows} -> {:ok, Enum.reverse(rows)}
      error -> error
    end
  end

  defp validate_table_style(style) do
    size = Map.get(style, :font_size)
    spacing = Map.get(style, :border_spacing, {0.0, 0.0})

    bounded_spacing? =
      case spacing do
        {horizontal, vertical} ->
          Enum.all?(
            [horizontal, vertical],
            &(is_number(&1) and &1 >= 0 and &1 <= Limits.get(:max_css_numeric_magnitude))
          )

        _ ->
          false
      end

    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}

    bounded_edges? =
      Enum.all?([:padding, :border_widths], fn property ->
        edges = Map.get(style, property, zero)

        is_map(edges) and
          Enum.all?([:top, :right, :bottom, :left], fn edge ->
            value = Map.get(edges, edge)
            is_number(value) and value >= 0 and value <= Limits.get(:max_css_numeric_magnitude)
          end)
      end)

    margin = Map.get(style, :margin, zero)

    zero_margins? =
      is_map(margin) and Enum.all?([:top, :right, :bottom, :left], &(Map.get(margin, &1) == 0))

    if not is_number(size) or size < 0 or size > Limits.get(:max_css_numeric_magnitude) or
         not bounded_spacing? or spacing != {0.0, 0.0} or not bounded_edges? or not zero_margins? or
         Map.get(style, :border_collapse, :separate) != :separate or
         Map.get(style, :width) != nil or Map.get(style, :height) != nil or
         Map.get(style, :background_image) != nil or Map.get(style, :table_layout, :auto) != :auto do
      failure(
        "natural MathML matrices require bounded separate-border box edges, zero spacing/margins and automatic unconstrained sizing",
        "mtable"
      )
    else
      :ok
    end
  end

  # The first matrix slice does not invent row/cell decoration or spanning paint.
  defp validate_cell_style(style, tag) do
    padding = Map.get(style, :padding, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0})

    bounded_padding? =
      is_map(padding) and
        Enum.all?([:top, :right, :bottom, :left], fn edge ->
          value = Map.get(padding, edge)
          is_number(value) and value >= 0 and value <= Limits.get(:max_css_numeric_magnitude)
        end)

    box_edges? =
      Enum.all?([:border_widths, :margin], fn property ->
        edges = Map.get(style, property, %{})

        is_map(edges) and
          Enum.all?(edges, fn {_edge, value} -> is_number(value) and value == 0 end)
      end)

    if not bounded_padding? or not box_edges? or Map.get(style, :width) != nil or
         Map.get(style, :height) != nil or
         Enum.any?(
           [:min_width, :max_width, :min_height, :max_height],
           &(Map.get(style, &1) != nil)
         ) or
         Map.get(style, :background_color) != nil or Map.get(style, :background_image) != nil or
         Map.get(style, :link_url) != nil or Map.get(style, :link_destination) != nil or
         Map.get(style, :html_target) != nil or
         Map.get(style, :position, :static) != :static or
         Map.get(style, :overflow, :visible) != :visible or
         Map.get(style, :box_sizing, :content_box) != :content_box or
         Map.get(style, :vertical_align, :baseline) != :baseline or
         Map.get(style, :text_align, :center) not in [:left, :right, :center] do
      failure(
        "basic MathML rows/cells require bounded padding and baseline alignment; constrained or decorated cell geometry is pending",
        tag
      )
    else
      :ok
    end
  end

  @doc "Validates measured matrix cells and prepares bounded columns, row baselines and placements."
  @spec prepare_geometry(term(), term()) :: {:ok, map()} | {:error, Diagnostics.detailed_error()}
  def prepare_geometry(prepared, measured) do
    MathLayout.with_context(fn ->
      case {prepared, measured} do
        {%{kind: :matrix, rows: rows, axis: axis, style: style}, measured}
        when is_list(rows) and is_list(measured) and is_number(axis) and is_map(style) ->
          # Reserve the existing input-row/cell work before allocating paired
          # rows or cells. Malformed collection tails remain recoverable.
          reserved = reserve_input_collection(rows, :rows)
          same_shape? = reserved == :ok and matching_collection_shape?(rows, measured, :rows)

          cond do
            match?({:error, _}, reserved) ->
              reserved

            same_shape? and abs(axis) <= Limits.get(:max_css_numeric_magnitude) ->
              columns_valid? =
                Enum.all?(rows, fn row ->
                  columns = Enum.map(row, &if(is_map(&1), do: Map.get(&1, :column), else: nil))

                  Enum.all?(Enum.with_index(columns), fn {column, index} ->
                    is_integer(column) and column == index
                  end)
                end)

              cells =
                Enum.flat_map(Enum.with_index(Enum.zip(rows, measured)), fn {{row, cells}, index} ->
                  Enum.map(Enum.zip(row, cells), fn {cell, box} ->
                    %{prepared: cell, measured: box, row: index}
                  end)
                end)

              with :ok <- validate_table_style(style),
                   :ok <-
                     if(columns_valid?,
                       do: :ok,
                       else:
                         failure(
                           "prepared matrix rows must use contiguous zero-based columns; gaps and spanning geometry are unsupported",
                           "mtd"
                         )
                     ),
                   :ok <-
                     Enum.reduce_while(cells, :ok, fn cell, :ok ->
                       case {cell.prepared,
                             MathLayoutValidator.validate_measured_geometry(cell.measured)} do
                         {%{column: column, style: style}, :ok}
                         when is_integer(column) and column >= 0 and is_map(style) ->
                           with :ok <-
                                  HtmlValidator.validate_layout_cardinality(
                                    :table_span,
                                    column + 1
                                  ),
                                :ok <- validate_cell_style(style, "mtd"),
                                do: {:cont, :ok},
                                else: (error -> {:halt, error})

                         {_, {:error, {reason, diagnostic}}} ->
                           {:halt,
                            {:error,
                             {reason, Diagnostics.with_context(diagnostic, source: "mtd")}}}

                         _ ->
                           {:halt,
                            failure(
                              "measured matrix cells require prepared columns and validated geometry",
                              "mtd"
                            )}
                       end
                     end) do
                geometry(rows, cells, axis, style)
              end

            true ->
              failure(
                "matrix measurement requires matching proper prepared/measured row lists and a bounded MATH axis",
                "mtable"
              )
          end

        _ ->
          failure("matrix measurement requires prepared state and measured rows", "mtable")
      end
    end)
  end

  # Count proper list links without materializing row/cell pairs, stopping at
  # the existing shared quota before oversized direct input can allocate work.
  defp reserve_input_collection(values, kind) do
    case values do
      [] ->
        :ok

      [value | rest] ->
        HtmlValidator.reserve_render_resource(:max_table_grid_work, 1, :layout)

        with :ok <- if(kind == :rows, do: reserve_input_collection(value, :cells), else: :ok) do
          reserve_input_collection(rest, kind)
        end

      _ ->
        failure(
          "prepared matrix rows and cells require proper lists with no malformed tails",
          if(kind == :rows, do: "mtable", else: "mtd")
        )
    end
  end

  # This second walk also rejects improper measured tails without traversing
  # unmatched excess input or allocating pairs before the input work quota.
  defp matching_collection_shape?(prepared, measured, kind) do
    case {prepared, measured} do
      {[], []} ->
        true

      {[left | prepared_rest], [right | measured_rest]} ->
        (kind != :rows or matching_collection_shape?(left, right, :cells)) and
          matching_collection_shape?(prepared_rest, measured_rest, kind)

      _ ->
        false
    end
  end

  # CSS separate-border natural columns use each cell's preferred widths; row
  # baselines include cell padding. Core centers the resulting table on MathAxis.
  defp geometry(rows, cells, axis, style) do
    column_count = Enum.max(Enum.map(cells, &(&1.prepared.column + 1)), fn -> 0 end)

    HtmlValidator.reserve_render_resource(
      :max_table_grid_work,
      length(cells) * 8 + length(rows) + column_count,
      :layout
    )

    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    padding = Map.get(style, :padding, zero)
    border = Map.get(style, :border_widths, zero)
    # The shared decorator adds owner box edges after this content geometry.
    # Compensate asymmetric edges so the entire border box is axis centered.
    axis_offset = (padding.bottom + border.bottom - padding.top - border.top) / 2
    by_column = Enum.group_by(cells, & &1.prepared.column)
    by_row = Enum.group_by(cells, & &1.row)
    zero_padding = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}

    columns =
      for column <- 0..max(column_count - 1, 0), column < column_count do
        # Every admitted row is a contiguous prefix. The longest row owns all
        # natural columns, so these maxima always have at least one cell.
        values = Map.fetch!(by_column, column)

        preferred =
          for field <- [:min_content_width, :max_content_width], into: %{} do
            {field,
             Enum.max(
               Enum.map(values, fn cell ->
                 padding = Map.get(cell.prepared.style, :padding, zero_padding)
                 Map.fetch!(cell.measured, field) + padding.left + padding.right
               end)
             )}
          end

        Map.put(preferred, :width, preferred.max_content_width)
      end

    row_sizes =
      for {_row, index} <- Enum.with_index(rows) do
        values = Map.get(by_row, index, [])

        ascent =
          Enum.max(
            Enum.map(values, fn cell ->
              cell.measured.ascent + Map.get(cell.prepared.style, :padding, zero_padding).top
            end),
            fn -> 0.0 end
          )

        descent =
          Enum.max(
            Enum.map(values, fn cell ->
              cell.measured.descent + Map.get(cell.prepared.style, :padding, zero_padding).bottom
            end),
            fn -> 0.0 end
          )

        %{ascent: ascent, descent: descent}
      end

    {column_offsets, _} =
      Enum.map_reduce(columns, 0.0, fn column, offset -> {offset, offset + column.width} end)

    {row_offsets, height} =
      Enum.map_reduce(row_sizes, 0.0, fn row, offset ->
        {offset, offset + row.ascent + row.descent}
      end)

    column_offsets = List.to_tuple(column_offsets)
    row_offsets = List.to_tuple(row_offsets)
    column_sizes = List.to_tuple(columns)
    row_metrics = List.to_tuple(row_sizes)
    ascent = height / 2 + axis + axis_offset
    descent = height / 2 - axis - axis_offset

    positioned =
      Enum.map(cells, fn cell ->
        column = cell.prepared.column
        style = cell.prepared.style
        padding = Map.get(style, :padding, zero_padding)
        width = elem(column_sizes, column).width
        available = width - padding.left - padding.right - cell.measured.width

        shift =
          case Map.get(style, :text_align, :center) do
            :left -> 0.0
            :right -> available
            :center -> available / 2
          end

        x = elem(column_offsets, column) + padding.left + shift
        y = ascent - elem(row_offsets, cell.row) - elem(row_metrics, cell.row).ascent
        %{measured: cell.measured, x: x, y: y}
      end)

    result = %{
      width: Enum.sum(Enum.map(columns, & &1.width)),
      min_content_width: Enum.sum(Enum.map(columns, & &1.min_content_width)),
      max_content_width: Enum.sum(Enum.map(columns, & &1.max_content_width)),
      ascent: ascent,
      descent: descent,
      ink_ascent: Enum.max(Enum.map(positioned, &(&1.measured.ink_ascent + &1.y)), fn -> 0.0 end),
      ink_descent:
        Enum.max(Enum.map(positioned, &(&1.measured.ink_descent - &1.y)), fn -> 0.0 end),
      cells: positioned,
      columns: columns,
      row_sizes: row_sizes
    }

    numeric =
      Map.values(Map.drop(result, [:cells, :columns, :row_sizes])) ++
        Enum.flat_map(positioned, fn cell ->
          [cell.x, cell.y] ++
            Enum.flat_map(cell.measured.drawing_items, &[&1.x + cell.x, &1.y + cell.y])
        end)

    owner_bounds = [
      result.width + padding.left + padding.right + border.left + border.right,
      result.min_content_width + padding.left + padding.right + border.left + border.right,
      result.max_content_width + padding.left + padding.right + border.left + border.right,
      result.ascent + padding.top + border.top,
      result.descent + padding.bottom + border.bottom,
      height + padding.top + padding.bottom + border.top + border.bottom,
      result.ink_ascent + padding.top + border.top,
      result.ink_descent + padding.bottom + border.bottom
    ]

    owner_bounds =
      owner_bounds ++
        Enum.flat_map(positioned, fn cell ->
          Enum.map(cell.measured.drawing_items, &(&1.x + cell.x + padding.left + border.left))
        end)

    if Enum.all?(numeric ++ owner_bounds, &(abs(&1) <= Limits.get(:max_css_numeric_magnitude))),
      do: {:ok, result},
      else:
        failure(
          "matrix intrinsic sizes, decorated box extents or positioned paint exceed shared numeric bounds",
          "mtable"
        )
  end

  defp failure(message, source) do
    Diagnostics.error(:layout, :unsupported_html, message,
      operation: :measure_math,
      module: __MODULE__,
      source: source
    )
  end
end
