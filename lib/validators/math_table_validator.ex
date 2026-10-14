defmodule NativeElixirPdfUtilities.Validators.MathTableValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, Layout, MathLayout}

  alias NativeElixirPdfUtilities.Validators.{
    FontValidator,
    HtmlTableValidator,
    HtmlValidator,
    MathLayoutValidator
  }

  # Fixed source transport fields preserve facts from real authored/generated
  # nodes. They never issue source receipts or resource permissions.
  @source_fields [
    :type,
    :namespace,
    :tag,
    :attributes,
    :attribute_namespaces,
    :_selector_id,
    :_document_order,
    :node_id,
    :parent_id,
    :document_id,
    :tree_scope_id,
    :source,
    :cloned_from_node_id,
    :clone_id,
    :composed_parent_id,
    :shadow_scope_id,
    :shadow_host_id,
    :shadow_root_id,
    :assigned_slot_id,
    :assigned_shadow_scope_id,
    :assigned_shadow_host_id
  ]

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

  @doc "Validates prepared matrix, row and cell styles with the shared CSS size-expression authority."
  @spec validate_style(term(), term()) :: :ok | {:error, Diagnostics.detailed_error()}
  def validate_style(style, tag) do
    case {style, tag} do
      {style, tag} when is_map(style) and tag in ["mtable", "mtr", "mtd"] ->
        with :ok <-
               Enum.reduce_while(
                 [:width, :height, :min_width, :max_width, :min_height, :max_height],
                 :ok,
                 fn property, :ok ->
                   case HtmlValidator.validate_prepared_css_size(
                          Map.get(style, property),
                          property
                        ) do
                     :ok -> {:cont, :ok}
                     error -> {:halt, error}
                   end
                 end
               ) do
          with :ok <- MathLayoutValidator.validate_background_style(style, tag) do
            if tag == "mtable",
              do: validate_table_style(style),
              else: validate_cell_style(style, tag)
          end
        else
          {:error, {reason, diagnostic}} ->
            {:error, {reason, Diagnostics.with_context(diagnostic, source: tag)}}
        end

      _ ->
        failure("matrix style requires a prepared style map and a MathML table role", "mtable")
    end
  end

  @doc "Delegates actual Math block width dependencies to the canonical Math layout authority."
  @spec prepare_block_flow_dependency(term()) ::
          {:ok, boolean()} | {:error, Diagnostics.detailed_error()}
  def prepare_block_flow_dependency(node),
    do: MathLayoutValidator.prepare_block_flow_dependency(node)

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
          with :ok <-
                 MathLayoutValidator.validate_available_inline_size(
                   Map.get(context, :available_inline_size)
                 ),
               :ok <- validate_style(style, "mtable"),
               style =
                 (if Map.get(style, :_math_element) == {:mathml, "mtable"} do
                    # Match the existing ordinary CSS table used-value rule.
                    # Blink stores CSS border-spacing as integral CSS pixels;
                    # source declarations and symbolic point matrices stay raw.
                    {horizontal, vertical} = Map.get(style, :border_spacing, {0.0, 0.0})

                    Map.put(style, :border_spacing, {
                      Float.floor(horizontal / 0.75) * 0.75,
                      Float.floor(vertical / 0.75) * 0.75
                    })
                  else
                    style
                  end),
               {:ok, projected} <- prepare_nodes(visible_nodes(children)),
               {:ok, table} <- HtmlTableValidator.form(projected),
               {:ok, {rows, row_boxes}} <- prepare_cells(table.rows, context),
               :ok <- validate_grid_spacing(rows, style),
               {:ok, metrics} <- Font.math_metrics(font),
               {:ok, {axis, pixels}} <-
                 MathLayoutValidator.prepare_value(metrics.constants.axis_height, size) do
            FontValidator.reserve(:max_font_math_work, 1)
            axis = axis * size / metrics.units_per_em + pixels

            {:ok,
             %{
               kind: :matrix,
               rows: rows,
               row_boxes: row_boxes,
               axis: axis,
               style: style,
               available_inline_size: Map.get(context, :available_inline_size)
             }}
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
            with :ok <- validate_style(style, "mtr"),
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

                         with :ok <- validate_style(cell_style, "mtd"),
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
    Enum.reduce_while(rows, {:ok, {[], []}}, fn row, {:ok, {prepared, row_boxes}} ->
      result =
        Enum.reduce_while(row.cells, {:ok, []}, fn placement, {:ok, cells} ->
          cell = placement.cell
          # Core mtd owns an anonymous mrow. Its CSS cell padding is placed once
          # by the matrix; authored children retain their complete final styles.
          row_style = MathLayoutValidator.anonymous_box_style(cell.style)

          anonymous = %{cell | tag: "mrow", style: row_style}

          content_context =
            Map.drop(context, [
              :inline_stretch_size,
              :block_stretch_size,
              :operator_context,
              :available_inline_size,
              :available_content_width
            ])

          with {:ok, content} <- MathLayoutValidator.prepare_element(anonymous, content_context) do
            prepared_cell = %{
              content: Map.delete(content, :root),
              style: cell.style,
              column: placement.column,
              colspan: placement.colspan,
              rowspan: placement.rowspan,
              source: Map.take(cell, @source_fields)
            }

            prepared_cell =
              if native_flow_content?(content) do
                prepared_cell
                |> Map.put(:content_layout_source, anonymous)
                |> Map.put(:content_layout_context, content_context)
              else
                prepared_cell
              end

            {:cont, {:ok, [prepared_cell | cells]}}
          else
            error -> {:halt, error}
          end
        end)

      case result do
        {:ok, cells} ->
          row_box = %{
            style: row.row.style,
            source: Map.take(row.row, @source_fields)
          }

          {:cont, {:ok, {[Enum.reverse(cells) | prepared], [row_box | row_boxes]}}}

        error ->
          {:halt, error}
      end
    end)
    |> case do
      {:ok, {rows, row_boxes}} -> {:ok, {Enum.reverse(rows), Enum.reverse(row_boxes)}}
      error -> error
    end
  end

  # Preparation already guarantees the supported proper subtree. The extra
  # walk identifies real ordinary HTML layout rather than changing its rules.
  defp native_flow_content?(content) do
    HtmlValidator.reserve_render_resource(:max_css_work, 1, :layout)

    case content do
      %{kind: :native_flow} ->
        true

      %{kind: :matrix, rows: rows} ->
        Enum.any?(rows, fn row -> Enum.any?(row, &native_flow_content?(&1.content)) end)

      %{kind: :operator, content: child} ->
        native_flow_content?(child)

      %{kind: :radical, base: base, index: index} ->
        native_flow_content?(base) or (index != nil and native_flow_content?(index))

      %{children: children} ->
        Enum.any?(children, &native_flow_content?/1)

      _ ->
        false
    end
  end

  @doc "Prepares retained actual cell content for measurement at its allocated content width."
  @spec prepare_cell_reflow(term(), term()) ::
          {:ok, map()} | {:error, Diagnostics.detailed_error()}
  def prepare_cell_reflow(cell, width) do
    case cell do
      %{
        content_layout_source: %{type: :element, namespace: :mathml, tag: "mrow"} = source,
        content_layout_context: context
      }
      when is_map(context) ->
        with :ok <- MathLayoutValidator.validate_available_inline_size(width) do
          if is_number(width) do
            with {:ok, prepared} <-
                   MathLayoutValidator.prepare_element(
                     source,
                     Map.put(context, :available_content_width, width)
                   ) do
              {:ok, %{prepared: prepared}}
            end
          else
            failure("matrix cell reflow requires its definite allocated content width", "mtd")
          end
        end

      _ ->
        failure(
          "matrix cell reflow requires retained actual anonymous Math content and context",
          "mtd"
        )
    end
  end

  # The measured boundary checks proper prepared subtree shapes for spaced cells.
  defp validate_grid_spacing(rows, style) do
    if Map.get(style, :border_spacing, {0.0, 0.0}) == {0.0, 0.0} do
      :ok
    else
      Enum.reduce_while(rows, :ok, fn row, :ok ->
        case Enum.reduce_while(row, :ok, fn cell, :ok ->
               case cell do
                 %{content: content} ->
                   case validate_spaced_content(content) do
                     :ok -> {:cont, :ok}
                     error -> {:halt, error}
                   end

                 _ ->
                   {:halt, failure("matrix cells require prepared content", "mtd")}
               end
             end) do
          :ok -> {:cont, :ok}
          error -> {:halt, error}
        end
      end)
    end
  end

  # Each actual prepared subtree contributes to the same depth and work bounds,
  # including nested matrices and content hidden by phantom.
  defp validate_spaced_content(content, depth \\ 0) do
    if depth > Limits.get(:max_html_depth) do
      Diagnostics.error(
        :layout,
        :resource_limit_exceeded,
        "spaced matrix prepared content exceeds max_html_depth",
        operation: :measure_math,
        module: __MODULE__,
        source: "mtd"
      )
    else
      HtmlValidator.reserve_render_resource(:max_table_grid_work, 1, :layout)

      case content do
        %{kind: kind} when kind in [:glyph, :operator, :space] ->
          :ok

        %{kind: kind, children: children}
        when kind in [:row, :token, :flow, :fraction, :scripts, :limits] ->
          validate_spaced_children(children, depth + 1)

        %{kind: :native_flow, children: [_ | _] = children} ->
          with :ok <- reserve_input_collection(children, :cells) do
            Enum.reduce_while(children, :ok, fn child, :ok ->
              HtmlValidator.reserve_render_resource(:max_table_grid_work, 1, :layout)

              case child do
                %{boxes: boxes} ->
                  case HtmlValidator.validate_measured_child_paint(boxes, %{tag: "mtd"}) do
                    :ok -> {:cont, :ok}
                    error -> {:halt, error}
                  end

                _ ->
                  {:halt, failure("matrix native flow requires measured child paint", "mtd")}
              end
            end)
          end

        %{kind: :radical, base: base, index: index} ->
          validate_spaced_children(if(index == nil, do: [base], else: [base, index]), depth + 1)

        %{kind: :matrix, rows: rows} when is_list(rows) ->
          with :ok <- reserve_input_collection(rows, :rows) do
            Enum.reduce_while(rows, :ok, fn row, :ok ->
              contents =
                Enum.map(row, fn
                  %{content: content} -> content
                  _ -> nil
                end)

              case validate_spaced_children(contents, depth + 1) do
                :ok -> {:cont, :ok}
                error -> {:halt, error}
              end
            end)
          end

        _ ->
          failure("matrix content requires a supported prepared MathML subtree", "mtd")
      end
    end
  end

  defp validate_spaced_children(children, depth) do
    case children do
      [] ->
        :ok

      [child | rest] ->
        with :ok <- validate_spaced_content(child, depth),
             do: validate_spaced_children(rest, depth)

      _ ->
        failure("spaced matrix content requires proper prepared child lists", "mtd")
    end
  end

  defp validate_table_style(style) do
    size = Map.get(style, :font_size)
    spacing = Map.get(style, :border_spacing, {0.0, 0.0})

    bounded_spacing? =
      case spacing do
        {horizontal, vertical} ->
          Enum.all?([horizontal, vertical], fn length ->
            is_number(length) and length >= 0 and length <= Limits.get(:max_css_numeric_magnitude)
          end)

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
         not bounded_spacing? or not bounded_edges? or not zero_margins? or
         Map.get(style, :border_collapse, :separate) != :separate or
         Map.get(style, :table_layout, :auto) != :auto do
      failure(
        "MathML matrices require bounded separate-border box edges, continuous spacing, zero margins and automatic table layout",
        "mtable"
      )
    else
      MathLayoutValidator.validate_link_style(style, "mtable")
    end
  end

  # Separate-border rows paint backgrounds over occupied cell rectangles. Their
  # CSS padding and borders do not participate in row or cell geometry.
  defp validate_cell_style(style, tag) do
    padding = Map.get(style, :padding, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0})

    bounded_padding? =
      is_map(padding) and
        Enum.all?([:top, :right, :bottom, :left], fn edge ->
          value = Map.get(padding, edge)
          is_number(value) and value >= 0 and value <= Limits.get(:max_css_numeric_magnitude)
        end)

    border = Map.get(style, :border_widths, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0})

    bounded_border? =
      is_map(border) and
        Enum.all?([:top, :right, :bottom, :left], fn edge ->
          value = Map.get(border, edge)

          is_number(value) and value >= 0 and value <= Limits.get(:max_css_numeric_magnitude)
        end)

    margin = Map.get(style, :margin, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0})

    zero_margins? =
      is_map(margin) and Enum.all?([:top, :right, :bottom, :left], &(Map.get(margin, &1) == 0))

    if not bounded_padding? or not bounded_border? or not zero_margins? or
         Map.get(style, :html_target) != nil or
         Map.get(style, :position, :static) != :static or
         Map.get(style, :overflow, :visible) != :visible or
         Map.get(style, :box_sizing, :content_box) not in [:content_box, :border_box] or
         Map.get(style, :vertical_align, :baseline) not in [:baseline, :top, :middle, :bottom] or
         Map.get(style, :text_align, :center) not in [:left, :right, :center] do
      failure(
        "MathML cells require bounded padding/borders and table alignment; positioned/clipped cells remain unsupported",
        tag
      )
    else
      MathLayoutValidator.validate_link_style(style, tag)
    end
  end

  # Every preferred width, baseline and allocated content rectangle includes
  # cell padding and separate borders exactly once.
  defp cell_edges(style) do
    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    padding = Map.get(style, :padding, zero)
    border = Map.get(style, :border_widths, zero)

    Map.new(
      [:top, :right, :bottom, :left],
      &{&1, Map.fetch!(padding, &1) + Map.fetch!(border, &1)}
    )
  end

  @doc "Validates measured matrix cells and prepares bounded columns, row baselines and placements."
  @spec prepare_geometry(term(), term()) :: {:ok, map()} | {:error, Diagnostics.detailed_error()}
  def prepare_geometry(prepared, measured) do
    MathLayout.with_context(fn ->
      case {prepared, measured} do
        {%{kind: :matrix, rows: rows, row_boxes: row_boxes, axis: axis, style: style} = prepared,
         measured}
        when is_list(rows) and is_list(row_boxes) and is_list(measured) and is_number(axis) and
               is_map(style) ->
          reserved = reserve_input_collection(rows, :rows)

          same_shape? =
            reserved == :ok and matching_collection_shape?(rows, measured, :rows) and
              matching_collection_shape?(row_boxes, rows, :cells)

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

              with :ok <- validate_style(style, "mtable"),
                   :ok <- validate_grid_spacing(rows, style),
                   :ok <-
                     Enum.reduce_while(row_boxes, :ok, fn row_box, :ok ->
                       case row_box do
                         %{style: row_style, source: %{namespace: :mathml, tag: "mtr"}}
                         when is_map(row_style) ->
                           case validate_style(row_style, "mtr") do
                             :ok -> {:cont, :ok}
                             error -> {:halt, error}
                           end

                         _ ->
                           {:halt,
                            failure(
                              "prepared matrix rows require validated styles and original MathML row sources",
                              "mtr"
                            )}
                       end
                     end),
                   :ok <- validate_columns(rows, row_boxes, columns_valid?),
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
                                :ok <- validate_style(style, "mtd"),
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
                with :ok <-
                       MathLayoutValidator.validate_available_inline_size(
                         Map.get(prepared, :available_inline_size)
                       ) do
                  geometry(
                    rows,
                    row_boxes,
                    cells,
                    axis,
                    style,
                    Map.get(prepared, :available_inline_size)
                  )
                end
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

  defp validate_columns(rows, row_boxes, contiguous?) do
    spanning? =
      Enum.any?(rows, fn row ->
        Enum.any?(row, fn cell ->
          is_map(cell) and (Map.get(cell, :colspan, 1) != 1 or Map.get(cell, :rowspan, 1) != 1)
        end)
      end)

    cond do
      not spanning? and contiguous? ->
        :ok

      not spanning? ->
        failure(
          "prepared matrix rows require contiguous zero-based columns or actual span-covered slots",
          "mtd"
        )

      not Enum.all?(rows, fn row ->
        Enum.all?(row, fn cell ->
          is_map(cell) and is_map(Map.get(cell, :style)) and is_integer(Map.get(cell, :column)) and
              cell.column >= 0
        end)
      end) ->
        failure(
          "matrix span placements require cell style maps and contiguous zero-based columns",
          "mtd"
        )

      true ->
        projected =
          Enum.zip_with(rows, row_boxes, fn cells, row ->
            %{
              style: row.style,
              children:
                Enum.map(cells, fn cell ->
                  %{
                    style:
                      Map.merge(cell.style, %{
                        colspan: Map.get(cell, :colspan, 1),
                        rowspan: Map.get(cell, :rowspan, 1)
                      }),
                    children: []
                  }
                end)
            }
          end)

        with {:ok, formed} <- HtmlTableValidator.form(projected) do
          if Enum.all?(Enum.zip(rows, formed.rows), fn {cells, row} ->
               Enum.all?(Enum.zip(cells, row.cells), fn {cell, placement} ->
                 cell.column == placement.column and
                   Map.get(cell, :colspan, 1) == placement.colspan and
                   Map.get(cell, :rowspan, 1) == placement.rowspan
               end)
             end),
             do: :ok,
             else:
               failure(
                 "matrix spans require the actual shared nonoverlapping table placements",
                 "mtd"
               )
        end
    end
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
  defp geometry(rows, row_boxes, cells, axis, style, available_inline_size) do
    column_count =
      Enum.max(Enum.map(cells, &(&1.prepared.column + Map.get(&1.prepared, :colspan, 1))), fn ->
        0
      end)

    HtmlValidator.reserve_render_resource(
      :max_table_grid_work,
      length(cells) * 8 + length(rows) + column_count,
      :layout
    )

    spanning = Enum.filter(cells, &(Map.get(&1.prepared, :colspan, 1) > 1))

    if spanning != [] or Enum.any?(cells, &(Map.get(&1.prepared, :rowspan, 1) > 1)) do
      # Reserve the actual full-column redistribution walks and linked-list
      # slices before their arrays are allocated. The existing single-slot
      # profile retains its original budget contract.
      sorting = length(spanning) * ceil(:math.log2(max(length(spanning), 1)))
      redistribution = length(spanning) * column_count * 2

      slices =
        Enum.sum(
          Enum.map(cells, fn cell ->
            cell.prepared.column + Map.get(cell.prepared, :colspan, 1) + cell.row +
              Map.get(cell.prepared, :rowspan, 1)
          end)
        )

      HtmlValidator.reserve_render_resource(
        :max_table_grid_work,
        sorting + redistribution + slices,
        :layout
      )
    end

    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    padding = Map.get(style, :padding, zero)
    border = Map.get(style, :border_widths, zero)
    # The shared decorator adds owner box edges after this content geometry.
    # Compensate asymmetric edges so the entire border box is axis centered.
    axis_offset = (padding.bottom + border.bottom - padding.top - border.top) / 2

    by_column =
      cells
      |> Enum.filter(&(Map.get(&1.prepared, :colspan, 1) == 1))
      |> Enum.group_by(& &1.prepared.column)

    by_row = Enum.group_by(cells, & &1.row)

    {horizontal_spacing, vertical_spacing} =
      if column_count == 0, do: {0.0, 0.0}, else: Map.get(style, :border_spacing, {0.0, 0.0})

    columns =
      for column <- 0..max(column_count - 1, 0), column < column_count do
        # Spanning-only columns acquire their real intrinsic contribution below.
        values = Map.get(by_column, column, [])

        preferred =
          for field <- [:min_content_width, :max_content_width], into: %{} do
            {field,
             Enum.max(
               Enum.map(values, fn cell ->
                 edges = cell_edges(cell.prepared.style)
                 preferred = Map.fetch!(cell.measured, field)

                 max(
                   preferred,
                   Layout.resolve_content_size(
                     cell.prepared.style,
                     :width,
                     Layout.resolve_content_size(style, :width, available_inline_size, nil),
                     preferred
                   )
                 ) + edges.left + edges.right
               end),
               fn -> 0.0 end
             )}
          end

        Map.put(preferred, :width, preferred.max_content_width)
      end

    columns =
      spanning
      |> Enum.sort_by(&Map.get(&1.prepared, :colspan))
      |> Enum.reduce(columns, fn cell, columns ->
        span = cell.prepared.colspan
        HtmlValidator.reserve_render_resource(:max_table_grid_work, span, :layout)
        edges = cell_edges(cell.prepared.style)

        Enum.reduce([:min_content_width, :max_content_width], columns, fn field, columns ->
          preferred = Map.fetch!(cell.measured, field)

          required =
            max(
              preferred,
              Layout.resolve_content_size(
                cell.prepared.style,
                :width,
                Layout.resolve_content_size(style, :width, available_inline_size, nil),
                preferred
              )
            ) + edges.left + edges.right - horizontal_spacing * (span - 1)

          values = Enum.slice(columns, cell.prepared.column, span)
          total = Enum.sum(Enum.map(values, &Map.fetch!(&1, field)))
          extra = max(required - total, 0.0)

          Enum.with_index(columns, fn column, index ->
            if index >= cell.prepared.column and index < cell.prepared.column + span do
              weight = if total > 0, do: Map.fetch!(column, field) / total, else: 1 / span
              Map.update!(column, field, &(&1 + extra * weight))
            else
              column
            end
          end)
        end)
      end)
      |> Enum.map(&Map.put(&1, :width, &1.max_content_width))

    intrinsic = Enum.sum(Enum.map(columns, & &1.width))
    minimum = Enum.sum(Enum.map(columns, & &1.min_content_width))
    spacing_width = horizontal_spacing * if(column_count == 0, do: 0, else: column_count + 1)

    target =
      max(
        Layout.resolve_content_size(
          style,
          :width,
          available_inline_size,
          intrinsic + spacing_width
        ) - spacing_width,
        minimum
      )

    table_basis = Layout.resolve_content_size(style, :width, available_inline_size, nil)

    constrained_columns =
      MapSet.new(cells, fn cell ->
        if Map.get(cell.prepared, :colspan, 1) == 1 and
             Map.get(cell.prepared.style, :width) not in [nil, :auto] and
             Layout.resolve_content_size(cell.prepared.style, :width, table_basis, nil) != nil,
           do: cell.prepared.column,
           else: nil
      end)
      |> MapSet.delete(nil)

    automatic_columns =
      Enum.with_index(columns)
      |> Enum.reject(fn {_column, index} -> MapSet.member?(constrained_columns, index) end)

    expanding_columns =
      if automatic_columns == [], do: Enum.with_index(columns), else: automatic_columns

    expanding_indexes = MapSet.new(expanding_columns, &elem(&1, 1))
    expanding_width = Enum.sum(Enum.map(expanding_columns, fn {column, _} -> column.width end))

    columns =
      Enum.with_index(columns, fn column, index ->
        width =
          cond do
            target >= intrinsic and MapSet.member?(expanding_indexes, index) ->
              column.width +
                (target - intrinsic) *
                  if(expanding_width > 0,
                    do: column.width / expanding_width,
                    else: 1 / max(length(expanding_columns), 1)
                  )

            target >= intrinsic ->
              column.width

            true ->
              # target >= minimum, so a target below intrinsic implies that
              # intrinsic > minimum. There is no zero-denominator branch.
              column.min_content_width +
                (target - minimum) * (column.width - column.min_content_width) /
                  (intrinsic - minimum)
          end

        Map.put(column, :width, width)
      end)

    row_sizes =
      for {_row, index} <- Enum.with_index(rows) do
        values = Map.get(by_row, index, [])

        ascent =
          Enum.max(
            Enum.map(values, &(&1.measured.ascent + cell_edges(&1.prepared.style).top)),
            fn -> 0.0 end
          )

        descent =
          Enum.max(
            Enum.map(values, &(&1.measured.descent + cell_edges(&1.prepared.style).bottom)),
            fn -> 0.0 end
          )

        %{ascent: ascent, descent: descent}
      end

    needs_baseline_allocation =
      Enum.any?(
        cells,
        &(Map.get(&1.prepared, :rowspan, 1) > 1 or
            Map.get(&1.prepared.style, :vertical_align, :baseline) != :baseline or
            Enum.any?([:height, :min_height, :max_height], fn property ->
              Map.get(&1.prepared.style, property) != nil
            end))
      ) or
        Enum.any?(
          row_boxes,
          &Enum.any?([:height, :min_height, :max_height], fn property ->
            Map.get(&1.style, property) != nil
          end)
        )

    row_sizes =
      if needs_baseline_allocation do
        projected =
          Enum.zip_with(rows, row_boxes, fn row, row_box ->
            row_style =
              Map.put(
                row_box.style,
                :height,
                Layout.resolve_content_size(row_box.style, :height, nil, 0.0)
              )

            %{
              row: %{style: row_style},
              cells:
                Enum.map(row, fn cell ->
                  %{
                    column: cell.column,
                    colspan: Map.get(cell, :colspan, 1),
                    rowspan: Map.get(cell, :rowspan, 1),
                    cell: %{style: cell.style, children: []}
                  }
                end)
            }
          end)

        metrics =
          Map.new(cells, fn cell ->
            edges = cell_edges(cell.prepared.style)

            intrinsic_height =
              cell.measured.ascent + cell.measured.descent + edges.top + edges.bottom

            minimum_height =
              max(
                intrinsic_height,
                Layout.resolve_content_size(
                  cell.prepared.style,
                  :height,
                  Layout.resolve_content_size(style, :height, nil, nil),
                  cell.measured.ascent + cell.measured.descent
                ) + edges.top + edges.bottom
              )

            {{cell.row, cell.prepared.column},
             %{
               intrinsic_height: intrinsic_height,
               minimum_height: minimum_height,
               baseline: cell.measured.ascent + edges.top,
               inset_bottom: edges.bottom,
               has_flow: true
             }}
          end)

        # Canonical span placements and validated cell geometry establish this
        # internal projection's shape. Resource failures propagate through the
        # existing outer Math render scope, rather than returning a second result.
        {:ok, allocated} =
          HtmlTableValidator.prepare_baseline_geometry(projected, metrics, vertical_spacing)

        Enum.zip_with(
          allocated.heights,
          allocated.baselines,
          &%{ascent: &2, descent: &1 - &2}
        )
      else
        row_sizes
      end

    natural_height = Enum.sum(Enum.map(row_sizes, &(&1.ascent + &1.descent)))
    spacing_height = vertical_spacing * if(column_count == 0, do: 0, else: length(rows) + 1)

    target_height =
      max(
        Layout.resolve_content_size(style, :height, nil, natural_height + spacing_height) -
          spacing_height,
        natural_height
      )

    row_sizes =
      Enum.map(row_sizes, fn row ->
        height = row.ascent + row.descent

        extra =
          (target_height - natural_height) *
            if(natural_height > 0, do: height / natural_height, else: 1 / max(length(rows), 1))

        %{row | descent: row.descent + extra}
      end)

    {column_offsets, column_extent} =
      Enum.map_reduce(columns, horizontal_spacing, fn column, offset ->
        {offset, offset + column.width + horizontal_spacing}
      end)

    {row_offsets, height} =
      Enum.map_reduce(row_sizes, vertical_spacing, fn row, offset ->
        {offset, offset + row.ascent + row.descent + vertical_spacing}
      end)

    height = if rows == [], do: target_height + spacing_height, else: height

    column_offsets = List.to_tuple(column_offsets)
    row_offsets = List.to_tuple(row_offsets)
    row_metrics = List.to_tuple(row_sizes)
    ascent = height / 2 + axis + axis_offset
    descent = height / 2 - axis - axis_offset

    positioned =
      Enum.map(cells, fn cell ->
        column = cell.prepared.column
        style = cell.prepared.style
        edges = cell_edges(style)
        colspan = Map.get(cell.prepared, :colspan, 1)
        width = columns |> Enum.slice(column, colspan) |> Enum.map(& &1.width) |> Enum.sum()
        width = width + horizontal_spacing * (colspan - 1)
        content_width = width - edges.left - edges.right
        available = content_width - cell.measured.width

        shift =
          case Map.get(style, :text_align, :center) do
            :left -> 0.0
            :right -> available
            :center -> available / 2
          end

        row = elem(row_metrics, cell.row)

        painted? =
          Map.get(style, :background_color) not in [nil, :transparent] or
            Enum.any?(
              Map.values(
                Map.get(style, :border_widths, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0})
              ),
              &(&1 > 0)
            )

        x = elem(column_offsets, column)
        rowspan = Map.get(cell.prepared, :rowspan, 1)

        allocated_height =
          row_sizes
          |> Enum.slice(cell.row, rowspan)
          |> Enum.map(&(&1.ascent + &1.descent))
          |> Enum.sum()

        allocated_height = allocated_height + vertical_spacing * (rowspan - 1)
        content_height = allocated_height - edges.top - edges.bottom

        content_ascent =
          case Map.get(style, :vertical_align, :baseline) do
            :top -> cell.measured.ascent
            :bottom -> content_height - cell.measured.descent
            :middle -> (content_height + cell.measured.ascent - cell.measured.descent) / 2
            :baseline -> row.ascent - edges.top
          end

        y = ascent - elem(row_offsets, cell.row) - content_ascent - edges.top

        %{
          measured: cell.measured,
          style:
            Map.put(
              style,
              :_math_source,
              Map.get(cell.prepared, :source, %{namespace: :mathml, tag: "mtd"})
            ),
          x: x,
          y: y,
          inner_x: shift,
          content_width: content_width,
          content_ascent: content_ascent,
          content_descent: content_height - content_ascent,
          painted_ascent:
            if(painted?,
              do: max(cell.measured.ink_ascent, content_ascent + edges.top),
              else: cell.measured.ink_ascent
            ),
          painted_descent:
            if(painted?,
              do: max(cell.measured.ink_descent, content_height - content_ascent + edges.bottom),
              else: cell.measured.ink_descent
            )
        }
      end)

    background_x = if columns == [], do: 0.0, else: elem(column_offsets, 0)
    background_width = max(column_extent - 2 * horizontal_spacing, 0.0)

    row_backgrounds =
      Enum.flat_map(Enum.with_index(row_boxes), fn {row_box, index} ->
        HtmlValidator.reserve_render_resource(:max_table_grid_work, 1, :layout)

        if Map.get(row_box.style, :background_color) not in [nil, :transparent] or
             Map.get(row_box.style, :background_image) != nil do
          covered =
            if Enum.any?(cells, &(Map.get(&1.prepared, :rowspan, 1) > 1)) do
              HtmlValidator.reserve_render_resource(:max_table_grid_work, length(cells), :layout)

              Enum.filter(
                cells,
                &(index >= &1.row and index < &1.row + Map.get(&1.prepared, :rowspan, 1))
              )
            else
              Map.get(by_row, index, [])
            end

          Enum.map(covered, fn cell ->
            HtmlValidator.reserve_render_resource(:max_table_grid_work, 1, :layout)
            column_index = cell.prepared.column
            row = elem(row_metrics, index)

            %{
              x: elem(column_offsets, column_index),
              background_x: background_x,
              background_width: background_width,
              y: ascent - elem(row_offsets, index) - row.ascent,
              width:
                columns
                |> Enum.slice(column_index, Map.get(cell.prepared, :colspan, 1))
                |> Enum.map(& &1.width)
                |> Enum.sum()
                |> Kernel.+(horizontal_spacing * (Map.get(cell.prepared, :colspan, 1) - 1)),
              ascent: row.ascent,
              descent: row.descent,
              background_style:
                row_box.style
                |> Map.put(:background_color, nil)
                |> Map.put(:border_styles, %{top: :none, right: :none, bottom: :none, left: :none}),
              style:
                row_box.style
                |> Map.merge(%{padding: zero, border_widths: zero})
                |> Map.put(:_math_source, row_box.source)
            }
          end)
        else
          []
        end
      end)

    result = %{
      width: target + spacing_width,
      min_content_width:
        max(
          minimum + spacing_width,
          Layout.resolve_content_size(
            style,
            :width,
            available_inline_size,
            minimum + spacing_width
          )
        ),
      max_content_width:
        max(
          minimum + spacing_width,
          Layout.resolve_content_size(
            style,
            :width,
            available_inline_size,
            intrinsic + spacing_width
          )
        ),
      ascent: ascent,
      descent: descent,
      ink_ascent:
        Enum.max(
          Enum.map(positioned, &(&1.painted_ascent + &1.y)) ++
            Enum.map(row_backgrounds, &(&1.ascent + &1.y)),
          fn -> 0.0 end
        ),
      ink_descent:
        Enum.max(
          Enum.map(positioned, &(&1.painted_descent - &1.y)) ++
            Enum.map(row_backgrounds, &(&1.descent - &1.y)),
          fn -> 0.0 end
        ),
      cells: positioned,
      row_backgrounds: row_backgrounds,
      columns: columns,
      row_sizes: row_sizes
    }

    owner_painted? =
      Map.get(style, :background_color) not in [nil, :transparent] or
        Map.get(style, :background_image) != nil or
        Enum.any?(Map.values(border), &(&1 > 0))

    result =
      if owner_painted?,
        do: %{
          result
          | ink_ascent: max(result.ink_ascent, result.ascent),
            ink_descent: max(result.ink_descent, result.descent)
        },
        else: result

    numeric =
      Map.values(Map.drop(result, [:cells, :row_backgrounds, :columns, :row_sizes])) ++
        Enum.flat_map(positioned, fn cell ->
          [
            cell.x,
            cell.y,
            cell.inner_x,
            cell.content_width,
            cell.content_ascent,
            cell.content_descent,
            cell.content_ascent + cell.content_descent
          ] ++
            Enum.flat_map(
              cell.measured.drawing_items,
              &[&1.x + cell.x + cell.inner_x + cell_edges(cell.style).left, &1.y + cell.y]
            )
        end)

    numeric =
      numeric ++
        Enum.flat_map(
          row_backgrounds,
          &[
            &1.x,
            &1.y,
            &1.width,
            &1.ascent,
            &1.descent,
            &1.ascent + &1.descent,
            &1.x + padding.left + border.left
          ]
        )

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
          Enum.map(
            cell.measured.drawing_items,
            &(&1.x + cell.x + cell.inner_x + cell_edges(cell.style).left + padding.left +
                border.left)
          )
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
