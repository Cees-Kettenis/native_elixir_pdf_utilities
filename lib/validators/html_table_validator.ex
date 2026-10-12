defmodule NativeElixirPdfUtilities.Validators.HtmlTableValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.HtmlMetadataValidator

  @doc """
  Prepares logical HTML table models and header associations on validated DOM nodes.

  This model is independent of CSS display and the visible grid returned by `form/1`.
  Positive spans may imply rows; uncovered slots do not invent authored cells.
  `:_table_model.errors` retains actionable table model diagnostics, including
  unanchored rows/columns and overlapping source cells. These recover according
  to the HTML model, so successful rendering does not certify table conformance.
  Visible CSS slot overlaps continue to fail through `form/1`.

  `:_document_order` paths are locators and association keys. Canonical parser
  identity fields are preserved unchanged and are never issued as capabilities.
  ID lookup uses first tree order within each document/tree scope, includes hidden
  and non-cell elements, and excludes inert template content from the light tree.
  """
  @spec prepare_document([map()]) :: {:ok, [map()]} | {:error, Diagnostics.detailed_error()}
  def prepare_document(children) do
    HtmlValidator.with_render_budget(fn ->
      try do
        {scopes, node_count, explicit_scopes} = table_preparation_scopes(children)

        if MapSet.size(scopes) > 0 do
          HtmlValidator.reserve_render_resource(:max_table_model_work, node_count, :style)

          ids =
            Enum.reduce(explicit_scopes, %{}, fn scope, ids ->
              Map.merge(ids, HtmlMetadataValidator.first_tree_ids(children, scope))
            end)

          {:ok, prepare_logical_nodes(children, ids)}
        else
          {:ok, children}
        end
      catch
        {:table_span_validation, error} -> error
      end
    end)
  end

  # Automatic associations use table slots, while explicit headers require the
  # shared first-ID index. Collect both scopes and work in one traversal.
  defp table_preparation_scopes(nodes, acc \\ {MapSet.new(), 0, MapSet.new()}) do
    Enum.reduce(nodes, acc, fn node, {scopes, count, explicit_scopes} ->
      case node do
        %{type: :element, tag: tag, children: children} ->
          html? = Map.get(node, :namespace, :html) == :html

          scopes =
            if html? and tag in ["table", "td", "th", "col", "colgroup"],
              do: MapSet.put(scopes, tree_scope(node)),
              else: scopes

          explicit_scopes =
            if html? and tag in ["td", "th"] and Map.has_key?(node.attributes, "headers"),
              do: MapSet.put(explicit_scopes, tree_scope(node)),
              else: explicit_scopes

          acc = {scopes, count + 1, explicit_scopes}
          if tag == "template" and html?, do: acc, else: table_preparation_scopes(children, acc)

        _ ->
          {scopes, count + 1, explicit_scopes}
      end
    end)
  end

  defp tree_scope(node), do: {Map.get(node, :document_id), Map.get(node, :tree_scope_id)}

  defp prepare_logical_nodes(nodes, ids) do
    Enum.map(nodes, fn node ->
      case node do
        %{type: :element, tag: "template"} ->
          node

        %{type: :element, children: children} ->
          node = %{node | children: prepare_logical_nodes(children, ids)}

          node =
            case {node.tag, Map.get(node, :namespace, :html)} do
              {tag, :html} when tag in ["td", "th"] ->
                Map.put(node, :_table_spans, %{
                  colspan: source_span(node, "colspan"),
                  rowspan: source_span(node, "rowspan")
                })

              {"colgroup", :html} ->
                # With authored columns, the group's span attribute is ignored.
                span =
                  if Enum.any?(node.children, &(Map.get(&1, :tag) == "col")),
                    do: 1,
                    else: source_span(node, "span")

                Map.put(node, :_table_spans, %{span: span})

              {"col", :html} ->
                Map.put(node, :_table_spans, %{span: source_span(node, "span")})

              _ ->
                node
            end

          if node.tag == "table" and Map.get(node, :namespace, :html) == :html do
            model = logical_model(node, ids)
            cells = Map.new(model.cells, &{&1.source, &1})

            node
            |> Map.put(:_table_model, model)
            |> Map.put(:children, attach_cell_semantics(node.children, cells))
          else
            node
          end

        _ ->
          node
      end
    end)
  end

  defp attach_cell_semantics(nodes, cells) do
    Enum.map(nodes, fn node ->
      case node do
        %{type: :element, tag: "table"} ->
          # Nested tables already own their cell metadata and associations.
          node

        %{type: :element, children: children} ->
          node =
            case Map.fetch(cells, node._document_order) do
              {:ok, semantics} -> Map.put(node, :_table_cell, semantics)
              :error -> node
            end

          %{node | children: attach_cell_semantics(children, cells)}

        _ ->
          node
      end
    end)
  end

  defp logical_model(table, ids) do
    {column_groups, initial_width} =
      Enum.reduce(table.children, {[], 0}, fn node, {groups, width} ->
        case node do
          %{tag: "colgroup", children: children} ->
            columns = Enum.filter(children, &(Map.get(&1, :tag) == "col"))
            columns = if columns == [], do: [node], else: columns

            span =
              Enum.reduce(columns, 0, fn column, span ->
                span + column._table_spans.span
              end)

            HtmlValidator.reserve_render_resource(:max_table_model_work, span, :style)

            {[%{source: node._document_order, column: width, colspan: span} | groups],
             width + span}

          _ ->
            {groups, width}
        end
      end)

    {ordinary, footers} = Enum.split_with(table.children, &(Map.get(&1, :tag) != "tfoot"))

    # Consecutive direct rows are a run without a source row group. A group ends
    # the run even when empty, so downward-growing cells cannot leak into it.
    runs =
      Enum.reduce(ordinary ++ footers, [], fn node, runs ->
        case node do
          %{tag: "tr"} ->
            case runs do
              [{nil, rows} | rest] -> [{nil, [node | rows]} | rest]
              _ -> [{nil, [node]} | runs]
            end

          %{tag: tag, children: rows} when tag in ["thead", "tbody", "tfoot"] ->
            [
              {node._document_order,
               Enum.reverse(Enum.filter(rows, &(Map.get(&1, :tag) == "tr")))}
              | runs
            ]

          _ ->
            runs
        end
      end)
      |> Enum.reverse()

    {cells, rows, row_groups, height, width} =
      Enum.reduce(runs, {[], [], [], 0, initial_width}, fn {group, group_rows},
                                                           {cells, rows, groups, start, width} ->
        group_rows = Enum.reverse(group_rows)

        {placed, height, width, _occupied} =
          group_rows
          |> Enum.with_index(start)
          |> Enum.reduce({[], start + length(group_rows), width, %{}}, fn {row, y},
                                                                          {placed, height, width,
                                                                           occupied} ->
            HtmlValidator.reserve_render_resource(:max_table_model_work, 1, :style)

            source_cells = Enum.filter(row.children, &(Map.get(&1, :tag) in ["td", "th"]))

            {placed, height, width, _next, occupied} =
              Enum.reduce(source_cells, {placed, height, width, 0, occupied}, fn node,
                                                                                 {placed, height,
                                                                                  width, next,
                                                                                  occupied} ->
                %{colspan: colspan, rowspan: requested} = node._table_spans

                column = next_logical_column(occupied, y, next)
                rowspan = max(requested, 1)

                cell = %{
                  source: node._document_order,
                  identity: Map.take(node, [:document_id, :tree_scope_id, :node_id, :parent_id]),
                  source_metadata: Map.get(node, :source),
                  tag: node.tag,
                  id: Map.get(node.attributes, "id"),
                  row: y,
                  column: column,
                  rowspan: rowspan,
                  colspan: colspan,
                  grows_downward: requested == 0,
                  row_group: group,
                  column_group: nil,
                  scope: header_scope(Map.get(node.attributes, "scope")),
                  abbr: Map.get(node.attributes, "abbr"),
                  empty: empty_cell?(node.children),
                  attributes: node.attributes,
                  headers: []
                }

                HtmlValidator.reserve_render_resource(
                  :max_table_model_work,
                  colspan * rowspan,
                  :style
                )

                end_row = if requested == 0, do: :infinity, else: y + rowspan

                occupied =
                  Enum.reduce(
                    column..(column + colspan - 1),
                    occupied,
                    &Map.update(&2, &1, end_row, fn existing -> max(existing, end_row) end)
                  )

                {[cell | placed], max(height, y + rowspan), max(width, column + colspan),
                 column + colspan, occupied}
              end)

            {placed, height, width, occupied}
          end)

        placed =
          Enum.map(placed, fn cell ->
            if cell.grows_downward do
              rowspan = height - cell.row

              HtmlValidator.reserve_render_resource(
                :max_table_model_work,
                cell.colspan * (rowspan - 1),
                :style
              )

              %{cell | rowspan: rowspan}
            else
              cell
            end
          end)

        actual_rows =
          Map.new(Enum.with_index(group_rows, start), fn {node, row} ->
            {row, node._document_order}
          end)

        model_rows =
          if height == start,
            do: [],
            else: Enum.map(start..(height - 1), &%{row: &1, source: Map.get(actual_rows, &1)})

        HtmlValidator.reserve_render_resource(:max_table_model_work, height - start, :style)

        groups =
          if group && height > start,
            do: [%{source: group, row: start, rowspan: height - start} | groups],
            else: groups

        {Enum.reverse(placed) ++ cells, Enum.reverse(model_rows, rows), groups, height, width}
      end)

    cells = Enum.sort_by(cells, &{&1.row, &1.column})
    column_groups = Enum.reverse(column_groups)

    cells =
      Enum.map(cells, fn cell ->
        group =
          Enum.find(
            column_groups,
            &(cell.column >= &1.column and cell.column < &1.column + &1.colspan)
          )

        %{cell | column_group: if(group, do: group.source)}
      end)

    {slots, errors} =
      Enum.reduce(cells, {%{}, []}, fn cell, {slots, errors} ->
        Enum.reduce(cell.row..(cell.row + cell.rowspan - 1), {slots, errors}, fn row, acc ->
          Enum.reduce(cell.column..(cell.column + cell.colspan - 1), acc, fn column,
                                                                             {slots, errors} ->
            slot = {column, row}
            previous = Map.get(slots, slot, [])

            errors =
              if previous == [],
                do: errors,
                else: [
                  model_diagnostic(
                    table,
                    "logical table model error at row #{row + 1}, column #{column + 1}: overlapping source cells; reduce the spans or place each cell in unused slots"
                  )
                  | errors
                ]

            {Map.put(slots, slot, [cell.source | previous]), errors}
          end)
        end)
      end)

    HtmlValidator.reserve_render_resource(:max_table_model_work, width * height, :style)

    uncovered =
      for y <- indices(height), x <- indices(width), not Map.has_key?(slots, {x, y}), do: {x, y}

    anchored_rows = MapSet.new(cells, & &1.row)
    anchored_columns = MapSet.new(cells, & &1.column)

    errors =
      Enum.reduce(
        [{indices(height), anchored_rows, "row"}, {indices(width), anchored_columns, "column"}],
        errors,
        fn {indices, anchors, axis}, errors ->
          Enum.reduce(indices, errors, fn index, errors ->
            if width > 0 and height > 0 and not MapSet.member?(anchors, index),
              do: [
                model_diagnostic(
                  table,
                  "logical table model error: #{axis} #{index + 1} has no anchored cell; add a cell anchored in this #{axis} or reduce the spanning cell; implied rows and vacant slots are retained for rendering"
                )
                | errors
              ],
              else: errors
          end)
        end
      )

    cells = associate_headers(cells, slots, ids, tree_scope(table))

    %{
      source: table._document_order,
      identity: Map.take(table, [:document_id, :tree_scope_id, :node_id, :parent_id]),
      source_metadata: Map.get(table, :source),
      width: width,
      height: height,
      rows: Enum.reverse(rows),
      row_groups: Enum.reverse(row_groups),
      column_groups: column_groups,
      cells: Enum.map(cells, &Map.drop(&1, [:attributes, :grows_downward])),
      uncovered_slots: uncovered,
      errors: Enum.reverse(errors)
    }
  end

  # Share scalar recovery once between the logical model and CSS defaults.
  # The local throw short-circuits nested formation and is caught by preparation.
  defp source_span(node, name) do
    case HtmlValidator.validate_table_span_attribute(node.attributes, name) do
      {:ok, span} ->
        span

      {:error, {reason, diagnostic}} ->
        throw(
          {:table_span_validation,
           {:error,
            {reason,
             Diagnostics.diagnostic(
               diagnostic.stage,
               reason,
               diagnostic.message,
               Keyword.merge(
                 source_context(node),
                 diagnostic
                 |> Map.take([:operation, :module, :source, :line, :column])
                 |> Map.to_list()
               )
             )}}}
        )
    end
  end

  defp indices(count), do: if(count == 0, do: [], else: 0..(count - 1))

  defp next_logical_column(occupied, row, column) do
    HtmlValidator.reserve_render_resource(:max_table_model_work, 1, :style)

    case Map.get(occupied, column, 0) do
      :infinity -> next_logical_column(occupied, row, column + 1)
      end_row when end_row > row -> next_logical_column(occupied, row, column + 1)
      _ -> column
    end
  end

  defp header_scope(value) do
    case String.downcase(value || "", :ascii) do
      "row" -> :row
      "col" -> :col
      "rowgroup" -> :rowgroup
      "colgroup" -> :colgroup
      _ -> :auto
    end
  end

  defp empty_cell?(children) do
    Enum.all?(children, fn child ->
      case child do
        %{type: :element} -> false
        %{type: :text, text: text} -> HtmlValidator.html_class_tokens(text) == []
        _ -> true
      end
    end)
  end

  defp source_context(node) do
    case Map.get(node, :source) do
      %{text: text, line: line, column: column} ->
        [source: text, line: line, column: column]

      _ ->
        [source: "#{node.tag} #{inspect(node._document_order)}"]
    end
  end

  defp model_diagnostic(table, message) do
    Diagnostics.diagnostic(
      :style,
      :invalid_document,
      message,
      [module: __MODULE__, operation: :table_model] ++ source_context(table)
    )
  end

  defp associate_headers(cells, slots, ids, scope) do
    by_source = Map.new(cells, &{&1.source, &1})
    data = Enum.reject(cells, &(&1.tag == "th"))

    headers =
      cells
      |> Enum.filter(&(&1.tag == "th"))
      |> Map.new(fn cell ->
        HtmlValidator.reserve_render_resource(
          :max_table_header_work,
          length(data) * 2 + 1,
          :style
        )

        column_header =
          cell.scope == :col or
            (cell.scope == :auto and
               not Enum.any?(
                 data,
                 &(&1.row < cell.row + cell.rowspan and &1.row + &1.rowspan > cell.row)
               ))

        row_header =
          cell.scope == :row or
            (cell.scope == :auto and not column_header and
               not Enum.any?(
                 data,
                 &(&1.column < cell.column + cell.colspan and &1.column + &1.colspan > cell.column)
               ))

        {cell.source, %{column: column_header, row: row_header}}
      end)

    group_headers = Enum.filter(cells, &(&1.tag == "th" and &1.scope in [:rowgroup, :colgroup]))

    Enum.map(cells, fn cell ->
      associated =
        case Map.fetch(cell.attributes, "headers") do
          {:ok, explicit} ->
            HtmlValidator.reserve_render_resource(
              :max_table_header_work,
              byte_size(explicit),
              :style
            )

            {document_id, tree_scope_id} = scope

            explicit
            |> HtmlValidator.html_class_tokens()
            |> Enum.map(fn id ->
              case Map.get(ids, {document_id, tree_scope_id, id}) do
                nil -> nil
                node -> node._document_order
              end
            end)

          :error when map_size(headers) == 0 ->
            []

          :error ->
            left =
              Enum.flat_map(
                cell.row..(cell.row + cell.rowspan - 1),
                &scan_headers(cell, cell.column - 1, &1, -1, 0, slots, by_source, headers)
              )

            above =
              Enum.flat_map(
                cell.column..(cell.column + cell.colspan - 1),
                &scan_headers(cell, &1, cell.row - 1, 0, -1, slots, by_source, headers)
              )

            HtmlValidator.reserve_render_resource(
              :max_table_header_work,
              length(group_headers),
              :style
            )

            groups =
              Enum.filter(group_headers, fn header ->
                header.tag == "th" and header.column <= cell.column + cell.colspan - 1 and
                  header.row <= cell.row + cell.rowspan - 1 and
                  ((header.scope == :rowgroup and not is_nil(cell.row_group) and
                      header.row_group == cell.row_group) or
                     (header.scope == :colgroup and not is_nil(cell.column_group) and
                        header.column_group == cell.column_group))
              end)
              |> Enum.map(& &1.source)

            left ++ above ++ groups
        end

      associated =
        Enum.uniq(associated)
        |> Enum.filter(fn source ->
          source != cell.source and Map.has_key?(by_source, source) and
            not Map.fetch!(by_source, source).empty
        end)

      %{cell | headers: associated}
    end)
  end

  defp scan_headers(principal, x, y, dx, dy, slots, cells, kinds) do
    # Every scan reaches the leading row/column boundary. Reserve its exact slot
    # visits before reading slots; opaque-header comparisons remain incremental.
    visits = if x < 0 or y < 0, do: 0, else: if(dx == 0, do: y + 1, else: x + 1)

    if visits > 0,
      do: HtmlValidator.reserve_render_resource(:max_table_header_work, visits, :style)

    initial = %{
      headers: [],
      opaque: [],
      block: if(principal.tag == "th", do: [principal], else: []),
      in_block: principal.tag == "th"
    }

    result = scan_header_slots(x, y, dx, dy, slots, cells, kinds, initial)
    Enum.reverse(result.headers)
  end

  defp scan_header_slots(x, y, dx, dy, slots, cells, kinds, state) do
    if x < 0 or y < 0 do
      state
    else
      if state.opaque != [] do
        HtmlValidator.reserve_render_resource(
          :max_table_header_work,
          length(state.opaque),
          :style
        )
      end

      state =
        case Map.get(slots, {x, y}) do
          [source] ->
            cell = Map.fetch!(cells, source)

            if cell.tag == "th" do
              kind = Map.fetch!(kinds, source)

              blocked =
                if dx == 0,
                  do:
                    not kind.column or
                      Enum.any?(
                        state.opaque,
                        &(&1.column == cell.column and &1.colspan == cell.colspan)
                      ),
                  else:
                    not kind.row or
                      Enum.any?(
                        state.opaque,
                        &(&1.row == cell.row and &1.rowspan == cell.rowspan)
                      )

              %{
                state
                | in_block: true,
                  block: [cell | state.block],
                  headers: if(blocked, do: state.headers, else: [source | state.headers])
              }
            else
              if state.in_block,
                do: %{state | in_block: false, opaque: state.block ++ state.opaque, block: []},
                else: state
            end

          _ ->
            state
        end

      scan_header_slots(x + dx, y + dy, dx, dy, slots, cells, kinds, state)
    end
  end

  @doc "Prepares validated visible table rows, column hints and non-overlapping cell placements."
  @spec form(term()) ::
          {:ok, %{rows: [map()], columns: [map()]}} | {:error, Diagnostics.detailed_error()}
  def form(children) do
    HtmlValidator.with_render_budget(fn ->
      case children do
        children when is_list(children) ->
          result =
            children
            |> Enum.with_index()
            |> Enum.reduce_while({:ok, [], [], 0}, fn {child, index},
                                                      {:ok, rows, columns, implicit_group} ->
              next =
                case child do
                  %{style: %{display: display}} when display in [:none, :table_caption] ->
                    {:ok, [], [], implicit_group}

                  %{style: %{display: :table_row}} ->
                    with {:ok, row} <- prepare_row(child, :body, {:implicit, implicit_group}, nil),
                         do: {:ok, [row], [], implicit_group}

                  %{style: %{display: :table_row_group} = style, children: group_rows}
                  when is_list(group_rows) ->
                    section = Map.get(style, :table_section, :body)

                    if section in [:head, :body, :foot] do
                      Enum.reduce_while(group_rows, {:ok, []}, fn row, {:ok, prepared} ->
                        case row do
                          %{style: %{display: :none}} ->
                            {:cont, {:ok, prepared}}

                          _ ->
                            case prepare_row(row, section, index, style) do
                              {:ok, row} -> {:cont, {:ok, [row | prepared]}}
                              error -> {:halt, error}
                            end
                        end
                      end)
                      |> case do
                        {:ok, prepared} -> {:ok, Enum.reverse(prepared), [], index + 1}
                        error -> error
                      end
                    else
                      invalid_table(child, "table row group section must be head, body or foot")
                    end

                  %{style: %{display: :table_column_group} = style, children: column_nodes}
                  when is_list(column_nodes) ->
                    visible = Enum.reject(column_nodes, &match?(%{style: %{display: :none}}, &1))
                    nodes = if visible == [], do: [child], else: visible

                    Enum.reduce_while(nodes, {:ok, []}, fn column, {:ok, prepared} ->
                      case column do
                        %{style: %{display: display} = column_style}
                        when display == :table_column or column == child ->
                          with {:ok, span} <- validated_span(column, "span") do
                            HtmlValidator.reserve_render_resource(
                              :max_table_grid_work,
                              span,
                              :layout
                            )

                            hint = %{
                              width: Map.get(column_style, :width) || Map.get(style, :width)
                            }

                            {:cont, {:ok, Enum.reverse(List.duplicate(hint, span), prepared)}}
                          else
                            error -> {:halt, error}
                          end

                        _ ->
                          {:halt,
                           invalid_table(
                             column,
                             "column groups require column children; anonymous table formatting is not implemented"
                           )}
                      end
                    end)
                    |> case do
                      {:ok, prepared} -> {:ok, [], Enum.reverse(prepared), implicit_group}
                      error -> error
                    end

                  _ ->
                    invalid_table(
                      child,
                      "tables require row, row group, column group or caption boxes; anonymous table formatting is not implemented"
                    )
                end

              case next do
                {:ok, child_rows, child_columns, next_group} ->
                  {:cont,
                   {:ok, Enum.reverse(child_rows, rows), Enum.reverse(child_columns, columns),
                    next_group}}

                error ->
                  {:halt, error}
              end
            end)

          with {:ok, rows, columns, _group} <- result,
               {:ok, rows} <- form_grid(Enum.reverse(rows)) do
            {:ok, %{rows: rows, columns: Enum.reverse(columns)}}
          end

        _ ->
          invalid_table(%{}, "table formation requires a list of styled children")
      end
    end)
  end

  defp prepare_row(node, section, group, group_style) do
    case node do
      %{style: %{display: :table_row}, children: cells} when is_list(cells) ->
        cells = Enum.reject(cells, &match?(%{style: %{display: :none}}, &1))

        Enum.reduce_while(cells, :ok, fn cell, :ok ->
          case cell do
            %{style: %{display: :table_cell}, children: children} when is_list(children) ->
              {:cont, :ok}

            _ ->
              {:halt,
               invalid_table(
                 cell,
                 "table rows require cell boxes; anonymous table cells are not implemented"
               )}
          end
        end)
        |> case do
          :ok ->
            {:ok,
             %{
               row: Map.put(node, :children, cells),
               section: section,
               row_group: group,
               group_style: group_style
             }}

          error ->
            error
        end

      _ ->
        invalid_table(node, "table row groups require row boxes with list children")
    end
  end

  defp form_grid(rows) do
    # Count consecutive group rows once. Empty rows take part in zero spans.
    {_group, _count, remaining} =
      Enum.reduce(Enum.reverse(rows), {nil, 0, []}, fn row, {previous, count, remaining} ->
        count = if row.row_group == previous, do: count + 1, else: 1
        {row.row_group, count, [count | remaining]}
      end)

    rows
    |> Enum.zip(remaining)
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, [], %{}}, fn {{row, group_length}, row_index},
                                            {:ok, prepared, occupied} ->
      HtmlValidator.reserve_render_resource(:max_table_grid_work, 1 + map_size(occupied), :layout)

      active = for {column, end_row} <- occupied, end_row > row_index, do: column

      result =
        Enum.reduce_while(row.row.children, {:ok, [], occupied, 0}, fn cell,
                                                                       {:ok, placed, occupied,
                                                                        next_column} ->
          with {:ok, colspan} <- validated_span(cell, "colspan"),
               {:ok, requested_rowspan} <- validated_span(cell, "rowspan") do
            # CSS visible cell geometry ends at its row group. Logical implied
            # rows and complete HTML header association are a separate model phase.
            rowspan =
              if requested_rowspan == 0,
                do: group_length,
                else: min(requested_rowspan, group_length)

            column = next_free_column(occupied, row_index, next_column)
            HtmlValidator.reserve_render_resource(:max_table_grid_work, colspan, :layout)

            overlap =
              Enum.find(column..(column + colspan - 1), &(Map.get(occupied, &1, 0) > row_index))

            if is_nil(overlap) do
              occupied =
                if rowspan > 1 do
                  Enum.reduce(
                    column..(column + colspan - 1),
                    occupied,
                    &Map.put(&2, &1, row_index + rowspan)
                  )
                else
                  occupied
                end

              cell = %{
                cell
                | style: Map.merge(cell.style, %{colspan: colspan, rowspan: requested_rowspan})
              }

              placement = %{cell: cell, column: column, colspan: colspan, rowspan: rowspan}
              {:cont, {:ok, [placement | placed], occupied, column + colspan}}
            else
              {:halt,
               invalid_table(
                 cell,
                 "table model error at row #{row_index + 1}, column #{column + 1}: colspan overlaps a prior rowspan in column #{overlap + 1}; reduce the spans or place the cell in unused slots"
               )}
            end
          else
            error -> {:halt, error}
          end
        end)

      case result do
        {:ok, placed, occupied, _next_column} ->
          placed = Enum.reverse(placed)
          covered = Enum.map(placed, &(&1.column + &1.colspan - 1)) ++ active
          consumed = Enum.max(covered, fn -> -1 end) + 1

          row =
            Map.merge(row, %{cells: placed, active_columns: active, consumed_columns: consumed})

          {:cont, {:ok, [row | prepared], occupied}}

        error ->
          {:halt, error}
      end
    end)
    |> case do
      {:ok, rows, _occupied} -> {:ok, Enum.reverse(rows)}
      error -> error
    end
  end

  defp next_free_column(occupied, row_index, column) do
    HtmlValidator.reserve_render_resource(:max_table_grid_work, 1, :layout)

    if Map.get(occupied, column, 0) > row_index,
      do: next_free_column(occupied, row_index, column + 1),
      else: column
  end

  defp validated_span(node, name) do
    case HtmlValidator.normalize_table_span(
           name,
           Map.get(node.style, String.to_existing_atom(name), 1)
         ) do
      :error ->
        invalid_table(node, "#{name} in styled table input must be a non-negative integer")

      {:error, {reason, diagnostic}} ->
        {:error,
         {reason,
          Diagnostics.with_context(diagnostic,
            source: Map.get(node, :tag, "table"),
            operation: :layout
          )}}

      result ->
        result
    end
  end

  defp invalid_table(node, message) do
    source = if is_map(node), do: Map.get(node, :tag, "table"), else: "table"

    Diagnostics.error(:layout, :invalid_layout, message,
      module: __MODULE__,
      operation: :layout,
      source: source
    )
  end
end
