defmodule NativeElixirPdfUtilities.Validators.HtmlTableValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.HtmlMetadataValidator

  @doc "Reports whether a legacy table presentation attribute belongs to an HTML element."
  @spec presentation_attribute?(String.t(), String.t()) :: boolean()
  def presentation_attribute?(tag, name) do
    case tag do
      "table" ->
        name in ~w(width height bgcolor cellspacing cellpadding align border bordercolor frame rules)

      "caption" ->
        name == "align"

      "col" ->
        name == "width"

      tag when tag in ~w(thead tbody tfoot tr) ->
        name in ~w(height bgcolor align valign bordercolor)

      tag when tag in ~w(td th) ->
        name in ~w(width height bgcolor align valign nowrap bordercolor)

      _ ->
        false
    end
  end

  @doc "Prepares HTML table presentation hints before the authored CSS cascade."
  @spec presentation_hints(map(), [map()], map()) ::
          {:ok, map()} | {:error, Diagnostics.detailed_error()}
  def presentation_hints(node, ancestors, inherited) do
    case node do
      %{tag: tag, attributes: attributes}
      when tag in ~w(table caption col colgroup thead tbody tfoot tr td th) ->
        if Map.get(node, :namespace, :html) == :html do
          dimensions =
            case tag do
              "table" -> [{"width", :width}, {"height", :height}]
              tag when tag in ~w(thead tbody tfoot tr) -> [{"height", :height}]
              tag when tag in ~w(td th) -> [{"width", :width}, {"height", :height}]
              "col" -> [{"width", :width}]
              _ -> []
            end

          table =
            Enum.find(ancestors, &(&1.tag == "table" and Map.get(&1, :namespace, :html) == :html))

          table_attributes = if table == nil, do: %{}, else: table.attributes

          result =
            with {:ok, hints} <- HtmlValidator.html_dimension_hints(attributes, dimensions, tag),
                 {:ok, spacing} <- legacy_pixel_attribute(attributes, "cellspacing", nil),
                 {:ok, padding} <-
                   legacy_pixel_attribute(
                     if(tag == "table", do: attributes, else: table_attributes),
                     "cellpadding",
                     nil
                   ),
                 {:ok, border_hints} <- legacy_border_hints(node, table_attributes, inherited) do
              hints = Map.merge(hints, border_hints)

              hints =
                Map.reject(hints, fn {property, value} ->
                  value in [0, 0.0, {:percent, 0.0}] and
                    ((tag == "table" and property == :width) or tag in ~w(td th))
                end)

              hints =
                case HtmlValidator.parse_html_legacy_color(Map.get(attributes, "bgcolor")) do
                  {:ok, color} when tag in ~w(table thead tbody tfoot tr td th) ->
                    Map.put(hints, :background_color, color)

                  _ ->
                    hints
                end

              hints =
                if tag == "table" and spacing != nil,
                  do: Map.put(hints, :border_spacing, {spacing, spacing}),
                  else: hints

              hints =
                if tag in ~w(td th) and padding != nil,
                  do:
                    Map.put(
                      hints,
                      :padding,
                      NativeElixirPdfUtilities.Validators.CssValueValidator.edges(padding)
                    ),
                  else: hints

              align = String.downcase(Map.get(attributes, "align", ""), :ascii)
              valign = String.downcase(Map.get(attributes, "valign", ""), :ascii)

              hints =
                cond do
                  tag == "table" and align == "center" ->
                    Map.put(hints, :margin, %{top: 0.0, right: :auto, bottom: 0.0, left: :auto})

                  tag == "caption" and align == "bottom" ->
                    Map.put(hints, :caption_side, :bottom)

                  tag in ~w(thead tbody tfoot tr td th) and
                      align in ~w(left right center middle absmiddle justify) ->
                    align = if align in ~w(middle absmiddle), do: "center", else: align

                    hints
                    |> Map.put(:text_align, String.to_existing_atom(align))
                    |> Map.put(:_text_align_initial, false)

                  tag == "th" and not Map.get(inherited, :_text_align_initial, true) ->
                    Map.put(hints, :text_align, Map.get(inherited, :text_align, :left))

                  true ->
                    hints
                end

              hints =
                if tag in ~w(tr td th) do
                  Map.put(
                    hints,
                    :vertical_align,
                    Map.get(
                      inherited,
                      :_table_vertical_align,
                      Map.get(inherited, :vertical_align, :middle)
                    )
                  )
                else
                  hints
                end

              hints =
                if tag in ~w(thead tbody tfoot tr td th) and
                     valign in ~w(top middle bottom baseline),
                   do: Map.put(hints, :vertical_align, String.to_existing_atom(valign)),
                   else: hints

              hints =
                if tag in ~w(td th) and Map.has_key?(attributes, "nowrap"),
                  do: Map.put(hints, :white_space, :nowrap),
                  else: hints

              if tag == "table" and align in ~w(left right) do
                Diagnostics.error(
                  :style,
                  :unsupported_html,
                  "table align=#{align} requires float layout, which is not implemented; use align=center or authored margins for a non-floating table",
                  operation: :compute_style,
                  module: __MODULE__,
                  source: "table align=" <> Map.fetch!(attributes, "align")
                )
              else
                {:ok, hints}
              end
            end

          case result do
            {:error, {reason, diagnostic}} ->
              context = Keyword.take(source_context(node), [:line, :column])

              opts =
                Map.to_list(Map.take(diagnostic, [:operation, :module, :source, :line, :column]))

              Diagnostics.error(diagnostic.stage, reason, diagnostic.message, opts ++ context)

            other ->
              other
          end
        else
          {:ok, %{}}
        end

      _ ->
        {:ok, %{}}
    end
  end

  # HTML's magic border, frame and rules hints have distinct fallback and cascade
  # order. Keep this preparation in the table validator before author CSS.
  defp legacy_border_hints(node, table_attributes, inherited) do
    tag = node.tag
    attributes = node.attributes
    table_attributes = if tag == "table", do: attributes, else: table_attributes
    rules = String.downcase(Map.get(table_attributes, "rules", ""), :ascii)
    frame = String.downcase(Map.get(table_attributes, "frame", ""), :ascii)
    rules? = rules in ~w(none groups rows cols all)
    frame? = frame in ~w(void above below hsides lhs rhs vsides box border)

    inherited_colors =
      Map.get(
        inherited,
        :_table_border_colors,
        Map.get(
          inherited,
          :border_colors,
          NativeElixirPdfUtilities.Validators.CssValueValidator.edges(
            Map.get(inherited, :color, {0, 0, 0})
          )
        )
      )

    with {:ok, border} <- legacy_pixel_attribute(table_attributes, "border", 0.75) do
      hints =
        if (tag in ~w(thead tbody tfoot tr) or
              (tag in ~w(td th) and
                 ((is_number(border) and border > 0) or rules in ~w(rows cols all)))) and
             (Map.has_key?(inherited, :_table_border_colors) or
                Map.has_key?(inherited, :border_colors)),
           do: %{border_colors: inherited_colors, border_color: inherited_colors.top},
           else: %{}

      hints =
        cond do
          tag == "table" ->
            styles = if rules?, do: :hidden, else: :none
            styles = if is_number(border) and border > 0, do: :outset, else: styles

            styles =
              if Map.get(table_attributes, "bordercolor", "") != "", do: :solid, else: styles

            styles = NativeElixirPdfUtilities.Validators.CssValueValidator.edges(styles)

            styles =
              case frame do
                "void" ->
                  NativeElixirPdfUtilities.Validators.CssValueValidator.edges(:hidden)

                "above" ->
                  %{top: :solid, right: :hidden, bottom: :hidden, left: :hidden}

                "below" ->
                  %{top: :hidden, right: :hidden, bottom: :solid, left: :hidden}

                "hsides" ->
                  %{top: :solid, right: :hidden, bottom: :solid, left: :hidden}

                "lhs" ->
                  %{top: :hidden, right: :hidden, bottom: :hidden, left: :solid}

                "rhs" ->
                  %{top: :hidden, right: :solid, bottom: :hidden, left: :hidden}

                "vsides" ->
                  %{top: :hidden, right: :solid, bottom: :hidden, left: :solid}

                value when value in ["box", "border"] ->
                  NativeElixirPdfUtilities.Validators.CssValueValidator.edges(:solid)

                _ ->
                  styles
              end

            hints = if rules?, do: Map.put(hints, :border_collapse, :collapse), else: hints

            hints =
              if border != nil,
                do:
                  Map.put(
                    hints,
                    :border_widths,
                    NativeElixirPdfUtilities.Validators.CssValueValidator.edges(border)
                  ),
                else: hints

            hints =
              if frame?,
                do:
                  Map.put(
                    hints,
                    :border_widths,
                    NativeElixirPdfUtilities.Validators.CssValueValidator.edges(0.75)
                  ),
                else: hints

            if rules? or frame? or border != nil or
                 Map.get(table_attributes, "bordercolor", "") != "",
               do: Map.put(hints, :border_styles, styles),
               else: hints

          tag in ~w(td th) ->
            styles =
              if is_number(border) and border > 0,
                do:
                  if(Map.get(table_attributes, "bordercolor", "") != "", do: :solid, else: :inset),
                else: :none

            styles = NativeElixirPdfUtilities.Validators.CssValueValidator.edges(styles)

            styles =
              case rules do
                "cols" ->
                  %{top: :none, right: :solid, bottom: :none, left: :solid}

                "all" ->
                  NativeElixirPdfUtilities.Validators.CssValueValidator.edges(:solid)

                "rows" ->
                  %{top: :solid, right: :none, bottom: :solid, left: :none}

                value when value in ~w(none groups) ->
                  NativeElixirPdfUtilities.Validators.CssValueValidator.edges(:none)

                _ ->
                  styles
              end

            if rules? or (is_number(border) and border > 0),
              do:
                hints
                |> Map.put(
                  :border_widths,
                  NativeElixirPdfUtilities.Validators.CssValueValidator.edges(0.75)
                )
                |> Map.put(:border_styles, styles),
              else: hints

          tag in ~w(thead tbody tfoot) and rules == "groups" ->
            hints
            |> Map.put(
              :border_widths,
              NativeElixirPdfUtilities.Validators.CssValueValidator.edges(0.75)
            )
            |> Map.put(:border_styles, %{top: :solid, right: :none, bottom: :solid, left: :none})

          tag == "colgroup" and rules == "groups" ->
            hints
            |> Map.put(
              :border_widths,
              NativeElixirPdfUtilities.Validators.CssValueValidator.edges(0.75)
            )
            |> Map.put(:border_styles, %{top: :none, right: :solid, bottom: :none, left: :solid})

          true ->
            hints
        end

      case HtmlValidator.parse_html_legacy_color(Map.get(attributes, "bordercolor")) do
        {:ok, color} when tag in ~w(table thead tbody tfoot tr td th) ->
          {:ok,
           hints
           |> Map.put(:border_color, color)
           |> Map.put(
             :border_colors,
             NativeElixirPdfUtilities.Validators.CssValueValidator.edges(color)
           )}

        _ ->
          {:ok, hints}
      end
    end
  end

  @doc "Validates effective table presentation after authored CSS has overridden HTML hints."
  @spec validate_presentation_style(map(), map()) :: :ok | {:error, Diagnostics.detailed_error()}
  @spec validate_presentation_style(map(), map(), :style | :layout) ::
          :ok | {:error, Diagnostics.detailed_error()}
  def validate_presentation_style(node, style, stage \\ :style) do
    case style do
      %{display: :table_cell, vertical_align: :baseline} ->
        if Map.get(node, :namespace, :html) == :html do
          opts =
            Keyword.merge(source_context(node),
              operation: if(stage == :layout, do: :layout, else: :compute_style),
              module: __MODULE__,
              source: Map.get(node, :tag, "table cell") <> " effective vertical-align=baseline"
            )

          Diagnostics.error(
            stage,
            :unsupported_html,
            "table-cell baseline alignment requires row baseline geometry, which is not implemented; use authored vertical-align:top, middle or bottom",
            opts
          )
        else
          :ok
        end

      %{text_align: :justify} ->
        tag = Map.get(node, :tag, "table cell")

        if Map.get(node, :namespace, :html) == :html and tag in ~w(thead tbody tfoot tr td th) and
             Map.get(style, :display) not in [:none, :table_row, :table_row_group] do
          attrs =
            case Map.get(node, :attributes) do
              attributes when is_map(attributes) -> attributes
              _ -> %{}
            end

          source =
            if Map.get(attrs, "align") == nil,
              do: tag <> " inherited text-align=justify",
              else: tag <> " align=justify"

          opts =
            Keyword.merge(source_context(node),
              operation: if(stage == :layout, do: :layout, else: :compute_style),
              module: __MODULE__,
              source: source
            )

          Diagnostics.error(
            stage,
            :unsupported_html,
            "table text alignment requires text justification, which is not implemented; use authored left, center or right alignment",
            opts
          )
        else
          :ok
        end

      _ ->
        :ok
    end
  end

  # HTML pixel hints share integer grammar and the central CSS magnitude limit.
  defp legacy_pixel_attribute(attributes, name, default) do
    case Map.fetch(attributes, name) do
      {:ok, value} ->
        case HtmlValidator.parse_html_integer(value, :nonnegative) do
          {:ok, integer} ->
            case HtmlValidator.parse_css_number(Integer.to_string(integer)) do
              {number, ""} ->
                {:ok, number * 0.75}

              :error ->
                Diagnostics.error(
                  :style,
                  :resource_limit_exceeded,
                  "table #{name} exceeds max_css_numeric_magnitude; reduce the pixel value",
                  operation: :compute_style,
                  module: __MODULE__,
                  source: name <> "=" <> value
                )
            end

          :error ->
            {:ok, default}
        end

      :error ->
        {:ok, nil}
    end
  end

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
        [
          source:
            "#{Map.get(node, :tag, "table cell")} #{inspect(Map.get(node, :_document_order, []))}"
        ]
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
  @spec form(term(), map()) ::
          {:ok,
           %{
             rows: [map()],
             columns: [map()],
             column_presentation: [map()],
             collapsed_edges: map()
           }}
          | {:error, Diagnostics.detailed_error()}
  def form(children, table_style \\ %{}) do
    HtmlValidator.with_render_budget(fn ->
      case {children, table_style} do
        {children, table_style} when is_list(children) and is_map(table_style) ->
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

                    nodes
                    |> Enum.with_index()
                    |> Enum.reduce_while({:ok, []}, fn {column, column_index}, {:ok, prepared} ->
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
                              width: Map.get(column_style, :width) || Map.get(style, :width),
                              presentation: %{
                                style:
                                  if(column == child,
                                    do: %{},
                                    else:
                                      Map.take(column_style, [
                                        :border_widths,
                                        :border_styles,
                                        :border_colors,
                                        :border_color
                                      ])
                                  ),
                                group_style:
                                  Map.take(style, [
                                    :border_widths,
                                    :border_styles,
                                    :border_colors,
                                    :border_color
                                  ]),
                                group: index,
                                column: {index, column_index}
                              }
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
            columns = Enum.reverse(columns)

            with :ok <- validate_border_grid_support(rows, columns, table_style) do
              {:ok,
               %{
                 rows: rows,
                 columns: Enum.map(columns, &Map.take(&1, [:width])),
                 column_presentation: Enum.map(columns, & &1.presentation),
                 collapsed_edges:
                   if(Map.get(table_style, :border_collapse) == :collapse,
                     do:
                       collapsed_table_edges(
                         rows,
                         Enum.map(columns, & &1.presentation),
                         table_style
                       ),
                     else: %{}
                   )
               }}
            end
          end

        _ ->
          invalid_table(%{}, "table formation requires a list of styled children")
      end
    end)
  end

  # Table-part borders need a complete authored or span-covered visible grid.
  # Empty slot formation remains a separate, unsupported formatting prerequisite.
  defp validate_border_grid_support(rows, columns, table_style) do
    if Map.get(table_style, :border_collapse) == :collapse do
      count = max(Enum.reduce(rows, 0, &max(&1.consumed_columns, &2)), length(columns))

      case Enum.find(rows, &(&1.consumed_columns < count)) do
        nil ->
          :ok

        incomplete ->
          styles =
            Enum.flat_map(rows, &[&1.row.style, &1.group_style || %{}]) ++
              Enum.flat_map(columns, &[&1.presentation.style, &1.presentation.group_style])

          wide_borders? =
            Enum.any?(styles, fn style ->
              widths =
                Map.get(
                  style,
                  :border_widths,
                  NativeElixirPdfUtilities.Validators.CssValueValidator.edges(0.0)
                )

              styles =
                Map.get(
                  style,
                  :border_styles,
                  NativeElixirPdfUtilities.Validators.CssValueValidator.edges(:none)
                )

              Enum.any?(widths, fn {side, width} ->
                width > 0 and Map.fetch!(styles, side) not in [:none, :hidden]
              end)
            end)

          if wide_borders? do
            Diagnostics.error(
              :layout,
              :unsupported_html,
              "row, row-group, column or column-group borders over uncovered table slots require anonymous cell formation, which is not implemented; add authored cells covering the missing slots or remove table-part borders",
              Keyword.merge(source_context(incomplete.row),
                operation: :layout,
                module: __MODULE__,
                source: "tr uncovered table slots"
              )
            )
          else
            :ok
          end
      end
    else
      :ok
    end
  end

  # CSS2 border conflict order is a fixed rendering rule, not a resource limit.
  defp collapsed_table_edges(rows, columns, table_style) do
    no_edges? = fn style ->
      case Map.get(style, :border_styles) do
        nil -> true
        %{top: :none, right: :none, bottom: :none, left: :none} -> true
        _ -> false
      end
    end

    empty? =
      no_edges?.(table_style) and
        Enum.all?(rows, fn row ->
          no_edges?.(row.row.style) and no_edges?.(row.group_style || %{}) and
            Enum.all?(row.cells, &no_edges?.(&1.cell.style))
        end) and
        Enum.all?(columns, &(no_edges?.(&1.style) and no_edges?.(&1.group_style)))

    if empty? do
      %{}
    else
      row_count = length(rows)
      column_count = max(Enum.reduce(rows, 0, &max(&1.consumed_columns, &2)), length(columns))

      cells =
        Enum.with_index(rows)
        |> Enum.reduce(%{}, fn {row, row_index}, edges ->
          Enum.reduce(row.cells, edges, fn cell, acc ->
            horizontal = cell.column..(cell.column + cell.colspan - 1)
            vertical = row_index..(row_index + cell.rowspan - 1)

            keys =
              Enum.flat_map(
                horizontal,
                &[
                  {{:horizontal, row_index, &1}, :top},
                  {{:horizontal, row_index + cell.rowspan, &1}, :bottom}
                ]
              ) ++
                Enum.flat_map(
                  vertical,
                  &[
                    {{:vertical, &1, cell.column}, :left},
                    {{:vertical, &1, cell.column + cell.colspan}, :right}
                  ]
                )

            put_collapsed_table_edges(acc, keys, cell.cell.style, 5, row_index, cell.column, true)
          end)
        end)

      if row_count == 0 or column_count == 0 do
        cells
      else
        row_entries = Enum.with_index(rows)

        edges =
          Enum.reduce(row_entries, cells, fn {row, index}, acc ->
            keys =
              Enum.flat_map(
                0..(column_count - 1),
                &[{{:horizontal, index, &1}, :top}, {{:horizontal, index + 1, &1}, :bottom}]
              ) ++
                [{{:vertical, index, 0}, :left}, {{:vertical, index, column_count}, :right}]

            put_collapsed_table_edges(acc, keys, row.row.style, 4, index, 0, false)
          end)

        edges =
          Enum.group_by(row_entries, fn {row, _index} -> row.row_group end)
          |> Enum.reduce(edges, fn {_id, entries}, acc ->
            {first, top} = hd(entries)
            {_last, bottom} = List.last(entries)

            keys =
              Enum.flat_map(
                0..(column_count - 1),
                &[{{:horizontal, top, &1}, :top}, {{:horizontal, bottom + 1, &1}, :bottom}]
              ) ++
                Enum.flat_map(
                  top..bottom,
                  &[{{:vertical, &1, 0}, :left}, {{:vertical, &1, column_count}, :right}]
                )

            put_collapsed_table_edges(acc, keys, first.group_style || %{}, 3, top, 0, false)
          end)

        column_entries = Enum.with_index(columns)

        edges =
          Enum.group_by(column_entries, fn {column, _index} -> column.column end)
          |> Enum.reduce(edges, fn {_id, entries}, acc ->
            {first, left} = hd(entries)
            {_last, right} = List.last(entries)

            keys =
              Enum.flat_map(
                0..(row_count - 1),
                &[{{:vertical, &1, left}, :left}, {{:vertical, &1, right + 1}, :right}]
              ) ++
                Enum.flat_map(
                  left..right,
                  &[{{:horizontal, 0, &1}, :top}, {{:horizontal, row_count, &1}, :bottom}]
                )

            put_collapsed_table_edges(acc, keys, first.style, 2, 0, left, false)
          end)

        edges =
          Enum.group_by(column_entries, fn {column, _index} -> column.group end)
          |> Enum.reduce(edges, fn {_id, entries}, acc ->
            {first, left} = hd(entries)
            {_last, right} = List.last(entries)

            keys =
              Enum.flat_map(
                0..(row_count - 1),
                &[{{:vertical, &1, left}, :left}, {{:vertical, &1, right + 1}, :right}]
              ) ++
                Enum.flat_map(
                  left..right,
                  &[{{:horizontal, 0, &1}, :top}, {{:horizontal, row_count, &1}, :bottom}]
                )

            put_collapsed_table_edges(acc, keys, first.group_style, 1, 0, left, false)
          end)

        keys =
          Enum.flat_map(
            0..(column_count - 1),
            &[{{:horizontal, 0, &1}, :top}, {{:horizontal, row_count, &1}, :bottom}]
          ) ++
            Enum.flat_map(
              0..(row_count - 1),
              &[{{:vertical, &1, 0}, :left}, {{:vertical, &1, column_count}, :right}]
            )

        put_collapsed_table_edges(edges, keys, table_style, 0, 0, 0, false)
      end
    end
  end

  defp put_collapsed_table_edges(edges, keys, style, origin, row, column, cell?) do
    widths =
      Map.get(
        style,
        :border_widths,
        NativeElixirPdfUtilities.Validators.CssValueValidator.edges(0.0)
      )

    styles =
      Map.get(
        style,
        :border_styles,
        NativeElixirPdfUtilities.Validators.CssValueValidator.edges(:none)
      )

    colors =
      Map.get(
        style,
        :border_colors,
        NativeElixirPdfUtilities.Validators.CssValueValidator.edges(
          Map.get(style, :border_color, Map.get(style, :color, {0, 0, 0}))
        )
      )

    HtmlValidator.reserve_render_resource(:max_table_grid_work, length(keys), :layout)

    Enum.reduce(keys, edges, fn {key, side}, acc ->
      # Row/column borders do not cut through the interior of a spanning cell.
      if cell? or Map.has_key?(acc, key) do
        border_style = Map.fetch!(styles, side)
        width = if border_style in [:none, :hidden], do: 0.0, else: Map.fetch!(widths, side)

        style_order =
          Enum.find_index(
            [:none, :inset, :groove, :outset, :ridge, :dotted, :dashed, :solid, :double, :hidden],
            &(&1 == border_style)
          ) || 0

        rank = {border_style == :hidden, width, style_order, origin, -row, -column}

        candidate = %{
          width: width,
          style: border_style,
          color: Map.fetch!(colors, side),
          rank: rank,
          owner: if(cell?, do: {row, column, side}, else: nil)
        }

        Map.update(acc, key, candidate, fn current ->
          winner = if rank > current.rank, do: candidate, else: current
          # Equal-width shared edges use the following cell's top/left paint
          # geometry; stronger-width cell edges retain their own geometry.
          owner =
            cond do
              cell? and candidate.width == current.width and side in [:top, :left] ->
                candidate.owner

              cell? and rank > current.rank ->
                candidate.owner

              true ->
                current.owner || candidate.owner
            end

          Map.put(winner, :owner, owner)
        end)
      else
        acc
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
