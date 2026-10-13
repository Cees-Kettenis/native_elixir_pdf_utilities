defmodule NativeElixirPdfUtilities.Validators.HtmlTableValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Limits
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

  @doc "Runs validated layout in its actual paginated or unfragmented child-surface context, restoring the parent afterward."
  @spec with_layout_context(:paginated | :surface, (-> term())) :: term()
  def with_layout_context(context, callback) do
    case {context, callback} do
      {context, callback} when context in [:paginated, :surface] and is_function(callback, 0) ->
        key = {__MODULE__, :layout_context}
        previous = Process.get(key)
        Process.put(key, context)

        try do
          callback.()
        after
          if previous == nil, do: Process.delete(key), else: Process.put(key, previous)
        end

      _ ->
        Diagnostics.error(
          :layout,
          :invalid_layout,
          "table layout context requires paginated PDF flow or an unfragmented child surface and a layout callback",
          module: __MODULE__,
          operation: :prepare_baseline_geometry
        )
    end
  end

  @doc "Issues the actual table baseline alignment policy for the current validated layout context and atomic ancestry."
  @spec baseline_policy(term()) ::
          :aligned | :fragmented_start | {:error, Diagnostics.detailed_error()}
  def baseline_policy(atomic_context) do
    case atomic_context do
      true ->
        :aligned

      false ->
        if Process.get({__MODULE__, :layout_context}, :paginated) == :paginated,
          do: :fragmented_start,
          else: :aligned

      _ ->
        Diagnostics.error(
          :layout,
          :invalid_layout,
          "table baseline policy requires validated boolean atomic ancestry",
          module: __MODULE__,
          operation: :prepare_baseline_geometry
        )
    end
  end

  @doc "Prepares the inherited atomic table context before validating effective presentation."
  @spec prepare_presentation_style(map(), map()) ::
          {:ok, map()} | {:error, Diagnostics.detailed_error()}
  def prepare_presentation_style(node, style) do
    style =
      if Map.get(style, :display) == :inline_block,
        do: Map.put(style, :_table_atomic_context, true),
        else: style

    with :ok <- validate_presentation_style(node, style), do: {:ok, style}
  end

  @doc "Validates effective table presentation after authored CSS has overridden HTML hints."
  @spec validate_presentation_style(map(), map()) :: :ok | {:error, Diagnostics.detailed_error()}
  @spec validate_presentation_style(map(), map(), :style | :layout) ::
          :ok | {:error, Diagnostics.detailed_error()}
  @spec validate_presentation_style(map(), map(), :style | :layout, boolean() | nil) ::
          :ok | {:error, Diagnostics.detailed_error()}
  def validate_presentation_style(node, style, stage \\ :style, _atomic_context \\ nil) do
    case style do
      %{display: :table_cell, vertical_align: :baseline} ->
        if stage == :layout and Map.get(node, :namespace, :html) == :html,
          do: validate_baseline_cell(node),
          else: :ok

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

  @doc "Validates measured atomic table line allocations against the printable page height."
  @spec validate_atomic_pagination([map()], [map()], number()) ::
          :ok | {:error, Diagnostics.detailed_error()}
  def validate_atomic_pagination(boxes, markers, printable_height) do
    HtmlValidator.with_render_budget(fn ->
      case {boxes, markers, printable_height} do
        {boxes, [], height} when is_list(boxes) and is_number(height) and height > 0 ->
          :ok

        {boxes, markers, height}
        when is_list(boxes) and is_list(markers) and is_number(height) and height > 0 ->
          with {:ok, box_count} <- table_collection_size(boxes, 0, :pagination),
               {:ok, marker_count} <- table_collection_size(markers, 0, :pagination) do
            HtmlValidator.reserve_render_resource(
              :max_table_grid_work,
              4 * box_count + 6 * marker_count,
              :pagination
            )

            valid_markers? =
              Enum.all?(boxes, &is_map/1) and
                Enum.all?(markers, fn marker ->
                  case marker do
                    %{
                      type: :flow_marker,
                      role: :atomic_table_line,
                      flow_id: id
                    } ->
                      fragment? =
                        case Map.get(marker, :fragment_lines, false) do
                          true ->
                            case Map.get(marker, :fragment_id) do
                              {:line, ^id, index} -> is_integer(index) and index >= 0
                              _ -> false
                            end

                          false ->
                            not Map.has_key?(marker, :fragment_id)

                          _ ->
                            false
                        end

                      id != nil and fragment? and Map.get(marker, :out_of_flow, false) != true

                    _ ->
                      false
                  end
                end)

            if valid_markers? do
              keys = MapSet.new(markers, &{&1.flow_id, Map.get(&1, :fragment_id)})

              in_flow =
                Enum.filter(boxes, fn box ->
                  Map.get(box, :out_of_flow, false) != true and
                    MapSet.member?(keys, {Map.get(box, :flow_id), Map.get(box, :fragment_id)})
                end)

              with :ok <-
                     NativeElixirPdfUtilities.Validators.WriterValidator.validate_native_boxes(
                       markers
                     ),
                   :ok <-
                     NativeElixirPdfUtilities.Validators.WriterValidator.validate_native_boxes(
                       in_flow
                     ) do
                bounds =
                  Enum.reduce(markers ++ in_flow, %{}, fn box, bounds ->
                    key = {box.flow_id, Map.get(box, :fragment_id)}

                    {top, bottom} =
                      NativeElixirPdfUtilities.HtmlToPdf.PageGeometry.box_vertical_bounds(box)

                    Map.update(bounds, key, {top, bottom}, fn {previous_top, previous_bottom} ->
                      {max(top, previous_top), min(bottom, previous_bottom)}
                    end)
                  end)

                oversized =
                  Enum.find(markers, fn marker ->
                    {top, bottom} =
                      Map.fetch!(bounds, {marker.flow_id, Map.get(marker, :fragment_id)})

                    top - bottom > height
                  end)

                if oversized == nil do
                  :ok
                else
                  Diagnostics.error(
                    :pagination,
                    :unsupported_html,
                    "atomic table line allocation exceeds the printable page height; continuation of oversized atomic tables is not implemented; reduce the table allocation or use a block table with authored top, middle or bottom cell alignment",
                    operation: :paginate,
                    module: __MODULE__,
                    source:
                      "atomic table flow " <>
                        inspect(oversized.flow_id) <>
                        " fragment " <> inspect(Map.get(oversized, :fragment_id))
                  )
                end
              else
                {:error, {reason, diagnostic}} ->
                  Diagnostics.error(
                    :pagination,
                    if(reason == :resource_limit_exceeded, do: reason, else: :invalid_layout),
                    "atomic table line geometry is invalid; " <> diagnostic.message,
                    operation: :paginate,
                    module: __MODULE__,
                    source: "atomic table line geometry"
                  )
              end
            else
              Diagnostics.error(
                :pagination,
                :invalid_layout,
                "atomic table pagination requires canonical box maps, its issued zero-width flow marker and outer line fragment identity",
                operation: :paginate,
                module: __MODULE__,
                source: "atomic table line metadata"
              )
            end
          else
            _ ->
              Diagnostics.error(
                :pagination,
                :invalid_layout,
                "atomic table geometry requires proper box and marker lists",
                operation: :paginate,
                module: __MODULE__,
                source: "atomic table line geometry"
              )
          end

        _ ->
          Diagnostics.error(
            :pagination,
            :invalid_layout,
            "atomic table pagination requires canonical box and marker lists with a positive printable height",
            operation: :paginate,
            module: __MODULE__,
            source: "atomic table pagination"
          )
      end
    end)
  end

  @doc "Prepares atomic table flow intent from the fully styled in-flow descendants."
  @spec prepare_atomic_flow_style(map(), [map()]) :: map()
  @spec prepare_atomic_flow_style(map(), [map()], :style | :layout) :: map()
  def prepare_atomic_flow_style(style, children, stage \\ :style) do
    if Map.get(style, :display) == :inline_block and atomic_flow_table?(children, stage),
      do: Map.put(style, :_atomic_table_flow, true),
      else: Map.delete(style, :_atomic_table_flow)
  end

  defp atomic_flow_table?(children, stage) do
    Enum.any?(children, fn child ->
      HtmlValidator.reserve_render_resource(:max_table_grid_work, 1, stage)

      case child do
        %{style: %{display: :none}} -> false
        %{style: %{position: :absolute}} -> false
        %{style: %{display: :table}} -> true
        %{children: children} -> atomic_flow_table?(children, stage)
        _ -> false
      end
    end)
  end

  @doc "Validates issued table context against the actual prepared tree ancestry."
  @spec validate_atomic_context(map(), boolean()) :: :ok | {:error, Diagnostics.detailed_error()}
  def validate_atomic_context(node, atomic_context) do
    flow =
      case node do
        %{type: :element, style: %{display: :inline_block} = style, children: children} ->
          expected = prepare_atomic_flow_style(style, children, :layout)

          if Map.get(style, :_atomic_table_flow) == Map.get(expected, :_atomic_table_flow),
            do: :ok,
            else:
              invalid_table(
                node,
                "atomic table flow intent must match the in-flow table descendants; recompute styles for this tree"
              )

        %{style: %{_atomic_table_flow: _}} ->
          invalid_table(
            node,
            "atomic table flow intent requires an actual inline-block owner; recompute styles for this tree"
          )

        _ ->
          :ok
      end

    with :ok <- flow do
      case node do
        %{type: :element, style: %{_table_atomic_context: true} = style} ->
          if atomic_context or Map.get(style, :display) == :inline_block do
            :ok
          else
            invalid_table(
              node,
              "atomic table context requires an actual inline-block ancestor; recompute styles for this tree"
            )
          end

        _ ->
          :ok
      end
    end
  end

  @doc "Prepares bounded measured row bands for canonical spanning-cell placements."
  @spec prepare_row_band_geometry(term(), term(), term()) ::
          {:ok, map()} | {:error, Diagnostics.detailed_error()}
  def prepare_row_band_geometry(rows, heights, spacing) do
    HtmlValidator.with_render_budget(fn ->
      {valid_rows, row_count, placements} = prepare_table_geometry_rows(rows)
      proper_heights = table_collection_size(heights)

      valid =
        valid_rows and proper_heights == {:ok, row_count} and is_number(spacing) and
          spacing >= 0 and spacing <= Limits.get(:max_css_numeric_magnitude) and
          Enum.all?(heights, fn height ->
            is_number(height) and height >= 0 and height <= Limits.get(:max_css_numeric_magnitude)
          end)

      if valid do
        HtmlValidator.reserve_render_resource(:max_table_grid_work, 8 * row_count, :layout)

        {prefixes, _total} =
          Enum.map_reduce(heights, 0.0, fn height, offset ->
            {offset, offset + height + spacing}
          end)

        indexed =
          Enum.zip(prefixes, heights)
          |> Enum.with_index()
          |> Map.new(fn {geometry, index} -> {index, geometry} end)

        Enum.each(
          prefixes,
          &HtmlValidator.check_render_resource(:max_pdf_numeric_magnitude, abs(&1), :layout)
        )

        plans =
          Enum.reduce(placements, %{}, fn {{start, _column} = key, placement}, plans ->
            if placement.rowspan > 1 do
              HtmlValidator.reserve_render_resource(
                :max_table_grid_work,
                5 * placement.rowspan,
                :layout
              )

              {first, _height} = Map.fetch!(indexed, start)
              last = start + placement.rowspan - 1

              bands =
                Enum.map(start..last, fn row ->
                  {offset, height} = Map.fetch!(indexed, row)

                  %{
                    row: row,
                    offset: offset - first,
                    height: height + if(row == last, do: 0.0, else: spacing)
                  }
                end)

              Map.put(plans, key, bands)
            else
              plans
            end
          end)

        {:ok, plans}
      else
        Diagnostics.error(
          :layout,
          :invalid_layout,
          "table continuation requires canonical placements, one bounded measured height per row and nonnegative spacing; recompute table geometry",
          module: __MODULE__,
          operation: :prepare_row_band_geometry
        )
      end
    end)
  end

  @doc "Prepares row and row-group background positioning boxes and cell-owned coverage for a measured table grid."
  @spec prepare_cell_background_geometry(term(), term(), term(), term()) ::
          {:ok, map()} | {:error, Diagnostics.detailed_error()}
  def prepare_cell_background_geometry(rows, heights, widths, spacing) do
    HtmlValidator.with_render_budget(fn ->
      {valid_rows, row_count, placements} = prepare_table_geometry_rows(rows)
      height_count = table_collection_size(heights)
      width_count = table_collection_size(widths)

      valid =
        valid_rows and height_count == {:ok, row_count} and match?({:ok, _}, width_count) and
          match?(
            {horizontal, vertical} when is_number(horizontal) and is_number(vertical),
            spacing
          ) and
          Enum.all?(Tuple.to_list(spacing) ++ heights ++ widths, fn value ->
            is_number(value) and value >= 0 and value <= Limits.get(:max_css_numeric_magnitude)
          end)

      valid =
        valid and
          Enum.all?(rows, fn row ->
            Map.has_key?(row, :row_group) and Map.has_key?(row, :group_style) and
              (Map.get(row, :group_style) == nil or is_map(row.group_style))
          end) and
          Enum.all?(placements, fn {{_row, column}, placement} ->
            case placement do
              %{colspan: span} when is_integer(span) and span >= 1 ->
                column + span <= elem(width_count, 1)

              _ ->
                false
            end
          end)

      if valid do
        {horizontal, vertical} = spacing
        columns = elem(width_count, 1)

        HtmlValidator.reserve_render_resource(
          :max_table_grid_work,
          10 * row_count + 4 * columns + 12 * length(placements),
          :layout
        )

        {row_starts, row_end} = Enum.map_reduce(heights, 0.0, &{&2, &2 + &1 + vertical})

        {column_starts, column_end} =
          Enum.map_reduce(widths, horizontal, &{&2, &2 + &1 + horizontal})

        row_positions =
          Map.new(Enum.with_index(row_starts ++ [row_end]), fn {value, index} ->
            {index, value}
          end)

        column_positions =
          Map.new(Enum.with_index(column_starts ++ [column_end]), fn {value, index} ->
            {index, value}
          end)

        indexed_rows = Map.new(Enum.with_index(rows), fn {row, index} -> {index, row} end)

        indexed_heights =
          Map.new(Enum.with_index(heights), fn {height, index} -> {index, height} end)

        groups =
          Enum.reduce(Enum.with_index(rows), %{}, fn {row, index}, groups ->
            case Map.fetch(groups, row.row_group) do
              :error ->
                Map.put(groups, row.row_group, {index, index + 1})

              {:ok, {first, last}} ->
                if last == index and
                     Map.fetch!(indexed_rows, first).group_style == row.group_style do
                  Map.put(groups, row.row_group, {first, index + 1})
                else
                  throw(
                    {:render_resource_limit,
                     Diagnostics.error(
                       :layout,
                       :invalid_layout,
                       "table row groups require contiguous canonical rows with one prepared group style; recompute table geometry",
                       module: __MODULE__,
                       operation: :prepare_cell_background_geometry
                     )}
                  )
                end
            end
          end)

        Enum.each(row_starts ++ column_starts ++ [row_end, column_end], fn value ->
          HtmlValidator.check_render_resource(:max_pdf_numeric_magnitude, value, :layout)
        end)

        geometry =
          Map.new(placements, fn {{index, column} = key, placement} ->
            row = Map.fetch!(indexed_rows, index)
            top = Map.fetch!(row_positions, index)
            height = Map.fetch!(row_positions, index + placement.rowspan) - top - vertical
            left = Map.fetch!(column_positions, column)
            width = Map.fetch!(column_positions, column + placement.colspan) - left - horizontal
            coverage = %{x: left, y: -top - height, width: width, height: height}
            {group_start, group_end} = Map.fetch!(groups, row.row_group)
            group_top = Map.fetch!(row_positions, group_start)
            group_height = Map.fetch!(row_positions, group_end) - group_top - vertical
            row_width = column_end

            layers = [
              %{
                style: row.group_style,
                role: :table_row_group_background,
                position: %{
                  x: 0.0,
                  y: -group_top - group_height,
                  width: row_width,
                  height: group_height
                },
                coverage: coverage
              },
              %{
                style: row.row.style,
                role: :table_row_background,
                position: %{
                  x: 0.0,
                  y: -top - Map.fetch!(indexed_heights, index),
                  width: row_width,
                  height: Map.fetch!(indexed_heights, index)
                },
                coverage: coverage
              }
            ]

            {key, layers}
          end)

        {:ok, geometry}
      else
        Diagnostics.error(
          :layout,
          :invalid_layout,
          "table backgrounds require canonical cell spans, row-group identities, one measured height per row, bounded column widths and nonnegative spacing; recompute table geometry",
          module: __MODULE__,
          operation: :prepare_cell_background_geometry
        )
      end
    end)
  end

  @doc "Prepares row-band clips from actual native artwork, preserving horizontal ink overflow and reserving fragment copies."
  @spec prepare_cell_band_paint(term(), term(), term()) ::
          {:ok, [map()]} | {:error, Diagnostics.detailed_error()}
  def prepare_cell_band_paint(boxes, bands, top) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, _count} <- table_collection_size(bands),
           true <- is_number(top) and abs(top) <= Limits.get(:max_pdf_numeric_magnitude),
           true <-
             Enum.all?(bands, fn band ->
               case band do
                 %{row: row, offset: offset, height: height} ->
                   is_integer(row) and row >= 0 and is_number(offset) and offset >= 0 and
                     is_number(height) and height >= 0 and
                     Enum.all?(
                       [offset, height, top - offset - height],
                       &(abs(&1) <= Limits.get(:max_pdf_numeric_magnitude))
                     )

                 _ ->
                   false
               end
             end),
           true <-
             Enum.reduce_while(bands, nil, fn band, previous ->
               if (previous == nil and band.offset == 0) or
                    (previous != nil and band.row == previous.row + 1 and
                       Float.round((band.offset - previous.offset - previous.height) * 1.0, 4) ==
                         0.0),
                  do: {:cont, band},
                  else: {:halt, false}
             end) != false,
           {:ok, bounds, count} <- table_paint_ink_bounds(boxes) do
        HtmlValidator.reserve_render_resource(
          :max_table_grid_work,
          (2 * count + 3) * length(bands),
          :layout
        )

        HtmlValidator.reserve_render_resource(
          :max_layout_boxes,
          (2 * count + 3) * length(bands),
          :layout
        )

        case bounds do
          nil ->
            {:ok, []}

          {left, _bottom, right, _top} ->
            last = List.last(bands)
            total_height = if last == nil, do: 0.0, else: last.offset + last.height
            fragment_identity = make_ref()
            HtmlValidator.check_render_resource(:max_pdf_numeric_magnitude, right - left, :layout)

            {:ok,
             Enum.map(bands, fn band ->
               Map.put(band, :clip, %{
                 x: left,
                 y: top - band.offset - band.height,
                 width: right - left,
                 height: band.height
               })
               |> Map.put(:cell_geometry, %{
                 identity: fragment_identity,
                 total_height: total_height,
                 offset: band.offset
               })
             end)}
        end
      else
        false ->
          Diagnostics.error(
            :layout,
            :invalid_layout,
            "table cell bands require canonical row indices, bounded measured offsets and heights, and a PDF origin",
            module: __MODULE__,
            operation: :prepare_cell_band_paint
          )

        :error ->
          Diagnostics.error(
            :layout,
            :invalid_layout,
            "table cell bands require proper measured collections",
            module: __MODULE__,
            operation: :prepare_cell_band_paint
          )

        error ->
          error
      end
    end)
  end

  @doc "Consumes actual page capacity for spanning cell surfaces and returns their visible native fragments with continuing state."
  @spec prepare_cell_page_fragments(term(), term(), term()) ::
          {:ok, [map()], map()} | {:error, Diagnostics.detailed_error()}
  def prepare_cell_page_fragments(boxes, previous, bottom) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, count} <- table_collection_size(boxes, 0, :pagination),
           true <-
             is_map(previous) and is_number(bottom) and bottom >= 0 and
               bottom <= Limits.get(:max_pdf_numeric_magnitude) do
        HtmlValidator.reserve_render_resource(
          :max_table_grid_work,
          5 * count + map_size(previous),
          :pagination
        )

        HtmlValidator.reserve_render_resource(:max_layout_boxes, 2 * count, :pagination)

        valid_state =
          Enum.all?(previous, fn {identity, consumed} ->
            is_reference(identity) and is_number(consumed) and consumed >= 0 and
              consumed <= Limits.get(:max_pdf_numeric_magnitude)
          end)

        if not valid_state,
          do:
            throw(
              {:render_resource_limit,
               cell_fragment_failure(
                 "cell continuation state requires issued cell identities and bounded nonnegative consumed heights"
               )}
            )

        result =
          Enum.reduce_while(boxes, {:ok, [], previous, %{}}, fn box,
                                                                {:ok, result, next, allocations} ->
            case box do
              %{table_span_fragment: descriptor} when is_map(descriptor) ->
                with :ok <- validate_cell_fragment_descriptor(box),
                     :ok <-
                       NativeElixirPdfUtilities.Validators.HtmlLinkValidator.validate_boxes([box]),
                     {:ok, _bounds, descendants} <- table_paint_ink_bounds(box.boxes),
                     :ok <-
                       HtmlValidator.reserve_render_resource(
                         :max_table_grid_work,
                         descendants + 1,
                         :pagination
                       ),
                     :ok <-
                       NativeElixirPdfUtilities.Validators.WriterValidator.validate_native_boxes([
                         box
                       ]) do
                  if Map.get(box, :role) == :table_border do
                    {:cont, {:ok, [box | result], next, allocations}}
                  else
                    id = descriptor.identity
                    part = {id, descriptor.part}

                    geometry = {descriptor.source, descriptor.total_height}

                    if Map.get(allocations, id, geometry) != geometry,
                      do:
                        throw(
                          {:render_resource_limit,
                           cell_fragment_failure(
                             "one cell continuation identity must retain the same canonical source and measured total height"
                           )}
                        )

                    allocations = Map.put(allocations, id, geometry)

                    case Map.get(allocations, part) do
                      :painted ->
                        {:cont, {:ok, result, next, allocations}}

                      nil ->
                        consumed =
                          if Map.get(box, :table_section) == :head,
                            do: 0.0,
                            else: Map.get(previous, id, 0.0)

                        top = box.y + box.height

                        if is_number(consumed) and consumed >= 0 and
                             consumed <= descriptor.total_height and top >= bottom do
                          height = min(descriptor.total_height - consumed, top - bottom)
                          delta = height + consumed - box.height - descriptor.offset

                          HtmlValidator.reserve_render_resource(
                            :max_layout_boxes,
                            2 * descendants + 2,
                            :pagination
                          )

                          HtmlValidator.reserve_render_resource(
                            :max_table_grid_work,
                            4 * descendants + 2,
                            :pagination
                          )

                          children =
                            NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.translate_boxes(
                              box.boxes,
                              0.0,
                              delta
                            )

                          clip = %{x: 0.0, y: 0.0, width: box.width, height: height}

                          with {:ok, children} <-
                                 visible_cell_fragment_paint(
                                   children,
                                   {0.0, 0.0, box.width, height}
                                 ) do
                            {a, b, c, d, e, f} = box.transform

                            painted = %{
                              box
                              | y: top - height,
                                height: height,
                                clip: clip,
                                transform: {a, b, c, d, e, f + box.height - height},
                                boxes: children
                            }

                            painted =
                              case Map.fetch(box, :html_targets) do
                                :error ->
                                  painted

                                {:ok, targets} ->
                                  HtmlValidator.reserve_render_resource(
                                    :max_table_grid_work,
                                    2 * length(targets),
                                    :pagination
                                  )

                                  targets =
                                    Enum.map(targets, fn target ->
                                      offset = target.dy + height - box.height

                                      HtmlValidator.check_render_resource(
                                        :max_pdf_numeric_magnitude,
                                        abs(offset),
                                        :pagination
                                      )

                                      Map.put(target, :dy, offset)
                                    end)

                                  Map.put(painted, :html_targets, targets)
                              end

                            next = Map.put(next, id, consumed + height)
                            allocations = Map.put(allocations, part, :painted)
                            {:cont, {:ok, [painted | result], next, allocations}}
                          else
                            error -> {:halt, error}
                          end
                        else
                          {:halt,
                           cell_fragment_failure(
                             "cell continuation requires bounded consumed height and an actual printable page capacity"
                           )}
                        end
                    end
                  end
                else
                  error -> {:halt, error}
                end

              %{table_span_fragment: _} ->
                {:halt,
                 cell_fragment_failure(
                   "cell continuation requires validator-prepared source geometry"
                 )}

              box when is_map(box) ->
                {:cont, {:ok, [box | result], next, allocations}}

              _ ->
                {:halt, cell_fragment_failure("cell continuation requires native page boxes")}
            end
          end)

        case result do
          {:ok, result, next, _allocations} -> {:ok, Enum.reverse(result), next}
          error -> error
        end
      else
        _ ->
          cell_fragment_failure(
            "cell continuation requires proper page boxes, a consumed-height map and bounded page bottom"
          )
      end
    end)
  end

  defp validate_cell_fragment_descriptor(box) do
    case box do
      %{
        type: :drawing_group,
        table_id: table,
        x: x,
        y: y,
        transform: {a, b, c, d, e, f},
        table_span_fragment: %{
          identity: identity,
          source: {table, row, column},
          part: part,
          total_height: height,
          offset: offset
        }
      } ->
        if a == 1 and b == 0 and c == 0 and d == 1 and e == x and f == y and
             is_reference(identity) and is_integer(row) and row >= 0 and is_integer(column) and
             column >= 0 and
             is_integer(part) and part >= 0 and is_number(height) and height >= 0 and
             is_number(offset) and offset >= 0 and offset <= height and
             height <= Limits.get(:max_pdf_numeric_magnitude) do
          :ok
        else
          cell_fragment_failure(
            "cell continuation requires canonical table/row/column identity and bounded measured geometry"
          )
        end

      _ ->
        cell_fragment_failure(
          "cell continuation requires a native group with canonical table/row/column identity"
        )
    end
  end

  defp cell_fragment_failure(message) do
    Diagnostics.error(:pagination, :invalid_layout, message,
      module: __MODULE__,
      operation: :prepare_cell_page_fragments,
      source: "table_span_fragment"
    )
  end

  # A page carries only actual visible source artwork. In particular, whole
  # spanning text is not duplicated invisibly under every logical row clip.
  defp visible_cell_fragment_paint(boxes, clip) do
    Enum.reduce_while(boxes, {:ok, []}, fn box, {:ok, visible} ->
      with {:ok, bounds, _count} <- table_paint_ink_bounds([box]) do
        retained =
          cond do
            bounds == nil ->
              if Map.get(box, :type) == :flow_marker, do: [box], else: []

            true ->
              {left, bottom, right, top} =
                NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.intersect_clip(bounds, clip)

              cond do
                right > left and top > bottom ->
                  [box]

                Map.get(box, :html_targets, []) != [] ->
                  # Keep the actual selected origin under the same cell clip,
                  # without copying invisible text or inventing a source ID.
                  [
                    Map.take(box, [:x, :y, :html_targets])
                    |> Map.merge(%{type: :flow_marker, width: 0.0, height: 0.0})
                  ]

                true ->
                  []
              end
          end

        {:cont, {:ok, Enum.reverse(retained, visible)}}
      else
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, visible} -> {:ok, Enum.reverse(visible)}
      error -> error
    end
  end

  # Actual selected outlines preserve italic and atomic overflow without an
  # invented horizontal ceiling. Native groups keep their existing transforms.
  defp table_paint_ink_bounds(boxes, depth \\ 1) do
    HtmlValidator.check_render_resource(:max_drawing_group_depth, depth, :layout)

    with {:ok, count} <- table_collection_size(boxes) do
      HtmlValidator.reserve_render_resource(:max_table_grid_work, 3 * count, :layout)

      Enum.reduce_while(boxes, {:ok, nil, count}, fn box, {:ok, union, count} ->
        result =
          with :ok <- NativeElixirPdfUtilities.Validators.HtmlLinkValidator.validate_boxes([box]) do
            case box do
              %{type: type, boxes: children} when type in [:drawing_group, :glyph_group] ->
                # Glyph groups contain text only. Validate that shared writer
                # contract before recursing into an invalid nested collection.
                glyph_shape =
                  if type == :glyph_group do
                    with {:ok, count} <- table_collection_size(children),
                         :ok <-
                           HtmlValidator.reserve_render_resource(
                             :max_table_grid_work,
                             count,
                             :layout
                           ) do
                      NativeElixirPdfUtilities.Validators.WriterValidator.validate_native_boxes([
                        box
                      ])
                    else
                      :error ->
                        Diagnostics.error(
                          :layout,
                          :invalid_layout,
                          "table glyph artwork requires a proper native text collection",
                          module: __MODULE__,
                          operation: :prepare_cell_band_paint
                        )

                      error ->
                        error
                    end
                  else
                    :ok
                  end

                with :ok <- glyph_shape,
                     {:ok, bounds, descendants} <-
                       table_paint_ink_bounds(
                         children,
                         if(type == :drawing_group, do: depth + 1, else: depth)
                       ),
                     :ok <-
                       HtmlValidator.reserve_render_resource(
                         :max_table_grid_work,
                         descendants + 1,
                         :layout
                       ),
                     :ok <-
                       NativeElixirPdfUtilities.Validators.WriterValidator.validate_native_boxes([
                         box
                       ]) do
                  bounds =
                    if type == :drawing_group and bounds != nil do
                      bounds =
                        NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.intersect_clip(
                          bounds,
                          NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.box_clip_bounds(
                            Map.get(box, :clip)
                          )
                        )

                      {left, bottom, right, top} = bounds
                      {a, _b, _c, d, e, f} = box.transform

                      if right > left and top > bottom,
                        do: {a * left + e, d * bottom + f, a * right + e, d * top + f},
                        else: nil
                    else
                      bounds
                    end

                  {:ok, bounds, descendants}
                end

              _ ->
                with :ok <-
                       NativeElixirPdfUtilities.Validators.WriterValidator.validate_native_boxes([
                         box
                       ]) do
                  case box do
                    %{type: :text} ->
                      with {:ok, bounds} <-
                             NativeElixirPdfUtilities.Validators.FontValidator.prepare_text_ink_rectangle(
                               box
                             ),
                           do: {:ok, bounds, 0}

                    %{x: x, y: y, width: width, height: height} ->
                      stroke = Map.get(box, :stroke_width, 0.0) / 2
                      bounds = {x - stroke, y - stroke, x + width + stroke, y + height + stroke}

                      Enum.each(Tuple.to_list(bounds), fn value ->
                        HtmlValidator.check_render_resource(
                          :max_pdf_numeric_magnitude,
                          abs(value),
                          :layout
                        )
                      end)

                      if Map.get(box, :type) == :flow_marker,
                        do: {:ok, nil, 0},
                        else: {:ok, bounds, 0}

                    _ ->
                      Diagnostics.error(
                        :layout,
                        :invalid_layout,
                        "table cell paint requires actual bounded native artwork or selected target markers",
                        module: __MODULE__,
                        operation: :prepare_cell_band_paint
                      )
                  end
                end
            end
          end

        case result do
          {:ok, nil, descendants} ->
            {:cont, {:ok, union, count + descendants}}

          {:ok, bounds, descendants} ->
            {left, bottom, right, top} = bounds

            union =
              case union do
                nil -> bounds
                {a, b, c, d} -> {min(a, left), min(b, bottom), max(c, right), max(d, top)}
              end

            {:cont, {:ok, union, count + descendants}}

          error ->
            {:halt, error}
        end
      end)
    else
      :error ->
        Diagnostics.error(
          :layout,
          :invalid_layout,
          "table cell artwork requires proper native paint collections",
          module: __MODULE__,
          operation: :prepare_cell_band_paint
        )

      error ->
        error
    end
  end

  @doc "Validates measured table cells and prepares baseline row heights and canonical offsets."
  @spec prepare_baseline_geometry(term(), term(), term()) ::
          {:ok, map()} | {:error, Diagnostics.detailed_error()}
  @spec prepare_baseline_geometry(term(), term(), term(), :aligned | :fragmented_start) ::
          {:ok, map()} | {:error, Diagnostics.detailed_error()}
  def prepare_baseline_geometry(rows, metrics, vertical_spacing, policy \\ :aligned) do
    HtmlValidator.with_render_budget(fn ->
      {valid_rows, row_count, placements} = prepare_table_geometry_rows(rows)

      valid =
        policy in [:aligned, :fragmented_start] and valid_rows and is_map(metrics) and
          is_number(vertical_spacing) and
          vertical_spacing >= 0 and
          map_size(metrics) == length(placements) and
          Enum.all?(placements, fn {key, _placement} ->
            case Map.get(metrics, key) do
              %{
                intrinsic_height: intrinsic,
                minimum_height: minimum,
                baseline: baseline,
                inset_bottom: inset_bottom,
                has_flow: has_flow
              }
              when is_number(intrinsic) and intrinsic >= 0 and is_number(minimum) and
                     minimum >= intrinsic and
                     is_number(baseline) and is_number(inset_bottom) and inset_bottom >= 0 and
                     is_boolean(has_flow) ->
                Enum.each(
                  [intrinsic, minimum, baseline, inset_bottom],
                  &HtmlValidator.check_render_resource(
                    :max_css_numeric_magnitude,
                    abs(&1),
                    :layout
                  )
                )

                true

              _ ->
                false
            end
          end)

      if valid do
        with :ok <- validate_baseline_dependencies(placements) do
          HtmlValidator.check_render_resource(
            :max_css_numeric_magnitude,
            vertical_spacing,
            :layout
          )

          HtmlValidator.reserve_render_resource(:max_table_grid_work, length(placements), :layout)

          row_geometry =
            Enum.with_index(rows, fn row, index ->
              cells = Enum.map(row.cells, &{&1, Map.fetch!(metrics, {index, &1.column})})

              # Fragmentation suppresses the cell alignment offset, while the
              # table grid still reserves its measured baseline ascent/descent.
              eligible =
                Enum.filter(cells, fn {placement, metric} ->
                  Map.get(placement.cell.style, :vertical_align, :middle) == :baseline and
                    metric.has_flow
                end)

              ascent =
                eligible
                |> Enum.map(fn {_cell, metric} -> metric.baseline end)
                |> Enum.max(fn -> nil end)

              descent =
                eligible
                |> Enum.filter(fn {cell, _metric} -> cell.rowspan == 1 end)
                |> Enum.map(fn {_cell, metric} -> metric.intrinsic_height - metric.baseline end)
                |> Enum.max(fn -> 0.0 end)

              minimum =
                cells
                |> Enum.filter(fn {cell, _metric} -> cell.rowspan == 1 end)
                |> Enum.map(fn {_cell, metric} -> metric.minimum_height end)
                |> Enum.max(fn -> 0.0 end)

              declared =
                case Map.get(row.row.style, :height) do
                  value when is_number(value) -> value
                  _ -> 0.0
                end

              height = max(minimum, declared)
              height = if ascent == nil, do: height, else: max(height, ascent + descent)

              fallback =
                cells
                |> Enum.map(fn {_cell, metric} -> metric.inset_bottom end)
                |> Enum.min(fn -> 0.0 end)

              %{
                height: height,
                baseline: if(ascent == nil, do: max(height - fallback, 0.0), else: ascent),
                baseline_tracks_height: ascent == nil and cells != []
              }
            end)

          HtmlValidator.reserve_render_resource(:max_table_grid_work, row_count, :layout)

          geometry_by_row =
            Map.new(Enum.with_index(row_geometry), fn {geometry, index} -> {index, geometry} end)

          offsets =
            Map.new(placements, fn {{index, _column} = key, placement} ->
              metric = Map.fetch!(metrics, key)

              offset =
                if policy == :aligned and
                     Map.get(placement.cell.style, :vertical_align, :middle) == :baseline and
                     metric.has_flow,
                   do: Map.fetch!(geometry_by_row, index).baseline - metric.baseline,
                   else: 0.0

              {key, offset}
            end)

          heights = Enum.map(row_geometry, & &1.height)

          heights =
            Enum.reduce(placements, heights, fn {{index, _column} = key, placement}, heights ->
              if placement.rowspan > 1 do
                metric = Map.fetch!(metrics, key)

                HtmlValidator.reserve_render_resource(
                  :max_table_grid_work,
                  index + placement.rowspan,
                  :layout
                )

                current =
                  Enum.sum(Enum.slice(heights, index, placement.rowspan)) +
                    vertical_spacing * (placement.rowspan - 1)

                extra =
                  max(metric.minimum_height + Map.fetch!(offsets, key) - current, 0.0) /
                    placement.rowspan

                if extra > 0 do
                  HtmlValidator.reserve_render_resource(:max_table_grid_work, row_count, :layout)

                  Enum.with_index(heights, fn height, row ->
                    if row >= index and row < index + placement.rowspan,
                      do: height + extra,
                      else: height
                  end)
                else
                  heights
                end
              else
                heights
              end
            end)

          Enum.each(
            heights ++ Map.values(offsets),
            &HtmlValidator.check_render_resource(:max_css_numeric_magnitude, abs(&1), :layout)
          )

          baselines =
            Enum.zip_with(row_geometry, heights, fn row, height ->
              if row.baseline_tracks_height,
                do: row.baseline + height - row.height,
                else: row.baseline
            end)

          {:ok,
           %{
             heights: heights,
             baselines: baselines,
             offsets: offsets,
             baseline_tracks_height: Enum.map(row_geometry, & &1.baseline_tracks_height)
           }}
        end
      else
        invalid_table(
          %{tag: "table measured geometry"},
          "table baseline geometry requires bounded intrinsic/minimum heights, flow baselines and metrics for every canonical cell"
        )
      end
    end)
  end

  # Percentage descendants need a second allocation-dependent baseline pass.
  defp validate_baseline_dependencies(placements) do
    Enum.reduce_while(placements, :ok, fn {_key, placement}, :ok ->
      result =
        if Map.get(placement.cell.style, :vertical_align, :middle) == :baseline,
          do: validate_baseline_cell(placement.cell),
          else: :ok

      case result do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  defp validate_baseline_cell(node) do
    children = Map.get(node, :children, [])

    pending =
      case table_collection_size(children) do
        {:ok, _count} -> Enum.find_value(children, &baseline_dependency/1)
        :error -> {:invalid_descendant, node}
      end

    case pending do
      nil ->
        :ok

      {:invalid_descendant, descendant} ->
        invalid_table(
          descendant,
          "active table baseline descendants require maps and a list of children when supplied; correct the prepared cell tree or recompute styles"
        )

      pending ->
        Diagnostics.error(
          :layout,
          :unsupported_html,
          "table baseline #{pending} requires allocation-dependent baseline geometry, which is not implemented; use authored vertical-align:top, middle or bottom",
          Keyword.merge(source_context(node),
            operation: :layout,
            module: __MODULE__,
            source: Map.get(node, :tag, "table cell") <> " baseline #{pending}"
          )
        )
    end
  end

  defp baseline_dependency(node) do
    HtmlValidator.reserve_render_resource(:max_table_grid_work, 1, :layout)

    case node do
      %{style: %{display: :none}} ->
        nil

      node when is_map(node) ->
        children = Map.get(node, :children, [])

        case table_collection_size(children) do
          {:ok, _count} ->
            case node do
              %{style: %{height: {:percent, _}}} ->
                "percentage-height descendant"

              _ ->
                Enum.find_value(children, &baseline_dependency/1)
            end

          :error ->
            {:invalid_descendant, node}
        end

      _ ->
        {:invalid_descendant, node}
    end
  end

  # Direct measured models need bounded proper-list inspection before
  # enumeration or placement allocation, including improper-list tails.
  # Baseline measurement and continuation share canonical placement shape and
  # preallocation accounting; neither execution path reinterprets spans.
  defp prepare_table_geometry_rows(rows) do
    {proper_rows, row_count} =
      case table_collection_size(rows) do
        {:ok, count} -> {true, count}
        :error -> {false, 0}
      end

    valid_rows =
      proper_rows and
        Enum.all?(Enum.with_index(rows), fn {row, index} ->
          case row do
            %{row: %{style: style}, cells: cells} when is_map(style) and is_list(cells) ->
              case table_collection_size(cells) do
                {:ok, count} ->
                  HtmlValidator.reserve_render_resource(
                    :max_table_grid_work,
                    2 * count,
                    :layout
                  )

                  Enum.all?(cells, fn cell ->
                    case cell do
                      %{
                        cell: %{style: cell_style, children: children},
                        column: column,
                        rowspan: rowspan
                      }
                      when is_map(cell_style) and is_list(children) and is_integer(column) and
                             column >= 0 and
                             is_integer(rowspan) and rowspan >= 1 and
                             rowspan <= row_count - index ->
                        match?({:ok, _}, table_collection_size(children))

                      _ ->
                        false
                    end
                  end)

                :error ->
                  false
              end

            _ ->
              false
          end
        end)

    placements =
      if valid_rows,
        do:
          Enum.flat_map(Enum.with_index(rows), fn {row, index} ->
            Enum.map(row.cells, &{{index, &1.column}, &1})
          end),
        else: []

    {valid_rows, row_count, placements}
  end

  defp table_collection_size(collection, count \\ 0, stage \\ :layout) do
    case collection do
      [] ->
        {:ok, count}

      [_head | tail] ->
        HtmlValidator.reserve_render_resource(:max_table_grid_work, 1, stage)
        table_collection_size(tail, count + 1, stage)

      _ ->
        :error
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
             has_spanning_cells: boolean(),
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
               {:ok, rows, spanning_cells?} <- form_grid(Enum.reverse(rows)) do
            columns = Enum.reverse(columns)

            with :ok <- validate_border_grid_support(rows, columns, table_style) do
              {:ok,
               %{
                 rows: rows,
                 has_spanning_cells: spanning_cells?,
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
      {:ok, rows, occupied} -> {:ok, Enum.reverse(rows), map_size(occupied) > 0}
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
