defmodule NativeElixirPdfUtilities.HtmlToPdf.ColumnLayout do
  @moduledoc "Column selection, spanning sections and bounded balancing over the shared fragmentation engine."
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, Layout, Pagination}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  # One CSS pixel is 0.75 points. Used-size searches stop at 1/64 pixel,
  # a geometry precision rather than an independently configurable limit.
  @precision 0.75 / 64

  @page_context {__MODULE__, :page_context}

  @doc "Scopes the placed source prefix used when columns enter a physical page."
  @spec with_page([map()], {number(), number()}, map(), function()) :: term()
  def with_page(_children, page_size, margins, callback) do
    context =
      if NativeElixirPdfUtilities.HtmlToPdf.Hyphenator.fragmentainer_height() != nil,
        do: %{page_size: page_size, margins: margins, prefixes: []},
        else: nil

    with_context(context, callback)
  end

  @doc "Adds preceding sibling boxes for the duration of a descendant layout."
  @spec with_prefix([map()], function()) :: term()
  @spec with_prefix([map()], function(), boolean()) :: term()
  def with_prefix(boxes, callback, reversed? \\ false) do
    case Process.get(@page_context) do
      nil ->
        callback.()

      context ->
        with_context(%{context | prefixes: [{boxes, reversed?} | context.prefixes]}, callback)
    end
  end

  @doc "Isolates intrinsic and column-local measurements from physical page coordinates."
  @spec without_page(function()) :: term()
  def without_page(callback), do: with_context(nil, callback)

  defp with_context(context, callback) do
    previous = Process.get(@page_context)
    Process.put(@page_context, context)

    try do
      callback.()
    after
      if previous == nil,
        do: Process.delete(@page_context),
        else: Process.put(@page_context, previous)
    end
  end

  @doc "Recognizes a multicolumn principal box after the cascade."
  @spec enabled?(map()) :: boolean()
  def enabled?(style) do
    case style do
      style when not is_map_key(style, :column_count) and not is_map_key(style, :column_width) ->
        false

      _ ->
        Map.get(style, :column_count, :auto) != :auto or
          Map.get(style, :column_width, :auto) != :auto
    end
  end

  @doc "Lays out source sections once, balancing their fragments without repeating text measurement."
  @spec layout([map()], map(), number(), number(), number(), function(), function()) ::
          {:ok, [map()], number()} | {:error, term()}
  def layout(children, style, x, y, width, render, paint_rule) do
    style =
      NativeElixirPdfUtilities.Validators.CssBoxAlignmentValidator.used_gaps(style, width, nil)

    gap =
      if Map.get(style, :_column_gap_normal, not Map.has_key?(style, :column_gap)),
        do: Map.get(style, :font_size, 12.0),
        else: Map.get(style, :column_gap, 0.0)

    preferred = Map.get(style, :column_width, :auto)
    count = Map.get(style, :column_count, :auto)

    fitting =
      if is_number(preferred),
        do: max(floor((width + gap) / (max(preferred, 0.75) + gap)), 1),
        else: count

    count =
      cond do
        count == :auto -> fitting
        preferred == :auto -> count
        true -> min(count, fitting)
      end

    HtmlValidator.reserve_render_resource(:max_entries, count, :columns)
    column_width = max((width - gap * (count - 1)) / count, 0.0)
    page_capacity = NativeElixirPdfUtilities.HtmlToPdf.Hyphenator.fragmentainer_height()
    height = Layout.resolve_content_size(style, :height, Map.get(style, :_containing_height), nil)

    maximum =
      Layout.resolve_content_size(
        Map.put(style, :height, Map.get(style, :max_height)),
        :height,
        Map.get(style, :_containing_height),
        nil
      )

    height = if height == nil and is_number(maximum), do: maximum, else: height

    inner =
      style
      |> Map.put(:column_count, :auto)
      |> Map.put(:column_width, :auto)
      |> Map.delete(:height)
      |> Map.delete(:min_height)
      |> Map.delete(:max_height)

    sections =
      children |> promote_spanners() |> Enum.chunk_by(&spanning?/1)

    last_section = length(sections) - 1

    sections
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, [], 0.0}, fn {section, index}, {:ok, boxes, used} ->
      spanning? = spanning?(hd(section))

      result =
        if spanning? do
          with_prefix(boxes, fn -> render.(section, inner, x, y - used, width) end)
        else
          remaining = if is_number(height), do: max(height - used, @precision), else: nil
          followed_by_span? = index < last_section

          with {:ok, source, natural_height} <-
                 without_page(fn -> render.(section, inner, 0.0, 0.0, column_width) end) do
            fill =
              if followed_by_span?, do: :balance, else: Map.get(style, :column_fill, :balance)

            upper =
              if remaining == nil,
                do:
                  min(
                    max(natural_height, @precision),
                    page_capacity || max(natural_height, @precision)
                  ),
                else: remaining

            paged? = remaining == nil and page_capacity != nil

            initial =
              if natural_height == 0,
                do: {:ok, [{[%{size: {column_width, 0.0}, boxes: source}], 0.0}]},
                else:
                  entry_column_sets(
                    source,
                    max(column_width, 0.75),
                    upper,
                    count,
                    fill,
                    paged?,
                    boxes,
                    x,
                    y - used,
                    style
                  )

            with {:ok, sets} <- initial do
              minimum_height = sets |> Enum.map(&elem(&1, 1)) |> Enum.min()

              sets =
                if minimum_height > 0 and nested_columns?(section) and
                     minimum_height < (page_capacity || upper) do
                  # Nested fragmentainers need their final outer capacity before
                  # allocating source text to inner columns. Repeat layout once at
                  # that capacity, then use the same bounded fragmentation pass.
                  result =
                    NativeElixirPdfUtilities.HtmlToPdf.Hyphenator.with_fragmentainer(
                      minimum_height,
                      fn ->
                        with {:ok, source, _} <-
                               without_page(fn ->
                                 render.(section, inner, 0.0, 0.0, column_width)
                               end),
                             do:
                               entry_column_sets(
                                 source,
                                 max(column_width, 0.75),
                                 upper,
                                 count,
                                 fill,
                                 paged?,
                                 boxes,
                                 x,
                                 y - used,
                                 style
                               )
                      end
                    )

                  case result do
                    {:ok, sets} -> sets
                    error -> throw({:render_resource_limit, error})
                  end
                else
                  sets
                end

              {placed, total_height} =
                Enum.map_reduce(sets, 0.0, fn {set, used_height}, offset ->
                  id = {:column_set, make_ref()}
                  top = y - used - offset

                  before? =
                    Enum.any?(set, fn page ->
                      Enum.any?(page.boxes, &Map.get(&1, :column_page_break_before, false))
                    end)

                  after? =
                    Enum.any?(set, fn page ->
                      Enum.any?(page.boxes, &Map.get(&1, :column_page_break_after, false))
                    end)

                  breaks = %{
                    break_before: if(before?, do: :page, else: :auto),
                    break_after: if(after?, do: :page, else: :auto)
                  }

                  placed =
                    set
                    |> Enum.with_index()
                    |> Enum.flat_map(fn {page, index} ->
                      physical =
                        if Map.get(style, :direction, :ltr) == :rtl,
                          do: count - index - 1,
                          else: index

                      page.boxes
                      |> DrawingGeometry.translate_boxes(
                        x + physical * (column_width + gap),
                        top - used_height
                      )
                      |> Enum.map(fn box ->
                        box
                        |> Map.drop([
                          :fieldset_paint_id,
                          :fieldset_bounds,
                          :fieldset_ancestors,
                          :fieldset_source_y,
                          :table_id,
                          :table_section,
                          :repeat_table_header,
                          :fragment_id,
                          :fragment_lines
                        ])
                        |> Map.merge(breaks)
                        |> Map.put(:flow_id, id)
                        |> Map.put(:parallel_item_bounds, {top, top - used_height})
                      end)
                    end)

                  rules =
                    if length(set) > 1 do
                      Enum.flat_map(1..(length(set) - 1), fn index ->
                        physical =
                          if Map.get(style, :direction, :ltr) == :rtl,
                            do: count - index,
                            else: index

                        paint_rule.(
                          style,
                          x + physical * (column_width + gap) - gap / 2,
                          top,
                          used_height
                        )
                        |> Enum.map(&(&1 |> Map.put(:flow_id, id) |> Map.merge(breaks)))
                      end)
                    else
                      []
                    end

                  marker = %{
                    type: :flow_marker,
                    x: x,
                    y: top - used_height,
                    width: 0.0,
                    height: used_height,
                    flow_id: id,
                    parallel_item_bounds: {top, top - used_height}
                  }

                  marker = Map.merge(marker, breaks)
                  {rules ++ [marker | placed], offset + used_height}
                end)

              {:ok, List.flatten(placed), total_height}
            end
          end
        end

      case result do
        {:ok, next, section_height} -> {:cont, {:ok, boxes ++ next, used + section_height}}
        error -> {:halt, error}
      end
    end)
  end

  defp spanning?(node) do
    style = Map.get(node, :style, %{})

    Map.get(style, :column_span, :none) == :all and
      Map.get(style, :position, :static) not in [:absolute, :fixed] and
      Map.get(style, :float, :none) == :none and
      Map.get(style, :display) not in [:inline, :none, :contents]
  end

  # A spanner's containing block is the multicol container. Normal ancestors
  # fragment around it; separate formatting/fixed-position contexts stop it.
  defp promote_spanners(children) do
    Enum.flat_map(children, fn node ->
      HtmlValidator.reserve_render_resource(:max_work, 1, :columns)
      style = Map.get(node, :style, %{})
      nested = Map.get(node, :children, [])

      open? =
        Map.get(style, :display) in [:block, :contents] and not spanning?(node) and
          not NativeElixirPdfUtilities.Validators.CssDisplayValidator.independent?(style) and
          not NativeElixirPdfUtilities.Validators.CssStackingValidator.containing_block?(
            style,
            :fixed
          )

      expanded = if open?, do: promote_spanners(nested), else: nested

      if open? and Enum.any?(expanded, &spanning?/1) do
        {parts, current} =
          Enum.reduce(expanded, {[], []}, fn child, {parts, current} ->
            if spanning?(child),
              do: {[{:span, child}, {:flow, Enum.reverse(current)} | parts], []},
              else: {parts, [child | current]}
          end)

        parts = Enum.reverse([{:flow, Enum.reverse(current)} | parts])
        last = length(parts) - 1
        HtmlValidator.reserve_render_resource(:max_entries, length(parts), :columns)

        parts
        |> Enum.with_index()
        |> Enum.map(fn {part, index} ->
          case part do
            {:span, child} ->
              child

            {:flow, content} ->
              fragment_style =
                Enum.reduce([:margin, :padding, :border_widths], style, fn key, result ->
                  Map.update(result, key, %{top: 0, right: 0, bottom: 0, left: 0}, fn edges ->
                    edges
                    |> Map.put(:top, if(index == 0, do: edges.top, else: 0.0))
                    |> Map.put(:bottom, if(index == last, do: edges.bottom, else: 0.0))
                  end)
                end)

              fragment_style =
                if index == 0,
                  do: fragment_style,
                  else: Map.drop(fragment_style, [:html_target, :html_target_source])

              %{node | style: fragment_style, children: content}
          end
        end)
      else
        [node]
      end
    end)
  end

  defp nested_columns?(nodes) do
    Enum.any?(nodes, fn node ->
      enabled?(Map.get(node, :style, %{})) or nested_columns?(Map.get(node, :children, []))
    end)
  end

  defp column_sets(source, width, upper, count, fill, paged?) do
    balance? = fill != :auto

    with {:ok, pages, height} <- fragments(source, width, upper, count, balance?) do
      sets = column_groups(pages, count, paged?)

      sets = Enum.map(sets, &{&1, height})
      set_count = length(sets)

      sets =
        sets
        |> Enum.with_index()
        |> Enum.map(fn {{set, height}, index} ->
          rebalance? =
            balance? and paged? and set_count > 1 and
              (fill == :balance_all or index == set_count - 1)

          if rebalance? do
            {:ok, balanced} = rebalance_set(set, width, height, count)
            balanced
          else
            {set, height}
          end
        end)

      {:ok, sets}
    end
  end

  defp column_groups(pages, count, paged?) do
    Enum.chunk_while(
      pages,
      [],
      fn page, current ->
        forced? =
          Enum.any?(page.boxes, &Map.get(&1, :column_page_break_before, false)) or
            Enum.any?(current, fn page ->
              Enum.any?(page.boxes, &Map.get(&1, :column_page_break_after, false))
            end)

        if current != [] and ((paged? and length(current) == count) or forced?),
          do: {:cont, Enum.reverse(current), [page]},
          else: {:cont, [page | current]}
      end,
      fn current ->
        if current == [], do: {:cont, []}, else: {:cont, Enum.reverse(current), []}
      end
    )
  end

  defp entry_column_sets(source, width, upper, count, fill, paged?, boxes, x, y, style) do
    context = Process.get(@page_context)

    remaining =
      if paged? and context != nil and Map.get(style, :break_before) != :page do
        prefix =
          context.prefixes
          |> Enum.reverse()
          |> Enum.flat_map(fn {chunk, reversed?} ->
            if reversed?, do: Enum.reverse(chunk), else: chunk
          end)

        {:ok, available} =
          Pagination.remaining_height(prefix ++ boxes, context.page_size, context.margins, x, y)

        min(available, upper)
      else
        upper
      end

    if remaining >= upper - @precision or remaining <= @precision do
      column_sets(source, width, upper, count, fill, paged?)
    else
      # Only the first physical page has reduced capacity. Recompose the unused
      # source once, then fragment it at the normal page height.
      with {:ok, pages} <- Pagination.fragment_columns(source, width, remaining) do
        [first | rest] = column_groups(pages, count, true)

        first_reservation =
          Enum.find_value(source, 0.0, fn box ->
            if abs(
                 elem(NativeElixirPdfUtilities.HtmlToPdf.PageGeometry.box_vertical_bounds(box), 0)
               ) <= @precision,
               do: Map.get(box, :block_placement_height)
          end)

        if column_overflow(first, remaining) > @precision or
             (first_reservation > remaining and first_reservation <= upper) do
          # An indivisible first unit belongs on a fresh page.
          column_sets(source, width, upper, count, fill, paged?)
        else
          case rest do
            [] ->
              if fill == :auto,
                do: {:ok, [{first, remaining}]},
                else:
                  (
                    {:ok, set} = rebalance_set(first, width, remaining, count)
                    {:ok, [set]}
                  )

            _ ->
              suffix = recompose_columns(List.flatten(rest), remaining)

              with {:ok, sets} <- column_sets(suffix, width, upper, count, fill, true),
                   do: {:ok, [{first, remaining} | sets]}
          end
        end
      end
    end
  end

  defp recompose_columns(set, height) do
    set
    |> Enum.with_index()
    |> Enum.flat_map(fn {page, index} ->
      page.boxes
      |> Enum.reject(&Map.get(&1, :repeated_table_header_copy, false))
      |> Enum.map(fn box ->
        box =
          Map.drop(box, [
            :fieldset_paint_id,
            :fieldset_bounds,
            :fieldset_ancestors,
            :fieldset_source_y
          ])

        if Map.get(box, :role) == :explicit_block_space,
          do: box |> Map.put(:flow_id, {:explicit_block_space, make_ref()}) |> Map.delete(:clip),
          else: box
      end)
      |> DrawingGeometry.translate_boxes(0.0, -height * (index + 1))
    end)
  end

  defp rebalance_set(set, width, height, count) do
    source = recompose_columns(set, height)

    with {:ok, pages, height} <- fragments(source, width, height, count, true),
         do: {:ok, {pages, height}}
  end

  defp fragments(source, width, upper, count, balance?) do
    HtmlValidator.reserve_render_resource(:max_work, length(source) + 1, :columns)

    with {:ok, pages} <- Pagination.fragment_columns(source, width, upper) do
      if balance? and length(pages) <= count do
        target = %{count: count, overflow: column_overflow(pages, upper)}

        minimum = Pagination.minimum_column_height(source, upper)

        balance(source, width, minimum, upper, target, pages)
      else
        {:ok, pages, upper}
      end
    end
  end

  defp balance(source, width, lower, upper, count, best) do
    if upper - lower <= @precision do
      {:ok, best, upper}
    else
      middle = (lower + upper) / 2
      HtmlValidator.reserve_render_resource(:max_work, length(source) + 1, :columns)

      with {:ok, pages} <- Pagination.fragment_columns(source, width, middle) do
        if length(pages) <= count.count and
             column_overflow(pages, middle) <= count.overflow + @precision,
           do: balance(source, width, lower, middle, count, pages),
           else: balance(source, width, middle, upper, count, best)
      end
    end
  end

  defp column_overflow(pages, height) do
    Enum.reduce(pages, 0.0, fn page, overflow ->
      Enum.reduce(page.boxes, overflow, fn box, overflow ->
        {top, bottom} = NativeElixirPdfUtilities.HtmlToPdf.PageGeometry.box_vertical_bounds(box)
        max(overflow, max(-bottom, top - height))
      end)
    end)
  end
end
