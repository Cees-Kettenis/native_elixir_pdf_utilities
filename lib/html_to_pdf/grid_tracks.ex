defmodule NativeElixirPdfUtilities.HtmlToPdf.GridTracks do
  @moduledoc "Expands validated explicit grid tracks and resolves named grid lines."
  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    CssLengthValidator,
    CssSizingValidator
  }

  @doc "Expands auto-repeat and records line names against the used explicit grid."
  @spec expand(map(), number() | nil, number() | nil) :: map()
  def expand(style, width, height) do
    style =
      NativeElixirPdfUtilities.Validators.CssBoxAlignmentValidator.used_gaps(style, width, height)

    Enum.reduce(
      [
        {:column, :grid_template_columns, width, :column_gap},
        {:row, :grid_template_rows, height, :row_gap}
      ],
      style,
      fn {axis, property, available, gap_property}, style ->
        gap = Map.get(style, gap_property, 0.0)
        template = Map.get(style, property, [])
        {expanded, fit} = expand_repeat(template, available, gap, style)

        {tracks, names, _} =
          Enum.reduce(expanded, {[], %{}, 1}, fn track, {tracks, names, line} ->
            case track do
              {:line_names, labels} ->
                names =
                  Enum.reduce(labels, names, fn name, map ->
                    Map.update(map, name, [line], &Enum.uniq(&1 ++ [line]))
                  end)

                {tracks, names, line}

              {:subgrid, _} ->
                {tracks, names, line}

              track ->
                {[resolve(track, available, style) | tracks], names, line + 1}
            end
          end)

        areas = Map.get(style, :grid_template_areas, [])
        names = area_names(areas, names, axis)
        area_count = if axis == :row, do: length(areas), else: length(List.first(areas) || [])
        tracks = Enum.reverse(tracks)
        tracks = tracks ++ List.duplicate(:auto, max(area_count - length(tracks), 0))
        auto_key = if axis == :row, do: :grid_auto_rows, else: :grid_auto_columns

        auto =
          case Map.get(style, auto_key, :auto) do
            {:track_cycle, values} ->
              {:track_cycle, Enum.map(values, &resolve(&1, available, style))}

            track ->
              resolve(track, available, style)
          end

        style
        |> Map.put(property, tracks)
        |> Map.put(auto_key, auto)
        |> Map.put(if(axis == :column, do: :grid_column_names, else: :grid_row_names), names)
        |> Map.put(if(axis == :column, do: :grid_column_auto_fit, else: :grid_row_auto_fit), fit)
      end
    )
  end

  defp expand_repeat(template, available, gap, style) do
    outside =
      template
      |> Enum.reject(&(match?({:repeat, _, _}, &1) or match?({:line_names, _}, &1)))
      |> Enum.map(&repeat_size(resolve(&1, available, style)))

    Enum.reduce(template, {[], []}, fn track, {result, fit} ->
      case track do
        {:repeat, mode, pattern} ->
          sizing =
            pattern
            |> Enum.reject(&match?({:line_names, _}, &1))
            |> Enum.map(&repeat_size(resolve(&1, available, style)))

          count = length(sizing)
          unit = max(Enum.sum(sizing) + gap * count, 0.75)

          repeats =
            if is_number(available),
              do:
                max(
                  trunc(
                    Float.floor(
                      (available - Enum.sum(outside) - gap * length(outside) + gap) / unit
                    )
                  ),
                  1
                ),
              else: 1

          case HtmlValidator.validate_layout_cardinality(
                 :grid_tracks,
                 repeats * count + length(outside)
               ) do
            :ok -> :ok
            error -> throw({:render_resource_limit, error})
          end

          HtmlValidator.reserve_render_resource(:max_work, repeats * length(pattern), :layout)
          first = Enum.count(result, &(not match?({:line_names, _}, &1)))

          indices =
            if mode == :auto_fit, do: Enum.to_list(first..(first + repeats * count - 1)), else: []

          {result ++ (List.duplicate(pattern, repeats) |> List.flatten()), fit ++ indices}

        _ ->
          {result ++ [track], fit}
      end
    end)
  end

  defp repeat_size(track) do
    case track do
      {:length, value} -> max(value, 0.75)
      {:minmax, {:length, minimum}, {:length, maximum}} -> max(minimum, maximum)
      {:minmax, {:length, minimum}, _} -> max(minimum, 0.75)
      {:minmax, _, {:length, maximum}} -> max(maximum, 0.75)
      _ -> 0.75
    end
  end

  @doc "Resolves a track's length dependencies against a definite or indefinite axis."
  @spec resolve(term(), number() | nil, map()) :: term()
  def resolve(track, available, style) do
    case track do
      {:length, value} ->
        case length_value(value, available, style) do
          nil -> :auto
          length -> {:length, length}
        end

      {:fit_content, value} ->
        {:fit_content, length_value(value, available, style)}

      {:minmax, minimum, maximum} ->
        {:minmax, resolve(minimum, available, style), resolve(maximum, available, style)}

      _ ->
        track
    end
  end

  defp length_value(value, available, style) do
    result =
      case value do
        {:css_size_math, _} -> CssSizingValidator.resolve_math(value, available, style)
        {:percent, ratio} -> {:ok, if(is_number(available), do: ratio * available)}
        _ -> CssLengthValidator.resolve(value, available, style)
      end

    case result do
      {:ok, value} -> value
      error -> throw({:render_resource_limit, error})
    end
  end

  defp area_names(rows, names, axis) do
    cells =
      for {row, y} <- Enum.with_index(rows),
          {name, x} <- Enum.with_index(row),
          not String.starts_with?(name, "."),
          do: {name, if(axis == :column, do: x, else: y) + 1}

    cells
    |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
    |> Enum.reduce(names, fn {name, positions}, names ->
      names
      |> Map.update(
        name <> "-start",
        [Enum.min(positions)],
        &Enum.uniq(&1 ++ [Enum.min(positions)])
      )
      |> Map.update(
        name <> "-end",
        [Enum.max(positions) + 1],
        &Enum.uniq(&1 ++ [Enum.max(positions) + 1])
      )
    end)
  end

  @doc "Resolves named, negative and spanning lines while retaining automatic placement."
  @spec place_lines(map(), map()) :: map()
  def place_lines(item, style) do
    Enum.reduce(
      [
        {:column_start, :column_end, :grid_template_columns, :grid_column_names},
        {:row_start, :row_end, :grid_template_rows, :grid_row_names}
      ],
      item,
      fn {start_key, end_key, template_key, names_key}, item ->
        count = length(Map.get(style, template_key, [])) + 1
        names = Map.get(style, names_key, %{})
        start = line(Map.get(item, start_key), names, count, :start)
        finish = line(Map.get(item, end_key), names, count, :end)

        {start, finish} =
          case {start, finish} do
            {start, {:named_span, name, n}} when is_integer(start) ->
              positions = Map.get(names, name, []) |> Enum.filter(&(&1 > start))

              {start,
               Enum.at(positions, n - 1, max(count, start) + max(n - length(positions), 0))}

            {{:named_span, name, n}, finish} when is_integer(finish) ->
              positions =
                Map.get(names, name, []) |> Enum.filter(&(&1 < finish)) |> Enum.reverse()

              {Enum.at(positions, n - 1, min(1, finish) - max(n - length(positions), 0)), finish}

            {{:named_span, _, n}, finish} ->
              {{:span, n}, finish}

            {start, {:named_span, _, n}} ->
              {start, {:span, n}}

            pair ->
              pair
          end

        {start, finish} =
          if is_integer(start) and is_integer(finish),
            do: {min(start, finish), max(max(start, finish), min(start, finish) + 1)},
            else: {start, finish}

        item |> Map.put(start_key, start) |> Map.put(end_key, finish)
      end
    )
  end

  @doc "Adds implicit tracks before the explicit grid and shifts used line indices together."
  @spec prepend_implicit([map()], map()) :: {[map()], map()}
  def prepend_implicit(items, style) do
    Enum.reduce(
      [
        {:column_start, :column_end, :grid_template_columns, :grid_auto_columns,
         :grid_column_names},
        {:row_start, :row_end, :grid_template_rows, :grid_auto_rows, :grid_row_names}
      ],
      {items, style},
      fn {start_key, end_key, template_key, auto_key, names_key}, {items, style} ->
        first =
          Enum.reduce(items, 1, fn item, first ->
            start = Map.fetch!(item, start_key)
            finish = Map.fetch!(item, end_key)

            candidate =
              case {start, finish} do
                {start, _} when is_integer(start) -> start
                {{:span, n}, finish} when is_integer(finish) -> finish - n
                {_, finish} when is_integer(finish) -> finish - 1
                _ -> 1
              end

            min(candidate, first)
          end)

        count = 1 - first

        if count > 0 do
          HtmlValidator.reserve_render_resource(:max_entries, count, :layout)

          auto =
            case Map.get(style, auto_key, :auto) do
              {:track_cycle, tracks} -> tracks
              track -> [track]
            end

          prefix = Enum.map(-count..-1, &Enum.at(auto, Integer.mod(&1, length(auto))))

          items =
            Enum.map(items, fn item ->
              Enum.reduce([start_key, end_key], item, fn key, item ->
                Map.update!(item, key, fn value ->
                  if is_integer(value), do: value + count, else: value
                end)
              end)
            end)

          names =
            Map.new(Map.get(style, names_key, %{}), fn {name, positions} ->
              {name, Enum.map(positions, &(&1 + count))}
            end)

          {items,
           style |> Map.update(template_key, prefix, &(prefix ++ &1)) |> Map.put(names_key, names)}
        else
          {items, style}
        end
      end
    )
  end

  @doc "Collapses empty auto-fit tracks together with their adjoining gutters."
  @spec collapse_auto_fit([map()], map()) :: {[map()], map()}
  def collapse_auto_fit(items, style) do
    Enum.reduce(
      [
        {:column_start, :column_end, :grid_template_columns, :grid_column_auto_fit},
        {:row_start, :row_end, :grid_template_rows, :grid_row_auto_fit}
      ],
      {items, style},
      fn {start_key, end_key, template_key, fit_key}, {items, style} ->
        empty =
          Enum.filter(Map.get(style, fit_key, []), fn index ->
            not Enum.any?(
              items,
              &(Map.fetch!(&1, start_key) <= index + 1 and Map.fetch!(&1, end_key) > index + 1)
            )
          end)

        if empty == [] do
          {items, style}
        else
          tracks =
            Map.get(style, template_key, [])
            |> Enum.with_index()
            |> Enum.reject(&(elem(&1, 1) in empty))
            |> Enum.map(&elem(&1, 0))

          items =
            Enum.map(items, fn item ->
              Enum.reduce([start_key, end_key], item, fn key, item ->
                Map.update!(item, key, fn line -> line - Enum.count(empty, &(&1 + 1 < line)) end)
              end)
            end)

          names_key = if start_key == :column_start, do: :grid_column_names, else: :grid_row_names

          names =
            Map.new(Map.get(style, names_key, %{}), fn {name, positions} ->
              {name,
               Enum.map(positions, fn line -> line - Enum.count(empty, &(&1 + 1 < line)) end)
               |> Enum.uniq()}
            end)

          {items,
           style
           |> Map.put(template_key, if(tracks == [], do: [{:length, 0.0}], else: tracks))
           |> Map.put(names_key, names)}
        end
      end
    )
  end

  @doc "Binds subgrid axes to the spanned parent tracks, gaps and line names."
  @spec inherit(map(), map(), [number()] | nil, [number()] | nil) :: map()
  def inherit(item, parent, columns, rows) do
    source_style =
      case Map.get(item, :orthogonal_node) do
        nil ->
          Map.put(
            item.style,
            :writing_mode,
            Map.get(
              item.style,
              :_writing_plane,
              Map.get(item.style, :writing_mode, :horizontal_tb)
            )
          )

        node ->
          Map.get(item, :orthogonal_style, node._writing_source.style)
      end

    child_axes = NativeElixirPdfUtilities.Validators.CssLogicalValidator.axes(source_style)

    parent_axes =
      NativeElixirPdfUtilities.Validators.CssLogicalValidator.axes(
        Map.put(
          parent,
          :writing_mode,
          Map.get(parent, :_writing_plane, Map.get(parent, :writing_mode, :horizontal_tb))
        )
      )

    descriptors =
      Enum.map([:column, :row], fn axis ->
        {property, gap_key, dimension, start_side} =
          if axis == :column,
            do:
              {:grid_template_columns, :column_gap, child_axes.inline_size,
               child_axes.inline_start},
            else: {:grid_template_rows, :row_gap, child_axes.block_size, child_axes.block_start}

        {parent_property, start_key, end_key, parent_names, parent_gap, sizes, parent_start} =
          if dimension == parent_axes.inline_size,
            do:
              {:grid_template_columns, :column_start, :column_end, :grid_column_names,
               :column_gap, columns, parent_axes.inline_start},
            else:
              {:grid_template_rows, :row_start, :row_end, :grid_row_names, :row_gap, rows,
               parent_axes.block_start}

        {property, parent_property, start_key, end_key, parent_names, gap_key, parent_gap, sizes,
         start_side != parent_start}
      end)

    mapping =
      Map.new(descriptors, fn {property, parent_property, _, _, _, _, _, _, reverse?} ->
        parent_axis = if parent_property == :grid_template_columns, do: :column, else: :row
        child_axis = if property == :grid_template_columns, do: :column, else: :row
        {parent_axis, %{axis: child_axis, reverse?: reverse?}}
      end)

    item = Map.put(item, :subgrid_axes, mapping)

    Enum.reduce(
      descriptors,
      item,
      fn {property, parent_property, start_key, end_key, parent_names_key, gap_key,
          parent_gap_key, sizes, reverse?},
         item ->
        case Map.get(item.style, property) do
          [{:subgrid, local_names}] ->
            first = Map.fetch!(item, start_key)
            count = Map.fetch!(item, end_key) - first
            fixed_names = Enum.count(local_names, &(not match?({:auto_fill, _}, &1)))

            local_names =
              Enum.flat_map(local_names, fn group ->
                case group do
                  {:auto_fill, pattern} ->
                    repeats = max(div(count + 1 - fixed_names, length(pattern)), 0)

                    HtmlValidator.reserve_render_resource(
                      :max_work,
                      repeats * length(pattern),
                      :layout
                    )

                    List.duplicate(pattern, repeats) |> Enum.flat_map(& &1)

                  names ->
                    [names]
                end
              end)

            tracks =
              if is_list(sizes),
                do: sizes |> Enum.slice(first - 1, count) |> Enum.map(&{:length, &1}),
                else: Enum.slice(Map.get(parent, parent_property, []), first - 1, count)

            tracks = tracks ++ List.duplicate(:auto, max(count - length(tracks), 0))

            tracks = if reverse?, do: Enum.reverse(tracks), else: tracks
            parent_gap = Map.get(parent, parent_gap_key, 0.0)

            normal? =
              Map.get(
                item.style,
                if(gap_key == :column_gap, do: :_column_gap_normal, else: :_row_gap_normal),
                true
              )

            child_gap = if normal?, do: parent_gap, else: Map.get(item.style, gap_key, 0.0)
            difference = parent_gap - child_gap

            basis =
              if is_list(columns),
                do:
                  Enum.sum(
                    Enum.slice(
                      columns,
                      item.column_start - 1,
                      item.column_end - item.column_start
                    )
                  ) +
                    Map.get(parent, :column_gap, 0.0) *
                      max(item.column_end - item.column_start - 1, 0),
                else: 0.0

            {start_side, end_side} =
              if Map.has_key?(item, :orthogonal_node) do
                if gap_key == :column_gap,
                  do: {child_axes.inline_start, child_axes.inline_end},
                  else: {child_axes.block_start, child_axes.block_end}
              else
                case {gap_key, Map.get(item.style, :direction, :ltr)} do
                  {:row_gap, _} -> {:top, :bottom}
                  {_, :rtl} -> {:right, :left}
                  _ -> {:left, :right}
                end
              end

            edge_style =
              if Map.has_key?(item, :orthogonal_node), do: source_style, else: item.style

            inset = fn side ->
              Enum.reduce([:margin, :padding, :border_widths], 0.0, fn key, total ->
                total + length_value(get_in(edge_style, [key, side]) || 0.0, basis, edge_style)
              end)
            end

            start_inset = inset.(start_side)
            end_inset = inset.(end_side)

            tracks =
              tracks
              |> Enum.with_index()
              |> Enum.map(fn {track, index} ->
                case track do
                  {:length, size} ->
                    {:length,
                     max(
                       size +
                         difference / 2 *
                           (if(index > 0, do: 1, else: 0) + if(index < count - 1, do: 1, else: 0)) -
                         if(index == 0, do: start_inset, else: 0.0) -
                         if(index == count - 1, do: end_inset, else: 0.0),
                       0.0
                     )}

                  _ ->
                    track
                end
              end)

            parent_names = Map.get(parent, parent_names_key, %{})

            named =
              Enum.flat_map(0..count, fn offset ->
                names =
                  for {name, positions} <- parent_names,
                      (first + if(reverse?, do: count - offset, else: offset)) in positions,
                      do: name

                names = Enum.uniq(names ++ Enum.at(local_names, offset, []))

                [{:line_names, names}] ++
                  if(offset < count, do: [Enum.at(tracks, offset)], else: [])
              end)

            parent_gap = Map.get(parent, parent_gap_key, 0.0)
            # Normal subgrid gaps inherit the parent geometry. Explicit subgrid
            # gaps are retained and affect its items within those fixed tracks.
            style =
              item.style
              |> Map.put(property, named)
              |> Map.put_new(gap_key, parent_gap)

            style =
              if Map.get(
                   item.style,
                   if(gap_key == :column_gap, do: :_column_gap_normal, else: :_row_gap_normal),
                   true
                 ), do: Map.put(style, gap_key, parent_gap), else: style

            parent_axis = if parent_property == :grid_template_columns, do: :column, else: :row
            insets = if reverse?, do: {end_inset, start_inset}, else: {start_inset, end_inset}

            %{item | style: style}
            |> Map.update(
              :subgrid_insets,
              %{parent_axis => insets},
              &Map.put(&1, parent_axis, insets)
            )

          _ ->
            item
        end
      end
    )
  end

  defp line(value, names, count, edge) do
    case value do
      n when is_integer(n) and n < 0 ->
        count + n + 1

      {:named_line, name, occurrence} ->
        positions =
          Map.get(
            names,
            name,
            Map.get(names, name <> if(edge == :start, do: "-start", else: "-end"), [])
          )

        if occurrence > 0,
          do: Enum.at(positions, occurrence - 1, count + max(occurrence - length(positions), 0)),
          else: Enum.at(positions, occurrence, 1 - max(-occurrence - length(positions), 0))

      _ ->
        value
    end
  end
end
