defmodule NativeElixirPdfUtilities.HtmlToPdf.GridGapPaint do
  @moduledoc false

  alias NativeElixirPdfUtilities.HtmlToPdf.Layout
  alias NativeElixirPdfUtilities.Validators.{CssLengthValidator, HtmlValidator}

  @widths %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
  @styles %{top: :none, right: :none, bottom: :none, left: :none}
  @colors %{top: nil, right: nil, bottom: nil, left: nil}
  @rules %{
    row:
      {:row_rule_width, :row_rule_style, :row_rule_color, :row_rule_break,
       :row_rule_visibility_items, :row_rule_inset_start, :row_rule_inset_end},
    column:
      {:column_rule_width, :column_rule_style, :column_rule_color, :column_rule_break,
       :column_rule_visibility_items, :column_rule_inset_start, :column_rule_inset_end}
  }

  @doc false
  @spec grid(map(), [map()], map(), {[number()], number()}, {[number()], number()}) :: [map()]
  def grid(style, items, area, columns, rows) do
    column = axis_rules(:column, style, items, area, columns, rows)
    row = axis_rules(:row, style, items, area, rows, columns)

    if Map.get(style, :rule_overlap, :row) == :row, do: column ++ row, else: row ++ column
  end

  defp axis_rules(axis, style, items, area, {sizes, gap}, {cross_sizes, cross_gap}) do
    with %{kind: _} = settings <- settings(style, axis),
         true <- length(sizes) >= 2 do
      HtmlValidator.reserve_render_resource(
        :max_work,
        length(sizes) * max(length(cross_sizes), 1) * (length(items) + 1),
        :layout
      )

      positions = track_positions(sizes, gap)
      cross = track_positions(cross_sizes, cross_gap)

      positions
      |> Enum.drop(-1)
      |> Enum.with_index(1)
      |> Enum.flat_map(fn {{_start, stop}, index} ->
        middle = stop + gap / 2

        draw_segments(axis, items, index, cross, cross_gap, settings)
        |> Enum.flat_map(fn {a, b} ->
          inset_a = length_value(settings.inset_start, b - a, style)
          inset_b = length_value(settings.inset_end, b - a, style)
          paint(settings, area, middle, a + inset_a, b - inset_b)
        end)
      end)
    else
      _ -> []
    end
  end

  @doc false
  @spec lanes(map(), [map()], map(), atom(), [number()], number(), number()) :: [map()]
  def lanes(style, items, area, axis, sizes, gap, stack_gap) do
    other = if axis == :column, do: :row, else: :column
    stack_size = if axis == :column, do: area.height, else: area.width
    tracks = track_positions(sizes, gap)
    HtmlValidator.reserve_render_resource(:max_work, length(sizes) * (length(items) + 1), :layout)

    intervals =
      Enum.map(items, fn item ->
        {first, stop} = lines(item, axis)

        {a, b} =
          if axis == :column do
            {area.top - item.y - item.margin.top,
             area.top - item.y + item.box_height + item.margin.bottom}
          else
            {item.x - area.x - item.margin.left,
             item.x - area.x + item.box_width + item.margin.right}
          end

        {first, stop, a, b}
      end)

    cross_rules =
      case settings(style, axis) do
        nil ->
          []

        settings ->
          tracks
          |> Enum.drop(-1)
          |> Enum.with_index(1)
          |> Enum.flat_map(fn {{_, middle}, index} ->
            covered =
              for {first, stop, a, b} <- intervals,
                  first <= index and stop > index + 1,
                  do: {a, b}

            ranges =
              covered
              |> Enum.sort()
              |> Enum.reduce([{0.0, stack_size}], fn {a, b}, free ->
                Enum.flat_map(free, fn {left, right} ->
                  if a < right and b > left,
                    do:
                      Enum.filter([{left, min(a, right)}, {max(b, left), right}], fn {x, y} ->
                        y > x
                      end),
                    else: [{left, right}]
                end)
              end)

            Enum.flat_map(ranges, fn {a, b} ->
              paint_inset(settings, style, area, middle + gap / 2, a, b)
            end)
          end)
      end

    stack_rules =
      case settings(style, other) do
        nil ->
          []

        settings ->
          tracks
          |> Enum.with_index(1)
          |> Enum.flat_map(fn {{left, right}, col} ->
            intervals
            |> Enum.filter(fn {first, stop, _, _} -> first <= col and stop > col end)
            |> Enum.sort_by(&elem(&1, 2))
            |> Enum.chunk_every(2, 1, :discard)
            |> Enum.flat_map(fn [{_, _, _, end_}, {_, _, start, _}] ->
              if abs(start - end_ - stack_gap) < 1.0e-5,
                do: paint_inset(settings, style, area, (start + end_) / 2, left, right),
                else: []
            end)
          end)
      end

    if Map.get(style, :rule_overlap, :row) == axis,
      do: stack_rules ++ cross_rules,
      else: cross_rules ++ stack_rules
  end

  defp paint_inset(settings, style, area, middle, a, b) do
    inset_a = length_value(settings.inset_start, b - a, style)
    inset_b = length_value(settings.inset_end, b - a, style)
    paint(settings, area, middle, a + inset_a, b - inset_b)
  end

  defp settings(style, axis) do
    {width_key, style_key, color_key, breaks, visibility, inset_start, inset_end} =
      Map.fetch!(@rules, axis)

    kind = Map.get(style, style_key, :none)
    width = length_value(Map.get(style, width_key, 2.25), 0.0, style)

    if kind not in [:none, :hidden] and width > 0 do
      %{
        axis: axis,
        width: width,
        kind: kind,
        color:
          case Map.get(style, color_key, :current_color) do
            :current_color -> style.color
            color -> color
          end,
        breaks: Map.get(style, breaks, :normal),
        visibility: Map.get(style, visibility, :normal),
        inset_start: Map.get(style, inset_start, 0.0),
        inset_end: Map.get(style, inset_end, 0.0)
      }
    end
  end

  defp track_positions(sizes, gap) do
    {positions, _} =
      Enum.map_reduce(sizes, 0.0, fn size, start ->
        {{start, start + size}, start + size + gap}
      end)

    positions
  end

  defp draw_segments(axis, items, index, cross, gap, settings) do
    other = if axis == :column, do: :row, else: :column

    segments =
      cross
      |> Enum.with_index(1)
      |> Enum.flat_map(fn {{a, b}, line} ->
        adjacent =
          Enum.filter(items, fn item ->
            {first, stop} = lines(item, other)
            line >= first and line < stop
          end)

        covered? =
          Enum.any?(adjacent, fn item ->
            {first, stop} = lines(item, axis)
            first <= index and stop > index + 1
          end)

        left? = Enum.any?(adjacent, fn item -> elem(lines(item, axis), 1) == index + 1 end)
        right? = Enum.any?(adjacent, fn item -> elem(lines(item, axis), 0) == index + 1 end)

        visible? =
          case settings.visibility do
            :around -> left? or right?
            :between -> left? and right?
            _ -> true
          end

        if not covered? and visible? do
          start = if line == 1, do: a, else: a - gap / 2
          stop = if line == length(cross), do: b, else: b + gap / 2
          [{start, stop}]
        else
          []
        end
      end)

    if settings.breaks == :intersection do
      segments
    else
      segments
      |> Enum.reduce([], fn
        {a, b}, [{first, last} | rest] when abs(last - a) < 1.0e-7 -> [{first, b} | rest]
        segment, acc -> [segment | acc]
      end)
      |> Enum.reverse()
    end
  end

  defp lines(item, axis) do
    case axis do
      :row -> {item.row_start, item.row_end}
      :column -> {item.column_start, item.column_end}
    end
  end

  defp paint(settings, area, middle, a, b) do
    if b <= a do
      []
    else
      width = settings.width

      {side, x, y, w, h} =
        case settings.axis do
          :column ->
            position = if area.rtl?, do: area.width - middle, else: middle
            {:left, area.x + position - width / 2, area.top - b, width, b - a}

          :row ->
            position = if area.rtl?, do: area.width - b, else: a
            {:top, area.x + position, area.top - middle - width / 2, b - a, width}
        end

      style = %{
        color: settings.color,
        border_colors: Map.put(@colors, side, settings.color),
        border_widths: Map.put(@widths, side, width),
        border_styles: Map.put(@styles, side, settings.kind),
        border_radius: 0.0
      }

      Layout.paint_background(style, x, y, w, h)
      |> Enum.map(&Map.put(&1, :role, :gap_rule))
    end
  end

  defp length_value(value, basis, style) do
    case CssLengthValidator.resolve(value, basis, style) do
      {:ok, number} when is_number(number) -> number
      {:ok, {:percent, ratio}} -> ratio * basis
      {:error, _} = error -> throw({:render_resource_limit, error})
    end
  end
end
