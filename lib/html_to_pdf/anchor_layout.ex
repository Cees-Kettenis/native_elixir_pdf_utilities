defmodule NativeElixirPdfUtilities.HtmlToPdf.AnchorLayout do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.Validators.{
    CssLogicalValidator,
    CssPositionValidator,
    CssValueValidator,
    HtmlValidator
  }

  @zero_offsets %{top: :auto, right: :auto, bottom: :auto, left: :auto}
  @sizes [:width, :height, :min_width, :max_width, :min_height, :max_height]

  @doc false
  @spec mark(map(), [map()], map()) :: [map()]
  def mark(style, boxes, bounds) do
    case style do
      %{anchor_name: [_ | _] = names, _anchor_source_order: order} ->
        marker = %{
          type: HtmlValidator.reserve_layout_box(:flow_marker),
          x: bounds.x,
          y: bounds.top,
          width: 0.0,
          height: 0.0,
          css_anchor: %{
            names: names,
            order: order,
            tree_scope: Map.get(style, :_anchor_tree_scope),
            width: bounds.width,
            height: bounds.height,
            visible?: :maps.get(:visibility, style, :visible) == :visible,
            writing_mode: Map.get(style, :writing_mode, :horizontal_tb),
            direction: Map.get(style, :direction, :ltr)
          }
        }

        boxes ++ [marker]

      _ ->
        boxes
    end
  end

  @doc false
  @spec collect([map()]) :: {[map()], list()}
  def collect(boxes) do
    Enum.reduce(boxes, {[], []}, fn
      %{css_anchor: anchor} = box, {anchors, scopes} ->
        {[Map.merge(anchor, %{x: box.x, top: box.y}) | anchors], scopes}

      %{css_anchor_scope: scope}, {anchors, scopes} ->
        {anchors, [scope | scopes]}

      _box, result ->
        result
    end)
  end

  @doc false
  @spec mark_scope(map(), [map()], number(), number()) :: [map()]
  def mark_scope(style, boxes, x, top) do
    case style do
      %{anchor_scope: names, _anchor_source_order: order} when names != [] ->
        boxes ++
          [
            %{
              type: HtmlValidator.reserve_layout_box(:flow_marker),
              x: x,
              y: top,
              width: 0.0,
              height: 0.0,
              css_anchor_scope: {Map.get(style, :_anchor_tree_scope), order, names}
            }
          ]

      _ ->
        boxes
    end
  end

  @doc false
  @spec track?(map()) :: boolean()
  def track?(style) do
    case style do
      %{anchor_name: [_ | _]} ->
        true

      %{anchor_scope: scope} when scope != [] ->
        true

      %{position_anchor: name} when name != :none ->
        true

      %{position_area: area} when is_list(area) ->
        true

      %{position_try_fallbacks: [_ | _]} ->
        true

      %{position_visibility: [_ | _]} ->
        true

      %{position: position} when position in [:absolute, :fixed] ->
        offsets = Map.get(style, :offsets, @zero_offsets)

        Enum.any?(@sizes, &anchor_function?(Map.get(style, &1))) or
          Enum.any?(offsets, fn {_side, value} -> anchor_function?(value) end)

      _ ->
        false
    end
  end

  defp anchor_function?(value), do: match?({:css_anchor, _, _, _, _}, value)

  @doc false
  @spec place(map(), map(), {[map()], list()}, (map(), map() -> term())) :: term()
  def place(child, block, context, layout) do
    style = child.style
    order = Map.get(style, :position_try_order, :normal)

    with {:ok, alternatives} <- alternatives(style),
         {:ok, options} <- candidates(alternatives, block, context),
         {:ok, entry, fitted?} <-
           choose(sort_options(options, order, style), child, block, layout) do
      visibility = Map.get(style, :position_visibility, [])

      if (not fitted? and "no-overflow" in visibility) or
           (not entry.anchor_valid? and "anchors-valid" in visibility) or
           (not entry.anchor_visible? and "anchors-visible" in visibility) do
        {:ok, %{entry | boxes: Enum.filter(entry.boxes, &Map.has_key?(&1, :css_anchor_scope))}}
      else
        {:ok, entry}
      end
    end
  end

  defp alternatives(style) do
    style
    |> Map.get(:position_try_fallbacks, [])
    |> Enum.reduce_while({:ok, [style]}, fn
      {:area, area}, {:ok, acc} ->
        {:cont, {:ok, [Map.put(style, :position_area, area) | acc]}}

      {:rule, name, flips}, {:ok, acc} ->
        declarations = get_in(style, [:_position_tries, name])

        if name != nil and declarations == nil do
          {:cont, {:ok, acc}}
        else
          budget = HtmlValidator.new_css_budget()

          result =
            Enum.reduce_while(declarations || [], {:ok, style}, fn declaration, {:ok, style} ->
              case CssValueValidator.validate_declaration(style, declaration, budget) do
                {:ok, style} -> {:cont, {:ok, style}}
                {:error, {:resource_limit_exceeded, _}} = error -> {:halt, error}
                {:error, _} -> {:cont, {:ok, style}}
              end
            end)

          case result do
            {:ok, candidate} ->
              {:cont, {:ok, [Enum.reduce(flips, candidate, &flip/2) | acc]}}

            error ->
              {:halt, error}
          end
        end
    end)
    |> case do
      {:ok, reversed} -> {:ok, Enum.reverse(reversed)}
      error -> error
    end
  end

  defp flip(axis, style) do
    axes = CssLogicalValidator.axes(style)
    horizontal? = axes.inline_size == :width

    pairs =
      case axis do
        :x -> [left: :right]
        :y -> [top: :bottom]
        :inline when horizontal? -> [left: :right]
        :inline -> [top: :bottom]
        :block when horizontal? -> [top: :bottom]
        :block -> [left: :right]
        :diagonal -> [{axes.inline_start, axes.block_start}, {axes.inline_end, axes.block_end}]
      end

    map = Map.new(pairs ++ Enum.map(pairs, fn {a, b} -> {b, a} end))

    offsets =
      Map.new(Map.get(style, :offsets, @zero_offsets), fn {key, value} ->
        value =
          case value do
            {:css_anchor, "anchor", name, keyword, fallback} ->
              replacement =
                Enum.find_value(map, keyword, fn {from, to} ->
                  if Atom.to_string(from) == keyword, do: Atom.to_string(to)
                end)

              {:css_anchor, "anchor", name, replacement, fallback}

            value ->
              value
          end

        {Map.get(map, key, key), value}
      end)

    area =
      case physical_area(Map.get(style, :position_area), style) do
        nil ->
          nil

        pair when is_list(pair) ->
          Enum.map(pair, fn {physical, span?} ->
            side = Atom.to_string(Map.get(map, physical, physical))
            if span? or side == "all", do: "span-" <> side, else: side
          end)

        :error ->
          Map.get(style, :position_area)
      end

    style |> Map.put(:offsets, offsets) |> Map.put(:position_area, area)
  end

  defp candidates(styles, block, context) do
    Enum.reduce_while(styles, {:ok, []}, fn style, {:ok, acc} ->
      default = select(nil, style, context)

      area = physical_area(Map.get(style, :position_area), style)

      if area == :error do
        {:halt,
         Diagnostics.error(
           :layout,
           :invalid_layout,
           "position-area must select one region per axis",
           module: __MODULE__,
           source: "position-area"
         )}
      else
        area_block = area_block(area, default, block)
        offsets = Map.get(style, :offsets, @zero_offsets)

        {offsets, state} =
          Enum.map_reduce(offsets, {false, visible?(default, block)}, fn {side, value}, state ->
            {value, state} = used_value(value, side, style, area_block, context, state)
            {{side, value}, state}
          end)

        {sizes, {invalid?, visible?}} =
          Enum.map_reduce(Map.take(style, @sizes), state, fn {key, value}, state ->
            {value, state} = used_value(value, key, style, area_block, context, state)
            {{key, value}, state}
          end)

        candidate = style |> Map.merge(Map.new(sizes)) |> Map.put(:offsets, Map.new(offsets))

        valid? =
          not invalid? and
            (default != nil or (area == nil and Map.get(style, :position_anchor, :none) == :none))

        {:cont, {:ok, [{candidate, area_block, valid?, visible?} | acc]}}
      end
    end)
    |> case do
      {:ok, reversed} -> {:ok, Enum.reverse(reversed)}
      error -> error
    end
  end

  defp sort_options(options, order, style) do
    if order == :normal do
      options
    else
      axes = CssLogicalValidator.axes(style)
      axis = Map.get(axes, order, order)

      Enum.sort_by(options, fn {_style, block, _valid?, _visible?} -> -Map.fetch!(block, axis) end)
    end
  end

  defp choose(options, child, original, layout) do
    result =
      Enum.reduce_while(options, nil, fn {style, block, valid?, visible?}, first ->
        case layout.(%{child | style: style}, block) do
          {:ok, entry} ->
            entry =
              Map.merge(entry, %{anchor_valid?: valid?, anchor_visible?: valid? and visible?})

            fitted? = fits?(entry.bounds, original) and fits?(entry.bounds, block)

            if fitted?,
              do: {:halt, {:ok, entry, true}},
              else: {:cont, first || {:ok, entry, false}}

          error ->
            {:halt, error}
        end
      end)

    result
  end

  defp fits?(bounds, block) do
    epsilon = 0.0001

    bounds.x >= block.x - epsilon and bounds.x + bounds.width <= block.x + block.width + epsilon and
      bounds.top <= block.top + epsilon and
      bounds.top - bounds.height >= block.top - block.height - epsilon
  end

  defp select(name, style, {anchors, scopes}) do
    case name || Map.get(style, :position_anchor, :none) do
      name when is_binary(name) ->
        order = Map.fetch!(style, :_anchor_source_order)
        tree_scope = Map.get(style, :_anchor_tree_scope)
        scope = scope_for(scopes, tree_scope, order, name)

        anchors
        |> Enum.filter(fn anchor ->
          name in anchor.names and anchor.order < order and
            Map.get(anchor, :tree_scope) == tree_scope and
            scope_for(scopes, tree_scope, anchor.order, name) == scope
        end)
        |> Enum.max_by(& &1.order, fn -> nil end)

      _ ->
        nil
    end
  end

  defp scope_for(scopes, tree_scope, order, name) do
    scopes
    |> Enum.filter(fn {owner, path, names} ->
      owner == tree_scope and (names == :all or name in names) and List.starts_with?(order, path)
    end)
    |> Enum.max_by(fn {_owner, path, _names} -> length(path) end, fn -> nil end)
  end

  defp visible?(anchor, block) do
    case anchor do
      nil ->
        true

      anchor ->
        Map.get(anchor, :visible?, true) and
          anchor.x + anchor.width > block.x and anchor.x < block.x + block.width and
          anchor.top > block.top - block.height and anchor.top - anchor.height < block.top
    end
  end

  defp used_value(value, property, style, block, context, {invalid?, visible?} = state) do
    case value do
      {:css_anchor, kind, name, keyword, fallback} ->
        anchor = select(name, style, context)

        value =
          case {kind, anchor} do
            {_, nil} -> nil
            {"anchor-size", anchor} -> anchor_size(anchor, keyword, property, style)
            {"anchor", anchor} -> anchor_inset(anchor, keyword, property, style, block)
          end

        if value == nil do
          basis =
            if property in [:left, :right, :width, :min_width, :max_width],
              do: block.width,
              else: block.height

          fallback =
            case CssPositionValidator.resolve(fallback, basis, style) do
              {:ok, value} -> value
              _ -> fallback
            end

          {fallback, {true, false}}
        else
          {value, {invalid?, visible? and Map.get(anchor, :visible?, true)}}
        end

      value ->
        {value, state}
    end
  end

  defp anchor_size(anchor, keyword, property, style) do
    ref_axes = CssLogicalValidator.axes(anchor)
    self_axes = CssLogicalValidator.axes(style)

    dimension =
      case keyword do
        :property ->
          if property in [:left, :right, :width, :min_width, :max_width],
            do: :width,
            else: :height

        "width" ->
          :width

        "height" ->
          :height

        "block" ->
          ref_axes.block_size

        "inline" ->
          ref_axes.inline_size

        "self-block" ->
          self_axes.block_size

        "self-inline" ->
          self_axes.inline_size
      end

    Map.fetch!(anchor, dimension)
  end

  defp anchor_inset(anchor, keyword, side, style, block) do
    horizontal? = side in [:left, :right]
    {start, stop} = if horizontal?, do: {:left, :right}, else: {:top, :bottom}
    opposite = if side == start, do: stop, else: start

    ratio =
      case keyword do
        {:percent, ratio} ->
          if logical_edge(block, "start", horizontal?) == start, do: ratio, else: 1.0 - ratio

        "center" ->
          0.5

        keyword ->
          edge =
            case keyword do
              "inside" ->
                side

              "outside" ->
                opposite

              "self-" <> relative ->
                logical_edge(style, relative, horizontal?)

              relative when relative in ["start", "end"] ->
                logical_edge(block, relative, horizontal?)

              "top" ->
                :top

              "right" ->
                :right

              "bottom" ->
                :bottom

              "left" ->
                :left
            end

          cond do
            edge == start -> 0.0
            edge == stop -> 1.0
            true -> nil
          end
      end

    case {side, ratio} do
      {_, nil} -> nil
      {:left, r} -> anchor.x + r * anchor.width - block.x
      {:right, r} -> block.x + block.width - anchor.x - r * anchor.width
      {:top, r} -> block.top - anchor.top + r * anchor.height
      {:bottom, r} -> anchor.top - r * anchor.height - block.top + block.height
    end
  end

  defp logical_edge(style, relative, horizontal?) do
    axes = CssLogicalValidator.axes(style)
    inline? = axes.inline_size == :width == horizontal?

    case {inline?, relative} do
      {true, "start"} -> axes.inline_start
      {true, "end"} -> axes.inline_end
      {false, "start"} -> axes.block_start
      {false, "end"} -> axes.block_end
    end
  end

  defp physical_area(tokens, style) do
    if tokens != nil do
      axes = CssLogicalValidator.axes(style)
      areas = Enum.map(tokens, &area_side(&1, axes, style))

      {horizontal, vertical} =
        Enum.split_with(areas, fn {side, _span?} -> side in [:left, :right] end)

      case {horizontal, vertical} do
        {[x], []} ->
          [x, {:all, false}]

        {[], [{:center, false}]} ->
          [{:center, false}, {:center, false}]

        {[], [y]} when elem(y, 0) in [:top, :bottom] ->
          [{:all, false}, y]

        {[], [all]} ->
          [all, all]

        {[x], [y]} ->
          [x, y]

        {[], [{side, _} = y, neutral]}
        when side in [:top, :bottom] and elem(neutral, 0) in [:center, :all] ->
          [neutral, y]

        {[], [neutral, {side, _} = y]}
        when side in [:top, :bottom] and elem(neutral, 0) in [:center, :all] ->
          [neutral, y]

        {[], [x, y]} when elem(x, 0) in [:center, :all] and elem(y, 0) in [:center, :all] ->
          [x, y]

        _ ->
          :error
      end
    end
  end

  defp area_side(side, axes, style) do
    case side do
      "span-" <> side ->
        {side, _} = area_side(side, axes, style)
        {side, true}

      side ->
        side =
          case side do
            "top" -> :top
            "right" -> :right
            "bottom" -> :bottom
            "left" -> :left
            "all" -> :all
            "center" -> :center
            "x-" <> relative -> logical_edge(style, relative, true)
            "y-" <> relative -> logical_edge(style, relative, false)
            "self-" <> relative -> elem(area_side(relative, axes, style), 0)
            relative when relative in ["start", "block-start"] -> axes.block_start
            relative when relative in ["end", "block-end"] -> axes.block_end
            "inline-start" -> axes.inline_start
            "inline-end" -> axes.inline_end
          end

        {side, false}
    end
  end

  defp area_block(area, ref, block) do
    case {area, ref} do
      {[x_area, y_area], ref} when not is_nil(ref) ->
        {left, right, x_align} =
          interval(x_area, :left, block.x, ref.x, ref.x + ref.width, block.x + block.width)

        {top, bottom, y_align} =
          interval(
            y_area,
            :top,
            -block.top,
            -ref.top,
            -ref.top + ref.height,
            -block.top + block.height
          )

        %{
          block
          | x: left,
            top: -top,
            width: max(right - left, 0.0),
            height: max(bottom - top, 0.0)
        }
        |> Map.put(:anchor_alignment, {x_align, y_align})

      _ ->
        block
    end
  end

  defp interval(part, start_side, start, first, last, stop) do
    case part do
      {:all, _} -> {start, stop, :center}
      {:center, _} -> {first, last, :center}
      {^start_side, false} -> {start, first, :end}
      {^start_side, true} -> {start, last, :end}
      {_, false} -> {last, stop, :start}
      {_, true} -> {first, stop, :start}
    end
  end

  @doc false
  @spec align(map(), map(), number(), number(), map()) :: {number(), number()}
  def align(block, style, width, height, margin) do
    {x, y} = block.anchor_alignment
    x = Map.get(style, :justify_self, x)
    y = Map.get(style, :align_self, y)
    dx = align_offset(x, block.width - width - margin.left - margin.right)
    dy = align_offset(y, block.height - height - margin.top - margin.bottom)
    {block.x + dx, block.top - dy}
  end

  defp align_offset(alignment, value) do
    case alignment do
      :end -> value
      :center -> value / 2
      _ -> 0.0
    end
  end
end
