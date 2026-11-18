defmodule NativeElixirPdfUtilities.HtmlToPdf.BorderShape do
  @moduledoc false

  alias NativeElixirPdfUtilities.HtmlToPdf.PathGeometry

  alias NativeElixirPdfUtilities.Validators.{
    CssClipValidator,
    CssLengthValidator,
    CssLogicalValidator,
    HtmlValidator
  }

  @zero %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
  @clear %{top: nil, right: nil, bottom: nil, left: nil}
  @corners [:top_left, :top_right, :bottom_right, :bottom_left]
  @sides [:top, :right, :bottom, :left]
  @shape_fields %{
    top_left: :corner_top_left_shape,
    top_right: :corner_top_right_shape,
    bottom_right: :corner_bottom_right_shape,
    bottom_left: :corner_bottom_left_shape
  }
  @clip_fields %{
    top: :border_top_clip,
    right: :border_right_clip,
    bottom: :border_bottom_clip,
    left: :border_left_clip
  }

  @doc false
  @spec apply(map(), map(), [map()]) :: [map()]
  def apply(style, rect, artwork) do
    shaped? = shaped?(style)

    clipped? =
      style[:border_limit] != nil or Enum.any?(@clip_fields, fn {_, key} -> style[key] != nil end)

    if shaped? or clipped? do
      {outer, inner} = contours(style, rect)
      shape = style[:border_shape]

      artwork =
        Enum.flat_map(artwork, fn
          %{type: :rect} = box ->
            base = %{
              box
              | stroke_color: nil,
                stroke_width: 0.0,
                border_widths: @zero,
                border_colors: @clear
            }

            if base.fill_color != nil or Map.get(base, :form_control) != nil do
              [if(shaped?, do: Map.put(base, :clip, outer), else: base)]
            else
              []
            end

          box ->
            [if(shaped?, do: Map.put(box, :clip, outer), else: box)]
        end)

      border =
        cond do
          shape != nil ->
            side = relevant_side(style, rect.border_styles)
            [ring(rect, outer, inner, rect.border_colors[side])]

          shaped? ->
            color = rect.border_colors[relevant_side(style, rect.border_styles)]
            if rect.stroke_width > 0, do: [ring(rect, outer, inner, color)], else: []

          true ->
            Enum.flat_map(@sides, &clipped_edge(style, rect, &1))
        end

      artwork ++ Enum.reject(border, &is_nil/1)
    else
      artwork
    end
  end

  @doc false
  @spec shaped?(map()) :: boolean()
  def shaped?(style) do
    style[:border_shape] != nil or
      Enum.any?(@shape_fields, fn {_, key} -> style[key] not in [nil, 1.0] end)
  end

  defp relevant_side(style, styles) do
    axes = CssLogicalValidator.axes(style)

    Enum.find(
      [axes.block_start, axes.inline_start, axes.block_end, axes.inline_end],
      axes.block_start,
      &(styles[&1] not in [:none, :hidden])
    )
  end

  @doc false
  @spec contours(map(), map()) :: {map(), map()}
  def contours(style, rect) do
    case style[:border_shape] do
      [path] ->
        side = relevant_side(style, rect.border_styles)
        half = rect.border_widths[side] / 2
        center = resolve_path(style, rect, path)
        {resize_path(center, half), resize_path(center, -half)}

      [outside, inside] ->
        {resolve_path(style, rect, outside), resolve_path(style, rect, inside)}

      _ ->
        inside =
          NativeElixirPdfUtilities.HtmlToPdf.PageGeometry.inset_rounded_clip(
            rect,
            rect.border_widths
          )

        {corners(style, rect), corners(style, inside)}
    end
  end

  defp resolve_path(style, rect, path) do
    bounds = %{x: rect.x, top: rect.y + rect.height, width: rect.width, height: rect.height}

    {bounds, path} =
      if path.reference == :half_border_box do
        side = relevant_side(style, rect.border_styles)
        half = rect.border_widths[side] / 2

        bounds = %{
          bounds
          | x: bounds.x + half,
            top: bounds.top - half,
            width: max(bounds.width - 2 * half, 0.0),
            height: max(bounds.height - 2 * half, 0.0)
        }

        {bounds, %{path | reference: :border_box}}
      else
        {bounds, path}
      end

    case CssClipValidator.resolve(Map.put(style, :css_clip_path, path), bounds) do
      {:ok, clip} -> clip
      error -> throw({:render_resource_limit, error})
    end
  end

  defp resize_path(clip, delta) do
    width = max(clip.width + 2 * delta, 0.0)
    height = max(clip.height + 2 * delta, 0.0)
    x = clip.x + (clip.width - width) / 2
    y = clip.y + (clip.height - height) / 2
    a = if clip.width > 0, do: width / clip.width, else: 0.0
    d = if clip.height > 0, do: height / clip.height, else: 0.0

    commands =
      PathGeometry.transform(clip.path_commands, {a, 0, 0, d, x - a * clip.x, y - d * clip.y})

    %{clip | x: x, y: y, width: width, height: height, path_commands: commands}
  end

  defp corners(style, rect) do
    radii =
      Map.get_lazy(rect, :border_radii, fn ->
        radius = Map.get(rect, :border_radius, 0.0)
        Map.new(@corners, &{&1, {radius, radius}})
      end)

    points =
      Enum.flat_map(@corners, fn name ->
        {rx, ry} = Map.fetch!(radii, name)
        rx = min(rx, rect.width / 2)
        ry = min(ry, rect.height / 2)
        parameter = Map.get(style, Map.fetch!(@shape_fields, name), 1.0)

        points = arc(parameter, rx, ry)

        points = if name in [:top_right, :bottom_left], do: Enum.reverse(points), else: points

        Enum.map(points, fn {x, y} ->
          case name do
            :top_left -> {rect.x + x, rect.y + rect.height - y}
            :top_right -> {rect.x + rect.width - x, rect.y + rect.height - y}
            :bottom_right -> {rect.x + rect.width - x, rect.y + y}
            :bottom_left -> {rect.x + x, rect.y + y}
          end
        end)
      end)

    [{x, y} | tail] = points
    commands = [{0, [x, y]} | Enum.map(tail, fn {x, y} -> {1, [x, y]} end)] ++ [{4, []}]

    Map.merge(Map.take(rect, [:x, :y, :width, :height]), %{
      path_commands: commands,
      fill_rule: :nonzero
    })
  end

  defp arc(parameter, x, y) do
    cond do
      x <= 0 or y <= 0 ->
        [{0.0, 0.0}]

      parameter == :square ->
        [{0.0, y}, {0.0, 0.0}, {x, 0.0}]

      parameter == :notch ->
        [{0.0, y}, {x, y}, {x, 0.0}]

      parameter == 0 ->
        [{0.0, y}, {x, 0.0}]

      true ->
        power = :math.pow(2, 1 - min(abs(parameter), 16.0))

        [{0.0, y}] ++
          Enum.map(1..15, fn index ->
            angle = index * :math.pi() / 32
            c = :math.pow(:math.cos(angle), power)
            s = :math.pow(:math.sin(angle), power)
            if parameter < 0, do: {x * s, y * c}, else: {x * (1 - c), y * (1 - s)}
          end) ++ [{x, 0.0}]
    end
  end

  defp ring(rect, outer, inner, color) do
    if color not in [nil, :transparent] do
      clip = %{
        outer
        | path_commands: outer.path_commands ++ inner.path_commands,
          fill_rule: :evenodd
      }

      rect
      |> Map.merge(Map.take(outer, [:x, :y, :width, :height]))
      |> Map.merge(%{
        type: HtmlValidator.reserve_layout_box(:rect),
        role: :shaped_border,
        css_paint_role: :shaped_border,
        clip: clip,
        fill_color: color,
        stroke_color: nil,
        stroke_width: 0.0,
        border_radius: 0.0,
        border_widths: @zero,
        border_colors: @clear,
        snap_to_css_pixel_grid: false
      })
      |> Map.drop([:form_control, :form_owner, :initial_option_id, :border_radii])
    end
  end

  defp clipped_edge(style, rect, side) do
    if rect.border_styles[side] in [:none, :hidden] or rect.border_widths[side] <= 0 do
      []
    else
      horizontal? = side in [:top, :bottom]
      size = if horizontal?, do: rect.width, else: rect.height
      intervals = intervals(style[Map.fetch!(@clip_fields, side)], size, style)
      limits = limit_intervals(style[:border_limit], rect, side, size, horizontal?, style)

      for {a, b} <- intervals,
          {c, d} <- limits,
          start = max(a, c),
          stop = min(b, d),
          stop - start > 1.0e-7 do
        clip =
          if horizontal?,
            do: %{x: rect.x + start, y: rect.y, width: stop - start, height: rect.height},
            else: %{
              x: rect.x,
              y: rect.y + rect.height - stop,
              width: rect.width,
              height: stop - start
            }

        rect
        |> Map.merge(%{
          type: HtmlValidator.reserve_layout_box(:rect),
          role: :clipped_border,
          css_paint_role: :clipped_border,
          fill_color: nil,
          clip: clip,
          border_colors: Map.put(@clear, side, rect.border_colors[side]),
          border_styles:
            Map.new(@sides, &{&1, if(&1 == side, do: rect.border_styles[side], else: :none)})
        })
        |> Map.drop([:form_control, :form_owner, :initial_option_id])
      end
    end
  end

  defp intervals(values, size, style) do
    if values == nil do
      [{0.0, size}]
    else
      parts =
        Enum.map(values, fn
          {:fraction, _} = fraction -> fraction
          value -> distance(value, size, style)
        end)

      parts =
        if Enum.any?(parts, &match?({:fraction, _}, &1)),
          do: parts,
          else: parts ++ [{:fraction, 1.0}]

      {fixed, flexible} =
        Enum.reduce(parts, {0.0, 0.0}, fn
          {:fraction, n}, {fixed, flex} -> {fixed, flex + n}
          n, {fixed, flex} -> {fixed + n, flex}
        end)

      share = if flexible > 0, do: max(size - fixed, 0.0) / flexible, else: 0.0

      {ranges, _} =
        Enum.map_reduce(parts, 0.0, fn part, start ->
          width =
            case part do
              {:fraction, ratio} -> ratio * share
              width -> width
            end

          stop = min(start + width, size)
          {{start, stop}, stop}
        end)

      ranges |> Enum.take_every(2)
    end
  end

  defp limit_intervals(limit, rect, side, size, horizontal?, style) do
    case limit do
      nil ->
        [{0.0, size}]

      {kind, value} ->
        {a, b} = corner_extents(rect, side, horizontal?)

        case kind do
          :sides ->
            middle = max(size - a - b, 0.0)

            length =
              if value == nil, do: middle, else: min(distance(value, middle, style), middle)

            start = a + (middle - length) / 2
            [{start, start + length}]

          :corners ->
            [
              {0.0, corner_limit(value, a, size, style)},
              {size - corner_limit(value, b, size, style), size}
            ]

          ^side ->
            [{0.0, size}]

          kind ->
            if kind in [:top, :bottom] == horizontal? do
              []
            else
              if kind in [:top, :left],
                do: [{0.0, corner_limit(value, a, size, style)}],
                else: [{size - corner_limit(value, b, size, style), size}]
            end
        end
    end
  end

  defp corner_extents(rect, side, horizontal?) do
    {first, last} =
      case side do
        :top -> {:top_left, :top_right}
        :right -> {:top_right, :bottom_right}
        :bottom -> {:bottom_left, :bottom_right}
        :left -> {:top_left, :bottom_left}
      end

    radius = Map.get(rect, :border_radius, 0.0)
    radii = Map.get(rect, :border_radii, %{})
    index = if horizontal?, do: 0, else: 1

    {elem(Map.get(radii, first, {radius, radius}), index),
     elem(Map.get(radii, last, {radius, radius}), index)}
  end

  defp corner_limit(value, corner, size, style) do
    case value do
      nil -> corner
      {:percent, _} -> distance(value, size, style)
      value -> min(corner + distance(value, size, style), size)
    end
  end

  defp distance(value, size, style) do
    case CssLengthValidator.resolve(value, size, style) do
      {:ok, {:percent, percent}} -> max(percent * size, 0.0)
      {:ok, number} -> max(number, 0.0)
      error -> throw({:render_resource_limit, error})
    end
  end

  @doc false
  @spec shadow_contours(map(), map(), map()) :: {list(), list()}
  def shadow_contours(style, rect, shadow) do
    {outer, inner} = contours(style, %{rect | x: 0.0, y: 0.0})
    contour = if shadow.inset, do: inner, else: outer
    dx = if shadow.inset, do: -rect.border_widths.left, else: 0.0
    dy = if shadow.inset, do: -rect.border_widths.bottom, else: 0.0
    spread = if shadow.inset, do: -shadow.spread, else: shadow.spread
    silhouette = resize_path(contour, spread)

    outline = PathGeometry.transform(contour.path_commands, {1, 0, 0, 1, dx, dy})

    silhouette =
      if silhouette.width > 0 and silhouette.height > 0,
        do:
          PathGeometry.transform(
            silhouette.path_commands,
            {1, 0, 0, 1, dx + shadow.x, dy - shadow.y}
          ),
        else: []

    {outline, silhouette}
  end
end
