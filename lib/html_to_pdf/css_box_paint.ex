defmodule NativeElixirPdfUtilities.HtmlToPdf.CssBoxPaint do
  @moduledoc "Static outline and box shadow paint, with blur delegated to the existing SVG converter."
  alias NativeElixirPdfUtilities.HtmlToPdf.{
    BorderShape,
    DrawingGeometry,
    PageGeometry,
    PngDecoder,
    ResourceContext,
    SvgRasterizer
  }

  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  @zero %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}

  @doc "Reports decoration paint that needs a host rectangle even without a fill or border."
  @spec present?(map()) :: boolean()
  def present?(style) do
    case style do
      %{box_shadow: [_ | _]} -> true
      %{box_shadow_offset: [_ | _]} -> true
      %{outline_style: pattern} when pattern != :none -> true
      _ -> false
    end
  end

  @doc "Places outer shadows below background paint, inset shadows above it and outlines after the border."
  @spec boxes(map(), map(), [map()]) :: [map()]
  def boxes(style, rect, artwork) do
    shadows =
      style
      |> NativeElixirPdfUtilities.Validators.CssBoxPaintValidator.computed_shadows()
      |> Enum.reverse()

    {outer, inner} = Enum.split_with(shadows, &(not &1.inset))

    {borders, background} =
      Enum.split_with(
        artwork,
        &(Map.get(&1, :stroke_width, 0) > 0 or
            Map.get(&1, :role) in [:border_image, :shaped_border])
      )

    Enum.flat_map(outer, &shadow(style, rect, &1)) ++
      background ++
      Enum.flat_map(inner, &shadow(style, rect, &1)) ++ borders ++ outline(style, rect)
  end

  defp outline(style, rect) do
    width = Map.get(style, :outline_width, 2.25)
    pattern = Map.get(style, :outline_style, :none)
    color = Map.get(style, :outline_color, :current_color)
    color = if color == :current_color, do: style.color, else: color
    offset = Map.get(style, :outline_offset, 0.0)

    if pattern != :none and width > 0 and color != :transparent do
      offset_x = max(offset, -rect.width / 2)
      offset_y = max(offset, -rect.height / 2)

      box =
        rect
        |> Map.drop([:form_control, :form_owner, :initial_option_id])
        |> Map.merge(%{
          type: HtmlValidator.reserve_layout_box(:rect),
          role: :css_outline,
          css_paint_role: :css_outline,
          x: rect.x - offset_x - width,
          y: rect.y - offset_y - width,
          width: rect.width + 2 * (offset_x + width),
          height: rect.height + 2 * (offset_y + width),
          fill_color: nil,
          stroke_color: color,
          stroke_width: width,
          border_widths: Map.new(@zero, fn {side, _} -> {side, width} end),
          border_colors: Map.new(@zero, fn {side, _} -> {side, color} end),
          border_styles:
            Map.new(@zero, fn {side, _} ->
              {side, if(pattern == :auto, do: :solid, else: pattern)}
            end),
          border_radius:
            if(rect.border_radius > 0, do: max(rect.border_radius + offset + width, 0), else: 0)
        })

      box =
        case Map.get(rect, :border_radii) do
          nil ->
            box

          radii ->
            Map.put(
              box,
              :border_radii,
              Map.new(radii, fn {corner, {x, y}} ->
                {corner, {max(x + offset_x + width, 0), max(y + offset_y + width, 0)}}
              end)
            )
        end

      [box]
    else
      []
    end
  end

  defp shadow(style, rect, shadow) do
    color = if shadow.color == :current_color, do: style.color, else: shadow.color

    if color == :transparent do
      []
    else
      # Gaussian support is three standard deviations, as for text shadows.
      support = 1.5 * shadow.blur + 0.75

      host =
        if shadow.inset, do: PageGeometry.inset_rounded_clip(rect, rect.border_widths), else: rect

      host = %{
        host
        | x: 0.0,
          y: 0.0,
          border_radius: min(host.border_radius, min(host.width, host.height) / 2)
      }

      spread = if shadow.inset, do: -shadow.spread, else: shadow.spread

      silhouette =
        grow(host, spread)
        |> Map.update!(:x, &(&1 + shadow.x))
        |> Map.update!(:y, &(&1 - shadow.y))

      pad = max(abs(shadow.x), abs(shadow.y)) + abs(spread) + support
      left = Float.floor(-pad / 0.75) * 0.75
      bottom = left
      width = Float.ceil((host.width + 2 * pad) / 0.75) * 0.75
      height = Float.ceil((host.height + 2 * pad) / 0.75) * 0.75
      frame = %{x: left, y: bottom, width: width, height: height, border_radius: 0.0}
      frame_path = path(frame)

      {host_path, hole} =
        if BorderShape.shaped?(style) do
          {outline, cast} = BorderShape.shadow_contours(style, rect, shadow)
          {serialize_path(outline), serialize_path(cast)}
        else
          {path(host),
           if(silhouette.width > 0 and silhouette.height > 0, do: path(silhouette), else: "")}
        end

      shape = if shadow.inset, do: frame_path <> " " <> hole, else: hole
      clip = if shadow.inset, do: host_path, else: frame_path <> " " <> host_path

      {r, g, b, alpha} =
        case color do
          {r, g, b} -> {r, g, b, 1.0}
          tuple -> tuple
        end

      svg =
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"#{width / 0.75}\" height=\"#{height / 0.75}\" viewBox=\"#{left} #{bottom} #{width} #{height}\"><defs><filter id=\"blur\" x=\"#{left}\" y=\"#{bottom}\" width=\"#{width}\" height=\"#{height}\" filterUnits=\"userSpaceOnUse\" color-interpolation-filters=\"sRGB\"><feGaussianBlur stdDeviation=\"#{shadow.blur / 2}\"/></filter><clipPath id=\"edge\"><path d=\"#{clip}\" clip-rule=\"evenodd\"/></clipPath></defs><g transform=\"translate(0 #{2 * bottom + height}) scale(1 -1)\" clip-path=\"url(#edge)\"><path d=\"#{shape}\" fill-rule=\"evenodd\" fill=\"rgb(#{r * 255},#{g * 255},#{b * 255})\" opacity=\"#{alpha}\" filter=\"url(#blur)\"/></g></svg>"

      budget =
        case ResourceContext.current() do
          nil -> HtmlValidator.new_image_budget()
          context -> context.image_budget
        end

      with {:ok, png} <- SvgRasterizer.rasterize(svg, [], budget),
           {:ok, image} <- PngDecoder.decode(png, budget, false) do
        inset_x = if shadow.inset, do: rect.border_widths.left, else: 0
        inset_y = if shadow.inset, do: rect.border_widths.bottom, else: 0

        [
          %{
            type: HtmlValidator.reserve_layout_box(:image),
            role: :box_shadow,
            css_paint_role: :box_shadow,
            clip: %{
              x: rect.x + inset_x + left,
              y: rect.y + inset_y + bottom,
              width: width,
              height: height
            },
            paint_layer: :container_background,
            x: rect.x + inset_x + left,
            y: rect.y + inset_y + bottom,
            width: width,
            height: height,
            image: image
          }
        ]
      else
        error -> throw({:render_resource_limit, error})
      end
    end
  end

  defp grow(rect, spread) do
    box = %{
      rect
      | x: rect.x - spread,
        y: rect.y - spread,
        width: max(rect.width + 2 * spread, 0),
        height: max(rect.height + 2 * spread, 0),
        border_radius: spread_radius(rect.border_radius, spread)
    }

    case Map.get(rect, :border_radii) do
      nil ->
        box

      radii ->
        Map.put(
          box,
          :border_radii,
          Map.new(radii, fn {corner, {x, y}} ->
            {corner, {spread_radius(x, spread), spread_radius(y, spread)}}
          end)
        )
    end
  end

  # CSS Backgrounds 3 keeps square corners and sharpens radii smaller
  # than a positive spread with its specified cubic radius correction.
  defp spread_radius(radius, spread) do
    cond do
      radius == 0 -> 0.0
      spread > radius -> radius + spread * (1 - :math.pow(1 - radius / spread, 3))
      true -> max(radius + spread, 0.0)
    end
  end

  defp path(rect) do
    commands =
      case Map.get(rect, :border_radii) do
        nil ->
          DrawingGeometry.rounded_rect_commands(
            rect.x,
            rect.y,
            rect.width,
            rect.height,
            min(rect.border_radius, min(rect.width, rect.height) / 2)
          )

        radii ->
          DrawingGeometry.elliptical_rect_commands(
            rect.x,
            rect.y,
            rect.width,
            rect.height,
            radii,
            4
          )
      end

    serialize_path(commands)
  end

  defp serialize_path(commands) do
    Enum.map_join(commands, " ", fn {operation, coordinates} ->
      Enum.at(["M", "L", "", "C", "Z"], operation) <>
        Enum.map_join(coordinates, " ", &to_string/1)
    end)
  end
end
