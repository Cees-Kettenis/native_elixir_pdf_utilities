defmodule NativeElixirPdfUtilities.HtmlToPdf.TextShadowBlur do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.{
    PngDecoder,
    ResourceContext,
    SvgRasterizer,
    TextInk,
    TextDecorationWave,
    Visibility
  }

  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Blurs selected glyph outlines and their decorations through the existing bounded SVG converter."
  @spec boxes(map(), map(), map(), term(), [map()], map()) :: [map()]
  def boxes(box, style, shadow, color, decorations, metadata) do
    with {:ok, ink} <-
           TextInk.measure(
             [
               %{
                 text: box.text,
                 box: box,
                 style:
                   Map.merge(
                     style,
                     Map.take(box, [:font_face, :font_size, :letter_spacing, :word_spacing])
                   )
               }
             ],
             :paths
           ) do
      points =
        ink.points ++
          Enum.flat_map(decorations, fn line ->
            [
              {line.x - box.x, line.y - box.y},
              {line.x + line.width - box.x, line.y + line.height - box.y}
            ]
          end)

      if points == [] do
        []
      else
        # Gaussian support uses three standard deviations, a fixed paint
        # extent rather than a configurable resource ceiling.
        pad = 1.5 * shadow.blur + 0.75
        left = Float.floor((Enum.min(Enum.map(points, &elem(&1, 0))) - pad) / 0.75) * 0.75
        right = Float.ceil((Enum.max(Enum.map(points, &elem(&1, 0))) + pad) / 0.75) * 0.75
        bottom = Float.floor((Enum.min(Enum.map(points, &elem(&1, 1))) - pad) / 0.75) * 0.75
        top = Float.ceil((Enum.max(Enum.map(points, &elem(&1, 1))) + pad) / 0.75) * 0.75
        width = right - left
        height = top - bottom

        {r, g, b, alpha} =
          case color do
            {r, g, b} -> {r, g, b, 1}
            {r, g, b, a} -> {r, g, b, a}
          end

        paint = "rgb(#{r * 255},#{g * 255},#{b * 255})"

        glyphs =
          Enum.map_join(ink.glyphs, fn glyph -> "<path d=\"#{path(glyph.commands)}\"/>" end)

        lines =
          Enum.map_join(Enum.with_index(decorations), fn {line, index} ->
            x = line.x - box.x
            y = line.y - box.y

            case Map.get(line, :shape) do
              :wave ->
                geometry = TextDecorationWave.geometry(line.stroke_width)
                period = 2 * geometry.step
                offset = Map.get(line, :wave_offset, 0.0) / 0.75

                prepared =
                  Map.merge(geometry, %{
                    x: x / 0.75,
                    y: -(y + line.height) / 0.75,
                    width: line.width / 0.75,
                    height: line.height / 0.75,
                    count: ceil(line.width / 0.75 / period) + 1,
                    phase: offset - floor(offset / period) * period
                  })

                HtmlValidator.reserve_render_resource(:max_entries, prepared.count + 2, :layout)

                commands =
                  Enum.map(TextDecorationWave.commands(prepared), fn {operation, coords} ->
                    operation = if operation == :move, do: 0, else: 3

                    {operation,
                     Enum.flat_map(Enum.chunk_every(coords, 2), fn [x, y] ->
                       [x * 0.75, -y * 0.75]
                     end)}
                  end)

                "<defs><clipPath id=\"w#{index}\"><rect x=\"#{x}\" y=\"#{y}\" width=\"#{line.width}\" height=\"#{line.height}\"/></clipPath></defs><path d=\"#{path(commands)}\" fill=\"none\" stroke=\"#{paint}\" stroke-width=\"#{line.stroke_width}\" clip-path=\"url(#w#{index})\"/>"

              _ ->
                pattern = get_in(line, [:border_styles, :bottom])

                case NativeElixirPdfUtilities.HtmlToPdf.CssBorderArtwork.open_pattern(
                       pattern,
                       line.height,
                       line.width
                     ) do
                  nil ->
                    "<rect x=\"#{x}\" y=\"#{y}\" width=\"#{line.width}\" height=\"#{line.height}\"/>"

                  pattern ->
                    inset = if pattern.cap == :round, do: line.height / 2, else: 0.0
                    center = y + line.height / 2 + 0.75 * 0.75 / 2

                    "<path d=\"M#{x + inset} #{center} L#{x + line.width - inset} #{center}\" fill=\"none\" stroke=\"#{paint}\" stroke-width=\"#{line.height}\" stroke-dasharray=\"#{pattern.dash} #{pattern.gap}\" stroke-linecap=\"#{if pattern.cap == :round, do: "round", else: "butt"}\"/>"
                end
            end
          end)

        svg =
          "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"#{width / 0.75}\" height=\"#{height / 0.75}\" viewBox=\"#{left} #{bottom} #{width} #{height}\"><defs><filter id=\"s\" filterUnits=\"userSpaceOnUse\" x=\"#{left}\" y=\"#{bottom}\" width=\"#{width}\" height=\"#{height}\" color-interpolation-filters=\"sRGB\"><feGaussianBlur stdDeviation=\"#{shadow.blur / 2}\"/></filter></defs><g transform=\"translate(0 #{top + bottom}) scale(1 -1)\"><g filter=\"url(#s)\" fill=\"#{paint}\" opacity=\"#{alpha}\">#{glyphs}#{lines}</g></g></svg>"

        budget =
          case ResourceContext.current() do
            nil -> HtmlValidator.new_image_budget()
            context -> context.image_budget
          end

        with {:ok, png} <- SvgRasterizer.rasterize(svg, [], budget),
             {:ok, image} <- PngDecoder.decode(png, budget, false) do
          [
            %{
              type: HtmlValidator.reserve_layout_box(:image),
              image: image,
              x: box.x + shadow.x + left,
              y: box.y - shadow.y + bottom,
              width: width,
              height: height,
              role: :text_shadow,
              text_baseline_offset: bottom - shadow.y
            }
            |> Visibility.mark(style)
            |> Map.merge(metadata)
          ]
        else
          error -> throw({:render_resource_limit, error})
        end
      end
    else
      error -> throw({:render_resource_limit, error})
    end
  end

  defp path(commands) do
    Enum.map_join(commands, " ", fn {operation, coordinates} ->
      Enum.at(["M", "L", "", "C", "Z"], operation) <>
        Enum.map_join(coordinates, " ", &to_string/1)
    end)
  end
end
