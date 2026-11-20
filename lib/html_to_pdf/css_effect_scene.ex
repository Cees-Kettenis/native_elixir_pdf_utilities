defmodule NativeElixirPdfUtilities.HtmlToPdf.CssEffectScene do
  @moduledoc "Bounded static CSS effects over the existing native paint instructions."
  alias NativeElixirPdfUtilities.HtmlToPdf.{
    DrawingGeometry,
    Font,
    PngDecoder,
    ResourceContext,
    SvgRasterizer,
    TextInk
  }

  alias NativeElixirPdfUtilities.Validators.{
    CssCompositingValidator,
    CssEffectsValidator,
    HtmlValidator,
    WriterValidator
  }

  @doc "Prepares effect groups while preserving native logical text and annotations."
  @spec prepare(term(), keyword()) :: {:ok, term()} | {:error, {atom(), map()}}
  def prepare(pages, opts) do
    with {:ok, active} <- WriterValidator.effect_scene(pages) do
      if active do
        with {:ok, _context} <- WriterValidator.prepare(pages, opts) do
          Enum.reduce_while(pages, {:ok, []}, fn page, {:ok, pages} ->
            case prepare_boxes(page.boxes, page.size, opts, []) do
              {:ok, boxes} -> {:cont, {:ok, [%{page | boxes: boxes} | pages]}}
              error -> {:halt, error}
            end
          end)
          |> case do
            {:ok, pages} -> {:ok, Enum.reverse(pages)}
            error -> error
          end
        end
      else
        {:ok, pages}
      end
    end
  end

  defp prepare_boxes(boxes, size, opts, preceding) do
    Enum.reduce_while(boxes, {:ok, []}, fn box, {:ok, acc} ->
      result =
        case box do
          %{type: :drawing_group, css_projection: _} ->
            NativeElixirPdfUtilities.HtmlToPdf.ProjectionScene.prepare(box, size, opts)

          %{type: :drawing_group, boxes: children} ->
            child_size =
              if Map.get(box, :role) in [:css_transform_plane, :overflow_clip],
                do: size,
                else: {box.width, box.height}

            with {:ok, children} <-
                   prepare_boxes(children, child_size, opts, preceding ++ Enum.reverse(acc)),
                 {:ok, box} <-
                   effect(
                     %{box | boxes: children},
                     child_size,
                     opts,
                     preceding ++ Enum.reverse(acc)
                   ),
                 do: {:ok, box}

          _ ->
            {:ok, box}
        end

      case result do
        {:ok, box} -> {:cont, {:ok, [box | acc]}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, boxes} -> {:ok, Enum.reverse(boxes)}
      error -> error
    end
  end

  defp effect(box, size, opts, backdrop) do
    case Map.get(box, :css_effect) do
      nil ->
        {:ok, box}

      effect ->
        with :ok <- CssEffectsValidator.validate_group(effect),
             :ok <- CssEffectsValidator.validate_artwork([box], opts),
             {:ok, context} <- WriterValidator.prepare_paint_plane(box.boxes, size),
             {:ok, context} <-
               NativeElixirPdfUtilities.Validators.ColorFontValidator.prepare(context, opts),
             [page] = context.pages,
             {:ok, region} <-
               CssEffectsValidator.region(
                 effect.bounds,
                 page.boxes,
                 elem(size, 1),
                 effect.filters
               ),
             {:ok, foreground} <- render(page.boxes, region, effect.filters, effect, size),
             {:ok, background} <- backdrop(backdrop, region, effect, size),
             {:ok, paint} <- mask_paint(foreground, background, effect, region, size) do
          logical = logical_boxes(box.boxes)

          logical =
            case Map.get(effect, :clip) do
              nil ->
                logical

              clip ->
                [
                  %{
                    type: :drawing_group,
                    width: elem(size, 0),
                    height: elem(size, 1),
                    x: 0.0,
                    y: 0.0,
                    transform: {1, 0, 0, 1, 0, 0},
                    clip: clip,
                    boxes: logical
                  }
                ]
            end

          {:ok,
           box
           |> Map.delete(:css_effect)
           |> Map.delete(:clip)
           |> Map.put(:boxes, paint ++ logical)}
        end
    end
  end

  # Masking follows filter/backdrop composition once for the entire element.
  defp mask_paint(foreground, background, effect, region, size) do
    if NativeElixirPdfUtilities.Validators.CssMaskValidator.active?(Map.get(effect, :style, %{})) do
      with {:ok, combined} <-
             if(background == [],
               do: {:ok, foreground},
               else:
                 render(
                   background ++ [foreground],
                   region,
                   [],
                   %{definitions: "", color: {0, 0, 0}},
                   size
                 )
             ),
           {:ok, image} <-
             NativeElixirPdfUtilities.HtmlToPdf.CssMask.apply(
               combined.image,
               effect,
               region,
               size
             ) do
        {:ok,
         [
           combined
           |> Map.put(:image, image)
           |> Map.put(:css_compositing_scopes, foreground.css_compositing_scopes)
         ]}
      end
    else
      {:ok, background ++ [foreground]}
    end
  end

  defp backdrop(boxes, region, effect, size) do
    if effect.backdrop == [] do
      {:ok, []}
    else
      bounds = effect.bounds

      clip = %{
        x: bounds.x,
        y: bounds.top - bounds.height,
        width: bounds.width,
        height: bounds.height
      }

      with {:ok, context} <- WriterValidator.prepare_paint_plane(boxes, size),
           {:ok, image} <- render(hd(context.pages).boxes, region, effect.backdrop, effect, size),
           do: {:ok, [Map.put(image, :clip, clip)]}
    end
  end

  @doc "Renders admitted native paint at the configured SVG sampling scale through the existing converter."
  @spec render([map()], map(), list(), map(), tuple()) :: {:ok, map()} | {:error, {atom(), map()}}
  def render(boxes, region, operations, effect, {_width, height}) do
    prefix = Map.get_lazy(effect, :compositing_prefix, fn -> common_scopes(boxes) end)

    with {:ok, artwork, resources} <- svg_boxes(boxes, height, prefix),
         {:ok, primitives, filter} <- filters(operations, effect) do
      # Paint coordinates remain PDF points. The outer reflection presents SVG's
      # top-down filter axes without changing native glyph or image coordinates.
      {clip_defs, clip_attribute} =
        case Map.get(effect, :clip) do
          nil ->
            {"", ""}

          clip ->
            commands =
              NativeElixirPdfUtilities.HtmlToPdf.PathGeometry.transform(
                clip.path_commands,
                {1, 0, 0, -1, 0, 0}
              )

            rule = if clip.fill_rule == :evenodd, do: "evenodd", else: "nonzero"

            {"<clipPath id=\"css-source-clip\"><path d=\"#{path(commands)}\" clip-rule=\"#{rule}\"/></clipPath>",
             " clip-path=\"url(#css-source-clip)\""}
        end

      svg =
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"#{region.width / 0.75}\" height=\"#{region.height / 0.75}\" viewBox=\"#{region.x} #{-region.y - region.height} #{region.width} #{region.height}\"><defs>#{effect.definitions}#{primitives}#{clip_defs}</defs><g#{clip_attribute}><g filter=\"#{filter}\"><g transform=\"scale(1 -1)\">#{artwork}</g></g></g></svg>"

      budget =
        case ResourceContext.current() do
          nil -> HtmlValidator.new_image_budget()
          context -> context.image_budget
        end

      with {:ok, png} <-
             SvgRasterizer.rasterize(
               svg,
               [scale: {:up_to, NativeElixirPdfUtilities.Limits.get(:svg_raster_scale)}],
               budget,
               [],
               %{mode: :inline, resources: Map.merge(Map.get(effect, :resources, %{}), resources)}
             ),
           {:ok, image} <- PngDecoder.decode(png, budget, false) do
        {:ok,
         Map.merge(region, %{
           type: HtmlValidator.reserve_layout_box(:image),
           image: Map.put(image, :interpolate, true),
           css_source_positioned: true,
           css_compositing_scopes: prefix
         })}
      end
    end
  end

  @doc "Returns the common admitted compositing ancestry of one normalized source paint plane."
  @spec common_scopes([map()]) :: list()
  def common_scopes(boxes) do
    boxes =
      Enum.flat_map(boxes, fn box ->
        case box do
          %{type: type, boxes: children} when type in [:drawing_group, :glyph_group] ->
            case common_scopes(children) do
              [] -> [%{css_compositing_scopes: []}]
              scopes -> [%{css_compositing_scopes: scopes}]
            end

          %{type: type}
          when type in [
                 :drawing_group_start,
                 :drawing_group_end,
                 :actual_text_start,
                 :actual_text_end,
                 :flow_marker
               ] ->
            []

          _ ->
            [box]
        end
      end)

    case boxes do
      [] ->
        []

      [first | rest] ->
        Enum.reduce(rest, Map.get(first, :css_compositing_scopes, []), fn box, prefix ->
          Enum.zip(prefix, Map.get(box, :css_compositing_scopes, []))
          |> Enum.take_while(fn {a, b} -> a == b end)
          |> Enum.map(&elem(&1, 0))
        end)
    end
  end

  defp filters(operations, effect) do
    Enum.reduce_while(Enum.with_index(operations), {:ok, [], "SourceGraphic"}, fn {{name, value},
                                                                                   index},
                                                                                  {:ok, defs,
                                                                                   input} ->
      output = "f#{index}"

      primitive =
        case name do
          "blur" ->
            "<feGaussianBlur in=\"#{input}\" stdDeviation=\"#{value}\" result=\"#{output}\"/>"

          "drop-shadow" ->
            color = if value.color == :current_color, do: effect.color, else: value.color

            "<feDropShadow in=\"#{input}\" dx=\"#{value.x}\" dy=\"#{value.y}\" stdDeviation=\"#{value.blur}\" flood-color=\"#{color(color)}\" result=\"#{output}\"/>"

          "grayscale" ->
            "<feColorMatrix in=\"#{input}\" type=\"saturate\" values=\"#{1 - value}\" result=\"#{output}\"/>"

          "saturate" ->
            "<feColorMatrix in=\"#{input}\" type=\"saturate\" values=\"#{value}\" result=\"#{output}\"/>"

          "hue-rotate" ->
            "<feColorMatrix in=\"#{input}\" type=\"hueRotate\" values=\"#{value * 180 / :math.pi()}\" result=\"#{output}\"/>"

          "sepia" ->
            matrix = [
              1 - 0.607 * value,
              0.769 * value,
              0.189 * value,
              0,
              0,
              0.349 * value,
              1 - 0.314 * value,
              0.168 * value,
              0,
              0,
              0.272 * value,
              0.534 * value,
              1 - 0.869 * value,
              0,
              0,
              0,
              0,
              0,
              1,
              0
            ]

            "<feColorMatrix in=\"#{input}\" values=\"#{Enum.join(matrix, " ")}\" result=\"#{output}\"/>"

          "url" ->
            nil

          _ ->
            {slope, intercept, channels} =
              case name do
                "brightness" -> {value, 0, ~w(R G B)}
                "contrast" -> {value, 0.5 - 0.5 * value, ~w(R G B)}
                "invert" -> {1 - 2 * value, value, ~w(R G B)}
                "opacity" -> {value, 0, ["A"]}
              end

            functions =
              Enum.map_join(
                channels,
                &"<feFunc#{&1} type=\"linear\" slope=\"#{slope}\" intercept=\"#{intercept}\"/>"
              )

            "<feComponentTransfer in=\"#{input}\" result=\"#{output}\">#{functions}</feComponentTransfer>"
        end

      if primitive == nil do
        if length(operations) == 1 and
             String.contains?(effect.definitions, "id=\"#{escape(value)}\""),
           do: {:halt, {:reference, "url(##{value})"}},
           else:
             {:halt,
              CssEffectsValidator.unsupported(
                "Referenced HTML filters require an owned same-document filter graph; chained referenced filters require a separate nested element",
                "filter url(##{value})"
              )}
      else
        {:cont, {:ok, [primitive | defs], output}}
      end
    end)
    |> case do
      {:ok, [], _} ->
        {:ok, "", "none"}

      {:ok, defs, _} ->
        {:ok,
         "<filter id=\"css-effect\" x=\"-100%\" y=\"-100%\" width=\"300%\" height=\"300%\" color-interpolation-filters=\"sRGB\">#{Enum.reverse(defs)}</filter>",
         "url(#css-effect)"}

      {:reference, filter} ->
        {:ok, "", filter}

      error ->
        error
    end
  end

  defp svg_boxes(boxes, height, prefix) do
    Enum.reduce_while(boxes, {:ok, [], [], %{}}, fn box, {:ok, output, open, resources} ->
      scopes = Map.get(box, :css_compositing_scopes, []) |> Enum.drop(length(prefix))
      common = Enum.zip(open, scopes) |> Enum.take_while(fn {a, b} -> a == b end) |> length()
      close = List.duplicate("</g>", length(open) - common)

      start =
        scopes
        |> Enum.drop(common)
        |> Enum.map(fn scope ->
          "<g opacity=\"#{scope.opacity}\" style=\"isolation:isolate;mix-blend-mode:#{CssCompositingValidator.keyword(scope.mix_blend_mode)}\">"
        end)

      result =
        case paint(box, height) do
          {:ok, piece} -> {:ok, piece, %{}}
          other -> other
        end

      case result do
        {:ok, piece, added} ->
          {:cont, {:ok, [output, close, start, piece], scopes, Map.merge(resources, added)}}

        error ->
          {:halt, error}
      end
    end)
    |> case do
      {:ok, output, open, resources} ->
        {:ok, IO.iodata_to_binary([output, List.duplicate("</g>", length(open))]), resources}

      error ->
        error
    end
  end

  defp paint(box, height) do
    HtmlValidator.reserve_render_resource(:max_work, 1, :writer)

    case box do
      %{type: :drawing_group_start, transform: matrix, clip: clip} ->
        transform = matrix |> Tuple.to_list() |> Enum.join(" ")
        {:ok, "<g transform=\"matrix(#{transform})\">" <> clipped("", clip, true)}

      %{type: :drawing_group_end} ->
        {:ok, "</g></g>"}

      %{type: type}
      when type in [:actual_text_start, :actual_text_end, :link_region, :flow_marker] ->
        {:ok, ""}

      %{type: :text, font_face: %{type: :embedded}} ->
        with {:ok, ink} <- TextInk.measure([%{text: box.text, box: box, style: box}], :paths) do
          y =
            case WriterValidator.prepare_text_paint(box, height) do
              nil ->
                box.y

              paint ->
                height - paint.y * 0.75 - Font.baseline_offset(box.font_face, box.font_size)
            end

          paths = Enum.map_join(ink.glyphs, &"<path d=\"#{path(&1.commands)}\"/>")

          stroke =
            if Map.get(box, :text_stroke_width, 0) > 0,
              do:
                " stroke=\"#{color(Map.get(box, :text_stroke_color, box.color))}\" stroke-width=\"#{box.text_stroke_width}\" paint-order=\"stroke fill\"",
              else: ""

          {:ok,
           clipped(
             "<g transform=\"translate(#{box.x} #{y})\" fill=\"#{color(box.color)}\"#{stroke}>#{paths}</g>",
             Map.get(box, :clip)
           )}
        end

      %{type: :text} ->
        CssEffectsValidator.unsupported(
          "HTML filter text requires selected embedded glyph outlines; choose an embedded font",
          "filter text"
        )

      %{type: :rect, shape: :wave} ->
        paint = WriterValidator.prepare_wave(box, height)
        canvas = DrawingGeometry.css_print_canvas(height)
        scale = canvas.point_scale * canvas.css_scale

        commands =
          NativeElixirPdfUtilities.HtmlToPdf.TextDecorationWave.commands(paint)
          |> Enum.map(fn {kind, values} -> {if(kind == :move, do: 0, else: 3), values} end)
          |> NativeElixirPdfUtilities.HtmlToPdf.PathGeometry.transform(
            {scale, 0, 0, -scale, 0, height}
          )

        markup =
          "<path d=\"#{path(commands)}\" fill=\"none\" stroke=\"#{color(box.stroke_color)}\" stroke-width=\"#{paint.thickness * scale}\"/>"

        {:ok,
         clipped(clipped(markup, Map.take(box, [:x, :y, :width, :height])), Map.get(box, :clip))}

      %{type: :rect} ->
        box =
          if Map.get(box, :snap_to_css_pixel_grid, false),
            do:
              NativeElixirPdfUtilities.HtmlToPdf.PageGeometry.snap_box_to_css_pixel_grid(%{
                box
                | y: height - box.y - box.height
              })
              |> then(&%{&1 | y: height - &1.y - &1.height}),
            else: box

        marker = Map.get(box, :shape, :rectangle) != :rectangle

        commands =
          if marker do
            DrawingGeometry.marker_commands(box)
          else
            case Map.get(box, :border_radii) do
              nil ->
                DrawingGeometry.rounded_rect_commands(
                  box.x,
                  box.y,
                  box.width,
                  box.height,
                  min(Map.get(box, :border_radius, 0), min(box.width, box.height) / 2)
                )

              radii ->
                DrawingGeometry.elliptical_rect_commands(
                  box.x,
                  box.y,
                  box.width,
                  box.height,
                  radii,
                  4
                )
            end
          end

        stroke =
          if marker and box.stroke_width > 0,
            do: " stroke=\"#{color(box.stroke_color)}\" stroke-width=\"#{box.stroke_width}\"",
            else: ""

        fill = "<path d=\"#{path(commands)}\" fill=\"#{color(box.fill_color)}\"#{stroke}/>"

        borders =
          if not marker and box.stroke_width > 0 and box.stroke_color != nil do
            widths =
              Map.get(
                box,
                :border_widths,
                Map.new([:top, :right, :bottom, :left], &{&1, box.stroke_width})
              )

            Enum.map_join(widths, fn {side, w} ->
              {x, y, width, h} =
                case side do
                  :top -> {box.x, box.y + box.height - w, box.width, w}
                  :bottom -> {box.x, box.y, box.width, w}
                  :left -> {box.x, box.y, w, box.height}
                  :right -> {box.x + box.width - w, box.y, w, box.height}
                end

              paint = Map.get(Map.get(box, :border_colors, %{}), side, box.stroke_color)

              "<rect x=\"#{x}\" y=\"#{y}\" width=\"#{width}\" height=\"#{h}\" fill=\"#{color(paint)}\"/>"
            end)
          else
            ""
          end

        {:ok, clipped(fill <> borders, Map.get(box, :clip))}

      %{type: :image, image: %{format: format} = image} when format in [:png, :jpeg] ->
        {data, descriptor} = image_source(image)

        orientation =
          NativeElixirPdfUtilities.HtmlToPdf.CssImageValue.orientation_matrix(
            Map.get(image, :image_orientation, 1)
          )
          |> Enum.join(" ")

        markup =
          "<g transform=\"translate(#{box.x} #{box.y}) scale(#{box.width} #{box.height}) matrix(#{orientation})\"><image x=\"0\" y=\"-1\" width=\"1\" height=\"1\" preserveAspectRatio=\"none\" href=\"#{data}\" transform=\"scale(1 -1)\"/></g>"

        {:ok, clipped(markup, Map.get(box, :clip)), %{data => descriptor}}

      _ ->
        CssEffectsValidator.unsupported(
          "This filter paint instruction cannot be rasterized without losing its source semantics",
          "filter #{inspect(Map.get(box, :type))}"
        )
    end
  end

  defp image_source(image) do
    name = "svg-resource:effect-#{System.unique_integer([:positive])}"

    if image.format == :jpeg do
      {name, %{data: image.data, media_type: "image/jpeg"}}
    else
      colors = if image.color_space == :device_gray, do: 1, else: 3
      depth = image.bits_per_component
      sample_bytes = div(depth, 8)
      color_bytes = colors * sample_bytes
      alpha = Map.get(image, :alpha_data)
      channels = colors + if(alpha, do: 1, else: 0)
      count = image.width_px * image.height_px

      HtmlValidator.reserve_render_resource(
        :max_decoded_bytes,
        count * channels * sample_bytes,
        :writer
      )

      pixels =
        if alpha do
          for index <- 0..(count - 1), into: <<>> do
            binary_part(image.data, index * color_bytes, color_bytes) <>
              binary_part(alpha, index * sample_bytes, sample_bytes)
          end
        else
          image.data
        end

      row_bytes = image.width_px * channels * sample_bytes
      rows = for <<row::binary-size(^row_bytes) <- pixels>>, do: [0, row]

      color_type =
        case {colors, alpha != nil} do
          {1, false} -> 0
          {1, true} -> 4
          {3, false} -> 2
          {3, true} -> 6
        end

      chunk = fn name, data ->
        <<byte_size(data)::32, name::binary, data::binary, :erlang.crc32(name <> data)::32>>
      end

      png =
        [
          <<137, 80, 78, 71, 13, 10, 26, 10>>,
          chunk.("IHDR", <<image.width_px::32, image.height_px::32, depth, color_type, 0, 0, 0>>),
          chunk.("IDAT", :zlib.compress(IO.iodata_to_binary(rows))),
          chunk.("IEND", "")
        ]
        |> IO.iodata_to_binary()

      HtmlValidator.reserve_render_resource(:max_output_bytes, byte_size(png), :writer)
      {name, %{data: png, media_type: "image/png"}}
    end
  end

  defp clipped(markup, clip, open_only \\ false) do
    prefix =
      if clip == nil do
        "<g>"
      else
        id = "clip-#{System.unique_integer([:positive])}"

        clip_definitions(clip, id) <> "<g clip-path=\"url(##{id})\">"
      end

    if open_only, do: prefix <> markup, else: prefix <> markup <> "</g>"
  end

  defp clip_definitions(clip, id) do
    commands =
      Map.get_lazy(clip, :path_commands, fn ->
        case Map.get(clip, :border_radii) do
          nil ->
            DrawingGeometry.rounded_rect_commands(
              clip.x,
              clip.y,
              clip.width,
              clip.height,
              Map.get(clip, :border_radius, 0)
            )

          radii ->
            DrawingGeometry.elliptical_rect_commands(
              clip.x,
              clip.y,
              clip.width,
              clip.height,
              radii,
              4
            )
        end
      end)

    {definitions, attribute} =
      case Map.get(clip, :inner_clip) do
        nil -> {"", ""}
        inner -> {clip_definitions(inner, id <> "-inner"), " clip-path=\"url(##{id}-inner)\""}
      end

    rule = if Map.get(clip, :fill_rule) == :evenodd, do: "evenodd", else: "nonzero"

    definitions <>
      "<defs><clipPath id=\"#{id}\"><path d=\"#{path(commands)}\" clip-rule=\"#{rule}\"#{attribute}/></clipPath></defs>"
  end

  @doc "Serializes admitted shared move, line, cubic and close drawing commands as SVG path data."
  @spec path([{integer(), [number()]}]) :: binary()
  def path(commands),
    do:
      Enum.map_join(commands, " ", fn {operation, coordinates} ->
        Enum.at(["M", "L", "", "C", "Z"], operation) <> Enum.join(coordinates, " ")
      end)

  defp color(value) do
    case value do
      nil -> "none"
      {r, g, b} -> "rgb(#{r * 255},#{g * 255},#{b * 255})"
      {r, g, b, a} -> "rgba(#{r * 255},#{g * 255},#{b * 255},#{a})"
    end
  end

  defp escape(text),
    do:
      text
      |> String.replace("&", "&amp;")
      |> String.replace("\"", "&quot;")
      |> String.replace("<", "&lt;")
      |> String.replace(">", "&gt;")

  defp logical_boxes(boxes) do
    Enum.flat_map(boxes, fn box ->
      case box do
        %{type: type, boxes: children} when type in [:drawing_group, :glyph_group] ->
          [%{box | boxes: logical_boxes(children)}]

        %{type: :text} ->
          [
            box
            |> Map.put(:color, {0.0, 0.0, 0.0, 0.0})
            |> Map.delete(:text_stroke_color)
            |> Map.delete(:text_stroke_width)
          ]

        %{type: :image} ->
          if Map.has_key?(box, :link_url) or Map.has_key?(box, :link_destination),
            do: [Map.drop(box, [:image]) |> Map.put(:type, :link_region)],
            else: []

        %{type: :rect} ->
          if Map.has_key?(box, :link_url) or Map.has_key?(box, :link_destination),
            do: [%{box | fill_color: nil, stroke_color: nil, stroke_width: 0}],
            else: []

        _ ->
          [box]
      end
    end)
  end
end
