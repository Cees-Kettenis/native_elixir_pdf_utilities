defmodule NativeElixirPdfUtilities.Validators.ColorFontValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, Font, PngDecoder}

  alias NativeElixirPdfUtilities.Validators.{
    BitmapFontValidator,
    FontValidator,
    HtmlValidator,
    WriterValidator
  }

  @doc "Expands admitted color text into bounded image artwork with its logical Unicode, then admits the generated writer boxes."
  @spec prepare(map(), keyword()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(context, _opts) do
    color? = Map.get(context, :color_fonts?, false)

    if color? do
      FontValidator.with_budget(fn ->
        budget = HtmlValidator.new_image_budget()

        result =
          Enum.reduce_while(context.pages, {:ok, []}, fn page, {:ok, pages} ->
            case prepare_boxes(page.boxes, elem(page.size, 1), budget) do
              {:ok, boxes} -> {:cont, {:ok, [%{page | boxes: boxes} | pages]}}
              error -> {:halt, error}
            end
          end)

        with {:ok, pages} <- result do
          pages = Enum.reverse(pages)

          HtmlValidator.check_render_resource(
            :max_layout_boxes,
            Enum.reduce(pages, 0, &(length(&1.boxes) + &2)),
            :writer
          )

          {:ok, Map.put(context, :pages, pages)}
        end
      end)
    else
      {:ok, context}
    end
  end

  defp prepare_boxes(boxes, height, budget) do
    Enum.reduce_while(boxes, {:ok, []}, fn box, {:ok, output} ->
      case box do
        %{type: :text, font_face: %{color_tables: [_ | _]} = font} ->
          with {:ok, strikes} <-
                 FontValidator.memo({:bitmap_font, font.data}, fn ->
                   BitmapFontValidator.prepare(font)
                 end),
               {:ok, glyphs} <- glyph_boxes(box, height, strikes, budget) do
            {:cont, {:ok, [glyphs | output]}}
          else
            error -> {:halt, error}
          end

        _ ->
          {:cont, {:ok, [[box] | output]}}
      end
    end)
    |> case do
      {:ok, output} -> {:ok, output |> Enum.reverse() |> List.flatten()}
      error -> error
    end
  end

  defp glyph_boxes(box, page_height, strikes, budget) do
    font = box.font_face

    size =
      case WriterValidator.prepare_text_paint(box, page_height) do
        nil -> Font.used_size(font, box.font_size)
        paint -> paint.size * 0.75
      end

    # The selected strikes were admitted and memoized by prepare_boxes/3.
    # Metric selection reuses those records; work-limit throws retain the scope.
    {:ok, font} = BitmapFontValidator.prepare_metrics(font, size / 0.75)

    box = box |> Map.put(:font_face, font) |> Map.put(:font, Font.pdf_name(font))
    {left, baseline} = DrawingGeometry.text_position(box, page_height)
    baseline = baseline + Font.baseline_offset(font, box.font_size)

    advances =
      if WriterValidator.css_source_text?(box) do
        Font.css_glyph_advances(box.text, font, box.font_size, Map.get(box, :letter_spacing, 0))
      else
        text = Font.shape_ligatures(box.text, font, Map.get(box, :letter_spacing, 0))

        ids =
          Map.get(box, :glyph_ids, Enum.map(String.to_charlist(text), &Map.get(font.cmap, &1, 0)))

        widths = Font.glyph_widths(font, Enum.max(ids, fn -> 0 end))

        next_ids = tl(ids ++ [nil])

        Enum.zip([String.to_charlist(text), ids, next_ids])
        |> Enum.map(fn {unicode, id, next} ->
          kern =
            if Map.has_key?(box, :glyph_ids),
              do: 0,
              else: Map.get(Map.get(font, :kerning, %{}), {id, next}, 0)

          {unicode, id, (elem(widths, id) + kern) * size / font.units_per_em}
        end)
      end

    result =
      Enum.reduce_while(advances, {:ok, [], left}, fn {unicode, glyph, advance},
                                                      {:ok, output, x} ->
        FontValidator.reserve(:max_font_color_work, 1)
        text = <<unicode::utf8>>

        ordinary =
          box
          |> Map.put(:text, text)
          |> Map.put(:x, x)
          |> Map.put(:width, advance)
          |> Map.delete(:color_font_prepared)

        ordinary =
          if Map.has_key?(box, :glyph_ids),
            do: Map.put(ordinary, :glyph_ids, [glyph]),
            else: ordinary

        record_result =
          case BitmapFontValidator.select(strikes, glyph, size / 0.75) do
            nil -> vector_record(font, glyph, box.color, size)
            record -> {:ok, record}
          end

        record =
          case record_result do
            {:ok, record} -> record
            error -> throw({:color_font_error, error})
          end

        if record == nil or record == :empty or size == 0 do
          ordinary =
            if record == :empty, do: Map.put(ordinary, :color, {0, 0, 0, 0}), else: ordinary

          {:cont, {:ok, [[ordinary] | output], x + advance + Map.get(box, :letter_spacing, 0)}}
        else
          with {:ok, image} <- decode(record, budget),
               :ok <- dimensions(image, record.expected),
               {:ok, bounds} <- FontValidator.prepare_glyph_bounds(font) do
            {gx, gy, _, _} = elem(bounds, glyph)
            gx = if record.format == :sbix, do: gx * size / font.units_per_em, else: 0
            gy = if record.format == :sbix, do: gy * size / font.units_per_em, else: 0
            width = Map.get(record, :design_width, image.width_px) * size / record.ppem_x
            height = Map.get(record, :design_height, image.height_px) * size / record.ppem_y

            paint =
              box
              |> Map.take([
                :clip,
                :link_url,
                :link_destination,
                :form_owner,
                :drawing_page_height,
                :initial_select_opacity
              ])
              |> Map.merge(%{
                type: :image,
                x: x + gx + record.x * size / record.ppem_x,
                y: baseline + gy + record.y * size / record.ppem_y,
                width: width,
                height: height,
                css_source_positioned: WriterValidator.css_source_text?(box),
                image: Map.put(image, :interpolate, true)
              })

            glyphs = if record.overlay, do: [paint, ordinary], else: [paint]
            {:cont, {:ok, [glyphs | output], x + advance + Map.get(box, :letter_spacing, 0)}}
          else
            error -> {:halt, error}
          end
        end
      end)

    case result do
      {:ok, output, _} ->
        # Keep the selected text run in PDF text space for selection geometry
        # as well as Unicode. Its transparent paint sits behind the color art;
        # the surrounding ActualText replaces all glyph/layer content once.
        text_geometry = Map.put(box, :color, {0, 0, 0, 0})
        boxes = [text_geometry | output |> Enum.reverse() |> List.flatten()]

        with {:ok, boxes} <- WriterValidator.prepare_color_boxes(boxes, page_height) do
          {:ok,
           [%{type: :actual_text_start, text: box.text}] ++ boxes ++ [%{type: :actual_text_end}]}
        end

      error ->
        error
    end
  catch
    {:color_font_error, error} -> error
  end

  defp vector_record(font, glyph, current, size) do
    color =
      case current do
        {r, g, b} -> {r, g, b, 1}
        {r, g, b, a} -> {r, g, b, a}
      end

    with {:ok, paint} <-
           NativeElixirPdfUtilities.Validators.ColrFontValidator.prepare(font, glyph, color) do
      if paint == nil do
        with {:ok, paint} <-
               NativeElixirPdfUtilities.Validators.SvgFontValidator.prepare(font, glyph, color) do
          case paint do
            nil ->
              {:ok, nil}

            :empty ->
              {:ok, :empty}

            paint ->
              {:ok,
               Map.put(
                 vector_image(font, paint.source, paint.bounds, size, :svg_font),
                 :context,
                 paint.context
               )}
          end
        end
      else
        with {:ok, source, {a, b, c, d}, context} <-
               NativeElixirPdfUtilities.Validators.ColrPaintValidator.prepare(font, paint, size) do
          if source == "" do
            {:ok, :empty}
          else
            {:ok,
             Map.put(vector_image(font, source, {a, b, c, d}, size, :colr), :context, context)}
          end
        end
      end
    end
  end

  defp vector_image(font, source, {a, b, c, d}, size, format) do
    %{
      encoding: :svg,
      data: source,
      x: a,
      y: b,
      expected: nil,
      design_width: c - a,
      design_height: d - b,
      ppem_x: font.units_per_em,
      ppem_y: font.units_per_em,
      format: format,
      overlay: false,
      raster_width: max(1, ceil((c - a) * size / font.units_per_em / 0.75)),
      raster_height: max(1, ceil((d - b) * size / font.units_per_em / 0.75))
    }
  end

  defp decode(record, budget) do
    case record.encoding do
      :svg ->
        with {:ok, png} <-
               NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer.rasterize(
                 record.data,
                 [
                   width: record.raster_width,
                   height: record.raster_height,
                   scale: {:up_to, NativeElixirPdfUtilities.Limits.get(:svg_raster_scale)}
                 ],
                 budget,
                 [],
                 Map.get(record, :context, %{mode: :inline, resources: %{}})
               ),
             {:ok, image} <- PngDecoder.decode(png, budget, true),
             do: {:ok, image}

      :png ->
        PngDecoder.decode(record.data, budget, true)

      :jpeg ->
        case HtmlValidator.jpeg_metadata(record.data) do
          {:ok, metadata} ->
            case HtmlValidator.reserve_decoded_image(
                   budget,
                   metadata.width_px,
                   metadata.height_px,
                   3
                 ) do
              :ok -> :ok
              error -> throw({:color_font_error, error})
            end

            {:ok,
             Map.merge(metadata, %{
               format: :jpeg,
               data: record.data,
               width: metadata.width_px * 0.75,
               height: metadata.height_px * 0.75,
               bits_per_component: 8
             })}

          :error ->
            failure("sbix JPEG glyph is malformed")
        end

      :bgra ->
        {width, height} = record.expected

        case HtmlValidator.reserve_decoded_image(budget, width, height, 4) do
          :ok -> :ok
          error -> throw({:color_font_error, error})
        end

        pixels =
          for <<b, g, r, a <- record.data>>, reduce: {[], []} do
            {colors, alphas} ->
              if max(r, max(g, b)) > a,
                do:
                  throw(
                    {:color_font_error,
                     failure("CBDT BGRA colors must be premultiplied by alpha")}
                  )

              rgb =
                if a == 0,
                  do: <<0, 0, 0>>,
                  else: <<round(r * 255 / a), round(g * 255 / a), round(b * 255 / a)>>

              {[rgb | colors], [<<a>> | alphas]}
          end

        {colors, alpha} = pixels

        {:ok,
         %{
           format: :png,
           data: IO.iodata_to_binary(Enum.reverse(colors)),
           alpha_data: IO.iodata_to_binary(Enum.reverse(alpha)),
           width_px: width,
           height_px: height,
           width: width * 0.75,
           height: height * 0.75,
           color_space: :device_rgb,
           bits_per_component: 8
         }}
    end
  catch
    {:color_font_error, error} -> error
  end

  defp dimensions(image, expected) do
    if expected == nil or expected == {image.width_px, image.height_px},
      do: :ok,
      else: failure("CBDT PNG dimensions disagree with the selected glyph metrics")
  end

  defp failure(message),
    do:
      Diagnostics.error(:font, :invalid_font, message,
        module: __MODULE__,
        operation: :paint_color_font,
        source: "color glyph"
      )
end
