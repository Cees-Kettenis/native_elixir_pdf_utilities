defmodule NativeElixirPdfUtilities.Validators.WriterValidator do
  @moduledoc """
  Semantic validation and normalization for the low-level PDF writer.

  Successful preparation returns pages and normalized metadata that the byte
  writer can serialize without revalidating the page model.
  """

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Validators.FontValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.InfoValidator
  alias NativeElixirPdfUtilities.Validators.OutlineValidator
  alias NativeElixirPdfUtilities.Validators.PdfFormValidator

  @border_styles [
    :none,
    :hidden,
    :dotted,
    :dashed,
    :solid,
    :double,
    :groove,
    :ridge,
    :inset,
    :outset
  ]
  @built_in_fonts [
    "Courier",
    "Courier-Bold",
    "Courier-Oblique",
    "Courier-BoldOblique",
    "Helvetica",
    "Helvetica-Bold",
    "Helvetica-Oblique",
    "Helvetica-BoldOblique",
    "Times-Roman",
    "Times-Bold",
    "Times-Italic",
    "Times-BoldItalic"
  ]
  @writer_font_fields [
    :data,
    :pdf_name,
    :units_per_em,
    :widths,
    :default_width,
    :cmap,
    :ascent,
    :descent,
    :bbox
  ]

  @typedoc "Validated input consumed by the PDF byte writer."
  @type context :: %{
          required(:pages) => [map()],
          required(:metadata) => map(),
          required(:outlines) => [OutlineValidator.item()],
          required(:compress_streams) => boolean(),
          required(:subset_fonts) => boolean()
        }

  @doc false
  @spec use_compressed_stream?(non_neg_integer(), non_neg_integer(), non_neg_integer()) ::
          boolean()
  def use_compressed_stream?(source_bytes, compressed_bytes, minimum_size) do
    source_bytes >= minimum_size and
      (minimum_size == 0 or compressed_bytes + byte_size(" /Filter /FlateDecode") < source_bytes) and
      source_bytes <= compressed_bytes * Limits.get(:max_pdf_decompression_ratio)
  end

  @doc "Prepares the genuine top-down CSS print canvas and bounded curved stroke contours from validated writer geometry."
  @spec prepare_curved_border_paint(map(), number()) :: map()
  def prepare_curved_border_paint(box, page_height) do
    geometry = PageGeometry.curved_border_geometry(box, page_height)
    canvas = DrawingGeometry.css_print_canvas(page_height)

    Enum.each([geometry.outer, geometry.inner, geometry.center], fn contour ->
      values = Map.take(contour, [:x, :y, :width, :height, :radius]) |> Map.values()
      values = values ++ (contour.radii |> Map.values() |> Enum.flat_map(&Tuple.to_list/1))

      Enum.each(values, fn value ->
        HtmlValidator.check_render_resource(:max_pdf_numeric_magnitude, abs(value), :writer)
      end)
    end)

    Map.put(geometry, :canvas, canvas)
  end

  @doc "Prepares conservative resource bounds for validated source paint, preserving negative local artwork without changing the canvas or primitives."
  @spec prepare_paint_group_bounds(map(), [map()]) ::
          {:ok, tuple()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_paint_group_bounds(size, boxes) do
    bounds =
      Enum.reduce(boxes, {0.0, 0.0, size.width, size.height}, fn box,
                                                                 {left, bottom, right, top} =
                                                                   bounds ->
        case box do
          %{x: x, y: y, width: width} when is_number(x) and is_number(y) and is_number(width) ->
            height =
              Map.get(box, :height, Map.get(box, :line_height, Map.get(box, :font_size, 0) * 1.2))

            {min(left, x), min(bottom, y), max(right, x + width), max(top, y + height)}

          _ ->
            bounds
        end
      end)

    if Enum.all?(Tuple.to_list(bounds), &bounded_number?/1),
      do: {:ok, bounds},
      else: drawing_group_error("paint group bounds must remain within max_pdf_numeric_magnitude")
  end

  @doc """
  Validates pages and writer options and normalizes document metadata.
  """
  @spec prepare(term(), term()) ::
          {:ok, context()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare(pages, opts) do
    HtmlValidator.with_render_budget(fn -> prepare(pages, opts, :pages) end)
  end

  @doc "Validates ordinary PDF appearance placement and clipping before native Form composition."
  @spec prepare_appearance_projection(term(), term(), term(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_appearance_projection(appearance, matrix, polygons, bounds) do
    HtmlValidator.with_render_budget(fn ->
      case {appearance, matrix, polygons, bounds} do
        {%{size: {width, height}, boxes: boxes}, {_a, _b, _c, _d, _e, _f}, polygons,
         {left, bottom, right, top}}
        when is_list(boxes) and is_list(polygons) and is_number(width) and width > 0 and
               is_number(height) and height > 0 and is_number(left) and is_number(bottom) and
               is_number(right) and is_number(top) and right > left and top > bottom ->
          with :ok <-
                 validate_pdf_geometry([
                   width,
                   height,
                   left,
                   bottom,
                   right,
                   top,
                   right - left,
                   top - bottom
                 ]),
               :ok <-
                 validate_drawing_group(%{
                   type: :drawing_group,
                   width: width,
                   height: height,
                   transform: matrix,
                   boxes: []
                 }),
               {:ok, polygons} <-
                 Enum.reduce_while(polygons, {:ok, []}, fn polygon, {:ok, result} ->
                   case NativeElixirPdfUtilities.Validators.ClipGeometryValidator.prepare_polygon(
                          polygon
                        ) do
                     {:ok, polygon} -> {:cont, {:ok, [polygon | result]}}
                     error -> {:halt, error}
                   end
                 end),
               :ok <-
                 validate_pdf_geometry([
                   elem(matrix, 4) - left,
                   elem(matrix, 5) - bottom,
                   0,
                   0,
                   0,
                   0,
                   0,
                   0
                 ]),
               :ok <-
                 Enum.reduce_while(polygons, :ok, fn polygon, :ok ->
                   case validate_pdf_geometry(
                          Enum.flat_map(polygon, fn {x, y} -> [x - left, y - bottom] end)
                        ) do
                     :ok -> {:cont, :ok}
                     error -> {:halt, error}
                   end
                 end) do
            {:ok,
             %{
               appearance: Map.take(appearance, [:size, :boxes, :paint_frame]),
               source_size: {width, height},
               matrix: matrix,
               polygons: Enum.reverse(polygons),
               bounds: bounds
             }}
          end

        _ ->
          drawing_group_error(
            "appearance projection requires an actual drawing frame, affine matrix, clip polygons and positive bounded output rectangle"
          )
      end
    end)
  end

  @doc false
  @spec prepare_image_paint(map(), number()) :: map() | nil
  def prepare_image_paint(box, page_height) do
    case box do
      %{type: :image, snap_to_css_pixel_grid: true} ->
        canvas = DrawingGeometry.css_print_canvas(page_height)
        pixels = canvas.css_pixels_per_point
        device = canvas.css_scale

        transform = [
          box.width * pixels * device,
          0,
          0,
          -box.height * pixels * device,
          box.x * pixels * device,
          (page_height - box.y) * pixels * device
        ]

        clip = %{
          x: box.x * pixels * device,
          y: (page_height - box.y - box.height) * pixels * device,
          width: box.width * pixels * device,
          height: box.height * pixels * device
        }

        # Rounded image content clips are painted in the printer's device
        # plane. Moving them through a point-space clip loses raster edge ties.
        source_clips =
          case Map.get(box, :clip) do
            nil -> []
            content -> [content, Map.get(content, :inner_clip)] |> Enum.reject(&is_nil/1)
          end

        content_clips =
          if Enum.any?(source_clips, &Map.has_key?(&1, :border_radii)) do
            scale = pixels * device

            swap = %{
              bottom_left: :top_left,
              bottom_right: :top_right,
              top_left: :bottom_left,
              top_right: :bottom_right
            }

            Enum.map(source_clips, fn content ->
              radii =
                Map.get_lazy(content, :border_radii, fn ->
                  radius = Map.get(content, :border_radius, 0)
                  Map.new(Map.keys(swap), &{&1, {radius, radius}})
                end)

              %{
                x: content.x * scale,
                y: (page_height - content.y - content.height) * scale,
                width: content.width * scale,
                height: content.height * scale,
                border_radii:
                  Map.new(swap, fn {corner, source} ->
                    {rx, ry} = radii[source]
                    {corner, {rx * scale, ry * scale}}
                  end)
              }
            end)
          end

        clip_values =
          Enum.flat_map(content_clips || [], fn content ->
            Map.values(Map.take(content, [:x, :y, :width, :height])) ++
              Enum.flat_map(Map.values(content.border_radii), &Tuple.to_list/1)
          end)

        try do
          Enum.each(transform ++ Map.values(clip) ++ clip_values ++ [page_height], fn value ->
            HtmlValidator.check_render_resource(:max_pdf_numeric_magnitude, abs(value), :writer)
          end)
        catch
          {:render_resource_limit, {:error, {reason, diagnostic}}} ->
            throw(
              {:render_resource_limit,
               {:error,
                {reason,
                 Diagnostics.with_context(diagnostic,
                   operation: :write_pdf,
                   module: __MODULE__,
                   source: "CSS image source print coordinates"
                 )}}}
            )
        end

        canvas
        |> Map.put(:transform, transform)
        |> Map.put(:clip, clip)
        |> Map.put(:content_clips, content_clips)

      _ ->
        nil
    end
  end

  @doc "Identifies validated CSS text paint whose coordinate snapping or source positioning was already resolved."
  @spec css_source_text?(map()) :: boolean()
  def css_source_text?(box) do
    Map.get(box, :snap_to_css_pixel_grid, false) == true or
      Map.get(box, :css_source_positioned, false) == true
  end

  @doc false
  @spec prepare_text_paint(map(), number()) :: map() | nil
  def prepare_text_paint(box, page_height) do
    case {box, css_source_text?(box)} do
      {%{type: :text, font_face: %{type: :embedded}}, true} ->
        canvas = DrawingGeometry.css_print_canvas(page_height)

        baseline =
          if Map.get(box, :css_source_positioned, false) do
            box.y
          else
            DrawingGeometry.text_baseline(
              box.y,
              page_height,
              Map.get(box, :baseline_snap_depth, 0)
            )
          end

        x = box.x / 0.75
        y = (page_height - baseline) / 0.75
        size = Float.floor(box.font_size / 0.75 * 100) / 100

        spacing = Map.get(box, :letter_spacing, 0.0) / 0.75

        try do
          Enum.each([x, y, size, spacing, page_height], fn value ->
            HtmlValidator.check_render_resource(:max_pdf_numeric_magnitude, abs(value), :writer)
          end)
        catch
          {:render_resource_limit, {:error, {reason, diagnostic}}} ->
            throw(
              {:render_resource_limit,
               {:error,
                {reason,
                 Diagnostics.with_context(diagnostic,
                   operation: :write_pdf,
                   module: __MODULE__,
                   source: "CSS source text print coordinates and spacing"
                 )}}}
            )
        end

        Map.merge(canvas, %{x: x, y: y, size: size})

      _ ->
        nil
    end
  end

  @doc "Checks a widget rectangle and its clipped rectangle through the writer's existing numeric bounds before appearance allocation."
  @spec validate_pdf_geometry(term()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_pdf_geometry(values) do
    case values do
      [_, _, _, _, _, _, _, _] ->
        if Enum.all?(values, &bounded_number?/1),
          do: :ok,
          else:
            drawing_group_error(
              "composed widget geometry must remain within max_pdf_numeric_magnitude"
            )

      _ ->
        drawing_group_error("widget geometry requires two bounded PDF rectangles")
    end
  end

  @doc false
  @spec prepare_appearances(term(), term()) ::
          {:ok, context()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_appearances(frames, first_object_id) do
    HtmlValidator.with_render_budget(fn ->
      case first_object_id do
        id when is_integer(id) and id > 0 ->
          HtmlValidator.check_render_resource(:max_pdf_objects, id, :writer)
          prepare(frames, [], :appearances)

        _ ->
          Diagnostics.error(
            :pdf,
            :invalid_pdf_input,
            "appearance object IDs must start at a positive integer",
            operation: :write_pdf,
            module: __MODULE__
          )
      end
    end)
  end

  defp prepare(pages, opts, kind) do
    case {pages, opts} do
      {pages, opts} when is_list(pages) and is_list(opts) ->
        case Keyword.keyword?(opts) do
          true ->
            with :ok <- HtmlValidator.validate_pdf_output_options(opts),
                 {:ok, metadata} <-
                   InfoValidator.normalize_new_metadata(Keyword.get(opts, :metadata, [])) do
              case OutlineValidator.normalize(
                     normalized_outline_input(Keyword.get(opts, :outlines)),
                     length(pages)
                   ) do
                {:ok, outlines} ->
                  validity =
                    Enum.reduce_while(pages, :ok, fn page, validity ->
                      case valid_page?(page, kind) do
                        {:error, _} = error -> {:halt, error}
                        false -> {:halt, false}
                        :box_normalization -> {:cont, :box_normalization}
                        true -> {:cont, validity}
                      end
                    end)

                  case {pages != [], validity} do
                    {_, {:error, _} = error} ->
                      error

                    {true, validity} when validity in [true, :ok, :box_normalization] ->
                      destination_pages = pages

                      pages =
                        if validity == :box_normalization,
                          do: normalize_layout_boxes(pages),
                          else: pages

                      pages =
                        Enum.map(pages, fn page ->
                          %{
                            page
                            | boxes:
                                Enum.map(page.boxes, fn box ->
                                  styles =
                                    case Map.get(box, :border_styles) do
                                      styles when is_map(styles) -> Map.values(styles)
                                      _ -> []
                                    end

                                  if Map.get(box, :type) == :rect and
                                       Map.get(box, :shape, :rectangle) == :rectangle and
                                       Map.get(box, :snap_to_css_pixel_grid, false) and
                                       Map.get(box, :border_radius, 0) > 0 and
                                       (is_nil(Map.get(box, :form_owner)) or
                                          Map.get(box, :role) in [
                                            :temporal_host,
                                            :css_control_host
                                          ]) and
                                       Enum.any?(styles, &(&1 in [:dotted, :dashed])) do
                                    Map.put(box, :css_border_surface, %{
                                      alpha: 1.0,
                                      clip:
                                        Map.take(box, [:x, :y, :width, :height, :border_radius])
                                    })
                                  else
                                    box
                                  end
                                end)
                          }
                        end)

                      case kind do
                        :pages ->
                          HtmlValidator.check_render_resource(
                            :max_rendered_pages,
                            length(pages),
                            :writer
                          )

                          HtmlValidator.check_render_resource(
                            :max_pdf_pages,
                            length(pages),
                            :writer
                          )

                        :appearances ->
                          HtmlValidator.check_render_resource(
                            :max_appearance_widgets,
                            length(pages),
                            :writer
                          )
                      end

                      Enum.reduce(pages, 0, fn page, count ->
                        count = count + length(page.boxes)
                        HtmlValidator.check_render_resource(:max_layout_boxes, count, :writer)
                        count
                      end)

                      with :ok <-
                             NativeElixirPdfUtilities.Validators.HtmlLinkValidator.validate_destinations(
                               destination_pages
                             ) do
                        case validate_embedded_fonts(pages) do
                          :ok ->
                            {:ok,
                             %{
                               pages: pages,
                               metadata: metadata,
                               outlines: outlines,
                               compress_streams: Keyword.get(opts, :compress_streams, true),
                               subset_fonts: Keyword.get(opts, :subset_fonts, true)
                             }}

                          {:error, {_reason, _diagnostic}} = error ->
                            error

                          {:error, message} ->
                            Diagnostics.error(:pdf, :invalid_pdf_input, message,
                              operation: :write_pdf,
                              module: __MODULE__
                            )
                        end
                      end

                    _ ->
                      Diagnostics.error(
                        :pdf,
                        :invalid_pdf_input,
                        "PDF writer requires non-empty valid pages; link_region requires bounded convex counterclockwise QuadPoints and a positive annotation rectangle; rectangle shape must be :rectangle, {:triangle, direction} with up/down/left/right direction, or {:chevron, :down}; select_arrow requires a chevron and clip rectangle; flow_marker requires zero width, non-negative height and no link/field; baseline_snap_depth must be non-negative numeric when supplied; rectangle text_baseline_offset must be numeric when supplied; paint_frame requires a finite bounded rectangle supplying the original page paint origin and height; clip requires numeric finite bounded x/y and nonnegative width/height, optional nonnegative border_radius and at most one inner_clip rectangle when supplied; border_cutout requires numeric offsets and positive dimensions when supplied; glyph_group requires valid UTF-8 actual_text and text-only boxes; link_quad_points requires eight finite coordinates forming a nonzero convex quadrilateral within its link_region rectangle; explicit glyph_ids require an embedded font and one positive 16-bit glyph ID per Unicode scalar",
                        operation: :write_pdf,
                        module: __MODULE__
                      )
                  end

                {:error, _error} = outline_error ->
                  outline_error
              end
            else
              {:error, {:invalid_options, _diagnostic}} = error ->
                error

              {:error, _reason} ->
                Diagnostics.error(
                  :pdf,
                  :invalid_pdf_input,
                  "PDF metadata must use supported fields and value types",
                  operation: :write_pdf,
                  module: __MODULE__
                )
            end

          false ->
            Diagnostics.error(
              :pdf,
              :invalid_pdf_input,
              "PDF writer options must be a keyword list",
              operation: :write_pdf,
              module: __MODULE__
            )
        end

      _ ->
        Diagnostics.error(:pdf, :invalid_pdf_input, "PDF writer requires a list of pages",
          operation: :write_pdf,
          module: __MODULE__
        )
    end
  end

  defp normalized_outline_input(outlines) do
    case outlines do
      nil -> []
      false -> []
      outlines -> outlines
    end
  end

  @doc false
  @spec visible_border?(map()) :: boolean()
  def visible_border?(box) do
    border_widths =
      Map.get(box, :border_widths, %{
        top: box.stroke_width,
        right: box.stroke_width,
        bottom: box.stroke_width,
        left: box.stroke_width
      })

    Enum.any?([:top, :right, :bottom, :left], fn side ->
      border_style =
        case Map.get(box, :border_styles) do
          %{^side => style} -> style
          _ -> :solid
        end

      border_color =
        case Map.get(box, :border_colors) do
          %{^side => color} -> color
          _ -> box.stroke_color
        end

      Map.fetch!(border_widths, side) > 0 and border_style not in [:none, :hidden] and
        not is_nil(border_color)
    end)
  end

  defp valid_page?(page, kind) do
    case page do
      %{size: {width, height}, boxes: boxes}
      when is_number(width) and is_number(height) and width > 0 and height > 0 and is_list(boxes) ->
        frame_valid? =
          case Map.fetch(page, :paint_frame) do
            :error ->
              true

            {:ok, origin} ->
              kind == :appearances and is_map(origin) and map_size(origin) == 4 and
                valid_clip_rectangle?(origin)
          end

        if frame_valid? and bounded_number?(width) and bounded_number?(height) do
          page_height = Map.get(page, :paint_frame, %{height: height}).height

          Enum.reduce_while(boxes, true, fn box, validity ->
            result =
              is_map(box) and is_boolean(Map.get(box, :snap_to_css_pixel_grid, false)) and
                valid_box?(box, page_height)

            case result do
              {:error, _} = error ->
                {:halt, error}

              true ->
                if valid_prepared_box_clips?(box, page_height) do
                  {:cont,
                   if(
                     box.type in [:drawing_group, :glyph_group, :flow_marker] or
                       is_map(Map.get(box, :clip)) or is_list(Map.get(box, :link_quad_points)) or
                       is_map(Map.get(box, :css_border_surface)) or
                       (box.type == :rect and Map.get(box, :border_widths) == nil and
                          (is_map(Map.get(box, :border_styles)) or
                             is_map(Map.get(box, :border_colors)))),
                     do: :box_normalization,
                     else: validity
                   )}
                else
                  {:halt, false}
                end

              false ->
                {:halt, false}
            end
          end)
        else
          false
        end

      _ ->
        false
    end
  end

  defp normalize_layout_boxes(pages) do
    {pages, _count} =
      Enum.map_reduce(pages, 0, fn page, count ->
        page_height = Map.get(page, :paint_frame, %{height: elem(page.size, 1)}).height
        {boxes, count} = normalize_boxes(page.boxes, count, nil, page_height)

        boxes = boxes |> List.flatten() |> Enum.map(&normalize_page_link_quad(&1, page.size))
        {%{page | boxes: boxes}, count}
      end)

    pages
  end

  # Prepare final link hit geometry after all drawing transforms and clips,
  # with the real PDF page bounds. The byte writer consumes this projection.
  defp normalize_page_link_quad(box, {page_width, page_height}) do
    case Map.get(box, :drawing_affine_link_quads, Map.get(box, :link_quad_points)) do
      polygons when is_list(polygons) and is_tuple(hd(hd(polygons))) ->
        quads =
          Enum.flat_map(polygons, fn polygon ->
            case NativeElixirPdfUtilities.Validators.ClipGeometryValidator.project(
                   nil,
                   polygon,
                   {0, 0, page_width, page_height}
                 ) do
              {:ok, projected} -> projected
              {:error, _} = error -> throw({:render_resource_limit, error})
            end
          end)

        if quads == [] do
          Map.drop(box, [:link_url, :link_destination, :drawing_affine_link_quads])
        else
          polygons =
            Enum.map(quads, fn [x1, y1, x2, y2, x3, y3, x4, y4] ->
              [{x1, y1}, {x2, y2}, {x4, y4}, {x3, y3}]
            end)

          box
          |> Map.delete(:drawing_affine_link_quads)
          |> Map.put(
            :drawing_annotation_rect,
            NativeElixirPdfUtilities.HtmlToPdf.ImageMapGeometry.bounds(polygons)
          )
          |> Map.put(:drawing_link_quad_points, List.flatten(quads))
        end

      nil ->
        box

      [x1, y1, x2, y2, x3, y3, x4, y4] ->
        rect =
          case Map.fetch(box, :drawing_annotation_rect) do
            {:ok, prepared} ->
              prepared

            :error ->
              DrawingGeometry.intersect_clip(
                {box.x, box.y, box.x + box.width, box.y + box.height},
                DrawingGeometry.box_clip_bounds(Map.get(box, :clip))
              )
          end

        {left, bottom, right, top} =
          DrawingGeometry.intersect_clip(rect, {0, 0, page_width, page_height})

        case NativeElixirPdfUtilities.Validators.ClipGeometryValidator.project(
               nil,
               [{x1, y1}, {x2, y2}, {x4, y4}, {x3, y3}],
               {left, bottom, right, top}
             ) do
          {:ok, []} ->
            Map.drop(box, [:link_url, :link_destination])

          {:ok, quads} ->
            box
            |> Map.put(:drawing_annotation_rect, {left, bottom, right, top})
            |> Map.put(:drawing_link_quad_points, List.flatten(quads))

          {:error, _} = error ->
            throw({:render_resource_limit, error})
        end
    end
  end

  defp normalize_boxes(boxes, count, composition, parent_height \\ nil) do
    Enum.map_reduce(boxes, count, fn box, count ->
      case box do
        %{type: :drawing_group} ->
          count = count + 2
          HtmlValidator.check_render_resource(:max_layout_boxes, count, :writer)

          parent_height =
            if composition == nil,
              do: parent_height,
              else: elem(DrawingGeometry.composition_parts(composition), 2)

          box =
            case Map.get(box, :clip) do
              nil -> box
              clip -> Map.put(box, :clip, normalize_clip_rectangle(clip, parent_height, false))
            end

          transform = DrawingGeometry.local_group_transform(box, parent_height)

          composition =
            DrawingGeometry.compose_group(box, composition || {1, 1, 0, 0, nil, parent_height})

          local_clip = DrawingGeometry.local_clip(box, parent_height)
          {children, count} = normalize_boxes(box.boxes, count, composition)

          start = %{type: :drawing_group_start, transform: transform, clip: local_clip}

          start =
            case Map.get(box, :embedded_widgets) do
              nil ->
                start

              descriptor ->
                fields =
                  case composition do
                    %{transform: matrix, clip: clip} ->
                      viewport =
                        DrawingGeometry.clip_bounds(clip) ||
                          DrawingGeometry.clip_bounds(
                            DrawingGeometry.transform_clip(
                              %{x: 0, y: 0, width: box.width, height: box.height},
                              matrix
                            )
                          )

                      case NativeElixirPdfUtilities.Validators.EmbeddedFormValidator.compose_affine(
                             descriptor,
                             matrix,
                             viewport,
                             DrawingGeometry.clip_expression(clip)
                           ) do
                        {:ok, fields} -> fields
                        {:error, _} = error -> throw({:render_resource_limit, error})
                      end

                    _ ->
                      {:ok, fields} =
                        NativeElixirPdfUtilities.Validators.EmbeddedFormValidator.compose(
                          descriptor,
                          composition
                        )

                      fields
                  end

                {:ok, source_token} =
                  NativeElixirPdfUtilities.Validators.EmbeddedFormValidator.source_identity(
                    descriptor
                  )

                Map.put(start, :embedded_widgets, %{
                  source_token: source_token,
                  document_id: descriptor.context.document_id,
                  context: descriptor.context,
                  token: descriptor.token,
                  fields: fields
                })
            end

          start =
            if(
              Map.get(box, :viewport_paint_role, Map.get(box, :role)) in [
                :frameset_viewport,
                :iframe_viewport
              ],
              do: Map.put(start, :role, Map.get(box, :viewport_paint_role, Map.get(box, :role))),
              else: start
            )

          {[start, children, %{type: :drawing_group_end}], count}

        %{type: :flow_marker} ->
          count = count + 1
          HtmlValidator.check_render_resource(:max_layout_boxes, count, :writer)
          {[], count}

        %{type: :glyph_group, actual_text: text, boxes: children} ->
          count = count + length(children) + 2
          HtmlValidator.check_render_resource(:max_layout_boxes, count, :writer)

          HtmlValidator.reserve_render_resource(
            :max_rendered_text_bytes,
            byte_size(text),
            :writer
          )

          children =
            Enum.map(children, &normalize_group_annotation(&1, composition, parent_height))

          {[%{type: :actual_text_start, text: text}] ++
             children ++ [%{type: :actual_text_end}], count}

        box ->
          count = count + 1
          HtmlValidator.check_render_resource(:max_layout_boxes, count, :writer)

          box =
            if Map.get(box, :type) == :rect and Map.get(box, :border_widths) == nil do
              Map.put(box, :border_widths, %{
                top: box.stroke_width,
                right: box.stroke_width,
                bottom: box.stroke_width,
                left: box.stroke_width
              })
            else
              box
            end

          box =
            if is_map(Map.get(box, :css_border_surface)),
              do:
                update_in(box, [:css_border_surface, :clip], &Map.put_new(&1, :border_radius, 0)),
              else: box

          {[normalize_group_annotation(box, composition, parent_height)], count}
      end
    end)
  end

  @doc "Validates the shared writer clip rectangle grammar, including rounded outer and inner rectangles."
  @spec validate_clip_rectangle(term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_clip_rectangle(clip) do
    if valid_clip_rectangle?(clip, true),
      do: :ok,
      else:
        drawing_group_error(
          "clip requires bounded rectangle geometry and an optional rounded inner rectangle"
        )
  end

  @doc "Validates native paint boxes using the same geometry and annotation rules as PDF writing."
  @spec validate_native_boxes(term()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_native_boxes(boxes) do
    HtmlValidator.with_render_budget(fn ->
      case boxes do
        boxes when is_list(boxes) ->
          HtmlValidator.check_render_resource(:max_layout_boxes, length(boxes), :writer)

          Enum.reduce_while(boxes, :ok, fn box, :ok ->
            case valid_box?(box) do
              true ->
                {:cont, :ok}

              false ->
                {:halt, drawing_group_error("native paint requires valid existing writer boxes")}

              error ->
                {:halt, error}
            end
          end)

        _ ->
          drawing_group_error("native paint requires an ordered list of writer boxes")
      end
    end)
  end

  defp valid_box?(box, parent_height \\ nil) do
    case box do
      %{css_source_positioned: positioned} ->
        is_boolean(positioned) and
          (not positioned or
             (Map.get(box, :type) == :text and
                match?(%{type: :embedded}, Map.get(box, :font_face)) and
                Map.get(box, :snap_to_css_pixel_grid, false) == false)) and
          valid_box?(Map.delete(box, :css_source_positioned), parent_height)

      %{initial_select_opacity: opacity} ->
        case opacity do
          %{owner: owner, alpha: alpha} when is_reference(owner) ->
            alpha in [0.7, 1.0] and Enum.all?(Map.keys(opacity), &(&1 in [:owner, :alpha, :clip])) and
              (not Map.has_key?(opacity, :clip) or
                 (is_map(opacity.clip) and valid_clip_rectangle?(opacity.clip, true))) and
              Map.get(box, :form_owner, owner) == owner and
              valid_box?(Map.delete(box, :initial_select_opacity), parent_height)

          _ ->
            false
        end

      %{css_border_surface: surface} ->
        case surface do
          %{alpha: 1.0, clip: clip} ->
            map_size(surface) == 2 and is_map(clip) and valid_clip_rectangle?(clip, true) and
              valid_box?(Map.delete(box, :css_border_surface), parent_height)

          _ ->
            false
        end

      %{native_menu_clip: clip} ->
        is_map(clip) and valid_clip_rectangle?(clip, true) and
          valid_box?(Map.delete(box, :native_menu_clip), parent_height)

      %{drawing_annotation_rect: _} ->
        false

      %{drawing_page_height: _} ->
        false

      %{drawing_link_quad_points: _} ->
        false

      %{drawing_affine_link_quads: _} ->
        false

      %{type: :drawing_group} ->
        case valid_drawing_group(box, 1, {1, 1, 0, 0}, 0, parent_height) do
          {:ok, _count} -> true
          {:error, _} = error -> error
        end

      %{type: :glyph_group, actual_text: text, boxes: children}
      when is_binary(text) and is_list(children) ->
        String.valid?(text, :fast_ascii) and
          Enum.all?(children, fn child ->
            is_map(child) and Map.get(child, :type) == :text and
              is_boolean(Map.get(child, :snap_to_css_pixel_grid, false)) and valid_box?(child)
          end)

      %{type: :flow_marker, x: x, y: y, width: width, height: height}
      when is_number(x) and is_number(y) and is_number(width) and width == 0 and
             is_number(height) and height >= 0 ->
        Enum.all?(
          [
            :text,
            :image,
            :fill_color,
            :stroke_color,
            :font_face,
            :glyph_ids,
            :link_url,
            :link_destination,
            :form_control
          ],
          &(Map.get(box, &1) == nil)
        )

      %{type: :link_region, x: x, y: y, width: width, height: height}
      when is_number(x) and is_number(y) and is_number(width) and is_number(height) and width > 0 and
             height > 0 ->
        Enum.all?([x, y, width, height, x + width, y + height], &bounded_number?/1) and
          Enum.all?(
            [:text, :image, :font_face, :glyph_ids, :form_control, :fill_color, :stroke_color],
            &(Map.get(box, &1) == nil)
          ) and
          (Map.get(box, :link_url) != nil or Map.get(box, :link_destination) != nil) and
          (not Map.has_key?(box, :link_quads) or
             NativeElixirPdfUtilities.Validators.ImageMapValidator.valid_link_region?(box)) and
          valid_clip_rectangle?(Map.get(box, :clip), true) and valid_link_box?(box)

      %{type: :text, text: text, x: x, y: y, font_size: font_size, font: font, color: color}
      when is_binary(text) and is_number(x) and is_number(y) and is_number(font_size) and
             font_size >= 0 and is_binary(font) ->
        valid_baseline_depth?(box) and valid_color?(color) and valid_font_box?(box) and
          valid_clip_rectangle?(Map.get(box, :clip), true) and valid_link_box?(box)

      %{
        type: :rect,
        x: x,
        y: y,
        width: width,
        height: height,
        fill_color: fill_color,
        stroke_color: stroke_color,
        stroke_width: stroke_width,
        border_radius: border_radius
      }
      when is_number(x) and is_number(y) and is_number(width) and is_number(height) and
             width > 0 and height > 0 and is_number(stroke_width) and stroke_width >= 0 and
             is_number(border_radius) and border_radius >= 0 ->
        Map.get(box, :shape, :rectangle) in [
          :rectangle,
          {:triangle, :up},
          {:triangle, :down},
          {:triangle, :left},
          {:triangle, :right},
          {:chevron, :down},
          {:diagonal, :up}
        ] and
          (Map.get(box, :role) != :select_arrow or
             (Map.get(box, :shape) == {:chevron, :down} and is_map(Map.get(box, :clip)))) and
          valid_optional_color?(fill_color) and valid_optional_color?(stroke_color) and
          case box do
            %{text_baseline_offset: offset} -> is_number(offset)
            _ -> true
          end and
          valid_baseline_depth?(box) and
          valid_gauge_frame?(box) and
          valid_range_frame?(box) and
          is_boolean(Map.get(box, :gauge_quantize_value, false)) and
          is_boolean(Map.get(box, :gauge_clip_track, false)) and
          case Map.get(box, :gauge_fraction) do
            nil -> true
            fraction -> is_number(fraction) and fraction >= 0 and fraction <= 1
          end and
          case Map.get(box, :gauge_gradient) do
            nil ->
              true

            colors ->
              is_list(colors) and length(colors) == 4 and
                Enum.all?(colors, fn color ->
                  is_tuple(color) and tuple_size(color) == 3 and valid_color?(color)
                end)
          end and
          valid_clip_rectangle?(Map.get(box, :clip), true) and
          valid_clip_rectangle?(Map.get(box, :border_cutout)) and
          valid_border_widths?(Map.get(box, :border_widths)) and
          valid_border_colors?(Map.get(box, :border_colors)) and
          valid_border_styles?(Map.get(box, :border_styles)) and
          (not is_nil(fill_color) or visible_border?(box) or is_map(Map.get(box, :form_control)))

      %{type: :image, x: x, y: y, width: width, height: height, image: image}
      when is_number(x) and is_number(y) and is_number(width) and is_number(height) and
             width > 0 and height > 0 ->
        valid_image?(image) and valid_clip_rectangle?(Map.get(box, :clip), true) and
          valid_baseline_depth?(box) and valid_link_box?(box)

      _ ->
        false
    end
  end

  # The frame is relative to each rectangle, so pagination can translate paint
  # without reparsing state or losing the native widget's final page-grid phase.
  @doc false
  @spec valid_range_frame?(map()) :: boolean()
  def valid_range_frame?(box) do
    tick_valid? =
      case {Map.get(box, :role), Map.get(box, :range_tick_frame)} do
        {:range_tick, {content_left, content_width, position}} ->
          Enum.all?(
            [content_left, content_width, position],
            &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
          ) and
            content_left >= 0 and content_width >= 0 and position >= 0 and position <= 1

        {role, nil} ->
          role != :range_tick

        _ ->
          false
      end

    tick_valid? and
      case Map.get(box, :range_frame) do
        nil ->
          Map.get(box, :role) not in [
            :range_track,
            :range_value,
            :range_border,
            :range_thumb,
            :range_tick
          ]

        {dx, dy, width, height, thumb_left, thumb_top} ->
          Map.get(box, :role) in [
            :range_track,
            :range_value,
            :range_border,
            :range_thumb,
            :range_tick
          ] and
            Enum.all?(
              [dx, dy, width, height, thumb_left, thumb_top, Map.get(box, :x), Map.get(box, :y)],
              &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
            ) and
            Enum.all?(
              [box.x - dx, box.y - dy, box.x - dx + width, box.y - dy + height],
              &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
            ) and width > 0 and height > 0

        _ ->
          false
      end
  end

  defp valid_gauge_frame?(box) do
    case Map.get(box, :gauge_frame) do
      nil ->
        true

      {dx, dy, width, height} ->
        Enum.all?(
          [dx, dy, width, height],
          &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
        ) and width > 0 and height > 0 and
          dx >= 0 and dx <= width and dy >= 0 and dy <= height and
          Map.get(box, :role) in [
            :gauge_track,
            :gauge_value,
            :gauge_indeterminate,
            :gauge_outline
          ] and
          NativeElixirPdfUtilities.Validators.HtmlGaugeValidator.valid_prepared?(
            Map.get(box, :gauge_state)
          )

      _ ->
        false
    end
  end

  @doc false
  @spec validate_drawing_group(map()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_drawing_group(box) do
    case valid_drawing_group(box, 1, {1, 1, 0, 0}, 0) do
      {:ok, _count} -> :ok
      error -> error
    end
  end

  # Group dimensions are viewport metadata. Only the explicit clip restricts
  # nondegenerate viewports; a zero dimension prepares an empty visible region.
  defp valid_drawing_group(box, depth, parent_transform, count, parent_height \\ nil) do
    HtmlValidator.check_render_resource(:max_drawing_group_depth, depth, :writer)
    HtmlValidator.check_render_resource(:max_layout_boxes, count + 2, :writer)

    case box do
      %{width: width, height: height, transform: {a, b, c, d, e, f}, boxes: children}
      when is_number(width) and width >= 0 and is_number(height) and height >= 0 and
             is_list(children) ->
        baseline = Map.get(box, :baseline, 0)
        clip = Map.get(box, :clip)

        cond do
          Map.has_key?(box, :drawing_annotation_rect) or Map.has_key?(box, :drawing_page_height) ->
            drawing_group_error(
              "drawing_group cannot contain caller-supplied private prepared fields"
            )

          not is_boolean(Map.get(box, :snap_to_css_pixel_grid, false)) ->
            drawing_group_error("drawing_group snap_to_css_pixel_grid must be boolean")

          Map.get(box, :role) == :overflow_clip and
              (box.transform != {1, 0, 0, 1, 0, 0} or not is_map(clip)) ->
            drawing_group_error("overflow clipping must preserve the containing coordinate plane")

          Map.get(box, :viewport_paint_role) not in [nil, :iframe_viewport, :frameset_viewport] ->
            drawing_group_error(
              "drawing_group viewport paint role must identify an existing iframe or frameset viewport"
            )

          not Enum.all?([width, height, a, b, c, d, e, f, baseline], &bounded_number?/1) ->
            drawing_group_error(
              "drawing_group dimensions, baseline and affine values must be numeric and within max_pdf_numeric_magnitude"
            )

          baseline < 0 or baseline > height ->
            drawing_group_error("drawing_group baseline must lie within its local height")

          Float.round(a * 1.0, 4) * Float.round(d * 1.0, 4) -
            Float.round(b * 1.0, 4) * Float.round(c * 1.0, 4) == 0 ->
            drawing_group_error(
              "drawing_group affine matrix must remain nondegenerate at the writer's four-decimal PDF precision"
            )

          not valid_clip_rectangle?(clip, true) ->
            drawing_group_error(
              "drawing_group clip requires bounded numeric x/y, non-negative width/height and valid rounded corners"
            )

          not is_nil(Map.get(box, :form_control)) ->
            drawing_group_error(
              "drawing_group form controls require transformed widget support",
              :unsupported_form
            )

          not is_nil(Map.get(box, :link_destination)) ->
            drawing_group_error(
              "drawing_group destinations require scoped root placement and final composition geometry",
              :unsupported_html
            )

          not is_nil(Map.get(box, :link_url)) ->
            drawing_group_error("drawing_group links belong on child text boxes")

          true ->
            box =
              if is_map(clip),
                do: Map.put(box, :clip, normalize_clip_rectangle(clip, parent_height, false)),
                else: box

            parent =
              case parent_transform do
                {sx, sy, tx, ty} -> {sx, sy, tx, ty, nil, parent_height}
                matrix -> %{transform: matrix, clip: nil, height: parent_height}
              end

            composition = DrawingGeometry.compose_group(box, parent)
            {transform, _clip, paint_height} = DrawingGeometry.composition_parts(composition)

            widget_validation =
              case Map.get(box, :embedded_widgets) do
                nil ->
                  :ok

                descriptor ->
                  case composition do
                    %{} ->
                      case NativeElixirPdfUtilities.Validators.EmbeddedFormValidator.resolve(
                             descriptor
                           ) do
                        {:ok, _fields} -> :ok
                        error -> error
                      end

                    _ ->
                      NativeElixirPdfUtilities.Validators.EmbeddedFormValidator.validate_composition(
                        descriptor,
                        composition
                      )
                  end
              end

            clip_bounds =
              case DrawingGeometry.transform_clip(
                     DrawingGeometry.local_clip(box, parent_height),
                     transform
                   ) do
                nil -> []
                bounds -> Tuple.to_list(DrawingGeometry.clip_bounds(bounds))
              end

            viewport_bounds =
              DrawingGeometry.transform_clip(
                %{x: 0, y: 0, width: width, height: height},
                transform
              )
              |> DrawingGeometry.clip_bounds()

            matrix_values = DrawingGeometry.affine_matrix(transform) |> Tuple.to_list()

            if widget_validation != :ok do
              widget_validation
            else
              if Enum.all?(
                   matrix_values ++ Tuple.to_list(viewport_bounds) ++ clip_bounds,
                   &bounded_number?/1
                 ) do
                Enum.reduce_while(children, {:ok, count + 2}, fn child, {:ok, count} ->
                  result =
                    case child do
                      %{type: :drawing_group} ->
                        valid_drawing_group(child, depth + 1, transform, count, paint_height)

                      child when is_map(child) ->
                        HtmlValidator.check_render_resource(:max_layout_boxes, count + 1, :writer)

                        pieces =
                          if child[:type] == :glyph_group,
                            do: Map.get(child, :boxes),
                            else: [child]

                        if is_list(pieces) and child[:type] == :glyph_group do
                          HtmlValidator.check_render_resource(
                            :max_layout_boxes,
                            count + length(pieces) + 2,
                            :writer
                          )
                        end

                        cond do
                          not is_list(pieces) ->
                            drawing_group_error(
                              "drawing_group children must contain valid existing writer boxes"
                            )

                          not is_nil(Map.get(child, :form_control)) or
                              Enum.any?(
                                pieces,
                                &(is_map(&1) and not is_nil(Map.get(&1, :form_control)))
                              ) ->
                            drawing_group_error(
                              "drawing_group form controls require transformed widget support",
                              :unsupported_form
                            )

                          child[:type] not in [
                            :text,
                            :rect,
                            :image,
                            :link_region,
                            :glyph_group,
                            :flow_marker
                          ] or
                            not is_boolean(Map.get(child, :snap_to_css_pixel_grid, false)) or
                              not valid_box?(child) ->
                            drawing_group_error(
                              "drawing_group children must contain valid text, rectangle, image, link_region, glyph_group or drawing_group boxes"
                            )

                          not Enum.all?(
                            pieces,
                            &valid_group_geometry?(&1, transform, paint_height)
                          ) ->
                            drawing_group_error(
                              "drawing_group child geometry and transformed bounds must be within max_pdf_numeric_magnitude"
                            )

                          true ->
                            count =
                              count +
                                if child[:type] == :glyph_group, do: length(pieces) + 2, else: 1

                            HtmlValidator.check_render_resource(:max_layout_boxes, count, :writer)
                            {:ok, count}
                        end

                      _ ->
                        drawing_group_error("drawing_group children must be writer box maps")
                    end

                  case result do
                    {:ok, _count} -> {:cont, result}
                    {:error, _} -> {:halt, result}
                  end
                end)
              else
                drawing_group_error(
                  "drawing_group cumulative transform and viewport bounds exceed max_pdf_numeric_magnitude"
                )
              end
            end
        end

      _ ->
        drawing_group_error(
          "drawing_group requires non-negative width/height, a six-value transform and a list of boxes"
        )
    end
  end

  defp bounded_number?(value) do
    is_number(value) and abs(value) <= Limits.get(:max_pdf_numeric_magnitude)
  end

  defp valid_group_geometry?(box, transform, page_height) do
    width = Map.get(box, :annotation_width, Map.get(box, :width, 0))

    height =
      if box.type == :text, do: Map.get(box, :line_height, box.font_size * 1.2), else: box.height

    dimensions =
      [box.x, box.y, width, height] ++
        Enum.map(
          [
            :font_size,
            :baseline_snap_depth,
            :text_baseline_offset,
            :stroke_width,
            :border_radius,
            :letter_spacing
          ],
          &Map.get(box, &1, 0)
        ) ++
        if(box.type == :rect, do: Map.values(Map.get(box, :border_widths) || %{}), else: [])

    {x, y} =
      if box.type == :text,
        do: DrawingGeometry.text_position(box, page_height),
        else: {box.x, box.y}

    corners =
      Enum.map([{x, y}, {x + width, y}, {x + width, y + height}, {x, y + height}], fn
        {x, y} -> DrawingGeometry.transform_point(x, y, transform)
      end)

    Enum.all?(dimensions, &bounded_number?/1) and width >= 0 and height >= 0 and
      Enum.all?(corners, fn {x, y} -> bounded_number?(x) and bounded_number?(y) end) and
      valid_prepared_box_clips?(box, page_height) and
      Enum.all?(
        [Map.get(box, :link_rect)] ++
          case Map.get(box, :clip) do
            nil ->
              []

            clip ->
              prepared =
                normalize_clip_rectangle(
                  clip,
                  page_height,
                  Map.get(box, :snap_to_css_pixel_grid, false) and
                    (Map.has_key?(clip, :inner_clip) or Map.has_key?(clip, :border_radii))
                )

              [prepared, Map.get(prepared, :inner_clip)]
          end,
        fn clip ->
          case DrawingGeometry.transform_clip(clip, transform) do
            nil ->
              true

            bounds ->
              Enum.all?(Tuple.to_list(DrawingGeometry.clip_bounds(bounds)), &bounded_number?/1)
          end
        end
      )
  end

  defp drawing_group_error(message, reason \\ :invalid_pdf_input) do
    Diagnostics.error(:pdf, reason, message, operation: :write_pdf, module: __MODULE__)
  end

  defp normalize_group_annotation(box, composition, parent_height) do
    page_height =
      if composition == nil,
        do: parent_height,
        else: elem(DrawingGeometry.composition_parts(composition), 2)

    box =
      case Map.get(box, :clip) do
        nil ->
          box

        clip ->
          Map.put(
            box,
            :clip,
            normalize_clip_rectangle(
              clip,
              page_height,
              Map.get(box, :snap_to_css_pixel_grid, false) and
                (Map.has_key?(clip, :inner_clip) or Map.has_key?(clip, :border_radii))
            )
          )
      end

    composition =
      case composition do
        {sx, sy, tx, ty, clip, height} ->
          content_clip = Map.get(box, :clip)

          if match?({:polygon, _}, clip) or
               (is_map(content_clip) and
                  (Map.has_key?(content_clip, :border_radii) or
                     Map.get(content_clip, :border_radius, 0) > 0 or
                     Map.has_key?(content_clip, :inner_clip))) do
            %{transform: {sx, 0, 0, sy, tx, ty}, clip: clip, height: height}
          else
            composition
          end

        _ ->
          composition
      end

    box =
      case composition do
        nil ->
          box

        composition ->
          Map.put(
            box,
            :drawing_page_height,
            elem(DrawingGeometry.composition_parts(composition), 2)
          )
      end

    box =
      case {Map.get(box, :type), composition} do
        {:link_region, {sx, sy, tx, ty, clip, _height}} when is_map_key(box, :link_quads) ->
          quads =
            Enum.map(box.link_quads, fn quad ->
              Enum.map(quad, fn {x, y} ->
                DrawingGeometry.transform_point(x, y, {sx, sy, tx, ty})
              end)
            end)

          clip =
            DrawingGeometry.intersect_clip(
              clip,
              DrawingGeometry.transform_box_clip(Map.get(box, :clip), {sx, sy, tx, ty})
            )

          quads =
            if clip == nil,
              do: quads,
              else: NativeElixirPdfUtilities.HtmlToPdf.ImageMapGeometry.clip_quads(quads, clip)

          if quads == [] do
            Map.drop(box, [:link_url, :link_destination])
          else
            box
            |> Map.put(:link_quads, quads)
            |> Map.put(
              :drawing_annotation_rect,
              NativeElixirPdfUtilities.HtmlToPdf.ImageMapGeometry.bounds(quads)
            )
          end

        _ ->
          box
      end

    box =
      case {composition, Map.get(box, :link_quad_points)} do
        {{sx, sy, tx, ty, _clip, _height}, points} when is_list(points) ->
          points =
            points
            |> Enum.chunk_every(2)
            |> Enum.flat_map(fn [x, y] -> [sx * x + tx, sy * y + ty] end)

          Map.put(box, :link_quad_points, points)

        _ ->
          box
      end

    case {composition, box} do
      {%{transform: _, clip: _} = affine, linked}
      when is_map_key(linked, :link_url) or is_map_key(linked, :link_destination) ->
        normalize_affine_annotation(linked, affine)

      {{sx, sy, tx, ty, clip, page_height}, %{type: :text} = linked}
      when is_map_key(linked, :link_url) or is_map_key(linked, :link_destination) ->
        width = Map.get(box, :annotation_width, Map.get(box, :width))
        height = Map.get(box, :line_height, box.font_size * 1.2)

        {x, y} = DrawingGeometry.text_position(box, page_height)
        [x, y] = Enum.map([x, y], &Float.round(&1 * 1.0, 4))
        rect = {sx * x + tx, sy * y + ty, sx * (x + width) + tx, sy * (y + height) + ty}

        rect =
          DrawingGeometry.intersect_clip(
            rect,
            DrawingGeometry.transform_box_clip(Map.get(box, :clip), {sx, sy, tx, ty})
          )

        {left, bottom, right, top} = DrawingGeometry.intersect_clip(rect, clip)

        if right > left and top > bottom,
          do: Map.put(box, :drawing_annotation_rect, {left, bottom, right, top}),
          else: Map.drop(box, [:link_url, :link_destination])

      {{_sx, _sy, _tx, _ty, clip, _height}, %{type: type} = linked}
      when type in [:image, :link_region] and not is_map_key(linked, :link_quads) and
             (is_map_key(linked, :link_url) or is_map_key(linked, :link_destination)) ->
        {sx, sy, tx, ty, _clip, _height} = composition
        region = Map.get(box, :link_rect, Map.take(box, [:x, :y, :width, :height]))
        rect = DrawingGeometry.transform_clip(region, {sx, sy, tx, ty})
        # Explicit link_rect is the border box, including contain letterboxing.
        # An image's fit clip restricts ink, not that authored clickable box.
        rect =
          if Map.has_key?(box, :link_rect),
            do: rect,
            else:
              DrawingGeometry.intersect_clip(
                rect,
                DrawingGeometry.transform_box_clip(Map.get(box, :clip), {sx, sy, tx, ty})
              )

        {left, bottom, right, top} = DrawingGeometry.intersect_clip(rect, clip)

        if right > left and top > bottom do
          box = Map.put(box, :drawing_annotation_rect, {left, bottom, right, top})

          box
        else
          Map.drop(box, [:link_url, :link_destination])
        end

      _ ->
        box
    end
  end

  # Affine hit geometry follows each actual transformed corner and containing
  # clip polygon. The rectangle is only the PDF annotation's required bounds.
  defp normalize_affine_annotation(box, %{transform: matrix, clip: clip, height: height}) do
    polygons =
      case box do
        %{link_quads: quads} ->
          quads

        %{link_quad_points: [x1, y1, x2, y2, x3, y3, x4, y4]} ->
          [[{x1, y1}, {x2, y2}, {x4, y4}, {x3, y3}]]

        %{type: :text} ->
          {x, y} = DrawingGeometry.text_position(box, height)
          width = Map.get(box, :annotation_width, box.width)
          line = Map.get(box, :line_height, box.font_size * 1.2)
          [[{x, y}, {x + width, y}, {x + width, y + line}, {x, y + line}]]

        _ ->
          region = Map.get(box, :link_rect, Map.take(box, [:x, :y, :width, :height]))
          x = region.x
          y = region.y
          width = region.width
          height = region.height
          [[{x, y}, {x + width, y}, {x + width, y + height}, {x, y + height}]]
      end

    content_clip = Map.get(box, :clip)

    use_content_clip =
      not Map.has_key?(box, :link_rect) or
        (is_map(content_clip) and
           (Map.has_key?(content_clip, :inner_clip) or
              (Map.has_key?(content_clip, :border_radius) or
                 Map.has_key?(content_clip, :border_radii))))

    clip =
      if use_content_clip,
        do:
          DrawingGeometry.intersect_clip(
            clip,
            DrawingGeometry.transform_box_clip(content_clip, matrix)
          ),
        else: clip

    expression = DrawingGeometry.clip_expression(clip)

    expression =
      if use_content_clip and content_clip != nil,
        do:
          {:intersection,
           [
             expression || {:intersection, []},
             DrawingGeometry.box_clip_expression(content_clip, matrix)
           ]},
        else: expression

    projected =
      Enum.flat_map(polygons, fn polygon ->
        polygon =
          Enum.map(polygon, fn {x, y} -> DrawingGeometry.transform_point(x, y, matrix) end)

        bounds = DrawingGeometry.clip_bounds({:polygon, polygon})
        viewport = DrawingGeometry.clip_bounds(clip) || bounds

        if NativeElixirPdfUtilities.HtmlToPdf.ClipGeometry.convex?(polygon) do
          case NativeElixirPdfUtilities.Validators.ClipGeometryValidator.project(
                 expression,
                 polygon,
                 viewport
               ) do
            {:ok, quads} -> quads
            {:error, _} = error -> throw({:render_resource_limit, error})
          end
        else
          []
        end
      end)

    if projected == [] do
      Map.drop(box, [:link_url, :link_destination, :link_quad_points, :link_quads])
    else
      polygons =
        Enum.map(projected, fn [x1, y1, x2, y2, x3, y3, x4, y4] ->
          [{x1, y1}, {x2, y2}, {x4, y4}, {x3, y3}]
        end)

      box
      |> Map.drop([:link_quad_points, :link_quads])
      |> Map.put(:drawing_affine_link_quads, polygons)
      |> Map.put(
        :drawing_annotation_rect,
        NativeElixirPdfUtilities.HtmlToPdf.ImageMapGeometry.bounds(polygons)
      )
    end
  end

  @compile {:inline, valid_baseline_depth?: 1}
  defp valid_baseline_depth?(box) do
    case Map.fetch(box, :baseline_snap_depth) do
      {:ok, depth} -> is_number(depth) and depth >= 0
      :error -> true
    end
  end

  defp valid_image?(image) do
    case image do
      %{logical_text: text} ->
        if is_binary(text),
          do:
            HtmlValidator.check_render_resource(
              :max_rendered_text_bytes,
              byte_size(text),
              :writer
            )

        is_binary(text) and String.valid?(text, :fast_ascii) and
          valid_image?(Map.delete(image, :logical_text))

      %{format: :pdf_form, data: source, width_px: width_px, height_px: height_px}
      when is_binary(source) and is_integer(width_px) and width_px > 0 and
             is_integer(height_px) and height_px > 0 ->
        match?({:ok, _}, PdfFormValidator.prepare_source(source))

      %{
        format: format,
        data: data,
        width_px: width_px,
        height_px: height_px,
        color_space: color_space,
        bits_per_component: depth
      }
      when format in [:png, :jpeg] and is_binary(data) and is_integer(width_px) and
             is_integer(height_px) and width_px > 0 and height_px > 0 and
             color_space in [:device_gray, :device_rgb, :device_cmyk] and depth in [8, 16] ->
        inverted = Map.get(image, :inverted_cmyk, false)

        convention = Map.get(image, :color_transform)

        valid_convention =
          case {format, color_space, convention, inverted} do
            {_, _, nil, _} -> true
            {:jpeg, :device_gray, :gray, false} -> true
            {:jpeg, :device_rgb, value, false} when value in [:rgb, :ycbcr] -> true
            {:jpeg, :device_cmyk, :cmyk, false} -> true
            {:jpeg, :device_cmyk, value, true} when value in [:adobe_cmyk, :ycck] -> true
            _ -> false
          end

        valid_depth = format == :png or depth == 8

        valid_samples =
          format == :jpeg or
            (color_space in [:device_gray, :device_rgb] and
               byte_size(data) ==
                 width_px * height_px * div(depth, 8) *
                   if(color_space == :device_gray, do: 1, else: 3))

        valid_depth and valid_samples and valid_convention and is_boolean(inverted) and
          (not inverted or (format == :jpeg and color_space == :device_cmyk)) and
          case Map.get(image, :alpha_data) do
            nil ->
              true

            alpha when is_binary(alpha) ->
              image.format == :png and
                byte_size(alpha) == image.width_px * image.height_px * div(depth, 8)

            _ ->
              false
          end

      _ ->
        false
    end
  end

  defp valid_clip_rectangle?(clip, rounded? \\ false) do
    case clip do
      nil ->
        true

      %{x: x, y: y, width: width, height: height} ->
        allowed =
          if rounded?,
            do: [:x, :y, :width, :height, :border_radius, :border_radii, :inner_clip],
            else: [:x, :y, :width, :height]

        radius = Map.get(clip, :border_radius, 0.0)

        radii_valid? =
          case Map.get(clip, :border_radii) do
            nil ->
              true

            radii when is_map(radii) ->
              Enum.sort(Map.keys(radii)) == [:bottom_left, :bottom_right, :top_left, :top_right] and
                Enum.all?(radii, fn {_corner, pair} ->
                  case pair do
                    {rx, ry} ->
                      Enum.all?([rx, ry], fn value ->
                        is_number(value) and value >= 0 and
                          match?({:ok, _}, HtmlValidator.normalize_geometry_number(value))
                      end)

                    _ ->
                      false
                  end
                end)

            _ ->
              false
          end

        radii_valid? and Enum.all?(Map.keys(clip), &(&1 in allowed)) and
          Enum.all?(
            [x, y, width, height, radius],
            &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
          ) and
          width >= 0 and height >= 0 and radius >= 0 and
          (rounded? or (width > 0 and height > 0)) and
          Enum.all?(
            [x + width, y + height],
            &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
          ) and
          case Map.fetch(clip, :inner_clip) do
            :error ->
              true

            {:ok, inner} ->
              is_map(inner) and not Map.has_key?(inner, :inner_clip) and
                valid_clip_rectangle?(inner, true)
          end

      _ ->
        false
    end
  end

  defp valid_prepared_box_clips?(box, page_height) do
    case box do
      %{type: :glyph_group, boxes: children} ->
        Enum.all?(children, &valid_prepared_box_clips?(&1, page_height))

      %{type: :drawing_group} ->
        true

      _ ->
        case Map.get(box, :clip) do
          nil ->
            true

          clip ->
            valid_clip_rectangle?(
              normalize_clip_rectangle(
                clip,
                page_height,
                Map.get(box, :snap_to_css_pixel_grid, false) and
                  (Map.has_key?(clip, :inner_clip) or Map.has_key?(clip, :border_radii))
              ),
              true
            )
        end
    end
  end

  # Validation above bounds the single optional inner clip. Normalize radii
  # centrally so every byte-writer path consumes the same prepared geometry.
  defp normalize_clip_rectangle(clip, page_height, snap?) do
    clip =
      if snap? do
        snapped =
          PageGeometry.snap_box_to_css_pixel_grid(
            Map.put(
              %{clip | y: page_height - clip.y - clip.height},
              :snap_to_css_pixel_grid,
              true
            )
          )

        %{
          clip
          | x: snapped.x,
            y: page_height - snapped.y - snapped.height,
            width: snapped.width,
            height: snapped.height
        }
      else
        clip
      end

    clip =
      if Map.has_key?(clip, :border_radius),
        do: Map.update!(clip, :border_radius, &min(&1, min(clip.width, clip.height) / 2)),
        else: clip

    clip =
      case Map.get(clip, :border_radii) do
        nil ->
          clip

        radii ->
          {blx, bly} = radii.bottom_left
          {brx, bry} = radii.bottom_right
          {tlx, tly} = radii.top_left
          {trx, try_} = radii.top_right

          factor =
            [
              {clip.width, blx + brx},
              {clip.width, tlx + trx},
              {clip.height, bly + tly},
              {clip.height, bry + try_}
            ]
            |> Enum.reduce(1.0, fn {extent, sum}, factor ->
              if sum > 0, do: min(factor, extent / sum), else: factor
            end)

          Map.put(
            clip,
            :border_radii,
            Map.new(radii, fn {corner, {rx, ry}} ->
              {corner, {rx * factor, ry * factor}}
            end)
          )
      end

    case Map.fetch(clip, :inner_clip) do
      {:ok, inner} ->
        Map.put(clip, :inner_clip, normalize_clip_rectangle(inner, page_height, snap?))

      :error ->
        clip
    end
  end

  @doc "Validates an optional canonical color shared by computed styles and PDF paint."
  @spec valid_optional_color?(term()) :: boolean()
  def valid_optional_color?(color) do
    case color do
      nil -> true
      color -> valid_color?(color)
    end
  end

  @doc false
  @spec valid_color?(term()) :: boolean()
  def valid_color?(color) do
    case color do
      {red, green, blue} ->
        valid_color_channel?(red) and valid_color_channel?(green) and valid_color_channel?(blue)

      {red, green, blue, alpha} ->
        valid_color_channel?(red) and valid_color_channel?(green) and valid_color_channel?(blue) and
          valid_color_channel?(alpha)

      _ ->
        false
    end
  end

  @compile {:inline, valid_color_channel?: 1}
  defp valid_color_channel?(channel) do
    is_number(channel) and channel >= 0 and channel <= 1
  end

  defp valid_border_widths?(border_widths) do
    case border_widths do
      nil ->
        true

      %{top: top, right: right, bottom: bottom, left: left} ->
        Enum.all?([top, right, bottom, left], &(is_number(&1) and &1 >= 0))

      _ ->
        false
    end
  end

  @doc "Validates canonical per-edge colors shared by computed root geometry and PDF paint."
  @spec valid_border_colors?(term()) :: boolean()
  def valid_border_colors?(border_colors) do
    case border_colors do
      nil ->
        true

      %{top: top, right: right, bottom: bottom, left: left} ->
        Enum.all?([top, right, bottom, left], &valid_optional_color?/1)

      _ ->
        false
    end
  end

  @doc "Validates canonical border styles shared by computed root geometry and PDF paint."
  @spec valid_border_styles?(term()) :: boolean()
  def valid_border_styles?(border_styles) do
    case border_styles do
      nil ->
        true

      %{top: top, right: right, bottom: bottom, left: left} ->
        Enum.all?([top, right, bottom, left], &(&1 in @border_styles))

      _ ->
        false
    end
  end

  defp valid_link_box?(box) do
    url = Map.get(box, :link_url)
    destination = Map.get(box, :link_destination)

    NativeElixirPdfUtilities.Validators.ClipGeometryValidator.valid_quad?(
      Map.get(box, :link_quad_points),
      box
    ) and
      valid_clip_rectangle?(Map.get(box, :link_rect)) and
      case {url, destination} do
        {nil, nil} ->
          true

        _ ->
          width = Map.get(box, :annotation_width, Map.get(box, :width))

          height =
            case box.type do
              :text -> Map.get(box, :line_height, box.font_size * 1.2)
              type when type in [:image, :link_region] -> box.height
            end

          is_number(width) and width > 0 and is_number(height) and height > 0 and
            (url == nil or (is_binary(url) and HtmlValidator.valid_link_url?(url)))
      end
  end

  defp valid_font_box?(box) do
    case Map.get(box, :font_face) do
      %{type: :embedded, id: id, cmap: cmap} when is_binary(id) and is_map(cmap) ->
        box.font == Font.pdf_name(box.font_face) and String.valid?(box.text, :fast_ascii) and
          case Map.fetch(box, :glyph_ids) do
            {:ok, glyphs} when is_list(glyphs) ->
              length(glyphs) == length(String.to_charlist(box.text)) and
                Enum.all?(glyphs, &(is_integer(&1) and &1 in 1..65_535))

            :error ->
              Enum.all?(String.to_charlist(box.text), fn codepoint ->
                glyph = Map.get(cmap, codepoint, 0)
                # PDF CIDToGIDMap entries are fixed unsigned 16-bit glyph indexes.
                is_integer(glyph) and glyph in 1..65_535 and
                  codepoint <= 0x10FFFF and codepoint not in 0xD800..0xDFFF
              end)

            _ ->
              false
          end

      %{type: :built_in, pdf_name: pdf_name} when is_binary(pdf_name) ->
        not Map.has_key?(box, :glyph_ids) and box.font == pdf_name and pdf_name in @built_in_fonts and
          Font.supports_text?(box.font_face, box.text)

      nil ->
        not Map.has_key?(box, :glyph_ids) and box.font in @built_in_fonts and
          Font.supports_text?(%{type: :built_in, family: box.font, pdf_name: box.font}, box.text)

      _ ->
        false
    end
  end

  defp validate_embedded_fonts(pages) do
    pages
    |> Enum.flat_map(& &1.boxes)
    |> Enum.reduce_while({:ok, %{}, %{}}, fn box, {:ok, faces, glyph_faces} ->
      case Map.get(box, :font_face) do
        %{type: :embedded, id: id} = face ->
          glyph_faces =
            if Map.has_key?(box, :glyph_ids),
              do: Map.put(glyph_faces, id, face),
              else: glyph_faces

          case Map.fetch(faces, id) do
            {:ok, ^face} ->
              {:cont, {:ok, faces, glyph_faces}}

            {:ok, previous} ->
              # Compare only serialized font facts when callers supply another
              # face map. Reused immutable faces avoid rebuilding this projection
              # for every glyph box, including list-box row appearances.
              if Map.take(previous, @writer_font_fields) == Map.take(face, @writer_font_fields) and
                   Map.get(previous, :kerning, %{}) == Map.get(face, :kerning, %{}) do
                {:cont, {:ok, Map.put(faces, id, face), glyph_faces}}
              else
                {:halt, {:error, "PDF writer font ID #{inspect(id)} has conflicting face data"}}
              end

            :error ->
              case valid_embedded_font_face?(face) do
                true -> {:cont, {:ok, Map.put(faces, id, face), glyph_faces}}
                false -> {:halt, {:error, "PDF writer requires valid embedded font data"}}
              end
          end

        _ ->
          {:cont, {:ok, faces, glyph_faces}}
      end
    end)
    |> case do
      {:ok, _faces, glyph_faces} -> validate_glyph_faces(pages, glyph_faces)
      {:error, _message} = error -> error
    end
  end

  defp validate_glyph_faces(pages, faces) do
    if map_size(faces) == 0 do
      :ok
    else
      result =
        FontValidator.with_budget(fn ->
          Enum.reduce_while(faces, :ok, fn {id, face}, :ok ->
            boxes =
              for page <- pages,
                  box <- page.boxes,
                  box.type == :text,
                  Map.get(box, :font_face, %{})[:id] == id,
                  do: box

            glyph_count =
              Enum.reduce(boxes, 0, fn box, total ->
                total + length(Map.get(box, :glyph_ids, []))
              end)

            FontValidator.reserve(:max_font_math_work, glyph_count)

            case FontValidator.prepare_glyph_bounds(face) do
              {:ok, bounds} ->
                maximum_glyph = min(tuple_size(bounds), length(face.widths))

                valid =
                  Enum.all?(boxes, fn box ->
                    Enum.all?(Map.get(box, :glyph_ids, []), &(&1 < maximum_glyph))
                  end)

                if valid do
                  keys =
                    Enum.reduce(boxes, MapSet.new(), fn box, keys ->
                      {unicode, glyphs, literal?} =
                        case Map.fetch(box, :glyph_ids) do
                          {:ok, glyphs} ->
                            {String.to_charlist(box.text), glyphs, true}

                          :error ->
                            unicode =
                              box.text
                              |> Font.shape_ligatures(face, Map.get(box, :letter_spacing, 0))
                              |> String.to_charlist()

                            {unicode, Enum.map(unicode, &Map.fetch!(face.cmap, &1)), false}
                        end

                      Enum.reduce(Enum.zip(unicode, glyphs), keys, fn {unicode, glyph}, keys ->
                        key =
                          if unicode in 0xFB00..0xFB04,
                            do: {unicode, glyph, literal?},
                            else: {unicode, glyph}

                        MapSet.put(keys, key)
                      end)
                    end)

                  # PDF CIDs are fixed 16-bit format values, not a tunable resource limit.
                  if MapSet.size(keys) <= 65_535,
                    do: {:cont, :ok},
                    else:
                      {:halt, {:error, "PDF font glyph runs exceed the 65,535 CID format bound"}}
                else
                  {:halt,
                   {:error,
                    "PDF glyph IDs must exist in the selected font's outlines and width metrics"}}
                end

              {:error, {_reason, diagnostic}} ->
                {:halt, {:error, diagnostic.message}}
            end
          end)
        end)

      case result do
        {:error, {reason, diagnostic}} ->
          Diagnostics.error(diagnostic.stage, reason, diagnostic.message,
            operation: :write_pdf,
            module: __MODULE__,
            source: Map.get(diagnostic, :source)
          )

        result ->
          result
      end
    end
  end

  defp valid_embedded_font_face?(face) do
    case face do
      %{
        data: data,
        pdf_name: pdf_name,
        units_per_em: units_per_em,
        widths: widths,
        default_width: default_width,
        cmap: cmap,
        ascent: ascent,
        descent: descent,
        bbox: {x_min, y_min, x_max, y_max}
      }
      when is_binary(data) and is_binary(pdf_name) and is_integer(units_per_em) and
             units_per_em > 0 and is_list(widths) and is_integer(default_width) and
             default_width >= 0 and is_map(cmap) and is_integer(ascent) and
             is_integer(descent) and is_integer(x_min) and is_integer(y_min) and
             is_integer(x_max) and is_integer(y_max) ->
        valid_kerning?(Map.get(face, :kerning, %{}), length(widths)) and
          Regex.match?(~r/\A[A-Za-z0-9_.+-]+\z/, pdf_name) and
          units_per_em <= 65_535 and default_width <= 65_535 and
          Enum.all?([ascent, descent, x_min, y_min, x_max, y_max], &(&1 in -32_768..32_767)) and
          length(widths) <= 65_535 and
          Enum.all?(widths, &(is_integer(&1) and &1 in 0..65_535)) and
          map_size(cmap) <= 65_535

      _ ->
        false
    end
  end

  defp valid_kerning?(kerning, glyph_count) do
    case kerning do
      pairs when is_map(pairs) ->
        map_size(pairs) <= Limits.get(:max_font_kerning_pairs) and
          Enum.all?(pairs, fn
            {{left, right}, value} ->
              # Legacy kern records are signed 16-bit adjustments and their
              # table count is unsigned 16-bit. These format invariants bound
              # every possible accumulated adjustment, independent of budgets.
              is_integer(left) and left >= 0 and left < glyph_count and
                is_integer(right) and right >= 0 and right < glyph_count and
                is_integer(value) and value >= -32_768 * 65_535 and value <= 32_767 * 65_535

            _ ->
              false
          end)

      _ ->
        false
    end
  end
end
