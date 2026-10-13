defmodule NativeElixirPdfUtilities.HtmlToPdf.PdfWriter do
  @moduledoc """
  PDF writer stage for the native HTML-to-PDF renderer.

  This module is the low-level PDF byte writer used by the HTML renderer. It
  supports one or more pages containing built-in or embedded-font text boxes,
  simple rectangle fills, borders, URI and same-document link annotations, and PNG/JPEG image
  XObjects.
  """

  alias NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf.TrueTypeSubset
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.InfoCodec
  alias NativeElixirPdfUtilities.Pdf.OutlineBuilder
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.WriterValidator
  alias NativeElixirPdfUtilities.Validators.SvgFormValidator

  # CSS defines one pixel as 1/96 inch, which is a fixed 0.75 PDF points.
  @css_pixel_points 0.75
  # One unit of the writer's four-decimal output precision resolves raster ties.
  @raster_boundary_bias 0.0001
  # The Adobe CMap format fixes character-mapping sections at no more than 100 entries.
  @cmap_section_size 100

  @type page :: NativeElixirPdfUtilities.HtmlToPdf.Pagination.page()
  @type render_option :: NativeElixirPdfUtilities.HtmlToPdf.render_option()
  @type error_reason ::
          :invalid_document
          | :invalid_form
          | :invalid_options
          | :invalid_outlines
          | :invalid_pdf_input
          | :resource_limit_exceeded
          | :unsupported_form

  @doc """
  Renders paginated drawing instructions to a PDF binary.

  Embedded-font text boxes may supply `:glyph_ids`, one positive glyph ID for
  each Unicode scalar in `:text`. These explicit runs paint selected glyphs
  without Latin ligature shaping or automatic kerning, and retain their original
  Unicode in ToUnicode mappings. The selected outlines and widths, cumulative
  font work, and fixed PDF CID bound are validated before writing.

  A `:glyph_group` box contains `:actual_text` and a list of text `:boxes`.
  Its painted pieces share one PDF replacement string, so an assembled symbol
  can be extracted once. Groups cannot contain other groups.

  Text and image links may supply `:link_destination` as
  `{:xyz, page_number, x, y}` using one-based pages and PDF bottom-left points.
  The validator checks the referenced page and its bounds. The writer emits an
  explicit PDF destination using the actual page object reference. Prepared HTML
  target identities must first pass through pagination; they are not writer inputs.

  A `:link_region` has positive point dimensions and a URI or explicit destination.
  It creates an annotation over real content already painted by a Form and emits no drawing instructions.

  A `:drawing_group` contains non-negative local `:width` and `:height`, a
  six-value `:transform` tuple, optional `:baseline` within its height, optional
  rectangular `:clip`, and child `:boxes`. Coordinates are PDF points measured
  from the local bottom-left. This bounded slice accepts positive axis-aligned
  scaling and translation, `{scale_x, 0, 0, scale_y, x, y}`. It composes text,
  rectangles, images, glyph groups and nested drawing groups. Clips use local
  coordinates and intersect ancestor clips; linked text receives the same
  transform and visible intersection. A zero dimension prepares an empty clip.
  Dimensions alone do not clip nondegenerate groups. Rotation, skew, reflection
  and grouped form controls return explicit diagnostics pending further support.
  Group depth and all nested drawing instructions use shared resource limits.

  Text and image links may supply `:link_destination` as
  `{:xyz, page_number, x, y}` using one-based pages and PDF bottom-left points.
  The validator checks the referenced page and its bounds. The writer emits an
  explicit PDF destination using the actual page object reference. Prepared HTML
  target identities must first pass through pagination; they are not writer inputs.

  A zero-width `:flow_marker` retains non-negative vertical geometry for layout
  and pagination. It cannot carry text, painting, links or form controls. The
  validator counts it toward layout limits and removes it before serialization.

  Rectangles may supply `:shape` as `:rectangle` or `{:triangle, direction}`, with
  `direction` one of `:up`, `:down`, `:left` or `:right`. `{:chevron, :down}`
  supplies an open down chevron for native menu strokes.

  Text, rectangle and image boxes may carry an absolute `:clip` rectangle with
  finite bounded `x`/`y` and positive `width`/`height`. It may supply a
  nonnegative `border_radius` and one `inner_clip` rectangle with the same numeric
  fields and optional radius. Radii are normalized to half the smaller dimension;
  only rounded or nested clips may have zero area, which paints nothing.
  Each intersected clip is scoped to its box.

  Gauge rectangles can carry a validator-checked `:gauge_fraction`, boolean
  `:gauge_quantize_value` and `:gauge_clip_track` paint flags, and a
  `:gauge_gradient` of four RGB stops. The fixed stops describe platform artwork,
  not a configurable complexity limit. Gradients use scoped PDF shading resources
  in pages and appearance Forms; they create no image or interactive field.
  Native rectangles retain a relative `:gauge_frame` tuple of left/bottom offsets
  and outer width/height. This frame survives pagination and supplies final-page
  pixel alignment after its state and dimensions pass writer validation.

  Native Range rectangles carry a validator-checked `:range_frame` tuple of
  relative left/bottom offsets, host width/height and thumb left/top offsets.
  Fixed source artwork snaps against the original page once and survives the
  existing original-page appearance translation. Overflowing disabled thumbs
  require static rendering because PDF widget BBoxes clip their appearance.
  """
  @spec render([page()], [render_option()]) ::
          {:ok, binary()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def render(pages, opts \\ []) do
    HtmlValidator.with_render_budget(fn ->
      case WriterValidator.prepare(pages, opts) do
        {:ok, context} ->
          with {:ok, pages, controls} <-
                 NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare(
                   context.pages,
                   opts
                 ),
               {font_resources, next_object_id} <- font_resources(pages, 3),
               {:ok, font_resources} <-
                 subset_font_resources(font_resources, context.subset_fonts) do
            pdf =
              pages_to_pdf(
                pages,
                context.metadata,
                context.outlines,
                context.compress_streams,
                font_resources,
                next_object_id
              )

            case controls do
              [] ->
                {:ok, pdf}

              _ ->
                with {:ok, document} <- NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf) do
                  NativeElixirPdfUtilities.Pdf.FormWriter.create(document, controls)
                end
            end
          end

        {:error, {reason, diagnostic}} ->
          {:error, {reason, Map.put(diagnostic, :module, __MODULE__)}}
      end
    end)
  end

  @doc """
  Builds appearance Form objects from validated drawing frames.

  A frame normally paints in local coordinates. Its optional `:paint_frame`
  must have exactly finite bounded `:x`, `:y`, `:width` and `:height`, with
  positive dimensions. The origin is the widget's absolute page paint origin;
  width and height retain the original page dimensions. Boxes and clips then
  stay absolute, snap using that page height and translate into the Form once.
  Ordinary page rendering does not accept this appearance-only transport.
  """
  @spec appearance_objects(term(), term()) ::
          {:ok, [NativeElixirPdfUtilities.Pdf.IncrementalWriter.object_entry()],
           [NativeElixirPdfUtilities.Pdf.Reader.value()], pos_integer()}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def appearance_objects(frames, first_object_id) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, context} <- WriterValidator.prepare_appearances(frames, first_object_id),
           {fonts, next} <- font_resources(context.pages, first_object_id),
           {:ok, fonts} <- subset_font_resources(fonts, context.subset_fonts) do
        frames = prepare_images(context.pages)
        images = image_resources(frames, next)
        states_start = next + image_object_count(images)
        states = graphics_state_resources(frames, states_start)
        first_form_id = states_start + map_size(states)

        {forms, next} =
          Enum.map_reduce(frames, first_form_id, fn frame, id ->
            {fonts, images, states} = page_resources(frame, fonts, images, states)
            {width, height} = frame.size

            content =
              case Map.get(frame, :paint_frame) do
                nil ->
                  content_stream(frame.boxes, height, fonts, images, states)

                origin ->
                  "q 1 0 0 1 #{format_number(-origin.x)} #{format_number(-origin.y)} cm\n" <>
                    content_stream(frame.boxes, origin.height, fonts, images, states) <> "\nQ"
              end

            dictionary =
              "<< /Type /XObject /Subtype /Form /BBox [0 0 #{format_number(width)} #{format_number(height)}] /Resources << /Font << #{font_resource_dictionary(fonts)} >>#{xobject_resource_dictionary(images)}#{graphics_state_resource_dictionary(states)}#{gauge_gradient_dictionary(frame.boxes)} >> /Length #{byte_size(content)} >>"

            {{id, dictionary <> "\nstream\n" <> content <> "\nendstream"}, id + 1}
          end)

        HtmlValidator.check_render_resource(:max_pdf_objects, next, :writer)

        serialized =
          font_objects(fonts, context.compress_streams) ++
            image_objects(images) ++ graphics_state_objects(states) ++ forms

        Enum.reduce(serialized, 0, fn {_id, body}, bytes ->
          bytes = bytes + byte_size(body)
          HtmlValidator.check_render_resource(:max_rendered_pdf_bytes, bytes, :writer)
          bytes
        end)

        with {:ok, objects} <- NativeElixirPdfUtilities.Pdf.Reader.parse_object_bodies(serialized) do
          {:ok, objects, Enum.map(forms, fn {id, _body} -> {:ref, {id, 0}} end), next}
        end
      end
    end)
  end

  defp pages_to_pdf(
         pages,
         metadata,
         outlines,
         compress_streams?,
         font_resources,
         next_object_id
       ) do
    pages = prepare_images(pages)

    image_resources = image_resources(pages, next_object_id)
    graphics_state_object_id = next_object_id + image_object_count(image_resources)
    graphics_state_resources = graphics_state_resources(pages, graphics_state_object_id)
    first_page_object_id = graphics_state_object_id + map_size(graphics_state_resources)
    pages_object_id = 2

    {page_entries, next_object_id} =
      page_entries(
        pages,
        pages_object_id,
        font_resources,
        image_resources,
        graphics_state_resources,
        first_page_object_id
      )

    page_object_ids = Enum.map(page_entries, & &1.page_object_id)
    page_refs = List.to_tuple(page_object_ids)

    outline_objects =
      OutlineBuilder.build(
        outlines,
        fn page -> {elem(page_refs, page - 1), 0} end,
        next_object_id
      )

    next_object_id = outline_objects.next_id

    page_objects =
      Enum.flat_map(page_entries, fn entry ->
        [
          {entry.page_object_id,
           page_object(
             entry.page,
             pages_object_id,
             entry.font_resources,
             entry.image_resources,
             entry.graphics_state_resources,
             entry.content_object_id,
             Enum.map(entry.annotation_objects, fn {object_id, _annotation} -> object_id end)
           )},
          {entry.content_object_id,
           content_object(
             entry.page,
             entry.font_resources,
             entry.image_resources,
             entry.graphics_state_resources,
             compress_streams?
           )}
        ] ++ annotation_objects(entry.annotation_objects, page_refs)
      end)

    objects =
      [
        {1, catalog_object(pages_object_id, outline_objects.root_ref)},
        {pages_object_id, pages_object(page_object_ids)}
      ] ++
        font_objects(font_resources, compress_streams?) ++
        image_objects(image_resources) ++
        graphics_state_objects(graphics_state_resources) ++
        page_objects ++ serialized_outline_objects(outline_objects.objects)

    case map_size(metadata) do
      0 ->
        objects_to_pdf(objects, nil)

      _ ->
        objects_to_pdf(objects ++ [{next_object_id, metadata_object(metadata)}], next_object_id)
    end
  end

  defp catalog_object(pages_object_id, outline_root_ref) do
    outline_entry =
      case outline_root_ref do
        nil -> ""
        {object, generation} -> " /Outlines #{object} #{generation} R"
      end

    "<< /Type /Catalog /Pages #{pages_object_id} 0 R#{outline_entry} >>"
  end

  defp serialized_outline_objects(objects) do
    Enum.map(objects, fn {id, 0, value} ->
      {:ok, serialized} = InfoCodec.serialize_value(value)
      {id, serialized}
    end)
  end

  defp metadata_object(metadata) do
    entries =
      [
        title: :Title,
        author: :Author,
        subject: :Subject,
        keywords: :Keywords,
        producer: :Producer,
        creation_date: :CreationDate,
        modification_date: :ModDate
      ]
      |> Enum.flat_map(fn {field, pdf_key} ->
        case Map.fetch(metadata, field) do
          {:ok, value} ->
            {:ok, encoded} = value |> InfoCodec.encode_text() |> InfoCodec.serialize_value()
            [["/", Atom.to_string(pdf_key), " ", encoded]]

          :error ->
            []
        end
      end)
      |> Enum.intersperse(" ")
      |> IO.iodata_to_binary()

    "<< #{entries} >>"
  end

  defp pages_object(page_object_ids) do
    kids = Enum.map_join(page_object_ids, " ", &"#{&1} 0 R")

    "<< /Type /Pages /Kids [#{kids}] /Count #{length(page_object_ids)} >>"
  end

  defp prepare_images(pages) do
    Enum.map(pages, fn page ->
      boxes =
        Enum.map(page.boxes, fn box ->
          case box.type do
            :image -> Map.put(box, :image_resource_key, image_key(box.image))
            _ -> box
          end
        end)

      %{page | boxes: boxes}
    end)
  end

  defp page_resources(page, font_resources, image_resources, graphics_state_resources) do
    {font_keys, image_keys, state_keys} =
      Enum.reduce(page.boxes, {[], [], []}, fn box, {fonts, images, states} ->
        {fonts, images} =
          case box.type do
            :text ->
              {[font_key(box) | fonts], images}

            :image ->
              {fonts, [box.image_resource_key | images]}

            type
            when type in [
                   :rect,
                   :link_region,
                   :actual_text_start,
                   :actual_text_end,
                   :drawing_group_start,
                   :drawing_group_end
                 ] ->
              {fonts, images}
          end

        {fonts, images, box_graphics_state_keys(box) ++ states}
      end)

    {Map.take(font_resources, font_keys), Map.take(image_resources, image_keys),
     Map.take(graphics_state_resources, state_keys)}
  end

  defp page_entries(
         pages,
         pages_object_id,
         font_resources,
         image_resources,
         graphics_state_resources,
         first_page_object_id
       ) do
    multiple_pages? = length(pages) > 1

    {entries, next_object_id} =
      Enum.reduce(pages, {[], first_page_object_id}, fn page, {entries, next_object_id} ->
        annotations = link_annotations(page)

        annotation_objects =
          annotations
          |> Enum.with_index(next_object_id + 2)
          |> Enum.map(fn {annotation, object_id} -> {object_id, annotation} end)

        {page_fonts, page_images, page_states} =
          if multiple_pages? do
            page_resources(page, font_resources, image_resources, graphics_state_resources)
          else
            {font_resources, image_resources, graphics_state_resources}
          end

        entry = %{
          page: page,
          page_object_id: next_object_id,
          content_object_id: next_object_id + 1,
          pages_object_id: pages_object_id,
          font_resources: page_fonts,
          image_resources: page_images,
          graphics_state_resources: page_states,
          annotation_objects: annotation_objects
        }

        {[entry | entries], next_object_id + 2 + length(annotation_objects)}
      end)

    {Enum.reverse(entries), next_object_id}
  end

  defp page_object(
         page,
         pages_object_id,
         font_resources,
         image_resources,
         graphics_state_resources,
         content_object_id,
         annotation_object_ids
       ) do
    {width, height} = page.size
    fonts = font_resource_dictionary(font_resources)
    xobjects = xobject_resource_dictionary(image_resources)
    graphics_states = graphics_state_resource_dictionary(graphics_state_resources)
    annotations = annotation_dictionary(annotation_object_ids)
    gradients = gauge_gradient_dictionary(page.boxes)

    """
    << /Type /Page /Parent #{pages_object_id} 0 R /MediaBox [0 0 #{format_number(width)} #{format_number(height)}] /Resources << /Font << #{fonts} >>#{xobjects}#{graphics_states}#{gradients} >> /Contents #{content_object_id} 0 R#{annotations} >>
    """
    |> String.trim()
  end

  # Four stops define the fixed platform artwork; this is not a resource ceiling.
  # Shadings are page/Form resources, shared by identical gauge color ramps.
  defp gauge_gradient_dictionary(boxes) do
    gradients =
      boxes
      |> Enum.flat_map(fn box ->
        case Map.get(box, :gauge_gradient) do
          nil -> []
          colors -> [colors]
        end
      end)
      |> Enum.uniq()

    case gradients do
      [] ->
        ""

      gradients ->
        entries =
          Enum.map_join(gradients, " ", fn colors ->
            functions =
              colors
              |> Enum.chunk_every(2, 1, :discard)
              |> Enum.map_join(" ", fn [first, last] ->
                start = first |> Tuple.to_list() |> Enum.map_join(" ", &format_number/1)
                finish = last |> Tuple.to_list() |> Enum.map_join(" ", &format_number/1)
                "<< /FunctionType 2 /Domain [0 1] /C0 [#{start}] /C1 [#{finish}] /N 1 >>"
              end)

            "/#{gauge_gradient_name(colors)} << /ShadingType 2 /ColorSpace /DeviceRGB /Coords [0 1 0 0] /Function << /FunctionType 3 /Domain [0 1] /Functions [#{functions}] /Bounds [0.2 0.5] /Encode [0 1 0 1 0 1] >> /Extend [true true] >>"
          end)

        " /Shading << #{entries} >>"
    end
  end

  defp gauge_gradient_name(colors) do
    "Gauge" <> Base.encode16(:crypto.hash(:sha256, :erlang.term_to_binary(colors)), case: :lower)
  end

  defp annotation_dictionary(annotation_object_ids) do
    case annotation_object_ids do
      [] ->
        ""

      annotation_object_ids ->
        annotations = Enum.map_join(annotation_object_ids, " ", &"#{&1} 0 R")
        " /Annots [#{annotations}]"
    end
  end

  defp content_object(page, font_resources, image_resources, graphics_state_resources, compress?) do
    content =
      content_stream(
        page.boxes,
        elem(page.size, 1),
        font_resources,
        image_resources,
        graphics_state_resources
      )

    case compress? do
      true ->
        stream_object(content, true)

      false ->
        length = byte_size(content)

        """
        << /Length #{length} >>
        stream
        #{content}
        endstream
        """
        |> String.trim()
    end
  end

  defp content_stream(
         boxes,
         page_height,
         font_resources,
         image_resources,
         graphics_state_resources
       ) do
    Enum.map_join(boxes, "\n", fn box ->
      case box.type do
        :text ->
          text_stream(
            box,
            Map.get(box, :drawing_page_height, page_height),
            font_resources,
            graphics_state_resources
          )

        :rect ->
          rect_stream(
            box,
            Map.get(box, :drawing_page_height, page_height),
            graphics_state_resources
          )

        :image ->
          image_stream(box, image_resources)

        :actual_text_start ->
          {:ok, encoded} = box.text |> InfoCodec.encode_text() |> InfoCodec.serialize_value()
          IO.iodata_to_binary(["/Span << /ActualText ", encoded, " >> BDC"])

        :actual_text_end ->
          "EMC"

        :drawing_group_start ->
          transform =
            if Map.get(box, :role) in [:frameset_viewport, :iframe_viewport] do
              {a, b, c, d, x, y} = box.transform
              # Original Chromium clips use .23999999 * 3.125 rather than an
              # exact .75 scale. Keep only paint below those half-pixel ties.
              # Iframe client clipping needs the vertical tie adjustment too.
              # Layout and transported annotation geometry retain their real bounds.
              {a, b, c, d, x - @raster_boundary_bias,
               if(Map.get(box, :role) == :iframe_viewport, do: y + @raster_boundary_bias, else: y)}
            else
              box.transform
            end

          matrix = transform |> Tuple.to_list() |> Enum.map_join(" ", &format_number/1)

          clip =
            case box.clip do
              nil ->
                ""

              %{x: x, y: y, width: width, height: height} ->
                Enum.map_join([x, y, width, height], " ", &format_number/1) <> " re W n"
            end

          "q " <> matrix <> " cm " <> clip

        :link_region ->
          ""

        :drawing_group_end ->
          "Q"
      end
    end)
  end

  defp link_annotations(page) do
    {page_width, page_height} = page.size

    page.boxes
    |> Enum.filter(
      &(&1.type in [:text, :image, :link_region] and
          (is_binary(Map.get(&1, :link_url)) or Map.get(&1, :link_destination) != nil))
    )
    |> Enum.flat_map(fn box ->
      {width, height} =
        case box.type do
          :text ->
            {Map.get(box, :annotation_width, Map.get(box, :width)),
             Map.get(box, :line_height, box.font_size * 1.2)}

          type when type in [:image, :link_region] ->
            {box.width, box.height}
        end

      {region, clip} =
        case Map.get(box, :link_rect) do
          nil ->
            {%{x: box.x, y: box.y, width: width, height: height}, Map.get(box, :clip)}

          region ->
            clip = Map.get(box, :clip)

            content_clip =
              if is_map(clip) and
                   (Map.has_key?(clip, :inner_clip) or Map.has_key?(clip, :border_radius)),
                 do: clip,
                 else: nil

            {region, content_clip}
        end

      {left, bottom, right, top} =
        case Map.fetch(box, :drawing_annotation_rect) do
          {:ok, prepared} ->
            prepared

          :error ->
            DrawingGeometry.intersect_clip(
              {region.x, region.y, region.x + region.width, region.y + region.height},
              DrawingGeometry.box_clip_bounds(clip)
            )
        end

      left = max(0, left)
      bottom = max(0, bottom)
      right = min(page_width, right)
      top = min(page_height, top)

      if right > left and top > bottom,
        do: [
          %{
            destination: Map.get(box, :link_destination) || {:uri, box.link_url},
            rect: {left, bottom, right, top}
          }
        ],
        else: []
    end)
  end

  defp annotation_objects(annotation_objects, page_refs) do
    Enum.map(annotation_objects, fn {object_id, annotation} ->
      {object_id, annotation_object(annotation, page_refs)}
    end)
  end

  defp annotation_object(annotation, page_refs) do
    {left, bottom, right, top} = annotation.rect

    action =
      case annotation.destination do
        {:uri, url} ->
          "/A << /S /URI /URI (#{escape_text(url)}) >>"

        {:xyz, page, x, y} ->
          "/Dest [#{elem(page_refs, page - 1)} 0 R /XYZ #{format_number(x)} #{format_number(y)} null]"
      end

    """
    << /Type /Annot /Subtype /Link /Rect [#{format_number(left)} #{format_number(bottom)} #{format_number(right)} #{format_number(top)}] /Border [0 0 0] #{action} >>
    """
    |> String.trim()
  end

  defp text_stream(box, page_height, font_resources, graphics_state_resources) do
    case {box.text, painted_font_size(box)} do
      {"", _font_size} ->
        []

      {_text, font_size} when font_size == 0 ->
        []

      {_text, font_size} ->
        {red, green, blue} = color_channels(box.color)
        font_resource = Map.fetch!(font_resources, font_key(box))
        text_operator = text_operator(box, font_resource)

        {text_x, text_y} = DrawingGeometry.text_position(box, page_height)

        text =
          [
            "BT",
            " /",
            font_resource.name,
            " ",
            format_number(font_size),
            " Tf",
            " ",
            format_number(red),
            " ",
            format_number(green),
            " ",
            format_number(blue),
            " rg",
            " ",
            format_number(text_x),
            " ",
            format_number(text_y),
            " Td",
            text_operator,
            " ET"
          ]

        text =
          case opacity_resource(box.color, :fill, graphics_state_resources) do
            nil -> text
            resource -> ["q /", resource.name, " gs "] ++ text ++ [" Q"]
          end

        case Map.get(box, :clip) do
          nil ->
            text

          clip ->
            [
              "q ",
              clip_rectangle_stream(clip),
              text,
              " Q"
            ]
        end
    end
  end

  defp painted_font_size(box) do
    case Map.get(box, :snap_to_css_pixel_grid, false) do
      true ->
        size = Float.floor(box.font_size / @css_pixel_points * 100) / 100 * @css_pixel_points
        # Explicit math forms can land on a font-size raster tie. Bias below it;
        # glyph_text_operator compensates advances to retain the measured width.
        if Map.has_key?(box, :glyph_ids) and size > 0,
          do: size - @raster_boundary_bias,
          else: size

      false ->
        box.font_size
    end
  end

  defp text_operator(box, font_resource) do
    operator =
      case {Map.get(font_resource, :font_face), Map.fetch(box, :glyph_ids)} do
        {%{type: :embedded}, {:ok, glyph_ids}} ->
          glyph_text_operator(box, glyph_ids, font_resource)

        {%{type: :embedded} = font, :error} ->
          size = box.font_size
          shaped_text = Font.shape_ligatures(box.text, font, Map.get(box, :letter_spacing, 0))

          shaped_size =
            if Map.get(box, :snap_to_css_pixel_grid, false),
              do: Float.floor(size / 0.75 * 64) / 64 * 0.75,
              else: size

          painted_size = painted_font_size(box)

          case painted_size > 0 and
                 (shaped_size != painted_size or map_size(Map.get(font, :kerning, %{})) > 0) do
            true ->
              glyphs =
                shaped_text
                |> String.to_charlist()
                |> Enum.chunk_every(2, 1, [nil])
                |> Enum.map(fn [codepoint, next] ->
                  glyph_id = Map.get(font.cmap, codepoint, 0)
                  next_id = Map.get(font.cmap, next, 0)

                  width =
                    if glyph_id < tuple_size(font_resource.widths),
                      do: elem(font_resource.widths, glyph_id),
                      else: font.default_width

                  kerning = Map.get(Map.get(font, :kerning, %{}), {glyph_id, next_id}, 0)

                  adjustment =
                    (width * (1 - shaped_size / painted_size) -
                       kerning * shaped_size / painted_size) * 1000 / font.units_per_em

                  {Font.encode_embedded_text(<<codepoint::utf8>>, font_resource.encoding),
                   format_number(adjustment)}
                end)

              {segments, pending} =
                Enum.reduce(glyphs, {[], []}, fn {encoded, adjustment}, {segments, pending} ->
                  pending = [encoded | pending]

                  case adjustment do
                    "0" ->
                      {segments, pending}

                    _ ->
                      text = pending |> Enum.reverse() |> IO.iodata_to_binary()
                      {["<" <> text <> "> " <> adjustment | segments], []}
                  end
                end)

              trailing = pending |> Enum.reverse() |> IO.iodata_to_binary()

              case segments do
                [] ->
                  " <" <> trailing <> "> Tj"

                _ ->
                  " [" <>
                    Enum.join(Enum.reverse(segments), " ") <>
                    if(trailing == "", do: "", else: " <" <> trailing <> ">") <> "] TJ"
              end

            false ->
              " <" <>
                Font.encode_embedded_text(
                  shaped_text,
                  font_resource.encoding,
                  Map.get(box, :letter_spacing, 0)
                ) <> "> Tj"
          end

        _ ->
          " (" <> escape_text(box.text) <> ") Tj"
      end

    case Map.get(box, :letter_spacing, 0.0) do
      letter_spacing when is_number(letter_spacing) and letter_spacing != 0.0 ->
        " " <> format_number(letter_spacing) <> " Tc" <> operator <> " 0 Tc"

      _ ->
        operator
    end
  end

  defp glyph_text_operator(box, glyph_ids, resource) do
    font = resource.font_face

    shaped_size =
      if Map.get(box, :snap_to_css_pixel_grid, false),
        do: Float.floor(box.font_size / 0.75 * 64) / 64 * 0.75,
        else: box.font_size

    painted_size = painted_font_size(box)

    segments =
      box.text
      |> String.to_charlist()
      |> Enum.zip(glyph_ids)
      |> Enum.map(fn {unicode, glyph} ->
        cid = Map.fetch!(resource.encoding.codepoint_to_cid, {unicode, glyph})
        encoded = Base.encode16(<<cid::16>>, case: :upper)
        width = elem(resource.widths, glyph)

        adjustment = width * (1 - shaped_size / painted_size) * 1000 / font.units_per_em

        {encoded, format_number(adjustment)}
      end)

    if Enum.all?(segments, fn {_, adjustment} -> adjustment == "0" end) do
      " <" <> Enum.map_join(segments, fn {encoded, _} -> encoded end) <> "> Tj"
    else
      " [" <>
        Enum.map_join(segments, " ", fn {encoded, adjustment} ->
          "<" <> encoded <> "> " <> adjustment
        end) <> "] TJ"
    end
  end

  # Relative gauge frames survive page translations. Snap the platform artwork
  # against the final page grid, rather than against pre-pagination coordinates.
  defp snap_gauge_box(box, page_height) do
    case Map.get(box, :gauge_frame) do
      nil ->
        box

      {dx, dy, frame_width, frame_height} ->
        frame_x = box.x - dx
        frame_y = box.y - dy
        left = PageGeometry.snap_css_pixel(frame_x)
        right = PageGeometry.snap_css_pixel(frame_x + frame_width)

        {bottom, top} =
          if box.gauge_state.kind == :progress do
            bottom = page_height - PageGeometry.snap_css_pixel(page_height - frame_y)
            top = page_height - PageGeometry.snap_css_pixel(page_height - frame_y - frame_height)
            track_height = (top - bottom) / 2
            track_y = bottom + track_height / 2
            {track_y, track_y + track_height}
          else
            bottom =
              page_height - PageGeometry.snap_css_pixel(page_height - frame_y - frame_height / 4)

            top =
              page_height -
                PageGeometry.snap_css_pixel(page_height - frame_y - frame_height * 3 / 4)

            {bottom, top}
          end

        width = max(right - left, 0.0)
        height = max(top - bottom, 0.0)

        case box.role do
          :gauge_value ->
            inset = if box.gauge_state.kind == :meter, do: min(0.75, height / 2), else: 0.0

            %{
              box
              | x: left + inset,
                y: bottom + inset,
                width: max(width - 2 * inset, 0.0),
                height: max(height - 2 * inset, 0.0),
                border_radius: max(height / 2 - inset, 0.0)
            }

          :gauge_indeterminate ->
            extent = min(width, height)

            %{
              box
              | x: left + (width - extent) / 2,
                y: bottom,
                width: extent,
                height: height,
                border_radius: height / 2
            }

          _ ->
            %{box | x: left, y: bottom, width: width, height: height, border_radius: height / 2}
        end
    end
  end

  defp rect_stream(box, page_height, graphics_state_resources) do
    box = snap_gauge_box(box, page_height)

    box =
      if Map.has_key?(box, :range_frame),
        do: NativeElixirPdfUtilities.HtmlToPdf.RangeArtwork.snap_box(box, page_height),
        else: box

    if box.width <= 0 or box.height <= 0 do
      ""
    else
      stream =
        case Map.get(box, :clip) do
          nil ->
            rect_paint_stream(box, page_height, graphics_state_resources)

          clip ->
            [
              "q ",
              clip_rectangle_stream(clip),
              rect_paint_stream(box, page_height, graphics_state_resources),
              " Q"
            ]
        end

      if Map.get(box, :role) == :range_value,
        do: ["q ", clip_rectangle_stream(box.range_fill_clip), stream, " Q"],
        else: stream
    end
  end

  defp rect_paint_stream(box, page_height, graphics_state_resources) do
    box =
      case box do
        %{snap_to_css_pixel_grid: true, text_baseline_offset: offset} ->
          baseline =
            case box do
              %{baseline_snap_depth: depth} ->
                DrawingGeometry.text_baseline(box.y - offset, page_height, depth)

              _ ->
                page_height - PageGeometry.snap_css_pixel(page_height - box.y + offset)
            end

          %{box | y: baseline + offset}

        _ ->
          box
      end

    box =
      case Map.get(box, :snap_to_css_pixel_grid, false) do
        true ->
          snapped =
            PageGeometry.snap_box_to_css_pixel_grid(%{box | y: page_height - box.y - box.height})

          %{snapped | y: page_height - snapped.y - snapped.height}

        false ->
          box
      end

    box =
      case Map.get(box, :role) do
        :fieldset ->
          %{box | x: box.x - @raster_boundary_bias}

        :frameset_canvas ->
          %{box | y: box.y + @raster_boundary_bias}

        role when role in [:inline_background, :frameset_border, :scrollbar] ->
          # Match Chromium's print-matrix ties at the left and top raster edges.
          %{box | x: box.x - @raster_boundary_bias, y: box.y + @raster_boundary_bias}

        role
        when role in [
               :range_track,
               :range_value,
               :range_border,
               :range_thumb,
               :native_color_button,
               :native_file_button
             ] ->
          # NativeTheme uses the same Chromium print-matrix raster edge rule.
          %{box | x: box.x - @raster_boundary_bias, y: box.y + @raster_boundary_bias}

        :table_border ->
          borders =
            Map.get(box, :border_widths, %{
              top: box.stroke_width,
              right: box.stroke_width,
              bottom: box.stroke_width,
              left: box.stroke_width
            })

          # Collapsed table borders straddle grid edges. Expand only the paint
          # rectangle so pagination retains the original cell bounds.
          %{
            box
            | x: box.x - borders.left / 2,
              y: box.y - borders.bottom / 2,
              width: box.width + (borders.left + borders.right) / 2,
              height: box.height + (borders.top + borders.bottom) / 2
          }

        _ ->
          box
      end

    box =
      if Map.get(box, :role) == :select_arrow do
        # SetupMenuListArrow receives a rounded integer-CSS-pixel paint rect
        # and computes its vertical center with integer division.
        parent =
          box.clip
          |> Map.put(:snap_to_css_pixel_grid, true)
          |> Map.update!(:y, &(page_height - &1 - box.clip.height))
          |> PageGeometry.snap_box_to_css_pixel_grid()

        %{
          box
          | x: PageGeometry.snap_css_pixel(box.x),
            y:
              page_height - parent.y -
                Float.floor(parent.height / @css_pixel_points / 2) * @css_pixel_points -
                box.height / 2
        }
      else
        box
      end

    border_style = uniform_border_style(box)

    case side_specific_border?(box) or
           (border_style in [:dotted, :dashed] and box.border_radius == 0) or
           (Map.get(box, :role) in [:table_border, :table_outline] and
              Map.has_key?(box, :border_widths) and box.border_radius == 0) do
      true ->
        box =
          if Map.get(box, :role) == :table_outline,
            do: Map.put(box, :role, :table_border),
            else: box

        box = Map.put(box, :raster_page_height, page_height)
        side_specific_rect_stream(box, graphics_state_resources)

      false ->
        fill_stream =
          case box.fill_color do
            nil ->
              []

            _color ->
              ["q"]
              |> put_opacity(box.fill_color, :fill, graphics_state_resources)
              |> put_fill_color(box.fill_color)
              |> Kernel.++(
                case Map.get(box, :gauge_fraction) do
                  nil ->
                    []

                  fraction ->
                    extent = box.width * fraction

                    extent =
                      if Map.get(box, :gauge_quantize_value, false),
                        do: PageGeometry.snap_css_pixel(extent),
                        else: extent

                    [
                      "#{format_number(box.x)} #{format_number(box.y)} #{format_number(extent)} #{format_number(box.height)} re W n"
                    ]
                end
              )
              |> Kernel.++(
                case Map.get(box, :gauge_gradient) do
                  nil ->
                    if Map.has_key?(box, :gauge_fraction) or
                         Map.get(box, :gauge_clip_track, false) do
                      [
                        rect_path(box),
                        "W n",
                        "#{format_number(box.x)} #{format_number(box.y)} #{format_number(box.width)} #{format_number(box.height)} re f",
                        "Q"
                      ]
                    else
                      fill_box =
                        if Map.get(box, :role) in [:native_color_button, :native_file_button],
                          do: inset_stroke_box(box),
                          else: box

                      [rect_path(fill_box), "f", "Q"]
                    end

                  colors ->
                    [
                      rect_path(box),
                      "W n",
                      "#{format_number(box.width)} 0 0 #{format_number(box.height)} #{format_number(box.x)} #{format_number(box.y)} cm /#{gauge_gradient_name(colors)} sh",
                      "Q"
                    ]
                end
              )
          end

        stroke_stream =
          case WriterValidator.visible_border?(box) and border_style not in [:none, :hidden] do
            true ->
              stroke_box =
                if Map.get(box, :shape) == {:chevron, :down},
                  do: box,
                  else: inset_stroke_box(box)

              stroke_box =
                if Map.get(box, :snap_to_css_pixel_grid, false) and border_style == :solid and
                     (box.border_radius == 0 or
                        Map.get(box, :role) in [:native_listbox, :native_menu, :css_menu]) and
                     color_alpha(box.stroke_color) == 1.0 and
                     (is_nil(Map.get(box, :form_control)) or
                        not is_nil(Map.get(box, :form_owner))) do
                  # Chromium's 0.23999999 print matrix places edge ties just
                  # below exact CSS-pixel coordinates, as for inline backgrounds.
                  # Control appearances use their own local coordinate system.
                  %{
                    stroke_box
                    | x: stroke_box.x - @raster_boundary_bias,
                      y: stroke_box.y + @raster_boundary_bias
                  }
                else
                  stroke_box
                end

              scale = @css_pixel_points

              stroke_box = %{
                stroke_box
                | x: stroke_box.x / scale,
                  y: stroke_box.y / scale,
                  width: stroke_box.width / scale,
                  height: stroke_box.height / scale,
                  border_radius: stroke_box.border_radius / scale
              }

              contours = PageGeometry.rounded_border_contours(box, page_height)

              if contours do
                # CSS BoxBorderPainter's rounded solid fast path fills the
                # independently snapped outer and inner contours (drawDRRect).
                paths =
                  Enum.map(contours, fn contour ->
                    contour
                    |> Map.update!(:x, &(&1 / scale))
                    |> Map.update!(:y, &(&1 / scale))
                    |> Map.update!(:width, &(&1 / scale))
                    |> Map.update!(:height, &(&1 / scale))
                    |> Map.update!(:border_radius, &(&1 / scale))
                    |> rect_path()
                  end)

                ["q", "#{format_number(scale)} 0 0 #{format_number(scale)} 0 0 cm"]
                |> put_opacity(box.stroke_color, :fill, graphics_state_resources)
                |> put_fill_color(box.stroke_color)
                |> Kernel.++(paths ++ ["f*", "Q"])
              else
                (["q"] ++
                   if(Map.get(box, :gauge_clip_track, false),
                     do: [rect_path(box), "W n"],
                     else: []
                   ) ++
                   ["#{format_number(scale)} 0 0 #{format_number(scale)} 0 0 cm"])
                |> put_opacity(box.stroke_color, :stroke, graphics_state_resources)
                |> put_stroke_color(
                  box.stroke_color,
                  if(Map.get(box, :role) == :native_menu, do: 0.0, else: box.stroke_width / scale)
                )
                |> put_stroke_pattern(border_style, box.stroke_width / scale, nil)
                |> Kernel.++(
                  if(Map.get(box, :role) in [:native_color_button, :native_file_button],
                    do: [
                      "/#{Map.fetch!(graphics_state_resources, {:stroke_adjust, 1.0}).name} gs"
                    ],
                    else: []
                  )
                )
                |> Kernel.++([rect_path(stroke_box), "S", "Q"])
              end

            false ->
              []
          end

        Enum.join(fill_stream ++ border_cutout_stream(stroke_stream, box), " ")
    end
  end

  defp inset_stroke_box(box) do
    inset = box.stroke_width / 2

    %{
      box
      | x: box.x + inset,
        y: box.y + inset,
        width: max(box.width - box.stroke_width, 0.0),
        height: max(box.height - box.stroke_width, 0.0),
        border_radius: max(box.border_radius - inset, 0.0)
    }
  end

  defp clip_rectangle_stream(clip) do
    path =
      if Map.get(clip, :border_radius, 0.0) > 0 do
        [
          rounded_rect_path(
            clip.x,
            clip.y,
            clip.width,
            clip.height,
            clip.border_radius
          ),
          " W n "
        ]
      else
        [
          format_number(clip.x),
          " ",
          format_number(clip.y),
          " ",
          format_number(clip.width),
          " ",
          format_number(clip.height),
          " re W n "
        ]
      end

    case Map.get(clip, :inner_clip) do
      nil -> path
      inner -> [path, clip_rectangle_stream(inner)]
    end
  end

  defp image_stream(box, image_resources) do
    box = PageGeometry.snap_box_to_css_pixel_grid(box)

    box =
      case Map.get(box, :snap_to_css_pixel_grid, false) do
        true ->
          # Keep the far edges inside the CSS rectangle. Exact integer boundaries
          # otherwise make PDF rasterizers allocate an extra resampling pixel.
          %{
            box
            | width: max(box.width - 0.0001, 0.0),
              height: max(box.height - 0.0001, 0.0),
              y: box.y + 0.0001
          }

        false ->
          box
      end

    image_resource = Map.fetch!(image_resources, box.image_resource_key)

    clip =
      case Map.get(box, :clip) do
        nil ->
          []

        clip ->
          clip_rectangle_stream(clip)
      end

    [
      "q ",
      clip,
      format_number(box.width),
      " 0 0 ",
      format_number(box.height),
      " ",
      format_number(box.x),
      " ",
      format_number(box.y),
      " cm /",
      image_resource.name,
      " Do Q"
    ]
  end

  defp put_fill_color(parts, color) do
    case color do
      {r, g, b} ->
        parts ++ [format_number(r), format_number(g), format_number(b), "rg"]

      {r, g, b, _alpha} ->
        parts ++ [format_number(r), format_number(g), format_number(b), "rg"]
    end
  end

  defp put_stroke_color(parts, color, stroke_width) do
    case color do
      {r, g, b} ->
        parts ++
          [
            format_number(r),
            format_number(g),
            format_number(b),
            "RG",
            format_number(stroke_width),
            "w"
          ]

      {r, g, b, _alpha} ->
        parts ++
          [
            format_number(r),
            format_number(g),
            format_number(b),
            "RG",
            format_number(stroke_width),
            "w"
          ]
    end
  end

  defp put_stroke_pattern(parts, border_style, stroke_width, side_length) do
    case {border_style, stroke_width > 0} do
      {:dotted, true} ->
        spacing =
          case side_length do
            length when is_number(length) and length > stroke_width ->
              intervals = max(round((length - stroke_width) / (stroke_width * 2)), 1)
              (length - stroke_width) / intervals

            _ ->
              stroke_width * 2
          end

        parts ++ ["[0 #{format_number(spacing)}] 0 d", "1 J"]

      {:dashed, true} ->
        dash_length = stroke_width * 2

        gap =
          case side_length do
            length when is_number(length) and length > dash_length ->
              count = max(round((length + stroke_width) / (dash_length + stroke_width)), 1)

              case count > 1 do
                true -> max((length - count * dash_length) / (count - 1), 0.0)
                false -> stroke_width
              end

            _ ->
              stroke_width
          end

        parts ++ ["[#{format_number(dash_length)} #{format_number(gap)}] 0 d", "0 J"]

      _ ->
        parts
    end
  end

  defp rect_path(box) do
    case Map.get(box, :shape, :rectangle) do
      {:chevron, :down} ->
        "#{format_number(box.x)} #{format_number(box.y + box.height)} m " <>
          "#{format_number(box.x + box.width / 2)} #{format_number(box.y)} l " <>
          "#{format_number(box.x + box.width)} #{format_number(box.y + box.height)} l"

      {:triangle, direction} ->
        left = box.x
        right = box.x + box.width
        bottom = box.y
        top = box.y + box.height

        [{x, y} | points] =
          case direction do
            :up -> [{left, bottom}, {right, bottom}, {(left + right) / 2, top}]
            :down -> [{left, top}, {right, top}, {(left + right) / 2, bottom}]
            :left -> [{right, bottom}, {right, top}, {left, (bottom + top) / 2}]
            :right -> [{left, bottom}, {left, top}, {right, (bottom + top) / 2}]
          end

        Enum.join(
          [
            "#{format_number(x)} #{format_number(y)} m"
            | Enum.map(points, fn {x, y} -> "#{format_number(x)} #{format_number(y)} l" end)
          ] ++ ["h"],
          " "
        )

      :rectangle ->
        radius = min(box.border_radius, min(box.width, box.height) / 2)

        if radius > 0,
          do: rounded_rect_path(box.x, box.y, box.width, box.height, radius),
          else:
            "#{format_number(box.x)} #{format_number(box.y)} #{format_number(box.width)} #{format_number(box.height)} re"
    end
  end

  defp rounded_rect_path(x, y, width, height, radius) do
    right = x + width
    top = y + height
    control = radius * 0.552_284_7498

    [
      "#{format_number(x + radius)} #{format_number(y)} m",
      "#{format_number(right - radius)} #{format_number(y)} l",
      "#{format_number(right - radius + control)} #{format_number(y)} #{format_number(right)} #{format_number(y + radius - control)} #{format_number(right)} #{format_number(y + radius)} c",
      "#{format_number(right)} #{format_number(top - radius)} l",
      "#{format_number(right)} #{format_number(top - radius + control)} #{format_number(right - radius + control)} #{format_number(top)} #{format_number(right - radius)} #{format_number(top)} c",
      "#{format_number(x + radius)} #{format_number(top)} l",
      "#{format_number(x + radius - control)} #{format_number(top)} #{format_number(x)} #{format_number(top - radius + control)} #{format_number(x)} #{format_number(top - radius)} c",
      "#{format_number(x)} #{format_number(y + radius)} l",
      "#{format_number(x)} #{format_number(y + radius - control)} #{format_number(x + radius - control)} #{format_number(y)} #{format_number(x + radius)} #{format_number(y)} c",
      "h"
    ]
    |> Enum.join(" ")
  end

  defp side_specific_border?(box) do
    case {Map.get(box, :border_widths), Map.get(box, :border_colors),
          Map.get(box, :border_styles)} do
      {%{top: top, right: right, bottom: bottom, left: left}, border_colors, border_styles}
      when box.stroke_width > 0 ->
        Enum.uniq([top, right, bottom, left]) |> length() > 1 or
          side_specific_border_colors?(border_colors, box.stroke_color) or
          side_specific_border_styles?(border_styles)

      _ ->
        false
    end
  end

  defp side_specific_border_styles?(border_styles) do
    case border_styles do
      %{top: top, right: right, bottom: bottom, left: left} ->
        Enum.uniq([top, right, bottom, left]) |> length() > 1 or
          Enum.any?(
            [top, right, bottom, left],
            &(&1 in [:double, :groove, :ridge, :inset, :outset])
          )

      _ ->
        false
    end
  end

  defp side_specific_border_colors?(border_colors, fallback_color) do
    case border_colors do
      %{top: top, right: right, bottom: bottom, left: left} ->
        Enum.uniq([top, right, bottom, left, fallback_color]) |> length() > 1

      _ ->
        false
    end
  end

  defp side_specific_rect_stream(box, graphics_state_resources) do
    fill_parts =
      case box.fill_color do
        nil ->
          []

        _ ->
          ["q"]
          |> put_opacity(box.fill_color, :fill, graphics_state_resources)
          |> put_fill_color(box.fill_color)
          |> Kernel.++([rect_path(box), "f", "Q"])
      end

    stroke_parts =
      box.border_widths
      |> Enum.flat_map(fn {side, stroke_width} ->
        border_side_stream(box, side, stroke_width, graphics_state_resources)
      end)

    contours =
      if Map.get(box, :role) == :temporal_host,
        do: PageGeometry.rounded_border_contours(box, box.raster_page_height)

    stroke_parts =
      if contours,
        do: ["q"] ++ Enum.map(contours, &rect_path/1) ++ ["W* n"] ++ stroke_parts ++ ["Q"],
        else: stroke_parts

    Enum.join(fill_parts ++ border_cutout_stream(stroke_parts, box), " ")
  end

  # Cut only border paint. The legend remains transparent to the fieldset's
  # background, and box-relative offsets survive pagination and inline placement.
  defp border_cutout_stream(parts, box) do
    case Map.get(box, :border_cutout) do
      nil ->
        parts

      %{x: x, y: y, width: width, height: height} ->
        cutout = %{
          x: box.x + x,
          y: box.y + box.height + y,
          width: width,
          height: height,
          border_radius: 0
        }

        ["q", rect_path(box), rect_path(cutout), "W* n"] ++ parts ++ ["Q"]
    end
  end

  defp border_side_color(box, side) do
    case Map.get(box, :border_colors) do
      %{^side => color} -> color
      _ -> box.stroke_color
    end
  end

  defp border_side_style(box, side) do
    case Map.get(box, :border_styles) do
      %{^side => border_style} -> border_style
      _ -> :solid
    end
  end

  defp uniform_border_style(box) do
    border_side_style(box, :top)
  end

  defp border_side_stream(box, side, stroke_width, graphics_state_resources) do
    border_style = border_side_style(box, side)
    color = border_side_color(box, side)

    case {stroke_width > 0, border_style, color} do
      {false, _, _} ->
        []

      {_, border_style, _} when border_style in [:none, :hidden] ->
        []

      {_, _, nil} ->
        []

      {true, :double, color} ->
        line_width = stroke_width / 3

        border_stroke(box, side, line_width, color, :solid, 0.0, graphics_state_resources) ++
          border_stroke(
            box,
            side,
            line_width,
            color,
            :solid,
            stroke_width * 2 / 3,
            graphics_state_resources
          )

      {true, border_style, color} when border_style in [:groove, :ridge] ->
        line_width = stroke_width / 2
        {outer_color, inner_color} = relief_pair(color, side, border_style, Map.get(box, :role))

        strips = [{outer_color, 0.0}, {inner_color, line_width}]
        # Blink paints relief halves from top to bottom and left to right.
        # The fill order matters where two subpixel strips share a raster pixel.
        strips =
          if Map.get(box, :role) == :fieldset and side in [:bottom, :right],
            do: Enum.reverse(strips),
            else: strips

        Enum.flat_map(strips, fn {strip_color, inset} ->
          border_stroke(
            box,
            side,
            line_width,
            strip_color,
            :solid,
            inset,
            graphics_state_resources
          )
        end)

      {true, border_style, color} when border_style in [:inset, :outset] ->
        border_stroke(
          box,
          side,
          stroke_width,
          relief_color(color, side, border_style, Map.get(box, :role)),
          :solid,
          0.0,
          graphics_state_resources
        )

      {true, border_style, color} ->
        border_stroke(
          box,
          side,
          stroke_width,
          color,
          border_style,
          0.0,
          graphics_state_resources
        )
    end
  end

  defp border_stroke(
         box,
         side,
         stroke_width,
         color,
         border_style,
         inset,
         graphics_state_resources
       ) do
    scale = @css_pixel_points

    box = %{
      box
      | x: box.x / scale,
        y: box.y / scale,
        width: box.width / scale,
        height: box.height / scale
    }

    stroke_width = stroke_width / scale
    inset = inset / scale
    side_length = if side in [:top, :bottom], do: box.width, else: box.height

    painted_inset =
      case {Map.get(box, :role), border_style, side, Map.get(box, :raster_page_height)} do
        {:table_border, :solid, side, page_height}
        when side in [:top, :bottom] and is_number(page_height) ->
          painted_edge =
            if side == :top,
              do: box.y + box.height - inset,
              else: box.y + inset + stroke_width

          top_down = page_height / scale - painted_edge
          fractional_pixel = top_down - Float.floor(top_down)
          # Chromium snaps solid table edges on the CSS pixel grid. Bottom
          # edges occupy the preceding pixel except at an exact half pixel.
          grid_edge =
            if side == :bottom and stroke_width <= 1.0 and
                 abs(fractional_pixel - 0.5) > 0.0001,
               do: Float.floor(top_down),
               else: Float.floor(top_down + 0.5)

          adjustment = top_down - grid_edge
          inset + stroke_width / 2 + if(side == :top, do: -adjustment, else: adjustment)

        _ ->
          inset + stroke_width / 2
      end

    if Map.get(box, :role) in [
         :fieldset,
         :iframe_border,
         :table_border,
         :table_cell_background,
         :table_separate_background
       ] and border_style == :solid do
      inset =
        if Map.get(box, :role) == :table_border, do: painted_inset - stroke_width / 2, else: inset

      strip =
        case side do
          :top ->
            %{
              x: box.x,
              y: box.y + box.height - inset - stroke_width,
              width: box.width,
              height: stroke_width
            }

          :bottom ->
            %{x: box.x, y: box.y + inset, width: box.width, height: stroke_width}

          :left ->
            %{x: box.x + inset, y: box.y, width: stroke_width, height: box.height}

          :right ->
            %{
              x: box.x + box.width - inset - stroke_width,
              y: box.y,
              width: stroke_width,
              height: box.height
            }
        end

      strip =
        if Map.get(box, :role) == :fieldset,
          do: strip,
          else: %{
            strip
            | x: strip.x - @raster_boundary_bias / scale,
              y: strip.y + @raster_boundary_bias / scale
          }

      ["q", "#{format_number(scale)} 0 0 #{format_number(scale)} 0 0 cm"]
      |> put_opacity(color, :fill, graphics_state_resources)
      |> put_fill_color(color)
      |> Kernel.++([rect_path(Map.put(strip, :border_radius, 0.0)), "f", "Q"])
    else
      ["q", "#{format_number(scale)} 0 0 #{format_number(scale)} 0 0 cm"]
      |> put_opacity(color, :stroke, graphics_state_resources)
      |> put_stroke_color(color, stroke_width)
      |> put_stroke_pattern(border_style, stroke_width, side_length)
      |> Kernel.++([
        border_side_path(box, side, painted_inset, border_style),
        "S",
        "Q"
      ])
    end
  end

  defp relief_pair(color, side, border_style, role) do
    outer_style =
      case border_style do
        :groove -> :inset
        :ridge -> :outset
      end

    inner_style =
      case border_style do
        :groove -> :outset
        :ridge -> :inset
      end

    {relief_color(color, side, outer_style, role), relief_color(color, side, inner_style, role)}
  end

  defp relief_color(color, side, border_style, role) do
    dark_side? = side in [:top, :left]

    dark? =
      case border_style do
        :inset -> dark_side?
        :outset -> not dark_side?
      end

    channels = color |> Tuple.to_list() |> Enum.take(3)
    value = Enum.max(channels)

    adjusted =
      case {dark?, value} do
        {false, value} when value == 0 ->
          List.duplicate(84 / 255, 3)

        {dark?, value} ->
          target = if dark?, do: max(0.0, value - 0.33), else: min(1.0, value + 0.33)
          multiplier = if value == 0.0, do: 0.0, else: target / value
          Enum.map(channels, &(round(&1 * multiplier * 255) / 255))
      end

    luminance = fn rgb ->
      rgb
      |> Tuple.to_list()
      |> Enum.take(3)
      |> Enum.zip([0.2126, 0.7152, 0.0722])
      |> Enum.reduce(0.0, fn {channel, weight}, sum ->
        linear =
          if channel <= 0.04045,
            do: channel / 12.92,
            else: :math.pow((channel + 0.055) / 1.055, 2.4)

        sum + linear * weight
      end)
    end

    # Chromium's current table relief rule gives very dark borders contrast
    # by lightening once for the shadow and twice for the lighter edge.
    adjusted =
      if role in [
           :iframe_border,
           :table_cell_background,
           :table_separate_background,
           :table_border
         ] do
        relative_luminance = luminance.(color)

        cond do
          relative_luminance <= 0.014443844 ->
            light =
              if value == 0,
                do: List.duplicate(84 / 255, 3),
                else:
                  Enum.map(channels, &(round(&1 * min(1.0, value + 0.33) / value * 255) / 255))

            light_value = Enum.max(light)

            if dark?,
              do: light,
              else:
                Enum.map(
                  light,
                  &(round(&1 * min(1.0, light_value + 0.33) / light_value * 255) / 255)
                )

          not dark? and relative_luminance > 0.83077 ->
            channels

          true ->
            adjusted
        end
      else
        adjusted
      end

    adjusted =
      if role in [:fieldset, :temporal_host] and not dark? do
        # Chromium 152's CalculateInsetOutsetColor keeps the original lighter
        # edge unless its contrast against Color::Dark is below 1.75.
        dark =
          relief_color(color, side, if(side in [:top, :left], do: :inset, else: :outset), nil)

        if (luminance.(color) + 0.05) / (luminance.(dark) + 0.05) >= 1.75,
          do: channels,
          else: adjusted
      else
        adjusted
      end

    case tuple_size(color) do
      3 -> List.to_tuple(adjusted)
      4 -> List.to_tuple(adjusted ++ [elem(color, 3)])
    end
  end

  defp border_side_path(box, side, inset, border_style) do
    left = box.x
    right = box.x + box.width
    bottom = box.y
    top = box.y + box.height

    {start_inset, end_inset} =
      case border_style do
        :dotted -> {inset, inset}
        _ -> {0.0, 0.0}
      end

    case side do
      :top ->
        y = top - inset

        "#{format_number(left + start_inset)} #{format_number(y)} m #{format_number(right - end_inset)} #{format_number(y)} l"

      :right ->
        x = right - inset

        "#{format_number(x)} #{format_number(bottom + start_inset)} m #{format_number(x)} #{format_number(top - end_inset)} l"

      :bottom ->
        pixel_alignment =
          case Map.get(box, :role) do
            role when role in [:table_border, :horizontal_rule, :fieldset] -> 0.0
            _ -> @css_pixel_points / 2
          end

        y = bottom + inset + pixel_alignment

        "#{format_number(left + start_inset)} #{format_number(y)} m #{format_number(right - end_inset)} #{format_number(y)} l"

      :left ->
        pixel_alignment =
          case Map.get(box, :role) do
            :table_border -> @css_pixel_points / 6
            _ -> 0.0
          end

        x = left + inset - pixel_alignment

        "#{format_number(x)} #{format_number(bottom + start_inset)} m #{format_number(x)} #{format_number(top - end_inset)} l"
    end
  end

  defp graphics_state_resources(pages, first_object_id) do
    pages
    |> Enum.flat_map(& &1.boxes)
    |> Enum.flat_map(&box_graphics_state_keys/1)
    |> Enum.uniq()
    |> Enum.sort()
    |> Enum.with_index()
    |> Map.new(fn {{kind, alpha} = key, index} ->
      {key,
       %{
         name: "GS#{index + 1}",
         object_id: first_object_id + index,
         kind: kind,
         alpha: alpha
       }}
    end)
  end

  defp box_graphics_state_keys(box) do
    case box.type do
      :text ->
        opacity_resource_keys(box.color, :fill)

      :rect ->
        border_colors =
          case Map.get(box, :border_colors) do
            colors when is_map(colors) -> Map.values(colors)
            _ -> []
          end

        if(Map.get(box, :role) in [:native_color_button, :native_file_button],
          do: [{:stroke_adjust, 1.0}],
          else: []
        ) ++
          opacity_resource_keys(box.fill_color, :fill) ++
          Enum.flat_map([box.stroke_color | border_colors], fn color ->
            opacity_resource_keys(color, :stroke) ++
              if(Map.get(box, :role) == :fieldset,
                do: opacity_resource_keys(color, :fill),
                else: []
              )
          end)

      type
      when type in [
             :image,
             :link_region,
             :actual_text_start,
             :actual_text_end,
             :drawing_group_start,
             :drawing_group_end
           ] ->
        []
    end
  end

  defp opacity_resource_keys(color, kind) do
    case color_alpha(color) do
      alpha when is_number(alpha) and alpha < 1.0 -> [{kind, alpha * 1.0}]
      _ -> []
    end
  end

  defp graphics_state_objects(graphics_state_resources) do
    graphics_state_resources
    |> Map.values()
    |> Enum.sort_by(& &1.object_id)
    |> Enum.map(fn resource ->
      body =
        if resource.kind == :stroke_adjust do
          # Chromium's AA native button stroke serializes Skia stroke
          # adjustment. Keep this graphics state local to the button paint.
          "<< /Type /ExtGState /SA true >>"
        else
          alpha_key = if resource.kind == :fill, do: "/ca", else: "/CA"
          "<< /Type /ExtGState #{alpha_key} #{format_number(resource.alpha)} >>"
        end

      {resource.object_id, body}
    end)
  end

  defp put_opacity(parts, color, kind, graphics_state_resources) do
    case opacity_resource(color, kind, graphics_state_resources) do
      nil -> parts
      resource -> parts ++ ["/#{resource.name}", "gs"]
    end
  end

  defp opacity_resource(color, kind, graphics_state_resources) do
    case color_alpha(color) do
      alpha when is_number(alpha) and alpha < 1.0 ->
        Map.fetch!(graphics_state_resources, {kind, alpha * 1.0})

      _ ->
        nil
    end
  end

  defp color_alpha(color) do
    case color do
      {_red, _green, _blue, alpha} -> alpha
      _ -> 1.0
    end
  end

  defp color_channels(color) do
    case color do
      {red, green, blue} -> {red, green, blue}
      {red, green, blue, _alpha} -> {red, green, blue}
    end
  end

  defp font_resources(pages, first_object_id) do
    pages
    |> Enum.flat_map(& &1.boxes)
    |> Enum.filter(&(&1.type == :text))
    |> Enum.reduce(%{}, fn box, acc ->
      key = font_key(box)

      case key do
        {:built_in, _name} ->
          # Builtin resources do not consume collected text from later boxes.
          case Map.fetch(acc, key) do
            {:ok, _entry} -> acc
            :error -> Map.put(acc, key, font_entry(key, box))
          end

        _ ->
          Map.update(acc, key, font_entry(key, box), fn entry ->
            entry = update_in(entry.texts, &[font_text_entry(box) | &1])
            if Map.has_key?(box, :glyph_ids), do: Map.put(entry, :glyph_text?, true), else: entry
          end)
      end
    end)
    |> Map.values()
    |> Enum.reduce({%{}, first_object_id, 1}, fn entry, {resources, object_id, index} ->
      resource = font_resource(entry, object_id, index)
      {Map.put(resources, entry.key, resource), object_id + resource.object_count, index + 1}
    end)
    |> case do
      {resources, next_object_id, _index} -> {resources, next_object_id}
    end
  end

  defp subset_font_resources(resources, subset?) do
    case subset? do
      false ->
        {:ok, resources}

      true ->
        Enum.reduce_while(resources, {:ok, %{}}, fn {key, resource}, {:ok, prepared} ->
          case Map.get(resource, :font_face) do
            %{type: :embedded} = font ->
              glyph_ids =
                resource.encoding.cid_to_gid |> Map.values() |> Enum.uniq() |> Enum.sort()

              case TrueTypeSubset.subset(font, glyph_ids) do
                {:ok, data} when data == font.data ->
                  {:cont, {:ok, Map.put(prepared, key, resource)}}

                {:ok, data} ->
                  digest = :crypto.hash(:sha256, :erlang.term_to_binary({font.id, glyph_ids}))

                  prefix =
                    for <<byte <- binary_part(digest, 0, 6)>>,
                      into: "",
                      do: <<?A + rem(byte, 26)>>

                  resource = %{resource | pdf_name: prefix <> "+" <> resource.pdf_name}

                  {:cont, {:ok, Map.put(prepared, key, Map.put(resource, :font_file_data, data))}}

                {:error, _reason} = error ->
                  {:halt, error}
              end

            _ ->
              {:cont, {:ok, Map.put(prepared, key, resource)}}
          end
        end)
    end
  end

  defp font_objects(font_resources, compress?) do
    font_resources
    |> Enum.sort_by(fn {_font, resource} -> resource.object_id end)
    |> Enum.flat_map(fn {_font, resource} ->
      case Map.get(resource, :font_face) do
        %{type: :embedded} = font ->
          embedded_font_objects(resource, font, compress?)

        _ ->
          [
            {resource.object_id,
             "<< /Type /Font /Subtype /Type1 /BaseFont /#{resource.pdf_name} >>"}
          ]
      end
    end)
  end

  defp font_entry(key, box) do
    font_face =
      case Map.get(box, :font_face) do
        nil -> %{type: :built_in, family: box.font, pdf_name: box.font}
        font_face -> font_face
      end

    entry = %{key: key, font_face: font_face, texts: [font_text_entry(box)]}
    if Map.has_key?(box, :glyph_ids), do: Map.put(entry, :glyph_text?, true), else: entry
  end

  defp font_text_entry(box) do
    spacing = Map.get(box, :letter_spacing, 0)

    case Map.fetch(box, :glyph_ids) do
      {:ok, glyph_ids} -> {box.text, spacing, glyph_ids}
      :error -> {box.text, spacing}
    end
  end

  defp glyph_encoding(entries, encoding) do
    Enum.reduce(entries, encoding, fn entry, encoding ->
      case entry do
        {text, _spacing, glyph_ids} ->
          text
          |> String.to_charlist()
          |> Enum.zip(glyph_ids)
          |> Enum.reduce(encoding, fn {unicode, glyph} = key, encoding ->
            case Map.fetch(encoding.codepoint_to_cid, key) do
              {:ok, _cid} ->
                encoding

              :error ->
                normal_cid = Map.get(encoding.codepoint_to_cid, unicode)
                normal_glyph = Map.get(encoding.cid_to_gid, normal_cid)
                # Explicit glyphs preserve literal Unicode, including presentation ligatures.
                if normal_glyph == glyph and unicode not in 0xFB00..0xFB04 do
                  %{
                    encoding
                    | codepoint_to_cid: Map.put(encoding.codepoint_to_cid, key, normal_cid)
                  }
                else
                  cid = map_size(encoding.cid_to_gid) + 1

                  %{
                    encoding
                    | codepoint_to_cid: Map.put(encoding.codepoint_to_cid, key, cid),
                      cid_to_gid: Map.put(encoding.cid_to_gid, cid, glyph),
                      cid_to_unicode: Map.put(encoding.cid_to_unicode, cid, <<unicode::utf8>>)
                  }
                end
            end
          end)

        _ ->
          encoding
      end
    end)
  end

  defp font_resource(entry, object_id, index) do
    case entry.font_face do
      %{type: :embedded} = font ->
        encoding =
          if Map.get(entry, :glyph_text?, false) do
            texts = Enum.reverse(entry.texts)
            normal_texts = Enum.filter(texts, &(tuple_size(&1) == 2))
            glyph_encoding(texts, Font.pdf_encoding(normal_texts, font))
          else
            Font.pdf_encoding(Enum.reverse(entry.texts), font)
          end

        %{
          name: "F#{index}",
          object_id: object_id,
          descendant_object_id: object_id + 1,
          descriptor_object_id: object_id + 2,
          font_file_object_id: object_id + 3,
          cid_to_gid_object_id: object_id + 4,
          to_unicode_object_id: object_id + 5,
          object_count: 6,
          font_face: font,
          widths: List.to_tuple(font.widths),
          encoding: encoding,
          pdf_name: font.pdf_name
        }

      %{type: :built_in, pdf_name: pdf_name} ->
        %{
          name: "F#{index}",
          object_id: object_id,
          object_count: 1,
          font_face: entry.font_face,
          pdf_name: pdf_name
        }
    end
  end

  defp embedded_font_objects(resource, font, compress?) do
    [
      {resource.object_id, embedded_type0_font_object(resource)},
      {resource.descendant_object_id, embedded_cid_font_object(resource, font)},
      {resource.descriptor_object_id, embedded_descriptor_object(resource, font)},
      {resource.font_file_object_id,
       stream_object(Map.get(resource, :font_file_data, font.data), compress?)},
      {resource.cid_to_gid_object_id, cid_to_gid_object(resource, compress?)},
      {resource.to_unicode_object_id, to_unicode_object(resource, compress?)}
    ]
  end

  defp embedded_type0_font_object(resource) do
    "<< /Type /Font /Subtype /Type0 /BaseFont /#{resource.pdf_name} /Encoding /Identity-H /DescendantFonts [#{resource.descendant_object_id} 0 R] /ToUnicode #{resource.to_unicode_object_id} 0 R >>"
  end

  defp embedded_cid_font_object(resource, font) do
    "<< /Type /Font /Subtype /CIDFontType2 /BaseFont /#{resource.pdf_name} /CIDSystemInfo << /Registry (Adobe) /Ordering (Identity) /Supplement 0 >> /FontDescriptor #{resource.descriptor_object_id} 0 R /W #{cid_widths(resource, font)} /CIDToGIDMap #{resource.cid_to_gid_object_id} 0 R >>"
  end

  defp embedded_descriptor_object(resource, font) do
    {x_min, y_min, x_max, y_max} = scale_bbox(font.bbox, font.units_per_em)
    ascent = scale_metric(font.ascent, font.units_per_em)
    descent = scale_metric(font.descent, font.units_per_em)

    "<< /Type /FontDescriptor /FontName /#{resource.pdf_name} /Flags 4 /FontBBox [#{x_min} #{y_min} #{x_max} #{y_max}] /ItalicAngle 0 /Ascent #{ascent} /Descent #{descent} /CapHeight #{ascent} /StemV 80 /FontFile2 #{resource.font_file_object_id} 0 R >>"
  end

  defp to_unicode_object(resource, compress?) do
    mappings =
      resource.encoding.cid_to_unicode
      |> Enum.sort_by(fn {cid, _unicode} -> cid end)

    mapping_sections =
      mappings
      |> Enum.chunk_every(@cmap_section_size)
      |> Enum.map_join("\n", fn section ->
        [
          "#{length(section)} beginbfchar",
          Enum.map_join(section, "\n", fn {cid, unicode} ->
            extracted =
              case unicode do
                0xFB00 -> ~c"ff"
                0xFB01 -> ~c"fi"
                0xFB02 -> ~c"fl"
                0xFB03 -> ~c"ffi"
                0xFB04 -> ~c"ffl"
                value when is_binary(value) -> value
                _ -> [unicode]
              end

            encoded = :unicode.characters_to_binary(extracted, :unicode, {:utf16, :big})
            "<#{hex16(cid)}> <#{Base.encode16(encoded)}>"
          end),
          "endbfchar"
        ]
        |> Enum.join("\n")
      end)

    stream =
      [
        "/CIDInit /ProcSet findresource begin",
        "12 dict begin",
        "begincmap",
        "/CIDSystemInfo << /Registry (Adobe) /Ordering (UCS) /Supplement 0 >> def",
        "/CMapName /Adobe-Identity-UCS def",
        "/CMapType 2 def",
        "1 begincodespacerange",
        "<0000> <FFFF>",
        "endcodespacerange",
        mapping_sections,
        "endcmap",
        "CMapName currentdict /CMap defineresource pop",
        "end",
        "end"
      ]
      |> Enum.join("\n")

    stream_object(stream, compress?)
  end

  defp stream_object(data, compress?) do
    compressed = if compress? and byte_size(data) >= 1_024, do: :zlib.compress(data), else: <<>>

    if compress? and byte_size(data) >= 1_024 and
         byte_size(compressed) + byte_size(" /Filter /FlateDecode") < byte_size(data) and
         byte_size(data) <= byte_size(compressed) * Limits.get(:max_pdf_decompression_ratio) do
      "<< /Length #{byte_size(compressed)} /Filter /FlateDecode >>\nstream\n" <>
        compressed <> "\nendstream"
    else
      "<< /Length #{byte_size(data)} >>\nstream\n" <> data <> "\nendstream"
    end
  end

  defp cid_to_gid_object(resource, compress?) do
    max_cid = Enum.reduce(Map.keys(resource.encoding.cid_to_gid), 0, &max/2)

    data =
      for cid <- 0..max_cid, into: <<>> do
        <<Map.get(resource.encoding.cid_to_gid, cid, 0)::16>>
      end

    stream_object(data, compress?)
  end

  defp cid_widths(resource, font) do
    widths =
      resource.encoding.cid_to_gid
      |> Enum.sort_by(fn {cid, _glyph_id} -> cid end)
      |> Enum.map(fn {cid, glyph_id} ->
        width =
          if glyph_id < tuple_size(resource.widths),
            do: elem(resource.widths, glyph_id),
            else: font.default_width

        "#{cid} [#{format_number(width * 1000 / font.units_per_em)}]"
      end)
      |> Enum.join(" ")

    "[" <> widths <> "]"
  end

  defp font_key(box) do
    case Map.get(box, :font_face) do
      %{type: :embedded, id: id} -> {:embedded, id}
      %{type: :built_in, pdf_name: pdf_name} -> {:built_in, pdf_name}
      nil -> {:built_in, box.font}
    end
  end

  defp image_resources(pages, first_object_id) do
    pages
    |> Enum.flat_map(& &1.boxes)
    |> Enum.filter(&(&1.type == :image))
    |> Enum.uniq_by(& &1.image_resource_key)
    |> Enum.reduce({%{}, first_object_id, 1}, fn %{image: image, image_resource_key: key},
                                                 {resources, object_id, index} ->
      resource =
        case image.format do
          :pdf_form ->
            {:ok, form} = SvgFormValidator.prepare_source(image.data)

            %{
              name: "Im#{index}",
              object_id: object_id,
              object_count: 1 + map_size(form.objects),
              image: image,
              form: form
            }

          _ ->
            mask_object_id = if Map.has_key?(image, :alpha_data), do: object_id + 1
            base_object_count = if is_nil(mask_object_id), do: 1, else: 2

            tint_object_id =
              if image.color_space == :device_cmyk, do: object_id + base_object_count

            object_count = base_object_count + if(is_nil(tint_object_id), do: 0, else: 1)

            %{
              name: "Im#{index}",
              object_id: object_id,
              object_count: object_count,
              mask_object_id: mask_object_id,
              tint_object_id: tint_object_id,
              image: image
            }
        end

      {Map.put(resources, key, resource), object_id + resource.object_count, index + 1}
    end)
    |> elem(0)
  end

  defp image_object_count(image_resources) do
    image_resources
    |> Map.values()
    |> Enum.reduce(0, fn resource, count -> count + resource.object_count end)
  end

  defp image_objects(image_resources) do
    image_resources
    |> Enum.sort_by(fn {_key, resource} -> resource.object_id end)
    |> Enum.flat_map(fn {_key, resource} ->
      image_objects_for_resource(resource)
    end)
  end

  defp image_objects_for_resource(resource) do
    if resource.image.format == :pdf_form do
      form = resource.form

      source_objects =
        Enum.sort_by(form.objects, fn {{id, generation}, _object} -> {id, generation} end)

      mapping =
        source_objects
        |> Enum.with_index(resource.object_id + 1)
        |> Map.new(fn {{ref, _object}, id} -> {ref, id} end)

      form_dictionary =
        form.content_dictionary
        |> Map.delete("Length")
        |> Map.merge(%{
          "Type" => {:name, "XObject"},
          "Subtype" => {:name, "Form"},
          "BBox" => [0, 0, form.width, form.height],
          "Matrix" => [1 / form.width, 0, 0, 1 / form.height, 0, 0],
          "Resources" => form.resources
        })
        |> then(fn dictionary ->
          if is_nil(form.group), do: dictionary, else: Map.put(dictionary, "Group", form.group)
        end)

      objects =
        Enum.map(source_objects, fn {ref, object} ->
          {Map.fetch!(mapping, ref), svg_form_object(object.value, object.stream, mapping)}
        end)

      [{resource.object_id, svg_form_object(form_dictionary, form.stream, mapping)} | objects]
    else
      image_object = {resource.object_id, image_object(resource)}

      objects =
        case resource.mask_object_id do
          nil -> [image_object]
          mask_object_id -> [image_object, {mask_object_id, image_mask_object(resource.image)}]
        end

      case resource.tint_object_id do
        nil ->
          objects

        object_id ->
          # Multilinear interpolation reproduces the unprofiled JPEG conversion
          # R=(1-C)*(1-K), G=(1-M)*(1-K), B=(1-Y)*(1-K).
          samples =
            for black <- 0..1, yellow <- 0..1, magenta <- 0..1, cyan <- 0..1, into: <<>> do
              <<255 * (1 - cyan) * (1 - black), 255 * (1 - magenta) * (1 - black),
                255 * (1 - yellow) * (1 - black)>>
            end

          tint =
            "<< /FunctionType 0 /Domain [0 1 0 1 0 1 0 1] /Range [0 1 0 1 0 1] /Size [2 2 2 2] /BitsPerSample 8 /Length 48 >>\nstream\n" <>
              samples <> "\nendstream"

          objects ++ [{object_id, tint}]
      end
    end
  end

  defp svg_form_object(value, stream, mapping) do
    value = remap_svg_form_refs(value, mapping)

    if is_binary(stream) do
      value = Map.put(value, "Length", byte_size(stream))
      {:ok, serialized} = InfoCodec.serialize_value(value)
      IO.iodata_to_binary([serialized, "\nstream\n", stream, "\nendstream"])
    else
      {:ok, serialized} = InfoCodec.serialize_value(value)
      IO.iodata_to_binary(serialized)
    end
  end

  defp remap_svg_form_refs(value, mapping) do
    case value do
      {:ref, ref} ->
        {:ref, {Map.fetch!(mapping, ref), 0}}

      values when is_list(values) ->
        Enum.map(values, &remap_svg_form_refs(&1, mapping))

      values when is_map(values) ->
        Map.new(values, fn {key, entry} -> {key, remap_svg_form_refs(entry, mapping)} end)

      value ->
        value
    end
  end

  defp image_object(resource) do
    image = resource.image

    data =
      case image.format do
        :png -> :zlib.compress(image.data)
        :jpeg -> image.data
      end

    filter =
      case image.format do
        :png -> "/FlateDecode"
        :jpeg -> "/DCTDecode"
      end

    smask =
      case resource.mask_object_id do
        nil -> ""
        mask_object_id -> " /SMask #{mask_object_id} 0 R"
      end

    color_transform =
      case Map.get(image, :color_transform) do
        value when value in [:rgb, :cmyk, :adobe_cmyk] -> " /DecodeParms << /ColorTransform 0 >>"
        value when value in [:ycbcr, :ycck] -> " /DecodeParms << /ColorTransform 1 >>"
        _ -> ""
      end

    color_space =
      case resource.tint_object_id do
        nil -> pdf_color_space(image.color_space)
        id -> "[/DeviceN [/JpegC /JpegM /JpegY /JpegK] /DeviceRGB #{id} 0 R]"
      end

    decode = if Map.get(image, :inverted_cmyk, false), do: " /Decode [1 0 1 0 1 0 1 0]", else: ""

    "<< /Type /XObject /Subtype /Image /Width #{image.width_px} /Height #{image.height_px} /ColorSpace #{color_space} /BitsPerComponent #{image.bits_per_component} /Filter #{filter}#{smask}#{decode}#{color_transform} /Length #{byte_size(data)} >>\nstream\n" <>
      data <> "\nendstream"
  end

  defp image_mask_object(image) do
    data = :zlib.compress(Map.fetch!(image, :alpha_data))

    "<< /Type /XObject /Subtype /Image /Width #{image.width_px} /Height #{image.height_px} /ColorSpace /DeviceGray /BitsPerComponent #{image.bits_per_component} /Filter /FlateDecode /Length #{byte_size(data)} >>\nstream\n" <>
      data <> "\nendstream"
  end

  defp font_resource_dictionary(font_resources) do
    font_resources
    |> Enum.sort_by(fn {_font, resource} -> resource.object_id end)
    |> Enum.map_join(" ", fn {_font, resource} ->
      "/#{resource.name} #{resource.object_id} 0 R"
    end)
  end

  defp xobject_resource_dictionary(image_resources) do
    case map_size(image_resources) do
      0 ->
        ""

      _ ->
        resources =
          image_resources
          |> Enum.sort_by(fn {_key, resource} -> resource.object_id end)
          |> Enum.map_join(" ", fn {_key, resource} ->
            "/#{resource.name} #{resource.object_id} 0 R"
          end)

        " /XObject << #{resources} >>"
    end
  end

  defp graphics_state_resource_dictionary(graphics_state_resources) do
    case map_size(graphics_state_resources) do
      0 ->
        ""

      _ ->
        resources =
          graphics_state_resources
          |> Map.values()
          |> Enum.sort_by(& &1.object_id)
          |> Enum.map_join(" ", fn resource ->
            "/#{resource.name} #{resource.object_id} 0 R"
          end)

        " /ExtGState << #{resources} >>"
    end
  end

  defp image_key(image) do
    if image.format == :pdf_form do
      :crypto.hash(:sha256, image.data) |> Base.encode16(case: :lower)
    else
      :crypto.hash(:sha256, [
        :erlang.term_to_binary(
          {image.format, image.width_px, image.height_px, image.color_space,
           image.bits_per_component, Map.get(image, :color_transform),
           Map.get(image, :inverted_cmyk, false)}
        ),
        image.data,
        Map.get(image, :alpha_data, "")
      ])
      |> Base.encode16(case: :lower)
    end
  end

  defp pdf_color_space(color_space) do
    case color_space do
      :device_gray -> "/DeviceGray"
      :device_rgb -> "/DeviceRGB"
    end
  end

  defp scale_bbox({x_min, y_min, x_max, y_max}, units_per_em) do
    {
      scale_metric(x_min, units_per_em),
      scale_metric(y_min, units_per_em),
      scale_metric(x_max, units_per_em),
      scale_metric(y_max, units_per_em)
    }
  end

  defp scale_metric(value, units_per_em) do
    value
    |> Kernel.*(1000)
    |> Kernel./(units_per_em)
    |> Float.round()
    |> trunc()
  end

  defp hex16(value) do
    value
    |> Integer.to_string(16)
    |> String.pad_leading(4, "0")
    |> String.upcase()
  end

  defp objects_to_pdf(objects, info_object_id) do
    HtmlValidator.check_render_resource(:max_pdf_objects, length(objects), :writer)
    header = "%PDF-1.4\n%\xFF\xFF\xFF\xFF\n"

    {body, offsets, position} =
      Enum.reduce(objects, {[], [], byte_size(header)}, fn {id, content},
                                                           {pieces, offsets, position} ->
        HtmlValidator.check_render_resource(
          :max_rendered_pdf_bytes,
          position + IO.iodata_length(content),
          :writer
        )

        object = "#{id} 0 obj\n#{content}\nendobj\n"

        {[object | pieces], [position | offsets], position + byte_size(object)}
      end)

    body = Enum.reverse(body)
    offsets = Enum.reverse(offsets)
    xref_position = position
    size = length(objects) + 1

    xref_entries =
      offsets
      |> Enum.map(&"#{pad_offset(&1)} 00000 n \n")
      |> Enum.join()

    info_reference = if is_integer(info_object_id), do: " /Info #{info_object_id} 0 R", else: ""

    output = [
      header,
      body,
      "xref\n0 #{size}\n0000000000 65535 f \n",
      xref_entries,
      "trailer\n<< /Size #{size} /Root 1 0 R#{info_reference} >>\nstartxref\n#{xref_position}\n%%EOF\n"
    ]

    HtmlValidator.check_render_resource(
      :max_rendered_pdf_bytes,
      IO.iodata_length(output),
      :writer
    )

    IO.iodata_to_binary(output)
  end

  defp escape_text(text) do
    text
    |> String.replace("\\", "\\\\")
    |> String.replace("(", "\\(")
    |> String.replace(")", "\\)")
    |> String.replace("\r\n", "\\n")
    |> String.replace("\n", "\\n")
    |> String.replace("\r", "\\n")
  end

  defp pad_offset(offset) do
    offset
    |> Integer.to_string()
    |> String.pad_leading(10, "0")
  end

  defp format_number(number) do
    rounded = Float.round(number * 1.0, 4)

    case rounded == trunc(rounded) do
      true ->
        Integer.to_string(trunc(rounded))

      false ->
        rounded
        |> :erlang.float_to_binary(decimals: 4)
        |> String.trim_trailing("0")
        |> String.trim_trailing(".")
    end
  end
end
