defmodule NativeElixirPdfUtilities.HtmlToPdf.PdfWriter do
  @moduledoc """
  PDF writer stage for the native HTML-to-PDF renderer.

  This module is the low-level PDF byte writer used by the HTML renderer. It
  supports one or more pages containing built-in or embedded-font text boxes,
  simple rectangle fills, borders, URI link annotations, and PNG/JPEG image
  XObjects.
  """

  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.InfoCodec
  alias NativeElixirPdfUtilities.Pdf.OutlineBuilder
  alias NativeElixirPdfUtilities.Validators.WriterValidator

  # CSS defines one pixel as 1/96 inch, which is a fixed 0.75 PDF points.
  @css_pixel_points 0.75
  # The Adobe CMap format fixes character-mapping sections at no more than 100 entries.
  @cmap_section_size 100

  @type page :: NativeElixirPdfUtilities.HtmlToPdf.Pagination.page()
  @type render_option :: NativeElixirPdfUtilities.HtmlToPdf.render_option()
  @type error_reason :: :invalid_pdf_input

  @doc """
  Renders paginated drawing instructions to a PDF binary.
  """
  @spec render([page()], [render_option()]) ::
          {:ok, binary()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def render(pages, opts \\ []) do
    case WriterValidator.prepare(pages, opts) do
      {:ok, context} ->
        {:ok, pages_to_pdf(context.pages, context.metadata, context.outlines)}

      {:error, {reason, diagnostic}} ->
        {:error, {reason, Map.put(diagnostic, :module, __MODULE__)}}
    end
  end

  defp pages_to_pdf(pages, metadata, outlines) do
    {font_resources, next_object_id} = font_resources(pages, 3)
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

    outline_objects =
      OutlineBuilder.build(
        outlines,
        fn page -> {Enum.fetch!(page_object_ids, page - 1), 0} end,
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
             font_resources,
             image_resources,
             graphics_state_resources,
             entry.content_object_id,
             Enum.map(entry.annotation_objects, fn {object_id, _annotation} -> object_id end)
           )},
          {entry.content_object_id,
           content_object(
             entry.page,
             font_resources,
             image_resources,
             graphics_state_resources
           )}
        ] ++ annotation_objects(entry.annotation_objects)
      end)

    objects =
      [
        {1, catalog_object(pages_object_id, outline_objects.root_ref)},
        {pages_object_id, pages_object(page_object_ids)}
      ] ++
        font_objects(font_resources) ++
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

  defp page_entries(
         pages,
         pages_object_id,
         font_resources,
         image_resources,
         graphics_state_resources,
         first_page_object_id
       ) do
    {entries, next_object_id} =
      Enum.reduce(pages, {[], first_page_object_id}, fn page, {entries, next_object_id} ->
        annotations = link_annotations(page)

        annotation_objects =
          annotations
          |> Enum.with_index(next_object_id + 2)
          |> Enum.map(fn {annotation, object_id} -> {object_id, annotation} end)

        entry = %{
          page: page,
          page_object_id: next_object_id,
          content_object_id: next_object_id + 1,
          pages_object_id: pages_object_id,
          font_resources: font_resources,
          image_resources: image_resources,
          graphics_state_resources: graphics_state_resources,
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

    """
    << /Type /Page /Parent #{pages_object_id} 0 R /MediaBox [0 0 #{format_number(width)} #{format_number(height)}] /Resources << /Font << #{fonts} >>#{xobjects}#{graphics_states} >> /Contents #{content_object_id} 0 R#{annotations} >>
    """
    |> String.trim()
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

  defp content_object(page, font_resources, image_resources, graphics_state_resources) do
    content =
      content_stream(page.boxes, font_resources, image_resources, graphics_state_resources)

    length = byte_size(content)

    """
    << /Length #{length} >>
    stream
    #{content}
    endstream
    """
    |> String.trim()
  end

  defp content_stream(boxes, font_resources, image_resources, graphics_state_resources) do
    Enum.map_join(boxes, "\n", fn box ->
      case box.type do
        :text ->
          text_stream(box, font_resources, graphics_state_resources)

        :rect ->
          rect_stream(box, graphics_state_resources)

        :image ->
          image_stream(box, image_resources)
      end
    end)
  end

  defp link_annotations(page) do
    page.boxes
    |> Enum.filter(&(&1.type == :text and is_binary(Map.get(&1, :link_url))))
    |> Enum.map(fn box ->
      width = Map.get(box, :annotation_width, Map.get(box, :width))
      height = Map.get(box, :line_height, box.font_size * 1.2)

      %{
        url: box.link_url,
        rect: {box.x, box.y, box.x + width, box.y + height}
      }
    end)
  end

  defp annotation_objects(annotation_objects) do
    Enum.map(annotation_objects, fn {object_id, annotation} ->
      {object_id, annotation_object(annotation)}
    end)
  end

  defp annotation_object(annotation) do
    {left, bottom, right, top} = annotation.rect

    """
    << /Type /Annot /Subtype /Link /Rect [#{format_number(left)} #{format_number(bottom)} #{format_number(right)} #{format_number(top)}] /Border [0 0 0] /A << /S /URI /URI (#{escape_text(annotation.url)}) >> >>
    """
    |> String.trim()
  end

  defp text_stream(box, font_resources, graphics_state_resources) do
    {red, green, blue} = color_channels(box.color)
    font_resource = Map.fetch!(font_resources, font_key(box))
    text_operator = text_operator(box, font_resource)

    text_y =
      case Map.get(box, :snap_to_css_pixel_grid, false) do
        true ->
          Float.ceil(box.y / @css_pixel_points) * @css_pixel_points + 0.5

        false ->
          box.y
      end

    text =
      [
        "BT",
        " /",
        font_resource.name,
        " ",
        format_number(box.font_size),
        " Tf",
        " ",
        format_number(red),
        " ",
        format_number(green),
        " ",
        format_number(blue),
        " rg",
        " ",
        format_number(box.x),
        " ",
        format_number(text_y),
        " Td",
        text_operator,
        " ET"
      ]

    case opacity_resource(box.color, :fill, graphics_state_resources) do
      nil -> text
      resource -> ["q /", resource.name, " gs "] ++ text ++ [" Q"]
    end
  end

  defp text_operator(box, font_resource) do
    operator =
      case Map.get(font_resource, :font_face) do
        %{type: :embedded} ->
          " <" <> Font.encode_embedded_text(box.text, font_resource.encoding) <> "> Tj"

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

  defp rect_stream(box, graphics_state_resources) do
    case side_specific_border?(box) do
      true ->
        side_specific_rect_stream(box, graphics_state_resources)

      false ->
        border_style = uniform_border_style(box)
        box = snap_box_to_css_pixel_grid(box)

        fill_stream =
          case box.fill_color do
            nil ->
              []

            _color ->
              ["q"]
              |> put_opacity(box.fill_color, :fill, graphics_state_resources)
              |> put_fill_color(box.fill_color)
              |> Kernel.++([rect_path(box), "f", "Q"])
          end

        stroke_stream =
          case WriterValidator.visible_border?(box) and border_style not in [:none, :hidden] do
            true ->
              stroke_box = inset_stroke_box(box)

              ["q"]
              |> put_opacity(box.stroke_color, :stroke, graphics_state_resources)
              |> put_stroke_color(box.stroke_color, box.stroke_width)
              |> put_stroke_pattern(border_style, box.stroke_width)
              |> Kernel.++([rect_path(stroke_box), "S", "Q"])

            false ->
              []
          end

        Enum.join(fill_stream ++ stroke_stream, " ")
    end
  end

  defp snap_box_to_css_pixel_grid(box) do
    case Map.get(box, :snap_to_css_pixel_grid, false) do
      true ->
        left = snap_css_pixel(box.x)
        bottom = snap_css_pixel(box.y)
        right = snap_css_pixel(box.x + box.width)
        top = snap_css_pixel(box.y + box.height)

        %{box | x: left, y: bottom, width: max(right - left, 0.0), height: max(top - bottom, 0.0)}

      false ->
        box
    end
  end

  defp snap_css_pixel(value) do
    Float.floor(value / @css_pixel_points + 0.5) * @css_pixel_points
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

  defp image_stream(box, image_resources) do
    box = snap_box_to_css_pixel_grid(box)
    image_resource = Map.fetch!(image_resources, image_key(box.image))

    clip =
      case Map.get(box, :clip) do
        %{x: x, y: y, width: width, height: height} ->
          [
            format_number(x),
            " ",
            format_number(y),
            " ",
            format_number(width),
            " ",
            format_number(height),
            " re W n "
          ]

        _ ->
          []
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

  defp put_stroke_pattern(parts, border_style, stroke_width) do
    case {border_style, stroke_width > 0} do
      {:dotted, true} ->
        parts ++ ["[0 #{format_number(stroke_width * 2)}] 0 d", "1 J"]

      {:dashed, true} ->
        dash_length = stroke_width * 3
        parts ++ ["[#{format_number(dash_length)} #{format_number(dash_length)}] 0 d", "0 J"]

      _ ->
        parts
    end
  end

  defp rect_path(box) do
    radius = min(box.border_radius, min(box.width, box.height) / 2)

    case radius > 0 do
      true ->
        rounded_rect_path(box.x, box.y, box.width, box.height, radius)

      false ->
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

    Enum.join(fill_parts ++ stroke_parts, " ")
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
        {outer_color, inner_color} = relief_pair(color, side, border_style)

        border_stroke(
          box,
          side,
          line_width,
          outer_color,
          :solid,
          0.0,
          graphics_state_resources
        ) ++
          border_stroke(
            box,
            side,
            line_width,
            inner_color,
            :solid,
            line_width,
            graphics_state_resources
          )

      {true, border_style, color} when border_style in [:inset, :outset] ->
        border_stroke(
          box,
          side,
          stroke_width,
          relief_color(color, side, border_style),
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
    ["q"]
    |> put_opacity(color, :stroke, graphics_state_resources)
    |> put_stroke_color(color, stroke_width)
    |> put_stroke_pattern(border_style, stroke_width)
    |> Kernel.++([border_side_path(box, side, inset), "S", "Q"])
  end

  defp relief_pair(color, side, border_style) do
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

    {relief_color(color, side, outer_style), relief_color(color, side, inner_style)}
  end

  defp relief_color(color, side, border_style) do
    dark_side? = side in [:top, :left]

    dark? =
      case border_style do
        :inset -> dark_side?
        :outset -> not dark_side?
      end

    case dark? do
      true -> shade_color(color, 0.5)
      false -> tint_color(color, 0.5)
    end
  end

  defp shade_color(color, amount) do
    case color do
      {red, green, blue} ->
        {red * amount, green * amount, blue * amount}

      {red, green, blue, alpha} ->
        {red * amount, green * amount, blue * amount, alpha}
    end
  end

  defp tint_color(color, amount) do
    case color do
      {red, green, blue} ->
        {
          red + (1 - red) * amount,
          green + (1 - green) * amount,
          blue + (1 - blue) * amount
        }

      {red, green, blue, alpha} ->
        {
          red + (1 - red) * amount,
          green + (1 - green) * amount,
          blue + (1 - blue) * amount,
          alpha
        }
    end
  end

  defp border_side_path(box, side, inset) do
    left = box.x
    right = box.x + box.width
    bottom = box.y
    top = box.y + box.height

    case side do
      :top ->
        y = top - inset

        "#{format_number(left)} #{format_number(y)} m #{format_number(right)} #{format_number(y)} l"

      :right ->
        x = right - inset

        "#{format_number(x)} #{format_number(bottom)} m #{format_number(x)} #{format_number(top)} l"

      :bottom ->
        y = bottom + inset

        "#{format_number(left)} #{format_number(y)} m #{format_number(right)} #{format_number(y)} l"

      :left ->
        x = left + inset

        "#{format_number(x)} #{format_number(bottom)} m #{format_number(x)} #{format_number(top)} l"
    end
  end

  defp graphics_state_resources(pages, first_object_id) do
    pages
    |> Enum.flat_map(& &1.boxes)
    |> Enum.flat_map(fn box ->
      case box.type do
        :text ->
          opacity_resource_keys(box.color, :fill)

        :rect ->
          border_colors =
            case Map.get(box, :border_colors) do
              colors when is_map(colors) -> Map.values(colors)
              _ -> []
            end

          opacity_resource_keys(box.fill_color, :fill) ++
            Enum.flat_map([box.stroke_color | border_colors], fn color ->
              opacity_resource_keys(color, :stroke)
            end)

        :image ->
          []
      end
    end)
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
      alpha_key = if resource.kind == :fill, do: "/ca", else: "/CA"

      {resource.object_id, "<< /Type /ExtGState #{alpha_key} #{format_number(resource.alpha)} >>"}
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

      Map.update(acc, key, font_entry(key, box), fn entry ->
        update_in(entry.texts, &(&1 ++ [box.text]))
      end)
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

  defp font_objects(font_resources) do
    font_resources
    |> Enum.sort_by(fn {_font, resource} -> resource.object_id end)
    |> Enum.flat_map(fn {_font, resource} ->
      case Map.get(resource, :font_face) do
        %{type: :embedded} = font ->
          embedded_font_objects(resource, font)

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

    %{key: key, font_face: font_face, texts: [box.text]}
  end

  defp font_resource(entry, object_id, index) do
    case entry.font_face do
      %{type: :embedded} = font ->
        encoding = Font.pdf_encoding(entry.texts, font)

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

  defp embedded_font_objects(resource, font) do
    [
      {resource.object_id, embedded_type0_font_object(resource, font)},
      {resource.descendant_object_id, embedded_cid_font_object(resource, font)},
      {resource.descriptor_object_id, embedded_descriptor_object(resource, font)},
      {resource.font_file_object_id, stream_object(font.data)},
      {resource.cid_to_gid_object_id, cid_to_gid_object(resource)},
      {resource.to_unicode_object_id, to_unicode_object(resource)}
    ]
  end

  defp embedded_type0_font_object(resource, font) do
    "<< /Type /Font /Subtype /Type0 /BaseFont /#{font.pdf_name} /Encoding /Identity-H /DescendantFonts [#{resource.descendant_object_id} 0 R] /ToUnicode #{resource.to_unicode_object_id} 0 R >>"
  end

  defp embedded_cid_font_object(resource, font) do
    "<< /Type /Font /Subtype /CIDFontType2 /BaseFont /#{font.pdf_name} /CIDSystemInfo << /Registry (Adobe) /Ordering (Identity) /Supplement 0 >> /FontDescriptor #{resource.descriptor_object_id} 0 R /W #{cid_widths(resource, font)} /CIDToGIDMap #{resource.cid_to_gid_object_id} 0 R >>"
  end

  defp embedded_descriptor_object(resource, font) do
    {x_min, y_min, x_max, y_max} = scale_bbox(font.bbox, font.units_per_em)
    ascent = scale_metric(font.ascent, font.units_per_em)
    descent = scale_metric(font.descent, font.units_per_em)

    "<< /Type /FontDescriptor /FontName /#{font.pdf_name} /Flags 4 /FontBBox [#{x_min} #{y_min} #{x_max} #{y_max}] /ItalicAngle 0 /Ascent #{ascent} /Descent #{descent} /CapHeight #{ascent} /StemV 80 /FontFile2 #{resource.font_file_object_id} 0 R >>"
  end

  defp to_unicode_object(resource) do
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
            "<#{hex16(cid)}> <#{hex16(unicode)}>"
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

    stream_object(stream)
  end

  defp stream_object(data) do
    "<< /Length #{byte_size(data)} >>\nstream\n" <> data <> "\nendstream"
  end

  defp cid_to_gid_object(resource) do
    max_cid = Enum.reduce(Map.keys(resource.encoding.cid_to_gid), 0, &max/2)

    data =
      for cid <- 0..max_cid, into: <<>> do
        <<Map.get(resource.encoding.cid_to_gid, cid, 0)::16>>
      end

    stream_object(data)
  end

  defp cid_widths(resource, font) do
    widths =
      resource.encoding.cid_to_gid
      |> Enum.sort_by(fn {cid, _glyph_id} -> cid end)
      |> Enum.map(fn {cid, glyph_id} ->
        width = Enum.at(font.widths, glyph_id, font.default_width)
        "#{cid} [#{scale_metric(width, font.units_per_em)}]"
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
    |> Enum.map(& &1.image)
    |> Enum.uniq_by(&image_key/1)
    |> Enum.reduce({%{}, first_object_id, 1}, fn image, {resources, object_id, index} ->
      key = image_key(image)
      mask_object_id = if Map.has_key?(image, :alpha_data), do: object_id + 1
      object_count = if is_nil(mask_object_id), do: 1, else: 2

      resource = %{
        name: "Im#{index}",
        object_id: object_id,
        mask_object_id: mask_object_id,
        image: image
      }

      {Map.put(resources, key, resource), object_id + object_count, index + 1}
    end)
    |> elem(0)
  end

  defp image_object_count(image_resources) do
    image_resources
    |> Map.values()
    |> Enum.reduce(0, fn resource, count ->
      case resource.mask_object_id do
        nil -> count + 1
        _ -> count + 2
      end
    end)
  end

  defp image_objects(image_resources) do
    image_resources
    |> Enum.sort_by(fn {_key, resource} -> resource.object_id end)
    |> Enum.flat_map(fn {_key, resource} ->
      image_objects_for_resource(resource)
    end)
  end

  defp image_objects_for_resource(resource) do
    image_object = {resource.object_id, image_object(resource)}

    case resource.mask_object_id do
      nil -> [image_object]
      mask_object_id -> [image_object, {mask_object_id, image_mask_object(resource.image)}]
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

    "<< /Type /XObject /Subtype /Image /Width #{image.width_px} /Height #{image.height_px} /ColorSpace #{pdf_color_space(image.color_space)} /BitsPerComponent #{image.bits_per_component} /Filter #{filter}#{smask} /Length #{byte_size(data)} >>\nstream\n" <>
      data <> "\nendstream"
  end

  defp image_mask_object(image) do
    data = :zlib.compress(Map.fetch!(image, :alpha_data))

    "<< /Type /XObject /Subtype /Image /Width #{image.width_px} /Height #{image.height_px} /ColorSpace /DeviceGray /BitsPerComponent 8 /Filter /FlateDecode /Length #{byte_size(data)} >>\nstream\n" <>
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
    :crypto.hash(:sha256, [
      Atom.to_string(image.format),
      image.data,
      Map.get(image, :alpha_data, "")
    ])
    |> Base.encode16(case: :lower)
  end

  defp pdf_color_space(color_space) do
    case color_space do
      :device_gray -> "/DeviceGray"
      :device_rgb -> "/DeviceRGB"
      :device_cmyk -> "/DeviceCMYK"
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
    header = "%PDF-1.4\n%\xFF\xFF\xFF\xFF\n"

    {body, offsets, position} =
      Enum.reduce(objects, {[], [], byte_size(header)}, fn {id, content},
                                                           {pieces, offsets, position} ->
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

    IO.iodata_to_binary([
      header,
      body,
      "xref\n0 #{size}\n0000000000 65535 f \n",
      xref_entries,
      "trailer\n<< /Size #{size} /Root 1 0 R#{info_reference} >>\nstartxref\n#{xref_position}\n%%EOF\n"
    ])
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
