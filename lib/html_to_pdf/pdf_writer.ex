defmodule NativeElixirPdfUtilities.HtmlToPdf.PdfWriter do
  @moduledoc """
  PDF writer stage for the native HTML-to-PDF renderer.

  This module is the low-level PDF byte writer used by the HTML renderer. It
  supports one or more pages containing built-in or embedded-font text boxes,
  simple rectangle fills, borders, URI link annotations, and PNG/JPEG image
  XObjects.
  """

  alias NativeElixirPdfUtilities.HtmlToPdf.Font

  @type page :: NativeElixirPdfUtilities.HtmlToPdf.Pagination.page()
  @type render_option :: NativeElixirPdfUtilities.HtmlToPdf.render_option()

  @doc """
  Renders paginated drawing instructions to a PDF binary.
  """
  @spec render([page()], [render_option()]) :: {:ok, binary()} | {:error, :invalid_pdf_input}
  def render(pages, opts \\ []) do
    case {pages, opts} do
      {pages, opts} when is_list(pages) and is_list(opts) ->
        build_pdf(pages)

      _ ->
        {:error, :invalid_pdf_input}
    end
  end

  defp build_pdf(pages) do
    case pages != [] and Enum.all?(pages, &valid_page?/1) do
      true ->
        {:ok, pages_to_pdf(pages)}

      false ->
        {:error, :invalid_pdf_input}
    end
  end

  defp valid_page?(page) do
    case page do
      %{size: {width, height}, boxes: boxes}
      when is_number(width) and is_number(height) and width > 0 and height > 0 and is_list(boxes) ->
        Enum.all?(boxes, &valid_box?/1)

      _ ->
        false
    end
  end

  defp valid_box?(box) do
    case box do
      %{type: :text, text: text, x: x, y: y, font_size: font_size, font: font, color: {r, g, b}}
      when is_binary(text) and is_number(x) and is_number(y) and is_number(font_size) and
             font_size > 0 and
             is_binary(font) and is_number(r) and is_number(g) and is_number(b) ->
        valid_font_box?(box) and valid_link_box?(box)

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
        valid_optional_color?(fill_color) and valid_optional_color?(stroke_color) and
          (not is_nil(fill_color) or stroke_width > 0)

      %{type: :image, x: x, y: y, width: width, height: height, image: image}
      when is_number(x) and is_number(y) and is_number(width) and is_number(height) and
             width > 0 and height > 0 ->
        valid_image?(image)

      _ ->
        false
    end
  end

  defp valid_image?(image) do
    case image do
      %{
        format: format,
        data: data,
        width_px: width_px,
        height_px: height_px,
        color_space: color_space,
        bits_per_component: 8
      }
      when format in [:png, :jpeg] and is_binary(data) and is_integer(width_px) and
             is_integer(height_px) and width_px > 0 and height_px > 0 and
             color_space in [:device_gray, :device_rgb, :device_cmyk] ->
        true

      _ ->
        false
    end
  end

  defp valid_optional_color?(color) do
    case color do
      nil -> true
      {r, g, b} when is_number(r) and is_number(g) and is_number(b) -> true
      _ -> false
    end
  end

  defp valid_link_box?(box) do
    case Map.get(box, :link_url) do
      nil ->
        true

      link_url when is_binary(link_url) ->
        width = Map.get(box, :annotation_width, Map.get(box, :width))
        is_number(width) and width > 0 and valid_uri?(link_url)

      _ ->
        false
    end
  end

  defp valid_font_box?(box) do
    case Map.get(box, :font_face) do
      %{
        type: :embedded,
        id: id,
        data: data,
        units_per_em: units_per_em,
        widths: widths,
        cmap: cmap
      }
      when is_binary(id) and is_binary(data) and is_integer(units_per_em) and units_per_em > 0 and
             is_list(widths) and is_map(cmap) ->
        box.font == Font.pdf_name(box.font_face)

      %{type: :built_in, pdf_name: pdf_name} when is_binary(pdf_name) ->
        box.font == pdf_name and pdf_name in built_in_fonts()

      nil ->
        box.font in built_in_fonts()

      _ ->
        false
    end
  end

  defp pages_to_pdf(pages) do
    {font_resources, next_object_id} = font_resources(pages, 3)
    image_resources = image_resources(pages, next_object_id)
    first_page_object_id = next_object_id + map_size(image_resources)
    pages_object_id = 2

    {page_entries, _next_object_id} =
      page_entries(pages, pages_object_id, font_resources, image_resources, first_page_object_id)

    page_object_ids = Enum.map(page_entries, & &1.page_object_id)

    page_objects =
      Enum.flat_map(page_entries, fn entry ->
        [
          {entry.page_object_id,
           page_object(
             entry.page,
             pages_object_id,
             font_resources,
             image_resources,
             entry.content_object_id,
             Enum.map(entry.annotation_objects, fn {object_id, _annotation} -> object_id end)
           )},
          {entry.content_object_id, content_object(entry.page, font_resources, image_resources)}
        ] ++ annotation_objects(entry.annotation_objects)
      end)

    objects =
      [
        {1, "<< /Type /Catalog /Pages #{pages_object_id} 0 R >>"},
        {pages_object_id, pages_object(page_object_ids)}
      ] ++ font_objects(font_resources) ++ image_objects(image_resources) ++ page_objects

    objects_to_pdf(objects)
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
         first_page_object_id
       ) do
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
        annotation_objects: annotation_objects
      }

      {entries ++ [entry], next_object_id + 2 + length(annotation_objects)}
    end)
  end

  defp page_object(
         page,
         pages_object_id,
         font_resources,
         image_resources,
         content_object_id,
         annotation_object_ids
       ) do
    {width, height} = page.size
    fonts = font_resource_dictionary(font_resources)
    xobjects = xobject_resource_dictionary(image_resources)
    annotations = annotation_dictionary(annotation_object_ids)

    """
    << /Type /Page /Parent #{pages_object_id} 0 R /MediaBox [0 0 #{format_number(width)} #{format_number(height)}] /Resources << /Font << #{fonts} >>#{xobjects} >> /Contents #{content_object_id} 0 R#{annotations} >>
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

  defp content_object(page, font_resources, image_resources) do
    content = content_stream(page.boxes, font_resources, image_resources)
    length = byte_size(content)

    """
    << /Length #{length} >>
    stream
    #{content}
    endstream
    """
    |> String.trim()
  end

  defp content_stream(boxes, font_resources, image_resources) do
    Enum.map_join(boxes, "\n", fn box ->
      case box.type do
        :text ->
          text_stream(box, font_resources)

        :rect ->
          rect_stream(box)

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

  defp text_stream(box, font_resources) do
    {r, g, b} = box.color
    font_resource = Map.fetch!(font_resources, font_key(box))
    text_operator = text_operator(box, font_resource)

    [
      "BT",
      " /",
      font_resource.name,
      " ",
      format_number(box.font_size),
      " Tf",
      " ",
      format_number(r),
      " ",
      format_number(g),
      " ",
      format_number(b),
      " rg",
      " ",
      format_number(box.x),
      " ",
      format_number(box.y),
      " Td",
      text_operator,
      " ET"
    ]
  end

  defp text_operator(box, font_resource) do
    case Map.get(font_resource, :font_face) do
      %{type: :embedded} = font ->
        " <" <> Font.encode_embedded_text(box.text, font) <> "> Tj"

      _ ->
        " (" <> escape_text(box.text) <> ") Tj"
    end
  end

  defp rect_stream(box) do
    graphics_state =
      ["q"]
      |> put_fill_color(box.fill_color)
      |> put_stroke_color(box.stroke_color, box.stroke_width)
      |> Kernel.++([rect_path(box), paint_operator(box), "Q"])

    Enum.join(graphics_state, " ")
  end

  defp image_stream(box, image_resources) do
    image_resource = Map.fetch!(image_resources, image_key(box.image))

    [
      "q ",
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

      nil ->
        parts
    end
  end

  defp put_stroke_color(parts, color, stroke_width) do
    case {color, stroke_width > 0} do
      {{r, g, b}, true} ->
        parts ++
          [
            format_number(r),
            format_number(g),
            format_number(b),
            "RG",
            format_number(stroke_width),
            "w"
          ]

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

  defp paint_operator(box) do
    case {box.fill_color, box.stroke_width > 0} do
      {nil, true} -> "S"
      {_, true} -> "B"
      {_, false} -> "f"
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
        %{
          name: "F#{index}",
          object_id: object_id,
          descendant_object_id: object_id + 1,
          descriptor_object_id: object_id + 2,
          font_file_object_id: object_id + 3,
          to_unicode_object_id: object_id + 4,
          object_count: 5,
          font_face: font,
          unicode_mappings: Font.unicode_mappings(entry.texts, font),
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
      {resource.to_unicode_object_id, to_unicode_object(resource)}
    ]
  end

  defp embedded_type0_font_object(resource, font) do
    "<< /Type /Font /Subtype /Type0 /BaseFont /#{font.pdf_name} /Encoding /Identity-H /DescendantFonts [#{resource.descendant_object_id} 0 R] /ToUnicode #{resource.to_unicode_object_id} 0 R >>"
  end

  defp embedded_cid_font_object(resource, font) do
    "<< /Type /Font /Subtype /CIDFontType2 /BaseFont /#{font.pdf_name} /CIDSystemInfo << /Registry (Adobe) /Ordering (Identity) /Supplement 0 >> /FontDescriptor #{resource.descriptor_object_id} 0 R /W #{cid_widths(font)} /CIDToGIDMap /Identity >>"
  end

  defp embedded_descriptor_object(resource, font) do
    {x_min, y_min, x_max, y_max} = scale_bbox(font.bbox, font.units_per_em)
    ascent = scale_metric(font.ascent, font.units_per_em)
    descent = scale_metric(font.descent, font.units_per_em)

    "<< /Type /FontDescriptor /FontName /#{font.pdf_name} /Flags 4 /FontBBox [#{x_min} #{y_min} #{x_max} #{y_max}] /ItalicAngle 0 /Ascent #{ascent} /Descent #{descent} /CapHeight #{ascent} /StemV 80 /FontFile2 #{resource.font_file_object_id} 0 R >>"
  end

  defp to_unicode_object(resource) do
    mappings =
      resource.unicode_mappings
      |> Enum.sort_by(fn {glyph_id, _unicode} -> glyph_id end)

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
        "#{length(mappings)} beginbfchar",
        Enum.map_join(mappings, "\n", fn {glyph_id, unicode} ->
          "<#{hex16(glyph_id)}> <#{hex16(unicode)}>"
        end),
        "endbfchar",
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

  defp cid_widths(font) do
    widths =
      font.widths
      |> Enum.with_index()
      |> Enum.reject(fn {_width, glyph_id} -> glyph_id == 0 end)
      |> Enum.map(fn {width, glyph_id} ->
        "#{glyph_id} [#{scale_metric(width, font.units_per_em)}]"
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
    |> Enum.with_index(first_object_id)
    |> Map.new(fn {image, object_id} ->
      key = image_key(image)
      index = object_id - first_object_id + 1
      {key, %{name: "Im#{index}", object_id: object_id, image: image}}
    end)
  end

  defp image_objects(image_resources) do
    image_resources
    |> Enum.sort_by(fn {_key, resource} -> resource.object_id end)
    |> Enum.map(fn {_key, resource} ->
      {resource.object_id, image_object(resource.image)}
    end)
  end

  defp image_object(image) do
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

    "<< /Type /XObject /Subtype /Image /Width #{image.width_px} /Height #{image.height_px} /ColorSpace #{pdf_color_space(image.color_space)} /BitsPerComponent #{image.bits_per_component} /Filter #{filter} /Length #{byte_size(data)} >>\nstream\n" <>
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

  defp image_key(image) do
    :crypto.hash(:sha256, [Atom.to_string(image.format), image.data])
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

  defp built_in_fonts do
    [
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
  end

  defp valid_uri?(uri) do
    Regex.match?(~r/^(https?:\/\/[^\s<>]+|mailto:[^\s<>@]+@[^\s<>@]+)$/iu, uri)
  end

  defp objects_to_pdf(objects) do
    header = "%PDF-1.4\n%\xFF\xFF\xFF\xFF\n"

    {body, offsets, _position} =
      Enum.reduce(objects, {"", [], byte_size(header)}, fn {id, content},
                                                           {acc, offsets, position} ->
        object = "#{id} 0 obj\n#{content}\nendobj\n"

        {acc <> object, offsets ++ [position], position + byte_size(object)}
      end)

    xref_position = byte_size(header <> body)
    size = length(objects) + 1

    xref_entries =
      offsets
      |> Enum.map(&"#{pad_offset(&1)} 00000 n \n")
      |> Enum.join()

    header <>
      body <>
      "xref\n0 #{size}\n0000000000 65535 f \n" <>
      xref_entries <>
      "trailer\n<< /Size #{size} /Root 1 0 R >>\nstartxref\n#{xref_position}\n%%EOF\n"
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
