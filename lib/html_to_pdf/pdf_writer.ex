defmodule NativeElixirPdfUtilities.HtmlToPdf.PdfWriter do
  @moduledoc """
  PDF writer stage for the native HTML-to-PDF renderer.

  This module is the low-level PDF byte writer used by the HTML renderer. It
  supports one or more pages containing built-in-font text boxes and simple
  rectangle fills, borders, and URI link annotations.
  """

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
        font in built_in_fonts() and valid_link_box?(box)

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

  defp pages_to_pdf(pages) do
    font_resources = font_resources(pages)
    first_page_object_id = 3 + map_size(font_resources)
    pages_object_id = 2

    {page_entries, _next_object_id} =
      page_entries(pages, pages_object_id, font_resources, first_page_object_id)

    page_object_ids = Enum.map(page_entries, & &1.page_object_id)

    page_objects =
      Enum.flat_map(page_entries, fn entry ->
        [
          {entry.page_object_id,
           page_object(
             entry.page,
             pages_object_id,
             font_resources,
             entry.content_object_id,
             Enum.map(entry.annotation_objects, fn {object_id, _annotation} -> object_id end)
           )},
          {entry.content_object_id, content_object(entry.page, font_resources)}
        ] ++ annotation_objects(entry.annotation_objects)
      end)

    objects =
      [
        {1, "<< /Type /Catalog /Pages #{pages_object_id} 0 R >>"},
        {pages_object_id, pages_object(page_object_ids)}
      ] ++ font_objects(font_resources) ++ page_objects

    objects_to_pdf(objects)
  end

  defp pages_object(page_object_ids) do
    kids = Enum.map_join(page_object_ids, " ", &"#{&1} 0 R")

    "<< /Type /Pages /Kids [#{kids}] /Count #{length(page_object_ids)} >>"
  end

  defp page_entries(pages, pages_object_id, font_resources, first_page_object_id) do
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
        annotation_objects: annotation_objects
      }

      {entries ++ [entry], next_object_id + 2 + length(annotation_objects)}
    end)
  end

  defp page_object(
         page,
         pages_object_id,
         font_resources,
         content_object_id,
         annotation_object_ids
       ) do
    {width, height} = page.size
    fonts = font_resource_dictionary(font_resources)
    annotations = annotation_dictionary(annotation_object_ids)

    """
    << /Type /Page /Parent #{pages_object_id} 0 R /MediaBox [0 0 #{format_number(width)} #{format_number(height)}] /Resources << /Font << #{fonts} >> >> /Contents #{content_object_id} 0 R#{annotations} >>
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

  defp content_object(page, font_resources) do
    content = content_stream(page.boxes, font_resources)
    length = byte_size(content)

    """
    << /Length #{length} >>
    stream
    #{content}
    endstream
    """
    |> String.trim()
  end

  defp content_stream(boxes, font_resources) do
    Enum.map_join(boxes, "\n", fn box ->
      case box.type do
        :text ->
          text_stream(box, font_resources)

        :rect ->
          rect_stream(box)
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
    font_resource = Map.fetch!(font_resources, box.font)

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
      " (",
      escape_text(box.text),
      ") Tj",
      " ET"
    ]
  end

  defp rect_stream(box) do
    graphics_state =
      ["q"]
      |> put_fill_color(box.fill_color)
      |> put_stroke_color(box.stroke_color, box.stroke_width)
      |> Kernel.++([rect_path(box), paint_operator(box), "Q"])

    Enum.join(graphics_state, " ")
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

  defp font_resources(pages) do
    pages
    |> Enum.flat_map(& &1.boxes)
    |> Enum.filter(&(&1.type == :text))
    |> Enum.map(& &1.font)
    |> Enum.uniq()
    |> Enum.with_index(3)
    |> Map.new(fn {font, object_id} ->
      {font, %{name: "F#{object_id - 2}", object_id: object_id}}
    end)
  end

  defp font_objects(font_resources) do
    font_resources
    |> Enum.sort_by(fn {_font, resource} -> resource.object_id end)
    |> Enum.map(fn {font, resource} ->
      {resource.object_id, "<< /Type /Font /Subtype /Type1 /BaseFont /#{font} >>"}
    end)
  end

  defp font_resource_dictionary(font_resources) do
    font_resources
    |> Enum.sort_by(fn {_font, resource} -> resource.object_id end)
    |> Enum.map_join(" ", fn {_font, resource} ->
      "/#{resource.name} #{resource.object_id} 0 R"
    end)
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
