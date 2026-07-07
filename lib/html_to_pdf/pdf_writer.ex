defmodule NativeElixirPdfUtilities.HtmlToPdf.PdfWriter do
  @moduledoc """
  PDF writer stage for the native HTML-to-PDF renderer.

  This module is the low-level PDF byte writer used by the HTML renderer. It
  supports one or more pages containing built-in-font text boxes.
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
        font in built_in_fonts()

      _ ->
        false
    end
  end

  defp pages_to_pdf(pages) do
    page_count = length(pages)
    font_resources = font_resources(pages)
    first_page_object_id = 3 + map_size(font_resources)
    page_object_ids = Enum.map(0..(page_count - 1)//1, &(&1 * 2 + first_page_object_id))
    pages_object_id = 2

    page_objects =
      pages
      |> Enum.with_index()
      |> Enum.flat_map(fn {page, index} ->
        page_object_id = Enum.at(page_object_ids, index)
        content_object_id = page_object_id + 1

        [
          {page_object_id, page_object(page, pages_object_id, font_resources, content_object_id)},
          {content_object_id, content_object(page, font_resources)}
        ]
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

  defp page_object(page, pages_object_id, font_resources, content_object_id) do
    {width, height} = page.size
    fonts = font_resource_dictionary(font_resources)

    """
    << /Type /Page /Parent #{pages_object_id} 0 R /MediaBox [0 0 #{format_number(width)} #{format_number(height)}] /Resources << /Font << #{fonts} >> >> /Contents #{content_object_id} 0 R >>
    """
    |> String.trim()
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
    end)
  end

  defp font_resources(pages) do
    pages
    |> Enum.flat_map(& &1.boxes)
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
