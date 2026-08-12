defmodule NativeElixirPdfUtilities.Validators.WriterValidator do
  @moduledoc """
  Semantic validation and normalization for the low-level PDF writer.

  Successful preparation returns pages and normalized metadata that the byte
  writer can serialize without revalidating the page model.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.Font

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

  @typedoc "Validated input consumed by the PDF byte writer."
  @type context :: %{required(:pages) => [map()], required(:metadata) => map()}

  @doc """
  Validates pages and writer options and normalizes document metadata.
  """
  @spec prepare(term(), term()) ::
          {:ok, context()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare(pages, opts) do
    case {pages, opts} do
      {pages, opts} when is_list(pages) and is_list(opts) ->
        case Keyword.keyword?(opts) do
          true ->
            with {:ok, metadata} <- normalize_metadata(Keyword.get(opts, :metadata, [])),
                 true <- pages != [] and Enum.all?(pages, &valid_page?/1) do
              {:ok, %{pages: pages, metadata: metadata}}
            else
              :error ->
                Diagnostics.error(
                  :pdf,
                  :invalid_pdf_input,
                  "PDF metadata must use supported fields and value types",
                  operation: :write_pdf,
                  module: __MODULE__
                )

              false ->
                Diagnostics.error(
                  :pdf,
                  :invalid_pdf_input,
                  "PDF writer requires non-empty valid pages",
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
      %{type: :text, text: text, x: x, y: y, font_size: font_size, font: font, color: color}
      when is_binary(text) and is_number(x) and is_number(y) and is_number(font_size) and
             font_size > 0 and is_binary(font) ->
        valid_color?(color) and valid_font_box?(box) and valid_link_box?(box)

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
          valid_border_widths?(Map.get(box, :border_widths)) and
          valid_border_colors?(Map.get(box, :border_colors)) and
          valid_border_styles?(Map.get(box, :border_styles)) and
          (not is_nil(fill_color) or visible_border?(box))

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
        case Map.get(image, :alpha_data) do
          nil ->
            true

          alpha when is_binary(alpha) ->
            image.format == :png and byte_size(alpha) == image.width_px * image.height_px

          _ ->
            false
        end

      _ ->
        false
    end
  end

  defp valid_optional_color?(color) do
    case color do
      nil -> true
      color -> valid_color?(color)
    end
  end

  defp valid_color?(color) do
    case color do
      {red, green, blue} -> Enum.all?([red, green, blue], &valid_color_channel?/1)
      {red, green, blue, alpha} -> Enum.all?([red, green, blue, alpha], &valid_color_channel?/1)
      _ -> false
    end
  end

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

  defp valid_border_colors?(border_colors) do
    case border_colors do
      nil ->
        true

      %{top: top, right: right, bottom: bottom, left: left} ->
        Enum.all?([top, right, bottom, left], &valid_optional_color?/1)

      _ ->
        false
    end
  end

  defp valid_border_styles?(border_styles) do
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
    case Map.get(box, :link_url) do
      nil ->
        true

      link_url when is_binary(link_url) ->
        width = Map.get(box, :annotation_width, Map.get(box, :width))

        is_number(width) and width > 0 and
          Regex.match?(~r/^(https?:\/\/[^\s<>]+|mailto:[^\s<>@]+@[^\s<>@]+)$/iu, link_url)

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
        box.font == Font.pdf_name(box.font_face) and Font.supports_text?(box.font_face, box.text)

      %{type: :built_in, pdf_name: pdf_name} when is_binary(pdf_name) ->
        box.font == pdf_name and pdf_name in @built_in_fonts and
          Font.supports_text?(box.font_face, box.text)

      nil ->
        box.font in @built_in_fonts and
          Font.supports_text?(%{type: :built_in, family: box.font, pdf_name: box.font}, box.text)

      _ ->
        false
    end
  end

  defp normalize_metadata(metadata) do
    metadata =
      case metadata do
        metadata when is_map(metadata) -> metadata
        metadata when is_list(metadata) -> if Keyword.keyword?(metadata), do: Map.new(metadata)
        _ -> nil
      end

    allowed_fields = [:title, :author, :subject, :keywords, :creation_date, :modification_date]

    case is_map(metadata) and Enum.all?(Map.keys(metadata), &(&1 in allowed_fields)) do
      true ->
        Enum.reduce_while(metadata, {:ok, %{}}, fn {field, value}, {:ok, normalized} ->
          case normalize_metadata_value(field, value) do
            {:ok, value} -> {:cont, {:ok, Map.put(normalized, field, value)}}
            :error -> {:halt, :error}
          end
        end)

      false ->
        :error
    end
  end

  defp normalize_metadata_value(field, value) do
    case {field, value} do
      {field, value} when field in [:title, :author, :subject] and is_binary(value) ->
        if String.valid?(value), do: {:ok, value}, else: :error

      {:keywords, value} when is_binary(value) ->
        if String.valid?(value), do: {:ok, value}, else: :error

      {:keywords, values} when is_list(values) ->
        case Enum.all?(values, &(is_binary(&1) and String.valid?(&1))) do
          true -> {:ok, Enum.join(values, ", ")}
          false -> :error
        end

      {field, value} when field in [:creation_date, :modification_date] ->
        pdf_date(value)

      _ ->
        :error
    end
  end

  defp pdf_date(value) do
    case value do
      %DateTime{} = date_time ->
        offset_seconds = date_time.utc_offset + date_time.std_offset
        sign = if offset_seconds < 0, do: "-", else: "+"
        offset_seconds = abs(offset_seconds)
        hours = div(offset_seconds, 3600) |> Integer.to_string() |> String.pad_leading(2, "0")

        minutes =
          div(rem(offset_seconds, 3600), 60) |> Integer.to_string() |> String.pad_leading(2, "0")

        {:ok,
         "D:#{calendar_date(date_time)}#{calendar_time(date_time)}#{sign}#{hours}'#{minutes}'"}

      %NaiveDateTime{} = date_time ->
        {:ok, "D:#{calendar_date(date_time)}#{calendar_time(date_time)}"}

      %Date{} = date ->
        {:ok, "D:#{calendar_date(date)}"}

      value when is_binary(value) ->
        parsed_iso_date(value)

      _ ->
        :error
    end
  end

  defp parsed_iso_date(value) do
    case DateTime.from_iso8601(value) do
      {:ok, date_time, _offset} ->
        pdf_date(date_time)

      {:error, _reason} ->
        case NaiveDateTime.from_iso8601(value) do
          {:ok, date_time} ->
            pdf_date(date_time)

          {:error, _reason} ->
            case Date.from_iso8601(value) do
              {:ok, date} -> pdf_date(date)
              {:error, _reason} -> :error
            end
        end
    end
  end

  defp calendar_date(value) do
    (value.year |> Integer.to_string() |> String.pad_leading(4, "0")) <>
      two_digits(value.month) <> two_digits(value.day)
  end

  defp calendar_time(value) do
    two_digits(value.hour) <> two_digits(value.minute) <> two_digits(value.second)
  end

  defp two_digits(value) do
    value |> Integer.to_string() |> String.pad_leading(2, "0")
  end
end
