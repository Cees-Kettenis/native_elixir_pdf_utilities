defmodule NativeElixirPdfUtilities.Pdf.OutlineDetector do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Text

  @maximum_title_graphemes 160

  @doc false
  @spec detect(binary()) :: {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def detect(pdf) do
    with {:ok, document} <- Text.extract_spans(pdf, order: :visual),
         {:ok, headings} <- visual_headings(document.pages) do
      {:ok, nest(headings)}
    end
  end

  @doc false
  @spec from_paginated_headings([map()]) :: [map()]
  def from_paginated_headings(pages) do
    headings =
      pages
      |> Enum.with_index(1)
      |> Enum.flat_map(fn {page, page_number} ->
        Enum.flat_map(page.boxes, fn box ->
          case Map.get(box, :outline_anchor) do
            %{title: title, level: level} ->
              page_height = elem(page.size, 1)
              top = min(max(outline_top(box), 0.0), page_height * 1.0)

              [
                %{
                  level: level,
                  item: %{
                    title: title,
                    page: page_number,
                    view: {:fit_h, top},
                    open: true,
                    children: []
                  }
                }
              ]

            _ ->
              []
          end
        end)
      end)

    nest(headings)
  end

  defp visual_headings(pages) do
    lines = Enum.flat_map(pages, &page_lines/1)

    case body_font_size(lines) do
      nil ->
        no_source_error()

      body_size ->
        candidates =
          lines
          |> Enum.filter(fn line -> heading_line?(line, body_size) end)
          |> reject_repeated_lines()

        case candidates do
          [] ->
            no_source_error()

          candidates ->
            sizes =
              candidates
              |> Enum.map(&rounded_font_size(&1.font_size))
              |> Enum.uniq()
              |> Enum.sort(:desc)

            headings =
              Enum.map(candidates, fn line ->
                level = Enum.find_index(sizes, &(&1 == rounded_font_size(line.font_size))) + 1

                %{
                  level: min(level, 6),
                  item: %{
                    title: line.text,
                    page: line.page,
                    view: :fit,
                    open: true,
                    children: []
                  }
                }
              end)

            {:ok, headings}
        end
    end
  end

  defp page_lines(page) do
    page.spans
    |> Enum.filter(fn span -> span.paints_text? and String.trim(span.text) != "" end)
    |> Enum.reduce([], fn span, lines ->
      case lines do
        [%{y: y, font_size: font_size, spans: spans} = line | rest]
        when abs(y - span.y) <= 2.0 and abs(font_size - span.font_size) <= 0.5 ->
          [%{line | spans: [span | spans], font_size: max(font_size, span.font_size)} | rest]

        _ ->
          [%{page: page.number, y: span.y, font_size: span.font_size, spans: [span]} | lines]
      end
    end)
    |> Enum.reverse()
    |> Enum.map(fn line -> Map.put(line, :text, line_text(Enum.reverse(line.spans))) end)
  end

  defp line_text(spans) do
    spans
    |> Enum.reduce({[], nil}, fn span, {parts, previous} ->
      separator =
        case previous do
          nil ->
            ""

          previous ->
            if span.x - previous.end_x > max(span.font_size * 0.15, 1.0), do: " ", else: ""
        end

      {[span.text, separator | parts], span}
    end)
    |> elem(0)
    |> Enum.reverse()
    |> IO.iodata_to_binary()
    |> String.replace(~r/\s+/u, " ")
    |> String.trim()
  end

  defp body_font_size(lines) do
    frequencies =
      Enum.reduce(lines, %{}, fn line, frequencies ->
        size = rounded_font_size(line.font_size)
        weight = max(String.length(line.text), 1)
        Map.update(frequencies, size, weight, &(&1 + weight))
      end)

    case Enum.max_by(frequencies, fn {_size, weight} -> weight end, fn -> nil end) do
      nil -> nil
      {size, _weight} -> size
    end
  end

  defp heading_line?(line, body_size) do
    grapheme_count = String.length(line.text)

    line.font_size >= max(body_size * 1.2, body_size + 1.0) and
      grapheme_count >= 2 and grapheme_count <= @maximum_title_graphemes and
      Regex.match?(~r/[\p{L}\p{N}]/u, line.text)
  end

  defp reject_repeated_lines(lines) do
    page_counts =
      lines
      |> Enum.group_by(&String.downcase(&1.text))
      |> Map.new(fn {title, matches} ->
        {title, matches |> Enum.map(& &1.page) |> Enum.uniq() |> length()}
      end)

    Enum.reject(lines, fn line -> Map.fetch!(page_counts, String.downcase(line.text)) > 1 end)
  end

  defp rounded_font_size(size) do
    round(size * 2.0) / 2.0
  end

  defp nest(headings) do
    {stack, roots} =
      Enum.reduce(headings, {[], []}, fn heading, {stack, roots} ->
        {stack, roots} = close_through_level(stack, roots, heading.level)
        {[%{level: heading.level, item: heading.item, children: []} | stack], roots}
      end)

    {_stack, roots} = close_all(stack, roots)
    Enum.reverse(roots)
  end

  defp close_through_level(stack, roots, level) do
    case stack do
      [%{level: current_level} | _rest] when current_level >= level ->
        {stack, roots} = close_one(stack, roots)
        close_through_level(stack, roots, level)

      _ ->
        {stack, roots}
    end
  end

  defp close_all(stack, roots) do
    case stack do
      [] ->
        {[], roots}

      _ ->
        {stack, roots} = close_one(stack, roots)
        close_all(stack, roots)
    end
  end

  defp close_one(stack, roots) do
    case stack do
      [current | remaining] ->
        item = %{current.item | children: Enum.reverse(current.children)}

        case remaining do
          [parent | ancestors] ->
            {[%{parent | children: [item | parent.children]} | ancestors], roots}

          [] ->
            {[], [item | roots]}
        end
    end
  end

  defp outline_top(box) do
    case box do
      %{y: y, height: height} when is_number(y) and is_number(height) ->
        y + height

      %{y: y, line_height: line_height} when is_number(y) and is_number(line_height) ->
        y + line_height

      %{y: y} when is_number(y) ->
        y

      _ ->
        0.0
    end
  end

  defp no_source_error do
    Diagnostics.error(
      :outline_detection,
      :no_outline_source,
      "PDF has no existing outline and no text that can be identified as headings",
      operation: :detect_outlines,
      module: __MODULE__
    )
  end
end
