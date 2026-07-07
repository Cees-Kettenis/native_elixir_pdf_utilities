defmodule NativeElixirPdfUtilities.HtmlToPdf.Layout do
  @moduledoc """
  Layout engine for the native HTML-to-PDF renderer.

  Milestone 3 lays out block text elements and inline text runs on one page.
  Later milestones add richer wrapping, list, table, flexbox, and grid layout
  behavior behind this module.
  """

  @type box :: map()
  @type layout_tree :: %{
          type: :layout,
          page_size: term(),
          margin: term(),
          boxes: [box()]
        }
  @type render_option :: NativeElixirPdfUtilities.HtmlToPdf.render_option()

  @doc """
  Converts a styled document tree into a layout tree.
  """
  @spec layout(term(), term()) ::
          {:ok, layout_tree()} | {:error, :invalid_layout | :invalid_margin | :invalid_page_size}
  def layout(styled_tree, opts \\ []) do
    case {styled_tree, opts} do
      {%{type: :document, children: children}, opts} when is_list(children) and is_list(opts) ->
        with {:ok, page_size} <- page_size(Keyword.get(opts, :page_size, :a4)),
             {:ok, margin} <- margin(Keyword.get(opts, :margin, "20mm")),
             {:ok, boxes} <- layout_blocks(children, page_size, margin) do
          {page_width, page_height} = page_size

          {:ok,
           %{
             type: :layout,
             page_size: page_size,
             margin: margin,
             boxes: boxes,
             content_width: page_width - margin * 2,
             content_height: page_height - margin * 2
           }}
        end

      _ ->
        {:error, :invalid_layout}
    end
  end

  @spec layout_blocks([term()], {number(), number()}, number()) ::
          {:ok, [box()]} | {:error, :invalid_layout}
  defp layout_blocks(children, page_size, margin) do
    {page_width, page_height} = page_size

    result =
      Enum.reduce(children, {:ok, [], page_height - margin}, fn child, acc ->
        case acc do
          {:ok, boxes, y} ->
            case layout_block(child, margin, y, page_width - margin * 2) do
              {:ok, block_boxes, next_y} -> {:ok, boxes ++ block_boxes, next_y}
              {:error, reason} -> {:error, reason}
            end

          {:error, reason} ->
            {:error, reason}
        end
      end)

    case result do
      {:ok, boxes, _y} -> {:ok, boxes}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec layout_block(term(), number(), number(), number()) ::
          {:ok, [box()], number()} | {:error, :invalid_layout}
  defp layout_block(block, x, y, width) do
    case block do
      %{type: :element, style: %{display: :block} = style, children: children}
      when is_list(children) ->
        with {:ok, runs} <- inline_runs(children) do
          baseline_y = y - Map.fetch!(style, :font_size)

          {boxes, _next_x} =
            Enum.reduce(runs, {[], x}, fn run, {acc, current_x} ->
              box = text_box(run, current_x, baseline_y, width)
              {acc ++ [box], current_x + text_width(run.text, run.style)}
            end)

          next_y = y - Map.fetch!(style, :line_height) - Map.fetch!(style, :margin_after)
          {:ok, boxes, next_y}
        end

      _ ->
        {:error, :invalid_layout}
    end
  end

  @spec inline_runs([term()]) ::
          {:ok, [%{text: String.t(), style: map()}]} | {:error, :invalid_layout}
  defp inline_runs(children) do
    Enum.reduce(children, {:ok, []}, fn child, acc ->
      case acc do
        {:ok, runs} ->
          append_inline_run(child, runs)

        {:error, reason} ->
          {:error, reason}
      end
    end)
  end

  defp append_inline_run(child, runs) do
    case child do
      %{type: :text, text: text, style: style} when is_binary(text) and is_map(style) ->
        {:ok, runs ++ [%{text: text, style: style}]}

      %{type: :element, style: %{display: :inline}, children: children}
      when is_list(children) ->
        case inline_runs(children) do
          {:ok, child_runs} -> {:ok, runs ++ child_runs}
          {:error, reason} -> {:error, reason}
        end

      _ ->
        {:error, :invalid_layout}
    end
  end

  @spec text_box(%{text: String.t(), style: map()}, number(), number(), number()) :: box()
  defp text_box(run, x, y, width) do
    style = run.style

    %{
      type: :text,
      text: run.text,
      x: x,
      y: y,
      width: width,
      font:
        pdf_font(
          Map.fetch!(style, :font_family),
          Map.fetch!(style, :font_weight),
          Map.fetch!(style, :font_style)
        ),
      font_size: Map.fetch!(style, :font_size),
      line_height: Map.fetch!(style, :line_height),
      color: Map.fetch!(style, :color)
    }
  end

  @spec text_width(String.t(), map()) :: number()
  defp text_width(text, style) do
    text
    |> String.length()
    |> Kernel.*(Map.fetch!(style, :font_size))
    |> Kernel.*(0.6)
  end

  @spec pdf_font(String.t(), number(), atom()) :: String.t()
  defp pdf_font(font_family, font_weight, font_style) do
    case {font_family, font_weight >= 700, font_style} do
      {"Helvetica", true, :italic} -> "Helvetica-BoldOblique"
      {"Helvetica", true, _} -> "Helvetica-Bold"
      {"Helvetica", false, :italic} -> "Helvetica-Oblique"
      {"Helvetica", false, _} -> "Helvetica"
      {font_family, _, _} -> font_family
    end
  end

  defp page_size(page_size) do
    case page_size do
      :a4 ->
        {:ok, {595.28, 841.89}}

      :letter ->
        {:ok, {612.0, 792.0}}

      {width, height} when is_number(width) and is_number(height) and width > 0 and height > 0 ->
        {:ok, {width * 1.0, height * 1.0}}

      _ ->
        {:error, :invalid_page_size}
    end
  end

  defp margin(margin) do
    case margin do
      margin when is_number(margin) and margin >= 0 ->
        {:ok, margin * 1.0}

      margin when is_binary(margin) ->
        case Regex.run(~r/^\s*(\d+(?:\.\d+)?)(pt|px|mm|cm|in)\s*$/u, margin) do
          [_, value, unit] ->
            {number, ""} = Float.parse(value)
            {:ok, number * points_per_unit(unit)}

          _ ->
            {:error, :invalid_margin}
        end

      _ ->
        {:error, :invalid_margin}
    end
  end

  defp points_per_unit(unit) do
    case unit do
      "pt" -> 1.0
      "px" -> 0.75
      "mm" -> 72.0 / 25.4
      "cm" -> 72.0 / 2.54
      "in" -> 72.0
    end
  end
end
