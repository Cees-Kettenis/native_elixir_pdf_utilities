defmodule NativeElixirPdfUtilities.HtmlToPdf.Layout do
  @moduledoc """
  Layout engine for the native HTML-to-PDF renderer.

  Milestone 2 lays out one paragraph on one page using default block text
  metrics. Later milestones add richer block, inline, list, table, flexbox, and
  grid layout behavior behind this module.
  """

  @type box :: %{
          type: :text,
          text: String.t(),
          x: float(),
          y: float(),
          width: float(),
          font: String.t(),
          font_size: float(),
          line_height: float(),
          color: {number(), number(), number()}
        }
  @type layout_tree :: %{
          type: :layout,
          page_size: {float(), float()},
          margin: float(),
          boxes: [box()]
        }
  @type render_option :: NativeElixirPdfUtilities.HtmlToPdf.render_option()

  @doc """
  Converts a styled document tree into a layout tree.
  """
  @spec layout(term(), [render_option()]) ::
          {:ok, layout_tree()} | {:error, :invalid_layout | :invalid_margin | :invalid_page_size}
  def layout(styled_tree, opts \\ []) do
    case {styled_tree, opts} do
      {%{type: :document, children: [%{tag: "p", style: style, children: children}]}, opts}
      when is_map(style) and is_list(children) and is_list(opts) ->
        with {:ok, page_size} <- page_size(Keyword.get(opts, :page_size, :a4)),
             {:ok, margin} <- margin(Keyword.get(opts, :margin, "20mm")),
             {:ok, text} <- paragraph_text(children) do
          {page_width, page_height} = page_size

          {:ok,
           %{
             type: :layout,
             page_size: page_size,
             margin: margin,
             boxes: [
               %{
                 type: :text,
                 text: text,
                 x: margin,
                 y: page_height - margin - Map.fetch!(style, :font_size),
                 width: page_width - margin * 2,
                 font: Map.fetch!(style, :font_family),
                 font_size: Map.fetch!(style, :font_size),
                 line_height: Map.fetch!(style, :line_height),
                 color: Map.fetch!(style, :color)
               }
             ]
           }}
        end

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp paragraph_text(children) do
    text =
      Enum.reduce_while(children, "", fn child, acc ->
        case child do
          %{type: :text, text: text} when is_binary(text) ->
            {:cont, acc <> text}

          _ ->
            {:halt, :invalid_child}
        end
      end)

    case text do
      :invalid_child -> {:error, :invalid_layout}
      text -> {:ok, text}
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
