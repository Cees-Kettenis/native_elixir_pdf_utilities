defmodule NativeElixirPdfUtilities.HtmlToPdf.Style do
  @moduledoc """
  Style computation for the native HTML-to-PDF renderer.

  This module applies defaults and inheritance for the milestone 3 text subset.
  Later milestones add the broader CSS parser and full cascade behavior.
  """

  @type text_node :: %{type: :text, text: String.t(), style: map()}
  @type styled_element :: %{
          type: :element,
          tag: String.t(),
          style: map(),
          children: [text_node() | styled_element()]
        }
  @type styled_tree :: %{type: :document, children: [styled_element()]}
  @type render_option :: NativeElixirPdfUtilities.HtmlToPdf.render_option()

  @doc """
  Computes styles for a parsed HTML document tree.
  """
  @spec compute(term(), [render_option()]) :: {:ok, styled_tree()} | {:error, :invalid_document}
  def compute(dom, opts \\ []) do
    case {dom, opts} do
      {%{type: :document, children: children}, opts} when is_list(opts) and is_list(children) ->
        base_style = %{
          color: {0, 0, 0},
          font_family: Keyword.get(opts, :default_font, "Helvetica"),
          font_size: 12.0,
          font_style: :normal,
          font_weight: 400,
          line_height: 14.4,
          margin_after: 12.0
        }

        with {:ok, styled_children} <- style_children(children, base_style) do
          {:ok, %{type: :document, children: styled_children}}
        end

      _ ->
        {:error, :invalid_document}
    end
  end

  defp style_children(children, inherited_style) do
    Enum.reduce_while(children, {:ok, []}, fn child, {:ok, acc} ->
      case style_node(child, inherited_style) do
        {:ok, styled_child} -> {:cont, {:ok, acc ++ [styled_child]}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp style_node(node, inherited_style) do
    case node do
      %{type: :text, text: text} when is_binary(text) ->
        {:ok, %{type: :text, text: text, style: inherited_style}}

      %{type: :element, tag: tag, attributes: attributes, children: children}
      when is_binary(tag) and is_map(attributes) and is_list(children) ->
        with {:ok, element_style} <- element_style(tag, attributes, inherited_style),
             {:ok, styled_children} <- style_children(children, element_style) do
          {:ok, %{type: :element, tag: tag, style: element_style, children: styled_children}}
        end

      _ ->
        {:error, :invalid_document}
    end
  end

  defp element_style(tag, attributes, inherited_style) do
    defaults =
      case tag do
        "p" -> %{display: :block, font_size: 12.0, font_weight: 400, margin_after: 12.0}
        "h1" -> %{display: :block, font_size: 24.0, font_weight: 700, margin_after: 16.0}
        "h2" -> %{display: :block, font_size: 20.0, font_weight: 700, margin_after: 14.0}
        "h3" -> %{display: :block, font_size: 16.0, font_weight: 700, margin_after: 12.0}
        "h4" -> %{display: :block, font_size: 14.0, font_weight: 700, margin_after: 10.0}
        "h5" -> %{display: :block, font_size: 12.0, font_weight: 700, margin_after: 8.0}
        "h6" -> %{display: :block, font_size: 10.0, font_weight: 700, margin_after: 8.0}
        "strong" -> %{display: :inline, font_weight: 700}
        "b" -> %{display: :inline, font_weight: 700}
        "em" -> %{display: :inline, font_style: :italic}
        "i" -> %{display: :inline, font_style: :italic}
        "span" -> %{display: :inline}
        _ -> :invalid
      end

    case defaults do
      :invalid ->
        {:error, :invalid_document}

      defaults ->
        style =
          inherited_style
          |> Map.merge(defaults)
          |> put_line_height()

        apply_inline_style(style, Map.get(attributes, "style", ""))
    end
  end

  defp put_line_height(style) do
    Map.put(style, :line_height, Map.fetch!(style, :font_size) * 1.2)
  end

  defp apply_inline_style(style, inline_style) do
    case String.trim(inline_style) do
      "" ->
        {:ok, style}

      inline_style ->
        inline_style
        |> String.split(";")
        |> Enum.map(&String.trim/1)
        |> Enum.reject(&(&1 == ""))
        |> Enum.reduce_while({:ok, style}, fn declaration, {:ok, acc} ->
          case parse_declaration(declaration) do
            {:ok, property, value} when property == "color" ->
              case parse_color(value) do
                {:ok, color} -> {:cont, {:ok, Map.put(acc, :color, color)}}
                :error -> {:halt, {:error, :invalid_document}}
              end

            _ ->
              {:halt, {:error, :invalid_document}}
          end
        end)
    end
  end

  defp parse_declaration(declaration) do
    case String.split(declaration, ":", parts: 2) do
      [property, value] -> {:ok, String.downcase(String.trim(property)), String.trim(value)}
      _ -> {:error, :invalid_document}
    end
  end

  defp parse_color(value) do
    normalized = value |> String.trim() |> String.downcase()

    cond do
      Map.has_key?(named_colors(), normalized) ->
        {:ok, Map.fetch!(named_colors(), normalized)}

      Regex.match?(~r/^#[0-9a-f]{6}$/u, normalized) ->
        <<"#", red::binary-size(2), green::binary-size(2), blue::binary-size(2)>> = normalized
        {:ok, {hex_to_pdf_color(red), hex_to_pdf_color(green), hex_to_pdf_color(blue)}}

      Regex.match?(~r/^#[0-9a-f]{3}$/u, normalized) ->
        <<"#", red::binary-size(1), green::binary-size(1), blue::binary-size(1)>> = normalized

        {:ok,
         {hex_to_pdf_color(red <> red), hex_to_pdf_color(green <> green),
          hex_to_pdf_color(blue <> blue)}}

      true ->
        :error
    end
  end

  defp named_colors do
    %{
      "black" => {0, 0, 0},
      "blue" => {0, 0, 1},
      "green" => {0, 0.5019607843, 0},
      "red" => {1, 0, 0},
      "white" => {1, 1, 1}
    }
  end

  defp hex_to_pdf_color(hex) do
    {integer, ""} = Integer.parse(hex, 16)
    integer / 255
  end
end
