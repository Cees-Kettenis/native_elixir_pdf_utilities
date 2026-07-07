defmodule NativeElixirPdfUtilities.HtmlToPdf.Style do
  @moduledoc """
  Style computation for the native HTML-to-PDF renderer.

  This module applies defaults and inheritance for the milestone 5 text, box,
  list, and link styling subset.
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
          line_height: 14.4
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
        {:ok, %{type: :text, text: text, style: text_style(inherited_style)}}

      %{type: :element, tag: tag, attributes: attributes, children: children}
      when is_binary(tag) and is_map(attributes) and is_list(children) ->
        with {:ok, element_style} <- element_style(tag, attributes, inherited_style),
             {:ok, styled_children} <- style_children(children, text_style(element_style)) do
          {:ok, %{type: :element, tag: tag, style: element_style, children: styled_children}}
        end

      _ ->
        {:error, :invalid_document}
    end
  end

  defp element_style(tag, attributes, inherited_style) do
    defaults =
      case tag do
        "p" -> block_defaults(12.0, 400, 12.0)
        "h1" -> block_defaults(24.0, 700, 16.0)
        "h2" -> block_defaults(20.0, 700, 14.0)
        "h3" -> block_defaults(16.0, 700, 12.0)
        "h4" -> block_defaults(14.0, 700, 10.0)
        "h5" -> block_defaults(12.0, 700, 8.0)
        "h6" -> block_defaults(10.0, 700, 8.0)
        "ul" -> list_defaults(:disc)
        "ol" -> list_defaults(:decimal)
        "li" -> list_item_defaults()
        "strong" -> %{display: :inline, font_weight: 700}
        "b" -> %{display: :inline, font_weight: 700}
        "em" -> %{display: :inline, font_style: :italic}
        "i" -> %{display: :inline, font_style: :italic}
        "span" -> %{display: :inline}
        "a" -> link_defaults(attributes)
        _ -> :invalid
      end

    case defaults do
      {:ok, defaults} ->
        style =
          inherited_style
          |> text_style()
          |> Map.merge(defaults)
          |> put_line_height()

        apply_inline_style(style, Map.get(attributes, "style", ""))

      :invalid ->
        {:error, :invalid_document}

      defaults ->
        style =
          inherited_style
          |> text_style()
          |> Map.merge(defaults)
          |> put_line_height()

        apply_inline_style(style, Map.get(attributes, "style", ""))
    end
  end

  defp put_line_height(style) do
    Map.put(style, :line_height, Map.fetch!(style, :font_size) * 1.2)
  end

  defp text_style(style) do
    Map.take(style, [
      :color,
      :font_family,
      :font_size,
      :font_style,
      :font_weight,
      :line_height,
      :link_url
    ])
  end

  defp block_defaults(font_size, font_weight, margin_bottom) do
    %{
      background_color: nil,
      border_color: {0, 0, 0},
      border_radius: 0.0,
      border_widths: edges(0.0),
      display: :block,
      font_size: font_size,
      font_weight: font_weight,
      margin: edges(0.0, 0.0, margin_bottom, 0.0),
      margin_after: margin_bottom,
      padding: edges(0.0)
    }
  end

  defp list_defaults(marker_type) do
    block_defaults(12.0, 400, 12.0)
    |> Map.merge(%{
      display: :list,
      list_marker_type: marker_type,
      margin: edges(0.0, 0.0, 12.0, 0.0),
      padding: edges(0.0, 0.0, 0.0, 24.0)
    })
  end

  defp list_item_defaults do
    %{
      display: :list_item,
      font_size: 12.0,
      font_weight: 400,
      margin: edges(0.0, 0.0, 4.0, 0.0),
      margin_after: 4.0
    }
  end

  defp link_defaults(attributes) do
    case Map.get(attributes, "href") do
      nil ->
        {:ok, %{display: :inline, color: {0, 0, 1}}}

      href ->
        case valid_link_url?(href) do
          true -> {:ok, %{display: :inline, color: {0, 0, 1}, link_url: href}}
          false -> :invalid
        end
    end
  end

  defp edges(value) do
    edges(value, value, value, value)
  end

  defp edges(top, right, bottom, left) do
    %{top: top, right: right, bottom: bottom, left: left}
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

            {:ok, property, value} when property == "background-color" ->
              case parse_color(value) do
                {:ok, color} -> {:cont, {:ok, Map.put(acc, :background_color, color)}}
                :error -> {:halt, {:error, :invalid_document}}
              end

            {:ok, property, value} when property in ["margin", "padding", "border-width"] ->
              case parse_box_lengths(value) do
                {:ok, lengths} -> {:cont, {:ok, put_box_lengths(acc, property, lengths)}}
                :error -> {:halt, {:error, :invalid_document}}
              end

            {:ok, property, value}
            when property in [
                   "margin-top",
                   "margin-right",
                   "margin-bottom",
                   "margin-left",
                   "padding-top",
                   "padding-right",
                   "padding-bottom",
                   "padding-left"
                 ] ->
              case parse_length(value) do
                {:ok, length} -> {:cont, {:ok, put_edge_length(acc, property, length)}}
                :error -> {:halt, {:error, :invalid_document}}
              end

            {:ok, property, value} when property == "border-color" ->
              case parse_color(value) do
                {:ok, color} -> {:cont, {:ok, Map.put(acc, :border_color, color)}}
                :error -> {:halt, {:error, :invalid_document}}
              end

            {:ok, property, value} when property == "border-radius" ->
              case parse_length(value) do
                {:ok, length} -> {:cont, {:ok, Map.put(acc, :border_radius, length)}}
                :error -> {:halt, {:error, :invalid_document}}
              end

            {:ok, property, value} when property == "border" ->
              case parse_border(value, Map.fetch!(acc, :color)) do
                {:ok, border_widths, border_color} ->
                  acc =
                    acc
                    |> Map.put(:border_widths, border_widths)
                    |> Map.put(:border_color, border_color)

                  {:cont, {:ok, acc}}

                :error ->
                  {:halt, {:error, :invalid_document}}
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

  defp parse_box_lengths(value) do
    lengths =
      value
      |> String.split(~r/\s+/u, trim: true)
      |> Enum.map(&parse_length/1)

    case lengths do
      [{:ok, all}] ->
        {:ok, edges(all)}

      [{:ok, vertical}, {:ok, horizontal}] ->
        {:ok, edges(vertical, horizontal, vertical, horizontal)}

      [{:ok, top}, {:ok, horizontal}, {:ok, bottom}] ->
        {:ok, edges(top, horizontal, bottom, horizontal)}

      [{:ok, top}, {:ok, right}, {:ok, bottom}, {:ok, left}] ->
        {:ok, edges(top, right, bottom, left)}

      _ ->
        :error
    end
  end

  defp parse_length(value) do
    normalized = String.trim(value)

    case Regex.run(~r/^(?:(0)|(\d+(?:\.\d+)?)(pt|px|mm|cm|in))$/u, normalized) do
      [_, "0"] ->
        {:ok, 0.0}

      [_, "", value, unit] ->
        {number, ""} = Float.parse(value)
        {:ok, number * points_per_unit(unit)}

      _ ->
        :error
    end
  end

  defp put_box_lengths(style, property, lengths) do
    case property do
      "margin" ->
        style
        |> Map.put(:margin, lengths)
        |> Map.put(:margin_after, lengths.bottom)

      "padding" ->
        Map.put(style, :padding, lengths)

      "border-width" ->
        Map.put(style, :border_widths, lengths)
    end
  end

  defp put_edge_length(style, property, length) do
    [box_property, edge] = String.split(property, "-", parts: 2)
    key = String.to_existing_atom(edge)
    map_key = String.to_existing_atom(box_property)
    lengths = style |> Map.fetch!(map_key) |> Map.put(key, length)
    style = Map.put(style, map_key, lengths)

    case property do
      "margin-bottom" -> Map.put(style, :margin_after, length)
      _ -> style
    end
  end

  defp parse_border(value, current_color) do
    tokens = String.split(value, ~r/\s+/u, trim: true)

    case tokens do
      ["none"] ->
        {:ok, edges(0.0), current_color}

      tokens ->
        parsed =
          Enum.reduce_while(tokens, %{width: nil, style: nil, color: nil}, fn token, acc ->
            cond do
              token == "solid" and is_nil(acc.style) ->
                {:cont, %{acc | style: :solid}}

              is_nil(acc.width) and match?({:ok, _}, parse_length(token)) ->
                {:ok, width} = parse_length(token)
                {:cont, %{acc | width: width}}

              is_nil(acc.color) and match?({:ok, _}, parse_color(token)) ->
                {:ok, color} = parse_color(token)
                {:cont, %{acc | color: color}}

              true ->
                {:halt, :error}
            end
          end)

        case parsed do
          %{width: width, style: :solid, color: color} when is_number(width) ->
            {:ok, edges(width), color || current_color}

          _ ->
            :error
        end
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

  defp valid_link_url?(href) do
    case href do
      href when is_binary(href) ->
        Regex.match?(~r/^(https?:\/\/[^\s<>]+|mailto:[^\s<>@]+@[^\s<>@]+)$/iu, href)

      _ ->
        false
    end
  end

  defp hex_to_pdf_color(hex) do
    {integer, ""} = Integer.parse(hex, 16)
    integer / 255
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
