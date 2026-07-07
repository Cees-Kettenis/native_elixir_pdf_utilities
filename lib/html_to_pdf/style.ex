defmodule NativeElixirPdfUtilities.HtmlToPdf.Style do
  @moduledoc """
  Style computation for the native HTML-to-PDF renderer.

  This module applies defaults, inheritance, and the CSS cascade for the text,
  box, list, link, table, page-break, flexbox, and milestone 10 grid styling subset.
  Unsupported properties or invalid values fail the render instead of being
  ignored.
  """

  alias NativeElixirPdfUtilities.HtmlToPdf.CssParser

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

        with {:ok, rules} <- stylesheet_rules(children, opts),
             {:ok, styled_children} <- style_children(children, base_style, rules, []) do
          {:ok, %{type: :document, children: styled_children}}
        end

      _ ->
        {:error, :invalid_document}
    end
  end

  defp style_children(children, inherited_style, rules, ancestors) do
    Enum.reduce_while(children, {:ok, []}, fn child, {:ok, acc} ->
      case style_node(child, inherited_style, rules, ancestors) do
        {:ok, styled_children} -> {:cont, {:ok, acc ++ styled_children}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp style_node(node, inherited_style, rules, ancestors) do
    case node do
      %{type: :text, text: text} when is_binary(text) ->
        {:ok, [%{type: :text, text: text, style: text_style(inherited_style)}]}

      %{type: :element, tag: tag, attributes: attributes, children: children}
      when is_binary(tag) and is_map(attributes) and is_list(children) ->
        case tag do
          "style" ->
            {:ok, []}

          "head" ->
            {:ok, []}

          tag when tag in ["html", "body"] ->
            with {:ok, element_style} <- element_style(node, inherited_style, rules, ancestors),
                 {:ok, styled_children} <-
                   style_children(children, text_style(element_style), rules, [node | ancestors]) do
              {:ok, styled_children}
            end

          _ ->
            with {:ok, element_style} <- element_style(node, inherited_style, rules, ancestors),
                 {:ok, styled_children} <-
                   style_children(children, text_style(element_style), rules, [node | ancestors]) do
              {:ok,
               [%{type: :element, tag: tag, style: element_style, children: styled_children}]}
            end
        end

      _ ->
        {:error, :invalid_document}
    end
  end

  defp element_style(
         %{tag: tag, attributes: attributes} = node,
         inherited_style,
         rules,
         ancestors
       ) do
    defaults =
      case tag do
        "html" -> %{display: :block}
        "body" -> block_defaults(12.0, 400, 0.0)
        "div" -> block_defaults(12.0, 400, 0.0)
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
        "table" -> table_defaults()
        "caption" -> table_caption_defaults()
        "thead" -> table_row_group_defaults(:head)
        "tbody" -> table_row_group_defaults(:body)
        "tfoot" -> table_row_group_defaults(:foot)
        "tr" -> table_row_defaults()
        "th" -> table_cell_defaults(:header)
        "td" -> table_cell_defaults(:data)
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

        apply_author_styles(style, node, ancestors, rules)

      :invalid ->
        {:error, :invalid_document}

      defaults ->
        style =
          inherited_style
          |> text_style()
          |> Map.merge(defaults)

        apply_author_styles(style, node, ancestors, rules)
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

  defp table_defaults do
    block_defaults(12.0, 400, 12.0)
    |> Map.merge(%{
      display: :table,
      margin: edges(0.0, 0.0, 12.0, 0.0),
      padding: edges(0.0)
    })
  end

  defp table_caption_defaults do
    block_defaults(12.0, 700, 4.0)
    |> Map.merge(%{
      display: :table_caption,
      text_align: :center
    })
  end

  defp table_row_group_defaults(section) do
    %{
      background_color: nil,
      border_color: {0, 0, 0},
      border_radius: 0.0,
      border_widths: edges(0.0),
      display: :table_row_group,
      padding: edges(0.0),
      table_section: section
    }
  end

  defp table_row_defaults do
    %{
      background_color: nil,
      border_color: {0, 0, 0},
      border_radius: 0.0,
      border_widths: edges(0.0),
      display: :table_row,
      padding: edges(0.0)
    }
  end

  defp table_cell_defaults(kind) do
    base = %{
      background_color: nil,
      border_color: {0, 0, 0},
      border_radius: 0.0,
      border_widths: edges(1.0),
      display: :table_cell,
      font_size: 12.0,
      font_weight: 400,
      margin: edges(0.0),
      padding: edges(4.0),
      text_align: :left
    }

    case kind do
      :header ->
        Map.merge(base, %{
          background_color: {0.9333333333, 0.9333333333, 0.9333333333},
          font_weight: 700,
          text_align: :center
        })

      :data ->
        base
    end
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

  defp stylesheet_rules(children, opts) do
    case Keyword.get(opts, :stylesheets, []) do
      stylesheets when is_list(stylesheets) ->
        with {:ok, configured_rules} <- parse_stylesheets(stylesheets, 0),
             {:ok, embedded_rules} <-
               children
               |> style_sources()
               |> parse_stylesheets(length(configured_rules)) do
          {:ok, configured_rules ++ embedded_rules}
        end

      _ ->
        {:error, :invalid_document}
    end
  end

  defp parse_stylesheets(stylesheets, initial_order) do
    parsed =
      Enum.reduce_while(stylesheets, {:ok, [], initial_order}, fn stylesheet,
                                                                  {:ok, acc, next_order} ->
        with {:ok, stylesheet} <- stylesheet_source(stylesheet),
             {:ok, rules} <- CssParser.parse(stylesheet) do
          ordered_rules =
            rules
            |> Enum.with_index(next_order)
            |> Enum.map(fn {rule, order} -> %{rule | order: order} end)

          {:cont, {:ok, acc ++ ordered_rules, next_order + length(rules)}}
        else
          _ ->
            {:halt, {:error, :invalid_document}}
        end
      end)

    case parsed do
      {:ok, rules, _next_order} -> {:ok, rules}
      {:error, reason} -> {:error, reason}
    end
  end

  defp stylesheet_source(stylesheet) do
    case stylesheet do
      stylesheet when is_binary(stylesheet) ->
        case String.contains?(stylesheet, "{") do
          true ->
            {:ok, stylesheet}

          false ->
            case File.read(stylesheet) do
              {:ok, css} -> {:ok, css}
              {:error, _reason} -> {:error, :invalid_document}
            end
        end

      _ ->
        {:error, :invalid_document}
    end
  end

  defp style_sources(children) do
    Enum.flat_map(children, fn child ->
      case child do
        %{type: :element, tag: "style", children: style_children} ->
          [text_content(style_children)]

        %{type: :element, children: nested_children} ->
          style_sources(nested_children)

        _ ->
          []
      end
    end)
  end

  defp text_content(children) do
    Enum.map_join(children, "", fn child ->
      case child do
        %{type: :text, text: text} -> text
        _ -> ""
      end
    end)
  end

  defp apply_author_styles(style, node, ancestors, rules) do
    with {:ok, style} <- apply_stylesheet_rules(style, node, ancestors, rules),
         {:ok, style} <- apply_inline_style(style, Map.get(node.attributes, "style", "")) do
      {:ok, put_line_height(style)}
    end
  end

  defp apply_stylesheet_rules(style, node, ancestors, rules) do
    rules
    |> matching_declarations(node, ancestors)
    |> Enum.map(& &1.declaration)
    |> then(&apply_declarations(style, &1))
  end

  defp matching_declarations(rules, node, ancestors) do
    rules
    |> Enum.flat_map(fn rule ->
      case matching_specificity(rule.selectors, node, ancestors) do
        nil ->
          []

        specificity ->
          rule.declarations
          |> Enum.with_index()
          |> Enum.map(fn {declaration, declaration_index} ->
            %{
              declaration: declaration,
              specificity: specificity,
              order: rule.order,
              declaration_index: declaration_index
            }
          end)
      end
    end)
    |> Enum.sort_by(fn matched ->
      {matched.specificity, matched.order, matched.declaration_index}
    end)
  end

  defp matching_specificity(selectors, node, ancestors) do
    selectors
    |> Enum.filter(&matches_selector?(&1, node, ancestors))
    |> Enum.map(& &1.specificity)
    |> Enum.max(fn -> nil end)
  end

  defp matches_selector?(selector, node, ancestors) do
    selector.parts
    |> Enum.reverse()
    |> match_selector_parts(node, ancestors)
  end

  defp match_selector_parts(parts, node, ancestors) do
    case parts do
      [part] ->
        matches_simple_selector?(part, node)

      [part, left_part | remaining] ->
        case matches_simple_selector?(part, node) do
          true -> matches_ancestor_selector?(part.combinator, left_part, remaining, ancestors)
          false -> false
        end

      _ ->
        false
    end
  end

  defp matches_ancestor_selector?(combinator, left_part, remaining, ancestors) do
    case {combinator, ancestors} do
      {:child, [parent | rest]} ->
        match_selector_parts([left_part | remaining], parent, rest)

      {:descendant, ancestors} ->
        ancestors
        |> Enum.with_index()
        |> Enum.any?(fn {ancestor, index} ->
          rest = Enum.drop(ancestors, index + 1)
          match_selector_parts([left_part | remaining], ancestor, rest)
        end)

      _ ->
        false
    end
  end

  defp matches_simple_selector?(part, node) do
    attributes = Map.get(node, :attributes, %{})
    classes = attributes |> Map.get("class", "") |> String.split(~r/\s+/u, trim: true)

    tag_matches? = is_nil(part.tag) or Map.get(node, :tag) == part.tag
    id_matches? = is_nil(part.id) or Map.get(attributes, "id") == part.id
    classes_match? = Enum.all?(part.classes, &(&1 in classes))

    tag_matches? and id_matches? and classes_match?
  end

  defp apply_inline_style(style, inline_style) do
    case CssParser.parse_declarations(inline_style) do
      {:ok, declarations} -> apply_declarations(style, declarations)
      {:error, _reason} -> {:error, :invalid_document}
    end
  end

  defp apply_declarations(style, declarations) do
    Enum.reduce_while(declarations, {:ok, style}, fn declaration, {:ok, acc} ->
      case apply_declaration(acc, declaration) do
        {:ok, style} -> {:cont, {:ok, style}}
        {:error, reason} -> {:halt, {:error, reason}}
        _ -> {:halt, {:error, :invalid_document}}
      end
    end)
  end

  defp apply_declaration(style, {property, value}) do
    case property do
      "color" ->
        with {:ok, color} <- parse_color(value), do: {:ok, Map.put(style, :color, color)}

      "background-color" ->
        with {:ok, color} <- parse_color(value),
             do: {:ok, Map.put(style, :background_color, color)}

      property when property in ["margin", "padding", "border-width"] ->
        with {:ok, lengths} <- parse_box_lengths(value),
             do: {:ok, put_box_lengths(style, property, lengths)}

      property
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
        with {:ok, length} <- parse_length(value),
             do: {:ok, put_edge_length(style, property, length)}

      "border-color" ->
        with {:ok, color} <- parse_color(value),
             do: {:ok, Map.put(style, :border_color, color)}

      "border-radius" ->
        with {:ok, length} <- parse_length(value),
             do: {:ok, Map.put(style, :border_radius, length)}

      "text-align" ->
        put_text_align(style, value)

      property when property in ["break-before", "page-break-before"] ->
        with {:ok, break_value} <- page_break_value(value),
             do: {:ok, Map.put(style, :break_before, break_value)}

      property when property in ["break-after", "page-break-after"] ->
        with {:ok, break_value} <- page_break_value(value),
             do: {:ok, Map.put(style, :break_after, break_value)}

      "border" ->
        with {:ok, border_widths, border_color} <- parse_border(value, Map.fetch!(style, :color)) do
          style =
            style
            |> Map.put(:border_widths, border_widths)
            |> Map.put(:border_color, border_color)

          {:ok, style}
        end

      "font-size" ->
        with {:ok, length} <- parse_length(value), do: {:ok, Map.put(style, :font_size, length)}

      "font-weight" ->
        put_font_weight(style, value)

      "font-style" ->
        put_font_style(style, value)

      "font-family" ->
        put_font_family(style, value)

      "display" ->
        put_display(style, value)

      property when property in ["width", "height"] ->
        with {:ok, length} <- parse_length(value),
             do: {:ok, Map.put(style, length_property(property), length)}

      "flex-direction" ->
        put_flex_direction(style, value)

      "flex-wrap" ->
        put_flex_wrap(style, value)

      "gap" ->
        with {:ok, row_gap, column_gap} <- parse_gap(value) do
          {:ok, style |> Map.put(:row_gap, row_gap) |> Map.put(:column_gap, column_gap)}
        end

      property when property in ["row-gap", "column-gap"] ->
        with {:ok, length} <- parse_length(value),
             do: {:ok, Map.put(style, gap_property(property), length)}

      "justify-content" ->
        put_justify_content(style, value)

      "align-items" ->
        put_align_items(style, value)

      "align-self" ->
        put_align_self(style, value)

      "order" ->
        put_order(style, value)

      "flex-grow" ->
        put_nonnegative_number(style, :flex_grow, value)

      "flex-shrink" ->
        put_nonnegative_number(style, :flex_shrink, value)

      "flex-basis" ->
        put_flex_basis(style, value)

      "flex" ->
        put_flex(style, value)

      property when property in ["grid-template-columns", "grid-template-rows"] ->
        with {:ok, tracks} <- parse_grid_tracks(value) do
          {:ok, Map.put(style, grid_template_property(property), tracks)}
        end

      property when property in ["grid-auto-columns", "grid-auto-rows"] ->
        with {:ok, track} <- parse_grid_track(value) do
          {:ok, Map.put(style, grid_auto_property(property), track)}
        end

      property
      when property in [
             "grid-column",
             "grid-row",
             "grid-column-start",
             "grid-column-end",
             "grid-row-start",
             "grid-row-end",
             "grid-area"
           ] ->
        put_grid_placement(style, property, value)

      "justify-items" ->
        put_grid_alignment(style, :justify_items, value)

      "align-content" ->
        put_justify_content(style, value, :align_content)

      _ ->
        {:error, :invalid_document}
    end
  end

  defp put_display(style, value) do
    case value |> String.trim() |> String.downcase() do
      "block" ->
        {:ok, Map.put(style, :display, :block)}

      "inline" ->
        {:ok, Map.put(style, :display, :inline)}

      "none" ->
        {:ok, Map.put(style, :display, :none)}

      "flex" ->
        {:ok, style |> flex_container_defaults() |> Map.put(:display, :flex)}

      "inline-flex" ->
        {:ok, style |> flex_container_defaults() |> Map.put(:display, :inline_flex)}

      "grid" ->
        {:ok, style |> grid_container_defaults() |> Map.put(:display, :grid)}

      "inline-grid" ->
        {:ok, style |> grid_container_defaults() |> Map.put(:display, :inline_grid)}

      _ ->
        {:error, :invalid_document}
    end
  end

  defp flex_container_defaults(style) do
    style
    |> Map.put_new(:flex_direction, :row)
    |> Map.put_new(:flex_wrap, :nowrap)
    |> Map.put_new(:row_gap, 0.0)
    |> Map.put_new(:column_gap, 0.0)
    |> Map.put_new(:justify_content, :flex_start)
    |> Map.put_new(:align_items, :stretch)
  end

  defp grid_container_defaults(style) do
    style
    |> Map.put_new(:grid_template_columns, [])
    |> Map.put_new(:grid_template_rows, [])
    |> Map.put_new(:grid_auto_columns, :auto)
    |> Map.put_new(:grid_auto_rows, :auto)
    |> Map.put_new(:row_gap, 0.0)
    |> Map.put_new(:column_gap, 0.0)
    |> Map.put_new(:justify_items, :stretch)
    |> Map.put_new(:align_items, :stretch)
    |> Map.put_new(:justify_content, :flex_start)
    |> Map.put_new(:align_content, :flex_start)
  end

  defp put_flex_direction(style, value) do
    case value |> String.trim() |> String.downcase() do
      "row" -> {:ok, Map.put(style, :flex_direction, :row)}
      "row-reverse" -> {:ok, Map.put(style, :flex_direction, :row_reverse)}
      "column" -> {:ok, Map.put(style, :flex_direction, :column)}
      "column-reverse" -> {:ok, Map.put(style, :flex_direction, :column_reverse)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_flex_wrap(style, value) do
    case value |> String.trim() |> String.downcase() do
      "nowrap" -> {:ok, Map.put(style, :flex_wrap, :nowrap)}
      "wrap" -> {:ok, Map.put(style, :flex_wrap, :wrap)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_justify_content(style, value) do
    put_justify_content(style, value, :justify_content)
  end

  defp put_justify_content(style, value, property) do
    case value |> String.trim() |> String.downcase() do
      value when value in ["start", "flex-start"] -> {:ok, Map.put(style, property, :flex_start)}
      value when value in ["end", "flex-end"] -> {:ok, Map.put(style, property, :flex_end)}
      "center" -> {:ok, Map.put(style, property, :center)}
      "stretch" -> {:ok, Map.put(style, property, :stretch)}
      "space-between" -> {:ok, Map.put(style, property, :space_between)}
      "space-around" -> {:ok, Map.put(style, property, :space_around)}
      "space-evenly" -> {:ok, Map.put(style, property, :space_evenly)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_align_items(style, value) do
    case value |> String.trim() |> String.downcase() do
      "stretch" ->
        {:ok, Map.put(style, :align_items, :stretch)}

      value when value in ["start", "flex-start"] ->
        {:ok, Map.put(style, :align_items, :flex_start)}

      value when value in ["end", "flex-end"] ->
        {:ok, Map.put(style, :align_items, :flex_end)}

      "center" ->
        {:ok, Map.put(style, :align_items, :center)}

      _ ->
        {:error, :invalid_document}
    end
  end

  defp put_align_self(style, value) do
    case value |> String.trim() |> String.downcase() do
      "auto" ->
        {:ok, Map.put(style, :align_self, :auto)}

      "stretch" ->
        {:ok, Map.put(style, :align_self, :stretch)}

      value when value in ["start", "flex-start"] ->
        {:ok, Map.put(style, :align_self, :flex_start)}

      value when value in ["end", "flex-end"] ->
        {:ok, Map.put(style, :align_self, :flex_end)}

      "center" ->
        {:ok, Map.put(style, :align_self, :center)}

      _ ->
        {:error, :invalid_document}
    end
  end

  defp put_grid_alignment(style, property, value) do
    case value |> String.trim() |> String.downcase() do
      "stretch" -> {:ok, Map.put(style, property, :stretch)}
      value when value in ["start", "flex-start"] -> {:ok, Map.put(style, property, :flex_start)}
      value when value in ["end", "flex-end"] -> {:ok, Map.put(style, property, :flex_end)}
      "center" -> {:ok, Map.put(style, property, :center)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_order(style, value) do
    case Integer.parse(String.trim(value)) do
      {order, ""} -> {:ok, Map.put(style, :order, order)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_nonnegative_number(style, key, value) do
    case parse_nonnegative_number(value) do
      {:ok, number} -> {:ok, Map.put(style, key, number)}
      :error -> {:error, :invalid_document}
    end
  end

  defp put_flex_basis(style, value) do
    case value |> String.trim() |> String.downcase() do
      "auto" ->
        {:ok, Map.put(style, :flex_basis, :auto)}

      _ ->
        with {:ok, length} <- parse_length(value), do: {:ok, Map.put(style, :flex_basis, length)}
    end
  end

  defp put_flex(style, value) do
    tokens = value |> String.trim() |> String.downcase() |> String.split(~r/\s+/u, trim: true)

    case tokens do
      ["none"] ->
        {:ok,
         style
         |> Map.put(:flex_grow, 0.0)
         |> Map.put(:flex_shrink, 0.0)
         |> Map.put(:flex_basis, :auto)}

      ["auto"] ->
        {:ok,
         style
         |> Map.put(:flex_grow, 1.0)
         |> Map.put(:flex_shrink, 1.0)
         |> Map.put(:flex_basis, :auto)}

      ["initial"] ->
        {:ok,
         style
         |> Map.put(:flex_grow, 0.0)
         |> Map.put(:flex_shrink, 1.0)
         |> Map.put(:flex_basis, :auto)}

      [grow] ->
        with {:ok, grow} <- parse_nonnegative_number(grow) do
          {:ok,
           style
           |> Map.put(:flex_grow, grow)
           |> Map.put_new(:flex_shrink, 1.0)
           |> Map.put_new(:flex_basis, 0.0)}
        end

      [grow, shrink] ->
        with {:ok, grow} <- parse_nonnegative_number(grow),
             {:ok, shrink} <- parse_nonnegative_number(shrink) do
          {:ok,
           style
           |> Map.put(:flex_grow, grow)
           |> Map.put(:flex_shrink, shrink)
           |> Map.put_new(:flex_basis, 0.0)}
        end

      [grow, shrink, basis] ->
        with {:ok, grow} <- parse_nonnegative_number(grow),
             {:ok, shrink} <- parse_nonnegative_number(shrink),
             {:ok, basis} <- flex_basis_value(basis) do
          {:ok,
           style
           |> Map.put(:flex_grow, grow)
           |> Map.put(:flex_shrink, shrink)
           |> Map.put(:flex_basis, basis)}
        end

      _ ->
        {:error, :invalid_document}
    end
  end

  defp length_property(property) do
    case property do
      "width" -> :width
      "height" -> :height
    end
  end

  defp gap_property(property) do
    case property do
      "row-gap" -> :row_gap
      "column-gap" -> :column_gap
    end
  end

  defp flex_basis_value(value) do
    case value do
      "auto" -> {:ok, :auto}
      value -> parse_length(value)
    end
  end

  defp parse_grid_tracks(value) do
    tracks =
      value
      |> String.trim()
      |> String.downcase()
      |> String.split(~r/\s+/u, trim: true)
      |> Enum.map(&parse_grid_track/1)

    case tracks do
      [] ->
        :error

      tracks ->
        case Enum.all?(tracks, &match?({:ok, _track}, &1)) do
          true -> {:ok, Enum.map(tracks, fn {:ok, track} -> track end)}
          false -> :error
        end
    end
  end

  defp parse_grid_track(value) do
    normalized = value |> String.trim() |> String.downcase()

    cond do
      normalized == "auto" ->
        {:ok, :auto}

      Regex.match?(~r/^\d+(?:\.\d+)?fr$/u, normalized) ->
        {number, "fr"} = Float.parse(normalized)
        {:ok, {:fr, number}}

      true ->
        with {:ok, length} <- parse_length(normalized), do: {:ok, {:length, length}}
    end
  end

  defp put_grid_placement(style, property, value) do
    case property do
      property when property in ["grid-column-start", "grid-column-end"] ->
        with {:ok, placement} <- parse_grid_line(value) do
          {:ok, Map.put(style, grid_line_property(property), placement)}
        end

      property when property in ["grid-row-start", "grid-row-end"] ->
        with {:ok, placement} <- parse_grid_line(value) do
          {:ok, Map.put(style, grid_line_property(property), placement)}
        end

      "grid-column" ->
        with {:ok, start_line, end_line} <- parse_grid_axis(value) do
          {:ok,
           style
           |> Map.put(:grid_column_start, start_line)
           |> Map.put(:grid_column_end, end_line)}
        end

      "grid-row" ->
        with {:ok, start_line, end_line} <- parse_grid_axis(value) do
          {:ok,
           style
           |> Map.put(:grid_row_start, start_line)
           |> Map.put(:grid_row_end, end_line)}
        end

      "grid-area" ->
        with {:ok, row_start, column_start, row_end, column_end} <- parse_grid_area(value) do
          {:ok,
           style
           |> Map.put(:grid_row_start, row_start)
           |> Map.put(:grid_column_start, column_start)
           |> Map.put(:grid_row_end, row_end)
           |> Map.put(:grid_column_end, column_end)}
        end
    end
  end

  defp parse_grid_axis(value) do
    case value |> String.trim() |> String.downcase() |> String.split("/", trim: true) do
      [start_line] ->
        with {:ok, start_line} <- parse_grid_line(start_line), do: {:ok, start_line, :auto}

      [start_line, end_line] ->
        with {:ok, start_line} <- parse_grid_line(start_line),
             {:ok, end_line} <- parse_grid_line(end_line) do
          {:ok, start_line, end_line}
        end

      _ ->
        :error
    end
  end

  defp parse_grid_area(value) do
    case value |> String.trim() |> String.downcase() |> String.split("/", trim: true) do
      [row_start, column_start, row_end, column_end] ->
        with {:ok, row_start} <- parse_grid_line(row_start),
             {:ok, column_start} <- parse_grid_line(column_start),
             {:ok, row_end} <- parse_grid_line(row_end),
             {:ok, column_end} <- parse_grid_line(column_end) do
          {:ok, row_start, column_start, row_end, column_end}
        end

      _ ->
        :error
    end
  end

  defp parse_grid_line(value) do
    normalized = value |> String.trim() |> String.downcase()

    case normalized do
      "auto" ->
        {:ok, :auto}

      _ ->
        parse_grid_number_or_span(normalized)
    end
  end

  defp parse_grid_number_or_span(value) do
    cond do
      Regex.match?(~r/^span\s+[1-9]\d*$/u, value) ->
        ["span", count] = String.split(value, ~r/\s+/u, trim: true)
        {count, ""} = Integer.parse(count)
        {:ok, {:span, count}}

      Regex.match?(~r/^[1-9]\d*$/u, value) ->
        {line, ""} = Integer.parse(value)
        {:ok, line}

      true ->
        :error
    end
  end

  defp grid_template_property(property) do
    case property do
      "grid-template-columns" -> :grid_template_columns
      "grid-template-rows" -> :grid_template_rows
    end
  end

  defp grid_auto_property(property) do
    case property do
      "grid-auto-columns" -> :grid_auto_columns
      "grid-auto-rows" -> :grid_auto_rows
    end
  end

  defp grid_line_property(property) do
    case property do
      "grid-column-start" -> :grid_column_start
      "grid-column-end" -> :grid_column_end
      "grid-row-start" -> :grid_row_start
      "grid-row-end" -> :grid_row_end
    end
  end

  defp put_text_align(style, value) do
    case String.downcase(String.trim(value)) do
      "left" -> {:ok, Map.put(style, :text_align, :left)}
      "center" -> {:ok, Map.put(style, :text_align, :center)}
      "right" -> {:ok, Map.put(style, :text_align, :right)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_font_weight(style, value) do
    normalized = value |> String.trim() |> String.downcase()

    cond do
      normalized == "normal" ->
        {:ok, Map.put(style, :font_weight, 400)}

      normalized == "bold" ->
        {:ok, Map.put(style, :font_weight, 700)}

      Regex.match?(~r/^[1-9]00$/u, normalized) ->
        {weight, ""} = Integer.parse(normalized)

        case weight <= 900 do
          true -> {:ok, Map.put(style, :font_weight, weight)}
          false -> {:error, :invalid_document}
        end

      true ->
        {:error, :invalid_document}
    end
  end

  defp put_font_style(style, value) do
    case String.downcase(String.trim(value)) do
      "normal" -> {:ok, Map.put(style, :font_style, :normal)}
      "italic" -> {:ok, Map.put(style, :font_style, :italic)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_font_family(style, value) do
    family =
      value
      |> String.trim()
      |> String.trim("\"")
      |> String.trim("'")

    case family in ["Helvetica", "Courier", "Times-Roman"] do
      true -> {:ok, Map.put(style, :font_family, family)}
      false -> {:error, :invalid_document}
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

  defp parse_gap(value) do
    gaps =
      value
      |> String.split(~r/\s+/u, trim: true)
      |> Enum.map(&parse_length/1)

    case gaps do
      [{:ok, both}] -> {:ok, both, both}
      [{:ok, row_gap}, {:ok, column_gap}] -> {:ok, row_gap, column_gap}
      _ -> :error
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

  defp parse_nonnegative_number(value) do
    case Float.parse(String.trim(value)) do
      {number, ""} when number >= 0 -> {:ok, number}
      _ -> :error
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

  defp page_break_value(value) do
    case String.downcase(String.trim(value)) do
      "auto" -> {:ok, :auto}
      "always" -> {:ok, :page}
      "page" -> {:ok, :page}
      _ -> :error
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
