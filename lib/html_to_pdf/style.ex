defmodule NativeElixirPdfUtilities.HtmlToPdf.Style do
  @moduledoc """
  Style computation for the native HTML-to-PDF renderer.

  This module applies defaults, inheritance, font resolution, and the CSS
  cascade for the supported text, box, list, link, table, page-break, flexbox,
  grid, image, and embedded-font styling subset. Unsupported properties or
  invalid values fail the render instead of being ignored.
  """

  alias NativeElixirPdfUtilities.HtmlToPdf.CssParser
  alias NativeElixirPdfUtilities.HtmlToPdf.Font

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
        with {:ok, font_registry} <- font_registry(opts),
             {:ok, font_families, font_face} <-
               resolve_font(
                 Keyword.get(opts, :default_font, "Helvetica"),
                 400,
                 :normal,
                 font_registry
               ),
             {:ok, rules} <- stylesheet_rules(children, opts) do
          base_style = %{
            _custom_properties: %{},
            _font_registry: font_registry,
            color: {0, 0, 0},
            font_face: font_face,
            font_families: font_families,
            font_family: font_face.family,
            font_size: 12.0,
            font_style: :normal,
            font_weight: 400,
            letter_spacing: 0.0,
            line_height: 14.4,
            text_align: :left,
            text_transform: :none
          }

          with {:ok, root_style} <- fragment_root_style(children, base_style, rules, opts) do
            case root_style do
              nil ->
                {:ok, %{type: :document, children: []}}

              root_style ->
                with {:ok, styled_children} <-
                       style_children(children, root_style, rules, [], opts) do
                  {:ok, %{type: :document, children: styled_children}}
                end
            end
          end
        end

      _ ->
        {:error, :invalid_document}
    end
  end

  defp style_children(children, inherited_style, rules, ancestors, opts) do
    Enum.reduce_while(children, {:ok, []}, fn child, {:ok, acc} ->
      case style_node(child, inherited_style, rules, ancestors, opts) do
        {:ok, styled_children} -> {:cont, {:ok, acc ++ styled_children}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp style_node(node, inherited_style, rules, ancestors, opts) do
    case node do
      %{type: :text, text: text} when is_binary(text) ->
        {:ok,
         [
           %{
             type: :text,
             text: transform_text(text, Map.get(inherited_style, :text_transform, :none)),
             style: text_style(inherited_style)
           }
         ]}

      %{type: :element, tag: tag, attributes: attributes, children: children}
      when is_binary(tag) and is_map(attributes) and is_list(children) ->
        case tag do
          tag when tag in ["style", "meta", "title"] ->
            {:ok, []}

          "head" ->
            {:ok, []}

          tag when tag in ["html", "body"] ->
            with {:ok, element_style} <-
                   element_style(node, inherited_style, rules, ancestors, opts) do
              case Map.get(element_style, :display) do
                :none ->
                  {:ok, []}

                _ ->
                  style_children(
                    children,
                    text_style(element_style),
                    rules,
                    [node | ancestors],
                    opts
                  )
              end
            end

          _ ->
            with {:ok, element_style} <-
                   element_style(node, inherited_style, rules, ancestors, opts) do
              case Map.get(element_style, :display) do
                :none ->
                  {:ok, [%{type: :element, tag: tag, style: element_style, children: []}]}

                _ ->
                  with {:ok, styled_children} <-
                         style_children(
                           children,
                           text_style(element_style),
                           rules,
                           [node | ancestors],
                           opts
                         ) do
                    {:ok,
                     [
                       %{
                         type: :element,
                         tag: tag,
                         style: element_style,
                         children: styled_children
                       }
                     ]}
                  end
              end
            end
        end

      _ ->
        {:error, :invalid_document}
    end
  end

  defp fragment_root_style(children, base_style, rules, opts) do
    case Enum.any?(children, &document_container?/1) do
      true ->
        {:ok, base_style}

      false ->
        body_node = %{type: :element, tag: "body", attributes: %{}, children: []}

        with {:ok, body_style} <- element_style(body_node, base_style, rules, [], opts) do
          case Map.get(body_style, :display) do
            :none -> {:ok, nil}
            _ -> {:ok, text_style(body_style)}
          end
        end
    end
  end

  defp document_container?(node) do
    case node do
      %{type: :element, tag: tag} when tag in ["html", "body"] -> true
      _ -> false
    end
  end

  defp element_style(
         %{tag: tag, attributes: attributes} = node,
         inherited_style,
         rules,
         ancestors,
         opts
       ) do
    inherited_font_size = Map.fetch!(inherited_style, :font_size)

    defaults =
      case tag do
        "html" ->
          %{display: :block}

        "body" ->
          block_defaults(inherited_font_size, 400, 0.0)

        tag
        when tag in ["article", "aside", "div", "footer", "header", "main", "nav", "section"] ->
          block_defaults(inherited_font_size, 400, 0.0)

        "p" ->
          block_defaults(inherited_font_size, 400, inherited_font_size)

        "h1" ->
          block_defaults(inherited_font_size * 2.0, 700, inherited_font_size * 1.3333333333)

        "h2" ->
          block_defaults(inherited_font_size * 1.5, 700, inherited_font_size * 1.1666666667)

        "h3" ->
          block_defaults(inherited_font_size * 1.17, 700, inherited_font_size)

        "h4" ->
          block_defaults(inherited_font_size, 700, inherited_font_size * 0.8333333333)

        "h5" ->
          block_defaults(inherited_font_size * 0.83, 700, inherited_font_size * 0.6666666667)

        "h6" ->
          block_defaults(inherited_font_size * 0.67, 700, inherited_font_size * 0.6666666667)

        "ul" ->
          list_defaults(:disc, inherited_font_size)

        "ol" ->
          list_defaults(:decimal, inherited_font_size)

        "li" ->
          list_item_defaults(inherited_font_size)

        "table" ->
          table_defaults(inherited_font_size)

        "caption" ->
          table_caption_defaults(inherited_font_size)

        "thead" ->
          table_row_group_defaults(:head)

        "tbody" ->
          table_row_group_defaults(:body)

        "tfoot" ->
          table_row_group_defaults(:foot)

        "tr" ->
          table_row_defaults()

        "th" ->
          table_cell_defaults(:header, attributes, inherited_font_size)

        "td" ->
          table_cell_defaults(:data, attributes, inherited_font_size)

        "strong" ->
          %{display: :inline, font_weight: 700}

        "b" ->
          %{display: :inline, font_weight: 700}

        "em" ->
          %{display: :inline, font_style: :italic}

        "i" ->
          %{display: :inline, font_style: :italic}

        "span" ->
          %{display: :inline}

        "a" ->
          link_defaults(attributes)

        "br" ->
          %{display: :line_break}

        "img" ->
          image_defaults(attributes, opts, inherited_font_size)

        _ ->
          :invalid
      end

    case defaults do
      {:ok, defaults} ->
        style =
          inherited_style
          |> text_style()
          |> Map.merge(defaults)

        tag
        |> finalize_element_style(apply_author_styles(style, node, ancestors, rules))

      :invalid ->
        {:error, :invalid_document}

      defaults ->
        style =
          inherited_style
          |> text_style()
          |> Map.merge(defaults)

        tag
        |> finalize_element_style(apply_author_styles(style, node, ancestors, rules))
    end
  end

  defp finalize_element_style(tag, result) do
    case {tag, result} do
      {"img", {:ok, style}} ->
        finalize_image_style(style)

      {_tag, result} ->
        result
    end
  end

  defp finalize_image_style(style) do
    case {Map.get(style, :display), Map.get(style, :image), Map.get(style, :svg_image)} do
      {:image, nil, svg} when is_binary(svg) ->
        with {:ok, png} <- rasterize_svg(svg, svg_raster_options(style)),
             {:ok, image} <- decode_image(png) do
          {:ok,
           style
           |> Map.put(:image, image)
           |> Map.delete(:svg_image)}
        else
          _ -> {:error, :invalid_document}
        end

      _ ->
        {:ok, Map.delete(style, :svg_image)}
    end
  end

  defp put_line_height(style) do
    case Map.get(style, :line_height_multiplier) do
      multiplier when is_number(multiplier) ->
        style
        |> Map.put(:line_height, Map.fetch!(style, :font_size) * multiplier)
        |> Map.delete(:line_height_multiplier)
        |> Map.delete(:line_height_explicit)

      _ ->
        case Map.get(style, :line_height_explicit, false) do
          true ->
            case Map.get(style, :line_height_normal, false) do
              true ->
                style
                |> Map.put(:line_height, normal_line_height(style))
                |> Map.delete(:line_height_normal)
                |> Map.delete(:line_height_explicit)

              false ->
                Map.delete(style, :line_height_explicit)
            end

          false ->
            Map.put(style, :line_height, normal_line_height(style))
        end
    end
  end

  defp normal_line_height(style) do
    font_size = Map.fetch!(style, :font_size)

    case Map.get(style, :font_face) do
      %{type: :embedded, ascent: ascent, descent: descent, units_per_em: units_per_em}
      when is_number(ascent) and is_number(descent) and is_number(units_per_em) and
             units_per_em > 0 ->
        max((ascent - descent) / units_per_em * font_size, font_size)

      _ ->
        font_size * 1.2
    end
  end

  defp text_style(style) do
    Map.take(style, [
      :_custom_properties,
      :_font_registry,
      :color,
      :font_face,
      :font_families,
      :font_family,
      :font_size,
      :font_style,
      :font_weight,
      :letter_spacing,
      :line_break,
      :line_height,
      :link_url,
      :text_align,
      :text_transform
    ])
  end

  defp block_defaults(font_size, font_weight, margin_bottom) do
    %{
      background_color: nil,
      border_color: {0, 0, 0},
      border_colors: edges({0, 0, 0}),
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

  defp list_defaults(marker_type, font_size) do
    block_defaults(font_size, 400, font_size)
    |> Map.merge(%{
      display: :list,
      list_marker_type: marker_type,
      margin: edges(0.0, 0.0, font_size, 0.0),
      padding: edges(0.0, 0.0, 0.0, 24.0)
    })
  end

  defp list_item_defaults(font_size) do
    %{
      display: :list_item,
      font_size: font_size,
      font_weight: 400,
      margin: edges(0.0, 0.0, 4.0, 0.0),
      margin_after: 4.0
    }
  end

  defp table_defaults(font_size) do
    block_defaults(font_size, 400, 0.0)
    |> Map.merge(%{
      display: :table,
      border_collapse: :separate,
      margin: edges(0.0),
      padding: edges(0.0)
    })
  end

  defp table_caption_defaults(font_size) do
    block_defaults(font_size, 700, 4.0)
    |> Map.merge(%{
      display: :table_caption,
      text_align: :center
    })
  end

  defp table_row_group_defaults(section) do
    %{
      background_color: nil,
      border_color: {0, 0, 0},
      border_colors: edges({0, 0, 0}),
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
      border_colors: edges({0, 0, 0}),
      border_radius: 0.0,
      border_widths: edges(0.0),
      display: :table_row,
      padding: edges(0.0)
    }
  end

  defp table_cell_defaults(kind, attributes, font_size) do
    base = %{
      background_color: nil,
      border_color: {0, 0, 0},
      border_colors: edges({0, 0, 0}),
      border_radius: 0.0,
      border_widths: edges(1.0),
      colspan: 1,
      display: :table_cell,
      font_size: font_size,
      font_weight: 400,
      margin: edges(0.0),
      padding: edges(4.0),
      rowspan: 1,
      text_align: :left,
      vertical_align: :middle
    }

    with {:ok, colspan} <- positive_attribute(attributes, "colspan"),
         {:ok, rowspan} <- positive_attribute(attributes, "rowspan") do
      cell =
        base
        |> Map.put(:colspan, colspan)
        |> Map.put(:rowspan, rowspan)

      case kind do
        :header ->
          {:ok,
           Map.merge(cell, %{
             background_color: {0.9333333333, 0.9333333333, 0.9333333333},
             font_weight: 700,
             text_align: :center
           })}

        :data ->
          {:ok, cell}
      end
    else
      _ -> :invalid
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

  defp image_defaults(attributes, opts, font_size) do
    with src when is_binary(src) <- Map.get(attributes, "src"),
         {:ok, image_style} <- load_image_style(src, opts) do
      {:ok,
       block_defaults(font_size, 400, font_size)
       |> Map.merge(%{
         display: :image,
         margin: edges(0.0, 0.0, font_size, 0.0),
         padding: edges(0.0)
       })
       |> Map.merge(image_style)}
    else
      _ -> :invalid
    end
  end

  defp positive_attribute(attributes, name) do
    case Map.get(attributes, name) do
      nil ->
        {:ok, 1}

      value when is_binary(value) ->
        case Integer.parse(String.trim(value)) do
          {integer, ""} when integer >= 1 -> {:ok, integer}
          _ -> :error
        end

      _ ->
        :error
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

  defp font_registry(opts) do
    case Font.load_registry(opts) do
      {:ok, registry} -> {:ok, registry}
      :error -> {:error, :invalid_document}
    end
  end

  defp resolve_font(family_value, weight, style, registry) do
    case Font.resolve(family_value, weight, style, registry) do
      {:ok, families, font_face} -> {:ok, families, font_face}
      :error -> {:error, :invalid_document}
    end
  end

  defp apply_author_styles(style, node, ancestors, rules) do
    with {:ok, style} <- apply_stylesheet_rules(style, node, ancestors, rules),
         {:ok, style} <- apply_inline_style(style, Map.get(node.attributes, "style", "")),
         {:ok, style} <- put_font_face(style) do
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
              important: important_declaration?(declaration),
              specificity: specificity,
              order: rule.order,
              declaration_index: declaration_index
            }
          end)
      end
    end)
    |> Enum.sort_by(fn matched ->
      {matched.important, matched.specificity, matched.order, matched.declaration_index}
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
        matches_simple_selector?(part, node, ancestors)

      [part, left_part | remaining] ->
        case matches_simple_selector?(part, node, ancestors) do
          true -> matches_ancestor_selector?(part.combinator, left_part, remaining, ancestors)
          false -> false
        end
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

  defp matches_simple_selector?(part, node, ancestors) do
    attributes = Map.get(node, :attributes, %{})
    classes = attributes |> Map.get("class", "") |> String.split(~r/\s+/u, trim: true)

    tag_matches? = is_nil(part.tag) or Map.get(node, :tag) == part.tag
    id_matches? = is_nil(part.id) or Map.get(attributes, "id") == part.id
    classes_match? = Enum.all?(part.classes, &(&1 in classes))

    pseudo_classes_match? =
      Enum.all?(part.pseudo_classes, &pseudo_class_matches?(&1, node, ancestors))

    tag_matches? and id_matches? and classes_match? and pseudo_classes_match?
  end

  defp pseudo_class_matches?(pseudo_class, node, ancestors) do
    case pseudo_class do
      :root ->
        Map.get(node, :tag) == "html" and ancestors == []

      :first_child ->
        case ancestors do
          [%{children: children} | _rest] when is_list(children) ->
            children
            |> Enum.filter(&match?(%{type: :element}, &1))
            |> List.first()
            |> Kernel.==(node)

          _ ->
            false
        end

      :last_child ->
        case ancestors do
          [%{children: children} | _rest] when is_list(children) ->
            children
            |> Enum.filter(&match?(%{type: :element}, &1))
            |> List.last()
            |> Kernel.==(node)

          _ ->
            false
        end

      {:nth_child, index} ->
        case ancestors do
          [%{children: children} | _rest] when is_list(children) ->
            children
            |> Enum.filter(&match?(%{type: :element}, &1))
            |> Enum.at(index - 1)
            |> Kernel.==(node)

          _ ->
            false
        end
    end
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

  defp important_declaration?(declaration) do
    case declaration do
      {_property, _value, :important} -> true
      _ -> false
    end
  end

  defp apply_declaration(style, declaration) do
    case declaration do
      {property, value} -> apply_declaration_value(style, property, value)
      {property, value, :important} -> apply_declaration_value(style, property, value)
    end
  end

  defp apply_declaration_value(style, property, value) do
    case property do
      "--" <> _custom_property when is_binary(value) ->
        custom_properties =
          style
          |> Map.get(:_custom_properties, %{})
          |> Map.put(property, String.trim(value))

        {:ok, Map.put(style, :_custom_properties, custom_properties)}

      "color" ->
        with {:ok, color} <- parse_color(value, Map.fetch!(style, :color)) do
          case color do
            :transparent -> {:ok, style}
            color -> {:ok, Map.put(style, :color, color)}
          end
        end

      "background-color" ->
        with {:ok, color} <- parse_color(value, Map.fetch!(style, :color)) do
          {:ok, Map.put(style, :background_color, transparent_color_to_nil(color))}
        end

      "background" ->
        put_background(style, value)

      "box-sizing" ->
        put_box_sizing(style, value)

      "margin" ->
        with {:ok, lengths} <- parse_box_lengths(value, :margin),
             do: {:ok, put_box_lengths(style, "margin", lengths)}

      property when property in ["padding", "border-width"] ->
        with {:ok, lengths} <- parse_box_lengths(value),
             do: {:ok, put_box_lengths(style, property, lengths)}

      property when property in ["margin-top", "margin-right", "margin-bottom", "margin-left"] ->
        with {:ok, length} <- parse_length(value, :margin),
             do: {:ok, put_edge_length(style, property, length)}

      property
      when property in ["padding-top", "padding-right", "padding-bottom", "padding-left"] ->
        with {:ok, length} <- parse_length(value),
             do: {:ok, put_edge_length(style, property, length)}

      "border-color" ->
        with {:ok, color} <- parse_color(value, Map.fetch!(style, :color)) do
          case color do
            :transparent ->
              {:ok, style}

            color ->
              {:ok,
               style |> Map.put(:border_color, color) |> Map.put(:border_colors, edges(color))}
          end
        end

      property when property in ["border-top", "border-right", "border-bottom", "border-left"] ->
        with {:ok, width, color} <- parse_border_edge(value, Map.fetch!(style, :color)) do
          style = put_border_edge_width(style, property, width)

          case color do
            nil -> {:ok, style}
            color -> {:ok, put_border_edge_color(style, property, color)}
          end
        end

      property
      when property in [
             "border-top-width",
             "border-right-width",
             "border-bottom-width",
             "border-left-width"
           ] ->
        with {:ok, width} <- parse_length(value) do
          {:ok, put_border_edge_width(style, border_edge_property(property), width)}
        end

      property
      when property in [
             "border-top-color",
             "border-right-color",
             "border-bottom-color",
             "border-left-color"
           ] ->
        with {:ok, color} <- parse_color(value, Map.fetch!(style, :color)) do
          case color do
            :transparent -> {:ok, style}
            color -> {:ok, put_border_edge_color(style, border_edge_property(property), color)}
          end
        end

      property
      when property in [
             "border-style",
             "border-top-style",
             "border-right-style",
             "border-bottom-style",
             "border-left-style"
           ] ->
        put_border_style(style, property, value)

      "border-radius" ->
        with {:ok, length} <- parse_length(value),
             do: {:ok, Map.put(style, :border_radius, length)}

      "text-align" ->
        put_text_align(style, value)

      "text-transform" ->
        put_text_transform(style, value)

      "letter-spacing" ->
        put_letter_spacing(style, value)

      property when property in ["white-space", "word-break", "word-wrap", "overflow-wrap"] ->
        put_text_wrapping(style, property, value)

      property when property in ["break-before", "page-break-before"] ->
        with {:ok, break_value} <- page_break_value(value),
             do: {:ok, Map.put(style, :break_before, break_value)}

      property when property in ["break-after", "page-break-after"] ->
        with {:ok, break_value} <- page_break_value(value),
             do: {:ok, Map.put(style, :break_after, break_value)}

      "page-break-inside" ->
        put_break_inside(style, value)

      "line-break" ->
        put_line_break(style, value)

      "vertical-align" ->
        put_vertical_align(style, value)

      "min-height" ->
        put_size(style, :min_height, value)

      property when property in ["min-width", "max-width", "max-height"] ->
        put_size(style, length_property(property), value)

      "border" ->
        with {:ok, border_widths, border_color} <- parse_border(value, Map.fetch!(style, :color)) do
          style =
            style
            |> Map.put(:border_widths, border_widths)
            |> Map.put(:border_color, border_color)
            |> Map.put(:border_colors, edges(border_color))

          {:ok, style}
        end

      "font-size" ->
        with {:ok, length} <- parse_length(value), do: {:ok, Map.put(style, :font_size, length)}

      "line-height" ->
        case parse_line_height(value) do
          {:ok, {:multiplier, multiplier}} ->
            {:ok,
             style
             |> Map.put(:line_height_multiplier, multiplier)
             |> Map.put(:line_height_explicit, true)}

          {:ok, :normal} ->
            {:ok,
             style
             |> Map.put(:line_height_normal, true)
             |> Map.delete(:line_height_multiplier)
             |> Map.put(:line_height_explicit, true)}

          {:ok, line_height} ->
            {:ok,
             style
             |> Map.put(:line_height, line_height)
             |> Map.delete(:line_height_multiplier)
             |> Map.put(:line_height_explicit, true)}

          :error ->
            :error
        end

      "font-weight" ->
        put_font_weight(style, value)

      "font-style" ->
        put_font_style(style, value)

      "font-family" ->
        put_font_family(style, value)

      "display" ->
        put_display(style, value)

      property when property in ["width", "height"] ->
        put_size(style, length_property(property), value)

      "position" ->
        put_position(style, value)

      "overflow" ->
        put_overflow(style, value)

      "aspect-ratio" ->
        with {:ok, aspect_ratio} <- parse_aspect_ratio(value),
             do: {:ok, Map.put(style, :aspect_ratio, aspect_ratio)}

      "border-collapse" ->
        put_border_collapse(style, value)

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

      "justify-self" ->
        put_grid_alignment(style, :justify_self, value)

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
        {:ok, style |> ensure_box_style() |> Map.put(:display, :block)}

      "inline" ->
        {:ok, Map.put(style, :display, :inline)}

      "inline-block" ->
        {:ok, style |> ensure_box_style() |> Map.put(:display, :block)}

      "none" ->
        {:ok, Map.put(style, :display, :none)}

      "flex" ->
        {:ok,
         style |> ensure_box_style() |> flex_container_defaults() |> Map.put(:display, :flex)}

      "inline-flex" ->
        {:ok,
         style
         |> ensure_box_style()
         |> flex_container_defaults()
         |> Map.put(:display, :inline_flex)}

      "grid" ->
        {:ok,
         style |> ensure_box_style() |> grid_container_defaults() |> Map.put(:display, :grid)}

      "inline-grid" ->
        {:ok,
         style
         |> ensure_box_style()
         |> grid_container_defaults()
         |> Map.put(:display, :inline_grid)}

      _ ->
        {:error, :invalid_document}
    end
  end

  defp ensure_box_style(style) do
    style
    |> Map.put_new(:background_color, nil)
    |> Map.put_new(:border_color, {0, 0, 0})
    |> Map.put_new(:border_colors, edges(Map.get(style, :border_color, {0, 0, 0})))
    |> Map.put_new(:border_radius, 0.0)
    |> Map.put_new(:border_widths, edges(0.0))
    |> Map.put_new(:margin, edges(0.0, 0.0, Map.get(style, :margin_after, 0.0), 0.0))
    |> Map.put_new(:margin_after, 0.0)
    |> Map.put_new(:padding, edges(0.0))
  end

  defp put_size(style, property, value) do
    case resolved_css_value(style, value) do
      {:ok, value} ->
        with {:ok, size} <- parse_size(value), do: {:ok, Map.put(style, property, size)}

      :error ->
        {:error, :invalid_document}
    end
  end

  defp put_vertical_align(style, value) do
    case value |> String.trim() |> String.downcase() do
      "baseline" -> {:ok, Map.put(style, :vertical_align, :baseline)}
      "top" -> {:ok, Map.put(style, :vertical_align, :top)}
      "middle" -> {:ok, Map.put(style, :vertical_align, :middle)}
      "bottom" -> {:ok, Map.put(style, :vertical_align, :bottom)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_line_break(style, value) do
    case value |> String.trim() |> String.downcase() do
      value when value in ["auto", "normal"] -> {:ok, Map.put(style, :line_break, :normal)}
      "anywhere" -> {:ok, Map.put(style, :line_break, :anywhere)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_background(style, value) do
    case value |> String.trim() |> String.downcase() do
      "none" ->
        {:ok, Map.put(style, :background_color, nil)}

      value ->
        with {:ok, color} <- parse_color(value, Map.fetch!(style, :color)) do
          {:ok, Map.put(style, :background_color, transparent_color_to_nil(color))}
        end
    end
  end

  defp put_position(style, value) do
    case value |> String.trim() |> String.downcase() do
      value when value in ["static", "relative"] -> {:ok, style}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_overflow(style, value) do
    case value |> String.trim() |> String.downcase() do
      "visible" -> {:ok, Map.put(style, :overflow, :visible)}
      "hidden" -> {:ok, Map.put(style, :overflow, :hidden)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_border_style(style, property, value) do
    normalized = value |> String.trim() |> String.downcase()

    cond do
      normalized in ["solid", "dashed"] ->
        {:ok, style}

      normalized == "none" and property == "border-style" ->
        {:ok, Map.put(style, :border_widths, edges(0.0))}

      normalized == "none" ->
        {:ok, put_border_edge_width(style, border_edge_property(property), 0.0)}

      true ->
        {:error, :invalid_document}
    end
  end

  defp put_box_sizing(style, value) do
    case value |> String.trim() |> String.downcase() do
      value when value in ["border-box", "content-box"] -> {:ok, style}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_text_transform(style, value) do
    case value |> String.trim() |> String.downcase() do
      "none" -> {:ok, Map.put(style, :text_transform, :none)}
      "uppercase" -> {:ok, Map.put(style, :text_transform, :uppercase)}
      "lowercase" -> {:ok, Map.put(style, :text_transform, :lowercase)}
      "capitalize" -> {:ok, Map.put(style, :text_transform, :capitalize)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_letter_spacing(style, value) do
    case value |> String.trim() |> String.downcase() do
      "normal" ->
        {:ok, Map.put(style, :letter_spacing, 0.0)}

      value ->
        with {:ok, length} <- parse_letter_spacing(value, Map.fetch!(style, :font_size)) do
          {:ok, Map.put(style, :letter_spacing, length)}
        else
          :error -> {:error, :invalid_document}
        end
    end
  end

  defp put_text_wrapping(style, property, value) do
    normalized = value |> String.trim() |> String.downcase()

    case {property, normalized} do
      {"white-space", value} when value in ["normal", "pre-line"] ->
        {:ok, style}

      {"white-space", "nowrap"} ->
        {:ok, Map.put(style, :line_break, :normal)}

      {property, "break-word"} when property in ["word-break", "word-wrap", "overflow-wrap"] ->
        {:ok, Map.put(style, :line_break, :break_word)}

      {property, "anywhere"} when property in ["word-break", "word-wrap", "overflow-wrap"] ->
        {:ok, Map.put(style, :line_break, :anywhere)}

      {property, "normal"} when property in ["word-break", "word-wrap", "overflow-wrap"] ->
        {:ok, Map.put(style, :line_break, :normal)}

      _ ->
        {:error, :invalid_document}
    end
  end

  defp put_break_inside(style, value) do
    case value |> String.trim() |> String.downcase() do
      value when value in ["auto", "avoid"] -> {:ok, style}
      _ -> {:error, :invalid_document}
    end
  end

  defp transform_text(text, transform) do
    case transform do
      :uppercase -> String.upcase(text)
      :lowercase -> String.downcase(text)
      :capitalize -> Regex.replace(~r/\b\p{L}/u, text, &String.upcase/1)
      _ -> text
    end
  end

  defp resolved_css_value(style, value) do
    normalized = String.trim(value)

    case Regex.named_captures(~r/^var\((?<name>--[a-zA-Z_][a-zA-Z0-9_-]*)\)$/u, normalized) do
      %{"name" => name} ->
        case Map.get(Map.get(style, :_custom_properties, %{}), name) do
          value when is_binary(value) -> {:ok, value}
          _ -> :error
        end

      _ ->
        {:ok, normalized}
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
    |> Map.put_new(:align_content, :stretch)
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

  defp put_border_collapse(style, value) do
    case value |> String.trim() |> String.downcase() do
      "collapse" -> {:ok, Map.put(style, :border_collapse, :collapse)}
      "separate" -> {:ok, Map.put(style, :border_collapse, :separate)}
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
      "min-width" -> :min_width
      "max-width" -> :max_width
      "max-height" -> :max_height
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
    parsed_tracks =
      value
      |> String.trim()
      |> String.downcase()
      |> split_css_tokens()
      |> Enum.map(&parse_grid_track_source/1)

    case Enum.all?(parsed_tracks, &match?({:ok, _track}, &1)) do
      true -> {:ok, Enum.flat_map(parsed_tracks, fn {:ok, tracks} -> tracks end)}
      false -> :error
    end
  end

  defp split_css_tokens(value) do
    value
    |> String.graphemes()
    |> Enum.reduce({[], "", 0}, fn grapheme, {tokens, current, depth} ->
      cond do
        grapheme == "(" ->
          {tokens, current <> grapheme, depth + 1}

        grapheme == ")" ->
          {tokens, current <> grapheme, max(depth - 1, 0)}

        String.match?(grapheme, ~r/\s/u) and depth == 0 ->
          case current do
            "" -> {tokens, "", depth}
            current -> {tokens ++ [current], "", depth}
          end

        true ->
          {tokens, current <> grapheme, depth}
      end
    end)
    |> then(fn {tokens, current, _depth} -> tokens ++ [current] end)
  end

  defp split_css_comma(value) do
    value
    |> String.graphemes()
    |> Enum.reduce({[], "", 0}, fn grapheme, {tokens, current, depth} ->
      cond do
        grapheme == "(" ->
          {tokens, current <> grapheme, depth + 1}

        grapheme == ")" ->
          {tokens, current <> grapheme, max(depth - 1, 0)}

        grapheme == "," and depth == 0 ->
          {tokens ++ [String.trim(current)], "", depth}

        true ->
          {tokens, current <> grapheme, depth}
      end
    end)
    |> then(fn {tokens, current, _depth} -> tokens ++ [String.trim(current)] end)
    |> Enum.reject(&(&1 == ""))
  end

  defp parse_grid_track_source(value) do
    normalized = value |> String.trim() |> String.downcase()

    case Regex.named_captures(~r/^repeat\((?<count>[1-9]\d*)\s*,\s*(?<track>.+)\)$/u, normalized) do
      %{"count" => count, "track" => track} ->
        with {count, ""} <- Integer.parse(count),
             {:ok, parsed_track} <- parse_grid_track(track) do
          {:ok, List.duplicate(parsed_track, count)}
        else
          _ -> :error
        end

      _ ->
        with {:ok, track} <- parse_grid_track(normalized), do: {:ok, [track]}
    end
  end

  defp parse_grid_track(value) do
    normalized = value |> String.trim() |> String.downcase()

    cond do
      String.starts_with?(normalized, "minmax(") ->
        parse_minmax_grid_track(normalized)

      normalized == "auto" ->
        {:ok, :auto}

      Regex.match?(~r/^\d+(?:\.\d+)?fr$/u, normalized) ->
        {number, "fr"} = Float.parse(normalized)
        {:ok, {:fr, number}}

      true ->
        with {:ok, length} <- parse_length(normalized), do: {:ok, {:length, length}}
    end
  end

  defp parse_minmax_grid_track(value) do
    case Regex.named_captures(~r/^minmax\((?<min>.+)\s*,\s*(?<max>.+)\)$/u, value) do
      %{"max" => max_track} -> parse_grid_track(max_track)
      _ -> :error
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
        {:ok, Map.put(style, :font_weight, weight)}

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
    with {:ok, families, font_face} <-
           resolve_font(
             value,
             Map.fetch!(style, :font_weight),
             Map.fetch!(style, :font_style),
             Map.fetch!(style, :_font_registry)
           ) do
      {:ok,
       style
       |> Map.put(:font_families, families)
       |> Map.put(:font_family, font_face.family)
       |> Map.put(:font_face, font_face)}
    end
  end

  defp put_font_face(style) do
    with {:ok, families, font_face} <-
           resolve_font(
             Map.fetch!(style, :font_families),
             Map.fetch!(style, :font_weight),
             Map.fetch!(style, :font_style),
             Map.fetch!(style, :_font_registry)
           ) do
      {:ok,
       style
       |> Map.put(:font_families, families)
       |> Map.put(:font_family, font_face.family)
       |> Map.put(:font_face, font_face)}
    end
  end

  defp parse_color(value, current_color) do
    normalized = value |> String.trim() |> String.downcase()

    cond do
      normalized == "currentcolor" and not is_nil(current_color) ->
        {:ok, current_color}

      normalized == "transparent" ->
        {:ok, :transparent}

      Map.has_key?(named_colors(), normalized) ->
        {:ok, Map.fetch!(named_colors(), normalized)}

      Regex.match?(~r/^#[0-9a-f]{6}$/u, normalized) ->
        <<"#", red::binary-size(2), green::binary-size(2), blue::binary-size(2)>> = normalized
        {:ok, {hex_to_pdf_color(red), hex_to_pdf_color(green), hex_to_pdf_color(blue)}}

      Regex.match?(~r/^#[0-9a-f]{8}$/u, normalized) ->
        <<"#", red::binary-size(2), green::binary-size(2), blue::binary-size(2),
          _alpha::binary-size(2)>> = normalized

        {:ok, {hex_to_pdf_color(red), hex_to_pdf_color(green), hex_to_pdf_color(blue)}}

      Regex.match?(~r/^#[0-9a-f]{3}$/u, normalized) ->
        <<"#", red::binary-size(1), green::binary-size(1), blue::binary-size(1)>> = normalized

        {:ok,
         {hex_to_pdf_color(red <> red), hex_to_pdf_color(green <> green),
          hex_to_pdf_color(blue <> blue)}}

      String.starts_with?(normalized, "rgb(") or String.starts_with?(normalized, "rgba(") ->
        parse_rgb_color(normalized)

      true ->
        :error
    end
  end

  defp parse_rgb_color(value) do
    case Regex.named_captures(~r/^rgba?\((?<channels>.+)\)$/u, value) do
      %{"channels" => channels} ->
        channels = split_css_comma(channels)

        case channels do
          [red, green, blue] ->
            parse_rgb_channels(red, green, blue)

          [red, green, blue, _alpha] ->
            parse_rgb_channels(red, green, blue)

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  defp parse_rgb_channels(red, green, blue) do
    with {:ok, red} <- parse_rgb_channel(red),
         {:ok, green} <- parse_rgb_channel(green),
         {:ok, blue} <- parse_rgb_channel(blue) do
      {:ok, {red, green, blue}}
    end
  end

  defp parse_rgb_channel(channel) do
    normalized = String.trim(channel)

    cond do
      Regex.match?(~r/^\d+(?:\.\d+)?%$/u, normalized) ->
        {number, "%"} = Float.parse(normalized)
        {:ok, number |> max(0.0) |> min(100.0) |> Kernel./(100.0)}

      true ->
        case Float.parse(normalized) do
          {number, ""} -> {:ok, number |> max(0.0) |> min(255.0) |> Kernel./(255.0)}
          _ -> :error
        end
    end
  end

  defp parse_box_lengths(value) do
    parse_box_lengths(value, :nonnegative)
  end

  defp parse_box_lengths(value, length_context) do
    lengths =
      value
      |> String.split(~r/\s+/u, trim: true)
      |> Enum.map(&parse_length(&1, length_context))

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
    parse_length(value, :nonnegative)
  end

  defp parse_size(value) do
    normalized = String.trim(value)

    cond do
      Regex.match?(~r/^\d+(?:\.\d+)?%$/u, normalized) ->
        {number, "%"} = Float.parse(normalized)
        {:ok, {:percent, number / 100}}

      String.starts_with?(String.downcase(normalized), "min(") ->
        parse_min_size(normalized)

      true ->
        parse_length(value)
    end
  end

  defp parse_min_size(value) do
    case Regex.named_captures(~r/^min\((?<sizes>.+)\)$/u, String.downcase(String.trim(value))) do
      %{"sizes" => sizes} ->
        parsed_sizes =
          sizes
          |> split_css_comma()
          |> Enum.map(&parse_size/1)

        case Enum.all?(parsed_sizes, &match?({:ok, _size}, &1)) do
          true -> {:ok, {:min, Enum.map(parsed_sizes, fn {:ok, size} -> size end)}}
          false -> :error
        end

      _ ->
        :error
    end
  end

  defp parse_length(value, length_context) do
    normalized = String.trim(value)

    case Regex.run(~r/^(?:(0)|(-?\d+(?:\.\d+)?)(pt|px|mm|cm|in|rem))$/u, normalized) do
      [_, "0"] ->
        {:ok, 0.0}

      [_, "", value, unit] ->
        {number, ""} = Float.parse(value)

        case length_context == :margin or number >= 0 do
          true -> {:ok, number * points_per_unit(unit)}
          false -> :error
        end

      _ ->
        :error
    end
  end

  defp parse_letter_spacing(value, font_size) do
    normalized = String.trim(value)

    case Regex.run(~r/^(?:(0)|(-?\d+(?:\.\d+)?)(pt|px|mm|cm|in|rem|em))$/u, normalized) do
      [_, "0"] ->
        {:ok, 0.0}

      [_, "", value, "em"] ->
        {number, ""} = Float.parse(value)
        {:ok, number * font_size}

      [_, "", value, unit] ->
        {number, ""} = Float.parse(value)
        {:ok, number * points_per_unit(unit)}

      _ ->
        :error
    end
  end

  defp parse_aspect_ratio(value) do
    tokens =
      value
      |> String.trim()
      |> String.downcase()
      |> String.split("/", trim: true)
      |> Enum.map(&String.trim/1)

    case tokens do
      [ratio] ->
        with {:ok, number} <- parse_positive_number(ratio), do: {:ok, number}

      [width, height] ->
        with {:ok, width} <- parse_positive_number(width),
             {:ok, height} <- parse_positive_number(height) do
          {:ok, width / height}
        end

      _ ->
        :error
    end
  end

  defp parse_line_height(value) do
    normalized = value |> String.trim() |> String.downcase()

    cond do
      normalized == "normal" ->
        {:ok, :normal}

      match?({:ok, _length}, parse_length(normalized)) ->
        parse_length(normalized)

      true ->
        with {:ok, multiplier} <- parse_nonnegative_number(normalized) do
          {:ok, {:multiplier, multiplier}}
        end
    end
  end

  defp parse_positive_number(value) do
    case Float.parse(String.trim(value)) do
      {number, ""} when number > 0 -> {:ok, number}
      _ -> :error
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

  defp put_border_edge_width(style, property, width) do
    edge =
      property
      |> String.replace_prefix("border-", "")
      |> String.to_existing_atom()

    Map.update!(style, :border_widths, &Map.put(&1, edge, width))
  end

  defp put_border_edge_color(style, property, color) do
    edge =
      property
      |> String.replace_prefix("border-", "")
      |> String.to_existing_atom()

    style
    |> Map.update(
      :border_colors,
      edges(Map.get(style, :border_color, {0, 0, 0})),
      &Map.put(&1, edge, color)
    )
    |> Map.put(:border_color, color)
  end

  defp border_edge_property(property) do
    property
    |> String.replace_suffix("-width", "")
    |> String.replace_suffix("-color", "")
    |> String.replace_suffix("-style", "")
  end

  defp parse_border(value, current_color) do
    tokens = String.split(value, ~r/\s+/u, trim: true)

    case tokens do
      ["none"] ->
        {:ok, edges(0.0), current_color}

      ["0"] ->
        {:ok, edges(0.0), current_color}

      tokens ->
        parsed =
          Enum.reduce_while(tokens, %{width: nil, style: nil, color: nil}, fn token, acc ->
            cond do
              token in ["solid", "dashed"] and is_nil(acc.style) ->
                {:cont, %{acc | style: String.to_atom(token)}}

              is_nil(acc.width) and match?({:ok, _}, parse_length(token)) ->
                {:ok, width} = parse_length(token)
                {:cont, %{acc | width: width}}

              is_nil(acc.color) and match?({:ok, _}, parse_color(token, current_color)) ->
                {:ok, color} = parse_color(token, current_color)
                {:cont, %{acc | color: color}}

              true ->
                {:halt, :error}
            end
          end)

        case parsed do
          %{width: width, style: style, color: color}
          when is_number(width) and style in [:solid, :dashed] ->
            border_color =
              case color do
                nil -> current_color
                :transparent -> current_color
                color -> color
              end

            {:ok, edges(width), border_color}

          _ ->
            :error
        end
    end
  end

  defp parse_border_edge(value, current_color) do
    case value |> String.trim() |> String.downcase() do
      "none" ->
        {:ok, 0.0, nil}

      "0" ->
        {:ok, 0.0, nil}

      _ ->
        with {:ok, border_widths, border_color} <- parse_border(value, current_color) do
          {:ok, Enum.max(Map.values(border_widths)), border_color}
        end
    end
  end

  defp transparent_color_to_nil(color) do
    case color do
      :transparent -> nil
      color -> color
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

  defp load_image_style(src, opts) do
    case image_source(src, Keyword.get(opts, :base_url)) do
      {:ok, svg, :svg} ->
        {:ok, %{svg_image: svg}}

      {:ok, data, expected_format} ->
        with {:ok, image} <- decode_image(data) do
          case is_nil(expected_format) or image.format == expected_format do
            true -> {:ok, %{image: image}}
            false -> :error
          end
        end

      _ ->
        :error
    end
  end

  defp data_uri_image_source(src) do
    case Regex.run(~r/^data:image\/svg\+xml;base64,([A-Za-z0-9+\/=\r\n]+)$/u, src) do
      [_, encoded] ->
        with {:ok, svg} <- Base.decode64(String.replace(encoded, ~r/\s/u, "")) do
          {:ok, svg, :svg}
        end

      _ ->
        case Regex.run(~r/^data:(image\/(?:png|jpeg));base64,([A-Za-z0-9+\/=\r\n]+)$/u, src) do
          [_, mime_type, encoded] ->
            with {:ok, data} <- Base.decode64(String.replace(encoded, ~r/\s/u, "")) do
              {:ok, data, data_uri_format(mime_type)}
            end

          _ ->
            :error
        end
    end
  end

  defp rasterize_svg(svg, raster_options) do
    case String.valid?(svg) do
      true ->
        sanitized_svg =
          svg
          |> String.replace(~r/<\?xml[^>]*>/iu, "")
          |> String.replace(~r/<!DOCTYPE[^>]*(?:\[[\s\S]*?\]\s*)?>/iu, "")

        case Resvg.svg_string_to_png_buffer(
               sanitized_svg,
               [
                 resources_dir: System.tmp_dir!(),
                 shape_rendering: :optimize_speed,
                 text_rendering: :optimize_speed,
                 image_rendering: :optimize_speed,
                 skip_system_fonts: true
               ] ++ raster_options
             ) do
          {:ok, png} -> {:ok, IO.iodata_to_binary(png)}
          {:error, _reason} -> :error
        end

      false ->
        :error
    end
  end

  defp svg_raster_options(style) do
    dimensions =
      [
        width: style |> target_image_width() |> points_to_pixels(),
        height: style |> target_image_height() |> points_to_pixels()
      ]
      |> Enum.reject(fn {_key, value} -> is_nil(value) end)

    dimensions
  end

  defp target_image_width(style) do
    case {Map.get(style, :width), Map.get(style, :height), Map.get(style, :aspect_ratio)} do
      {width, _height, _ratio} when is_number(width) ->
        width

      {_width, height, ratio} when is_number(height) and is_number(ratio) ->
        height * ratio

      _ ->
        nil
    end
  end

  defp target_image_height(style) do
    case {Map.get(style, :height), Map.get(style, :width), Map.get(style, :aspect_ratio)} do
      {height, _width, _ratio} when is_number(height) ->
        height

      {_height, width, ratio} when is_number(width) and is_number(ratio) ->
        width / ratio

      _ ->
        nil
    end
  end

  defp points_to_pixels(points) do
    case points do
      points when is_number(points) -> max(round(points / 0.75), 1)
      _ -> nil
    end
  end

  defp image_source(src, base_url) do
    cond do
      String.starts_with?(src, "data:") ->
        data_uri_image_source(src)

      Path.type(src) == :absolute ->
        with {:ok, data} <- File.read(src), do: {:ok, data, nil}

      is_binary(base_url) ->
        with {:ok, path} <- relative_image_path(src, base_url),
             {:ok, data} <- File.read(path) do
          {:ok, data, nil}
        end

      true ->
        :error
    end
  end

  defp data_uri_format(mime_type) do
    case mime_type do
      "image/png" -> :png
      "image/jpeg" -> :jpeg
    end
  end

  defp relative_image_path(src, base_url) do
    case String.contains?(src, ["\0", "://"]) do
      true ->
        :error

      false ->
        base_path =
          case URI.parse(base_url) do
            %URI{scheme: nil, path: path} when is_binary(path) -> path
            %URI{scheme: "file", path: path} when is_binary(path) -> path
            _ -> nil
          end

        case base_path do
          nil ->
            :error

          base_path ->
            expanded_base = Path.expand(base_path)
            path = Path.expand(src, expanded_base)

            case path == expanded_base or String.starts_with?(path, expanded_base <> "/") do
              true -> {:ok, path}
              false -> :error
            end
        end
    end
  end

  defp decode_image(data) do
    cond do
      String.starts_with?(data, <<137, 80, 78, 71, 13, 10, 26, 10>>) ->
        decode_png(data)

      String.starts_with?(data, <<255, 216>>) ->
        decode_jpeg(data)

      true ->
        :error
    end
  end

  defp decode_png(<<137, 80, 78, 71, 13, 10, 26, 10, chunks::binary>>) do
    with {:ok, parsed} <- png_chunks(chunks, %{idat: []}),
         %{
           width_px: width,
           height_px: height,
           bit_depth: 8,
           color_type: color_type,
           compression: 0,
           filter: 0,
           interlace: 0,
           idat: idat
         }
         when width > 0 and height > 0 and color_type in [2, 6] <- parsed,
         {:ok, inflated} <- png_inflate(Enum.join(idat, "")),
         {:ok, rgb_data, alpha_data} <- png_image_data(inflated, width, height, color_type) do
      image = %{
        format: :png,
        data: rgb_data,
        width_px: width,
        height_px: height,
        width: width * 0.75,
        height: height * 0.75,
        color_space: :device_rgb,
        bits_per_component: 8
      }

      {:ok,
       case alpha_data do
         nil -> image
         alpha_data -> Map.put(image, :alpha_data, alpha_data)
       end}
    else
      _ -> :error
    end
  end

  defp png_chunks(chunks, acc) do
    case chunks do
      <<0::32, "IEND", _crc::32>> ->
        {:ok, acc}

      <<length::32, type::binary-size(4), data::binary-size(length), _crc::32, rest::binary>> ->
        png_chunk(type, data, rest, acc)

      _ ->
        :error
    end
  end

  defp png_chunk(type, data, rest, acc) do
    case type do
      "IHDR" ->
        case data do
          <<width::32, height::32, bit_depth, color_type, compression, filter, interlace>> ->
            png_chunks(
              rest,
              Map.merge(acc, %{
                width_px: width,
                height_px: height,
                bit_depth: bit_depth,
                color_type: color_type,
                compression: compression,
                filter: filter,
                interlace: interlace
              })
            )

          _ ->
            :error
        end

      "IDAT" ->
        png_chunks(rest, Map.update!(acc, :idat, &(&1 ++ [data])))

      _ ->
        png_chunks(rest, acc)
    end
  end

  defp png_inflate(data) do
    try do
      {:ok, :zlib.uncompress(data)}
    rescue
      ErlangError -> :error
    end
  end

  defp png_image_data(data, width, height, color_type) do
    bytes_per_pixel =
      case color_type do
        2 -> 3
        6 -> 4
      end

    row_size = width * bytes_per_pixel

    case png_rows(data, width, height, bytes_per_pixel, row_size, [], "") do
      {:ok, rows} ->
        {rgb, alpha} = split_png_rows(rows, color_type)

        {:ok, rgb, alpha}

      :error ->
        :error
    end
  end

  defp split_png_rows(rows, color_type) do
    case color_type do
      2 ->
        {Enum.join(rows, ""), nil}

      6 ->
        {rgb_rows, alpha_rows} =
          Enum.map(rows, &split_png_rgba/1)
          |> Enum.unzip()

        alpha = Enum.join(alpha_rows, "")

        case alpha == :binary.copy(<<255>>, byte_size(alpha)) do
          true -> {Enum.join(rgb_rows, ""), nil}
          false -> {Enum.join(rgb_rows, ""), alpha}
        end
    end
  end

  defp png_rows(data, _width, height, _bytes_per_pixel, _row_size, rows, _previous)
       when length(rows) == height do
    case data do
      "" -> {:ok, Enum.reverse(rows)}
      _ -> :error
    end
  end

  defp png_rows(data, width, height, bytes_per_pixel, row_size, rows, previous) do
    case data do
      <<filter, row::binary-size(row_size), rest::binary>> ->
        with {:ok, decoded} <- png_unfilter_row(filter, row, previous, bytes_per_pixel) do
          png_rows(rest, width, height, bytes_per_pixel, row_size, [decoded | rows], decoded)
        end

      _ ->
        :error
    end
  end

  defp png_unfilter_row(filter, row, previous, bytes_per_pixel) do
    case filter do
      0 ->
        {:ok, row}

      filter when filter in [1, 2, 3, 4] ->
        previous =
          case previous do
            "" -> :binary.copy(<<0>>, byte_size(row))
            previous -> previous
          end

        {:ok, png_unfilter_bytes(filter, row, previous, bytes_per_pixel, 0, [], [], [])}

      _ ->
        :error
    end
  end

  defp png_unfilter_bytes(
         _filter,
         "",
         "",
         _bytes_per_pixel,
         _index,
         _left_window,
         _up_left_window,
         acc
       ) do
    acc
    |> Enum.reverse()
    |> :binary.list_to_bin()
  end

  defp png_unfilter_bytes(
         filter,
         <<byte, row_rest::binary>>,
         <<up, previous_rest::binary>>,
         bytes_per_pixel,
         index,
         left_window,
         up_left_window,
         acc
       ) do
    left = if index >= bytes_per_pixel, do: hd(left_window), else: 0
    up_left = if index >= bytes_per_pixel, do: hd(up_left_window), else: 0

    predictor =
      case filter do
        1 -> left
        2 -> up
        3 -> div(left + up, 2)
        4 -> png_paeth(left, up, up_left)
      end

    decoded = rem(byte + predictor, 256)

    png_unfilter_bytes(
      filter,
      row_rest,
      previous_rest,
      bytes_per_pixel,
      index + 1,
      png_window_push(left_window, decoded, bytes_per_pixel),
      png_window_push(up_left_window, up, bytes_per_pixel),
      [decoded | acc]
    )
  end

  defp png_window_push(window, byte, bytes_per_pixel) do
    case length(window) < bytes_per_pixel do
      true -> window ++ [byte]
      false -> tl(window) ++ [byte]
    end
  end

  defp png_paeth(left, up, up_left) do
    estimate = left + up - up_left
    left_distance = abs(estimate - left)
    up_distance = abs(estimate - up)
    up_left_distance = abs(estimate - up_left)

    cond do
      left_distance <= up_distance and left_distance <= up_left_distance -> left
      up_distance <= up_left_distance -> up
      true -> up_left
    end
  end

  defp split_png_rgba(row) do
    row
    |> :binary.bin_to_list()
    |> Enum.chunk_every(4)
    |> Enum.reduce({"", ""}, fn [red, green, blue, alpha], {rgb, mask} ->
      {rgb <> <<red, green, blue>>, mask <> <<alpha>>}
    end)
  end

  defp decode_jpeg(data) do
    with {:ok, width, height, color_space} <- jpeg_dimensions(data) do
      {:ok,
       %{
         format: :jpeg,
         data: data,
         width_px: width,
         height_px: height,
         width: width * 0.75,
         height: height * 0.75,
         color_space: color_space,
         bits_per_component: 8
       }}
    end
  end

  defp jpeg_dimensions(<<255, 216, rest::binary>>) do
    jpeg_marker_dimensions(rest)
  end

  defp jpeg_marker_dimensions(data) do
    case data do
      <<255, marker, rest::binary>> when marker in [192, 194] ->
        case rest do
          <<_length::16, 8, height::16, width::16, components, _component_data::binary>>
          when width > 0 and height > 0 and components in [1, 3, 4] ->
            {:ok, width, height, jpeg_color_space(components)}

          _ ->
            :error
        end

      <<255, marker, rest::binary>> when marker in [216, 217] ->
        jpeg_marker_dimensions(rest)

      <<255, marker, rest::binary>> when marker >= 208 and marker <= 215 ->
        jpeg_marker_dimensions(rest)

      <<255, _marker, length::16, _segment::binary-size(length - 2), rest::binary>>
      when length >= 2 ->
        jpeg_marker_dimensions(rest)

      _ ->
        :error
    end
  end

  defp jpeg_color_space(components) do
    case components do
      1 -> :device_gray
      3 -> :device_rgb
      4 -> :device_cmyk
    end
  end

  defp named_colors do
    %{
      "black" => {0, 0, 0},
      "blue" => {0, 0, 1},
      "gray" => {0.5019607843, 0.5019607843, 0.5019607843},
      "green" => {0, 0.5019607843, 0},
      "grey" => {0.5019607843, 0.5019607843, 0.5019607843},
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
      "rem" -> 12.0
    end
  end
end
