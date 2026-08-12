defmodule NativeElixirPdfUtilities.Validators.HtmlValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry

  @render_option_keys [
    :page_size,
    :margin,
    :base_url,
    :stylesheets,
    :default_font,
    :fonts,
    :metadata,
    :page_furniture
  ]
  @furniture_keys [:header, :footer]
  @variant_keys [:default, :first, :odd, :even]

  @doc false
  @spec validate_render_request(term(), term()) ::
          {:ok, %{html: binary(), options: keyword()}}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_render_request(html, opts) do
    with {:ok, html} <- validate_html(html),
         {:ok, opts} <- validate_render_options(opts) do
      {:ok, %{html: html, options: opts}}
    end
  end

  @doc false
  @spec validate_paths(term(), term()) ::
          {:ok, %{input_path: binary(), output_path: binary()}}
          | {:error, {:invalid_path, Diagnostics.diagnostic()}}
  def validate_paths(input_path, output_path) do
    case {input_path, output_path} do
      {input_path, output_path} when is_binary(input_path) and is_binary(output_path) ->
        {:ok, %{input_path: input_path, output_path: output_path}}

      _ ->
        Diagnostics.error(:file, :invalid_path, "input and output paths must be strings")
    end
  end

  @doc false
  @spec validate_style_input(term(), term()) ::
          {:ok, %{dom: map(), options: keyword()}}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_style_input(dom, opts) do
    with :ok <- validate_dom(dom),
         {:ok, opts} <- validate_style_options(opts) do
      {:ok, %{dom: dom, options: opts}}
    else
      :invalid_document ->
        Diagnostics.error(
          :style,
          :invalid_document,
          "document tree must be a parsed HTML document"
        )

      {:error, {_reason, _diagnostic}} = error ->
        error
    end
  end

  @doc false
  @spec prepare_layout(term(), term()) ::
          {:ok, %{children: [term()], page_size: {float(), float()}, margins: map()}}
          | {:error, :invalid_layout | :invalid_margin | :invalid_page_size}
  def prepare_layout(styled_tree, opts) do
    case {styled_tree, opts} do
      {%{type: :document, children: children}, opts}
      when is_list(children) and is_list(opts) ->
        case Keyword.keyword?(opts) do
          true ->
            with {:ok, page_size} <-
                   PageGeometry.normalize_page_size(Keyword.get(opts, :page_size, :a4)),
                 {:ok, margins} <-
                   PageGeometry.normalize_margins(Keyword.get(opts, :margin, 0)),
                 true <- PageGeometry.valid_printable_area?(page_size, margins) do
              {:ok, %{children: children, page_size: page_size, margins: margins}}
            else
              false -> {:error, :invalid_margin}
              {:error, reason} -> {:error, reason}
            end

          false ->
            {:error, :invalid_layout}
        end

      _ ->
        {:error, :invalid_layout}
    end
  end

  @doc false
  @spec prepare_pagination(term(), term()) ::
          {:ok,
           %{layout_tree: map(), page_size: {number(), number()}, boxes: [term()], margins: map()}}
          | {:error, {:invalid_layout, Diagnostics.diagnostic()}}
  def prepare_pagination(layout_tree, opts) do
    case {layout_tree, opts} do
      {%{type: :layout, page_size: {width, height} = page_size, boxes: boxes} = layout_tree, opts}
      when is_number(width) and width > 0 and is_number(height) and height > 0 and
             is_list(boxes) and is_list(opts) ->
        margin = Map.get(layout_tree, :margins, Map.get(layout_tree, :margin, 0.0))

        with true <- Keyword.keyword?(opts),
             {:ok, margins} <- PageGeometry.normalize_margins(margin),
             true <- PageGeometry.valid_printable_area?(page_size, margins) do
          {:ok, %{layout_tree: layout_tree, page_size: page_size, boxes: boxes, margins: margins}}
        else
          _ -> invalid_pagination()
        end

      _ ->
        invalid_pagination()
    end
  end

  @doc false
  @spec prepare_furniture(term(), term(), term()) ::
          {:ok,
           %{
             pages: [map()],
             page_size: {number(), number()},
             margins: map(),
             furniture: map() | nil,
             options: keyword()
           }}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_furniture(pages, layout_tree, opts) do
    case {pages, layout_tree, opts} do
      {pages, %{page_size: {width, height} = page_size} = layout_tree, opts}
      when is_list(pages) and is_number(width) and width > 0 and is_number(height) and
             height > 0 and is_list(opts) ->
        case Keyword.keyword?(opts) do
          true ->
            margin = Map.get(layout_tree, :margins, Map.get(layout_tree, :margin, :missing))

            with {:ok, margins} <- PageGeometry.normalize_margins(margin),
                 true <- PageGeometry.valid_printable_area?(page_size, margins),
                 true <- valid_pages?(pages, page_size),
                 {:ok, furniture} <-
                   normalize_furniture_option(Keyword.get(opts, :page_furniture)) do
              {:ok,
               %{
                 pages: pages,
                 page_size: page_size,
                 margins: margins,
                 furniture: furniture,
                 options: opts
               }}
            else
              false -> invalid_furniture_layout()
              {:error, {_reason, _diagnostic}} = error -> error
              {:error, :invalid_margin} -> invalid_furniture_layout()
            end

          false ->
            invalid_furniture_options("page furniture options require a keyword list")
        end

      {_pages, _layout_tree, opts} when is_list(opts) ->
        case Keyword.keyword?(opts) do
          true -> invalid_furniture_layout()
          false -> invalid_furniture_options("page furniture options require a keyword list")
        end

      _ ->
        invalid_furniture_layout()
    end
  end

  @doc false
  @spec prepare_font_fallback(term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_font_fallback(styled_tree) do
    case styled_tree do
      %{type: :document, children: children} = document when is_list(children) ->
        with {:ok, prepared_children} <- prepare_styled_nodes(children) do
          {:ok, %{document | children: prepared_children}}
        end

      _ ->
        invalid_styled_document("font fallback requires a styled document tree")
    end
  end

  @doc false
  @spec prepare_font_options(term()) :: {:ok, keyword()} | :error
  def prepare_font_options(opts) do
    case Keyword.keyword?(opts) do
      true ->
        case prepare_font_configs(Keyword.get(opts, :fonts, [])) do
          {:ok, fonts} -> {:ok, Keyword.put(opts, :fonts, fonts)}
          :error -> :error
        end

      false ->
        :error
    end
  end

  @doc false
  @spec prepare_font_configs(term()) :: {:ok, [map()]} | :error
  def prepare_font_configs(fonts) do
    case is_list(fonts) do
      true ->
        Enum.reduce_while(fonts, {:ok, []}, fn font, {:ok, prepared} ->
          case normalize_font_config(font) do
            {:ok, normalized} -> {:cont, {:ok, prepared ++ [normalized]}}
            :error -> {:halt, :error}
          end
        end)

      false ->
        :error
    end
  end

  defp validate_html(html) do
    case html do
      html when is_binary(html) ->
        case String.valid?(html) do
          true -> {:ok, html}
          false -> Diagnostics.error(:html, :invalid_encoding, "HTML input must be valid UTF-8")
        end

      _ ->
        Diagnostics.error(:html, :invalid_html, "HTML input must be a string")
    end
  end

  defp validate_render_options(opts) do
    case Keyword.keyword?(opts) do
      true ->
        unknown =
          opts
          |> Keyword.keys()
          |> Enum.reject(&(&1 in @render_option_keys))
          |> Enum.uniq()
          |> Enum.sort()

        case unknown do
          [] ->
            validate_style_options(opts)

          unknown ->
            Diagnostics.error(
              :options,
              :invalid_options,
              "render options contain unsupported keys: #{inspect(unknown)}"
            )
        end

      false ->
        Diagnostics.error(:options, :invalid_options, "render options must be a keyword list")
    end
  end

  defp validate_style_options(opts) do
    case Keyword.keyword?(opts) do
      true ->
        with :ok <- validate_stylesheets(Keyword.get(opts, :stylesheets, [])),
             :ok <- validate_base_url(Keyword.get(opts, :base_url)),
             :ok <- validate_default_font(Keyword.get(opts, :default_font, "Helvetica")),
             :ok <- validate_fonts(Keyword.get(opts, :fonts, [])) do
          {:ok, opts}
        end

      false ->
        Diagnostics.error(:style, :invalid_document, "style options must be a keyword list")
    end
  end

  defp validate_stylesheets(stylesheets) do
    valid? =
      is_list(stylesheets) and
        Enum.all?(stylesheets, fn stylesheet ->
          case stylesheet do
            {:css, css} when is_binary(css) -> true
            {:file, path} when is_binary(path) -> true
            _ -> false
          end
        end)

    case valid? do
      true ->
        :ok

      false ->
        Diagnostics.error(
          :options,
          :invalid_options,
          "stylesheets option must be a list of {:css, css} or {:file, path} tuples"
        )
    end
  end

  defp validate_base_url(base_url) do
    case is_nil(base_url) or is_binary(base_url) do
      true -> :ok
      false -> invalid_style_configuration()
    end
  end

  defp validate_default_font(default_font) do
    valid? =
      case default_font do
        font when is_binary(font) -> String.trim(font) != ""
        fonts when is_list(fonts) -> fonts != [] and Enum.all?(fonts, &is_binary/1)
        _ -> false
      end

    case valid? do
      true -> :ok
      false -> invalid_style_configuration()
    end
  end

  defp validate_fonts(fonts) do
    case prepare_font_configs(fonts) do
      {:ok, _fonts} -> :ok
      :error -> invalid_style_configuration()
    end
  end

  defp normalize_font_config(font) do
    case font do
      {family, path} ->
        normalize_font_config(%{family: family, path: path})

      font when is_list(font) ->
        case Keyword.keyword?(font) do
          true -> normalize_font_config(Map.new(font))
          false -> :error
        end

      font when is_map(font) ->
        family = Map.get(font, :family) || Map.get(font, "family")
        path = Map.get(font, :path) || Map.get(font, "path")
        weight = Map.get(font, :weight) || Map.get(font, "weight") || 400
        style = Map.get(font, :style) || Map.get(font, "style") || :normal

        with true <- is_binary(family) and String.trim(family) != "",
             {:ok, paths} <- normalize_font_paths(path),
             {:ok, weight} <- normalize_font_weight(weight),
             {:ok, style} <- normalize_font_style(style) do
          {:ok, %{family: String.trim(family), path: paths, weight: weight, style: style}}
        else
          _ -> :error
        end

      _ ->
        :error
    end
  end

  defp normalize_font_paths(path) do
    case path do
      path when is_binary(path) ->
        case String.trim(path) do
          "" -> :error
          path -> {:ok, [path]}
        end

      paths when is_list(paths) ->
        case paths != [] and Enum.all?(paths, &(is_binary(&1) and String.trim(&1) != "")) do
          true -> {:ok, Enum.map(paths, &String.trim/1)}
          false -> :error
        end

      _ ->
        :error
    end
  end

  defp normalize_font_weight(weight) do
    case weight do
      weight when is_number(weight) and weight >= 100 and weight <= 900 ->
        {:ok, weight}

      "normal" ->
        {:ok, 400}

      "bold" ->
        {:ok, 700}

      weight when is_binary(weight) ->
        case Integer.parse(String.trim(weight)) do
          {weight, ""} when weight >= 100 and weight <= 900 -> {:ok, weight}
          _ -> :error
        end

      _ ->
        :error
    end
  end

  defp normalize_font_style(style) do
    case style do
      :normal -> {:ok, :normal}
      :italic -> {:ok, :italic}
      "normal" -> {:ok, :normal}
      "italic" -> {:ok, :italic}
      _ -> :error
    end
  end

  defp invalid_style_configuration do
    Diagnostics.error(
      :style,
      :invalid_document,
      "document style validation failed; check fonts, images, attributes, and supported CSS values"
    )
  end

  defp validate_dom(node) do
    case node do
      %{type: :document, children: children} when is_list(children) ->
        validate_dom_nodes(children)

      _ ->
        :invalid_document
    end
  end

  defp validate_dom_nodes(nodes) do
    case Enum.all?(nodes, fn node ->
           case node do
             %{type: :text, text: text} ->
               is_binary(text)

             %{type: :element, tag: tag, attributes: attributes, children: children} ->
               is_binary(tag) and is_map(attributes) and is_list(children) and
                 validate_dom_nodes(children) == :ok

             _ ->
               false
           end
         end) do
      true -> :ok
      false -> :invalid_document
    end
  end

  defp invalid_pagination do
    Diagnostics.error(
      :pagination,
      :invalid_layout,
      "pagination requires a positive page size and margins that leave a positive printable area"
    )
  end

  defp valid_pages?(pages, page_size) do
    Enum.all?(pages, fn page ->
      case page do
        %{size: ^page_size, boxes: boxes} when is_list(boxes) -> true
        _ -> false
      end
    end)
  end

  defp normalize_furniture_option(furniture) do
    case furniture do
      value when value in [nil, false] ->
        {:ok, nil}

      furniture when is_list(furniture) ->
        case Keyword.keyword?(furniture) do
          true -> normalize_furniture(Map.new(furniture))
          false -> invalid_furniture_options("page_furniture must be a keyword list or map")
        end

      furniture when is_map(furniture) ->
        normalize_furniture(furniture)

      _ ->
        invalid_furniture_options("page_furniture must be a keyword list or map")
    end
  end

  defp normalize_furniture(furniture) do
    case Enum.reject(Map.keys(furniture), &(&1 in @furniture_keys)) do
      [] ->
        Enum.reduce_while(@furniture_keys, {:ok, %{}}, fn position, {:ok, normalized} ->
          case normalize_variants(Map.get(furniture, position), position) do
            {:ok, variants} -> {:cont, {:ok, Map.put(normalized, position, variants)}}
            {:error, {_reason, _diagnostic}} = error -> {:halt, error}
          end
        end)

      unknown ->
        invalid_furniture_options(
          "page_furniture contains unsupported keys: #{inspect(Enum.sort(unknown))}"
        )
    end
  end

  defp normalize_variants(value, position) do
    case value do
      value when value in [nil, false] ->
        {:ok, %{}}

      template when is_binary(template) ->
        {:ok, %{default: template}}

      variants when is_list(variants) ->
        case Keyword.keyword?(variants) do
          true -> normalize_variant_map(Map.new(variants), position)
          false -> invalid_variant_options(position)
        end

      variants when is_map(variants) ->
        normalize_variant_map(variants, position)

      _ ->
        invalid_variant_options(position)
    end
  end

  defp normalize_variant_map(variants, position) do
    unknown = Enum.reject(Map.keys(variants), &(&1 in @variant_keys))

    cond do
      unknown != [] ->
        invalid_furniture_options(
          "#{position} page furniture contains unsupported keys: #{inspect(Enum.sort(unknown))}"
        )

      Enum.all?(variants, fn {_variant, template} ->
        is_binary(template) or template in [nil, false]
      end) ->
        {:ok, variants}

      true ->
        invalid_variant_options(position)
    end
  end

  defp invalid_variant_options(position) do
    invalid_furniture_options(
      "#{position} page furniture must be HTML or default/first/odd/even HTML variants"
    )
  end

  defp invalid_furniture_options(message) do
    Diagnostics.error(:options, :invalid_options, message)
  end

  defp invalid_furniture_layout do
    Diagnostics.error(
      :layout,
      :invalid_layout,
      "page furniture requires valid pages and matching page geometry"
    )
  end

  defp prepare_styled_nodes(nodes) do
    Enum.reduce_while(nodes, {:ok, []}, fn node, {:ok, prepared} ->
      case prepare_styled_node(node) do
        {:ok, prepared_node} -> {:cont, {:ok, prepared ++ [prepared_node]}}
        {:error, {_reason, _diagnostic}} = error -> {:halt, error}
      end
    end)
  end

  defp prepare_styled_node(node) do
    case node do
      %{type: :text, text: text, style: style} when is_binary(text) and is_map(style) ->
        case String.valid?(text) do
          true ->
            with {:ok, candidates} <- font_candidates(style),
                 :ok <- validate_supported_graphemes(text, candidates) do
              {:ok, Map.put(node, :_font_candidates, candidates)}
            end

          false ->
            Diagnostics.error(:font, :invalid_encoding, "styled text must be valid UTF-8")
        end

      %{type: :element, children: children} = element when is_list(children) ->
        with {:ok, prepared_children} <- prepare_styled_nodes(children) do
          {:ok, %{element | children: prepared_children}}
        end

      _ ->
        invalid_styled_document("font fallback encountered an invalid styled node")
    end
  end

  defp font_candidates(style) do
    case style do
      %{
        _font_registry: registry,
        font_face: selected,
        font_families: families,
        font_weight: weight,
        font_style: font_style
      }
      when is_map(registry) and is_map(selected) and is_list(families) and is_number(weight) and
             font_style in [:normal, :italic] ->
        requested =
          Enum.flat_map(families, fn family ->
            case Font.resolve([family], weight, font_style, registry) do
              {:ok, _families, font_face} -> [font_face]
              :error -> []
            end
          end)

        candidates =
          [selected | requested ++ Font.fallback_faces(registry, weight, font_style)]
          |> Enum.uniq_by(&Font.pdf_name/1)

        {:ok, candidates}

      _ ->
        invalid_styled_document("styled text is missing its resolved font registry")
    end
  end

  defp validate_supported_graphemes(text, candidates) do
    text
    |> String.replace("\r\n", "\n")
    |> String.replace("\r", "\n")
    |> String.graphemes()
    |> Enum.reduce_while(:ok, fn grapheme, :ok ->
      layout_whitespace? =
        grapheme |> String.to_charlist() |> Enum.all?(&(&1 in [9, 10, 13]))

      supported? = layout_whitespace? or Enum.any?(candidates, &Font.supports_text?(&1, grapheme))

      case supported? do
        true -> {:cont, :ok}
        false -> {:halt, unsupported_glyph(grapheme)}
      end
    end)
  end

  defp invalid_styled_document(message) do
    Diagnostics.error(:font, :invalid_document, message)
  end

  defp unsupported_glyph(grapheme) do
    codepoints =
      grapheme
      |> String.to_charlist()
      |> Enum.map_join(
        " ",
        &("U+" <> (&1 |> Integer.to_string(16) |> String.upcase() |> String.pad_leading(4, "0")))
      )

    Diagnostics.error(
      :font,
      :unsupported_glyph,
      "no requested, configured, or bundled font contains every glyph in #{inspect(grapheme)} (#{codepoints})",
      source: grapheme
    )
  end
end
