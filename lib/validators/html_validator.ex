defmodule NativeElixirPdfUtilities.Validators.HtmlValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.Font

  @render_option_keys [
    :page_size,
    :margin,
    :base_url,
    :stylesheets,
    :default_font,
    :fonts,
    :metadata,
    :page_furniture,
    :unsupported_glyphs
  ]
  @variant_keys [:default, :first, :odd, :even]

  @doc false
  @spec validate_page_size(term()) :: :ok | {:error, :invalid_page_size}
  def validate_page_size(page_size) do
    case page_size do
      {width, height} when is_number(width) and is_number(height) and width > 0 and height > 0 ->
        :ok

      _ ->
        {:error, :invalid_page_size}
    end
  end

  @doc false
  @spec validate_margins(term()) :: :ok | {:error, :invalid_margin}
  def validate_margins(margins) do
    case margins do
      %{top: top, right: right, bottom: bottom, left: left}
      when map_size(margins) == 4 and is_number(top) and top >= 0 and is_number(right) and
             right >= 0 and is_number(bottom) and bottom >= 0 and is_number(left) and left >= 0 ->
        :ok

      _ ->
        {:error, :invalid_margin}
    end
  end

  @doc false
  @spec validate_printable_area(term(), term()) :: :ok | {:error, :invalid_margin}
  def validate_printable_area(page_size, margins) do
    case {page_size, margins} do
      {{page_width, page_height}, %{top: top, right: right, bottom: bottom, left: left}}
      when is_number(page_width) and is_number(page_height) and is_number(top) and
             is_number(right) and is_number(bottom) and is_number(left) ->
        case page_width > 0 and page_height > 0 and
               Enum.all?([top, right, bottom, left], &(&1 >= 0)) and
               left + right < page_width and top + bottom < page_height do
          true -> :ok
          false -> {:error, :invalid_margin}
        end

      _ ->
        {:error, :invalid_margin}
    end
  end

  @doc false
  @spec validate_furniture_fit(:header | :footer, number(), number()) ::
          :ok | {:error, {:invalid_layout, Diagnostics.diagnostic()}}
  def validate_furniture_fit(position, height, available_margin) do
    case {position, height, available_margin} do
      {position, height, available_margin}
      when position in [:header, :footer] and is_number(height) and height >= 0 and
             is_number(available_margin) and available_margin >= 0 ->
        case height <= available_margin + 0.0001 do
          true ->
            :ok

          false ->
            Diagnostics.error(
              :layout,
              :invalid_layout,
              "#{position} page furniture height #{format_number(height)}pt exceeds the #{format_number(available_margin)}pt page margin"
            )
        end

      _ ->
        Diagnostics.error(
          :layout,
          :invalid_layout,
          "page furniture fit requires a header or footer and nonnegative numeric dimensions"
        )
    end
  end

  @doc false
  @spec validate_render_request(term(), term(), term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_render_request(html, opts, font_options_result) do
    with {:ok, _html} <- validate_html_source(html),
         :ok <- validate_render_options(opts, font_options_result) do
      :ok
    end
  end

  @doc false
  @spec validate_html_source(term()) ::
          {:ok, binary()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_html_source(html) do
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

  @doc false
  @spec validate_css_source(term(), :stylesheet | :declarations) ::
          {:ok, binary()} | {:error, {:invalid_css, Diagnostics.diagnostic()}}
  def validate_css_source(css, kind \\ :stylesheet) do
    case {css, kind} do
      {css, kind} when is_binary(css) and kind in [:stylesheet, :declarations] ->
        case String.valid?(css) do
          true -> {:ok, css}
          false -> Diagnostics.error(:css, :invalid_css, "CSS input must be valid UTF-8")
        end

      {_css, :declarations} ->
        Diagnostics.error(:css, :invalid_css, "CSS declaration input must be a string")

      _ ->
        Diagnostics.error(:css, :invalid_css, "CSS input must be a string")
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
  @spec valid_link_url?(term()) :: boolean()
  def valid_link_url?(href) do
    case href do
      href when is_binary(href) ->
        Regex.match?(~r/^(https?:\/\/[^\s<>]+|mailto:[^\s<>@]+@[^\s<>@]+)$/iu, href)

      _ ->
        false
    end
  end

  @doc false
  @spec validate_style_input(term(), term(), term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_style_input(dom, opts, font_options_result) do
    with :ok <- validate_dom(dom),
         :ok <- validate_style_options(opts, font_options_result) do
      :ok
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
  @spec validate_layout_input(term(), term(), term(), term()) ::
          :ok | {:error, :invalid_layout | :invalid_margin | :invalid_page_size}
  def validate_layout_input(styled_tree, opts, page_size_result, margins_result) do
    case {styled_tree, opts} do
      {%{type: :document, children: children}, opts}
      when is_list(children) and is_list(opts) ->
        case Keyword.keyword?(opts) do
          true ->
            with :ok <- validate_layout_nodes(children),
                 {:ok, page_size} <- page_size_result,
                 :ok <- validate_page_size(page_size),
                 {:ok, margins} <- margins_result,
                 :ok <- validate_margins(margins),
                 :ok <- validate_printable_area(page_size, margins) do
              :ok
            else
              :invalid_layout -> {:error, :invalid_layout}
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
  @spec validate_pagination_input(term(), term(), term()) ::
          :ok | {:error, {:invalid_layout, Diagnostics.diagnostic()}}
  def validate_pagination_input(layout_tree, opts, margins_result) do
    case {layout_tree, opts} do
      {%{type: :layout, page_size: {width, height} = page_size, boxes: boxes}, opts}
      when is_number(width) and width > 0 and is_number(height) and height > 0 and
             is_list(boxes) and is_list(opts) ->
        with true <- Keyword.keyword?(opts),
             {:ok, margins} <- margins_result,
             :ok <- validate_page_size(page_size),
             :ok <- validate_margins(margins),
             :ok <- validate_printable_area(page_size, margins) do
          :ok
        else
          _ -> invalid_pagination()
        end

      _ ->
        invalid_pagination()
    end
  end

  @doc false
  @spec validate_furniture_input(term(), term(), term(), term(), term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_furniture_input(pages, layout_tree, opts, margins_result, furniture_result) do
    case {pages, layout_tree, opts} do
      {pages, %{page_size: {width, height} = page_size}, opts}
      when is_list(pages) and is_number(width) and width > 0 and is_number(height) and
             height > 0 and is_list(opts) ->
        case Keyword.keyword?(opts) do
          true ->
            with {:ok, margins} <- margins_result,
                 :ok <- validate_page_size(page_size),
                 :ok <- validate_margins(margins),
                 :ok <- validate_printable_area(page_size, margins),
                 true <- valid_pages?(pages, page_size),
                 :ok <- validate_furniture_result(furniture_result) do
              :ok
            else
              false ->
                invalid_furniture_layout()

              {:error, {_reason, _diagnostic}} = error ->
                error

              {:error, reason} when reason in [:invalid_margin, :invalid_page_size] ->
                invalid_furniture_layout()
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
  @spec validate_font_fallback_input(term(), term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_font_fallback_input(styled_tree, unsupported_glyphs) do
    with :ok <- validate_unsupported_glyphs(unsupported_glyphs) do
      case styled_tree do
        %{type: :document, children: children} when is_list(children) ->
          validate_font_input_nodes(children)

        _ ->
          invalid_styled_document("font fallback requires a styled document tree")
      end
    end
  end

  @doc false
  @spec validate_font_coverage(term(), term(), String.t()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_font_coverage(styled_tree, unsupported_glyphs, replacement_character) do
    case styled_tree do
      %{type: :document, children: children}
      when is_list(children) and unsupported_glyphs in [:replace, :error] and
             is_binary(replacement_character) ->
        validate_font_coverage_nodes(children, unsupported_glyphs, replacement_character)

      _ ->
        invalid_styled_document("font fallback requires prepared font candidates")
    end
  end

  @doc false
  @spec validate_font_configs(term()) :: :ok | :error
  def validate_font_configs(fonts) do
    valid? =
      is_list(fonts) and
        Enum.all?(fonts, fn font ->
          case font do
            %{family: family, path: paths, weight: weight, style: style}
            when map_size(font) == 4 and is_binary(family) and is_list(paths) and
                   is_number(weight) and weight >= 100 and weight <= 900 and
                   style in [:normal, :italic] ->
              String.trim(family) != "" and paths != [] and
                Enum.all?(paths, &(is_binary(&1) and String.trim(&1) != ""))

            _ ->
              false
          end
        end)

    case valid? do
      true -> :ok
      false -> :error
    end
  end

  defp validate_render_options(opts, font_options_result) do
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
            validate_style_options(opts, font_options_result)

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

  defp validate_style_options(opts, font_options_result) do
    case Keyword.keyword?(opts) do
      true ->
        with :ok <- validate_stylesheets(Keyword.get(opts, :stylesheets, [])),
             :ok <- validate_base_url(Keyword.get(opts, :base_url)),
             :ok <- validate_default_font(Keyword.get(opts, :default_font, "Helvetica")),
             :ok <- validate_unsupported_glyphs(Keyword.get(opts, :unsupported_glyphs, :replace)),
             :ok <- validate_font_options_result(font_options_result) do
          :ok
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

  defp validate_unsupported_glyphs(unsupported_glyphs) do
    case unsupported_glyphs do
      unsupported_glyphs when unsupported_glyphs in [:replace, :error] ->
        :ok

      _ ->
        Diagnostics.error(
          :options,
          :invalid_options,
          "unsupported_glyphs must be :replace or :error"
        )
    end
  end

  defp validate_font_options_result(font_options_result) do
    with {:ok, normalized_opts} <- font_options_result,
         fonts <- Keyword.fetch!(normalized_opts, :fonts),
         :ok <- validate_font_configs(fonts) do
      :ok
    else
      _ -> invalid_style_configuration()
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

  defp validate_layout_nodes(nodes) do
    case Enum.all?(nodes, fn node ->
           case node do
             %{type: :text, text: text, style: style} ->
               is_binary(text) and String.valid?(text) and is_map(style)

             %{type: :element, style: %{display: display}}
             when display in [:image, :none] ->
               true

             %{type: :element, style: style, children: children} ->
               is_map(style) and is_list(children) and validate_layout_nodes(children) == :ok

             _ ->
               false
           end
         end) do
      true -> :ok
      false -> :invalid_layout
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

  defp validate_furniture_result(furniture_result) do
    case furniture_result do
      {:ok, normalized} ->
        validate_furniture_option(normalized)

      {:error, :invalid_furniture_container} ->
        invalid_furniture_options("page_furniture must be a keyword list or map")

      {:error, {:unknown_furniture_keys, unknown}} ->
        invalid_furniture_options("page_furniture contains unsupported keys: #{inspect(unknown)}")

      {:error, {:unknown_variant_keys, position, unknown}} ->
        invalid_furniture_options(
          "#{position} page furniture contains unsupported keys: #{inspect(unknown)}"
        )

      {:error, {:invalid_variants, position}} ->
        invalid_furniture_options(
          "#{position} page furniture must be HTML or default/first/odd/even HTML variants"
        )
    end
  end

  @doc false
  @spec validate_furniture_option(term()) ::
          :ok | {:error, {:invalid_options, Diagnostics.diagnostic()}}
  def validate_furniture_option(furniture) do
    valid? =
      case furniture do
        nil ->
          true

        %{header: header, footer: footer} when map_size(furniture) == 2 ->
          Enum.all?([header, footer], fn variants ->
            is_map(variants) and
              Enum.all?(variants, fn {variant, template} ->
                variant in @variant_keys and (is_binary(template) or template in [nil, false])
              end)
          end)

        _ ->
          false
      end

    case valid? do
      true -> :ok
      false -> invalid_furniture_options("page_furniture normalization produced invalid data")
    end
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

  defp validate_font_input_nodes(nodes) do
    Enum.reduce_while(nodes, :ok, fn node, :ok ->
      case node do
        %{
          type: :text,
          text: text,
          style: %{
            _font_registry: registry,
            font_face: selected,
            font_families: families,
            font_weight: weight,
            font_style: font_style
          }
        }
        when is_binary(text) and is_map(registry) and is_map(selected) and is_list(families) and
               is_number(weight) and font_style in [:normal, :italic] ->
          case String.valid?(text) do
            true ->
              {:cont, :ok}

            false ->
              {:halt,
               Diagnostics.error(:font, :invalid_encoding, "styled text must be valid UTF-8")}
          end

        %{type: :element, children: children} when is_list(children) ->
          case validate_font_input_nodes(children) do
            :ok -> {:cont, :ok}
            {:error, {_reason, _diagnostic}} = error -> {:halt, error}
          end

        %{type: :text} ->
          {:halt, invalid_styled_document("styled text is missing its resolved font registry")}

        _ ->
          {:halt, invalid_styled_document("font fallback encountered an invalid styled node")}
      end
    end)
  end

  defp validate_font_coverage_nodes(nodes, unsupported_glyphs, replacement_character) do
    Enum.reduce_while(nodes, :ok, fn node, :ok ->
      case node do
        %{type: :text, _font_candidates: candidates, _font_graphemes: graphemes}
        when is_list(candidates) and candidates != [] and is_list(graphemes) ->
          case validate_supported_graphemes(
                 graphemes,
                 candidates,
                 unsupported_glyphs,
                 replacement_character
               ) do
            :ok -> {:cont, :ok}
            {:error, {_reason, _diagnostic}} = error -> {:halt, error}
          end

        %{type: :element, children: children} when is_list(children) ->
          case validate_font_coverage_nodes(
                 children,
                 unsupported_glyphs,
                 replacement_character
               ) do
            :ok -> {:cont, :ok}
            {:error, {_reason, _diagnostic}} = error -> {:halt, error}
          end

        _ ->
          {:halt, invalid_styled_document("font fallback requires prepared font candidates")}
      end
    end)
  end

  defp validate_supported_graphemes(
         graphemes,
         candidates,
         unsupported_glyphs,
         replacement_character
       ) do
    Enum.reduce_while(graphemes, :ok, fn grapheme, :ok ->
      case grapheme do
        %{text: text, layout_whitespace?: layout_whitespace?}
        when is_binary(text) and is_boolean(layout_whitespace?) ->
          supported? = layout_whitespace? or Enum.any?(candidates, &Font.supports_text?(&1, text))

          replacement_supported? =
            unsupported_glyphs == :replace and
              Enum.any?(candidates, &Font.supports_text?(&1, replacement_character))

          case supported? or replacement_supported? do
            true -> {:cont, :ok}
            false -> {:halt, unsupported_glyph(text)}
          end

        _ ->
          {:halt, invalid_styled_document("font fallback requires prepared graphemes")}
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

  defp format_number(number) do
    number
    |> Kernel.*(100)
    |> round()
    |> Kernel./(100)
    |> to_string()
  end
end
