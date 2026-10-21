defmodule NativeElixirPdfUtilities.Validators.HtmlValidator do
  @moduledoc """
  Native HTML validation, stage input contracts and shared resource accounting.

  Rendering stages use this internal validator as the source of truth for
  admitted structures and budgets. Applications should use `NativeElixirPdfUtilities.HtmlToPdf`.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlEntities
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf.CounterStyle
  alias NativeElixirPdfUtilities.HtmlToPdf.RenderCache
  alias NativeElixirPdfUtilities.Validators.MimeValidator
  alias NativeElixirPdfUtilities.Validators.FontValidator
  alias NativeElixirPdfUtilities.Validators.LanguageValidator
  alias NativeElixirPdfUtilities.Validators.HtmlMetadataValidator
  alias NativeElixirPdfUtilities.Validators.HtmlUrlValidator
  alias NativeElixirPdfUtilities.Limits

  @structural_tags ~w(html head body frameset noframes base basefont bgsound style title meta script template)
  @flow_container_tags ~w(form article aside div footer header main nav section address blockquote figure figcaption hgroup search center li dt dd details fieldset dialog)
  @block_tags @flow_container_tags ++
                ~w(p h1 h2 h3 h4 h5 h6 ul ol menu dir dl dt pre listing xmp plaintext table img picture hr summary legend)
  @text_inline_tags ~w(map area basefont bgsound iframe object embed label output datalist noscript noembed canvas slot selectedcontent u s strong b em i span a br wbr nobr abbr acronym q font blink keygen cite dfn data time code samp kbd tt var big small sub sup mark ins del strike ruby rb rt rp rtc)
  @form_control_tags ~w(input select textarea button)
  @text_input_types ~w(text search tel url email password)
  @button_input_types ~w(button reset submit)
  @input_types @text_input_types ++
                 @button_input_types ++
                 ~w(checkbox radio hidden number range color image file date month week time datetime-local)
  # Fixed HTML enumerated keywords, including recognized states still unsupported.
  @input_type_keywords Map.new(
                         ~w(hidden text search tel url email password date month week time datetime-local number range color checkbox radio file submit image reset button),
                         &{&1, &1}
                       )
  # Longer values cannot name a fixed keyword and select Text without folding.
  # This is vocabulary recognition, not a configurable resource ceiling.
  @input_type_keyword_bytes Enum.max(Enum.map(Map.keys(@input_type_keywords), &byte_size/1))
  # This fixed list is defined by HTML's selector matching rules, not a resource limit.
  @case_insensitive_html_attributes ~w(accept accept-charset align alink axis bgcolor charset checked clear codetype color compact declare defer dir direction disabled enctype face frame hreflang http-equiv lang language link media method multiple nohref noresize noshade nowrap readonly rel rev rules scope scrolling selected shape target text type valign valuetype vlink)
  @inline_tags @text_inline_tags ++ @form_control_tags ++ ~w(progress meter)
  @list_tags ~w(ul ol menu dir)
  @table_content_tags ~w(caption th td)
  @void_tags ~w(area base basefont bgsound keygen meta br wbr img source col input hr embed frame)
  @supported_tags Map.new(
                    @structural_tags ++
                      @block_tags ++
                      @inline_tags ++
                      ~w(frame source caption colgroup col thead tbody tfoot tr th td option optgroup),
                    &{&1, true}
                  )
  # HTML reserves these eight names; they are not configurable resource limits.
  @reserved_custom_names ~w(annotation-xml color-profile font-face font-face-src font-face-uri font-face-format font-face-name missing-glyph)

  @optional_end_tags ~w(html head body p li dt dd option optgroup caption colgroup thead tbody tfoot tr td th rb rt rp rtc)
  @paragraph_following_tags ~w(address article aside blockquote details dialog div dl fieldset figcaption figure footer form h1 h2 h3 h4 h5 h6 header hgroup hr main menu nav ol p pre xmp plaintext search section table ul)

  @paragraph_following_starts Enum.map(@paragraph_following_tags, &{:start, &1})
  @head_starts Enum.map(
                 ~w(base basefont bgsound style meta title script template noscript noframes),
                 &{:start, &1}
               )
  @table_starts Enum.map(~w(colgroup thead tbody tfoot tr), &{:start, &1})

  @render_option_keys [
    :forms,
    :print_viewport,
    :image_selection_density,
    :page_size,
    :margin,
    :base_url,
    :assets,
    :asset_resolver,
    :stylesheets,
    :print_viewport,
    :image_selection_density,
    :default_font,
    :fonts,
    :system_font_discovery,
    :metadata,
    :outlines,
    :page_furniture,
    :unsupported_glyphs,
    :compress_streams,
    :subset_fonts
  ]
  @variant_keys [:default, :first, :odd, :even]
  @type image_budget :: :atomics.atomics_ref()

  @preserved_global_attributes ~w(accesskey autocapitalize autocorrect autofocus draggable enterkeyhint inputmode nonce spellcheck tabindex writingsuggestions)

  @script_attributes ~w(type src language charset async defer nomodule crossorigin integrity referrerpolicy blocking fetchpriority)
  @script_data_boundaries ~r/<!--|-->|<\/?[sS][cC][rR][iI][pP][tT](?=[ \t\n\f\r\/>])/

  # Six hexadecimal digits are a fixed CSS escape grammar rule, not a resource ceiling.
  @css_escape_pattern ~S"\\(?:[0-9a-fA-F]{1,6}(?:\r\n|[ \t\n\r\f])?|[^\n\r\f0-9a-fA-F])"
  @css_identifier_pattern "(?:--|-?(?:[a-zA-Z_\\x{80}-\\x{10FFFF}]|#{@css_escape_pattern}))(?:[a-zA-Z0-9_\\x{80}-\\x{10FFFF}-]|#{@css_escape_pattern})*"
  @css_identifier_regex Regex.compile!(
                          "\\A(?<name>#{@css_identifier_pattern})(?<rest>.*)\\z",
                          "su"
                        )
  @css_escape_regex ~r/\\(?:([0-9a-fA-F]{1,6})(?:\r\n|[ \t\n\r\f])?|([^\n\r\f0-9a-fA-F]))/u
  @css_syntax_unit_regex Regex.compile!(
                           "#{@css_escape_pattern}|\\\\(?:\\r\\n|[\\n\\r\\f])|.",
                           "su"
                         )
  @css_attribute_selector_regex Regex.compile!(
                                  "\\A\\[[ \\t\\r\\n\\f]*(?<name>#{@css_identifier_pattern})(?:[ \\t\\r\\n\\f]*(?<operator>[~|^$*]?=)[ \\t\\r\\n\\f]*(?<value>\"(?:#{@css_escape_pattern}|\\\\(?:\\r\\n|[\\n\\r\\f])|[^\"\\\\\\r\\n\\f])*\"|'(?:#{@css_escape_pattern}|\\\\(?:\\r\\n|[\\n\\r\\f])|[^'\\\\\\r\\n\\f])*'|#{@css_identifier_pattern})(?:[ \\t\\r\\n\\f]+(?<flag>#{@css_identifier_pattern}))?)?[ \\t\\r\\n\\f]*\\](?<rest>.*)\\z",
                                  "su"
                                )

  @css_simple_pseudo_regex Regex.compile!(
                             "\\A:(?<keyword>#{@css_identifier_pattern})(?<rest>.*)\\z",
                             "su"
                           )
  @css_nth_pseudo_regex Regex.compile!(
                          "\\A:(?<keyword>#{@css_identifier_pattern})\\((?<expression>[^()]*)\\)(?<rest>.*)\\z",
                          "su"
                        )
  @css_string_pattern "(?:\"(?:#{@css_escape_pattern}|\\\\(?:\\r\\n|[\\n\\r\\f])|[^\"\\\\\\r\\n\\f])*\"|'(?:#{@css_escape_pattern}|\\\\(?:\\r\\n|[\\n\\r\\f])|[^'\\\\\\r\\n\\f])*')"
  @css_component_unit_regex Regex.compile!(
                              "#{@css_string_pattern}|<=|>=|#{Regex.source(@css_syntax_unit_regex)}",
                              "su"
                            )
  @css_language_range_regex Regex.compile!(
                              "\\A[ \\t\\r\\n\\f]*(?<value>#{@css_string_pattern}|#{@css_identifier_pattern})[ \\t\\r\\n\\f]*(?<rest>.*)\\z",
                              "su"
                            )

  @css_opaque_source "(?:#{@css_escape_pattern}|\"(?:\\\\(?:\\r\\n|[\\n\\r\\f]|.)|[^\"\\\\])*\"|'(?:\\\\(?:\\r\\n|[\\n\\r\\f]|.)|[^'\\\\])*')"
  @css_function_unit_regex Regex.compile!("#{@css_escape_pattern}|#{@css_string_pattern}|.", "su")

  @css_important_suffix_regex Regex.compile!(
                                "(?:#{@css_string_pattern}|#{@css_escape_pattern})(*SKIP)(*F)|!(?<keyword>[ \t\r\n\f]*#{@css_identifier_pattern})[ \t\r\n\f]*\\z",
                                "su"
                              )

  @doc false
  @spec normal_line_height(map()) :: number()
  def normal_line_height(style) do
    font_size = Map.fetch!(style, :font_size)

    %{
      type: :embedded,
      ascent: ascent,
      descent: descent,
      line_gap: line_gap,
      units_per_em: units_per_em
    } =
      Map.fetch!(style, :font_face)

    ascent_pixels = round(ascent / units_per_em * font_size / 0.75)
    descent_pixels = round(-descent / units_per_em * font_size / 0.75)
    line_gap_pixels = round(max(line_gap, 0) / units_per_em * font_size / 0.75)
    max((ascent_pixels + descent_pixels + line_gap_pixels) * 0.75, font_size)
  end

  @doc false
  @spec text_baseline_depth(map()) :: number()
  def text_baseline_depth(style) do
    font_size = Map.fetch!(style, :font_size)
    line_height = Map.fetch!(style, :line_height)

    case {Map.get(style, :_math_flow, false), Map.fetch!(style, :font_face)} do
      {true, _} ->
        0.0

      {false, %{ascent: ascent, descent: descent, units_per_em: units_per_em}} ->
        ascent_pixels = round(font_size * ascent / units_per_em / 0.75)
        descent_pixels = round(-font_size * descent / units_per_em / 0.75)
        leading_pixels = Float.floor((line_height / 0.75 - ascent_pixels - descent_pixels) / 2)
        (ascent_pixels + leading_pixels) * 0.75

      _ ->
        font_size
    end
  end

  @doc false
  @spec text_x_height(map()) :: number() | {:error, {atom(), Diagnostics.diagnostic()}}
  def text_x_height(style) do
    case Map.fetch!(style, :font_face) do
      %{type: :embedded, cmap: cmap, units_per_em: units} = face ->
        case Map.get(cmap, ?x, 0) do
          0 ->
            Map.fetch!(style, :font_size) * 0.5

          glyph ->
            case Font.glyph_bounds(face) do
              {:ok, bounds} ->
                {_left, _bottom, _right, top} = elem(bounds, glyph)
                top / units * Map.fetch!(style, :font_size)

              error ->
                error
            end
        end

      _ ->
        Map.fetch!(style, :font_size) * 0.5
    end
  end

  @doc false
  @spec css_opaque_pattern() :: String.t()
  def css_opaque_pattern do
    @css_opaque_source
  end

  @doc false
  @spec css_syntax_units(String.t()) :: [String.t()]
  def css_syntax_units(source) do
    if String.contains?(source, "\\") do
      @css_syntax_unit_regex |> Regex.scan(source) |> Enum.map(&hd/1)
    else
      String.codepoints(source)
    end
  end

  @doc "Returns CSS lexical units with quoted strings and escapes kept opaque for component parsing."
  @spec css_component_units(String.t()) :: [String.t()]
  def css_component_units(source) do
    if String.contains?(source, ["\"", "'", "\\", "<=", ">="]),
      do: @css_component_unit_regex |> Regex.scan(source) |> Enum.map(&hd/1),
      else: String.codepoints(source)
  end

  @doc false
  @spec trim_css_whitespace(String.t()) :: String.t()
  def trim_css_whitespace(source) do
    whitespace = [32, 9, 10, 13, 12]

    if source != "" and
         (:binary.first(source) in whitespace or :binary.last(source) in whitespace) do
      if String.contains?(source, "\\") do
        source
        |> css_syntax_units()
        |> Enum.drop_while(&(&1 in [" ", "\t", "\n", "\r", "\f"]))
        |> Enum.reverse()
        |> Enum.drop_while(&(&1 in [" ", "\t", "\n", "\r", "\f"]))
        |> Enum.reverse()
        |> IO.iodata_to_binary()
      else
        trim_ascii_whitespace(source)
      end
    else
      source
    end
  end

  @doc false
  @spec html_class_tokens(String.t()) :: [String.t()]
  def html_class_tokens(source) do
    String.split(source, ~r/[ \t\n\r\f]+/, trim: true)
  end

  @doc false
  @spec parse_css_identifier(String.t()) :: {:ok, String.t(), String.t()} | :error
  def parse_css_identifier(source) do
    case Regex.named_captures(@css_identifier_regex, source) do
      %{"name" => name, "rest" => rest} -> {:ok, decode_css_escapes(name), rest}
      nil -> :error
    end
  end

  @doc false
  @spec parse_css_pseudo_element(String.t()) :: {:ok, :before | :after | :picker_icon} | :error
  def parse_css_pseudo_element(source) do
    case source do
      <<"::", name::binary>> ->
        case parse_css_identifier(name) do
          {:ok, name, ""} ->
            case String.downcase(name, :ascii) do
              "before" -> {:ok, :before}
              "after" -> {:ok, :after}
              "picker-icon" -> {:ok, :picker_icon}
              _ -> :error
            end

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  defp decode_css_escapes(source) do
    if String.contains?(source, "\\") do
      Regex.replace(@css_escape_regex, source, fn _escape, hexadecimal, literal ->
        case hexadecimal do
          "" ->
            literal

          hexadecimal ->
            codepoint = String.to_integer(hexadecimal, 16)

            if codepoint == 0 or codepoint in 0xD800..0xDFFF or codepoint > 0x10FFFF,
              do: "\uFFFD",
              else: <<codepoint::utf8>>
        end
      end)
    else
      source
    end
  end

  @doc false
  @spec css_string_pattern() :: String.t()
  def css_string_pattern, do: @css_string_pattern

  @doc "Decodes the contents of an already validated CSS string token."
  @spec decode_css_string(String.t()) :: String.t()
  def decode_css_string(token) do
    token
    |> String.slice(1, String.length(token) - 2)
    |> String.replace(~r/\\(?:\r\n|[\n\r\f])/u, "")
    |> decode_css_escapes()
  end

  @doc false
  @spec document_metadata_options(map(), keyword()) :: keyword()
  def document_metadata_options(dom, opts) do
    html =
      Enum.find(dom.children, fn node ->
        Map.get(node, :type) == :element and Map.get(node, :tag) == "html" and
          Map.get(node, :namespace, :html) == :html
      end)

    head =
      if html do
        Enum.find(html.children, fn node ->
          Map.get(node, :type) == :element and Map.get(node, :tag) == "head" and
            Map.get(node, :namespace, :html) == :html
        end)
      end

    head_children = if head, do: head.children, else: []

    defaults = document_metadata_defaults(head_children, %{})

    defaults =
      Map.new(defaults, fn
        {:author, authors} ->
          {:author, authors |> Enum.reverse() |> Enum.join(", ")}

        {:keywords, groups} ->
          {:keywords, groups |> Enum.reverse() |> List.flatten() |> Enum.uniq()}

        entry ->
          entry
      end)

    defaults =
      case document_title(dom) do
        nil -> defaults
        title -> Map.put(defaults, :title, title)
      end

    case {map_size(defaults), Keyword.fetch(opts, :metadata)} do
      {0, _} ->
        opts

      {_, :error} ->
        Keyword.put(opts, :metadata, defaults)

      {_, {:ok, metadata}} when is_map(metadata) ->
        Keyword.put(opts, :metadata, Map.merge(defaults, metadata))

      {_, {:ok, metadata}} when is_list(metadata) ->
        if Keyword.keyword?(metadata) do
          merged =
            Enum.reduce(defaults, metadata, fn {field, value}, acc ->
              Keyword.put_new(acc, field, value)
            end)

          Keyword.put(opts, :metadata, merged)
        else
          opts
        end

      _ ->
        opts
    end
  end

  defp document_title(node) do
    case node do
      %{type: :element, tag: "title", children: children} ->
        if Map.get(node, :namespace, :html) == :html do
          title =
            children
            |> Enum.map_join("", & &1.text)
            |> String.replace(~r/[ \t\n\f\r]+/u, " ")
            |> String.trim(" ")

          if title == "", do: nil, else: title
        end

      %{children: children} when is_list(children) ->
        Enum.find_value(children, &document_title/1)

      _ ->
        nil
    end
  end

  defp document_metadata_defaults(nodes, metadata) do
    Enum.reduce(nodes, metadata, fn node, metadata ->
      case node do
        %{tag: "noscript", children: children} ->
          if Map.get(node, :namespace, :html) == :html,
            do: document_metadata_defaults(children, metadata),
            else: metadata

        %{tag: "meta", attributes: %{"name" => name, "content" => content}} ->
          field =
            if Map.get(node, :namespace, :html) == :html and
                 is_nil(Map.get(Map.get(node, :attribute_namespaces, %{}), "name")) and
                 is_nil(Map.get(Map.get(node, :attribute_namespaces, %{}), "content")) do
              case String.downcase(name, :ascii) do
                "author" -> :author
                "description" -> :subject
                "keywords" -> :keywords
                _ -> nil
              end
            end

          case field do
            :author ->
              Map.update(metadata, :author, [content], &[content | &1])

            :keywords ->
              tokens =
                content
                |> String.split(",")
                |> Enum.map(&trim_ascii_whitespace/1)
                |> Enum.reject(&(&1 == ""))

              Map.update(metadata, :keywords, [tokens], &[tokens | &1])

            :subject ->
              Map.put_new(metadata, :subject, content)

            nil ->
              metadata
          end

        _ ->
          metadata
      end
    end)
  end

  @doc false
  @spec script_type(map()) :: :classic | :module | :importmap | :speculationrules | :data
  def script_type(attributes) do
    type =
      case Map.fetch(attributes, "type") do
        {:ok, ""} ->
          "text/javascript"

        {:ok, type} ->
          trim_ascii_whitespace(type)

        :error ->
          case Map.get(attributes, "language", "") do
            "" -> "text/javascript"
            language -> "text/" <> language
          end
      end

    cond do
      MimeValidator.javascript_essence_match?(type) ->
        :classic

      # Vocabulary recognition only, not a configurable resource ceiling.
      byte_size(type) <= byte_size("speculationrules") ->
        Map.get(
          %{
            "module" => :module,
            "importmap" => :importmap,
            "speculationrules" => :speculationrules
          },
          String.downcase(type, :ascii),
          :data
        )

      true ->
        :data
    end
  end

  @doc false
  @spec script_data_end_offset(binary()) :: non_neg_integer() | nil
  def script_data_end_offset(html), do: scan_script_data_end(html, 0, :data)

  # Script data has escaped and double-escaped states. A double-escaped end tag
  # exits that state rather than closing the element. Scan only state boundaries.
  defp scan_script_data_end(html, offset, state) do
    case Regex.run(@script_data_boundaries, html, return: :index, capture: :first, offset: offset) do
      [{index, length}] ->
        token = binary_part(html, index, length)
        token = String.downcase(token, :ascii)

        cond do
          token == "</script" and state != :double_escaped ->
            index

          token == "<!--" ->
            # The opening dashes can also be the beginning of an escape end.
            scan_script_data_end(html, index + 2, if(state == :data, do: :escaped, else: state))

          token == "-->" ->
            scan_script_data_end(html, index + length, :data)

          token == "<script" and state == :escaped ->
            scan_script_data_end(html, index + length, :double_escaped)

          token == "</script" ->
            scan_script_data_end(html, index + length, :escaped)

          true ->
            scan_script_data_end(html, index + length, state)
        end

      nil ->
        nil
    end
  end

  defp valid_script_attribute?(name) do
    name in @script_attributes or name in @preserved_global_attributes or
      name in ~w(style id class title role is hidden inert popover contenteditable headingoffset headingreset lang xml:lang itemscope itemtype itemid itemprop itemref translate) or
      String.starts_with?(name, ["data-", "aria-"])
  end

  @doc false
  @spec construct_html_template(map()) :: map()
  def construct_html_template(element) do
    reserve_render_resource(:max_html_nodes, 1, :html)
    fragment = %{type: :document_fragment, children: element.children}
    element |> Map.put(:children, []) |> Map.put(:content, fragment)
  end

  @doc false
  @spec html_inert?(map(), boolean()) :: boolean()
  def html_inert?(attributes, inherited?) do
    inherited? or Map.has_key?(attributes, "inert")
  end

  # URL parsing removes ASCII tabs/newlines and leading C0 controls/spaces.
  # These two active scheme names are fixed protocol vocabulary.
  @active_url_reference ~r/\A[\x00-\x20]*(?:[jJ][\t\n\r]*[aA][\t\n\r]*[vV][\t\n\r]*[aA][\t\n\r]*[sS][\t\n\r]*[cC][\t\n\r]*[rR][\t\n\r]*[iI][\t\n\r]*[pP][\t\n\r]*[tT]|[vV][\t\n\r]*[bB][\t\n\r]*[sS][\t\n\r]*[cC][\t\n\r]*[rR][\t\n\r]*[iI][\t\n\r]*[pP][\t\n\r]*[tT])[\t\n\r]*:/

  # The longest popover keyword is a fixed vocabulary bound, not a resource limit.
  @doc false
  @spec html_popover_state(map()) :: :none | :auto | :hint | :manual
  def html_popover_state(attributes) do
    case attributes do
      %{"popover" => ""} ->
        :auto

      %{"popover" => value} when byte_size(value) <= byte_size("manual") ->
        Map.get(
          %{"auto" => :auto, "hint" => :hint, "manual" => :manual},
          String.downcase(value, :ascii),
          :manual
        )

      %{"popover" => _value} ->
        :manual

      _ ->
        :none
    end
  end

  @doc "Returns static popover box defaults without inferring activation or top-layer state."
  @spec html_popover_defaults(map(), String.t(), number()) :: map()
  def html_popover_defaults(attributes, tag, font_size) do
    alias NativeElixirPdfUtilities.Validators.CssValueValidator

    %{
      display: if(tag == "dialog" and Map.has_key?(attributes, "open"), do: :block, else: :none),
      position: :fixed,
      offsets: CssValueValidator.edges(0.0),
      width: :fit_content,
      height: :fit_content,
      margin: CssValueValidator.edges(:auto),
      padding: CssValueValidator.edges(font_size * 0.25),
      border_widths: CssValueValidator.edges(2.25),
      border_styles: CssValueValidator.edges(:solid),
      border_color: :current_color,
      border_colors: CssValueValidator.edges(:current_color),
      background_color: {1.0, 1.0, 1.0},
      color: {0.0, 0.0, 0.0},
      overflow: :auto,
      _popover: true,
      _popover_default_padding: true
    }
  end

  @doc false
  @spec validate_static_url_attributes(map(), String.t() | nil) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_static_url_attributes(attributes, html_tag \\ nil) do
    attributes =
      if html_tag == "input",
        do:
          NativeElixirPdfUtilities.Validators.HtmlFormValidator.applicable_input_attributes(
            attributes
          ),
        else: attributes

    case attributes do
      attributes
      when is_map_key(attributes, "href") or is_map_key(attributes, "src") or
             is_map_key(attributes, "xlink:href") or is_map_key(attributes, "srcset") or
             is_map_key(attributes, "background") ->
        # Unregistered custom attributes do not create URL resources or links.
        if is_binary(html_tag) and not Map.has_key?(@supported_tags, html_tag) do
          :ok
        else
          Enum.reduce_while(["href", "src", "xlink:href", "srcset", "background"], :ok, fn name,
                                                                                           :ok ->
            case Map.fetch(attributes, name) do
              {:ok, value} ->
                references =
                  if name == "srcset" and html_tag in ["img", "source"] do
                    NativeElixirPdfUtilities.Validators.ResponsiveImageValidator.parse_srcset(
                      value
                    )
                    |> Enum.map(& &1.url)
                  else
                    if name == "srcset", do: [], else: [value]
                  end

                if Enum.any?(references, &Regex.match?(@active_url_reference, &1)) do
                  {:halt,
                   Diagnostics.error(
                     :html,
                     :unsupported_html,
                     "Active URL schemes are outside the static rendering profile; use a non-executable reference.",
                     module: __MODULE__,
                     source: name
                   )}
                else
                  {:cont, :ok}
                end

              :error ->
                {:cont, :ok}
            end
          end)
        end

      _ ->
        :ok
    end
  end

  @doc false
  @spec parse_background_image_reference(String.t()) ::
          {:ok, String.t() | nil}
          | {:error, :invalid_document}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def parse_background_image_reference(value) do
    normalized = String.trim(value)

    if byte_size(normalized) == 4 and downcase_ascii(normalized) == "none" do
      {:ok, nil}
    else
      case Regex.named_captures(
             ~r/^url\(\s*(?:"(?<double>[^"]+)"|'(?<single>[^']+)'|(?<bare>[^\s)'\"]+))\s*\)$/iu,
             normalized
           ) do
        captures when is_map(captures) ->
          source =
            Enum.find_value(["double", "single", "bare"], fn name ->
              case Map.get(captures, name) do
                value when is_binary(value) and value != "" -> value
                _ -> nil
              end
            end)

          source = if String.contains?(source, "\\"), do: decode_css_escapes(source), else: source

          case validate_static_url_attributes(%{"src" => source}) do
            :ok ->
              {:ok, source}

            {:error, {reason, diagnostic}} ->
              Diagnostics.error(:css, reason, diagnostic.message,
                module: __MODULE__,
                source: "background-image"
              )
          end

        _ ->
          {:error, :invalid_document}
      end
    end
  end

  @doc false
  @spec html_hidden_state(map()) :: :not_hidden | :hidden | :until_found
  def html_hidden_state(attributes) do
    case attributes do
      %{"hidden" => value} ->
        if String.downcase(value, :ascii) == "until-found", do: :until_found, else: :hidden

      _ ->
        :not_hidden
    end
  end

  @doc false
  @spec validate_html_element_attributes(String.t(), map()) ::
          :ok | {:error, {:unsupported_html, Diagnostics.diagnostic()}}
  @spec validate_html_element_attributes(String.t(), map(), term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_html_element_attributes(tag, attributes, context \\ nil) do
    with :ok <- validate_static_url_attributes(attributes, tag) do
      case tag do
        "input" ->
          NativeElixirPdfUtilities.Validators.HtmlFormValidator.validate_initial_special_attributes(
            attributes
          )

        tag when tag in ~w(xmp plaintext noembed) ->
          NativeElixirPdfUtilities.Validators.ForeignContentValidator.validate_static_input(
            :html,
            tag,
            attributes
          )

        tag
        when tag in ~w(canvas progress meter output datalist dialog font body q acronym basefont bgsound blink keygen) ->
          # Reuse the parser's attribute authority for direct DOM inputs.
          captures =
            Enum.map(attributes, fn {name, value} -> ["", name, "\"" <> value <> "\""] end)

          case attributes_to_map(captures, tag, :html) do
            {:ok, _} ->
              :ok

            {:error, :unsupported_html} ->
              Diagnostics.error(
                :html,
                :unsupported_html,
                "#{tag} requires global attributes and its applicable #{if(tag in ~w(progress meter output datalist), do: "control", else: "static")} attributes; remove unsupported attributes",
                module: __MODULE__,
                source: tag
              )
          end

        "template" ->
          NativeElixirPdfUtilities.Validators.TemplatePatchValidator.validate_attributes(
            attributes,
            context
          )

        "script" ->
          cond do
            not Enum.all?(Map.keys(attributes), &valid_script_attribute?/1) ->
              Diagnostics.error(
                :html,
                :unsupported_html,
                "script data blocks require supported attributes without event handlers",
                module: __MODULE__
              )

            script_type(attributes) != :data ->
              Diagnostics.error(
                :html,
                :unsupported_html,
                "#{script_type(attributes)} script elements are outside the static rendering profile; only non-executable data blocks are supported",
                module: __MODULE__
              )

            true ->
              :ok
          end

        "base" ->
          NativeElixirPdfUtilities.Validators.HtmlUrlValidator.validate_base_element(
            attributes,
            []
          )

        "meta" ->
          directive = attributes |> Map.get("http-equiv", "") |> String.downcase()

          cond do
            directive in ~w(refresh content-security-policy content-security-policy-report-only set-cookie) ->
              Diagnostics.error(
                :html,
                :unsupported_html,
                "meta http-equiv=#{inspect(directive)} requests browser navigation, security enforcement, or session state outside the static rendering profile",
                module: __MODULE__
              )

            Map.has_key?(attributes, "charset") and
                String.downcase(attributes["charset"]) != "utf-8" ->
              Diagnostics.error(
                :html,
                :unsupported_html,
                "HTML input is UTF-8; meta charset must declare utf-8",
                module: __MODULE__
              )

            true ->
              :ok
          end

        _ ->
          if custom_html_tag?(tag) do
            case Enum.find(~w(dir), &Map.has_key?(attributes, &1)) do
              nil ->
                NativeElixirPdfUtilities.Validators.ForeignContentValidator.validate_static_input(
                  :html,
                  tag,
                  attributes
                )

              attribute ->
                Diagnostics.error(
                  :html,
                  :unsupported_html,
                  "custom container #{attribute} requires direction semantics that are not implemented yet",
                  module: __MODULE__,
                  source: tag
                )
            end
          else
            :ok
          end
      end
    end
  end

  @doc false
  @spec jpeg_metadata(binary()) :: {:ok, map()} | :error
  def jpeg_metadata(data) do
    NativeElixirPdfUtilities.Validators.JpegValidator.metadata(data)
  end

  @doc false
  @spec new_css_budget() :: :atomics.atomics_ref()
  def new_css_budget, do: :atomics.new(2, signed: false)

  @doc false
  @spec compute_custom_properties(map(), :atomics.atomics_ref()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def compute_custom_properties(properties, budget \\ new_css_budget()) do
    with {:ok, memo} <- custom_property_cycle_memo(properties) do
      Enum.reduce_while(properties, {:ok, memo}, fn {name, _value}, {:ok, memo} ->
        case resolve_custom_property(name, properties, %{}, memo, budget) do
          {:ok, _value, _height, memo} ->
            {:cont, {:ok, memo}}

          {:error, _detail} = error ->
            {:halt, error}

          :error ->
            {:halt,
             Diagnostics.error(:css, :invalid_css, "custom property has invalid variable syntax",
               source: name
             )}
        end
      end)
      |> case do
        {:ok, memo} ->
          {:ok,
           Map.new(properties, fn {name, _value} -> {name, elem(Map.fetch!(memo, name), 0)} end)}

        error ->
          error
      end
    else
      :error ->
        Diagnostics.error(:css, :invalid_css, "custom property has invalid variable syntax",
          module: __MODULE__,
          operation: :compute_custom_properties
        )

      error ->
        error
    end
  end

  # Fallback references participate in the CSS dependency graph even when the
  # chosen property has a value. Only actual custom-property computation needs
  # this cycle result; source preparation retains the same dependencies as tokens.
  defp custom_property_cycle_memo(properties) do
    if Enum.any?(properties, fn {_name, value} ->
         is_binary(value) and String.contains?(value, ",")
       end) do
      graph = :digraph.new([:private])

      try do
        Enum.each(properties, fn {name, _} -> :digraph.add_vertex(graph, name) end)

        with :ok <-
               Enum.reduce_while(properties, :ok, fn {name, value}, :ok ->
                 parsed = if is_nil(value), do: {:ok, []}, else: css_variable_references(value, 0)

                 case parsed do
                   {:ok, references} ->
                     Enum.each(references, fn reference ->
                       Enum.each(reference.dependencies, fn dependency ->
                         reserve_render_resource(:max_css_work, 1, :css)

                         if Map.has_key?(properties, dependency),
                           do: :digraph.add_edge(graph, name, dependency)
                       end)
                     end)

                     {:cont, :ok}

                   error ->
                     {:halt, error}
                 end
               end) do
          {:ok,
           graph
           |> :digraph_utils.cyclic_strong_components()
           |> List.flatten()
           |> Map.new(&{&1, {nil, 0}})}
        end
      after
        :digraph.delete(graph)
      end
    else
      {:ok, %{}}
    end
  end

  @doc false
  @spec resolve_css_variables(String.t(), map(), map(), :atomics.atomics_ref()) ::
          {:ok, String.t()} | :error | {:error, {atom(), Diagnostics.diagnostic()}}
  def resolve_css_variables(value, properties, resolving, budget \\ new_css_budget()) do
    case expand_css_value(value, properties, resolving, %{}, budget) do
      {:ok, nil, _height, _memo} -> :error
      {:ok, resolved, _height, _memo} -> {:ok, resolved}
      error -> error
    end
  end

  defp resolve_custom_property(name, properties, resolving, memo, budget) do
    depth = map_size(resolving)

    with :ok <- css_limit(:max_css_variable_work, :atomics.add_get(budget, 2, 1)),
         :ok <- css_limit(:max_css_variable_depth, depth + 1) do
      case {Map.has_key?(resolving, name), Map.fetch(memo, name), Map.get(properties, name)} do
        {true, _cached, _value} ->
          {:ok, nil, 0, memo}

        {false, {:ok, {value, height}}, _value} ->
          with :ok <- css_limit(:max_css_variable_depth, depth + height) do
            {:ok, value, height, memo}
          end

        {false, :error, nil} ->
          {:ok, nil, 0, Map.put(memo, name, {nil, 0})}

        {false, :error, value} ->
          with {:ok, resolved, height, memo} <-
                 expand_css_value(value, properties, Map.put(resolving, name, true), memo, budget) do
            height = height + 1
            {:ok, resolved, height, Map.put(memo, name, {resolved, height})}
          end
      end
    end
  end

  @doc "Parses balanced CSS variable references and fallback dependencies without substitution."
  @spec parse_css_variable_references(String.t()) ::
          {:ok, [map()]} | :error | {:error, {atom(), Diagnostics.diagnostic()}}
  def parse_css_variable_references(value) do
    with_render_budget(fn ->
      if is_binary(value) and String.valid?(value, :fast_ascii),
        do: css_variable_references(value, 0),
        else: :error
    end)
  end

  @doc "Prepares variable syntax and source dependencies without substituting an inherited value."
  @spec prepare_css_variable_source(String.t(), :atomics.atomics_ref()) ::
          {:ok, [map()]} | :error | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_css_variable_source(value, budget) do
    with_render_budget(fn ->
      with :ok <- validate_css_budget(budget),
           true <- is_binary(value) and String.valid?(value, :fast_ascii),
           :ok <- css_limit(:max_css_variable_bytes, byte_size(value)),
           {:ok, references} <- css_variable_references(value, 0),
           :ok <-
             css_limit(
               :max_css_variable_work,
               :atomics.add_get(budget, 2, Enum.reduce(references, 0, &(&1.syntax_work + &2)))
             ),
           :ok <-
             css_limit(
               :max_css_variable_total_bytes,
               :atomics.add_get(budget, 1, byte_size(value))
             ) do
        {:ok, references}
      else
        false -> :error
        error -> error
      end
    end)
  end

  # Source preparation and actual substitution consume the same balanced grammar.
  # Strings and escapes are opaque lexical units. Fallbacks remain authored token
  # streams until a missing or cyclic property actually selects them.
  defp css_variable_references(value, depth) do
    with :ok <- css_limit(:max_css_variable_depth, depth),
         {:ok, _} <-
           NativeElixirPdfUtilities.Validators.CssMathValidator.component_tokens(value, false) do
      reserve_render_resource(:max_css_work, byte_size(value), :css)

      # Balanced components without a function opening cannot contain var().
      if :binary.match(value, "(") == :nomatch do
        {:ok, []}
      else
        {units, _} =
          value
          |> css_component_units()
          |> Enum.map_reduce(0, fn unit, offset -> {{unit, offset}, offset + byte_size(unit)} end)

        css_variable_scan(units, value, depth, [])
      end
    else
      :invalid -> :error
      result -> result
    end
  end

  defp css_variable_scan(units, value, depth, references) do
    case units do
      [] ->
        {:ok, Enum.reverse(references)}

      [{unit, offset} | rest] ->
        remaining = binary_part(value, offset, byte_size(value) - offset)

        parsed =
          if String.starts_with?(unit, ["\"", "'"]),
            do: :error,
            else: parse_css_identifier(remaining)

        case parsed do
          {:ok, name, tail} ->
            finish = byte_size(value) - byte_size(tail)
            following = Enum.drop_while(units, fn {_, at} -> at < finish end)

            case {String.downcase(name, :ascii), following} do
              {"var", [{"(", opening} | body]} ->
                with {:ok, closing, comma, following} <- css_variable_body(body, 1, nil),
                     body_source =
                       binary_part(value, opening + 1, (comma || closing) - opening - 1),
                     property = trim_css_whitespace(body_source),
                     {:ok, {^property, _}} <- validate_css_declaration(property <> ":x"),
                     true <- String.starts_with?(property, "--"),
                     fallback =
                       if(comma,
                         do:
                           binary_part(value, comma + 1, closing - comma - 1)
                           |> trim_css_whitespace()
                       ),
                     {:ok, fallback_references} <-
                       if(is_nil(fallback),
                         do: {:ok, []},
                         else: css_variable_references(fallback, depth + 1)
                       ) do
                  reference = %{
                    offset: offset,
                    length: closing + 1 - offset,
                    name: property,
                    fallback: fallback,
                    syntax_work: 1 + Enum.reduce(fallback_references, 0, &(&1.syntax_work + &2)),
                    dependencies:
                      Enum.uniq([
                        property | Enum.flat_map(fallback_references, & &1.dependencies)
                      ])
                  }

                  css_variable_scan(following, value, depth, [reference | references])
                else
                  {:error, {_reason, _diagnostic}} = error -> error
                  _ -> :error
                end

              _ ->
                css_variable_scan(following, value, depth, references)
            end

          :error ->
            css_variable_scan(rest, value, depth, references)
        end
    end
  end

  # The canonical component tokenizer has already proved balanced parentheses.
  # Every var opening therefore has its closing lexical unit in this suffix.
  defp css_variable_body(units, depth, comma) do
    [{unit, offset} | rest] = units

    cond do
      unit == ")" and depth == 1 -> {:ok, offset, comma, rest}
      unit == ")" -> css_variable_body(rest, depth - 1, comma)
      unit == "(" -> css_variable_body(rest, depth + 1, comma)
      unit == "," and depth == 1 and is_nil(comma) -> css_variable_body(rest, depth, offset)
      true -> css_variable_body(rest, depth, comma)
    end
  end

  defp expand_css_value(value, properties, resolving, memo, budget) do
    with :ok <- css_limit(:max_css_variable_bytes, byte_size(value)),
         {:ok, matches} <- css_variable_references(value, map_size(resolving)) do
      Enum.reduce_while(matches, {:ok, [], 0, 0, 0, memo}, fn
        %{offset: offset, length: length, name: name, fallback: fallback},
        {:ok, chunks, cursor, size, height, memo} ->
          resolved =
            case resolve_custom_property(name, properties, resolving, memo, budget) do
              {:ok, nil, _height, memo} when not is_nil(fallback) ->
                expand_css_value(fallback, properties, resolving, memo, budget)

              result ->
                result
            end

          case resolved do
            {:ok, nil, _height, memo} ->
              {:halt, {:ok, nil, 0, memo}}

            {:ok, replacement, dependency_height, memo} ->
              size = size + offset - cursor + byte_size(replacement)

              case css_limit(:max_css_variable_bytes, size) do
                :ok ->
                  prefix = binary_part(value, cursor, offset - cursor)

                  {:cont,
                   {:ok, [replacement, prefix | chunks], offset + length, size,
                    max(height, dependency_height), memo}}

                error ->
                  {:halt, error}
              end

            error ->
              {:halt, error}
          end
      end)
      |> case do
        {:ok, chunks, cursor, size, height, memo} ->
          size = size + byte_size(value) - cursor

          with :ok <- css_limit(:max_css_variable_bytes, size),
               :ok <- css_limit(:max_css_variable_total_bytes, :atomics.add_get(budget, 1, size)) do
            suffix = binary_part(value, cursor, byte_size(value) - cursor)
            {:ok, IO.iodata_to_binary(Enum.reverse([suffix | chunks])), height, memo}
          end

        result ->
          result
      end
    end
  end

  defp css_limit(key, amount) do
    limit = Limits.get(key)

    if amount <= limit do
      :ok
    else
      Diagnostics.error(
        :limits,
        :resource_limit_exceeded,
        "CSS custom-property expansion exceeds #{key}=#{limit}; reduce variable values, references, or dependency depth"
      )
    end
  end

  @doc false
  @spec new_image_budget() :: image_budget()
  def new_image_budget do
    :atomics.new(3, signed: false)
  end

  @doc false
  @spec validate_image_budget(term()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_image_budget(budget) do
    case :atomics.info(budget) do
      %{size: 3} -> :ok
      _ -> Diagnostics.error(:style, :invalid_document, "image resource budget is invalid")
    end
  rescue
    ArgumentError ->
      Diagnostics.error(:style, :invalid_document, "image resource budget is invalid")
  end

  @doc false
  @spec image_source_read_limit(image_budget() | nil) ::
          {:ok, pos_integer()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def image_source_read_limit(budget) do
    {count, bytes} =
      if is_nil(budget), do: {0, 0}, else: {:atomics.get(budget, 1), :atomics.get(budget, 2)}

    cond do
      count >= Limits.get(:max_image_count) ->
        Diagnostics.error(:limits, :resource_limit_exceeded, "image count exceeds the limit")

      bytes >= Limits.get(:max_aggregate_image_source_bytes) ->
        Diagnostics.error(
          :limits,
          :resource_limit_exceeded,
          "aggregate image source bytes exceed the limit"
        )

      true ->
        {:ok,
         min(
           Limits.get(:max_image_source_bytes),
           Limits.get(:max_aggregate_image_source_bytes) - bytes
         )}
    end
  rescue
    ArgumentError -> Diagnostics.error(:style, :invalid_document, "image budget is invalid")
  end

  @doc false
  @spec reserve_image_source(image_budget(), non_neg_integer()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def reserve_image_source(budget, source_bytes) do
    max_image_source_bytes = Limits.get(:max_image_source_bytes)

    cond do
      not is_integer(source_bytes) or source_bytes < 0 ->
        Diagnostics.error(:style, :invalid_document, "image source size is invalid")

      source_bytes > max_image_source_bytes ->
        Diagnostics.error(
          :limits,
          :resource_limit_exceeded,
          "image source exceeds the #{max_image_source_bytes}-byte limit"
        )

      true ->
        with :ok <-
               reserve_image_budget(
                 budget,
                 1,
                 1,
                 Limits.get(:max_image_count),
                 "image count exceeds the limit"
               ),
             :ok <-
               reserve_image_budget(
                 budget,
                 2,
                 source_bytes,
                 Limits.get(:max_aggregate_image_source_bytes),
                 "aggregate image source bytes exceed the limit"
               ) do
          :ok
        end
    end
  end

  @doc false
  @spec reserve_decoded_image(image_budget(), pos_integer(), pos_integer(), pos_integer()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def reserve_decoded_image(budget, width, height, channels) do
    max_decoded_image_bytes = Limits.get(:max_decoded_image_bytes)

    case is_integer(width) and width > 0 and is_integer(height) and height > 0 and
           is_integer(channels) and channels > 0 do
      true ->
        decoded_bytes = width * height * channels

        cond do
          decoded_bytes > max_decoded_image_bytes ->
            Diagnostics.error(
              :limits,
              :resource_limit_exceeded,
              "decoded image exceeds the #{max_decoded_image_bytes}-byte limit"
            )

          true ->
            reserve_image_budget(
              budget,
              3,
              decoded_bytes,
              Limits.get(:max_aggregate_decoded_image_bytes),
              "aggregate decoded image bytes exceed the limit"
            )
        end

      false ->
        Diagnostics.error(:style, :invalid_document, "decoded image dimensions are invalid")
    end
  end

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
  @spec validate_document_render_options(term(), term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_document_render_options(opts, font_options_result) do
    validate_render_options(opts, font_options_result)
  end

  @doc false
  @spec validate_pdf_output_options(keyword()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_pdf_output_options(opts) do
    private_option =
      Enum.find(
        [
          :__link_context__,
          :__document_url_context__,
          :link_scope,
          :link_expected_targets,
          :link_document_targets,
          :html_target
        ],
        &Keyword.has_key?(opts, &1)
      )

    if private_option != nil do
      Diagnostics.error(
        :options,
        :invalid_options,
        "HTML link preparation contexts and target descriptors are internal stage data; supply canonical page destinations to the PDF writer",
        operation: :write_pdf,
        module: __MODULE__,
        source: Atom.to_string(private_option)
      )
    else
      case {Keyword.get(opts, :compress_streams, true), Keyword.get(opts, :subset_fonts, true)} do
        {compress?, subset?} when is_boolean(compress?) and is_boolean(subset?) ->
          :ok

        {compress?, _subset?} when not is_boolean(compress?) ->
          Diagnostics.error(:options, :invalid_options, "compress_streams must be a boolean")

        _ ->
          Diagnostics.error(:options, :invalid_options, "subset_fonts must be a boolean")
      end
    end
  end

  @doc false
  @spec with_render_budget((-> result)) :: result | {:error, {atom(), Diagnostics.diagnostic()}}
        when result: term()
  def with_render_budget(fun) do
    key = {__MODULE__, :render_budget}

    case Process.get(key) do
      nil ->
        Process.put(key, %{})
        Process.delete({__MODULE__, :validated_dom})

        try do
          fun.()
        catch
          {:render_resource_limit, error} -> error
        after
          Process.delete(key)
          Process.delete({__MODULE__, :validated_dom})
        end

      _budget ->
        fun.()
    end
  end

  @doc false
  @spec reserve_render_resource(Limits.key(), non_neg_integer(), atom()) :: :ok
  def reserve_render_resource(limit, amount, stage) do
    key = {__MODULE__, :render_budget}

    case Process.get(key) do
      nil ->
        check_render_resource(limit, amount, stage)

      budget ->
        used =
          case budget do
            %{^limit => used} -> used + amount
            _ -> amount
          end

        check_render_resource(limit, used, stage)
        if amount != 0, do: Process.put(key, Map.put(budget, limit, used))
    end

    :ok
  end

  @doc false
  @spec check_render_resource(Limits.key(), number(), atom()) :: :ok
  @compile {:inline, check_render_resource: 3}
  def check_render_resource(limit, amount, stage) do
    case amount <= Limits.get(limit) do
      true ->
        :ok

      false ->
        throw(
          {:render_resource_limit,
           Diagnostics.error(
             stage,
             :resource_limit_exceeded,
             "rendering exceeds #{limit} (configured limit #{Limits.get(limit)})",
             module: __MODULE__
           )}
        )
    end
  end

  @doc false
  @spec within_html_element((-> result)) :: result when result: term()
  def within_html_element(fun) do
    key = {__MODULE__, :render_budget}
    budget = Process.get(key)
    depth = Map.get(budget, :max_html_depth, 0)
    check_render_resource(:max_html_depth, depth + 1, :html)
    Process.put(key, Map.put(budget, :max_html_depth, depth + 1))

    try do
      fun.()
    after
      Process.put(key, Map.put(Process.get(key), :max_html_depth, depth))
    end
  end

  @doc false
  @spec reserve_layout_box(atom()) :: atom()
  def reserve_layout_box(type) do
    reserve_render_resource(:max_layout_boxes, 1, :layout)
    type
  end

  @doc false
  @spec validate_html_source(term()) ::
          {:ok, binary()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_html_source(html) do
    case html do
      html when is_binary(html) ->
        case {byte_size(html) <= Limits.get(:max_html_source_bytes),
              String.valid?(html, :fast_ascii)} do
          {false, _} ->
            Diagnostics.error(
              :html,
              :resource_limit_exceeded,
              "HTML input exceeds max_html_source_bytes"
            )

          {true, true} ->
            {:ok, html}

          {true, false} ->
            Diagnostics.error(:html, :invalid_encoding, "HTML input must be valid UTF-8")
        end

      _ ->
        Diagnostics.error(:html, :invalid_html, "HTML input must be a string")
    end
  end

  @doc false
  @spec valid_html_child?(atom() | String.t(), String.t(), list()) :: boolean()
  def valid_html_child?(context, tag, ancestors \\ []) do
    # An ordinary template starts an inert document fragment, not descendants
    # of its host for active-tree content model restrictions.
    ancestors =
      if "template" in ancestors,
        do: Enum.take_while(ancestors, &(&1 != "template")),
        else: ancestors

    cond do
      tag == "area" ->
        "map" in [context | ancestors]

      tag == "frame" ->
        context == "frameset"

      tag == "frameset" ->
        context in [:document, :after_doctype, :after_head, "html", "frameset"]

      context == "frameset" ->
        tag == "noframes"

      tag == "noframes" ->
        context in [:document, :after_doctype, :after_head, "html", "head", "body"]

      tag == "base" and context not in [:document, :after_doctype, "html", "head", "template"] ->
        false

      tag in ["form", "label"] and tag in [context | ancestors] ->
        false

      tag == "noscript" and "noscript" in [context | ancestors] ->
        false

      tag in ~w(progress meter) and tag in [context | ancestors] ->
        false

      tag == "legend" and context != "fieldset" ->
        false

      tag == "summary" and context != "details" ->
        false

      custom_html_tag?(tag) ->
        valid_html_child?(context, "span", ancestors)

      context == "noscript" ->
        case ancestors do
          ["head" | _] -> tag in ~w(style meta)
          [parent | ancestors] -> valid_html_child?(parent, tag, ancestors)
          [] -> valid_html_child?(:document, tag)
        end

      context == "template" ->
        supported_html_tag?(tag) and tag not in ~w(html head body)

      tag == "source" and context != "picture" ->
        false

      context == "picture" ->
        tag in ~w(source img script template)

      context == "table" ->
        tag in ~w(caption colgroup col thead tbody tfoot tr script template)

      context == "colgroup" ->
        tag in ~w(col template)

      context in ~w(thead tbody tfoot) ->
        tag in ~w(tr script template)

      context == "tr" ->
        tag in ~w(th td script template)

      tag == "li" and context not in @list_tags ->
        false

      tag in ~w(header footer article aside nav section h1 h2 h3 h4 h5 h6) and
          "dt" in [context | ancestors] ->
        false

      tag in ~w(dt dd) and context != "dl" and
          not (context == "div" and List.first(ancestors) == "dl") ->
        false

      tag == "figcaption" and context != "figure" ->
        false

      tag == "a" and "a" in [context | ancestors] ->
        false

      context in ["a", "ins", "del", "object", "canvas", "slot", "map"] or
          custom_html_tag?(context) ->
        case ancestors do
          [parent | rest] ->
            valid_html_child?(if(is_tuple(parent), do: "div", else: parent), tag, rest)

          [] ->
            valid_html_child?(:document, tag)
        end

      context in [:document, :after_doctype] ->
        tag in @block_tags or tag in @inline_tags or
          tag in ~w(html head body base basefont bgsound style meta title script template)

      context == :after_head ->
        tag in @block_tags or tag in @inline_tags or tag in ~w(body style meta script template)

      context in [:after_body, :after_html] ->
        false

      context == "html" ->
        tag in @block_tags or tag in @inline_tags or
          tag in ~w(head body base basefont bgsound style meta title script template)

      context == "head" ->
        tag in ~w(base basefont bgsound style meta title script template noscript)

      context == "body" ->
        tag in @block_tags or tag in @inline_tags or tag in ~w(style meta title script template)

      context in @list_tags ->
        tag in ~w(li script template)

      context == "style" ->
        false

      context == "title" ->
        false

      context == "datalist" ->
        tag in @inline_tags or tag in ~w(option img picture script template)

      context == "select" or
          (context == "div" and "select" in ancestors and "option" not in ancestors) ->
        tag in ~w(button div option optgroup hr script template noscript)

      context == "optgroup" ->
        tag in ~w(option script template)

      context == "option" ->
        tag in @inline_tags or tag in ~w(div img picture script template)

      context == "textarea" ->
        false

      context == "button" ->
        tag in @inline_tags or tag in ~w(img progress meter script template)

      context in ["summary", "legend"] ->
        tag in @inline_tags or tag in ~w(img h1 h2 h3 h4 h5 h6 script template)

      context in @table_content_tags ->
        tag in @inline_tags or tag in @block_tags

      context == "dl" ->
        tag in ~w(dt dd div script template)

      context in @flow_container_tags ->
        tag in @block_tags or tag in @inline_tags or tag in ~w(style script template)

      context == "li" or context in @block_tags or context in @inline_tags ->
        tag in @inline_tags or tag in ~w(img picture script template)

      true ->
        false
    end
  end

  @doc false
  @spec valid_html_child_element?(term(), map(), list()) :: boolean()
  def valid_html_child_element?(context, element, ancestors \\ []) do
    ancestors =
      cond do
        context == "template" -> []
        "template" in ancestors -> Enum.take_while(ancestors, &(&1 != "template"))
        true -> ancestors
      end

    html? = Map.get(element, :namespace, :html) == :html
    parent = if is_tuple(context), do: "div", else: context
    tag = element.tag

    (not html? or valid_html_child?(parent, tag, ancestors)) and
      if Enum.any?([context | ancestors], &(&1 in ["a", "button", "option", "canvas"])) do
        attributes =
          Map.reject(element.attributes, fn {name, _} ->
            not is_nil(Map.get(Map.get(element, :attribute_namespaces, %{}), name))
          end)

        # Interactive content is conditional for anchors, images and inputs.
        # Hidden controls and placeholder anchors are static phrasing content.
        interactive? =
          html? and
            (tag in ~w(iframe embed button select textarea label details) or
               (tag == "a" and Map.has_key?(attributes, "href")) or
               (tag == "input" and input_type(Map.get(attributes, "type", "text")) != "hidden") or
               (tag in ["img", "object"] and Map.has_key?(attributes, "usemap")))

        canvas_only? =
          "canvas" in [context | ancestors] and
            not Enum.any?([context | ancestors], &(&1 in ["a", "button", "option"]))

        canvas_exception? =
          tag in ~w(a button) or
            (tag == "img" and Map.has_key?(attributes, "usemap")) or
            (tag == "input" and
               input_type(Map.get(attributes, "type", "text")) in ~w(checkbox radio button reset submit image)) or
            (tag == "select" and
               (Map.has_key?(attributes, "multiple") or
                  case parse_html_integer(Map.get(attributes, "size"), :nonnegative) do
                    {:ok, size} -> size > 1
                    :error -> false
                  end))

        if canvas_only?,
          do: not interactive? or canvas_exception?,
          else: not interactive? and not Map.has_key?(attributes, "tabindex")
      else
        true
      end
  end

  @doc false
  @spec supported_html_tag?(term()) :: boolean()
  def supported_html_tag?(tag) do
    Map.has_key?(@supported_tags, tag) or custom_html_tag?(tag)
  end

  @doc false
  @spec custom_html_tag?(term()) :: boolean()
  def custom_html_tag?(tag) do
    case tag do
      tag when is_map_key(@supported_tags, tag) ->
        false

      <<first, _::binary>> when first in ?a..?z ->
        String.contains?(tag, "-") and
          tag not in @reserved_custom_names and String.valid?(tag) and
          Regex.match?(~r/\A[a-z][^A-Z\x00\x09\x0a\x0c\x0d\x20\/>]*\z/u, tag)

      _ ->
        false
    end
  end

  @doc false
  @spec parse_html_integer(term(), :signed | :nonnegative) :: {:ok, integer()} | :error
  def parse_html_integer(value, kind \\ :signed) do
    case value do
      value when is_binary(value) ->
        regex =
          if kind == :nonnegative,
            do: ~r/^[ \t\n\f\r]*([+]?[0-9]+)/,
            else: ~r/^[ \t\n\f\r]*([+-]?[0-9]+)/

        case Regex.run(regex, value, capture: :all_but_first) do
          [digits] ->
            try do
              check_render_resource(:max_html_numeric_token_bytes, byte_size(digits), :html)
            catch
              {:render_resource_limit, {:error, {reason, diagnostic}}} ->
                throw(
                  {:render_resource_limit,
                   {:error, {reason, Diagnostics.with_context(diagnostic, source: value)}}}
                )
            end

            {integer, ""} = Integer.parse(digits)
            {:ok, integer}

          nil ->
            :error
        end

      _ ->
        :error
    end
  end

  @doc "Parses a legacy browser attribute using its fixed signed 32-bit conversion invariant."
  @spec parse_html_legacy_int32(term()) :: {:ok, integer()} | :error
  def parse_html_legacy_int32(value) do
    case parse_html_integer(value) do
      {:ok, integer} when integer >= -2_147_483_648 and integer <= 2_147_483_647 -> {:ok, integer}
      _ -> :error
    end
  end

  @doc false
  @spec resolve_html_heading_context(map(), non_neg_integer()) ::
          {non_neg_integer(), 1..9 | nil}
  def resolve_html_heading_context(node, inherited_offset) do
    case node do
      %{attributes: attributes}
      when inherited_offset == 0 and not is_map_key(attributes, "headingoffset") and
             not is_map_key(attributes, "headingreset") ->
        {0, nil}

      _ ->
        offset =
          if Map.get(node, :namespace, :html) == :html do
            attributes = node.attributes
            namespaces = Map.get(node, :attribute_namespaces, %{})

            own_offset =
              if is_nil(Map.get(namespaces, "headingoffset")) do
                case parse_html_integer(Map.get(attributes, "headingoffset")) do
                  {:ok, value} when value >= 0 -> value
                  _ -> 0
                end
              else
                0
              end

            if Map.has_key?(attributes, "headingreset") and
                 is_nil(Map.get(namespaces, "headingreset")),
               do: own_offset,
               else: inherited_offset + own_offset
          else
            inherited_offset
          end

        level =
          case node do
            %{tag: <<"h", base>>} when base in ?1..?6 and offset > 0 ->
              # HTML fixes the maximum computed heading level at nine.
              if Map.get(node, :namespace, :html) == :html,
                do: min(base - ?0 + offset, 9),
                else: nil

            _ ->
              nil
          end

        {offset, level}
    end
  catch
    {:render_resource_limit, {:error, {reason, diagnostic}}} ->
      throw(
        {:render_resource_limit,
         {:error,
          {reason,
           Diagnostics.diagnostic(diagnostic.stage, diagnostic.reason, diagnostic.message,
             operation: Map.get(diagnostic, :operation),
             module: Map.get(diagnostic, :module),
             source: "headingoffset=" <> node.attributes["headingoffset"],
             line: Map.get(diagnostic, :line, Map.get(node, :line)),
             column: Map.get(diagnostic, :column, Map.get(node, :column))
           )}}}
      )
  end

  @doc false
  @spec resolve_html_list_ordinals(String.t(), map(), list()) :: list()
  def resolve_html_list_ordinals(tag, attributes, children) do
    reversed? = tag == "ol" and Map.has_key?(attributes, "reversed")
    step = if reversed?, do: -1, else: 1

    start =
      case parse_html_integer(Map.get(attributes, "start")) do
        {:ok, integer} when tag == "ol" ->
          integer

        _ ->
          if reversed?,
            do: Enum.count(children, &match?(%{style: %{display: :list_item}}, &1)),
            else: 1
      end

    {children, _next} =
      Enum.map_reduce(children, start, fn child, next ->
        case child do
          %{style: %{display: :list_item} = style} ->
            ordinal =
              case Map.get(style, :_list_value) do
                {:ok, integer} when tag == "ol" -> integer
                _ -> next
              end

            marker = CounterStyle.marker(Map.get(style, :list_marker_type, :disc), ordinal)
            reserve_render_resource(:max_rendered_text_bytes, byte_size(marker), :style)
            markers = if marker == "", do: [], else: [%{type: :text, text: marker, style: style}]
            {Map.put(child, :style, Map.put(style, :_list_marker, markers)), ordinal + step}

          _ ->
            {child, next}
        end
      end)

    children
  end

  @doc false
  @spec html_list_marker_type(String.t(), map()) :: atom() | nil
  def html_list_marker_type(tag, attributes) do
    case {tag, Map.get(attributes, "type")} do
      {tag, "1"} when tag in ~w(ol li) -> :decimal
      {tag, "a"} when tag in ~w(ol li) -> :lower_alpha
      {tag, "A"} when tag in ~w(ol li) -> :upper_alpha
      {tag, "i"} when tag in ~w(ol li) -> :lower_roman
      {tag, "I"} when tag in ~w(ol li) -> :upper_roman
      {tag, "disc"} when tag in ~w(ul li) -> :disc
      {tag, "circle"} when tag in ~w(ul li) -> :circle
      {tag, "square"} when tag in ~w(ul li) -> :square
      _ -> nil
    end
  end

  @doc false
  @spec parse_css_list_style(String.t(), String.t()) :: {:ok, map()} | :error
  def parse_css_list_style(property, value) do
    tokens = split_css_tokens(value)

    Enum.reduce_while(tokens, {:ok, %{}}, fn token, {:ok, parsed} ->
      normalized = String.downcase(token, :ascii)

      result =
        cond do
          property == "list-style" and String.starts_with?(normalized, "url(") ->
            case parse_background_image_reference(token) do
              {:ok, source} -> {:ok, :list_style_image_source, source}
              _ -> :error
            end

          normalized in ~w(inside outside) and property != "list-style-type" ->
            {:ok, :list_style_position, if(normalized == "inside", do: :inside, else: :outside)}

          property != "list-style-position" ->
            case normalized do
              "none" ->
                {:ok, :list_marker_type, :none}

              "disc" ->
                {:ok, :list_marker_type, :disc}

              "circle" ->
                {:ok, :list_marker_type, :circle}

              "square" ->
                {:ok, :list_marker_type, :square}

              "disclosure-open" ->
                {:ok, :list_marker_type, :disclosure_open}

              "disclosure-closed" ->
                {:ok, :list_marker_type, :disclosure_closed}

              "decimal" ->
                {:ok, :list_marker_type, :decimal}

              "decimal-leading-zero" ->
                {:ok, :list_marker_type, :decimal_leading_zero}

              value when value in ~w(lower-alpha lower-latin) ->
                {:ok, :list_marker_type, :lower_alpha}

              value when value in ~w(upper-alpha upper-latin) ->
                {:ok, :list_marker_type, :upper_alpha}

              "lower-roman" ->
                {:ok, :list_marker_type, :lower_roman}

              "upper-roman" ->
                {:ok, :list_marker_type, :upper_roman}

              _ ->
                :error
            end

          true ->
            :error
        end

      case result do
        {:ok, key, value} ->
          if Map.has_key?(parsed, key),
            do: {:halt, :error},
            else: {:cont, {:ok, Map.put(parsed, key, value)}}

        :error ->
          {:halt, :error}
      end
    end)
    |> case do
      {:ok, parsed} when map_size(parsed) == 0 ->
        :error

      {:ok, parsed} when property == "list-style" ->
        {:ok,
         Map.merge(
           %{list_marker_type: :disc, list_style_position: :outside, list_style_image: nil},
           parsed
         )}

      result ->
        result
    end
  end

  @doc false
  @spec normalize_html_element_children(String.t(), list(), String.t() | nil) :: list()
  def normalize_html_element_children(tag, children, first_token) do
    case {tag, children} do
      {tag, [%{type: :text, text: text} = first | rest]} when tag in ~w(pre listing textarea) ->
        text =
          if is_binary(first_token) and not String.starts_with?(first_token, "<!--") do
            String.replace(text, ~r/^\r?\n|^\r/u, "", global: false)
          else
            text
          end

        [%{first | text: text} | rest]

      _ ->
        children
    end
  end

  @doc false
  @spec parse_css_text_decoration(String.t(), String.t(), number() | nil, term()) ::
          {:ok, map()} | :error
  def parse_css_text_decoration(property, value, font_size, color) do
    normalized = value |> trim_ascii_whitespace() |> downcase_ascii()

    case property do
      "text-decoration-line" ->
        lines = split_css_tokens(normalized)

        cond do
          lines == ["none"] ->
            {:ok, %{text_decoration_line: []}}

          lines != [] and length(lines) == length(Enum.uniq(lines)) and
              Enum.all?(lines, &(&1 in ~w(underline overline line-through))) ->
            {:ok,
             %{
               text_decoration_line:
                 Enum.map(lines, fn line ->
                   case line do
                     "underline" -> :underline
                     "overline" -> :overline
                     "line-through" -> :line_through
                   end
                 end)
             }}

          true ->
            :error
        end

      "text-decoration-style" ->
        case normalized do
          style when style in ~w(solid dotted dashed) ->
            {:ok, %{text_decoration_style: String.to_existing_atom(style)}}

          _ ->
            :error
        end

      "text-decoration-color" ->
        case normalized do
          "currentcolor" ->
            {:ok, %{text_decoration_color: :current_color}}

          _ ->
            with {:ok, parsed} <- parse_css_color(normalized, color),
                 do: {:ok, %{text_decoration_color: parsed}}
        end

      "text-decoration-thickness" ->
        case normalized do
          "auto" ->
            {:ok, %{text_decoration_thickness: :auto}}

          "from-font" ->
            {:ok, %{text_decoration_thickness: :from_font}}

          _ ->
            em? = String.ends_with?(normalized, "em") and not String.ends_with?(normalized, "rem")

            result =
              case parse_css_number(normalized) do
                {number, "%"} when number >= 0 -> {:ok, {:percent, number}}
                _ -> parse_css_length(normalized, :nonnegative, if(em?, do: 1, else: font_size))
              end

            with {:ok, thickness} <- result do
              {:ok, %{text_decoration_thickness: if(em?, do: {:em, thickness}, else: thickness)}}
            end
        end

      "text-decoration" ->
        tokens = split_css_tokens(normalized)

        Enum.reduce_while(
          tokens,
          {:ok,
           %{
             text_decoration_line: [],
             text_decoration_color: :current_color,
             text_decoration_thickness: :auto,
             text_decoration_style: :solid
           }, MapSet.new()},
          fn token, {:ok, result, seen} ->
            parsed =
              cond do
                token in ~w(none underline overline line-through) ->
                  parse_css_text_decoration("text-decoration-line", token, font_size, color)

                token in ~w(solid dotted dashed) ->
                  parse_css_text_decoration("text-decoration-style", token, font_size, color)

                match?(
                  {:ok, _},
                  parse_css_text_decoration("text-decoration-thickness", token, font_size, color)
                ) ->
                  parse_css_text_decoration("text-decoration-thickness", token, font_size, color)

                true ->
                  parse_css_text_decoration("text-decoration-color", token, font_size, color)
              end

            case parsed do
              {:ok, %{text_decoration_line: lines}} ->
                existing = result.text_decoration_line

                if MapSet.member?(seen, token) or (MapSet.member?(seen, "none") and lines != []) or
                     (token == "none" and existing != []) do
                  {:halt, :error}
                else
                  {:cont,
                   {:ok, %{result | text_decoration_line: existing ++ lines},
                    MapSet.put(seen, token)}}
                end

              {:ok, properties} ->
                key = hd(Map.keys(properties))

                if MapSet.member?(seen, key),
                  do: {:halt, :error},
                  else: {:cont, {:ok, Map.merge(result, properties), MapSet.put(seen, key)}}

              :error ->
                {:halt, :error}
            end
          end
        )
        |> case do
          {:ok, result, _seen} when tokens != [] -> {:ok, result}
          _ -> :error
        end

      _ ->
        :error
    end
  end

  @doc false
  @spec resolve_text_decorations(map()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def resolve_text_decorations(style) do
    # Ruby containers decorate their bases. Real annotations start a separate
    # scope; authored display overrides already remove the annotation role.
    style =
      if Map.get(style, :display) in [:inline_block, :inline_flex, :inline_grid] or
           Map.get(style, :position) in [:absolute, :fixed] or
           Map.get(style, :_ruby_annotation, false),
         do: Map.delete(style, :_text_decorations),
         else: style

    case Map.get(style, :text_decoration_line, []) do
      [] ->
        {:ok, style}

      lines ->
        font = style.font_face

        case FontValidator.prepare_text_metrics(font) do
          {:ok, metrics} ->
            size = style.font_size
            scale = size / metrics.units_per_em
            # CSS fixes one px at 0.75 PDF points. These UA paint rules match
            # pinned Chromium: auto thickness is 1/10 em, whole-pixel strokes,
            # a thickness-dependent underline gap, and ascent-based strikes.
            configured = Map.get(style, :text_decoration_thickness, :auto)

            thickness =
              case configured do
                :auto ->
                  size / 10

                :from_font ->
                  if metrics.underline, do: metrics.underline.thickness * scale, else: size / 10

                {:percent, percent} ->
                  size * percent / 100

                {:em, multiplier} ->
                  size * multiplier

                thickness ->
                  thickness
              end

            thickness =
              if configured in [:auto, :from_font],
                do: max(0.75, Float.floor(thickness / 0.75) * 0.75),
                else: max(0.75, round(thickness / 0.75) * 0.75)

            color =
              case Map.get(style, :text_decoration_color, :current_color) do
                :current_color -> style.color
                color -> color
              end

            ascent = font.ascent * scale

            decoration = %{
              lines: lines,
              color: color,
              thickness: thickness,
              style: Map.get(style, :text_decoration_style, :solid),
              underline: -max(1, Float.ceil(thickness / 0.75 / 2)) * 0.75,
              overline: round(ascent / 0.75) * 0.75,
              strike: ascent / 3
            }

            decorations = Map.get(style, :_text_decorations, []) ++ [decoration]

            if valid_text_decorations?(decorations),
              do: {:ok, Map.put(style, :_text_decorations, decorations)},
              else:
                Diagnostics.error(
                  :style,
                  :invalid_document,
                  "text decoration geometry or color is invalid"
                )

          :error ->
            Diagnostics.error(
              :font,
              :invalid_document,
              "text decorations require valid selected-font metrics",
              source: Map.get(font, :family),
              module: FontValidator
            )
        end
    end
  end

  @doc false
  @spec parse_css_attribute_selector(String.t()) :: {:ok, tuple(), String.t()} | :error
  def parse_css_attribute_selector(source) do
    case Regex.named_captures(@css_attribute_selector_regex, source) do
      %{"name" => name, "operator" => operator, "value" => value, "flag" => flag, "rest" => rest} ->
        decoded_flag = decode_css_escapes(flag)

        if decoded_flag in ["", "i", "I", "s", "S"] do
          name = name |> decode_css_escapes() |> String.downcase(:ascii)

          attribute =
            case operator do
              "" ->
                {:present, name}

              operator ->
                operator =
                  case operator do
                    "=" -> :equals
                    "~=" -> :includes
                    "|=" -> :dash_match
                    "^=" -> :prefix
                    "$=" -> :suffix
                    "*=" -> :substring
                  end

                value = decode_css_value(value)

                case decoded_flag do
                  "" -> {operator, name, value}
                  flag when flag in ["i", "I"] -> {operator, name, value, :insensitive}
                  _ -> {operator, name, value, :sensitive}
                end
            end

          {:ok, attribute, rest}
        else
          :error
        end

      nil ->
        :error
    end
  end

  @doc false
  @spec attribute_selector_matches?(tuple(), map()) :: boolean()
  def attribute_selector_matches?(selector, node) do
    attributes = Map.fetch!(node, :attributes)

    case selector do
      {:present, name} ->
        Map.has_key?(attributes, name)

      selector ->
        {operator, name, value, mode} =
          case selector do
            {operator, name, value} -> {operator, name, value, :default}
            {operator, name, value, mode} -> {operator, name, value, mode}
          end

        case Map.fetch(attributes, name) do
          {:ok, actual} when operator == :equals and actual == value ->
            true

          {:ok, _actual}
          when operator in [:includes, :prefix, :suffix, :substring] and value == "" ->
            false

          {:ok, actual} ->
            namespace = Map.get(node, :namespace, :html)
            attribute_namespace = Map.get(Map.get(node, :attribute_namespaces, %{}), name)

            insensitive? =
              mode == :insensitive or
                (mode == :default and namespace == :html and is_nil(attribute_namespace) and
                   name in @case_insensitive_html_attributes)

            if insensitive? or operator != :equals do
              reserve_render_resource(:max_css_work, byte_size(actual) + byte_size(value), :css)
            end

            {actual, value} =
              if insensitive?,
                do: {String.downcase(actual, :ascii), String.downcase(value, :ascii)},
                else: {actual, value}

            case operator do
              :equals ->
                actual == value

              :includes ->
                not Regex.match?(~r/[\x20\t\r\n\f]/u, value) and
                  value in String.split(actual, ~r/[\x20\t\r\n\f]+/u, trim: true)

              :dash_match ->
                actual == value or String.starts_with?(actual, value <> "-")

              :prefix ->
                String.starts_with?(actual, value)

              :suffix ->
                String.ends_with?(actual, value)

              :substring ->
                String.contains?(actual, value)
            end

          :error ->
            false
        end
    end
  end

  @doc false
  @spec parse_css_text_wrapping(String.t(), String.t()) :: {:ok, atom(), atom()} | :error
  def parse_css_text_wrapping(property, value) do
    case {property, value |> String.trim() |> String.downcase()} do
      {"white-space", "normal"} ->
        {:ok, :white_space, :normal}

      {"white-space", "pre-line"} ->
        {:ok, :white_space, :pre_line}

      {"white-space", "nowrap"} ->
        {:ok, :white_space, :nowrap}

      {"white-space", "pre"} ->
        {:ok, :white_space, :pre}

      {"white-space", "pre-wrap"} ->
        {:ok, :white_space, :pre_wrap}

      {"white-space", "break-spaces"} ->
        {:ok, :white_space, :break_spaces}

      {"line-break", value} when value in ["auto", "normal"] ->
        {:ok, :line_break, :normal}

      {"line-break", "anywhere"} ->
        {:ok, :line_break, :anywhere}

      {property, "break-word"} when property in ~w(word-wrap overflow-wrap) ->
        {:ok, :overflow_wrap, :break_word}

      {property, "anywhere"} when property in ~w(word-wrap overflow-wrap) ->
        {:ok, :overflow_wrap, :anywhere}

      {property, "normal"} when property in ~w(word-wrap overflow-wrap) ->
        {:ok, :overflow_wrap, :normal}

      {"word-break", "break-word"} ->
        {:ok, :word_break, :break_word}

      {"word-break", "normal"} ->
        {:ok, :word_break, :normal}

      _ ->
        :error
    end
  end

  @doc false
  @spec valid_html_element?(String.t(), map(), list()) :: boolean()
  def valid_html_element?(tag, attributes, children) do
    children =
      if tag in (@void_tags ++ ~w(style script title xmp plaintext noembed noframes)),
        do: children,
        else: html_content_nodes(children)

    case tag do
      "head" ->
        Enum.all?(
          children,
          &match?(
            %{tag: tag}
            when tag in [
                   "base",
                   "basefont",
                   "bgsound",
                   "style",
                   "meta",
                   "title",
                   "script",
                   "template",
                   "noscript",
                   "noframes"
                 ],
            &1
          )
        )

      "noscript" ->
        valid_noscript_element?(children, nil, [])

      "template" ->
        true

      tag when tag in ["a", "canvas"] ->
        valid_noninteractive_children?(children, tag)

      "frameset" ->
        Enum.all?(children, &match?(%{tag: tag} when tag in ~w(frame frameset noframes), &1))

      tag when tag in ~w(style script iframe xmp plaintext noembed noframes) ->
        Enum.all?(children, &match?(%{type: :text}, &1))

      "title" ->
        Enum.all?(children, &match?(%{type: :text}, &1))

      tag when tag in @form_control_tags or tag in ["option", "optgroup", "output", "datalist"] ->
        valid_form_element?(tag, attributes, children)

      tag when tag in @void_tags ->
        children == []

      tag when tag in @list_tags ->
        Enum.all?(children, &(match?(%{tag: "li"}, &1) or script_supporting_element?(&1)))

      "dl" ->
        case Enum.any?(children, &match?(%{tag: "div"}, &1)) do
          true ->
            Enum.all?(children, fn child ->
              script_supporting_element?(child) or
                (match?(%{tag: "div"}, child) and description_groups_valid?(child.children))
            end)

          false ->
            description_groups_valid?(children)
        end

      "picture" ->
        NativeElixirPdfUtilities.Validators.ResponsiveImageValidator.valid_picture_children?(
          children
        )

      "figure" ->
        meaningful =
          Enum.reject(children, fn child ->
            match?(%{type: :text}, child) and String.trim(child.text) == ""
          end)

        captions = Enum.filter(meaningful, &match?(%{tag: "figcaption"}, &1))

        captions == [] or
          (length(captions) == 1 and
             (hd(captions) == List.first(meaningful) or hd(captions) == List.last(meaningful)))

      "caption" ->
        descendants_exclude_html_tag?(children, "table")

      "table" ->
        table_children_valid?(children)

      "colgroup" ->
        span? = Map.has_key?(attributes, "span")

        (not span? and Enum.all?(children, &match?(%{tag: tag} when tag in ~w(col template), &1))) or
          (children == [] and span?)

      tag when tag in ~w(thead tbody tfoot) ->
        Enum.all?(children, &(match?(%{tag: "tr"}, &1) or script_supporting_element?(&1)))

      "tr" ->
        Enum.all?(
          children,
          &(match?(%{tag: tag} when tag in ["th", "td"], &1) or script_supporting_element?(&1))
        )

      _ ->
        true
    end
  end

  @doc false
  @spec valid_noscript_element?(list(), term(), list()) :: boolean()
  def valid_noscript_element?(children, context, ancestors) do
    children = html_content_nodes(children)

    Enum.all?(children, fn child ->
      case child do
        %{type: :text, text: text} ->
          context != "head" or Regex.match?(~r/^[ \t\n\f\r]*$/, text)

        %{type: :element, tag: tag} ->
          if context == "head",
            do: tag in ~w(style meta),
            else: valid_html_child?(context || :document, tag, ancestors)

        _ ->
          false
      end
    end) and descendants_exclude_html_tag?(children, "noscript")
  end

  # Template fragments are separate trees, not descendants of their template.
  defp descendants_exclude_html_tag?(nodes, excluded_tag) do
    Enum.all?(nodes, fn node ->
      case node do
        %{type: :element, tag: tag, children: children} ->
          not (tag == excluded_tag and Map.get(node, :namespace, :html) == :html) and
            descendants_exclude_html_tag?(children, excluded_tag)

        _ ->
          true
      end
    end)
  end

  defp description_groups_valid?(children) do
    Enum.reduce_while(children, :empty, fn child, state ->
      cond do
        script_supporting_element?(child) -> {:cont, state}
        match?(%{tag: "dt"}, child) -> {:cont, :term}
        match?(%{tag: "dd"}, child) and state in [:term, :description] -> {:cont, :description}
        true -> {:halt, :invalid}
      end
    end) in [:empty, :description]
  end

  defp script_supporting_element?(node) do
    match?(%{type: :element, tag: tag} when tag in ~w(script template), node) and
      Map.get(node, :namespace, :html) == :html
  end

  defp table_children_valid?(children) do
    Enum.reduce_while(children, :start, fn child, state ->
      cond do
        script_supporting_element?(child) ->
          {:cont, state}

        match?(%{tag: "caption"}, child) and state == :start ->
          {:cont, :caption}

        match?(%{tag: tag} when tag in ~w(colgroup col), child) and
            state in [:start, :caption, :columns] ->
          {:cont, :columns}

        match?(%{tag: "thead"}, child) and state in [:start, :caption, :columns] ->
          {:cont, :header}

        match?(%{tag: tag} when tag in ~w(tr tbody), child) and state != :footer ->
          {:cont, :rows}

        match?(%{tag: "tfoot"}, child) and state != :footer ->
          {:cont, :footer}

        true ->
          {:halt, :invalid}
      end
    end) != :invalid
  end

  @doc false
  @spec html_insertion_action(term(), String.t() | :eof, String.t() | nil, boolean()) ::
          :process | :close | {:imply, String.t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def html_insertion_action(context, token, previous, implied?) do
    # This describes strict, valid-input insertion modes. Recovery, foster
    # parenting and duplicate structural tags remain validator diagnostics.
    next =
      case token do
        :eof ->
          :eof

        token ->
          # Classification and ASCII tag folding can each scan the token.
          reserve_render_resource(:max_html_tree_work, 2 * byte_size(token), :html)

          cond do
            String.starts_with?(token, "<?") or String.starts_with?(token, "<!") ->
              :metadata

            Regex.match?(~r/\A[ \t\n\f\r]*\z/, token) ->
              :whitespace

            true ->
              case Regex.run(~r/\A<(\/?)([a-zA-Z][^ \t\n\f\r\x00\/>]*)/, token) do
                [_, "", tag] -> {:start, String.downcase(tag, :ascii)}
                [_, "/", tag] -> {:end, String.downcase(tag, :ascii)}
                _ -> :text
              end
          end
      end

    cond do
      next in [:metadata, :whitespace] ->
        :process

      context in [:document, :after_doctype] and previous == nil and
          next != {:start, "html"} ->
        {:imply, "html"}

      context == "html" and previous == nil and next != {:start, "head"} ->
        {:imply, "head"}

      context == "html" and
          ((previous == "head" and next != {:start, "noscript"} and
              next in [{:start, "head"} | @head_starts]) or
             (previous == "body" and
                next in [{:start, "head"}, {:start, "body"}, {:start, "title"}])) ->
        Diagnostics.error(
          :html,
          :unsupported_html,
          "document head and body must occur once in order; place title in the head and printed content in the body",
          module: __MODULE__,
          source: token
        )

      context == "html" and previous == "head" and
          next not in [{:start, "body"}, {:start, "frameset"}] ->
        {:imply, "body"}

      context == "head" and implied? and next in [:text, {:start, "noscript"}] ->
        # Keep the established wrapper-free root noscript body selection. An
        # authored head still gives noscript its strict metadata-only context.
        :close

      context == "table" and next == {:start, "col"} ->
        {:imply, "colgroup"}

      context == "table" and next == {:start, "tr"} ->
        {:imply, "tbody"}

      true ->
        :process
    end
  end

  @doc false
  @spec validate_constructed_html_document(map()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_constructed_html_document(document) do
    # Construction is the shared source of document-section rules. A parser
    # whose parents already exist must return its canonical tree unchanged.
    case construct_html_document(document) do
      {:ok, ^document} ->
        :ok

      {:ok, _changed} ->
        Diagnostics.error(
          :html,
          :unsupported_html,
          "document head and body must occur once in order; place title in the head and printed content in the body",
          module: __MODULE__
        )

      error ->
        error
    end
  end

  @doc false
  @spec construct_html_document(map()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def construct_html_document(dom) do
    case html_content_nodes(dom.children) do
      [%{tag: "html"} = html] ->
        with {:ok, children} <- document_sections(html.children) do
          children =
            Enum.map(dom.children, fn node ->
              if node == html, do: %{html | children: children}, else: node
            end)

          {:ok, %{dom | children: children}}
        end

      children ->
        case Enum.any?(children, &match?(%{tag: "html"}, &1)) do
          true ->
            Diagnostics.error(
              :html,
              :unsupported_html,
              "a document must contain one html element enclosing its head and body; remove sibling or duplicate html elements"
            )

          false ->
            {leading, content} =
              Enum.split_while(dom.children, &match?(%{type: :processing_instruction}, &1))

            with {:ok, children} <- document_sections(content) do
              reserve_render_resource(:max_html_nodes, 1, :html)

              html = %{
                type: :element,
                tag: "html",
                attributes: %{},
                children: children
              }

              {:ok, %{dom | children: leading ++ [html]}}
            end
        end
    end
  end

  defp document_sections(children) do
    {leading, children} =
      Enum.split_while(children, &match?(%{type: :processing_instruction}, &1))

    {head_content, body_content} =
      Enum.split_while(children, fn child ->
        match?(%{type: :processing_instruction}, child) or
          match?(
            %{tag: tag}
            when tag in ~w(head base basefont bgsound style meta title script template),
            child
          )
      end)

    {head, head_nodes} =
      case html_content_nodes(head_content) do
        [%{tag: "head"} = head] ->
          {head, head_content}

        _ ->
          reserve_render_resource(:max_html_nodes, 1, :html)
          head = %{type: :element, tag: "head", attributes: %{}, children: head_content}
          {head, [head]}
      end

    {body, body_nodes} =
      case html_content_nodes(body_content) do
        [%{tag: tag} = body] when tag in ["body", "frameset"] ->
          {body, body_content}

        _ ->
          reserve_render_resource(:max_html_nodes, 1, :html)
          body = %{type: :element, tag: "body", attributes: %{}, children: body_content}
          {body, [body]}
      end

    valid? =
      not Enum.any?(head.children, &match?(%{tag: tag} when tag in ~w(html head body), &1)) and
        not Enum.any?(
          body.children,
          &match?(%{tag: tag} when tag in ~w(html head body title), &1)
        )

    case valid? do
      true ->
        {:ok, leading ++ head_nodes ++ body_nodes}

      false ->
        Diagnostics.error(
          :html,
          :unsupported_html,
          "document head and body must occur once in order; place title in the head and printed content in the body"
        )
    end
  end

  @doc false
  @spec html_text_action(atom() | String.t(), String.t()) :: :ignore | :text | :invalid
  def html_text_action(context, text) do
    case context in [
           :document,
           :after_doctype,
           :after_head,
           :after_body,
           :after_html,
           "html",
           "head",
           "body",
           "frameset",
           "ul",
           "ol",
           "dl",
           "table",
           "colgroup",
           "thead",
           "tbody",
           "tfoot",
           "tr",
           "select",
           "optgroup"
         ] do
      false ->
        :text

      true ->
        case Regex.match?(~r/^[ \t\n\f\r]*$/, text) do
          true ->
            :ignore

          false ->
            if context in [:document, :after_doctype, :after_head, "html", "body"],
              do: :text,
              else: :invalid
        end
    end
  end

  @doc false
  @spec html_implied_start_closure(atom() | String.t(), String.t(), list()) ::
          atom() | tuple() | nil
  def html_implied_start_closure(context, tag, children) do
    cond do
      context in [:document, :after_doctype] and tag == "head" and
          Enum.all?(
            html_content_nodes(children),
            &match?(
              %{tag: tag} when tag in ~w(base basefont bgsound style meta title script template),
              &1
            )
          ) ->
        :after_head

      context in [:document, :after_doctype, :after_head] and tag == "body" and
          not Enum.any?(children, &match?(%{tag: tag} when tag in ~w(html body), &1)) ->
        :after_body

      context in [:document, :after_doctype, :after_head, :after_body] and tag == "html" and
          not Enum.any?(children, &match?(%{tag: "html"}, &1)) ->
        :after_html

      context == "table" and tag in ~w(tbody colgroup) ->
        child_tag = if tag == "tbody", do: "tr", else: "col"

        {group_children, previous} =
          Enum.split_while(children, fn child ->
            Map.get(child, :tag) == child_tag or
              match?(%{type: :processing_instruction}, child) or
              (script_supporting_element?(child) and
                 (tag == "tbody" or Map.get(child, :tag) == "template"))
          end)

        if Enum.any?(group_children, &(Map.get(&1, :tag) == child_tag)),
          do: {:group, tag, Enum.reverse(group_children), previous},
          else: nil

      true ->
        nil
    end
  end

  @doc false
  @spec validate_html_doctype(String.t(), atom() | String.t(), list()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_html_doctype(token, context, children) do
    valid? =
      context == :document and html_content_nodes(children) == [] and
        Regex.match?(
          ~r/^<!doctype[ \t\n\f\r]+html(?:[ \t\n\f\r]+system[ \t\n\f\r]+(?:"about:legacy-compat"|'about:legacy-compat'))?[ \t\n\f\r]*>$/i,
          token
        )

    if valid?,
      do: :ok,
      else:
        Diagnostics.error(
          :html,
          :unsupported_html,
          "use one HTML doctype before document content; legacy quirks doctypes and XML declarations are unsupported",
          source: token
        )
  end

  @doc false
  @spec construct_html_group(String.t(), list()) :: map()
  def construct_html_group(tag, children) do
    reserve_render_resource(:max_html_nodes, 1, :html)
    %{type: :element, tag: tag, attributes: %{}, children: children}
  end

  @doc false
  @spec html_parent_stack(atom() | String.t(), String.t(), list()) :: list()
  def html_parent_stack(context, tag, ancestors) do
    cond do
      context == "table" and tag == "tr" ->
        ["tbody", context | ancestors]

      context in [:document, :after_doctype, :after_head, "html"] and
          tag not in ~w(html head body frameset) ->
        if tag in ~w(base basefont bgsound style meta title script template),
          do: ["head", "html", context | ancestors],
          else: ["body", "html", context | ancestors]

      context in [:document, :after_doctype, :after_head] and tag in ~w(head body frameset) ->
        ["html", context | ancestors]

      true ->
        [context | ancestors]
    end
  end

  @doc false
  @spec construct_html_table_groups(list()) :: list()
  def construct_html_table_groups(children) do
    {groups, _pending} =
      Enum.reduce(children, {[], nil}, fn child, {groups, pending} ->
        kind =
          case Map.get(child, :type) do
            :processing_instruction when pending in ~w(tbody colgroup) ->
              pending

            _ ->
              case Map.get(child, :tag) do
                "tr" -> "tbody"
                "col" -> "colgroup"
                "template" when pending in ~w(tbody colgroup) -> pending
                "script" when pending == "tbody" -> pending
                _ -> nil
              end
          end

        case {kind, pending, groups} do
          {nil, _, _} ->
            {[{child, false} | groups], nil}

          {kind, kind, [{group, true} | rest]} ->
            {[{%{group | children: [child | group.children]}, true} | rest], kind}

          {kind, _, _} ->
            reserve_render_resource(:max_html_nodes, 1, :html)
            group = %{type: :element, tag: kind, attributes: %{}, children: [child]}
            {[{group, true} | groups], kind}
        end
      end)

    groups
    |> Enum.reverse()
    |> Enum.map(fn {group, generated?} ->
      if generated?, do: %{group | children: Enum.reverse(group.children)}, else: group
    end)
  end

  @doc false
  @spec validate_html_fragment_context(term()) ::
          {:ok, String.t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_html_fragment_context(context) do
    normalized = if is_binary(context), do: String.downcase(context), else: nil

    case supported_html_tag?(normalized) and normalized not in @void_tags and normalized != "html" do
      true ->
        {:ok, normalized}

      false ->
        NativeElixirPdfUtilities.Validators.ForeignContentValidator.prepare_fragment_context(
          normalized || context
        )
    end
  end

  # HTML-only tokenizer modes. Foreign contexts retain their namespace's data
  # rules, including elements whose local names happen to match these tags.
  @doc false
  @spec html_text_mode(term()) :: :data | :raw_text | :rcdata | :plaintext
  def html_text_mode(context) do
    case context do
      tag when tag in ~w(style script iframe xmp noembed noframes) -> :raw_text
      tag when tag in ~w(title textarea) -> :rcdata
      "plaintext" -> :plaintext
      _ -> :data
    end
  end

  @doc false
  @spec noembed_render_style(map()) :: map()
  def noembed_render_style(style) do
    # The pinned UA stylesheet specifies display:none. Chromium additionally
    # creates no noembed layout object even when author CSS overrides display.
    # Preserve that static selection, and never load its unused background.
    style |> Map.put(:display, :none) |> Map.delete(:background_image_source)
  end

  @doc false
  @spec html_text_end_match(String.t(), term()) :: {non_neg_integer(), non_neg_integer()} | nil
  def html_text_end_match(source, context) do
    case context do
      "script" ->
        case script_data_end_offset(source) do
          nil -> nil
          offset -> {offset, 0}
        end

      tag when tag in ~w(style title textarea iframe xmp noembed noframes) ->
        pattern =
          case tag do
            "iframe" -> ~r/<\/iframe(?=[ \t\n\f\r\/>])/i
            "noframes" -> ~r/<\/noframes(?=[ \t\n\f\r\/>])/i
            "style" -> ~r/<\/style(?=[ \t\n\f\r\/>])/i
            "title" -> ~r/<\/title(?=[ \t\n\f\r\/>])/i
            "textarea" -> ~r/<\/textarea(?=[ \t\n\f\r\/>])/i
            "xmp" -> ~r/<\/xmp(?=[ \t\n\f\r\/>])/i
            "noembed" -> ~r/<\/noembed(?=[ \t\n\f\r\/>])/i
          end

        case Regex.run(pattern, source, return: :index) do
          [match] -> match
          nil -> nil
        end

      _ ->
        nil
    end
  end

  @doc false
  @spec html_text_token_action(term(), String.t()) :: :text | :end | :invalid
  def html_text_token_action(context, token) do
    case html_text_end_match(token, context) do
      {0, _length} ->
        # Appropriate end tags with attributes or a self-closing flag are parse
        # errors. Keep the strict syntax policy rather than painting them as text.
        if Regex.match?(~r/\A<\/[a-zA-Z]+[ \t\n\f\r]*>\z/, token),
          do: :end,
          else: :invalid

      _ ->
        :text
    end
  end

  @doc false
  @spec html_eof_end_tag_allowed?(term(), term(), list(), list()) :: boolean()
  def html_eof_end_tag_allowed?(context, tag, children, ancestors) do
    # A plaintext start tag makes all following source literal, including every
    # authored ancestor end tag. EOF must therefore finish those open ancestors.
    plaintext? =
      context == "plaintext" or
        children
        |> List.last()
        |> Stream.iterate(fn
          %{type: :element, tag: "template", content: %{children: children}} ->
            List.last(children)

          %{type: :element, children: children} ->
            List.last(children)

          _ ->
            nil
        end)
        |> Enum.take_while(&(not is_nil(&1)))
        |> Enum.any?(
          &match?(
            %{type: :element, tag: "plaintext", namespace: :html},
            Map.put_new(&1, :namespace, :html)
          )
        )

    plaintext? or omitted_html_end_tag?(tag, :eof, ancestors)
  end

  @doc false
  @spec html_void_tag?(String.t()) :: boolean()
  def html_void_tag?(tag), do: tag in @void_tags

  @doc false
  @spec omitted_html_end_tag?(atom() | String.t(), term(), list()) :: boolean()
  def omitted_html_end_tag?(tag, next, ancestors) do
    case tag in @optional_end_tags do
      false ->
        false

      true ->
        matching_end? = next == {:end, tag}
        parent = List.first(ancestors)

        ruby_in_scope? =
          if tag in ~w(rb rt rp rtc) do
            Enum.reduce_while(ancestors, false, fn context, _ ->
              case context do
                "ruby" ->
                  {:halt, true}

                boundary
                when boundary in ~w(applet caption html table td th marquee object template) ->
                  {:halt, false}

                {:foreign, :mathml, boundary, _}
                when boundary in ~w(mi mo mn ms mtext annotation-xml) ->
                  {:halt, false}

                {:foreign, :svg, boundary, _} when boundary in ~w(foreignObject desc title) ->
                  {:halt, false}

                _ ->
                  {:cont, false}
              end
            end)
          else
            false
          end

        parent_end? =
          next == :eof or
            (match?({:end, _}, next) and
               Enum.any?(
                 ancestors,
                 &(NativeElixirPdfUtilities.Validators.ForeignContentValidator.context_tag(&1) ==
                     elem(next, 1))
               )) or
            (not matching_end? and parent in @optional_end_tags and
               omitted_html_end_tag?(parent, next, Enum.drop(ancestors, 1)))

        not matching_end? and
          case tag do
            "html" ->
              next == :eof

            "head" ->
              next not in [:comment, :whitespace] and
                next not in @head_starts

            "body" ->
              parent_end? and next != :comment

            "p" ->
              next in @paragraph_following_starts or
                (parent_end? and parent not in ~w(a audio del ins map noscript video) and
                   not (is_binary(parent) and String.contains?(parent, "-")))

            "li" ->
              next == {:start, "li"} or parent_end?

            "dt" ->
              next in [{:start, "dt"}, {:start, "dd"}]

            "dd" ->
              next in [{:start, "dt"}, {:start, "dd"}] or parent_end?

            tag when tag in ~w(rb rt rp) ->
              (ruby_in_scope? and
                 next in [{:start, "rb"}, {:start, "rt"}, {:start, "rp"}, {:start, "rtc"}]) or
                parent_end?

            "rtc" ->
              (ruby_in_scope? and next in [{:start, "rb"}, {:start, "rtc"}]) or parent_end?

            "option" ->
              next in [{:start, "option"}, {:start, "optgroup"}, {:start, "hr"}] or parent_end?

            "optgroup" ->
              next in [{:start, "optgroup"}, {:start, "hr"}] or parent_end?

            "caption" ->
              next not in [:comment, :whitespace] and
                (parent_end? or next in @table_starts)

            "colgroup" ->
              next not in [:comment, :whitespace, {:start, "col"}, {:start, "template"}]

            "thead" ->
              next in [{:start, "tbody"}, {:start, "tfoot"}]

            "tbody" ->
              next in [{:start, "tbody"}, {:start, "tfoot"}] or parent_end?

            "tfoot" ->
              parent_end?

            "tr" ->
              next == {:start, "tr"} or parent_end?

            tag when tag in ["td", "th"] ->
              next in [{:start, "td"}, {:start, "th"}] or parent_end?
          end
    end
  end

  # PI target vocabulary and data boundaries are fixed HTML syntax, not resource limits.
  @processing_instruction_target ~r/\A[A-Za-z_][A-Za-z0-9_-]*\z/
  @processing_instruction_token ~r/\A<\?(?<target>[A-Za-z_][A-Za-z0-9_-]*)(?<tail>[ \t\n\f\r][^>]*|\??)>\z/

  @doc false
  @spec validate_html_processing_instruction(term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_html_processing_instruction(value) do
    with_render_budget(fn ->
      {node, stage, source} =
        case value do
          token when is_binary(token) ->
            check_render_resource(:max_html_source_bytes, byte_size(token), :html)
            reserve_render_resource(:max_html_tree_work, byte_size(token), :html)

            node =
              case Regex.named_captures(@processing_instruction_token, token) do
                %{"target" => target, "tail" => tail} ->
                  data = Regex.replace(~r/\A[ \t\n\f\r]+/, tail, "")

                  data =
                    if String.ends_with?(data, "?"),
                      do: binary_part(data, 0, byte_size(data) - 1),
                      else: data

                  %{type: :processing_instruction, target: target, data: data}

                _ ->
                  nil
              end

            {node, :html, token}

          node when is_map(node) ->
            {node, :style, "processing_instruction"}

          _ ->
            {nil, :style, "processing_instruction"}
        end

      case valid_processing_instruction_node?(node) do
        true ->
          check_render_resource(
            :max_html_source_bytes,
            byte_size(node.target) + byte_size(node.data),
            :html
          )

          {:ok, node}

        false ->
          Diagnostics.error(
            stage,
            if(stage == :html, do: :unsupported_html, else: :invalid_document),
            "processing instructions require an ASCII letter or underscore target followed by letters, digits, hyphens or underscores, literal data and a > or ?> terminator; xml and xml-stylesheet declarations are unsupported",
            module: __MODULE__,
            source: source
          )
      end
    end)
  end

  defp valid_processing_instruction_node?(node) do
    case node do
      %{type: :processing_instruction, target: target, data: data}
      when is_binary(target) and is_binary(data) ->
        allowed_keys = [
          :type,
          :target,
          :data,
          :node_id,
          :source,
          :document_id,
          :tree_scope_id,
          :parent_id,
          :composed_parent_id,
          :shadow_scope_id,
          :shadow_host_id
        ]

        String.valid?(target) and String.valid?(data) and
          Regex.match?(@processing_instruction_target, target) and
          String.downcase(target, :ascii) not in ["xml", "xml-stylesheet"] and
          not String.contains?(data, [">", <<0>>]) and not String.ends_with?(data, "?") and
          Enum.all?(Map.keys(node), &(&1 in allowed_keys)) and
          NativeElixirPdfUtilities.Validators.HtmlTreeValidator.validate_node_metadata(node) ==
            :ok and
          (not Map.has_key?(node, :node_id) or is_reference(node.node_id)) and
          case Map.fetch(node, :source) do
            :error ->
              true

            {:ok, %{text: text, line: line, column: column} = source}
            when is_binary(text) and is_integer(line) and line > 0 and is_integer(column) and
                   column > 0 ->
              reserve_render_resource(:max_html_tree_work, 2 * byte_size(text), :html)

              map_size(source) == 3 and String.valid?(text, :fast_ascii) and
                case validate_html_processing_instruction(String.replace(text, ~r/\r\n?/, "\n")) do
                  {:ok, parsed} -> parsed.target == target and parsed.data == data
                  _ -> false
                end

            _ ->
              false
          end

      _ ->
        false
    end
  end

  # PIs occupy authored tree positions but do not participate in element content models.
  @doc false
  @spec html_content_nodes([map()]) :: [map()]
  def html_content_nodes(children),
    do: Enum.reject(children, &match?(%{type: :processing_instruction}, &1))

  @doc false
  @spec validate_html_comment(String.t()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_html_comment(token) do
    content_size = byte_size(token) - 7

    valid? =
      content_size >= 0 and String.ends_with?(token, "-->") and
        then(binary_part(token, 4, content_size), fn content ->
          not String.starts_with?(content, [">", "->"]) and
            not String.contains?(content, ["<!--", "-->", "--!>"]) and
            not String.ends_with?(content, "<!-")
        end)

    case valid? do
      true ->
        :ok

      false ->
        Diagnostics.error(
          :html,
          :unsupported_html,
          "HTML comment is malformed or unterminated; use <!-- text --> without nested comment delimiters",
          source: token
        )
    end
  end

  @doc false
  @spec preprocess_html(String.t()) :: String.t()
  def preprocess_html(html) do
    if String.contains?(html, "\r"), do: String.replace(html, ~r/\r\n?/, "\n"), else: html
  end

  @doc false
  @spec html_attribute_captures(String.t(), keyword()) :: list()
  def html_attribute_captures(attributes, options \\ []) do
    Regex.scan(
      ~r/[ \t\n\f\r]+([^ \t\n\f\r\x00\/="'<>]+)(?:[ \t\n\f\r]*=[ \t\n\f\r]*("[^"]*"|'[^']*'|[^ \t\n\f\r"'`=<>]+))?/u,
      attributes,
      options
    )
  end

  @doc false
  @spec validate_html_attributes(String.t(), String.t(), :html | :mathml | :svg) ::
          {:ok, map(), boolean()} | {:error, :unsupported_html}
  def validate_html_attributes(attributes, tag, namespace \\ :html) do
    case prepare_html_attributes(attributes, tag, namespace) do
      {:ok, prepared} -> {:ok, prepared.attributes, prepared.self_closing?}
      error -> error
    end
  end

  @doc false
  @spec prepare_html_attributes(String.t(), String.t(), :html | :mathml | :svg) ::
          {:ok, map()} | {:error, :unsupported_html}
  def prepare_html_attributes(attributes, tag, namespace) do
    case attributes do
      "" ->
        {:ok, %{attributes: %{}, attribute_order: [], self_closing?: false}}

      attributes ->
        captures =
          html_attribute_captures(attributes)

        parsed_source = Enum.map_join(captures, "", &List.first/1)

        remainder =
          binary_part(
            attributes,
            byte_size(parsed_source),
            byte_size(attributes) - byte_size(parsed_source)
          )

        with true <- String.starts_with?(attributes, parsed_source),
             true <- Regex.match?(~r/^[ \t\n\f\r]*\/?[ \t\n\f\r]*$/, remainder),
             false <-
               namespace == :html and tag in ~w(xmp plaintext noembed) and
                 String.contains?(remainder, "/"),
             {:ok, parsed} <- attributes_to_map(captures, tag, namespace) do
          order =
            Enum.map(captures, fn [_, name | _] ->
              reserve_render_resource(:max_html_tree_work, byte_size(name) + 1, :html)
              String.downcase(name, :ascii)
            end)

          {:ok,
           %{
             attributes: parsed,
             attribute_order: order,
             self_closing?: String.contains?(remainder, "/")
           }}
        else
          _ -> {:error, :unsupported_html}
        end
    end
  end

  @doc false
  @spec validate_attribute_order(term()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_attribute_order(node) do
    with_render_budget(fn ->
      case node do
        %{attribute_order: order} ->
          valid? =
            case node do
              %{type: :element, attributes: attributes}
              when is_map(attributes) and is_list(order) ->
                reserve_render_resource(
                  :max_html_tree_work,
                  length(order) + map_size(attributes),
                  :html
                )

                result =
                  Enum.reduce_while(order, {:ok, MapSet.new()}, fn name, {:ok, seen} ->
                    if is_binary(name) do
                      reserve_render_resource(:max_html_tree_work, byte_size(name) + 1, :html)

                      if String.valid?(name) and Map.has_key?(attributes, name) and
                           not MapSet.member?(seen, name),
                         do: {:cont, {:ok, MapSet.put(seen, name)}},
                         else: {:halt, :invalid}
                    else
                      {:halt, :invalid}
                    end
                  end)

                case result do
                  {:ok, seen} -> MapSet.size(seen) == map_size(attributes)
                  _ -> false
                end

              _ ->
                false
            end

          if valid?,
            do: :ok,
            else:
              Diagnostics.error(
                :style,
                :invalid_document,
                "attribute_order requires each validated element attribute name exactly once in original token order; omit it for a metadata-free caller tree",
                module: __MODULE__,
                operation: :compute,
                source:
                  case Map.get(node, :source) do
                    %{text: text} when is_binary(text) -> text
                    _ -> "attribute_order"
                  end
              )

        _ ->
          :ok
      end
    end)
  end

  defp attributes_to_map(captures, tag, namespace) do
    custom? = namespace == :html and custom_html_tag?(tag)

    Enum.reduce_while(captures, {:ok, %{}}, fn capture, {:ok, acc} ->
      [_, name | quoted] = capture
      source_value = List.first(quoted) || ""
      name = String.downcase(name, :ascii)

      value =
        case source_value do
          "" ->
            ""

          <<quote, _rest::binary>> when quote in [?", ?'] ->
            source_value |> String.slice(1..-2//1) |> HtmlEntities.decode(:attribute)

          source_value ->
            HtmlEntities.decode(source_value, :attribute)
        end

      case {name, tag, Map.has_key?(acc, name)} do
        {name, _, false} when namespace in [:mathml, :svg] or custom? ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, _, false} when name in @preserved_global_attributes ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, _, false}
        when name in ~w(checked disabled readonly multiple required selected alpha) ->
          # HTML retains known boolean metadata on inapplicable hosts as source.
          {:cont, {:ok, Map.put(acc, name, value)}}

        {"style", _, false} ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {"id", _, false} ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {"class", _, false} ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {"title", _, false} ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {"role", _, false} ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {"is", _, false} ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {"hidden", _, false} ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {"inert", _, false} ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, _, false}
        when name in ~w(contenteditable itemscope itemtype itemid itemprop itemref translate slot) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {"popover", _, false} ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, _, false} when name in ~w(headingoffset headingreset) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, _, false}
        when is_binary(name) and
               (binary_part(name, 0, min(byte_size(name), 5)) == "data-" or
                  binary_part(name, 0, min(byte_size(name), 5)) == "aria-") ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {"xmlns", _, false} ->
          if value == "http://www.w3.org/1999/xhtml",
            do: {:cont, {:ok, Map.put(acc, name, value)}},
            else: {:halt, {:error, :unsupported_html}}

        {name, _, false} when name in ["lang", "xml:lang"] ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "ol", false} when name in ~w(start reversed type compact) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "ul", false} when name in ~w(type compact) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "li", false} when name in ~w(value type) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, tag, false}
        when {name, tag} in [
               {"value", "data"},
               {"datetime", "time"},
               {"cite", "blockquote"},
               {"cite", "q"}
             ] ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, tag, false} when name in ["cite", "datetime"] and tag in ["ins", "del"] ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "template", false}
        when name in ~w(for shadowrootmode shadowrootdelegatesfocus shadowrootserializable shadowrootslotassignment shadowrootclonable shadowrootcustomelementregistry) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "script", false} ->
          if valid_script_attribute?(name),
            do: {:cont, {:ok, Map.put(acc, name, value)}},
            else: {:halt, {:error, :unsupported_html}}

        {name, "base", false} when name in ~w(href target) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "a", false}
        when name in ~w(href target download ping rel rev hreflang type referrerpolicy name charset coords shape) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "iframe", false}
        when name in ~w(src srcdoc width height name frameborder marginwidth marginheight scrolling) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "frame", false}
        when name in ~w(src name frameborder marginwidth marginheight scrolling noresize) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "frameset", false} when name in ~w(rows cols frameborder border bordercolor) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "embed", false} when name in ~w(src type width height name) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "canvas", false} when name in ~w(width height) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "object", false} when name in ~w(data type width height name usemap) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {"src", "img", false} ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {"alt", "img", false} ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "img", false}
        when name in ~w(width height srcset sizes loading decoding fetchpriority usemap) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {"name", "map", false} ->
          {:cont, {:ok, Map.put(acc, "name", value)}}

        {name, "area", false}
        when name in ~w(alt coords shape href target download ping rel referrerpolicy hreflang type nohref) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "source", false} when name in ~w(srcset sizes media type width height) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "hr", false} when name in ~w(align color height noshade size width) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {"align", "legend", false} ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "bgsound", false} when name in ~w(src loop balance volume) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "keygen", false} when name in ~w(name challenge keytype autofocus disabled form) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, tag, false} when tag in ~w(font basefont) and name in ~w(color face size) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "body", false}
        when name in ~w(background bgcolor text link alink vlink marginwidth marginheight topmargin leftmargin rightmargin bottommargin) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "dialog", false} when name in ~w(open closedby autofocus) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "details", false} when name in ~w(open name) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {"name", "slot", false} ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "style", false} when name in ~w(type media blocking nonce disabled) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, "meta", false} when name in ~w(charset content http-equiv name property) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, tag, false} when tag in ~w(td th) and name in ~w(colspan rowspan headers) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {"abbr", "th", false} ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {"scope", "th", false} ->
          # Missing and invalid keywords have the HTML Auto state.
          {:cont, {:ok, Map.put(acc, name, value)}}

        {"span", tag, false} when tag in ~w(colgroup col) ->
          {:cont, {:ok, Map.put(acc, name, value)}}

        {name, tag, false}
        when tag in ~w(table caption colgroup col thead tbody tfoot tr td th) ->
          if NativeElixirPdfUtilities.Validators.HtmlTableValidator.presentation_attribute?(
               tag,
               name
             ),
             do: {:cont, {:ok, Map.put(acc, name, value)}},
             else: {:halt, {:error, :unsupported_html}}

        {name, tag, false}
        when tag in ~w(input select textarea button form label option optgroup fieldset progress meter output datalist) ->
          case valid_form_attribute?(tag, name, value) do
            true -> {:cont, {:ok, Map.put(acc, name, value)}}
            false -> {:halt, {:error, :unsupported_html}}
          end

        _ ->
          {:halt, {:error, :unsupported_html}}
      end
    end)
  end

  @doc false
  @spec validate_css_source(term(), :stylesheet | :declarations) ::
          {:ok, binary()} | {:error, {:invalid_css, Diagnostics.diagnostic()}}
  def validate_css_source(css, kind \\ :stylesheet) do
    case {css, kind} do
      {css, kind} when is_binary(css) and kind in [:stylesheet, :declarations] ->
        case {byte_size(css) <= Limits.get(:max_css_source_bytes),
              String.valid?(css, :fast_ascii)} do
          {false, _} ->
            Diagnostics.error(:css, :invalid_css, "CSS input exceeds max_css_source_bytes")

          {true, true} ->
            {:ok, css}

          {true, false} ->
            Diagnostics.error(:css, :invalid_css, "CSS input must be valid UTF-8")
        end

      {_css, :declarations} ->
        Diagnostics.error(:css, :invalid_css, "CSS declaration input must be a string")

      _ ->
        Diagnostics.error(:css, :invalid_css, "CSS input must be a string")
    end
  end

  @doc false
  @spec validate_css_declaration(term()) ::
          {:ok, {String.t(), String.t()} | {String.t(), String.t(), :important}}
          | {:error, :invalid_css}
  def validate_css_declaration(declaration) do
    case declaration do
      declaration when is_binary(declaration) ->
        case String.split(declaration, ":", parts: 2) do
          [property, value] ->
            property = trim_css_whitespace(property)

            property =
              if String.starts_with?(property, "--"), do: property, else: downcase_ascii(property)

            value = trim_css_whitespace(value)

            # Ordinary declarations have no priority marker. Preserve quoted and
            # escaped exclamation marks before recognizing the final identifier.
            important_offset =
              if String.contains?(value, "!") do
                case Regex.run(@css_important_suffix_regex, value, return: :index) do
                  [{offset, _length}, {keyword_offset, keyword_length}] ->
                    keyword = binary_part(value, keyword_offset, keyword_length)

                    if keyword
                       |> trim_ascii_whitespace()
                       |> decode_css_escapes()
                       |> downcase_ascii() == "important",
                       do: offset,
                       else: nil

                  nil ->
                    nil
                end
              end

            value =
              case important_offset do
                nil -> value
                offset -> value |> binary_part(0, offset) |> trim_css_whitespace()
              end

            valid_property =
              Regex.match?(~r/\A[a-z][a-z-]*\z/u, property) or
                Regex.match?(~r/\A--[a-zA-Z_][a-zA-Z0-9_-]*\z/u, property)

            cond do
              not valid_property or value == "" -> {:error, :invalid_css}
              not is_nil(important_offset) -> {:ok, {property, value, :important}}
              true -> {:ok, {property, value}}
            end

          _ ->
            {:error, :invalid_css}
        end

      _ ->
        {:error, :invalid_css}
    end
  end

  @doc false
  @spec horizontal_rule_hints(map()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def horizontal_rule_hints(attributes) do
    with {:ok, hints} <- html_dimension_hints(attributes, [{"width", :width}], "hr") do
      solid = Map.has_key?(attributes, "color") or Map.has_key?(attributes, "noshade")

      hints =
        Map.put(
          hints,
          :border_styles,
          Map.new([:top, :right, :bottom, :left], &{&1, if(solid, do: :solid, else: :inset)})
        )

      hints =
        case parse_html_legacy_color(Map.get(attributes, "color")) do
          {:ok, color} -> Map.put(hints, :color, color)
          :error -> hints
        end

      align = Map.get(attributes, "align", "")
      align = if byte_size(align) <= byte_size("center"), do: String.downcase(align, :ascii)

      hints =
        case align do
          "left" -> Map.put(hints, :margin, %{top: 0.0, right: :auto, bottom: 0.0, left: 0.0})
          "right" -> Map.put(hints, :margin, %{top: 0.0, right: 0.0, bottom: 0.0, left: :auto})
          _ -> hints
        end

      case parse_html_integer(Map.get(attributes, "size")) do
        {:ok, size} when size >= 0 ->
          case parse_css_number(Integer.to_string(size)) do
            {number, ""} ->
              hints =
                cond do
                  solid ->
                    Map.put(
                      hints,
                      :border_widths,
                      Map.new([:top, :right, :bottom, :left], &{&1, number * 0.75 / 2})
                    )

                  size == 1 ->
                    Map.put(hints, :border_widths, %{
                      top: 0.75,
                      right: 0.75,
                      bottom: 0.0,
                      left: 0.75
                    })

                  size > 1 ->
                    Map.put(hints, :height, (number - 2) * 0.75)

                  true ->
                    hints
                end

              {:ok, hints}

            :error ->
              Diagnostics.error(
                :style,
                :resource_limit_exceeded,
                "hr size exceeds max_css_numeric_magnitude; reduce the rule size",
                source: "size=" <> Map.fetch!(attributes, "size"),
                module: __MODULE__
              )
          end

        _ ->
          {:ok, hints}
      end
    end
  end

  @doc false
  @spec image_dimension_hints(map()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def image_dimension_hints(attributes) do
    html_dimension_hints(attributes, [{"width", :width}, {"height", :height}], "image")
  end

  @doc "Prepares iframe dimensions and legacy border suppression before author CSS."
  @spec iframe_hints(term()) :: {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def iframe_hints(attributes) do
    case attributes do
      attributes when is_map(attributes) ->
        with {:ok, hints} <-
               html_dimension_hints(
                 attributes,
                 [{"width", :width}, {"height", :height}],
                 "iframe"
               ),
             {:ok, gray} <- parse_css_color("#eeeeee", nil) do
          # Chromium's iframe UA border paints a fixed light-gray theme even
          # though computed currentColor is black. Author border colors override it.
          hints =
            hints
            |> Map.put(:border_color, gray)
            |> Map.put(
              :border_colors,
              NativeElixirPdfUtilities.Validators.CssValueValidator.edges(gray)
            )
            |> Map.put(:_iframe_border, true)

          case Map.fetch(attributes, "frameborder") do
            :error ->
              {:ok, hints}

            {:ok, value} when is_binary(value) ->
              {:ok,
               if(parse_html_legacy_int32(value) in [:error, {:ok, 0}],
                 do:
                   Map.put(
                     hints,
                     :border_widths,
                     NativeElixirPdfUtilities.Validators.CssValueValidator.edges(0.0)
                   ),
                 else: hints
               )}

            _ ->
              Diagnostics.error(
                :style,
                :invalid_document,
                "iframe frameborder requires a legacy string value",
                module: __MODULE__,
                source: "iframe",
                operation: :prepare_hints
              )
          end
        end

      _ ->
        Diagnostics.error(:style, :invalid_document, "iframe hints require an attribute map",
          module: __MODULE__,
          source: "iframe",
          operation: :prepare_hints
        )
    end
  catch
    {:render_resource_limit, error} -> error
  end

  @typedoc "Canonical document canvas decision from computed root/body styles."
  @type document_canvas :: %{style: map(), body_background_propagated: boolean()}

  @doc "Internal canvas selection from computed CSS styles; a painted root keeps body paint on its own box."
  @spec prepare_document_canvas(map() | nil, map() | nil) :: document_canvas()
  def prepare_document_canvas(root_style, body_style) do
    root_style = root_style || %{}
    body_style = body_style || %{}
    root_color = Map.get(root_style, :background_color)

    root_paints =
      Map.get(root_style, :background_image) != nil or
        case root_color do
          nil -> false
          {_, _, _, alpha} when alpha == 0 -> false
          _ -> true
        end

    %{
      style:
        Map.take(
          if(
            root_paints or Map.get(root_style, :content_visibility) == :hidden or
              Map.get(body_style, :content_visibility) == :hidden,
            do: root_style,
            else: body_style
          ),
          [
            :background_color,
            :background_image,
            :background_position,
            :background_repeat,
            :background_size
          ]
        ),
      body_background_propagated:
        not root_paints and Map.get(root_style, :content_visibility) != :hidden and
          Map.get(body_style, :content_visibility) != :hidden
    }
  end

  @doc "Internal resolution of an already validated styled tree's canvas plan or legacy body-only canvas before allocation."
  @spec layout_canvas(NativeElixirPdfUtilities.HtmlToPdf.Style.styled_tree()) ::
          {:ok, document_canvas()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def layout_canvas(tree) do
    case Map.fetch(tree, :document_canvas) do
      :error ->
        body_style =
          Enum.find_value(tree.children, fn node ->
            case node do
              %{tag: "body", style: style} -> style
              _ -> nil
            end
          end)

        {:ok, prepare_document_canvas(nil, body_style)}

      {:ok, %{style: style, body_background_propagated: propagated} = plan}
      when is_map(style) and is_boolean(propagated) and map_size(plan) == 2 ->
        if Enum.all?(
             Map.keys(style),
             &(&1 in [
                 :background_color,
                 :background_image,
                 :background_position,
                 :background_repeat,
                 :background_size
               ])
           ) and
             (style[:background_color] == nil or
                NativeElixirPdfUtilities.Validators.WriterValidator.valid_color?(
                  style[:background_color]
                )) do
          {:ok, plan}
        else
          Diagnostics.error(
            :layout,
            :invalid_document,
            "document canvas requires canonical background properties",
            module: __MODULE__,
            operation: :layout_document_canvas
          )
        end

      _ ->
        Diagnostics.error(
          :layout,
          :invalid_document,
          "document canvas requires a prepared background style and body propagation flag",
          module: __MODULE__,
          operation: :layout_document_canvas
        )
    end
  end

  @doc false
  @spec html_dimension_hints(map(), [{String.t(), atom()}], String.t()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def html_dimension_hints(attributes, properties, source_tag) do
    if Enum.any?(properties, fn {name, _property} -> Map.has_key?(attributes, name) end) do
      Enum.reduce_while(properties, {:ok, %{}}, fn {name, property}, {:ok, hints} ->
        case Map.fetch(attributes, name) do
          {:ok, value} ->
            case Regex.named_captures(
                   ~r/\A[ \t\n\f\r]*(?<integer>[0-9]+)(?:\.(?<fraction>[0-9]*))?(?<rest>.*)\z/s,
                   value
                 ) do
              %{"integer" => integer, "fraction" => fraction, "rest" => rest} ->
                token = if fraction == "", do: integer, else: integer <> "." <> fraction

                try do
                  check_render_resource(:max_html_numeric_token_bytes, byte_size(token), :style)
                catch
                  {:render_resource_limit, {:error, {reason, diagnostic}}} ->
                    throw(
                      {:render_resource_limit,
                       {:error,
                        {reason,
                         Diagnostics.with_context(diagnostic, source: name <> "=" <> value)}}}
                    )
                end

                case parse_css_number(token) do
                  {number, ""} ->
                    dimension =
                      if String.starts_with?(rest, "%"),
                        do: {:percent, number / 100.0},
                        else: number * 0.75

                    {:cont, {:ok, Map.put(hints, property, dimension)}}

                  :error ->
                    {:halt,
                     Diagnostics.error(
                       :style,
                       :resource_limit_exceeded,
                       source_tag <>
                         " " <>
                         name <> " exceeds max_css_numeric_magnitude; reduce the dimension",
                       source: name <> "=" <> value,
                       module: __MODULE__
                     )}
                end

              nil ->
                {:cont, {:ok, hints}}
            end

          :error ->
            {:cont, {:ok, hints}}
        end
      end)
    else
      {:ok, %{}}
    end
  end

  @doc false
  @spec validate_svg_raster(term(), term()) ::
          {:ok, [width: pos_integer(), height: pos_integer()]}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_svg_raster(svg, raster_options) do
    validate_svg_raster(svg, raster_options, nil)
  end

  @doc false
  @spec validate_svg_raster(term(), term(), image_budget() | nil) ::
          {:ok, [width: pos_integer(), height: pos_integer()]}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_svg_raster(svg, raster_options, image_budget) do
    NativeElixirPdfUtilities.Validators.SvgValidator.validate(svg, raster_options, image_budget)
  end

  defp reserve_image_budget(budget, index, amount, limit, message) do
    case :atomics.add_get(budget, index, amount) <= limit do
      true ->
        :ok

      false ->
        Diagnostics.error(:limits, :resource_limit_exceeded, message)
    end
  rescue
    ArgumentError ->
      Diagnostics.error(:style, :invalid_document, "image resource budget is invalid")
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
    context = %{fallback_base: nil, resolution_base: nil, authored_base?: false}

    case NativeElixirPdfUtilities.Validators.HtmlUrlValidator.prepare_anchor(href, context) do
      {:ok, {:uri, _url}} -> true
      _ -> false
    end
  end

  @doc false
  @spec valid_table_header_scope?(term()) :: boolean()
  def valid_table_header_scope?(scope) do
    case scope do
      scope when is_binary(scope) -> String.downcase(scope) in ~w(row col rowgroup colgroup)
      _ -> false
    end
  end

  @doc false
  @spec validate_local_resource_path(term(), term()) ::
          {:ok, String.t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_local_resource_path(source, base_url) do
    NativeElixirPdfUtilities.Validators.ResourceValidator.authorize_local_path(
      source,
      base_url,
      base_url,
      :legacy
    )
  end

  @doc false
  @spec validate_asset_options(term(), term()) ::
          :ok | {:error, {:invalid_options, Diagnostics.diagnostic()}}
  def validate_asset_options(assets, asset_resolver) do
    valid_assets? =
      is_map(assets) and
        Enum.all?(assets, fn
          {reference, {:bytes, bytes}} ->
            is_binary(reference) and String.trim(reference) != "" and is_binary(bytes)

          {reference, {:file, path}} ->
            is_binary(reference) and String.trim(reference) != "" and is_binary(path) and
              String.trim(path) != ""

          _ ->
            false
        end)

    valid_resolver? = is_nil(asset_resolver) or is_function(asset_resolver, 1)

    cond do
      not valid_assets? ->
        Diagnostics.error(
          :options,
          :invalid_options,
          "assets must be a map of non-empty references to {:bytes, binary} or {:file, path}"
        )

      not valid_resolver? ->
        Diagnostics.error(
          :options,
          :invalid_options,
          "asset_resolver must be a one-argument function or nil"
        )

      true ->
        :ok
    end
  end

  @doc false
  @spec validate_asset_resolver_result(term(), String.t()) ::
          {:ok, binary()} | :not_found | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_asset_resolver_result(result, reference) do
    case result do
      {:ok, bytes} when is_binary(bytes) ->
        {:ok, bytes}

      :not_found ->
        :not_found

      {:error, _reason} ->
        Diagnostics.error(
          :asset,
          :invalid_document,
          "caller-provided asset resolver could not resolve the asset",
          source: reference
        )

      _ ->
        Diagnostics.error(
          :asset,
          :invalid_document,
          "asset_resolver must return {:ok, binary}, :not_found, or {:error, reason}",
          source: reference
        )
    end
  end

  @doc false
  @spec validate_table_span_attribute(term(), term()) ::
          {:ok, non_neg_integer()} | :error | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_table_span_attribute(attributes, name) do
    case {attributes, name} do
      {attributes, name} when is_map(attributes) and name in ["span", "colspan", "rowspan"] ->
        case Map.get(attributes, name) do
          nil ->
            {:ok, 1}

          value when is_binary(value) ->
            parsed =
              if String.starts_with?(trim_ascii_whitespace(value), "-"),
                do: :error,
                else: parse_html_integer(value)

            value =
              case parsed do
                {:ok, integer} -> integer
                :error -> 1
              end

            normalize_table_span(name, value)

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  @doc false
  @spec normalize_table_span(term(), term()) ::
          {:ok, non_neg_integer()} | :error | {:error, {atom(), Diagnostics.diagnostic()}}
  def normalize_table_span(name, value) do
    case {name, value} do
      {name, value}
      when name in ["span", "colspan", "rowspan"] and is_integer(value) and value >= 0 ->
        # HTML mandates these bounds. They are protocol invariants, not resource limits.
        bound = if name == "rowspan", do: 65_534, else: 1_000
        span = if value == 0 and name != "rowspan", do: 1, else: min(value, bound)
        result = if span == 0, do: :ok, else: validate_layout_cardinality(:table_span, span)
        with :ok <- result, do: {:ok, span}

      _ ->
        :error
    end
  end

  @doc false
  @spec parse_css_simple_pseudo_class(String.t()) :: {:ok, atom(), String.t()} | nil
  def parse_css_simple_pseudo_class(selector) do
    case Regex.named_captures(@css_simple_pseudo_regex, selector) do
      %{"keyword" => keyword, "rest" => rest} ->
        keywords = %{
          "first-child" => :first_child,
          "last-child" => :last_child,
          "first-of-type" => :first_of_type,
          "last-of-type" => :last_of_type,
          "only-child" => :only_child,
          "only-of-type" => :only_of_type,
          "root" => :root,
          "enabled" => :enabled,
          "disabled" => :disabled,
          "checked" => :checked,
          "indeterminate" => :indeterminate,
          "required" => :required,
          "optional" => :optional,
          "valid" => :valid,
          "invalid" => :invalid,
          "read-only" => :read_only,
          "read-write" => :read_write,
          "popover-open" => :popover_open
        }

        case Map.get(keywords, keyword |> decode_css_escapes() |> String.downcase(:ascii)) do
          nil -> nil
          pseudo_class -> {:ok, pseudo_class, rest}
        end

      nil ->
        nil
    end
  end

  @doc false
  @spec parse_css_language_selector(String.t()) :: {:ok, tuple(), String.t()} | :error | nil
  def parse_css_language_selector(selector) do
    case parse_css_function(selector, "lang") do
      {:ok, ranges, rest} ->
        case parse_language_ranges(ranges, []) do
          {:ok, ranges} -> {:ok, {:lang, ranges}, rest}
          :error -> :error
        end

      other ->
        other
    end
  end

  # CSS range arguments are identifiers or strings. Quoted commas, escaped
  # whitespace and empty strings are values, while empty comma-list items fail.
  defp parse_language_ranges(source, ranges) do
    case Regex.named_captures(@css_language_range_regex, source) do
      %{"value" => value, "rest" => rest} ->
        range = value |> decode_css_value() |> LanguageValidator.canonicalize()

        case rest do
          "" -> {:ok, Enum.reverse([range | ranges])}
          <<",", rest::binary>> -> parse_language_ranges(rest, [range | ranges])
          _ -> :error
        end

      nil ->
        :error
    end
  end

  @doc false
  @spec parse_css_negated_selector(String.t()) :: {:ok, String.t(), String.t()} | :error | nil
  def parse_css_negated_selector(selector) do
    parse_css_function(selector, "not")
  end

  @doc false
  @spec parse_css_shadow_selector(String.t()) ::
          {:ok, atom(), String.t() | nil, String.t()} | :error | nil
  def parse_css_shadow_selector(selector) do
    case selector do
      <<"::", rest::binary>> ->
        case parse_css_function(":" <> rest, "slotted") do
          {:ok, compound, rest} -> {:ok, :slotted, compound, rest}
          other -> other
        end

      _ ->
        case Regex.named_captures(@css_simple_pseudo_regex, selector) do
          %{"keyword" => keyword, "rest" => rest} ->
            if keyword |> decode_css_escapes() |> String.downcase(:ascii) == "host" do
              case rest do
                <<"(", _::binary>> ->
                  case parse_css_function(selector, "host") do
                    {:ok, compound, rest} -> {:ok, :host, compound, rest}
                    other -> other
                  end

                _ ->
                  {:ok, :host, nil, rest}
              end
            end

          _ ->
            nil
        end
    end
  end

  # Escape units and complete quoted strings are opaque to function balancing.
  # Both :lang() and :not() use this parser rather than separate quote rules.
  defp parse_css_function(selector, expected) do
    case Regex.named_captures(@css_simple_pseudo_regex, selector) do
      %{"keyword" => keyword, "rest" => rest} ->
        if keyword |> decode_css_escapes() |> String.downcase(:ascii) == expected do
          case rest do
            <<"(", body::binary>> ->
              reserve_render_resource(:max_css_work, byte_size(body), :css)
              units = Regex.scan(@css_function_unit_regex, body)

              result =
                Enum.reduce_while(units, {1, [], 0}, fn [unit], {depth, content, offset} ->
                  cond do
                    unit in ["\"", "'"] ->
                      {:halt, :error}

                    unit == ")" and depth == 1 ->
                      {:halt,
                       {:ok, content |> Enum.reverse() |> IO.iodata_to_binary(),
                        binary_part(body, offset + 1, byte_size(body) - offset - 1)}}

                    unit == ")" ->
                      {:cont, {depth - 1, [unit | content], offset + byte_size(unit)}}

                    unit == "(" ->
                      {:cont, {depth + 1, [unit | content], offset + byte_size(unit)}}

                    true ->
                      {:cont, {depth, [unit | content], offset + byte_size(unit)}}
                  end
                end)

              case result do
                {:ok, _content, _rest} = parsed -> parsed
                _ -> :error
              end

            _ ->
              :error
          end
        else
          nil
        end

      nil ->
        nil
    end
  end

  # Shared CSS string/identifier decoding for attribute and language selectors.
  defp decode_css_value(value) do
    case value do
      <<quote, inner::binary>> when quote in [?", ?'] ->
        inner
        |> binary_part(0, byte_size(inner) - 1)
        |> String.replace(~r/\\(?:\r\n|[\n\r\f])/u, "")
        |> decode_css_escapes()

      value ->
        decode_css_escapes(value)
    end
  end

  @doc false
  @spec parse_css_nth_pseudo_class(String.t()) :: {:ok, tuple(), String.t()} | :error | nil
  def parse_css_nth_pseudo_class(selector) do
    case Regex.named_captures(@css_nth_pseudo_regex, selector) do
      %{"keyword" => keyword, "expression" => expression, "rest" => rest} ->
        kinds = %{
          "nth-child" => :nth_child,
          "nth-last-child" => :nth_last_child,
          "nth-of-type" => :nth_of_type,
          "nth-last-of-type" => :nth_last_of_type
        }

        case Map.get(kinds, keyword |> decode_css_escapes() |> String.downcase(:ascii)) do
          nil ->
            nil

          kind ->
            case parse_css_nth_expression(expression) do
              {:ok, index} -> {:ok, {kind, index}, rest}
              :error -> :error
            end
        end

      nil ->
        nil
    end
  end

  # An+B uses integer tokens, ASCII keywords and whitespace only around the B sign.
  defp parse_css_nth_expression(expression) do
    expression = trim_ascii_whitespace(expression)

    case String.downcase(expression, :ascii) do
      "odd" ->
        {:ok, :odd}

      "even" ->
        {:ok, :even}

      _ ->
        tokens =
          cond do
            Regex.match?(~r/\A[+-]?[0-9]+\z/, expression) ->
              {:integer, [expression]}

            captures =
                Regex.named_captures(
                  ~r/\A(?<a>[+-]?[0-9]*)[nN](?:[ \t\n\f\r]*(?<sign>[+-])[ \t\n\f\r]*(?<b>[0-9]+))?\z/,
                  expression
                ) ->
              a =
                case captures["a"] do
                  "" -> "1"
                  "+" -> "1"
                  "-" -> "-1"
                  a -> a
                end

              b = if captures["b"] == "", do: "0", else: captures["sign"] <> captures["b"]
              {:series, [a, b]}

            true ->
              :error
          end

        case tokens do
          {kind, values} ->
            numbers =
              Enum.map(values, fn token ->
                check_render_resource(:max_html_numeric_token_bytes, byte_size(token), :css)

                case parse_css_number(token) do
                  {number, ""} ->
                    trunc(number)

                  :error ->
                    throw(
                      {:render_resource_limit,
                       Diagnostics.error(
                         :css,
                         :resource_limit_exceeded,
                         "positional selector exceeds max_css_numeric_magnitude; reduce its integer coefficients",
                         module: __MODULE__,
                         source: expression
                       )}
                    )
                end
              end)

            case {kind, numbers} do
              {:integer, [index]} -> {:ok, index}
              {:series, [a, b]} -> {:ok, {a, b}}
            end

          :error ->
            :error
        end
    end
  catch
    {:render_resource_limit, {:error, {reason, diagnostic}}} ->
      throw(
        {:render_resource_limit,
         {:error,
          {reason, Diagnostics.with_context(diagnostic, source: expression, operation: :parse)}}}
      )
  end

  @doc false
  @spec html_contenteditable_state(map()) :: :editable | :not_editable | :inherit
  def html_contenteditable_state(attributes) do
    case Map.get(attributes, "contenteditable") do
      value when is_binary(value) and byte_size(value) <= byte_size("plaintext-only") ->
        case String.downcase(value, :ascii) do
          value when value in ["", "true", "plaintext-only"] -> :editable
          "false" -> :not_editable
          _ -> :inherit
        end

      _ ->
        :inherit
    end
  end

  @doc "Resolves applicable editing keywords; non-root foreign elements stop editing inheritance."
  @spec html_editing_state(map()) :: :editable | :not_editable | :inherit
  def html_editing_state(node) do
    html? = Map.get(node, :namespace, :html) == :html

    eligible? =
      html? or
        {Map.get(node, :namespace), Map.get(node, :tag)} in [{:svg, "svg"}, {:mathml, "math"}]

    cond do
      not eligible? ->
        :not_editable

      html? and is_nil(Map.get(Map.get(node, :attribute_namespaces, %{}), "contenteditable")) ->
        html_contenteditable_state(Map.get(node, :attributes, %{}))

      true ->
        :inherit
    end
  end

  @doc false
  @spec editing_state_matches?(:read_only | :read_write, map(), [map()], reference() | nil) ::
          boolean()
  def editing_state_matches?(state, node, ancestors, cache \\ nil) do
    html? = Map.get(node, :namespace, :html) == :html
    attributes = Map.get(node, :attributes, %{})
    namespaces = Map.get(node, :attribute_namespaces, %{})

    readonly? =
      Map.get(
        Map.get(node, :_initial_form_state, %{}),
        :readonly,
        Map.has_key?(attributes, "readonly") and is_nil(Map.get(namespaces, "readonly"))
      )

    disabled? =
      Map.get(
        Map.get(node, :_initial_form_state, %{}),
        :disabled,
        Map.has_key?(attributes, "disabled") and is_nil(Map.get(namespaces, "disabled"))
      )

    writable? =
      case {html?, Map.get(node, :tag)} do
        {true, "input"} ->
          type =
            if is_nil(Map.get(namespaces, "type")),
              do: Map.get(attributes, "type", "text"),
              else: "text"

          readonly_input_type?(input_type(type)) and not readonly? and
            not disabled?

        {true, "textarea"} ->
          not readonly? and not disabled?

        _ ->
          editable_region?(node, ancestors, cache)
      end

    if state == :read_write, do: writable?, else: not writable?
  end

  # Editing inheritance crosses ordinary containers, but only an unqualified
  # HTML contenteditable attribute establishes or stops an editing region.
  # Cache the region independently of a form control's own mutability.
  defp editable_region?(node, ancestors, cache) do
    resolve = fn ->
      reserve_render_resource(:max_css_work, 1, :css)

      case html_editing_state(node) do
        :editable ->
          true

        :not_editable ->
          false

        :inherit ->
          case ancestors do
            [parent | rest] -> editable_region?(parent, rest, cache)
            [] -> false
          end
      end
    end

    case Map.fetch(node, :_html_editable) do
      {:ok, editable?} ->
        editable?

      :error ->
        if is_nil(cache),
          do: resolve.(),
          else: RenderCache.fetch(cache, {:editing_region, node._selector_id}, resolve)
    end
  end

  @doc false
  @spec control_state_matches?(
          :enabled
          | :disabled
          | :checked
          | :required
          | :optional
          | :popover_open
          | :indeterminate
          | :valid
          | :invalid,
          map()
        ) ::
          boolean()
  def control_state_matches?(state, node) do
    html? = Map.get(node, :namespace, :html) == :html
    attributes = Map.get(node, :attributes, %{})

    case state do
      state when state in [:valid, :invalid] ->
        NativeElixirPdfUtilities.Validators.HtmlConstraintValidator.matches_initial_state?(
          state,
          node
        )

      :indeterminate ->
        html? and
          case Map.get(node, :tag) do
            "progress" -> not Map.fetch!(node, :_initial_gauge_state).determinate
            "input" -> Map.get(Map.get(node, :_initial_form_state, %{}), :indeterminate, false)
            _ -> false
          end

      :popover_open ->
        false

      state when state in [:enabled, :disabled] ->
        applicable? = html? and valid_boolean_form_attribute?(Map.get(node, :tag), "disabled")

        disabled? =
          if Map.get(node, :tag) == "option",
            do: Map.get(node, :_initial_option_disabled, Map.has_key?(attributes, "disabled")),
            else:
              Map.get(
                Map.get(node, :_initial_form_state, Map.get(node, :initial_state, %{})),
                :disabled,
                Map.has_key?(attributes, "disabled")
              )

        applicable? and if(state == :enabled, do: not disabled?, else: disabled?)

      state when state in [:required, :optional] ->
        required? =
          Map.get(
            Map.get(node, :_initial_form_state, Map.get(node, :initial_state, %{})),
            :required,
            Map.has_key?(attributes, "required")
          )

        if(state == :required, do: required?, else: not required?) and html? and
          case Map.get(node, :tag) do
            "input" ->
              type = input_type(Map.get(attributes, "type", "text"))

              NativeElixirPdfUtilities.Validators.HtmlFormValidator.input_attribute_applies?(
                type,
                "required"
              )

            tag when tag in ["select", "textarea"] ->
              true

            _ ->
              false
          end

      :checked ->
        if html? do
          case Map.get(node, :tag) do
            "input" ->
              input_type(Map.get(attributes, "type", "text")) in ["checkbox", "radio"] and
                Map.get(Map.get(node, :_initial_form_state, %{}), :checked, false)

            "option" ->
              Map.get(node, :_initial_selected, false)

            _ ->
              false
          end
        else
          false
        end
    end
  end

  @doc false
  @spec valid_boolean_form_attribute?(String.t(), String.t()) :: boolean()
  def valid_boolean_form_attribute?(tag, name) do
    case {tag, name} do
      {"input", name}
      when name in ["checked", "disabled", "readonly", "multiple", "required", "alpha"] ->
        true

      {"select", name} when name in ["disabled", "required", "multiple"] ->
        true

      {"option", name} when name in ["selected", "disabled"] ->
        true

      {"optgroup", "disabled"} ->
        true

      {"textarea", name} when name in ["disabled", "readonly", "required"] ->
        true

      {tag, "disabled"} when tag in ["button", "fieldset"] ->
        true

      _ ->
        false
    end
  end

  @doc false
  @spec valid_form_attribute?(String.t(), String.t(), String.t()) :: boolean()
  def valid_form_attribute?(tag, name, value) do
    case {tag, name, value} do
      {_tag, name, _value}
      when name in ~w(checked disabled readonly multiple required selected alpha) ->
        true

      {"form", name, _value}
      when name in ~w(name action method target enctype accept-charset autocomplete rel novalidate) ->
        true

      {"label", "for", _value} ->
        true

      {"output", name, _value} when name in ~w(for name) ->
        true

      {"progress", name, _value} when name in ~w(value max) ->
        true

      {"meter", name, _value} when name in ~w(value min max low high optimum) ->
        true

      {tag, "form", _value} when tag in ~w(input select textarea button fieldset output) ->
        true

      {"fieldset", name, _value} when name in ~w(name disabled) ->
        true

      {"input", "type", value} ->
        input_type(value) in @input_types

      {"input", name, _value}
      when name in [
             "value",
             "name",
             "checked",
             "disabled",
             "readonly",
             "multiple",
             "required",
             "size",
             "placeholder",
             "list",
             "min",
             "max",
             "step",
             "alpha",
             "colorspace",
             "src",
             "alt",
             "width",
             "height",
             "accept",
             "maxlength",
             "minlength",
             "pattern",
             "autocomplete",
             "capture",
             "dirname",
             "formaction",
             "formenctype",
             "formmethod",
             "formnovalidate",
             "formtarget",
             "popovertarget",
             "popovertargetaction"
           ] ->
        true

      {"select", name, _value}
      when name in ["name", "disabled", "required", "multiple", "size"] ->
        true

      {"option", name, _value} when name in ["value", "label", "selected", "disabled"] ->
        true

      {"optgroup", name, _value} when name in ["label", "disabled"] ->
        true

      {"textarea", name, _value}
      when name in [
             "value",
             "name",
             "disabled",
             "readonly",
             "required",
             "rows",
             "cols",
             "wrap",
             "placeholder",
             "maxlength",
             "minlength"
           ] ->
        true

      {"button", "type", _value} ->
        true

      {"button", name, _value} when name in ["value", "name", "disabled"] ->
        true

      _ ->
        false
    end
  end

  @doc false
  @spec input_type(String.t()) :: String.t()
  def input_type(value) do
    if byte_size(value) > @input_type_keyword_bytes do
      "text"
    else
      Map.get(@input_type_keywords, String.downcase(value, :ascii), "text")
    end
  end

  @doc false
  @spec text_input_type?(String.t()) :: boolean()
  def text_input_type?(type), do: type in @text_input_types

  @doc false
  @spec readonly_input_type?(String.t()) :: boolean()
  defdelegate readonly_input_type?(type),
    to: NativeElixirPdfUtilities.Validators.HtmlFormValidator

  @doc false
  @spec button_input_type?(String.t()) :: boolean()
  def button_input_type?(type), do: type in @button_input_types

  @doc false
  @spec prepare_input_attributes(map()) :: map()
  def prepare_input_attributes(attributes) do
    case {input_type(Map.get(attributes, "type", "text")), Map.fetch(attributes, "value")} do
      {type, {:ok, value}} when type in @text_input_types ->
        value = :binary.replace(value, ["\r", "\n"], "", [:global])

        value =
          if type == "email" and Map.has_key?(attributes, "multiple") do
            value
            |> String.split(",")
            |> Enum.map(&trim_ascii_whitespace/1)
            |> Enum.join(",")
          else
            if type in ["url", "email"], do: trim_ascii_whitespace(value), else: value
          end

        Map.put(attributes, "value", value)

      {type, :error} when type in @button_input_types ->
        # Labels are implementation-defined for submit/reset; use the English
        # initial labels of the pinned print reference. Explicit empty values stay empty.
        value =
          case type do
            "submit" -> "Submit"
            "reset" -> "Reset"
            "button" -> ""
          end

        Map.put(attributes, "value", value)

      _ ->
        attributes
    end
  end

  @doc false
  @spec trim_ascii_whitespace(String.t()) :: String.t()
  def trim_ascii_whitespace(value) do
    case value do
      "" ->
        value

      <<first, _rest::binary>> ->
        if first in [32, 9, 10, 12, 13] or :binary.last(value) in [32, 9, 10, 12, 13] do
          first_kept = if first in [32, 9, 10, 12, 13], do: 1, else: 0

          last_kept =
            if :binary.last(value) in [32, 9, 10, 12, 13],
              do: byte_size(value) - 1,
              else: byte_size(value)

          cond do
            first_kept >= last_kept ->
              ""

            :binary.at(value, first_kept) not in [32, 9, 10, 12, 13] and
                :binary.at(value, last_kept - 1) not in [32, 9, 10, 12, 13] ->
              binary_part(value, first_kept, last_kept - first_kept)

            true ->
              String.replace(value, ~r/^[ \t\n\f\r]+|[ \t\n\f\r]+$/u, "")
          end
        else
          value
        end
    end
  end

  @doc false
  @spec valid_form_element?(String.t(), map(), [term()]) :: boolean()
  def valid_form_element?(tag, attributes, children),
    do: valid_form_element?(tag, attributes, children, :authored)

  @doc "Checks authored control content or the initial DOM after selected-content copying."
  @spec valid_form_element?(String.t(), map(), [term()], :authored | :initial_dom) :: boolean()
  def valid_form_element?(tag, attributes, children, surface) do
    children = if tag in ~w(input textarea), do: children, else: html_content_nodes(children)

    case tag do
      "input" ->
        type = input_type(Map.get(attributes, "type", "text"))

        type in @input_types and
          NativeElixirPdfUtilities.Validators.HtmlFormValidator.valid_initial_input?(
            type,
            attributes,
            children
          )

      "output" ->
        Enum.all?(children, fn child ->
          match?(%{type: :text}, child) or
            (match?(%{type: :element}, child) and valid_html_child_element?("output", child, []))
        end)

      "datalist" ->
        if Enum.any?(children, &match?(%{tag: "option"}, &1)) do
          Enum.all?(children, fn child ->
            match?(%{tag: "option"}, child) or script_supporting_element?(child)
          end)
        else
          valid_form_element?("output", attributes, children)
        end

      "select" ->
        Enum.with_index(children)
        |> Enum.all?(fn {child, index} ->
          case child do
            %{type: :element, tag: tag, attributes: attributes, children: children}
            when tag in ["option", "optgroup"] ->
              valid_form_element?(tag, attributes, children, surface)

            %{type: :element, tag: "hr", attributes: attributes, children: children} ->
              valid_html_element?("hr", attributes, children)

            %{type: :element, tag: "button", attributes: attributes, children: children} ->
              index == 0 and valid_form_element?("button", attributes, children, surface)

            %{type: :element, tag: "div", children: children} ->
              valid_form_element?("select", %{}, children, surface)

            _ ->
              script_supporting_element?(child)
          end
        end)

      "optgroup" ->
        Map.has_key?(attributes, "label") and
          Enum.all?(children, fn child ->
            case child do
              %{type: :element, tag: "option", attributes: attributes, children: children} ->
                valid_form_element?("option", attributes, children, surface)

              _ ->
                script_supporting_element?(child)
            end
          end)

      "option" ->
        valid_noninteractive_children?(children, "option", [], surface)

      "textarea" ->
        Enum.all?(children, &match?(%{type: :text}, &1))

      "button" ->
        valid_noninteractive_children?(children, "button", [], surface)

      _ ->
        false
    end
  end

  @doc false
  @spec select_options([map()]) :: [map()]
  def select_options(children) do
    Enum.flat_map(children, fn child ->
      if Map.get(child, :namespace, :html) == :html do
        case child do
          %{type: :element, tag: "option"} ->
            attributes =
              NativeElixirPdfUtilities.Validators.HtmlFormValidator.initial_control_attributes(
                child
              )

            [Map.put(child, :_initial_option_disabled, Map.has_key?(attributes, "disabled"))]

          %{type: :element, tag: "div", children: children} ->
            select_options(children)

          %{type: :element, tag: "optgroup", children: children} ->
            attributes =
              NativeElixirPdfUtilities.Validators.HtmlFormValidator.initial_control_attributes(
                child
              )

            disabled? = Map.has_key?(attributes, "disabled")

            Enum.map(select_options(children), fn option ->
              Map.put(
                option,
                :_initial_option_disabled,
                disabled? or option._initial_option_disabled
              )
            end)

          _ ->
            []
        end
      else
        []
      end
    end)
  end

  @doc false
  @spec descendant_text_content([map()]) :: binary()
  def descendant_text_content(children) do
    children |> descendant_text_chunks() |> IO.iodata_to_binary()
  end

  # DOM textContent includes hidden/script text but not template fragment text.
  # Charge both the visits and copied bytes before building semantic values.
  defp descendant_text_chunks(children, skip_scripts? \\ false) do
    Enum.map(children, fn node ->
      reserve_render_resource(:max_html_tree_work, 1, :style)

      case node do
        %{type: :text, text: text} ->
          reserve_render_resource(:max_html_tree_work, 2 * byte_size(text), :style)
          text

        %{type: :element, tag: "script"} when skip_scripts? ->
          []

        %{type: :element, children: nested} ->
          descendant_text_chunks(nested, skip_scripts?)

        _ ->
          []
      end
    end)
  end

  @doc false
  @spec initial_option_text([map()]) :: binary()
  def initial_option_text(children) do
    children
    |> descendant_text_chunks(true)
    |> IO.iodata_to_binary()
    |> trim_ascii_whitespace()
    |> String.replace(~r/[ \t\n\f\r]+/u, " ")
  end

  # Anchors and buttons forbid interactive or tabindex descendants in the
  # active tree. Template contents have their own document-fragment context.
  defp valid_noninteractive_children?(children, context, ancestors \\ [], surface \\ :authored) do
    children = html_content_nodes(children)

    Enum.all?(children, fn child ->
      case child do
        %{type: :text} ->
          true

        %{type: :element, tag: tag, attributes: attributes, children: nested}
        when is_map(attributes) and is_list(nested) ->
          html? = Map.get(child, :namespace, :html) == :html

          # Copied option children are actual executed DOM, rather than authored
          # children of selectedcontent. Keep all interactive ancestry checks.
          valid_html_child_element?(
            if(surface == :initial_dom and context == "selectedcontent",
              do: "option",
              else: context
            ),
            child,
            ancestors
          ) and
            if(html? and tag == "template",
              do: script_supporting_element?(child),
              else:
                valid_noninteractive_children?(
                  nested,
                  if(html?, do: tag, else: {:foreign, tag}),
                  [context | ancestors],
                  surface
                )
            )

        _ ->
          false
      end
    end)
  end

  @doc false
  @spec validate_layout_cardinality(:grid_placement | :grid_tracks | :table_span, term()) ::
          :ok | :error | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_layout_cardinality(kind, value) do
    max_layout_cardinality = Limits.get(:max_layout_cardinality)

    case {kind, value} do
      {kind, value}
      when kind in [:grid_placement, :grid_tracks, :table_span] and is_integer(value) and
             value >= 1 and value <= max_layout_cardinality ->
        :ok

      {kind, value}
      when kind in [:grid_placement, :grid_tracks, :table_span] and is_integer(value) and
             value >= 1 ->
        label =
          case kind do
            :grid_placement -> "grid placement"
            :grid_tracks -> "grid track count"
            :table_span -> "table span"
          end

        Diagnostics.error(
          :limits,
          :resource_limit_exceeded,
          "#{label} exceeds the #{max_layout_cardinality}-item limit"
        )

      _ ->
        :error
    end
  end

  @doc false
  @spec validate_background_image_tile_count(term()) ::
          :ok | :error | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_background_image_tile_count(tile_count) do
    max_background_image_tiles = Limits.get(:max_background_image_tiles)

    case tile_count do
      tile_count
      when is_integer(tile_count) and tile_count >= 1 and
             tile_count <= max_background_image_tiles ->
        :ok

      tile_count when is_integer(tile_count) and tile_count >= 1 ->
        Diagnostics.error(
          :limits,
          :resource_limit_exceeded,
          "background image tile count exceeds the #{max_background_image_tiles}-tile limit"
        )

      _ ->
        :error
    end
  end

  @doc false
  @spec validate_background_image_tile_dimensions(
          term(),
          term(),
          term(),
          term(),
          term()
        ) :: :ok | :error | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_background_image_tile_dimensions(
        tile_width,
        tile_height,
        area_width,
        area_height,
        repeat
      ) do
    max_background_image_tiles = Limits.get(:max_background_image_tiles)

    case {tile_width, tile_height, area_width, area_height, repeat} do
      {tile_width, tile_height, area_width, area_height, repeat}
      when is_number(tile_width) and is_number(tile_height) and is_number(area_width) and
             area_width >= 0 and is_number(area_height) and area_height >= 0 and
             repeat in [:repeat, :repeat_x, :repeat_y, :no_repeat] ->
        horizontal_over_limit? =
          repeat in [:repeat, :repeat_x] and tile_width > 0 and
            tile_width < area_width / max_background_image_tiles

        vertical_over_limit? =
          repeat in [:repeat, :repeat_y] and tile_height > 0 and
            tile_height < area_height / max_background_image_tiles

        case horizontal_over_limit? or vertical_over_limit? do
          true -> validate_background_image_tile_count(max_background_image_tiles + 1)
          false -> :ok
        end

      _ ->
        :error
    end
  end

  @doc false
  @spec validate_style_input(term(), term(), term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_style_input(dom, opts, font_options_result) do
    case prepare_style_input(dom, opts, font_options_result) do
      {:ok, _initial_controls?} -> :ok
      error -> error
    end
  end

  @doc false
  @spec assign_render_metadata([map()]) :: [map()]
  def assign_render_metadata(children), do: assign_selector_ids(children)

  @doc false
  @spec disclosure_children(map()) :: [map()]
  def disclosure_children(node) do
    {preceding, rest} = disclosure_parts(node)

    {summary, body} =
      case rest do
        [summary | following] ->
          {summary, preceding ++ following}

        [] ->
          [summary] =
            assign_render_metadata([
              %{
                type: :element,
                tag: "summary",
                attributes: %{},
                children: [%{type: :text, text: "Details"}]
              }
            ])

          reserve_render_resource(:max_rendered_text_bytes, byte_size("Details"), :style)
          {Map.put(summary, :_default_disclosure_summary, true), preceding}
      end

    open? = Map.has_key?(node.attributes, "open")
    summary = Map.put(summary, :_disclosure_open, open?)
    [summary | if(open?, do: body, else: [])]
  end

  @doc false
  @spec source_disclosure_children(map()) :: [map()]
  def source_disclosure_children(node) do
    {preceding, rest} = disclosure_parts(node)

    case rest do
      [summary | following] ->
        preceding ++
          [Map.put(summary, :_disclosure_open, Map.has_key?(node.attributes, "open")) | following]

      [] ->
        node.children
    end
  end

  defp disclosure_parts(node) do
    Enum.split_while(node.children, fn child ->
      not (Map.get(child, :tag) == "summary" and Map.get(child, :namespace, :html) == :html)
    end)
  end

  defp assign_selector_ids(
         nodes,
         path \\ [],
         parent_inert? \\ false,
         heading_offset \\ 0,
         parent_translation? \\ true
       ) do
    nodes
    |> Enum.with_index()
    |> Enum.map(fn {node, index} ->
      case node do
        %{type: :element, children: children} ->
          node =
            Map.drop(node, [
              :_html_editable,
              :_default_disclosure_summary,
              :_disclosure_open,
              :_table_model,
              :_table_cell
            ])

          inert? =
            parent_inert? or
              (Map.get(node, :namespace, :html) == :html and
                 is_nil(Map.get(Map.get(node, :attribute_namespaces, %{}), "inert")) and
                 html_inert?(node.attributes, false))

          translation? = HtmlMetadataValidator.translation_mode(node, parent_translation?)

          {heading_offset, heading_level} =
            resolve_html_heading_context(node, heading_offset)

          node =
            if inert?, do: Map.put(node, :_html_inert, true), else: Map.delete(node, :_html_inert)

          node =
            if is_nil(heading_level),
              do: Map.delete(node, :_heading_level),
              else: Map.put(node, :_heading_level, heading_level)

          node
          |> Map.put(:_html_translate, translation?)
          |> Map.put(:_selector_id, make_ref())
          |> Map.put(:_document_order, path ++ [index])
          |> Map.put(
            :_selector_classes,
            node.attributes
            |> Map.get("class", "")
            |> html_class_tokens()
            |> MapSet.new()
          )
          |> Map.put(
            :children,
            assign_selector_ids(children, path ++ [index], inert?, heading_offset, translation?)
          )

        _ ->
          node
      end
    end)
  end

  @doc false
  @spec validate_css_budget(term()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_css_budget(budget) do
    valid? =
      try do
        match?(%{size: 2, min: 0}, :atomics.info(budget))
      rescue
        ArgumentError -> false
      end

    if valid?,
      do: :ok,
      else:
        Diagnostics.error(
          :css,
          :invalid_options,
          "source contexts require the invocation's unsigned two-slot CSS budget",
          module: __MODULE__,
          source: "css_budget"
        )
  end

  @doc false
  @spec validate_source_style_input(term(), term(), term(), term()) ::
          {:ok, boolean()} | {:error, {atom(), Diagnostics.diagnostic()}}
  @spec validate_source_style_input(term(), term(), term(), term(), term()) ::
          {:ok, boolean()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_source_style_input(dom, sources, budget, consumer, opts \\ []) do
    with {:ok, controls?} <-
           prepare_style_input(
             dom,
             opts,
             NativeElixirPdfUtilities.HtmlToPdf.Font.normalize_options(opts)
           ) do
      budget_result = validate_css_budget(budget)

      cond do
        not (is_list(sources) and Enum.all?(sources, &is_binary/1)) ->
          Diagnostics.error(
            :css,
            :invalid_options,
            "source stylesheets must be a list of CSS strings; file and URL references require separate resource policy",
            module: __MODULE__,
            source: "stylesheets"
          )

        not is_function(consumer, 3) ->
          Diagnostics.error(
            :css,
            :invalid_options,
            "source context consumer must accept the original node, pseudo-element and limited context",
            module: __MODULE__,
            source: "consumer"
          )

        budget_result != :ok ->
          budget_result

        true ->
          {:ok, controls?}
      end
    end
  end

  @doc false
  @spec consume_source_style_context(function(), map(), atom() | nil, map()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def consume_source_style_context(consumer, node, pseudo, context) do
    case consumer.(node, pseudo, context) do
      :ok ->
        :ok

      {:error, {reason, diagnostic}} = error when is_atom(reason) and is_map(diagnostic) ->
        error

      _ ->
        Diagnostics.error(
          :css,
          :invalid_options,
          "source context consumer must return :ok or a shared diagnostic",
          module: __MODULE__,
          source: "consumer"
        )
    end
  end

  @doc false
  @spec source_style_error(map(), term()) :: {:error, {atom(), Diagnostics.diagnostic()}}
  @spec source_style_error(map(), term(), term()) :: {:error, {atom(), Diagnostics.diagnostic()}}
  def source_style_error(node, declaration, failure \\ nil) do
    source =
      case declaration do
        %{source: source} -> source
        _ -> Map.get(node, :source, %{text: Map.get(node.attributes, "style", node.tag)})
      end

    {reason, message} =
      case failure do
        {:error, {reason, diagnostic}} ->
          {reason, diagnostic.message}

        _ ->
          {:invalid_css,
           "declaration/context policy failed for #{inspect(declaration)}; font metrics and resource policy have not been checked"}
      end

    Diagnostics.error(
      :css,
      reason,
      message,
      module: __MODULE__,
      operation: :source_style_contexts,
      source: source.text,
      line: Map.get(source, :line),
      column: Map.get(source, :column)
    )
  end

  @doc false
  @spec prepare_style_input(term(), term(), term(), :render | :prepare_svg) ::
          {:ok, boolean()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_style_input(dom, opts, font_options_result, mode \\ :render) do
    with_render_budget(fn ->
      with {:ok, initial_controls?} <- validate_dom(dom, mode),
           :ok <- validate_style_options(opts, font_options_result) do
        {:ok, initial_controls?}
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
    end)
  end

  @doc "Releases the exact DOM validation memo after its style preparation has completed."
  @spec release_style_input(map()) :: :ok
  def release_style_input(dom) do
    key = {__MODULE__, :validated_dom}

    case Process.get(key) do
      {^dom, _limits, _mode, _result} -> Process.delete(key)
      _ -> :ok
    end

    :ok
  end

  @doc false
  @spec prepare_viewport_dimensions(term(), term()) ::
          {:ok, {number(), number()}} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_viewport_dimensions(width, height) do
    with_render_budget(fn ->
      case {width, height} do
        {width, height}
        when is_number(width) and width >= 0 and is_number(height) and height >= 0 ->
          check_render_resource(:max_css_numeric_magnitude, width, :layout)
          check_render_resource(:max_css_numeric_magnitude, height, :layout)
          {:ok, {width, height}}

        _ ->
          Diagnostics.error(
            :layout,
            :invalid_layout,
            "viewport dimensions must be nonnegative finite point values",
            operation: :layout_viewport,
            module: __MODULE__
          )
      end
    end)
  end

  @doc false
  @spec validate_layout_input(term(), term(), term(), term()) ::
          :ok
          | {:error, :invalid_layout | :invalid_margin | :invalid_page_size}
          | {:error,
             {:invalid_layout | :unsupported_html | :resource_limit_exceeded,
              Diagnostics.diagnostic()}}
  def validate_layout_input(styled_tree, opts, page_size_result, margins_result) do
    with_render_budget(fn ->
      case {styled_tree, opts} do
        {%{type: :document, children: children}, opts}
        when is_list(children) and is_list(opts) ->
          case Keyword.keyword?(opts) and
                 is_boolean(Map.get(styled_tree, :_css_pixel_viewport, false)) do
            true ->
              validate_render_tree(children)

              with :ok <-
                     NativeElixirPdfUtilities.Validators.HtmlLinkValidator.validate_expected_targets(
                       Map.get(styled_tree, :link_document_targets, []),
                       Map.get(styled_tree, :link_scope)
                     ),
                   :ok <- validate_layout_nodes(children),
                   :ok <- validate_initial_control_root_context(children),
                   :ok <-
                     NativeElixirPdfUtilities.Validators.CssSizingValidator.validate_root(
                       Map.get(styled_tree, :document_root_sizing)
                     ),
                   {:ok, page_size} <- page_size_result,
                   :ok <- validate_page_size(page_size),
                   {:ok, margins} <- margins_result,
                   :ok <- validate_margins(margins),
                   :ok <- validate_printable_area(page_size, margins),
                   :ok <-
                     NativeElixirPdfUtilities.Validators.MathLayoutValidator.validate_flow_geometry(
                       children,
                       elem(page_size, 0) - margins.left - margins.right,
                       elem(page_size, 1) - margins.top - margins.bottom
                     ) do
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
    end)
  end

  # Complete page layout needs an actual formatting container for an inline
  # control. The wrapper-free parser intentionally does not supply that owner;
  # child measurement instead receives its owner through the issued context.
  defp validate_initial_control_root_context(children) do
    Enum.reduce_while(children, :ok, fn node, :ok ->
      case node do
        %{type: :element, tag: tag, style: %{display: display, form_kind: _kind}}
        when tag in ~w(input textarea select button) and display in [:inline, :inline_block] ->
          {:halt,
           Diagnostics.error(
             :layout,
             :invalid_layout,
             "root #{tag} with inline display requires an actual HTML formatting container; use HtmlParser.parse_document_detailed/1 or an authored block container for complete page layout, or measure the child in its issued context",
             module: __MODULE__,
             operation: :layout,
             source: tag
           )}

        _ ->
          {:cont, :ok}
      end
    end)
  end

  @doc false
  @spec prepare_measured_child(term(), term(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_measured_child(node, context, constraints) do
    with :ok <- NativeElixirPdfUtilities.Validators.ResourceValidator.validate_context(context) do
      case {node, constraints} do
        {%{
           type: type,
           style:
             %{font_face: font, font_size: size, line_height: line_height, color: color} = style
         }, constraints}
        when type in [:text, :element] and is_map(font) and is_number(size) and size >= 0 and
               is_number(line_height) and line_height >= 0 and is_tuple(color) and
               is_map(constraints) ->
          allowed = [
            :available_width,
            :available_height,
            :unsupported_glyphs,
            :formatting_context
          ]

          style =
            NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare_initial_control_box(
              node,
              style
            )

          node = Map.put(node, :style, style)
          dimensions = Enum.map([:available_width, :available_height], &Map.get(constraints, &1))
          display = Map.get(style, :display, :inline)
          formatting_context = Map.get(constraints, :formatting_context, :flow)

          # CSS Display 3: inline flow produces an inline box only in flow
          # contexts. Math, flex and grid items instead form block containers.
          used_display =
            if display == :inline and formatting_context in [:math, :flex, :grid],
              do: :inline_block,
              else: display

          valid? =
            Enum.all?(Map.keys(constraints), &(&1 in allowed)) and
              formatting_context in [:flow, :math, :flex, :grid] and
              Enum.all?([size, line_height], &(&1 <= Limits.get(:max_css_numeric_magnitude))) and
              Enum.all?(
                dimensions,
                &(is_nil(&1) or
                    (is_number(&1) and &1 >= 0 and &1 <= Limits.get(:max_css_numeric_magnitude)))
              )

          cond do
            not valid? ->
              measured_child_failure(
                "child measurement requires bounded nonnegative available dimensions and formatting_context in [:flow, :math, :flex, :grid]",
                node
              )

            display not in [
              :inline,
              :block,
              :inline_block,
              :flex,
              :inline_flex,
              :grid,
              :inline_grid,
              :image,
              :line_break,
              :soft_break,
              :none
            ] ->
              Diagnostics.error(
                :layout,
                :unsupported_html,
                "intrinsic measurement of #{display} HTML children requires its shared layout adapter",
                operation: :measure_child,
                module: __MODULE__,
                source: Map.get(node, :tag, "text")
              )

            true ->
              with :ok <-
                     validate_unsupported_glyphs(
                       Map.get(constraints, :unsupported_glyphs, :replace)
                     ),
                   :ok <-
                     (case Map.fetch(style, :_math_box) do
                        {:ok, measured} ->
                          case NativeElixirPdfUtilities.Validators.MathLayoutValidator.validate_measured_geometry(
                                 measured
                               ) do
                            {:error, {reason, diagnostic}} ->
                              {:error,
                               {reason,
                                Diagnostics.with_context(diagnostic,
                                  operation: :measure_child,
                                  source: Map.get(node, :tag, "math")
                                )}}

                            result ->
                              result
                          end

                        :error ->
                          :ok
                      end) do
                # The measured subtree has a real unfragmented owner supplied by
                # this validated entry, even when its external Math/inline owner
                # is absent from the isolated subtree. Flow intent still requires
                # an actual inline-block node in validate_atomic_context/2.
                case validate_layout_nodes([node], formatting_context, true) do
                  :ok ->
                    {:ok,
                     %{
                       node: node,
                       unsupported_glyphs: Map.get(constraints, :unsupported_glyphs, :replace),
                       context: context,
                       available_width: Map.get(constraints, :available_width),
                       available_height: Map.get(constraints, :available_height),
                       used_display: used_display,
                       atomic:
                         used_display in [
                           :block,
                           :inline_block,
                           :flex,
                           :inline_flex,
                           :grid,
                           :inline_grid
                         ]
                     }}

                  {:error, {_reason, _diagnostic}} = error ->
                    error

                  _ ->
                    measured_child_failure(
                      "child measurement requires a validated styled node",
                      node
                    )
                end
              end
          end

        _ ->
          measured_child_failure(
            "child measurement requires a styled node and supported constraints",
            node
          )
      end
    end
  end

  @doc false
  @spec validate_measured_child_paint(term(), term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_measured_child_paint(boxes, node) do
    cond do
      not is_list(boxes) ->
        measured_child_failure("child layout did not return ordered native boxes", node)

      Enum.any?(boxes, &(not is_map(&1) or Map.get(&1, :type) == :layout_error)) ->
        measured_child_failure("child content could not be laid out", node)

      true ->
        result =
          with {:ok, measured} <-
                 NativeElixirPdfUtilities.Validators.ResponsiveImageValidator.measurement_boxes(
                   boxes
                 ),
               do:
                 NativeElixirPdfUtilities.Validators.WriterValidator.validate_native_boxes(
                   measured
                 )

        case result do
          :ok ->
            :ok

          {:error, {reason, diagnostic}} ->
            Diagnostics.error(:layout, reason, diagnostic.message,
              operation: :measure_child,
              module: diagnostic.module,
              source: if(is_map(node), do: Map.get(node, :tag, "HTML child"), else: "HTML child")
            )
        end
    end
  end

  defp measured_child_failure(message, node) do
    Diagnostics.error(:layout, :invalid_layout, message,
      operation: :measure_child,
      module: __MODULE__,
      source: if(is_map(node), do: Map.get(node, :tag, "text"), else: "HTML child")
    )
  end

  @doc false
  @spec validate_pagination_input(term(), term(), term()) ::
          :ok
          | {:error,
             {:invalid_layout | :unsupported_html | :resource_limit_exceeded,
              Diagnostics.diagnostic()}}
  def validate_pagination_input(layout_tree, opts, margins_result) do
    with_render_budget(fn ->
      case {layout_tree, opts} do
        {%{type: :layout, page_size: {width, height} = page_size, boxes: boxes}, opts}
        when is_number(width) and width > 0 and is_number(height) and height > 0 and
               is_list(boxes) and is_list(opts) ->
          check_render_resource(:max_layout_boxes, length(boxes), :pagination)

          with true <- Keyword.keyword?(opts),
               {:ok, margins} <- margins_result,
               :ok <- validate_page_size(page_size),
               :ok <- validate_margins(margins),
               :ok <- validate_printable_area(page_size, margins),
               :ok <-
                 NativeElixirPdfUtilities.Validators.HtmlLinkValidator.validate_boxes(
                   boxes,
                   Map.get(layout_tree, :link_scope)
                 ),
               :ok <-
                 NativeElixirPdfUtilities.Validators.HtmlLinkValidator.validate_expected_targets(
                   Map.get(layout_tree, :link_expected_targets, []),
                   Map.get(layout_tree, :link_scope)
                 ),
               :ok <-
                 NativeElixirPdfUtilities.Validators.HtmlLinkValidator.validate_document_targets(
                   Map.get(layout_tree, :link_document_targets, []),
                   Map.get(layout_tree, :link_scope),
                   page_size
                 ),
               :ok <-
                 validate_fieldset_pagination_metadata(
                   boxes,
                   height - margins.top - margins.bottom
                 ),
               :ok <- validate_paragraph_line_limits(boxes, :pagination) do
            :ok
          else
            {:error, {_reason, _diagnostic}} = error -> error
            _ -> invalid_pagination()
          end

        _ ->
          invalid_pagination()
      end
    end)
  end

  # Pagination consumes already prepared positive CSS line counts, including
  # directly supplied layout boxes. CSS numeric magnitude remains the bound.
  defp validate_paragraph_line_limits(boxes, stage) do
    Enum.reduce_while(boxes, :ok, fn box, :ok ->
      case validate_paragraph_line_counts(box, stage) do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  @doc "Validates the canonical positive orphan and widow counts carried by one source paragraph."
  @spec validate_paragraph_line_counts(term(), atom()) ::
          :ok | {:error, Diagnostics.detailed_error()}
  def validate_paragraph_line_counts(box, stage) do
    counts =
      case box do
        box when is_map(box) -> [Map.get(box, :orphans, 2), Map.get(box, :widows, 2)]
        _ -> nil
      end

    if counts != nil and Enum.all?(counts, &(is_integer(&1) and &1 > 0)) do
      Enum.each(counts, &check_render_resource(:max_css_numeric_magnitude, &1, stage))
      :ok
    else
      Diagnostics.error(
        stage,
        :invalid_layout,
        "orphans and widows require positive validator-prepared integer line counts",
        module: __MODULE__,
        operation: if(stage == :layout, do: :layout, else: :paginate),
        source: "orphans/widows"
      )
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
  @spec aspect_ratio_dimension(number(), number(), :width | :height, atom()) :: number()
  def aspect_ratio_dimension(size, ratio, axis, stage) do
    try do
      dimension = if axis == :height, do: size / ratio, else: size * ratio
      check_render_resource(:max_css_numeric_magnitude, abs(dimension), stage)
      dimension
    rescue
      ArithmeticError ->
        throw(
          {:render_resource_limit,
           Diagnostics.error(
             stage,
             :resource_limit_exceeded,
             "aspect-ratio produces an unrepresentable #{axis}; reduce the dimensions or use a less extreme ratio",
             source: "aspect-ratio",
             module: __MODULE__
           )}
        )
    end
  end

  @doc false
  @spec parse_aspect_ratio(String.t()) :: {:ok, float()} | :error
  def parse_aspect_ratio(value) do
    tokens = value |> String.trim() |> String.split("/", trim: true) |> Enum.map(&String.trim/1)

    case tokens do
      [ratio] ->
        case parse_css_number(ratio) do
          {number, ""} when number > 0 -> {:ok, number}
          _ -> :error
        end

      [width, height] ->
        with {width, ""} when width > 0 <- parse_css_number(width),
             {height, ""} when height > 0 <- parse_css_number(height) do
          ratio = aspect_ratio_dimension(width, height, :height, :css)
          if ratio > 0, do: {:ok, ratio}, else: :error
        else
          _ -> :error
        end

      _ ->
        :error
    end
  end

  @doc false
  @spec valid_html_float?(term()) :: boolean()
  def valid_html_float?(value) do
    is_binary(value) and
      Regex.match?(~r/\A-?(?:[0-9]+(?:\.[0-9]+)?|\.[0-9]+)(?:[eE][+-]?[0-9]+)?\z/, value) and
      match?({:ok, _}, parse_html_float(value))
  end

  @doc "Parses HTML's permissive list-of-floating-point-numbers microsyntax using the shared scalar parser."
  @spec parse_html_float_list(String.t()) :: [float()]
  def parse_html_float_list(value) do
    value
    |> String.split(~r/[ \t\n\r\f,;]+/, trim: true)
    |> Enum.map(fn token ->
      token = Regex.replace(~r/\A[^0-9.\-]*/, token, "")

      case parse_html_float(token) do
        {:ok, number} -> number
        _ -> 0.0
      end
    end)
  end

  @doc false
  @spec parse_html_float(term(), :float | :decimal) :: {:ok, float() | map()} | :error
  def parse_html_float(value, representation \\ :float) do
    # HTML's UA grammar accepts a numeric prefix, unlike author conformance and CSS.
    # Finite IEEE double precision is a format invariant, not a geometry ceiling.
    case is_binary(value) &&
           Regex.run(
             ~r/\A[ \t\n\r\f]*([+-]?)([0-9]+(?:\.[0-9]*)?|\.[0-9]+)([eE][+-]?[0-9]+)?/,
             value
           ) do
      [token, sign, significand | exponent] ->
        check_render_resource(:max_html_numeric_token_bytes, byte_size(token), :html)

        original_significand = significand

        significand =
          if String.starts_with?(significand, "."), do: "0" <> significand, else: significand

        significand =
          if String.ends_with?(significand, "."), do: significand <> "0", else: significand

        significand =
          if String.contains?(significand, "."), do: significand, else: significand <> ".0"

        case Float.parse(sign <> significand <> Enum.join(exponent)) do
          {number, ""} ->
            number = if number == 0, do: 0.0, else: number

            case representation do
              :float ->
                {:ok, number}

              :decimal ->
                # Chromium Decimal uses a fixed 18-digit coefficient and decimal
                # exponent range -1023..1023. These are source format invariants,
                # not configurable processing ceilings. Reuse the parsed token.
                [integer | fraction] = String.split(original_significand, ".")
                integer = String.trim_leading(integer, "0")
                fraction = Enum.join(fraction)
                integer_digits = byte_size(integer)
                fraction_digits = min(byte_size(fraction), max(18 - integer_digits, 0))
                coefficient = String.slice(integer <> fraction, 0, 18)
                coefficient = String.trim_leading(coefficient, "0")
                digits = max(byte_size(coefficient), 1)

                exponent =
                  case exponent do
                    [] -> 0
                    [value] -> value |> String.slice(1..-1//1) |> String.to_integer()
                  end

                exponent = exponent - fraction_digits + max(integer_digits - 18, 0)
                zero? = coefficient == "" or exponent < -1023

                {:ok,
                 %{
                   value: number,
                   coefficient: if(zero?, do: 0, else: String.to_integer(coefficient)),
                   negative: sign == "-" and not zero?,
                   coefficient_digits: if(zero?, do: 1, else: digits),
                   exponent: if(zero?, do: 0, else: exponent)
                 }}
            end

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  @doc false
  @spec parse_css_accent_color(String.t(), term()) :: {:ok, term()} | :error
  def parse_css_accent_color(value, current_color) do
    case value |> trim_ascii_whitespace() |> String.downcase(:ascii) do
      "auto" -> {:ok, :auto}
      _ -> parse_css_color(value, current_color)
    end
  end

  @doc false
  @spec parse_control_appearance(term()) :: {:ok, :auto | :none | :base_select} | :error
  def parse_control_appearance(value) do
    if value |> trim_ascii_whitespace() |> String.downcase(:ascii) == "base-select",
      do: {:ok, :base_select},
      else: parse_gauge_appearance(value)
  end

  @doc false
  @spec parse_gauge_appearance(term()) :: {:ok, :auto | :none} | :error
  def parse_gauge_appearance(value) do
    case value |> trim_ascii_whitespace() |> String.downcase(:ascii) do
      value when value in ~w(auto progress-bar meter) -> {:ok, :auto}
      "none" -> {:ok, :none}
      _ -> :error
    end
  end

  @doc false
  @spec parse_vertical_align(term(), number() | :source) :: {:ok, term()} | :error
  def parse_vertical_align(value, font_size) do
    case value |> trim_ascii_whitespace() |> String.downcase(:ascii) do
      "baseline" ->
        {:ok, :baseline}

      "sub" ->
        {:ok, :sub}

      "super" ->
        {:ok, :super}

      "top" ->
        {:ok, :top}

      "middle" ->
        {:ok, :middle}

      "bottom" ->
        {:ok, :bottom}

      value ->
        if font_size == :source,
          do: prepare_css_length_source(value, :signed_length),
          else: parse_css_length(value, :margin, font_size)
    end
  end

  @doc false
  @spec normalize_geometry_number(term()) :: {:ok, float()} | :error
  def normalize_geometry_number(value) do
    case is_number(value) && parse_css_number(to_string(value)) do
      {number, ""} -> {:ok, number}
      _ -> :error
    end
  end

  @doc false
  @spec parse_css_number(term(), number()) :: {float(), binary()} | :error
  def parse_css_number(value, scale \\ 1.0) do
    limit = Limits.get(:max_css_numeric_magnitude)

    case is_binary(value) and is_number(scale) do
      true ->
        case Regex.run(~r/\A([+-]?(?:\d+|(?=\.\d)))(\.\d+)?([eE][+-]?\d+)?(.*)\z/s, value) do
          [_, integer, fraction, exponent, rest] ->
            integer = if integer in ["", "+", "-"], do: integer <> "0", else: integer
            fraction = if fraction == "", do: ".0", else: fraction

            try do
              number = :erlang.binary_to_float(integer <> fraction <> exponent)

              with true <- abs(number) <= limit and abs(scale) <= limit,
                   scaled = number * scale,
                   true <- abs(scaled) <= limit do
                {scaled, rest}
              else
                false -> :error
              end
            rescue
              ArgumentError -> :error
            end

          _ ->
            :error
        end

      false ->
        :error
    end
  end

  @doc "Validates a prepared CSS size expression using the shared numeric, depth and work bounds."
  @spec validate_prepared_css_size(term()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  @spec validate_prepared_css_size(term(), term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_prepared_css_size(value, context \\ :width) do
    if context in [:size, :width, :height, :min_width, :max_width, :min_height, :max_height] and
         prepared_css_size?(value, 0, context) do
      :ok
    else
      Diagnostics.error(
        :layout,
        :invalid_layout,
        "prepared CSS sizes require nonnegative bounded point lengths, percentages, auto or proper min expressions",
        operation: :validate_css_size,
        module: __MODULE__
      )
    end
  catch
    {:render_resource_limit, error} -> error
  end

  defp prepared_css_size?(value, depth, context) do
    check_render_resource(:max_html_depth, depth, :layout)
    reserve_render_resource(:max_css_work, 1, :layout)

    case value do
      nil ->
        depth == 0

      :auto ->
        depth == 0 and context in [:width, :height]

      number when is_number(number) ->
        number >= 0 and number <= Limits.get(:max_css_numeric_magnitude)

      {:percent, ratio} when is_number(ratio) ->
        ratio >= 0 and ratio <= Limits.get(:max_css_numeric_magnitude) / 100

      {:css_size_math, _} = calculation ->
        NativeElixirPdfUtilities.Validators.CssSizingValidator.valid_calculation?(calculation)

      {:min, [_ | _] = values} ->
        prepared_css_sizes?(values, depth + 1)

      _ ->
        false
    end
  end

  defp prepared_css_sizes?(values, depth) do
    case values do
      [] ->
        true

      [value | rest] ->
        prepared_css_size?(value, depth, :size) and prepared_css_sizes?(rest, depth)

      _ ->
        false
    end
  end

  @doc false
  @spec parse_css_size(
          String.t(),
          :size | :width | :height | :min_width | :max_width | :min_height | :max_height
        ) :: {:ok, term()} | :error
  @spec parse_css_size(String.t(), atom(), number() | nil) :: {:ok, term()} | :error
  def parse_css_size(value, context \\ :size, em_scale \\ nil) do
    normalized = String.trim(value)

    cond do
      context in [:width, :height] and String.downcase(normalized, :ascii) == "auto" ->
        {:ok, :auto}

      Regex.match?(~r/^\d+(?:\.\d+)?%$/u, normalized) ->
        with {number, "%"} <- parse_css_number(normalized),
             do: {:ok, {:percent, number / 100}}

      String.starts_with?(String.downcase(normalized), "min(") ->
        case Regex.named_captures(~r/^min\((?<sizes>.+)\)$/u, String.downcase(normalized)) do
          %{"sizes" => sizes} ->
            parsed_sizes =
              sizes |> split_css_comma() |> Enum.map(&parse_css_size(&1, :size, em_scale))

            case Enum.all?(parsed_sizes, &match?({:ok, _size}, &1)) do
              true ->
                if Enum.any?(parsed_sizes, &match?({:ok, {:css_size_math, _}}, &1)),
                  do: NativeElixirPdfUtilities.Validators.CssSizingValidator.prepare_math(value),
                  else: {:ok, {:min, Enum.map(parsed_sizes, fn {:ok, size} -> size end)}}

              false ->
                NativeElixirPdfUtilities.Validators.CssSizingValidator.prepare_math(value)
            end

          _ ->
            NativeElixirPdfUtilities.Validators.CssSizingValidator.prepare_math(value)
        end

      true ->
        case parse_css_length(value, :nonnegative, em_scale) do
          :error -> NativeElixirPdfUtilities.Validators.CssSizingValidator.prepare_math(value)
          result -> result
        end
    end
  end

  @doc false
  @spec parse_css_margin(term()) :: {:ok, float() | {:rem, float()} | :auto} | :error
  def parse_css_margin(value) do
    case value do
      value when is_binary(value) ->
        normalized = trim_ascii_whitespace(value)

        if byte_size(normalized) == byte_size("auto") and
             String.downcase(normalized, :ascii) == "auto",
           do: {:ok, :auto},
           else: parse_css_length(normalized, :margin)

      _ ->
        :error
    end
  end

  @doc false
  @spec parse_css_box_lengths(
          term(),
          :margin | :nonnegative | {:source, :margin | :nonnegative | :length}
        ) ::
          {:ok, map()} | :error
  def parse_css_box_lengths(value, context \\ :nonnegative) do
    case value do
      value when is_binary(value) ->
        tokens =
          case context do
            {:source, _} ->
              NativeElixirPdfUtilities.Validators.CssMathValidator.component_terms(value, false)

            _ ->
              String.split(value, ~r/[ \t\n\f\r]+/, trim: true)
          end

        parsed =
          tokens
          |> Enum.map(fn token ->
            case context do
              :margin -> parse_css_margin(token)
              :nonnegative -> parse_css_length(token, :nonnegative)
              {:source, source_context} -> prepare_css_length_source(token, source_context)
              _ -> :error
            end
          end)

        case parsed do
          [{:ok, all}] ->
            {:ok, %{top: all, right: all, bottom: all, left: all}}

          [{:ok, vertical}, {:ok, horizontal}] ->
            {:ok, %{top: vertical, right: horizontal, bottom: vertical, left: horizontal}}

          [{:ok, top}, {:ok, horizontal}, {:ok, bottom}] ->
            {:ok, %{top: top, right: horizontal, bottom: bottom, left: horizontal}}

          [{:ok, top}, {:ok, right}, {:ok, bottom}, {:ok, left}] ->
            {:ok, %{top: top, right: right, bottom: bottom, left: left}}

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  @doc "Prepares source length grammar with symbolic unit and percentage dependencies, without measured scales."
  @spec prepare_css_length_source(String.t(), :nonnegative | :margin | :length | :signed_length) ::
          {:ok, map() | :auto} | :error | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_css_length_source(value, context \\ :nonnegative) do
    if context in [:nonnegative, :margin, :length, :signed_length] do
      case NativeElixirPdfUtilities.Validators.CssMathValidator.length_dependencies(value) do
        {:ok, dependencies} ->
          case parse_css_number(value) do
            {number, _} when number < 0 and context not in [:margin, :signed_length] ->
              :error

            _ ->
              if context in [:length, :signed_length] and dependencies.percentage?,
                do: :error,
                else: {:ok, %{value: value, geometry: dependencies}}
          end

        :error ->
          parsed =
            if context == :margin,
              do: parse_css_margin(value),
              else:
                parse_css_length(
                  value,
                  if(context == :signed_length, do: :margin, else: :nonnegative)
                )

          case parsed do
            {:ok, :auto} -> {:ok, :auto}
            {:ok, _} -> {:ok, %{value: value, geometry: %{units: [], percentage?: false}}}
            _ -> :error
          end

        error ->
          error
      end
    else
      :error
    end
  end

  @doc false
  @spec parse_css_length(term(), :nonnegative | :margin, number() | nil) ::
          {:ok, float() | {:rem, float()}} | :error
  def parse_css_length(value, context, em_scale \\ nil) do
    with true <- is_binary(value) and context in [:nonnegative, :margin],
         {number, unit} <- parse_css_number(String.trim(value)) do
      scale =
        case unit do
          "" -> if number == 0, do: 1.0
          "pt" -> 1.0
          "px" -> 0.75
          "mm" -> 72.0 / 25.4
          "cm" -> 72.0 / 2.54
          "in" -> 72.0
          "rem" -> 1.0
          "em" -> em_scale
          _ -> nil
        end

      limit = Limits.get(:max_css_numeric_magnitude)

      with true <- is_number(scale) and abs(scale) <= limit,
           true <- context == :margin or number >= 0,
           scaled = number * scale,
           true <- abs(scaled) <= limit do
        {:ok, if(unit == "rem", do: {:rem, scaled}, else: scaled)}
      else
        _ -> :error
      end
    else
      _ -> :error
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

            %{family: family, data: candidates, weight: weight, style: style}
            when map_size(font) == 4 and is_binary(family) and is_list(candidates) and
                   is_number(weight) and weight >= 100 and weight <= 900 and
                   style in [:normal, :italic] ->
              String.trim(family) != "" and candidates != [] and
                Enum.all?(candidates, &(is_binary(&1) and byte_size(&1) > 0))

            _ ->
              false
          end
        end)

    case valid? do
      true -> :ok
      false -> :error
    end
  end

  @doc false
  @spec validate_font_embedding(String.t(), term(), term()) ::
          :ok | {:error, {:invalid_document, Diagnostics.diagnostic()}}
  def validate_font_embedding(family, embedding_flags, variable_font?) do
    restricted? =
      is_integer(embedding_flags) and
        (Bitwise.band(embedding_flags, 0x0002) != 0 or
           Bitwise.band(embedding_flags, 0x0200) != 0)

    case {variable_font?, restricted?} do
      {true, _restricted?} ->
        Diagnostics.error(
          :font,
          :invalid_document,
          "font #{inspect(family)} uses OpenType variations; provide a static font instance for PDF embedding",
          source: family
        )

      {_variable_font?, true} ->
        Diagnostics.error(
          :font,
          :invalid_document,
          "font #{inspect(family)} does not permit outline embedding in a PDF",
          source: family
        )

      {_variable_font?, false} ->
        :ok
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
            with {:ok, _mode} <-
                   NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare_mode(
                     Keyword.get(opts, :forms, :interactive),
                     :options
                   ),
                 do: validate_style_options(opts, font_options_result)

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
        with :ok <- NativeElixirPdfUtilities.Validators.PrintMediaValidator.validate_options(opts),
             :ok <- validate_stylesheets(Keyword.get(opts, :stylesheets, [])),
             :ok <-
               validate_asset_options(
                 Keyword.get(opts, :assets, %{}),
                 Keyword.get(opts, :asset_resolver)
               ),
             :ok <- validate_base_url(Keyword.get(opts, :base_url)),
             :ok <- validate_default_font(Keyword.get(opts, :default_font, "DejaVu Sans")),
             :ok <-
               validate_system_font_discovery(Keyword.get(opts, :system_font_discovery, true)),
             :ok <- validate_unsupported_glyphs(Keyword.get(opts, :unsupported_glyphs, :replace)),
             {:ok, output_opts} <- HtmlUrlValidator.prepare_style_output_options(opts),
             :ok <- validate_pdf_output_options(output_opts),
             :ok <- validate_outline_option(Keyword.get(opts, :outlines)),
             :ok <- validate_font_options_result(font_options_result) do
          :ok
        end

      false ->
        Diagnostics.error(:style, :invalid_document, "style options must be a keyword list")
    end
  end

  defp validate_outline_option(outlines) do
    case is_nil(outlines) or outlines == false or outlines == :headings or is_list(outlines) do
      true ->
        :ok

      false ->
        Diagnostics.error(
          :options,
          :invalid_options,
          "outlines must be :headings, a list, false, or nil"
        )
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

  @doc false
  @spec validate_system_font_discovery(term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_system_font_discovery(system_font_discovery) do
    case is_boolean(system_font_discovery) do
      true ->
        :ok

      false ->
        Diagnostics.error(
          :options,
          :invalid_options,
          "system_font_discovery must be true or false"
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

  # Walk before recursive semantic validation, without creating a flattened tree.
  defp validate_render_tree(nodes, depth \\ 0, count \\ 0, groups \\ %{}, ancestor_groups \\ []) do
    check_render_resource(:max_html_depth, depth, :html)

    Enum.reduce(nodes, {count, groups}, fn node, {count, groups} ->
      check_render_resource(:max_html_nodes, count + 1, :html)

      case node do
        %{tag: "template", children: [], content: %{type: :document_fragment, children: children}}
        when is_list(children) ->
          check_render_resource(:max_html_nodes, count + 2, :html)
          {count, _fragment_groups} = validate_render_tree(children, depth + 1, count + 2)
          {count, groups}

        %{tag: "details", attributes: attributes, children: children}
        when is_map(attributes) and is_list(children) ->
          if Map.get(node, :namespace, :html) == :html do
            {groups, child_groups} =
              validate_disclosure_group(attributes, groups, ancestor_groups)

            validate_render_tree(children, depth + 1, count + 1, groups, child_groups)
          else
            validate_render_tree(children, depth + 1, count + 1, groups, ancestor_groups)
          end

        %{children: children} when is_list(children) ->
          validate_render_tree(children, depth + 1, count + 1, groups, ancestor_groups)

        %{type: :processing_instruction, data: data} when is_binary(data) ->
          check_render_resource(:max_html_source_bytes, byte_size(data), :html)
          {count + 1, groups}

        %{text: text} when is_binary(text) ->
          check_render_resource(:max_html_source_bytes, byte_size(text), :html)
          {count + 1, groups}

        _ ->
          {count + 1, groups}
      end
    end)
  end

  # Named disclosure groups are a document-tree rule; template fragments have
  # independent groups. Closed members also cannot nest in their own group.
  defp validate_disclosure_group(attributes, groups, ancestors) do
    case Map.fetch(attributes, "name") do
      {:ok, name} when is_binary(name) ->
        open? = Map.has_key?(attributes, "open")

        message =
          cond do
            name == "" ->
              "details name must be nonempty; omit name for an independent disclosure"

            name in ancestors ->
              "details cannot nest within another member of the same named group"

            open? and Map.get(groups, name, false) ->
              "a named details group may have only one initially open member; remove the extra open attribute"

            true ->
              nil
          end

        if message do
          throw(
            {:render_resource_limit,
             Diagnostics.error(:html, :invalid_document, message,
               operation: :compute,
               module: __MODULE__,
               source: "details name=" <> name
             )}
          )
        end

        {Map.put(groups, name, open? or Map.get(groups, name, false)), [name | ancestors]}

      _ ->
        {groups, ancestors}
    end
  end

  defp validate_dom(node, mode) do
    key = {__MODULE__, :validated_dom}
    limits = Limits.effective()

    case Process.get(key) do
      {^node, ^limits, ^mode, result} ->
        result

      _ ->
        result =
          case {node, mode} do
            {%{type: :document, children: children}, mode}
            when is_list(children) and mode in [:render, :prepare_svg] ->
              validate_render_tree(children)

              with :ok <-
                     NativeElixirPdfUtilities.Validators.HtmlTreeValidator.validate_transport(
                       node
                     ),
                   {:ok, controls?} <-
                     validate_dom_nodes(children, false, nil, [], nil, :html, mode),
                   :ok <-
                     if(mode == :render,
                       do:
                         NativeElixirPdfUtilities.Validators.SvgSceneValidator.validate_document_svg(
                           children
                         ),
                       else: :ok
                     ),
                   do: {:ok, controls?}

            _ ->
              :invalid_document
          end

        # Only exact successful input under identical limits and processing mode
        # can be reused. SVG preparation does not authorize rendering its content.
        # Options and resource authorization remain independently validated.
        if match?({:ok, _}, result) and Process.get({__MODULE__, :render_budget}) != nil,
          do: Process.put(key, {node, limits, mode, result})

        result
    end
  end

  defp validate_dom_nodes(nodes, inert?, parent, ancestors, tree, parent_namespace, mode) do
    tree = tree || nodes

    Enum.reduce_while(nodes, {:ok, false}, fn node, {:ok, initial_controls?} ->
      result =
        case node do
          %{type: :processing_instruction} ->
            with {:ok, _node} <- validate_html_processing_instruction(node) do
              if parent_namespace == :html and
                   (html_text_mode(parent) != :data or parent in @void_tags) do
                message =
                  if parent in @void_tags,
                    do: "#{parent} is a void element and requires empty children",
                    else:
                      "#{parent} requires literal text-only contents, not processing instruction nodes"

                Diagnostics.error(
                  :style,
                  :invalid_document,
                  message,
                  module: __MODULE__,
                  operation: :compute,
                  source: parent
                )
              else
                {:ok, false}
              end
            end

          %{type: :text, text: text} when is_binary(text) ->
            if String.valid?(text, :fast_ascii),
              do: {:ok, false},
              else:
                Diagnostics.error(:style, :invalid_document, "HTML text requires valid UTF-8",
                  module: __MODULE__,
                  operation: :compute,
                  source: "text"
                )

          %{type: :element, tag: tag, attributes: attributes, children: children}
          when is_binary(tag) and is_map(attributes) and is_list(children) ->
            attribute_error_message =
              cond do
                not is_map(Map.get(node, :attribute_namespaces, %{})) ->
                  "attribute_namespaces must be a map"

                Enum.all?(attributes, fn {name, value} ->
                  is_binary(name) and is_binary(value) and String.valid?(name) and
                      String.valid?(value, :fast_ascii)
                end) ->
                  nil

                true ->
                  "HTML attributes require valid UTF-8 string names and values"
              end

            case attribute_error_message do
              nil ->
                html_tag = if Map.get(node, :namespace, :html) == :html, do: tag, else: nil

                with :ok <-
                       validate_static_url_attributes(
                         if(html_tag == "input",
                           do:
                             NativeElixirPdfUtilities.Validators.HtmlFormValidator.initial_control_attributes(
                               node
                             ),
                           else:
                             if(html_tag == "body",
                               do: HtmlMetadataValidator.unqualified_attributes(node),
                               else: attributes
                             )
                         ),
                         html_tag
                       ),
                     :ok <- HtmlMetadataValidator.validate_attributes(node),
                     :ok <- HtmlMetadataValidator.validate_item_references(node, tree),
                     {:ok, _custom?} <- validate_dom_html_element(node, parent, ancestors),
                     :ok <-
                       NativeElixirPdfUtilities.Validators.ForeignContentValidator.validate_render_support(
                         node,
                         not inert?,
                         mode
                       ),
                     :ok <-
                       (if tag in ["base", "script", "template"] do
                          case validate_html_element_attributes(tag, attributes) do
                            :ok ->
                              :ok

                            {:error, {reason, diagnostic}} ->
                              {:error,
                               {reason,
                                Diagnostics.with_context(diagnostic,
                                  operation: :compute,
                                  source: tag
                                )}}
                          end
                        else
                          :ok
                        end),
                     {:ok, child_controls?} <-
                       if(tag == "template" and Map.get(node, :namespace, :html) == :html,
                         do:
                           case validate_template_content(node) do
                             :ok -> {:ok, false}
                             error -> error
                           end,
                         else:
                           validate_dom_nodes(
                             children,
                             inert?,
                             if(Map.get(node, :namespace, :html) == :html,
                               do: tag,
                               else:
                                 NativeElixirPdfUtilities.Validators.ForeignContentValidator.child_context(
                                   node
                                 )
                             ),
                             if(is_nil(parent), do: ancestors, else: [parent | ancestors]),
                             tree,
                             Map.get(node, :namespace, :html),
                             mode
                           )
                       ) do
                  semantic_attribute =
                    cond do
                      is_nil(html_tag) ->
                        nil

                      Map.has_key?(attributes, "multiple") and
                        valid_boolean_form_attribute?(tag, "multiple") and
                          is_nil(Map.get(Map.get(node, :attribute_namespaces, %{}), "multiple")) ->
                        "multiple"

                      Map.has_key?(attributes, "readonly") and
                        valid_boolean_form_attribute?(tag, "readonly") and
                          is_nil(Map.get(Map.get(node, :attribute_namespaces, %{}), "readonly")) ->
                        "readonly"

                      tag == "input" ->
                        "type"

                      tag == "script" ->
                        "script"

                      tag == "select" ->
                        "select"

                      tag == "noscript" ->
                        "noscript"

                      true ->
                        nil
                    end

                  if semantic_attribute &&
                       not (case tag do
                              "script" ->
                                valid_html_element?(tag, attributes, children)

                              "noscript" ->
                                valid_noscript_element?(children, parent, ancestors)

                              _ ->
                                valid_form_element?(
                                  tag,
                                  if(tag == "input",
                                    do:
                                      NativeElixirPdfUtilities.Validators.HtmlFormValidator.initial_control_attributes(
                                        node
                                      ),
                                    else: attributes
                                  ),
                                  children
                                )
                            end) do
                    message =
                      case semantic_attribute do
                        "type" ->
                          "input requires a supported effective type, empty contents and applicable checked/multiple/readonly attributes"

                        "script" ->
                          "script data blocks require text-only contents"

                        "noscript" ->
                          "noscript requires scripting-disabled transparent contents without nested noscript descendants; head contents accept only supported style and meta elements"

                        "select" ->
                          "select requires supported option contents; only data scripts and ordinary templates may accompany options"

                        "multiple" ->
                          "multiple requires a valid email input"

                        "readonly" ->
                          "readonly requires a valid text/search/tel/url/email/password/number input or textarea"
                      end

                    Diagnostics.error(
                      :style,
                      :invalid_document,
                      message,
                      operation: :compute,
                      module: __MODULE__,
                      source: tag
                    )
                  else
                    if child_controls? or
                         (tag in ~w(input select textarea button form label fieldset progress meter output datalist) and
                            Map.get(node, :namespace, :html) == :html),
                       do: {:ok, true},
                       else: {:ok, false}
                  end
                end

              message ->
                Diagnostics.error(
                  :style,
                  :invalid_document,
                  message,
                  operation: :compute,
                  module: __MODULE__,
                  source: tag
                )
            end

          _ ->
            :invalid_document
        end

      case result do
        {:ok, true} ->
          {:cont, {:ok, true}}

        {:ok, false} ->
          if initial_controls?, do: {:cont, {:ok, true}}, else: {:cont, {:ok, false}}

        error ->
          {:halt, error}
      end
    end)
  end

  # Name support and transparent custom content are validated before Style uses defaults.
  defp validate_dom_html_element(node, parent, ancestors) do
    if Map.get(node, :namespace, :html) == :html do
      native? = Map.has_key?(@supported_tags, node.tag)
      custom? = not native? and custom_html_tag?(node.tag)

      cond do
        not native? and not custom? ->
          Diagnostics.error(
            :style,
            :invalid_document,
            "HTML tag requires a supported builtin or a valid unregistered custom element name",
            operation: :compute,
            module: __MODULE__,
            source: node.tag
          )

        node.tag in ~w(picture source) and
            (not valid_html_child?(parent || :document, node.tag, ancestors) or
               not valid_html_element?(node.tag, node.attributes, node.children)) ->
          Diagnostics.error(
            :style,
            :invalid_document,
            "picture requires sources before one img, source requires a picture parent, and images/sources are void",
            operation: :compute,
            module: __MODULE__,
            source: node.tag
          )

        node.tag in ["a", "button", "canvas"] and
            not valid_html_element?(node.tag, node.attributes, node.children) ->
          Diagnostics.error(
            :style,
            :invalid_document,
            "anchors and buttons require noninteractive static contents; canvas permits only its specified fallback controls",
            operation: :compute,
            module: __MODULE__,
            source: node.tag
          )

        node.tag in ~w(style title iframe xmp plaintext noembed) and
            not valid_html_element?(node.tag, node.attributes, node.children) ->
          Diagnostics.error(
            :style,
            :invalid_document,
            "#{node.tag} requires literal text-only contents, not active HTML nodes",
            operation: :compute,
            module: __MODULE__,
            source: node.tag
          )

        node.tag == "base" and
            (not valid_html_child?(parent || :document, "base", ancestors) or
               not valid_html_element?("base", node.attributes, node.children)) ->
          Diagnostics.error(
            :style,
            :invalid_document,
            "base requires metadata placement and empty contents",
            operation: :compute,
            module: __MODULE__,
            source: "base"
          )

        node.tag == "caption" and
            not valid_html_element?(node.tag, node.attributes, node.children) ->
          Diagnostics.error(
            :style,
            :invalid_document,
            "caption accepts flow content without descendant table elements; move the nested table outside the caption",
            operation: :compute,
            module: __MODULE__,
            source: "caption"
          )

        node.tag == "wbr" and not valid_html_element?(node.tag, node.attributes, node.children) ->
          Diagnostics.error(
            :style,
            :invalid_document,
            "wbr is a void element and requires empty children",
            operation: :compute,
            module: __MODULE__,
            source: "wbr"
          )

        (custom? or custom_html_tag?(parent) or "a" in ancestors or "button" in ancestors or
           "canvas" in ancestors or
           node.tag in [
             "frameset",
             "frame",
             "noframes",
             "object",
             "canvas",
             "mark",
             "ins",
             "del",
             "strike",
             "wbr",
             "nobr",
             "form",
             "label",
             "details",
             "summary",
             "xmp",
             "plaintext",
             "noembed",
             "progress",
             "meter",
             "fieldset",
             "legend"
           ] or
           parent in [
             "frameset",
             "noframes",
             "a",
             "button",
             "object",
             "canvas",
             "mark",
             "ins",
             "del",
             "strike",
             "wbr",
             "nobr",
             "form",
             "label",
             "details",
             "summary",
             "xmp",
             "plaintext",
             "noembed",
             "progress",
             "meter",
             "fieldset",
             "legend"
           ]) and
            not valid_html_child?(parent || :document, node.tag, ancestors) ->
          Diagnostics.error(
            :style,
            :invalid_document,
            "element contents must obey their parent and transparent ancestor content models",
            operation: :compute,
            module: __MODULE__,
            source: node.tag
          )

        custom? or
          node.tag in ~w(picture source img canvas xmp plaintext noembed progress meter dialog font body q acronym basefont bgsound blink keygen) or
            (Map.has_key?(node.attributes, "hidden") and node.tag not in ["script", "template"]) ->
          case validate_html_element_attributes(
                 node.tag,
                 if(node.tag in ~w(font body dialog),
                   do: HtmlMetadataValidator.unqualified_attributes(node),
                   else: node.attributes
                 )
               ) do
            :ok ->
              {:ok, custom?}

            {:error, {reason, diagnostic}} ->
              {:error,
               {reason,
                Diagnostics.with_context(diagnostic, operation: :compute, source: node.tag)}}
          end

        true ->
          {:ok, false}
      end
    else
      if Map.get(node, :namespace) in [:svg, :mathml] do
        {:ok, false}
      else
        Diagnostics.error(
          :style,
          :invalid_document,
          "element namespace must be :html, :svg or :mathml",
          operation: :compute,
          module: __MODULE__,
          source: node.tag
        )
      end
    end
  end

  @doc false
  @spec validate_composition_document(map()) :: :ok | {:error, {atom(), map()}}
  def validate_composition_document(dom) do
    result =
      case dom.type do
        :document ->
          validate_dom(dom, :render)

        :document_fragment ->
          validate_render_tree(dom.children)
          validate_dom_nodes(dom.children, false, nil, [], nil, :html, :render)
      end

    case result do
      {:ok, _controls?} ->
        :ok

      {:error, _} = error ->
        error

      :invalid_document ->
        Diagnostics.error(
          :html,
          :invalid_document,
          "composition requires valid authored HTML node maps",
          module: __MODULE__,
          operation: :compose
        )
    end
  end

  @doc false
  @spec validate_composed_fragment([map()]) :: :ok | {:error, {atom(), map()}}
  def validate_composed_fragment(children) do
    with_render_budget(fn ->
      validate_render_tree(children)

      case validate_dom_nodes(children, false, nil, [], nil, :html, :render) do
        {:ok, _controls?} -> :ok
        error -> error
      end
    end)
  end

  defp validate_template_content(node) do
    case node do
      %{children: [], content: %{type: :document_fragment, children: children}}
      when is_list(children) ->
        case validate_dom_nodes(children, true, nil, [], nil, :html, :render) do
          {:ok, _initial_controls?} -> :ok
          error -> error
        end

      _ ->
        Diagnostics.error(
          :style,
          :invalid_document,
          "template requires empty children and a document_fragment content map",
          operation: :compute,
          module: __MODULE__,
          source: "template"
        )
    end
  end

  defp validate_layout_nodes(nodes, parent_display \\ nil, atomic_context \\ false) do
    Enum.reduce_while(nodes, :ok, fn node, status ->
      result =
        with :ok <-
               NativeElixirPdfUtilities.Validators.RubyValidator.validate_layout_metadata(node),
             :ok <- validate_layout_metadata(node),
             :ok <-
               NativeElixirPdfUtilities.Validators.HtmlVisibilityValidator.validate_layout_metadata(
                 node
               ),
             :ok <- validate_list_marker_image(node),
             :ok <- validate_auto_margin_context(node, parent_display) do
          case node do
            %{type: :element, style: %{svg_scene: scene} = style} ->
              if valid_layout_style?(style),
                do:
                  NativeElixirPdfUtilities.Validators.SvgSceneValidator.validate_prepared(scene),
                else:
                  Diagnostics.error(
                    :layout,
                    :invalid_layout,
                    "SVG source layout requires canonical style metadata; recompute styles before layout",
                    module: __MODULE__,
                    operation: :layout,
                    source: "svg"
                  )

            %{type: :text, text: text, style: style} ->
              if is_binary(text) and String.valid?(text, :fast_ascii) and
                   valid_layout_style?(style),
                 do: :ok,
                 else: :invalid_layout

            %{
              type: :element,
              tag: tag,
              style: %{display: :soft_break} = style,
              children: children
            } ->
              if tag == "wbr" and children == [] and valid_layout_style?(style) do
                :ok
              else
                Diagnostics.error(
                  :layout,
                  :invalid_layout,
                  "soft break layout requires a wbr element with empty children and valid style",
                  operation: :layout,
                  module: __MODULE__,
                  source: tag
                )
              end

            %{type: :element, style: %{display: :image} = style} ->
              maximum = Limits.get(:max_css_numeric_magnitude)

              image =
                case Map.get(style, :image) do
                  image when is_map(image) -> image
                  _ -> %{}
                end

              pixels? =
                match?(
                  %{width_px: width, height_px: height}
                  when is_integer(width) and width > 0 and width <= maximum and
                         is_integer(height) and height > 0 and height <= maximum,
                  image
                )

              points? =
                match?(
                  %{width: width, height: height}
                  when is_number(width) and width > 0 and width <= maximum and
                         is_number(height) and height > 0 and height <= maximum,
                  image
                )

              pixel_metadata? = Map.has_key?(image, :width_px) or Map.has_key?(image, :height_px)

              document_validation =
                if Map.has_key?(image, :svg_document_image),
                  do:
                    NativeElixirPdfUtilities.Validators.SvgDocumentImageValidator.request(image),
                  else: nil

              cond do
                not valid_layout_style?(style) ->
                  :invalid_layout

                match?({:error, _}, document_validation) ->
                  document_validation

                Map.get(style, :image_display, :block) not in [:inline, :block] ->
                  Diagnostics.error(
                    :layout,
                    :invalid_layout,
                    "image_display must be inline or block",
                    operation: :layout,
                    module: __MODULE__,
                    source: Map.get(node, :tag, "img")
                  )

                not (match?({:ok, _}, document_validation) or
                       NativeElixirPdfUtilities.Validators.ResponsiveImageValidator.valid_request?(
                         Map.get(style, :_responsive_image)
                       ) or
                         ((pixels? or points?) and (not pixel_metadata? or pixels?) and
                            Enum.all?([:width, :height], fn dimension ->
                              value = Map.get(image, dimension)

                              is_nil(value) or
                                  (is_number(value) and value >= 0 and value <= maximum)
                            end))) ->
                  Diagnostics.error(
                    :layout,
                    :invalid_layout,
                    "image layout requires bounded positive pixel or natural point dimensions; optional natural dimensions may be zero",
                    operation: :layout,
                    module: __MODULE__,
                    source: Map.get(node, :tag, "img")
                  )

                parent_display == :inline and Map.get(style, :image_display, :block) == :block and
                    Map.get(style, :position, :static) not in [:absolute, :fixed] ->
                  :block_flow

                true ->
                  :ok
              end

            %{type: :element, style: %{display: display} = style}
            when display == :none ->
              if valid_layout_style?(style), do: :ok, else: :invalid_layout

            %{type: :element, style: style, children: children} ->
              if valid_layout_style?(style) and is_list(children) do
                # Inline flow owners form block containers as Math, flex or
                # grid items. Validate their children in that used context while
                # retaining the original computed display on the source node.
                display =
                  if parent_display in [:math, :flex, :inline_flex, :grid, :inline_grid] and
                       Map.get(style, :display) == :inline,
                     do: :block,
                     else: Map.get(style, :display)

                case validate_layout_nodes(
                       children,
                       display,
                       atomic_context or display == :inline_block
                     ) do
                  :block_flow when display == :inline ->
                    alignment = Map.get(style, :vertical_align, :baseline)

                    if Map.get(node, :positioned_children, []) == [] and
                         (alignment in [:baseline, :sub, :super, :top, :middle, :bottom] or
                            (is_number(alignment) and
                               abs(alignment) <= Limits.get(:max_css_numeric_magnitude))) and
                         Enum.all?([:padding, :margin, :border_widths], fn key ->
                           case Map.fetch(style, key) do
                             :error ->
                               true

                             {:ok, %{top: top, right: right, bottom: bottom, left: left}} ->
                               Enum.all?([top, right, bottom, left], fn value ->
                                 is_number(value) and
                                   abs(value) <= Limits.get(:max_css_numeric_magnitude) and
                                   (key == :margin or value >= 0)
                               end)

                             _ ->
                               false
                           end
                         end) do
                      if parent_display == :inline, do: :block_flow, else: :ok
                    else
                      Diagnostics.error(
                        :layout,
                        :invalid_layout,
                        "interrupted inline box edges require bounded numeric top, right, bottom and left values and uncollected positioned descendants; recompute styles",
                        module: __MODULE__,
                        operation: :layout,
                        source: Map.get(node, :tag, "element")
                      )
                    end

                  :ok ->
                    if parent_display == :inline and
                         display not in [
                           :inline,
                           :inline_block,
                           :inline_flex,
                           :inline_grid,
                           :line_break,
                           :soft_break
                         ] and
                         Map.get(style, :position, :static) not in [:absolute, :fixed],
                       do: :block_flow,
                       else: :ok

                  error ->
                    error
                end
              else
                :invalid_layout
              end

            _ ->
              :invalid_layout
          end
        end

      result =
        case result do
          valid when valid in [:ok, :block_flow] ->
            with :ok <-
                   NativeElixirPdfUtilities.Validators.HtmlTableValidator.validate_presentation_style(
                     node,
                     Map.get(node, :style, %{}),
                     :layout,
                     atomic_context
                   ),
                 :ok <-
                   NativeElixirPdfUtilities.Validators.HtmlTableValidator.validate_atomic_context(
                     node,
                     atomic_context
                   ),
                 do: valid

          error ->
            error
        end

      case result do
        :ok -> {:cont, status}
        :block_flow -> {:cont, :block_flow}
        error -> {:halt, error}
      end
    end)
  end

  defp validate_layout_metadata(node) do
    gauge_result =
      case node do
        %{style: %{_gauge: gauge} = style} ->
          if is_boolean(Map.get(style, :_gauge_native)) and
               NativeElixirPdfUtilities.Validators.HtmlGaugeValidator.valid_prepared?(gauge) do
            :ok
          else
            Diagnostics.error(
              :layout,
              :invalid_layout,
              "gauge artwork requires canonical initial gauge state and a prepared native appearance flag; compute styles before layout",
              operation: :layout,
              module: __MODULE__,
              source: Map.get(node, :tag, "gauge")
            )
          end

        _ ->
          :ok
      end

    heading_result =
      case node do
        %{style: %{heading_level: level}} ->
          case node do
            %{type: :element, tag: <<"h", base>>}
            when base in ?1..?6 and is_integer(level) and level >= base - ?0 and level <= 9 ->
              :ok

            _ ->
              Diagnostics.error(
                :layout,
                :invalid_layout,
                "heading_level requires an h1-h6 element and an integer between its base level and HTML's fixed maximum of nine",
                operation: :layout,
                module: __MODULE__,
                source: Map.get(node, :tag, "text")
              )
          end

        _ ->
          :ok
      end

    caption_result =
      case node do
        %{style: %{caption_side: side}} when side not in [:top, :bottom] ->
          Diagnostics.error(
            :layout,
            :invalid_layout,
            "caption_side requires the validated top or bottom placement; received #{inspect(side)}",
            operation: :layout,
            module: __MODULE__,
            source: "caption-side"
          )

        _ ->
          :ok
      end

    line_result =
      case node do
        %{style: %{_root_body_fragmentation: _}} ->
          Diagnostics.error(
            :layout,
            :invalid_layout,
            "Root body fragmentation identity is created by Layout; do not supply internal body scope in a styled tree",
            module: __MODULE__,
            operation: :layout,
            source: "body"
          )

        %{style: style} when is_map(style) ->
          validate_paragraph_line_limits([style], :layout)

        _ ->
          :ok
      end

    table_border_result =
      case node do
        %{style: style}
        when is_map_key(style, :_collapsed_border_segments) or
               is_map_key(style, :_collapsed_border_winners) ->
          Diagnostics.error(
            :layout,
            :invalid_layout,
            "collapsed table edge winners and segments are internal formation metadata; compute styles before layout and do not supply border calculations",
            operation: :layout,
            module: __MODULE__,
            source: Map.get(node, :tag, "element")
          )

        _ ->
          :ok
      end

    with :ok <- gauge_result,
         :ok <- table_border_result,
         :ok <- heading_result,
         :ok <- caption_result,
         :ok <- line_result,
         :ok <-
           NativeElixirPdfUtilities.Validators.HtmlFormValidator.validate_initial_number_layout(
             node
           ),
         :ok <-
           NativeElixirPdfUtilities.Validators.HtmlFormValidator.validate_initial_range_layout(
             node
           ),
         :ok <-
           NativeElixirPdfUtilities.Validators.HtmlFormValidator.validate_initial_color_layout(
             node
           ),
         :ok <-
           NativeElixirPdfUtilities.Validators.HtmlFileValidator.validate_initial_layout(node),
         :ok <-
           NativeElixirPdfUtilities.Validators.HtmlTemporalValidator.validate_initial_layout(node) do
      case node do
        %{style: style}
        when is_map(style) and not is_map_key(style, :line_break) and
               not is_map_key(style, :overflow_wrap) and not is_map_key(style, :word_break) ->
          :ok

        %{style: style} when is_map(style) ->
          case {Map.get(style, :line_break, :normal), Map.get(style, :overflow_wrap, :normal),
                Map.get(style, :word_break, :normal)} do
            {line, overflow, word}
            when line in [:normal, :anywhere, :break_word] and
                   overflow in [:normal, :anywhere, :break_word] and
                   word in [:normal, :break_word] ->
              :ok

            values ->
              Diagnostics.error(
                :layout,
                :invalid_layout,
                "styled text requires line_break and overflow_wrap in [:normal, :anywhere, :break_word] and word_break in [:normal, :break_word]; received #{inspect(values)}",
                operation: :layout,
                module: __MODULE__,
                source: Map.get(node, :tag, "text")
              )
          end

        _ ->
          :ok
      end
    end
  end

  defp validate_auto_margin_context(node, _parent_display) do
    case node do
      %{style: style} when is_map(style) ->
        with :ok <-
               NativeElixirPdfUtilities.Validators.CssSizingValidator.validate_auto_margins(
                 style,
                 Map.get(node, :tag, "element")
               ),
             do:
               NativeElixirPdfUtilities.Validators.CssSizingValidator.validate_calculations(
                 style,
                 Map.get(node, :tag, "element")
               )

      _ ->
        :ok
    end
  end

  defp valid_layout_style?(style) do
    # Collapsed edge metadata is prepared after input validation.
    # Incoming styled trees must not supply internal border calculations.
    is_map(style) and
      NativeElixirPdfUtilities.Validators.CssTransformValidator.valid_style?(style) and
      not Map.has_key?(style, :_collapsed_borders) and
      (Map.get(style, :_image_map) == nil or
         NativeElixirPdfUtilities.Validators.ImageMapValidator.valid_model?(style._image_map)) and
      (Map.get(style, :html_target) == nil or
         NativeElixirPdfUtilities.Validators.HtmlLinkValidator.valid_identity?(style.html_target)) and
      NativeElixirPdfUtilities.Validators.HtmlLinkValidator.valid_prepared_destination?(
        Map.get(style, :link_destination)
      ) and
      valid_text_decorations?(Map.get(style, :_text_decorations, [])) and
      case Map.fetch(style, :_list_marker) do
        :error ->
          true

        {:ok, markers} when is_list(markers) ->
          Enum.all?(markers, &match?(%{type: :text}, &1)) and
            validate_layout_nodes(markers) == :ok

        _ ->
          false
      end
  end

  defp validate_list_marker_image(node) do
    case node do
      %{style: %{list_style_image: image} = style} when image != nil ->
        image_style =
          style
          |> Map.delete(:list_style_image)
          |> Map.merge(%{display: :image, image_display: :inline, image: image})

        case validate_layout_nodes([
               %{type: :element, tag: "img", style: image_style, children: []}
             ]) do
          {:error, {reason, diagnostic}} ->
            {:error, {reason, Diagnostics.with_context(diagnostic, source: "list-style-image")}}

          result ->
            result
        end

      _ ->
        :ok
    end
  end

  defp valid_text_decorations?(decorations) do
    case decorations do
      [] ->
        true

      decorations when is_list(decorations) ->
        check_render_resource(:max_text_decoration_layers, length(decorations), :style)

        Enum.all?(decorations, fn decoration ->
          case decoration do
            %{
              lines: lines,
              color: color,
              thickness: thickness,
              underline: underline,
              overline: overline,
              strike: strike
            } ->
              numbers = [thickness, underline, overline, strike]

              Map.get(decoration, :style, :solid) in [:solid, :dotted, :dashed] and
                is_list(lines) and lines != [] and
                Enum.all?(lines, &(&1 in [:underline, :overline, :line_through])) and
                length(lines) == length(Enum.uniq(lines)) and
                is_number(thickness) and thickness >= 0 and
                Enum.all?(numbers, fn number ->
                  if is_number(number) do
                    check_render_resource(:max_css_numeric_magnitude, abs(number), :style)
                    true
                  else
                    false
                  end
                end) and
                (color == :transparent or
                   NativeElixirPdfUtilities.Validators.WriterValidator.valid_color?(color))

            _ ->
              false
          end
        end)

      _ ->
        false
    end
  end

  defp validate_fieldset_pagination_metadata(boxes, printable_height) do
    {paint_ids, atomic_markers} =
      Enum.reduce(boxes, {[], []}, fn box, state ->
        case box do
          %{fieldset_paint_id: id} -> {[id | elem(state, 0)], elem(state, 1)}
          %{role: :atomic_table_line} -> {elem(state, 0), [box | elem(state, 1)]}
          _ -> state
        end
      end)

    paint_ids = MapSet.new(paint_ids)

    valid? =
      Enum.all?(boxes, fn box ->
        case Map.fetch(box, :table_caption_side) do
          :error -> true
          {:ok, side} -> side in [:top, :bottom] and not is_nil(Map.get(box, :table_id))
        end and
          valid_fieldset_pagination_metadata?(box) and
          Enum.all?(Map.get(box, :fieldset_ancestors, []), &MapSet.member?(paint_ids, &1)) and
          case Map.fetch(box, :positioned_fragment_owner) do
            :error ->
              true

            {:ok, owner} ->
              Map.get(box, :out_of_flow, false) == true and is_reference(owner) and
                owner in Map.get(box, :fieldset_ancestors, []) and
                MapSet.member?(paint_ids, owner)
          end
      end)

    inline_sources = Enum.filter(boxes, &Map.has_key?(&1, :inline_positioned_anchor))

    inline_valid? =
      if inline_sources == [] do
        true
      else
        case inline_fragment_anchor_ids(boxes, MapSet.new()) do
          {:ok, ids} ->
            Enum.all?(inline_sources, fn box ->
              case box.inline_positioned_anchor do
                %{id: id, source_y: y} when is_reference(id) and is_number(y) ->
                  Map.get(box, :out_of_flow, false) == true and MapSet.member?(ids, id) and
                    abs(y) <= Limits.get(:max_pdf_numeric_magnitude)

                _ ->
                  false
              end
            end)

          :invalid ->
            false
        end
      end

    if not inline_valid? do
      Diagnostics.error(
        :pagination,
        :invalid_layout,
        "positioned inline fragments require matching reference anchors and bounded source coordinates",
        operation: :paginate,
        module: __MODULE__,
        source: "inline_positioned_anchor"
      )
    else
      if valid? do
        NativeElixirPdfUtilities.Validators.HtmlTableValidator.validate_atomic_pagination(
          boxes,
          Enum.reverse(atomic_markers),
          printable_height
        )
      else
        Diagnostics.error(
          :pagination,
          :invalid_layout,
          "fragment decoration requires reference IDs with matching paint boxes, numeric coordinates and ordered top/bottom bounds; image paints require a clip rectangle and captions require a table ID with a top or bottom side",
          operation: :paginate,
          module: __MODULE__,
          source: "fieldset_paint_id"
        )
      end
    end
  end

  defp inline_fragment_anchor_ids(boxes, ids) do
    # HtmlLinkValidator already validated native group structure. This pass
    # owns only the optional inline fragment references consumed by pagination.
    Enum.reduce_while(boxes, {:ok, ids}, fn box, {:ok, ids} ->
      anchors = Map.get(box, :inline_fragment_anchors, [])

      if is_list(anchors) and Enum.all?(anchors, &is_reference/1) do
        children =
          if box.type in [:drawing_group, :glyph_group], do: box.boxes, else: []

        case inline_fragment_anchor_ids(children, Enum.reduce(anchors, ids, &MapSet.put(&2, &1))) do
          {:ok, ids} -> {:cont, {:ok, ids}}
          :invalid -> {:halt, :invalid}
        end
      else
        {:halt, :invalid}
      end
    end)
  end

  defp valid_fieldset_pagination_metadata?(box) do
    case box do
      %{fieldset_ancestors: ancestors} when is_list(ancestors) ->
        Enum.all?(ancestors, &is_reference/1) and
          case Map.fetch(box, :fieldset_paint_id) do
            :error ->
              is_number(Map.get(box, :y))

            {:ok, id} ->
              is_reference(id) and id in ancestors and is_number(Map.get(box, :y)) and
                Map.get(box, :type) in [:rect, :image] and
                (Map.get(box, :type) != :image or
                   match?(
                     %{clip: %{x: x, width: width}}
                     when is_number(x) and is_number(width) and width > 0,
                     box
                   )) and
                case Map.get(box, :fieldset_bounds) do
                  %{top: top, bottom: bottom} ->
                    is_number(top) and is_number(bottom) and top > bottom

                  _ ->
                    false
                end
          end

      # HtmlLinkValidator.validate_boxes/2 has already validated each box map.
      box ->
        not Map.has_key?(box, :fieldset_paint_id)
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
          case String.valid?(text, :fast_ascii) do
            true ->
              {:cont, :ok}

            false ->
              {:halt,
               Diagnostics.error(:font, :invalid_encoding, "styled text must be valid UTF-8")}
          end

        %{type: :element, children: children} when is_list(children) ->
          markers = node |> Map.get(:style, %{}) |> Map.get(:_list_marker, [])

          case if(is_list(markers),
                 do: validate_font_input_nodes(markers ++ children),
                 else: invalid_styled_document("list markers must be styled text nodes")
               ) do
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
          markers = node |> Map.get(:style, %{}) |> Map.get(:_list_marker, [])

          case validate_font_coverage_nodes(
                 markers ++ children,
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
            not supported? and unsupported_glyphs == :replace and
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

  @doc false
  @spec split_css_tokens(String.t()) :: [String.t()]
  def split_css_tokens(value) do
    value
    |> String.graphemes()
    |> Enum.reduce({[], "", 0}, fn grapheme, {tokens, current, depth} ->
      cond do
        grapheme == "(" ->
          {tokens, current <> grapheme, depth + 1}

        grapheme == ")" ->
          {tokens, current <> grapheme, max(depth - 1, 0)}

        grapheme in [" ", "\t", "\n", "\r", "\f"] and depth == 0 ->
          case current do
            "" -> {tokens, "", depth}
            current -> {tokens ++ [current], "", depth}
          end

        true ->
          {tokens, current <> grapheme, depth}
      end
    end)
    |> then(fn {tokens, current, _depth} ->
      if current == "", do: tokens, else: tokens ++ [current]
    end)
  end

  @doc false
  @spec split_css_comma(String.t()) :: [String.t()]
  def split_css_comma(value) do
    value
    |> String.graphemes()
    |> Enum.reduce({[], "", 0}, fn grapheme, {tokens, current, depth} ->
      cond do
        grapheme == "(" ->
          {tokens, current <> grapheme, depth + 1}

        grapheme == ")" ->
          {tokens, current <> grapheme, max(depth - 1, 0)}

        grapheme == "," and depth == 0 ->
          {tokens ++ [trim_ascii_whitespace(current)], "", depth}

        true ->
          {tokens, current <> grapheme, depth}
      end
    end)
    |> then(fn {tokens, current, _depth} -> tokens ++ [trim_ascii_whitespace(current)] end)
    |> Enum.reject(&(&1 == ""))
  end

  @doc false
  @spec parse_html_legacy_color(term()) :: {:ok, {number(), number(), number()}} | :error
  def parse_html_legacy_color(value) do
    case value do
      "" ->
        :error

      value when is_binary(value) ->
        if String.valid?(value, :fast_ascii) do
          input = trim_ascii_whitespace(value)
          # The longest keyword fixes recognition cost; this is not a limit
          # on legacy color input, whose separate format algorithm follows.
          keyword =
            if byte_size(input) <= byte_size("lightgoldenrodyellow"),
              do: String.downcase(input, :ascii)

          named_color =
            if is_binary(keyword),
              do: NativeElixirPdfUtilities.Validators.CssColorValidator.named_color(keyword)

          cond do
            keyword == "transparent" ->
              :error

            not is_nil(named_color) ->
              {:ok, named_color}

            byte_size(input) == 4 and Regex.match?(~r/\A#[0-9A-Fa-f]{3}\z/, input) ->
              parse_css_color(input, nil)

            true ->
              # HTML fixes this truncation at 128 code points after replacing
              # supplementary scalars with 00. It is a format invariant.
              input = legacy_color_prefix(input, 128, [])

              input =
                if String.starts_with?(input, "#"),
                  do: binary_part(input, 1, byte_size(input) - 1),
                  else: input

              input = Regex.replace(~r/[^0-9A-Fa-f]/u, input, "0")
              padding = if input == "", do: 3, else: rem(3 - rem(byte_size(input), 3), 3)
              input = input <> String.duplicate("0", padding)
              length = div(byte_size(input), 3)

              components =
                for offset <- [0, length, length * 2], do: binary_part(input, offset, length)

              length = min(length, 8)
              components = Enum.map(components, &binary_part(&1, byte_size(&1) - length, length))

              shared_zeros =
                components
                |> Enum.map(&(byte_size(&1) - byte_size(String.trim_leading(&1, "0"))))
                |> Enum.min()

              drop = min(shared_zeros, max(length - 2, 0))

              [red, green, blue] =
                Enum.map(components, &binary_part(&1, drop, min(length - drop, 2)))

              {:ok, {hex_to_pdf_color(red), hex_to_pdf_color(green), hex_to_pdf_color(blue)}}
          end
        else
          :error
        end

      _ ->
        :error
    end
  end

  # Stops at the fixed legacy-format prefix instead of constructing a complete
  # scalar list for a long color attribute. Supplementary scalars consume two
  # characters of the prefix because replacement precedes truncation.
  defp legacy_color_prefix(input, remaining, reversed) do
    case {remaining, input} do
      {0, _} ->
        reversed |> Enum.reverse() |> IO.iodata_to_binary()

      {_, ""} ->
        reversed |> Enum.reverse() |> IO.iodata_to_binary()

      {remaining, <<scalar::utf8, rest::binary>>} ->
        replacement = if scalar > 0xFFFF, do: "00", else: <<scalar::utf8>>
        count = if scalar > 0xFFFF, do: min(remaining, 2), else: 1
        replacement = if count == 1 and scalar > 0xFFFF, do: "0", else: replacement
        legacy_color_prefix(rest, remaining - count, [replacement | reversed])
    end
  end

  @doc false
  @spec parse_css_background_color(String.t(), term()) :: {:ok, term()} | :error
  def parse_css_background_color(value, current_color) do
    normalized = trim_ascii_whitespace(value)

    if byte_size(normalized) == byte_size("none") and
         downcase_ascii(normalized) == "none" do
      {:ok, nil}
    else
      parse_css_color(value, current_color)
    end
  end

  @doc false
  @spec parse_css_color(String.t(), term()) :: {:ok, term()} | :error
  def parse_css_color(value, current_color) do
    NativeElixirPdfUtilities.Validators.CssColorValidator.parse_function_color(
      value,
      current_color
    )
  end

  @doc false
  @spec parse_css_color_declaration(String.t(), term()) :: {:ok, term()} | :error
  def parse_css_color_declaration(value, inherited_color) do
    case value |> trim_ascii_whitespace() |> downcase_ascii() do
      "inherit" -> {:ok, inherited_color}
      "unset" -> {:ok, inherited_color}
      "initial" -> {:ok, {0, 0, 0}}
      _ -> parse_css_color(value, inherited_color)
    end
  end

  # Fold ASCII letters without allocating a list for every byte.
  defp downcase_ascii(value) do
    for <<character <- value>>, into: <<>> do
      <<if(character >= ?A and character <= ?Z, do: character + 32, else: character)>>
    end
  end

  defp hex_to_pdf_color(hex) do
    {integer, ""} = Integer.parse(hex, 16)
    integer / 255
  end
end
