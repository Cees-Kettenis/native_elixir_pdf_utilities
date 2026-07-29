defmodule NativeElixirPdfUtilities.HtmlToPdf.CssParser do
  @moduledoc """
  Strict CSS parser for the native HTML-to-PDF renderer.

  The parser accepts the document-oriented selector subset used by the style
  cascade: element, class, id, element.class, descendant, child, and comma
  groups. Simple `@page` and `@font-face` rules are accepted outside the style
  cascade, and `@media print` rules are included in the active print cascade.
  Declarations are kept as normalized property/value pairs so the style layer
  can validate values against the renderer's supported property set.
  """

  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry

  @type declaration :: {String.t(), String.t()} | {String.t(), String.t(), :important}
  @type selector_part :: %{
          tag: String.t() | nil,
          id: String.t() | nil,
          classes: [String.t()],
          pseudo_classes: [:first_child | :last_child | :root | {:nth_child, pos_integer()}],
          combinator: nil | :descendant | :child
        }
  @type selector :: %{
          parts: [selector_part()],
          specificity: {non_neg_integer(), non_neg_integer(), non_neg_integer()}
        }
  @type rule :: %{
          selectors: [selector()],
          declarations: [declaration()],
          order: non_neg_integer()
        }
  @type stylesheet :: [rule()]
  @type font_face :: %{
          family: String.t(),
          sources: [String.t()],
          weight: 100..900,
          style: :normal | :italic
        }
  @type page_option ::
          {:page_size, PageGeometry.page_size_input()}
          | {:margin, PageGeometry.margin_input()}

  @page_context_properties [
    "background",
    "background-attachment",
    "background-color",
    "background-image",
    "background-position",
    "background-repeat",
    "bleed",
    "border",
    "border-bottom",
    "border-bottom-color",
    "border-bottom-style",
    "border-bottom-width",
    "border-color",
    "border-left",
    "border-left-color",
    "border-left-style",
    "border-left-width",
    "border-right",
    "border-right-color",
    "border-right-style",
    "border-right-width",
    "border-style",
    "border-top",
    "border-top-color",
    "border-top-style",
    "border-top-width",
    "border-width",
    "color",
    "counter-increment",
    "counter-reset",
    "direction",
    "font",
    "font-family",
    "font-size",
    "font-style",
    "font-variant",
    "font-weight",
    "height",
    "letter-spacing",
    "line-height",
    "margin",
    "margin-bottom",
    "margin-left",
    "margin-right",
    "margin-top",
    "marks",
    "max-height",
    "max-width",
    "min-height",
    "min-width",
    "outline",
    "outline-color",
    "outline-style",
    "outline-width",
    "padding",
    "padding-bottom",
    "padding-left",
    "padding-right",
    "padding-top",
    "page-orientation",
    "quotes",
    "size",
    "text-align",
    "text-decoration",
    "text-indent",
    "text-transform",
    "visibility",
    "white-space",
    "width",
    "word-spacing"
  ]
  @page_size_names ~w(a5 a4 a3 b5 b4 jis-b5 jis-b4 letter legal ledger)
  @page_length_units ~w(cm mm q in pc pt px em ex ch rem lh rlh vw vh vmin vmax)
  @page_number_pattern "(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:e[+-]?\\d+)?"
  @page_length_pattern Enum.join(@page_length_units, "|")
  @page_length_regex Regex.compile!(
                       "^[+-]?#{@page_number_pattern}(?:#{@page_length_pattern})$",
                       "u"
                     )
  @page_nonnegative_length_regex Regex.compile!(
                                   "^\\+?#{@page_number_pattern}(?:#{@page_length_pattern})$",
                                   "u"
                                 )
  @page_percentage_regex Regex.compile!("^[+-]?#{@page_number_pattern}%$", "u")
  @css_wide_keywords ~w(initial inherit unset revert revert-layer)

  @doc """
  Parses a CSS stylesheet into strict renderer rules.
  """
  @spec parse(String.t()) :: {:ok, stylesheet()} | {:error, :invalid_css}
  def parse(css) do
    case parse_detailed(css) do
      {:ok, stylesheet} -> {:ok, stylesheet}
      {:error, {:invalid_css, _detail}} -> {:error, :invalid_css}
    end
  end

  @doc """
  Parses a CSS stylesheet and returns source-location details when parsing fails.
  """
  @spec parse_detailed(String.t()) ::
          {:ok, stylesheet()} | {:error, {:invalid_css, map()}}
  def parse_detailed(css) do
    case css do
      css when is_binary(css) ->
        with {:ok, active_css} <- css |> strip_comments() |> active_media_rules(),
             {:ok, _font_faces} <- parse_font_faces(active_css, css),
             {:ok, _page_options} <- parse_page_rules(active_css, css) do
          parsed_css = active_css |> strip_font_face_rules() |> strip_page_rules()

          case parse_rules(parsed_css) do
            {:ok, stylesheet} -> {:ok, stylesheet}
            {:error, :invalid_css} -> {:error, {:invalid_css, css_error_detail(css, parsed_css)}}
          end
        else
          {:error, {:invalid_css, detail}} ->
            {:error, {:invalid_css, detail}}

          {:error, :invalid_css} ->
            {:error, {:invalid_css, css_error_detail(css, css)}}
        end

      _ ->
        {:error,
         {:invalid_css,
          %{
            stage: :css,
            reason: :invalid_css,
            message: "CSS input must be a string"
          }}}
    end
  end

  @doc """
  Extracts active local font declarations from `@font-face` rules.

  Sources must use `url(...)` with a TrueType or OpenType source. Remote URLs,
  data URIs, WOFF/WOFF2 sources, and unsupported descriptors are rejected.
  Relative paths are returned unchanged for the style layer to resolve against
  the stylesheet location or renderer `:base_url`. Supported sources retain
  their declared order so loading can fall back when an earlier file is
  unavailable or invalid.
  """
  @spec font_faces(String.t()) :: {:ok, [font_face()]} | {:error, :invalid_css}
  def font_faces(css) do
    case css do
      css when is_binary(css) ->
        case css |> strip_comments() |> active_media_rules() do
          {:ok, active_css} ->
            case parse_font_faces(active_css, active_css) do
              {:ok, font_faces} -> {:ok, font_faces}
              {:error, {:invalid_css, _detail}} -> {:error, :invalid_css}
            end

          {:error, :invalid_css} ->
            {:error, :invalid_css}
        end

      _ ->
        {:error, :invalid_css}
    end
  end

  @doc """
  Extracts renderer page defaults from simple `@page` rules.

  Valid page-context properties are accepted even when the renderer does not
  apply them yet. Rendering consumes accepted named and explicit two-length
  `size` values, portrait and landscape orientations, one-to-four-value
  `margin` shorthands, and the four margin longhands. Malformed declarations,
  unknown properties, and invalid paged-media descriptor values return
  `{:error, :invalid_css}`.
  """
  @spec page_options(String.t()) :: {:ok, [page_option()]} | {:error, :invalid_css}
  def page_options(css) do
    case css do
      css when is_binary(css) ->
        with {:ok, active_css} <- css |> strip_comments() |> active_media_rules() do
          case parse_page_rules(active_css, css) do
            {:ok, page_options} -> {:ok, page_options}
            {:error, {:invalid_css, _detail}} -> {:error, :invalid_css}
          end
        end

      _ ->
        {:error, :invalid_css}
    end
  end

  @doc """
  Parses a CSS declaration block into normalized property/value pairs.

  This is used for both stylesheet blocks and inline `style` attributes.
  """
  @spec parse_declarations(String.t()) :: {:ok, [declaration()]} | {:error, :invalid_css}
  def parse_declarations(css) do
    case parse_declarations_detailed(css) do
      {:ok, declarations} -> {:ok, declarations}
      {:error, {:invalid_css, _detail}} -> {:error, :invalid_css}
    end
  end

  @doc """
  Parses a CSS declaration block and returns source-location details on failure.
  """
  @spec parse_declarations_detailed(String.t()) ::
          {:ok, [declaration()]} | {:error, {:invalid_css, map()}}
  def parse_declarations_detailed(css) do
    case css do
      css when is_binary(css) ->
        declarations =
          css
          |> String.split(";")
          |> Enum.map(&String.trim/1)
          |> Enum.reject(&(&1 == ""))

        Enum.reduce_while(declarations, {:ok, []}, fn declaration, {:ok, acc} ->
          case parse_declaration(declaration) do
            {:ok, parsed} ->
              {:cont, {:ok, acc ++ [parsed]}}

            {:error, :invalid_css} ->
              {:halt, {:error, {:invalid_css, declaration_error_detail(css, declaration)}}}
          end
        end)

      _ ->
        {:error,
         {:invalid_css,
          %{
            stage: :css,
            reason: :invalid_css,
            message: "CSS declaration input must be a string"
          }}}
    end
  end

  defp strip_comments(css) do
    Regex.replace(~r/\/\*.*?\*\//us, css, "")
  end

  defp strip_page_rules(css) do
    Regex.replace(~r/@page\s*(?:[^{]*)\{[^{}]*\}/ui, css, "")
  end

  defp strip_font_face_rules(css) do
    Regex.replace(~r/@font-face\s*\{[^{}]*\}/ui, css, "")
  end

  defp active_media_rules(css) do
    media_rule = ~r/@media\s+(?<query>[^{}]+)\{(?<body>(?:[^{}]|\{[^{}]*\})*)\}/ui

    active_css =
      Regex.replace(media_rule, css, fn _rule, query, body ->
        query = query |> String.trim() |> String.downcase()

        case query in ["print", "only print", "all", "only all"] do
          true -> body
          false -> ""
        end
      end)

    case Regex.match?(~r/@media\b/ui, active_css) do
      true -> {:error, :invalid_css}
      false -> {:ok, active_css}
    end
  end

  defp parse_font_faces(css, diagnostic_css) do
    ~r/@font-face\s*\{(?<declarations>[^{}]*)\}/ui
    |> Regex.scan(css, capture: ["declarations"])
    |> List.flatten()
    |> Enum.reduce_while({:ok, []}, fn block, {:ok, acc} ->
      case parse_font_face(block) do
        {:ok, font_face} ->
          {:cont, {:ok, acc ++ [font_face]}}

        {:error, {message, source}} ->
          {:halt,
           {:error, {:invalid_css, font_face_error_detail(diagnostic_css, message, source)}}}
      end
    end)
  end

  defp parse_font_face(block) do
    case parse_declarations_detailed(block) do
      {:ok, declarations} ->
        case Enum.find(declarations, &(not supported_font_descriptor?(&1))) do
          nil ->
            family = font_family_descriptor(declarations)
            sources = font_source_descriptor(declarations)
            weight = font_weight_descriptor(declarations)
            style = font_style_descriptor(declarations)

            case {family, sources, weight, style} do
              {{:ok, family}, {:ok, sources}, {:ok, weight}, {:ok, style}} ->
                {:ok, %{family: family, sources: sources, weight: weight, style: style}}

              _ ->
                error =
                  [
                    {"font-family", family},
                    {"src", sources},
                    {"font-weight", weight},
                    {"font-style", style}
                  ]
                  |> Enum.find_value(fn {property, result} ->
                    case result do
                      {:error, :invalid_css} -> font_face_descriptor_error(block, property)
                      _ -> nil
                    end
                  end)

                {:error, error}
            end

          declaration ->
            property = elem(declaration, 0)
            {:error, font_face_descriptor_error(block, property)}
        end

      {:error, {:invalid_css, detail}} ->
        source = Map.get(detail, :source, "@font-face")

        {:error, {~s(@font-face declaration "#{source}" is invalid or unsupported), source}}
    end
  end

  defp font_face_descriptor_error(block, property) do
    source =
      block
      |> String.split(";")
      |> Enum.map(&String.trim/1)
      |> Enum.find(&Regex.match?(~r/^#{Regex.escape(property)}\s*:/iu, &1))

    case source do
      nil ->
        {~s(@font-face is missing required "#{property}" descriptor), "@font-face"}

      source ->
        {~s(@font-face declaration "#{source}" is invalid or unsupported), source}
    end
  end

  defp font_face_error_detail(css, message, source) do
    {line, column} = source_location(css, source)

    %{
      stage: :css,
      reason: :invalid_css,
      message: "line #{line}: #{message}",
      line: line,
      column: column,
      source: source
    }
  end

  defp supported_font_descriptor?(declaration) do
    case declaration do
      {property, _value} when property in ["font-family", "src", "font-weight", "font-style"] ->
        true

      {"font-display", value} ->
        String.downcase(value) in ["auto", "block", "swap", "fallback", "optional"]

      _ ->
        false
    end
  end

  defp font_family_descriptor(declarations) do
    case declarations |> Enum.reverse() |> List.keyfind("font-family", 0) do
      {"font-family", value} ->
        family = value |> String.trim() |> String.trim("\"") |> String.trim("'")
        if family == "", do: {:error, :invalid_css}, else: {:ok, family}

      _ ->
        {:error, :invalid_css}
    end
  end

  defp font_source_descriptor(declarations) do
    case declarations |> Enum.reverse() |> List.keyfind("src", 0) do
      {"src", value} ->
        sources =
          value
          |> font_source_candidates()
          |> Enum.flat_map(fn candidate ->
            captures =
              Regex.named_captures(
                ~r/^url\(\s*(?:"(?<double>[^"]+)"|'(?<single>[^']+)'|(?<bare>[^)'"\s]+))\s*\)(?:\s+format\(\s*(?:"(?<format_double>[^"]+)"|'(?<format_single>[^']+)'|(?<format_bare>[^)'"\s]+))\s*\))?$/ui,
                candidate
              )

            case captures do
              captures when is_map(captures) ->
                source = first_capture(captures, ["double", "single", "bare"])

                format =
                  first_capture(captures, ["format_double", "format_single", "format_bare"])

                if supported_font_source?(source, format), do: [source], else: []

              _ ->
                []
            end
          end)

        case sources do
          [] -> {:error, :invalid_css}
          sources -> {:ok, sources}
        end

      _ ->
        {:error, :invalid_css}
    end
  end

  defp font_source_candidates(value) do
    {candidates, current, _quote, _depth} =
      value
      |> String.graphemes()
      |> Enum.reduce({[], [], nil, 0}, fn character, {candidates, current, quote, depth} ->
        cond do
          character in ["\"", "'"] and is_nil(quote) ->
            {candidates, [character | current], character, depth}

          character == quote ->
            {candidates, [character | current], nil, depth}

          is_nil(quote) and character == "(" ->
            {candidates, [character | current], quote, depth + 1}

          is_nil(quote) and character == ")" ->
            {candidates, [character | current], quote, max(depth - 1, 0)}

          is_nil(quote) and depth == 0 and character == "," ->
            candidate = current |> Enum.reverse() |> Enum.join() |> String.trim()
            {[candidate | candidates], [], quote, depth}

          true ->
            {candidates, [character | current], quote, depth}
        end
      end)

    final_candidate = current |> Enum.reverse() |> Enum.join() |> String.trim()

    [final_candidate | candidates]
    |> Enum.reverse()
    |> Enum.reject(&(&1 == ""))
  end

  defp supported_font_source?(source, format) do
    normalized_format = String.downcase(format)
    extension = source |> Path.extname() |> String.downcase()

    local? =
      source != "" and not String.contains?(source, ["\0", "://"]) and
        not String.starts_with?(String.downcase(source), "data:")

    format_supported? =
      case normalized_format do
        "" -> true
        format -> format in ["truetype", "opentype"]
      end

    local? and extension in [".ttf", ".otf"] and format_supported?
  end

  defp first_capture(captures, names) do
    Enum.find_value(names, "", fn name ->
      case Map.get(captures, name) do
        value when is_binary(value) and value != "" -> value
        _ -> nil
      end
    end)
  end

  defp font_weight_descriptor(declarations) do
    case declarations |> Enum.reverse() |> List.keyfind("font-weight", 0) do
      nil ->
        {:ok, 400}

      {"font-weight", value} ->
        case String.downcase(String.trim(value)) do
          "normal" -> {:ok, 400}
          "bold" -> {:ok, 700}
          value -> parsed_font_weight(Integer.parse(value))
        end
    end
  end

  defp parsed_font_weight({weight, ""}) when weight >= 100 and weight <= 900, do: {:ok, weight}
  defp parsed_font_weight(_parsed), do: {:error, :invalid_css}

  defp font_style_descriptor(declarations) do
    case declarations |> Enum.reverse() |> List.keyfind("font-style", 0) do
      nil ->
        {:ok, :normal}

      {"font-style", value} ->
        case String.downcase(String.trim(value)) do
          "normal" -> {:ok, :normal}
          "italic" -> {:ok, :italic}
          _ -> {:error, :invalid_css}
        end
    end
  end

  defp page_rule_blocks(css) do
    ~r/@page\s*(?:[^{]*)\{(?<declarations>[^{}]*)\}/ui
    |> Regex.scan(css, capture: ["declarations"])
    |> List.flatten()
  end

  defp parse_page_rules(css, diagnostic_css) do
    css
    |> page_rule_blocks()
    |> Enum.reduce_while({:ok, []}, fn block, {:ok, acc} ->
      case page_options_from(block) do
        {:ok, page_options} ->
          {:cont, {:ok, PageGeometry.merge_page_options(acc, page_options)}}

        {:error, source} ->
          {:halt, {:error, {:invalid_css, declaration_error_detail(diagnostic_css, source)}}}
      end
    end)
  end

  defp page_options_from(block) do
    block
    |> String.split(";")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.reduce_while({:ok, []}, fn source, {:ok, acc} ->
      case parse_declaration(source) do
        {:ok, {property, value}} ->
          put_page_option(acc, property, value, source)

        {:ok, {property, value, :important}} ->
          put_page_option(acc, property, value, source)

        {:error, :invalid_css} ->
          {:halt, {:error, source}}
      end
    end)
  end

  defp put_page_option(options, property, value, source) do
    case page_declaration_option(property, value) do
      {:ok, nil} ->
        {:cont, {:ok, options}}

      {:ok, {:margin_side, side, option}} ->
        margin =
          options
          |> Keyword.get(:margin)
          |> PageGeometry.merge_margin_defaults(%{side => option})

        {:cont, {:ok, Keyword.put(options, :margin, margin)}}

      {:ok, {key, option}} ->
        {:cont, {:ok, Keyword.put(options, key, option)}}

      :error ->
        {:halt, {:error, source}}
    end
  end

  defp page_declaration_option(property, value) do
    case property do
      "size" ->
        case valid_page_size?(value) do
          true ->
            case page_size_option(value) do
              nil -> {:ok, nil}
              page_size -> {:ok, {:page_size, page_size}}
            end

          false ->
            :error
        end

      "margin" ->
        case valid_page_margin?(value, 4) do
          true ->
            case page_margin_option(value) do
              nil -> {:ok, nil}
              margin -> {:ok, {:margin, margin}}
            end

          false ->
            :error
        end

      property when property in ["margin-top", "margin-right", "margin-bottom", "margin-left"] ->
        case valid_page_margin?(value, 1) do
          true ->
            case PageGeometry.css_margin_option(value) do
              nil -> {:ok, nil}
              margin -> {:ok, {:margin_side, page_margin_side(property), margin}}
            end

          false ->
            :error
        end

      "page-orientation" ->
        case value |> String.trim() |> String.downcase() do
          value when value in ["upright", "rotate-left", "rotate-right"] -> {:ok, nil}
          value when value in @css_wide_keywords -> {:ok, nil}
          value -> if valid_page_function?(value), do: {:ok, nil}, else: :error
        end

      "marks" ->
        marks = value |> String.trim() |> String.downcase() |> String.split(~r/\s+/u, trim: true)

        case marks do
          ["none"] ->
            {:ok, nil}

          [value] when value in @css_wide_keywords ->
            {:ok, nil}

          [value] ->
            if valid_page_function?(value), do: {:ok, nil}, else: page_marks_option(marks)

          marks when length(marks) in 1..2 ->
            page_marks_option(marks)

          _ ->
            :error
        end

      "bleed" ->
        case value |> String.trim() |> String.downcase() do
          "auto" ->
            {:ok, nil}

          value when value in @css_wide_keywords ->
            {:ok, nil}

          value ->
            if valid_page_length?(value, true) or valid_page_function?(value),
              do: {:ok, nil},
              else: :error
        end

      property ->
        case property in @page_context_properties and String.trim(value) != "" do
          true -> {:ok, nil}
          false -> :error
        end
    end
  end

  defp valid_page_size?(value) do
    normalized = value |> String.trim() |> String.downcase()
    tokens = String.split(normalized, ~r/\s+/u, trim: true)

    case tokens do
      ["auto"] ->
        true

      [value] when value in @css_wide_keywords ->
        true

      [orientation] when orientation in ["portrait", "landscape"] ->
        true

      [page_size] ->
        page_size in @page_size_names or valid_page_length?(page_size, false) or
          valid_page_function?(normalized)

      [first, second] ->
        (first in @page_size_names and second in ["portrait", "landscape"]) or
          (second in @page_size_names and first in ["portrait", "landscape"]) or
          (valid_page_length?(first, false) and valid_page_length?(second, false))

      _ ->
        false
    end
  end

  defp valid_page_margin?(value, maximum_values) do
    normalized = value |> String.trim() |> String.downcase()

    values =
      case valid_page_function?(normalized) do
        true -> [normalized]
        false -> String.split(normalized, ~r/\s+/u, trim: true)
      end

    length(values) in 1..maximum_values and
      Enum.all?(values, fn value ->
        value == "auto" or value in @css_wide_keywords or
          Regex.match?(@page_percentage_regex, value) or valid_page_length?(value, true) or
          valid_page_function?(value)
      end)
  end

  defp valid_page_length?(value, allow_negative?) do
    length_regex =
      if allow_negative?, do: @page_length_regex, else: @page_nonnegative_length_regex

    value == "0" or Regex.match?(length_regex, value)
  end

  defp valid_page_function?(value) do
    Regex.match?(~r/^(?:calc|min|max|clamp|var|env)\(.+\)$/u, value)
  end

  defp page_marks_option(marks) do
    case Enum.all?(marks, &(&1 in ["crop", "cross"])) and Enum.uniq(marks) == marks do
      true -> {:ok, nil}
      false -> :error
    end
  end

  defp page_size_option(value) do
    PageGeometry.css_page_size_option(value)
  end

  defp page_margin_option(value) do
    PageGeometry.css_margin_option(value)
  end

  defp page_margin_side(property) do
    case property do
      "margin-top" -> :top
      "margin-right" -> :right
      "margin-bottom" -> :bottom
      "margin-left" -> :left
    end
  end

  defp parse_rules(css) do
    case String.trim(css) do
      "" ->
        {:ok, []}

      css ->
        rule_sources = Regex.scan(~r/[^{}]+\{[^{}]*\}/u, css) |> Enum.map(&List.first/1)
        unparsed = Regex.replace(~r/[^{}]+\{[^{}]*\}/u, css, "")

        case String.trim(unparsed) do
          "" -> sources_to_rules(rule_sources)
          _ -> {:error, :invalid_css}
        end
    end
  end

  defp sources_to_rules(rule_sources) do
    rule_sources
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn {source, order}, {:ok, acc} ->
      case parse_rule(source, order) do
        {:ok, rule} -> {:cont, {:ok, acc ++ [rule]}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp parse_rule(source, order) do
    captures =
      Regex.named_captures(
        ~r/^\s*(?<selectors>[^{}]+)\{(?<declarations>[^{}]*)\}\s*$/u,
        source
      )

    %{"selectors" => selector_source, "declarations" => declaration_source} = captures

    with {:ok, selectors} <- parse_selectors(selector_source),
         true <- selectors != [],
         {:ok, declarations} <- parse_declarations(declaration_source),
         true <- declarations != [] do
      {:ok, %{selectors: selectors, declarations: declarations, order: order}}
    else
      _ -> {:error, :invalid_css}
    end
  end

  defp parse_selectors(selector_source) do
    selector_source
    |> String.split(",")
    |> Enum.map(&String.trim/1)
    |> Enum.reduce_while({:ok, []}, fn selector, {:ok, acc} ->
      case parse_selector(selector) do
        {:ok, parsed} -> {:cont, {:ok, acc ++ [parsed]}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp parse_selector(selector) do
    tokens =
      selector
      |> String.replace(~r/\s*>\s*/u, " > ")
      |> String.split(~r/\s+/u, trim: true)

    case tokens do
      [] -> {:error, :invalid_css}
      tokens -> selector_tokens_to_parts(tokens)
    end
  end

  defp selector_tokens_to_parts(tokens) do
    parsed =
      Enum.reduce_while(tokens, {:ok, [], nil}, fn token, {:ok, parts, pending_combinator} ->
        cond do
          token == ">" and (parts == [] or not is_nil(pending_combinator)) ->
            {:halt, {:error, :invalid_css}}

          token == ">" ->
            {:cont, {:ok, parts, :child}}

          true ->
            case parse_simple_selector(token) do
              {:ok, part} ->
                combinator =
                  case parts do
                    [] -> nil
                    _ -> pending_combinator || :descendant
                  end

                {:cont, {:ok, parts ++ [Map.put(part, :combinator, combinator)], nil}}

              {:error, reason} ->
                {:halt, {:error, reason}}
            end
        end
      end)

    case parsed do
      {:ok, _parts, pending_combinator} when not is_nil(pending_combinator) ->
        {:error, :invalid_css}

      {:ok, parts, nil} ->
        {:ok, %{parts: parts, specificity: specificity(parts)}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp parse_simple_selector(selector) do
    captures =
      Regex.named_captures(
        ~r/^(?<tag>\*|[a-zA-Z][a-zA-Z0-9]*)?(?<modifiers>(?:[#.][a-zA-Z_-][a-zA-Z0-9_-]*)*)(?<pseudo>:(?:first-child|last-child|root|nth-child\([1-9]\d*\)))?$/u,
        selector
      )

    case captures do
      %{"tag" => tag, "modifiers" => modifiers, "pseudo" => pseudo} ->
        part = %{
          tag: tag_name(tag),
          id: nil,
          classes: [],
          pseudo_classes: pseudo_classes(pseudo),
          combinator: nil
        }

        parse_selector_modifiers(modifiers, part)

      _ ->
        {:error, :invalid_css}
    end
  end

  defp parse_selector_modifiers("", part), do: {:ok, part}

  defp parse_selector_modifiers(modifiers, part) do
    captures = Regex.scan(~r/([#.])([a-zA-Z_-][a-zA-Z0-9_-]*)/u, modifiers)
    modifier_captures_to_part(captures, part)
  end

  defp modifier_captures_to_part(captures, part) do
    Enum.reduce_while(captures, {:ok, part}, fn [_, prefix, name], {:ok, acc} ->
      case {prefix, acc.id} do
        {"#", nil} -> {:cont, {:ok, %{acc | id: name}}}
        {"#", _id} -> {:halt, {:error, :invalid_css}}
        {".", _id} -> {:cont, {:ok, %{acc | classes: acc.classes ++ [name]}}}
      end
    end)
  end

  defp specificity(parts) do
    Enum.reduce(parts, {0, 0, 0}, fn part, {ids, classes, elements} ->
      id_count = if is_nil(part.id), do: 0, else: 1
      element_count = if is_nil(part.tag), do: 0, else: 1
      class_count = length(part.classes) + length(part.pseudo_classes)
      {ids + id_count, classes + class_count, elements + element_count}
    end)
  end

  defp pseudo_classes(pseudo) do
    case pseudo do
      ":first-child" ->
        [:first_child]

      ":last-child" ->
        [:last_child]

      ":root" ->
        [:root]

      pseudo ->
        case Regex.named_captures(~r/^:nth-child\((?<index>[1-9]\d*)\)$/u, pseudo || "") do
          %{"index" => index} ->
            {index, ""} = Integer.parse(index)
            [{:nth_child, index}]

          _ ->
            []
        end
    end
  end

  defp tag_name("") do
    nil
  end

  defp tag_name("*") do
    nil
  end

  defp tag_name(tag) do
    String.downcase(tag)
  end

  defp parse_declaration(declaration) do
    case String.split(declaration, ":", parts: 2) do
      [property, value] ->
        property = property |> String.trim() |> String.downcase()
        value = String.trim(value)
        important? = String.match?(value, ~r/\s*!important\s*$/iu)

        value =
          value
          |> String.replace(~r/\s*!important\s*$/iu, "")
          |> String.trim()

        case valid_property?(property) and value != "" do
          true ->
            case important? do
              true -> {:ok, {property, value, :important}}
              false -> {:ok, {property, value}}
            end

          false ->
            {:error, :invalid_css}
        end

      _ ->
        {:error, :invalid_css}
    end
  end

  defp valid_property?(property) do
    Regex.match?(~r/^[a-z][a-z-]*$/u, property) or
      Regex.match?(~r/^--[a-zA-Z_][a-zA-Z0-9_-]*$/u, property)
  end

  defp css_error_detail(original_css, parsed_css) do
    parsed_css
    |> first_css_issue()
    |> css_issue_to_detail(original_css)
  end

  defp first_css_issue(css) do
    rule_sources = Regex.scan(~r/[^{}]+\{[^{}]*\}/u, css) |> Enum.map(&List.first/1)
    unparsed = Regex.replace(~r/[^{}]+\{[^{}]*\}/u, css, "")

    case String.trim(unparsed) do
      "" ->
        Enum.find_value(rule_sources, {:stylesheet, String.trim(css)}, &rule_issue/1)

      unparsed ->
        {:stylesheet, String.trim(unparsed)}
    end
  end

  defp rule_issue(rule_source) do
    %{"selectors" => selectors, "declarations" => declarations} =
      Regex.named_captures(
        ~r/^\s*(?<selectors>[^{}]+)\{(?<declarations>[^{}]*)\}\s*$/u,
        rule_source
      )

    cond do
      invalid_selector(selectors) ->
        {:selector, invalid_selector(selectors)}

      invalid_declaration(declarations) ->
        {:declaration, invalid_declaration(declarations)}

      true ->
        nil
    end
  end

  defp invalid_selector(selectors) do
    selectors
    |> String.split(",")
    |> Enum.map(&String.trim/1)
    |> Enum.find(fn selector ->
      selector == "" or match?({:error, :invalid_css}, parse_selector(selector))
    end)
  end

  defp invalid_declaration(declarations) do
    declarations
    |> String.split(";")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.find(fn declaration ->
      match?({:error, :invalid_css}, parse_declaration(declaration))
    end)
    |> case do
      nil ->
        case String.trim(declarations) do
          "" -> declarations
          _ -> nil
        end

      declaration ->
        declaration
    end
  end

  defp css_issue_to_detail({kind, source}, css) do
    source = String.trim(source)
    {line, column} = source_location(css, source)

    %{
      stage: :css,
      reason: :invalid_css,
      message: css_issue_message(kind, line, source),
      line: line,
      column: column,
      source: source
    }
  end

  defp css_issue_message(kind, line, source) do
    case kind do
      :selector -> ~s(line #{line}: selector "#{source}" is invalid or unsupported)
      :declaration -> ~s(line #{line}: declaration "#{source}" is invalid or unsupported)
      :stylesheet -> ~s(line #{line}: CSS source "#{source}" is invalid)
    end
  end

  defp declaration_error_detail(css, declaration) do
    source = String.trim(declaration)
    {line, column} = source_location(css, source)

    %{
      stage: :css,
      reason: :invalid_css,
      message: ~s(line #{line}: declaration "#{source}" is invalid or unsupported),
      line: line,
      column: column,
      source: source
    }
  end

  defp source_location(source, snippet) do
    case snippet == "" do
      true ->
        {1, 1}

      false ->
        case :binary.match(source, snippet) do
          {index, _length} ->
            prefix = binary_part(source, 0, index)
            lines = String.split(prefix, "\n", trim: false)
            line = length(lines)
            column = String.length(List.last(lines) || "") + 1
            {line, column}

          :nomatch ->
            {1, 1}
        end
    end
  end
end
