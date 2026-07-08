defmodule NativeElixirPdfUtilities.HtmlToPdf.CssParser do
  @moduledoc """
  Strict CSS parser for the native HTML-to-PDF renderer.

  The parser accepts the document-oriented selector subset used by the style
  cascade: element, class, id, element.class, descendant, child, and comma
  groups. Simple `@page` rules are accepted outside the style cascade so the
  renderer can use page size and margin defaults. Declarations are kept as
  normalized property/value pairs so the style layer can validate values against
  the renderer's supported property set.
  """

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
  @type page_option ::
          {:page_size, :a4 | :letter | {number(), number()}} | {:margin, String.t() | number()}

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
        parsed_css = css |> strip_comments() |> strip_page_rules()

        case parse_rules(parsed_css) do
          {:ok, stylesheet} -> {:ok, stylesheet}
          {:error, :invalid_css} -> {:error, {:invalid_css, css_error_detail(css, parsed_css)}}
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
  Extracts renderer page defaults from simple `@page` rules.

  Supported declarations are single-value `margin` lengths and common
  `size` values such as `A4`, `A4 landscape`, `letter`, and
  `letter landscape`. Unsupported page declarations are ignored so normal CSS
  parsing remains strict for the supported style cascade.
  """
  @spec page_options(String.t()) :: {:ok, [page_option()]} | {:error, :invalid_css}
  def page_options(css) do
    case css do
      css when is_binary(css) ->
        css
        |> strip_comments()
        |> page_rule_blocks()
        |> Enum.reduce({:ok, []}, fn block, {:ok, acc} ->
          case parse_declarations(block) do
            {:ok, declarations} -> {:ok, Keyword.merge(acc, page_options_from(declarations))}
            {:error, _reason} -> {:ok, acc}
          end
        end)

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

  defp page_rule_blocks(css) do
    ~r/@page\s*(?:[^{]*)\{(?<declarations>[^{}]*)\}/ui
    |> Regex.scan(css, capture: ["declarations"])
    |> List.flatten()
  end

  defp page_options_from(declarations) do
    Enum.reduce(declarations, [], fn declaration, acc ->
      case declaration do
        {"size", value} ->
          case page_size_option(value) do
            nil -> acc
            page_size -> Keyword.put(acc, :page_size, page_size)
          end

        {"margin", value} ->
          case page_margin_option(value) do
            nil -> acc
            margin -> Keyword.put(acc, :margin, margin)
          end

        _ ->
          acc
      end
    end)
  end

  defp page_size_option(value) do
    tokens = value |> String.trim() |> String.downcase() |> String.split(~r/\s+/u, trim: true)

    case tokens do
      ["a4"] -> :a4
      ["a4", "portrait"] -> :a4
      ["portrait", "a4"] -> :a4
      ["a4", "landscape"] -> {841.89, 595.28}
      ["landscape", "a4"] -> {841.89, 595.28}
      ["letter"] -> :letter
      ["letter", "portrait"] -> :letter
      ["portrait", "letter"] -> :letter
      ["letter", "landscape"] -> {792.0, 612.0}
      ["landscape", "letter"] -> {792.0, 612.0}
      _ -> nil
    end
  end

  defp page_margin_option(value) do
    normalized = String.trim(value)

    cond do
      normalized == "0" ->
        0.0

      String.match?(normalized, ~r/^\d+(?:\.\d+)?(?:pt|px|mm|cm|in)$/u) ->
        normalized

      true ->
        nil
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
        {index, _length} = :binary.match(source, snippet)
        prefix = binary_part(source, 0, index)
        lines = String.split(prefix, "\n", trim: false)
        line = length(lines)
        column = String.length(List.last(lines) || "") + 1
        {line, column}
    end
  end
end
