defmodule NativeElixirPdfUtilities.HtmlToPdf.CssParser do
  @moduledoc """
  Strict CSS parser for the native HTML-to-PDF renderer.

  The parser accepts the document-oriented selector subset used by the style
  cascade: element, class, id, element.class, descendant, child, and comma
  groups. Declarations are kept as normalized property/value pairs so the style
  layer can validate values against the renderer's supported property set.
  """

  @type declaration :: {String.t(), String.t()}
  @type selector_part :: %{
          tag: String.t() | nil,
          id: String.t() | nil,
          classes: [String.t()],
          pseudo_classes: [:first_child],
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

  @doc """
  Parses a CSS stylesheet into strict renderer rules.
  """
  @spec parse(String.t()) :: {:ok, stylesheet()} | {:error, :invalid_css}
  def parse(css) do
    case css do
      css when is_binary(css) ->
        css
        |> strip_comments()
        |> parse_rules()

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
    case css do
      css when is_binary(css) ->
        declarations =
          css
          |> String.split(";")
          |> Enum.map(&String.trim/1)
          |> Enum.reject(&(&1 == ""))

        Enum.reduce_while(declarations, {:ok, []}, fn declaration, {:ok, acc} ->
          case parse_declaration(declaration) do
            {:ok, parsed} -> {:cont, {:ok, acc ++ [parsed]}}
            {:error, reason} -> {:halt, {:error, reason}}
          end
        end)

      _ ->
        {:error, :invalid_css}
    end
  end

  defp strip_comments(css) do
    Regex.replace(~r/\/\*.*?\*\//us, css, "")
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
        ~r/^(?<tag>[a-zA-Z][a-zA-Z0-9]*)?(?<modifiers>(?:[#.][a-zA-Z_-][a-zA-Z0-9_-]*)*)(?<pseudo>:(?:first-child))?$/u,
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
      ":first-child" -> [:first_child]
      _ -> []
    end
  end

  defp tag_name("") do
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

        case Regex.match?(~r/^[a-z][a-z-]*$/u, property) and value != "" do
          true -> {:ok, {property, value}}
          false -> {:error, :invalid_css}
        end

      _ ->
        {:error, :invalid_css}
    end
  end
end
