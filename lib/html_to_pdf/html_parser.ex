defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlParser do
  @moduledoc """
  Strict HTML parser for the native HTML-to-PDF renderer.

  Milestone 6 supports a strict subset of document-oriented HTML: paragraphs,
  headings, inline emphasis/color containers, lists, links, and tables.
  Unsupported or malformed markup returns an error instead of guessing at
  browser behavior.
  """

  @type text_node :: %{type: :text, text: String.t()}
  @type element_node :: %{
          type: :element,
          tag: String.t(),
          attributes: map(),
          children: [text_node() | element_node()]
        }
  @type dom_tree :: %{type: :document, children: [element_node()]}

  @block_tags ~w(p h1 h2 h3 h4 h5 h6 ul ol table)
  @inline_tags ~w(strong b em i span a)
  @list_tags ~w(ul ol)
  @table_structure_tags ~w(table thead tbody tfoot tr)
  @table_content_tags ~w(caption th td)

  @doc """
  Parses an HTML binary into a renderer DOM tree.
  """
  @spec parse(String.t()) :: {:ok, dom_tree()} | {:error, :invalid_html | :unsupported_html}
  def parse(html) do
    case html do
      html when is_binary(html) ->
        parse_document(html)

      _ ->
        {:error, :invalid_html}
    end
  end

  defp parse_document(html) do
    tokens = html |> tokenize() |> List.flatten()

    with true <- Enum.join(tokens) == html,
         {:ok, children, []} <- parse_children(tokens, :document, nil, []),
         true <- children != [] do
      {:ok, %{type: :document, children: children}}
    else
      _ -> {:error, :unsupported_html}
    end
  end

  defp tokenize(html) do
    Regex.scan(~r/<[^>]*>|[^<]+/u, html)
  end

  defp parse_children(tokens, context, closing_tag, children) do
    case tokens do
      [] when is_nil(closing_tag) ->
        {:ok, children, []}

      [] ->
        {:error, :unsupported_html}

      [token | remaining] ->
        parse_child(token, remaining, context, closing_tag, children)
    end
  end

  defp parse_child(token, remaining, context, closing_tag, children) do
    cond do
      String.starts_with?(token, "</") ->
        case parse_closing_tag(token) do
          {:ok, tag} when tag == closing_tag -> {:ok, children, remaining}
          _ -> {:error, :unsupported_html}
        end

      String.starts_with?(token, "<") ->
        with {:ok, tag, attributes} <- parse_opening_tag(token),
             true <- allowed_child?(context, tag),
             {:ok, element_children, rest} <- parse_children(remaining, tag, tag, []),
             true <- valid_element?(tag, element_children) do
          element = %{
            type: :element,
            tag: tag,
            attributes: attributes,
            children: element_children
          }

          parse_children(rest, context, closing_tag, children ++ [element])
        else
          _ -> {:error, :unsupported_html}
        end

      context == :document and String.trim(token) == "" ->
        parse_children(remaining, context, closing_tag, children)

      context in @list_tags and String.trim(token) == "" ->
        parse_children(remaining, context, closing_tag, children)

      context in @table_structure_tags and String.trim(token) == "" ->
        parse_children(remaining, context, closing_tag, children)

      context == :document ->
        {:error, :unsupported_html}

      true ->
        parse_children(
          remaining,
          context,
          closing_tag,
          children ++ [%{type: :text, text: decode_entities(token)}]
        )
    end
  end

  defp parse_opening_tag(token) do
    captures =
      Regex.named_captures(
        ~r/^<\s*(?<tag>[a-zA-Z][a-zA-Z0-9]*)\s*(?<attributes>[^<>]*?)\s*>$/u,
        token
      )

    case captures do
      %{"tag" => tag, "attributes" => attributes} ->
        tag = String.downcase(tag)

        with true <- supported_tag?(tag),
             {:ok, attributes} <- parse_attributes(attributes, tag) do
          {:ok, tag, attributes}
        else
          _ -> {:error, :unsupported_html}
        end

      _ ->
        {:error, :unsupported_html}
    end
  end

  defp parse_closing_tag(token) do
    case Regex.named_captures(~r/^<\/\s*(?<tag>[a-zA-Z][a-zA-Z0-9]*)\s*>$/u, token) do
      %{"tag" => tag} ->
        tag = String.downcase(tag)

        case supported_tag?(tag) do
          true -> {:ok, tag}
          false -> {:error, :unsupported_html}
        end

      _ ->
        {:error, :unsupported_html}
    end
  end

  defp parse_attributes(attributes, tag) do
    case String.trim(attributes) do
      "" ->
        {:ok, %{}}

      attributes ->
        captures =
          Regex.scan(~r/\s*([a-zA-Z][a-zA-Z0-9_-]*)\s*=\s*("[^"]*"|'[^']*')/u, attributes)

        parsed_source = captures |> Enum.map_join("", &List.first/1) |> String.trim()

        with true <- parsed_source == attributes,
             {:ok, parsed} <- attributes_to_map(captures, tag) do
          {:ok, parsed}
        else
          _ -> {:error, :unsupported_html}
        end
    end
  end

  defp attributes_to_map(captures, tag) do
    Enum.reduce_while(captures, {:ok, %{}}, fn [_, name, quoted_value], {:ok, acc} ->
      name = String.downcase(name)
      value = quoted_value |> String.slice(1..-2//1) |> decode_entities()

      case {name, tag, Map.has_key?(acc, name)} do
        {"style", _, false} -> {:cont, {:ok, Map.put(acc, name, value)}}
        {"href", "a", false} -> {:cont, {:ok, Map.put(acc, name, value)}}
        _ -> {:halt, {:error, :unsupported_html}}
      end
    end)
  end

  defp allowed_child?(context, tag) do
    cond do
      context == :document ->
        tag in @block_tags

      context in @list_tags ->
        tag == "li"

      context == "table" ->
        tag in ~w(caption thead tbody tfoot tr)

      context in ~w(thead tbody tfoot) ->
        tag == "tr"

      context == "tr" ->
        tag in ~w(th td)

      context == "a" ->
        tag in @inline_tags and tag != "a"

      context in @table_content_tags ->
        tag in @inline_tags

      context == "li" or context in @block_tags or context in @inline_tags ->
        tag in @inline_tags

      true ->
        false
    end
  end

  defp supported_tag?(tag) do
    tag in @block_tags or tag in @inline_tags or tag == "li" or
      tag in ~w(caption thead tbody tfoot tr th td)
  end

  defp valid_element?(tag, children) do
    case tag do
      "table" ->
        caption_count = Enum.count(children, &match?(%{tag: "caption"}, &1))
        caption_first? = children == [] or hd(children).tag == "caption"
        caption_count <= 1 and caption_first? and Enum.any?(children, &table_row_container?/1)

      tag when tag in ~w(thead tbody tfoot) ->
        children != [] and Enum.all?(children, &match?(%{tag: "tr"}, &1))

      "tr" ->
        children != [] and Enum.all?(children, &match?(%{tag: tag} when tag in ["th", "td"], &1))

      _ ->
        true
    end
  end

  defp table_row_container?(child) do
    case child do
      %{tag: tag} when tag in ~w(thead tbody tfoot tr) -> true
      _ -> false
    end
  end

  defp decode_entities(text) do
    text
    |> String.replace("&amp;", "&")
    |> String.replace("&lt;", "<")
    |> String.replace("&gt;", ">")
    |> String.replace("&quot;", "\"")
    |> String.replace("&#39;", "'")
    |> String.replace("&apos;", "'")
  end
end
