defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlParser do
  @moduledoc """
  Strict HTML parser for the native HTML-to-PDF renderer.

  Milestone 2 intentionally supports only a strict paragraph document. Unsupported
  or malformed markup returns an error instead of guessing at browser behavior.
  """

  @type text_node :: %{type: :text, text: String.t()}
  @type element_node :: %{
          type: :element,
          tag: String.t(),
          attributes: map(),
          children: [text_node()]
        }
  @type dom_tree :: %{type: :document, children: [element_node()]}

  @doc """
  Parses an HTML binary into a renderer DOM tree.
  """
  @spec parse(String.t()) :: {:ok, dom_tree()} | {:error, :invalid_html | :unsupported_html}
  def parse(html) do
    case html do
      html when is_binary(html) ->
        captures = Regex.named_captures(~r/^\s*<p>(?<text>[^<>]*)<\/p>\s*$/u, html)

        case captures do
          %{"text" => text} ->
            {:ok,
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "p",
                   attributes: %{},
                   children: [%{type: :text, text: decode_entities(text)}]
                 }
               ]
             }}

          _ ->
            {:error, :unsupported_html}
        end

      _ ->
        {:error, :invalid_html}
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
