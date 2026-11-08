defmodule NativeElixirPdfUtilities.TestSupport.CssStyles do
  @moduledoc false
  import ExUnit.Assertions
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

  @doc "Computes authored source cases and indexes their computed styles by element ID."
  @spec by_id(String.t(), [String.t()]) :: map()
  def by_id(html, stylesheets \\ []) do
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)

    assert {:ok, styled} =
             Style.compute_detailed(dom,
               stylesheets: Enum.map(stylesheets, &{:css, &1}),
               system_font_discovery: false
             )

    collect(styled.children)
  end

  defp collect(nodes) do
    Enum.reduce(nodes, %{}, fn node, result ->
      result = Map.merge(result, collect(Map.get(node, :children, [])))

      case get_in(node, [:attributes, "id"]) do
        nil -> result
        id -> Map.put(result, id, node.style)
      end
    end)
  end
end
