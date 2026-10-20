defmodule NativeElixirPdfUtilities.Validators.SvgReferenceValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.SvgSceneValidator

  @doc "Selects SVG href before an XLink href, using its expanded XML name."
  @spec href_attribute(map()) :: String.t()
  def href_attribute(node) do
    if Map.has_key?(node.attributes, "href") do
      "href"
    else
      namespaces = Map.get(node, :attribute_namespaces, %{})
      fallback = if Map.has_key?(namespaces, "xlink:href"), do: "href", else: "xlink:href"

      Enum.find_value(namespaces, fallback, fn
        {name, %{local_name: "href", namespace: "http://www.w3.org/1999/xlink"}} -> name
        _ -> nil
      end)
    end
  end

  @doc false
  @spec referenced_ids(map()) :: MapSet.t()
  def referenced_ids(root) do
    root
    |> SvgSceneValidator.tree_nodes()
    |> Enum.reduce(MapSet.new(), fn {node, _}, ids ->
      if node.tag == "use" do
        case Map.get(node.attributes, href_attribute(node), "") do
          "#" <> id -> MapSet.put(ids, id)
          _ -> ids
        end
      else
        ids
      end
    end)
  end
end
