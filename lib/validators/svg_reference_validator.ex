defmodule NativeElixirPdfUtilities.Validators.SvgReferenceValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.SvgSceneValidator

  @doc false
  @spec referenced_ids(map()) :: MapSet.t()
  def referenced_ids(root) do
    root
    |> SvgSceneValidator.tree_nodes()
    |> Enum.reduce(MapSet.new(), fn {node, _}, ids ->
      if node.tag == "use" do
        case Map.get(node.attributes, "href", Map.get(node.attributes, "xlink:href", "")) do
          "#" <> id -> MapSet.put(ids, id)
          _ -> ids
        end
      else
        ids
      end
    end)
  end
end
