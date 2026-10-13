defmodule NativeElixirPdfUtilities.HtmlTreeHelpers do
  @moduledoc false

  @doc "Removes additive parser transport while preserving the complete authored content tree."
  @spec tree_content(term()) :: term()
  def tree_content(value) do
    case value do
      value when is_map(value) ->
        value
        |> Map.drop([
          :node_id,
          :document_id,
          :tree_scope_id,
          :parent_id,
          :source,
          :attribute_order
        ])
        |> Map.new(fn {key, value} -> {key, tree_content(value)} end)

      values when is_list(values) ->
        Enum.map(values, &tree_content/1)

      {:ok, value} ->
        {:ok, tree_content(value)}

      value ->
        value
    end
  end
end
