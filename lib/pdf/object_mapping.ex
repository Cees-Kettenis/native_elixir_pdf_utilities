defmodule NativeElixirPdfUtilities.Pdf.ObjectMapping do
  @moduledoc false

  @doc false
  @spec remap(term(), map()) :: term()
  def remap(value, reference_map) do
    case value do
      {:ref, reference} ->
        {:ref, {Map.fetch!(reference_map, reference), elem(reference, 1)}}

      values when is_list(values) ->
        Enum.map(values, &remap(&1, reference_map))

      dictionary when is_map(dictionary) ->
        Map.new(dictionary, fn {key, item} -> {key, remap(item, reference_map)} end)

      value ->
        value
    end
  end

  @doc "Collects indirect references in an already parsed PDF value for native resource graph traversal."
  @spec references(term()) :: [tuple()]
  def references(value) do
    case value do
      {:ref, ref} -> [ref]
      values when is_map(values) -> Enum.flat_map(Map.values(values), &references/1)
      values when is_list(values) -> Enum.flat_map(values, &references/1)
      _ -> []
    end
  end
end
