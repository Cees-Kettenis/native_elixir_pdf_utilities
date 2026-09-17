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
end
