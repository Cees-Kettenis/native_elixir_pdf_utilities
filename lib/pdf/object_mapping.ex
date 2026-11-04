defmodule NativeElixirPdfUtilities.Pdf.ObjectMapping do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.PdfValidator

  @doc "Visits parsed references incrementally under the active shared reader work allowance."
  @spec reduce_references_while(term(), acc, (tuple(), acc -> {:cont, acc} | {:halt, acc})) ::
          {:cont, acc} | {:halt, acc}
        when acc: term()
  def reduce_references_while(value, acc, fun) do
    PdfValidator.charge_reader_budget(:max_pdf_reader_work, 1)

    case value do
      {:ref, reference} ->
        fun.(reference, acc)

      values when is_list(values) or is_map(values) ->
        dictionary = is_map(values)

        Enum.reduce_while(values, {:cont, acc}, fn entry, {:cont, acc} ->
          child = if dictionary, do: elem(entry, 1), else: entry

          case reduce_references_while(child, acc, fun) do
            {:cont, acc} -> {:cont, {:cont, acc}}
            {:halt, acc} -> {:halt, {:halt, acc}}
          end
        end)

      _ ->
        {:cont, acc}
    end
  end

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
