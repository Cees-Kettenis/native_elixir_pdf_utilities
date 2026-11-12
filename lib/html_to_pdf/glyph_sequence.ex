defmodule NativeElixirPdfUtilities.HtmlToPdf.GlyphSequence do
  @moduledoc false

  @doc "Builds a glyph sequence with stable identifiers for contextual lookup actions."
  @spec new([map()]) :: map()
  def new(glyphs) do
    count = length(glyphs)

    nodes =
      glyphs
      |> Enum.with_index()
      |> Map.new(fn {glyph, id} ->
        {id,
         glyph
         |> Map.put_new(:input_ids, MapSet.new([id]))
         |> Map.merge(%{
           id: id,
           previous: if(id > 0, do: id - 1),
           next: if(id + 1 < count, do: id + 1)
         })}
      end)

    %{
      nodes: nodes,
      removed: %{},
      first: if(count > 0, do: 0),
      last: if(count > 0, do: count - 1),
      next_id: count
    }
  end

  @doc "Returns surviving glyphs in logical order."
  @spec to_list(map()) :: [map()]
  def to_list(sequence), do: collect(sequence, sequence.first, [])

  defp collect(sequence, id, output) do
    if id == nil do
      Enum.reverse(output)
    else
      glyph = Map.fetch!(sequence.nodes, id)
      collect(sequence, glyph.next, [glyph | output])
    end
  end

  @doc "Updates one surviving glyph while preserving its sequence links."
  @spec put(map(), non_neg_integer(), map()) :: map()
  def put(sequence, id, changes),
    do: %{sequence | nodes: Map.update!(sequence.nodes, id, &Map.merge(&1, changes))}

  @doc "Removes a glyph without invalidating the identifiers of adjacent glyphs."
  @spec delete(map(), non_neg_integer()) :: map()
  def delete(sequence, id) do
    node = Map.fetch!(sequence.nodes, id)

    sequence = %{
      sequence
      | nodes: Map.delete(sequence.nodes, id),
        removed: Map.put(sequence.removed, id, %{next: node.next, previous: node.previous})
    }

    sequence =
      if node.previous == nil,
        do: %{sequence | first: node.next},
        else: put(sequence, node.previous, %{next: node.next})

    if node.next == nil,
      do: %{sequence | last: node.previous},
      else: put(sequence, node.next, %{previous: node.previous})
  end

  @doc "Inserts one glyph after a surviving identifier and assigns a fresh identifier."
  @spec insert_after(map(), non_neg_integer(), map()) :: {map(), non_neg_integer()}
  def insert_after(sequence, previous, glyph) do
    id = sequence.next_id
    next = Map.fetch!(sequence.nodes, previous).next
    glyph = Map.merge(glyph, %{id: id, previous: previous, next: next})
    sequence = %{sequence | nodes: Map.put(sequence.nodes, id, glyph), next_id: id + 1}
    sequence = put(sequence, previous, %{next: id})

    sequence =
      if next == nil, do: %{sequence | last: id}, else: put(sequence, next, %{previous: id})

    {sequence, id}
  end

  @doc "Reorders surviving identifiers while preserving contextual targets and original input ownership."
  @spec reorder(map(), [non_neg_integer()]) :: map()
  def reorder(sequence, ids) do
    count = length(ids)
    tuple = List.to_tuple(ids)

    nodes =
      ids
      |> Enum.with_index()
      |> Enum.reduce(sequence.nodes, fn {id, index}, nodes ->
        Map.update!(
          nodes,
          id,
          &%{
            &1
            | previous: if(index > 0, do: elem(tuple, index - 1)),
              next: if(index + 1 < count, do: elem(tuple, index + 1))
          }
        )
      end)

    %{sequence | nodes: nodes, first: List.first(ids), last: List.last(ids)}
  end
end
