defmodule NativeElixirPdfUtilities.HtmlToPdf.Font.Cff do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.CffSubsetValidator

  @doc "Builds a validated CID-keyed CFF program with source glyph IDs as CIDs."
  @spec subset(map(), [non_neg_integer()], boolean()) ::
          {:ok, binary(), boolean()} | {:error, {atom(), map()}}
  def subset(font, glyph_ids, subset?) do
    with {:ok, plan} <- CffSubsetValidator.prepare(font, glyph_ids, subset?) do
      {:ok, IO.iodata_to_binary(plan.parts), plan.subset?}
    end
  end

  @doc false
  @spec index([binary()]) :: binary()
  def index(objects) do
    case objects do
      [] ->
        <<0::16>>

      _ ->
        {offsets, finish} =
          Enum.map_reduce(objects, 1, fn bytes, offset ->
            {<<offset::32>>, offset + byte_size(bytes)}
          end)

        IO.iodata_to_binary([<<length(objects)::16, 4>>, offsets, <<finish::32>>, objects])
    end
  end

  @doc false
  @spec dictionary(map()) :: binary()
  def dictionary(values) do
    values
    |> Enum.sort_by(fn {key, _} -> if key == {12, 30}, do: {-1, 0}, else: {0, key} end)
    |> Enum.map(fn {operator, operands} ->
      [
        Enum.map(operands, fn value ->
          case value do
            value when is_integer(value) ->
              <<29, value::signed-32>>

            value ->
              nibbles =
                :erlang.float_to_binary(value, [:short])
                |> String.replace("e-", "c")
                |> String.replace("e+", "b")
                |> String.replace("e", "b")
                |> String.to_charlist()
                |> Enum.map(fn char ->
                  case char do
                    ?. -> 10
                    ?b -> 11
                    ?c -> 12
                    ?- -> 14
                    char -> char - ?0
                  end
                end)

              nibbles = nibbles ++ if(rem(length(nibbles), 2) == 0, do: [15, 15], else: [15])

              <<30,
                for([a, b] <- Enum.chunk_every(nibbles, 2), into: <<>>, do: <<a::4, b::4>>)::binary>>
          end
        end),
        case operator do
          {12, escaped} -> <<12, escaped>>
          value -> <<value>>
        end
      ]
    end)
    |> IO.iodata_to_binary()
  end
end
