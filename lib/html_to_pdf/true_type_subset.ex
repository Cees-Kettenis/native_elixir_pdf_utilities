defmodule NativeElixirPdfUtilities.HtmlToPdf.TrueTypeSubset do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.TrueTypeSubsetValidator

  @doc false
  @spec subset(map(), [non_neg_integer()]) ::
          {:ok, binary()} | {:error, {atom(), map()}}
  def subset(font, glyph_ids) do
    case TrueTypeSubsetValidator.prepare(font, glyph_ids) do
      {:ok, :keep_full} ->
        {:ok, font.data}

      {:ok, plan} ->
        {glyf, loca} = glyph_tables(plan)

        tables =
          plan.tables
          |> Enum.reject(&(elem(&1, 0) == "DSIG"))
          |> Enum.map(fn
            {"glyf", _data} -> {"glyf", glyf}
            {"loca", _data} -> {"loca", loca}
            {"head", data} -> {"head", put_u32(data, 8, 0)}
            table -> table
          end)

        {:ok, NativeElixirPdfUtilities.HtmlToPdf.Font.Sfnt.rebuild(plan.scaler, tables)}

      {:error, _reason} = error ->
        error
    end
  end

  defp glyph_tables(plan) do
    # Every omitted slot shares the next retained glyph's offset. Emit those
    # identical entries in bulk instead of walking all unused glyph programs.
    {glyph_chunks, locations, position, next_id} =
      plan.retained
      |> Enum.sort()
      |> Enum.reduce({[], [], 0, 0}, fn glyph_id, {chunks, locations, position, next_id} ->
        start = elem(plan.offsets, glyph_id)
        finish = elem(plan.offsets, glyph_id + 1)
        glyph = binary_part(plan.glyf, start, finish - start)
        padding = rem(2 - rem(byte_size(glyph), 2), 2)

        offset =
          case plan.location_format do
            0 -> <<div(position, 2)::16>>
            1 -> <<position::32>>
          end

        {[:binary.copy(<<0>>, padding), glyph | chunks],
         [:binary.copy(offset, glyph_id - next_id + 1) | locations],
         position + byte_size(glyph) + padding, glyph_id + 1}
      end)

    final_offset =
      case plan.location_format do
        0 -> <<div(position, 2)::16>>
        1 -> <<position::32>>
      end

    locations = [:binary.copy(final_offset, plan.glyph_count - next_id + 1) | locations]

    {glyph_chunks |> Enum.reverse() |> IO.iodata_to_binary(),
     locations |> Enum.reverse() |> IO.iodata_to_binary()}
  end

  defp put_u32(data, offset, value) do
    <<before::binary-size(^offset), _old::32, after_value::binary>> = data
    <<before::binary, value::32, after_value::binary>>
  end
end
