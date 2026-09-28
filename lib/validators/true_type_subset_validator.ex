defmodule NativeElixirPdfUtilities.Validators.TrueTypeSubsetValidator do
  @moduledoc false

  import Bitwise

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @type plan :: %{
          scaler: non_neg_integer(),
          tables: [{binary(), binary()}],
          glyf: binary(),
          offsets: tuple(),
          retained: MapSet.t(),
          glyph_count: pos_integer(),
          location_format: 0 | 1
        }

  @doc false
  @spec prepare(map(), [non_neg_integer()]) ::
          {:ok, plan() | :keep_full} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare(font, glyph_ids) do
    case {font, glyph_ids} do
      {%{data: data} = font, glyph_ids} when is_binary(data) and is_list(glyph_ids) ->
        with {:ok, scaler, tables} <- table_directory(data),
             {:ok, head} <- required_table(tables, "head"),
             {:ok, maxp} <- required_table(tables, "maxp"),
             {:ok, glyf} <- required_table(tables, "glyf"),
             {:ok, loca} <- required_table(tables, "loca"),
             {:ok, embedding_mode} <- embedding_mode(font, tables),
             {:ok, glyph_count, location_format} <- font_header(head, maxp),
             {:ok, offset_list} <- glyph_offsets(loca, glyf, glyph_count, location_format),
             :ok <- validate_glyph_ids(glyph_ids, glyph_count) do
          case embedding_mode do
            :keep_full ->
              {:ok, :keep_full}

            :subset ->
              offsets = List.to_tuple(offset_list)

              with {:ok, retained} <-
                     retain_dependencies(
                       Enum.map([0 | glyph_ids], &{:enter, &1}),
                       %{},
                       %{},
                       0,
                       offsets,
                       glyf
                     ) do
                {:ok,
                 %{
                   scaler: scaler,
                   tables: tables,
                   glyf: glyf,
                   offsets: offsets,
                   retained: MapSet.new(Map.keys(retained)),
                   glyph_count: glyph_count,
                   location_format: location_format
                 }}
              end
          end
        end

      _ ->
        invalid("font subsetting requires a TrueType font and glyph IDs")
    end
  end

  defp table_directory(data) do
    case data do
      <<scaler::32, count::16, _search::16, _selector::16, _shift::16, rest::binary>>
      when scaler in [0x0001_0000, 0x7472_7565] and count > 0 and
             byte_size(rest) >= count * 16 ->
        directory_bytes = 12 + count * 16

        records =
          for <<tag::binary-size(4), _checksum::32, offset::32,
                length::32 <-
                  binary_part(rest, 0, count * 16)>> do
            {tag, offset, length}
          end

        cond do
          length(Enum.uniq_by(records, &elem(&1, 0))) != count ->
            invalid("TrueType table directory repeats a tag")

          Enum.any?(records, fn {_tag, offset, length} ->
            offset < directory_bytes or offset + length > byte_size(data)
          end) ->
            invalid("TrueType table directory points outside the font")

          true ->
            tables =
              records
              |> Enum.map(fn {tag, offset, length} ->
                {tag, binary_part(data, offset, length)}
              end)
              |> Enum.sort_by(&elem(&1, 0))

            {:ok, scaler, tables}
        end

      _ ->
        invalid("font subsetting requires a standalone TrueType sfnt")
    end
  end

  defp required_table(tables, tag) do
    case List.keyfind(tables, tag, 0) do
      {^tag, data} -> {:ok, data}
      nil -> invalid("TrueType font is missing the #{tag} table")
    end
  end

  defp embedding_mode(font, tables) do
    flags =
      case List.keyfind(tables, "OS/2", 0) do
        {"OS/2", data} when byte_size(data) >= 10 ->
          <<_::binary-size(8), flags::16, _::binary>> = data
          flags

        {"OS/2", _data} ->
          :malformed

        nil ->
          0
      end

    cond do
      flags == :malformed ->
        invalid("TrueType OS/2 embedding flags are truncated")

      true ->
        with :ok <-
               HtmlValidator.validate_font_embedding(
                 Map.get(font, :family, "embedded font"),
                 flags,
                 List.keymember?(tables, "fvar", 0)
               ) do
          if (flags &&& 0x0100) != 0, do: {:ok, :keep_full}, else: {:ok, :subset}
        end
    end
  end

  defp font_header(head, maxp) do
    case {head, maxp} do
      {<<_::binary-size(50), format::signed-16, _::binary>>,
       <<_::binary-size(4), count::16, _::binary>>}
      when format in [0, 1] and count > 0 and byte_size(head) >= 54 ->
        {:ok, count, format}

      _ ->
        invalid("TrueType head or maxp table is malformed")
    end
  end

  defp glyph_offsets(loca, glyf, glyph_count, format) do
    bytes_per_offset = if format == 0, do: 2, else: 4
    required_bytes = (glyph_count + 1) * bytes_per_offset

    case byte_size(loca) >= required_bytes do
      true ->
        offsets =
          case format do
            0 ->
              for <<offset::16 <- binary_part(loca, 0, required_bytes)>>, do: offset * 2

            1 ->
              for <<offset::32 <- binary_part(loca, 0, required_bytes)>>, do: offset
          end

        valid? =
          Enum.all?(Enum.chunk_every(offsets, 2, 1, :discard), fn [left, right] ->
            left <= right and right <= byte_size(glyf)
          end)

        if hd(offsets) == 0 and valid?,
          do: {:ok, offsets},
          else: invalid("TrueType glyph offsets are invalid")

      false ->
        invalid("TrueType loca table is truncated")
    end
  end

  defp validate_glyph_ids(glyph_ids, glyph_count) do
    if Enum.all?(glyph_ids, &(is_integer(&1) and &1 >= 0 and &1 < glyph_count)) do
      :ok
    else
      invalid("font subset contains an out-of-range glyph ID")
    end
  end

  @spec retain_dependencies(list(), map(), map(), non_neg_integer(), tuple(), binary()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  defp retain_dependencies(queue, retained, active, work, offsets, glyf) do
    case queue do
      [] ->
        {:ok, retained}

      [{:exit, glyph_id} | rest] ->
        retain_dependencies(
          rest,
          Map.put(retained, glyph_id, true),
          Map.delete(active, glyph_id),
          work,
          offsets,
          glyf
        )

      [{:enter, glyph_id} | rest] ->
        cond do
          Map.has_key?(active, glyph_id) ->
            invalid("TrueType composite glyph dependencies contain a cycle")

          Map.has_key?(retained, glyph_id) ->
            retain_dependencies(rest, retained, active, work, offsets, glyf)

          work >= Limits.get(:max_font_subset_work) ->
            Diagnostics.error(
              :limits,
              :resource_limit_exceeded,
              "font subsetting exceeds max_font_subset_work"
            )

          true ->
            start = elem(offsets, glyph_id)
            finish = elem(offsets, glyph_id + 1)
            glyph = binary_part(glyf, start, finish - start)

            with {:ok, dependencies, component_work} <- component_glyphs(glyph) do
              cond do
                work + 1 + component_work > Limits.get(:max_font_subset_work) ->
                  Diagnostics.error(
                    :limits,
                    :resource_limit_exceeded,
                    "font subsetting exceeds max_font_subset_work"
                  )

                Enum.any?(dependencies, &(&1 >= tuple_size(offsets) - 1)) ->
                  invalid("composite glyph references an out-of-range glyph")

                true ->
                  retain_dependencies(
                    Enum.map(dependencies, &{:enter, &1}) ++ [{:exit, glyph_id} | rest],
                    retained,
                    Map.put(active, glyph_id, true),
                    work + 1 + component_work,
                    offsets,
                    glyf
                  )
              end
            end
        end
    end
  end

  defp component_glyphs(glyph) do
    case glyph do
      <<>> ->
        {:ok, [], 0}

      <<contours::signed-16, _bbox::binary-size(8), rest::binary>> ->
        if contours < 0, do: component_records(rest, [], 0, false), else: {:ok, [], 0}

      _ ->
        invalid("TrueType glyph data is truncated")
    end
  end

  defp component_records(data, glyphs, work, instructions?) do
    case data do
      <<flags::16, glyph_id::16, rest::binary>> ->
        argument_bytes = if (flags &&& 0x0001) != 0, do: 4, else: 2

        transform_bytes =
          cond do
            (flags &&& 0x0008) != 0 -> 2
            (flags &&& 0x0040) != 0 -> 4
            (flags &&& 0x0080) != 0 -> 8
            true -> 0
          end

        skipped = argument_bytes + transform_bytes
        scale_flags = Enum.count([0x0008, 0x0040, 0x0080], &((flags &&& &1) != 0))

        cond do
          scale_flags > 1 ->
            invalid("TrueType composite glyph has conflicting scale flags")

          work >= Limits.get(:max_font_subset_work) ->
            Diagnostics.error(
              :limits,
              :resource_limit_exceeded,
              "font subsetting exceeds max_font_subset_work"
            )

          byte_size(rest) < skipped ->
            invalid("TrueType composite glyph is truncated")

          (flags &&& 0x0020) != 0 ->
            component_records(
              binary_part(rest, skipped, byte_size(rest) - skipped),
              [glyph_id | glyphs],
              work + 1,
              instructions? or (flags &&& 0x0100) != 0
            )

          true ->
            remaining = binary_part(rest, skipped, byte_size(rest) - skipped)

            if instructions? or (flags &&& 0x0100) != 0 do
              case remaining do
                <<instruction_bytes::16, _instructions::binary-size(instruction_bytes),
                  _padding::binary>> ->
                  {:ok, Enum.reverse([glyph_id | glyphs]), work + 1}

                _ ->
                  invalid("TrueType composite glyph instructions are truncated")
              end
            else
              {:ok, Enum.reverse([glyph_id | glyphs]), work + 1}
            end
        end

      _ ->
        invalid("TrueType composite glyph is truncated")
    end
  end

  defp invalid(message) do
    Diagnostics.error(:font, :invalid_document, message,
      operation: :write_pdf,
      module: __MODULE__
    )
  end
end
