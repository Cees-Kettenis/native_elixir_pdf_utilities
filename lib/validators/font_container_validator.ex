defmodule NativeElixirPdfUtilities.Validators.FontContainerValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.Font.Sfnt
  alias NativeElixirPdfUtilities.Validators.FontValidator

  @doc "Admits WOFF, WOFF2 and a selected collection face as standalone sfnt bytes."
  @spec prepare(term(), non_neg_integer()) :: {:ok, binary()} | {:error, {atom(), map()}}
  def prepare(data, face_index \\ 0) do
    FontValidator.with_budget(fn ->
      if is_binary(data), do: FontValidator.reserve_source(data)

      case {data, face_index} do
        {<<"wOFF", _::binary>>, 0} ->
          prepare_woff(data)

        {<<"wOF2", _::binary>>, index} when is_integer(index) and index >= 0 ->
          with {:ok, scaler, tables} <-
                 NativeElixirPdfUtilities.Validators.Woff2Validator.prepare(data, index) do
            admit_rebuild(scaler, tables)
          end

        {<<"ttcf", _::binary>>, index} when is_integer(index) and index >= 0 ->
          prepare_collection(data, index)

        {data, 0} when is_binary(data) ->
          {:ok, data}

        _ ->
          invalid(
            "font collection index requires a collection and a nonnegative face index",
            "font source"
          )
      end
    end)
  end

  defp prepare_woff(data) do
    case data do
      <<"wOFF", flavor::32, length::32, count::16, 0::16, total::32, _major::16, _minor::16,
        meta_offset::32, meta_length::32, meta_original::32, private_offset::32,
        private_length::32, directory::binary-size(count * 20), _::binary>>
      when length == byte_size(data) and count > 0 and count <= 4095 and
             flavor in [0x0001_0000, 0x7472_7565, 0x4F54_544F] ->
        # sfnt searchRange is uint16, so at most 4095 tables fit its format.
        FontValidator.check(:max_font_decoded_bytes, total)
        FontValidator.reserve(:max_font_container_work, count)

        records =
          for <<tag::binary-size(4), offset::32, compressed::32, original::32,
                checksum::32 <- directory>>,
              do: {tag, offset, compressed, original, checksum}

        expected =
          12 + count * 16 +
            Enum.sum(
              Enum.map(records, fn {_, _, _, size, _} -> size + rem(4 - rem(size, 4), 4) end)
            )

        tags = Enum.map(records, &elem(&1, 0))
        ranges = Enum.map(records, fn {_, offset, size, _, _} -> {offset, size} end)

        with true <-
               total == expected and tags == Enum.sort(Enum.uniq(tags)) and
                 Enum.all?(tags, fn tag ->
                   Enum.all?(:binary.bin_to_list(tag), &(&1 in 0x20..0x7E))
                 end),
             :ok <-
               validate_woff_blocks(
                 data,
                 44 + count * 20,
                 ranges,
                 {meta_offset, meta_length, meta_original},
                 {private_offset, private_length}
               ),
             {:ok, tables} <- decode_woff_tables(data, records) do
          admit_rebuild(flavor, tables)
        else
          false -> invalid("WOFF table tags or totalSfntSize are invalid", "WOFF directory")
          error -> error
        end

      _ ->
        invalid(
          "WOFF header, reserved field, length or table directory is invalid",
          "WOFF header"
        )
    end
  end

  defp validate_woff_blocks(
         data,
         directory_end,
         tables,
         {meta_offset, meta_length, meta_original},
         {private_offset, private_length}
       ) do
    metadata? =
      {meta_offset, meta_length, meta_original} == {0, 0, 0} or
        (meta_offset > 0 and meta_length > 0 and meta_original > 0)

    private? =
      {private_offset, private_length} == {0, 0} or (private_offset > 0 and private_length > 0)

    table_end = tables |> Enum.map(fn {offset, size} -> offset + size end) |> Enum.max()
    metadata? = metadata? and (meta_offset == 0 or meta_offset >= table_end)

    private? =
      private? and
        (private_offset == 0 or private_offset >= max(table_end, meta_offset + meta_length))

    extra = if meta_offset == 0, do: [], else: [{meta_offset, meta_length}]
    extra = extra ++ if private_offset == 0, do: [], else: [{private_offset, private_length}]
    all = Enum.sort(tables ++ extra)

    result =
      Enum.reduce_while(all, directory_end, fn {offset, size}, previous_end ->
        finish = offset + size

        cond do
          offset != previous_end + rem(4 - rem(previous_end, 4), 4) or rem(offset, 4) != 0 or
              finish > byte_size(data) ->
            {:halt, :error}

          offset > previous_end and
              binary_part(data, previous_end, offset - previous_end) !=
                :binary.copy(<<0>>, offset - previous_end) ->
            {:halt, :error}

          true ->
            {:cont, finish}
        end
      end)

    if metadata? and private? and result != :error and byte_size(data) - result < 4 and
         binary_part(data, result, byte_size(data) - result) ==
           :binary.copy(<<0>>, byte_size(data) - result),
       do: :ok,
       else:
         invalid(
           "WOFF blocks overlap, exceed the source or have invalid alignment or padding",
           "WOFF directory"
         )
  end

  defp decode_woff_tables(data, records) do
    Enum.reduce_while(records, {:ok, []}, fn {tag, offset, compressed, original, checksum},
                                             {:ok, tables} ->
      FontValidator.reserve(:max_font_container_work, original + compressed)
      bytes = binary_part(data, offset, compressed)

      decoded =
        cond do
          compressed == original -> {:ok, bytes}
          compressed < original -> inflate(bytes, original, tag)
          true -> invalid("WOFF compressed table is longer than its original length", tag)
        end

      with {:ok, table} <- decoded,
           {:ok, table} <- checksum_table(tag, table),
           true <- Sfnt.checksum(table) == checksum do
        {:cont, {:ok, [{tag, table} | tables]}}
      else
        false -> {:halt, invalid("WOFF table checksum does not match its directory", tag)}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, tables} -> {:ok, Enum.reverse(tables)}
      error -> error
    end
  end

  defp inflate(bytes, expected, tag) do
    zlib = :zlib.open()

    try do
      :ok = :zlib.inflateInit(zlib)
      inflate_chunks(zlib, bytes, expected, 0, [], tag)
    rescue
      ErlangError -> invalid("WOFF compressed table is invalid or truncated", tag)
    after
      :zlib.close(zlib)
    end
  end

  defp inflate_chunks(zlib, bytes, expected, size, chunks, tag) do
    {status, output} = :zlib.safeInflate(zlib, bytes)
    size = size + IO.iodata_length(output)

    cond do
      size > expected ->
        invalid("WOFF decompression exceeds its declared table length", tag)

      status == :finished and size == expected ->
        :ok = :zlib.inflateEnd(zlib)
        {:ok, IO.iodata_to_binary(Enum.reverse([output | chunks]))}

      status == :finished ->
        invalid("WOFF decompression is shorter than its declared table length", tag)

      true ->
        inflate_chunks(zlib, <<>>, expected, size, [output | chunks], tag)
    end
  end

  defp prepare_collection(data, index) do
    case data do
      <<"ttcf", version::32, count::32, rest::binary>>
      when version in [0x0001_0000, 0x0002_0000] and count > 0 and byte_size(rest) >= count * 4 ->
        FontValidator.check(:max_font_candidates, count)
        FontValidator.reserve(:max_font_container_work, count)
        header_end = 12 + count * 4 + if(version == 0x0002_0000, do: 12, else: 0)

        with true <- index < count and header_end <= byte_size(data),
             <<offset::32>> = binary_part(rest, index * 4, 4),
             true <-
               rem(offset, 4) == 0 and offset >= header_end and offset + 12 <= byte_size(data),
             {:ok, directories} <- collection_directories(data, rest, count, header_end, version),
             {:ok, scaler, tables} <- collection_tables(data, offset, header_end, directories) do
          admit_rebuild(scaler, tables)
        else
          false ->
            invalid(
              "collection face index or directory offset is outside the source",
              "TTC directory"
            )

          error ->
            error
        end

      _ ->
        invalid("collection header or face-offset array is invalid", "TTC header")
    end
  end

  defp collection_directories(data, offsets, count, header_end, version) do
    directories = for <<offset::32 <- binary_part(offsets, 0, count * 4)>>, do: offset

    signatures =
      case version do
        0x0001_0000 ->
          {:ok, []}

        _ ->
          case binary_part(data, header_end - 12, 12) do
            <<0::96>> ->
              {:ok, []}

            <<0x4453_4947::32, length::32, offset::32>>
            when length > 0 and offset >= header_end and offset + length <= byte_size(data) ->
              {:ok, [{offset, offset + length}]}

            _ ->
              invalid("collection signature directory is invalid", "TTC DSIG")
          end
      end

    with {:ok, signatures} <- signatures do
      Enum.reduce_while(Enum.uniq(directories), {:ok, signatures}, fn offset, {:ok, ranges} ->
        if offset >= header_end and rem(offset, 4) == 0 and offset + 12 <= byte_size(data) do
          <<scaler::32, tables::16, _::48>> = binary_part(data, offset, 12)
          finish = offset + 12 + tables * 16

          cond do
            scaler not in [0x0001_0000, 0x7472_7565, 0x4F54_544F] or tables == 0 or tables > 4095 or
                finish > byte_size(data) ->
              {:halt, invalid("collection has an invalid face directory", "TTC directory")}

            Enum.any?(ranges, fn {first, last} -> offset < last and finish > first end) ->
              {:halt,
               invalid("collection face directories or signature overlap", "TTC directory")}

            true ->
              {:cont, {:ok, [{offset, finish} | ranges]}}
          end
        else
          {:halt, invalid("collection face directory offset is invalid", "TTC directory")}
        end
      end)
    end
  end

  defp collection_tables(data, offset, header_end, directories) do
    <<scaler::32, count::16, _::48>> = binary_part(data, offset, 12)

    FontValidator.reserve(:max_font_container_work, count)
    records = binary_part(data, offset + 12, count * 16)

    parsed =
      for <<tag::binary-size(4), _checksum::32, start::32, length::32 <- records>>,
        do: {tag, start, length}

    tags = Enum.map(parsed, &elem(&1, 0))

    with true <- length(tags) == length(Enum.uniq(tags)),
         {:ok, tables, _} <-
           Enum.reduce_while(Enum.sort_by(parsed, &elem(&1, 1)), {:ok, [], header_end}, fn {tag,
                                                                                            start,
                                                                                            length},
                                                                                           {:ok,
                                                                                            tables,
                                                                                            previous} ->
             cond do
               start < previous or start + length > byte_size(data) or
                   Enum.any?(directories, fn {first, last} ->
                     start < last and start + length > first
                   end) ->
                 {:halt,
                  invalid(
                    "collection face tables overlap a directory, each other or exceed the source",
                    tag
                  )}

               true ->
                 FontValidator.reserve(:max_font_container_work, length)

                 {:cont,
                  {:ok, [{tag, binary_part(data, start, length)} | tables], start + length}}
             end
           end) do
      {:ok, scaler, Enum.sort(tables)}
    else
      false -> invalid("collection face repeats a table tag", "TTC directory")
      error -> error
    end
  end

  defp admit_rebuild(scaler, tables) do
    # Repacking changes sfnt offsets and invalidates a source digital signature.
    tables = Enum.reject(tables, &(elem(&1, 0) == "DSIG"))

    bytes =
      12 + length(tables) * 16 +
        Enum.sum(
          Enum.map(tables, fn {_, data} ->
            byte_size(data) + rem(4 - rem(byte_size(data), 4), 4)
          end)
        )

    FontValidator.check(:max_font_decoded_bytes, bytes)
    FontValidator.reserve(:max_font_container_work, bytes)

    case List.keyfind(tables, "head", 0) do
      {"head", head} when byte_size(head) >= 12 ->
        tables =
          Enum.map(tables, fn {tag, data} ->
            {:ok, data} = checksum_table(tag, data)
            {tag, data}
          end)

        {:ok, Sfnt.rebuild(scaler, tables)}

      _ ->
        invalid("reconstructed font requires a complete head checksum field", "head")
    end
  end

  defp checksum_table(tag, data) do
    case {tag, data} do
      {"head", <<prefix::binary-size(8), _::32, rest::binary>>} ->
        {:ok, <<prefix::binary, 0::32, rest::binary>>}

      {"head", _} ->
        invalid("WOFF head table is truncated", "head")

      _ ->
        {:ok, data}
    end
  end

  defp invalid(message, source) do
    Diagnostics.error(:font, :invalid_font, message,
      module: __MODULE__,
      operation: :prepare_font_source,
      source: source
    )
  end
end
