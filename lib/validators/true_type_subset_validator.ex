defmodule NativeElixirPdfUtilities.Validators.TrueTypeSubsetValidator do
  @moduledoc false

  import Bitwise

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.FontValidator

  @type plan :: %{
          scaler: non_neg_integer(),
          tables: [{binary(), binary()}],
          glyf: binary(),
          offsets: tuple(),
          retained: MapSet.t(),
          glyph_count: pos_integer(),
          location_format: 0 | 1,
          output_bytes: pos_integer()
        }

  @doc false
  @spec prepare(map(), [non_neg_integer()]) ::
          {:ok, plan() | :keep_full} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare(font, glyph_ids) do
    FontValidator.with_budget(fn ->
      case {font, glyph_ids} do
        {%{data: data} = font, glyph_ids} when is_binary(data) and is_list(glyph_ids) ->
          FontValidator.reserve_source(data)

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
                  retained = MapSet.new(Map.keys(retained))

                  with {:ok, output_bytes} <-
                         admit_rebuild(tables, offsets, retained, location_format, glyph_count) do
                    {:ok,
                     %{
                       scaler: scaler,
                       tables: tables,
                       glyf: glyf,
                       offsets: offsets,
                       retained: retained,
                       output_bytes: output_bytes,
                       glyph_count: glyph_count,
                       location_format: location_format
                     }}
                  end
                end
            end
          end

        _ ->
          invalid("font subsetting requires a TrueType font and glyph IDs")
      end
    end)
  end

  @doc false
  @spec prepare_glyph_bounds(term()) ::
          {:ok, tuple()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_glyph_bounds(data) do
    FontValidator.with_budget(fn ->
      case data do
        data when is_binary(data) ->
          with {:ok, _scaler, tables} <- table_directory(data),
               {:ok, head} <- required_table(tables, "head"),
               {:ok, maxp} <- required_table(tables, "maxp"),
               {:ok, glyf} <- required_table(tables, "glyf"),
               {:ok, loca} <- required_table(tables, "loca"),
               {:ok, glyph_count, location_format} <- font_header(head, maxp) do
            offset_bytes = (glyph_count + 1) * if(location_format == 0, do: 2, else: 4)
            # Decode one location and ten header bytes per glyph, plus its result entry.
            FontValidator.reserve(:max_font_math_work, offset_bytes + glyph_count * 11)

            with {:ok, offsets} <- glyph_offsets(loca, glyf, glyph_count, location_format) do
              offsets
              |> Enum.chunk_every(2, 1, :discard)
              |> Enum.reduce_while({:ok, []}, fn [start, finish], {:ok, bounds} ->
                case binary_part(glyf, start, finish - start) do
                  <<>> ->
                    {:cont, {:ok, [{0, 0, 0, 0} | bounds]}}

                  <<_contours::signed-16, xmin::signed-16, ymin::signed-16, xmax::signed-16,
                    ymax::signed-16, _rest::binary>>
                  when xmin <= xmax and ymin <= ymax ->
                    {:cont, {:ok, [{xmin, ymin, xmax, ymax} | bounds]}}

                  _ ->
                    {:halt, invalid("TrueType glyph has malformed ink bounds")}
                end
              end)
              |> case do
                {:ok, bounds} -> {:ok, bounds |> Enum.reverse() |> List.to_tuple()}
                error -> error
              end
            end
          end

        _ ->
          invalid("glyph metrics require a standalone TrueType font")
      end
    end)
  end

  # Account for every retained payload occurrence, not unique source ranges.
  # This runs before glyph copying, padding, checksumming and font assembly.
  defp admit_rebuild(tables, offsets, retained, location_format, glyph_count) do
    glyf_bytes =
      Enum.reduce(retained, 0, fn glyph, total ->
        bytes = elem(offsets, glyph + 1) - elem(offsets, glyph)
        total + bytes + rem(bytes, 2)
      end)

    loca_bytes = (glyph_count + 1) * if(location_format == 0, do: 2, else: 4)
    retained_tables = Enum.reject(tables, &(elem(&1, 0) == "DSIG"))
    count = length(retained_tables)

    output_bytes =
      Enum.reduce(retained_tables, 12 + count * 16, fn {tag, data}, total ->
        bytes =
          case tag do
            "glyf" -> glyf_bytes
            "loca" -> loca_bytes
            _ -> byte_size(data)
          end

        total + bytes + rem(4 - rem(bytes, 4), 4)
      end)

    # Fixed sfnt field capacities, not configurable resource ceilings: offsets
    # are 32-bit and the power-of-two searchRange is a 16-bit byte count.
    if output_bytes <= 0xFFFF_FFFF and count < 4096 do
      FontValidator.reserve(:max_font_subset_bytes, output_bytes)
      FontValidator.reserve(:max_font_subset_checksum_work, div(output_bytes, 4))
      {:ok, output_bytes}
    else
      invalid("TrueType subset exceeds sfnt directory field capacity")
    end
  catch
    {:font_resource_limit, {:error, {reason, diagnostic}}} ->
      Diagnostics.error(:font, reason, "font subsetting: #{diagnostic.message}",
        operation: :write_pdf,
        module: __MODULE__,
        source: "TrueType subset"
      )
  end

  defp table_directory(data) do
    with {:ok, scaler, records} <- FontValidator.prepare_table_directory(data) do
      tables =
        records
        |> Enum.map(fn {tag, {offset, length}} ->
          {tag, binary_part(data, offset, length)}
        end)
        |> Enum.sort_by(&elem(&1, 0))

      {:ok, scaler, tables}
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
