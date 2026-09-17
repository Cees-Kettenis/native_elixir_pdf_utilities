defmodule NativeElixirPdfUtilities.Validators.FontValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Limits

  @doc false
  @spec with_budget((-> result)) :: result | {:error, {atom(), Diagnostics.diagnostic()}}
        when result: term()
  def with_budget(fun) do
    key = {__MODULE__, :budget}

    case Process.get(key) do
      nil ->
        Process.put(key, %{sources: MapSet.new(), faces: MapSet.new(), memo: %{}})

        try do
          fun.()
        catch
          {:font_resource_limit, error} -> error
        after
          Process.delete(key)
        end

      _ ->
        fun.()
    end
  end

  @doc false
  @spec reserve(Limits.key(), non_neg_integer()) :: :ok
  def reserve(limit, amount) do
    key = {__MODULE__, :budget}
    budget = Process.get(key)
    used = Map.get(budget, limit, 0) + amount
    check(limit, used)
    Process.put(key, Map.put(budget, limit, used))
    :ok
  end

  @doc false
  @spec check(Limits.key(), non_neg_integer()) :: :ok
  def check(limit, amount) do
    case amount <= Limits.get(limit) do
      true ->
        :ok

      false ->
        throw(
          {:font_resource_limit,
           Diagnostics.error(
             :font,
             :resource_limit_exceeded,
             "font loading exceeds #{limit} (configured limit #{Limits.get(limit)})",
             operation: :load_registry,
             module: __MODULE__
           )}
        )
    end
  end

  @doc false
  @spec read_limit() :: pos_integer()
  def read_limit do
    budget = Process.get({__MODULE__, :budget})
    used = Map.get(budget, :max_aggregate_font_source_bytes, 0)
    check(:max_aggregate_font_source_bytes, used + 1)
    min(Limits.get(:max_font_source_bytes), Limits.get(:max_aggregate_font_source_bytes) - used)
  end

  @doc false
  @spec discovery_result(map() | nil | {:error, {atom(), map()}}) :: map() | nil
  def discovery_result(result) do
    case result do
      %{data: data} = font ->
        reserve_source(data)
        reserve_face(font)
        font

      {:error, {_reason, _diagnostic}} = error ->
        throw({:font_resource_limit, error})

      nil ->
        nil
    end
  end

  @doc false
  @spec reserve_face(map()) :: :ok
  def reserve_face(font) do
    key = {__MODULE__, :budget}
    budget = Process.get(key)
    identity = {font.family, font.weight, font.style}
    faces = MapSet.put(budget.faces, identity)
    check(:max_font_count, MapSet.size(faces))
    Process.put(key, %{budget | faces: faces})
    :ok
  end

  @doc false
  @spec reserve_source(binary()) :: :ok
  def reserve_source(data) do
    check(:max_font_source_bytes, byte_size(data))
    identity = :crypto.hash(:sha256, data)
    key = {__MODULE__, :budget}
    budget = Process.get(key)

    case MapSet.member?(budget.sources, identity) do
      true ->
        :ok

      false ->
        reserve(:max_aggregate_font_source_bytes, byte_size(data))
        Process.put(key, %{Process.get(key) | sources: MapSet.put(budget.sources, identity)})
        :ok
    end
  end

  @doc false
  @spec memo(term(), (-> result)) :: result when result: term()
  def memo(identity, loader) do
    key = {__MODULE__, :budget}
    budget = Process.get(key)

    case Map.fetch(budget.memo, identity) do
      {:ok, result} ->
        result

      :error ->
        result = loader.()
        budget = Process.get(key)
        Process.put(key, %{budget | memo: Map.put(budget.memo, identity, result)})
        result
    end
  end

  @doc false
  @spec source_result(term()) :: term()
  def source_result(result) do
    case result do
      {:error, {:resource_limit_exceeded, diagnostic}} ->
        diagnostic = %{
          diagnostic
          | stage: :font,
            message:
              "font source exceeds max_font_source_bytes or remaining max_aggregate_font_source_bytes: " <>
                diagnostic.message
        }

        throw({:font_resource_limit, {:error, {:resource_limit_exceeded, diagnostic}}})

      result ->
        result
    end
  end

  @doc false
  @spec cacheable_result?(term()) :: boolean()
  def cacheable_result?(result) do
    case result do
      {:error, {:resource_limit_exceeded, _diagnostic}} -> false
      _ -> true
    end
  end

  @doc false
  @spec cache_bytes(term()) :: non_neg_integer()
  def cache_bytes(term) do
    :erts_debug.flat_size(term) * :erlang.system_info(:wordsize) + :erlang.external_size(term)
  end

  @doc false
  @spec cache_evictions([tuple()], pos_integer(), Limits.key()) :: [term()]
  def cache_evictions(entries, maximum_entries, byte_limit) do
    # Both cache formats put the insertion sequence last and their key first.
    {_retained, _bytes, evicted} =
      entries
      |> Enum.sort_by(&elem(&1, tuple_size(&1) - 1), :desc)
      |> Enum.reduce({0, 0, []}, fn entry, {count, bytes, evicted} ->
        size = cache_bytes(entry)

        case count < maximum_entries and bytes + size <= Limits.get(byte_limit) do
          true -> {count + 1, bytes + size, evicted}
          false -> {count, bytes, [elem(entry, 0) | evicted]}
        end
      end)

    evicted
  end

  @type segment :: {non_neg_integer(), non_neg_integer(), integer(), binary() | nil}

  @doc false
  @spec prepare_cmap(term()) ::
          {:ok, [[segment()]]}
          | :error
          | {:error, {:resource_limit_exceeded, Diagnostics.diagnostic()}}
  def prepare_cmap(cmap) do
    limit = Limits.get(:max_font_cmap_work)

    case cmap do
      <<_version::16, count::16, records::binary-size(count * 8), _rest::binary>>
      when count > 0 and count <= limit ->
        offsets =
          for <<platform::16, encoding::16, offset::32 <- records>> do
            priority =
              cond do
                platform == 3 and encoding == 10 -> 0
                platform == 3 and encoding == 1 -> 1
                platform == 0 -> 2
                true -> 3
              end

            {priority, offset}
          end
          |> Enum.sort_by(&elem(&1, 0))
          |> Enum.map(&elem(&1, 1))
          |> Enum.uniq()

        offsets
        |> Enum.reduce_while({:ok, [], count}, fn offset, {:ok, subtables, work} ->
          case prepare_subtable(cmap, offset, work, limit) do
            {:ok, segments, work} -> {:cont, {:ok, [segments | subtables], work}}
            {:skip, work} -> {:cont, {:ok, subtables, work}}
            :limit -> {:halt, limit_error()}
          end
        end)
        |> case do
          {:ok, subtables, _work} -> {:ok, Enum.reverse(subtables)}
          error -> error
        end

      <<_version::16, count::16, _rest::binary>> when count > limit ->
        limit_error()

      _ ->
        :error
    end
  end

  @doc false
  @spec prepare_kerning(binary(), pos_integer()) ::
          {:ok, map()} | :error | {:error, {:resource_limit_exceeded, Diagnostics.diagnostic()}}
  def prepare_kerning(data, glyph_count) do
    limit = Limits.get(:max_font_kerning_pairs)

    case data do
      <<0::16, count::16, subtables::binary>> when count <= limit ->
        Enum.reduce_while(List.duplicate(nil, count), {:ok, %{}, subtables, 0}, fn _,
                                                                                   {:ok, pairs,
                                                                                    remaining,
                                                                                    work} ->
          case {work < limit, remaining} do
            {true,
             <<0::16, length::16, format::8, _reserved::4, override::1, cross_stream::1,
               minimum::1, horizontal::1, rest::binary>>}
            when length >= 6 and length - 6 <= byte_size(rest) ->
              <<body::binary-size(^length - 6), rest::binary>> = rest

              case {format, horizontal, minimum, cross_stream, body} do
                {0, 1, 0, 0, <<pair_count::16, _search::binary-size(6), records::binary>>}
                when byte_size(records) == pair_count * 6 ->
                  case work + pair_count + 1 <= limit do
                    true ->
                      parsed =
                        for <<left::16, right::16, value::signed-16 <- records>>,
                          do: {{left, right}, value}

                      prepared =
                        Enum.reduce_while(parsed, {:ok, pairs, nil}, fn
                          {{left, right} = key, value}, {:ok, acc, previous} ->
                            case left < glyph_count and right < glyph_count and
                                   (is_nil(previous) or previous < key) do
                              true ->
                                acc =
                                  if override == 1,
                                    do: Map.put(acc, key, value),
                                    else: Map.update(acc, key, value, &(&1 + value))

                                {:cont, {:ok, acc, key}}

                              false ->
                                {:halt, :error}
                            end
                        end)

                      case prepared do
                        {:ok, pairs, _} ->
                          {:cont, {:ok, pairs, rest, work + pair_count + 1}}

                        :error ->
                          {:halt, :error}
                      end

                    false ->
                      {:halt, kerning_limit_error()}
                  end

                {0, 1, 0, 0, _} ->
                  {:halt, :error}

                _ ->
                  {:cont, {:ok, pairs, rest, work + 1}}
              end

            {false, _} ->
              {:halt, kerning_limit_error()}

            _ ->
              {:halt, :error}
          end
        end)
        |> case do
          {:ok, pairs, <<>>, _} -> {:ok, pairs}
          {:ok, _, _, _} -> :error
          failure -> failure
        end

      <<0::16, count::16, _::binary>> when count > limit ->
        kerning_limit_error()

      # Apple and other kerning formats are not interpreted.
      _ ->
        {:ok, %{}}
    end
  end

  defp kerning_limit_error do
    Diagnostics.error(
      :limits,
      :resource_limit_exceeded,
      "font kerning-pair work exceeds the limit",
      operation: :load_registry,
      module: NativeElixirPdfUtilities.HtmlToPdf.Font,
      source: "kern"
    )
  end

  defp limit_error do
    Diagnostics.error(
      :limits,
      :resource_limit_exceeded,
      "font character-map work exceeds the limit",
      operation: :load_registry,
      module: NativeElixirPdfUtilities.HtmlToPdf.Font
    )
  end

  defp prepare_subtable(cmap, offset, work, limit) do
    case cmap do
      <<_prefix::binary-size(^offset), 4::16, length::16, _language::16, count_x2::16,
        _rest::binary>>
      when count_x2 > 0 and rem(count_x2, 2) == 0 and
             length >= 16 + 4 * count_x2 and offset + length <= byte_size(cmap) ->
        count = div(count_x2, 2)
        work = work + count

        case work <= limit do
          true ->
            subtable = binary_part(cmap, offset, length)

            <<_header::binary-size(14), ends::binary-size(^count_x2), _reserved::16,
              starts::binary-size(^count_x2), deltas::binary-size(^count_x2),
              ranges::binary-size(^count_x2), _glyphs::binary>> = subtable

            segments =
              Enum.zip([
                for(<<first::16 <- starts>>, do: first),
                for(<<last::16 <- ends>>, do: last),
                for(<<delta::signed-16 <- deltas>>, do: delta),
                for(<<range::16 <- ranges>>, do: range)
              ])

            prepare_segments(segments, subtable, 16 + 6 * count, work, limit)

          false ->
            :limit
        end

      _ ->
        {:skip, work}
    end
  end

  defp prepare_segments(segments, subtable, ranges_offset, work, limit) do
    glyphs_offset = ranges_offset + 2 * length(segments)

    segments
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, [], -1, work}, fn
      {{first, last, delta, range}, index}, {:ok, prepared, previous_end, work} ->
        # Format 4 uses 16-bit character codes; 0xFFFF is its fixed sentinel.
        work = work + max(min(last, 0xFFFE) - first + 1, 0)
        glyph_offset = ranges_offset + 2 * index + range
        glyph_bytes = 2 * (last - first + 1)

        cond do
          work > limit ->
            {:halt, :limit}

          first > last or first <= previous_end ->
            {:halt, {:skip, work}}

          range != 0 and
              (rem(range, 2) != 0 or glyph_offset < glyphs_offset or
                 glyph_offset + glyph_bytes > byte_size(subtable)) ->
            {:halt, {:skip, work}}

          true ->
            glyphs =
              if range == 0, do: nil, else: binary_part(subtable, glyph_offset, glyph_bytes)

            {:cont, {:ok, [{first, last, delta, glyphs} | prepared], last, work}}
        end
    end)
    |> case do
      {:ok, prepared, _last, work} -> {:ok, Enum.reverse(prepared), work}
      failure -> failure
    end
  end
end
