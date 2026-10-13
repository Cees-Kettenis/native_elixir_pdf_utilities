defmodule NativeElixirPdfUtilities.Validators.FontValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.MathFontValidator

  @doc false
  @spec prepare_x_height(term()) :: {:ok, number()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_x_height(font) do
    with_budget(fn ->
      case font do
        %{type: :embedded, cmap: cmap, units_per_em: units}
        when is_map(cmap) and is_integer(units) and units > 0 ->
          with {:ok, bounds} <- prepare_glyph_bounds(font) do
            glyph = Map.get(cmap, ?x, 0)

            if is_integer(glyph) and glyph > 0 and glyph < tuple_size(bounds) do
              {:ok, elem(elem(bounds, glyph), 3) / units}
            else
              Diagnostics.error(
                :font,
                :unsupported_glyph,
                "selected font has no usable x-height glyph for ex units",
                operation: :measure_math,
                module: __MODULE__,
                source: "x"
              )
            end
          end

        _ ->
          MathFontValidator.failure(
            "ex units need a selected embedded font and usable units",
            "font metrics"
          )
      end
    end)
  end

  @doc false
  @spec prepare_math_kerning(map(), [{non_neg_integer(), non_neg_integer()}], String.t()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_math_kerning(font, pairs, script) do
    with_budget(fn ->
      case font do
        %{type: :embedded, id: id, data: data, widths: widths}
        when is_binary(data) and is_list(widths) ->
          with :ok <- MathFontValidator.validate_pair_request(length(widths), pairs, script) do
            reserve(:max_font_math_work, length(pairs))

            fingerprint = :crypto.hash(:sha256, data)
            reserve_source_fingerprint(fingerprint, byte_size(data))

            result =
              memo({:math_kerning, id, fingerprint, pairs, script}, fn ->
                reserve_source(data)

                result =
                  with {:ok, tables} <- table_directory(data) do
                    case Map.fetch(tables, "GPOS") do
                      :error ->
                        {:ok, Map.take(Map.get(font, :kerning, %{}), pairs)}

                      {:ok, _} ->
                        with {:ok, table} <- table(data, tables, "GPOS"),
                             do:
                               MathFontValidator.prepare_pair_adjustments(
                                 table,
                                 length(widths),
                                 pairs,
                                 script
                               )
                    end
                  end

                result
              end)

            case result do
              :error ->
                MathFontValidator.failure("invalid table directory or GPOS bounds", "GPOS")

              result ->
                result
            end
          end

        _ ->
          MathFontValidator.failure("pair adjustment requires a selected embedded font", "GPOS")
      end
    end)
  end

  @doc false
  @spec with_budget((-> result)) :: result | {:error, {atom(), Diagnostics.diagnostic()}}
        when result: term()
  def with_budget(fun) do
    key = {__MODULE__, :budget}

    case Process.get(key) do
      nil ->
        Process.put(key, %{
          budget_id: make_ref(),
          sources: MapSet.new(),
          faces: MapSet.new(),
          memo: %{}
        })

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
    reserve_source_fingerprint(:crypto.hash(:sha256, data), byte_size(data))
  end

  @doc false
  @spec reserve_source_fingerprint(binary(), non_neg_integer()) :: :ok
  def reserve_source_fingerprint(identity, source_bytes) do
    check(:max_font_source_bytes, source_bytes)
    key = {__MODULE__, :budget}
    budget = Process.get(key)

    case MapSet.member?(budget.sources, identity) do
      true ->
        :ok

      false ->
        reserve(:max_aggregate_font_source_bytes, source_bytes)
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

  @type segment ::
          {non_neg_integer(), non_neg_integer(), integer(),
           binary() | nil | {:sequential, non_neg_integer()}}

  @doc false
  @spec prepare_cmap(term()) ::
          {:ok, [[segment()]]}
          | :error
          | {:error, {:resource_limit_exceeded, Diagnostics.diagnostic()}}
  def prepare_cmap(cmap) do
    # Glyph IDs are unsigned 16-bit values. This is a fixed OpenType bound.
    prepare_cmap(cmap, 65_536)
  end

  @doc false
  @spec prepare_cmap(term(), term()) ::
          {:ok, [[segment()]]}
          | :error
          | {:error, {:resource_limit_exceeded, Diagnostics.diagnostic()}}
  def prepare_cmap(cmap, glyph_count) do
    limit = Limits.get(:max_font_cmap_work)

    case is_integer(glyph_count) and glyph_count > 0 and glyph_count <= 65_536 do
      false ->
        :error

      true ->
        case cmap do
          <<_version::16, count::16, records::binary-size(count * 8), _rest::binary>>
          when count > 0 and count <= limit ->
            offsets =
              for <<platform::16, encoding::16, offset::32 <- records>> do
                priority =
                  cond do
                    platform == 3 and encoding == 10 -> 0
                    platform == 0 and encoding in [4, 6] -> 0
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
              case prepare_subtable(cmap, offset, work, limit, glyph_count) do
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

  defp prepare_subtable(cmap, offset, work, limit, glyph_count) do
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

      <<_prefix::binary-size(^offset), 12::16, 0::16, length::32, 0::32, count::32,
        _rest::binary>>
      when length == 16 + count * 12 and offset + length <= byte_size(cmap) ->
        work = work + count

        if work > limit do
          :limit
        else
          groups = binary_part(cmap, offset + 16, count * 12)

          for(<<first::32, last::32, glyph::32 <- groups>>, do: {first, last, glyph})
          |> Enum.reduce_while({:ok, [], -1, work}, fn {first, last, glyph},
                                                       {:ok, prepared, previous_end, work} ->
            work = work + max(last - first + 1, 0)

            cond do
              work > limit ->
                {:halt, :limit}

              first > last or first <= previous_end or last > 0x10FFFF or
                (first <= 0xDFFF and last >= 0xD800) or
                  glyph + last - first >= glyph_count ->
                {:halt, {:skip, work}}

              true ->
                {:cont, {:ok, [{first, last, 0, {:sequential, glyph}} | prepared], last, work}}
            end
          end)
          |> case do
            {:ok, segments, _last, work} -> {:ok, Enum.reverse(segments), work}
            result -> result
          end
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

  @doc false
  @spec prepare_text_metrics(term()) :: {:ok, map()} | :error
  def prepare_text_metrics(font) do
    case font do
      %{type: :embedded, id: id, data: data} when is_binary(data) ->
        with_budget(fn ->
          {cached_data, result} =
            memo({:text_metrics, id}, fn -> {data, parse_text_metrics(data)} end)

          if cached_data == data, do: result, else: parse_text_metrics(data)
        end)

      _ ->
        :error
    end
  end

  defp parse_text_metrics(data) do
    with {:ok, tables} <- table_directory(data),
         {:ok, head} <- table(data, tables, "head"),
         {:ok, units, _bbox} <- parse_head(head) do
      underline =
        case table(data, tables, "post") do
          {:ok, post} ->
            with {:ok, position} <- read_i16(post, 8),
                 {:ok, thickness} <- read_i16(post, 10),
                 true <- thickness >= 0 do
              %{position: position, thickness: thickness}
            else
              _ -> nil
            end

          :error ->
            nil
        end

      strike =
        case table(data, tables, "OS/2") do
          {:ok, os2} ->
            with {:ok, thickness} <- read_i16(os2, 26),
                 {:ok, position} <- read_i16(os2, 28),
                 true <- thickness >= 0 do
              %{position: position, thickness: thickness}
            else
              _ -> nil
            end

          :error ->
            nil
        end

      average_width =
        case table(data, tables, "OS/2") do
          {:ok, os2} ->
            case read_i16(os2, 2) do
              {:ok, width} when width > 0 -> width
              _ -> nil
            end

          :error ->
            nil
        end

      max_width =
        case table(data, tables, "hhea") do
          {:ok, hhea} ->
            case read_u16(hhea, 10) do
              {:ok, width} -> width
              _ -> nil
            end

          :error ->
            nil
        end

      {:ok,
       %{
         units_per_em: units,
         underline: underline,
         strike: strike,
         average_width: average_width,
         max_width: max_width
       }}
    end
  end

  @doc false
  @spec prepare_text_ink_bounds(term()) ::
          {:ok, {number(), number()} | nil} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_text_ink_bounds(box) do
    with_budget(fn ->
      case box do
        %{
          text: text,
          font_face: %{type: :embedded, units_per_em: units, cmap: cmap} = font,
          font_size: size,
          y: baseline
        }
        when is_binary(text) and is_integer(units) and units > 0 and is_map(cmap) and
               is_number(size) and size >= 0 and is_number(baseline) ->
          check(:max_rendered_text_bytes, byte_size(text))
          reserve(:max_font_math_work, byte_size(text))

          with true <- String.valid?(text),
               {:ok, bounds} <- prepare_glyph_bounds(font) do
            selected_text =
              if Map.has_key?(box, :glyph_ids),
                do: text,
                else:
                  NativeElixirPdfUtilities.HtmlToPdf.Font.shape_ligatures(
                    text,
                    font,
                    Map.get(box, :letter_spacing, 0.0)
                  )

            ids =
              Map.get(
                box,
                :glyph_ids,
                Enum.map(String.to_charlist(selected_text), &Map.get(cmap, &1, 0))
              )

            if is_list(ids), do: reserve(:max_font_math_work, length(ids))

            valid? =
              is_list(ids) and length(ids) == length(String.codepoints(selected_text)) and
                Enum.all?(ids, &(is_integer(&1) and &1 > 0 and &1 < tuple_size(bounds))) and
                size <= Limits.get(:max_css_numeric_magnitude) and
                abs(baseline) <= Limits.get(:max_pdf_numeric_magnitude)

            if valid? do
              ink = ids |> Enum.map(&elem(bounds, &1)) |> Enum.reject(&(&1 == {0, 0, 0, 0}))

              case ink do
                [] ->
                  {:ok, nil}

                _ ->
                  minimum = Enum.min(Enum.map(ink, &elem(&1, 1))) * size / units + baseline
                  maximum = Enum.max(Enum.map(ink, &elem(&1, 3))) * size / units + baseline
                  {:ok, {maximum, minimum}}
              end
            else
              Diagnostics.error(
                :font,
                :invalid_document,
                "text ink measurement requires valid selected glyph IDs and bounded point geometry",
                operation: :measure_child,
                module: __MODULE__,
                source: text
              )
            end
          else
            false ->
              Diagnostics.error(:font, :invalid_encoding, "text ink requires valid UTF-8",
                operation: :measure_child,
                module: __MODULE__
              )

            error ->
              error
          end

        _ ->
          Diagnostics.error(
            :font,
            :invalid_document,
            "text ink measurement requires an embedded font and prepared text geometry",
            operation: :measure_child,
            module: __MODULE__
          )
      end
    end)
  end

  @doc false
  @spec prepare_glyph_bounds(term()) ::
          {:ok, tuple()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_glyph_bounds(font) do
    result =
      with_budget(fn ->
        case font do
          %{type: :embedded, id: id, data: data} when is_binary(data) ->
            loader = fn ->
              reserve_source(data)

              NativeElixirPdfUtilities.Validators.TrueTypeSubsetValidator.prepare_glyph_bounds(
                data
              )
            end

            {cached_data, result} = memo({:glyph_bounds, id}, fn -> {data, loader.()} end)
            result = if cached_data == data, do: result, else: loader.()

            result

          _ ->
            Diagnostics.error(
              :font,
              :invalid_document,
              "glyph bounds require an embedded TrueType font",
              module: __MODULE__,
              operation: :glyph_bounds
            )
        end
      end)

    case result do
      {:error, {reason, diagnostic}} ->
        Diagnostics.error(diagnostic.stage, reason, diagnostic.message,
          operation: :glyph_bounds,
          module: diagnostic.module,
          source:
            if(is_map(font),
              do: Map.get(font, :family, "selected font outlines"),
              else: "selected font outlines"
            )
        )

      result ->
        result
    end
  end

  @doc false
  @spec prepare_math_metrics(term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_math_metrics(font) do
    with_budget(fn ->
      case font do
        %{type: :embedded, id: id, data: data} when is_binary(data) ->
          {cached_data, result} =
            memo({:math_metrics, id}, fn ->
              reserve_source(data)
              {data, parse_math_metrics(data)}
            end)

          if cached_data == data do
            result
          else
            reserve_source(data)
            parse_math_metrics(data)
          end

        _ ->
          MathFontValidator.failure("select an embedded TrueType font with a MATH table")
      end
    end)
  end

  defp parse_math_metrics(data) do
    with {:ok, tables} <- table_directory(data),
         {:ok, head} <- table(data, tables, "head"),
         {:ok, units, _bbox} <- parse_head(head),
         {:ok, maxp} <- table(data, tables, "maxp"),
         {:ok, glyph_count} <- read_u16(maxp, 4),
         {:ok, math} <- table(data, tables, "MATH"),
         {:ok, metrics} <- MathFontValidator.prepare(math, glyph_count),
         {:ok, features} <-
           (case table(data, tables, "GSUB") do
              {:ok, gsub} -> MathFontValidator.prepare_substitution_features(gsub, glyph_count)
              :error -> {:ok, %{substitutions: %{}, substitution_kinds: %{}}}
            end) do
      {:ok, metrics |> Map.merge(features) |> Map.put(:units_per_em, units)}
    else
      {:error, {_reason, _diagnostic}} = error ->
        error

      _ ->
        MathFontValidator.failure("selected font has missing or invalid head, maxp, or MATH data")
    end
  end

  @doc false
  @spec prepare_ttf(term()) ::
          {:ok, map()} | :error | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_ttf(data) do
    case data do
      data when is_binary(data) ->
        with {:ok, tables} <- table_directory(data),
             {:ok, head} <- table(data, tables, "head"),
             {:ok, hhea} <- table(data, tables, "hhea"),
             {:ok, maxp} <- table(data, tables, "maxp"),
             {:ok, hmtx} <- table(data, tables, "hmtx"),
             {:ok, cmap} <- table(data, tables, "cmap"),
             {:ok, units_per_em, bbox} <- parse_head(head),
             {:ok, ascent, descent, line_gap, hmetric_count} <- parse_hhea(hhea),
             {:ok, glyph_count} <- read_u16(maxp, 4),
             {:ok, widths} <- parse_hmtx(hmtx, glyph_count, hmetric_count),
             {:ok, cmap} <- parse_cmap(cmap, glyph_count),
             {:ok, kerning} <-
               prepare_kerning(
                 case table(data, tables, "kern") do
                   {:ok, kern} -> kern
                   :error -> <<>>
                 end,
                 glyph_count
               ),
             {:ok, embedding_flags} <- font_embedding_flags(data, tables) do
          {:ok,
           %{
             units_per_em: units_per_em,
             widths: widths,
             default_width: List.last(widths) || 600,
             cmap: cmap,
             kerning: kerning,
             ascent: ascent,
             descent: descent,
             line_gap: line_gap,
             bbox: bbox,
             embedding_flags: embedding_flags,
             variable_font?: Map.has_key?(tables, "fvar")
           }}
        end

      _ ->
        :error
    end
  end

  defp font_embedding_flags(data, tables) do
    case table(data, tables, "OS/2") do
      {:ok, os2} -> read_u16(os2, 8)
      :error -> {:ok, 0}
    end
  end

  defp table_directory(data) do
    with {:ok, scaler_type} <- read_u32(data, 0),
         true <- scaler_type in [0x0001_0000, 0x7472_7565],
         {:ok, table_count} <- read_u16(data, 4),
         true <- table_count > 0,
         true <- byte_size(data) >= 12 + table_count * 16 do
      records =
        0..(table_count - 1)
        |> Enum.reduce(%{}, fn index, acc ->
          offset = 12 + index * 16
          tag = binary_part(data, offset, 4)
          <<_checksum::32, table_offset::32, length::32>> = binary_part(data, offset + 4, 12)
          Map.put(acc, tag, {table_offset, length})
        end)

      {:ok, records}
    else
      _ -> :error
    end
  end

  defp table(data, tables, tag) do
    case Map.get(tables, tag) do
      {offset, length} when offset >= 0 and length >= 0 and byte_size(data) >= offset + length ->
        {:ok, binary_part(data, offset, length)}

      _ ->
        :error
    end
  end

  defp parse_head(head) do
    with {:ok, units_per_em} <- read_u16(head, 18),
         {:ok, x_min} <- read_i16(head, 36),
         {:ok, y_min} <- read_i16(head, 38),
         {:ok, x_max} <- read_i16(head, 40),
         {:ok, y_max} <- read_i16(head, 42),
         true <- units_per_em > 0 do
      {:ok, units_per_em, {x_min, y_min, x_max, y_max}}
    else
      _ -> :error
    end
  end

  defp parse_hhea(hhea) do
    with {:ok, ascent} <- read_i16(hhea, 4),
         {:ok, descent} <- read_i16(hhea, 6),
         {:ok, line_gap} <- read_i16(hhea, 8),
         {:ok, hmetric_count} <- read_u16(hhea, 34),
         true <- hmetric_count > 0 do
      {:ok, ascent, descent, line_gap, hmetric_count}
    else
      _ -> :error
    end
  end

  defp parse_hmtx(hmtx, glyph_count, hmetric_count) do
    cond do
      glyph_count <= 0 or hmetric_count <= 0 or byte_size(hmtx) < hmetric_count * 4 ->
        :error

      true ->
        metric_widths =
          0..(hmetric_count - 1)
          |> Enum.map(fn index ->
            {:ok, width} = read_u16(hmtx, index * 4)
            width
          end)

        last_width = List.last(metric_widths)
        extra_count = max(glyph_count - hmetric_count, 0)
        {:ok, metric_widths ++ List.duplicate(last_width, extra_count)}
    end
  end

  defp parse_cmap(cmap, glyph_count) do
    with {:ok, subtables} <- prepare_cmap(cmap, glyph_count) do
      Enum.find_value(subtables, :error, fn segments ->
        mappings =
          Enum.reduce(segments, %{}, fn {first, last, delta, glyphs}, mappings ->
            first..last
            |> Enum.reject(&(&1 == 0xFFFF and not is_tuple(glyphs)))
            |> Enum.reduce(mappings, fn codepoint, mappings ->
              glyph_id =
                case glyphs do
                  nil ->
                    Integer.mod(codepoint + delta, 65_536)

                  {:sequential, glyph} ->
                    glyph + codepoint - first

                  glyphs ->
                    case :binary.decode_unsigned(binary_part(glyphs, 2 * (codepoint - first), 2)) do
                      0 -> 0
                      glyph_id -> Integer.mod(glyph_id + delta, 65_536)
                    end
                end

              case glyph_id > 0 do
                true -> Map.put(mappings, codepoint, glyph_id)
                false -> mappings
              end
            end)
          end)

        if map_size(mappings) > 0, do: {:ok, mappings}
      end)
    end
  end

  defp read_u16(data, offset) do
    case byte_size(data) >= offset + 2 do
      true ->
        <<value::16>> = binary_part(data, offset, 2)
        {:ok, value}

      false ->
        :error
    end
  end

  defp read_i16(data, offset) do
    case byte_size(data) >= offset + 2 do
      true ->
        <<value::signed-16>> = binary_part(data, offset, 2)
        {:ok, value}

      false ->
        :error
    end
  end

  defp read_u32(data, offset) do
    case byte_size(data) >= offset + 4 do
      true ->
        <<value::32>> = binary_part(data, offset, 4)
        {:ok, value}

      false ->
        :error
    end
  end
end
