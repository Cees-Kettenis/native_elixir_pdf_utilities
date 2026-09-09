defmodule NativeElixirPdfUtilities.Validators.FontValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Limits

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
