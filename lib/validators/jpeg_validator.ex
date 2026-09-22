defmodule NativeElixirPdfUtilities.Validators.JpegValidator do
  @moduledoc false

  @doc false
  @spec metadata(binary()) :: {:ok, map()} | :error
  def metadata(data) do
    case data do
      <<255, 216, rest::binary>> ->
        markers(rest, %{
          frame: nil,
          mode: nil,
          adobe: nil,
          quantization: %{},
          huffman: MapSet.new(),
          coefficients: %{},
          restart_interval: 0
        })

      _ ->
        :error
    end
  end

  defp markers(data, state) do
    case data do
      <<255, 217, _rest::binary>> ->
        case state.frame do
          %{components: components} ->
            if Enum.all?(components, &Map.has_key?(state.coefficients, {elem(&1, 0), 0})),
              do: color_metadata(state.frame, state.adobe),
              else: :error

          nil ->
            :error
        end

      <<255, 255, _rest::binary>> ->
        <<_fill, rest::binary>> = data
        markers(rest, state)

      <<255, marker, length::16, segment::binary-size(length - 2), rest::binary>>
      when length >= 2 ->
        case marker do
          218 ->
            scan(segment, rest, state)

          _ ->
            with {:ok, state} <- segment(marker, segment, state) do
              markers(rest, state)
            end
        end

      _ ->
        :error
    end
  end

  defp segment(marker, bytes, state) do
    case {marker, bytes} do
      {marker, <<8, height::16, width::16, count, descriptors::binary>>}
      when marker in [192, 194] and width > 0 and height > 0 and count in [1, 3, 4] ->
        components =
          for <<id, horizontal::4, vertical::4, table <- descriptors>>,
            do: {id, horizontal, vertical, table}

        if is_nil(state.frame) and byte_size(descriptors) == count * 3 and
             length(Enum.uniq_by(components, &elem(&1, 0))) == count and
             Enum.all?(components, fn {_id, h, v, table} ->
               h in 1..4 and v in 1..4 and table <= 3
             end) do
          {:ok,
           %{
             state
             | frame: %{width_px: width, height_px: height, components: components},
               mode: marker
           }}
        else
          :error
        end

      {219, <<_, _::binary>>} ->
        with {:ok, tables} <- quantization_tables(bytes, state.quantization) do
          {:ok, %{state | quantization: tables}}
        end

      {196, <<_, _::binary>>} ->
        with {:ok, tables} <- huffman_tables(bytes, state.huffman) do
          {:ok, %{state | huffman: tables}}
        end

      {221, <<interval::16>>} ->
        {:ok, %{state | restart_interval: interval}}

      {238, <<"Adobe", _version::16, _flags0::16, _flags1::16, transform>>}
      when transform in 0..2 ->
        if is_nil(state.adobe) or state.adobe == transform,
          do: {:ok, %{state | adobe: transform}},
          else: :error

      {238, <<"Adobe", _::binary>>} ->
        :error

      {marker, _} when marker in 224..239 or marker == 254 ->
        {:ok, state}

      _ ->
        :error
    end
  end

  defp quantization_tables(data, tables) do
    case data do
      <<>> ->
        {:ok, tables}

      <<precision::4, id::4, rest::binary>> when precision in 0..1 and id <= 3 ->
        size = 64 * (precision + 1)

        case rest do
          <<values::binary-size(^size), rest::binary>> ->
            bits = 8 * (precision + 1)

            if Enum.all?(for(<<value::size(^bits) <- values>>, do: value), &(&1 > 0)),
              do: quantization_tables(rest, Map.put(tables, id, precision)),
              else: :error

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  defp huffman_tables(data, tables) do
    case data do
      <<>> ->
        {:ok, tables}

      <<class::4, id::4, counts::binary-size(16), rest::binary>>
      when class in 0..1 and id <= 3 ->
        counts = :binary.bin_to_list(counts)
        count = Enum.sum(counts)

        # JPEG Huffman tables have at most 256 symbols and leave the all-ones
        # code unused. These are format invariants, not resource limits.
        slots =
          Enum.reduce_while(counts, 1, fn count, slots ->
            available = slots * 2 - count
            if available > 0, do: {:cont, available}, else: {:halt, 0}
          end)

        case rest do
          <<symbols::binary-size(^count), rest::binary>> when count in 1..256 and slots > 0 ->
            valid_symbols? =
              Enum.all?(:binary.bin_to_list(symbols), fn value ->
                if class == 0, do: value <= 11, else: rem(value, 16) <= 10
              end)

            if valid_symbols?,
              do: huffman_tables(rest, MapSet.put(tables, {class, id})),
              else: :error

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  defp scan(header, rest, state) do
    case header do
      <<count, selectors::binary-size(count * 2), first, last, high::4, low::4>>
      when count in 1..4 and not is_nil(state.frame) ->
        selectors = for <<id, dc::4, ac::4 <- selectors>>, do: {id, dc, ac}
        components = Map.new(state.frame.components, fn {id, _, _, table} -> {id, table} end)

        parameters_valid? =
          case state.mode do
            192 ->
              first == 0 and last == 63 and high == 0 and low == 0

            194 ->
              first <= last and last <= 63 and high <= 13 and low <= 13 and
                (high == 0 or high == low + 1) and
                if(first == 0, do: last == 0, else: count == 1)
          end

        selectors_valid? =
          length(Enum.uniq_by(selectors, &elem(&1, 0))) == count and
            Enum.all?(selectors, fn {id, dc, ac} ->
              Map.has_key?(components, id) and dc <= 3 and ac <= 3 and
                Map.has_key?(state.quantization, components[id]) and
                (state.mode != 192 or state.quantization[components[id]] == 0) and
                (first != 0 or high != 0 or MapSet.member?(state.huffman, {0, dc})) and
                (last == 0 or MapSet.member?(state.huffman, {1, ac})) and
                (state.mode != 194 or if(first == 0, do: ac == 0, else: dc == 0))
            end)

        if parameters_valid? and selectors_valid? do
          keys = for {id, _, _} <- selectors, coefficient <- first..last, do: {id, coefficient}

          progression_valid? =
            Enum.all?(keys, fn {id, _} = key ->
              previous = Map.get(state.coefficients, key)

              (first == 0 or Map.has_key?(state.coefficients, {id, 0})) and
                if(high == 0, do: is_nil(previous), else: previous == high)
            end)

          if progression_valid? do
            coefficients = Enum.reduce(keys, state.coefficients, &Map.put(&2, &1, low))
            scan_data(rest, %{state | coefficients: coefficients}, false, 0)
          else
            :error
          end
        else
          :error
        end

      _ ->
        :error
    end
  end

  defp scan_data(data, state, has_data?, next_restart) do
    case :binary.match(data, <<255>>) do
      :nomatch ->
        :error

      {offset, 1} ->
        <<_entropy::binary-size(^offset), marker::binary>> = data
        has_data? = has_data? or offset > 0

        case marker do
          <<255, 0, rest::binary>> ->
            scan_data(rest, state, true, next_restart)

          <<255, 255, _rest::binary>> ->
            <<_fill, rest::binary>> = marker
            scan_data(rest, state, has_data?, next_restart)

          <<255, restart, rest::binary>> when restart in 208..215 ->
            if has_data? and state.restart_interval > 0 and restart == 208 + next_restart,
              do: scan_data(rest, state, false, rem(next_restart + 1, 8)),
              else: :error

          _ ->
            if has_data?, do: markers(marker, state), else: :error
        end
    end
  end

  defp color_metadata(frame, adobe) do
    case frame do
      %{components: components} ->
        ids = Enum.map(components, &elem(&1, 0))

        convention =
          case {ids, adobe} do
            {[_], value} when value in [nil, 0] -> :gray
            {_, 0} when length(ids) == 3 -> :rgb
            {_, 1} when length(ids) == 3 -> :ycbcr
            {[?R, ?G, ?B], nil} -> :rgb
            {[1, 2, 3], nil} -> :ycbcr
            {_, 0} when length(ids) == 4 -> :adobe_cmyk
            {_, 2} when length(ids) == 4 -> :ycck
            {[?C, ?M, ?Y, ?K], nil} -> :cmyk
            {[1, 2, 3, 4], nil} -> :cmyk
            _ -> :unsupported
          end

        if convention == :unsupported do
          :error
        else
          {:ok,
           Map.merge(frame, %{
             color_space:
               case length(ids) do
                 1 -> :device_gray
                 3 -> :device_rgb
                 4 -> :device_cmyk
               end,
             color_transform: convention,
             inverted_cmyk: convention in [:adobe_cmyk, :ycck]
           })}
        end
    end
  end
end
