defmodule NativeElixirPdfUtilities.Validators.BrotliValidator do
  @moduledoc false
  import Bitwise
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.Font.BrotliData
  alias NativeElixirPdfUtilities.Validators.FontValidator

  # RFC 7932, July 2016. Alphabet sizes, code depths, window encodings and
  # dictionary dimensions are fixed format invariants, not resource settings.
  @code_order {1, 2, 3, 4, 0, 5, 17, 6, 16, 7, 8, 9, 10, 11, 12, 13, 14, 15}
  @block_bases {1, 5, 9, 13, 17, 25, 33, 41, 49, 65, 81, 97, 113, 145, 177, 209, 241, 305, 369,
                497, 753, 1265, 2289, 4337, 8433, 16625}
  @block_bits {2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 7, 8, 9, 10, 11, 12, 13, 24}
  @insert_bases {0, 1, 2, 3, 4, 5, 6, 8, 10, 14, 18, 26, 34, 50, 66, 98, 130, 194, 322, 578, 1090,
                 2114, 6210, 22594}
  @insert_bits {0, 0, 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 7, 8, 9, 10, 12, 14, 24}
  @copy_bases {2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 18, 22, 30, 38, 54, 70, 102, 134, 198, 326,
               582, 1094, 2118}
  @copy_bits {0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 7, 8, 9, 10, 24}
  @insert_ranges {0, 0, 8, 8, 0, 16, 8, 16, 16}
  @copy_ranges {0, 8, 0, 8, 16, 0, 16, 8, 16}
  @distance_offsets {3, 2, 1, 0, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2}
  @distance_deltas {0, 0, 0, 0, -1, 1, -2, 2, -3, 3, -1, 1, -2, 2, -3, 3}

  @doc "Decodes one complete RFC 7932 stream with an exact, pre-admitted output length and cumulative font work limits."
  @spec decompress(term(), term()) :: {:ok, binary()} | {:error, {atom(), map()}}
  def decompress(data, expected) do
    FontValidator.with_budget(fn ->
      try do
        unless is_binary(data) and is_integer(expected) and expected >= 0,
          do: invalid("Brotli requires binary source bytes and a nonnegative decoded length", 0)

        FontValidator.reserve_source(data)
        FontValidator.check(:max_decoded_bytes, expected)

        state = %{
          input: data,
          bit: 0,
          size: 0,
          expected: expected,
          blocks: :array.new(),
          buffer: :array.new(256, default: 0),
          used: 0,
          prev1: 0,
          prev2: 0,
          distances: {16, 15, 11, 4},
          distance_index: 0
        }

        {first, state} = take(state, 1)

        {window, state} =
          if first == 0 do
            {16, state}
          else
            {n, state} = take(state, 3)

            if n > 0 do
              {17 + n, state}
            else
              {n, state} = take(state, 3)
              if n == 1, do: invalid("Brotli window bit pattern is reserved", state.bit)
              {if(n == 0, do: 17, else: 8 + n), state}
            end
          end

        state = metablocks(Map.put(state, :window, (1 <<< window) - 16)) |> align()

        unless state.bit == byte_size(data) * 8,
          do: invalid("Brotli stream has trailing bytes", state.bit)

        unless state.size == expected,
          do: invalid("Brotli decoded length differs from its declared length", state.bit)

        chunks = :array.to_list(state.blocks)
        tail = for i <- 0..(state.used - 1)//1, into: <<>>, do: <<:array.get(i, state.buffer)>>
        {:ok, IO.iodata_to_binary([chunks, tail])}
      catch
        {:invalid_brotli, error} -> error
      end
    end)
  end

  defp metablocks(state) do
    {last, state} = take(state, 1)
    {empty, state} = if last == 1, do: take(state, 1), else: {0, state}

    if empty == 1 do
      state
    else
      {nibbles, state} = take(state, 2)

      if nibbles == 3 do
        {reserved, state} = take(state, 1)
        if reserved != 0, do: invalid("Brotli metadata reserved bit must be zero", state.bit)
        {bytes, state} = take(state, 2)
        {length, state} = if bytes == 0, do: {0, state}, else: encoded_length(state, bytes, 8)
        state = align(state)
        {_metadata, state} = raw(state, length)
        if last == 1, do: state, else: metablocks(state)
      else
        {length, state} = encoded_length(state, nibbles + 4, 4)

        if state.size + length > state.expected,
          do: invalid("Brotli meta-block exceeds the declared decoded length", state.bit)

        {stored, state} = if last == 0, do: take(state, 1), else: {0, state}

        state =
          if stored == 1 do
            state = align(state)
            {bytes, state} = raw(state, length)
            append_bytes(state, bytes)
          else
            compressed(state, length)
          end

        if last == 1, do: state, else: metablocks(state)
      end
    end
  end

  defp encoded_length(state, count, bits) do
    {value, state} = take(state, count * bits)

    minimum = if bits == 8, do: 1, else: 4

    if count > minimum and value >>> ((count - 1) * bits) == 0,
      do: invalid("Brotli meta-block length uses a redundant high digit", state.bit)

    {value + 1, state}
  end

  defp compressed(state, length) do
    {literal, state} = block_domain(state)
    {command, state} = block_domain(state)
    {distance, state} = block_domain(state)
    {postfix, state} = take(state, 2)
    {direct, state} = take(state, 4)
    direct = 16 + (direct <<< postfix)
    {modes, state} = Enum.map_reduce(1..literal.count, state, fn _, state -> take(state, 2) end)
    {literal_count, literal_map, state} = context_map(state, literal.count * 64)
    {distance_count, distance_map, state} = context_map(state, distance.count * 4)
    {literal_trees, state} = trees(state, literal_count, 256)
    {command_trees, state} = trees(state, command.count, 704)
    {distance_trees, state} = trees(state, distance_count, direct + (48 <<< postfix))

    config = %{
      literal: literal,
      command: command,
      distance: distance,
      modes: List.to_tuple(modes),
      literal_map: literal_map,
      distance_map: distance_map,
      literal_trees: literal_trees,
      command_trees: command_trees,
      distance_trees: distance_trees,
      postfix: postfix,
      direct: direct
    }

    commands(state, length, config)
  end

  defp block_domain(state) do
    {count, state} = variable_byte(state)
    count = count + 1

    if count == 1 do
      {%{count: 1, type: 0, left: :infinity, previous: 1}, state}
    else
      {type_tree, state} = huffman(state, count + 2)
      {length_tree, state} = huffman(state, 26)
      {length, state} = block_length(state, length_tree)

      {%{
         count: count,
         type: 0,
         left: length,
         previous: 1,
         type_tree: type_tree,
         length_tree: length_tree
       }, state}
    end
  end

  defp consume(state, domain) do
    case domain do
      %{count: 1} ->
        {domain, state}

      %{left: 0} ->
        {code, state} = symbol(state, domain.type_tree)

        type =
          case code do
            0 -> domain.previous
            1 -> domain.type + 1
            code -> code - 2
          end

        type = if type >= domain.count, do: type - domain.count, else: type

        # The admitted count+2 symbol alphabet and the two previous valid
        # types already bound this selected index to the domain.
        {length, state} = block_length(state, domain.length_tree)
        {%{domain | type: type, previous: domain.type, left: length - 1}, state}

      _ ->
        {%{domain | left: domain.left - 1}, state}
    end
  end

  defp block_length(state, tree) do
    {code, state} = symbol(state, tree)
    {extra, state} = take(state, elem(@block_bits, code))
    {elem(@block_bases, code) + extra, state}
  end

  defp commands(state, remaining, config) do
    if remaining == 0 do
      state
    else
      {domain, state} = consume(state, config.command)
      {code, state} = symbol(state, elem(config.command_trees, domain.type))
      range = code >>> 6
      implicit? = range < 2
      range = if implicit?, do: range, else: range - 2
      insert_code = elem(@insert_ranges, range) + (code >>> 3 &&& 7)
      copy_code = elem(@copy_ranges, range) + (code &&& 7)
      {extra, state} = take(state, elem(@insert_bits, insert_code))
      insert = elem(@insert_bases, insert_code) + extra
      {extra, state} = take(state, elem(@copy_bits, copy_code))
      copy = elem(@copy_bases, copy_code) + extra

      if insert > remaining,
        do: invalid("Brotli literal insertion exceeds its meta-block", state.bit)

      config = %{config | command: domain}
      {state, config} = literals(state, insert, config)
      remaining = remaining - insert

      if remaining == 0 do
        state
      else
        {code, state, config} =
          if implicit? do
            {0, state, config}
          else
            {domain, state} = consume(state, config.distance)
            context = min(copy - 2, 3)
            tree = elem(config.distance_map, domain.type * 4 + context)
            {code, state} = symbol(state, elem(config.distance_trees, tree))

            {code, state} =
              if code >= config.direct do
                value = code - config.direct
                postfix = value &&& (1 <<< config.postfix) - 1
                prefix = value >>> config.postfix
                bits = (prefix >>> 1) + 1
                offset = ((2 + (prefix &&& 1)) <<< bits) - 4
                {extra, state} = take(state, bits)
                {config.direct + ((offset + extra) <<< config.postfix) + postfix, state}
              else
                {code, state}
              end

            {code, state, %{config | distance: domain}}
          end

        distance =
          if code < 16 do
            elem(state.distances, state.distance_index + elem(@distance_offsets, code) &&& 3) +
              elem(@distance_deltas, code)
          else
            code - 15
          end

        if distance <= 0, do: invalid("Brotli copy distance must be positive", state.bit)
        maximum = min(state.size, state.window)

        {state, written} =
          if distance > maximum do
            word = dictionary_word(copy, distance - maximum - 1, state.bit)

            if byte_size(word) > remaining,
              do: invalid("Brotli dictionary transform exceeds its meta-block", state.bit)

            {append_bytes(state, word), byte_size(word)}
          else
            if copy > remaining,
              do: invalid("Brotli history copy exceeds its meta-block", state.bit)

            state =
              if code > 0 do
                %{
                  state
                  | distances: put_elem(state.distances, state.distance_index &&& 3, distance),
                    distance_index: state.distance_index + 1
                }
              else
                state
              end

            {copy_history(state, copy, distance), copy}
          end

        commands(state, remaining - written, config)
      end
    end
  end

  defp literals(state, count, config) do
    if count == 0 do
      {state, config}
    else
      {domain, state} = consume(state, config.literal)
      mode = elem(config.modes, domain.type)
      lookup = BrotliData.context()

      context =
        :binary.at(lookup, mode * 512 + state.prev1) |||
          :binary.at(lookup, mode * 512 + 256 + state.prev2)

      tree = elem(config.literal_map, domain.type * 64 + context)
      {value, state} = symbol(state, elem(config.literal_trees, tree))
      literals(append(state, value), count - 1, %{config | literal: domain})
    end
  end

  defp context_map(state, size) do
    FontValidator.reserve(:max_work, size)
    {count, state} = variable_byte(state)
    count = count + 1

    if count == 1 do
      {count, List.duplicate(0, size) |> List.to_tuple(), state}
    else
      {rle, state} = take(state, 1)

      {run, state} =
        if rle == 1 do
          {value, state} = take(state, 4)
          {value + 1, state}
        else
          {0, state}
        end

      {tree, state} = huffman(state, count + run)
      {values, state} = context_values(state, tree, size, run, [])
      {mtf, state} = take(state, 1)

      values =
        if mtf == 1 do
          {values, _} =
            Enum.map_reduce(values, Enum.to_list(0..255), fn index, table ->
              {before, [value | after_values]} = Enum.split(table, index)
              {value, [value | before ++ after_values]}
            end)

          values
        else
          values
        end

      # The admitted map alphabet bounds direct indices; move-to-front only
      # permutes those first count entries.
      {count, List.to_tuple(values), state}
    end
  end

  defp context_values(state, tree, left, run, values) do
    if left == 0 do
      {Enum.reverse(values), state}
    else
      {code, state} = symbol(state, tree)

      {amount, value, state} =
        cond do
          code == 0 ->
            {1, 0, state}

          code <= run ->
            {extra, state} = take(state, code)
            {(1 <<< code) + extra, 0, state}

          true ->
            {1, code - run, state}
        end

      if amount > left, do: invalid("Brotli context-map zero run exceeds its map", state.bit)
      context_values(state, tree, left - amount, run, List.duplicate(value, amount) ++ values)
    end
  end

  defp trees(state, count, alphabet) do
    {trees, state} = Enum.map_reduce(1..count, state, fn _, state -> huffman(state, alphabet) end)
    {List.to_tuple(trees), state}
  end

  defp huffman(state, alphabet) do
    FontValidator.reserve(:max_work, alphabet)
    {kind, state} = take(state, 2)

    if kind == 1 do
      {count, state} = take(state, 2)
      count = count + 1
      bits = (alphabet - 1) |> Integer.digits(2) |> length()
      {symbols, state} = Enum.map_reduce(1..count, state, fn _, state -> take(state, bits) end)

      unless length(Enum.uniq(symbols)) == count and Enum.all?(symbols, &(&1 < alphabet)),
        do:
          invalid(
            "Brotli simple Huffman symbols must be unique and within their alphabet",
            state.bit
          )

      {lengths, state} =
        case count do
          1 ->
            {[1], state}

          2 ->
            {[1, 1], state}

          3 ->
            {[1, 2, 2], state}

          4 ->
            {choice, state} = take(state, 1)
            {if(choice == 0, do: [2, 2, 2, 2], else: [1, 2, 3, 3]), state}
        end

      {build_tree(Enum.zip(symbols, lengths), state.bit), state}
    else
      {lengths, state} = code_length_alphabet(state, kind, 32, 0, %{})
      tree = build_tree(Map.to_list(lengths), state.bit)
      {lengths, state} = code_lengths(state, tree, alphabet, 0, 32768, 8, 0, 0, [])

      {build_tree(
         Enum.with_index(lengths) |> Enum.map(fn {length, index} -> {index, length} end),
         state.bit
       ), state}
    end
  end

  defp code_length_alphabet(state, index, space, count, lengths) do
    if index == 18 or space == 0 do
      unless count == 1 or space == 0,
        do: invalid("Brotli code-length alphabet has unused prefix space", state.bit)

      {lengths, state}
    else
      {prefix, state} = take(state, 2)

      {value, state} =
        case prefix do
          0 ->
            {0, state}

          1 ->
            {4, state}

          2 ->
            {3, state}

          3 ->
            {bit, state} = take(state, 1)

            if bit == 0 do
              {2, state}
            else
              {bit, state} = take(state, 1)
              {if(bit == 0, do: 1, else: 5), state}
            end
        end

      space = if value == 0, do: space, else: space - (32 >>> value)
      if space < 0, do: invalid("Brotli code-length alphabet is oversubscribed", state.bit)

      code_length_alphabet(
        state,
        index + 1,
        space,
        count + if(value == 0, do: 0, else: 1),
        Map.put(lengths, elem(@code_order, index), value)
      )
    end
  end

  defp code_lengths(state, tree, alphabet, index, space, previous, repeat, repeat_length, values) do
    cond do
      space == 0 ->
        {Enum.reverse(values) ++ List.duplicate(0, alphabet - index), state}

      index == alphabet ->
        invalid("Brotli Huffman code leaves unused prefix space", state.bit)

      true ->
        {code, state} = symbol(state, tree)

        if code < 16 do
          next_space = if code == 0, do: space, else: space - (32768 >>> code)
          if next_space < 0, do: invalid("Brotli Huffman code is oversubscribed", state.bit)

          code_lengths(
            state,
            tree,
            alphabet,
            index + 1,
            next_space,
            if(code == 0, do: previous, else: code),
            0,
            repeat_length,
            [code | values]
          )
        else
          length = if code == 16, do: previous, else: 0
          repeat = if length == repeat_length, do: repeat, else: 0
          bits = code - 14
          {extra, state} = take(state, bits)
          next_repeat = if repeat == 0, do: 0, else: (repeat - 2) <<< bits
          next_repeat = next_repeat + extra + 3
          delta = next_repeat - repeat

          unless delta > 0 and index + delta <= alphabet,
            do: invalid("Brotli repeated code lengths exceed their alphabet", state.bit)

          space = if length == 0, do: space, else: space - delta * (32768 >>> length)

          if space < 0,
            do: invalid("Brotli repeated code lengths oversubscribe prefix space", state.bit)

          code_lengths(
            state,
            tree,
            alphabet,
            index + delta,
            space,
            previous,
            next_repeat,
            length,
            List.duplicate(length, delta) ++ values
          )
        end
    end
  end

  defp build_tree(entries, bit) do
    entries =
      entries
      |> Enum.reject(&(elem(&1, 1) == 0))
      |> Enum.sort_by(fn {symbol, length} -> {length, symbol} end)

    case entries do
      # Both callers admit a nonempty simple alphabet or consume all prefix
      # space before constructing this tree.
      [{symbol, _}] ->
        {:single, symbol}

      _ ->
        {codes, code, length} =
          Enum.reduce(entries, {%{}, 0, 0}, fn {symbol, length}, {codes, code, previous} ->
            # Length symbols are admitted in 0..15, zero lengths removed,
            # and entries sorted above. No second depth validation is needed.
            code = code <<< (length - previous)
            if code >= 1 <<< length, do: invalid("Brotli Huffman tree is oversubscribed", bit)
            {Map.put(codes, {length, code}, symbol), code + 1, length}
          end)

        unless code == 1 <<< length, do: invalid("Brotli Huffman tree is incomplete", bit)
        {:tree, codes, length}
    end
  end

  defp symbol(state, tree) do
    case tree do
      {:single, value} -> {value, state}
      {:tree, codes, maximum} -> symbol_bits(state, codes, maximum, 0, 0)
    end
  end

  defp symbol_bits(state, codes, maximum, length, code) do
    {bit, state} = take(state, 1)
    length = length + 1
    code = code <<< 1 ||| bit

    case Map.fetch(codes, {length, code}) do
      {:ok, value} ->
        {value, state}

      :error ->
        if length == maximum, do: invalid("Brotli prefix code is unavailable", state.bit)
        symbol_bits(state, codes, maximum, length, code)
    end
  end

  defp variable_byte(state) do
    {flag, state} = take(state, 1)

    if flag == 0 do
      {0, state}
    else
      {bits, state} = take(state, 3)

      if bits == 0 do
        {1, state}
      else
        {value, state} = take(state, bits)
        {(1 <<< bits) + value, state}
      end
    end
  end

  defp take(state, bits) do
    FontValidator.reserve(:max_work, 1)

    if state.bit + bits > byte_size(state.input) * 8,
      do: invalid("Brotli bitstream is truncated", state.bit)

    value = read_bits(state.input, state.bit, bits, 0, 0)
    {value, %{state | bit: state.bit + bits}}
  end

  defp read_bits(input, position, left, shift, value) do
    if left == 0 do
      value
    else
      count = min(left, 8 - (position &&& 7))
      chunk = :binary.at(input, position >>> 3) >>> (position &&& 7) &&& (1 <<< count) - 1
      read_bits(input, position + count, left - count, shift + count, value ||| chunk <<< shift)
    end
  end

  defp align(state) do
    {padding, state} = take(state, rem(8 - rem(state.bit, 8), 8))
    if padding != 0, do: invalid("Brotli byte-alignment padding must be zero", state.bit)
    state
  end

  defp raw(state, length) do
    unless rem(state.bit, 8) == 0 and state.bit + length * 8 <= byte_size(state.input) * 8,
      do: invalid("Brotli byte payload is truncated", state.bit)

    FontValidator.reserve(:max_work, length)
    {binary_part(state.input, div(state.bit, 8), length), %{state | bit: state.bit + length * 8}}
  end

  defp append_bytes(state, bytes) do
    case bytes do
      <<>> -> state
      <<value, rest::binary>> -> append_bytes(append(state, value), rest)
    end
  end

  defp append(state, value) do
    FontValidator.reserve(:max_work, 1)

    # Meta-block admission owns the declared output bound. Literal, history
    # and dictionary counts have already been bounded by the remaining block.
    buffer = :array.set(state.used, value, state.buffer)

    state = %{
      state
      | size: state.size + 1,
        used: state.used + 1,
        buffer: buffer,
        prev2: state.prev1,
        prev1: value
    }

    if state.used == 256 do
      bytes = :array.to_list(buffer) |> :binary.list_to_bin()

      %{
        state
        | blocks: :array.set(div(state.size, 256) - 1, bytes, state.blocks),
          buffer: :array.new(256, default: 0),
          used: 0
      }
    else
      state
    end
  end

  defp copy_history(state, left, distance) do
    if left == 0 do
      state
    else
      position = state.size - distance

      value =
        if div(position, 256) == div(state.size, 256) do
          :array.get(rem(position, 256), state.buffer)
        else
          :binary.at(:array.get(div(position, 256), state.blocks), rem(position, 256))
        end

      copy_history(append(state, value), left - 1, distance)
    end
  end

  defp dictionary_word(length, index, bit) do
    unless length in 4..24,
      do: invalid("Brotli dictionary reference has an invalid base length", bit)

    bits = elem(BrotliData.size_bits(), length)
    word = index &&& (1 <<< bits) - 1
    transform = index >>> bits

    unless transform < 121,
      do: invalid("Brotli dictionary reference selects an unavailable transform", bit)

    word =
      binary_part(
        BrotliData.dictionary(),
        elem(BrotliData.offsets(), length) + word * length,
        length
      )

    {prefix, operator, suffix} = elem(BrotliData.transforms(), transform)

    word =
      cond do
        operator in 1..9 ->
          binary_part(word, 0, max(0, length - operator))

        operator in 12..20 ->
          skip = min(length, operator - 11)
          binary_part(word, skip, length - skip)

        operator == 10 ->
          ferment(word, false)

        operator == 11 ->
          ferment(word, true)

        true ->
          word
      end

    elem(BrotliData.prefixes(), prefix) <> word <> elem(BrotliData.prefixes(), suffix)
  end

  defp ferment(bytes, all?) do
    case bytes do
      <<>> ->
        <<>>

      <<a, rest::binary>> when a < 0xC0 ->
        a = if a in ?a..?z, do: bxor(a, 32), else: a
        <<a, if(all?, do: ferment(rest, true), else: rest)::binary>>

      <<a, b, rest::binary>> when a < 0xE0 ->
        <<a, bxor(b, 32), if(all?, do: ferment(rest, true), else: rest)::binary>>

      <<a, b, c, rest::binary>> ->
        <<a, b, bxor(c, 5), if(all?, do: ferment(rest, true), else: rest)::binary>>

      _ ->
        bytes
    end
  end

  defp invalid(message, bit) do
    throw(
      {:invalid_brotli,
       Diagnostics.error(:font, :invalid_font, message,
         module: __MODULE__,
         operation: :decode_brotli,
         source: "Brotli bit #{bit}"
       )}
    )
  end
end
