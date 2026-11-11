defmodule NativeElixirPdfUtilities.Validators.CffFontValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.Font.CffData
  alias NativeElixirPdfUtilities.Validators.{FontValidator, HtmlValidator}

  @doc "Returns the fixed Type 2 subroutine bias for an admitted INDEX count."
  @spec subroutine_bias(non_neg_integer()) :: 107 | 1131 | 32768
  def subroutine_bias(count) do
    cond do
      count < 1240 -> 107
      count < 33900 -> 1131
      true -> 32768
    end
  end

  @doc "Reads a bounded CFF 1 INDEX, preserving its individual object boundaries."
  @spec prepare_index(term(), term(), 1 | 2) ::
          {:ok, tuple(), non_neg_integer()} | {:error, {atom(), map()}}
  def prepare_index(data, offset, version \\ 1) do
    with_data(data, fn ->
      unless version in [1, 2], do: invalid("CFF INDEX version must be one or two", "CFF INDEX")
      {objects, finish} = index(data, offset, if(version == 1, do: 2, else: 4))
      {:ok, objects, finish}
    end)
  end

  @doc "Reads CFF DICT operands and operators without executing their semantics."
  @spec prepare_dictionary(term(), map() | nil) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_dictionary(data, context \\ nil) do
    with_data(data, fn -> {:ok, dictionary(data, context)} end)
  end

  @doc "Admits CFF 1 dictionary, character-program and subroutine ownership. This does not execute or enable embedding of its programs."
  @spec prepare(term(), term(), [number()]) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(data, glyph_count, coordinates \\ []) do
    case data do
      <<2, _::binary>> ->
        NativeElixirPdfUtilities.Validators.Cff2FontValidator.prepare(
          data,
          glyph_count,
          coordinates
        )

      _ ->
        prepare_cff1(data, glyph_count)
    end
  end

  defp prepare_cff1(data, glyph_count) do
    with_data(data, fn ->
      unless is_integer(glyph_count) and glyph_count in 1..65_535,
        do:
          invalid(
            "CFF glyph count must match a positive OpenType uint16 glyph count",
            "CFF CharStrings"
          )

      <<major, minor, header, offset_size>> = read(data, 0, 4)

      unless major == 1 and minor == 0 and header >= 4 and offset_size in 1..4,
        do:
          invalid(
            "CFF 1 requires version 1.0, a complete header and a one-to-four-byte offset size",
            "CFF header"
          )

      read(data, 0, header)
      {names, next} = index(data, header)
      owners = own([], :header, 0, header) |> own(:names, header, next)
      {top_dicts, finish} = index(data, next)
      owners = own(owners, :top_dicts, next, finish)
      {strings, next} = index(data, finish)
      owners = own(owners, :strings, finish, next)
      {global_subrs, finish} = index(data, next)
      owners = own(owners, :global_subrs, next, finish)

      unless tuple_size(names) == 1 and tuple_size(top_dicts) == 1,
        do:
          invalid("an OpenType CFF table requires exactly one Name and Top DICT", "CFF font set")

      name = elem(names, 0)

      unless byte_size(name) in 1..127 and
               Enum.all?(:binary.bin_to_list(name), &(&1 in 33..126 and &1 not in ~c"[](){}<>/%")),
             do:
               invalid(
                 "CFF font name must be a nonempty PostScript name of at most 127 bytes",
                 "CFF Name"
               )

      # Standard SIDs occupy 0..390. SIDs 65000..65535 are reserved for implementation use.
      unless tuple_size(strings) <= 64_999 - 390,
        do: invalid("CFF String INDEX exceeds the fixed SID namespace", "CFF strings")

      top = dictionary(elem(top_dicts, 0))
      maximum_sid = 390 + tuple_size(strings)
      validate_sids(top, [0, 1, 2, 3, 4, {12, 0}, {12, 21}, {12, 22}], maximum_sid)

      unless number(top, {12, 6}, 2, "CFF CharstringType") == 2,
        do: invalid("OpenType CFF requires Type 2 charstrings", "CFF CharstringType")

      if Map.has_key?(top, {12, 20}),
        do:
          invalid(
            "OpenType CFF cannot resolve a synthetic font from a one-font set",
            "CFF SyntheticBase"
          )

      unless number(top, {12, 5}, 0, "CFF PaintType") == 0,
        do:
          invalid(
            "OpenType CFF outline metrics require filled PaintType 0 glyphs",
            "CFF PaintType"
          )

      font_matrix = vector(top, {12, 7}, [0.001, 0, 0, 0.001, 0, 0], 6, "CFF FontMatrix")
      bbox = vector(top, 5, [0, 0, 0, 0], 4, "CFF FontBBox")
      [xmin, ymin, xmax, ymax] = bbox

      unless xmin <= xmax and ymin <= ymax,
        do:
          invalid(
            "CFF FontBBox minimum coordinates exceed its maximum coordinates",
            "CFF FontBBox"
          )

      program_offset = integer(top, 17, nil, "CFF CharStrings")
      {charstrings, finish} = index(data, program_offset)
      owners = own(owners, :charstrings, program_offset, finish)

      unless tuple_size(charstrings) == glyph_count,
        do: invalid("CFF CharStrings count does not match maxp.numGlyphs", "CFF CharStrings")

      cid? = Map.has_key?(top, {12, 30})
      {charset, owners} = charset(data, top, glyph_count, tuple_size(strings), cid?, owners)
      {encoding, owners} = encoding(data, top, glyph_count, cid?, charset, owners)

      {fonts, fd_select, owners} =
        if cid? do
          unless number(top, {12, 33}, 0, "CFF CIDFontType") == 0,
            do: invalid("CFF 1 CID fonts require CIDFontType 0", "CFF CIDFontType")

          ros = vector(top, {12, 30}, nil, 3, "CFF ROS")
          [registry, ordering, supplement] = ros
          maximum_sid = 390 + tuple_size(strings)

          unless is_integer(registry) and registry in 0..maximum_sid and is_integer(ordering) and
                   ordering in 0..maximum_sid and is_integer(supplement) and
                   supplement >= 0,
                 do:
                   invalid(
                     "CFF ROS requires valid registry/ordering SIDs and a nonnegative supplement",
                     "CFF ROS"
                   )

          cid_count = integer(top, {12, 34}, 8720, "CFF CIDCount")
          maximum_cid = Enum.max([0 | charset])

          unless cid_count in 1..65_536 and maximum_cid < cid_count,
            do: invalid("CFF CIDCount does not contain every charset CID", "CFF CIDCount")

          array_offset = integer(top, {12, 36}, nil, "CFF FDArray")
          {dictionaries, finish} = index(data, array_offset)
          owners = own(owners, :fd_array, array_offset, finish)
          # Every CFF 1 FDSelect record uses an unsigned byte for its font index.
          unless tuple_size(dictionaries) in 1..256,
            do: invalid("CFF 1 FDArray must contain 1..256 font dictionaries", "CFF FDArray")

          {fonts, owners} =
            Enum.map_reduce(Tuple.to_list(dictionaries), owners, fn bytes, owners ->
              values = dictionary(bytes)
              validate_sids(values, [{12, 38}], maximum_sid)
              private_font(data, values, owners)
            end)

          select_offset = integer(top, {12, 37}, nil, "CFF FDSelect")
          {selection, finish} = fd_select(data, select_offset, glyph_count, length(fonts))
          owners = own(owners, :fd_select, select_offset, finish)
          {List.to_tuple(fonts), selection, owners}
        else
          {font, owners} = private_font(data, top, owners)
          {{font}, List.duplicate(0, glyph_count) |> List.to_tuple(), owners}
        end

      {:ok,
       %{
         name: name,
         top: top,
         strings: strings,
         charstrings: charstrings,
         global_subrs: global_subrs,
         fonts: fonts,
         fd_select: fd_select,
         charset: charset,
         encoding: encoding,
         cid?: cid?,
         font_matrix: font_matrix,
         bbox: List.to_tuple(bbox),
         owners: owners
       }}
    end)
  end

  defp with_data(data, runner) do
    FontValidator.with_budget(fn ->
      try do
        unless is_binary(data), do: invalid("CFF parsing requires font table bytes", "CFF data")
        FontValidator.reserve_source(data)
        runner.()
      catch
        {:cff_invalid, error} -> error
      end
    end)
  end

  defp index(data, offset, count_bytes \\ 2) do
    count = :binary.decode_unsigned(read(data, offset, count_bytes))
    FontValidator.reserve(:max_font_cff_work, count + 2)

    if count == 0 do
      {{}, offset + count_bytes}
    else
      <<size>> = read(data, offset + count_bytes, 1)
      unless size in 1..4, do: invalid("CFF INDEX offset size must be 1..4", "CFF INDEX")
      positions = read(data, offset + count_bytes + 1, (count + 1) * size)
      offsets = for <<value::unsigned-big-integer-size(^size * 8) <- positions>>, do: value
      [first | _] = offsets

      unless first == 1 and
               Enum.chunk_every(offsets, 2, 1, :discard) |> Enum.all?(fn [a, b] -> a <= b end),
             do: invalid("CFF INDEX offsets must be ascending and start at one", "CFF INDEX")

      size_of_data = List.last(offsets) - 1
      start = offset + count_bytes + 1 + byte_size(positions)
      read(data, start, size_of_data)
      FontValidator.reserve(:max_font_cff_work, byte_size(positions) + size_of_data)

      objects =
        offsets
        |> Enum.chunk_every(2, 1, :discard)
        |> Enum.map(fn [a, b] -> binary_part(data, start + a - 1, b - a) end)

      {List.to_tuple(objects), start + size_of_data}
    end
  end

  defp dictionary(data, context \\ nil) do
    # Admit the complete encoded dictionary before scanning variable-size real
    # operands. Its 48-operand stack is a fixed CFF protocol bound.
    FontValidator.reserve(:max_font_cff_work, byte_size(data))
    context = context || %{version: 1, store: {}, vsindex: 0, kind: :cff1}
    dictionary_items(data, [], %{}, context)
  end

  defp dictionary_items(data, operands, values, context) do
    case data do
      <<>> ->
        unless operands == [],
          do: invalid("CFF DICT ends with operands but no operator", "CFF DICT")

        values

      <<12, escaped, rest::binary>> ->
        if context.version == 2 and Map.has_key?(values, {12, escaped}),
          do: invalid("CFF2 DICT keys must be unique", "CFF2 DICT")

        dictionary_items(
          rest,
          [],
          Map.put(values, {12, escaped}, Enum.reverse(operands)),
          context
        )

      <<12>> ->
        invalid("CFF DICT escape operator is truncated", "CFF DICT")

      <<22, rest::binary>> when context.version == 2 and context.kind == :private ->
        if Map.has_key?(values, 22) or Map.get(context, :blend_seen?, false),
          do:
            invalid(
              "CFF2 Private vsindex may occur only once before blending",
              "CFF2 Private DICT"
            )

        case operands do
          [index] when is_integer(index) and index >= 0 and index < tuple_size(context.store) ->
            dictionary_items(rest, [], Map.put(values, 22, [index]), %{context | vsindex: index})

          _ ->
            invalid(
              "CFF2 Private vsindex selects an unavailable variation data table",
              "CFF2 Private DICT"
            )
        end

      <<23, rest::binary>> when context.version == 2 and context.kind == :private ->
        case NativeElixirPdfUtilities.Validators.VariationStoreValidator.blend(
               Enum.reverse(operands),
               context.store,
               context.vsindex
             ) do
          {:ok, blended} ->
            dictionary_items(
              rest,
              Enum.reverse(blended),
              values,
              Map.put(context, :blend_seen?, true)
            )

          error ->
            throw({:cff_invalid, error})
        end

      <<operator, rest::binary>>
      when operator <= 21 or (context.version == 2 and operator in [24, 25]) ->
        if context.version == 2 and Map.has_key?(values, operator),
          do: invalid("CFF2 DICT keys must be unique", "CFF2 DICT")

        dictionary_items(rest, [], Map.put(values, operator, Enum.reverse(operands)), context)

      _ ->
        maximum = if context.version == 2, do: 513, else: 48

        unless length(operands) < maximum,
          do: invalid("CFF DICT exceeds its fixed #{maximum}-operand stack", "CFF DICT")

        {number, rest} = operand(data)
        FontValidator.check(:max_font_numeric_magnitude, ceil(abs(number)))
        dictionary_items(rest, [number | operands], values, context)
    end
  end

  defp operand(data) do
    case data do
      <<value, rest::binary>> when value in 32..246 ->
        {value - 139, rest}

      <<value, next, rest::binary>> when value in 247..250 ->
        {(value - 247) * 256 + next + 108, rest}

      <<value, next, rest::binary>> when value in 251..254 ->
        {-(value - 251) * 256 - next - 108, rest}

      <<28, value::signed-16, rest::binary>> ->
        {value, rest}

      <<29, value::signed-32, rest::binary>> ->
        {value, rest}

      <<30, rest::binary>> ->
        real(rest, [])

      _ ->
        invalid("CFF DICT operand is truncated or uses a reserved encoding", "CFF DICT")
    end
  end

  defp real(data, parts) do
    case data do
      <<high::4, low::4, rest::binary>> ->
        values =
          Enum.map([high, low], fn nibble ->
            case nibble do
              value when value <= 9 -> Integer.to_string(value)
              10 -> "."
              11 -> "E"
              12 -> "E-"
              13 -> :reserved
              14 -> "-"
              15 -> :finish
            end
          end)

        case values do
          [:finish, :finish] -> finish_real(parts, rest)
          [:finish, _] -> invalid("CFF real padding nibble must terminate the number", "CFF DICT")
          [:reserved, _] -> invalid("CFF real contains a reserved nibble", "CFF DICT")
          [_, :reserved] -> invalid("CFF real contains a reserved nibble", "CFF DICT")
          [value, :finish] -> finish_real([value | parts], rest)
          [first, second] -> real(rest, [second, first | parts])
        end

      _ ->
        invalid("CFF real operand is truncated", "CFF DICT")
    end
  end

  defp finish_real(parts, rest) do
    token = parts |> Enum.reverse() |> Enum.join() |> String.replace(".E", ".0E")
    token = if String.ends_with?(token, "."), do: token <> "0", else: token

    case HtmlValidator.parse_number(token) do
      {value, ""} -> {value, rest}
      _ -> invalid("CFF real has invalid decimal or exponent syntax", "CFF DICT")
    end
  end

  defp private_font(data, values, owners) do
    {private, local_subrs, owners} =
      case Map.get(values, 18) do
        nil ->
          {%{}, {}, owners}

        [size, start] when is_integer(size) and size >= 0 and is_integer(start) and start >= 0 ->
          bytes = read(data, start, size)
          owners = own(owners, :private, start, start + size)
          private = dictionary(bytes)

          case Map.get(private, 19) do
            nil ->
              {private, {}, owners}

            [relative] when is_integer(relative) and relative >= size ->
              {subrs, finish} = index(data, start + relative)
              {private, subrs, own(owners, :local_subrs, start + relative, finish)}

            _ ->
              invalid("CFF local Subrs must follow their owning Private DICT", "CFF Subrs")
          end

        _ ->
          invalid("CFF Private requires a nonnegative integer size and offset", "CFF Private")
      end

    matrix =
      if Map.has_key?(values, {12, 7}), do: vector(values, {12, 7}, nil, 6, "CFF FD FontMatrix")

    seed = number(private, {12, 19}, 0, "CFF initialRandomSeed")

    unless is_integer(seed) and seed in -2_147_483_648..2_147_483_647,
      do: invalid("CFF initialRandomSeed requires a signed 32-bit integer", "CFF Private DICT")

    {%{
       dictionary: values,
       private: private,
       local_subrs: local_subrs,
       default_width: number(private, 20, 0, "CFF defaultWidthX"),
       nominal_width: number(private, 21, 0, "CFF nominalWidthX"),
       random_seed: seed,
       font_matrix: matrix
     }, owners}
  end

  defp charset(data, top, glyph_count, strings, cid?, owners) do
    start = integer(top, 15, 0, "CFF charset")

    if start in 0..2 do
      if cid?, do: invalid("CID-keyed CFF requires an explicit CID charset", "CFF charset")
      predefined = elem(CffData.predefined_charsets(), start)
      maximum = tuple_size(predefined)

      unless glyph_count <= maximum,
        do: invalid("CFF glyph count exceeds its predefined charset", "CFF charset")

      FontValidator.reserve(:max_font_cff_work, glyph_count)
      {predefined |> Tuple.to_list() |> Enum.take(glyph_count) |> tl(), owners}
    else
      maximum = if cid?, do: 65_535, else: 390 + strings
      <<format>> = read(data, start, 1)

      {identifiers, finish} =
        case format do
          0 ->
            bytes = read(data, start + 1, (glyph_count - 1) * 2)
            {for(<<sid::16 <- bytes>>, do: sid), start + 1 + byte_size(bytes)}

          value when value in [1, 2] ->
            charset_ranges(data, start + 1, glyph_count - 1, format, [])

          _ ->
            invalid("CFF charset format must be 0, 1 or 2", "CFF charset")
        end

      FontValidator.reserve(:max_font_cff_work, glyph_count)

      unless Enum.all?(identifiers, &(&1 > 0 and &1 <= maximum)) and
               length(identifiers) == MapSet.size(MapSet.new(identifiers)),
             do:
               invalid(
                 "CFF charset identifiers must be unique and within their SID or CID namespace",
                 "CFF charset"
               )

      {identifiers, own(owners, :charset, start, finish)}
    end
  end

  defp charset_ranges(data, cursor, remaining, format, ranges) do
    if remaining == 0 do
      {ranges |> Enum.reverse() |> List.flatten(), cursor}
    else
      FontValidator.reserve(:max_font_cff_work, 1)
      <<first::16>> = read(data, cursor, 2)
      width = format
      count = :binary.decode_unsigned(read(data, cursor + 2, width)) + 1

      unless count <= remaining and first + count - 1 <= 65_535,
        do:
          invalid(
            "CFF charset range exceeds its remaining glyphs or uint16 identifiers",
            "CFF charset"
          )

      charset_ranges(data, cursor + 2 + width, remaining - count, format, [
        Enum.to_list(first..(first + count - 1)) | ranges
      ])
    end
  end

  defp encoding(data, top, glyph_count, cid?, charset, owners) do
    start = integer(top, 16, 0, "CFF Encoding")

    if cid? do
      if Map.has_key?(top, 16),
        do: invalid("CID-keyed CFF must not declare a name-keyed Encoding", "CFF Encoding")

      {nil, owners}
    else
      if start in [0, 1] do
        {{:predefined, start}, owners}
      else
        <<header, count>> = read(data, start, 2)
        format = Bitwise.band(header, 0x7F)

        {codes, finish} =
          case format do
            0 ->
              bytes = read(data, start + 2, count)
              {:binary.bin_to_list(bytes), start + 2 + count}

            1 ->
              bytes = read(data, start + 2, count * 2)
              codes = for <<first, left <- bytes>>, value <- first..(first + left), do: value
              {codes, start + 2 + byte_size(bytes)}

            _ ->
              invalid("CFF Encoding format must be 0 or 1", "CFF Encoding")
          end

        FontValidator.reserve(:max_font_cff_work, length(codes))

        unless length(codes) < glyph_count and Enum.all?(codes, &(&1 <= 255)) and
                 length(codes) == MapSet.size(MapSet.new(codes)),
               do:
                 invalid(
                   "CFF Encoding codes must be unique uint8 values within the character-program count",
                   "CFF Encoding"
                 )

        {supplements, finish} =
          if Bitwise.band(header, 0x80) != 0 do
            <<count>> = read(data, finish, 1)
            bytes = read(data, finish + 1, count * 3)
            supplements = for <<code, sid::16 <- bytes>>, do: {code, sid}
            FontValidator.reserve(:max_font_cff_work, count)

            unless length(supplements) ==
                     MapSet.size(MapSet.new(Enum.map(supplements, &elem(&1, 0)))),
                   do: invalid("CFF Encoding supplements repeat a character code", "CFF Encoding")

            {supplements, finish + 1 + byte_size(bytes)}
          else
            {[], finish}
          end

        encoded_sids = charset |> Enum.take(length(codes)) |> MapSet.new()

        unless Enum.all?(supplements, fn {_code, sid} -> MapSet.member?(encoded_sids, sid) end),
          do:
            invalid(
              "CFF Encoding supplements must refer to glyphs already present in the main encoding",
              "CFF Encoding"
            )

        {%{codes: codes, supplements: supplements}, own(owners, :encoding, start, finish)}
      end
    end
  end

  @doc "Admits CFF font-dictionary selection, including CFF2's 32-bit range format."
  @spec prepare_fd_select(binary(), non_neg_integer(), pos_integer(), pos_integer(), 1 | 2) ::
          {:ok, tuple(), non_neg_integer()} | {:error, {atom(), map()}}
  def prepare_fd_select(data, start, glyph_count, font_count, version) do
    with_data(data, fn ->
      {select, finish} = fd_select(data, start, glyph_count, font_count, version)
      {:ok, select, finish}
    end)
  end

  defp fd_select(data, start, glyph_count, font_count, version \\ 1) do
    <<format>> = read(data, start, 1)

    {indices, finish} =
      case format do
        0 ->
          bytes = read(data, start + 1, glyph_count)
          {:binary.bin_to_list(bytes), start + 1 + glyph_count}

        format when format == 3 or (format == 4 and version == 2) ->
          count_width = if format == 3, do: 2, else: 4
          entry_width = if format == 3, do: 3, else: 6
          count = :binary.decode_unsigned(read(data, start + 1, count_width))
          unless count > 0, do: invalid("CFF FDSelect ranges must not be empty", "CFF FDSelect")
          FontValidator.reserve(:max_font_cff_work, count)
          bytes = read(data, start + 1 + count_width, count * entry_width)

          sentinel =
            :binary.decode_unsigned(
              read(data, start + 1 + count_width + byte_size(bytes), count_width)
            )

          ranges =
            if format == 3,
              do: for(<<first::16, font <- bytes>>, do: {first, font}),
              else: for(<<first::32, font::16 <- bytes>>, do: {first, font})

          [{first, _} | _] = ranges

          unless first == 0 and sentinel == glyph_count and
                   Enum.chunk_every(ranges ++ [{sentinel, 0}], 2, 1, :discard)
                   |> Enum.all?(fn [{a, _}, {b, _}] -> a < b end),
                 do:
                   invalid(
                     "CFF FDSelect ranges require an initial zero, ascending starts and the exact glyph-count sentinel",
                     "CFF FDSelect"
                   )

          indices =
            (ranges ++ [{sentinel, 0}])
            |> Enum.chunk_every(2, 1, :discard)
            |> Enum.flat_map(fn [{a, font}, {b, _}] -> List.duplicate(font, b - a) end)

          {indices, start + 1 + count_width * 2 + byte_size(bytes)}

        _ ->
          invalid("CFF 1 FDSelect format must be 0 or 3", "CFF FDSelect")
      end

    FontValidator.reserve(:max_font_cff_work, glyph_count)

    unless Enum.all?(indices, &(&1 < font_count)),
      do: invalid("CFF FDSelect references a missing font dictionary", "CFF FDSelect")

    {List.to_tuple(indices), finish}
  end

  defp validate_sids(values, keys, maximum) do
    Enum.each(keys, fn key ->
      if Map.has_key?(values, key) do
        sid = integer(values, key, nil, "CFF SID")

        unless sid <= maximum,
          do: invalid("CFF SID has no standard or supplied string", "CFF SID")
      end
    end)
  end

  defp vector(values, key, default, count, source) do
    case Map.get(values, key, default) do
      items when is_list(items) and length(items) == count -> items
      _ -> invalid("#{source} requires exactly #{count} numeric operands", source)
    end
  end

  defp number(values, key, default, source) do
    case Map.get(values, key, if(is_nil(default), do: nil, else: [default])) do
      [value] -> value
      _ -> invalid("#{source} requires one numeric operand", source)
    end
  end

  defp integer(values, key, default, source) do
    value = number(values, key, default, source)

    unless is_integer(value) and value >= 0,
      do: invalid("#{source} requires a nonnegative integer operand", source)

    value
  end

  defp own(owners, kind, start, finish) do
    if start == finish do
      owners
    else
      FontValidator.reserve(:max_font_cff_work, length(owners) + 1)
      owner = {kind, start, finish}

      unless Enum.all?(owners, fn {_, first, last} = existing ->
               existing == owner or start >= last or finish <= first
             end),
             do:
               invalid("CFF #{kind} overlaps a different owning data structure", "CFF ownership")

      if owner in owners, do: owners, else: [owner | owners]
    end
  end

  defp read(data, offset, length) do
    unless is_integer(offset) and is_integer(length) and offset >= 0 and length >= 0 and
             offset + length <= byte_size(data),
           do:
             invalid(
               "CFF byte range exceeds its owning data at offset #{inspect(offset)}",
               "CFF data"
             )

    binary_part(data, offset, length)
  end

  defp invalid(message, source) do
    throw(
      {:cff_invalid,
       Diagnostics.error(:font, :invalid_font, message,
         operation: :prepare_cff_source,
         module: __MODULE__,
         source: source
       )}
    )
  end
end
