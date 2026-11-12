defmodule NativeElixirPdfUtilities.Validators.Woff2Validator do
  @moduledoc false
  import Bitwise
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.{BrotliValidator, FontValidator}

  # WOFF2 Recommendation, 8 August 2024. Tag indices, 255UInt16/Base128 bounds,
  # sfnt directory widths and glyph coordinate widths are fixed format invariants.
  @tags {"cmap", "head", "hhea", "hmtx", "maxp", "name", "OS/2", "post", "cvt ", "fpgm", "glyf",
         "loca", "prep", "CFF ", "VORG", "EBDT", "EBLC", "gasp", "hdmx", "kern", "LTSH", "PCLT",
         "VDMX", "vhea", "vmtx", "BASE", "GDEF", "GPOS", "GSUB", "EBSC", "JSTF", "MATH", "CBDT",
         "CBLC", "COLR", "CPAL", "SVG ", "sbix", "acnt", "avar", "bdat", "bloc", "bsln", "cvar",
         "fdsc", "feat", "fmtx", "fvar", "gvar", "hsty", "just", "lcar", "mort", "morx", "opbd",
         "prop", "trak", "Zapf", "Silf", "Glat", "Gloc", "Feat", "Sill"}

  @doc "Validates and reconstructs one WOFF2 face's tables, including transformed glyphs and horizontal metrics."
  @spec prepare(term(), term()) ::
          {:ok, non_neg_integer(), [{binary(), binary()}]} | {:error, {atom(), map()}}
  def prepare(data, index) do
    FontValidator.with_budget(fn ->
      try do
        unless is_binary(data) and is_integer(index) and index >= 0,
          do: invalid("WOFF2 requires binary bytes and a nonnegative face index", "header")

        FontValidator.reserve_source(data)
        {header, bytes} = read(data, 48, "header")

        <<signature::binary-size(4), flavor::32, size::32, count::16, reserved::16, total::32,
          compressed_size::32, _major::16, _minor::16, meta_offset::32, meta_length::32,
          meta_original::32, private_offset::32, private_length::32>> = header

        unless signature == "wOF2" and size == byte_size(data) and count > 0 and reserved == 0 and
                 compressed_size > 0,
               do:
                 invalid(
                   "WOFF2 signature, source length, table count or reserved field is invalid",
                   "header"
                 )

        FontValidator.check(:max_decoded_bytes, total)
        FontValidator.reserve(:max_work, count)
        {records, bytes, decoded_size} = directory(bytes, count, [], 0)
        records = List.to_tuple(records)
        {faces, bytes} = faces(bytes, flavor, count)

        unless index < length(faces),
          do: invalid("WOFF2 collection index is outside its font directory", "collection")

        validate_faces(faces, records)
        directory_end = byte_size(data) - byte_size(bytes)
        finish = directory_end + compressed_size

        blocks(
          data,
          finish,
          {meta_offset, meta_length, meta_original},
          {private_offset, private_length}
        )

        {compressed, _} = read(bytes, compressed_size, "compressed font data")

        case BrotliValidator.decompress(compressed, decoded_size) do
          {:ok, decoded} ->
            {payloads, <<>>} =
              Enum.map_reduce(Tuple.to_list(records), decoded, fn record, bytes ->
                {payload, bytes} = read(bytes, record.length, record.tag)
                {Map.put(record, :data, payload), bytes}
              end)

            {scaler, indices} = Enum.at(faces, index)

            tables =
              Map.new(indices, fn i ->
                r = Enum.at(payloads, i)
                {r.tag, r}
              end)

            {:ok, scaler, reconstruct(tables)}

          error ->
            error
        end
      catch
        {:invalid_woff2, error} -> error
      end
    end)
  end

  defp directory(bytes, left, records, decoded_size) do
    if left == 0 do
      {Enum.reverse(records), bytes, decoded_size}
    else
      {flag, bytes} = uint(bytes, 1, "directory")
      tag_index = flag &&& 63

      {tag, bytes} =
        if tag_index == 63,
          do: read(bytes, 4, "directory tag"),
          else: {elem(@tags, tag_index), bytes}

      unless Enum.all?(:binary.bin_to_list(tag), &(&1 in 32..126)),
        do: invalid("WOFF2 table tag is not printable ASCII", tag)

      {original, bytes} = base128(bytes, 0, 0)
      version = flag >>> 6
      transformed = if tag in ["glyf", "loca"], do: version == 0, else: version != 0

      unless (tag in ["glyf", "loca"] and version in [0, 3]) or
               (tag == "hmtx" and version in [0, 1]) or version == 0,
             do: invalid("WOFF2 table uses an unknown transform version", tag)

      {length, bytes} = if transformed, do: base128(bytes, 0, 0), else: {original, bytes}

      if tag == "loca" and transformed and length != 0,
        do: invalid("WOFF2 transformed loca length must be zero", tag)

      size = decoded_size + length
      FontValidator.check(:max_decoded_bytes, size)
      record = %{tag: tag, original: original, length: length, transformed: transformed}
      directory(bytes, left - 1, [record | records], size)
    end
  end

  defp base128(bytes, value, count) do
    {byte, bytes} = uint(bytes, 1, "UIntBase128")

    if (count == 0 and byte == 128) or count == 5 or value > 0x01FF_FFFF,
      do:
        invalid(
          "WOFF2 UIntBase128 has leading zeroes, overflow or more than five bytes",
          "directory"
        )

    value = value <<< 7 ||| (byte &&& 127)
    if byte < 128, do: {value, bytes}, else: base128(bytes, value, count + 1)
  end

  defp ushort255(bytes, source) do
    {code, bytes} = uint(bytes, 1, source)

    case code do
      253 ->
        uint(bytes, 2, source)

      254 ->
        {value, bytes} = uint(bytes, 1, source)
        {value + 506, bytes}

      255 ->
        {value, bytes} = uint(bytes, 1, source)
        {value + 253, bytes}

      _ ->
        {code, bytes}
    end
  end

  defp faces(bytes, flavor, count) do
    if flavor == 0x7474_6366 do
      {version, bytes} = uint(bytes, 4, "collection version")

      unless version in [0x0001_0000, 0x0002_0000],
        do: invalid("WOFF2 collection version is invalid", "collection")

      {number, bytes} = ushort255(bytes, "collection count")
      if number == 0, do: invalid("WOFF2 collection has no fonts", "collection")
      FontValidator.check(:max_entries, number)

      Enum.map_reduce(1..number, bytes, fn _, bytes ->
        {tables, bytes} = ushort255(bytes, "collection table count")

        unless tables in 1..4095,
          do: invalid("WOFF2 face table count cannot fit an sfnt directory", "collection")

        {scaler, bytes} = uint(bytes, 4, "collection flavor")
        FontValidator.reserve(:max_work, tables)

        {indices, bytes} =
          Enum.map_reduce(1..tables, bytes, fn _, bytes ->
            ushort255(bytes, "collection table index")
          end)

        unless Enum.all?(indices, &(&1 < count)) and length(Enum.uniq(indices)) == tables,
          do: invalid("WOFF2 collection table index is unavailable or duplicated", "collection")

        {{scaler, indices}, bytes}
      end)
    else
      unless count <= 4095,
        do: invalid("WOFF2 table count cannot fit an sfnt directory", "directory")

      {[{flavor, Enum.to_list(0..(count - 1))}], bytes}
    end
  end

  defp validate_faces(faces, records) do
    Enum.each(faces, fn {scaler, indices} ->
      unless scaler in [0x0001_0000, 0x7472_7565, 0x4F54_544F],
        do: invalid("WOFF2 face has an unsupported sfnt flavor", "directory")

      tags = Enum.map(indices, &elem(records, &1).tag)

      unless length(Enum.uniq(tags)) == length(tags),
        do: invalid("WOFF2 face repeats a table tag", "directory")

      glyf = Enum.find(indices, &(elem(records, &1).tag == "glyf"))
      loca = Enum.find(indices, &(elem(records, &1).tag == "loca"))

      if glyf == nil != (loca == nil),
        do: invalid("WOFF2 glyf and loca must be paired", "directory")

      if glyf != nil do
        g = elem(records, glyf)
        l = elem(records, loca)

        paired =
          (glyf + 1)..(tuple_size(records) - 1)//1
          |> Enum.find(fn index -> elem(records, index).tag in ["glyf", "loca"] end)

        unless g.transformed == l.transformed and paired == loca,
          do: invalid("WOFF2 glyf and loca transforms or directory pairing differ", "directory")

        # Requiring the next global outline member to be this loca already
        # gives a fixed one-to-one pair, including every shared collection face.
      end
    end)

    :ok
  end

  defp blocks(data, finish, metadata, private) do
    {mo, ml, original} = metadata
    {po, pl} = private

    unless finish <= byte_size(data) and
             (metadata == {0, 0, 0} or (mo > 0 and ml > 0 and original > 0)) and
             (private == {0, 0} or (po > 0 and pl > 0)),
           do: invalid("WOFF2 optional block fields are inconsistent", "header")

    ranges = if(mo == 0, do: [], else: [{mo, ml}]) ++ if po == 0, do: [], else: [{po, pl}]

    last =
      Enum.reduce(ranges, finish, fn {offset, length}, previous ->
        unless offset == previous + rem(4 - rem(previous, 4), 4) and
                 offset + length <= byte_size(data),
               do:
                 invalid("WOFF2 metadata/private block overlaps or exceeds its source", "header")

        padding(data, previous, offset)
        offset + length
      end)

    unless byte_size(data) - last < 4,
      do: invalid("WOFF2 source has unowned trailing bytes", "header")

    padding(data, last, byte_size(data))
  end

  defp padding(data, first, last) do
    if binary_part(data, first, last - first) != :binary.copy(<<0>>, last - first),
      do: invalid("WOFF2 alignment padding must be zero", "header")
  end

  defp reconstruct(tables) do
    tables =
      case Map.get(tables, "glyf") do
        %{transformed: true, data: data} ->
          {glyf, loca, _xmins, count, format} = glyf(data)
          maxp = field(tables, "maxp", 4, 2)

          unless maxp == <<count::16>>,
            do: invalid("WOFF2 transformed glyph count differs from maxp", "glyf")

          unless tables["loca"].original == (count + 1) * if(format == 0, do: 2, else: 4),
            do:
              invalid(
                "WOFF2 loca original length differs from glyph count and index format",
                "loca"
              )

          head = required(tables, "head")
          {prefix, suffix} = read(head.data, 50, "head")
          {_old, suffix} = read(suffix, 2, "head")

          tables
          |> put_data("glyf", glyf)
          |> put_data("loca", loca)
          |> put_data("head", prefix <> <<format::16>> <> suffix)

        _ ->
          tables
      end

    tables =
      case Map.get(tables, "hmtx") do
        %{transformed: true, data: data} -> put_data(tables, "hmtx", hmtx(data, tables))
        _ -> tables
      end

    Enum.map(tables, fn {tag, record} -> {tag, record.data} end) |> Enum.sort()
  end

  defp required(tables, tag) do
    case Map.fetch(tables, tag) do
      {:ok, record} -> record
      :error -> invalid("WOFF2 transform requires the #{tag} table", tag)
    end
  end

  defp field(tables, tag, offset, length) do
    data = required(tables, tag).data
    {_prefix, rest} = read(data, offset, tag)
    {value, _rest} = read(rest, length, tag)
    value
  end

  defp put_data(tables, tag, data) do
    FontValidator.check(:max_decoded_bytes, byte_size(data))
    Map.update!(tables, tag, &%{&1 | data: data})
  end

  defp glyf(data) do
    {header, bytes} = read(data, 36, "glyf header")
    <<version::16, options::16, count::16, format::16, sizes::binary-size(28)>> = header

    unless version == 0 and options in [0, 1] and count > 0 and format in [0, 1],
      do:
        invalid(
          "WOFF2 transformed glyf version, options, count or index format is invalid",
          "glyf"
        )

    FontValidator.reserve(:max_work, count)
    names = [:contours, :points, :flags, :glyphs, :components, :boxes, :instructions]

    {streams, bytes} =
      Enum.map_reduce(for(<<size::32 <- sizes>>, do: size), bytes, fn size, bytes ->
        read(bytes, size, "glyf streams")
      end)

    state = Enum.zip(names, streams) |> Map.new()

    unless byte_size(state.contours) == count * 2,
      do: invalid("WOFF2 contour stream length differs from glyph count", "glyf")

    {overlap, bytes} =
      if options == 1, do: read(bytes, div(count + 7, 8), "overlap bitmap"), else: {<<>>, bytes}

    unless bytes == <<>>, do: invalid("WOFF2 transformed glyf has trailing bytes", "glyf")
    {bitmap, boxes} = read(state.boxes, div(count + 31, 32) * 4, "bounding box bitmap")
    state = %{state | boxes: boxes}

    {glyphs, offsets, xmins, _total, state} =
      Enum.reduce(0..(count - 1), {[], [0], [], 0, state}, fn id,
                                                              {glyphs, offsets, xmins, total,
                                                               state} ->
        {raw, state} = stream(state, :contours, 2)
        <<contours::signed-16>> = raw
        has_box = bitmap?(bitmap, id)

        {body, inferred, state} =
          cond do
            contours == 0 ->
              if has_box, do: invalid("WOFF2 empty glyph cannot supply a bounding box", "glyf")
              {<<>>, {0, 0, 0, 0}, state}

            contours == -1 ->
              unless has_box,
                do: invalid("WOFF2 composite requires an explicit bounding box", "glyf")

              {components, instructions?, state} = components(state, count, [], false)

              {instructions, state} =
                if instructions?, do: instructions(state), else: {<<>>, state}

              {components <> instructions, nil, state}

            contours > 0 ->
              simple(state, contours, bitmap?(overlap, id))

            true ->
              invalid("WOFF2 contour count is below -1", "glyf")
          end

        {box, state} =
          if has_box do
            {raw, state} = stream(state, :boxes, 8)
            <<a::signed-16, b::signed-16, c::signed-16, d::signed-16>> = raw
            {{a, b, c, d}, state}
          else
            {inferred, state}
          end

        {a, b, c, d} = box
        unless a <= c and b <= d, do: invalid("WOFF2 glyph bounds are reversed", "glyf")

        glyph =
          if contours == 0,
            do: <<>>,
            else:
              <<contours::signed-16, a::signed-16, b::signed-16, c::signed-16, d::signed-16,
                body::binary>>

        glyph = glyph <> :binary.copy(<<0>>, rem(2 - rem(byte_size(glyph), 2), 2))
        next = total + byte_size(glyph)
        FontValidator.check(:max_decoded_bytes, next)

        if format == 0 and next > 131_070,
          do: invalid("WOFF2 glyph offsets cannot fit short loca", "loca")

        {[glyph | glyphs], [next | offsets], [a | xmins], next, state}
      end)

    unless Enum.all?(Map.values(state), &(&1 == <<>>)),
      do: invalid("WOFF2 glyph streams have unused trailing bytes", "glyf")

    loca =
      for offset <- Enum.reverse(offsets),
          into: <<>>,
          do: if(format == 0, do: <<div(offset, 2)::16>>, else: <<offset::32>>)

    {IO.iodata_to_binary(Enum.reverse(glyphs)), loca, Enum.reverse(xmins), count, format}
  end

  defp bitmap?(bitmap, id) do
    bitmap != <<>> and (:binary.at(bitmap, div(id, 8)) &&& 128 >>> rem(id, 8)) != 0
  end

  defp simple(state, contours, overlap?) do
    {ends, total, state} =
      Enum.reduce(1..contours, {[], 0, state}, fn _, {ends, total, state} ->
        {n, rest} = ushort255(state.points, "contour points")

        if n == 0 or total + n > 65536,
          do: invalid("WOFF2 contour is empty or point indices exceed uint16", "glyf")

        {[total + n - 1 | ends], total + n, %{state | points: rest}}
      end)

    FontValidator.reserve(:max_work, total)
    {flags, state} = stream(state, :flags, total)

    {points, {x, y, state}} =
      Enum.map_reduce(:binary.bin_to_list(flags), {0, 0, state}, fn flag, {x, y, state} ->
        {dx, dy, state} = triplet(state, flag &&& 127)
        nx = x + dx
        ny = y + dy

        unless nx in -32768..32767 and ny in -32768..32767,
          do: invalid("WOFF2 glyph coordinates exceed signed int16", "glyf")

        {{nx, ny, dx, dy, if(flag < 128, do: 1, else: 0)}, {nx, ny, state}}
      end)

    _ = {x, y}
    {instructions, state} = instructions(state)
    ends = for n <- Enum.reverse(ends), into: <<>>, do: <<n::16>>
    # Use ordinary TrueType short/long deltas. No flag repetition is needed;
    # this also retains valid coordinate differences outside signed int16.
    encoded =
      Enum.with_index(points)
      |> Enum.map(fn {{_x, _y, dx, dy, on}, index} ->
        {xf, xb} = delta(dx, 2, 16)
        {yf, yb} = delta(dy, 4, 32)
        {on ||| xf ||| yf ||| if(index == 0 and overlap?, do: 64, else: 0), xb, yb}
      end)

    flags = for {f, _, _} <- encoded, into: <<>>, do: <<f>>
    xs = for {_, xb, _} <- encoded, do: xb
    ys = for {_, _, yb} <- encoded, do: yb

    {xmin, ymin, xmax, ymax} =
      Enum.reduce(points, {32767, 32767, -32768, -32768}, fn {x, y, _, _, _}, {a, b, c, d} ->
        {min(a, x), min(b, y), max(c, x), max(d, y)}
      end)

    {IO.iodata_to_binary([ends, instructions, flags, xs, ys]), {xmin, ymin, xmax, ymax}, state}
  end

  defp delta(value, short, same) do
    cond do
      value == 0 -> {same, <<>>}
      value in 1..255 -> {short ||| same, <<value>>}
      value in -255..-1 -> {short, <<-value>>}
      value in -32768..32767 -> {0, <<value::signed-16>>}
      true -> invalid("WOFF2 coordinate delta cannot be represented in TrueType", "glyf")
    end
  end

  defp triplet(state, flag) do
    count =
      cond do
        flag < 84 -> 1
        flag < 120 -> 2
        flag < 124 -> 3
        true -> 4
      end

    {raw, state} = stream(state, :glyphs, count)
    values = :binary.bin_to_list(raw)

    {dx, dy} =
      cond do
        flag < 10 ->
          [a] = values
          {0, ((flag &&& 14) <<< 7) + a}

        flag < 20 ->
          [a] = values
          {((flag - 10 &&& 14) <<< 7) + a, 0}

        flag < 84 ->
          [a] = values
          b = flag - 20
          {1 + (b &&& 48) + (a >>> 4), 1 + ((b &&& 12) <<< 2) + (a &&& 15)}

        flag < 120 ->
          [a, b] = values
          f = flag - 84
          {1 + (div(f, 12) <<< 8) + a, 1 + ((rem(f, 12) >>> 2) <<< 8) + b}

        flag < 124 ->
          [a, b, c] = values
          {(a <<< 4) + (b >>> 4), ((b &&& 15) <<< 8) + c}

        true ->
          [a, b, c, d] = values
          {(a <<< 8) + b, (c <<< 8) + d}
      end

    y_sign = if flag < 10, do: 1, else: 2

    {if((flag &&& 1) == 0, do: -dx, else: dx), if((flag &&& y_sign) == 0, do: -dy, else: dy),
     state}
  end

  defp components(state, count, records, instructions?) do
    {raw, state} = stream(state, :components, 4)
    <<flags::16, id::16>> = raw

    unless id < count and (flags &&& 0xE000) == 0 and
             Enum.count([8, 64, 128], &((flags &&& &1) != 0)) <= 1,
           do: invalid("WOFF2 composite component index, flags or transform is invalid", "glyf")

    extra =
      if((flags &&& 1) != 0, do: 4, else: 2) +
        cond do
          (flags &&& 8) != 0 -> 2
          (flags &&& 64) != 0 -> 4
          (flags &&& 128) != 0 -> 8
          true -> 0
        end

    {arguments, state} = stream(state, :components, extra)
    records = [raw <> arguments | records]
    instructions? = instructions? or (flags &&& 256) != 0

    if (flags &&& 32) != 0,
      do: components(state, count, records, instructions?),
      else: {IO.iodata_to_binary(Enum.reverse(records)), instructions?, state}
  end

  defp instructions(state) do
    {length, bytes} = ushort255(state.glyphs, "instruction length")
    {data, state} = stream(%{state | glyphs: bytes}, :instructions, length)
    {<<length::16, data::binary>>, state}
  end

  defp hmtx(data, tables) do
    {flags, bytes} = uint(data, 1, "hmtx flags")

    unless flags in 1..3,
      do:
        invalid(
          "WOFF2 hmtx flags must omit at least one bearing array and clear reserved bits",
          "hmtx"
        )

    <<count::16>> = field(tables, "maxp", 4, 2)
    <<metrics::16>> = field(tables, "hhea", 34, 2)

    unless metrics > 0 and metrics <= count,
      do: invalid("WOFF2 hmtx metric count is outside maxp glyphs", "hmtx")

    {advances, bytes} = read(bytes, metrics * 2, "hmtx advances")
    xmins = glyph_xmins(tables, count)

    {proportional, bytes} =
      if (flags &&& 1) == 0,
        do: read(bytes, metrics * 2, "hmtx proportional bearings"),
        else: {nil, bytes}

    {trailing, bytes} =
      if (flags &&& 2) == 0,
        do: read(bytes, (count - metrics) * 2, "hmtx trailing bearings"),
        else: {nil, bytes}

    unless bytes == <<>>, do: invalid("WOFF2 transformed hmtx has trailing bytes", "hmtx")

    output =
      for {xmin, id} <- Enum.with_index(xmins) do
        bearings = if id < metrics, do: proportional, else: trailing
        offset = if id < metrics, do: id * 2, else: (id - metrics) * 2
        lsb = if bearings, do: binary_part(bearings, offset, 2), else: <<xmin::signed-16>>
        if id < metrics, do: [binary_part(advances, id * 2, 2), lsb], else: lsb
      end

    output = IO.iodata_to_binary(output)

    unless byte_size(output) == tables["hmtx"].original,
      do: invalid("WOFF2 hmtx original length differs from its metrics", "hmtx")

    output
  end

  defp glyph_xmins(tables, count) do
    <<format::signed-16>> = field(tables, "head", 50, 2)
    unless format in [0, 1], do: invalid("WOFF2 hmtx requires a valid loca format", "head")
    loca = required(tables, "loca").data
    glyf = required(tables, "glyf").data
    width = if format == 0, do: 2, else: 4

    unless byte_size(loca) == (count + 1) * width,
      do: invalid("WOFF2 loca length differs from maxp glyphs", "loca")

    offsets =
      for <<value::unsigned-integer-size(^width * 8) <- loca>>,
        do: value * if(format == 0, do: 2, else: 1)

    Enum.chunk_every(offsets, 2, 1, :discard)
    |> Enum.map(fn [first, last] ->
      unless first <= last and last <= byte_size(glyf),
        do: invalid("WOFF2 loca glyph range is outside glyf", "loca")

      if first == last do
        0
      else
        unless last - first >= 10, do: invalid("WOFF2 glyph header is truncated", "glyf")
        <<xmin::signed-16>> = binary_part(glyf, first + 2, 2)
        xmin
      end
    end)
  end

  defp stream(state, key, length) do
    {data, rest} = read(Map.fetch!(state, key), length, Atom.to_string(key))
    {data, Map.put(state, key, rest)}
  end

  defp uint(bytes, length, source) do
    {raw, bytes} = read(bytes, length, source)
    {:binary.decode_unsigned(raw), bytes}
  end

  defp read(bytes, length, source) do
    FontValidator.reserve(:max_work, max(length, 1))
    if byte_size(bytes) < length, do: invalid("WOFF2 #{source} is truncated", source)
    <<value::binary-size(^length), rest::binary>> = bytes
    {value, rest}
  end

  defp invalid(message, source) do
    throw(
      {:invalid_woff2,
       Diagnostics.error(:font, :invalid_font, message,
         module: __MODULE__,
         operation: :decode_woff2,
         source: "WOFF2 #{source}"
       )}
    )
  end
end
