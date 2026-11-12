defmodule NativeElixirPdfUtilities.Validators.BitmapFontValidator do
  @moduledoc false
  import Bitwise
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.FontValidator

  @doc "Prepares sbix or CBDT/CBLC strikes with bounded, owned glyph records."
  @spec prepare(map()) :: {:ok, [map()]} | {:error, {atom(), map()}}
  def prepare(font) do
    FontValidator.with_budget(fn ->
      try do
        with {:ok, _, directory} <- FontValidator.prepare_table_directory(font.data) do
          tables =
            Map.new(directory, fn {tag, {offset, size}} ->
              {tag, binary_part(font.data, offset, size)}
            end)

          count = length(font.widths)

          strikes =
            cond do
              Map.has_key?(tables, "sbix") ->
                sbix(tables["sbix"], count)

              Map.has_key?(tables, "CBDT") and Map.has_key?(tables, "CBLC") ->
                cbdt(tables["CBDT"], tables["CBLC"], count)

              true ->
                []
            end

          {:ok, strikes}
        end
      catch
        {:bitmap_font_error, error} -> error
      end
    end)
  end

  @doc "Selects CBDT device advances in design units for the same strike used by color painting."
  @spec prepare_metrics(map(), number()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_metrics(font, pixels) do
    if "CBDT" in Map.get(font, :color_tables, []) do
      FontValidator.with_budget(fn ->
        with {:ok, strikes} <-
               FontValidator.memo({:bitmap_font, font.data}, fn -> prepare(font) end) do
          work(length(font.widths))

          widths =
            Enum.with_index(font.widths, fn width, glyph ->
              case select(strikes, glyph, pixels) do
                nil ->
                  width

                bitmap ->
                  width = bitmap.advance * font.units_per_em / bitmap.ppem_x
                  if width == trunc(width), do: trunc(width), else: width
              end
            end)
            |> List.to_tuple()

          if Map.get(font, :bitmap_widths) == widths do
            {:ok, font}
          else
            fingerprint =
              :crypto.hash(:sha256, :erlang.term_to_binary(widths))
              |> binary_part(0, 6)
              |> Base.encode16(case: :lower)

            {:ok,
             font
             |> Map.put(:bitmap_widths, widths)
             |> Map.update!(:id, &(&1 <> "-bitmap-" <> fingerprint))}
          end
        end
      end)
    else
      {:ok, font}
    end
  end

  @doc "Validates optional selected-strike advances carried by a public writer face."
  @spec validate_metrics(map()) :: :ok | {:error, {atom(), map()}}
  def validate_metrics(font) do
    case Map.get(font, :bitmap_widths) do
      nil ->
        :ok

      widths when is_tuple(widths) and tuple_size(widths) == length(font.widths) ->
        if Enum.all?(
             Tuple.to_list(widths),
             &(is_number(&1) and &1 >= 0 and
                 &1 <= NativeElixirPdfUtilities.Limits.get(:max_numeric_magnitude))
           ),
           do: :ok,
           else:
             Diagnostics.error(
               :font,
               :invalid_font,
               "bitmap advances must be finite, nonnegative design-unit metrics",
               module: __MODULE__,
               operation: :write_pdf,
               source: font.family
             )

      _ ->
        Diagnostics.error(
          :font,
          :invalid_font,
          "bitmap advances must contain one value per glyph",
          module: __MODULE__,
          operation: :write_pdf,
          source: font.family
        )
    end
  end

  @doc "Adds empty outline records to a bitmap-only sfnt so space and other nonpainting glyphs retain the existing PDF text contract."
  @spec prepare_source(binary()) :: {:ok, binary()} | {:error, {atom(), map()}}
  def prepare_source(data) do
    FontValidator.with_budget(fn ->
      try do
        with {:ok, scaler, directory} <- FontValidator.prepare_table_directory(data) do
          tables =
            Map.new(directory, fn {tag, {offset, size}} ->
              {tag, binary_part(data, offset, size)}
            end)

          <<_version::32, count::16>> = read(tables["maxp"] || <<>>, 0, 6)

          unless count > 0 and byte_size(tables["head"] || <<>>) >= 54,
            do: invalid("bitmap-only font requires valid head and maxp tables")

          work(count)
          head = tables["head"]
          <<prefix::binary-size(50), _format::16, suffix::binary>> = head

          tables =
            tables
            |> Map.put("head", prefix <> <<1::16>> <> suffix)
            |> Map.put("maxp", <<0x10000::32, count::16, 0::size(26 * 8)>>)
            |> Map.put("glyf", <<>>)
            |> Map.put("loca", :binary.copy(<<0::32>>, count + 1))
            |> Map.delete("DSIG")

          bytes = NativeElixirPdfUtilities.HtmlToPdf.Font.Sfnt.rebuild(scaler, Enum.sort(tables))
          FontValidator.check(:max_decoded_bytes, byte_size(bytes))
          {:ok, bytes}
        end
      catch
        {:bitmap_font_error, error} -> error
      end
    end)
  end

  @doc "Selects a covered strike, preferring the smallest strike at or above the requested CSS raster size."
  @spec select([map()], non_neg_integer(), number()) :: map() | nil
  def select(strikes, glyph, pixels) do
    strikes
    |> Enum.filter(&Map.has_key?(&1.glyphs, glyph))
    |> Enum.min_by(
      fn strike ->
        {if(strike.ppem_y >= pixels, do: 0, else: 1),
         if(strike.ppem_y >= pixels, do: strike.ppem_y, else: -strike.ppem_y),
         -Map.get(strike, :ppi, 72)}
      end,
      fn -> nil end
    )
    |> case do
      nil ->
        nil

      strike ->
        Map.merge(
          Map.fetch!(strike.glyphs, glyph),
          Map.take(strike, [:ppem_x, :ppem_y, :ppi, :overlay, :format])
        )
    end
  end

  defp sbix(data, count) do
    <<version::16, flags::16, strikes::32>> = read(data, 0, 8)

    unless version == 1 and (flags &&& bnot(3)) == 0 and (flags &&& 1) == 1,
      do: invalid("sbix header or reserved flags are invalid")

    work(strikes + count)
    offsets = for <<offset::32 <- read(data, 8, strikes * 4)>>, do: offset

    unless offsets == Enum.sort(Enum.uniq(offsets)) and
             Enum.all?(offsets, &(&1 >= 8 + strikes * 4 and &1 < byte_size(data))),
           do: invalid("sbix strike offsets overlap their directory or one another")

    Enum.zip(offsets, tl(offsets ++ [byte_size(data)]))
    |> Enum.map(fn {offset, finish} ->
      strike = read(data, offset, finish - offset)
      <<ppem::16, ppi::16>> = read(strike, 0, 4)
      unless ppem > 0 and ppi > 0, do: invalid("sbix strike size and density must be positive")
      work(count + 1)
      [first_position | _] = positions = for <<n::32 <- read(strike, 4, (count + 1) * 4)>>, do: n

      unless positions == Enum.sort(positions) and first_position >= 4 + (count + 1) * 4 and
               List.last(positions) <= byte_size(strike),
             do: invalid("sbix glyph offsets overlap the strike directory or exceed its bytes")

      records =
        positions
        |> Enum.chunk_every(2, 1, :discard)
        |> Enum.with_index()
        |> Map.new(fn {[a, b], id} -> {id, read(strike, a, b - a)} end)

      glyphs =
        Enum.reduce(0..(count - 1), %{}, fn id, glyphs ->
          case Map.fetch!(records, id) do
            <<>> -> glyphs
            _ -> Map.put(glyphs, id, sbix_glyph(records, id, %{}))
          end
        end)

      %{
        format: :sbix,
        ppem_x: ppem,
        ppem_y: ppem,
        ppi: ppi,
        overlay: (flags &&& 2) != 0,
        glyphs: glyphs
      }
    end)
  end

  @spec sbix_glyph(map(), non_neg_integer(), map()) :: map()
  defp sbix_glyph(records, id, seen) do
    FontValidator.check(:max_depth, map_size(seen))
    work(1)
    if Map.has_key?(seen, id), do: invalid("sbix duplicate glyph references form a cycle")
    data = Map.get(records, id) || invalid("sbix duplicate glyph ID is outside maxp")
    <<x::signed-16, y::signed-16, type::binary-size(4)>> = read(data, 0, 8)
    bytes = read(data, 8, byte_size(data) - 8)

    case type do
      "dupe" ->
        unless byte_size(bytes) == 2,
          do: invalid("sbix duplicate glyph record must contain exactly one glyph ID")

        <<target::16>> = bytes
        original = sbix_glyph(records, target, Map.put(seen, id, true))
        %{original | x: x, y: y}

      format when format in ["png ", "jpg "] ->
        %{
          encoding: if(format == "png ", do: :png, else: :jpeg),
          data: bytes,
          x: x,
          y: y,
          expected: nil
        }

      _ ->
        invalid("sbix glyph uses an unsupported bitmap encoding", :unsupported_font)
    end
  end

  defp cbdt(bitmap, locations, glyph_count) do
    unless read(bitmap, 0, 4) == <<3::16, 0::16>>, do: invalid("CBDT version must be 3.0")
    <<major::16, minor::16, count::32>> = read(locations, 0, 8)
    unless major == 3 and minor == 0, do: invalid("CBLC version must be 3.0")
    work(count)

    for <<offset::32, size::32, subtables::32, color::32, _hori::binary-size(12),
          _vert::binary-size(12), first::16, last::16, px, py, depth,
          flags <- read(locations, 8, count * 48)>> do
      unless color == 0 and first <= last and last < glyph_count and px > 0 and py > 0 and
               depth in [1, 2, 4, 8, 32] and flags in [1, 2, 3],
             do: invalid("CBLC strike metrics, glyph range or flags are invalid")

      work(subtables)
      table = read(locations, offset, size)
      records = read(table, 0, subtables * 8)

      {glyphs, _previous} =
        for <<a::16, b::16, relative::32 <- records>>, reduce: {%{}, first - 1} do
          {glyphs, previous} ->
            unless a > previous and a <= b and a >= first and b <= last and
                     relative >= subtables * 8,
                   do: invalid("CBLC index ranges overlap or lie outside their strike")

            <<index::16, image_format::16, image_offset::32>> = read(table, relative, 8)
            {entries, shared} = indices(table, relative + 8, index, a, b)

            glyphs =
              Enum.reduce(entries, glyphs, fn {id, start, finish}, glyphs ->
                unless id >= a and id <= b and finish >= start and image_offset >= 4,
                  do: invalid("CBLC image offsets or glyph IDs are invalid")

                work(1)

                if finish == start do
                  glyphs
                else
                  bytes = read(bitmap, image_offset + start, finish - start)
                  Map.put(glyphs, id, bitmap_glyph(bytes, image_format, shared, depth, flags))
                end
              end)

            {glyphs, b}
        end

      %{format: :cbdt, ppem_x: px, ppem_y: py, ppi: 72, overlay: false, glyphs: glyphs}
    end
  end

  defp indices(table, offset, format, first, last) do
    work(last - first + 1)

    case format do
      variable when variable in [1, 3] ->
        width = if variable == 1, do: 4, else: 2
        raw = read(table, offset, (last - first + 2) * width)

        positions =
          if width == 4, do: for(<<n::32 <- raw>>, do: n), else: for(<<n::16 <- raw>>, do: n)

        {positions
         |> Enum.chunk_every(2, 1, :discard)
         |> Enum.with_index(first)
         |> Enum.map(fn {[a, b], id} -> {id, a, b} end), nil}

      constant when constant in [2, 5] ->
        <<size::32>> = read(table, offset, 4)
        metrics = read(table, offset + 4, 8)

        ids =
          if constant == 2 do
            Enum.to_list(first..last)
          else
            <<count::32>> = read(table, offset + 12, 4)
            work(count)
            for <<id::16 <- read(table, offset + 16, count * 2)>>, do: id
          end

        unless ids == Enum.sort(Enum.uniq(ids)),
          do: invalid("CBLC constant-size sparse glyph IDs must be sorted and unique")

        {ids |> Enum.with_index() |> Enum.map(fn {id, i} -> {id, i * size, (i + 1) * size} end),
         metrics}

      4 ->
        <<count::32>> = read(table, offset, 4)
        work(count)

        pairs =
          for <<id::16, position::16 <- read(table, offset + 4, (count + 1) * 4)>>,
            do: {id, position}

        ids = Enum.map(Enum.drop(pairs, -1), &elem(&1, 0))

        unless ids == Enum.sort(Enum.uniq(ids)),
          do: invalid("CBLC sparse glyph IDs must be sorted and unique")

        {Enum.chunk_every(pairs, 2, 1, :discard)
         |> Enum.map(fn [{id, a}, {_, b}] -> {id, a, b} end), nil}

      _ ->
        invalid("CBLC index format must be 1 through 5")
    end
  end

  defp bitmap_glyph(bytes, format, shared, depth, flags) do
    {metrics, payload} =
      case format do
        small when small in [1, 2, 17] ->
          unless (flags &&& 1) != 0,
            do: invalid("horizontal CBDT text requires horizontal small metrics")

          {read(bytes, 0, 5), read(bytes, 5, byte_size(bytes) - 5)}

        big when big in [6, 7, 18] ->
          {read(bytes, 0, 8), read(bytes, 8, byte_size(bytes) - 8)}

        external when external in [5, 19] ->
          if shared == nil, do: invalid("CBDT image format requires shared CBLC metrics")
          {shared, bytes}

        _ ->
          invalid("CBDT image format is unsupported", :unsupported_font)
      end

    <<height, width, x::signed-8, top::signed-8, advance, _::binary>> = metrics
    unless height > 0 and width > 0, do: invalid("CBDT bitmap dimensions must be positive")

    if format in [17, 18, 19] do
      <<length::32>> = read(payload, 0, 4)

      unless byte_size(payload) == length + 4,
        do: invalid("CBDT PNG length does not own its complete glyph record")

      %{
        encoding: :png,
        data: read(payload, 4, length),
        x: x,
        y: top - height,
        advance: advance,
        expected: {width, height}
      }
    else
      # Color uncompressed records use premultiplied BGRA pixels. Grayscale
      # forms are retained by the monochrome outline fallback in this stage.
      unless depth == 32,
        do: invalid("uncompressed CBDT requires 32-bit color pixels", :unsupported_font)

      unless byte_size(payload) == width * height * 4,
        do: invalid("CBDT BGRA pixel length disagrees with its metrics")

      %{
        encoding: :bgra,
        data: payload,
        x: x,
        y: top - height,
        advance: advance,
        expected: {width, height}
      }
    end
  end

  defp read(data, offset, size) do
    if offset < 0 or size < 0 or offset > byte_size(data) - size,
      do: invalid("color bitmap table is truncated or references bytes outside its owner")

    binary_part(data, offset, size)
  end

  defp work(count), do: FontValidator.reserve(:max_work, count)

  @spec invalid(String.t()) :: no_return()
  @spec invalid(String.t(), atom()) :: no_return()
  defp invalid(message, reason \\ :invalid_font) do
    throw(
      {:bitmap_font_error,
       Diagnostics.error(:font, reason, message,
         module: __MODULE__,
         operation: :prepare_color_font,
         source: "bitmap font"
       )}
    )
  end
end
