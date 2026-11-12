defmodule NativeElixirPdfUtilities.HtmlToPdf.Font.Cff do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.CffSubsetValidator

  @doc "Builds a validated CID-keyed CFF program with source glyph IDs as CIDs."
  @spec subset(map(), [non_neg_integer()], boolean()) ::
          {:ok, binary(), boolean()} | {:error, {atom(), map()}}
  def subset(font, glyph_ids, subset?) do
    with {:ok, plan} <- CffSubsetValidator.prepare(font, glyph_ids, subset?) do
      {:ok, IO.iodata_to_binary(plan.parts), plan.subset?}
    end
  end

  @doc "Serializes an evaluated Type 2 outline, freezing legacy composition and random arithmetic."
  @spec program(map(), number()) :: binary()
  def program(glyph, nominal_width) do
    {commands, _point} =
      Enum.map_reduce(glyph.commands, {0, 0}, fn command, {x, y} ->
        case command do
          :close ->
            {[], {x, y}}

          {kind, nx, ny} ->
            {[operand(nx - x), operand(ny - y), if(kind == :move, do: <<21>>, else: <<5>>)],
             {nx, ny}}

          {:curve, a, b, c, d, e, f} ->
            {[Enum.map([a - x, b - y, c - a, d - b, e - c, f - d], &operand/1), <<8>>], {e, f}}
        end
      end)

    parts = [operand(glyph.width - nominal_width), commands, <<14>>]

    NativeElixirPdfUtilities.Validators.FontValidator.check(
      :max_decoded_bytes,
      IO.iodata_length(parts)
    )

    IO.iodata_to_binary(parts)
  end

  defp operand(value) do
    cond do
      is_integer(value) and value in -32768..32767 ->
        <<28, value::signed-16>>

      value >= -32768 and value < 32768 and round(value * 65536) <= 2_147_483_647 ->
        <<255, round(value * 65536)::signed-32>>

      true ->
        high = trunc(value / 16384)
        [operand(high), <<28, 16384::16, 12, 24>>, operand(value - high * 16384), <<12, 10>>]
    end
  end

  @doc "Builds an admitted static CID CFF face from evaluated programs in OpenType design units."
  @spec static_face([binary()], pos_integer(), {number(), number(), number(), number()}) ::
          binary()
  @spec static_face([binary()], pos_integer(), tuple(), [non_neg_integer()] | nil) :: binary()
  def static_face(programs, units, bbox, cids \\ nil) do
    {programs, subrs} = static_programs(programs)
    :ok = CffSubsetValidator.validate_static_programs(programs, subrs)
    count = length(programs)
    names = index(["StaticCFF2"])
    strings = index(["Adobe", "Identity"])
    globals = index(subrs)

    charset =
      if cids == nil do
        if count == 1, do: <<0>>, else: <<2, 1::16, count - 2::16>>
      else
        <<0, for(id <- tl(cids), into: <<>>, do: <<id::16>>)::binary>>
      end

    select = <<0, :binary.copy(<<0>>, count)::binary>>
    chars = index(programs)
    private = dictionary(%{20 => [0], 21 => [0]})
    fd = %{18 => [byte_size(private), 0]}

    top = %{
      15 => [0],
      17 => [0],
      5 => Tuple.to_list(bbox),
      {12, 7} => [1 / units, 0, 0, 1 / units, 0, 0],
      {12, 30} => [391, 392, 0],
      {12, 34} => [if(cids == nil, do: count, else: List.last(cids) + 1)],
      {12, 36} => [0],
      {12, 37} => [0]
    }

    prefix =
      4 + byte_size(names) + byte_size(index([dictionary(top)])) + byte_size(strings) +
        byte_size(globals)

    select_offset = prefix + byte_size(charset)
    char_offset = select_offset + byte_size(select)
    array_offset = char_offset + byte_size(chars)
    private_offset = array_offset + byte_size(index([dictionary(fd)]))

    top =
      Map.merge(top, %{
        15 => [prefix],
        17 => [char_offset],
        {12, 36} => [array_offset],
        {12, 37} => [select_offset]
      })

    fd = %{18 => [byte_size(private), private_offset]}

    IO.iodata_to_binary([
      <<1, 0, 4, 4>>,
      names,
      index([dictionary(top)]),
      strings,
      globals,
      charset,
      select,
      chars,
      index([dictionary(fd)]),
      private
    ])
  end

  # CFF 1 limits each program to 65,535 bytes. Splitting generated drawing
  # commands into globals retains large CFF2/legacy outlines within that fixed
  # format invariant; this is not a tunable complexity ceiling.
  defp static_programs(programs) do
    {planned, {subrs, count}} =
      Enum.map_reduce(programs, {[], 0}, fn program, {subrs, count} ->
        if byte_size(program) <= 65535 do
          {{:literal, program}, {subrs, count}}
        else
          commands = program_commands(program, [], [])

          {chunks, current, _size} =
            Enum.reduce(commands, {[], [], 0}, fn command, {chunks, current, size} ->
              bytes = byte_size(command)

              if size + bytes + 1 > 65535 do
                {[IO.iodata_to_binary(Enum.reverse(current)) | chunks], [command], bytes}
              else
                {chunks, [command | current], size + bytes}
              end
            end)

          chunks = Enum.reverse([IO.iodata_to_binary(Enum.reverse(current)) | chunks])
          {{:calls, count, length(chunks)}, {[chunks | subrs], count + length(chunks)}}
        end
      end)

    subrs = subrs |> Enum.reverse() |> List.flatten() |> Enum.map(&(&1 <> <<11>>))

    bias = NativeElixirPdfUtilities.Validators.CffFontValidator.subroutine_bias(count)

    programs =
      Enum.map(planned, fn plan ->
        case plan do
          {:literal, bytes} ->
            bytes

          {:calls, first, count} ->
            IO.iodata_to_binary([
              for(i <- first..(first + count - 1), do: [operand(i - bias), <<29>>]),
              <<14>>
            ])
        end
      end)

    {programs, subrs}
  end

  defp program_commands(data, current, commands) do
    NativeElixirPdfUtilities.Validators.FontValidator.reserve(:max_work, 1)

    case data do
      <<28, value::binary-size(2), rest::binary>> ->
        program_commands(rest, [<<28, value::binary>> | current], commands)

      <<255, value::binary-size(4), rest::binary>> ->
        program_commands(rest, [<<255, value::binary>> | current], commands)

      <<12, operator, rest::binary>> ->
        program_commands(rest, [<<12, operator>> | current], commands)

      <<operator, rest::binary>> when operator in [5, 8, 21] ->
        command = IO.iodata_to_binary(Enum.reverse([<<operator>> | current]))
        program_commands(rest, [], [command | commands])

      <<14>> ->
        Enum.reverse(commands)
    end
  end

  @doc false
  @spec index([binary()]) :: binary()
  def index(objects) do
    case objects do
      [] ->
        <<0::16>>

      _ ->
        {offsets, finish} =
          Enum.map_reduce(objects, 1, fn bytes, offset ->
            {<<offset::32>>, offset + byte_size(bytes)}
          end)

        IO.iodata_to_binary([<<length(objects)::16, 4>>, offsets, <<finish::32>>, objects])
    end
  end

  @doc false
  @spec dictionary(map()) :: binary()
  def dictionary(values) do
    values
    |> Enum.sort_by(fn {key, _} -> if key == {12, 30}, do: {-1, 0}, else: {0, key} end)
    |> Enum.map(fn {operator, operands} ->
      [
        Enum.map(operands, fn value ->
          case value do
            value when is_integer(value) ->
              <<29, value::signed-32>>

            value ->
              nibbles =
                :erlang.float_to_binary(value, [:short])
                |> String.replace("e-", "c")
                |> String.replace("e+", "b")
                |> String.replace("e", "b")
                |> String.to_charlist()
                |> Enum.map(fn char ->
                  case char do
                    ?. -> 10
                    ?b -> 11
                    ?c -> 12
                    ?- -> 14
                    char -> char - ?0
                  end
                end)

              nibbles = nibbles ++ if(rem(length(nibbles), 2) == 0, do: [15, 15], else: [15])

              <<30,
                for([a, b] <- Enum.chunk_every(nibbles, 2), into: <<>>, do: <<a::4, b::4>>)::binary>>
          end
        end),
        case operator do
          {12, escaped} -> <<12, escaped>>
          value -> <<value>>
        end
      ]
    end)
    |> IO.iodata_to_binary()
  end
end
