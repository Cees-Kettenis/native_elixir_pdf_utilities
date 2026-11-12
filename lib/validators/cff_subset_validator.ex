defmodule NativeElixirPdfUtilities.Validators.CffSubsetValidator do
  @moduledoc false
  import Bitwise

  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.Font.Cff
  alias NativeElixirPdfUtilities.Validators.{CollectionValidator, FontValidator, HtmlValidator}

  @doc "Prepares a genuine CID CFF program, preserving private dictionaries and subroutine ownership."
  @spec prepare(term(), term(), term()) ::
          {:ok, %{parts: iodata(), subset?: boolean()}} | {:error, {atom(), map()}}
  def prepare(font, glyph_ids, subset?) do
    FontValidator.with_budget(fn ->
      with %{data: data} when is_binary(data) <- font,
           true <- is_boolean(subset?),
           {:ok, selected_count} <-
             CollectionValidator.bounded_length(glyph_ids, Limits.get(:max_work)),
           :ok <- FontValidator.reserve_source(data),
           {:ok, %{cff: face} = parsed} <-
             FontValidator.memo({:validated_ttf, data}, fn -> FontValidator.prepare_ttf(data) end),
           :ok <-
             HtmlValidator.validate_font_embedding(
               Map.get(font, :family, "CFF font"),
               parsed.embedding_flags,
               parsed.variable_font?
             ),
           {:ok, registry_strings, [registry_sid, ordering_sid]} <-
             identity_registry(face.strings) do
        count = tuple_size(face.charstrings)
        FontValidator.reserve(:max_work, count + selected_count)

        cond do
          not Enum.all?(glyph_ids, &(is_integer(&1) and &1 >= 0 and &1 < count)) ->
            invalid("CFF subset contains an out-of-range glyph ID")

          true ->
            subset? = subset? and (parsed.embedding_flags &&& 0x0100) == 0

            retained =
              if subset?,
                do: Enum.uniq([0 | glyph_ids]) |> Enum.sort(),
                else: Enum.to_list(0..(count - 1))

            programs =
              Enum.map(retained, fn glyph ->
                value = elem(face.glyphs, glyph)

                if value.static? do
                  font = elem(face.fonts, elem(face.fd_select, glyph))
                  program = Cff.program(value, font.nominal_width)

                  program
                else
                  elem(face.charstrings, glyph)
                end
              end)

            if Enum.any?(programs, &(byte_size(&1) > 65535)) do
              programs =
                Enum.map(retained, fn id ->
                  commands =
                    NativeElixirPdfUtilities.HtmlToPdf.Font.Outline.cff_design_commands(
                      face,
                      id,
                      parsed.units_per_em
                    )

                  FontValidator.reserve(:max_work, length(commands))
                  Cff.program(%{commands: commands, width: Enum.at(parsed.widths, id)}, 0)
                end)

              bytes = Cff.static_face(programs, parsed.units_per_em, parsed.bbox, retained)
              FontValidator.check(:max_decoded_bytes, byte_size(bytes))
              {:ok, %{parts: [bytes], subset?: subset?}}
            else
              # Bound copied INDEX payloads before constructing their binaries.
              # The final exact check below also charges dictionaries/offsets.
              payload_bytes =
                Enum.sum(Enum.map(programs, &byte_size/1)) +
                  Enum.sum(Enum.map(Tuple.to_list(face.strings), &byte_size/1)) +
                  Enum.sum(Enum.map(Tuple.to_list(face.global_subrs), &byte_size/1)) +
                  Enum.sum(
                    Enum.map(Tuple.to_list(face.fonts), fn font ->
                      Enum.sum(Enum.map(Tuple.to_list(font.local_subrs), &byte_size/1))
                    end)
                  )

              FontValidator.check(:max_decoded_bytes, payload_bytes)
              # CIDs are original OpenType glyph IDs. Multiple document character
              # codes can map to the same CID without duplicating its program.
              charset = <<0, for(glyph <- tl(retained), into: <<>>, do: <<glyph::16>>)::binary>>

              select =
                <<0,
                  for(glyph <- retained, into: <<>>, do: <<elem(face.fd_select, glyph)>>)::binary>>

              strings = Cff.index(registry_strings)
              names = Cff.index([face.name])
              globals = Cff.index(Tuple.to_list(face.global_subrs))
              charstrings = Cff.index(programs)

              {privates, subrs} =
                face.fonts
                |> Tuple.to_list()
                |> Enum.map(fn font ->
                  local = Cff.index(Tuple.to_list(font.local_subrs))
                  private = Map.delete(font.private, 19)

                  private =
                    if font.local_subrs == {}, do: private, else: Map.put(private, 19, [0])

                  bytes = Cff.dictionary(private)

                  bytes =
                    if font.local_subrs == {},
                      do: bytes,
                      else: Cff.dictionary(Map.put(private, 19, [byte_size(bytes)]))

                  {bytes, local}
                end)
                |> Enum.unzip()

              dictionaries =
                face.fonts
                |> Tuple.to_list()
                |> Enum.with_index()
                |> Enum.map(fn {font, index} ->
                  values =
                    if face.cid?, do: Map.take(font.dictionary, [{12, 7}, {12, 38}]), else: %{}

                  Map.put(values, 18, [byte_size(Enum.at(privates, index)), 0])
                end)

              array = Cff.index(Enum.map(dictionaries, &Cff.dictionary/1))

              top =
                face.top
                |> Map.take([
                  0,
                  1,
                  2,
                  3,
                  4,
                  5,
                  {12, 0},
                  {12, 1},
                  {12, 2},
                  {12, 3},
                  {12, 4},
                  {12, 5},
                  {12, 7},
                  {12, 8},
                  {12, 21},
                  {12, 22}
                ])
                |> Map.merge(%{
                  15 => [0],
                  17 => [0],
                  {12, 30} => [registry_sid, ordering_sid, 0],
                  {12, 34} => [count],
                  {12, 36} => [0],
                  {12, 37} => [0]
                })

              prefix =
                4 + byte_size(names) + byte_size(Cff.index([Cff.dictionary(top)])) +
                  byte_size(strings) + byte_size(globals)

              offsets = %{
                charset: prefix,
                select: prefix + byte_size(charset),
                charstrings: prefix + byte_size(charset) + byte_size(select),
                array: prefix + byte_size(charset) + byte_size(select) + byte_size(charstrings)
              }

              start = offsets.array + byte_size(array)

              {dictionaries, _finish} =
                Enum.map_reduce(Enum.with_index(dictionaries), start, fn {values, index},
                                                                         offset ->
                  private = Enum.at(privates, index)
                  local = Enum.at(subrs, index)

                  {Map.put(values, 18, [byte_size(private), offset]),
                   offset + byte_size(private) + byte_size(local)}
                end)

              top =
                top
                |> Map.put(15, [offsets.charset])
                |> Map.put(17, [offsets.charstrings])
                |> Map.put({12, 36}, [offsets.array])
                |> Map.put({12, 37}, [offsets.select])

              plan = [
                <<1, 0, 4, 4>>,
                names,
                Cff.index([Cff.dictionary(top)]),
                strings,
                globals,
                charset,
                select,
                charstrings,
                Cff.index(Enum.map(dictionaries, &Cff.dictionary/1)),
                Enum.zip_with(privates, subrs, fn a, b -> [a, b] end)
              ]

              FontValidator.check(:max_decoded_bytes, IO.iodata_length(plan))
              {:ok, %{parts: plan, subset?: subset?}}
            end
        end
      else
        :limit_exceeded ->
          FontValidator.check(:max_work, Limits.get(:max_work) + 1)

        {:error, _} = error ->
          error

        _ ->
          invalid(
            "CFF subsetting requires a validated CFF OpenType font, glyph IDs and a boolean subset option"
          )
      end
    end)
  end

  @doc "Checks generated CFF 1 program and INDEX format invariants before serialization."
  @spec validate_static_programs([binary()], [binary()]) :: :ok
  def validate_static_programs(programs, subrs) do
    unless length(subrs) <= 65535 and Enum.all?(programs ++ subrs, &(byte_size(&1) <= 65535)) do
      throw(
        {:font_resource_limit,
         Diagnostics.error(
           :font,
           :unsupported_font,
           "expanded static outline exceeds CFF 1 program or subroutine INDEX capacity",
           module: __MODULE__,
           operation: :write_pdf,
           source: "CFF 1"
         )}
      )
    end

    FontValidator.check(
      :max_decoded_bytes,
      Enum.sum(Enum.map(programs ++ subrs, &byte_size/1))
    )

    :ok
  end

  defp identity_registry(strings) do
    FontValidator.reserve(:max_work, 2 * tuple_size(strings))

    {identifiers, strings} =
      Enum.map_reduce(["Adobe", "Identity"], Tuple.to_list(strings), fn name, strings ->
        case Enum.find_index(strings, &(&1 == name)) do
          nil -> {391 + length(strings), strings ++ [name]}
          index -> {391 + index, strings}
        end
      end)

    # Standard SIDs occupy 0..390; the CFF format reserves SIDs above 64,999.
    if length(strings) <= 64_609,
      do: {:ok, strings, identifiers},
      else: invalid("CFF String INDEX has no room for Adobe Identity registry strings")
  end

  defp invalid(message) do
    Diagnostics.error(:font, :invalid_font, message,
      operation: :write_pdf,
      module: __MODULE__,
      source: "CFF subset"
    )
  end
end
