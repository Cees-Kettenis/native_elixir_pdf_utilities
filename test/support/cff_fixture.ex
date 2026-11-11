defmodule NativeElixirPdfUtilities.TestSupport.CffFixture do
  @moduledoc false

  @doc "Encodes test-owned CFF INDEX objects with a selected offset width."
  @spec index([binary()], 1..4) :: binary()
  def index(objects, width \\ 4) do
    case objects do
      [] ->
        <<0::16>>

      objects ->
        {offsets, _} =
          Enum.map_reduce(objects, 1, fn bytes, offset -> {offset, offset + byte_size(bytes)} end)

        finish = 1 + Enum.sum(Enum.map(objects, &byte_size/1))

        positions =
          for offset <- offsets ++ [finish],
              into: <<>>,
              do: <<offset::unsigned-big-integer-size(width * 8)>>

        <<length(objects)::16, width, positions::binary>> <> IO.iodata_to_binary(objects)
    end
  end

  @doc "Encodes test-owned DICT operands with stable fixed-width integers."
  @spec dictionary(map()) :: binary()
  def dictionary(values) do
    values
    |> Enum.sort()
    |> Enum.map(fn {operator, numbers} ->
      [
        Enum.map(numbers, fn value ->
          if is_integer(value) do
            <<29, value::signed-32>>
          else
            encoded = NativeElixirPdfUtilities.HtmlToPdf.Font.Cff.dictionary(%{5 => [value]})
            binary_part(encoded, 0, byte_size(encoded) - 1)
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

  @doc "Builds a minimal CFF face with explicit dictionary and program ownership."
  @spec face_fixture(keyword()) :: binary()
  def face_fixture(opts \\ []) do
    count = Keyword.get(opts, :glyph_count, 3)
    cid? = Keyword.get(opts, :cid, false)

    charset =
      Keyword.get(
        opts,
        :charset,
        <<0, for(sid <- 1..max(count - 1, 0)//1, into: <<>>, do: <<sid::16>>)::binary>>
      )

    charset = if count == 1 and not Keyword.has_key?(opts, :charset), do: <<0>>, else: charset
    encoding = Keyword.get(opts, :encoding)
    subrs = Keyword.get(opts, :local_subrs, [<<11>>])
    private = Map.merge(%{20 => [600], 21 => [100]}, Keyword.get(opts, :private, %{}))
    private = if is_nil(subrs), do: private, else: Map.put_new(private, 19, [0])
    private_size = byte_size(dictionary(private))

    private =
      if Map.get(private, 19) == [0] and not Map.has_key?(Keyword.get(opts, :private, %{}), 19),
        do: Map.put(private, 19, [private_size]),
        else: private

    private_bytes = dictionary(private)
    omit? = Keyword.get(opts, :omit_private, false)
    fonts = Keyword.get(opts, :font_count, 1)
    fd_dict = if omit?, do: %{}, else: %{18 => [private_size, 0]}

    fd_dict =
      if Keyword.has_key?(opts, :fd_name_sid),
        do: Map.put(fd_dict, {12, 38}, [opts[:fd_name_sid]]),
        else: fd_dict

    fd_dict =
      if Keyword.has_key?(opts, :fd_matrix),
        do: Map.put(fd_dict, {12, 7}, opts[:fd_matrix]),
        else: fd_dict

    fd_array = if cid?, do: index(List.duplicate(dictionary(fd_dict), fonts)), else: <<>>

    fd_select =
      if cid?,
        do: Keyword.get(opts, :fd_select, <<0, :binary.copy(<<0>>, count)::binary>>),
        else: <<>>

    charstrings = index(Keyword.get(opts, :charstrings, List.duplicate(<<14>>, count)))
    top = %{17 => [0], 15 => [0]}

    top =
      if cid?,
        do: Map.merge(top, %{{12, 30} => [391, 392, 0], {12, 36} => [0], {12, 37} => [0]}),
        else: top

    top = if not cid? and not omit?, do: Map.put(top, 18, [private_size, 0]), else: top
    top = if is_nil(encoding), do: top, else: Map.put(top, 16, [0])
    extra = Keyword.get(opts, :top, %{})
    top = Map.merge(top, extra)
    names = index(Keyword.get(opts, :names, ["CorpusCFF"]))
    strings = index(Keyword.get(opts, :strings, if(cid?, do: ["Adobe", "Identity"], else: [])))
    globals = index(Keyword.get(opts, :global_subrs, [<<11>>]))

    prefix_size =
      4 + byte_size(names) + byte_size(index([dictionary(top)])) + byte_size(strings) +
        byte_size(globals)

    blocks = [
      {:charset, if(is_tuple(charset), do: <<>>, else: charset)},
      {:encoding, if(is_tuple(encoding) or is_nil(encoding), do: <<>>, else: encoding)},
      {:fds, fd_array},
      {:select, fd_select},
      {:charstrings, charstrings},
      {:private, private_bytes},
      {:subrs, if(is_nil(subrs), do: <<>>, else: index(subrs))}
    ]

    {offsets, _} =
      Enum.map_reduce(blocks, prefix_size, fn {key, bytes}, offset ->
        {{key, offset}, offset + byte_size(bytes)}
      end)

    offsets = Map.new(offsets)

    top =
      Map.put(top, 17, [offsets.charstrings])
      |> Map.put(15, [if(is_tuple(charset), do: elem(charset, 1), else: offsets.charset)])

    top =
      if cid?,
        do: top |> Map.put({12, 36}, [offsets.fds]) |> Map.put({12, 37}, [offsets.select]),
        else: top

    top =
      if not cid? and not omit?, do: Map.put(top, 18, [private_size, offsets.private]), else: top

    top =
      if is_nil(encoding),
        do: top,
        else:
          Map.put(top, 16, [if(is_tuple(encoding), do: elem(encoding, 1), else: offsets.encoding)])

    top = Map.merge(top, extra)
    fd_dict = if omit?, do: fd_dict, else: Map.put(fd_dict, 18, [private_size, offsets.private])
    fd_array = if cid?, do: index(List.duplicate(dictionary(fd_dict), fonts)), else: <<>>
    body = Enum.map(blocks, fn {key, bytes} -> if key == :fds, do: fd_array, else: bytes end)
    IO.iodata_to_binary([<<1, 0, 4, 4>>, names, index([dictionary(top)]), strings, globals, body])
  end
end
