defmodule NativeElixirPdfUtilities.Validators.Cff2FontValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.Validators.{
    CffFontValidator,
    FontValidator,
    VariationStoreValidator
  }

  @doc "Admits CFF2 dictionaries, INDEXes, variation regions and per-glyph subroutine ownership."
  @spec prepare(term(), term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(data, count, coordinates) do
    FontValidator.with_budget(fn ->
      try do
        unless is_binary(data) and is_integer(count) and count in 1..65535,
          do:
            invalid(
              "CFF2 requires bytes, a positive OpenType glyph count and normalized coordinates"
            )

        coordinates =
          case NativeElixirPdfUtilities.Validators.FontVariationValidator.prepare_normalized_coordinates(
                 coordinates
               ) do
            {:ok, coordinates} -> coordinates
            error -> throw({:invalid_cff2, error})
          end

        <<major, minor, header, size::16>> = read(data, 0, 5)

        unless major == 2 and minor == 0 and header == 5,
          do: invalid("CFF2 requires version 2.0 and a five-byte header")

        context = %{version: 2, kind: :top, store: {}, vsindex: 0}
        top = unwrap(CffFontValidator.prepare_dictionary(read(data, header, size), context))
        {globals, finish} = index(data, header + size)
        owners = [{:header, 0, header + size}, {:globals, header + size, finish}]

        {store, owners} =
          case Map.get(top, 24) do
            nil ->
              {{}, owners}

            [offset] when is_integer(offset) and offset >= 0 ->
              <<length::16>> = read(data, offset, 2)
              # The 0xFFFF length is the CFF2 large-store sentinel, not a
              # configurable resource limit. Validate the store's actual extent.
              payload =
                if length == 65535,
                  do: read(data, offset + 2, byte_size(data) - offset - 2),
                  else: read(data, offset + 2, length)

              case VariationStoreValidator.prepare_with_extent(payload, 0, coordinates) do
                {:ok, store, used} ->
                  unless Enum.all?(
                           Tuple.to_list(store),
                           fn %{values: values, word_count: words} ->
                             tuple_size(values) == 0 and words == 0
                           end
                         ),
                         do:
                           invalid("CFF2 variation data tables cannot contain stored item deltas")

                  {store,
                   [
                     {:store, offset, offset + 2 + if(length == 65535, do: used, else: length)}
                     | owners
                   ]}

                error ->
                  throw({:invalid_cff2, error})
              end

            _ ->
              invalid("CFF2 VariationStoreOffset requires a nonnegative integer")
          end

        matrix = vector(top, {12, 7}, [0.001, 0, 0, 0.001, 0, 0])

        case Map.get(top, 25, [513]) do
          [n] when is_integer(n) and n > 0 and n <= 513 -> :ok
          _ -> invalid("CFF2 maxstack cannot exceed its fixed 513-operand stack")
        end

        char_offset = integer(top, 17)
        {charstrings, finish} = index(data, char_offset)

        unless tuple_size(charstrings) == count,
          do: invalid("CFF2 CharStrings count differs from maxp")

        owners = [{:charstrings, char_offset, finish} | owners]
        array_offset = integer(top, {12, 36})
        {dictionaries, finish} = index(data, array_offset)

        if tuple_size(dictionaries) == 0,
          do: invalid("CFF2 FontDICTINDEX must contain at least one dictionary")

        owners = [{:fdarray, array_offset, finish} | owners]

        {fonts, owners} =
          Enum.map_reduce(Tuple.to_list(dictionaries), owners, fn bytes, owners ->
            values = unwrap(CffFontValidator.prepare_dictionary(bytes, context))

            {private, owners, local} =
              case Map.get(values, 18) do
                [size, offset]
                when is_integer(size) and size >= 0 and is_integer(offset) and offset >= 0 ->
                  private =
                    unwrap(
                      CffFontValidator.prepare_dictionary(read(data, offset, size), %{
                        context
                        | kind: :private,
                          store: store
                      })
                    )

                  owners =
                    if size == 0, do: owners, else: [{:private, offset, offset + size} | owners]

                  case Map.get(private, 19) do
                    nil ->
                      {private, owners, {}}

                    [relative] when is_integer(relative) and relative >= size ->
                      {subrs, finish} = index(data, offset + relative)
                      {private, [{:local_subrs, offset + relative, finish} | owners], subrs}

                    _ ->
                      invalid("CFF2 local Subrs must follow their owning Private DICT")
                  end

                _ ->
                  invalid("CFF2 FontDICT requires an owned Private DICT size and offset")
              end

            vsindex = Map.get(private, 22, [0])

            # The specialized Private DICT admission already owns vsindex's
            # integer/range rule and its order relative to blend operators.

            font = %{
              dictionary: values,
              private: private,
              local_subrs: local,
              default_width: 0,
              nominal_width: 0,
              random_seed: 0,
              font_matrix: if(Map.has_key?(values, {12, 7}), do: vector(values, {12, 7}, nil)),
              vsindex: hd(vsindex)
            }

            {font, owners}
          end)

        {select, owners} =
          case Map.get(top, {12, 37}) do
            nil ->
              unless length(fonts) == 1,
                do: invalid("CFF2 multiple font dictionaries require FontDICTSelect")

              {List.duplicate(0, count) |> List.to_tuple(), owners}

            [offset] when is_integer(offset) and offset >= 0 ->
              case CffFontValidator.prepare_fd_select(data, offset, count, length(fonts), 2) do
                {:ok, select, finish} -> {select, [{:select, offset, finish} | owners]}
                error -> throw({:invalid_cff2, error})
              end

            _ ->
              invalid("CFF2 FontDICTSelectOffset requires a nonnegative integer")
          end

        owners = owners |> Enum.uniq() |> Enum.sort_by(&elem(&1, 1))

        Enum.chunk_every(owners, 2, 1, :discard)
        |> Enum.each(fn [{_, _, finish}, {_, start, _}] ->
          if finish > start, do: invalid("CFF2 referenced structures overlap")
        end)

        {:ok,
         %{
           version: 2,
           name: "StaticCFF2",
           top: top,
           strings: {},
           charset: Enum.to_list(1..(count - 1)//1),
           cid?: true,
           font_matrix: matrix,
           bbox: {0, 0, 0, 0},
           global_subrs: globals,
           charstrings: charstrings,
           fonts: List.to_tuple(fonts),
           fd_select: select,
           store: store,
           owners: owners
         }}
      catch
        {:invalid_cff2, error} -> error
      end
    end)
  end

  defp vector(values, key, default) do
    case Map.get(values, key, default) do
      [a, b, c, d, e, f] = items
      when is_number(a) and is_number(b) and is_number(c) and is_number(d) and is_number(e) and
             is_number(f) ->
        unless a * d - b * c != 0, do: invalid("CFF2 FontMatrix is singular")
        Enum.each(items, &FontValidator.check(:max_font_numeric_magnitude, ceil(abs(&1))))
        items

      _ ->
        invalid("CFF2 FontMatrix requires six numeric operands")
    end
  end

  defp integer(values, key) do
    case Map.get(values, key) do
      [n] when is_integer(n) and n >= 0 -> n
      _ -> invalid("CFF2 #{inspect(key)} requires a nonnegative integer offset")
    end
  end

  defp index(data, offset) do
    case CffFontValidator.prepare_index(data, offset, 2) do
      {:ok, objects, finish} -> {objects, finish}
      error -> throw({:invalid_cff2, error})
    end
  end

  defp read(data, offset, size) do
    FontValidator.reserve(:max_font_cff_work, max(size, 1))

    # Header fields and DICT admission already guarantee nonnegative offsets
    # and sizes. The large-store remainder follows its admitted length field.
    unless offset + size <= byte_size(data),
      do: invalid("CFF2 byte range is outside its owning table")

    binary_part(data, offset, size)
  end

  defp unwrap(result) do
    case result do
      {:ok, value} -> value
      error -> throw({:invalid_cff2, error})
    end
  end

  defp invalid(message),
    do:
      throw(
        {:invalid_cff2,
         Diagnostics.error(:font, :invalid_font, message,
           module: __MODULE__,
           operation: :prepare_cff2_source,
           source: "OpenType CFF2"
         )}
      )
end
