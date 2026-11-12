defmodule NativeElixirPdfUtilities.Validators.OpenTypeCmapValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.FontValidator
  alias NativeElixirPdfUtilities.Validators.OpenTypeDataValidator, as: Data

  @doc "Admits format-14 Unicode variation sequences without changing the selected nominal cmap."
  @spec prepare_sequences(binary() | nil, pos_integer()) :: map()
  def prepare_sequences(data, glyph_count) do
    if data == nil do
      %{}
    else
      context = %{data: data, tag: "cmap", glyph_count: glyph_count}
      <<version::16, count::16>> = Data.read(context, 0, 4)
      if version != 0, do: Data.invalid(context, "unsupported cmap version", 0)
      raw = Data.read(context, 4, count * 8)

      tables =
        for <<_platform::16, _encoding::16, offset::32 <- raw>>,
            Data.u16(context, Data.offset(context, 0, offset, 4 + count * 8)) == 14,
            do: offset

      if length(Enum.uniq(tables)) > 1,
        do:
          Data.invalid(context, "cmap contains conflicting Unicode variation sequence tables", 0)

      case Enum.uniq(tables) do
        [] -> %{}
        [base] -> variation_table(context, base)
      end
    end
  end

  defp variation_table(context, base) do
    <<14::16, size::32, count::32>> = Data.read(context, base, 10)
    data = Data.read(context, base, size)
    context = %{context | data: data}
    raw = Data.read(context, 10, count * 11)

    selectors =
      for <<selector::24, default::32, nondefault::32 <- raw>>,
        do: {selector, default, nondefault}

    Data.ordered(
      context,
      Enum.map(selectors, &elem(&1, 0)),
      "variation selectors are unordered or repeated",
      10
    )

    Map.new(selectors, fn {selector, default, nondefault} ->
      if selector not in 0xFE00..0xFE0F and selector not in 0xE0100..0xE01EF and
           selector not in [0x180B, 0x180C, 0x180D, 0x180F],
         do: Data.invalid(context, "format-14 record is not a Unicode variation selector", 10)

      defaults =
        if default == 0 do
          MapSet.new()
        else
          offset = Data.offset(context, 0, default, 10 + count * 11)
          raw = Data.read(context, offset + 4, Data.u32(context, offset) * 4)

          {ranges, _} =
            Enum.map_reduce(
              for(<<first::24, additional::8 <- raw>>, do: {first, first + additional}),
              -1,
              fn {first, last}, previous ->
                scalar(context, first, offset)
                scalar(context, last, offset)

                if first <= previous or (first < 0xD800 and last > 0xDFFF),
                  do: Data.invalid(context, "invalid default variation range", offset)

                FontValidator.reserve(:max_work, last - first + 1)
                {Enum.to_list(first..last), last}
              end
            )

          ranges |> List.flatten() |> MapSet.new()
        end

      nondefaults =
        if nondefault == 0 do
          %{}
        else
          offset = Data.offset(context, 0, nondefault, 10 + count * 11)
          raw = Data.read(context, offset + 4, Data.u32(context, offset) * 5)

          pairs =
            for <<codepoint::24, glyph::16 <- raw>>,
              do: {scalar(context, codepoint, offset), Data.glyph(context, glyph, offset)}

          Data.ordered(
            context,
            Enum.map(pairs, &elem(&1, 0)),
            "variation mappings are unordered or repeated",
            offset
          )

          if Enum.any?(pairs, &MapSet.member?(defaults, elem(&1, 0))),
            do: Data.invalid(context, "default and nondefault variation mappings overlap", offset)

          Map.new(pairs)
        end

      {selector, %{defaults: defaults, nondefaults: nondefaults}}
    end)
  end

  defp scalar(context, value, offset) do
    if value > 0x10FFFF or value in 0xD800..0xDFFF,
      do: Data.invalid(context, "variation mapping is not a Unicode scalar", offset)

    value
  end
end
