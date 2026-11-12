defmodule NativeElixirPdfUtilities.HtmlToPdf.GlyphNormalization do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeTextData

  alias NativeElixirPdfUtilities.Validators.{
    FontValidator,
    TextShapingValidator,
    TextItemizationValidator
  }

  # Hebrew rendering order follows the SBL Hebrew manual's fixed-position marks.
  # https://www.sbl-site.org/Fonts/SBLHebrewUserManual1.5x.pdf
  @hebrew_order {22, 15, 16, 17, 23, 18, 19, 20, 21, 14, 24, 12, 25, 13, 10, 11, 26}

  @doc "Normalizes an admitted cluster while retaining the source of each composed/decomposed glyph."
  @spec prepare(map(), map()) :: [map()]
  def prepare(cluster, font) do
    decomposed =
      Enum.flat_map(cluster.scalars, fn scalar ->
        String.normalize(<<scalar.codepoint::utf8>>, :nfd)
        |> String.to_charlist()
        |> Enum.map(fn codepoint ->
          {codepoint, scalar.source.fragments}
        end)
      end)

    TextShapingValidator.reserve_glyphs(length(decomposed))
    FontValidator.reserve(:max_work, byte_size(cluster.text) + length(decomposed))

    sources =
      Enum.reduce(decomposed, %{}, fn {codepoint, fragments}, sources ->
        Map.update(
          sources,
          codepoint,
          :queue.in(fragments, :queue.new()),
          &:queue.in(fragments, &1)
        )
      end)

    scalars =
      cluster.text
      |> String.normalize(:nfc)
      |> String.to_charlist()
      |> Enum.flat_map(fn codepoint ->
        properties = UnicodeTextData.properties(codepoint)

        split_matra? =
          elem(properties, 0) in ~w(Beng Orya Taml Telu Knda Mlym Sinh) and
            elem(properties, 5) == :Vowel_Dependent

        if Map.get(font.cmap, codepoint, 0) != 0 and not split_matra?,
          do: [codepoint],
          else: String.normalize(<<codepoint::utf8>>, :nfd) |> String.to_charlist()
      end)

    TextShapingValidator.reserve_glyphs(length(scalars))

    {glyphs, _} =
      Enum.map_reduce(scalars, sources, fn codepoint, sources ->
        parts = String.normalize(<<codepoint::utf8>>, :nfd) |> String.to_charlist()

        {fragments, sources} =
          Enum.map_reduce(parts, sources, fn part, sources ->
            {{:value, fragments}, remaining} = :queue.out(Map.fetch!(sources, part))
            {fragments, Map.put(sources, part, remaining)}
          end)

        {%{
           codepoint: codepoint,
           properties: UnicodeTextData.properties(codepoint),
           fragments: TextItemizationValidator.merge_fragments(fragments)
         }, sources}
      end)

    glyphs
    |> Enum.chunk_by(&(elem(&1.properties, 3) != 0))
    |> Enum.flat_map(fn group ->
      if elem(hd(group).properties, 3) == 0, do: group, else: Enum.sort_by(group, &mark_order/1)
    end)
  end

  defp mark_order(glyph) do
    class = elem(glyph.properties, 3)

    cond do
      glyph.codepoint in [0x1A60, 0xFC6] -> 254
      glyph.codepoint == 0xF39 -> 127
      class in 10..26 -> elem(@hebrew_order, class - 10)
      class in 27..32 -> class + 1
      class == 33 -> 27
      class == 84 -> 4
      class == 91 -> 5
      class == 103 -> 3
      class == 130 -> 132
      class == 132 -> 131
      true -> class
    end
  end
end
