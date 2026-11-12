defmodule NativeElixirPdfUtilities.Validators.OpenTypeSubstitutionValidator do
  @moduledoc false
  import Bitwise
  alias NativeElixirPdfUtilities.Validators.OpenTypeDataValidator, as: Data
  alias NativeElixirPdfUtilities.Validators.OpenTypeContextValidator, as: Context

  @doc "Admits every standard GSUB subtable, with context formats shared by positioning."
  @spec prepare(map(), 1..6 | 8, non_neg_integer(), non_neg_integer()) :: map()
  def prepare(context, kind, base, lookup_count) do
    case kind do
      1 ->
        <<format::16, coverage::16, value::16>> = Data.read(context, base, 6)

        if format not in [1, 2],
          do: Data.invalid(context, "unsupported single substitution format", base)

        minimum = if format == 1, do: 6, else: 6 + value * 2
        covered = Data.coverage(context, Data.offset(context, base, coverage, minimum))

        replacements =
          if format == 1 do
            Enum.map(covered, &Data.glyph(context, &1 + value &&& 65535, base))
          else
            if length(covered) != value,
              do: Data.invalid(context, "single substitute count differs from coverage", base)

            Data.words(context, base + 6, value) |> Enum.map(&Data.glyph(context, &1, base))
          end

        %{kind: :single, replacements: Map.new(Enum.zip(covered, replacements))}

      kind when kind in [2, 3] ->
        <<format::16, coverage::16, count::16>> = Data.read(context, base, 6)

        if format != 1,
          do: Data.invalid(context, "unsupported multiple or alternate substitution format", base)

        covered = Data.coverage(context, Data.offset(context, base, coverage, 6 + count * 2))

        if length(covered) != count,
          do: Data.invalid(context, "substitute sequence count differs from coverage", base)

        replacements =
          for offset <- Data.words(context, base + 6, count) do
            offset = Data.offset(context, base, offset, 6 + count * 2)
            count = Data.u16(context, offset)

            if kind == 3 and count == 0,
              do: Data.invalid(context, "alternate set must not be empty", offset)

            Data.words(context, offset + 2, count) |> Enum.map(&Data.glyph(context, &1, offset))
          end

        %{
          kind: if(kind == 2, do: :multiple, else: :alternate),
          replacements: Map.new(Enum.zip(covered, replacements))
        }

      4 ->
        <<format::16, coverage::16, count::16>> = Data.read(context, base, 6)

        if format != 1,
          do: Data.invalid(context, "unsupported ligature substitution format", base)

        covered = Data.coverage(context, Data.offset(context, base, coverage, 6 + count * 2))

        if length(covered) != count,
          do: Data.invalid(context, "ligature set count differs from coverage", base)

        sets =
          for offset <- Data.words(context, base + 6, count) do
            set = Data.offset(context, base, offset, 6 + count * 2)
            count = Data.u16(context, set)

            for offset <- Data.words(context, set + 2, count) do
              offset = Data.offset(context, set, offset, 2 + count * 2)
              <<output::16, components::16>> = Data.read(context, offset, 4)

              if components == 0,
                do: Data.invalid(context, "ligature must consume an input glyph", offset)

              inputs =
                Data.words(context, offset + 4, components - 1)
                |> Enum.map(&Data.glyph(context, &1, offset))

              %{glyph: Data.glyph(context, output, offset), components: inputs}
            end
          end

        %{kind: :ligature, replacements: Map.new(Enum.zip(covered, sets))}

      5 ->
        Context.prepare(context, :context, base, lookup_count)

      6 ->
        Context.prepare(context, :chain, base, lookup_count)

      8 ->
        <<format::16, covered::16>> = Data.read(context, base, 4)

        if format != 1,
          do: Data.invalid(context, "unsupported reverse chaining substitution format", base)

        back_count = Data.u16(context, base + 4)
        back = Data.words(context, base + 6, back_count)
        next = base + 6 + back_count * 2
        look_count = Data.u16(context, next)
        look = Data.words(context, next + 2, look_count)
        next = next + 2 + look_count * 2
        count = Data.u16(context, next)
        outputs = Data.words(context, next + 2, count) |> Enum.map(&Data.glyph(context, &1, next))
        minimum = next + 2 + count * 2 - base
        coverage = Data.coverage(context, Data.offset(context, base, covered, minimum))

        if length(coverage) != count,
          do: Data.invalid(context, "reverse substitute count differs from coverage", base)

        cover = fn offsets ->
          Enum.map(
            offsets,
            &(Data.coverage(context, Data.offset(context, base, &1, minimum)) |> MapSet.new())
          )
        end

        %{
          kind: :reverse,
          replacements: Map.new(Enum.zip(coverage, outputs)),
          backtrack: cover.(back),
          lookahead: cover.(look)
        }
    end
  end
end
