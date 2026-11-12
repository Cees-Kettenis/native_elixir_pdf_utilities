defmodule NativeElixirPdfUtilities.Validators.OpenTypePositioningValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.FontValidator
  alias NativeElixirPdfUtilities.Validators.OpenTypeContextValidator, as: Context
  alias NativeElixirPdfUtilities.Validators.OpenTypeDataValidator, as: Data

  @doc "Admits standard positioning subtables, including nullable attachment anchors."
  @spec prepare(map(), 1..8, non_neg_integer(), non_neg_integer()) :: map()
  def prepare(context, kind, base, lookup_count) do
    case kind do
      1 ->
        <<format::16, coverage::16, value_format::16>> = Data.read(context, base, 6)

        {values, finish} =
          case format do
            1 ->
              {value, finish} = Data.value(context, base, base + 6, value_format)
              {[value], finish}

            2 ->
              values(context, base, base + 8, Data.u16(context, base + 6), value_format)

            _ ->
              Data.invalid(context, "unsupported single positioning format", base)
          end

        covered = Data.coverage(context, Data.offset(context, base, coverage, finish - base))

        values =
          if format == 1 do
            List.duplicate(hd(values), length(covered))
          else
            if length(values) != length(covered),
              do: Data.invalid(context, "position value count differs from coverage", base)

            values
          end

        %{kind: :single, values: Map.new(Enum.zip(covered, values))}

      2 ->
        pair(context, base)

      3 ->
        <<format::16, coverage::16, count::16>> = Data.read(context, base, 6)
        if format != 1, do: Data.invalid(context, "unsupported cursive positioning format", base)
        minimum = 6 + count * 4
        covered = Data.coverage(context, Data.offset(context, base, coverage, minimum))

        if length(covered) != count,
          do: Data.invalid(context, "cursive anchor count differs from coverage", base)

        raw = Data.read(context, base + 6, count * 4)

        anchors =
          for <<entry::16, exit::16 <- raw>>,
            do: %{
              entry: anchor(context, base, entry, minimum),
              exit: anchor(context, base, exit, minimum)
            }

        %{kind: :cursive, anchors: Map.new(Enum.zip(covered, anchors))}

      kind when kind in [4, 5, 6] ->
        attachments(context, kind, base)

      7 ->
        Context.prepare(context, :context, base, lookup_count)

      8 ->
        Context.prepare(context, :chain, base, lookup_count)
    end
  end

  defp values(context, parent, start, count, format) do
    # Even a zero-sized ValueRecord consumes admission work and an output slot.
    FontValidator.reserve(:max_work, count)

    Enum.map_reduce(List.duplicate(nil, count), start, fn _, position ->
      Data.value(context, parent, position, format)
    end)
  end

  defp pair(context, base) do
    <<format::16, coverage::16, first_format::16, second_format::16>> =
      Data.read(context, base, 8)

    case format do
      1 ->
        count = Data.u16(context, base + 8)
        minimum = 10 + count * 2
        covered = Data.coverage(context, Data.offset(context, base, coverage, minimum))

        if length(covered) != count,
          do: Data.invalid(context, "pair set count differs from coverage", base)

        pairs =
          for offset <- Data.words(context, base + 10, count) do
            parent = Data.offset(context, base, offset, minimum)
            count = Data.u16(context, parent)

            {records, _} =
              Enum.map_reduce(List.duplicate(nil, count), parent + 2, fn _, position ->
                glyph = Data.glyph(context, Data.u16(context, position), position)
                {first, position} = Data.value(context, parent, position + 2, first_format)
                {second, position} = Data.value(context, parent, position, second_format)
                {{glyph, {first, second}}, position}
              end)

            Data.ordered(
              context,
              Enum.map(records, &elem(&1, 0)),
              "pair glyphs are unordered or repeated",
              parent
            )

            Map.new(records)
          end

        %{
          kind: :pair,
          mode: :glyph,
          pairs: Map.new(Enum.zip(covered, pairs)),
          second_format: second_format
        }

      2 ->
        <<first_class::16, second_class::16, first_count::16, second_count::16>> =
          Data.read(context, base + 8, 8)

        if first_count == 0 or second_count == 0,
          do: Data.invalid(context, "pair class counts must be positive", base)

        count = first_count * second_count
        FontValidator.reserve(:max_work, count)

        {records, finish} =
          Enum.map_reduce(List.duplicate(nil, count), base + 16, fn _, position ->
            {first, position} = Data.value(context, base, position, first_format)
            {second, position} = Data.value(context, base, position, second_format)
            {{first, second}, position}
          end)

        minimum = finish - base

        %{
          kind: :pair,
          mode: :class,
          coverage:
            MapSet.new(Data.coverage(context, Data.offset(context, base, coverage, minimum))),
          first_classes:
            Data.classes(context, Data.offset(context, base, first_class, minimum), first_count),
          second_classes:
            Data.classes(context, Data.offset(context, base, second_class, minimum), second_count),
          second_count: second_count,
          second_format: second_format,
          pairs: List.to_tuple(records)
        }

      _ ->
        Data.invalid(context, "unsupported pair positioning format", base)
    end
  end

  defp attachments(context, kind, base) do
    <<format::16, marks::16, bases::16, classes::16, mark_array::16, base_array::16>> =
      Data.read(context, base, 12)

    if format != 1 or classes == 0,
      do: Data.invalid(context, "invalid attachment format or empty mark class count", base)

    marks = Data.coverage(context, Data.offset(context, base, marks, 12))
    bases = Data.coverage(context, Data.offset(context, base, bases, 12))
    parent = Data.offset(context, base, mark_array, 12)
    count = Data.u16(context, parent)

    if length(marks) != count,
      do: Data.invalid(context, "mark record count differs from coverage", parent)

    raw = Data.read(context, parent + 2, count * 4)

    records =
      for <<class::16, offset::16 <- raw>> do
        if class >= classes or offset == 0,
          do: Data.invalid(context, "invalid mark class or missing mark anchor", parent)

        %{class: class, anchor: anchor(context, parent, offset, 2 + count * 4)}
      end

    marks = Map.new(Enum.zip(marks, records))
    parent = Data.offset(context, base, base_array, 12)
    count = Data.u16(context, parent)

    if length(bases) != count,
      do: Data.invalid(context, "attachment target count differs from coverage", parent)

    records =
      if kind == 5 do
        for offset <- Data.words(context, parent + 2, count) do
          target = Data.offset(context, parent, offset, 2 + count * 2)
          components = Data.u16(context, target)

          if components == 0,
            do: Data.invalid(context, "ligature attachment requires a component", target)

          anchor_matrix(context, target, components, classes)
        end
      else
        anchor_matrix(context, parent, count, classes)
      end

    %{
      kind: %{4 => :mark_base, 5 => :mark_ligature, 6 => :mark_mark}[kind],
      marks: marks,
      bases: Map.new(Enum.zip(bases, records))
    }
  end

  defp anchor_matrix(context, parent, count, classes) do
    minimum = 2 + count * classes * 2

    Data.words(context, parent + 2, count * classes)
    |> Enum.map(&anchor(context, parent, &1, minimum))
    |> Enum.chunk_every(classes)
    |> Enum.map(&List.to_tuple/1)
  end

  defp anchor(context, parent, offset, minimum) do
    if offset == 0 do
      nil
    else
      Data.offset(context, parent, offset, minimum)
      Data.anchor(context, parent, offset)
    end
  end
end
