defmodule NativeElixirPdfUtilities.Validators.OpenTypeContextValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.OpenTypeDataValidator, as: Data

  @doc "Admits the shared contextual/chaining lookup formats used by both GSUB and GPOS."
  @spec prepare(map(), :context | :chain, non_neg_integer(), non_neg_integer()) :: map()
  def prepare(context, kind, base, lookup_count) do
    format = Data.u16(context, base)

    case format do
      1 ->
        <<covered::16, count::16>> = Data.read(context, base + 2, 4)
        minimum = 6 + count * 2
        coverage = Data.coverage(context, Data.offset(context, base, covered, minimum))

        if length(coverage) != count,
          do: Data.invalid(context, "context rule-set count differs from coverage", base)

        rules = Enum.zip(coverage, Data.words(context, base + 6, count))

        rules =
          Map.new(rules, fn {glyph, offset} ->
            {glyph, rule_set(context, kind, base, offset, minimum, glyph, :glyph, lookup_count)}
          end)

        %{kind: kind, mode: :glyph, coverage: MapSet.new(coverage), classes: %{}, rules: rules}

      2 ->
        {covered, class_offsets, count, start} =
          if kind == :chain do
            <<covered::16, back::16, input::16, look::16, count::16>> =
              Data.read(context, base + 2, 10)

            {covered, %{backtrack: back, input: input, lookahead: look}, count, 12}
          else
            <<covered::16, classes::16, count::16>> = Data.read(context, base + 2, 6)
            {covered, %{input: classes}, count, 8}
          end

        minimum = start + count * 2
        coverage = Data.coverage(context, Data.offset(context, base, covered, minimum))

        classes =
          Map.new(class_offsets, fn {key, offset} ->
            {key, Data.classes(context, Data.offset(context, base, offset, minimum))}
          end)

        if Enum.any?(coverage, &(Map.get(classes.input, &1, 0) >= count)),
          do: Data.invalid(context, "covered context glyph has no class rule set", base)

        rules = Data.words(context, base + start, count) |> Enum.with_index()

        rules =
          Map.new(rules, fn {offset, class} ->
            {class, rule_set(context, kind, base, offset, minimum, class, :class, lookup_count)}
          end)

        %{
          kind: kind,
          mode: :class,
          coverage: MapSet.new(coverage),
          classes: classes,
          rules: rules
        }

      3 ->
        {back, inputs, look, actions, finish} =
          if kind == :chain do
            {back, next} = coverage_offsets(context, base + 2)
            {inputs, next} = coverage_offsets(context, next)
            {look, next} = coverage_offsets(context, next)
            count = Data.u16(context, next)
            {actions, finish} = actions(context, next + 2, count, length(inputs), lookup_count)
            {back, inputs, look, actions, finish}
          else
            <<count::16, records::16>> = Data.read(context, base + 2, 4)
            inputs = Data.words(context, base + 6, count)

            {actions, finish} =
              actions(context, base + 6 + count * 2, records, count, lookup_count)

            {[], inputs, [], actions, finish}
          end

        if inputs == [],
          do: Data.invalid(context, "context input sequence must not be empty", base)

        cover = fn offsets ->
          Enum.map(
            offsets,
            &(Data.coverage(context, Data.offset(context, base, &1, finish - base))
              |> MapSet.new())
          )
        end

        input = cover.(inputs)
        rule = %{input: input, backtrack: cover.(back), lookahead: cover.(look), actions: actions}
        %{kind: kind, mode: :coverage, coverage: hd(input), classes: %{}, rules: %{all: [rule]}}

      _ ->
        Data.invalid(context, "unsupported contextual lookup format", base)
    end
  end

  defp rule_set(context, kind, parent, offset, minimum, first, mode, lookups) do
    if offset == 0 do
      []
    else
      base = Data.offset(context, parent, offset, minimum)
      count = Data.u16(context, base)

      for offset <- Data.words(context, base + 2, count) do
        offset = Data.offset(context, base, offset, 2 + count * 2)

        {back, input, look, action_count, position} =
          if kind == :chain do
            {back, next} = coverage_offsets(context, offset)
            input_count = Data.u16(context, next)

            if input_count == 0,
              do: Data.invalid(context, "context input sequence must not be empty", next)

            input = Data.words(context, next + 2, input_count - 1)
            {look, next} = coverage_offsets(context, next + 2 + (input_count - 1) * 2)
            {back, [first | input], look, Data.u16(context, next), next + 2}
          else
            <<input_count::16, count::16>> = Data.read(context, offset, 4)

            if input_count == 0,
              do: Data.invalid(context, "context input sequence must not be empty", offset)

            input = Data.words(context, offset + 4, input_count - 1)
            {[], [first | input], [], count, offset + 4 + (input_count - 1) * 2}
          end

        if mode == :glyph, do: Enum.each(back ++ input ++ look, &Data.glyph(context, &1, offset))
        {actions, _} = actions(context, position, action_count, length(input), lookups)
        %{input: input, backtrack: back, lookahead: look, actions: actions}
      end
    end
  end

  defp coverage_offsets(context, position) do
    count = Data.u16(context, position)
    {Data.words(context, position + 2, count), position + 2 + count * 2}
  end

  defp actions(context, position, count, inputs, lookups) do
    raw = Data.read(context, position, count * 4)

    actions =
      for <<sequence::16, lookup::16 <- raw>> do
        if sequence >= inputs or lookup >= lookups,
          do:
            Data.invalid(
              context,
              "context action references an unavailable input or lookup",
              position
            )

        {sequence, lookup}
      end

    {actions, position + count * 4}
  end
end
