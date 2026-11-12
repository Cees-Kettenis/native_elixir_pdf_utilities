defmodule NativeElixirPdfUtilities.HtmlToPdf.OpenTypeLookup do
  @moduledoc false
  import Bitwise
  alias NativeElixirPdfUtilities.HtmlToPdf.GlyphSequence, as: Sequence
  alias NativeElixirPdfUtilities.Validators.{FontValidator, TextShapingValidator}

  @doc "Executes a selected feature stage in encoded lookup order."
  @spec execute(map(), map(), [binary()]) :: map()
  def execute(state, selection, tags) do
    selection.selected
    |> Enum.sort_by(&elem(&1, 0))
    |> Enum.reduce(%{state | lookups: selection.lookups}, fn {index, features}, state ->
      features = Map.take(features, tags)
      if features == %{}, do: state, else: run(%{state | features: features}, index)
    end)
  end

  @doc "Applies one admitted lookup in encoded order, preserving stable context targets."
  @spec run(map(), non_neg_integer()) :: map()
  def run(state, index) do
    lookup = elem(state.lookups, index)
    reverse? = Enum.any?(lookup.subtables, &(&1.kind == :reverse))

    scan(
      state,
      index,
      if(reverse?, do: state.sequence.last, else: state.sequence.first),
      reverse?
    )
  end

  @doc "Selects the first and second value records from an already admitted glyph- or class-pair table."
  @spec pair_values(map(), non_neg_integer(), non_neg_integer()) :: {map(), map()} | nil
  def pair_values(table, first, second) do
    case table.mode do
      :glyph ->
        Map.get(Map.get(table.pairs, first, %{}), second)

      :class ->
        if MapSet.member?(table.coverage, first) do
          a = Map.get(table.first_classes, first, 0)
          b = Map.get(table.second_classes, second, 0)
          elem(table.pairs, a * table.second_count + b)
        end
    end
  end

  defp scan(state, index, id, reverse?) do
    if id == nil do
      state
    else
      previous = Map.fetch!(state.sequence.nodes, id).previous
      {state, _, next} = apply_at(state, index, id, 1)
      scan(state, index, if(reverse?, do: previous, else: next), reverse?)
    end
  end

  defp apply_at(state, index, id, depth) do
    TextShapingValidator.reserve_depth(depth)
    FontValidator.reserve(:max_work, 1)
    lookup = elem(state.lookups, index)

    case Map.get(state.sequence.nodes, id) do
      nil ->
        {state, false, nil}

      node ->
        if ignored?(state, lookup, node) or not enabled?(state, node) do
          {state, false, node.next}
        else
          Enum.reduce_while(lookup.subtables, {state, false, node.next}, fn subtable, _ ->
            result = subtable(state, lookup, subtable, id, depth)
            if elem(result, 1), do: {:halt, result}, else: {:cont, result}
          end)
        end
    end
  end

  defp enabled?(state, node) do
    Enum.any?(state.features, fn {tag, _value} -> enabled_feature?(tag, node) end)
  end

  defp enabled_feature?(tag, node) do
    cond do
      tag in ["isol", "init", "medi", "fina", "med2", "fin2", "fin3"] ->
        MapSet.member?(node.forms, tag)

      tag in ["rphf", "half", "blwf", "pstf", "pref"] and Map.has_key?(node, :indic_masks) ->
        MapSet.member?(node.indic_masks, tag)

      true ->
        true
    end
  end

  defp substituted(state, node, changes) do
    tags = for {tag, _value} <- state.features, enabled_feature?(tag, node), do: tag

    Map.put(
      changes,
      :applied_features,
      MapSet.union(Map.get(node, :applied_features, MapSet.new()), MapSet.new(tags))
    )
  end

  defp ignored?(state, lookup, node) do
    class = Map.get(state.gdef.classes, node.glyph, if(node.mark?, do: 3, else: 1))

    cond do
      class == 1 and (lookup.flags &&& 2) != 0 ->
        true

      class == 2 and (lookup.flags &&& 4) != 0 ->
        true

      class == 3 and (lookup.flags &&& 8) != 0 ->
        true

      class == 3 and lookup.mark_set != nil ->
        not MapSet.member?(elem(state.gdef.mark_sets, lookup.mark_set), node.glyph)

      class == 3 and lookup.flags >>> 8 != 0 ->
        Map.get(state.gdef.mark_classes, node.glyph, 0) != lookup.flags >>> 8

      true ->
        false
    end
  end

  defp step(state, lookup, id, direction) do
    if id == nil do
      nil
    else
      FontValidator.reserve(:max_work, 1)
      node = Map.fetch!(state.sequence.nodes, id)

      if ignored?(state, lookup, node),
        do: step(state, lookup, Map.fetch!(node, direction), direction),
        else: id
    end
  end

  defp subtable(state, lookup, table, id, depth) do
    node = Map.fetch!(state.sequence.nodes, id)
    miss = {state, false, node.next}

    case {state.table, table.kind} do
      {:gsub, :single} ->
        case Map.fetch(table.replacements, node.glyph) do
          {:ok, glyph} ->
            {%{
               state
               | sequence:
                   Sequence.put(state.sequence, id, substituted(state, node, %{glyph: glyph}))
             }, true, node.next}

          :error ->
            miss
        end

      {:gsub, kind} when kind in [:multiple, :alternate] ->
        case Map.fetch(table.replacements, node.glyph) do
          {:ok, glyphs} ->
            glyphs =
              if kind == :alternate do
                selection = state.features |> Map.values() |> Enum.max(fn -> 1 end)

                case Enum.at(glyphs, selection - 1) do
                  nil -> nil
                  glyph -> [glyph]
                end
              else
                glyphs
              end

            if glyphs == nil do
              miss
            else
              TextShapingValidator.reserve_glyphs(length(glyphs))

              sequence =
                case glyphs do
                  [] ->
                    Sequence.delete(state.sequence, id)

                  [first | rest] ->
                    sequence =
                      Sequence.put(state.sequence, id, substituted(state, node, %{glyph: first}))

                    Enum.reduce(rest, {sequence, id}, fn glyph, {sequence, previous} ->
                      Sequence.insert_after(
                        sequence,
                        previous,
                        Map.merge(node, substituted(state, node, %{glyph: glyph}))
                      )
                    end)
                    |> elem(0)
                end

              {%{state | sequence: sequence}, true, node.next}
            end

          :error ->
            miss
        end

      {:gsub, :ligature} ->
        match =
          Enum.find_value(Map.get(table.replacements, node.glyph, []), fn rule ->
            case match_sequence(state, lookup, node.next, rule.components, :next, :glyph, %{}, []) do
              nil ->
                nil

              ids ->
                if Enum.all?(ids, &enabled?(state, Map.fetch!(state.sequence.nodes, &1))),
                  do: {rule, [id | ids]}
            end
          end)

        case match do
          nil ->
            miss

          {rule, ids} ->
            consumed = Enum.map(ids, &Map.fetch!(state.sequence.nodes, &1))
            last = List.last(consumed)
            components = Enum.reduce(consumed, 0, &(&1.components + &2))

            changes = %{
              glyph: rule.glyph,
              cluster_start: Enum.min_by(consumed, & &1.cluster_start).cluster_start,
              cluster_end: Enum.max_by(consumed, & &1.cluster_end).cluster_end,
              scalar_start: Enum.min_by(consumed, & &1.scalar_start).scalar_start,
              scalar_end: Enum.max_by(consumed, & &1.scalar_end).scalar_end,
              codepoints: Enum.flat_map(consumed, & &1.codepoints),
              fragments:
                NativeElixirPdfUtilities.Validators.TextItemizationValidator.merge_fragments(
                  Enum.map(consumed, & &1.fragments)
                ),
              components: components,
              input_ids: Enum.reduce(consumed, MapSet.new(), &MapSet.union(&1.input_ids, &2)),
              ligature: id,
              mark?: Enum.all?(consumed, & &1.mark?),
              ignorable?: Enum.all?(consumed, & &1.ignorable?)
            }

            sequence = associate_ligature_marks(state.sequence, node.next, last.next, ids, id, 0)

            sequence =
              Enum.reduce(tl(ids), sequence, &Sequence.delete(&2, &1))
              |> Sequence.put(id, substituted(state, node, changes))

            {%{state | sequence: sequence}, true, last.next}
        end

      {:gsub, :reverse} ->
        case Map.fetch(table.replacements, node.glyph) do
          {:ok, glyph} ->
            back =
              match_sequence(
                state,
                lookup,
                node.previous,
                table.backtrack,
                :previous,
                :coverage,
                %{},
                []
              )

            look =
              match_sequence(state, lookup, node.next, table.lookahead, :next, :coverage, %{}, [])

            if back != nil and look != nil,
              do:
                {%{
                   state
                   | sequence:
                       Sequence.put(state.sequence, id, substituted(state, node, %{glyph: glyph}))
                 }, true, node.next},
              else: miss

          :error ->
            miss
        end

      {_, kind} when kind in [:context, :chain] ->
        key =
          case table.mode do
            :glyph -> node.glyph
            :class -> Map.get(table.classes.input, node.glyph, 0)
            :coverage -> :all
          end

        match =
          if MapSet.member?(table.coverage, node.glyph) do
            Enum.find_value(Map.get(table.rules, key, []), fn rule ->
              inputs =
                match_sequence(
                  state,
                  lookup,
                  id,
                  rule.input,
                  :next,
                  table.mode,
                  Map.get(table.classes, :input, %{}),
                  []
                )

              if inputs != nil do
                after_input = Map.fetch!(state.sequence.nodes, List.last(inputs)).next

                back =
                  match_sequence(
                    state,
                    lookup,
                    node.previous,
                    rule.backtrack,
                    :previous,
                    table.mode,
                    Map.get(table.classes, :backtrack, %{}),
                    []
                  )

                look =
                  match_sequence(
                    state,
                    lookup,
                    after_input,
                    rule.lookahead,
                    :next,
                    table.mode,
                    Map.get(table.classes, :lookahead, %{}),
                    []
                  )

                if back != nil and look != nil, do: {rule, inputs, after_input}
              end
            end)
          end

        case match do
          nil ->
            miss

          {rule, ids, next} ->
            targets = List.to_tuple(ids)

            state =
              Enum.reduce(rule.actions, state, fn {sequence, lookup}, state ->
                elem(apply_at(state, lookup, elem(targets, sequence), depth + 1), 0)
              end)

            {%{state | sequence: state.sequence}, true, next_survivor(state.sequence, next)}
        end

      {:gpos, :single} ->
        case Map.fetch(table.values, node.glyph) do
          {:ok, value} -> {adjust(state, id, value), true, node.next}
          :error -> miss
        end

      {:gpos, :pair} ->
        second = step(state, lookup, node.next, :next)

        values =
          if second != nil do
            glyph = Map.fetch!(state.sequence.nodes, second).glyph

            pair_values(table, node.glyph, glyph)
          end

        case values do
          nil ->
            miss

          {first, second_value} ->
            state = state |> adjust(id, first) |> adjust(second, second_value)

            next =
              if table.second_format == 0,
                do: second,
                else: Map.fetch!(state.sequence.nodes, second).next

            {state, true, next}
        end

      {:gpos, :cursive} ->
        second = step(state, lookup, node.next, :next)
        first_anchor = Map.get(table.anchors, node.glyph)

        second_anchor =
          if second != nil,
            do: Map.get(table.anchors, Map.fetch!(state.sequence.nodes, second).glyph)

        if first_anchor != nil and first_anchor.exit != nil and second_anchor != nil and
             second_anchor.entry != nil do
          {exit_x, exit_y} = anchor(state, first_anchor.exit)
          {entry_x, entry_y} = anchor(state, second_anchor.entry)
          other = Map.fetch!(state.sequence.nodes, second)
          advance = if state.direction == :rtl, do: entry_x - exit_x, else: exit_x - entry_x

          sequence =
            Sequence.put(state.sequence, id, %{
              x_advance: advance + node.x_offset - other.x_offset
            })

          sequence =
            if (lookup.flags &&& 1) != 0,
              do:
                Sequence.put(sequence, id, %{
                  attachment: %{parent: second, dx: 0, dy: entry_y - exit_y, axes: :y}
                }),
              else:
                Sequence.put(sequence, second, %{
                  attachment: %{parent: id, dx: 0, dy: exit_y - entry_y, axes: :y}
                })

          {%{state | sequence: sequence}, true, node.next}
        else
          miss
        end

      {:gpos, kind} when kind in [:mark_base, :mark_ligature, :mark_mark] ->
        mark = Map.get(table.marks, node.glyph)

        target =
          if mark != nil do
            attachment_target(state, lookup, node.previous, kind, node)
          end

        anchors =
          if target != nil,
            do: Map.get(table.bases, Map.fetch!(state.sequence.nodes, target).glyph)

        target_anchor =
          if anchors != nil do
            if kind == :mark_ligature do
              base = Map.fetch!(state.sequence.nodes, target)

              component =
                if node.ligature == base.ligature and node.ligature != nil,
                  do: node.component,
                  else: length(anchors) - 1

              anchors |> Enum.at(min(component, length(anchors) - 1)) |> elem(mark.class)
            else
              elem(anchors, mark.class)
            end
          end

        if target_anchor != nil do
          {base_x, base_y} = anchor(state, target_anchor)
          {mark_x, mark_y} = anchor(state, mark.anchor)

          sequence =
            Sequence.put(state.sequence, id, %{
              attachment: %{parent: target, dx: base_x - mark_x, dy: base_y - mark_y, axes: :both}
            })

          {%{state | sequence: sequence}, true, node.next}
        else
          miss
        end
    end
  end

  defp next_survivor(sequence, id) do
    # Context actions can remove their trailing target through nested ligatures.
    if id == nil or Map.has_key?(sequence.nodes, id),
      do: id,
      else: next_survivor(sequence, Map.fetch!(sequence.removed, id).next)
  end

  defp match_sequence(state, lookup, id, pattern, direction, mode, classes, matched) do
    case pattern do
      [] ->
        Enum.reverse(matched)

      [expected | rest] ->
        id = step(state, lookup, id, direction)

        if id != nil do
          node = Map.fetch!(state.sequence.nodes, id)

          matches? =
            case mode do
              :glyph -> node.glyph == expected
              :class -> Map.get(classes, node.glyph, 0) == expected
              :coverage -> MapSet.member?(expected, node.glyph)
            end

          if matches?,
            do:
              match_sequence(
                state,
                lookup,
                Map.fetch!(node, direction),
                rest,
                direction,
                mode,
                classes,
                [id | matched]
              )
        end
    end
  end

  defp associate_ligature_marks(sequence, id, finish, consumed, ligature, component) do
    if id == finish or id == nil do
      sequence
    else
      node = Map.fetch!(sequence.nodes, id)
      consumed? = id in consumed

      sequence =
        if node.mark? and not consumed?,
          do: Sequence.put(sequence, id, %{ligature: ligature, component: component}),
          else: sequence

      associate_ligature_marks(
        sequence,
        node.next,
        finish,
        consumed,
        ligature,
        component + if(consumed?, do: node.components, else: 0)
      )
    end
  end

  defp attachment_target(state, lookup, id, kind, mark) do
    if id != nil do
      FontValidator.reserve(:max_work, 1)
      node = Map.fetch!(state.sequence.nodes, id)
      class = Map.get(state.gdef.classes, node.glyph, if(node.mark?, do: 3, else: 1))

      cond do
        kind != :mark_mark and class == 3 ->
          attachment_target(state, lookup, node.previous, kind, mark)

        ignored?(state, lookup, node) ->
          attachment_target(state, lookup, node.previous, kind, mark)

        kind == :mark_mark and (class != 3 or node.ligature != mark.ligature) ->
          nil

        true ->
          id
      end
    end
  end

  defp adjust(state, id, value) do
    glyph = Map.fetch!(state.sequence.nodes, id)

    changes =
      Map.new(
        [
          {:x_placement, :x_offset},
          {:y_placement, :y_offset},
          {:x_advance, :x_advance},
          {:y_advance, :y_advance}
        ],
        fn {source, target} ->
          delta = Map.fetch!(value, source) + device(state, Map.get(value.devices, source))
          # GPOS yAdvance adjusts an advance height, positive downwards.
          # Shaping positions use the font's positive-up y coordinate system.
          delta = if source == :y_advance, do: -delta, else: delta
          {target, Map.fetch!(glyph, target) + delta}
        end
      )

    %{state | sequence: Sequence.put(state.sequence, id, changes)}
  end

  defp anchor(state, anchor),
    do: {anchor.x + device(state, anchor.x_device), anchor.y + device(state, anchor.y_device)}

  defp device(state, device) do
    NativeElixirPdfUtilities.Validators.OpenTypeDataValidator.device_adjustment(
      device,
      state.ppem,
      state.units
    )
  end
end
