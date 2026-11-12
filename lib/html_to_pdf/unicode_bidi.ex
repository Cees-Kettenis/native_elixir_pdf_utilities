defmodule NativeElixirPdfUtilities.HtmlToPdf.UnicodeBidi do
  @moduledoc false
  import Bitwise
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeTextData
  alias NativeElixirPdfUtilities.Validators.{FontValidator, TextBidiValidator}
  @removed [:RLE, :LRE, :RLO, :LRO, :PDF, :BN]
  @isolates [:LRI, :RLI, :FSI]
  @neutral [:B, :S, :WS, :ON, :LRI, :RLI, :FSI, :PDI]

  @doc "Resolves Unicode 17 paragraph levels, isolates, overrides and logical scalar ownership."
  @spec resolve(term(), :auto | :ltr | :rtl) :: {:ok, map()} | {:error, {atom(), map()}}
  def resolve(text, direction \\ :auto) do
    FontValidator.with_budget(fn ->
      with {:ok, itemization} <- TextBidiValidator.prepare(text, direction) do
        records =
          itemization.clusters
          |> Enum.flat_map(& &1.scalars)
          |> Enum.with_index()
          |> Enum.map(fn {scalar, index} ->
            Map.merge(scalar, %{
              index: index,
              original: elem(scalar.properties, 1),
              type: elem(scalar.properties, 1)
            })
          end)

        paragraphs =
          records
          |> Enum.chunk_while(
            [],
            fn record, accumulated ->
              if record.original == :B,
                do: {:cont, Enum.reverse([record | accumulated]), []},
                else: {:cont, [record | accumulated]}
            end,
            fn accumulated ->
              if accumulated == [], do: {:cont, []}, else: {:cont, Enum.reverse(accumulated), []}
            end
          )
          |> Enum.map(&paragraph(&1, direction))

        records = Enum.flat_map(paragraphs, & &1.records)

        {:ok,
         %{
           text: itemization.text,
           clusters: itemization.clusters,
           paragraphs: paragraphs,
           records: records,
           levels: Enum.map(records, & &1.level),
           order: Enum.flat_map(paragraphs, & &1.order)
         }}
      end
    end)
  end

  @doc "Applies line-end whitespace resets and visual reordering after a line has been fitted."
  @spec line(map(), non_neg_integer(), non_neg_integer()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def line(resolved, first, last) do
    FontValidator.with_budget(fn ->
      with :ok <- TextBidiValidator.validate_line(first, last, length(resolved.records)) do
        records = resolved.records |> Enum.slice(first, last - first) |> reset_line()
        {:ok, %{records: records, levels: Enum.map(records, & &1.level), order: reorder(records)}}
      end
    end)
  end

  @doc "Reorders already admitted resolved scalar levels after line layout."
  @spec reorder_levels([non_neg_integer()]) :: [non_neg_integer()]
  def reorder_levels(levels) do
    FontValidator.with_budget(fn ->
      levels
      |> Enum.with_index()
      |> Enum.map(fn {level, index} -> %{level: level, index: index, original: :ON} end)
      |> reorder()
    end)
  end

  defp paragraph(records, direction) do
    pairs = isolate_pairs(records)

    base =
      case direction do
        :ltr -> 0
        :rtl -> 1
        :auto -> strong_direction(records, pairs, 0)
      end

    records = explicit(records, base, pairs)
    active = Enum.reject(records, &(&1.original in @removed))
    by_index = Map.new(active, &{&1.index, &1})
    runs = Enum.chunk_by(active, & &1.level)
    run_by_start = Map.new(Enum.with_index(runs), fn {run, index} -> {hd(run).index, index} end)
    levels = List.to_tuple(active)

    positions =
      active
      |> Enum.with_index()
      |> Map.new(fn {record, position} -> {record.index, position} end)

    links =
      runs
      |> Enum.with_index()
      |> Enum.reduce(%{}, fn {run, index}, links ->
        last = List.last(run)
        target = if last.original in @isolates, do: Map.get(pairs, last.index)
        target_run = if target != nil, do: Map.get(run_by_start, target)
        if target_run == nil, do: links, else: Map.put(links, index, target_run)
      end)

    continuation = MapSet.new(Map.values(links))
    run_tuple = List.to_tuple(runs)

    resolved =
      runs
      |> Enum.with_index()
      |> Enum.reject(fn {_run, index} -> MapSet.member?(continuation, index) end)
      |> Enum.reduce(by_index, fn {_run, index}, resolved ->
        sequence = run_sequence(run_tuple, links, index, [])
        first = hd(sequence)
        last = List.last(sequence)
        before = Map.fetch!(positions, first.index) - 1
        after_position = Map.fetch!(positions, last.index) + 1
        before_level = if before < 0, do: base, else: elem(levels, before).level

        after_level =
          if last.original in @isolates or after_position == tuple_size(levels),
            do: base,
            else: elem(levels, after_position).level

        sos = level_direction(max(first.level, before_level))
        eos = level_direction(max(last.level, after_level))
        sequence = sequence |> weak(sos) |> brackets(sos) |> neutrals(sos, eos) |> implicit()
        Enum.reduce(sequence, resolved, &Map.put(&2, &1.index, &1))
      end)

    records =
      Enum.map(records, fn record ->
        Map.get(resolved, record.index, record) |> Map.put(:paragraph_level, base)
      end)
      |> reset_line()

    %{level: base, records: records, order: reorder(records)}
  end

  defp isolate_pairs(records) do
    {pairs, _} =
      Enum.reduce(records, {%{}, []}, fn record, {pairs, stack} ->
        cond do
          record.original in @isolates ->
            {pairs, [record.index | stack]}

          record.original == :PDI and stack != [] ->
            {Map.put(pairs, hd(stack), record.index), tl(stack)}

          true ->
            {pairs, stack}
        end
      end)

    pairs
  end

  defp strong_direction(records, pairs, fallback) do
    {direction, _} =
      Enum.reduce_while(records, {fallback, nil}, fn record, {fallback, skip} ->
        FontValidator.reserve(:max_work, 1)

        cond do
          skip != nil and record.index <= skip ->
            {:cont, {fallback, skip}}

          record.original in @isolates ->
            {:cont, {fallback, Map.get(pairs, record.index, :infinity)}}

          record.original == :L ->
            {:halt, {0, nil}}

          record.original in [:R, :AL] ->
            {:halt, {1, nil}}

          true ->
            {:cont, {fallback, nil}}
        end
      end)

    direction
  end

  defp explicit(records, base, pairs) do
    record_tuple = List.to_tuple(records)
    first_index = hd(records).index

    initial = %{
      stack: [%{level: base, override: nil, isolate: false}],
      overflow_isolates: 0,
      overflow_embeddings: 0,
      valid_isolates: 0
    }

    Enum.map_reduce(records, initial, fn record, state ->
      FontValidator.reserve(:max_work, 1)
      top = hd(state.stack)
      record = Map.put(record, :level, top.level)
      original = record.original

      cond do
        original in [:RLE, :LRE, :RLO, :LRO] ->
          odd? = original in [:RLE, :RLO]
          level = next_level(top.level, odd?)
          # UAX #9 fixes the maximum explicit embedding level at 125.
          state =
            cond do
              level <= 125 and state.overflow_isolates == 0 and state.overflow_embeddings == 0 ->
                override =
                  case original do
                    :RLO -> :R
                    :LRO -> :L
                    _ -> nil
                  end

                %{
                  state
                  | stack: [%{level: level, override: override, isolate: false} | state.stack]
                }

              state.overflow_isolates == 0 ->
                %{state | overflow_embeddings: state.overflow_embeddings + 1}

              true ->
                state
            end

          {%{record | type: :BN}, state}

        original in @isolates ->
          odd? =
            case original do
              :RLI ->
                true

              :LRI ->
                false

              :FSI ->
                last = Map.get(pairs, record.index, first_index + tuple_size(record_tuple))

                strong_range(record_tuple, record.index + 1, last, first_index, pairs) == 1
            end

          level = next_level(top.level, odd?)
          record = %{record | type: top.override || original}

          state =
            if level <= 125 and state.overflow_isolates == 0 and state.overflow_embeddings == 0,
              do: %{
                state
                | stack: [%{level: level, override: nil, isolate: true} | state.stack],
                  valid_isolates: state.valid_isolates + 1
              },
              else: %{state | overflow_isolates: state.overflow_isolates + 1}

          {record, state}

        original == :PDI ->
          state =
            cond do
              state.overflow_isolates > 0 ->
                %{state | overflow_isolates: state.overflow_isolates - 1}

              state.valid_isolates == 0 ->
                state

              true ->
                [_isolate | stack] = Enum.drop_while(state.stack, &(not &1.isolate))

                %{
                  state
                  | stack: stack,
                    overflow_embeddings: 0,
                    valid_isolates: state.valid_isolates - 1
                }
            end

          top = hd(state.stack)
          {%{record | level: top.level, type: top.override || original}, state}

        original == :PDF ->
          state =
            cond do
              state.overflow_isolates > 0 ->
                state

              state.overflow_embeddings > 0 ->
                %{state | overflow_embeddings: state.overflow_embeddings - 1}

              not top.isolate and length(state.stack) > 1 ->
                %{state | stack: tl(state.stack)}

              true ->
                state
            end

          {%{record | type: :BN}, state}

        original == :B ->
          {%{record | level: base}, state}

        true ->
          {%{record | type: if(original == :BN, do: :BN, else: top.override || original)}, state}
      end
    end)
    |> elem(0)
  end

  defp strong_range(records, index, last, first, pairs) do
    if index >= last do
      0
    else
      FontValidator.reserve(:max_work, 1)
      record = elem(records, index - first)

      cond do
        record.original == :L ->
          0

        record.original in [:R, :AL] ->
          1

        record.original in @isolates ->
          strong_range(records, Map.get(pairs, index, last - 1) + 1, last, first, pairs)

        true ->
          strong_range(records, index + 1, last, first, pairs)
      end
    end
  end

  defp next_level(level, odd?), do: if(odd?, do: level + 1 ||| 1, else: level + 2 &&& bnot(1))
  defp level_direction(level), do: if(rem(level, 2) == 0, do: :L, else: :R)

  defp run_sequence(runs, links, index, result) do
    result = [elem(runs, index) | result]

    case Map.fetch(links, index) do
      {:ok, next} -> run_sequence(runs, links, next, result)
      :error -> result |> Enum.reverse() |> List.flatten()
    end
  end

  defp weak(sequence, sos) do
    {sequence, _} =
      Enum.map_reduce(sequence, sos, fn record, previous ->
        type =
          if record.type == :NSM,
            do: if(previous in @isolates or previous == :PDI, do: :ON, else: previous),
            else: record.type

        {%{record | type: type}, type}
      end)

    {sequence, _} =
      Enum.map_reduce(sequence, sos, fn record, strong ->
        type = if record.type == :EN and strong == :AL, do: :AN, else: record.type

        {%{record | type: if(type == :AL, do: :R, else: type)},
         if(record.type in [:L, :R, :AL], do: record.type, else: strong)}
      end)

    tuple = List.to_tuple(sequence)

    sequence =
      sequence
      |> Enum.with_index()
      |> Enum.map(fn {record, index} ->
        before = if index > 0, do: elem(tuple, index - 1).type
        after_type = if index + 1 < tuple_size(tuple), do: elem(tuple, index + 1).type

        type =
          cond do
            record.type == :ES and before == :EN and after_type == :EN -> :EN
            record.type == :CS and before == after_type and before in [:EN, :AN] -> before
            true -> record.type
          end

        %{record | type: type}
      end)

    groups = Enum.chunk_by(sequence, &(&1.type == :ET))
    tuple = List.to_tuple(groups)

    sequence =
      groups
      |> Enum.with_index()
      |> Enum.flat_map(fn {group, index} ->
        before = if index > 0, do: List.last(elem(tuple, index - 1)).type
        after_type = if index + 1 < tuple_size(tuple), do: hd(elem(tuple, index + 1)).type
        type = if hd(group).type == :ET and (before == :EN or after_type == :EN), do: :EN
        if type == nil, do: group, else: Enum.map(group, &%{&1 | type: type})
      end)

    sequence =
      Enum.map(sequence, fn record ->
        if record.type in [:ES, :ET, :CS], do: %{record | type: :ON}, else: record
      end)

    Enum.map_reduce(sequence, sos, fn record, strong ->
      type = if record.type == :EN and strong == :L, do: :L, else: record.type
      {%{record | type: type}, if(record.type in [:L, :R], do: record.type, else: strong)}
    end)
    |> elem(0)
  end

  defp brackets(sequence, sos) do
    {pairs, _} =
      sequence
      |> Enum.with_index()
      |> Enum.reduce_while({[], []}, fn {record, index}, {pairs, stack} ->
        FontValidator.reserve(:max_work, 1)
        bracket = if record.type == :ON, do: UnicodeTextData.bracket(record.codepoint)

        case bracket do
          {_pair, :open} when length(stack) == 63 ->
            {:halt, {pairs, stack}}

          {pair, :open} ->
            {:cont, {pairs, [{canonical_bracket(pair), index} | stack]}}

          {_pair, :close} ->
            matched =
              Enum.drop_while(stack, fn {pair, _index} ->
                pair != canonical_bracket(record.codepoint)
              end)

            case matched do
              [{_, open} | remaining] -> {:cont, {[{open, index} | pairs], remaining}}
              [] -> {:cont, {pairs, stack}}
            end

          _ ->
            {:cont, {pairs, stack}}
        end
      end)

    tuple = List.to_tuple(sequence)

    resolved =
      Enum.sort(pairs)
      |> Enum.reduce(%{}, fn {open, close}, resolved ->
        embedding = level_direction(elem(tuple, open).level)
        opposite = if embedding == :L, do: :R, else: :L

        FontValidator.reserve(:max_work, close - open - 1)

        enclosed =
          for index <- (open + 1)..(close - 1)//1,
              do: strong_type(Map.get(resolved, index, elem(tuple, index).type))

        preceding =
          if opposite in enclosed and embedding not in enclosed do
            Enum.find_value((open - 1)..0//-1, sos, fn index ->
              FontValidator.reserve(:max_work, 1)
              type = strong_type(Map.get(resolved, index, elem(tuple, index).type))
              if type in [:L, :R], do: type
            end)
          end

        direction =
          cond do
            embedding in enclosed -> embedding
            opposite in enclosed and preceding == opposite -> opposite
            opposite in enclosed -> embedding
            true -> nil
          end

        if direction == nil,
          do: resolved,
          else: resolved |> Map.put(open, direction) |> Map.put(close, direction)
      end)

    {sequence, _} =
      sequence
      |> Enum.with_index()
      |> Enum.map_reduce(nil, fn {record, index}, previous ->
        type = Map.get(resolved, index)
        type = if type == nil and record.original == :NSM, do: previous, else: type
        {%{record | type: type || record.type}, type}
      end)

    sequence
  end

  defp canonical_bracket(codepoint), do: if(codepoint == 0x232A, do: 0x3009, else: codepoint)
  defp strong_type(type), do: if(type in [:EN, :AN], do: :R, else: type)

  defp neutrals(sequence, sos, eos) do
    groups = Enum.chunk_by(sequence, &(&1.type in @neutral))
    tuple = List.to_tuple(groups)

    groups
    |> Enum.with_index()
    |> Enum.flat_map(fn {group, index} ->
      if hd(group).type in @neutral do
        before = if index == 0, do: sos, else: strong_type(List.last(elem(tuple, index - 1)).type)

        after_type =
          if index + 1 == tuple_size(tuple),
            do: eos,
            else: strong_type(hd(elem(tuple, index + 1)).type)

        direction = if before == after_type, do: before, else: level_direction(hd(group).level)
        Enum.map(group, &%{&1 | type: direction})
      else
        group
      end
    end)
  end

  defp implicit(sequence) do
    Enum.map(sequence, fn record ->
      increment =
        cond do
          rem(record.level, 2) == 0 and record.type == :R -> 1
          rem(record.level, 2) == 0 and record.type in [:AN, :EN] -> 2
          rem(record.level, 2) == 1 and record.type in [:AN, :EN, :L] -> 1
          true -> 0
        end

      %{record | level: record.level + increment}
    end)
  end

  defp reset_line(records) do
    {records, _} =
      records
      |> Enum.reverse()
      |> Enum.map_reduce(true, fn record, trailing? ->
        separator? = record.original in [:S, :B]
        whitespace? = record.original in ([:WS, :LRI, :RLI, :FSI, :PDI] ++ @removed)

        level =
          if separator? or (trailing? and whitespace?),
            do: record.paragraph_level,
            else: record.level

        {%{record | level: level}, separator? or (trailing? and whitespace?)}
      end)

    Enum.reverse(records)
  end

  defp reorder(records) do
    active = Enum.reject(records, &(&1.original in @removed))

    if active == [] do
      []
    else
      maximum = Enum.max_by(active, & &1.level).level

      minimum =
        active
        |> Enum.filter(&(rem(&1.level, 2) == 1))
        |> Enum.min_by(& &1.level, fn -> %{level: maximum + 1} end)
        |> Map.fetch!(:level)

      FontValidator.reserve(
        :max_work,
        length(active) * max(1, maximum - minimum + 1)
      )

      Enum.reduce(maximum..minimum//-1, active, fn level, records ->
        records
        |> Enum.chunk_by(&(&1.level >= level))
        |> Enum.flat_map(fn group ->
          if hd(group).level >= level, do: Enum.reverse(group), else: group
        end)
      end)
      |> Enum.map(& &1.index)
    end
  end
end
