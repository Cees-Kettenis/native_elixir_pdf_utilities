defmodule NativeElixirPdfUtilities.HtmlToPdf.InlineBidi do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeBidi

  alias NativeElixirPdfUtilities.Validators.{
    FontValidator,
    NativeGlyphSourceValidator,
    TextBidiValidator
  }

  @doc "Resolves a complete inline paragraph before its runs are split by line fitting."
  @spec prepare([map()], map()) :: {:ok, [map()]} | {:error, {atom(), map()}}
  def prepare(runs, style) do
    if TextBidiValidator.required?(runs, style) do
      FontValidator.with_budget(fn ->
        mode = Map.get(style, :unicode_bidi, :normal)

        base_contexts =
          if mode in [:embed, :bidi_override, :isolate_override],
            do: [%{id: :paragraph_bidi, mode: mode, direction: Map.get(style, :direction, :ltr)}],
            else: []

        {slots, active} =
          runs
          |> Enum.with_index()
          |> Enum.reduce({[], []}, fn {run, index}, {slots, active} ->
            contexts = base_contexts ++ Map.get(run.style, :_bidi_contexts, [])
            controls = transition(active, contexts)

            slots =
              Enum.reduce(controls, slots, fn codepoint, slots -> [{codepoint, nil} | slots] end)

            scalars =
              if Map.get(run, :type) in [:inline_block, :inline_image],
                do: [0xFFFC],
                else: String.to_charlist(run.text)

            FontValidator.reserve(:max_work, length(controls) + length(scalars))

            slots =
              Enum.reduce(scalars, slots, fn codepoint, slots -> [{codepoint, index} | slots] end)

            {slots, contexts}
          end)

        slots =
          Enum.reduce(transition(active, []), slots, fn codepoint, slots ->
            [{codepoint, nil} | slots]
          end)
          |> Enum.reverse()

        text = slots |> Enum.map(&elem(&1, 0)) |> List.to_string()

        direction =
          if Map.get(style, :unicode_bidi, :normal) == :plaintext,
            do: :auto,
            else: Map.get(style, :direction, :ltr)

        with {:ok, bidi} <- UnicodeBidi.resolve(text, direction) do
          records =
            Enum.zip(slots, bidi.records)
            |> Enum.reduce(%{}, fn {{_codepoint, owner}, record}, records ->
              if owner == nil,
                do: records,
                else: Map.update(records, owner, [record], &[record | &1])
            end)

          prepared =
            runs
            |> Enum.with_index()
            |> Enum.map(fn {run, index} ->
              Map.put(run, :bidi_characters, Map.get(records, index, []) |> Enum.reverse())
            end)

          {:ok, split_levels(prepared)}
        end
      end)
    else
      {:ok, runs}
    end
  end

  @doc "Assigns visual line positions while retaining logical run and extraction order."
  @spec position([map()], number(), (map() -> number())) :: [map()]
  def position(runs, left, advance) do
    if Enum.any?(runs, &Map.has_key?(&1, :bidi_characters)) do
      FontValidator.with_budget(fn ->
        records =
          runs
          |> Enum.flat_map(&Map.get(&1, :bidi_characters, []))
          |> Enum.with_index()
          |> Enum.map(fn {record, index} -> %{record | index: index} end)

        {:ok, line} = UnicodeBidi.line(%{records: records}, 0, length(records))

        {runs, []} =
          Enum.map_reduce(runs, line.records, fn run, remaining ->
            {records, remaining} =
              Enum.split(remaining, length(Map.get(run, :bidi_characters, [])))

            {Map.put(run, :bidi_characters, records), remaining}
          end)

        runs = split_levels(runs)

        owners =
          runs
          |> Enum.with_index()
          |> Enum.flat_map(fn {run, index} ->
            Enum.map(run.bidi_characters, &{&1.index, index})
          end)
          |> Map.new()

        ranks =
          line.order
          |> Enum.map(&Map.fetch!(owners, &1))
          |> Enum.uniq()
          |> Enum.with_index()
          |> Map.new()

        run_count = length(runs)

        {ranked, _} =
          runs
          |> Enum.with_index()
          |> Enum.reverse()
          |> Enum.map_reduce(nil, fn {run, index}, following ->
            rank =
              if Map.has_key?(run, :_text_indent_policy),
                do:
                  if(Map.get(run.style, :direction, :ltr) == :rtl,
                    do: map_size(ranks) + 1,
                    else: -1
                  ),
                else: Map.get(ranks, index)

            fallback =
              if following == nil,
                do: map_size(ranks) + index / max(run_count, 1),
                else: following - 0.5

            {{run, index, rank || fallback}, rank || following}
          end)

        {positions, _} =
          ranked
          |> Enum.sort_by(&elem(&1, 2))
          |> Enum.map_reduce(left, fn {run, index, _rank}, x ->
            {{index, x}, x + advance.(run)}
          end)

        positions = Map.new(positions)

        runs
        |> Enum.with_index()
        |> Enum.map(fn {run, index} -> Map.put(run, :_bidi_x, Map.fetch!(positions, index)) end)
      end)
      |> case do
        {:error, _} = error -> throw({:render_resource_limit, error})
        runs -> runs
      end
    else
      runs
    end
  end

  defp split_levels(runs) do
    Enum.flat_map(runs, fn run ->
      if run.text != "" and
           length(Map.get(run, :bidi_characters, [])) == length(String.codepoints(run.text)) do
        chunks =
          Enum.zip(String.codepoints(run.text), run.bidi_characters)
          |> Enum.chunk_by(fn {_scalar, record} -> record.level end)

        Enum.map_reduce(chunks, 0, fn chunk, offset ->
          text = Enum.map_join(chunk, &elem(&1, 0))
          selected = NativeGlyphSourceValidator.slice_run(run, offset, text)
          {selected, offset + byte_size(text)}
        end)
        |> elem(0)
      else
        [run]
      end
    end)
  end

  defp transition(previous, following) do
    shared =
      Enum.zip(previous, following) |> Enum.take_while(fn {a, b} -> a.id == b.id end) |> length()

    closing =
      previous |> Enum.drop(shared) |> Enum.reverse() |> Enum.flat_map(&controls(&1, :close))

    opening = following |> Enum.drop(shared) |> Enum.flat_map(&controls(&1, :open))
    closing ++ opening
  end

  defp controls(context, endpoint) do
    rtl? = context.direction == :rtl

    case {context.mode, endpoint} do
      {:embed, :open} ->
        [if(rtl?, do: 0x202B, else: 0x202A)]

      {:embed, :close} ->
        [0x202C]

      {:bidi_override, :open} ->
        [if(rtl?, do: 0x202E, else: 0x202D)]

      {:bidi_override, :close} ->
        [0x202C]

      {:isolate, :open} ->
        [if(rtl?, do: 0x2067, else: 0x2066)]

      {:isolate, :close} ->
        [0x2069]

      {:isolate_override, :open} ->
        [if(rtl?, do: 0x2067, else: 0x2066), if(rtl?, do: 0x202E, else: 0x202D)]

      {:isolate_override, :close} ->
        [0x202C, 0x2069]

      {:plaintext, :open} ->
        [0x2068]

      {:plaintext, :close} ->
        [0x2069]
    end
  end
end
