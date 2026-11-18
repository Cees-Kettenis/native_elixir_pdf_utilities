defmodule NativeElixirPdfUtilities.HtmlToPdf.GridLanes do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc false
  @spec place([map()], pos_integer(), number(), number() | :infinite, function()) ::
          {[map()], number()}
  def place(items, count, gap, tolerance, measure) do
    lanes = List.duplicate(0.0, count)

    {placed, {lanes, _cursor}} =
      Enum.map_reduce(items, {lanes, 1}, fn item, {lanes, cursor} ->
        span = item.lane_span
        HtmlValidator.reserve_render_resource(:max_work, count * span, :layout)

        {line, offset} =
          if is_integer(item.lane_start) do
            {item.lane_start, current(lanes, item.lane_start, span)}
          else
            candidates = Enum.map(1..(count - span + 1), &{&1, current(lanes, &1, span)})
            smallest = candidates |> Enum.map(&elem(&1, 1)) |> Enum.min()

            options =
              if tolerance == :infinite,
                do: candidates,
                else:
                  Enum.filter(candidates, fn {_line, start} -> start <= smallest + tolerance end)

            Enum.find(options, &(elem(&1, 0) >= cursor)) || hd(options)
          end

        {item, length} = measure.(item, line, offset)
        finish = offset + length + gap

        lanes =
          lanes
          |> Enum.with_index(1)
          |> Enum.map(fn {used, index} ->
            if index >= line and index < line + span, do: max(used, finish), else: used
          end)

        {item, {lanes, if(is_integer(item.lane_start), do: cursor, else: line + span)}}
      end)

    {placed, max(Enum.max(lanes) - gap, 0.0)}
  end

  defp current(lanes, first, span), do: lanes |> Enum.slice(first - 1, span) |> Enum.max()
end
