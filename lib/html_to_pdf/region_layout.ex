defmodule NativeElixirPdfUtilities.HtmlToPdf.RegionLayout do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, Layout, PageGeometry, Pagination}
  alias NativeElixirPdfUtilities.Validators.{CssLengthValidator, HtmlValidator}

  @doc false
  @spec prepare(map(), (list(), map(), number() -> term())) :: {:ok, map()} | {:error, term()}
  def prepare(tree, render) do
    {nodes, {sources, frames}} = collect(tree.children, {%{}, %{}})

    with {:ok, sheets} <- build_sheets(frames, sources, render),
         do: {:ok, %{tree | children: install(nodes, sheets)}}
  end

  defp collect(nodes, state) do
    Enum.flat_map_reduce(nodes, state, fn
      %{style: %{display: :none}} = node, state ->
        {[node], state}

      %{style: style, children: children} = node, state ->
        {children, {sources, frames}} = collect(children, state)
        node = %{node | children: children}

        case style do
          %{flow_into: {name, mode}} ->
            source =
              if mode == :content,
                do: children,
                else: [%{node | style: Map.delete(style, :flow_into)}]

            sources = Map.update(sources, name, [source], &[source | &1])
            kept = if mode == :content, do: [%{node | children: []}], else: []
            {kept, {sources, frames}}

          %{flow_from: name} when is_binary(name) ->
            id = make_ref()
            frames = Map.update(frames, name, [{id, style}], &[{id, style} | &1])

            {[%{node | style: Map.put(style, :_region_frame, id), children: []}],
             {sources, frames}}

          _ ->
            {[node], {sources, frames}}
        end

      node, state ->
        {[node], state}
    end)
  end

  defp build_sheets(frames, sources, render) do
    Enum.reduce_while(frames, {:ok, %{}}, fn {name, list}, {:ok, sheets} ->
      frames = Enum.reverse(list)
      nodes = sources |> Map.get(name, []) |> Enum.reverse() |> Enum.concat()

      case frames_layout(frames, nodes, render) do
        {:ok, result} -> {:cont, {:ok, Map.merge(sheets, result)}}
        error -> {:halt, error}
      end
    end)
  end

  defp frames_layout(frames, nodes, render) do
    case nodes do
      [] ->
        {:ok, Map.new(frames, fn {id, _} -> {id, {[], 0.0}} end)}

      _ ->
        [{_, first} | _] = frames

        with {:ok, width, heights} <- compatible_frames(frames),
             {:ok, boxes, flow_height} <- render.(nodes, first, width) do
          {_id, last_style} = List.last(frames)
          overflow? = Map.get(last_style, :region_fragment, :auto) == :auto

          capacities =
            if overflow?, do: List.update_at(heights, -1, &max(&1, flow_height)), else: heights

          with {:ok, sheets} <-
                 Pagination.fragment_regions(region_breaks(boxes), width, capacities) do
            last = length(frames) - 1

            {:ok,
             Map.new(Enum.with_index(frames), fn {{id, _style}, index} ->
               pages =
                 if index == last and overflow?,
                   do: Enum.drop(sheets, index),
                   else: Enum.slice(sheets, index, 1)

               {id, local_artwork(pages)}
             end)}
          end
        end
    end
  end

  defp compatible_frames(frames) do
    sizes = Enum.map(frames, fn {_, style} -> frame_size(style) end)

    case sizes do
      [{:ok, width, _} | _] ->
        if Enum.all?(sizes, &match?({:ok, ^width, _}, &1)),
          do: {:ok, width, Enum.map(sizes, &elem(&1, 2))},
          else: unsupported_frame()

      [error | _] ->
        error
    end
  end

  defp local_artwork(pages) do
    {sheets, height} =
      Enum.map_reduce(pages, 0.0, fn sheet, used ->
        top = elem(sheet.size, 1)

        bottom =
          Enum.reduce(sheet.boxes, top, fn box, lowest ->
            {_, bottom} = PageGeometry.box_vertical_bounds(box)
            min(lowest, bottom)
          end)

        {DrawingGeometry.translate_boxes(sheet.boxes, 0.0, -top - used), used + top - bottom}
      end)

    {Enum.concat(sheets), height}
  end

  defp region_breaks(boxes) do
    Enum.map(boxes, fn box ->
      box
      |> Map.update(:break_before, :auto, &region_break/1)
      |> Map.update(:break_after, :auto, &region_break/1)
      |> Map.update(:break_inside, :auto, &region_break/1)
    end)
  end

  defp region_break(value) do
    case value do
      :region -> :column
      :avoid_region -> :avoid_column
      ordinary -> ordinary
    end
  end

  defp frame_size(style) do
    case style do
      %{display: :block, width: width, height: height}
      when is_number(width) and is_number(height) ->
        with {:ok, style} <- CssLengthValidator.resolve_edges(style) do
          width = Layout.resolved_content_size(style, :width, nil, width)
          height = Layout.resolved_content_size(style, :height, nil, height)
          if width > 0 and height > 0, do: {:ok, width, height}, else: unsupported_frame()
        end

      _ ->
        unsupported_frame()
    end
  end

  defp unsupported_frame do
    Diagnostics.error(
      :layout,
      :not_implemented,
      "named print flows require block regions with equal definite widths and definite heights",
      module: __MODULE__,
      source: "flow-from"
    )
  end

  defp install(nodes, sheets) do
    Enum.map(nodes, fn
      %{style: %{_region_frame: frame} = style} = node ->
        %{node | style: Map.put(style, :_region_paint, Map.fetch!(sheets, frame))}

      %{children: children} = node ->
        %{node | children: install(children, sheets)}

      node ->
        node
    end)
  end

  @doc false
  @spec place({[map()], number()}, number(), number()) :: {:ok, [map()], number()}
  def place({boxes, height}, x, y) do
    HtmlValidator.reserve_render_resource(:max_work, length(boxes), :layout)
    {:ok, DrawingGeometry.translate_boxes(boxes, x, y), height}
  end
end
