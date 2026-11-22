defmodule NativeElixirPdfUtilities.HtmlToPdf.ReadingFlow do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.CssLogicalValidator

  @doc "Scopes the reading-flow presence check to a single render."
  @spec with_document((-> result)) :: result when result: term()
  def with_document(render) do
    key = {__MODULE__, :active}
    previous = Process.put(key, false)

    try do
      render.()
    after
      if is_nil(previous), do: Process.delete(key), else: Process.put(key, previous)
    end
  end

  @doc "Records the reading flow around one layout item without moving its paint."
  @spec attach([map()], map(), map(), :block | :flex | :grid) :: [map()]
  def attach(boxes, container, item, layout) do
    case container do
      %{_reading_scope: scope, reading_flow: flow} ->
        if flow == :source_order or
             (layout == :flex and flow in [:flex_visual, :flex_flow]) or
             (layout == :grid and flow in [:grid_rows, :grid_columns, :grid_order]) do
          order = {Map.get(item.style, :reading_order, 0), key(flow, container, item), item.index}

          if Process.get({__MODULE__, :active}) == false do
            Process.put({__MODULE__, :active}, true)
          end

          Enum.map(boxes, fn box ->
            Map.update(box, :reading_path, [{scope, order}], &[{scope, order} | &1])
          end)
        else
          boxes
        end

      _ ->
        boxes
    end
  end

  defp key(flow, container, item) do
    case flow do
      :source_order ->
        item.index

      flow when flow in [:flex_flow, :grid_order] ->
        Map.get(item.style, :order, 0)

      :grid_rows ->
        {item.row_start, item.column_start, Map.get(item.style, :order, 0)}

      :grid_columns ->
        {item.column_start, item.row_start, Map.get(item.style, :order, 0)}

      :flex_visual ->
        axes = CssLogicalValidator.axes(container)

        {edge(item, axes.block_start), edge(item, axes.inline_start),
         Map.get(item.style, :order, 0)}
    end
  end

  defp edge(item, side) do
    case side do
      :top -> -item.y
      :bottom -> item.y - item.box_height
      :left -> item.x
      :right -> -(item.x + item.box_width)
    end
  end

  @doc false
  @spec line_boundaries([map()], [[map()]]) :: map()
  def line_boundaries(runs, lines) do
    paragraph = make_ref()

    source =
      if match?([_, _ | _], lines) do
        bytes = Enum.reduce(runs, 0, &(byte_size(&1.text) + &2))

        NativeElixirPdfUtilities.Validators.HtmlValidator.reserve_render_resource(
          :max_work,
          bytes,
          :layout
        )

        Enum.map_join(runs, & &1.text)
      else
        ""
      end

    {boundaries, _} =
      lines
      |> Enum.with_index()
      |> Enum.reduce({%{}, 0}, fn {line, index}, {boundaries, previous} ->
        fragments = Enum.filter(line, &is_map_key(&1, :_reading_offset))

        case fragments do
          [] ->
            {boundaries, previous}

          [first | _] ->
            last = List.last(fragments)
            start = first._reading_offset
            # Only restore whitespace discarded by the line fitter. Clipped
            # or replaced source text must never leak into the PDF's ActualText.
            gap =
              if start > previous and start <= byte_size(source) do
                skipped = binary_part(source, previous, start - previous)
                if String.trim(skipped) == "", do: skipped, else: ""
              else
                ""
              end

            {Map.put(boundaries, index, {paragraph, index, gap}),
             last._reading_offset + byte_size(last.text)}
        end
      end)

    boundaries
  end

  @doc "Adds page-local logical text spans before native PDF writing."
  @spec prepare_pages([map()]) :: [map()]
  def prepare_pages(pages) do
    if Process.get({__MODULE__, :active}) != false,
      do: Enum.map(pages, fn page -> %{page | boxes: prepare(page.boxes)} end),
      else: pages
  end

  @doc "Emits PDF ActualText for each page-local flow, preserving the original paint order."
  @spec prepare([map()]) :: [map()]
  def prepare(boxes) do
    if Enum.any?(boxes, &is_map_key(&1, :reading_path)) do
      NativeElixirPdfUtilities.Validators.HtmlValidator.reserve_render_resource(
        :max_work,
        length(boxes),
        :writer
      )

      scopes =
        Enum.group_by(boxes, &scope/1)
        |> Map.delete(nil)
        |> Map.new(fn {scope, boxes} -> {scope, logical_text(boxes)} end)

      {pieces, _} =
        boxes
        |> Enum.chunk_by(&scope/1)
        |> Enum.map_reduce(scopes, fn chunk, scopes ->
          case scope(hd(chunk)) do
            nil ->
              {Enum.map(chunk, &prepare_drawing_group/1), scopes}

            scope ->
              {text, scopes} = Map.pop(scopes, scope, "")

              marked =
                [%{type: :actual_text_start, text: text} | chunk] ++ [%{type: :actual_text_end}]

              {marked, scopes}
          end
        end)

      List.flatten(pieces)
    else
      Enum.map(boxes, &prepare_drawing_group/1)
    end
  end

  defp prepare_drawing_group(box) do
    case box do
      %{type: :drawing_group, boxes: children} when not is_map_key(box, :actual_text) ->
        %{box | boxes: prepare(children)}

      _ ->
        box
    end
  end

  defp scope(box) do
    case box do
      %{reading_path: [{scope, _} | _]} -> scope
      _ -> nil
    end
  end

  defp logical_text(boxes) do
    boxes
    |> Enum.group_by(fn %{reading_path: [{_, order} | _]} -> order end)
    |> Enum.sort_by(&elem(&1, 0))
    |> Enum.map_join("\n", fn {_order, boxes} ->
      boxes
      |> Enum.map(fn box -> Map.update!(box, :reading_path, &tl/1) end)
      |> item_text()
    end)
  end

  defp item_text(boxes) do
    groups = Enum.group_by(boxes, &scope/1)

    {pieces, _} =
      Enum.flat_map_reduce(boxes, groups, fn box, groups ->
        case scope(box) do
          nil ->
            case box_text(box) do
              "" -> {[], groups}
              text -> {[{text, Map.get(box, :_reading_line)}], groups}
            end

          scope ->
            case Map.pop(groups, scope) do
              {nil, groups} -> {[], groups}
              {boxes, groups} -> {[{logical_text(boxes), nil}], groups}
            end
        end
      end)

    {text, _} =
      Enum.map_reduce(pieces, :first, fn {text, line}, previous ->
        separator =
          case {previous, line} do
            {:first, _} -> ""
            {{paragraph, index, _}, {paragraph, index, _}} -> ""
            {{paragraph, _, _}, {paragraph, _, gap}} -> gap
            _ -> " "
          end

        {[separator, text], line}
      end)

    IO.iodata_to_binary(text)
  end

  defp box_text(box) do
    case box do
      %{actual_text: text} -> text
      %{type: :text, text: text} -> text
      %{type: :image, image: %{logical_text: text}} when is_binary(text) -> text
      %{boxes: boxes} -> item_text(boxes)
      _ -> ""
    end
  end
end
