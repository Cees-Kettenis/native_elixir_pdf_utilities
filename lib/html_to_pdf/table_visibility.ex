defmodule NativeElixirPdfUtilities.HtmlToPdf.TableVisibility do
  @moduledoc "Collapses table tracks after intrinsic sizing, retaining source clipping and annotation coordinates."
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, OverflowGeometry, PageGeometry}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Retains the original tracks for sizing and identifies the rows and columns removed from painting."
  @spec prepare(map(), [map()]) :: map() | nil
  def prepare(tracks, columns) do
    collapsed? =
      Enum.any?(tracks.rows, fn row ->
        match?(%{row: %{style: %{visibility: :collapse}}}, row) or
          match?(%{group_style: %{visibility: :collapse}}, row)
      end) or Enum.any?(columns, &match?(%{visibility_collapse: true}, &1))

    if collapsed? do
      rows =
        Enum.map(tracks.rows, fn row ->
          Map.get(row.row.style, :visibility) == :collapse or
            Map.get(row.group_style || %{}, :visibility) == :collapse
        end)

      {spacing, _} = tracks.spacing

      {bands, _} =
        Enum.map_reduce(Enum.with_index(tracks.widths), spacing, fn {width, index}, left ->
          collapsed? = Map.get(Enum.at(columns, index, %{}), :visibility_collapse, false)
          {if(collapsed?, do: {left, left + width + spacing}), left + width + spacing}
        end)

      collapsed_columns =
        bands
        |> Enum.with_index()
        |> Enum.filter(fn {band, _} -> band != nil end)
        |> MapSet.new(&elem(&1, 1))

      bands = Enum.reject(bands, &is_nil/1)

      if Enum.any?(rows) or bands != [],
        do: %{
          rows: rows,
          columns: bands,
          collapsed_columns: collapsed_columns,
          width: Enum.reduce(bands, 0.0, fn {left, right}, sum -> sum + right - left end)
        }
    end
  end

  @doc "Removes collapsed grid bands without reflowing the widths that supplied intrinsic table sizing."
  @spec collapse([map()], number(), map(), number(), number()) :: {[map()], number(), [map()]}
  def collapse(fragments, vertical_spacing, plan, x, bottom) do
    x_bands = Enum.map(plan.columns, fn {left, right} -> {x + left, x + right} end)

    y_bands =
      for {fragment, collapsed?} <- Enum.zip(fragments, plan.rows),
          collapsed?,
          do: {fragment.bottom - vertical_spacing, fragment.top}

    y_bands = Enum.sort(y_bands)

    fragments =
      Enum.zip(fragments, plan.rows)
      |> Enum.flat_map(fn {fragment, collapsed?} ->
        if collapsed? do
          []
        else
          boxes = compress(fragment.boxes, x_bands, y_bands)

          baseline =
            if is_number(fragment.baseline),
              do: coordinate(fragment.baseline, y_bands, :y),
              else: nil

          [
            %{
              fragment
              | boxes: boxes,
                top: coordinate(fragment.top, y_bands, :y),
                bottom: coordinate(fragment.bottom, y_bands, :y),
                baseline: baseline
            }
          ]
        end
      end)

    {Enum.flat_map(fragments, & &1.boxes), coordinate(bottom, y_bands, :y), fragments}
  end

  defp compress(boxes, x_bands, y_bands) do
    HtmlValidator.reserve_render_resource(
      :max_table_grid_work,
      length(boxes) * (length(x_bands) + length(y_bands) + 1),
      :layout
    )

    Enum.flat_map(boxes, fn box ->
      {top, bottom} = PageGeometry.box_vertical_bounds(box)

      if box.width == 0.0 or top == bottom do
        DrawingGeometry.translate_boxes(
          [box],
          coordinate(box.x, x_bands, :x) - box.x,
          coordinate(bottom, y_bands, :y) - bottom
        )
      else
        {left, bottom, right, top} =
          OverflowGeometry.ink_bounds([box], {box.x, bottom, box.x + box.width, top})

        horizontal = intervals(left, right, x_bands)
        vertical = intervals(bottom, top, y_bands)

        HtmlValidator.reserve_render_resource(
          :max_layout_boxes,
          length(horizontal) * length(vertical),
          :layout
        )

        for {left, right} <- horizontal, {bottom, top} <- vertical, reduce: [] do
          boxes ->
            clip = %{x: left, y: bottom, width: right - left, height: top - bottom}

            boxes ++
              (DrawingGeometry.clip_boxes([box], clip)
               |> DrawingGeometry.translate_boxes(
                 coordinate(left, x_bands, :x) - left,
                 coordinate(bottom, y_bands, :y) - bottom
               ))
        end
      end
    end)
  end

  defp intervals(low, high, bands) do
    Enum.reduce(bands, [{low, high}], fn {start, finish}, intervals ->
      Enum.flat_map(intervals, fn {left, right} ->
        if start < right and finish > left do
          if(left < start, do: [{left, min(start, right)}], else: []) ++
            if finish < right, do: [{max(finish, left), right}], else: []
        else
          [{left, right}]
        end
      end)
    end)
  end

  defp coordinate(value, bands, axis) do
    Enum.reduce(bands, value, fn {low, high}, result ->
      if axis == :x,
        do: result - max(min(value - low, high - low), 0.0),
        else: result + max(min(high - value, high - low), 0.0)
    end)
  end
end
