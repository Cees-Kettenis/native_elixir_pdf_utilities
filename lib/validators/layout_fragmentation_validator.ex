defmodule NativeElixirPdfUtilities.Validators.LayoutFragmentationValidator do
  @moduledoc "Owns whole-unit cuts for parallel layout contexts and shared row continuation."
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Prepares page/column break controls with a context-specific avoidance lifetime."
  @spec prepare_break(String.t(), String.t()) :: {:ok, atom()} | :error
  def prepare_break(property, value) do
    values = %{
      "auto" => :auto,
      "avoid" => :avoid,
      "avoid-page" => :avoid_page,
      "avoid-column" => :avoid_column
    }

    values =
      if property == "break-inside",
        do: values,
        else:
          Map.merge(values, %{
            "page" => :page,
            "always" => :page,
            "column" => :column,
            "left" => :left,
            "right" => :right,
            "recto" => :recto,
            "verso" => :verso
          })

    Map.fetch(values, value |> String.trim() |> String.downcase(:ascii))
  end

  @doc "Checks break controls before their fragmentainer context is assigned."
  @spec validate_style(map(), String.t()) :: :ok | {:error, term()}
  def validate_style(style, source) do
    case style do
      style
      when (not is_map_key(style, :break_before) or :erlang.map_get(:break_before, style) == :auto) and
             (not is_map_key(style, :break_after) or :erlang.map_get(:break_after, style) == :auto) and
             (not is_map_key(style, :break_inside) or
                :erlang.map_get(:break_inside, style) == :auto) ->
        :ok

      _ ->
        valid? =
          Map.get(style, :break_before, :auto) in [
            :auto,
            :avoid,
            :avoid_page,
            :avoid_column,
            :page,
            :column,
            :left,
            :right,
            :recto,
            :verso
          ] and
            Map.get(style, :break_after, :auto) in [
              :auto,
              :avoid,
              :avoid_page,
              :avoid_column,
              :page,
              :column,
              :left,
              :right,
              :recto,
              :verso
            ] and
            Map.get(style, :break_inside, :auto) in [:auto, :avoid, :avoid_page, :avoid_column]

        if valid?,
          do: :ok,
          else:
            Diagnostics.error(
              :layout,
              :invalid_layout,
              "break controls require a supported page/column break or avoidance value",
              operation: :layout,
              module: __MODULE__,
              source: source
            )
    end
  end

  @doc "Moves a source cut upward until it no longer intersects any reserved whole unit."
  @spec whole_unit_cut([{number(), number()}], number(), number()) :: number()
  def whole_unit_cut(units, top, cut) do
    HtmlValidator.reserve_render_resource(:max_work, length(units) + 1, :pagination)

    next =
      Enum.reduce(units, cut, fn {unit_top, unit_bottom}, boundary ->
        if unit_top <= top and unit_top > boundary and unit_bottom < boundary,
          do: max(boundary, unit_top),
          else: boundary
      end)

    if next == cut, do: cut, else: whole_unit_cut(units, top, next)
  end

  @doc "Builds shared fragment reservations from validated flex/grid item allocations and original paragraph lines."
  @spec prepare_parallel_units([map()], number()) ::
          {:ok, [{number(), number()}]} | {:error, term()}
  def prepare_parallel_units(boxes, page_height) do
    Enum.reduce_while(boxes, {:ok, [], %{}}, fn box, {:ok, units, paragraphs} ->
      HtmlValidator.reserve_render_resource(:max_work, 1, :pagination)

      case Map.get(box, :parallel_item_bounds) do
        {top, bottom} when is_number(top) and is_number(bottom) and top >= bottom ->
          # A fitting flex line or grid row stays together; taller rows expose
          # their canonical line/atomic boundaries to the page assignment.
          units = if top - bottom <= page_height, do: [{top, bottom} | units], else: units

          decorative? =
            Map.get(box, :type) in [:rect, :flow_marker, :page_break] or
              Map.get(box, :out_of_flow, false)

          units = if decorative?, do: units, else: [PageGeometry.box_vertical_bounds(box) | units]

          case Map.get(box, :parallel_source_flow) do
            nil ->
              {:cont, {:ok, units, paragraphs}}

            %{flow_id: id, fragment_id: {:line, id, index}} = source
            when not is_nil(id) and is_integer(index) and index >= 0 ->
              with :ok <- HtmlValidator.validate_paragraph_line_counts(source, :pagination) do
                bounds = PageGeometry.box_vertical_bounds(box)
                paragraph = Map.get(paragraphs, id, %{source: source, lines: %{}})

                lines =
                  Map.update(paragraph.lines, index, bounds, fn {top, bottom} ->
                    {next_top, next_bottom} = bounds
                    {max(top, next_top), min(bottom, next_bottom)}
                  end)

                {:cont, {:ok, units, Map.put(paragraphs, id, %{paragraph | lines: lines})}}
              else
                error -> {:halt, error}
              end

            _ ->
              {:halt, invalid_parallel_flow()}
          end

        nil ->
          {:cont, {:ok, units, paragraphs}}

        _ ->
          {:halt, invalid_parallel_flow()}
      end
    end)
    |> case do
      {:ok, units, paragraphs} ->
        reservations =
          Enum.flat_map(paragraphs, fn {_, paragraph} ->
            lines = paragraph.lines |> Enum.sort_by(&elem(&1, 0)) |> Enum.map(&elem(&1, 1))
            orphans = Map.get(paragraph.source, :orphans, 2)
            widows = Map.get(paragraph.source, :widows, 2)

            groups =
              if length(lines) < orphans + widows,
                do: [lines],
                else: [Enum.take(lines, orphans), Enum.take(lines, -widows)]

            Enum.flat_map(groups, fn lines ->
              top = lines |> Enum.map(&elem(&1, 0)) |> Enum.max()
              bottom = lines |> Enum.map(&elem(&1, 1)) |> Enum.min()
              if top - bottom <= page_height, do: [{top, bottom}], else: []
            end)
          end)

        {:ok, units ++ reservations}

      error ->
        error
    end
  end

  defp invalid_parallel_flow do
    Diagnostics.error(
      :pagination,
      :invalid_layout,
      "flex/grid continuation requires ordered item allocations and canonical paragraph line metadata",
      operation: :paginate,
      module: __MODULE__
    )
  end
end
