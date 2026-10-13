defmodule NativeElixirPdfUtilities.Validators.MathPaintValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, MathLayout}
  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.Validators.{
    FontValidator,
    HtmlValidator,
    MathLayoutValidator,
    WriterValidator
  }

  @doc "Measures visible Math paint separately from authored layout advance, in baseline-relative points."
  @spec prepare_geometry(term()) ::
          {:ok, {number(), number(), number(), number()} | nil}
          | {:error, {atom(), NativeElixirPdfUtilities.Diagnostics.diagnostic()}}
  def prepare_geometry(measured) do
    MathLayout.with_context(fn ->
      with :ok <- MathLayoutValidator.validate_measured_geometry(measured) do
        Enum.reduce_while(measured.drawing_items, {:ok, nil}, fn item, {:ok, bounds} ->
          FontValidator.reserve(:max_font_math_work, 1)

          result =
            case item do
              %{kind: :glyph} ->
                item
                |> Map.put(:glyph_ids, [item.glyph_id])
                |> FontValidator.prepare_text_ink_rectangle()

              %{kind: :native, box: box, x: x, y: y} ->
                [translated] = DrawingGeometry.translate_boxes([box], x, y)
                native_geometry(translated, nil)

              %{kind: :background, x: x, y: y, width: width, height: height} ->
                {:ok, if(width > 0 and height > 0, do: {x, y, x + width, y + height})}

              %{kind: :logical} ->
                {:ok, nil}
            end

          case result do
            {:ok, paint} -> {:cont, {:ok, union(bounds, paint)}}
            error -> {:halt, error}
          end
        end)
      end
    end)
  end

  @doc "Prepares emitted Math paint flow markers after actual placement and CSS clipping, preserving authored advance."
  @spec prepare_flow_boxes(term(), term(), term()) ::
          {:ok, [map()]} | {:error, {atom(), NativeElixirPdfUtilities.Diagnostics.diagnostic()}}
  def prepare_flow_boxes(boxes, page_width, page_height) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, dimensions} <-
             HtmlValidator.prepare_viewport_dimensions(page_width, page_height) do
        prepare_flow_links(boxes, dimensions, [])
      end
    end)
  end

  @doc "Fits an actual in-flow Math paint group below the printable top using the existing writer paint positions."
  @spec prepare_pagination_top(term(), term(), term(), term(), term()) ::
          {:ok, number()} | {:error, {atom(), NativeElixirPdfUtilities.Diagnostics.diagnostic()}}
  def prepare_pagination_top(boxes, group_top, target_top, page_size, page_top) do
    MathLayout.with_context(fn ->
      case page_size do
        {page_width, page_height} ->
          with {:ok, boxes} <- prepare_flow_boxes(boxes, page_width, page_height) do
            if Enum.any?(
                 boxes,
                 &(Map.get(&1, :role) == :math_paint and not Map.get(&1, :out_of_flow, false))
               ) do
              with {:ok, group_top} <- HtmlValidator.normalize_geometry_number(group_top),
                   {:ok, target_top} <- HtmlValidator.normalize_geometry_number(target_top),
                   {:ok, page_top} <- HtmlValidator.normalize_geometry_number(page_top) do
                target_top = min(target_top, page_top)
                shifted = DrawingGeometry.translate_boxes(boxes, 0.0, target_top - group_top)

                with {:ok, paint} <-
                       native_children_geometry(shifted, {1, 1, 0, 0, nil, page_height}) do
                  case paint do
                    {_left, _bottom, _right, top} when top > page_top ->
                      # A CSS pixel is fixed at 0.75pt. Move the entire atomic group
                      # across the writer's snap boundary, never round Math lengths.
                      shift = Float.ceil((top - page_top) / 0.75) * 0.75
                      {:ok, target_top - shift}

                    _ ->
                      {:ok, target_top}
                  end
                end
              else
                _ ->
                  Diagnostics.error(
                    :layout,
                    :invalid_layout,
                    "Math pagination requires bounded group and printable-top geometry",
                    operation: :paginate,
                    module: __MODULE__,
                    source: "math"
                  )
              end
            else
              if is_number(target_top),
                do: {:ok, target_top},
                else:
                  Diagnostics.error(
                    :layout,
                    :invalid_layout,
                    "pagination requires a numeric prepared top",
                    operation: :paginate,
                    module: __MODULE__
                  )
            end
          else
            {:error, _} = error ->
              error

            _ ->
              Diagnostics.error(
                :layout,
                :invalid_layout,
                "Math pagination requires bounded group and printable-top geometry",
                operation: :paginate,
                module: __MODULE__,
                source: "math"
              )
          end

        _ ->
          Diagnostics.error(
            :layout,
            :invalid_layout,
            "Math pagination requires an actual point page size",
            operation: :paginate,
            module: __MODULE__,
            source: "math"
          )
      end
    end)
  end

  # Reserve each actual link before processing it, and reject improper tails
  # before any Enum traversal. Non-Math layout boxes keep their existing owner.
  defp prepare_flow_links(boxes, dimensions, result) do
    case boxes do
      [] ->
        {:ok, Enum.reverse(result)}

      [box | rest] ->
        HtmlValidator.reserve_render_resource(:max_layout_boxes, 1, :layout)

        case prepare_flow_box(box, dimensions) do
          {:ok, nil} -> prepare_flow_links(rest, dimensions, result)
          {:ok, box} -> prepare_flow_links(rest, dimensions, [box | result])
          error -> error
        end

      _ ->
        Diagnostics.error(
          :layout,
          :invalid_layout,
          "Math paint flow requires a proper list of actual layout boxes",
          operation: :layout,
          module: __MODULE__,
          source: "math"
        )
    end
  end

  defp prepare_flow_box(box, {page_width, page_height}) do
    case box do
      %{
        type: :flow_marker,
        role: :math_paint,
        x: x,
        y: y,
        height: height,
        math_paint_width: width
      }
      when is_number(width) and width >= 0 ->
        with true <- is_boolean(Map.get(box, :out_of_flow, false)),
             :ok <- WriterValidator.validate_native_boxes([box]),
             :ok <- WriterValidator.validate_clip_rectangle(Map.get(box, :clip)),
             {:ok, x} <- HtmlValidator.normalize_geometry_number(x),
             {:ok, y} <- HtmlValidator.normalize_geometry_number(y),
             {:ok, right} <- HtmlValidator.normalize_geometry_number(x + width),
             {:ok, top} <- HtmlValidator.normalize_geometry_number(y + height) do
          {left, bottom, right, top} =
            DrawingGeometry.intersect_clip(
              {x, y, right, top},
              DrawingGeometry.box_clip_bounds(Map.get(box, :clip))
            )

          cond do
            right <= left or top <= bottom ->
              {:ok, nil}

            right <= 0 or left >= page_width or right > page_width or
                top - bottom > page_height ->
              Diagnostics.error(
                :layout,
                :invalid_layout,
                "atomic MathML visible paint spans x=#{left}..#{right}pt and height=#{top - bottom}pt outside the #{page_width} × #{page_height}pt page; increase the page size or adjust the equation placement or font size",
                operation: :layout,
                module: __MODULE__,
                source: "math"
              )

            true ->
              {:ok,
               %{box | x: left, y: bottom, height: top - bottom, math_paint_width: right - left}}
          end
        else
          {:error, _} = error ->
            error

          _ ->
            Diagnostics.error(
              :layout,
              :invalid_layout,
              "Math paint flow requires bounded placed geometry",
              operation: :layout,
              module: __MODULE__,
              source: "math"
            )
        end

      %{role: :math_paint} ->
        Diagnostics.error(
          :layout,
          :invalid_layout,
          "Math paint flow requires a nonnegative visible width on a genuine flow marker",
          operation: :layout,
          module: __MODULE__,
          source: "math"
        )

      %{type: type} when is_atom(type) ->
        {:ok, box}

      _ ->
        Diagnostics.error(
          :layout,
          :invalid_layout,
          "Math paint flow requires actual typed layout boxes",
          operation: :layout,
          module: __MODULE__,
          source: "math"
        )
    end
  end

  # The writer's shared composition supplies actual local transforms and clips.
  # Group viewport rectangles alone do not restrict visible child artwork.
  defp native_geometry(box, composition) do
    FontValidator.reserve(:max_font_math_work, 1)
    {sx, sy, tx, ty, clip, _height} = composition || {1, 1, 0, 0, nil, nil}

    result =
      case box do
        %{type: :drawing_group, boxes: children} ->
          child_composition = DrawingGeometry.compose_group(box, composition)
          native_children_geometry(children, child_composition)

        %{type: :glyph_group, boxes: children} ->
          native_children_geometry(children, composition)

        %{type: :text} ->
          box =
            case composition do
              {_, _, _, _, _, height} when is_number(height) ->
                {x, y} = DrawingGeometry.text_position(box, height)
                %{box | x: x, y: y}

              _ ->
                box
            end

          FontValidator.prepare_text_ink_rectangle(box)

        %{type: type, x: x, y: y, width: width, height: height}
        when type in [:rect, :image, :responsive_image] ->
          {:ok, if(width > 0 and height > 0, do: {x, y, x + width, y + height})}

        _ ->
          {:ok, nil}
      end

    case {box.type, result} do
      {type, {:ok, _}} when type in [:drawing_group, :glyph_group] ->
        result

      {_, {:ok, nil}} ->
        result

      {_, {:ok, {left, bottom, right, top}}} ->
        transform = {sx, sy, tx, ty}
        {left, bottom} = DrawingGeometry.transform_point(left, bottom, transform)
        {right, top} = DrawingGeometry.transform_point(right, top, transform)

        clip =
          DrawingGeometry.intersect_clip(
            clip,
            DrawingGeometry.transform_box_clip(Map.get(box, :clip), transform)
          )

        paint = DrawingGeometry.intersect_clip({left, bottom, right, top}, clip)
        {left, bottom, right, top} = paint
        {:ok, if(right > left and top > bottom, do: paint)}

      {_, error} ->
        error
    end
  end

  defp native_children_geometry(children, composition) do
    Enum.reduce_while(children, {:ok, nil}, fn child, {:ok, bounds} ->
      case native_geometry(child, composition) do
        {:ok, paint} -> {:cont, {:ok, union(bounds, paint)}}
        error -> {:halt, error}
      end
    end)
  end

  defp union(first, second) do
    case {first, second} do
      {nil, bounds} ->
        bounds

      {bounds, nil} ->
        bounds

      {{left, bottom, right, top}, {other_left, other_bottom, other_right, other_top}} ->
        {min(left, other_left), min(bottom, other_bottom), max(right, other_right),
         max(top, other_top)}
    end
  end
end
