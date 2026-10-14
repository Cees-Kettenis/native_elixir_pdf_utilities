defmodule NativeElixirPdfUtilities.Validators.ClipGeometryValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.ClipGeometry
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Projects a validated clip and transformed HTML link polygon into bounded PDF quadrilaterals."
  @spec project(term(), [{number(), number()}], tuple()) ::
          {:ok, [[number()]]} | {:error, {atom(), map()}}
  def project(expression, polygon, viewport) do
    HtmlValidator.with_render_budget(fn ->
      try do
        with :ok <- if(is_nil(expression), do: :ok, else: validate_expression(expression, 1)),
             {left, bottom, right, top} <- viewport,
             {:ok, polygon} <- prepare_polygon(polygon),
             true <-
               Enum.all?(
                 Tuple.to_list(viewport),
                 &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
               ) do
          xs = Enum.map(polygon, &elem(&1, 0))
          ys = Enum.map(polygon, &elem(&1, 1))

          bounds =
            {max(left, Enum.min(xs)), max(bottom, Enum.min(ys)), min(right, Enum.max(xs)),
             min(top, Enum.max(ys))}

          polygon = ClipGeometry.polygon(polygon)

          viewport =
            ClipGeometry.polygon([{left, bottom}, {right, bottom}, {right, top}, {left, top}])

          expression = {:intersection, [expression || {:intersection, []}, polygon, viewport]}
          all_edges = ClipGeometry.edges(expression)
          {x1, y1, x2, y2} = bounds

          edges =
            ClipGeometry.clip_edges(all_edges, bounds) ++
              ClipGeometry.edges(ClipGeometry.polygon([{x1, y1}, {x2, y1}, {x2, y2}, {x1, y2}]))

          # Reserve the real edge preparation. The arrangement sweep reserves
          # only pairs whose vertical ranges can actually intersect.
          HtmlValidator.reserve_render_resource(
            :max_clip_projection_work,
            length(all_edges) + length(edges),
            :layout
          )

          quads = if x2 > x1 and y2 > y1, do: ClipGeometry.project(expression, edges), else: []
          # The PDF writer has fixed four-decimal point precision. Quantizing the
          # returned hit geometry there removes zero-area slabs and roundoff at
          # exact edge crossings without borrowing any raster coordinates.
          quads =
            quads
            |> Enum.map(fn quad -> Enum.map(quad, &Float.round(&1 * 1.0, 4)) end)
            |> Enum.filter(fn [x1, y1, x2, y2, x3, y3, x4, y4] ->
              ClipGeometry.convex?([{x1, y1}, {x2, y2}, {x4, y4}, {x3, y3}])
            end)

          HtmlValidator.reserve_render_resource(:max_layout_boxes, length(quads), :layout)
          {:ok, quads}
        else
          {:error, {_reason, _diagnostic}} = error ->
            error

          _ ->
            invalid(
              "PDF link projection requires four finite convex corners and a finite viewport"
            )
        end
      catch
        {:render_resource_limit, error} -> error
      end
    end)
  end

  @doc "Prepares four finite convex corners from the actual transformed PDF bounds."
  @spec prepare_polygon(term()) :: {:ok, [{number(), number()}]} | {:error, {atom(), map()}}
  def prepare_polygon(polygon) do
    if is_list(polygon) and length(polygon) == 4 and
         Enum.all?(polygon, fn point ->
           case point do
             {x, y} ->
               match?({:ok, _}, HtmlValidator.normalize_geometry_number(x)) and
                 match?({:ok, _}, HtmlValidator.normalize_geometry_number(y))

             _ ->
               false
           end
         end) and ClipGeometry.convex?(polygon),
       do: {:ok, polygon},
       else:
         invalid("PDF link projection requires four finite convex corners", "PDF link polygon")
  end

  @doc "Checks an optional PDF link quadrilateral against its authored link rectangle."
  @spec valid_quad?(term(), map()) :: boolean()
  def valid_quad?(quad, box) do
    case quad do
      nil ->
        true

      [x1, y1, x2, y2, x3, y3, x4, y4] ->
        points = [{x1, y1}, {x2, y2}, {x4, y4}, {x3, y3}]

        box.type == :link_region and
          Enum.all?(points, fn {x, y} ->
            match?({:ok, _}, HtmlValidator.normalize_geometry_number(x)) and
              match?({:ok, _}, HtmlValidator.normalize_geometry_number(y)) and
              Float.round(x * 1.0, 4) >= Float.round(box.x * 1.0, 4) and
              Float.round(x * 1.0, 4) <= Float.round((box.x + box.width) * 1.0, 4) and
              Float.round(y * 1.0, 4) >= Float.round(box.y * 1.0, 4) and
              Float.round(y * 1.0, 4) <= Float.round((box.y + box.height) * 1.0, 4)
          end) and ClipGeometry.convex?(points)

      _ ->
        false
    end
  end

  # Public callers may supply a clip expression; only canonical finite HTML
  # box contours and intersections may reach the geometry engine.
  defp validate_expression(expression, depth) do
    HtmlValidator.check_render_resource(:max_clip_expression_depth, depth, :layout)

    case expression do
      {:contour, edges} when is_list(edges) ->
        HtmlValidator.reserve_render_resource(
          :max_clip_projection_work,
          length(edges) + 1,
          :layout
        )

        if Enum.all?(edges, fn edge ->
             case edge do
               {{x, y}, {a, b}} ->
                 Enum.all?([x, y, a, b], fn value ->
                   match?({:ok, _}, HtmlValidator.normalize_geometry_number(value))
                 end)

               _ ->
                 false
             end
           end), do: :ok, else: invalid("PDF clip contours require finite point pairs")

      {:intersection, children} when is_list(children) ->
        HtmlValidator.reserve_render_resource(
          :max_clip_projection_work,
          length(children) + 1,
          :layout
        )

        Enum.reduce_while(children, :ok, fn child, :ok ->
          case validate_expression(child, depth + 1) do
            :ok -> {:cont, :ok}
            error -> {:halt, error}
          end
        end)

      _ ->
        invalid("PDF clip projection requires canonical box contours and intersections")
    end
  end

  defp invalid(message, source \\ "PDF clip geometry") do
    Diagnostics.error(:layout, :invalid_document, message,
      module: __MODULE__,
      operation: :project,
      source: source
    )
  end
end
