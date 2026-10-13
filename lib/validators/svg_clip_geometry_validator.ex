defmodule NativeElixirPdfUtilities.Validators.SvgClipGeometryValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgClipGeometry
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Prepares native resolved clip expressions for geometric annotation projection."
  @spec prepare(term()) :: {:ok, term()} | {:error, {atom(), map()}}
  def prepare(codes) do
    HtmlValidator.with_render_budget(fn ->
      try do
        with true <- is_list(codes),
             :ok <-
               HtmlValidator.reserve_render_resource(
                 :max_svg_clip_projection_work,
                 length(codes),
                 :svg
               ),
             true <-
               Enum.all?(codes, &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))),
             {:ok, expression} <- expression(codes, []) do
          {:ok, expression}
        else
          _ -> invalid("Native resolved SVG clip geometry is malformed")
        end
      catch
        {:render_resource_limit, error} -> error
      end
    end)
  end

  @doc "Projects a validated clip and transformed link polygon into bounded PDF quadrilaterals."
  @spec project(term(), [{number(), number()}], tuple()) ::
          {:ok, [[number()]]} | {:error, {atom(), map()}}
  def project(expression, polygon, viewport) do
    HtmlValidator.with_render_budget(fn ->
      try do
        with {left, bottom, right, top} <- viewport,
             true <- is_list(polygon) and length(polygon) == 4,
             true <-
               Enum.all?(polygon, fn point ->
                 case point do
                   {x, y} ->
                     match?({:ok, _}, HtmlValidator.normalize_geometry_number(x)) and
                       match?({:ok, _}, HtmlValidator.normalize_geometry_number(y))

                   _ ->
                     false
                 end
               end),
             true <- SvgClipGeometry.convex?(polygon),
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

          polygon = SvgClipGeometry.polygon(polygon)

          viewport =
            SvgClipGeometry.polygon([{left, bottom}, {right, bottom}, {right, top}, {left, top}])

          expression = {:intersection, [expression || {:intersection, []}, polygon, viewport]}
          all_edges = SvgClipGeometry.edges(expression)
          {x1, y1, x2, y2} = bounds

          edges =
            SvgClipGeometry.clip_edges(all_edges, bounds) ++
              SvgClipGeometry.edges(
                SvgClipGeometry.polygon([{x1, y1}, {x2, y1}, {x2, y2}, {x1, y2}])
              )

          # Reserve the complete arrangement before its pairwise intersection work.
          HtmlValidator.reserve_render_resource(
            :max_svg_clip_projection_work,
            length(all_edges) + length(edges) * length(edges),
            :svg
          )

          quads = if x2 > x1 and y2 > y1, do: SvgClipGeometry.project(expression, edges), else: []
          # The PDF writer has fixed four-decimal point precision. Quantizing the
          # returned hit geometry there removes zero-area slabs and roundoff at
          # exact edge crossings without borrowing any raster coordinates.
          quads =
            quads
            |> Enum.map(fn quad -> Enum.map(quad, &Float.round(&1 * 1.0, 4)) end)
            |> Enum.filter(fn [x1, y1, x2, y2, x3, y3, x4, y4] ->
              SvgClipGeometry.convex?([{x1, y1}, {x2, y2}, {x4, y4}, {x3, y3}])
            end)

          HtmlValidator.reserve_render_resource(:max_layout_boxes, length(quads), :svg)
          {:ok, quads}
        else
          _ ->
            invalid(
              "SVG link projection requires four finite convex corners and a finite viewport"
            )
        end
      catch
        {:render_resource_limit, error} -> error
      end
    end)
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
          end) and SvgClipGeometry.convex?(points)

      _ ->
        false
    end
  end

  defp expression(codes, stack) do
    case codes do
      [] ->
        case stack do
          [{expression, _depth}] -> {:ok, expression}
          _ -> :error
        end

      [10, rule, count | rest] when rule in [0, 1] and is_integer(count) and count >= 0 ->
        with {:ok, commands, rest} <- commands(rest, count, [], false) do
          path =
            SvgClipGeometry.flatten(commands, rule, 1 / Limits.get(:svg_clip_curve_precision))

          expression(rest, [{path, 1} | stack])
        end

      [operation, count | rest] when operation in [20, 21] and is_integer(count) and count >= 0 ->
        if count <= length(stack) do
          {children, stack} = Enum.split(stack, count)
          kind = if operation == 20, do: :union, else: :intersection
          depth = 1 + Enum.reduce(children, 0, fn {_, depth}, maximum -> max(depth, maximum) end)

          HtmlValidator.check_render_resource(:max_svg_depth, depth, :svg)
          expression(rest, [{{kind, Enum.map(children, &elem(&1, 0))}, depth} | stack])
        else
          :error
        end

      _ ->
        :error
    end
  end

  defp commands(codes, count, commands, started?) do
    case {count, codes} do
      {0, _} ->
        {:ok, Enum.reverse(commands), codes}

      {_, [op | rest]} when op in [0, 1, 2, 3, 4] ->
        size = Enum.at([2, 2, 4, 6, 0], op)
        {coordinates, rest} = Enum.split(rest, size)

        if length(coordinates) == size and (started? or op == 0) do
          commands(rest, count - 1, [{op, coordinates} | commands], true)
        else
          :error
        end

      _ ->
        :error
    end
  end

  defp invalid(message) do
    Diagnostics.error(:svg, :invalid_document, message,
      module: __MODULE__,
      operation: :project,
      source: "NativeHtmlClipGeometry"
    )
  end
end
