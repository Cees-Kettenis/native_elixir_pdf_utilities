defmodule NativeElixirPdfUtilities.Validators.SvgFormSlotValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.Reader

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    WriterValidator,
    SvgForeignObjectValidator,
    SvgClipGeometryValidator
  }

  @doc false
  @spec prepare(binary(), map()) :: {:ok, map(), [map()]} | {:error, {atom(), map()}}
  def prepare(source, issued) do
    with {:ok, _ids} <- SvgForeignObjectValidator.validate_slots(issued),
         {:ok, context} <- Reader.read_validated(source) do
      Enum.reduce_while(context.document.objects, {:ok, context, []}, fn {ref, object},
                                                                         {:ok, context, slots} ->
        case object.value do
          %{
            "NativeHtmlSlot" => {:string, id},
            "NativeHtmlWidth" => width,
            "NativeHtmlHeight" => height,
            "NativeHtmlTransform" => matrix,
            "Subtype" => {:name, "Form"}
          } ->
            clip = Map.get(object.value, "NativeHtmlClip")

            geometry = Map.get(object.value, "NativeHtmlClipGeometry")

            result =
              with true <- Map.has_key?(issued, id),
                   {:ok, {width, height}} <-
                     HtmlValidator.prepare_viewport_dimensions(width, height),
                   true <- width > 0 and height > 0 and is_list(matrix) and length(matrix) == 6,
                   true <-
                     Enum.all?(
                       matrix,
                       &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
                     ),
                   true <-
                     is_nil(clip) or
                       (is_list(clip) and length(clip) == 4 and
                          Enum.all?(
                            clip,
                            &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
                          )),
                   {:ok, geometry} <-
                     if(is_nil(geometry),
                       do: {:ok, nil},
                       else: SvgClipGeometryValidator.prepare(geometry)
                     ) do
                HtmlValidator.reserve_render_resource(:max_layout_boxes, 1, :layout)

                {:ok,
                 %{
                   ref: ref,
                   id: id,
                   width: width,
                   height: height,
                   transform: List.to_tuple(matrix),
                   original: issued[id],
                   clip: clip,
                   clip_geometry: geometry
                 }}
              else
                {:error, _} = error ->
                  error

                _ ->
                  Diagnostics.error(
                    :svg,
                    :invalid_document,
                    "SVG native HTML slot must match an issued original subtree and bounded viewport",
                    source: id,
                    module: __MODULE__,
                    operation: :compose
                  )
              end

            case result do
              {:ok, slot} -> {:cont, {:ok, context, [slot | slots]}}
              error -> {:halt, error}
            end

          %{"NativeHtmlSlot" => _} ->
            {:halt,
             Diagnostics.error(
               :svg,
               :invalid_document,
               "SVG HTML slot metadata is incomplete or malformed",
               source: "NativeHtmlSlot",
               module: __MODULE__,
               operation: :compose
             )}

          _ ->
            {:cont, {:ok, context, slots}}
        end
      end)
    end
  end

  @doc false
  @spec prepare_links([map()], number(), number()) :: {:ok, [map()]} | {:error, {atom(), map()}}
  def prepare_links(placements, width, height) do
    Enum.reduce_while(placements, {:ok, []}, fn placement, {:ok, links} ->
      frame = %{size: {placement.width, placement.height}, boxes: [placement.group]}

      with {:ok, context} <- WriterValidator.prepare([frame], []) do
        [page] = context.pages

        page.boxes
        |> Enum.filter(&(Map.get(&1, :link_url) != nil or Map.get(&1, :link_destination) != nil))
        |> Enum.reduce_while({:ok, links}, fn box, {:ok, links} ->
          {left, bottom, right, top} = Map.fetch!(box, :drawing_annotation_rect)
          {a, b, c, d, e, f} = placement.transform

          polygons =
            case Map.get(box, :drawing_link_quad_points) do
              nil ->
                [[{left, bottom}, {right, bottom}, {right, top}, {left, top}]]

              points ->
                points
                |> Enum.chunk_every(8)
                |> Enum.map(fn [x1, y1, x2, y2, x3, y3, x4, y4] ->
                  [{x1, y1}, {x2, y2}, {x4, y4}, {x3, y3}]
                end)
            end

          [clip_left, clip_bottom, clip_right, clip_top] =
            Map.get(placement, :clip) || [0, 0, width, height]

          viewport =
            {max(clip_left, 0), max(clip_bottom, 0), min(clip_right, width),
             min(clip_top, height)}

          Enum.reduce_while(polygons, {:ok, links}, fn polygon, {:ok, links} ->
            corners = Enum.map(polygon, fn {x, y} -> {a * x + c * y + e, b * x + d * y + f} end)

            with {:ok, quads} <-
                   SvgClipGeometryValidator.project(
                     Map.get(placement, :clip_geometry),
                     corners,
                     viewport
                   ) do
              added =
                Enum.map(quads, fn quad ->
                  xs = Enum.take_every(quad, 2)
                  ys = quad |> Enum.drop(1) |> Enum.take_every(2)
                  left = Enum.min(xs)
                  bottom = Enum.min(ys)

                  %{
                    type: :link_region,
                    x: left,
                    y: bottom,
                    width: Enum.max(xs) - left,
                    height: Enum.max(ys) - bottom,
                    link_quad_points: quad
                  }
                  |> Map.merge(Map.take(box, [:link_url, :link_destination]))
                end)

              {:cont, {:ok, Enum.reverse(added, links)}}
            else
              error -> {:halt, error}
            end
          end)
          |> case do
            {:ok, links} -> {:cont, {:ok, links}}
            error -> {:halt, error}
          end
        end)
        |> case do
          {:ok, links} -> {:cont, {:ok, links}}
          error -> {:halt, error}
        end
      else
        error -> {:halt, error}
      end
    end)
  end
end
