defmodule NativeElixirPdfUtilities.Validators.SvgFormSlotValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Pdf.Reader

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    WriterValidator,
    SvgForeignObjectValidator
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
                          )) do
                HtmlValidator.reserve_render_resource(:max_layout_boxes, 1, :layout)

                {:ok,
                 %{
                   ref: ref,
                   id: id,
                   width: width,
                   height: height,
                   transform: List.to_tuple(matrix),
                   original: issued[id],
                   clip: clip
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

        added =
          page.boxes
          |> Enum.filter(
            &(Map.get(&1, :link_url) != nil or Map.get(&1, :link_destination) != nil)
          )
          |> Enum.flat_map(fn box ->
            {left, bottom, right, top} = Map.fetch!(box, :drawing_annotation_rect)
            {a, b, c, d, e, f} = placement.transform

            corners =
              Enum.map([{left, bottom}, {left, top}, {right, bottom}, {right, top}], fn {x, y} ->
                {a * x + c * y + e, b * x + d * y + f}
              end)

            xs = Enum.map(corners, &elem(&1, 0))
            ys = Enum.map(corners, &elem(&1, 1))

            [clip_left, clip_bottom, clip_right, clip_top] =
              Map.get(placement, :clip) || [0, 0, width, height]

            left = max(Enum.min(xs), max(clip_left, 0))
            right = min(Enum.max(xs), min(clip_right, width))
            bottom = max(Enum.min(ys), max(clip_bottom, 0))
            top = min(Enum.max(ys), min(clip_top, height))

            if right > left and top > bottom do
              [
                %{
                  type: :link_region,
                  x: left,
                  y: bottom,
                  width: right - left,
                  height: top - bottom
                }
                |> Map.merge(Map.take(box, [:link_url, :link_destination]))
              ]
            else
              []
            end
          end)

        {:cont, {:ok, Enum.reverse(added, links)}}
      else
        error -> {:halt, error}
      end
    end)
  end
end
