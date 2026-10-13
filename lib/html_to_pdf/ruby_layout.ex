defmodule NativeElixirPdfUtilities.HtmlToPdf.RubyLayout do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, Layout, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.RubyValidator

  @doc "Measures ruby groups with the ordinary inline font, atomic box and baseline policies."
  @spec measure(term(), term()) :: {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def measure(node, context) do
    with {:ok, groups} <- RubyValidator.prepare(node) do
      case context do
        nil -> ResourceContext.run([], HtmlValidator.new_image_budget(), &measure(node, &1))
        _ -> measure_groups(node, groups, context)
      end
    end
  end

  defp measure_groups(node, groups, context) do
    with {:ok, _request} <- HtmlValidator.prepare_measured_child(node, context, %{}) do
      Enum.reduce_while(groups, {:ok, []}, fn group, {:ok, measured} ->
        base = %{
          type: :element,
          tag: "span",
          style: Map.put(node.style, :display, :inline),
          children: group.base
        }

        with {:ok, base} <- Layout.measure_child(base, context),
             {:ok, annotations} <- measure_annotations(group.annotations, context) do
          base =
            if group.base == [] do
              ascent = HtmlValidator.text_baseline_depth(node.style)
              %{base | ascent: ascent, descent: node.style.line_height - ascent}
            else
              base
            end

          width = Enum.max([base.width | Enum.map(annotations, & &1.width)])

          boxes =
            DrawingGeometry.translate_boxes(base.boxes, (width - base.width) / 2, 0.0)

          {boxes, ascent} =
            Enum.reduce(
              annotations,
              {boxes,
               HtmlValidator.text_baseline_depth(
                 Map.put(node.style, :line_height, Style.normal_line_height(node.style))
               )},
              fn annotation, {boxes, top} ->
                baseline = top

                annotation_boxes =
                  DrawingGeometry.translate_boxes(
                    annotation.boxes,
                    (width - annotation.width) / 2,
                    baseline
                  )

                {boxes ++ annotation_boxes, baseline + annotation.ascent}
              end
            )

          result = %{
            width: width,
            ascent: max(ascent, base.ascent),
            annotation_ascent: max(ascent, base.ascent),
            base_ascent: base.ascent,
            descent: base.descent,
            boxes: boxes,
            break_before: group.break_before,
            overhang: min(max(width - base.width, 0.0) / 2, node.style.font_size / 4)
          }

          measured =
            if measured != [] and not result.break_before do
              List.update_at(measured, -1, fn previous ->
                %{
                  previous
                  | width: previous.width + result.width,
                    ascent: max(previous.ascent, result.ascent),
                    annotation_ascent: max(previous.annotation_ascent, result.annotation_ascent),
                    base_ascent: max(previous.base_ascent, result.base_ascent),
                    descent: max(previous.descent, result.descent),
                    boxes:
                      previous.boxes ++
                        DrawingGeometry.translate_boxes(result.boxes, previous.width, 0.0)
                }
              end)
            else
              measured ++ [result]
            end

          {:cont, {:ok, measured}}
        else
          {:error, _} = error -> {:halt, error}
        end
      end)
    end
  end

  defp measure_annotations(annotations, context) do
    Enum.reduce_while(annotations, {:ok, []}, fn annotation, {:ok, measured} ->
      annotation = %{annotation | style: Map.put(annotation.style, :display, :inline)}

      case Layout.measure_child(annotation, context) do
        {:ok, result} -> {:cont, {:ok, measured ++ [result]}}
        {:error, _} = error -> {:halt, error}
      end
    end)
  end
end
