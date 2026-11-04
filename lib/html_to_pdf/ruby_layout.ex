defmodule NativeElixirPdfUtilities.HtmlToPdf.RubyLayout do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, Layout, ResourceContext}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.RubyValidator

  @doc "Measures ruby groups with the ordinary inline font, atomic box and baseline policies."
  @spec measure(term(), term()) :: {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def measure(node, context) do
    case context do
      nil ->
        ResourceContext.run([], HtmlValidator.new_image_budget(), &measure(node, &1))

      _ ->
        with {:ok, groups} <- RubyValidator.prepare(node, context),
             do: measure_groups(node, groups, context)
    end
  end

  @doc "Splits an already measured ruby group to the widest complete proportional fragment that fits."
  @spec split(map(), number()) ::
          {:ok, nil | {map(), map()}} | {:error, {atom(), Diagnostics.diagnostic()}}
  def split(measured, width) do
    if not RubyValidator.splittable_group?(measured.source_group, measured.base_width) do
      {:ok, nil}
    else
      annotation_counts =
        measured.source_group.annotations
        |> Enum.zip(measured.annotation_widths)
        |> Enum.reduce_while({:ok, []}, fn {annotation, intrinsic}, {:ok, counts} ->
          target = width * intrinsic / measured.width

          case fitting_prefix(
                 %{base: annotation.children, annotations: []},
                 annotation.style,
                 target
               ) do
            {:ok, count} -> {:cont, {:ok, counts ++ [count]}}
            error -> {:halt, error}
          end
        end)

      with {:ok, reading_counts} <- annotation_counts,
           {:ok, count} <-
             fitting_prefix(
               measured.source_group,
               measured.source_node.style,
               width * measured.base_width / measured.width
             ) do
        if count == 0 or count >= RubyValidator.group_length(measured.source_group) do
          {:ok, nil}
        else
          {before, after_group} =
            RubyValidator.partition_group(measured.source_group, count, reading_counts)

          first = %{measured.source_node | children: before.base ++ before.annotations}
          second = %{measured.source_node | children: after_group.base ++ after_group.annotations}

          with {:ok, [prefix]} <- measure(first, ResourceContext.current()),
               {:ok, [suffix]} <- measure(second, ResourceContext.current()),
               do: {:ok, {prefix, suffix}}
        end
      end
    end
  end

  defp fitting_prefix(group, style, width) do
    total = RubyValidator.group_length(group)

    if total == 0 do
      {:ok, 0}
    else
      candidates = Map.get(group, :break_counts, Enum.to_list(1..total))

      Enum.reduce_while(candidates, {:ok, 0}, fn count, {:ok, selected} ->
        {before, _after} = RubyValidator.partition_group(group, count, [])

        node = %{
          type: :element,
          tag: "span",
          style: style |> Map.put(:display, :inline) |> Map.delete(:_ruby_annotation),
          children: before.base
        }

        result =
          case ResourceContext.current() do
            nil ->
              ResourceContext.run(
                [],
                HtmlValidator.new_image_budget(),
                &Layout.measure_child(node, &1)
              )

            context ->
              Layout.measure_child(node, context)
          end

        case result do
          {:ok, measured} ->
            if NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator.fits_width?(
                 Map.get(measured, :typographic_width, measured.width),
                 width
               ), do: {:cont, {:ok, count}}, else: {:halt, {:ok, selected}}

          error ->
            {:halt, error}
        end
      end)
    end
  end

  defp measure_groups(node, groups, context) do
    Enum.reduce_while(groups, {:ok, []}, fn group, {:ok, measured} ->
      base = %{
        type: :element,
        tag: "span",
        style: node.style |> Map.put(:display, :inline) |> Map.put(:position, :static),
        children: group.base
      }

      with {:ok, base} <- Layout.measure_child(base, context),
           {:ok, base_em} <- RubyValidator.em_metrics(node.style),
           {:ok, annotations} <- measure_annotations(group.annotations, context) do
        # The final font advance can be fractional while earlier styled runs
        # occupy fixed CSS units. Center the genuine unpositioned typographic
        # extent supplied by the shared measured-child flow authority.
        base = %{base | width: Map.get(base, :typographic_width, base.width)}

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
            {boxes, base_em.ascent},
            fn annotation, {boxes, top} ->
              baseline = top + annotation.em_metrics.descent

              annotation_boxes =
                DrawingGeometry.translate_boxes(
                  annotation.boxes,
                  (width - annotation.width) / 2,
                  baseline
                )

              {boxes ++ annotation_boxes, baseline + annotation.em_metrics.ascent}
            end
          )

        result = %{
          width: width,
          base_width: base.width,
          annotation_widths: Enum.map(annotations, & &1.width),
          ascent: max(ascent, base.ascent),
          annotation_ascent: max(ascent, base.ascent),
          base_ascent: base.ascent,
          descent: base.descent,
          boxes: boxes,
          break_before: group.break_before,
          overhang: min(max(width - base.width, 0.0) / 2, node.style.font_size / 4),
          source_group: group,
          source_node: %{node | children: group.base ++ group.annotations}
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

  defp measure_annotations(annotations, context) do
    Enum.reduce_while(annotations, {:ok, []}, fn annotation, {:ok, measured} ->
      annotation = %{annotation | style: Map.put(annotation.style, :display, :inline)}

      case Layout.measure_child(annotation, context) do
        {:ok, result} ->
          case RubyValidator.em_metrics(annotation.style) do
            {:ok, em_metrics} ->
              result = %{result | width: Map.get(result, :typographic_width, result.width)}
              {:cont, {:ok, measured ++ [Map.put(result, :em_metrics, em_metrics)]}}

            {:error, _} = error ->
              {:halt, error}
          end

        {:error, _} = error ->
          {:halt, error}
      end
    end)
  end
end
