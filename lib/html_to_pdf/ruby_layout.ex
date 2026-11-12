defmodule NativeElixirPdfUtilities.HtmlToPdf.RubyLayout do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, Layout, ResourceContext, TextSpacing}
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
  catch
    {:render_resource_limit, error} -> error
    {:font_resource_limit, error} -> error
  end

  @doc "Splits an already measured ruby group to the widest complete proportional fragment that fits."
  @spec split(map(), number()) ::
          {:ok, nil | {map(), map()}} | {:error, {atom(), Diagnostics.diagnostic()}}
  def split(measured, width) do
    if Map.has_key?(measured, :source_columns) do
      split_columns(measured.source_columns, width)
    else
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

            second = %{
              measured.source_node
              | children: after_group.base ++ after_group.annotations
            }

            with {:ok, [prefix]} <- measure(first, ResourceContext.current()),
                 {:ok, [suffix]} <- measure(second, ResourceContext.current()),
                 do: {:ok, {prefix, suffix}}
          end
        end
      end
    end
  end

  # A prohibited boundary joins columns for fitting, but does not discard legal
  # breaks inside either column or transfer one column's reading to the other.
  defp split_columns(columns, width) do
    columns
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, [], 0.0, nil}, fn {column, index},
                                                 {:ok, before, used, candidate} ->
      remaining = width - used

      case split(column, remaining) do
        {:ok, split} ->
          candidate =
            case split do
              {prefix, suffix} when prefix.width <= remaining ->
                {combine_columns(Enum.reverse([prefix | before])),
                 combine_columns([suffix | Enum.drop(columns, index + 1)])}

              _ ->
                candidate
            end

          if column.width <= remaining,
            do: {:cont, {:ok, [column | before], used + column.width, candidate}},
            else: {:halt, {:ok, before, used, candidate}}

        error ->
          {:halt, error}
      end
    end)
    |> case do
      {:ok, _, _, candidate} -> {:ok, candidate}
      error -> error
    end
  end

  defp combine_columns([first | rest]) do
    Enum.reduce(rest, first, &join_columns(&2, &1))
  end

  defp join_columns(previous, result) do
    columns =
      Map.get(previous, :source_columns, [previous]) ++ Map.get(result, :source_columns, [result])

    children = previous.source_node.children ++ result.source_node.children
    base = previous.source_group.base ++ result.source_group.base
    annotations = previous.source_group.annotations ++ result.source_group.annotations

    HtmlValidator.reserve_render_resource(
      :max_entries,
      length(columns) + length(children) + length(base) + length(annotations),
      :layout
    )

    %{
      previous
      | width: previous.width + result.width,
        base_width: previous.base_width + result.base_width,
        annotation_widths: previous.annotation_widths ++ result.annotation_widths,
        ascent: max(previous.ascent, result.ascent),
        annotation_ascent: max(previous.annotation_ascent, result.annotation_ascent),
        base_ascent: max(previous.base_ascent, result.base_ascent),
        descent: max(previous.descent, result.descent),
        boxes:
          previous.boxes ++ DrawingGeometry.translate_boxes(result.boxes, previous.width, 0.0),
        overhang_after: result.overhang_after,
        overhang: max(previous.overhang_before, result.overhang_after),
        source_group: %{
          previous.source_group
          | base: base,
            annotations: annotations,
            break_counts: []
        },
        source_node: %{previous.source_node | children: children}
    }
    |> Map.put(:source_columns, columns)
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

  defp aligned_boxes(measured, source, width, context) do
    align = Map.get(source.style, :ruby_align, :space_around)
    remaining = max(width - measured.width, 0.0)

    runs =
      Enum.map(measured.boxes, fn box ->
        if box.type == :text,
          do: %{text: box.text, style: box},
          else: %{type: :inline_block, text: "", style: source.style}
      end)

    {_plans, opportunities} =
      TextSpacing.justification_plan(runs, :auto, Map.get(source.style, :_text_language, "und"))

    cond do
      align == :start ->
        offset = if Map.get(source.style, :direction, :ltr) == :rtl, do: remaining, else: 0.0
        {DrawingGeometry.translate_boxes(measured.boxes, offset, 0.0), offset, remaining - offset}

      # Sub-LayoutUnit free space has no distributable CSS position. Preserve
      # the same centered glyph run instead of introducing tiny shaped gaps.
      align == :center or remaining <= 0.75 / 64 or opportunities == 0 ->
        {DrawingGeometry.translate_boxes(measured.boxes, remaining / 2, 0.0), remaining / 2,
         remaining / 2}

      true ->
        edge = if align == :space_around, do: remaining / (2 * (opportunities + 1)), else: 0.0
        zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}

        style =
          source.style
          |> Map.merge(%{
            display: :block,
            width: width - 2 * edge,
            text_align: :justify,
            text_align_last: :justify,
            text_justify: :auto,
            margin: zero,
            padding: zero,
            border_widths: zero
          })
          |> Map.delete(:_text_align_initial)

        case Layout.measure_child(%{source | style: style}, context, %{
               available_width: width - 2 * edge
             }) do
          {:ok, aligned} ->
            {DrawingGeometry.translate_boxes(aligned.boxes, edge, 0.0), edge, edge}

          error ->
            throw({:render_resource_limit, error})
        end
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

      base_source = base

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

        body_width =
          Enum.max([
            base.width
            | for(
                annotation <- annotations,
                not Map.get(annotation.source_node.style, :_ruby_inter_character, false),
                do: annotation.width
              )
          ])

        width =
          body_width +
            Enum.reduce(annotations, 0.0, fn annotation, sum ->
              if Map.get(annotation.source_node.style, :_ruby_inter_character, false),
                do: sum + annotation.width,
                else: sum
            end)

        {boxes, left_space, right_space} = aligned_boxes(base, base_source, body_width, context)

        {boxes, ascent, descent, _side_cursor} =
          annotations
          |> Enum.with_index()
          |> Enum.reduce(
            {boxes, base_em.ascent, base_em.descent, body_width},
            fn {annotation, level}, {boxes, top, bottom, side_cursor} ->
              if Map.get(annotation.source_node.style, :_ruby_inter_character, false) do
                start? = Map.get(annotation.source_node.style, :ruby_align) == :start

                baseline =
                  if start?,
                    do: base_em.ascent - annotation.ascent,
                    else:
                      (base_em.ascent - base_em.descent - annotation.ascent + annotation.descent) /
                        2

                placed = DrawingGeometry.translate_boxes(annotation.boxes, side_cursor, baseline)

                {boxes ++ placed, max(top, baseline + annotation.ascent),
                 max(bottom, annotation.descent - baseline), side_cursor + annotation.width}
              else
                policy = Map.get(annotation.source_node.style, :ruby_position, [:over])
                side = List.last(policy)

                side =
                  if :alternate in policy and rem(level, 2) == 1,
                    do: if(side == :over, do: :under, else: :over),
                    else: side

                baseline =
                  if side == :under,
                    do: -bottom - annotation.em_metrics.ascent,
                    else: top + annotation.em_metrics.descent

                annotation_boxes =
                  aligned_boxes(annotation, annotation.source_node, body_width, context)
                  |> elem(0)
                  |> DrawingGeometry.translate_boxes(0.0, baseline)

                if side == :under do
                  {boxes ++ annotation_boxes, top,
                   bottom + annotation.em_metrics.ascent + annotation.em_metrics.descent,
                   side_cursor}
                else
                  {boxes ++ annotation_boxes, baseline + annotation.em_metrics.ascent, bottom,
                   side_cursor}
                end
              end
            end
          )

        no_overhang? =
          Map.get(node.style, :ruby_overhang, :auto) == :none or
            Enum.any?(annotations, &Map.get(&1.source_node.style, :_ruby_inter_character, false))

        widest = Enum.max_by(annotations, & &1.width, fn -> nil end)

        cap =
          if no_overhang? or widest == nil,
            do: 0.0,
            else: widest.source_node.style.font_size / 2

        {before_space, after_space} =
          if Map.get(node.style, :direction, :ltr) == :rtl,
            do: {right_space, left_space},
            else: {left_space, right_space}

        overhang_before = min(before_space, cap)
        overhang_after = min(after_space, cap)

        result = %{
          width: width,
          base_width: base.width,
          annotation_widths: Enum.map(annotations, & &1.width),
          ascent: max(ascent, base.ascent),
          annotation_ascent: max(ascent, base.ascent),
          base_ascent: base.ascent,
          descent: max(base.descent, descent),
          boxes: boxes,
          break_before: group.break_before,
          overhang: max(overhang_before, overhang_after),
          overhang_before: overhang_before,
          overhang_after: overhang_after,
          source_group: group,
          source_node: %{node | children: group.base ++ group.annotations}
        }

        measured =
          if measured != [] and not result.break_before do
            List.update_at(measured, -1, &join_columns(&1, result))
          else
            measured ++ [result]
          end

        {:cont, {:ok, measured}}
      else
        {:error, _} = error -> {:halt, error}
      end
    end)
  end

  defp no_wrap_tree(node) do
    style =
      node.style
      |> Map.put(:text_wrap_mode, :nowrap)
      |> NativeElixirPdfUtilities.Validators.CssWhiteSpaceValidator.compute_style()

    node = Map.put(node, :style, style)

    if Map.has_key?(node, :children),
      do: Map.update!(node, :children, &Enum.map(&1, fn child -> no_wrap_tree(child) end)),
      else: node
  end

  defp measure_annotations(annotations, context) do
    Enum.reduce_while(annotations, {:ok, []}, fn annotation, {:ok, measured} ->
      used_style = Map.put(annotation.style, :display, :inline)

      used_style =
        if Map.get(used_style, :_ruby_inter_character, false),
          do: Map.merge(used_style, %{white_space: :nowrap, text_wrap_mode: :nowrap}),
          else: used_style

      annotation = %{annotation | style: used_style}

      annotation =
        if Map.get(used_style, :_ruby_inter_character, false),
          do: no_wrap_tree(annotation),
          else: annotation

      case Layout.measure_child(annotation, context) do
        {:ok, result} ->
          case RubyValidator.em_metrics(annotation.style) do
            {:ok, em_metrics} ->
              result = %{result | width: Map.get(result, :typographic_width, result.width)}

              {:cont,
               {:ok,
                measured ++
                  [
                    result
                    |> Map.put(:em_metrics, em_metrics)
                    |> Map.put(:source_node, annotation)
                  ]}}

            {:error, _} = error ->
              {:halt, error}
          end

        {:error, _} = error ->
          {:halt, error}
      end
    end)
  end
end
