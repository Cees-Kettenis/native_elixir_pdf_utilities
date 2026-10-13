defmodule NativeElixirPdfUtilities.HtmlToPdf.MathLayout do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.FontValidator
  alias NativeElixirPdfUtilities.Validators.MathLayoutValidator
  alias NativeElixirPdfUtilities.Validators.MathTableValidator

  @doc """
  Measures styled MathML tokens, operators, rows, fractions, radicals and scripts after preparation.

  Coordinates are points relative to the alphabetic baseline, with positive y
  upwards. Layout and ink extents are separate. Drawing items retain the selected
  face, glyph ID and original Unicode for the writer's ActualText contract.
  """
  @spec measure(term(), map()) :: {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def measure(node, context \\ %{}) do
    with_context(fn ->
      with {:ok, prepared} <- MathLayoutValidator.prepare_element(node, context) do
        {:ok, measure_prepared(prepared, prepared.stretch_constraint)}
      end
    end)
  end

  @doc "Remeasures a retained styled block Math atom using its actual allocated content width."
  @spec resolve_block_flow(term(), term()) ::
          {:ok, [map()]} | {:error, Diagnostics.detailed_error()}
  def resolve_block_flow(children, width) do
    with {:ok, request} <- MathLayoutValidator.prepare_block_flow(children, width),
         {:ok, measured} <- measure(request.source, request.context) do
      {:ok, [%{request.atom | style: Map.put(request.atom.style, :_math_box, measured)}]}
    end
  end

  defp measure_prepared(prepared, constraint \\ nil) do
    measured =
      case prepared do
        %{kind: :glyph, glyph: glyph, style: style} ->
          measured = glyph_geometry(glyph)
          %{measured | glyphs: Enum.map(measured.glyphs, &Map.put(&1, :color, style.color))}

        %{kind: :space, width: width, ascent: ascent, descent: descent} ->
          %{
            width: width,
            ascent: ascent,
            descent: descent,
            italic_correction: 0.0,
            top_accent_attachment: nil,
            glyphs: []
          }

        %{kind: :fraction, children: [numerator, denominator]} ->
          numerator = measure_prepared(numerator, constraint)
          denominator = measure_prepared(denominator)
          thickness = prepared.rule_thickness

          {up, down} =
            if thickness > 0 do
              {
                max(
                  prepared.numerator_shift,
                  prepared.axis + thickness / 2 + prepared.numerator_gap + numerator.ink_descent
                ),
                max(
                  prepared.denominator_shift,
                  thickness / 2 + prepared.denominator_gap + denominator.ink_ascent -
                    prepared.axis
                )
              }
            else
              gap =
                prepared.stack_bottom - denominator.ink_ascent +
                  prepared.stack_top - numerator.ink_descent

              deficit = max(prepared.stack_gap - gap, 0.0)
              {prepared.stack_top + deficit / 2, prepared.stack_bottom + deficit / 2}
            end

          width = Map.get(prepared.style, :width) || max(numerator.width, denominator.width)

          offsets = [
            {numerator, (width - numerator.width) / 2, up},
            {denominator, (width - denominator.width) / 2, -down}
          ]

          rule =
            if thickness > 0 and width > 0 do
              [
                %{
                  kind: :native,
                  x: 0.0,
                  y: 0.0,
                  box: %{
                    type: :rect,
                    x: 0.0,
                    y: prepared.axis - thickness / 2,
                    width: width,
                    height: thickness,
                    fill_color: prepared.style.color,
                    stroke_color: nil,
                    stroke_width: 0.0,
                    border_radius: 0.0,
                    snap_to_css_pixel_grid: false
                  }
                }
              ]
            else
              []
            end

          positioned = fn field ->
            Enum.flat_map(offsets, fn {child, x, y} ->
              Enum.map(Map.fetch!(child, field), &%{&1 | x: &1.x + x, y: &1.y + y})
            end)
          end

          ascent = max(up + numerator.ascent, -down + denominator.ascent)
          ink_ascent = max(up + numerator.ink_ascent, -down + denominator.ink_ascent)
          descent = max(max(-up + numerator.descent, down + denominator.descent), 0.0)
          ink_descent = max(-up + numerator.ink_descent, down + denominator.ink_descent)

          {ascent, descent, ink_ascent, ink_descent} =
            if thickness > 0 do
              top = prepared.axis + thickness / 2
              bottom = thickness / 2 - prepared.axis

              {max(ascent, top), max(descent, bottom), max(ink_ascent, top),
               max(ink_descent, bottom)}
            else
              {ascent, descent, ink_ascent, ink_descent}
            end

          %{
            width: width,
            min_content_width: max(numerator.min_content_width, denominator.min_content_width),
            max_content_width: max(numerator.max_content_width, denominator.max_content_width),
            ascent: ascent,
            descent: descent,
            ink_ascent: ink_ascent,
            ink_descent: ink_descent,
            italic_correction: 0.0,
            top_accent_attachment: nil,
            glyphs: positioned.(:glyphs),
            drawing_items: rule ++ positioned.(:drawing_items),
            actual_text: numerator.actual_text <> denominator.actual_text
          }

        %{kind: :matrix} ->
          rows =
            Enum.map(prepared.rows, fn row -> Enum.map(row, &measure_prepared(&1.content)) end)

          placement =
            case MathTableValidator.prepare_geometry(prepared, rows) do
              {:ok, geometry} -> geometry
              {:error, _} = error -> throw({:math_layout_error, error})
            end

          placement =
            if Enum.any?(prepared.rows, fn row ->
                 Enum.any?(row, &Map.has_key?(&1, :content_layout_source))
               end) do
              widths = List.to_tuple(Enum.map(placement.cells, & &1.content_width))
              intrinsic = List.to_tuple(Enum.map(placement.cells, & &1.measured))

              {rows, _} =
                Enum.map_reduce(prepared.rows, 0, fn row, index ->
                  Enum.map_reduce(row, index, fn cell, index ->
                    measured =
                      if Map.has_key?(cell, :content_layout_source) do
                        with {:ok, request} <-
                               MathTableValidator.prepare_cell_reflow(cell, elem(widths, index)) do
                          measured =
                            measure_prepared(
                              request.prepared,
                              request.prepared.stretch_constraint
                            )

                          # Allocation gives the anonymous cell row its used
                          # width before percentage children reflow. A narrower
                          # painted child does not recenter that containing row.
                          used_width = min(elem(intrinsic, index).width, elem(widths, index))
                          %{measured | width: max(measured.width, used_width)}
                        else
                          error -> throw({:math_layout_error, error})
                        end
                      else
                        elem(intrinsic, index)
                      end

                    {measured, index + 1}
                  end)
                end)

              case MathTableValidator.prepare_geometry(prepared, rows) do
                {:ok, geometry} -> geometry
                error -> throw({:math_layout_error, error})
              end
            else
              placement
            end

          cells =
            Enum.map(placement.cells, fn cell ->
              inner =
                cell.measured
                |> Map.merge(%{
                  width: cell.content_width,
                  ascent: cell.content_ascent,
                  descent: cell.content_descent
                })
                |> Map.put(
                  :glyphs,
                  Enum.map(cell.measured.glyphs, &%{&1 | x: &1.x + cell.inner_x})
                )
                |> Map.put(
                  :drawing_items,
                  Enum.map(cell.measured.drawing_items, &%{&1 | x: &1.x + cell.inner_x})
                )

              %{cell | measured: decorate_geometry(inner, allocated_matrix_style(cell.style))}
            end)

          row_backgrounds =
            Enum.flat_map(placement.row_backgrounds, fn row ->
              empty = %{
                width: row.width,
                min_content_width: row.width,
                max_content_width: row.width,
                ascent: row.ascent,
                descent: row.descent,
                ink_ascent: row.ascent,
                ink_descent: row.descent,
                glyphs: [],
                drawing_items: []
              }

              decorated = decorate_geometry(empty, allocated_matrix_style(row.style))
              Enum.map(decorated.drawing_items, &%{&1 | x: &1.x + row.x, y: &1.y + row.y})
            end)

          positioned = fn field ->
            Enum.flat_map(cells, fn %{measured: child, x: x, y: y} ->
              Enum.map(Map.fetch!(child, field), &%{&1 | x: &1.x + x, y: &1.y + y})
            end)
          end

          placement
          |> Map.take([
            :width,
            :min_content_width,
            :max_content_width,
            :ascent,
            :descent,
            :ink_ascent,
            :ink_descent
          ])
          |> Map.merge(%{
            italic_correction: 0.0,
            top_accent_attachment: nil,
            glyphs: positioned.(:glyphs),
            drawing_items: row_backgrounds ++ positioned.(:drawing_items),
            actual_text: Enum.map_join(cells, & &1.measured.actual_text)
          })

        %{kind: :padded} ->
          inner = measure_prepared(prepared.inner, constraint)

          placed =
            case MathLayoutValidator.prepare_padded_placement(prepared, inner) do
              {:ok, placed} -> placed
              {:error, _} = error -> throw({:math_layout_error, error})
            end

          inner
          |> Map.merge(
            Map.take(placed, [:width, :min_content_width, :max_content_width, :ascent, :descent])
          )
          |> Map.put(:ink_ascent, inner.ink_ascent + placed.y)
          |> Map.put(:ink_descent, inner.ink_descent - placed.y)
          |> Map.put(
            :glyphs,
            Enum.map(inner.glyphs, &%{&1 | x: &1.x + placed.x, y: &1.y + placed.y})
          )
          |> Map.put(
            :drawing_items,
            Enum.map(inner.drawing_items, &%{&1 | x: &1.x + placed.x, y: &1.y + placed.y})
          )

        %{kind: :radical} ->
          base = measure_prepared(prepared.base)
          index = if prepared.index, do: measure_prepared(prepared.index)

          resolved =
            case MathLayoutValidator.prepare_radical_stretch(prepared, base) do
              {:ok, resolved} -> resolved
              {:error, _} = error -> throw({:math_layout_error, error})
            end

          surd = stretch_geometry(resolved.shape)
          thickness = prepared.rule_thickness

          ascent =
            max(base.ascent, base.ink_ascent + prepared.gap + thickness + prepared.extra_ascender)

          descent =
            max(base.descent, surd.ascent + surd.descent + prepared.extra_ascender - ascent)

          surd_y = ascent - prepared.extra_ascender - surd.ascent

          {shift, offsets, width, minimum, maximum, final_ascent, final_descent} =
            if index do
              shift = prepared.kern_before + index.width + max(-index.width, prepared.kern_after)
              raise = prepared.degree_raise * (ascent + descent)
              index_y = -descent + raise + index.descent

              {shift, [{base, shift + surd.width, 0.0}, {index, prepared.kern_before, index_y}],
               shift + surd.width + base.width,
               prepared.kern_before + index.min_content_width +
                 max(-index.min_content_width, prepared.kern_after) + resolved.intrinsic_width +
                 base.min_content_width,
               prepared.kern_before + index.max_content_width +
                 max(-index.max_content_width, prepared.kern_after) + resolved.intrinsic_width +
                 base.max_content_width, max(ascent, -descent + raise + index.ascent),
               max(descent, descent - raise + index.ascent)}
            else
              {0.0, [{base, surd.width, 0.0}], surd.width + base.width,
               resolved.intrinsic_width + base.min_content_width,
               resolved.intrinsic_width + base.max_content_width, ascent, descent}
            end

          surd_glyphs =
            Enum.map(surd.glyphs, fn glyph ->
              glyph
              |> Map.put(:x, glyph.x + shift)
              |> Map.put(:y, glyph.y + surd_y)
              |> Map.put(:color, prepared.style.color)
            end)

          # The generated radical has no authored character. Suppress only its
          # extraction while retaining the original base/index groups in DOM order.
          group = make_ref()

          surd_items =
            Enum.map(
              surd_glyphs,
              &Map.merge(&1, %{kind: :glyph, logical_group: group, logical_text: ""})
            )

          rule =
            if thickness > 0 and base.width > 0 do
              [
                %{
                  kind: :native,
                  x: 0.0,
                  y: 0.0,
                  box: %{
                    type: :rect,
                    x: shift + surd.width,
                    y: ascent - prepared.extra_ascender - thickness,
                    width: base.width,
                    height: thickness,
                    fill_color: prepared.style.color,
                    stroke_color: nil,
                    stroke_width: 0.0,
                    border_radius: 0.0,
                    snap_to_css_pixel_grid: false
                  }
                }
              ]
            else
              []
            end

          positioned = fn field ->
            Enum.flat_map(offsets, fn {child, x, y} ->
              Enum.map(Map.fetch!(child, field), &%{&1 | x: &1.x + x, y: &1.y + y})
            end)
          end

          %{
            width: width,
            min_content_width: minimum,
            max_content_width: maximum,
            ascent: final_ascent,
            descent: final_descent,
            ink_ascent:
              Enum.max([
                ascent - prepared.extra_ascender
                | Enum.map(offsets, fn {child, _, y} -> child.ink_ascent + y end)
              ]),
            ink_descent:
              Enum.max([
                surd.descent - surd_y
                | Enum.map(offsets, fn {child, _, y} -> child.ink_descent - y end)
              ]),
            italic_correction: 0.0,
            top_accent_attachment: nil,
            glyphs: surd_glyphs ++ positioned.(:glyphs),
            drawing_items: rule ++ surd_items ++ positioned.(:drawing_items),
            actual_text: base.actual_text <> if(index, do: index.actual_text, else: "")
          }

        %{kind: kind, children: children} when kind in [:scripts, :limits, :multiscripts] ->
          measured =
            if kind in [:scripts, :multiscripts] do
              Enum.with_index(children)
              |> Enum.map(fn {child, index} ->
                measure_prepared(child, if(index == 0, do: constraint))
              end)
            else
              pending =
                if constraint != nil,
                  do: List.delete(prepared.inline_stretch_children, 0),
                  else: prepared.inline_stretch_children

              measured =
                children
                |> Enum.with_index()
                |> Enum.reject(fn {_, index} -> index in pending end)
                |> Map.new(fn {child, index} ->
                  {index, measure_prepared(child, if(index == 0, do: constraint))}
                end)

              measured =
                if map_size(measured) == 0 do
                  Map.new(pending, &{&1, measure_prepared(Enum.at(children, &1), 0.0)})
                else
                  measured
                end

              target =
                case MathLayoutValidator.prepare_limit_stretch(prepared, measured) do
                  {:ok, target} -> target
                  {:error, _} = error -> throw({:math_layout_error, error})
                end

              measured =
                Enum.reduce(pending, measured, fn index, measured ->
                  Map.put(measured, index, measure_prepared(Enum.at(children, index), target))
                end)

              Enum.map(Enum.with_index(children), fn {_, index} -> Map.fetch!(measured, index) end)
            end

          placed =
            case if kind == :multiscripts,
                   do: MathLayoutValidator.prepare_multiscript_placement(prepared, measured),
                   else: MathLayoutValidator.prepare_script_placement(prepared, measured) do
              {:ok, placement} -> placement
              {:error, _} = error -> throw({:math_layout_error, error})
            end

          positioned = fn field ->
            Enum.zip(measured, placed.offsets)
            |> Enum.flat_map(fn {child, {x, y}} ->
              Enum.map(Map.fetch!(child, field), &%{&1 | x: &1.x + x, y: &1.y + y})
            end)
          end

          placed
          |> Map.delete(:offsets)
          |> Map.merge(%{
            italic_correction: 0.0,
            top_accent_attachment: nil,
            glyphs: positioned.(:glyphs),
            drawing_items: positioned.(:drawing_items),
            actual_text: Enum.map_join(measured, & &1.actual_text)
          })

        %{kind: :operator} ->
          natural = measure_prepared(Map.put(prepared.content, :root, true))
          result = MathLayoutValidator.prepare_operator_stretch(prepared, constraint)

          case result do
            {:ok, nil} ->
              natural

            {:ok, %{shape: shape} = resolved} ->
              shaped = stretch_geometry(shape)

              offset =
                if is_number(resolved.target_ascent),
                  do:
                    (resolved.target_ascent - resolved.target_descent -
                       (shaped.ascent - shaped.descent)) / 2,
                  else: 0.0

              glyphs =
                Enum.map(
                  shaped.glyphs,
                  &(&1
                    |> Map.put(:y, &1.y + offset)
                    |> Map.put(:color, prepared.style.color))
                )

              shaped
              |> Map.put(:ascent, shaped.ascent + offset)
              |> Map.put(:descent, shaped.descent - offset)
              |> Map.put(:glyphs, glyphs)
              |> Map.put(:drawing_items, Enum.map(glyphs, &Map.put(&1, :kind, :glyph)))
              |> Map.put(:min_content_width, resolved.intrinsic_width)
              |> Map.put(:max_content_width, resolved.intrinsic_width)

            {:error, _} = error ->
              throw({:math_layout_error, error})
          end

        %{kind: :native_flow, children: children} ->
          # Preparation retains at least one visible native child or flow wrapper.
          [first | _] = children
          ascent = first.ascent

          {items, ink, total} =
            Enum.reduce(children, {[], [], 0.0}, fn child, {items, ink, height} ->
              offset = ascent - height - child.ascent

              positioned =
                Enum.map(child.boxes, fn box ->
                  %{kind: :native, box: box, x: 0.0, y: offset + child.baseline_offset}
                end)

              {items ++ positioned,
               [{child.ink_ascent + offset, offset - child.ink_descent} | ink],
               height + child.ascent + child.descent}
            end)

          %{
            width: Enum.max(Enum.map(children, & &1.width)),
            min_content_width: Enum.max(Enum.map(children, & &1.min_content_width)),
            max_content_width: Enum.max(Enum.map(children, & &1.max_content_width)),
            ascent: ascent,
            descent: total - ascent,
            ink_ascent: Enum.max(Enum.map(ink, &elem(&1, 0))),
            ink_descent: Enum.max(Enum.map(ink, &(-elem(&1, 1)))),
            italic_correction: 0.0,
            top_accent_attachment: nil,
            glyphs: [],
            drawing_items: items
          }

        %{kind: :flow, children: children, baseline_depth: baseline, line_height: height} ->
          children = Enum.map(children, &measure_prepared/1)

          glyphs =
            children
            |> Enum.with_index()
            |> Enum.flat_map(fn {line, index} ->
              Enum.map(line.glyphs, &%{&1 | y: &1.y - index * height})
            end)

          items =
            children
            |> Enum.with_index()
            |> Enum.flat_map(fn {line, index} ->
              Enum.map(line.drawing_items, &%{&1 | y: &1.y - index * height})
            end)

          %{
            width: Enum.max(Enum.map(children, & &1.width), fn -> 0.0 end),
            ascent: if(children == [], do: 0.0, else: baseline),
            descent: if(children == [], do: 0.0, else: height * length(children) - baseline),
            ink_ascent: Enum.max(Enum.map(children, & &1.ink_ascent), fn -> 0.0 end),
            ink_descent:
              children
              |> Enum.with_index()
              |> Enum.map(fn {line, index} -> line.ink_descent + index * height end)
              |> Enum.max(fn -> 0.0 end),
            italic_correction: 0.0,
            top_accent_attachment: nil,
            glyphs: glyphs,
            drawing_items: items
          }

        %{children: children, kind: kind} ->
          children =
            if kind == :row do
              measure_row_children(prepared, children, constraint)
            else
              Enum.map(children, &measure_prepared/1)
            end

          # A token is a text run. Italic row spacing applies between boxes, not
          # between letters of a multi-character token.
          children =
            if kind == :token and length(children) != 1,
              do: Enum.map(children, &%{&1 | italic_correction: 0.0}),
              else: children

          children =
            if kind == :token do
              Enum.zip(children, prepared.children)
              |> Enum.map(fn {child, prepared} ->
                adjustment = Map.get(prepared, :kerning_before, 0.0)

                %{
                  child
                  | width: child.width + adjustment,
                    glyphs: Enum.map(child.glyphs, &%{&1 | x: &1.x + adjustment})
                }
                |> Map.put(
                  :drawing_items,
                  Enum.map(child.drawing_items, &%{&1 | x: &1.x + adjustment})
                )
              end)
            else
              children
            end

          case {kind, children} do
            {:token, [child]} ->
              child

            _ ->
              {:ok, measured} = row(children)

              {items, _, _} =
                Enum.reduce(children, {[], 0.0, 0.0}, fn child, {items, offset, previous} ->
                  slanted = Map.get(child, :core_operator) == nil and child.italic_correction != 0
                  offset = if slanted, do: offset, else: offset + previous
                  positioned = Enum.map(child.drawing_items, &%{&1 | x: &1.x + offset})

                  {items ++ positioned, offset + child.width,
                   if(slanted, do: child.italic_correction, else: 0.0)}
                end)

              measured
              |> Map.put(
                :italic_correction,
                if(kind == :token, do: 0.0, else: measured.italic_correction)
              )
              |> Map.put(
                :min_content_width,
                measured.width + Enum.sum(Enum.map(children, &(&1.min_content_width - &1.width)))
              )
              |> Map.put(
                :max_content_width,
                measured.width + Enum.sum(Enum.map(children, &(&1.max_content_width - &1.width)))
              )
              |> Map.put(:actual_text, Enum.map_join(children, & &1.actual_text))
              |> Map.put(:drawing_items, items)
              |> Map.put(
                :ink_ascent,
                Enum.max(Enum.map(children, & &1.ink_ascent), fn -> 0.0 end)
              )
              |> Map.put(
                :ink_descent,
                Enum.max(Enum.map(children, & &1.ink_descent), fn -> 0.0 end)
              )
          end
      end

    measured =
      measured
      |> Map.put(:ink_ascent, Map.get(measured, :ink_ascent, measured.ascent))
      |> Map.put(:ink_descent, Map.get(measured, :ink_descent, measured.descent))
      |> Map.put_new(:min_content_width, measured.width)
      |> Map.put_new(:max_content_width, measured.width)
      |> Map.put(:core_operator, Map.get(prepared, :core_operator))
      |> Map.put(
        :drawing_items,
        Map.get(measured, :drawing_items, Enum.map(measured.glyphs, &Map.put(&1, :kind, :glyph)))
      )
      |> Map.put(
        :actual_text,
        Map.get(
          prepared,
          :actual_text,
          Map.get(measured, :actual_text, Enum.map_join(measured.glyphs, & &1.text))
        )
      )

    measured =
      if Map.get(prepared, :kind) in [:token, :operator] do
        group = make_ref()
        logical = %{logical_group: group, logical_text: measured.actual_text}

        measured
        |> Map.update!(:drawing_items, fn items ->
          Enum.map(items, fn item ->
            if item.kind == :glyph, do: Map.merge(item, logical), else: item
          end)
        end)
      else
        measured
      end

    measured =
      if match?(%{invisible: true}, Map.get(prepared, :core_operator)) and
           measured.drawing_items == [] and measured.actual_text != "" do
        Map.put(measured, :drawing_items, [
          %{kind: :logical, actual_text: measured.actual_text, x: 0.0, y: 0.0}
        ])
      else
        measured
      end

    measured =
      if Map.get(prepared, :root, false) or Map.get(prepared, :native_owner, false) or
           Map.get(prepared, :outer_box_measured, false),
         do: measured,
         else:
           decorate_geometry(
             measured,
             if(prepared.kind == :matrix,
               do: allocated_matrix_style(prepared.style),
               else: prepared.style
             )
           )

    if Map.get(prepared, :hide_paint, false),
      do: %{measured | glyphs: [], drawing_items: [], actual_text: ""},
      else: measured
  end

  # Core 3.3.1.1 measures an all-stretch row once at zero, then relayouts
  # each operator against the initial selected-font ink maxima.
  # Wrapped operators propagate the parent's constraint to their core only.
  defp measure_row_children(prepared, children, constraint) do
    {stretching, ordinary} =
      children
      |> Enum.with_index()
      |> Enum.split_with(fn {child, _} ->
        case {Map.get(child, :core_operator), constraint} do
          {%{stretchy: true, stretch_axis: :vertical}, target}
          when is_nil(target) or is_tuple(target) ->
            true

          {%{stretchy: true, stretch_axis: :horizontal}, target} when is_number(target) ->
            true

          _ ->
            false
        end
      end)

    measured =
      if ordinary == [] and is_nil(constraint) do
        Map.new(stretching, fn {child, index} ->
          {index, measure_prepared(child, {0.0, 0.0})}
        end)
      else
        Map.new(ordinary, fn {child, index} ->
          {index, measure_prepared(child)}
        end)
      end

    target =
      if constraint != nil do
        constraint
      else
        {Enum.max(Enum.map(Map.values(measured), & &1.ink_ascent), fn -> 0.0 end),
         Enum.max(Enum.map(Map.values(measured), & &1.ink_descent), fn -> 0.0 end)}
      end

    measured =
      Enum.reduce(stretching, measured, fn {child, index}, measured ->
        Map.put(measured, index, measure_prepared(child, target))
      end)

    Enum.map(Enum.with_index(children), fn {child, index} ->
      result = Map.fetch!(measured, index)
      operator = Map.get(child, :core_operator)

      if operator != nil and Map.get(prepared, :add_operator_space, false) do
        result
        |> Map.put(:width, result.width + operator.lspace + operator.rspace)
        |> Map.put(
          :min_content_width,
          result.min_content_width + operator.lspace + operator.rspace
        )
        |> Map.put(
          :max_content_width,
          result.max_content_width + operator.lspace + operator.rspace
        )
        |> Map.put(:glyphs, Enum.map(result.glyphs, &%{&1 | x: &1.x + operator.lspace}))
        |> Map.put(
          :drawing_items,
          Enum.map(result.drawing_items, &%{&1 | x: &1.x + operator.lspace})
        )
      else
        result
      end
    end)
  end

  # Table allocation has already resolved dimensions. Decoration adds edges once.
  defp allocated_matrix_style(style) do
    Map.drop(style, [:width, :height, :min_width, :max_width, :min_height, :max_height])
  end

  defp decorate_geometry(measured, style) do
    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    margin = Map.get(style, :margin, zero)
    padding = Map.get(style, :padding, zero)
    border = Map.get(style, :border_widths, zero)
    left = margin.left + padding.left + border.left
    right = margin.right + padding.right + border.right
    above = margin.top + padding.top + border.top
    below = margin.bottom + padding.bottom + border.bottom

    content_width =
      case Map.get(style, :width) do
        width when is_number(width) -> width
        _ -> measured.width
      end

    content_height =
      case Map.get(style, :height) do
        height when is_number(height) -> height
        _ -> measured.ascent + measured.descent
      end

    background =
      if Map.get(style, :background_color) not in [nil, :transparent] or
           Enum.any?(Map.values(border), &(&1 > 0)) do
        [
          %{
            kind: :background,
            x: margin.left,
            y: -content_height + measured.ascent - padding.bottom - border.bottom,
            width: content_width + padding.left + padding.right + border.left + border.right,
            height: content_height + padding.top + padding.bottom + border.top + border.bottom,
            style: style
          }
        ]
      else
        []
      end

    measured
    |> Map.put(:width, content_width + left + right)
    |> Map.put(:ascent, measured.ascent + above)
    |> Map.put(:descent, content_height - measured.ascent + below)
    |> Map.put(:ink_ascent, measured.ink_ascent + above)
    |> Map.put(:ink_descent, measured.ink_descent + below)
    |> Map.put(:glyphs, Enum.map(measured.glyphs, &%{&1 | x: &1.x + left}))
    |> Map.put(
      :drawing_items,
      background ++ Enum.map(measured.drawing_items, &%{&1 | x: &1.x + left})
    )
    |> Map.put(
      :min_content_width,
      if(is_number(Map.get(style, :width)), do: content_width, else: measured.min_content_width) +
        left + right
    )
    |> Map.put(
      :max_content_width,
      if(is_number(Map.get(style, :width)), do: content_width, else: measured.max_content_width) +
        left + right
    )
  end

  @doc false
  @spec glyph(term(), term(), term(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def glyph(text, font, size, opts \\ []) do
    with_context(fn ->
      with {:ok, glyph} <- MathLayoutValidator.prepare_glyph(text, font, size, opts) do
        {:ok, glyph_geometry(glyph)}
      end
    end)
  end

  @doc false
  @spec stretch(term(), term(), term(), term(), term(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def stretch(text, font, size, axis, target, opts \\ []) do
    with_context(fn ->
      with {:ok, prepared} <-
             MathLayoutValidator.prepare_stretch(text, font, size, axis, target, opts) do
        {:ok, stretch_geometry(prepared)}
      end
    end)
  end

  # Shared by public stretch measurement and prepared operators. The validator
  # has already chosen variants or an assembly and reserved expansion work.
  defp stretch_geometry(prepared) do
    axis = Map.get(prepared, :axis)

    case prepared do
      %{kind: :glyph, glyph: glyph} ->
        glyph_geometry(glyph)

      %{kind: :assembly, parts: parts} ->
        size = prepared.source.size

        {glyphs, _offset} =
          Enum.map_reduce(parts, 0.0, fn part, offset ->
            measured = glyph_geometry(part.metric)
            [glyph] = measured.glyphs

            glyph =
              if axis == :horizontal,
                do: %{glyph | x: offset},
                else: %{glyph | y: offset + measured.descent}

            {glyph,
             offset + part.full_advance * size / prepared.source.metrics.units_per_em -
               prepared.overlap}
          end)

        {width, ascent, descent} =
          if axis == :vertical do
            {Enum.max(
               Enum.map(
                 prepared.metric_parts,
                 &(&1.metric.advance * size / &1.metric.units_per_em)
               )
             ), prepared.stretch_size, 0.0}
          else
            {prepared.stretch_size,
             Enum.max(
               Enum.map(prepared.metric_parts, fn part ->
                 elem(part.metric.bounds, 3) * size / part.metric.units_per_em
               end)
             ),
             Enum.max(
               Enum.map(prepared.metric_parts, fn part ->
                 -elem(part.metric.bounds, 1) * size / part.metric.units_per_em
               end)
             )}
          end

        {value, pixels} = prepared.italic
        italic = value * size / prepared.source.metrics.units_per_em + pixels

        %{
          width: width,
          ascent: ascent,
          descent: descent,
          italic_correction: italic,
          top_accent_attachment: nil,
          glyphs: glyphs
        }
    end
  end

  defp glyph_geometry(glyph) do
    scale = glyph.font_size / glyph.units_per_em
    {_xmin, ymin, _xmax, ymax} = glyph.bounds
    ascent = ymax * scale
    descent = -ymin * scale
    width = glyph.advance * scale

    italic =
      case glyph.italic do
        nil -> 0.0
        {value, pixels} -> value * scale + pixels
      end

    accent =
      case glyph.accent do
        nil -> nil
        {value, pixels} -> value * scale + pixels
      end

    %{
      width: width,
      ascent: ascent,
      descent: descent,
      italic_correction: italic,
      top_accent_attachment: accent,
      glyphs: [
        %{
          x: 0.0,
          y: 0.0,
          text: glyph.text,
          glyph_id: glyph.glyph_id,
          font_face: glyph.font_face,
          font_size: glyph.font_size,
          width: width,
          height: ascent + descent
        }
      ]
    }
  end

  @doc false
  @spec row(term()) :: {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def row(children) do
    with_context(fn ->
      with :ok <- MathLayoutValidator.validate_row(children) do
        {glyphs, offset, previous} =
          Enum.reduce(children, {[], 0.0, 0.0}, fn child, {glyphs, offset, previous} ->
            slanted? = Map.get(child, :core_operator) == nil and child.italic_correction != 0
            offset = if slanted?, do: offset, else: offset + previous
            positioned = Enum.map(child.glyphs, &%{&1 | x: &1.x + offset})

            {Enum.reverse(positioned, glyphs), offset + child.width,
             if(slanted?, do: child.italic_correction, else: 0.0)}
          end)

        {:ok,
         %{
           width: offset + previous,
           ascent: Enum.max(Enum.map(children, & &1.ascent), fn -> 0.0 end),
           descent: Enum.max(Enum.map(children, & &1.descent), fn -> 0.0 end),
           italic_correction: previous,
           glyphs: Enum.reverse(glyphs)
         }}
      end
    end)
  end

  @doc false
  @spec with_context((-> result)) :: result | {:error, {atom(), Diagnostics.diagnostic()}}
        when result: term()
  def with_context(fun) do
    result =
      HtmlValidator.with_render_budget(fn ->
        try do
          Font.with_measurement_cache(fn -> FontValidator.with_budget(fun) end)
        catch
          {:math_layout_error, error} -> error
          {:font_resource_limit, error} -> error
          {:render_resource_limit, error} -> error
        end
      end)

    case result do
      {:error, {reason, diagnostic}} ->
        {:error, {reason, Map.put(diagnostic, :operation, :measure_math)}}

      result ->
        result
    end
  end
end
