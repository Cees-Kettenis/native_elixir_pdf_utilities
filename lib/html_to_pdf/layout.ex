defmodule NativeElixirPdfUtilities.HtmlToPdf.Layout do
  @moduledoc """
  Layout engine for the native HTML-to-PDF renderer.

  Lays out block text elements, inline text runs, box styling, lists, link
  annotation bounds, deterministic tables, a documented flexbox/grid subset,
  image boxes, embedded-font text metrics, and pagination metadata.

  Replaced inline images retain styled-node `display: :image` with
  `image_display: :inline` and become explicit `:inline_image` runs. Atomic
  measurements use `_inline_content_width`, `_inline_content_height`, and
  `_inline_baseline_depth` measured down from the border-box top. The bottom
  margin edge supplies an image's baseline. Painted boxes keep PDF bottom-left
  coordinates; optional `link_rect` is the clickable border box, independent of
  fitted image ink. Relative positioning and pagination move both rectangles.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.ResourceContext
  alias NativeElixirPdfUtilities.HtmlToPdf.EmbeddedViewport
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgScene
  alias NativeElixirPdfUtilities.HtmlToPdf.CounterStyle
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @css_pixel_points 0.75
  # Browser layout rounds lengths to 1/64 CSS pixel. This is a fixed precision allowance.
  @line_wrap_tolerance 0.01171875

  @type box :: map()
  @type layout_tree :: %{
          type: :layout,
          page_size: term(),
          margin: term(),
          margins: PageGeometry.margins(),
          boxes: [box()]
        }
  @type render_option :: NativeElixirPdfUtilities.HtmlToPdf.render_option()

  @doc """
  Converts a styled document tree into a layout tree.
  """
  @spec layout(term(), term()) ::
          {:ok, layout_tree()}
          | {:error, :invalid_layout | :invalid_margin | :invalid_page_size}
          | {:error,
             {:invalid_layout | :unsupported_html | :resource_limit_exceeded,
              NativeElixirPdfUtilities.Diagnostics.diagnostic()}}
  def layout(styled_tree, opts \\ []) do
    layout_document(styled_tree, opts, nil)
  end

  @doc "Lays out an existing styled surface at explicit nonnegative point dimensions, including a zero client area."
  @spec layout_viewport(term(), term(), term()) ::
          {:ok, layout_tree()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def layout_viewport(styled_tree, width, height) do
    result =
      HtmlValidator.with_render_budget(fn ->
        with {:ok, viewport} <- HtmlValidator.prepare_viewport_dimensions(width, height) do
          # Ordinary PDF pages require a positive printable area. The positive
          # page descriptor validates the same tree; execution uses the actual
          # viewport below, including zero. No caller option bypasses page policy.
          opts = [page_size: "#{max(width, 0.75)}pt #{max(height, 0.75)}pt", margin: 0]
          layout_document(styled_tree, opts, viewport)
        end
      end)

    case result do
      {:ok, _layout} = success ->
        success

      {:error, {reason, diagnostic}} ->
        {:error,
         {reason,
          Diagnostics.with_context(diagnostic, operation: :layout_viewport, module: __MODULE__)}}

      {:error, reason} ->
        Diagnostics.error(:layout, reason, "viewport requires a valid styled document tree",
          operation: :layout_viewport,
          module: __MODULE__
        )
    end
  end

  defp layout_document(styled_tree, opts, viewport) do
    HtmlValidator.with_render_budget(fn ->
      page_size =
        case Keyword.keyword?(opts) do
          true -> PageGeometry.normalize_page_size(Keyword.get(opts, :page_size, :a4))
          false -> {:error, :invalid_page_size}
        end

      margins =
        case Keyword.keyword?(opts) do
          true -> PageGeometry.normalize_margins(Keyword.get(opts, :margin, 0))
          false -> {:error, :invalid_margin}
        end

      case HtmlValidator.validate_layout_input(styled_tree, opts, page_size, margins) do
        :ok ->
          styled_tree = styled_tree |> collapse_child_margins() |> attach_positioned_descendants()
          {:ok, page_size} = page_size
          {:ok, margins} = margins
          page_size = if viewport == nil, do: page_size, else: viewport

          {layout_page_size, margins} =
            case viewport != nil or Map.get(styled_tree, :_css_pixel_viewport, false) do
              true ->
                {page_width, page_height} = page_size

                content_width =
                  Float.ceil((page_width - margins.left - margins.right) / 0.75) * 0.75

                margins =
                  Map.new(margins, fn {side, value} -> {side, round(value / 0.75) * 0.75} end)

                {{content_width + margins.left + margins.right, page_height}, margins}

              false ->
                {page_size, margins}
            end

          {children, canvas_boxes} =
            Enum.map_reduce(styled_tree.children, [], fn node, canvas_boxes ->
              case node do
                %{tag: "body", style: style} ->
                  canvas_style =
                    Map.take(style, [
                      :background_color,
                      :background_image,
                      :background_position,
                      :background_repeat,
                      :background_size
                    ])

                  canvas_boxes =
                    background_box(canvas_style, 0, 0, elem(page_size, 0), elem(page_size, 1))
                    |> Enum.map(&Map.put(&1, :position_anchor, :canvas))

                  style =
                    style |> Map.put(:background_color, nil) |> Map.put(:background_image, nil)

                  {%{node | style: style}, canvas_boxes}

                _ ->
                  {node, canvas_boxes}
              end
            end)

          positioned_children = Map.get(styled_tree, :positioned_children, [])

          with {:ok, boxes} <- layout_blocks(children, layout_page_size, margins),
               {:ok, boxes} <-
                 layout_positioned_children(
                   positioned_children,
                   boxes,
                   %{
                     x: margins.left,
                     top: elem(page_size, 1) - margins.top,
                     width: elem(layout_page_size, 0) - margins.left - margins.right,
                     height: elem(page_size, 1) - margins.top - margins.bottom
                   },
                   :root
                 ),
               false <- Enum.any?(boxes, &(Map.get(&1, :type) == :layout_error)) do
            {page_width, page_height} = layout_page_size
            link_metadata = Map.take(styled_tree, [:link_scope, :link_expected_targets])

            link_metadata =
              case Map.get(styled_tree, :link_document_targets, []) do
                [] ->
                  link_metadata

                targets ->
                  Map.put(
                    link_metadata,
                    :link_document_targets,
                    Enum.map(
                      targets,
                      &Map.merge(&1, %{x: margins.left, y: page_height - margins.top})
                    )
                  )
              end

            {:ok,
             Map.merge(
               %{
                 type: :layout,
                 page_size: page_size,
                 margin: PageGeometry.compact_margins(margins),
                 margins: margins,
                 boxes: canvas_boxes ++ group_math_glyphs(boxes),
                 content_width: page_width - margins.left - margins.right,
                 content_height: page_height - margins.top - margins.bottom
               },
               link_metadata
             )}
          else
            true -> {:error, :invalid_layout}
            {:error, reason} -> {:error, reason}
          end

        {:error, reason} ->
          {:error, reason}
      end
    end)
  end

  defp group_math_glyphs(boxes) do
    boxes
    |> Enum.chunk_by(&Map.get(&1, :_math_group))
    |> Enum.flat_map(fn boxes ->
      case hd(boxes) do
        %{_math_group: _} = first ->
          bounds = Enum.map(boxes, &PageGeometry.box_vertical_bounds/1)
          top = Enum.max(Enum.map(bounds, &elem(&1, 0)))
          bottom = Enum.min(Enum.map(bounds, &elem(&1, 1)))

          [
            first
            |> Map.drop([
              :text,
              :font_face,
              :glyph_ids,
              :_math_group,
              :link_destination,
              :link_url
            ])
            |> Map.update(:html_targets, [], fn targets ->
              Enum.map(targets, &%{&1 | dy: &1.dy + first.y - bottom})
            end)
            |> Map.merge(%{
              type: :glyph_group,
              actual_text:
                if(Map.get(first, :_math_actual_fragment, false),
                  do: Enum.map_join(boxes, & &1._math_actual_text),
                  else: first._math_actual_text
                ),
              y: bottom,
              height: top - bottom,
              boxes: Enum.map(boxes, &Map.delete(&1, :_math_group))
            })
          ]

        _ ->
          boxes
      end
    end)
  end

  @doc """
  Measures an already styled child in its existing issued document context.

  Returns intrinsic widths, layout/ink extents and ordered native paint boxes in
  points relative to the alphabetic baseline, positive y upwards. Forced breaks,
  links, images, decorations and form metadata retain their native representation.
  No parsing, font registration, resource-context creation, pagination or implicit
  viewport clipping occurs. Atomic elements retain their existing baseline rules.
  Prepared MathML retains its preferred widths independently of actual advance
  and paint geometry.
  `first_alphabetic_baseline` records the first content baseline relative to the
  atomic alphabetic baseline, for MathML Core block-box composition.
  """
  @spec measure_child(term(), term(), term()) ::
          {:ok, map()} | {:error, {atom(), NativeElixirPdfUtilities.Diagnostics.diagnostic()}}
  def measure_child(node, context, constraints \\ %{}) do
    try do
      with {:ok, request} <- HtmlValidator.prepare_measured_child(node, context, constraints) do
        ResourceContext.with_document(context, fn ->
          Font.with_measurement_cache(fn ->
            node =
              case request.node do
                %{type: :text, style: style} = text ->
                  %{
                    type: :element,
                    tag: "span",
                    style: Map.put(style, :display, :inline),
                    children: [text]
                  }

                element ->
                  element
              end

            tree = %{type: :document, children: [node]}

            with {:ok, %{children: [node]}} <-
                   NativeElixirPdfUtilities.HtmlToPdf.FontFallback.resolve(
                     tree,
                     request.unsupported_glyphs
                   ) do
              measure_prepared_child(%{request | node: node})
            end
          end)
        end)
      end
    catch
      {:render_resource_limit, error} -> error
      {:font_resource_limit, error} -> error
    end
  end

  # The same inline run, intrinsic sizing, atomic baseline and paint helpers are
  # used by ordinary document flow. Only the final coordinate origin changes.
  defp measure_prepared_child(request) do
    node = request.node
    style = node.style

    neutral_style =
      style
      |> Map.put(:display, :block)
      |> Map.put(:text_align, :left)
      |> Map.put(:_inline_available_height, request.available_height)

    node =
      if style.display == :image and Map.get(style, :image_display) == :block,
        do: %{node | style: Map.put(style, :image_display, :inline)},
        else: node

    node =
      if request.atomic do
        display =
          case style.display do
            :block -> :inline_block
            :flex -> :inline_flex
            :grid -> :inline_grid
            other -> other
          end

        %{node | style: Map.put(style, :display, display)}
      else
        node
      end

    with {:ok, runs} <- inline_runs([node], neutral_style) do
      maximum =
        runs
        |> inline_lines(nil, false)
        |> Enum.reject(&(&1 == []))
        |> Enum.map(&inline_line_width(&1, request.available_width))
        |> Enum.max(fn -> 0.0 end)

      minimum = inline_minimum_content_width(runs)

      width =
        if request.available_width == nil,
          do: maximum,
          else: min(maximum, request.available_width)

      # The measured MathML box keeps preferred operator widths independently
      # of its selected glyph advances. This adapter reports those intrinsic
      # widths without changing the atomic width, baseline or drawing items.
      {minimum, maximum} =
        case node.style do
          %{_math_box: measured} ->
            {minimum + measured.min_content_width - measured.width,
             maximum + measured.max_content_width - measured.width}

          _ ->
            {minimum, maximum}
        end

      {boxes, baseline, first_baseline, height, layout_width} =
        case runs do
          [%{type: type} = run] when type in [:inline_block, :inline_image] ->
            [[run]] = inline_lines([run], request.available_width)

            run =
              if Map.get(run.style, :display) in [:inline_flex, :inline_grid],
                do: measure_inline_block(run, request.available_width),
                else: run

            margin = Map.get(run.style, :margin, edges(0.0))
            baseline = run._inline_baseline_depth + margin.top

            height =
              run._inline_content_height + vertical_box_size(run.style) + margin.top +
                margin.bottom

            metadata = %{line_baseline_depth: baseline, line_box_height: height}

            first =
              baseline -
                (Map.get(run, :_inline_first_baseline_depth, run._inline_baseline_depth) +
                   margin.top)

            actual_width = inline_run_width(run, request.available_width)

            {inline_run_boxes(run, 0.0, baseline, 0.0, actual_width, metadata), baseline, first,
             height, actual_width}

          _ ->
            lines = if runs == [], do: [], else: inline_lines(runs, width)

            baseline =
              case lines do
                [] -> 0.0
                _ -> inline_line_baseline_depth(List.first(lines), neutral_style)
              end

            {boxes, height} =
              inline_text_boxes(runs, neutral_style, 0.0, baseline, width, %{}, lines)

            actual_width =
              lines |> Enum.map(&inline_line_width(&1, width)) |> Enum.max(fn -> 0.0 end)

            {boxes, baseline, 0.0, height, actual_width}
        end

      boxes = group_math_glyphs(boxes)

      with :ok <- HtmlValidator.validate_measured_child_paint(boxes, node),
           {:ok, bounds} <- measured_child_ink_bounds(boxes) do
        forced_breaks =
          runs
          |> Enum.with_index()
          |> Enum.flat_map(fn {run, index} ->
            if is_binary(Map.get(run, :text)) and String.contains?(run.text, "\n"),
              do: [%{kind: :forced_break, index: index}],
              else: []
          end)

        {:ok,
         %{
           width: layout_width,
           min_content_width: minimum,
           max_content_width: maximum,
           ascent: baseline,
           descent: height - baseline,
           alphabetic_baseline: 0.0,
           first_alphabetic_baseline: first_baseline,
           ink_ascent: Enum.max(Enum.map(bounds, &elem(&1, 0)), fn -> 0.0 end),
           ink_descent: Enum.max(Enum.map(bounds, &(-elem(&1, 1))), fn -> 0.0 end),
           boxes: boxes,
           forced_breaks: forced_breaks
         }}
      end
    else
      {:error, :invalid_layout} ->
        # Plain inline boxes containing block descendants use the ordinary
        # anonymous-flow splitter, inside a measurement-only atomic wrapper.
        wrapper_style =
          neutral_style
          |> Map.put(:display, :inline_block)
          |> Map.drop([:width, :height, :min_width, :max_width, :min_height, :max_height])

        wrapper = %{type: :element, tag: "span", style: wrapper_style, children: [node]}

        with {:ok, child} <- measure_prepared_child(%{request | node: wrapper, atomic: true}) do
          first = child.first_alphabetic_baseline

          {:ok,
           %{
             child
             | ascent: child.ascent - first,
               descent: child.descent + first,
               ink_ascent: child.ink_ascent - first,
               ink_descent: child.ink_descent + first,
               boxes: shift_layout_boxes(child.boxes, 0.0, -first),
               first_alphabetic_baseline: 0.0
           }}
        end
    end
  end

  # Font outlines provide text ink; ordinary page geometry deliberately uses
  # line boxes instead. Native groups keep their existing local transforms/clips.
  defp measured_child_ink_bounds(boxes) do
    Enum.reduce_while(boxes, {:ok, []}, fn box, {:ok, extents} ->
      result =
        case box do
          %{type: :text} ->
            case NativeElixirPdfUtilities.Validators.FontValidator.prepare_text_ink_bounds(box) do
              {:ok, nil} -> {:ok, []}
              {:ok, bounds} -> {:ok, [bounds]}
              error -> error
            end

          %{type: :glyph_group, boxes: children} ->
            measured_child_ink_bounds(children)

          %{type: :drawing_group, boxes: children, transform: {_a, _b, _c, d, _e, f}} ->
            with {:ok, bounds} <- measured_child_ink_bounds(children) do
              bounds = Enum.map(bounds, fn {top, bottom} -> {top * d + f, bottom * d + f} end)

              bounds =
                case NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.local_clip(box) do
                  %{width: width} when width == 0 ->
                    []

                  %{y: y, height: height} ->
                    bounds
                    |> Enum.map(fn {top, bottom} ->
                      {min(top, (y + height) * d + f), max(bottom, y * d + f)}
                    end)
                    |> Enum.filter(fn {top, bottom} -> top > bottom end)

                  _ ->
                    bounds
                end

              {:ok, bounds}
            end

          %{type: :image} ->
            geometry = Map.get(box, :clip, box)
            {:ok, [{geometry.y + geometry.height, geometry.y}]}

          %{type: :rect} ->
            {:ok, [PageGeometry.box_vertical_bounds(box)]}

          _ ->
            {:ok, []}
        end

      case result do
        {:ok, bounds} -> {:cont, {:ok, bounds ++ extents}}
        error -> {:halt, error}
      end
    end)
  end

  defp collapse_child_margins(node, parent_display \\ nil) do
    case node do
      %{children: children} ->
        display = node |> Map.get(:style, %{}) |> Map.get(:display)
        node = %{node | children: Enum.map(children, &collapse_child_margins(&1, display))}

        case node do
          %{style: %{display: display} = style}
          when display in [:block, :list_item] ->
            padding = Map.get(style, :padding, edges(0.0))
            borders = Map.get(style, :border_widths, edges(0.0))

            independent? =
              Map.get(style, :position, :static) == :absolute or
                parent_display in [:flex, :inline_flex, :grid, :inline_grid] or
                Map.get(style, :overflow, :visible) != :visible or
                Map.get(style, :_fieldset, false) or Map.get(node, :tag) == "legend"

            if independent? do
              node
            else
              Enum.reduce([:top, :bottom], node, fn edge, node ->
                height_allows? =
                  (edge == :top and Map.get(style, :break_before, :auto) not in [:page, :always]) or
                    (edge == :bottom and Map.get(style, :height) in [nil, :auto] and
                       Map.get(style, :min_height, 0.0) in [nil, 0, 0.0])

                if Map.fetch!(padding, edge) == 0 and Map.fetch!(borders, edge) == 0 and
                     height_allows? do
                  case margin_edge_block(node.children, edge, []) do
                    {child, path} ->
                      margin = Map.get(node.style, :margin, edges(0.0))
                      child_margin = Map.get(child.style, :margin, edges(0.0))
                      parent_value = Map.fetch!(margin, edge)
                      child_value = Map.fetch!(child_margin, edge)

                      collapsed =
                        max(max(parent_value, child_value), 0.0) +
                          min(min(parent_value, child_value), 0.0)

                      child = %{
                        child
                        | style: Map.put(child.style, :margin, Map.put(child_margin, edge, 0.0))
                      }

                      node =
                        case path do
                          [index] ->
                            %{node | children: List.replace_at(node.children, index, child)}

                          path ->
                            accesses =
                              Enum.flat_map(path, fn index ->
                                [Access.key(:children), Access.at(index)]
                              end)

                            put_in(node, accesses, child)
                        end

                      %{
                        node
                        | style: Map.put(node.style, :margin, Map.put(margin, edge, collapsed))
                      }

                    _ ->
                      node
                  end
                else
                  node
                end
              end)
            end

          _ ->
            node
        end

      _ ->
        node
    end
  end

  defp margin_edge_block(children, edge, path) do
    candidates = if edge == :top, do: children, else: Enum.reverse(children)

    result =
      Enum.reduce_while(candidates, 0, fn child, index ->
        outcome =
          case child do
            %{type: :text, text: text, style: %{white_space: mode}}
            when mode in [:pre, :pre_wrap, :break_spaces] ->
              if text == "", do: nil, else: :line

            %{type: :text, text: text} ->
              if Regex.match?(~r/^[ \t\n\f\r]*$/u, text), do: nil, else: :line

            %{style: %{display: :none}} ->
              nil

            %{style: %{position: :absolute}} ->
              nil

            %{style: %{display: display} = style}
            when display in [:inline, :block, :list, :grid, :flex] ->
              actual_index = if edge == :top, do: index, else: length(children) - 1 - index
              child_path = path ++ [actual_index]

              case display do
                :inline ->
                  if Map.get(style, :position, :static) == :static,
                    do: margin_edge_block(child.children, edge, child_path),
                    else: :line

                _ ->
                  {child, child_path}
              end

            _ ->
              :line
          end

        if outcome == nil, do: {:cont, index + 1}, else: {:halt, outcome}
      end)

    if is_integer(result), do: nil, else: result
  end

  defp attach_positioned_descendants(%{type: :document, children: children} = document) do
    {children, positioned_children} = collect_positioned_children(children)

    document
    |> Map.put(:children, children)
    |> Map.put(:positioned_children, positioned_children)
  end

  defp collect_positioned_children(children) do
    {normal, positioned} =
      Enum.reduce(children, {[], []}, fn child, {normal, positioned} ->
        case child do
          %{type: :element, style: %{display: :none}} ->
            {[child | normal], positioned}

          %{type: :element, style: %{display: :soft_break}} ->
            {[child | normal], positioned}

          %{type: :element, children: nested_children, style: style} = element ->
            {nested_children, nested_positioned} = collect_positioned_children(nested_children)

            element =
              element
              |> Map.put(:children, nested_children)
              |> maybe_attach_positioned_children(nested_positioned)

            case Map.get(style, :position, :static) do
              :absolute -> {normal, [element | positioned]}
              :relative -> {[element | normal], positioned}
              _ -> {[element | normal], Enum.reverse(nested_positioned, positioned)}
            end

          child ->
            {[child | normal], positioned}
        end
      end)

    {Enum.reverse(normal), Enum.reverse(positioned)}
  end

  defp maybe_attach_positioned_children(element, positioned_children) do
    case {Map.get(element.style, :position, :static), positioned_children} do
      {position, positioned_children} when position in [:relative, :absolute] ->
        Map.put(element, :positioned_children, positioned_children)

      _ ->
        element
    end
  end

  @spec layout_blocks([term()], {number(), number()}, PageGeometry.margins()) ::
          {:ok, [box()]} | {:error, :invalid_layout}
  defp layout_blocks(children, page_size, margins) do
    {page_width, page_height} = page_size

    result =
      Enum.reduce_while(children, {:ok, [], page_height - margins.top}, fn child,
                                                                           {:ok, boxes, y} ->
        case layout_block(
               child,
               margins.left,
               y,
               page_width - margins.left - margins.right
             ) do
          {:ok, block_boxes, next_y} -> {:cont, {:ok, Enum.reverse(block_boxes, boxes), next_y}}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)

    case result do
      {:ok, boxes, _y} -> {:ok, Enum.reverse(boxes)}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec layout_block(term(), number(), number(), number()) ::
          {:ok, [box()], number()} | {:error, :invalid_layout}
  defp layout_block(block, x, y, width) do
    case do_layout_block(block, x, y, width) do
      {:ok, boxes, next_y} ->
        style = block.style
        margin = Map.get(style, :margin, edges(0.0))

        boxes =
          if style.display == :none,
            do: boxes,
            else: attach_html_target(style, boxes, x + margin.left, y - margin.top)

        case position_container_contents(block, boxes, next_y, x, y, width) do
          {:ok, positioned_boxes, next_y} ->
            {:ok, attach_outline_anchor(block, positioned_boxes), next_y}

          {:error, reason} ->
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Offsets travel with the existing box through relative movement and pagination.
  # An empty element contributes a non-painting, zero-height flow marker.
  defp attach_html_target(style, boxes, x, top) do
    case Map.get(style, :html_target) do
      nil ->
        boxes

      identity ->
        if Enum.any?(boxes, fn box ->
             Enum.any?(Map.get(box, :html_targets, []), &(&1.identity == identity))
           end) do
          boxes
        else
          {breaks, content} = Enum.split_while(boxes, &(Map.get(&1, :type) == :page_break))

          content =
            case content do
              [] ->
                [
                  %{
                    type: HtmlValidator.reserve_layout_box(:flow_marker),
                    x: x,
                    y: top,
                    width: 0.0,
                    height: 0.0,
                    flow_id: {:html_target, identity}
                  }
                ]

              content ->
                content
            end

          [first | rest] = content
          target = %{identity: identity, dx: x - first.x, dy: top - first.y}
          breaks ++ [Map.update(first, :html_targets, [target], &[target | &1]) | rest]
        end
    end
  end

  defp attach_outline_anchor(block, boxes) do
    heading =
      case block do
        %{outline_anchor: heading} -> heading
        block -> outline_heading(block)
      end

    case heading do
      nil ->
        boxes

      heading ->
        case Enum.split_while(boxes, &(Map.get(&1, :type) == :page_break)) do
          {_page_breaks, []} ->
            boxes

          {page_breaks, [first | rest]} ->
            page_breaks ++ [Map.put(first, :outline_anchor, heading) | rest]
        end
    end
  end

  defp outline_heading(block) do
    case block do
      %{type: :element, tag: <<"h", level>>, children: children}
      when level in ?1..?6 and is_list(children) ->
        title = children |> outline_text() |> String.replace(~r/\s+/u, " ") |> String.trim()

        case title do
          "" -> nil
          title -> %{title: title, level: Map.get(block.style, :heading_level, level - ?0)}
        end

      _ ->
        nil
    end
  end

  defp preserve_item_outline(item, child) do
    case outline_heading(child) do
      nil -> item
      heading -> Map.put(item, :outline_anchor, heading)
    end
  end

  defp outline_text(children) do
    Enum.map_join(children, "", fn child ->
      case child do
        %{style: %{display: :none}} -> ""
        %{type: :text, text: text} -> text
        %{type: :element, tag: "br"} -> " "
        %{children: nested} when is_list(nested) -> outline_text(nested)
        _ -> ""
      end
    end)
  end

  defp do_layout_block(block, x, y, width) do
    case block do
      %{type: :element, style: %{display: :none}} ->
        {:ok, [], y}

      %{type: :element, style: %{_fieldset: true}} ->
        layout_fieldset(block, x, y, width)

      %{type: :element, style: %{svg_scene: _scene} = style} ->
        margin = Map.get(style, :margin, edges(0.0))
        {content_width, content_height} = svg_content_size(style, width)
        box_height = content_height + vertical_box_size(style)
        box_top = y - margin.top
        box_x = x + margin.left
        boxes = svg_drawing_boxes(style, box_x, box_top, content_width, content_height)
        metadata = style |> break_metadata() |> Map.put(:flow_id, {:svg, box_x, box_top})
        boxes = Enum.map(boxes, &Map.merge(&1, metadata))

        marker =
          %{
            type: HtmlValidator.reserve_layout_box(:flow_marker),
            x: box_x,
            y: box_top - box_height,
            width: 0.0,
            height: box_height
          }
          |> Map.merge(metadata)

        {:ok, boxes ++ [marker], box_top - box_height - margin.bottom}

      %{type: :element, tag: "summary", style: %{display: :list_item} = style} ->
        layout_list_item(block, Map.get(style, :list_marker_type, :disc), 1, x, y, width)

      %{type: :element, style: %{display: :block} = style, children: children}
      when is_list(children) ->
        with line_height when is_number(line_height) <- Map.get(style, :line_height),
             font_size when is_number(font_size) <- Map.get(style, :font_size) do
          margin =
            Map.get(style, :margin, edges(0.0, 0.0, Map.get(style, :margin_after, 0.0), 0.0))

          padding = Map.get(style, :padding, edges(0.0))
          border_widths = Map.get(style, :border_widths, edges(0.0))
          box_x = x + margin.left
          box_top = y - margin.top
          available_box_width = width - margin.left - margin.right

          content_width =
            resolved_content_size(
              style,
              :width,
              width,
              available_box_width - horizontal_box_size(style)
            )

          box_width =
            content_width + border_widths.left + padding.left + padding.right +
              border_widths.right

          content_width =
            if Map.get(style, :form_kind) == :listbox,
              do: max(content_width - 11.25, 0.0),
              else: content_width

          box_x =
            case Map.get(style, :margin_inline_auto) do
              %{left: true, right: true} ->
                box_x + max(available_box_width - box_width, 0.0) / 2

              %{left: true, right: false} ->
                box_x + max(available_box_width - box_width, 0.0)

              _ ->
                box_x
            end

          content_x = box_x + border_widths.left + padding.left

          content_top = box_top - border_widths.top - padding.top

          flow_metadata =
            style
            |> break_metadata()
            |> Map.put(
              :fragment_lines,
              Map.get(style, :background_color) == nil and
                Enum.all?(Map.values(border_widths), &(&1 <= 0))
            )
            |> Map.put(:flow_id, {:block, box_x, box_top})

          content_metadata =
            flow_metadata
            |> Map.put(:break_before, :auto)
            |> Map.put(:break_after, :auto)

          case layout_block_content(
                 children,
                 style,
                 content_x,
                 content_top,
                 content_width,
                 content_metadata
               ) do
            {:ok, content_boxes, content_height} ->
              preferred_height =
                case {Map.get(style, :height), Map.get(style, :aspect_ratio)} do
                  {height, ratio} when height in [nil, :auto] and is_number(ratio) ->
                    ratio_height =
                      case Map.get(style, :box_sizing, :content_box) do
                        :border_box ->
                          max(
                            HtmlValidator.aspect_ratio_dimension(
                              box_width,
                              ratio,
                              :height,
                              :layout
                            ) - vertical_box_size(style),
                            0.0
                          )

                        _ ->
                          HtmlValidator.aspect_ratio_dimension(
                            content_width,
                            ratio,
                            :height,
                            :layout
                          )
                      end

                    max(content_height, ratio_height)

                  _ ->
                    content_height
                end

              content_box_height =
                if is_number(Map.get(style, :height)) and
                     Enum.any?(children, fn child ->
                       Map.get(Map.get(child, :style, %{}), :display) == :inline_block
                     end) do
                  inline_content_size(
                    style,
                    :height,
                    Map.get(style, :_containing_height),
                    preferred_height
                  )
                else
                  resolved_content_size(
                    style,
                    :height,
                    Map.get(style, :_containing_height),
                    preferred_height
                  )
                end

              box_height =
                border_widths.top + padding.top + content_box_height + padding.bottom +
                  border_widths.bottom

              paint_height =
                case {Map.get(style, :height), Map.get(style, :background_color),
                      content_height > Map.get(style, :line_height, 0) * 1.5} do
                  {height, color, true} when height in [nil, :auto] and not is_nil(color) ->
                    # Multiline backgrounds cover the final fractional CSS pixel;
                    # the flow height still follows the unsnapped content box.
                    Float.ceil(box_height / @css_pixel_points) * @css_pixel_points

                  _ ->
                    box_height
                end

              content_boxes =
                clip_control_contents(content_boxes, style, %{
                  x: content_x,
                  y: content_top - content_box_height,
                  width: content_width,
                  height: content_box_height
                })

              background_box =
                style
                |> background_box(box_x, box_top - box_height, box_width, paint_height)
                |> tag_boxes(content_metadata)

              background_box =
                case block do
                  %{tag: "hr"} -> Enum.map(background_box, &Map.put(&1, :role, :horizontal_rule))
                  _ -> background_box
                end

              flow_marker =
                case {background_box, padding.top} do
                  {[], top_inset} when top_inset > 0 ->
                    [
                      %{
                        type: HtmlValidator.reserve_layout_box(:rect),
                        x: box_x,
                        y: box_top - top_inset,
                        width: 0.001,
                        height: top_inset,
                        fill_color: {0.0, 0.0, 0.0, 0.0},
                        stroke_color: nil,
                        stroke_width: 0.0,
                        border_widths: edges(0.0),
                        border_colors: edges(nil),
                        border_styles: edges(:none),
                        border_radius: 0.0,
                        paint_layer: :flow_marker
                      }
                      |> Map.merge(content_metadata)
                    ]

                  _ ->
                    []
                end

              next_y = box_top - box_height - margin.bottom

              page_break_box = fn position ->
                %{
                  type: HtmlValidator.reserve_layout_box(:page_break),
                  x: box_x,
                  y: if(position == :before, do: box_top, else: next_y),
                  width: box_width,
                  height: 0.0
                }
                |> Map.merge(%{
                  break_before: if(position == :before, do: :page, else: :auto),
                  break_after: if(position == :after, do: :page, else: :auto),
                  break_inside: :auto,
                  flow_id: {:page_break, position, box_x, box_top}
                })
              end

              extents =
                cond do
                  children != [] or is_number(Map.get(style, :width)) or
                      is_number(Map.get(style, :height)) ->
                    embedded_extent_boxes(
                      box_x,
                      box_top,
                      box_width,
                      box_height,
                      margin,
                      content_metadata
                    )

                  Map.get(block, :tag) == "body" ->
                    # An empty body's leading margin still contributes to the
                    # document scrolling area; its trailing zero-area margins do not.
                    embedded_extent_boxes(
                      box_x,
                      box_top,
                      0.0,
                      0.0,
                      edges(0.0),
                      content_metadata
                    )

                  true ->
                    []
                end

              boxes =
                attach_html_target(
                  style,
                  background_box ++ flow_marker ++ content_boxes ++ extents,
                  box_x,
                  box_top
                )

              boxes =
                case Map.get(flow_metadata, :break_before) do
                  :page -> [page_break_box.(:before) | boxes]
                  _ -> boxes
                end

              boxes =
                case Map.get(flow_metadata, :break_after) do
                  :page -> boxes ++ [page_break_box.(:after)]
                  _ -> boxes
                end

              {:ok, boxes, next_y}

            {:error, reason} ->
              {:error, reason}
          end
        else
          _ -> {:error, :invalid_layout}
        end

      %{type: :element, style: %{display: :image} = style} ->
        layout_image(style, x, y, width)

      %{type: :element, style: %{display: :list} = style, children: children}
      when is_list(children) ->
        layout_list(style, children, x, y, width)

      %{type: :element, style: %{display: :table} = style, children: children}
      when is_list(children) ->
        layout_table(style, children, x, y, width)

      %{type: :element, style: %{display: display} = style, children: children}
      when display in [:flex, :inline_flex] and is_list(children) ->
        layout_flex(style, children, x, y, width)

      %{type: :element, style: %{display: display} = style, children: children}
      when display in [:grid, :inline_grid] and is_list(children) ->
        layout_grid(style, children, x, y, width)

      _ ->
        {:error, :invalid_layout}
    end
  end

  # The rendered legend occupies the top border allocation; the remaining
  # children use an anonymous content box. Geometry and painting stay separate
  # so a transparent legend cuts the border without erasing the background.
  defp layout_fieldset(block, x, y, available_width) do
    style = block.style
    margin = Map.get(style, :margin, edges(0.0))
    padding = Map.get(style, :padding, edges(0.0))
    border = Map.get(style, :border_widths, edges(0.0))

    minimum =
      if Map.has_key?(style, :min_width),
        do: 0.0,
        else: fieldset_content_width(block.children, style, :min)

    content_width =
      max(
        minimum,
        resolved_content_size(
          style,
          :width,
          available_width,
          available_width - margin.left - margin.right - horizontal_box_size(style)
        )
      )

    box_width = content_width + horizontal_box_size(style)
    free_space = max(available_width - margin.left - margin.right - box_width, 0.0)

    box_x =
      x + margin.left +
        case Map.get(style, :margin_inline_auto) do
          %{left: true, right: true} -> free_space / 2
          %{left: true, right: false} -> free_space
          _ -> 0.0
        end

    box_top = y - margin.top
    content_x = box_x + border.left + padding.left

    {preceding, following} =
      Enum.split_while(block.children, fn child ->
        not (Map.get(child, :tag) == "legend" and
               Map.get(Map.get(child, :style, %{}), :display) != :none and
               Map.get(Map.get(child, :style, %{}), :position, :static) != :absolute)
      end)

    {legend, children} =
      case following do
        [legend | rest] -> {legend, preceding ++ rest}
        [] -> {nil, preceding}
      end

    with {:ok, legend_boxes, legend_height, legend_rect} <-
           layout_fieldset_legend(legend, content_x, box_top, content_width, border.top) do
      reserved = max(border.top, legend_height)
      spill = reserved - border.top

      proxy_style =
        style
        |> Map.delete(:_fieldset)
        |> Map.put(:display, :block)
        |> Map.put(:box_sizing, :content_box)
        |> Map.put(:width, content_width)
        |> Map.put(:margin, %{margin | left: box_x - x, right: 0.0})
        |> Map.delete(:margin_inline_auto)
        |> Map.put(:border_widths, %{border | top: reserved})
        |> Map.put(:border_styles, edges(:none))
        |> Map.put(:background_color, nil)
        |> Map.put(:background_image, nil)

      proxy_style =
        Enum.reduce([:height, :min_height, :max_height], proxy_style, fn property, acc ->
          case resolved_content_size(style, property, nil, nil) do
            value when is_number(value) -> Map.put(acc, property, max(value - spill, 0.0))
            _ -> Map.delete(acc, property)
          end
        end)

      proxy = %{block | style: proxy_style, children: children}

      with {:ok, content_boxes, next_y} <- do_layout_block(proxy, x, y, available_width) do
        total_height = box_top - next_y - margin.bottom
        border_top = box_top - spill / 2
        paint_height = max(total_height - spill / 2, 0.0)

        background =
          background_box(style, box_x, border_top - paint_height, box_width, paint_height)

        background =
          Enum.map(background, fn box ->
            box = Map.put(box, :role, :fieldset)

            case {box.type, legend_rect} do
              {:rect, %{x: legend_x, width: legend_width}}
              when legend_width > 0 and border.top > 0 and box.stroke_width > 0 ->
                Map.put(box, :border_cutout, %{
                  x: legend_x - box_x,
                  y: -border.top,
                  width: legend_width,
                  height: border.top
                })

              _ ->
                box
            end
          end)

        paint_id = make_ref()

        background =
          Enum.map(background, fn box ->
            box
            |> Map.put(:fieldset_paint_id, paint_id)
            |> Map.put(:fieldset_bounds, %{
              top: border_top - box.y,
              bottom: border_top - paint_height - box.y
            })
          end)

        metadata = style |> break_metadata() |> Map.put(:flow_id, {:fieldset, box_x, box_top})
        boxes = tag_boxes(background ++ legend_boxes ++ content_boxes, metadata)

        boxes =
          if background == [],
            do: boxes,
            else:
              Enum.map(boxes, fn box ->
                Map.update(box, :fieldset_ancestors, [paint_id], &[paint_id | &1])
              end)

        {:ok, boxes, next_y}
      end
    end
  end

  defp layout_fieldset_legend(legend, x, top, width, border_top) do
    case legend do
      nil ->
        {:ok, [], 0.0, nil}

      %{style: style} ->
        margin = Map.get(style, :margin, edges(0.0))
        intrinsic = fieldset_content_width(legend.children, style, :max)
        minimum = fieldset_content_width(legend.children, style, :min)

        content_width =
          max(
            if(Map.get(style, :width) in [nil, :auto], do: minimum, else: 0.0),
            resolved_content_size(
              style,
              :width,
              width,
              min(
                intrinsic,
                max(width - horizontal_box_size(style) - margin.left - margin.right, 0.0)
              )
            )
          )

        box_width = content_width + horizontal_box_size(style)
        free_space = max(width - box_width - margin.left - margin.right, 0.0)

        offset =
          case Map.get(style, :margin_inline_auto) do
            %{left: true, right: true} ->
              free_space / 2

            %{left: true, right: false} ->
              free_space

            _ ->
              case Map.get(style, :justify_self, :flex_start) do
                :center -> free_space / 2
                :flex_end -> free_space
                _ -> 0.0
              end
          end

        display =
          case style.display do
            value when value in [:flex, :inline_flex] -> :flex
            value when value in [:grid, :inline_grid] -> :grid
            _ -> :block
          end

        used_style =
          style
          |> Map.put(:display, display)
          |> Map.put(:width, content_width)
          |> Map.put(:box_sizing, :content_box)
          |> Map.delete(:margin_inline_auto)

        legend = %{legend | style: used_style}

        with {:ok, boxes, next_y} <-
               layout_block(legend, x + offset, top, box_width + margin.left + margin.right) do
          height = top - next_y
          shift = -max(border_top - height, 0.0) / 2
          boxes = shift_layout_boxes(boxes, 0.0, shift)
          {:ok, boxes, height, %{x: x + offset + margin.left, width: box_width}}
        end
    end
  end

  defp fieldset_content_width(children, style, kind) do
    case inline_runs(children, style) do
      {:ok, runs} ->
        case kind do
          :min ->
            inline_minimum_content_width(runs)

          :max ->
            runs
            |> inline_lines(nil, false)
            |> Enum.map(&inline_line_width(&1, nil))
            |> Enum.max(fn -> 0.0 end)
        end

      {:error, _} ->
        Enum.reduce(children, 0.0, fn child, minimum ->
          value =
            case child do
              %{style: %{display: :none}} ->
                0.0

              %{style: %{display: :image}} ->
                flex_child_intrinsic_width(child)

              %{style: child_style, children: nested} ->
                intrinsic = fieldset_content_width(nested, child_style, kind)

                resolved_content_size(child_style, :width, nil, intrinsic) +
                  horizontal_box_size(child_style)

              _ ->
                flex_child_intrinsic_width(child)
            end

          max(minimum, value)
        end)
    end
  end

  defp position_container_contents(block, boxes, next_y, x, y, available_width) do
    style = Map.fetch!(block, :style)
    positioned_children = Map.get(block, :positioned_children, [])

    case positioned_children do
      [] ->
        {:ok, relatively_shifted_boxes(boxes, style, x, y, available_width, next_y), next_y}

      positioned_children ->
        with {:ok, containing_block} <-
               positioned_containing_block(style, x, y, available_width, next_y),
             {:ok, positioned_boxes} <-
               positioned_child_boxes(positioned_children, containing_block) do
          boxes =
            boxes
            |> mark_container_background(style, x, y, available_width, next_y)
            |> stack_positioned_boxes(positioned_boxes, is_integer(Map.get(style, :z_index)))

          {:ok, relatively_shifted_boxes(boxes, style, x, y, available_width, next_y), next_y}
        end
    end
  end

  defp positioned_containing_block(style, x, y, available_width, next_y) do
    display = Map.get(style, :display)

    case display in [:block, :flex, :inline_flex, :grid, :inline_grid] do
      true ->
        margin = Map.get(style, :margin, edges(0.0))
        border_widths = Map.get(style, :border_widths, edges(0.0))
        box_x = x + margin.left
        box_top = y - margin.top
        box_width = positioned_element_border_width(style, available_width)
        box_height = max(box_top - next_y - margin.bottom, 0.0)

        {:ok,
         %{
           x: box_x + border_widths.left,
           top: box_top - border_widths.top,
           width: max(box_width - border_widths.left - border_widths.right, 0.0),
           height: max(box_height - border_widths.top - border_widths.bottom, 0.0)
         }}

      false ->
        {:error, :invalid_layout}
    end
  end

  defp positioned_element_border_width(style, available_width) do
    margin = Map.get(style, :margin, edges(0.0))
    available_box_width = available_width - margin.left - margin.right

    case Map.get(style, :display) do
      :image ->
        {content_width, _content_height} =
          image_content_size(
            style,
            available_width,
            nil
          )

        content_width + horizontal_box_size(style)

      _ ->
        content_width =
          resolved_content_size(
            style,
            :width,
            available_width,
            available_box_width - horizontal_box_size(style)
          )

        content_width + horizontal_box_size(style)
    end
  end

  defp layout_positioned_children(positioned_children, normal_boxes, containing_block, anchor) do
    case positioned_children do
      [] ->
        {:ok, normal_boxes}

      positioned_children ->
        with {:ok, positioned_boxes} <-
               positioned_child_boxes(positioned_children, containing_block) do
          positioned_boxes =
            Enum.map(positioned_boxes, fn entry ->
              %{entry | boxes: Enum.map(entry.boxes, &Map.put(&1, :position_anchor, anchor))}
            end)

          {:ok, stack_positioned_boxes(normal_boxes, positioned_boxes, true)}
        end
    end
  end

  defp positioned_child_boxes(positioned_children, containing_block) do
    positioned_children
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn {child, index}, {:ok, entries} ->
      case positioned_child_box(child, containing_block, index) do
        {:ok, entry} -> {:cont, {:ok, entries ++ [entry]}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp positioned_child_box(%{type: :element, style: style} = child, containing_block, index) do
    style =
      case Map.get(style, :display) do
        display when display in [:inline, :inline_block] -> Map.put(style, :display, :block)
        :inline_flex -> Map.put(style, :display, :flex)
        :inline_grid -> Map.put(style, :display, :grid)
        _ -> style
      end

    case Map.get(style, :display) in [:block, :image, :flex, :grid] do
      true ->
        offsets = Map.get(style, :offsets, edges(:auto))
        style = stretch_positioned_style(style, offsets, containing_block)
        child = Map.put(child, :style, style)

        case layout_block(child, 0.0, 0.0, containing_block.width) do
          {:ok, boxes, next_y} ->
            margin = Map.get(style, :margin, edges(0.0))
            border_width = positioned_element_border_width(style, containing_block.width)
            border_height = max(0.0 - margin.top - next_y - margin.bottom, 0.0)
            left = resolved_offset(offsets.left, containing_block.width)
            right = resolved_offset(offsets.right, containing_block.width)
            top = resolved_offset(offsets.top, containing_block.height)
            bottom = resolved_offset(offsets.bottom, containing_block.height)

            target_x =
              cond do
                is_number(left) ->
                  containing_block.x + left + margin.left

                is_number(right) ->
                  containing_block.x + containing_block.width - right - margin.right -
                    border_width

                true ->
                  containing_block.x + margin.left
              end

            target_top =
              cond do
                is_number(top) ->
                  containing_block.top - top - margin.top

                is_number(bottom) ->
                  containing_block.top - containing_block.height + bottom + margin.bottom +
                    border_height

                true ->
                  containing_block.top - margin.top
              end

            delta_x = target_x - margin.left
            delta_y = target_top - -margin.top

            boxes =
              boxes
              |> shift_layout_boxes(delta_x, delta_y)
              |> Enum.map(&Map.put(&1, :out_of_flow, true))

            z_index = Map.get(style, :z_index, :auto)
            {:ok, %{z_index: z_index, index: index, boxes: boxes}}

          {:error, reason} ->
            {:error, reason}
        end

      false ->
        {:error, :invalid_layout}
    end
  end

  defp stretch_positioned_style(style, offsets, containing_block) do
    style =
      case {Map.get(style, :width), resolved_offset(offsets.left, containing_block.width),
            resolved_offset(offsets.right, containing_block.width)} do
        {nil, left, right} when is_number(left) and is_number(right) ->
          target =
            max(
              containing_block.width - left - right -
                horizontal_margin_size(style),
              0.0
            )

          Map.put(style, :width, declared_size_for_border_box(style, :width, target))

        _ ->
          style
      end

    case {Map.get(style, :height), resolved_offset(offsets.top, containing_block.height),
          resolved_offset(offsets.bottom, containing_block.height)} do
      {nil, top, bottom} when is_number(top) and is_number(bottom) ->
        target =
          max(
            containing_block.height - top - bottom - vertical_margin_size(style),
            0.0
          )

        Map.put(style, :height, declared_size_for_border_box(style, :height, target))

      _ ->
        style
    end
  end

  defp declared_size_for_border_box(style, property, size) do
    case {Map.get(style, :box_sizing, :content_box), property} do
      {:border_box, _property} -> size
      {_box_sizing, :width} -> max(size - horizontal_box_size(style), 0.0)
      {_box_sizing, :height} -> max(size - vertical_box_size(style), 0.0)
    end
  end

  defp relatively_shifted_boxes(boxes, style, _x, y, available_width, next_y) do
    case Map.get(style, :position, :static) do
      :relative ->
        offsets = Map.get(style, :offsets, edges(:auto))
        margin = Map.get(style, :margin, edges(0.0))
        element_height = max(y - margin.top - next_y - margin.bottom, 0.0)
        left = resolved_offset(offsets.left, available_width)
        right = resolved_offset(offsets.right, available_width)
        top = resolved_offset(offsets.top, element_height)
        bottom = resolved_offset(offsets.bottom, element_height)
        delta_x = if(is_number(left), do: left, else: if(is_number(right), do: -right, else: 0.0))
        delta_y = if(is_number(top), do: -top, else: if(is_number(bottom), do: bottom, else: 0.0))
        shift_layout_boxes(boxes, delta_x, delta_y)

      _ ->
        boxes
    end
  end

  defp resolved_offset(offset, available) do
    case offset do
      :auto -> nil
      {:percent, ratio} -> ratio * available
      offset when is_number(offset) -> offset
      _ -> nil
    end
  end

  defp horizontal_margin_size(style) do
    margin = Map.get(style, :margin, edges(0.0))
    margin.left + margin.right
  end

  defp vertical_margin_size(style) do
    margin = Map.get(style, :margin, edges(0.0))
    margin.top + margin.bottom
  end

  defp stack_positioned_boxes(normal_boxes, positioned_entries, stacking_context?) do
    {background_boxes, content_boxes} =
      Enum.split_while(normal_boxes, &(Map.get(&1, :stacking_background, false) == true))

    sorted =
      Enum.sort_by(positioned_entries, fn entry ->
        z_index = if(is_integer(entry.z_index), do: entry.z_index, else: 0)
        {z_index, entry.index}
      end)

    {negative, nonnegative} =
      Enum.split_with(sorted, &(is_integer(&1.z_index) and &1.z_index < 0))

    negative_boxes = Enum.flat_map(negative, & &1.boxes)
    nonnegative_boxes = Enum.flat_map(nonnegative, & &1.boxes)

    case stacking_context? do
      true -> background_boxes ++ negative_boxes ++ content_boxes ++ nonnegative_boxes
      false -> negative_boxes ++ background_boxes ++ content_boxes ++ nonnegative_boxes
    end
  end

  defp mark_container_background(boxes, style, x, y, available_width, next_y) do
    margin = Map.get(style, :margin, edges(0.0))
    box_x = x + margin.left
    box_top = y - margin.top
    box_width = positioned_element_border_width(style, available_width)
    box_height = max(box_top - next_y - margin.bottom, 0.0)
    box_y = box_top - box_height

    {background, remaining} =
      Enum.split_while(boxes, fn box ->
        background_paints_area?(box, box_x, box_y, box_width, box_height)
      end)

    Enum.map(background, &Map.put(&1, :stacking_background, true)) ++ remaining
  end

  defp background_paints_area?(box, x, y, width, height) do
    case box do
      %{type: :rect, paint_layer: :container_background} ->
        approximately_equal?(box.x, x) and approximately_equal?(box.y, y) and
          approximately_equal?(box.width, width) and approximately_equal?(box.height, height)

      %{type: :image, paint_layer: :container_background, clip: clip} ->
        approximately_equal?(clip.x, x) and approximately_equal?(clip.y, y) and
          approximately_equal?(clip.width, width) and approximately_equal?(clip.height, height)

      _ ->
        false
    end
  end

  defp approximately_equal?(left, right) do
    is_number(left) and is_number(right) and abs(left - right) <= 0.0001
  end

  defp shift_layout_boxes(boxes, delta_x, delta_y) do
    Enum.map(boxes, fn box ->
      box =
        case box do
          %{type: :glyph_group, boxes: children} ->
            %{box | boxes: shift_layout_boxes(children, delta_x, delta_y)}

          _ ->
            box
        end

      NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.translate_box(box, delta_x, delta_y)
    end)
  end

  defp layout_block_content(children, style, x, y, width, metadata) do
    key = {__MODULE__, :svg_containing_height}
    previous = Process.get(key)
    height = resolved_content_size(style, :height, previous, nil)
    Process.put(key, height)

    try do
      do_layout_block_content(children, style, x, y, width, metadata)
    after
      if is_nil(previous), do: Process.delete(key), else: Process.put(key, previous)
    end
  end

  defp do_layout_block_content(children, style, x, y, width, metadata) do
    style =
      Map.put(
        style,
        :_inline_available_height,
        resolved_content_size(style, :height, Map.get(style, :_containing_height), nil)
      )

    cond do
      Map.has_key?(style, :embedded_document) ->
        surface = style.embedded_document
        height = resolved_content_size(style, :height, Map.get(style, :_containing_height), 0.0)

        with {:ok, group} <- EmbeddedViewport.layout(surface, width, height) do
          group =
            group
            |> Map.put(:role, :embedded_viewport_host)
            |> Map.put(:x, x)
            |> Map.put(:y, y - height)
            |> Map.put(:transform, {1, 0, 0, 1, x, y - height})
            |> Map.merge(metadata)

          {:ok, [group], height}
        end

      Map.get(style, :_fieldset, false) ->
        padding = Map.get(style, :padding, edges(0.0))
        border = Map.get(style, :border_widths, edges(0.0))
        used_style = style |> Map.put(:margin, edges(0.0)) |> Map.delete(:margin_inline_auto)

        with {:ok, boxes, next_y} <-
               layout_fieldset(
                 %{type: :element, style: used_style, children: children},
                 x - border.left - padding.left,
                 y + border.top + padding.top,
                 width + horizontal_box_size(style)
               ) do
          {:ok, boxes, y + border.top + padding.top - next_y - vertical_box_size(style)}
        end

      Map.get(style, :_fieldset_content_display) in [:flex, :grid] ->
        display = style._fieldset_content_display

        inner_style =
          style
          |> Map.drop([:width, :height, :min_height, :max_height, :min_width, :max_width])
          |> Map.put(:display, display)
          |> Map.put(:margin, edges(0.0))
          |> Map.put(:padding, edges(0.0))
          |> Map.put(:border_widths, edges(0.0))

        result =
          if display == :flex,
            do: layout_flex(inner_style, children, x, y, width),
            else: layout_grid(inner_style, children, x, y, width)

        case result do
          {:ok, boxes, next_y} -> {:ok, tag_boxes(boxes, metadata), y - next_y}
          error -> error
        end

      true ->
        case inline_runs(children, style) do
          {:ok, runs} ->
            {boxes, content_height} = inline_text_boxes(runs, style, x, y, width, metadata)
            {:ok, boxes, content_height}

          {:error, _reason} ->
            with {:ok, boxes, next_y} <- layout_block_flow(children, style, x, y, width) do
              {:ok, boxes, y - next_y}
            end
        end
    end
  end

  defp layout_block_flow(children, style, x, y, width) do
    result =
      children
      |> group_inline_flow_children()
      |> Enum.reduce_while({:ok, [], y, nil}, fn child,
                                                 {:ok, boxes, current_y, previous_margin_bottom} ->
        {margin_top, margin_bottom} = flow_child_vertical_margins(child, style)

        collapsed_margin = collapsed_sibling_margin(previous_margin_bottom, margin_top)

        case layout_flow_child(child, style, x, current_y + collapsed_margin, width) do
          {:ok, child_boxes, next_y} ->
            next_margin_bottom =
              following_sibling_margin(
                child_boxes,
                previous_margin_bottom,
                margin_top,
                margin_bottom
              )

            {:cont, {:ok, Enum.reverse(child_boxes, boxes), next_y, next_margin_bottom}}

          {:error, reason} ->
            {:halt, {:error, reason}}
        end
      end)

    case result do
      {:ok, boxes, next_y, _margin_bottom} -> {:ok, Enum.reverse(boxes), next_y}
      {:error, reason} -> {:error, reason}
    end
  end

  defp flow_child_vertical_margins(child, parent_style) do
    case {Map.get(parent_style, :display), child} do
      {parent_display, %{type: :element, style: %{display: display} = style}}
      when parent_display in [:block, :table_cell] and
             display not in [:none, :inline, :line_break, :soft_break] ->
        margin = Map.get(style, :margin, edges(0.0))
        {margin.top, margin.bottom}

      _ ->
        {0.0, 0.0}
    end
  end

  defp collapsed_sibling_margin(previous_margin_bottom, margin_top) do
    case previous_margin_bottom do
      previous when is_number(previous) ->
        collapsed = max(max(previous, margin_top), 0.0) + min(min(previous, margin_top), 0.0)
        previous + margin_top - collapsed

      _ ->
        0.0
    end
  end

  defp following_sibling_margin(child_boxes, previous_margin_bottom, margin_top, margin_bottom) do
    case child_boxes do
      [] ->
        margins =
          [previous_margin_bottom, margin_top, margin_bottom]
          |> Enum.reject(&is_nil/1)

        max(Enum.max(margins), 0.0) + min(Enum.min(margins), 0.0)

      _ ->
        margin_bottom
    end
  end

  defp layout_flow_child(child, style, x, y, width) do
    result =
      case child do
        %{type: :inline_flow, children: children} ->
          layout_inline_flow(children, style, x, y, width)

        %{style: child_style} ->
          case Map.get(style, :_inline_available_height) do
            height when is_number(height) ->
              layout_block(
                %{child | style: Map.put(child_style, :_containing_height, height)},
                x,
                y,
                width
              )

            _ ->
              layout_block(child, x, y, width)
          end
      end

    case result do
      {:ok, boxes, next_y} ->
        child_margin = Map.get(Map.get(child, :style, %{}), :margin, edges(0.0))

        boxes =
          Enum.reduce(Map.get(child, :html_target_ancestors, []), boxes, fn target, boxes ->
            attach_html_target(
              %{html_target: target},
              boxes,
              x + child_margin.left,
              y - child_margin.top
            )
          end)

        {:ok, boxes, next_y}

      error ->
        error
    end
  end

  defp group_inline_flow_children(children) do
    {groups, pending} =
      Enum.reduce(children, {[], []}, fn child, acc ->
        case child do
          %{type: :element, style: %{display: :inline} = inline_style, children: nested} ->
            case group_inline_flow_children(nested) do
              [] ->
                append_inline_flow_group(child, acc)

              [%{type: :inline_flow}] ->
                append_inline_flow_group(child, acc)

              nested_groups ->
                nested_groups =
                  case Map.get(inline_style, :html_target) do
                    nil ->
                      nested_groups

                    target ->
                      List.update_at(
                        nested_groups,
                        0,
                        &Map.update(&1, :html_target_ancestors, [target], fn targets ->
                          [target | targets]
                        end)
                      )
                  end

                Enum.reduce(nested_groups, acc, &append_inline_flow_group/2)
            end

          _ ->
            append_inline_flow_group(child, acc)
        end
      end)

    groups =
      case pending do
        [] -> groups
        pending -> [%{type: :inline_flow, children: Enum.reverse(pending)} | groups]
      end

    Enum.reverse(groups)
  end

  defp append_inline_flow_group(child, {groups, pending}) do
    case child do
      %{type: :inline_flow, children: nested} ->
        {groups, Enum.reverse(nested, pending)}

      _ ->
        case inline_flow_child?(child) do
          true ->
            {groups, [child | pending]}

          false ->
            groups =
              case pending do
                [] -> groups
                pending -> [%{type: :inline_flow, children: Enum.reverse(pending)} | groups]
              end

            {[child | groups], []}
        end
    end
  end

  defp inline_flow_child?(child) do
    case child do
      %{type: :text} ->
        true

      %{type: :element, style: %{display: :image, image_display: :inline}} ->
        true

      %{type: :element, style: %{display: display}} ->
        display in [
          :inline,
          :inline_block,
          :inline_flex,
          :inline_grid,
          :line_break,
          :soft_break,
          :none
        ]
    end
  end

  defp layout_inline_flow(children, style, x, y, width) do
    with {:ok, runs} <- inline_runs(children, style) do
      case Enum.any?(runs, fn run ->
             Map.get(run, :type) in [
               :inline_block,
               :inline_image,
               :inline_positioning_marker,
               :soft_break
             ] or
               Map.get(run, :hard_break, false) or trim_inline_whitespace(run.text, run) != ""
           end) do
        true ->
          {boxes, content_height} = inline_text_boxes(runs, style, x, y, width, %{})
          {:ok, boxes, y - content_height}

        false ->
          {:ok, [], y}
      end
    end
  end

  defp svg_content_size(style, available_width) do
    scene = style.svg_scene
    height = Process.get({__MODULE__, :svg_containing_height})

    case SvgScene.measure(scene, available_width || 0.0, height) do
      {:ok, viewport} ->
        apply_image_size_constraints(
          style,
          viewport.width,
          viewport.height,
          viewport.intrinsic_ratio || 1.0,
          available_width,
          height,
          is_nil(viewport.intrinsic_ratio) or
            (scene.style.svg_width != :auto and scene.style.svg_height != :auto)
        )

      {:error, _} = error ->
        throw({:render_resource_limit, error})
    end
  end

  defp svg_drawing_boxes(style, box_x, box_top, content_width, content_height) do
    padding = Map.get(style, :padding, edges(0.0))
    border = Map.get(style, :border_widths, edges(0.0))
    box_width = content_width + horizontal_box_size(style)
    box_height = content_height + vertical_box_size(style)

    scene = %{
      style.svg_scene
      | style:
          Map.merge(
            style.svg_scene.style,
            %{svg_width: {:points, content_width}, svg_height: {:points, content_height}}
          )
    }

    case SvgScene.paint(scene, content_width) do
      {:ok, measured} ->
        background = background_box(style, 0.0, 0.0, box_width, box_height)

        artwork =
          case measured.image do
            nil ->
              []

            image ->
              fitted_image_boxes(
                style,
                border.left + padding.left,
                border.bottom + padding.bottom,
                content_width,
                content_height,
                image
              )
              |> Enum.map(fn box ->
                if Map.get(style, :link_url) != nil or Map.get(style, :link_destination) != nil do
                  box
                  |> Map.merge(Map.take(style, [:link_url, :link_destination]))
                  |> Map.put(
                    :link_rect,
                    %{x: 0.0, y: 0.0, width: box_width, height: box_height}
                  )
                else
                  box
                end
              end)
          end

        if box_width > 0 and box_height > 0 do
          [
            %{
              type: HtmlValidator.reserve_layout_box(:drawing_group),
              x: box_x,
              y: box_top - box_height,
              width: box_width,
              height: box_height,
              transform: {1.0, 0.0, 0.0, 1.0, box_x, box_top - box_height},
              baseline: 0.0,
              boxes: background ++ artwork,
              svg_scene_contract: Map.drop(measured, [:image])
            }
          ]
        else
          []
        end

      {:error, _} = error ->
        throw({:render_resource_limit, error})
    end
  end

  defp layout_image(style, x, y, width) do
    margin = Map.get(style, :margin, edges(0.0, 0.0, Map.get(style, :margin_after, 0.0), 0.0))
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))
    {content_width, content_height} = image_content_size(style, width, nil)
    box_x = x + margin.left
    box_top = y - margin.top

    box_width =
      content_width + border_widths.left + padding.left + padding.right + border_widths.right

    box_height =
      border_widths.top + padding.top + content_height + padding.bottom + border_widths.bottom

    available_box_width = width - margin.left - margin.right

    box_x =
      case Map.get(style, :margin_inline_auto) do
        %{left: true, right: true} -> box_x + max(available_box_width - box_width, 0.0) / 2
        %{left: true, right: false} -> box_x + max(available_box_width - box_width, 0.0)
        _ -> box_x
      end

    flow_metadata =
      style
      |> break_metadata()
      |> Map.put(:flow_id, {:image, box_x, box_top})

    background_box =
      style
      |> background_box(box_x, box_top - box_height, box_width, box_height)
      |> tag_boxes(flow_metadata)

    image_boxes =
      fitted_image_boxes(
        style,
        box_x + border_widths.left + padding.left,
        box_top - border_widths.top - padding.top - content_height,
        content_width,
        content_height,
        Map.fetch!(style, :image)
      )
      |> tag_boxes(flow_metadata)
      |> Enum.map(fn box ->
        if is_binary(Map.get(style, :link_url)) do
          box
          |> Map.put(:link_url, style.link_url)
          |> Map.put(:link_rect, %{
            x: box_x,
            y: box_top - box_height,
            width: box_width,
            height: box_height
          })
        else
          box
        end
      end)

    next_y = box_top - box_height - margin.bottom
    extents = embedded_extent_boxes(box_x, box_top, box_width, box_height, margin, flow_metadata)

    {:ok, attach_html_target(style, background_box ++ image_boxes ++ extents, box_x, box_top),
     next_y}
  end

  # Invisible border-box extents still determine an embedded document's scroll
  # area. Existing nonpainting markers retain that geometry without extra paint.
  defp embedded_extent_boxes(x, top, width, height, margin, metadata) do
    case ResourceContext.current() do
      %{depth: depth} when depth > 0 ->
        [
          %{
            type: HtmlValidator.reserve_layout_box(:flow_marker),
            x: x + width + max(margin.right, 0),
            y: top - height - max(margin.bottom, 0),
            width: 0.0,
            height: height + max(margin.bottom, 0),
            role: :embedded_extent
          }
          |> Map.merge(metadata)
        ]

      _ ->
        []
    end
  end

  defp image_content_size(style) do
    image_content_size(style, nil, nil)
  end

  defp image_content_size(style, available_width, available_height) do
    image = Map.fetch!(style, :image)
    natural_width = Map.fetch!(image, :width)
    natural_height = Map.fetch!(image, :height)
    # A zero source size normalizes w candidates to infinite density and 0x0
    # natural dimensions. The decoded pixel ratio still supplies the intrinsic ratio.
    natural_ratio =
      if natural_height == 0,
        do: image.width_px / image.height_px,
        else: natural_width / natural_height

    ratio = Map.get(style, :aspect_ratio, natural_ratio)
    width = resolved_content_size(style, :width, available_width, nil)
    height = resolved_content_size(style, :height, available_height, nil)

    {content_width, content_height} =
      case {width, height} do
        {width, height} when is_number(width) and is_number(height) ->
          {width, height}

        {width, _height} when is_number(width) ->
          {width, HtmlValidator.aspect_ratio_dimension(width, ratio, :height, :layout)}

        {_width, height} when is_number(height) ->
          {HtmlValidator.aspect_ratio_dimension(height, ratio, :width, :layout), height}

        _ ->
          {natural_width, natural_height}
      end

    apply_image_size_constraints(
      style,
      content_width,
      content_height,
      ratio,
      available_width,
      available_height,
      is_number(width) and is_number(height)
    )
  end

  defp layout_grid(style, children, x, y, width) do
    margin = Map.get(style, :margin, edges(0.0, 0.0, Map.get(style, :margin_after, 0.0), 0.0))
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))
    available_box_width = width - margin.left - margin.right

    content_width =
      resolved_content_size(
        style,
        :width,
        width,
        available_box_width - horizontal_box_size(style)
      )

    box_width =
      content_width + border_widths.left + padding.left + padding.right + border_widths.right

    box_x = x + margin.left
    box_top = y - margin.top
    content_x = box_x + border_widths.left + padding.left
    content_top = box_top - border_widths.top - padding.top

    with {:ok, items} <- grid_items(children) do
      placed_items = place_grid_items(items, style)
      column_count = grid_axis_count(placed_items, style, :column)
      row_count = grid_axis_count(placed_items, style, :row)
      column_tracks = grid_tracks(style, :column, column_count)
      row_tracks = grid_tracks(style, :row, row_count)
      column_intrinsics = grid_column_intrinsics(placed_items, column_count)

      column_sizes =
        resolve_grid_columns(
          column_tracks,
          column_intrinsics,
          content_width,
          grid_column_gap(style)
        )

      placed_items =
        grid_items_with_resolved_heights(
          placed_items,
          style,
          column_sizes,
          grid_column_gap(style)
        )

      row_intrinsics = grid_row_intrinsics(placed_items, row_count)
      available_height = resolved_content_size(style, :height, nil, nil)

      row_sizes =
        resolve_grid_rows(
          row_tracks,
          row_intrinsics,
          grid_row_gap(style),
          available_height,
          Map.get(style, :align_content, :stretch)
        )

      content_height = available_height || grid_tracks_size(row_sizes, grid_row_gap(style))

      flow_metadata =
        style
        |> break_metadata()
        |> Map.put(:flow_id, {:grid, box_x, box_top})

      box_height =
        border_widths.top + padding.top + content_height + padding.bottom +
          border_widths.bottom

      background_box =
        style
        |> background_box(box_x, box_top - box_height, box_width, box_height)
        |> tag_boxes(flow_metadata)

      with {:ok, item_boxes} <-
             grid_item_boxes(
               placed_items,
               style,
               content_x,
               content_top,
               content_width,
               content_height,
               column_sizes,
               row_sizes,
               flow_metadata.flow_id
             ) do
        next_y = box_top - box_height - margin.bottom
        {:ok, background_box ++ item_boxes, next_y}
      end
    end
  end

  defp grid_items(children) do
    children
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn {child, index}, {:ok, acc} ->
      case grid_item(child, index) do
        {:ok, nil} -> {:cont, {:ok, acc}}
        {:ok, item} -> {:cont, {:ok, acc ++ [preserve_item_outline(item, child)]}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp grid_item(child, index) do
    case child do
      %{type: :text, text: text} when is_binary(text) ->
        text = normalize_inline_whitespace(text, child.style)

        case trim_inline_whitespace(text, false) do
          "" ->
            {:ok, nil}

          _ ->
            style = child.style |> text_style() |> Map.put(:display, :inline)
            {:ok, build_grid_item(style, [%{text: text, style: child.style}], index)}
        end

      %{type: :element, style: %{display: :none}} ->
        {:ok, nil}

      %{type: :element, style: %{display: :soft_break} = style} ->
        # Grid preserves an empty text item; the word break has no own CSS box.
        style = style |> text_style() |> Map.put(:display, :inline)
        {:ok, build_grid_item(style, [], index)}

      %{type: :element, style: %{display: :image} = style} ->
        {:ok, build_grid_image_item(style, index)}

      %{type: :element, style: %{_fieldset: true} = style, children: children} ->
        build_grid_block_item(style, children, index)

      %{type: :element, style: style, children: children} when is_list(children) ->
        case inline_runs(children, style) do
          {:ok, runs} -> {:ok, build_grid_item(style, runs, index)}
          {:error, _reason} -> build_grid_block_item(style, children, index)
        end
    end
  end

  defp build_grid_item(style, runs, index) do
    margin = Map.get(style, :margin, edges(0.0))
    text_width = Enum.reduce(runs, 0.0, fn run, acc -> acc + inline_run_width(run, nil) end)
    content_width = resolved_content_size(style, :width, nil, text_width)

    content_height =
      resolved_content_size(
        style,
        :height,
        nil,
        if(Enum.any?(runs, &(Map.get(&1, :type) == :inline_image)),
          do: inline_content_height(runs, content_width, style),
          else: Map.get(style, :line_height, 14.4)
        )
      )

    %{
      index: index,
      style: style,
      runs: runs,
      intrinsic_width: content_width + horizontal_box_size(style),
      intrinsic_height: content_height + vertical_box_size(style),
      margin: margin,
      row_start: Map.get(style, :grid_row_start, :auto),
      row_end: Map.get(style, :grid_row_end, :auto),
      column_start: Map.get(style, :grid_column_start, :auto),
      column_end: Map.get(style, :grid_column_end, :auto)
    }
  end

  defp build_grid_block_item(style, children, index) do
    margin = Map.get(style, :margin, edges(0.0))
    intrinsic_width = flex_block_intrinsic_width(children)
    content_width = resolved_content_size(style, :width, nil, intrinsic_width)

    case layout_container_content_height(style, children, max(content_width, 0.0)) do
      {:ok, content_height} ->
        content_height = resolved_content_size(style, :height, nil, content_height)

        {:ok,
         %{
           index: index,
           style: style,
           children: children,
           intrinsic_width: content_width + horizontal_box_size(style),
           intrinsic_height: content_height + vertical_box_size(style),
           margin: margin,
           row_start: Map.get(style, :grid_row_start, :auto),
           row_end: Map.get(style, :grid_row_end, :auto),
           column_start: Map.get(style, :grid_column_start, :auto),
           column_end: Map.get(style, :grid_column_end, :auto)
         }}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp build_grid_image_item(style, index) do
    margin = Map.get(style, :margin, edges(0.0))
    {content_width, content_height} = image_content_size(style)

    %{
      index: index,
      style: style,
      image: Map.fetch!(style, :image),
      intrinsic_width: content_width + horizontal_box_size(style),
      intrinsic_height: content_height + vertical_box_size(style),
      margin: margin,
      row_start: Map.get(style, :grid_row_start, :auto),
      row_end: Map.get(style, :grid_row_end, :auto),
      column_start: Map.get(style, :grid_column_start, :auto),
      column_end: Map.get(style, :grid_column_end, :auto)
    }
  end

  defp place_grid_items(items, style) do
    explicit_count = style |> Map.get(:grid_template_columns, []) |> length() |> max(1)

    column_count =
      Enum.reduce(items, explicit_count, fn item, count ->
        span = grid_axis_span(item.column_start, item.column_end)
        start = grid_line_start(item.column_start, item.column_end)
        max(count, if(is_integer(start), do: start + span - 1, else: span))
      end)

    ordered_items =
      Enum.sort_by(items, fn item ->
        row = grid_line_start(item.row_start, item.row_end)
        column = grid_line_start(item.column_start, item.column_end)

        cond do
          is_integer(row) and is_integer(column) -> 0
          is_integer(row) -> 1
          true -> 2
        end
      end)

    {_occupied, _cursor, placed} =
      Enum.reduce(ordered_items, {MapSet.new(), {1, 1}, []}, fn item, {occupied, cursor, acc} ->
        placement = grid_item_placement(item, column_count, occupied, cursor)

        occupied =
          placement.row_start..(placement.row_end - 1)
          |> Enum.reduce(occupied, fn row, row_acc ->
            placement.column_start..(placement.column_end - 1)
            |> Enum.reduce(row_acc, &MapSet.put(&2, {row, &1}))
          end)

        cursor =
          if grid_line_start(item.row_start, item.row_end) == :auto,
            do: {placement.row_start, placement.column_start},
            else: cursor

        {occupied, cursor, [Map.merge(item, placement) | acc]}
      end)

    Enum.sort_by(placed, & &1.index)
  end

  defp grid_item_placement(item, column_count, occupied, {cursor_row, cursor_column} = cursor) do
    column_start = grid_line_start(item.column_start, item.column_end)
    row_start = grid_line_start(item.row_start, item.row_end)
    column_span = grid_axis_span(item.column_start, item.column_end)
    row_span = grid_axis_span(item.row_start, item.row_end)

    case {row_start, column_start} do
      {row_start, column_start} when is_integer(row_start) and is_integer(column_start) ->
        grid_placement(row_start, column_start, row_span, column_span)

      {row_start, :auto} when is_integer(row_start) ->
        column_start =
          first_free_grid_column(occupied, row_start, column_count, row_span, column_span)

        grid_placement(row_start, column_start, row_span, column_span)

      {:auto, column_start} when is_integer(column_start) ->
        start_row = if column_start < cursor_column, do: cursor_row + 1, else: cursor_row
        row_start = first_free_grid_row(occupied, column_start, row_span, column_span, start_row)
        grid_placement(row_start, column_start, row_span, column_span)

      _ ->
        {row_start, column_start} =
          first_free_grid_cell(occupied, column_count, row_span, column_span, cursor)

        grid_placement(row_start, column_start, row_span, column_span)
    end
  end

  defp grid_placement(row_start, column_start, row_span, column_span) do
    %{
      row_start: row_start,
      row_end: row_start + row_span,
      column_start: column_start,
      column_end: column_start + column_span
    }
  end

  defp grid_line_start(start_line, end_line) do
    case {start_line, end_line} do
      {line, _end_line} when is_integer(line) ->
        line

      {_start_line, line} when is_integer(line) ->
        max(line - grid_axis_span(start_line, end_line), 1)

      _ ->
        :auto
    end
  end

  defp grid_axis_span(start_line, end_line) do
    case {start_line, end_line} do
      {_start_line, {:span, span}} ->
        span

      {start_line, end_line} when is_integer(start_line) and is_integer(end_line) ->
        max(end_line - start_line, 1)

      {{:span, span}, _end_line} ->
        span

      _ ->
        1
    end
  end

  defp first_free_grid_column(occupied, row_start, column_count, row_span, column_span) do
    1..column_count
    |> Enum.find(&grid_cells_free?(occupied, row_start, &1, row_span, column_span))
    |> case do
      nil -> column_count + 1
      column -> column
    end
  end

  defp first_free_grid_row(occupied, column_start, row_span, column_span, start_row) do
    start_row
    |> Stream.iterate(&(&1 + 1))
    |> Enum.find(&grid_cells_free?(occupied, &1, column_start, row_span, column_span))
  end

  defp first_free_grid_cell(
         occupied,
         column_count,
         row_span,
         column_span,
         {cursor_row, cursor_column}
       ) do
    cursor_row
    |> Stream.iterate(&(&1 + 1))
    |> Enum.reduce_while(nil, fn row, _acc ->
      column =
        1..max(column_count - column_span + 1, 1)
        |> Enum.find(fn column ->
          (row > cursor_row or column >= cursor_column) and
            grid_cells_free?(occupied, row, column, row_span, column_span)
        end)

      case column do
        nil -> {:cont, nil}
        column -> {:halt, {row, column}}
      end
    end)
  end

  defp grid_cells_free?(occupied, row_start, column_start, row_span, column_span) do
    row_start..(row_start + row_span - 1)
    |> Enum.all?(fn row ->
      column_start..(column_start + column_span - 1)
      |> Enum.all?(&(!MapSet.member?(occupied, {row, &1})))
    end)
  end

  defp grid_axis_count(items, style, axis) do
    explicit_count =
      case axis do
        :column -> style |> Map.get(:grid_template_columns, []) |> length()
        :row -> style |> Map.get(:grid_template_rows, []) |> length()
      end

    item_count =
      items
      |> Enum.map(fn item ->
        case axis do
          :column -> item.column_end - 1
          :row -> item.row_end - 1
        end
      end)
      |> Enum.max(fn -> 0 end)

    max(max(explicit_count, item_count), 1)
  end

  defp grid_tracks(style, axis, count) do
    {template, auto_track} =
      case axis do
        :column ->
          {Map.get(style, :grid_template_columns, []), Map.get(style, :grid_auto_columns, :auto)}

        :row ->
          {Map.get(style, :grid_template_rows, []), Map.get(style, :grid_auto_rows, :auto)}
      end

    missing = max(count - length(template), 0)
    template ++ List.duplicate(auto_track, missing)
  end

  defp grid_column_intrinsics(items, column_count) do
    1..column_count
    |> Enum.map(fn column ->
      items
      |> Enum.filter(&(&1.column_start == column and &1.column_end == column + 1))
      |> Enum.map(&(&1.intrinsic_width + &1.margin.left + &1.margin.right))
      |> Enum.max(fn -> 0.0 end)
    end)
  end

  defp resolve_grid_columns(tracks, column_intrinsics, available_size, gap) do
    resolve_grid_track_sizes(tracks, column_intrinsics, available_size, gap, :column)
  end

  defp grid_row_intrinsics(items, row_count) do
    1..row_count
    |> Enum.map(fn row ->
      items
      |> Enum.filter(&(&1.row_start == row and &1.row_end == row + 1))
      |> Enum.map(&(&1.intrinsic_height + &1.margin.top + &1.margin.bottom))
      |> Enum.max(fn -> 0.0 end)
    end)
  end

  defp grid_items_with_resolved_heights(items, container_style, column_sizes, column_gap) do
    Enum.map(
      items,
      &grid_item_with_resolved_height(&1, container_style, column_sizes, column_gap)
    )
  end

  defp grid_item_with_resolved_height(item, container_style, column_sizes, column_gap) do
    area_width = grid_track_span(column_sizes, column_gap, item.column_start, item.column_end)
    justify = grid_item_justify(item, container_style)
    box_width = grid_aligned_box_size(item, :width, justify, area_width)
    content_width = max(box_width - horizontal_box_size(item.style), 0.0)

    content_height = grid_item_content_height(item, content_width)
    Map.put(item, :intrinsic_height, content_height + vertical_box_size(item.style))
  end

  defp grid_item_content_height(item, content_width) do
    cond do
      Map.has_key?(item, :runs) ->
        resolved_content_size(
          item.style,
          :height,
          nil,
          inline_content_height(item.runs, content_width, item.style)
        )

      Map.has_key?(item, :children) ->
        {:ok, content_height} =
          layout_container_content_height(item.style, item.children, content_width)

        resolved_content_size(item.style, :height, nil, content_height)

      Map.has_key?(item, :image) ->
        {_content_width, content_height} = image_content_size(item.style, content_width, nil)
        content_height
    end
  end

  defp resolve_grid_rows(tracks, row_intrinsics, gap, available_height, align_content) do
    row_sizes =
      resolve_grid_track_sizes(tracks, row_intrinsics, available_height, gap, :row)

    stretch_grid_rows(row_sizes, tracks, gap, available_height, align_content)
  end

  defp resolve_grid_track_sizes(tracks, intrinsics, available_size, gap, axis) do
    track_data =
      tracks
      |> Enum.with_index()
      |> Enum.map(fn {track, index} ->
        intrinsic = Enum.at(intrinsics, index, 0.0)
        fraction = grid_track_fraction(track)

        %{
          fixed_size: grid_fixed_track_size(track, intrinsic),
          fraction: fraction,
          index: index,
          minimum:
            case fraction do
              nil -> 0.0
              _fraction -> grid_flexible_track_minimum(track, intrinsic, axis)
            end
        }
      end)

    fixed_total =
      Enum.reduce(track_data, 0.0, fn data, total ->
        case data.fraction do
          nil -> total + data.fixed_size
          _fraction -> total
        end
      end)

    flexible_tracks = Enum.reject(track_data, &is_nil(&1.fraction))
    minimum_total = Enum.reduce(flexible_tracks, 0.0, &(&1.minimum + &2))
    gap_total = gap * max(length(tracks) - 1, 0)

    track_space =
      case available_size do
        size when is_number(size) -> max(size - gap_total, 0.0)
        _ -> fixed_total + minimum_total
      end

    flexible_sizes =
      distribute_flexible_grid_tracks(flexible_tracks, max(track_space - fixed_total, 0.0))

    Enum.map(track_data, fn data ->
      case data.fraction do
        nil -> data.fixed_size
        _fraction -> Map.fetch!(flexible_sizes, data.index)
      end
    end)
  end

  defp grid_track_fraction(track) do
    case track do
      {:fr, fraction} -> fraction
      {:minmax, _minimum, {:fr, fraction}} -> fraction
      _ -> nil
    end
  end

  defp grid_fixed_track_size(track, intrinsic) do
    case track do
      {:length, length} ->
        length

      :auto ->
        intrinsic

      {:fr, _fraction} ->
        0.0

      {:minmax, minimum, maximum} ->
        minimum_size = grid_minimum_track_size(minimum, intrinsic)

        case maximum do
          {:length, length} -> max(minimum_size, length)
          :auto -> max(minimum_size, intrinsic)
          {:fr, _fraction} -> minimum_size
        end
    end
  end

  defp grid_flexible_track_minimum(track, intrinsic, axis) do
    minimum =
      case track do
        {:fr, _fraction} -> 0.0
        {:minmax, minimum, {:fr, _fraction}} -> grid_minimum_track_size(minimum, intrinsic)
      end

    case axis do
      :row -> max(minimum, intrinsic)
      :column -> minimum
    end
  end

  defp grid_minimum_track_size(minimum, intrinsic) do
    case minimum do
      {:length, length} -> length
      :auto -> intrinsic
    end
  end

  defp distribute_flexible_grid_tracks(tracks, available_size) do
    {zero_fraction_tracks, flexible_tracks} =
      Enum.split_with(tracks, &(&1.fraction <= 0))

    resolved =
      Map.new(zero_fraction_tracks, fn track ->
        {track.index, track.minimum}
      end)

    remaining =
      max(
        available_size -
          Enum.reduce(zero_fraction_tracks, 0.0, &(&1.minimum + &2)),
        0.0
      )

    case flexible_tracks do
      [] ->
        resolved

      flexible_tracks ->
        fraction_total = Enum.reduce(flexible_tracks, 0.0, &(&1.fraction + &2))
        fraction_unit = remaining / fraction_total

        {constrained_tracks, unconstrained_tracks} =
          Enum.split_with(
            flexible_tracks,
            &(fraction_unit * &1.fraction < &1.minimum)
          )

        case constrained_tracks do
          [] ->
            Enum.reduce(unconstrained_tracks, resolved, fn track, sizes ->
              Map.put(sizes, track.index, fraction_unit * track.fraction)
            end)

          constrained_tracks ->
            constrained_size =
              Enum.reduce(constrained_tracks, 0.0, &(&1.minimum + &2))

            constrained_sizes =
              Enum.reduce(constrained_tracks, resolved, fn track, sizes ->
                Map.put(sizes, track.index, track.minimum)
              end)

            Map.merge(
              constrained_sizes,
              distribute_flexible_grid_tracks(
                unconstrained_tracks,
                max(remaining - constrained_size, 0.0)
              )
            )
        end
    end
  end

  defp stretch_grid_rows(row_sizes, tracks, gap, available_height, align_content) do
    case {align_content, available_height} do
      {:stretch, available_height} when is_number(available_height) ->
        stretchable_indexes =
          tracks
          |> Enum.with_index()
          |> Enum.flat_map(fn
            {:auto, index} -> [index]
            {_track, _index} -> []
          end)

        free_space = max(available_height - grid_tracks_size(row_sizes, gap), 0.0)

        cond do
          stretchable_indexes == [] ->
            row_sizes

          free_space == 0.0 ->
            row_sizes

          true ->
            extra = free_space / length(stretchable_indexes)

            Enum.with_index(row_sizes, fn size, index ->
              case index in stretchable_indexes do
                true -> size + extra
                false -> size
              end
            end)
        end

      _ ->
        row_sizes
    end
  end

  defp grid_tracks_size(sizes, gap) do
    Enum.sum(sizes) + gap * max(length(sizes) - 1, 0)
  end

  defp grid_item_boxes(
         items,
         style,
         x,
         y,
         content_width,
         content_height,
         column_sizes,
         row_sizes,
         container_flow_id
       ) do
    column_gap = grid_column_gap(style)
    row_gap = grid_row_gap(style)
    grid_width = grid_tracks_size(column_sizes, column_gap)
    grid_height = grid_tracks_size(row_sizes, row_gap)

    {content_x, content_gap} =
      grid_content_distribution(
        style,
        :justify_content,
        content_width,
        grid_width,
        column_gap,
        length(column_sizes)
      )

    {content_y, row_gap} =
      grid_content_distribution(
        style,
        :align_content,
        content_height,
        grid_height,
        row_gap,
        length(row_sizes)
      )

    Enum.reduce_while(items, {:ok, []}, fn item, {:ok, acc} ->
      item =
        position_grid_item(
          item,
          style,
          x + content_x,
          y - content_y,
          column_sizes,
          row_sizes,
          content_gap,
          row_gap
        )

      {:ok, boxes} = flex_item_boxes(item, :row)
      tagged_boxes = tag_atomic_boxes(boxes, %{flow_id: container_flow_id})

      anchored_boxes =
        attach_outline_anchor(item, attach_html_target(item.style, tagged_boxes, item.x, item.y))

      {:cont, {:ok, acc ++ anchored_boxes}}
    end)
  end

  defp position_grid_item(
         item,
         container_style,
         x,
         y,
         column_sizes,
         row_sizes,
         column_gap,
         row_gap
       ) do
    area_x = x + grid_track_offset(column_sizes, column_gap, item.column_start)
    area_y = y - grid_track_offset(row_sizes, row_gap, item.row_start)
    area_width = grid_track_span(column_sizes, column_gap, item.column_start, item.column_end)
    area_height = grid_track_span(row_sizes, row_gap, item.row_start, item.row_end)
    justify = grid_item_justify(item, container_style)
    align = grid_item_align(item, container_style)
    box_width = grid_aligned_box_size(item, :width, justify, area_width)
    box_height = grid_aligned_box_size(item, :height, align, area_height)
    x_offset = grid_axis_position(justify, box_width, area_width)
    y_offset = grid_axis_position(align, box_height, area_height)

    Map.merge(item, %{
      x: area_x + x_offset + item.margin.left,
      y: area_y - y_offset - item.margin.top,
      box_width: box_width,
      box_height: box_height
    })
  end

  defp grid_content_distribution(style, property, container_size, grid_size, gap, count) do
    free_space = max(container_size - grid_size, 0.0)

    case Map.get(style, property, :flex_start) do
      :flex_end ->
        {free_space, gap}

      :center ->
        {free_space / 2, gap}

      :space_between when count > 1 ->
        {0.0, gap + free_space / (count - 1)}

      :space_around when count > 0 ->
        distributed_gap = gap + free_space / count
        {free_space / count / 2, distributed_gap}

      :space_evenly when count > 0 ->
        distributed_gap = gap + free_space / (count + 1)
        {free_space / (count + 1), distributed_gap}

      _ ->
        {0.0, gap}
    end
  end

  defp grid_track_offset(sizes, gap, start_line) do
    sizes
    |> Enum.take(start_line - 1)
    |> Enum.sum()
    |> Kernel.+(gap * max(start_line - 1, 0))
  end

  defp grid_track_span(sizes, gap, start_line, end_line) do
    span_count = max(end_line - start_line, 1)

    sizes
    |> Enum.drop(start_line - 1)
    |> Enum.take(span_count)
    |> Enum.sum()
    |> Kernel.+(gap * max(span_count - 1, 0))
  end

  defp grid_aligned_box_size(item, axis, align, area_size) do
    margin =
      case axis do
        :width -> item.margin.left + item.margin.right
        :height -> item.margin.top + item.margin.bottom
      end

    intrinsic =
      case axis do
        :width -> item.intrinsic_width
        :height -> item.intrinsic_height
      end

    box_size =
      case {align, Map.get(item.style, axis), Map.has_key?(item, :image)} do
        {:stretch, auto, false} when auto in [nil, :auto] -> max(area_size - margin, 0.0)
        _ -> intrinsic
      end

    decoration =
      case axis do
        :width -> horizontal_box_size(item.style)
        :height -> vertical_box_size(item.style)
      end

    resolved_content_size(item.style, axis, area_size, max(box_size - decoration, 0.0)) +
      decoration
  end

  defp grid_axis_position(align, box_size, area_size) do
    case align do
      :flex_end -> max(area_size - box_size, 0.0)
      :center -> max((area_size - box_size) / 2, 0.0)
      _ -> 0.0
    end
  end

  defp grid_item_align(item, container_style) do
    case Map.get(item.style, :align_self, :auto) do
      :auto -> Map.get(container_style, :align_items, :stretch)
      align -> align
    end
  end

  defp grid_item_justify(item, container_style) do
    case Map.get(item.style, :justify_self, :auto) do
      :auto -> Map.get(container_style, :justify_items, :stretch)
      justify -> justify
    end
  end

  defp grid_column_gap(style) do
    Map.get(style, :column_gap, 0.0)
  end

  defp grid_row_gap(style) do
    Map.get(style, :row_gap, 0.0)
  end

  defp layout_flex(style, children, x, y, width) do
    margin = Map.get(style, :margin, edges(0.0, 0.0, Map.get(style, :margin_after, 0.0), 0.0))
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))
    available_box_width = width - margin.left - margin.right

    content_width =
      resolved_content_size(
        style,
        :width,
        width,
        available_box_width - horizontal_box_size(style)
      )

    box_width =
      content_width + border_widths.left + padding.left + padding.right + border_widths.right

    box_x = x + margin.left
    box_top = y - margin.top
    content_x = box_x + border_widths.left + padding.left
    content_top = box_top - border_widths.top - padding.top

    main_axis = flex_main_axis(style)

    with {:ok, items} <-
           flex_items(
             children,
             main_axis,
             content_width,
             flex_available_cross(main_axis, content_width)
           ) do
      case items do
        [] ->
          content_height = resolved_content_size(style, :height, nil, 0.0)
          box_height = content_height + vertical_box_size(style)

          background_box =
            background_box(style, box_x, box_top - box_height, box_width, box_height)

          {:ok, tag_boxes(background_box, break_metadata(style)),
           box_top - box_height - margin.bottom}

        items ->
          lines = flex_lines(items, style, content_width)
          content_height = flex_content_height(lines, style)

          box_height =
            border_widths.top + padding.top + content_height + padding.bottom +
              border_widths.bottom

          flow_metadata =
            style
            |> break_metadata()
            |> Map.put(:flow_id, {:flex, box_x, box_top})

          background_box =
            style
            |> background_box(box_x, box_top - box_height, box_width, box_height)
            |> tag_boxes(flow_metadata)

          with {:ok, item_boxes} <-
                 flex_line_boxes(
                   lines,
                   style,
                   content_x,
                   content_top,
                   content_width,
                   content_height,
                   flow_metadata.flow_id
                 ) do
            next_y = box_top - box_height - margin.bottom
            {:ok, background_box ++ item_boxes, next_y}
          end
      end
    end
  end

  defp flex_items(children, main_axis, available_main, available_cross) do
    children
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn {child, index}, {:ok, acc} ->
      case flex_item(child, index, main_axis, available_main, available_cross) do
        {:ok, nil} -> {:cont, {:ok, acc}}
        {:ok, item} -> {:cont, {:ok, acc ++ [preserve_item_outline(item, child)]}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
    |> case do
      {:ok, items} -> {:ok, Enum.sort_by(items, &{&1.order, &1.index})}
      {:error, reason} -> {:error, reason}
    end
  end

  defp flex_item(child, index, main_axis, available_main, available_cross) do
    case child do
      %{type: :text, text: text} when is_binary(text) ->
        case text |> normalize_inline_whitespace(child.style) |> trim_inline_whitespace(false) do
          "" ->
            {:ok, nil}

          text ->
            style = child.style |> text_style() |> Map.put(:display, :inline)
            runs = [%{text: text, style: child.style}]
            {:ok, build_flex_item(style, runs, index, main_axis, available_main, available_cross)}
        end

      %{type: :element, style: %{display: :none}} ->
        {:ok, nil}

      %{type: :element, style: %{display: :line_break}} ->
        {:ok, nil}

      %{type: :element, style: %{display: :soft_break} = style} ->
        style = style |> text_style() |> Map.put(:display, :inline)
        {:ok, build_flex_item(style, [], index, main_axis, available_main, available_cross)}

      %{type: :element, style: %{display: :image} = style} ->
        {:ok, build_flex_image_item(style, index, main_axis, available_main, available_cross)}

      %{type: :element, style: %{_fieldset: true} = style, children: children} ->
        build_flex_block_item(style, children, index, main_axis, available_main, available_cross)

      %{type: :element, style: style, children: children} when is_list(children) ->
        case inline_runs(children, style) do
          {:ok, runs} ->
            {:ok, build_flex_item(style, runs, index, main_axis, available_main, available_cross)}

          {:error, _reason} ->
            build_flex_block_item(
              style,
              children,
              index,
              main_axis,
              available_main,
              available_cross
            )
        end
    end
  end

  defp build_flex_item(style, runs, index, main_axis, available_main, available_cross) do
    margin = Map.get(style, :margin, edges(0.0))

    text_width =
      Enum.reduce(runs, 0.0, fn run, acc -> acc + inline_run_width(run, available_main) end)

    {content_main, content_cross} =
      case main_axis do
        :row ->
          content_main = flex_basis(style, :width, available_main, text_width)
          content_height = inline_content_height(runs, content_main, style)
          {content_main, resolved_content_size(style, :height, nil, content_height)}

        :column ->
          content_width = resolved_content_size(style, :width, available_cross, text_width)

          content_height =
            if Enum.any?(runs, &(Map.get(&1, :type) == :inline_image)),
              do: inline_content_height(runs, content_width, style),
              else: Map.get(style, :line_height, 14.4)

          {flex_basis(style, :height, nil, content_height), content_width}
      end

    main_box = content_main + flex_main_box_size(style, main_axis)
    cross_box = content_cross + flex_cross_box_size(style, main_axis)

    %{
      index: index,
      style: style,
      runs: runs,
      order: Map.get(style, :order, 0),
      flex_grow: Map.get(style, :flex_grow, 0.0),
      flex_shrink: Map.get(style, :flex_shrink, 1.0),
      main_axis: main_axis,
      main_box: main_box,
      cross_box: cross_box,
      outer_main: main_box + flex_main_margin_size(margin, main_axis),
      outer_cross: cross_box + flex_cross_margin_size(margin, main_axis),
      margin: margin
    }
  end

  defp build_flex_block_item(style, children, index, main_axis, available_main, available_cross) do
    margin = Map.get(style, :margin, edges(0.0))
    intrinsic_width = flex_block_intrinsic_width(children)

    content_width =
      case main_axis do
        :row ->
          flex_basis(style, :width, available_main, min(intrinsic_width, available_main))

        :column ->
          default_width = max(available_cross - flex_cross_box_size(style, :column), 0.0)

          resolved_content_size(style, :width, available_cross, default_width)
      end

    case layout_container_content_height(style, children, max(content_width, 0.0)) do
      {:ok, content_height} ->
        {content_main, content_cross} =
          case main_axis do
            :row ->
              {content_width, resolved_content_size(style, :height, nil, content_height)}

            :column ->
              {flex_basis(style, :height, nil, content_height), content_width}
          end

        main_box = content_main + flex_main_box_size(style, main_axis)
        cross_box = content_cross + flex_cross_box_size(style, main_axis)

        {:ok,
         %{
           index: index,
           style: style,
           children: children,
           order: Map.get(style, :order, 0),
           flex_grow: Map.get(style, :flex_grow, 0.0),
           flex_shrink: Map.get(style, :flex_shrink, 1.0),
           main_axis: main_axis,
           main_box: main_box,
           cross_box: cross_box,
           outer_main: main_box + flex_main_margin_size(margin, main_axis),
           outer_cross: cross_box + flex_cross_margin_size(margin, main_axis),
           margin: margin
         }}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp layout_container_content_height(style, children, content_width) do
    case Map.get(style, :display) do
      :table ->
        nested_style = Map.put(style, :margin, edges(0.0))
        available_width = content_width + horizontal_box_size(style)

        with {:ok, _boxes, next_y} <-
               layout_table(nested_style, children, 0.0, 0.0, available_width) do
          {:ok, max(0.0 - next_y - vertical_box_size(style), 0.0)}
        end

      display when display in [:grid, :inline_grid] ->
        nested_style = Map.put(style, :margin, edges(0.0))
        available_width = content_width + horizontal_box_size(style)

        with {:ok, _boxes, next_y} <-
               layout_grid(nested_style, children, 0.0, 0.0, available_width) do
          {:ok, max(0.0 - next_y - vertical_box_size(style), 0.0)}
        end

      display when display in [:flex, :inline_flex] ->
        nested_style = Map.put(style, :margin, edges(0.0))
        available_width = content_width + horizontal_box_size(style)

        with {:ok, _boxes, next_y} <-
               layout_flex(nested_style, children, 0.0, 0.0, available_width) do
          {:ok, max(0.0 - next_y - vertical_box_size(style), 0.0)}
        end

      _ ->
        with {:ok, _boxes, content_height} <-
               layout_block_content(children, style, 0.0, 0.0, content_width, %{}) do
          {:ok, content_height}
        end
    end
  end

  defp build_flex_image_item(style, index, main_axis, available_main, available_cross) do
    margin = Map.get(style, :margin, edges(0.0))

    {available_width, available_height} =
      case main_axis do
        :row -> {available_main, available_cross}
        :column -> {available_cross, available_main}
      end

    {content_width, content_height} = image_content_size(style, available_width, available_height)

    {content_main, content_cross} =
      case main_axis do
        :row -> {content_width, content_height}
        :column -> {content_height, content_width}
      end

    main_box = content_main + flex_main_box_size(style, main_axis)
    cross_box = content_cross + flex_cross_box_size(style, main_axis)

    %{
      index: index,
      style: style,
      image: Map.fetch!(style, :image),
      order: Map.get(style, :order, 0),
      flex_grow: Map.get(style, :flex_grow, 0.0),
      flex_shrink: Map.get(style, :flex_shrink, 1.0),
      main_axis: main_axis,
      main_box: main_box,
      cross_box: cross_box,
      outer_main: main_box + flex_main_margin_size(margin, main_axis),
      outer_cross: cross_box + flex_cross_margin_size(margin, main_axis),
      margin: margin
    }
  end

  defp flex_basis(style, size_property, available_size, intrinsic_size) do
    case Map.get(style, :flex_basis, :auto) do
      :auto -> resolved_content_size(style, size_property, available_size, intrinsic_size)
      basis when is_number(basis) -> basis
    end
  end

  defp flex_lines(items, style, content_width) do
    main_axis = flex_main_axis(style)
    gap = flex_main_gap(style)
    available_main = flex_available_main(style, main_axis, content_width, items, gap)

    constraint_available_main =
      case main_axis do
        :row -> available_main
        :column -> resolved_content_size(style, :height, nil, nil)
      end

    wrap = Map.get(style, :flex_wrap, :nowrap)

    lines =
      Enum.reduce(items, [], fn item, lines ->
        append_flex_item_to_lines(lines, item, wrap, available_main, gap)
      end)

    lines
    |> Enum.map(&resolve_flex_line(&1, available_main, constraint_available_main, gap))
  end

  defp append_flex_item_to_lines(lines, item, wrap, available_main, gap) do
    case lines do
      [] ->
        [%{items: [item], base_main: item.outer_main}]

      lines ->
        [line | previous] = Enum.reverse(lines)
        next_base = line.base_main + gap + item.outer_main

        case wrap == :wrap and line.items != [] and next_base > available_main do
          true ->
            Enum.reverse(previous) ++ [line, %{items: [item], base_main: item.outer_main}]

          false ->
            Enum.reverse(previous) ++
              [%{line | items: line.items ++ [item], base_main: next_base}]
        end
    end
  end

  defp resolve_flex_line(line, available_main, constraint_available_main, gap) do
    item_gap_total = gap * max(length(line.items) - 1, 0)
    base_without_gap = Enum.reduce(line.items, 0.0, &(&1.outer_main + &2))

    free_space = available_main - base_without_gap - item_gap_total

    items =
      line.items
      |> resolve_flex_item_sizes(free_space, constraint_available_main)
      |> Enum.map(fn item ->
        case {item.main_axis, Map.get(item, :runs)} do
          {:row, runs} when is_list(runs) ->
            content_width = max(item.main_box - flex_main_box_size(item.style, :row), 0.0)
            content_height = inline_content_height(runs, content_width, item.style)

            cross_box =
              resolved_content_size(item.style, :height, nil, content_height) +
                flex_cross_box_size(item.style, :row)

            %{
              item
              | cross_box: cross_box,
                outer_cross: cross_box + flex_cross_margin_size(item.margin, :row)
            }

          _ ->
            item
        end
      end)

    outer_main = Enum.reduce(items, 0.0, &(&1.outer_main + &2)) + item_gap_total
    cross = items |> Enum.map(& &1.outer_cross) |> Enum.max(fn -> 0.0 end)

    %{items: items, main: outer_main, cross: cross}
  end

  defp resolve_flex_item_sizes(items, free_space, available_main) do
    {items, free_space} =
      Enum.map_reduce(items, free_space, fn item, remaining_space ->
        {minimum, maximum} = flex_main_constraints(item, available_main)
        constrained_main = constrain_flex_main(item.main_box, minimum, maximum)

        item =
          item
          |> resize_flex_item(constrained_main)
          |> Map.merge(%{
            flex_base_main: item.main_box,
            flex_minimum_main: minimum,
            flex_maximum_main: maximum,
            flex_frozen: false
          })

        {item, remaining_space - (constrained_main - item.flex_base_main)}
      end)

    mode =
      cond do
        free_space > 0 -> :grow
        free_space < 0 -> :shrink
        true -> :none
      end

    items
    |> redistribute_flex_space(free_space, mode)
    |> Enum.map(
      &Map.drop(&1, [:flex_base_main, :flex_minimum_main, :flex_maximum_main, :flex_frozen])
    )
  end

  defp redistribute_flex_space(items, free_space, mode) do
    total_weight =
      Enum.reduce(items, 0.0, fn item, total ->
        case item.flex_frozen do
          true -> total
          false -> total + flex_distribution_weight(item, mode)
        end
      end)

    case abs(free_space) <= 1.0e-9 or total_weight <= 0 do
      true ->
        items

      false ->
        {items, consumed_space, frozen_any?} =
          Enum.reduce(items, {[], 0.0, false}, fn item, {resolved, consumed, frozen_any?} ->
            weight = flex_distribution_weight(item, mode)

            case item.flex_frozen or weight <= 0 do
              true ->
                {resolved ++ [item], consumed, frozen_any?}

              false ->
                previous_main = item.main_box
                proposed_main = item.main_box + free_space * weight / total_weight

                constrained_main =
                  constrain_flex_main(
                    proposed_main,
                    item.flex_minimum_main,
                    item.flex_maximum_main
                  )

                case abs(constrained_main - proposed_main) > 1.0e-9 do
                  true ->
                    item =
                      item
                      |> resize_flex_item(constrained_main)
                      |> Map.put(:flex_frozen, true)

                    {
                      resolved ++ [item],
                      consumed + constrained_main - previous_main,
                      true
                    }

                  false ->
                    {resolved ++ [item], consumed, frozen_any?}
                end
            end
          end)

        case frozen_any? do
          true ->
            redistribute_flex_space(items, free_space - consumed_space, mode)

          false ->
            Enum.map(items, fn item ->
              weight = flex_distribution_weight(item, mode)

              case item.flex_frozen or weight <= 0 do
                true ->
                  item

                false ->
                  resize_flex_item(item, item.main_box + free_space * weight / total_weight)
              end
            end)
        end
    end
  end

  defp flex_distribution_weight(item, mode) do
    case mode do
      :grow -> item.flex_grow
      :shrink -> item.flex_shrink * item.flex_base_main
      :none -> 0.0
    end
  end

  defp flex_main_constraints(item, available_main) do
    {minimum_property, maximum_property} =
      case item.main_axis do
        :row -> {:min_width, :max_width}
        :column -> {:min_height, :max_height}
      end

    box_size = flex_main_box_size(item.style, item.main_axis)

    constraint_box_size = fn property, fallback ->
      case resolved_constraint_size(item.style, property, available_main) do
        constraint when is_number(constraint) ->
          case Map.get(item.style, :box_sizing, :content_box) do
            :border_box -> max(constraint, box_size)
            :content_box -> constraint + box_size
          end

        _ ->
          fallback
      end
    end

    minimum = constraint_box_size.(minimum_property, box_size)
    maximum = constraint_box_size.(maximum_property, nil)

    case maximum do
      maximum when is_number(maximum) -> {minimum, max(maximum, minimum)}
      nil -> {minimum, nil}
    end
  end

  defp constrain_flex_main(size, minimum, maximum) do
    size = max(size, minimum)

    case maximum do
      maximum when is_number(maximum) -> min(size, maximum)
      nil -> size
    end
  end

  defp resize_flex_item(item, main_box) do
    outer_main = main_box + flex_main_margin_size(item.margin, item.main_axis)
    %{item | main_box: main_box, outer_main: outer_main}
  end

  defp flex_line_boxes(lines, style, x, y, content_width, content_height, container_flow_id) do
    main_axis = flex_main_axis(style)
    cross_gap = flex_cross_gap(style)

    {lines, _cross_offset} =
      Enum.reduce(lines, {[], 0.0}, fn line, {acc, cross_offset} ->
        line_cross = flex_line_cross(line, style, length(lines))

        positioned =
          flex_position_line(
            line,
            style,
            x,
            y,
            content_width,
            content_height,
            cross_offset,
            line_cross
          )

        {acc ++ positioned, cross_offset + line_cross + cross_gap}
      end)

    Enum.reduce_while(lines, {:ok, []}, fn item, {:ok, acc} ->
      {:ok, boxes} = flex_item_boxes(item, main_axis)
      tagged_boxes = tag_atomic_boxes(boxes, %{flow_id: container_flow_id})

      anchored_boxes =
        attach_outline_anchor(item, attach_html_target(item.style, tagged_boxes, item.x, item.y))

      {:cont, {:ok, acc ++ anchored_boxes}}
    end)
  end

  defp flex_position_line(
         line,
         style,
         x,
         y,
         content_width,
         content_height,
         cross_offset,
         line_cross
       ) do
    main_axis = flex_main_axis(style)
    available_main = flex_position_available_main(main_axis, content_width, content_height)
    gap = flex_main_gap(style)
    items = flex_direction_items(line.items, style)
    {main_offset, item_gap} = flex_main_distribution(style, line, available_main, gap)

    items
    |> Enum.reduce({[], main_offset}, fn item, {acc, main_offset} ->
      item =
        position_flex_item(
          item,
          style,
          x,
          y,
          content_width,
          cross_offset,
          line_cross,
          main_offset
        )

      {acc ++ [item], main_offset + item.outer_main + item_gap}
    end)
    |> elem(0)
  end

  defp position_flex_item(item, style, x, y, content_width, cross_offset, line_cross, main_offset) do
    main_axis = flex_main_axis(style)
    align = flex_item_align(item, style)
    cross_position = flex_cross_position(align, item.outer_cross, line_cross)

    case main_axis do
      :row ->
        box_width = item.main_box
        box_height = flex_aligned_cross_box(item, align, line_cross, :row)

        Map.merge(item, %{
          x: x + main_offset + item.margin.left,
          y: y - cross_offset - cross_position - item.margin.top,
          box_width: box_width,
          box_height: box_height
        })

      :column ->
        box_width = flex_aligned_cross_box(item, align, content_width, :column)
        box_height = item.main_box

        Map.merge(item, %{
          x: x + cross_position + item.margin.left,
          y: y - main_offset - item.margin.top,
          box_width: box_width,
          box_height: box_height
        })
    end
  end

  defp flex_item_boxes(item, main_axis) do
    style = item.style
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))

    case Map.get(item, :image) do
      nil ->
        case Map.get(item, :children) do
          nil -> flex_text_item_boxes(item, main_axis, style, padding, border_widths)
          children -> flex_block_item_boxes(item, style, padding, border_widths, children)
        end

      image ->
        flex_image_item_boxes(item, main_axis, style, padding, border_widths, image)
    end
  end

  defp flex_text_item_boxes(item, main_axis, style, padding, border_widths) do
    content_x = item.x + border_widths.left + padding.left

    content_width =
      item.box_width - border_widths.left - padding.left - padding.right - border_widths.right

    content_top = item.y - border_widths.top - padding.top

    background_boxes =
      background_box(style, item.x, item.y - item.box_height, item.box_width, item.box_height)

    {text_boxes, _content_height} =
      inline_text_boxes(item.runs, style, content_x, content_top, max(content_width, 0.0), %{})

    {:ok,
     case main_axis do
       :row -> background_boxes ++ text_boxes
       :column -> background_boxes ++ text_boxes
     end}
  end

  defp flex_block_item_boxes(item, style, padding, border_widths, children) do
    case Map.get(style, :_fieldset, false) do
      true ->
        style = nested_flex_item_style(style, item)

        with {:ok, boxes, _} <-
               layout_fieldset(
                 %{type: :element, style: style, children: children},
                 item.x,
                 item.y,
                 item.box_width
               ),
             do: {:ok, boxes}

      false ->
        case Map.get(style, :display) do
          :table ->
            nested_style = nested_flex_item_style(style, item)

            with {:ok, boxes, _next_y} <-
                   layout_table(nested_style, children, item.x, item.y, item.box_width) do
              {:ok, boxes}
            end

          display when display in [:grid, :inline_grid] ->
            nested_style = nested_flex_item_style(style, item)

            with {:ok, boxes, _next_y} <-
                   layout_grid(nested_style, children, item.x, item.y, item.box_width) do
              {:ok, boxes}
            end

          display when display in [:flex, :inline_flex] ->
            nested_style = nested_flex_item_style(style, item)

            with {:ok, boxes, _next_y} <-
                   layout_flex(nested_style, children, item.x, item.y, item.box_width) do
              {:ok, boxes}
            end

          _ ->
            flex_plain_block_item_boxes(item, style, padding, border_widths, children)
        end
    end
  end

  defp nested_flex_item_style(style, item) do
    style
    |> Map.put(:margin, edges(0.0))
    |> Map.put(:height, max(item.box_height - vertical_box_size(style), 0.0))
  end

  defp flex_plain_block_item_boxes(item, style, padding, border_widths, children) do
    content_x = item.x + border_widths.left + padding.left

    content_width =
      item.box_width - border_widths.left - padding.left - padding.right - border_widths.right

    content_top = item.y - border_widths.top - padding.top

    background_boxes =
      background_box(style, item.x, item.y - item.box_height, item.box_width, item.box_height)

    {:ok, content_boxes, _content_height} =
      layout_block_content(children, style, content_x, content_top, max(content_width, 0.0), %{})

    {:ok, background_boxes ++ content_boxes}
  end

  defp flex_image_item_boxes(item, main_axis, style, padding, border_widths, image) do
    content_x = item.x + border_widths.left + padding.left

    content_width =
      item.box_width - border_widths.left - padding.left - padding.right - border_widths.right

    content_height =
      item.box_height - border_widths.top - padding.top - padding.bottom - border_widths.bottom

    image_boxes =
      fitted_image_boxes(
        style,
        content_x,
        item.y - border_widths.top - padding.top - max(content_height, 0.0),
        max(content_width, 0.0),
        max(content_height, 0.0),
        image
      )

    background_boxes =
      background_box(style, item.x, item.y - item.box_height, item.box_width, item.box_height)

    {:ok,
     case main_axis do
       :row -> background_boxes ++ image_boxes
       :column -> background_boxes ++ image_boxes
     end}
  end

  defp flex_content_height(lines, style) do
    main_axis = flex_main_axis(style)
    height = resolved_content_size(style, :height, nil, nil)

    case {main_axis, height} do
      {:row, height} when is_number(height) ->
        height

      {:row, _height} ->
        cross_gap = flex_cross_gap(style)

        Enum.reduce(lines, 0.0, &(&1.cross + &2)) +
          cross_gap * max(length(lines) - 1, 0)

      {:column, height} when is_number(height) ->
        height

      {:column, _height} ->
        [line] = lines
        line.main
    end
  end

  defp flex_line_cross(line, style, line_count) do
    height = resolved_content_size(style, :height, nil, nil)

    case {flex_main_axis(style), height, line_count} do
      {:row, height, 1} when is_number(height) -> height
      {:row, _height, _line_count} -> line.cross
      {:column, _height, _line_count} -> resolved_content_size(style, :width, nil, line.cross)
    end
  end

  defp flex_position_available_main(main_axis, content_width, content_height) do
    case main_axis do
      :row -> content_width
      :column -> content_height
    end
  end

  defp flex_main_distribution(style, line, available_main, gap) do
    free_space = max(available_main - line.main, 0.0)
    count = length(line.items)

    case Map.get(style, :justify_content, :flex_start) do
      :flex_end ->
        {free_space, gap}

      :center ->
        {free_space / 2, gap}

      :space_between when count > 1 ->
        {0.0, gap + free_space / (count - 1)}

      :space_around when count > 0 ->
        item_gap = gap + free_space / count
        {free_space / count / 2, item_gap}

      :space_evenly when count > 0 ->
        item_gap = gap + free_space / (count + 1)
        {free_space / (count + 1), item_gap}

      _ ->
        {0.0, gap}
    end
  end

  defp flex_cross_position(align, item_outer_cross, line_cross) do
    case align do
      :flex_end -> max(line_cross - item_outer_cross, 0.0)
      :center -> max((line_cross - item_outer_cross) / 2, 0.0)
      _ -> 0.0
    end
  end

  defp flex_aligned_cross_box(item, align, line_cross, main_axis) do
    cross_property =
      case main_axis do
        :row -> :height
        :column -> :width
      end

    # Flex stretch applies only when the computed cross size is auto.
    case {align, Map.get(item.style, cross_property)} do
      {:stretch, auto} when auto in [nil, :auto] ->
        max(line_cross - flex_cross_margin_size(item.margin, main_axis), 0.0)

      _ ->
        item.cross_box
    end
  end

  defp flex_item_align(item, container_style) do
    case Map.get(item.style, :align_self, :auto) do
      :auto -> Map.get(container_style, :align_items, :stretch)
      align -> align
    end
  end

  defp flex_direction_items(items, style) do
    case Map.get(style, :flex_direction, :row) do
      direction when direction in [:row_reverse, :column_reverse] -> Enum.reverse(items)
      _ -> items
    end
  end

  defp flex_available_main(style, main_axis, content_width, items, gap) do
    case main_axis do
      :row ->
        content_width

      :column ->
        case resolved_content_size(style, :height, nil, nil) do
          height when is_number(height) ->
            height

          _ ->
            Enum.reduce(items, 0.0, &(&1.outer_main + &2)) + gap * max(length(items) - 1, 0)
        end
    end
  end

  defp flex_available_cross(main_axis, content_width) do
    case main_axis do
      :column -> content_width
      :row -> nil
    end
  end

  defp flex_main_axis(style) do
    case Map.get(style, :flex_direction, :row) do
      direction when direction in [:column, :column_reverse] -> :column
      _ -> :row
    end
  end

  defp flex_main_gap(style) do
    case flex_main_axis(style) do
      :row -> Map.get(style, :column_gap, 0.0)
      :column -> Map.get(style, :row_gap, 0.0)
    end
  end

  defp flex_cross_gap(style) do
    case flex_main_axis(style) do
      :row -> Map.get(style, :row_gap, 0.0)
      :column -> Map.get(style, :column_gap, 0.0)
    end
  end

  defp flex_main_box_size(style, main_axis) do
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))

    case main_axis do
      :row -> padding.left + padding.right + border_widths.left + border_widths.right
      :column -> padding.top + padding.bottom + border_widths.top + border_widths.bottom
    end
  end

  defp flex_cross_box_size(style, main_axis) do
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))

    case main_axis do
      :row -> padding.top + padding.bottom + border_widths.top + border_widths.bottom
      :column -> padding.left + padding.right + border_widths.left + border_widths.right
    end
  end

  defp flex_main_margin_size(margin, main_axis) do
    case main_axis do
      :row -> margin.left + margin.right
      :column -> margin.top + margin.bottom
    end
  end

  defp flex_cross_margin_size(margin, main_axis) do
    case main_axis do
      :row -> margin.top + margin.bottom
      :column -> margin.left + margin.right
    end
  end

  defp flex_block_intrinsic_width(children) do
    Enum.reduce(children, 0.0, fn child, width ->
      max(width, flex_child_intrinsic_width(child))
    end)
  end

  defp flex_child_intrinsic_width(child) do
    case child do
      %{type: :text, text: text, style: style} when is_binary(text) ->
        text
        |> normalize_inline_whitespace(style)
        |> trim_inline_whitespace(false)
        |> text_width(style)

      %{type: :element, style: %{display: :none}} ->
        0.0

      %{type: :element, style: %{display: :image} = style} ->
        {width, _height} = image_content_size(style)
        width + horizontal_box_size(style)

      %{type: :element, style: style, children: children} when is_list(children) ->
        case inline_runs(children, style) do
          {:ok, runs} ->
            runs
            |> Enum.reduce(0.0, fn run, width -> width + inline_run_width(run, nil) end)
            |> Kernel.+(horizontal_box_size(style))

          {:error, _reason} ->
            flex_block_intrinsic_width(children) + horizontal_box_size(style)
        end
    end
  end

  defp resolved_size(style, property, available_size, default) do
    size =
      case Map.get(style, property) do
        {:min, sizes} when is_list(sizes) ->
          sizes
          |> Enum.map(&resolve_size_value(&1, available_size))
          |> Enum.reject(&is_nil/1)
          |> Enum.min(fn -> default end)

        {:percent, ratio} when is_number(available_size) ->
          max(available_size * ratio, 0.0)

        value when is_number(value) ->
          value

        _ ->
          default
      end

    apply_size_constraints(style, property, size, available_size)
  end

  defp resolved_content_size(style, property, available_size, default) do
    case {Map.get(style, :box_sizing, :content_box), property} do
      {:border_box, :width} ->
        box_size = horizontal_box_size(style)
        default = if is_number(default), do: default + box_size, else: default
        size = resolved_size(style, property, available_size, default)
        if is_number(size), do: max(size - box_size, 0.0), else: size

      {:border_box, :height} ->
        box_size = vertical_box_size(style)
        default = if is_number(default), do: default + box_size, else: default
        size = resolved_size(style, property, available_size, default)
        if is_number(size), do: max(size - box_size, 0.0), else: size

      _ ->
        resolved_size(style, property, available_size, default)
    end
  end

  defp resolve_size_value(value, available_size) do
    case value do
      {:min, sizes} when is_list(sizes) ->
        sizes
        |> Enum.map(&resolve_size_value(&1, available_size))
        |> Enum.reject(&is_nil/1)
        |> Enum.min(fn -> nil end)

      {:percent, ratio} when is_number(available_size) ->
        max(available_size * ratio, 0.0)

      value when is_number(value) ->
        value

      _ ->
        nil
    end
  end

  defp apply_size_constraints(style, property, size, available_size) do
    case {property, size} do
      {:width, size} when is_number(size) ->
        size
        |> apply_max_size(style, :max_width, available_size)
        |> apply_min_size(style, :min_width, available_size)

      {:height, size} when is_number(size) ->
        size
        |> apply_max_size(style, :max_height, available_size)
        |> apply_min_size(style, :min_height, available_size)

      _ ->
        size
    end
  end

  defp apply_min_size(size, style, property, available_size) do
    case resolved_constraint_size(style, property, available_size) do
      constraint when is_number(constraint) -> max(size, constraint)
      _ -> size
    end
  end

  defp apply_max_size(size, style, property, available_size) do
    case resolved_constraint_size(style, property, available_size) do
      constraint when is_number(constraint) -> min(size, constraint)
      _ -> size
    end
  end

  defp resolved_constraint_size(style, property, available_size) do
    style
    |> Map.get(property)
    |> resolve_size_value(available_size)
  end

  defp apply_image_size_constraints(
         style,
         width,
         height,
         ratio,
         available_width,
         available_height,
         explicit_dimensions?
       ) do
    case explicit_dimensions? do
      true ->
        {
          apply_size_constraints(style, :width, width, available_width),
          apply_size_constraints(style, :height, height, available_height)
        }

      false ->
        {width, height}
        |> scale_image_down_to_max(style, ratio, available_width, available_height)
        |> scale_image_up_to_min(style, ratio, available_width, available_height)
    end
  end

  defp scale_image_down_to_max({width, height}, style, ratio, available_width, available_height) do
    max_width = resolved_constraint_size(style, :max_width, available_width)
    max_height = resolved_constraint_size(style, :max_height, available_height)

    scale =
      [max_image_scale(width, max_width), max_image_scale(height, max_height)]
      |> Enum.reject(&is_nil/1)
      |> Enum.min(fn -> 1.0 end)
      |> min(1.0)

    {width * scale, HtmlValidator.aspect_ratio_dimension(width * scale, ratio, :height, :layout)}
  end

  defp scale_image_up_to_min({width, height}, style, ratio, available_width, available_height) do
    min_width = resolved_constraint_size(style, :min_width, available_width)
    min_height = resolved_constraint_size(style, :min_height, available_height)

    scale =
      [min_image_scale(width, min_width), min_image_scale(height, min_height)]
      |> Enum.reject(&is_nil/1)
      |> Enum.max(fn -> 1.0 end)
      |> max(1.0)

    {width * scale, HtmlValidator.aspect_ratio_dimension(width * scale, ratio, :height, :layout)}
  end

  defp max_image_scale(size, constraint) do
    case {size, constraint} do
      {size, constraint} when is_number(size) and size > 0 and is_number(constraint) ->
        constraint / size

      _ ->
        nil
    end
  end

  defp min_image_scale(size, constraint) do
    case {size, constraint} do
      {size, constraint} when is_number(size) and size > 0 and is_number(constraint) ->
        constraint / size

      _ ->
        nil
    end
  end

  defp horizontal_box_size(style) do
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))
    padding.left + padding.right + border_widths.left + border_widths.right
  end

  defp vertical_box_size(style) do
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))
    padding.top + padding.bottom + border_widths.top + border_widths.bottom
  end

  defp layout_table(style, children, x, y, width) do
    margin = Map.get(style, :margin, edges(0.0, 0.0, Map.get(style, :margin_after, 0.0), 0.0))
    collapsed? = Map.get(style, :border_collapse, :separate) == :collapse
    box_x = x + margin.left
    box_top = y - margin.top
    available_width = width - margin.left - margin.right

    box_width =
      case Map.get(style, :width) do
        declared when not is_nil(declared) ->
          declared_width = resolved_size(style, :width, width, available_width)

          if not collapsed? and Map.get(style, :box_sizing, :content_box) == :content_box,
            do: declared_width + horizontal_box_size(style),
            else: declared_width

        _ ->
          available_width
      end

    with {:ok, %{rows: rows, columns: columns}} <-
           NativeElixirPdfUtilities.Validators.HtmlTableValidator.form(children) do
      # With no columns there are no cell edges for collapsed table borders.
      # Visible rows still contribute their declared height, but neither table
      # padding nor collapsed borders contribute to this grid's geometry.
      style =
        if collapsed? and table_column_count(rows) == 0 and columns == [],
          do: style |> Map.put(:border_widths, edges(0.0)) |> Map.put(:padding, edges(0.0)),
          else: style

      borders = Map.get(style, :border_widths, edges(0.0))
      padding = Map.get(style, :padding, edges(0.0))

      outer =
        case collapsed? do
          true ->
            cells = rows
            row_count = length(cells)
            column_count = if cells == [], do: 0, else: table_column_count(cells)

            Enum.with_index(cells)
            |> Enum.reduce(borders, fn {row, row_index}, outer ->
              Enum.reduce(row.cells, outer, fn placement, acc ->
                cell_borders = Map.get(placement.cell.style, :border_widths, edges(0.0))

                %{
                  left:
                    if(placement.column == 0,
                      do: max(acc.left, cell_borders.left),
                      else: acc.left
                    ),
                  right:
                    if(placement.column + placement.colspan == column_count,
                      do: max(acc.right, cell_borders.right),
                      else: acc.right
                    ),
                  top: if(row_index == 0, do: max(acc.top, cell_borders.top), else: acc.top),
                  bottom:
                    if(row_index + placement.rowspan == row_count,
                      do: max(acc.bottom, cell_borders.bottom),
                      else: acc.bottom
                    )
                }
              end)
            end)

          false ->
            borders
        end

      insets =
        if collapsed?,
          do: Map.new(outer, fn {edge, value} -> {edge, value / 2} end),
          else: borders

      content_x = box_x + insets.left + padding.left

      content_width =
        max(box_width - insets.left - insets.right - padding.left - padding.right, 0.0)

      table_metadata = break_metadata(style)

      declared_height =
        if collapsed?,
          do: resolved_size(style, :height, nil, nil),
          else: resolved_content_size(style, :height, nil, nil)

      available_height =
        if is_number(declared_height) and collapsed?,
          do: max(declared_height - insets.top - insets.bottom, 0.0),
          else: declared_height

      tracks =
        table_tracks(
          rows,
          columns,
          content_width,
          Map.get(style, :border_collapse, :separate),
          Map.get(style, :border_spacing, {1.5, 1.5}),
          Map.get(style, :table_layout, :auto),
          outer
        )

      box_width =
        max(box_width, tracks.width + insets.left + insets.right + padding.left + padding.right)

      offset =
        case Map.get(style, :margin_inline_auto) do
          %{left: true, right: true} -> max(available_width - box_width, 0.0) / 2
          %{left: true, right: false} -> max(available_width - box_width, 0.0)
          _ -> 0.0
        end

      box_x = box_x + offset
      content_x = content_x + offset
      table_id = {:table, box_x, box_top}

      with {:ok, caption_boxes, table_top} <-
             layout_table_caption(children, box_x, box_top, box_width, table_id, :top),
           rows_top = table_top - padding.top - insets.top,
           {:ok, row_boxes, content_bottom, row_fragments} <-
             layout_table_rows(
               tracks,
               content_x,
               rows_top,
               table_id,
               Map.get(style, :border_collapse, :separate),
               available_height
             ) do
        bottom = content_bottom - padding.bottom - insets.bottom
        box_height = table_top - bottom

        table_boxes =
          table_background_boxes(
            style,
            box_x,
            bottom,
            box_width,
            box_height,
            Map.get(style, :border_collapse, :separate),
            Map.merge(table_metadata, %{flow_id: table_id, table_id: table_id})
          )

        with {:ok, bottom_caption_boxes, next_y} <-
               layout_table_caption(children, box_x, bottom, box_width, table_id, :bottom) do
          case table_boxes.after do
            [] ->
              {:ok, table_boxes.before ++ caption_boxes ++ row_boxes ++ bottom_caption_boxes,
               next_y - margin.bottom}

            outer_border_boxes ->
              # Keep decorations in their row's flow group until pagination has
              # placed every row. Pagination joins the outline pieces per page.
              last_row = length(row_fragments) - 1

              fragments =
                row_fragments
                |> Enum.with_index()
                |> Enum.flat_map(fn {row, index} ->
                  top = if index == 0, do: table_top, else: row.top
                  bottom = if index == last_row, do: bottom, else: row.bottom

                  outlines =
                    Enum.map(outer_border_boxes, fn outline ->
                      outline
                      |> Map.merge(row.metadata)
                      |> Map.put(:type, HtmlValidator.reserve_layout_box(:rect))
                      |> Map.put(:y, bottom)
                      |> Map.put(:height, top - bottom)
                    end)

                  {borders, content} =
                    Enum.split_with(row.boxes, fn box ->
                      Map.get(box, :role) == :table_border and Map.get(box, :table_id) == table_id
                    end)

                  content ++ outlines ++ borders
                end)

              fragments = if row_fragments == [], do: outer_border_boxes, else: fragments

              {:ok, table_boxes.before ++ caption_boxes ++ fragments ++ bottom_caption_boxes,
               next_y - margin.bottom}
          end
        end
      end
    end
  end

  defp table_background_boxes(style, x, y, width, height, border_collapse, metadata) do
    case border_collapse do
      :collapse ->
        background =
          style
          |> Map.put(:border_widths, edges(0.0))
          |> background_box(x, y, width, height)
          |> tag_boxes(metadata)

        border =
          style
          |> Map.put(:background_color, nil)
          |> Map.put(:background_image, nil)
          |> background_box(x, y, width, height)
          |> tag_boxes(metadata)
          |> Enum.map(
            &(&1
              |> Map.put(:role, :table_outline)
              |> Map.put(:table_paint_id, metadata.table_id))
          )

        %{before: background, after: border}

      _ ->
        %{before: style |> background_box(x, y, width, height) |> tag_boxes(metadata), after: []}
    end
  end

  defp layout_table_caption(children, x, y, width, table_id, side) do
    caption =
      Enum.find(children, fn child ->
        match?(%{style: %{display: :table_caption}}, child) and
          Map.get(child.style, :caption_side, :top) == side
      end)

    case caption do
      nil ->
        {:ok, [], y}

      %{style: style} = caption ->
        flow_metadata =
          Map.merge(break_metadata(style), %{
            flow_id: {:table_caption, table_id},
            table_id: table_id
          })

        # Caption contents establish block formatting. Reuse ordinary block box
        # sizing and flow instead of maintaining an inline-only caption path.
        caption = %{caption | style: Map.put(style, :display, :block)}

        with {:ok, boxes, next_y} <- layout_block(caption, x, y, width) do
          {:ok, tag_atomic_boxes(boxes, flow_metadata), next_y}
        end
    end
  end

  defp table_tracks(rows, columns, width, border_collapse, border_spacing, table_layout, outer) do
    case rows == [] or (table_column_count(rows) == 0 and columns == []) do
      true ->
        # Empty visible rows keep their row sizing and flow metadata. A table
        # without columns has no inter-cell spacing or row background area.
        %{rows: rows, widths: [], spacing: {0.0, 0.0}, width: width}

      false ->
        grid_rows = rows
        row_count = length(grid_rows)
        column_count = max(table_column_count(grid_rows), length(columns))

        grid_rows =
          case border_collapse do
            :collapse ->
              shared_edges =
                grid_rows
                |> Enum.with_index()
                |> Enum.reduce(%{}, fn {row, row_index}, shared ->
                  Enum.reduce(row.cells, shared, fn placement, edges ->
                    HtmlValidator.reserve_render_resource(
                      :max_table_grid_work,
                      2 * (placement.colspan + placement.rowspan),
                      :layout
                    )

                    widths = Map.get(placement.cell.style, :border_widths, edges(0.0))

                    horizontal =
                      Enum.flat_map(
                        placement.column..(placement.column + placement.colspan - 1),
                        fn column ->
                          [
                            {{:horizontal, row_index, column}, widths.top, :top},
                            {{:horizontal, row_index + placement.rowspan, column}, widths.bottom,
                             :bottom}
                          ]
                        end
                      )

                    vertical =
                      Enum.flat_map(row_index..(row_index + placement.rowspan - 1), fn row ->
                        [
                          {{:vertical, row, placement.column}, widths.left, :left},
                          {{:vertical, row, placement.column + placement.colspan}, widths.right,
                           :right}
                        ]
                      end)

                    Enum.reduce(horizontal ++ vertical, edges, fn {key, width, side}, acc ->
                      candidate = %{width: width, owner: {row_index, placement.column, side}}

                      Map.update(acc, key, candidate, fn current ->
                        if width >= current.width, do: candidate, else: current
                      end)
                    end)
                  end)
                end)

              Enum.with_index(grid_rows, fn row, row_index ->
                cells =
                  Enum.map(row.cells, fn placement ->
                    cell = placement.cell

                    HtmlValidator.reserve_render_resource(
                      :max_table_grid_work,
                      2 * (placement.colspan + placement.rowspan),
                      :layout
                    )

                    horizontal = placement.column..(placement.column + placement.colspan - 1)
                    vertical = row_index..(row_index + placement.rowspan - 1)

                    borders = %{
                      top:
                        Enum.map(
                          horizontal,
                          &Map.fetch!(shared_edges, {:horizontal, row_index, &1}).width
                        )
                        |> Enum.max(),
                      bottom:
                        Enum.map(
                          horizontal,
                          &Map.fetch!(
                            shared_edges,
                            {:horizontal, row_index + placement.rowspan, &1}
                          ).width
                        )
                        |> Enum.max(),
                      left:
                        Enum.map(
                          vertical,
                          &Map.fetch!(shared_edges, {:vertical, &1, placement.column}).width
                        )
                        |> Enum.max(),
                      right:
                        Enum.map(
                          vertical,
                          &Map.fetch!(
                            shared_edges,
                            {:vertical, &1, placement.column + placement.colspan}
                          ).width
                        )
                        |> Enum.max()
                    }

                    borders = %{
                      left:
                        if(placement.column == 0,
                          do: max(borders.left, outer.left),
                          else: borders.left
                        ),
                      right:
                        if(placement.column + placement.colspan == column_count,
                          do: max(borders.right, outer.right),
                          else: borders.right
                        ),
                      top: if(row_index == 0, do: max(borders.top, outer.top), else: borders.top),
                      bottom:
                        if(row_index + placement.rowspan == row_count,
                          do: max(borders.bottom, outer.bottom),
                          else: borders.bottom
                        )
                    }

                    paint_borders = %{
                      top:
                        Enum.any?(horizontal, fn column ->
                          Map.fetch!(shared_edges, {:horizontal, row_index, column}).owner ==
                            {row_index, placement.column, :top}
                        end),
                      bottom:
                        Enum.any?(horizontal, fn column ->
                          Map.fetch!(
                            shared_edges,
                            {:horizontal, row_index + placement.rowspan, column}
                          ).owner ==
                            {row_index, placement.column, :bottom}
                        end),
                      left:
                        Enum.any?(vertical, fn row ->
                          Map.fetch!(shared_edges, {:vertical, row, placement.column}).owner ==
                            {row_index, placement.column, :left}
                        end),
                      right:
                        Enum.any?(vertical, fn row ->
                          Map.fetch!(
                            shared_edges,
                            {:vertical, row, placement.column + placement.colspan}
                          ).owner ==
                            {row_index, placement.column, :right}
                        end)
                    }

                    %{
                      placement
                      | cell: %{
                          cell
                          | style:
                              cell.style
                              |> Map.put(:_collapsed_borders, borders)
                              |> Map.put(:_collapsed_border_winners, paint_borders)
                        }
                    }
                  end)

                %{row | cells: cells}
              end)

            _ ->
              grid_rows
          end

        {horizontal_spacing, vertical_spacing} =
          case {border_collapse, border_spacing} do
            {:separate, {horizontal, vertical}}
            when is_number(horizontal) and is_number(vertical) ->
              {
                Float.floor(horizontal / @css_pixel_points) * @css_pixel_points,
                Float.floor(vertical / @css_pixel_points) * @css_pixel_points
              }

            _ ->
              {0.0, 0.0}
          end

        column_grid_width = max(width - horizontal_spacing * (column_count + 1), 0.0)

        column_widths =
          table_column_widths(
            grid_rows,
            columns,
            column_count,
            column_grid_width,
            table_layout,
            border_collapse
          )

        %{
          rows: grid_rows,
          widths: column_widths,
          spacing: {horizontal_spacing, vertical_spacing},
          width: Enum.sum(column_widths) + horizontal_spacing * (column_count + 1)
        }
    end
  end

  defp layout_table_rows(tracks, x, y, table_id, border_collapse, available_height) do
    case tracks.rows do
      [] ->
        {:ok, [], y - (available_height || 0.0), []}

      grid_rows ->
        column_widths = tracks.widths
        {horizontal_spacing, vertical_spacing} = tracks.spacing

        with {:ok, row_heights} <-
               table_row_heights(
                 grid_rows,
                 column_widths,
                 horizontal_spacing,
                 vertical_spacing,
                 available_height,
                 border_collapse
               ) do
          {boxes, next_y, fragments} =
            grid_rows
            |> Enum.with_index()
            |> Enum.reduce({[], y - vertical_spacing, []}, fn {row, index},
                                                              {boxes, current_y, fragments} ->
              {:ok, row_boxes, next_y, metadata} =
                layout_table_row(
                  row,
                  table_id,
                  index,
                  x,
                  current_y,
                  column_widths,
                  row_heights,
                  border_collapse,
                  horizontal_spacing,
                  vertical_spacing
                )

              fragment = %{boxes: row_boxes, top: current_y, bottom: next_y, metadata: metadata}
              {Enum.reverse(row_boxes, boxes), next_y - vertical_spacing, [fragment | fragments]}
            end)

          {:ok, Enum.reverse(boxes), next_y, Enum.reverse(fragments)}
        end
    end
  end

  defp layout_table_row(
         %{
           row: %{style: %{display: :table_row} = style},
           section: section,
           group_style: group_style,
           row_group: row_group,
           cells: cells,
           consumed_columns: consumed_columns
         },
         table_id,
         index,
         x,
         y,
         column_widths,
         row_heights,
         border_collapse,
         horizontal_spacing,
         vertical_spacing
       ) do
    row_height = Enum.at(row_heights, index)

    row_metadata =
      table_id
      |> table_row_metadata(section, index)
      |> Map.merge(break_metadata(style))
      |> Map.put(:row_group, {table_id, row_group})
      |> Map.put(:row_group_avoid, Map.get(group_style || %{}, :break_inside) == :avoid)

    group_background_boxes =
      table_part_background_boxes(
        group_style,
        cells,
        x,
        y,
        column_widths,
        row_height,
        horizontal_spacing,
        row_metadata,
        :table_row_group_background
      )

    row_background_boxes =
      table_part_background_boxes(
        style,
        cells,
        x,
        y,
        column_widths,
        row_height,
        horizontal_spacing,
        row_metadata,
        :table_row_background
      )

    boxes =
      Enum.reduce(cells, [], fn
        %{cell: cell, column: column, colspan: colspan, rowspan: rowspan}, boxes ->
          cell_x =
            x + horizontal_spacing * (column + 1) +
              (column_widths |> Enum.take(column) |> Enum.sum())

          cell_width =
            table_cell_width(
              column_widths,
              column,
              colspan,
              horizontal_spacing
            )

          cell_height =
            (row_heights |> Enum.slice(index, rowspan) |> Enum.sum()) +
              vertical_spacing * max(rowspan - 1, 0)

          {:ok, cell_boxes} =
            layout_table_cell(
              cell,
              cell_x,
              y,
              cell_width,
              cell_height,
              row_metadata,
              border_collapse
            )

          boxes ++ cell_boxes
      end)

    cells_for_border = Enum.map(cells, & &1.cell)

    boxes =
      boxes ++
        trailing_collapsed_table_border(
          cells_for_border,
          x,
          y,
          row_height,
          column_widths,
          consumed_columns,
          border_collapse,
          row_metadata
        )

    {background_boxes, content_boxes} =
      Enum.split_with(
        boxes,
        &(Map.get(&1, :role) == :table_cell_background and
            Map.get(&1, :paint_ordered, false) == false)
      )

    {border_boxes, content_boxes} =
      Enum.split_with(
        content_boxes,
        &(Map.get(&1, :role) == :table_border and Map.get(&1, :paint_ordered, false) == false)
      )

    paint_boxes =
      Enum.map(background_boxes ++ border_boxes, &Map.put(&1, :paint_ordered, true))

    boxes =
      attach_html_target(
        style,
        group_background_boxes ++ row_background_boxes ++ paint_boxes ++ content_boxes,
        x,
        y
      )

    boxes = attach_html_target(group_style || %{}, boxes, x, y)
    {:ok, boxes, y - row_height, row_metadata}
  end

  # Row and group layers paint through transparent cells, leaving separated gaps
  # to the table layer. A spanning cell covers its internal column spacing.
  defp table_part_background_boxes(style, cells, x, y, widths, height, spacing, metadata, role) do
    if style == nil or
         (Map.get(style, :background_color) == nil and Map.get(style, :background_image) == nil) do
      []
    else
      row_width = Enum.sum(widths) + spacing * (length(widths) + 1)

      regions =
        cond do
          widths == [] ->
            []

          spacing == 0 ->
            [{0.0, row_width}]

          true ->
            covered =
              Enum.reduce(cells, MapSet.new(), fn cell, covered ->
                Enum.reduce(
                  cell.column..(cell.column + cell.colspan - 1),
                  covered,
                  &MapSet.put(&2, &1)
                )
              end)

            cells = Enum.map(cells, &{&1.column, &1.colspan})

            empty_tracks =
              for index <- 0..(length(widths) - 1),
                  not MapSet.member?(covered, index),
                  do: {index, 1}

            Enum.map(cells ++ empty_tracks, fn {column, span} ->
              left = Enum.sum(Enum.take(widths, column)) + spacing * (column + 1)
              width = Enum.sum(Enum.slice(widths, column, span)) + spacing * (span - 1)
              {left, width}
            end)
        end

      regions
      |> Enum.reject(fn {_left, width} -> width <= 0 end)
      |> Enum.flat_map(fn {left, width} ->
        style
        |> Map.put(:border_widths, edges(0.0))
        |> background_box(x, y - height, row_width, height)
        |> Enum.map(
          &Map.put(&1, :clip, %{x: x + left, y: y - height, width: width, height: height})
        )
        |> tag_boxes(metadata)
        |> Enum.map(&Map.put(&1, :role, role))
      end)
    end
  end

  defp layout_table_cell(
         %{style: %{display: :table_cell} = style, children: children},
         x,
         y,
         width,
         height,
         row_metadata,
         border_collapse
       ) do
    padding = Map.get(style, :padding, edges(0.0))

    border_widths =
      case border_collapse do
        :collapse ->
          Map.get(style, :_collapsed_borders, Map.get(style, :border_widths, edges(0.0)))
          |> Map.new(fn {edge, value} -> {edge, value / 2} end)

        _ ->
          Map.get(style, :border_widths, edges(0.0))
      end

    content_x = x + border_widths.left + padding.left

    content_width =
      width - border_widths.left - padding.left - padding.right - border_widths.right

    content_top = y - border_widths.top - padding.top

    content_area_height =
      max(
        height - border_widths.top - padding.top - padding.bottom - border_widths.bottom,
        0.0
      )

    with {:ok, content_boxes, content_bottom} <-
           layout_table_cell_content(
             children,
             style,
             content_x,
             content_top,
             content_width,
             content_area_height
           ) do
      content_height = content_top - content_bottom

      vertical_offset = table_cell_vertical_offset(style, content_area_height, content_height)

      content_boxes =
        Enum.map(content_boxes, fn box ->
          Map.update!(box, :y, &(&1 - vertical_offset))
        end)

      cell_box =
        table_cell_background_box(style, x, y - height, width, height, border_collapse)
        |> tag_boxes(row_metadata)
        |> Enum.map(fn box ->
          box
          |> Map.put(:role, :table_cell_background)
        end)

      border_box =
        table_cell_border_box(
          style,
          x,
          y - height,
          width,
          height,
          border_collapse
        )
        |> tag_boxes(row_metadata)
        |> Enum.map(fn box ->
          box
          |> Map.put(:role, :table_border)
          |> Map.put(:table_paint_id, row_metadata.table_id)
        end)

      boxes = cell_box ++ border_box ++ tag_atomic_boxes(content_boxes, row_metadata)

      boxes =
        case Map.fetch(style, :_table_cell) do
          {:ok, semantics} -> Enum.map(boxes, &Map.put_new(&1, :table_cell, semantics))
          :error -> boxes
        end

      {:ok, attach_html_target(style, boxes, x, y)}
    end
  end

  defp table_cell_background_box(style, x, y, width, height, border_collapse) do
    case border_collapse do
      :collapse ->
        style
        |> Map.put(:border_widths, edges(0.0))
        |> background_box(x, y, width, height)
        |> Enum.map(fn box ->
          box
          |> Map.put(:stroke_width, 0.0)
          |> Map.put(:border_widths, edges(0.0))
        end)

      _ ->
        background_box(style, x, y, width, height)
    end
  end

  defp table_cell_border_box(
         style,
         x,
         y,
         width,
         height,
         border_collapse
       ) do
    case border_collapse do
      :collapse ->
        border_widths = Map.get(style, :border_widths, edges(0.0))
        resolved_widths = Map.get(style, :_collapsed_borders, border_widths)
        winners = Map.get(style, :_collapsed_border_winners, edges(true))
        border_styles = Map.get(style, :border_styles, edges(:solid))

        border_colors =
          Map.get(style, :border_colors, edges(Map.get(style, :border_color, {0, 0, 0})))

        collapsed_widths = %{
          top:
            if(winners.top and border_widths.top >= resolved_widths.top,
              do: border_widths.top,
              else: 0.0
            ),
          right:
            if(winners.right and border_widths.right >= resolved_widths.right,
              do: border_widths.right,
              else: 0.0
            ),
          bottom:
            if(winners.bottom and border_widths.bottom >= resolved_widths.bottom,
              do: border_widths.bottom,
              else: 0.0
            ),
          left:
            if(winners.left and border_widths.left >= resolved_widths.left,
              do: border_widths.left,
              else: 0.0
            )
        }

        stroke_width = collapsed_widths |> Map.values() |> Enum.max()

        case visible_border?(collapsed_widths, border_styles, border_colors) do
          true ->
            [
              %{
                type: HtmlValidator.reserve_layout_box(:rect),
                x: x,
                y: y,
                width: width,
                height: height,
                fill_color: nil,
                stroke_color: Map.get(style, :border_color, {0, 0, 0}),
                stroke_width: stroke_width,
                border_widths: collapsed_widths,
                fragment_bottom_border_width: border_widths.bottom,
                border_colors: border_colors,
                border_styles: border_styles,
                border_radius: 0.0
              }
            ]

          false ->
            []
        end

      _ ->
        []
    end
  end

  defp trailing_collapsed_table_border(
         cells,
         x,
         y,
         row_height,
         column_widths,
         consumed_columns,
         border_collapse,
         row_metadata
       ) do
    case {border_collapse, length(cells) > 1 and consumed_columns < length(column_widths)} do
      {:collapse, true} ->
        style =
          cells
          |> List.last()
          |> Map.fetch!(:style)

        cell_x = x + (column_widths |> Enum.take(consumed_columns) |> Enum.sum())

        cell_width =
          column_widths
          |> Enum.drop(consumed_columns)
          |> Enum.sum()

        style
        |> table_cell_border_box(
          cell_x,
          y - row_height,
          cell_width,
          row_height,
          :collapse
        )
        |> tag_boxes(row_metadata)
        |> Enum.map(fn box ->
          box
          |> Map.put(:role, :table_border)
          |> Map.put(:table_paint_id, row_metadata.table_id)
        end)

      _ ->
        []
    end
  end

  defp table_cell_vertical_offset(style, content_area_height, content_height) do
    extra_space = max(content_area_height - content_height, 0.0)

    case Map.get(style, :vertical_align, :middle) do
      :bottom -> extra_space
      :middle -> extra_space / 2
      _ -> 0.0
    end
  end

  defp table_column_count(rows) do
    rows
    |> Enum.flat_map(& &1.cells)
    |> Enum.map(&(&1.column + &1.colspan))
    |> Enum.max(fn -> 0 end)
  end

  defp table_column_widths(
         rows,
         columns,
         column_count,
         table_width,
         table_layout,
         border_collapse
       ) do
    column_preferred =
      0..(column_count - 1)
      |> Enum.map(fn index ->
        columns
        |> Enum.at(index, %{})
        |> Map.get(:width)
        |> table_column_preferred_width(table_width)
      end)

    width_rows = if table_layout == :fixed, do: Enum.take(rows, 1), else: rows

    percentage_total =
      Enum.reduce(width_rows, 0.0, fn %{cells: cells}, total ->
        Enum.reduce(cells, total, fn
          %{cell: %{style: %{width: {:percent, ratio}}}}, acc ->
            acc + ratio

          _placement, acc ->
            acc
        end)
      end)

    percentage_widths_fill_table? = percentage_total >= 0.9999

    explicitly_sized_columns =
      columns
      |> Enum.with_index()
      |> Enum.reduce(MapSet.new(), fn
        {%{width: {:percent, ratio}}, index}, indexes when is_number(ratio) ->
          MapSet.put(indexes, index)

        {%{width: width}, index}, indexes when is_number(width) ->
          MapSet.put(indexes, index)

        {_column, _index}, indexes ->
          indexes
      end)
      |> then(fn indexes ->
        Enum.reduce(width_rows, indexes, fn %{cells: cells}, indexes ->
          Enum.reduce(cells, indexes, fn
            %{cell: %{style: %{width: {:percent, ratio}}}, column: index, colspan: 1}, acc
            when is_number(ratio) ->
              MapSet.put(acc, index)

            %{cell: %{style: %{width: width}}, column: index, colspan: 1}, acc
            when is_number(width) ->
              MapSet.put(acc, index)

            _placement, acc ->
              acc
          end)
        end)
      end)

    preferred =
      Enum.reduce(width_rows, column_preferred, fn %{cells: cells}, widths ->
        Enum.reduce(cells, widths, fn
          %{cell: cell, column: index, colspan: colspan}, acc ->
            preferred_width =
              case {table_layout, colspan, Map.get(cell.style, :width)} do
                {:auto, colspan, width} when colspan > 1 and width in [nil, :auto] ->
                  nil

                {:auto, 1, width} when width in [nil, :auto] ->
                  case MapSet.member?(explicitly_sized_columns, index) do
                    true ->
                      nil

                    false ->
                      table_cell_preferred_width(
                        cell,
                        table_width,
                        percentage_widths_fill_table?,
                        table_layout,
                        border_collapse
                      )
                  end

                _ ->
                  table_cell_preferred_width(
                    cell,
                    table_width,
                    percentage_widths_fill_table?,
                    table_layout,
                    border_collapse
                  )
              end

            case preferred_width do
              nil ->
                acc

              preferred_width ->
                share = preferred_width / colspan

                Enum.reduce(index..(index + colspan - 1), acc, fn column, column_acc ->
                  List.update_at(column_acc, column, fn
                    nil -> share
                    width when table_layout == :fixed -> width
                    width -> max(width, share)
                  end)
                end)
            end
        end)
      end)

    minimum =
      case table_layout do
        :fixed -> List.duplicate(0.0, column_count)
        _ -> table_minimum_column_widths(rows, column_count)
      end

    # An automatic table cannot squeeze its columns below min-content.
    # CSS width is a lower bound when indivisible cell contents require more.
    table_width = max(table_width, Enum.sum(minimum))

    fixed_total = preferred |> Enum.reject(&is_nil/1) |> Enum.sum()
    flexible_count = Enum.count(preferred, &is_nil/1)

    widths =
      cond do
        table_layout == :fixed and fixed_total > table_width and fixed_total > 0 ->
          Enum.map(preferred, fn
            nil -> 0.0
            width -> width / fixed_total * table_width
          end)

        table_layout == :fixed and flexible_count > 0 ->
          flexible_width = max((table_width - fixed_total) / flexible_count, 0.0)
          Enum.map(preferred, fn width -> width || flexible_width end)

        table_layout == :fixed and column_count > 0 ->
          extra_width = max((table_width - fixed_total) / column_count, 0.0)
          Enum.map(preferred, &(&1 + extra_width))

        fixed_total > table_width and fixed_total > 0 and percentage_total <= 0 ->
          Enum.map(preferred, &(&1 || 0.0))

        fixed_total > table_width and fixed_total > 0 ->
          preferred
          |> Enum.map(&(&1 || 0.0))
          |> shrink_largest_columns(List.duplicate(0.0, column_count), fixed_total - table_width)

        flexible_count > 0 ->
          fixed_widths =
            preferred
            |> Enum.with_index()
            |> Enum.map(fn
              {nil, _index} -> nil
              {width, index} -> max(width, Enum.at(minimum, index))
            end)

          fixed_total = fixed_widths |> Enum.reject(&is_nil/1) |> Enum.sum()
          flexible_width = max((table_width - fixed_total) / flexible_count, 0.0)

          fixed_widths
          |> Enum.with_index()
          |> Enum.map(fn
            {nil, index} -> max(flexible_width, Enum.at(minimum, index))
            {width, _index} -> width
          end)

        fixed_total == 0 ->
          List.duplicate(table_width / column_count, column_count)

        true ->
          explicit_count = MapSet.size(explicitly_sized_columns)
          automatic_count = column_count - explicit_count

          if automatic_count > 0 and explicit_count > 0 do
            automatic_total =
              preferred
              |> Enum.with_index()
              |> Enum.reduce(0.0, fn {width, index}, total ->
                if MapSet.member?(explicitly_sized_columns, index),
                  do: total,
                  else: total + (width || 0.0)
              end)

            extra = table_width - fixed_total

            Enum.with_index(preferred, fn width, index ->
              width = width || 0.0

              if MapSet.member?(explicitly_sized_columns, index) do
                width
              else
                share =
                  if automatic_total > 0, do: width / automatic_total, else: 1 / automatic_count

                width + extra * share
              end
            end)
          else
            Enum.map(preferred, &((&1 || 0.0) / fixed_total * table_width))
          end
      end

    if table_layout == :fixed do
      widths
    else
      widths
      |> Enum.zip_with(minimum, &max/2)
      |> shrink_columns_to_width(minimum, table_width)
    end
  end

  defp table_column_preferred_width(width, table_width) do
    case width do
      {:percent, ratio} when is_number(ratio) -> table_width * ratio
      width when is_number(width) and width > 0 -> min(width, table_width)
      _ -> nil
    end
  end

  defp table_minimum_column_widths(rows, column_count) do
    Enum.reduce(rows, List.duplicate(0.0, column_count), fn %{cells: cells}, widths ->
      Enum.reduce(cells, widths, fn
        %{cell: cell, column: index, colspan: colspan}, acc ->
          case {colspan, table_cell_minimum_width(cell)} do
            {1, min_width} when min_width > 0 ->
              List.update_at(acc, index, &max(&1, min_width))

            _ ->
              acc
          end
      end)
    end)
  end

  defp table_cell_minimum_width(%{style: style, children: children}) do
    case inline_runs(children, style) do
      {:ok, runs} ->
        padding = Map.get(style, :padding, edges(0.0))

        border_widths =
          case Map.get(style, :_collapsed_borders) do
            nil -> Map.get(style, :border_widths, edges(0.0))
            borders -> Map.new(borders, fn {edge, value} -> {edge, value / 2} end)
          end

        content_width = inline_minimum_content_width(runs)

        content_width + padding.left + padding.right + border_widths.left +
          border_widths.right

      {:error, _reason} ->
        0.0
    end
  end

  # Min-content measures complete unbroken words, including styled fragments.
  defp inline_minimum_content_width(runs) do
    case runs do
      [%{text: text, style: style} = run] when not is_map_key(run, :type) ->
        text
        |> inline_wrap_tokens(style, true)
        |> Enum.map(&text_width(trim_inline_whitespace(&1, true), style))
        |> Enum.max(fn -> 0.0 end)

      _ ->
        tokens =
          Enum.flat_map(runs, fn run ->
            case run do
              %{type: :inline_positioning_marker} ->
                []

              %{type: :soft_break} ->
                [:break]

              %{type: type} when type in [:inline_block, :inline_image] ->
                [run]

              _ ->
                key = System.unique_integer([:positive])

                style =
                  case run.style do
                    %{overflow_wrap: :anywhere} -> Map.put(run.style, :line_break, :anywhere)
                    %{word_break: :break_word} -> Map.put(run.style, :line_break, :anywhere)
                    _ -> run.style
                  end

                run.text
                |> String.split("\n", trim: false)
                |> Enum.intersperse(:break)
                |> Enum.flat_map(fn
                  :break ->
                    [:break]

                  text ->
                    text
                    |> inline_wrap_tokens(style, true)
                    |> Enum.map(&%{text: &1, style: style, merge_key: key})
                end)
            end
          end)

        {maximum, width, _chunks, _previous} =
          Enum.reduce(tokens, {0.0, 0.0, [], nil}, fn
            :break, {maximum, width, _chunks, _previous} ->
              {max(maximum, width), 0.0, [], nil}

            %{type: type} = run, {maximum, width, chunks, _previous}
            when type in [:inline_block, :inline_image] ->
              atomic_width = inline_run_width(run, nil)

              if run.style.white_space in [:nowrap, :pre] do
                {maximum, width + atomic_width, [run | chunks], run}
              else
                {max(maximum, width), atomic_width, [run], run}
              end

            run, {maximum, width, chunks, previous} ->
              line =
                if Map.get(run.style, :line_break) == :anywhere or
                     (previous != nil and Map.get(previous.style, :line_break) == :anywhere),
                   do: Enum.reverse(chunks),
                   else: []

              token_text =
                if run.style.white_space in [:nowrap, :pre],
                  do: run.text,
                  else: trim_inline_whitespace(run.text, true)

              token_width = text_width(token_text, run.style)

              if previous != nil and inline_break_boundary?(previous, run, line) do
                {max(maximum, width), token_width, [run], run}
              else
                {maximum, width + token_width, [run | chunks], run}
              end
          end)

        max(maximum, width)
    end
  end

  defp shrink_columns_to_width(widths, minimum, table_width) do
    total_width = Enum.sum(widths)

    case total_width > table_width and total_width > 0 do
      true ->
        shrinkable =
          widths
          |> Enum.zip(minimum)
          |> Enum.map(fn {width, min_width} -> max(width - min_width, 0.0) end)

        shrinkable_total = Enum.sum(shrinkable)
        overflow = total_width - table_width

        # The caller floors widths at minimum and reserves their sum, so
        # every overflowing column has enough capacity in this interpolation.
        ratio = min(overflow / shrinkable_total, 1.0)

        Enum.zip_with(widths, shrinkable, fn width, capacity ->
          width - ratio * capacity
        end)

      false ->
        widths
    end
  end

  defp shrink_largest_columns(widths, minimum, overflow) do
    shrink_order =
      widths
      |> Enum.zip(minimum)
      |> Enum.with_index()
      |> Enum.map(fn {{width, min_width}, index} ->
        {index, max(width - min_width, 0.0)}
      end)
      |> Enum.sort_by(fn {_index, shrinkable_width} -> shrinkable_width end, :desc)

    {shrunk_widths, _remaining_overflow} =
      Enum.reduce(shrink_order, {widths, overflow}, fn {index, shrinkable_width},
                                                       {acc, remaining_overflow} ->
        reduction = min(shrinkable_width, remaining_overflow)

        acc =
          List.update_at(acc, index, fn width ->
            width - reduction
          end)

        {acc, remaining_overflow - reduction}
      end)

    shrunk_widths
  end

  defp table_cell_preferred_width(
         cell,
         table_width,
         percentage_widths_fill_table?,
         table_layout,
         border_collapse
       ) do
    case cell do
      %{style: %{width: {:percent, ratio}} = style} when is_number(ratio) ->
        table_width * ratio +
          if(percentage_widths_fill_table?,
            do: 0.0,
            else: table_cell_horizontal_box_size(style, border_collapse)
          )

      %{style: %{width: width} = style} when is_number(width) and width > 0 ->
        width = width + horizontal_box_size(style)
        if table_layout == :fixed, do: min(width, table_width), else: width

      %{style: %{display: :table_cell} = style, children: children}
      when table_layout == :auto and is_list(children) ->
        case inline_runs(children, style) do
          {:ok, runs} ->
            runs
            |> Enum.reduce(0.0, fn run, width -> width + inline_run_width(run, nil) end)
            |> Kernel.+(table_cell_horizontal_box_size(style, border_collapse))

          {:error, _reason} ->
            visible_children =
              Enum.reject(children, fn
                %{type: :text, text: text} -> String.trim(text) == ""
                _ -> false
              end)

            case visible_children do
              [%{style: %{display: display} = flex_style, children: flex_children}]
              when display in [:flex, :inline_flex] and is_list(flex_children) ->
                child_widths =
                  Enum.map(flex_children, fn child ->
                    intrinsic_width = flex_child_intrinsic_width(child)
                    child_style = Map.fetch!(child, :style)
                    box_size = horizontal_box_size(child_style)

                    flex_basis(
                      child_style,
                      :width,
                      nil,
                      max(intrinsic_width - box_size, 0.0)
                    ) + box_size
                  end)

                flex_width =
                  case flex_main_axis(flex_style) do
                    :row ->
                      Enum.sum(child_widths) +
                        flex_main_gap(flex_style) * max(length(flex_children) - 1, 0)

                    :column ->
                      Enum.max(child_widths, fn -> 0.0 end)
                  end

                min(
                  resolved_content_size(flex_style, :width, nil, flex_width) +
                    horizontal_box_size(flex_style) +
                    table_cell_horizontal_box_size(style, border_collapse),
                  table_width
                )

              _ ->
                nil
            end
        end

      _ ->
        nil
    end
  end

  defp table_cell_horizontal_box_size(style, border_collapse) do
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))

    horizontal_border_size =
      case border_collapse do
        :collapse -> max(border_widths.left, border_widths.right)
        _ -> border_widths.left + border_widths.right
      end

    padding.left + padding.right + horizontal_border_size
  end

  defp table_cell_width(
         column_widths,
         index,
         colspan,
         horizontal_spacing
       ) do
    (column_widths |> Enum.slice(index, colspan) |> Enum.sum()) +
      horizontal_spacing * max(colspan - 1, 0)
  end

  defp table_row_heights(
         rows,
         column_widths,
         horizontal_spacing,
         vertical_spacing,
         available_height,
         border_collapse
       ) do
    initial_heights =
      Enum.reduce_while(rows, {:ok, []}, fn
        %{row: %{style: style}, cells: cells}, {:ok, row_heights} ->
          result =
            cells
            |> Enum.filter(&(&1.rowspan == 1))
            |> Enum.reduce_while({:ok, []}, fn placement, {:ok, cell_heights} ->
              width =
                table_cell_width(
                  column_widths,
                  placement.column,
                  placement.colspan,
                  horizontal_spacing
                )

              case table_cell_height(placement.cell, width, border_collapse) do
                {:ok, height} -> {:cont, {:ok, [height | cell_heights]}}
                {:error, reason} -> {:halt, {:error, reason}}
              end
            end)

          case result do
            {:ok, cell_heights} ->
              intrinsic_height = cell_heights |> Enum.reverse() |> Enum.max(fn -> 0.0 end)

              {:cont,
               {:ok, [max(intrinsic_height, table_row_declared_height(style)) | row_heights]}}

            {:error, reason} ->
              {:halt, {:error, reason}}
          end
      end)

    adjusted_heights =
      case initial_heights do
        {:ok, heights} ->
          heights = Enum.reverse(heights)

          rows
          |> Enum.with_index()
          |> Enum.reduce_while({:ok, heights}, fn {%{cells: cells}, row_index},
                                                  {:ok, row_heights} ->
            result =
              cells
              |> Enum.filter(&(&1.rowspan > 1))
              |> Enum.reduce_while({:ok, row_heights}, fn placement, {:ok, acc} ->
                width =
                  table_cell_width(
                    column_widths,
                    placement.column,
                    placement.colspan,
                    horizontal_spacing
                  )

                case table_cell_height(placement.cell, width, border_collapse) do
                  {:ok, required_height} ->
                    current_height =
                      acc
                      |> Enum.slice(row_index, placement.rowspan)
                      |> Enum.sum()

                    current_height =
                      current_height + vertical_spacing * max(placement.rowspan - 1, 0)

                    extra_height = max(required_height - current_height, 0.0)
                    share = extra_height / placement.rowspan

                    adjusted =
                      acc
                      |> Enum.with_index()
                      |> Enum.map(fn
                        {height, index}
                        when index >= row_index and index < row_index + placement.rowspan ->
                          height + share

                        {height, _index} ->
                          height
                      end)

                    {:cont, {:ok, adjusted}}

                  {:error, reason} ->
                    {:halt, {:error, reason}}
                end
              end)

            case result do
              {:ok, adjusted} -> {:cont, {:ok, adjusted}}
              {:error, reason} -> {:halt, {:error, reason}}
            end
          end)

        {:error, reason} ->
          {:error, reason}
      end

    case adjusted_heights do
      {:ok, heights} ->
        heights =
          stretch_table_row_group_heights(rows, heights, available_height, vertical_spacing)

        {:ok, stretch_table_row_heights(rows, heights, available_height, vertical_spacing)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp stretch_table_row_group_heights(rows, heights, available_height, vertical_spacing) do
    HtmlValidator.reserve_render_resource(:max_table_grid_work, length(rows), :layout)

    adjusted =
      Enum.zip(rows, heights)
      |> Enum.with_index()
      |> Enum.group_by(fn {{row, _height}, _index} -> row.row_group end)
      |> Enum.flat_map(fn {_group, entries} ->
        {{first, _height}, _index} = hd(entries)
        group_style = first.group_style || %{}
        minimum = resolved_size(group_style, :height, available_height, nil)
        group_rows = Enum.map(entries, fn {{row, _height}, _index} -> row end)
        group_heights = Enum.map(entries, fn {{_row, height}, _index} -> height end)

        # A group's height excludes spacing before its first and after its last row.
        minimum = if is_number(minimum), do: minimum + 2 * vertical_spacing, else: nil

        stretched =
          stretch_table_row_heights(group_rows, group_heights, minimum, vertical_spacing)

        Enum.zip(entries, stretched)
        |> Enum.map(fn {{{_row, _height}, index}, height} -> {index, height} end)
      end)
      |> Map.new()

    Enum.with_index(heights, fn _height, index -> Map.fetch!(adjusted, index) end)
  end

  defp stretch_table_row_heights(rows, heights, available_height, vertical_spacing) do
    case available_height do
      available_height when is_number(available_height) and heights != [] ->
        row_space =
          max(available_height - vertical_spacing * (length(heights) + 1), 0.0)

        extra_height = max(row_space - Enum.sum(heights), 0.0)

        percentage_rows =
          rows
          |> Enum.with_index()
          |> Enum.flat_map(fn
            {%{row: %{style: %{height: {:percent, ratio}}}}, index}
            when is_number(ratio) and ratio > 0 ->
              [{index, ratio}]

            {_row, _index} ->
              []
          end)

        case {extra_height, percentage_rows} do
          {extra_height, _percentage_rows} when extra_height <= 0 ->
            heights

          {extra_height, []} ->
            share = extra_height / length(heights)
            Enum.map(heights, &(&1 + share))

          {extra_height, percentage_rows} ->
            ratio_total =
              Enum.reduce(percentage_rows, 0.0, fn {_index, ratio}, acc -> acc + ratio end)

            Enum.with_index(heights, fn height, index ->
              case List.keyfind(percentage_rows, index, 0) do
                {_index, ratio} -> height + extra_height * ratio / ratio_total
                nil -> height
              end
            end)
        end

      _ ->
        heights
    end
  end

  defp table_row_declared_height(style) do
    resolved_content_size(style, :height, nil, nil) || 0.0
  end

  defp table_cell_height(%{style: style, children: children}, width, border_collapse) do
    padding = Map.get(style, :padding, edges(0.0))

    border_widths =
      case border_collapse do
        :collapse ->
          Map.get(style, :_collapsed_borders, Map.get(style, :border_widths, edges(0.0)))
          |> Map.new(fn {edge, value} -> {edge, value / 2} end)

        _ ->
          Map.get(style, :border_widths, edges(0.0))
      end

    content_width =
      width - border_widths.left - padding.left - padding.right - border_widths.right

    with {:ok, _boxes, content_bottom} <-
           layout_table_cell_content(children, style, 0.0, 0.0, content_width, nil) do
      content_height = 0.0 - content_bottom

      border_height =
        case border_collapse do
          :collapse -> border_widths.top + border_widths.bottom
          _ -> border_widths.top + border_widths.bottom
        end

      content_box_height = padding.top + content_height + padding.bottom + border_height

      {:ok, max(content_box_height, table_cell_declared_height(style, border_collapse))}
    end
  end

  defp table_cell_declared_height(style, border_collapse) do
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))

    box_inset =
      case border_collapse do
        :collapse ->
          padding.top + padding.bottom + max(border_widths.top, border_widths.bottom)

        _ ->
          vertical_box_size(style)
      end

    [
      resolved_content_size(style, :height, nil, nil),
      resolved_content_size(style, :min_height, nil, nil)
    ]
    |> Enum.filter(&is_number/1)
    |> Enum.map(&(&1 + box_inset))
    |> Enum.max(fn -> 0.0 end)
  end

  defp layout_table_cell_content(children, style, x, y, width, available_height) do
    case inline_runs(children, style) do
      {:ok, runs} ->
        runs = Enum.map(runs, &Map.put(&1, :_table_text, true))
        {boxes, content_height} = inline_text_boxes(runs, style, x, y, width, %{})
        {:ok, boxes, y - content_height}

      {:error, _reason} ->
        layout_table_cell_blocks(children, style, x, y, width, available_height)
    end
  end

  defp layout_table_cell_blocks(children, style, x, y, width, available_height) do
    result =
      children
      |> group_inline_flow_children()
      |> Enum.reduce_while({:ok, [], y, nil}, fn child,
                                                 {:ok, boxes, current_y, previous_margin_bottom} ->
        {margin_top, margin_bottom} = flow_child_vertical_margins(child, style)
        collapsed_margin = collapsed_sibling_margin(previous_margin_bottom, margin_top)

        remaining_height =
          case available_height do
            available_height when is_number(available_height) ->
              max(available_height - (y - current_y), 0.0)

            _ ->
              nil
          end

        case layout_table_cell_block(
               child,
               style,
               x,
               current_y + collapsed_margin,
               width,
               remaining_height
             ) do
          {:ok, child_boxes, next_y} ->
            next_margin_bottom =
              following_sibling_margin(
                child_boxes,
                previous_margin_bottom,
                margin_top,
                margin_bottom
              )

            {:cont, {:ok, Enum.reverse(child_boxes, boxes), next_y, next_margin_bottom}}

          {:error, reason} ->
            {:halt, {:error, reason}}
        end
      end)

    case result do
      {:ok, boxes, next_y, _margin_bottom} -> {:ok, Enum.reverse(boxes), next_y}
      {:error, reason} -> {:error, reason}
    end
  end

  defp layout_table_cell_block(child, style, x, y, width, available_height) do
    case child do
      %{type: :inline_flow, children: children} ->
        layout_inline_flow(children, style, x, y, width)

      _ ->
        child_style = Map.fetch!(child, :style)

        child =
          case resolved_size(child_style, :height, available_height, nil) do
            height when is_number(height) ->
              Map.put(child, :style, Map.put(child_style, :height, height))

            _ ->
              child
          end

        layout_block(child, x, y, width)
    end
  end

  defp table_row_metadata(table_id, section, index) do
    metadata = %{
      flow_id: {:table_row, table_id, index},
      table_id: table_id,
      table_section: section
    }

    case section do
      :head -> Map.put(metadata, :repeat_table_header, true)
      _ -> metadata
    end
  end

  defp aligned_text_x(runs, style, x, width) do
    text_width =
      Enum.reduce(runs, 0.0, fn run, acc ->
        acc + inline_run_width(run, width)
      end)

    case Map.get(style, :text_align, :left) do
      :center -> x + max((width - text_width) / 2, 0.0)
      :right -> x + max(width - text_width, 0.0)
      _ -> x
    end
  end

  defp layout_list(style, children, x, y, width) do
    margin = Map.get(style, :margin, edges(0.0, 0.0, Map.get(style, :margin_after, 0.0), 0.0))
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))
    box_x = x + margin.left
    box_top = y - margin.top
    available_box_width = width - margin.left - margin.right

    content_width =
      resolved_content_size(
        style,
        :width,
        width,
        available_box_width - horizontal_box_size(style)
      )

    box_width = content_width + horizontal_box_size(style)
    content_x = box_x + border_widths.left + padding.left
    content_top = box_top - border_widths.top - padding.top

    case layout_list_items(
           children,
           Map.fetch!(style, :list_marker_type),
           content_x,
           content_top,
           content_width
         ) do
      {:ok, item_boxes, content_bottom} ->
        content_height = content_top - content_bottom

        box_height =
          border_widths.top + padding.top + content_height + padding.bottom + border_widths.bottom

        background_box = background_box(style, box_x, box_top - box_height, box_width, box_height)
        next_y = box_top - box_height - margin.bottom
        {:ok, background_box ++ item_boxes, next_y}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp layout_list_items(children, marker_type, x, y, width) do
    result =
      children
      |> Enum.with_index(1)
      |> Enum.reduce_while({:ok, [], y}, fn {child, index}, {:ok, boxes, current_y} ->
        case layout_list_item(child, marker_type, index, x, current_y, width) do
          {:ok, item_boxes, next_y} -> {:cont, {:ok, Enum.reverse(item_boxes, boxes), next_y}}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)

    case result do
      {:ok, boxes, next_y} -> {:ok, Enum.reverse(boxes), next_y}
      {:error, reason} -> {:error, reason}
    end
  end

  defp layout_list_item(item, marker_type, index, x, y, width) do
    case item do
      %{type: :element, style: %{display: :list_item} = style, children: children} ->
        markers = Map.get(style, :_list_marker, [])
        content = children
        marker_style = text_style(style)

        markers =
          if markers == [] do
            text = CounterStyle.marker(Map.get(style, :list_marker_type, marker_type), index)
            if text == "", do: [], else: [%{type: :text, text: text, style: marker_style}]
          else
            markers
          end

        inside? = Map.get(style, :list_style_position, :outside) == :inside
        gap = if markers == [], do: [], else: [%{type: :text, text: " ", style: marker_style}]

        marker_advance =
          case {Map.get(style, :list_marker_type, marker_type), inside?} do
            {kind, true} when kind in [:disclosure_open, :disclosure_closed] ->
              # Chromium's symbolic disclosure marker occupies .66em followed
              # by a .4em inside margin, quantized in 1/64 CSS pixel units.
              marker_width = Float.floor(style.font_size / 0.75 * 0.66 * 64) / 64 * 0.75
              gap_width = Float.floor(style.font_size / 0.75 * 0.4 * 64) / 64 * 0.75
              marker_width + gap_width

            {kind, true} when kind in [:disc, :circle, :square] and item.tag == "summary" ->
              case text_font_face(style) do
                %{ascent: ascent, units_per_em: units} ->
                  ascent_pixels = round(style.font_size / 0.75 * ascent / units)
                  marker_pixels = div(div(ascent_pixels * 2, 3) + 1, 2) + 2

                  marker_pixels * 0.75 - 0.75 +
                    Float.floor(style.font_size / 0.75 * 64) / 64 * 0.75

                _ ->
                  nil
              end

            _ ->
              nil
          end

        {markers, gap} =
          if is_number(marker_advance) do
            markers =
              Enum.map(markers, fn marker ->
                typography = Map.put(marker.style, :letter_spacing, 0.0)
                text = marker.text <> " "
                spacing = marker_advance - text_width(text, typography)
                %{marker | text: text, style: Map.put(typography, :letter_spacing, spacing)}
              end)

            {markers, []}
          else
            {markers, gap}
          end

        content = if inside?, do: markers ++ gap ++ content, else: content

        block_style = Map.put(style, :display, :block)

        block_style =
          if Map.get(style, :height) in [nil, :auto] do
            Map.put(
              block_style,
              :min_height,
              max(Map.get(style, :min_height, 0) || 0, style.line_height)
            )
          else
            block_style
          end

        with {:ok, boxes, next_y} <-
               do_layout_block(%{item | style: block_style, children: content}, x, y, width) do
          marker_boxes =
            if inside? or markers == [] do
              []
            else
              margin = Map.get(style, :margin, edges(0.0))
              padding = Map.get(style, :padding, edges(0.0))
              borders = Map.get(style, :border_widths, edges(0.0))
              content_x = x + margin.left + borders.left + padding.left
              content_top = y - margin.top - borders.top - padding.top
              first_text = Enum.find(boxes, &(&1.type == :text))

              baseline =
                if first_text, do: first_text.y, else: content_top - text_baseline_depth(style)

              metadata =
                if first_text,
                  do:
                    Map.take(first_text, [
                      :flow_id,
                      :fragment_id,
                      :fragment_lines,
                      :break_before,
                      :break_after,
                      :break_inside
                    ]),
                  else: Map.put(break_metadata(style), :flow_id, {:list_item, x, y, index})

              total_width = Enum.reduce(markers, 0.0, &(&2 + text_width(&1.text, &1.style)))
              marker_x = content_x - total_width - text_width(" ", marker_style)

              {marker_boxes, _} =
                Enum.map_reduce(markers, marker_x, fn marker, current_x ->
                  marker_width = text_width(marker.text, marker.style)
                  box = text_box(marker, current_x, baseline, marker_width)
                  {Map.merge(box, metadata), current_x + marker_width}
                end)

              marker_boxes
            end

          {:ok, marker_boxes ++ boxes, next_y}
        end

      %{type: :element} ->
        layout_block(item, x, y, width)

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp inline_text_boxes(runs, style, x, y, width, metadata, prepared_lines \\ nil) do
    lines = prepared_lines || inline_lines(runs, width)
    metadata = Map.delete(metadata, :baseline_snap_depth)
    last_line_index = length(lines) - 1

    {boxes, _consumed_height} =
      lines
      |> Enum.with_index()
      |> Enum.reduce({[], 0.0}, fn {line_runs, index}, {boxes, consumed_height} ->
        current_line_height = inline_line_height(line_runs, style, width)
        baseline_depth = inline_line_baseline_depth(line_runs, style)
        line_top = y - consumed_height
        baseline_y = line_top - baseline_depth
        start_x = aligned_text_x(line_runs, style, x, width)

        line_metadata =
          case Map.get(metadata, :fragment_lines, false) do
            true ->
              metadata
              |> Map.put(:fragment_id, {:line, Map.fetch!(metadata, :flow_id), index})
              |> Map.put(
                :break_before,
                if(index == 0, do: Map.get(metadata, :break_before, :auto), else: :auto)
              )
              |> Map.put(
                :break_after,
                if(index == last_line_index,
                  do: Map.get(metadata, :break_after, :auto),
                  else: :auto
                )
              )

            false ->
              metadata
          end

        line_metadata =
          line_metadata
          |> Map.put(:line_baseline_depth, baseline_depth)
          |> Map.put(:line_box_height, current_line_height)

        line_metadata =
          if Enum.any?(line_runs, &Map.has_key?(&1.style, :_range_state)) do
            # An anonymous Range line has no enclosing block flow identity.
            # Keep its artwork and line marker together so the source line's
            # margin/strut bounds govern one pagination decision.
            Map.put_new(line_metadata, :flow_id, {:inline_line, x, line_top})
          else
            line_metadata
          end

        # Empty atomic boxes place their baseline at the bottom margin edge.
        # Keep that fractional depth after rasterizing the top edge, rather
        # than independently rounding the shared text baseline.
        line_metadata =
          if baseline_depth / @css_pixel_points !=
               Float.floor(baseline_depth / @css_pixel_points) and
               Enum.any?(line_runs, fn run ->
                 Map.get(run, :_inline_bottom_baseline, false) and
                   Map.fetch!(run, :_inline_baseline_depth) +
                     Map.get(run.style, :margin, edges(0.0)).top == baseline_depth
               end) do
            Map.put(line_metadata, :baseline_snap_depth, baseline_depth)
          else
            line_metadata
          end

        {line_boxes, _next_x} =
          Enum.reduce(line_runs, {[], start_x}, fn run, {acc, current_x} ->
            run_boxes =
              inline_run_boxes(run, current_x, line_top, baseline_y, width, line_metadata)

            {Enum.reverse(run_boxes, acc), current_x + inline_run_width(run, width)}
          end)

        line_boxes =
          if line_runs == [] and Enum.any?(runs, &String.contains?(&1.text, "\n")) do
            [
              %{
                type: HtmlValidator.reserve_layout_box(:flow_marker),
                x: start_x,
                y: line_top - current_line_height,
                width: 0.0,
                height: current_line_height,
                role: :empty_line
              }
              |> Map.merge(line_metadata)
            ]
          else
            line_boxes
          end

        line_boxes =
          if Enum.any?(line_runs, &(Map.get(&1, :type) == :inline_image)) do
            marker =
              %{
                type: HtmlValidator.reserve_layout_box(:flow_marker),
                x: start_x,
                y: line_top - current_line_height,
                width: 0.0,
                height: current_line_height,
                role: :inline_image_line
              }
              |> Map.merge(line_metadata)

            [marker | line_boxes]
          else
            line_boxes
          end

        line_boxes = mark_inline_flow_baseline(line_boxes, line_runs, style, baseline_y)

        {line_boxes ++ boxes, consumed_height + current_line_height}
      end)

    {position_inline_contexts(Enum.reverse(boxes), width),
     inline_content_height(runs, lines, style)}
  end

  # Measurement identities mark flow geometry without capturing an extra flag
  # in the ordinary line-rendering closure. Paint-only movement is excluded.
  defp mark_inline_flow_baseline(boxes, runs, style, baseline_y) do
    case style do
      %{_measure_inline_baseline: reference} when is_reference(reference) ->
        if reference == Process.get({__MODULE__, :baseline_measurement}) and
             (runs != [] or boxes != []) do
          case boxes do
            [] ->
              [
                %{
                  type: :baseline_measurement,
                  x: 0.0,
                  y: baseline_y,
                  width: 0.0,
                  height: 0.0,
                  _measured_baseline: baseline_y
                }
              ]

            boxes ->
              Enum.map(boxes, &Map.put(&1, :_measured_baseline, baseline_y))
          end
        else
          boxes
        end

      _ ->
        boxes
    end
  end

  # Block and atomic inline controls share one viewport. Offscreen artwork
  # must not enlarge a control's page allocation, while fields keep their value.
  defp clip_control_contents(boxes, style, clip) do
    if Map.get(style, :form_kind) in [:text, :textarea, :select, :listbox] do
      # A number's invisible initial spin button still occupies its fixed
      # native 15px width in the inner editor. Keep it outside the text clip.
      {boxes, clip} =
        if Map.has_key?(style, :_number_state) do
          offset =
            NativeElixirPdfUtilities.Validators.HtmlFormValidator.initial_number_content_offset(
              style,
              clip.height
            )

          {shift_layout_boxes(boxes, 0.0, -offset), %{clip | width: max(clip.width - 11.25, 0.0)}}
        else
          {boxes, clip}
        end

      {boxes, scrollbar_boxes} =
        if Map.get(style, :form_kind) == :listbox do
          id = style._select_state.id

          selected =
            Enum.filter(boxes, &(Map.get(&1, :initial_option_id) == id and not is_nil(id)))

          selected_bottom =
            Enum.reduce(selected, clip.y + clip.height, fn box, acc ->
              {_top, bottom} = PageGeometry.box_vertical_bounds(box)
              min(acc, bottom)
            end)

          bottom =
            Enum.reduce(boxes, clip.y, fn box, acc ->
              {_top, bottom} = PageGeometry.box_vertical_bounds(box)
              min(acc, bottom)
            end)

          padding = style.padding
          viewport_height = clip.height + padding.top + padding.bottom

          scroll_height =
            max(viewport_height, clip.y + clip.height + padding.top - bottom + padding.bottom)

          offset =
            min(
              # ScrollIntoView compares the option border box to the padding
              # scrollport, then ToRoundedVector2d rounds the CSS-pixel offset.
              round(max(clip.y - padding.bottom - selected_bottom, 0.0) / @css_pixel_points) *
                @css_pixel_points,
              max(scroll_height - viewport_height, 0.0)
            )

          geometry = %{
            vertical: true,
            horizontal: false,
            viewport_width: clip.width + padding.left + padding.right + 11.25,
            viewport_height: viewport_height,
            client_width: clip.width + padding.left + padding.right,
            client_height: viewport_height,
            scroll_width: clip.width + padding.left + padding.right,
            scroll_height: scroll_height,
            scroll_offset: %{x: 0.0, y: offset}
          }

          parts =
            case NativeElixirPdfUtilities.Validators.ScrollbarValidator.prepare(geometry) do
              {:ok, parts} -> parts
              {:error, _} = error -> throw({:render_resource_limit, error})
            end

          artwork = NativeElixirPdfUtilities.HtmlToPdf.ScrollbarPainter.paint(parts)
          artwork = shift_layout_boxes(artwork, clip.x - padding.left, clip.y - padding.bottom)
          # Blink supplies pixel-snapped scrollbar frames to the native theme.
          # Snap only after ordinary placement supplies final page coordinates.
          artwork =
            Enum.map(artwork, fn box ->
              box
              |> Map.put(:form_owner, Map.get(style, :_form_owner))
              |> Map.put(:snap_to_css_pixel_grid, true)
            end)

          {shift_layout_boxes(boxes, 0.0, offset), artwork}
        else
          {boxes, []}
        end

      # Chromium's rounded inner-border clip is separate from the client
      # overflow clip. Overflow controls deliberately bypass this rounded clip.
      padding_clip =
        if Map.get(style, :form_kind) == :listbox do
          padding = style.padding

          %{
            x: clip.x - padding.left,
            y: clip.y - padding.bottom,
            width: clip.width + padding.left + padding.right + 11.25,
            height: clip.height + padding.top + padding.bottom,
            border_radius:
              max(style.border_radius - Enum.max(Map.values(style.border_widths)), 0.0)
          }
        else
          nil
        end

      # OverflowClipRect contracts border outsets, then excludes the scrollbar.
      # Authored padding remains inside the scrolling client viewport.
      clip =
        if padding_clip do
          %{padding_clip | width: padding_clip.width - 11.25}
          |> Map.delete(:border_radius)
        else
          clip
        end

      client_background =
        if Map.get(style, :form_kind) == :listbox and style.background_color != nil and
             Map.get(style, :background_image) == nil and clip.width > 0 and clip.height > 0 do
          # A scroll container paints its field background in the scrolling
          # contents layer, inside the client clip, after the native border.
          client = clip

          [
            %{
              type: HtmlValidator.reserve_layout_box(:rect),
              x: client.x - style.border_widths.left,
              y: client.y - style.border_widths.bottom,
              width: client.width + style.border_widths.left + style.border_widths.right,
              height: client.height + style.border_widths.top + style.border_widths.bottom,
              clip: Map.put(client, :inner_clip, padding_clip),
              snap_to_css_pixel_grid: true,
              fill_color: style.background_color,
              stroke_color: nil,
              stroke_width: 0.0,
              border_radius: 0.0,
              form_owner: Map.get(style, :_form_owner)
            }
          ]
        else
          []
        end

      if clip.width > 0 and clip.height > 0 do
        client_background ++
          Enum.flat_map(boxes, fn box ->
            {top, bottom} = PageGeometry.box_vertical_bounds(box)

            if top > clip.y and bottom < clip.y + clip.height do
              clip = if padding_clip, do: Map.put(clip, :inner_clip, padding_clip), else: clip
              [box |> Map.put(:clip, clip) |> Map.put(:out_of_flow, true)]
            else
              []
            end
          end) ++ scrollbar_boxes
      else
        []
      end
    else
      boxes
    end
  end

  defp inline_run_boxes(run, x, line_top, baseline_y, width, metadata) do
    boxes =
      case run do
        %{type: :inline_block, style: %{_math_box: measured} = style} ->
          group = make_ref()

          {background_items, glyph_items} =
            Enum.split_with(measured.drawing_items, &(&1.kind == :background))

          background =
            background_box(
              style,
              x,
              baseline_y - measured.descent,
              measured.width,
              measured.ascent + measured.descent
            )

          backgrounds =
            Enum.flat_map(background_items, fn item ->
              background_box(item.style, x + item.x, baseline_y + item.y, item.width, item.height)
            end)

          native? = Enum.any?(glyph_items, &(&1.kind == :native))
          logical_anchor? = native? or not Enum.any?(glyph_items, &(&1.kind == :glyph))

          glyphs =
            Enum.flat_map(glyph_items, fn glyph ->
              cond do
                glyph.kind == :native ->
                  shift_layout_boxes([glyph.box], x + glyph.x, baseline_y + glyph.y)
                  |> Enum.map(&Map.put(&1, :_math_native_paint, true))

                glyph.kind == :logical ->
                  if logical_anchor? do
                    # A space supplies an extraction anchor without ink or flow width.
                    [
                      %{
                        type: HtmlValidator.reserve_layout_box(:glyph_group),
                        actual_text: glyph.actual_text,
                        x: x + glyph.x,
                        y: baseline_y + glyph.y,
                        width: 0.0,
                        height: 0.0,
                        boxes: [
                          %{
                            type: HtmlValidator.reserve_layout_box(:text),
                            text: " ",
                            font: Font.pdf_name(style.font_face),
                            font_face: style.font_face,
                            font_size: style.font_size,
                            color: style.color,
                            x: x + glyph.x,
                            y: baseline_y + glyph.y,
                            width: 0.0
                          }
                        ]
                      }
                    ]
                  else
                    []
                  end

                true ->
                  [
                    %{
                      type: HtmlValidator.reserve_layout_box(:text),
                      text: glyph.text,
                      glyph_ids: [glyph.glyph_id],
                      x: x + glyph.x,
                      y: baseline_y + glyph.y,
                      width: glyph.width,
                      height: glyph.height,
                      font: Font.pdf_name(glyph.font_face),
                      font_face: glyph.font_face,
                      font_size: glyph.font_size,
                      color: glyph.color,
                      snap_to_css_pixel_grid: true,
                      _math_group:
                        if(native?, do: Map.fetch!(glyph, :logical_group), else: group),
                      _math_actual_text:
                        if(native?,
                          do: Map.fetch!(glyph, :logical_text),
                          else: measured.actual_text
                        )
                    }
                  ]
              end
            end)

          Enum.map(background ++ backgrounds ++ glyphs, fn box ->
            inherited =
              if Map.get(box, :_math_native_paint, false),
                do:
                  Map.drop(metadata, [
                    :line_baseline_depth,
                    :line_box_height,
                    :baseline_snap_depth
                  ]),
                else: metadata

            box |> Map.delete(:_math_native_paint) |> Map.merge(inherited)
          end)
          |> then(&attach_html_target(style, &1, x, baseline_y + measured.ascent))

        %{type: :inline_image, style: style} ->
          margin = Map.get(style, :margin, edges(0.0))
          padding = Map.get(style, :padding, edges(0.0))
          border = Map.get(style, :border_widths, edges(0.0))
          content_width = run._inline_content_width
          content_height = run._inline_content_height
          box_width = content_width + horizontal_box_size(style)
          box_height = content_height + vertical_box_size(style)
          box_x = x + margin.left

          box_top =
            case Map.get(style, :vertical_align, :baseline) do
              :top -> line_top - margin.top
              :bottom -> line_top - metadata.line_box_height + box_height + margin.bottom
              _ -> baseline_y + run._inline_baseline_depth
            end

          background = background_box(style, box_x, box_top - box_height, box_width, box_height)

          images =
            fitted_image_boxes(
              style,
              box_x + border.left + padding.left,
              box_top - border.top - padding.top - content_height,
              content_width,
              content_height,
              Map.fetch!(style, :image)
            )

          (background ++ images)
          |> Enum.map(fn box ->
            box = Map.merge(box, metadata)

            box =
              if box.type == :image and
                   (Map.get(style, :link_url) != nil or Map.get(style, :link_destination) != nil) do
                box
                |> Map.merge(Map.take(style, [:link_url, :link_destination]))
                |> Map.put(:link_rect, %{
                  x: box_x,
                  y: box_top - box_height,
                  width: box_width,
                  height: box_height
                })
              else
                box
              end

            case Map.get(run, :inline_positioning_contexts) do
              nil -> box
              contexts -> Map.put(box, :inline_positioning_contexts, contexts)
            end
          end)
          |> then(&attach_html_target(style, &1, box_x, box_top))
          |> relatively_shifted_boxes(style, x, box_top, width, box_top - box_height)

        %{type: :inline_block, style: %{svg_scene: _} = style} ->
          margin = Map.get(style, :margin, edges(0.0))
          content_width = Map.fetch!(run, :_inline_content_width)
          content_height = Map.fetch!(run, :_inline_content_height)
          box_top = baseline_y + Map.fetch!(run, :_inline_baseline_depth)

          svg_drawing_boxes(style, x + margin.left, box_top, content_width, content_height)
          |> Enum.map(&Map.merge(&1, metadata))
          |> then(&attach_html_target(style, &1, x + margin.left, box_top))

        %{type: :inline_block, style: %{display: display} = style, children: children}
        when display in [:inline_flex, :inline_grid] ->
          content_width = inline_block_content_width(run, width)

          style =
            Map.put(
              style,
              :width,
              declared_size_for_border_box(
                style,
                :width,
                content_width + horizontal_box_size(style)
              )
            )

          case layout_block(
                 %{type: :element, style: style, children: children},
                 x,
                 line_top,
                 inline_run_width(run, width)
               ) do
            {:ok, boxes, _next_y} -> Enum.map(boxes, &Map.merge(&1, metadata))
            {:error, _} -> [%{type: :layout_error}]
          end

        %{type: :inline_block, style: %{_fieldset: true} = style, children: children} ->
          top = baseline_y + Map.fetch!(run, :_inline_baseline_depth)
          style = Map.put(style, :width, inline_block_content_width(run, width))

          case layout_block(
                 %{type: :element, style: style, children: children},
                 x,
                 top,
                 inline_run_width(run, width)
               ) do
            {:ok, boxes, _} -> Enum.map(boxes, &Map.merge(&1, metadata))
            {:error, _} -> [%{type: :layout_error}]
          end

        %{type: :inline_block, style: style} ->
          margin = Map.get(style, :margin, edges(0.0))
          padding = Map.get(style, :padding, edges(0.0))
          border_widths = Map.get(style, :border_widths, edges(0.0))
          content_width = inline_block_content_width(run, width)
          content_height = inline_block_content_height(run, content_width)
          box_width = content_width + horizontal_box_size(style)
          box_height = content_height + vertical_box_size(style)

          box_top =
            case {Map.has_key?(style, :embedded_document),
                  Map.get(style, :vertical_align, :baseline)} do
              {true, :top} -> line_top - margin.top
              {true, :bottom} -> line_top - metadata.line_box_height + box_height + margin.bottom
              _ -> baseline_y + Map.fetch!(run, :_inline_baseline_depth)
            end

          box_x = x + margin.left

          content_width =
            if Map.get(style, :form_kind) == :listbox,
              do: max(content_width - 11.25, 0.0),
              else: content_width

          content_x = box_x + border_widths.left + padding.left
          content_top = box_top - border_widths.top - padding.top

          background =
            style
            |> background_box(box_x, box_top - box_height, box_width, box_height)
            |> Enum.map(&Map.merge(&1, metadata))

          {content_boxes, _height} =
            case run do
              %{style: %{embedded_document: surface}} ->
                case EmbeddedViewport.layout(surface, content_width, content_height) do
                  {:ok, group} ->
                    group =
                      group
                      |> Map.put(:role, :embedded_viewport_host)
                      |> Map.put(:x, content_x)
                      |> Map.put(:y, content_top - content_height)
                      |> Map.put(
                        :transform,
                        {1, 0, 0, 1, content_x, content_top - content_height}
                      )
                      |> Map.merge(metadata)

                    {[group], content_height}

                  {:error, {_reason, _diagnostic}} = error ->
                    throw({:render_resource_limit, error})
                end

              %{runs: child_runs} ->
                inline_text_boxes(
                  child_runs,
                  style,
                  content_x,
                  content_top,
                  content_width,
                  metadata,
                  Map.fetch!(run, :_inline_lines)
                )

              %{children: children} ->
                case layout_block_content(
                       children,
                       style,
                       content_x,
                       content_top,
                       content_width,
                       metadata
                     ) do
                  {:ok, boxes, height} -> {boxes, height}
                  {:error, _reason} -> {[%{type: :layout_error}], 0.0}
                end
            end

          content_boxes =
            clip_control_contents(content_boxes, style, %{
              x: content_x,
              y: content_top - content_height,
              width: content_width,
              height: content_height
            })

          boxes = background ++ content_boxes

          boxes =
            if Map.has_key?(style, :_range_state) do
              # Range has no inner text box. Retain its source line's margin and
              # strut bounds when Pagination moves the atomic line to a new page.
              marker =
                %{
                  type: HtmlValidator.reserve_layout_box(:flow_marker),
                  x: box_x,
                  y: line_top - metadata.line_box_height,
                  width: 0.0,
                  height: metadata.line_box_height
                }
                |> Map.merge(metadata)

              boxes ++ [marker]
            else
              boxes
            end

          boxes = attach_html_target(style, boxes, box_x, box_top)

          case Map.get(run, :inline_positioning_contexts, []) do
            [] ->
              boxes

            contexts ->
              marker = %{
                type: :inline_positioning_marker,
                x: box_x,
                y: box_top - box_height,
                width: box_width,
                height: box_height,
                inline_positioning_contexts: contexts
              }

              [marker | inherit_inline_positioning_contexts(boxes, contexts)]
          end

        %{type: :soft_break} ->
          box =
            %{
              type: HtmlValidator.reserve_layout_box(:flow_marker),
              x: x,
              y: line_top - metadata.line_box_height,
              width: 0.0,
              height: metadata.line_box_height,
              role: :soft_break
            }
            |> Map.merge(metadata)

          case Map.get(run, :inline_positioning_contexts) do
            nil -> [box]
            contexts -> [Map.put(box, :inline_positioning_contexts, contexts)]
          end

        %{type: :inline_positioning_marker, style: style} ->
          line_height = min(Map.fetch!(style, :line_height), metadata.line_box_height)

          [
            %{
              type: :inline_positioning_marker,
              x: x,
              y: line_top - line_height,
              width: 0.0,
              height: line_height,
              inline_positioning_contexts: Map.fetch!(run, :inline_positioning_contexts)
            }
          ]

        _ ->
          box = run |> text_box(x, baseline_y, width) |> Map.merge(metadata)

          boxes =
            case run.style do
              %{_text_decorations: _} -> text_decoration_boxes(box, run.style, metadata) ++ [box]
              _ -> [box]
            end

          case Map.get(run, :inline_positioning_contexts, []) do
            [] -> boxes
            contexts -> Enum.map(boxes, &Map.put(&1, :inline_positioning_contexts, contexts))
          end
      end

    boxes =
      case Map.get(run, :html_target_ids, []) do
        [] ->
          boxes

        identities ->
          top = line_top

          Enum.map(boxes, fn box ->
            targets = Enum.map(identities, &%{identity: &1, dx: 0.0, dy: top - box.y})
            Map.update(box, :html_targets, targets, &(targets ++ &1))
          end)
      end

    boxes =
      case Map.get(run, :inline_backgrounds) do
        nil ->
          boxes

        backgrounds ->
          fragment_width = inline_run_width(run, width)

          backgrounds =
            Enum.flat_map(backgrounds, fn background ->
              height = background.ascent + background.descent

              if fragment_width > 0 and height > 0 do
                [
                  %{
                    type: HtmlValidator.reserve_layout_box(:rect),
                    x: x,
                    y: baseline_y - background.descent,
                    width: fragment_width,
                    height: height,
                    fill_color: background.color,
                    stroke_color: nil,
                    stroke_width: 0.0,
                    border_radius: 0.0,
                    role: :inline_background,
                    snap_to_css_pixel_grid: true
                  }
                  |> Map.merge(metadata)
                ]
              else
                []
              end
            end)

          backgrounds =
            case Map.get(run, :inline_positioning_contexts) do
              nil ->
                backgrounds

              contexts ->
                Enum.map(backgrounds, &Map.put(&1, :inline_positioning_contexts, contexts))
            end

          backgrounds ++ boxes
      end

    case run do
      %{outline_anchor: anchor} -> attach_outline_anchor(%{outline_anchor: anchor}, boxes)
      _ -> boxes
    end
  end

  defp text_decoration_boxes(box, style, metadata) do
    style
    |> Map.get(:_text_decorations, [])
    |> Enum.flat_map(fn decoration ->
      if box.font_size == 0 or decoration.thickness == 0 or box.annotation_width <= 0 or
           decoration.color == :transparent do
        []
      else
        Enum.map(decoration.lines, fn line ->
          position =
            case line do
              :underline -> decoration.underline - decoration.thickness
              :overline -> decoration.overline
              :line_through -> decoration.strike - decoration.thickness / 2
            end

          %{
            type: HtmlValidator.reserve_layout_box(:rect),
            x: box.x,
            y: box.y + position,
            width: box.annotation_width,
            height: decoration.thickness,
            fill_color: decoration.color,
            stroke_color: decoration.color,
            stroke_width: 0.0,
            border_radius: 0.0,
            role: :text_decoration,
            text_baseline_offset: position,
            snap_to_css_pixel_grid: true
          }
          |> Map.merge(metadata)
        end)
      end
    end)
  end

  defp position_inline_contexts(boxes, available_width) do
    contexts =
      boxes
      |> Enum.flat_map(fn box ->
        box
        |> Map.get(:inline_positioning_contexts, [])
        |> Enum.with_index()
      end)
      |> Enum.reduce(%{}, fn {context, depth}, acc ->
        Map.update(acc, context.id, {context, depth}, fn {existing, existing_depth} ->
          {existing, max(existing_depth, depth)}
        end)
      end)
      |> Map.values()
      |> Enum.sort_by(fn {_context, depth} -> -depth end)

    boxes =
      Enum.reduce(contexts, boxes, fn {context, _depth}, acc ->
        position_inline_context(acc, context, available_width)
      end)

    boxes
    |> Enum.flat_map(fn box ->
      case box do
        %{type: :inline_positioning_marker, html_targets: _targets} ->
          [
            box
            |> Map.put(:type, HtmlValidator.reserve_layout_box(:flow_marker))
            |> Map.put(:width, 0.0)
          ]

        %{type: :inline_positioning_marker} ->
          []

        box ->
          [box]
      end
    end)
    |> Enum.map(&Map.delete(&1, :inline_positioning_contexts))
  end

  defp position_inline_context(boxes, context, available_width) do
    [_member | _members] =
      member_indexes =
      boxes
      |> Enum.with_index()
      |> Enum.filter(fn {box, _index} -> inline_positioning_member?(box, context.id) end)

    first_index = member_indexes |> List.first() |> elem(1)
    last_index = member_indexes |> List.last() |> elem(1)
    member_boxes = Enum.map(member_indexes, &elem(&1, 0))
    containing_block = inline_positioning_containing_block(member_boxes)

    case positioned_child_boxes(context.positioned_children, containing_block) do
      {:ok, positioned_boxes} ->
        ancestors = inline_positioning_ancestors(member_boxes, context.id)

        positioned_boxes =
          Enum.map(positioned_boxes, fn entry ->
            %{entry | boxes: inherit_inline_positioning_contexts(entry.boxes, ancestors)}
          end)

        segment_length = last_index - first_index + 1

        positioned_segment =
          boxes
          |> Enum.slice(first_index, segment_length)
          |> stack_positioned_boxes(
            positioned_boxes,
            is_integer(Map.get(context.style, :z_index))
          )
          |> shift_positioned_inline(context.style, available_width, containing_block.height)

        Enum.take(boxes, first_index) ++
          positioned_segment ++ Enum.drop(boxes, last_index + 1)

      {:error, _reason} ->
        boxes ++ [%{type: :layout_error}]
    end
  end

  defp inline_positioning_member?(box, context_id) do
    box
    |> Map.get(:inline_positioning_contexts, [])
    |> Enum.any?(&(&1.id == context_id))
  end

  defp inline_positioning_containing_block(boxes) do
    {left, right, top, bottom} =
      Enum.reduce(boxes, {nil, nil, nil, nil}, fn box, {left, right, top, bottom} ->
        {box_left, box_right, box_top, box_bottom} = inline_positioning_bounds(box)

        {
          if(is_number(left), do: min(left, box_left), else: box_left),
          if(is_number(right), do: max(right, box_right), else: box_right),
          if(is_number(top), do: max(top, box_top), else: box_top),
          if(is_number(bottom), do: min(bottom, box_bottom), else: box_bottom)
        }
      end)

    %{x: left, top: top, width: max(right - left, 0.0), height: max(top - bottom, 0.0)}
  end

  defp inline_positioning_bounds(box) do
    right =
      case box do
        %{type: :text, annotation_width: width} -> box.x + width
        box -> box.x + Map.fetch!(box, :width)
      end

    {top, bottom} =
      case box do
        %{type: :inline_positioning_marker, y: y, height: height} -> {y + height, y}
        _ -> PageGeometry.box_vertical_bounds(box)
      end

    {box.x, right, top, bottom}
  end

  defp inline_positioning_ancestors([box | _boxes], context_id) do
    box
    |> Map.get(:inline_positioning_contexts, [])
    |> Enum.take_while(&(&1.id != context_id))
  end

  defp inherit_inline_positioning_contexts(boxes, contexts) do
    case contexts do
      [] ->
        boxes

      contexts ->
        Enum.map(boxes, fn box ->
          inherited = contexts ++ Map.get(box, :inline_positioning_contexts, [])
          Map.put(box, :inline_positioning_contexts, inherited)
        end)
    end
  end

  defp shift_positioned_inline(boxes, style, available_width, inline_height) do
    offsets = Map.get(style, :offsets, edges(:auto))
    left = resolved_offset(offsets.left, available_width)
    right = resolved_offset(offsets.right, available_width)
    top = resolved_offset(offsets.top, inline_height)
    bottom = resolved_offset(offsets.bottom, inline_height)
    delta_x = if(is_number(left), do: left, else: if(is_number(right), do: -right, else: 0.0))
    delta_y = if(is_number(top), do: -top, else: if(is_number(bottom), do: bottom, else: 0.0))
    shift_layout_boxes(boxes, delta_x, delta_y)
  end

  defp inline_content_height(runs, width_or_lines, style) do
    case width_or_lines do
      width when is_number(width) ->
        runs
        |> inline_lines(width)
        |> then(&inline_content_height(runs, &1, style))

      lines when is_list(lines) ->
        height =
          case runs do
            [] -> 0.0
            _ -> Enum.reduce(lines, 0.0, &(&2 + inline_line_height(&1, style, nil)))
          end

        if Map.get(style, :form_kind) in [:text, :input_button, :select],
          do: max(height, Map.fetch!(style, :line_height)),
          else: height
    end
  end

  defp inline_line_height(line_runs, parent_style, available_width) do
    if line_runs != [] and
         Enum.all?(
           line_runs,
           &match?(%{type: :inline_positioning_marker, html_target_ids: _}, &1)
         ) do
      0.0
    else
      parent_depth = text_baseline_depth(parent_style)

      parent_height =
        if Map.get(parent_style, :_math_flow, false),
          do: 0.0,
          else: Map.fetch!(parent_style, :line_height)

      {ascent, descent} =
        Enum.reduce(line_runs, {parent_depth, parent_height - parent_depth}, fn run,
                                                                                {above, below} ->
          case run do
            %{type: type, style: %{vertical_align: align} = style}
            when align in [:top, :bottom] and
                   (type == :inline_image or is_map_key(style, :embedded_document)) ->
              {above, below}

            %{type: type, _inline_baseline_depth: depth, style: style} = run
            when type in [:inline_block, :inline_image] ->
              margin = Map.get(style, :margin, edges(0.0))
              height = Map.fetch!(run, :_inline_content_height) + vertical_box_size(style)
              {max(above, margin.top + depth), max(below, height + margin.bottom - depth)}

            %{type: :inline_block} ->
              {above, below}

            %{style: style} ->
              depth = text_baseline_depth(style)
              {max(above, depth), max(below, Map.fetch!(style, :line_height) - depth)}
          end
        end)

      Enum.reduce(line_runs, ascent + descent, fn run, height ->
        case run do
          %{type: type, style: %{vertical_align: align} = style} = run
          when align in [:top, :bottom] and
                 (type == :inline_image or is_map_key(style, :embedded_document)) ->
            margin = Map.get(style, :margin, edges(0.0))

            max(
              height,
              run._inline_content_height + vertical_box_size(style) + margin.top + margin.bottom
            )

          %{type: type, _inline_baseline_depth: _depth}
          when type in [:inline_block, :inline_image] ->
            height

          %{type: :inline_block, style: style} ->
            margin = Map.get(style, :margin, edges(0.0))
            content_width = inline_block_content_width(run, available_width)

            max(
              height,
              inline_block_content_height(run, content_width) + vertical_box_size(style) +
                margin.top + margin.bottom
            )

          %{style: style} ->
            max(height, Map.fetch!(style, :line_height))
        end
      end)
    end
  end

  defp inline_line_baseline_depth(line_runs, parent_style) do
    text_baseline_depth = text_baseline_depth(parent_style)

    depth =
      Enum.reduce(line_runs, text_baseline_depth, fn run, depth ->
        case run do
          %{type: type, style: %{vertical_align: align} = style}
          when align in [:top, :bottom] and
                 (type == :inline_image or is_map_key(style, :embedded_document)) ->
            depth

          %{type: type, _inline_baseline_depth: baseline, style: style}
          when type in [:inline_block, :inline_image] ->
            margin = Map.get(style, :margin, edges(0.0))
            max(depth, margin.top + baseline)

          %{type: :inline_block, style: style} ->
            padding = Map.get(style, :padding, edges(0.0))
            border_widths = Map.get(style, :border_widths, edges(0.0))
            max(depth, border_widths.top + padding.top + Map.fetch!(style, :font_size))

          %{style: style} ->
            max(depth, text_baseline_depth(style))
        end
      end)

    bottom_runs =
      Enum.filter(
        line_runs,
        &match?(
          %{type: type, style: %{vertical_align: :bottom} = style}
          when type == :inline_image or is_map_key(style, :embedded_document),
          &1
        )
      )

    case bottom_runs do
      [] ->
        depth

      _ ->
        baseline_runs =
          Enum.reject(
            line_runs,
            &match?(
              %{type: type, style: %{vertical_align: align} = style}
              when align in [:top, :bottom] and
                     (type == :inline_image or is_map_key(style, :embedded_document)),
              &1
            )
          )

        base_height = inline_line_height(baseline_runs, parent_style, nil)

        Enum.reduce(bottom_runs, depth, fn run, above ->
          margin = Map.get(run.style, :margin, edges(0.0))

          height =
            run._inline_content_height + vertical_box_size(run.style) + margin.top + margin.bottom

          max(above, depth + max(height - base_height, 0.0))
        end)
    end
  end

  defp text_baseline_depth(style) do
    HtmlValidator.text_baseline_depth(style)
  end

  defp inline_lines(runs, width, measure_blocks? \\ true) do
    runs
    |> Enum.reduce([[]], fn run, lines ->
      case run do
        %{type: :inline_image, style: style} ->
          {content_width, content_height} =
            image_content_size(style, width, Map.get(run, :_inline_available_height))

          margin = Map.get(style, :margin, edges(0.0))

          depth =
            case Map.get(style, :vertical_align, :baseline) do
              :middle ->
                (content_height + vertical_box_size(style) + margin.top + margin.bottom +
                   run._inline_parent_x_height) / 2 - margin.top

              _ ->
                content_height + vertical_box_size(style) + margin.bottom
            end

          run =
            run
            |> Map.put(:_inline_content_width, content_width)
            |> Map.put(:_inline_content_height, content_height)
            |> Map.put(:_inline_baseline_depth, depth)
            |> Map.put(
              :_inline_bottom_baseline,
              Map.get(style, :vertical_align, :baseline) == :baseline
            )

          append_inline_atomic(lines, run, width)

        %{type: :soft_break} ->
          List.update_at(lines, length(lines) - 1, &(&1 ++ [run]))

        %{type: :inline_block, style: %{display: :inline_block}} ->
          run = if measure_blocks?, do: measure_inline_block(run, width), else: run
          append_inline_atomic(lines, run, width)

        %{type: type} when type in [:inline_block, :inline_image, :inline_positioning_marker] ->
          append_inline_atomic(lines, run, width)

        _ ->
          parts = String.split(run.text, "\n", trim: false)

          parts
          |> Enum.with_index()
          |> Enum.reduce(lines, fn {part, index}, acc ->
            acc =
              case part do
                "" -> acc
                _ -> append_wrapped_inline_text(acc, %{run | text: part}, width)
              end

            case index < length(parts) - 1 do
              true -> acc ++ [[]]
              false -> acc
            end
          end)
      end
    end)
    |> maybe_drop_trailing_break_line(runs)
    |> Enum.map(&trim_trailing_inline_whitespace/1)
  end

  defp maybe_drop_trailing_break_line(lines, runs) do
    last_meaningful_run =
      runs
      |> Enum.reverse()
      |> Enum.find(fn run ->
        Map.get(run, :hard_break, false) or trim_inline_whitespace(run.text, run) != ""
      end)

    terminal_newline? =
      case List.last(runs) do
        %{style: %{white_space: mode}, text: text}
        when mode in [:pre, :pre_wrap, :break_spaces] ->
          String.ends_with?(text, "\n")

        _ ->
          false
      end

    case {last_meaningful_run, List.last(lines)} do
      {%{hard_break: true}, []} when length(lines) > 1 -> Enum.drop(lines, -1)
      {_, []} when terminal_newline? and length(lines) > 1 -> Enum.drop(lines, -1)
      _ -> lines
    end
  end

  defp trim_trailing_inline_whitespace(line_runs) do
    last = List.last(line_runs)

    ordinary_tail? =
      case last do
        %{type: type} when type in [:inline_block, :inline_image, :inline_positioning_marker] ->
          true

        %{style: %{white_space: mode}} when mode in [:pre, :pre_wrap, :break_spaces] ->
          true

        %{_table_text: true, text: <<first, _rest::binary>>}
        when first not in [32, 9, 10, 12, 13] ->
          true

        %{text: text} ->
          trim_inline_whitespace(text, last) != ""

        _ ->
          false
      end

    {trailing, remaining} =
      if ordinary_tail? do
        {[], line_runs}
      else
        {trailing, remaining} =
          line_runs
          |> Enum.reverse()
          |> Enum.split_while(fn run ->
            Map.get(run, :type) == :soft_break or
              (Map.get(run, :type) not in [
                 :inline_block,
                 :inline_image,
                 :inline_positioning_marker
               ] and
                 Map.get(run.style, :white_space) not in [:pre, :pre_wrap, :break_spaces] and
                 trim_inline_whitespace(run.text, run) == "")
          end)

        {trailing, Enum.reverse(remaining)}
      end

    remaining =
      remaining
      |> List.update_at(-1, fn
        %{type: type} = run
        when type in [:inline_block, :inline_image, :inline_positioning_marker] ->
          run

        %{style: %{white_space: mode}} = run when mode in [:pre, :pre_wrap, :break_spaces] ->
          run

        run ->
          %{run | text: String.replace(run.text, ~r/[ \t\f\r]+$/u, "")}
      end)

    if trailing == [] do
      remaining
    else
      remaining ++
        (trailing |> Enum.filter(&(Map.get(&1, :type) == :soft_break)) |> Enum.reverse())
    end
  end

  defp append_inline_atomic(lines, run, width) do
    current_line = List.last(lines) || []

    case is_number(width) and Map.get(run.style, :white_space) not in [:nowrap, :pre] and
           current_line != [] and
           inline_line_width(current_line, width) + inline_run_width(run, width) >
             width do
      true -> lines ++ [[run]]
      false -> List.update_at(lines, length(lines) - 1, &(&1 ++ [run]))
    end
  end

  defp append_wrapped_inline_text(lines, run, width) do
    case width do
      width when is_number(width) and width > 0 ->
        merge_key = System.unique_integer([:positive])

        tokens = inline_wrap_tokens(run.text, run.style)

        tokens =
          case {tokens, List.last(List.last(lines) || [])} do
            {[<<"-", next::utf8, _::binary>> = token | rest], %{text: previous_text}}
            when run.style.white_space not in [:nowrap, :pre] ->
              if Regex.match?(~r/[\p{L}\p{N}]$/u, previous_text) and
                   Regex.match?(~r/^[\p{L}\p{N}]/u, <<next::utf8>>) do
                ["-", binary_part(token, 1, byte_size(token) - 1) | rest]
              else
                tokens
              end

            _ ->
              tokens
          end

        tokens
        |> Enum.reduce(lines, fn token, acc ->
          append_inline_token(
            acc,
            run |> Map.put(:text, token) |> Map.put(:merge_key, merge_key),
            width
          )
        end)

      _ ->
        List.update_at(lines, length(lines) - 1, &(&1 ++ [run]))
    end
  end

  defp inline_wrap_tokens(text, style, intrinsic? \\ false) do
    case style do
      %{white_space: mode} when mode in [:nowrap, :pre] ->
        [text]

      %{line_break: :anywhere} ->
        String.graphemes(text)

      %{overflow_wrap: :anywhere} when intrinsic? ->
        String.graphemes(text)

      %{word_break: :break_word} when intrinsic? ->
        String.graphemes(text)

      %{white_space: :break_spaces} ->
        Regex.scan(~r/[^ \t]+[ \t]?|[ \t]/u, text) |> Enum.map(&List.first/1)

      _ ->
        ~r/[^ \t\n\f\r]+[ \t\n\f\r]*|[ \t\n\f\r]+/u
        |> Regex.scan(text)
        |> Enum.map(&List.first/1)
        |> Enum.flat_map(&String.split(&1, ~r/(?<=[\p{L}\p{N}]-)(?=[\p{L}\p{N}])/u))
    end
  end

  defp append_inline_token(lines, run, width) do
    current_line = List.last(lines) || []
    mode = Map.get(run.style, :white_space)
    preserve? = mode in [:pre, :pre_wrap, :break_spaces]
    nowrap? = mode in [:nowrap, :pre]

    emergency? =
      case run.style do
        %{overflow_wrap: overflow} when overflow in [:anywhere, :break_word] -> not nowrap?
        %{word_break: :break_word} -> not nowrap?
        %{line_break: :break_word} -> not nowrap?
        _ -> false
      end

    run =
      if preserve? and String.contains?(run.text, "\t") do
        space_width = text_width(" ", run.style)
        start_width = inline_line_width(current_line, width)

        {text, _width} =
          run.text
          |> String.split("\t", trim: false)
          |> Enum.with_index()
          |> Enum.reduce({"", start_width}, fn {part, index}, {text, advance} ->
            spaces =
              cond do
                index == 0 -> 0
                space_width <= 0 -> 0
                true -> 8 - Integer.mod(round(advance / space_width), 8)
              end

            prefix = String.duplicate(" ", spaces)
            {text <> prefix <> part, advance + spaces * space_width + text_width(part, run.style)}
          end)

        %{run | text: text}
      else
        run
      end

    run =
      if nowrap? and not preserve? and current_line == [],
        do: %{run | text: trim_leading_inline_whitespace(run.text)},
        else: run

    token_text =
      if mode in [:pre, :break_spaces],
        do: run.text,
        else: String.replace(run.text, ~r/[ \t\f\r]+$/u, "")

    token_width = text_width(token_text, run.style)

    cond do
      not preserve? and trim_inline_whitespace(run.text, run) == "" and current_line == [] ->
        lines

      # An indivisible grapheme must overflow; splitting it again cannot make progress.
      current_line == [] and emergency? and token_width > width and
          length(String.graphemes(run.text)) > 1 ->
        append_break_word_token(lines, run, width)

      current_line != [] and
          inline_line_width(current_line, width) + token_width > width + @line_wrap_tolerance ->
        break? =
          case List.last(current_line) do
            %{text: text, merge_key: key}
            when text != "" and key == run.merge_key and mode not in [:nowrap, :pre] ->
              true

            _ ->
              previous =
                current_line
                |> Enum.reverse()
                |> Enum.find(
                  &(Map.get(&1, :type) in [:inline_block, :inline_image, :soft_break] or
                      (Map.get(&1, :type) != :inline_positioning_marker and &1.text != ""))
                )

              inline_break_boundary?(previous, run, current_line)
          end

        if break? do
          next_run = %{
            run
            | text: if(preserve?, do: run.text, else: trim_leading_inline_whitespace(run.text))
          }

          if emergency? and token_width > width and not Map.get(run, :emergency_break, false) do
            append_break_word_token(lines ++ [[]], next_run, width)
          else
            lines ++ [[next_run]]
          end
        else
          {prefix, tail} = split_unbroken_inline_tail(current_line, run)

          word_width =
            inline_line_width(tail, width) - text_width(run.text, run.style) + token_width

          cond do
            emergency? and word_width > width and not Map.get(run, :emergency_break, false) ->
              lines =
                if prefix == [],
                  do: List.replace_at(lines, -1, []),
                  else: List.replace_at(lines, -1, prefix) ++ [[]]

              Enum.reduce(tail, lines, fn fragment, acc ->
                cond do
                  Map.get(fragment, :type) in [
                    :inline_block,
                    :inline_image,
                    :inline_positioning_marker
                  ] ->
                    append_inline_atomic(acc, fragment, width)

                  Map.get(fragment.style, :white_space) in [:nowrap, :pre] ->
                    append_inline_token(acc, fragment, width)

                  true ->
                    append_break_word_token(acc, fragment, width)
                end
              end)

            prefix == [] ->
              List.update_at(lines, length(lines) - 1, &append_inline_line_run(&1, run))

            true ->
              List.replace_at(lines, -1, prefix) ++ [tail]
          end
        end

      true ->
        List.update_at(lines, length(lines) - 1, &append_inline_line_run(&1, run))
    end
  end

  # Inline markup does not itself introduce a wrapping opportunity.
  defp inline_break_boundary?(previous, run, line) do
    case previous do
      %{type: :soft_break} ->
        true

      %{type: type} when type in [:inline_block, :inline_image] ->
        Map.get(run.style, :white_space) not in [:nowrap, :pre]

      %{text: previous_text, style: previous_style} ->
        mode = Map.get(run.style, :white_space)

        cond do
          mode == :pre ->
            false

          mode == :nowrap ->
            Map.get(previous_style, :white_space) not in [:nowrap, :pre] and
              (Regex.match?(~r/[ \t\f\r]$/u, previous_text) or
                 Regex.match?(~r/^[ \t\f\r]/u, run.text))

          Map.get(previous, :merge_key) == Map.get(run, :merge_key) ->
            true

          Map.get(run, :emergency_break, false) or
            Map.get(run.style, :line_break) == :anywhere or
              Map.get(previous_style, :line_break) == :anywhere ->
            # A style boundary cannot split an extended grapheme cluster.
            last_cluster =
              line
              |> Enum.reverse()
              |> Enum.take_while(&(Map.get(&1, :type) not in [:inline_block, :inline_image]))
              |> Enum.reverse()
              |> Enum.map_join(& &1.text)
              |> String.graphemes()
              |> List.last()

            List.first(String.graphemes(last_cluster <> run.text)) == last_cluster

          Regex.match?(~r/[ \t\f\r]$/u, previous_text) or
              Regex.match?(~r/^[ \t\f\r]/u, run.text) ->
            true

          true ->
            Regex.match?(~r/[\p{L}\p{N}]-$/u, previous_text) and
              Regex.match?(~r/^[\p{L}\p{N}]/u, run.text)
        end

      _ ->
        false
    end
  end

  # Move the whole trailing word when its next styled fragment overflows.
  defp split_unbroken_inline_tail(line, run) do
    line
    |> Enum.with_index()
    |> Enum.reverse()
    |> Enum.reduce_while({[], [run]}, fn {previous, index}, {[], tail} ->
      cond do
        Map.get(previous, :type) == :soft_break ->
          {:halt, {Enum.take(line, index + 1), tail}}

        Map.get(previous, :type) == :inline_positioning_marker or
            (previous.text == "" and
               Map.get(previous, :type) not in [:inline_block, :inline_image]) ->
          {:cont, {[], [previous | tail]}}

        inline_break_boundary?(previous, hd(tail), Enum.take(line, index + 1)) ->
          {:halt, {Enum.take(line, index + 1), tail}}

        Map.get(previous, :type) in [:inline_block, :inline_image] ->
          if Map.get(run.style, :white_space) in [:nowrap, :pre],
            do: {:cont, {[], [previous | tail]}},
            else: {:halt, {Enum.take(line, index + 1), tail}}

        true ->
          last_token = previous.text |> inline_wrap_tokens(previous.style) |> List.last()
          split = byte_size(previous.text) - byte_size(last_token)

          if split > 0 do
            prefix = %{previous | text: binary_part(previous.text, 0, split)}
            suffix = %{previous | text: last_token}
            {:halt, {Enum.take(line, index) ++ [prefix], [suffix | tail]}}
          else
            {:cont, {[], [previous | tail]}}
          end
      end
    end)
  end

  defp append_break_word_token(lines, run, width) do
    run.text
    |> String.graphemes()
    |> Enum.reduce(lines, fn grapheme, acc ->
      append_inline_token(
        acc,
        run |> Map.put(:text, grapheme) |> Map.put(:emergency_break, true),
        width
      )
    end)
  end

  defp append_inline_line_run(line, run) do
    case List.last(line) do
      %{merge_key: merge_key, text: text} ->
        case merge_key == Map.get(run, :merge_key) do
          true -> List.update_at(line, length(line) - 1, &%{&1 | text: text <> run.text})
          false -> line ++ [run]
        end

      _ ->
        line ++ [run]
    end
  end

  defp inline_line_width(line_runs, available_width) do
    Enum.reduce(line_runs, 0.0, fn run, width ->
      width + inline_run_width(run, available_width)
    end)
  end

  @spec inline_runs([term()], map()) ::
          {:ok, [map()]} | {:error, :invalid_layout}
  defp inline_runs(children, parent_style) do
    Enum.reduce_while(children, {:ok, []}, fn child, {:ok, runs} ->
      case append_inline_run(child, runs, parent_style) do
        {:ok, runs} -> {:cont, {:ok, runs}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp append_inline_run(child, runs, parent_style) do
    case child do
      %{type: :element, style: %{_math_box: measured} = style} ->
        run = %{
          type: :inline_block,
          text: "",
          style: style,
          children: [],
          _inline_content_width: measured.width,
          _inline_content_height: measured.ascent + measured.descent,
          _inline_baseline_depth: measured.ascent
        }

        {:ok, runs ++ [run]}

      %{type: :text, text: text, style: style} when is_binary(text) and is_map(style) ->
        text = normalize_inline_whitespace(text, style)
        {:ok, append_whitespace_collapsed_run(runs, %{text: text, style: style})}

      %{type: :element, style: %{display: :inline} = style, children: children} = element
      when is_list(children) ->
        style =
          Map.put(
            style,
            :_inline_available_height,
            Map.get(parent_style, :_inline_available_height)
          )

        case inline_runs(children, style) do
          {:ok, child_runs} ->
            child_runs =
              case Map.get(style, :background_color) do
                color when color not in [nil, :transparent] ->
                  font_size = Map.fetch!(style, :font_size)

                  {ascent, descent} =
                    case text_font_face(style) do
                      %{ascent: ascent, descent: descent, units_per_em: units_per_em} ->
                        {round(font_size * ascent / units_per_em / 0.75) * 0.75,
                         round(-font_size * descent / units_per_em / 0.75) * 0.75}

                      _ ->
                        {font_size, 0.0}
                    end

                  background = %{color: color, ascent: ascent, descent: descent}

                  Enum.map(child_runs, fn run ->
                    backgrounds = [background | Map.get(run, :inline_backgrounds, [])]
                    Map.put(run, :inline_backgrounds, backgrounds)
                  end)

                _ ->
                  child_runs
              end

            child_runs =
              case Map.get(style, :position, :static) do
                :relative ->
                  context = %{
                    id: System.unique_integer([:positive]),
                    style: style,
                    positioned_children: Map.get(element, :positioned_children, [])
                  }

                  case child_runs do
                    [] ->
                      [
                        %{
                          type: :inline_positioning_marker,
                          text: "",
                          style: text_style(style),
                          inline_positioning_contexts: [context]
                        }
                      ]

                    child_runs ->
                      Enum.map(child_runs, fn run ->
                        contexts = [context | Map.get(run, :inline_positioning_contexts, [])]
                        Map.put(run, :inline_positioning_contexts, contexts)
                      end)
                  end

                _ ->
                  child_runs
              end

            child_runs =
              case element do
                %{tag: <<"h", level>>} when level in ?1..?6 ->
                  case outline_heading(element) do
                    nil ->
                      child_runs

                    heading ->
                      anchor = Map.put(heading, :id, make_ref())
                      Enum.map(child_runs, &Map.put(&1, :outline_anchor, anchor))
                  end

                _ ->
                  child_runs
              end

            child_runs =
              case Map.get(style, :html_target) do
                nil ->
                  child_runs

                identity ->
                  child_runs =
                    if child_runs == [],
                      do: [
                        %{
                          type: :inline_positioning_marker,
                          text: "",
                          style: text_style(style),
                          inline_positioning_contexts: []
                        }
                      ],
                      else: child_runs

                  Enum.map(
                    child_runs,
                    &Map.update(&1, :html_target_ids, [identity], fn ids -> [identity | ids] end)
                  )
              end

            {:ok,
             Enum.reduce(child_runs, runs, fn run, acc ->
               case Map.get(run, :type) do
                 type
                 when type in [
                        :inline_image,
                        :inline_block,
                        :inline_positioning_marker,
                        :soft_break
                      ] ->
                   acc ++ [run]

                 _ ->
                   append_whitespace_collapsed_run(acc, run)
               end
             end)}

          {:error, reason} ->
            {:error, reason}
        end

      %{type: :element, style: %{display: display} = style, children: children} = element
      when display in [:inline_block, :inline_flex, :inline_grid] and is_list(children) ->
        style =
          Map.put(style, :_containing_height, Map.get(parent_style, :_inline_available_height))

        style =
          if Map.get(style, :vertical_align) == :text_bottom do
            font = text_font_face(parent_style)

            descent =
              round(
                -font.descent / font.units_per_em * parent_style.font_size / @css_pixel_points
              ) * @css_pixel_points

            Map.put(style, :vertical_align, -descent)
          else
            style
          end

        inline_children =
          if display == :inline_block and not Map.get(style, :_fieldset, false) do
            inline_runs(
              children,
              Map.put(
                style,
                :_inline_available_height,
                resolved_content_size(style, :height, Map.get(style, :_containing_height), nil)
              )
            )
          else
            :container
          end

        run =
          case inline_children do
            {:ok, child_runs} ->
              %{type: :inline_block, text: "", style: style, runs: child_runs}

            _ ->
              %{type: :inline_block, text: "", style: style, children: children}
          end

        run =
          case Map.get(style, :position, :static) do
            :relative ->
              context = %{
                id: System.unique_integer([:positive]),
                style: style,
                positioned_children: Map.get(element, :positioned_children, [])
              }

              Map.put(run, :inline_positioning_contexts, [context])

            _ ->
              run
          end

        run =
          case element do
            %{tag: <<"h", level>>} when level in ?1..?6 ->
              case outline_heading(element) do
                nil -> run
                heading -> Map.put(run, :outline_anchor, Map.put(heading, :id, make_ref()))
              end

            _ ->
              run
          end

        {:ok, runs ++ [run]}

      %{type: :element, style: %{display: :image, image_display: :inline} = style} ->
        parent_x_height =
          if Map.get(style, :vertical_align, :baseline) == :middle do
            case text_font_face(parent_style) do
              %{type: :embedded, cmap: cmap, units_per_em: units} = face ->
                case Map.get(cmap, ?x, 0) do
                  0 ->
                    Map.fetch!(parent_style, :font_size) * 0.5

                  glyph ->
                    case Font.glyph_bounds(face) do
                      {:ok, bounds} ->
                        {_left, _bottom, _right, top} = elem(bounds, glyph)
                        top / units * Map.fetch!(parent_style, :font_size)

                      {:error, {_reason, _diagnostic}} = error ->
                        throw({:render_resource_limit, error})
                    end
                end

              _ ->
                Map.fetch!(parent_style, :font_size) * 0.5
            end
          else
            nil
          end

        {:ok,
         runs ++
           [
             %{
               type: :inline_image,
               text: "",
               _inline_parent_x_height: parent_x_height,
               style: style,
               _inline_available_height: Map.get(parent_style, :_inline_available_height)
             }
           ]}

      %{type: :element, style: %{display: :none}} ->
        {:ok, runs}

      %{type: :element, style: %{display: :soft_break}, children: []} ->
        {:ok, runs ++ [%{type: :soft_break, text: "", style: text_style(parent_style)}]}

      %{type: :element, style: %{display: :line_break} = style, children: []} ->
        {:ok, runs ++ [%{text: "\n", style: text_style(style), hard_break: true}]}

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp inline_run_width(run, available_width) do
    case run do
      %{type: :inline_image, style: style} ->
        margin = Map.get(style, :margin, edges(0.0))

        {content_width, _height} =
          image_content_size(style, available_width, Map.get(run, :_inline_available_height))

        Map.get(run, :_inline_content_width, content_width) + horizontal_box_size(style) +
          margin.left + margin.right

      %{type: :inline_block, style: style} ->
        margin = Map.get(style, :margin, edges(0.0))

        inline_block_content_width(run, available_width) + horizontal_box_size(style) +
          margin.left + margin.right

      %{type: type} when type in [:inline_positioning_marker, :soft_break] ->
        0.0

      _ ->
        text_width(run.text, run.style)
    end
  end

  # Fixed browser layout precision applies before an atomic inline box is
  # measured, aligned to its line, and painted. Intrinsic sizes retain their
  # existing font measurements.
  defp inline_content_size(style, property, available_size, default) do
    size = resolved_content_size(style, property, available_size, default)

    if is_number(Map.get(style, property)) and is_number(size),
      do: Float.floor(size / @css_pixel_points * 64) / 64 * @css_pixel_points,
      else: size
  end

  defp inline_block_content_width(run, available_width) do
    case run do
      %{_inline_content_width: width} ->
        width

      %{style: %{svg_scene: _} = style} ->
        {width, _height} = svg_content_size(style, available_width)
        width

      _ ->
        style = Map.fetch!(run, :style)

        default_width =
          case run do
            %{runs: runs} ->
              runs
              |> inline_lines(nil, false)
              |> Enum.map(&inline_line_width(&1, available_width))
              |> Enum.max(fn -> 0.0 end)

            %{children: children} ->
              intrinsic_width =
                case Map.get(style, :display) do
                  :inline_grid ->
                    {:ok, items} = grid_items(children)
                    placed = place_grid_items(items, style)
                    count = grid_axis_count(placed, style, :column)
                    tracks = grid_tracks(style, :column, count)
                    intrinsics = grid_column_intrinsics(placed, count)

                    sizes =
                      resolve_grid_columns(
                        tracks,
                        intrinsics,
                        Enum.sum(intrinsics),
                        grid_column_gap(style)
                      )

                    grid_tracks_size(sizes, grid_column_gap(style))

                  :inline_flex ->
                    Enum.sum(Enum.map(children, &flex_child_intrinsic_width/1))

                  _ ->
                    flex_block_intrinsic_width(children)
                end

              margin = Map.get(style, :margin, edges(0.0))

              case available_width do
                width when is_number(width) ->
                  available_content_width =
                    max(
                      width - horizontal_box_size(style) - margin.left - margin.right,
                      0.0
                    )

                  min(intrinsic_width, available_content_width)

                _ ->
                  intrinsic_width
              end
          end

        inline_content_size(style, :width, available_width, default_width)
    end
  end

  defp inline_block_content_height(run, content_width) do
    case run do
      %{_inline_content_height: height} ->
        height

      %{children: children, style: style} ->
        intrinsic_height =
          case layout_container_content_height(style, children, content_width) do
            {:ok, height} -> height
            {:error, _reason} -> 0.0
          end

        inline_content_size(style, :height, Map.get(style, :_containing_height), intrinsic_height)
    end
  end

  # Measurements are attached to the atomic run and reused by its line and paint.
  # A baseline follows normal flow, independent of the declared content height.
  defp measure_inline_block(run, available_width) do
    style = run.style
    content_width = inline_block_content_width(run, available_width)

    {intrinsic_height, first_baseline, baseline, lines} =
      case run do
        %{style: %{_math_box: measured}} ->
          {measured.ascent + measured.descent, measured.ascent, measured.ascent, nil}

        %{style: %{embedded_document: _surface}} ->
          {0.0, nil, nil, nil}

        %{style: %{svg_scene: _} = style} ->
          {_width, height} = svg_content_size(style, available_width)
          {height, nil, nil, nil}

        %{runs: runs} ->
          lines = inline_lines(runs, content_width)

          has_flow_line =
            Map.get(style, :form_kind) in [:text, :input_button, :select] or
              Enum.any?(lines, &(&1 != [])) or Enum.any?(runs, &String.contains?(&1.text, "\n"))

          height = if has_flow_line, do: inline_content_height(runs, lines, style), else: 0.0

          baseline =
            if not has_flow_line do
              nil
            else
              last = List.last(lines)

              height - inline_line_height(last, style, content_width) +
                inline_line_baseline_depth(last, style)
            end

          {height, if(has_flow_line, do: inline_line_baseline_depth(List.first(lines), style)),
           baseline, lines}

        %{children: children} ->
          key = {__MODULE__, :baseline_measurement}
          previous = Process.get(key)
          reference = make_ref()
          children = inline_baseline_children(children, reference)
          measured_style = Map.put(style, :_measure_inline_baseline, reference)
          Process.put(key, reference)

          try do
            result =
              if style.display in [:inline_flex, :inline_grid] do
                box_width = content_width + horizontal_box_size(style)

                measured_style =
                  measured_style
                  |> Map.put(:margin, edges(0.0))
                  |> Map.put(:width, declared_size_for_border_box(style, :width, box_width))

                with {:ok, boxes, next_y} <-
                       layout_block(
                         %{type: :element, style: measured_style, children: children},
                         0.0,
                         0.0,
                         box_width
                       ) do
                  offset =
                    Map.get(style, :padding, edges(0.0)).top +
                      Map.get(style, :border_widths, edges(0.0)).top

                  boxes =
                    Enum.map(boxes, fn box ->
                      if Map.has_key?(box, :_measured_baseline),
                        do: Map.update!(box, :_measured_baseline, &(&1 + offset)),
                        else: box
                    end)

                  {:ok, boxes, -next_y - vertical_box_size(style)}
                end
              else
                layout_block_content(children, measured_style, 0.0, 0.0, content_width, %{})
              end

            case result do
              {:ok, boxes, height} ->
                first = Enum.find(boxes, &Map.has_key?(&1, :_measured_baseline))
                last = Enum.find(Enum.reverse(boxes), &Map.has_key?(&1, :_measured_baseline))

                {height, if(first, do: -first._measured_baseline, else: nil),
                 if(last, do: -last._measured_baseline, else: nil), nil}

              {:error, _reason} ->
                {0.0, nil, nil, nil}
            end
          after
            if previous == nil, do: Process.delete(key), else: Process.put(key, previous)
          end
      end

    content_height =
      inline_content_size(style, :height, Map.get(style, :_containing_height), intrinsic_height)

    margin = Map.get(style, :margin, edges(0.0))
    padding = Map.get(style, :padding, edges(0.0))
    border = Map.get(style, :border_widths, edges(0.0))

    # TextField controls propagate their inner editor baseline. Their clipping
    # does not trigger ordinary inline-block bottom-edge synthesis.
    baseline =
      if Map.has_key?(style, :_number_state) and baseline != nil,
        do:
          baseline +
            NativeElixirPdfUtilities.Validators.HtmlFormValidator.initial_number_content_offset(
              style,
              content_height
            ),
        else: baseline

    bottom_baseline =
      baseline == nil or
        (Map.get(style, :overflow, :visible) != :visible and
           not Map.has_key?(style, :_number_state))

    first_baseline =
      if bottom_baseline,
        do: content_height + vertical_box_size(style) + margin.bottom,
        else: border.top + padding.top + first_baseline

    baseline =
      if bottom_baseline,
        do:
          content_height + vertical_box_size(style) +
            if(Map.has_key?(style, :_range_state), do: 0.0, else: margin.bottom),
        else: border.top + padding.top + baseline

    baseline =
      case Map.get(style, :vertical_align) do
        offset when is_number(offset) -> baseline + offset
        _ -> baseline
      end

    # Only fractional bottom-edge baselines need additional snapping metadata.
    # Ordinary content baselines and integral pixel depths retain their old shape.
    snap_bottom_baseline =
      if bottom_baseline do
        depth_pixels = (margin.top + baseline) / @css_pixel_points
        depth_pixels != Float.floor(depth_pixels)
      else
        false
      end

    run =
      if snap_bottom_baseline,
        do: Map.put(run, :_inline_bottom_baseline, true),
        else: Map.delete(run, :_inline_bottom_baseline)

    run
    |> Map.put(:_inline_content_width, content_width)
    |> Map.put(:_inline_content_height, content_height)
    |> Map.put(:_inline_baseline_depth, baseline)
    |> Map.put(:_inline_first_baseline_depth, first_baseline)
    |> Map.put(:_inline_lines, lines)
  end

  # Relative paint offsets and positioned descendants do not create flow lines.
  # The measurement copy carries this flag and never becomes rendered artwork.
  defp inline_baseline_children(children, reference) do
    Enum.map(children, fn child ->
      child = Map.delete(child, :positioned_children)

      style = Map.put(child.style, :_measure_inline_baseline, reference)

      style =
        if Map.get(style, :position) == :relative,
          do: Map.put(style, :position, :static),
          else: style

      child = Map.put(child, :style, style)

      case child do
        %{children: children} ->
          Map.put(child, :children, inline_baseline_children(children, reference))

        _ ->
          child
      end
    end)
  end

  defp normalize_inline_whitespace(text, style) do
    normalized_newlines = String.replace(text, ~r/\r\n?|\n/u, "\n")

    case Map.get(style, :white_space, :normal) do
      mode when mode in [:pre, :pre_wrap, :break_spaces] ->
        normalized_newlines

      :pre_line ->
        normalized_newlines
        |> String.split("\n", trim: false)
        |> Enum.map(&String.replace(&1, ~r/[ \t\f]+/u, " "))
        |> Enum.join("\n")

      _ ->
        String.replace(normalized_newlines, ~r/[ \t\n\f]+/u, " ")
    end
  end

  defp append_whitespace_collapsed_run(runs, %{text: text} = run) do
    previous =
      case List.last(runs) do
        %{type: :soft_break} ->
          Enum.find(Enum.reverse(runs), &(Map.get(&1, :type) != :soft_break))

        previous ->
          previous
      end

    text =
      case previous do
        %{text: previous_text, style: previous_style}
        when run.style.white_space not in [:pre, :pre_wrap, :break_spaces] and
               previous_style.white_space not in [:pre, :pre_wrap, :break_spaces] ->
          case Regex.match?(~r/[ \t\n\f\r]$/u, previous_text) do
            true -> String.replace(text, ~r/^[ \t\f]+/u, "")
            false -> text
          end

        _ ->
          text
      end

    runs ++ [%{run | text: text}]
  end

  defp trim_inline_whitespace(text, context) do
    table_context? =
      case context do
        true -> true
        %{_table_text: true} -> true
        _ -> false
      end

    if table_context? and text != "" and :binary.last(text) not in [32, 9, 10, 12, 13] and
         :binary.first(text) not in [32, 9, 10, 12, 13] do
      text
    else
      String.replace(text, ~r/^[ \t\n\f\r]+|[ \t\n\f\r]+$/u, "")
    end
  end

  defp trim_leading_inline_whitespace(text) do
    String.replace(text, ~r/^[ \t\n\f\r]+/u, "")
  end

  @spec text_box(%{text: String.t(), style: map()}, number(), number(), number()) :: box()
  defp text_box(run, x, y, width) do
    style = run.style
    font_face = text_font_face(style)

    box = %{
      type: HtmlValidator.reserve_layout_box(:text),
      text: run.text,
      x: x,
      y: y,
      snap_to_css_pixel_grid: true,
      width: width,
      annotation_width: text_width(run.text, style),
      font: Font.pdf_name(font_face),
      font_face: font_face,
      font_size: Map.fetch!(style, :font_size),
      letter_spacing: Map.get(style, :letter_spacing, 0.0),
      line_height: Map.fetch!(style, :line_height),
      color: Map.fetch!(style, :color)
    }

    box =
      case style do
        %{_math_native_group: group, _math_original_characters: characters} ->
          box
          |> Map.put(:_math_group, group)
          |> Map.put(:_math_actual_fragment, true)
          |> Map.put(
            :_math_actual_text,
            run.text |> String.codepoints() |> Enum.map_join(&Map.get(characters, &1, &1))
          )

        _ ->
          box
      end

    box =
      box
      |> Map.put(:form_owner, Map.get(style, :_form_owner))
      |> Map.merge(Map.take(style, [:link_destination]))
      |> Map.put(:initial_option_id, Map.get(style, :_initial_option_id))

    case Map.get(style, :link_url) do
      link_url when is_binary(link_url) -> Map.put(box, :link_url, link_url)
      _ -> box
    end
  end

  # Native gauge artwork shares the normal box path across inline, block, flex,
  # grid and positioned layout. The state is canonical validator output.
  defp background_box(style, x, y, width, height) do
    background_style =
      if Map.has_key?(style, :_range_state) and style.appearance == :auto do
        style
        # Preserve the host rectangle after static embedded transport removes
        # form metadata. Its transparent fill carries geometry without ink.
        |> Map.put(:background_color, {0.0, 0.0, 0.0, 0.0})
        |> Map.put(:background_image, nil)
        |> Map.put(:border_styles, edges(:none))
      else
        style
      end

    background = css_background_box(background_style, x, y, width, height)

    background =
      if Map.has_key?(style, :_range_state) do
        background ++
          NativeElixirPdfUtilities.HtmlToPdf.RangeArtwork.boxes(
            style,
            x,
            y,
            width,
            height
          )
      else
        background
      end

    background =
      if Map.get(style, :form_kind) == :select and width > 0 and height > 0 do
        # NativeThemeBase::PaintMenuList and ThemePainterDefault::SetupMenuListArrow:
        # an open 8px by 4px chevron stroked at 2px in the fixed 16px arrow area.
        arrow = %{
          type: HtmlValidator.reserve_layout_box(:rect),
          x: x + width - style.border_widths.right - 9.0,
          y: y + height / 2 - 1.5,
          width: 6.0,
          height: 3.0,
          border_radius: 0.0,
          fill_color: nil,
          stroke_color:
            if(get_in(style, [:_form_control, :initial_state, :disabled]),
              do: {128 / 255, 128 / 255, 128 / 255},
              else: {0, 0, 0}
            ),
          stroke_width: 1.5,
          shape: {:chevron, :down},
          clip: %{x: x, y: y, width: width, height: height},
          form_owner: Map.get(style, :_form_owner),
          role: :select_arrow
        }

        background ++ [arrow]
      else
        background
      end

    case Map.get(style, :_gauge) do
      nil ->
        background

      gauge ->
        padding = Map.get(style, :padding, edges(0.0))
        border = Map.get(style, :border_widths, edges(0.0))

        content_width =
          max(width - padding.left - padding.right - border.left - border.right, 0.0)

        content_height =
          max(height - padding.top - padding.bottom - border.top - border.bottom, 0.0)

        content_x = x + border.left + padding.left
        content_y = y + border.bottom + padding.bottom

        background ++
          NativeElixirPdfUtilities.HtmlToPdf.GaugeArtwork.boxes(
            gauge,
            style,
            content_x,
            content_y,
            content_width,
            content_height
          )
    end
  end

  @spec background_box(map(), number(), number(), number(), number()) :: [box()]
  defp css_background_box(style, x, y, width, height) do
    border_widths = Map.get(style, :border_widths, edges(0.0))
    border_styles = Map.get(style, :border_styles, edges(:solid))

    border_colors =
      Map.get(style, :border_colors, edges(Map.get(style, :border_color, {0, 0, 0})))

    stroke_width = Enum.max(Map.values(border_widths))
    fill_color = Map.get(style, :background_color)

    border_visible? = visible_border?(border_widths, border_styles, border_colors)

    rect = %{
      type: HtmlValidator.reserve_layout_box(:rect),
      paint_layer: :container_background,
      snap_to_css_pixel_grid: true,
      x: x,
      y: y,
      width: width,
      height: height,
      fill_color: fill_color,
      stroke_color: Map.get(style, :border_color, {0, 0, 0}),
      stroke_width: stroke_width,
      border_widths: border_widths,
      border_colors: border_colors,
      border_styles: border_styles,
      border_radius: Map.get(style, :border_radius, 0.0)
    }

    rect =
      rect
      |> Map.put(:form_control, Map.get(style, :_form_control))
      |> Map.put(:form_owner, Map.get(style, :_form_owner))
      |> Map.put(:initial_option_id, Map.get(style, :_initial_option_id))
      |> then(fn rect ->
        cond do
          Map.get(style, :form_kind) == :listbox ->
            Map.put(rect, :role, :native_listbox)

          Map.get(style, :_native_menu, false) and style.border_radius == 0 ->
            # NativeThemeBase paints a 2px radius on a half-border inset.
            # The native 1px non-AA stroke passes through ScopedRasterFlags
            # at the fixed 0.75 CSS-to-point scale: hairline, alpha *= 0.75.
            alpha =
              if Map.has_key?(style, :_number_state) and
                   get_in(style, [:_form_control, :initial_state, :disabled]),
                 do: 0.3 * @css_pixel_points,
                 else: @css_pixel_points

            color = {118 / 255, 118 / 255, 118 / 255, alpha}

            %{
              rect
              | border_radius: 1.5 + @css_pixel_points / 2,
                fill_color: if(fill_color, do: {1.0, 1.0, 1.0}, else: nil),
                stroke_width: if(border_visible?, do: @css_pixel_points, else: 0.0),
                stroke_color: color,
                border_colors: edges(color)
            }
            |> Map.put(:role, :native_menu)

          Map.get(style, :form_kind) == :select ->
            Map.put(rect, :role, :css_menu)

          true ->
            rect
        end
      end)

    case {width > 0 and height > 0, Map.get(style, :background_image)} do
      {false, _image} ->
        []

      {true, nil} ->
        case {fill_color, border_visible?} do
          {nil, false} ->
            cond do
              Map.get(style, :_form_control) ->
                [rect]

              Map.has_key?(style, :_initial_option_id) ->
                [%{rect | fill_color: {0.0, 0.0, 0.0, 0.0}}]

              true ->
                []
            end

          _ ->
            [rect]
        end

      {true, image} ->
        fill_boxes =
          case fill_color do
            nil ->
              []

            _ ->
              [
                %{rect | stroke_color: nil, stroke_width: 0.0, border_widths: edges(0.0)}
              ]
          end

        border_boxes =
          case border_visible? do
            true -> [%{rect | fill_color: nil}]
            false -> []
          end

        fill_boxes ++
          background_image_boxes(style, image, x, y, width, height, border_widths) ++
          border_boxes
    end
  end

  # A validated zero extent preserves layout space but has no image paint area.
  # Share this decision between normal and flex/grid image placement.
  defp fitted_image_boxes(style, x, y, width, height, image) do
    if width > 0 and height > 0,
      do: [fitted_image_box(style, x, y, width, height, image)],
      else: []
  end

  defp fitted_image_box(style, viewport_x, viewport_y, viewport_width, viewport_height, image) do
    case Map.get(style, :object_fit, :fill) do
      :fill ->
        %{
          type: HtmlValidator.reserve_layout_box(:image),
          snap_to_css_pixel_grid: true,
          x: viewport_x,
          y: viewport_y,
          width: viewport_width,
          height: viewport_height,
          image: image
        }

      fit when fit in [:contain, :cover, :none, :scale_down] ->
        {natural_width, natural_height} =
          if image.height == 0 do
            # Density correction can zero natural dimensions without removing
            # the decoded image's intrinsic aspect ratio.
            {image.width_px * 0.75, image.height_px * 0.75}
          else
            {image.width, image.height}
          end

        scale =
          case fit do
            :contain ->
              min(viewport_width / natural_width, viewport_height / natural_height)

            :cover ->
              max(viewport_width / natural_width, viewport_height / natural_height)

            :none ->
              1.0

            :scale_down ->
              min(1.0, min(viewport_width / natural_width, viewport_height / natural_height))
          end

        rendered_width = natural_width * scale
        rendered_height = natural_height * scale

        {horizontal, vertical} =
          Map.get(style, :object_position, {{:percent, 0.5}, {:percent, 0.5}})

        %{
          type: HtmlValidator.reserve_layout_box(:image),
          snap_to_css_pixel_grid: true,
          x: viewport_x + paint_position(horizontal, viewport_width - rendered_width),
          y:
            viewport_y + viewport_height - rendered_height -
              paint_position(vertical, viewport_height - rendered_height),
          width: rendered_width,
          height: rendered_height,
          image: image,
          clip: %{x: viewport_x, y: viewport_y, width: viewport_width, height: viewport_height}
        }
    end
  end

  defp background_image_boxes(style, image, x, y, width, height, border_widths) do
    area_x = x + border_widths.left
    area_y = y + border_widths.bottom
    area_width = max(width - border_widths.left - border_widths.right, 0.0)
    area_height = max(height - border_widths.top - border_widths.bottom, 0.0)
    {tile_width, tile_height} = background_image_size(style, image, area_width, area_height)
    {horizontal, vertical} = Map.get(style, :background_position)
    initial_x = area_x + paint_position(horizontal, area_width - tile_width)

    initial_y =
      area_y + area_height - tile_height - paint_position(vertical, area_height - tile_height)

    repeat = Map.get(style, :background_repeat, :repeat)

    with true <- tile_width > 0 and tile_height > 0,
         :ok <-
           HtmlValidator.validate_background_image_tile_dimensions(
             tile_width,
             tile_height,
             width,
             height,
             repeat
           ),
         x_plan =
           tile_plan(initial_x, tile_width, x, x + width, repeat in [:repeat, :repeat_x]),
         y_plan =
           tile_plan(initial_y, tile_height, y, y + height, repeat in [:repeat, :repeat_y]),
         :ok <-
           HtmlValidator.validate_background_image_tile_count(x_plan.count * y_plan.count) do
      x_positions = tile_positions(x_plan)
      y_positions = tile_positions(y_plan)

      for tile_x <- x_positions, tile_y <- y_positions do
        %{
          type: HtmlValidator.reserve_layout_box(:image),
          paint_layer: :container_background,
          snap_to_css_pixel_grid: true,
          x: tile_x,
          y: tile_y,
          width: tile_width,
          height: tile_height,
          image: image,
          clip: %{x: x, y: y, width: width, height: height}
        }
      end
    else
      false ->
        []

      {:error, {_reason, _diagnostic}} = error ->
        throw({:render_resource_limit, error})

      _error ->
        [%{type: :layout_error, reason: :background_image_tile_limit}]
    end
  end

  defp background_image_size(style, image, area_width, area_height) do
    natural_width = Map.fetch!(image, :width)
    natural_height = Map.fetch!(image, :height)

    case Map.get(style, :background_size, {:auto, :auto}) do
      :contain ->
        scale = min(area_width / natural_width, area_height / natural_height)
        {natural_width * scale, natural_height * scale}

      :cover ->
        scale = max(area_width / natural_width, area_height / natural_height)
        {natural_width * scale, natural_height * scale}

      {width, height} ->
        resolved_width = resolve_background_size(width, area_width)
        resolved_height = resolve_background_size(height, area_height)

        case {resolved_width, resolved_height} do
          {:auto, :auto} -> {natural_width, natural_height}
          {width, :auto} -> {width, width * natural_height / natural_width}
          {:auto, height} -> {height * natural_width / natural_height, height}
          {width, height} -> {width, height}
        end
    end
  end

  defp resolve_background_size(size, available) do
    case size do
      :auto -> :auto
      size -> resolve_size_value(size, available)
    end
  end

  defp paint_position(position, remaining) do
    case position do
      {:percent, ratio} -> remaining * ratio
      length -> length
    end
  end

  defp tile_plan(initial, tile_size, minimum, maximum, repeat?) do
    case repeat? and tile_size > 0 do
      true ->
        first = initial - max(Float.ceil((initial - minimum) / tile_size), 0) * tile_size
        count = max(Float.ceil((maximum - first) / tile_size) |> trunc(), 1)
        %{first: first, step: tile_size, count: count}

      false ->
        %{first: initial, step: tile_size, count: 1}
    end
  end

  defp tile_positions(%{first: first, step: step, count: count}) do
    Enum.map(0..(count - 1), &(first + &1 * step))
  end

  defp visible_border?(border_widths, border_styles, border_colors) do
    Enum.any?([:top, :right, :bottom, :left], fn side ->
      Map.fetch!(border_widths, side) > 0 and
        Map.fetch!(border_styles, side) not in [:none, :hidden] and
        not is_nil(Map.fetch!(border_colors, side))
    end)
  end

  defp tag_boxes(boxes, metadata) do
    Enum.map(boxes, &Map.merge(&1, metadata))
  end

  defp tag_atomic_boxes(boxes, metadata) do
    Enum.map(boxes, fn box ->
      box
      |> Map.drop([:fragment_id, :fragment_lines])
      |> Map.merge(metadata)
    end)
  end

  defp break_metadata(style) do
    %{
      break_before: Map.get(style, :break_before, :auto),
      break_after: Map.get(style, :break_after, :auto),
      break_inside: Map.get(style, :break_inside, :auto)
    }
  end

  @spec text_width(String.t(), map()) :: number()
  defp text_width(text, style) do
    HtmlValidator.reserve_render_resource(:max_layout_text_work, byte_size(text), :layout)

    text
    |> Font.text_width(
      text_font_face(style),
      Map.fetch!(style, :font_size),
      Map.get(style, :letter_spacing, 0)
    )
    |> Kernel.+(letter_spacing_width(text, style))
  end

  defp letter_spacing_width(text, style) do
    letter_spacing = Map.get(style, :letter_spacing, 0.0)

    case letter_spacing == 0.0 do
      true -> 0.0
      false -> max(String.length(text) - 1, 0) * letter_spacing
    end
  end

  defp text_font_face(style) do
    Map.fetch!(style, :font_face)
  end

  defp text_style(style) do
    Map.take(style, [
      :_text_decorations,
      :color,
      :font_face,
      :font_families,
      :font_family,
      :font_size,
      :font_style,
      :font_weight,
      :letter_spacing,
      :line_break,
      :overflow_wrap,
      :word_break,
      :line_height,
      :link_url,
      :link_destination,
      :text_align,
      :white_space
    ])
  end

  defp edges(value) do
    edges(value, value, value, value)
  end

  defp edges(top, right, bottom, left) do
    %{top: top, right: right, bottom: bottom, left: left}
  end
end
