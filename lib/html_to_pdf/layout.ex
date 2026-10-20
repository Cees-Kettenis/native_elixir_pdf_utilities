defmodule NativeElixirPdfUtilities.HtmlToPdf.Layout do
  alias NativeElixirPdfUtilities.Validators.NativeGlyphSourceValidator

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
  alias NativeElixirPdfUtilities.HtmlToPdf.MathLayout
  alias NativeElixirPdfUtilities.HtmlToPdf.ResourceContext
  alias NativeElixirPdfUtilities.HtmlToPdf.EmbeddedViewport
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgScene
  alias NativeElixirPdfUtilities.HtmlToPdf.CounterStyle
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.EmbeddedViewportValidator
  alias NativeElixirPdfUtilities.Validators.HtmlTableValidator
  alias NativeElixirPdfUtilities.Validators.CssTransformValidator

  @css_pixel_points 0.75

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
    HtmlTableValidator.with_layout_context(:paginated, fn ->
      layout_document(styled_tree, opts, nil)
    end)
  end

  @doc "Resolves issued lazy image candidates and SVG viewports from the surviving final layout boxes."
  @spec resolve_images(layout_tree()) :: {:ok, layout_tree()} | {:error, {atom(), map()}}
  def resolve_images(layout) do
    with {:ok, boxes} <- resolve_image_boxes(layout.boxes), do: {:ok, %{layout | boxes: boxes}}
  end

  # Measurements can create discarded boxes. Traversing the final paint tree
  # prevents those preliminary widths from fetching unused responsive candidates.
  defp resolve_image_boxes(boxes) do
    Enum.reduce_while(boxes, {:ok, []}, fn box, {:ok, result} ->
      resolved =
        case box do
          %{role: :responsive_image, responsive_style: style} ->
            with {:ok, style} <-
                   NativeElixirPdfUtilities.HtmlToPdf.Style.resolve_image(style, box.width) do
              paint = fitted_image_boxes(style, box.x, box.y, box.width, box.height, style.image)

              metadata =
                Map.drop(box, [:type, :role, :responsive_style, :x, :y, :width, :height, :clip])

              paint =
                case Map.get(box, :clip) do
                  nil ->
                    paint

                  clip ->
                    NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.clip_boxes(paint, clip)
                end

              resolve_image_boxes(Enum.map(paint, &Map.merge(&1, metadata)))
            end

          %{type: :image, image: %{svg_viewport: descriptor}} when is_map(descriptor) ->
            with {:ok, image} <-
                   NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer.resize_image(
                     box.image,
                     box.width,
                     box.height
                   ) do
              resolve_image_boxes([%{box | image: image}])
            end

          %{type: :image, _image_map_geometry: geometry} ->
            viewport = %{
              x: box.x + geometry.dx,
              y: box.y + geometry.dy,
              width: geometry.width,
              height: geometry.height
            }

            regions =
              NativeElixirPdfUtilities.HtmlToPdf.ImageMapGeometry.regions(
                geometry.model,
                viewport,
                box
              )

            metadata =
              Map.take(box, [
                :flow_id,
                :out_of_flow,
                :position_anchor,
                :break_inside,
                :line_id,
                :css_transform_scopes
              ])

            {:ok,
             [Map.delete(box, :_image_map_geometry) | Enum.map(regions, &Map.merge(&1, metadata))]}

          %{boxes: children} ->
            with {:ok, children} <- resolve_image_boxes(children),
                 do: {:ok, [%{box | boxes: children}]}

          _ ->
            {:ok, [box]}
        end

      case resolved do
        {:ok, paint} -> {:cont, {:ok, [paint | result]}}
        {:error, _} = error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, parts} -> {:ok, parts |> Enum.reverse() |> List.flatten()}
      error -> error
    end
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

          HtmlTableValidator.with_layout_context(:surface, fn ->
            layout_document(styled_tree, opts, viewport)
          end)
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
          NativeElixirPdfUtilities.Validators.HtmlFormValidator.with_layout_mode(
            Keyword.get(
              opts,
              :forms,
              NativeElixirPdfUtilities.Validators.HtmlFormValidator.layout_mode()
            ),
            fn ->
              styled_tree =
                styled_tree |> collapse_child_margins() |> attach_positioned_descendants()

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

              canvas =
                case HtmlValidator.layout_canvas(styled_tree) do
                  {:ok, canvas} -> canvas
                  error -> throw({:render_resource_limit, error})
                end

              canvas_boxes =
                background_box(canvas.style, 0, 0, elem(page_size, 0), elem(page_size, 1))
                |> Enum.map(&Map.put(&1, :position_anchor, :canvas))

              root_style = Map.get(styled_tree, :document_root_sizing) || %{}
              root_margin = Map.get(root_style, :margin, edges(0.0))
              root_padding = Map.get(root_style, :padding, edges(0.0))
              root_border = Map.get(root_style, :border_widths, edges(0.0))
              available_width = elem(layout_page_size, 0) - margins.left - margins.right

              root_width =
                resolved_content_size(
                  root_style,
                  :width,
                  available_width,
                  max(
                    available_width - root_margin.left - root_margin.right -
                      horizontal_box_size(root_style),
                    0.0
                  )
                )

              root_box_width = root_width + horizontal_box_size(root_style)

              root_box_x =
                margins.left + root_margin.left +
                  NativeElixirPdfUtilities.Validators.CssSizingValidator.auto_margin_offset(
                    root_style,
                    :width,
                    available_width - root_margin.left - root_margin.right - root_box_width,
                    :flow
                  )

              root_box_top = elem(page_size, 1) - margins.top - root_margin.top
              root_content_x = root_box_x + root_padding.left + root_border.left
              root_content_top = root_box_top - root_padding.top - root_border.top

              root_height =
                resolved_content_size(
                  root_style,
                  :height,
                  elem(page_size, 1) - margins.top - margins.bottom,
                  nil
                )

              root_layout_margins = %{
                margins
                | left: root_content_x,
                  right: elem(layout_page_size, 0) - root_content_x - root_width,
                  top: elem(page_size, 1) - root_content_top
              }

              children =
                Enum.map(styled_tree.children, fn node ->
                  case node do
                    %{tag: "body", style: style} ->
                      style =
                        if(canvas.body_background_propagated,
                          do:
                            style
                            |> Map.put(:background_color, nil)
                            |> Map.put(:background_image, nil),
                          else: style
                        )
                        |> Map.put(
                          :_root_body_fragmentation,
                          Map.get(node, :namespace, :html) == :html
                        )

                      style =
                        if Map.get(styled_tree, :document_root_sizing) != nil,
                          do: Map.put(style, :_containing_height, root_height),
                          else: style

                      %{node | style: style}

                    _ ->
                      node
                  end
                end)

              positioned_children = Map.get(styled_tree, :positioned_children, [])

              with {:ok, boxes, flow_bottom} <-
                     layout_blocks(children, layout_page_size, root_layout_margins),
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
                   false <- Enum.any?(boxes, &(Map.get(&1, :type) == :layout_error)),
                   boxes = paint_inline_background_images(boxes),
                   {:ok, boxes} <-
                     NativeElixirPdfUtilities.Validators.MathPaintValidator.prepare_flow_boxes(
                       boxes,
                       elem(page_size, 0),
                       elem(page_size, 1)
                     ) do
                root_paint_height =
                  resolved_content_size(
                    root_style,
                    :height,
                    elem(page_size, 1) - margins.top - margins.bottom,
                    max(root_content_top - flow_bottom, 0.0)
                  ) + vertical_box_size(root_style)

                root_boxes =
                  background_box(
                    root_style,
                    root_box_x,
                    root_box_top - root_paint_height,
                    root_box_width,
                    root_paint_height
                  )

                root_targets = Map.get(styled_tree, :link_document_targets, [])

                root_content_boxes = root_boxes ++ group_math_glyphs(boxes)

                {page_width, page_height} = layout_page_size
                link_metadata = Map.take(styled_tree, [:link_scope, :link_expected_targets])

                link_metadata =
                  case root_targets do
                    [] ->
                      link_metadata

                    targets ->
                      Map.put(
                        link_metadata,
                        :link_document_targets,
                        Enum.map(
                          targets,
                          &Map.merge(&1, %{x: root_box_x, y: root_box_top})
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
                     boxes: canvas_boxes ++ root_content_boxes,
                     content_width: page_width - margins.left - margins.right,
                     content_height:
                       max(
                         page_height - margins.top - margins.bottom,
                         page_height - margins.top - flow_bottom
                       )
                   },
                   link_metadata
                 )}
              else
                true -> {:error, :invalid_layout}
                {:error, reason} -> {:error, reason}
              end
            end
          )

        {:error, reason} ->
          {:error, reason}
      end
    end)
  catch
    {:render_resource_limit, {:error, _} = error} -> error
  end

  # Inline slice backgrounds use a single continuous source strip, even when
  # wrapping or a block descendant separates the actual painted fragments.
  # Prepare the strip once and project only intersecting tiles into each piece.
  defp paint_inline_background_images(boxes) do
    if Enum.any?(boxes, &(Map.has_key?(&1, :inline_background_image) or Map.has_key?(&1, :boxes))) do
      paint =
        boxes
        |> Enum.filter(
          &match?(%{type: :rect, role: :inline_background, inline_background_image: _}, &1)
        )
        |> Enum.group_by(& &1.inline_background_image.id)
        |> Enum.reduce(%{}, fn {_id, fragments}, paint ->
          first = hd(fragments)
          style = first.inline_background_image.style
          width = Enum.reduce(fragments, 0.0, &(&1.width + &2))
          HtmlValidator.check_render_resource(:max_css_numeric_magnitude, width, :layout)
          border = Map.get(style, :border_widths, edges(0.0))

          tiles =
            background_image_boxes(
              style,
              style.background_image,
              0.0,
              0.0,
              width,
              first.height,
              border
            )

          # Adjacent runs form one painted inline fragment. Clipping every word
          # independently creates raster seams and restarts the source pattern.
          bands =
            Enum.reduce(fragments, [], fn fragment, bands ->
              case bands do
                [[previous | _] = band | rest] ->
                  if approximately_equal?(previous.x + previous.width, fragment.x) and
                       approximately_equal?(previous.y, fragment.y) and
                       approximately_equal?(previous.height, fragment.height),
                     do: [[fragment | band] | rest],
                     else: [[fragment] | bands]

                [] ->
                  [[fragment]]
              end
            end)
            |> Enum.reverse()
            |> Enum.map(&Enum.reverse/1)

          {paint, _offset} =
            Enum.reduce(bands, {paint, 0.0}, fn band, {paint, offset} ->
              first = hd(band)
              band_width = Enum.reduce(band, 0.0, &(&1.width + &2))
              bounds = %{x: first.x, y: first.y, width: band_width, height: first.height}

              metadata =
                Map.drop(first, [
                  :type,
                  :x,
                  :y,
                  :width,
                  :height,
                  :fill_color,
                  :stroke_color,
                  :stroke_width,
                  :border_radius,
                  :clip,
                  :inline_background_image
                ])

              fragment_border = %{
                border
                | left: if(offset == 0, do: border.left, else: 0.0),
                  right:
                    if(approximately_equal?(offset + band_width, width),
                      do: border.right,
                      else: 0.0
                    )
              }

              plain =
                style
                |> Map.put(:background_image, nil)
                |> Map.put(:border_widths, fragment_border)

              fill =
                background_box(
                  Map.put(plain, :border_widths, edges(0.0)),
                  first.x,
                  first.y,
                  band_width,
                  first.height
                )

              borders =
                background_box(
                  Map.put(plain, :background_color, nil),
                  first.x,
                  first.y,
                  band_width,
                  first.height
                )

              images =
                tiles
                |> shift_layout_boxes(first.x - offset, first.y)
                |> Enum.filter(fn tile ->
                  tile.x < first.x + band_width and tile.x + tile.width > first.x and
                    tile.y < first.y + first.height and tile.y + tile.height > first.y
                end)
                |> NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.clip_boxes(bounds)

              prepared = Enum.map(fill ++ images ++ borders, &Map.merge(&1, metadata))

              prepared =
                case Map.get(first, :clip) do
                  nil ->
                    prepared

                  clip ->
                    NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.clip_boxes(prepared, clip)
                end

              HtmlValidator.reserve_render_resource(:max_layout_boxes, length(images), :layout)

              paint =
                Enum.reduce(band, paint, fn fragment, paint ->
                  Map.put(
                    paint,
                    fragment.inline_background_image.fragment,
                    if(fragment == first, do: prepared, else: [])
                  )
                end)

              {paint, offset + band_width}
            end)

          paint
        end)

      Enum.flat_map(boxes, fn box ->
        case box do
          %{type: :rect, role: :inline_background, inline_background_image: %{fragment: fragment}} ->
            Map.fetch!(paint, fragment)

          %{boxes: children} ->
            [%{box | boxes: paint_inline_background_images(children)}]

          _ ->
            [box]
        end
      end)
    else
      boxes
    end
  end

  defp group_math_glyphs(boxes) do
    {groups, _seen} =
      boxes
      |> Enum.chunk_by(&Map.get(&1, :_math_group))
      |> Enum.map_reduce(MapSet.new(), fn boxes, seen ->
        case hd(boxes) do
          %{_math_group: group} = first ->
            bounds = Enum.map(boxes, &PageGeometry.box_vertical_bounds/1)
            top = Enum.max(Enum.map(bounds, &elem(&1, 0)))
            bottom = Enum.min(Enum.map(bounds, &elem(&1, 1)))

            source = Map.get(first, :_math_actual_source)

            actual =
              if is_binary(source) do
                if MapSet.member?(seen, group), do: "", else: source
              else
                # Pure Math glyphs carry the complete token's logical source.
                # Native fragments always retain their original binary node
                # source, including through fallback and inline splitting.
                first._math_actual_text
              end

            seen = if is_binary(source), do: MapSet.put(seen, group), else: seen

            {[
               first
               |> Map.drop([
                 :text,
                 :font_face,
                 :glyph_ids,
                 :math_source_fragments,
                 :native_glyph_runs,
                 :displayed_unicode,
                 :css_source_positioned,
                 :_math_group,
                 :_math_actual_source,
                 :link_destination,
                 :link_url
               ])
               |> Map.update(:html_targets, [], fn targets ->
                 Enum.map(targets, &%{&1 | dy: &1.dy + first.y - bottom})
               end)
               |> Map.merge(%{
                 type: :glyph_group,
                 actual_text: actual,
                 y: bottom,
                 height: top - bottom,
                 boxes: Enum.map(boxes, &Map.delete(&1, :_math_group))
               })
             ], seen}

          _ ->
            {boxes, seen}
        end
      end)

    Enum.flat_map(groups, & &1)
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
  Ordinary block boxes use their allocated containing width, while their
  preferred widths use the shared CSS intrinsic and min/max sizing rules with
  an indefinite percentage basis. Authored box sizing applies to both axes.
  `first_alphabetic_baseline` records the first content baseline relative to the
  atomic alphabetic baseline, for MathML Core block-box composition.
  `typographic_width` records the unpositioned final line endpoint: occupied
  predecessor fragments followed by the final fragment's actual advance.
  Relative offsets and transforms affect paint, independently of this extent.
  The optional `:formatting_context` is `:flow` by default. `:math`, `:flex`
  and `:grid` use CSS's block-container box for authored inline flow items;
  the source node and its computed display remain unchanged.
  """
  @spec measure_child(term(), term(), term()) ::
          {:ok, map()} | {:error, {atom(), NativeElixirPdfUtilities.Diagnostics.diagnostic()}}
  def measure_child(node, context, constraints \\ %{}) do
    try do
      HtmlTableValidator.with_layout_context(:surface, fn ->
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

              {children, positioned} = collect_positioned_children(Map.get(node, :children, []))

              node =
                node
                |> Map.put(:children, children)
                |> maybe_attach_positioned_children(positioned)

              deferred =
                if Map.get(node.style, :position, :static) in [:relative, :absolute, :fixed],
                  do: [],
                  else: positioned

              tree = %{type: :document, children: [node]}

              with {:ok, %{children: [node]}} <-
                     NativeElixirPdfUtilities.HtmlToPdf.FontFallback.resolve(
                       tree,
                       request.unsupported_glyphs
                     ) do
                with {:ok, measured} <- measure_prepared_child(%{request | node: node}),
                     do: {:ok, Map.put(measured, :positioned_nodes, deferred)}
              end
            end)
          end)
        end
      end)
    catch
      {:render_resource_limit, error} -> error
      {:font_resource_limit, error} -> error
    end
  end

  @doc "Paints canonically validated closed editor text using the ordinary inline line, glyph and clipping geometry."
  @spec paint_source_editor([map()], map(), map()) :: {:ok, map()} | {:error, {atom(), map()}}
  def paint_source_editor(nodes, style, editor) do
    with {:ok, editor} <-
           NativeElixirPdfUtilities.Validators.FormAppearanceValidator.prepare_paint(
             nodes,
             style,
             editor
           ),
         {:ok, runs} <- inline_runs(nodes, style),
         {boxes, _height} =
           inline_text_boxes(runs, style, editor.x, editor.top, editor.clip.width, %{}),
         boxes =
           Enum.filter(boxes, fn box ->
             {top, bottom} = PageGeometry.box_vertical_bounds(box)
             top > editor.clip.y and bottom < editor.clip.y + editor.clip.height
           end),
         boxes = Enum.map(boxes, &Map.put(&1, :clip, editor.clip)),
         frame = %{size: editor.size, boxes: boxes},
         frame =
           if(
             editor.origin.x == 0 and editor.origin.y == 0 and
               {editor.origin.width, editor.origin.height} == editor.size,
             do: frame,
             else: Map.put(frame, :paint_frame, editor.origin)
           ),
         {:ok, _} <-
           NativeElixirPdfUtilities.Validators.WriterValidator.prepare_appearances([frame], 1) do
      {:ok, frame}
    end
  end

  # The same inline run, intrinsic sizing, atomic baseline and paint helpers are
  # used by ordinary document flow. Only the final coordinate origin changes.
  defp measure_prepared_child(request) do
    node = request.node
    style = node.style

    ordinary_box? =
      request.used_display in [:block, :inline_block] and Map.has_key?(node, :children) and
        not Enum.any?(
          [
            :_math_box,
            :_ruby_box,
            :svg_scene,
            :svg_image,
            :image,
            :embedded_document,
            :form_kind
          ],
          &Map.has_key?(style, &1)
        ) and not Map.get(style, :_fieldset, false)

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
      if ordinary_box? and style.display == :block and request.available_width != nil do
        margin = Map.get(style, :margin, edges(0.0))

        available =
          max(
            request.available_width - horizontal_box_size(style) - margin.left - margin.right,
            0.0
          )

        content_width = resolved_content_size(style, :width, request.available_width, available)

        used_width =
          if Map.get(style, :box_sizing, :content_box) == :border_box,
            do: content_width + horizontal_box_size(style),
            else: content_width

        %{node | style: Map.put(style, :width, used_width)}
      else
        node
      end

    node =
      if request.atomic do
        display =
          case request.used_display do
            :block -> :inline_block
            :flex -> :inline_flex
            :grid -> :inline_grid
            other -> other
          end

        %{node | style: Map.put(node.style, :display, display)}
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

      # An atomic wrapper has one advance, but its ordinary children still have
      # independent preferred widths. Reuse the existing CSS intrinsic sizing
      # and box constraint rules with an indefinite percentage basis.
      {minimum, maximum} =
        if ordinary_box? do
          margin = Map.get(style, :margin, edges(0.0))
          edges_width = horizontal_box_size(style) + margin.left + margin.right
          minimum = fieldset_content_width(node.children, style, :min)
          maximum = fieldset_content_width(node.children, style, :max)

          {resolved_content_size(style, :width, nil, minimum) + edges_width,
           resolved_content_size(style, :width, nil, maximum) + edges_width}
        else
          {minimum, maximum}
        end

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

      {boxes, baseline, first_baseline, height, layout_width, typographic_width} =
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
             height, actual_width, actual_width}

          _ ->
            lines =
              if runs == [],
                do: [],
                else: inline_lines(runs, if(request.available_width == nil, do: nil, else: width))

            baseline =
              case lines do
                [] -> 0.0
                _ -> inline_line_baseline_depth(List.first(lines), neutral_style)
              end

            {boxes, height} =
              inline_text_boxes(runs, neutral_style, 0.0, baseline, width, %{}, lines)

            actual_width =
              lines |> Enum.map(&inline_line_width(&1, width)) |> Enum.max(fn -> 0.0 end)

            typographic_width =
              lines
              |> Enum.map(fn line ->
                {predecessors, last} = Enum.split(line, -1)

                Enum.reduce(predecessors, 0.0, fn run, cursor ->
                  cursor + inline_fragment_advance(run, width)
                end) +
                  case last do
                    [run] -> inline_run_width(run, width)
                    [] -> 0.0
                  end
              end)
              |> Enum.max(fn -> 0.0 end)

            {boxes, baseline, 0.0, height, actual_width, typographic_width}
        end

      # Atomic children use the same source-relative positioning as line flow.
      boxes =
        boxes
        |> position_inline_contexts(layout_width)
        |> paint_inline_background_images()
        |> group_math_glyphs()

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
           typographic_width: typographic_width,
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

        with {:ok, child} <-
               measure_prepared_child(%{
                 request
                 | node: wrapper,
                   atomic: true,
                   used_display: :inline_block
               }) do
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
              Map.get(style, :position, :static) in [:absolute, :fixed] or
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

            %{style: %{position: position}} when position in [:absolute, :fixed] ->
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

  @doc false
  @spec collect_positioned_children([map()]) :: {[map()], [map()]}
  def collect_positioned_children(children) do
    {normal, positioned} =
      Enum.reduce(children, {[], []}, fn child, {normal, positioned} ->
        case child do
          %{type: :element, style: %{display: :none}} ->
            {[child | normal], positioned}

          %{type: :element, style: %{display: :soft_break}} ->
            {[child | normal], positioned}

          %{type: :element, children: nested_children, style: style} = element ->
            {nested_children, nested_positioned} = collect_positioned_children(nested_children)

            # A relative/absolute ancestor captures absolute descendants only.
            # A transform establishes the fixed containing block as in CSS.
            {fixed_children, nested_positioned} =
              if Map.get(style, :css_transform, :none) == :none,
                do:
                  Enum.split_with(
                    nested_positioned,
                    &(Map.get(&1.style, :position) == :fixed)
                  ),
                else: {[], nested_positioned}

            inherited_positioned = Enum.reverse(fixed_children, positioned)

            element =
              element
              |> Map.put(:children, nested_children)
              |> maybe_attach_positioned_children(nested_positioned)

            case Map.get(style, :position, :static) do
              position when position in [:absolute, :fixed] ->
                if position == :absolute and Map.get(style, :_dialog, false) do
                  anchor = make_ref()

                  marker = %{
                    type: :element,
                    tag: "::dialog-anchor",
                    children: [],
                    style: %{display: :block, _dialog_anchor: anchor}
                  }

                  {[marker | normal],
                   Enum.reverse(fixed_children, [
                     Map.put(element, :_dialog_anchor, anchor) | positioned
                   ])}
                else
                  {normal, Enum.reverse(fixed_children, [element | positioned])}
                end

              :relative ->
                {[element | normal], inherited_positioned}

              _ ->
                if Map.get(style, :css_transform, :none) != :none,
                  do: {[element | normal], inherited_positioned},
                  else:
                    {[element | normal], Enum.reverse(nested_positioned, inherited_positioned)}
            end

          child ->
            {[child | normal], positioned}
        end
      end)

    {Enum.reverse(normal), Enum.reverse(positioned)}
  end

  defp maybe_attach_positioned_children(element, positioned_children) do
    case {Map.get(element.style, :position, :static), positioned_children} do
      {position, positioned_children} when position in [:relative, :absolute, :fixed] ->
        Map.put(element, :positioned_children, positioned_children)

      {_position, positioned_children} ->
        if Map.get(element.style, :css_transform, :none) != :none,
          do: Map.put(element, :positioned_children, positioned_children),
          else: element
    end
  end

  @spec layout_blocks([term()], {number(), number()}, PageGeometry.margins()) ::
          {:ok, [box()], number()} | {:error, :invalid_layout}
  defp layout_blocks(children, page_size, margins) do
    {page_width, page_height} = page_size

    result =
      Enum.reduce_while(children, {:ok, [], page_height - margins.top}, fn child,
                                                                           {:ok, boxes, y} ->
        result =
          if Map.has_key?(child.style, :_frameset) and child.style.display != :none do
            height = page_height - margins.top - margins.bottom

            with {:ok, group} <-
                   NativeElixirPdfUtilities.HtmlToPdf.Frameset.layout(
                     child,
                     page_width - margins.left - margins.right,
                     height
                   ) do
              group =
                group
                |> Map.put(:x, margins.left)
                |> Map.put(:y, y - height)
                |> Map.put(:transform, {1, 0, 0, 1, margins.left, y - height})

              {:ok, [group], y - height}
            end
          else
            layout_block(child, margins.left, y, page_width - margins.left - margins.right)
          end

        case result do
          {:ok, block_boxes, next_y} -> {:cont, {:ok, Enum.reverse(block_boxes, boxes), next_y}}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)

    case result do
      {:ok, boxes, y} -> {:ok, Enum.reverse(boxes), y}
      {:error, reason} -> {:error, reason}
    end
  end

  @spec layout_block(term(), number(), number(), number()) ::
          {:ok, [box()], number()} | {:error, :invalid_layout}
  defp layout_block(block, x, y, width) do
    block = resolve_fit_content_style(block, width)

    case do_layout_block(block, x, y, width) do
      {:ok, boxes, next_y} ->
        style = block.style
        margin = Map.get(style, :margin, edges(0.0))

        boxes =
          if style.display == :none,
            do: boxes,
            else: attach_html_target(style, boxes, x + margin.left, y - margin.top)

        used_bounds =
          case Map.get(style, :_measure_table_reference) do
            reference when is_reference(reference) ->
              Enum.find_value(boxes, fn box ->
                if Map.get(box, :table_measurement) == reference do
                  {measured_width, measured_height} = box.table_measured_size

                  %{
                    x: box.x,
                    top: box.y + measured_height,
                    width: measured_width,
                    height: measured_height
                  }
                end
              end)

            _ ->
              nil
          end

        case position_container_contents(block, boxes, next_y, x, y, width, used_bounds) do
          {:ok, positioned_boxes, next_y} ->
            with {:ok, positioned_boxes} <-
                   CssTransformValidator.attach_scope(style, positioned_boxes),
                 do: {:ok, attach_outline_anchor(block, positioned_boxes), next_y}

          {:error, reason} ->
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Offsets travel with the existing box through relative movement and pagination.
  # An empty element contributes a non-painting, zero-height flow marker.
  @doc false
  @spec attach_html_target(map(), [map()], number(), number()) :: [map()]
  def attach_html_target(style, boxes, x, top) do
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
    item = Map.put(item, :positioned_children, Map.get(child, :positioned_children, []))

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
      %{type: :element, style: %{_dialog_anchor: anchor}} ->
        {:ok,
         [
           %{
             type: HtmlValidator.reserve_layout_box(:flow_marker),
             x: x,
             y: y,
             width: 0.0,
             height: 0.0,
             _dialog_anchor: anchor
           }
         ], y}

      %{type: :element, style: %{display: :none}} ->
        {:ok, [], y}

      %{type: :element, tag: "ruby", style: %{display: display} = style}
      when display != :inline ->
        atom = %{block | style: Map.put(style, :display, :inline)}
        wrapper = %{block | tag: "div", style: Map.put(style, :display, :block), children: [atom]}
        do_layout_block(wrapper, x, y, width)

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

      %{type: :element, style: %{display: :list_item} = style} ->
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
              if(Map.has_key?(style, :embedded_document),
                do: elem(EmbeddedViewportValidator.intrinsic_size(), 0),
                else: available_box_width - horizontal_box_size(style)
              )
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
            |> Map.put(:fragment_lines, true)
            |> Map.put(:flow_id, {:block, box_x, box_top})
            |> Map.put(:orphans, Map.get(style, :orphans, 2))
            |> Map.put(:widows, Map.get(style, :widows, 2))

          content_metadata =
            flow_metadata
            |> Map.put(:break_before, :auto)
            |> Map.put(:break_after, :auto)

          case layout_popover_contents(
                 children,
                 style,
                 content_x,
                 content_top,
                 content_width,
                 content_metadata
               ) do
            {:ok, content_boxes, content_height, scrollport} ->
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

              content_boxes =
                clip_overflow_contents(
                  content_boxes,
                  style,
                  %{
                    x: content_x - padding.left,
                    y: content_top - content_box_height - padding.bottom,
                    width: content_width + padding.left + padding.right,
                    height: content_box_height + padding.top + padding.bottom
                  },
                  scrollport
                )

              # Root bodies also fragment in the PDF page surface. Preserve
              # their trailing padding with the last content fragment, as the
              # embedded viewport already preserves the complete block extent.
              content_boxes =
                if Map.get(block, :tag) == "body" and
                     Map.get(style, :_root_body_fragmentation, false) and padding.bottom > 0 and
                     Map.get(style, :height) in [nil, :auto] and
                     Map.get(ResourceContext.current() || %{}, :depth, 0) == 0 do
                  case content_boxes
                       |> Enum.with_index()
                       |> Enum.reverse()
                       |> Enum.find(fn {box, _index} ->
                         box.type != :page_break and not Map.get(box, :out_of_flow, false)
                       end) do
                    {%{flow_id: _} = last, index} ->
                      marker =
                        %{
                          type: HtmlValidator.reserve_layout_box(:flow_marker),
                          role: :block_end_padding,
                          x: box_x,
                          y: box_top - box_height,
                          width: 0.0,
                          height: padding.bottom
                        }
                        |> Map.merge(
                          Map.take(last, [:flow_id, :fragment_id, :break_inside, :break_after])
                        )

                      List.insert_at(content_boxes, index + 1, marker)

                    _ ->
                      content_boxes
                  end
                else
                  content_boxes
                end

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

              reserved_content_height =
                if Enum.any?(content_boxes, fn box ->
                     box.type != :page_break and not Map.get(box, :out_of_flow, false) and
                       Map.get(box, :role) != :embedded_extent
                   end),
                   do: content_height,
                   else: 0.0

              trailing_space =
                if content_box_height > reserved_content_height and
                     Map.get(style, :overflow, :visible) == :visible and
                     not Map.has_key?(style, :form_kind) and
                     not Map.has_key?(style, :embedded_document) and
                     Map.get(style, :position, :static) not in [:absolute, :fixed] and
                     Map.get(ResourceContext.current() || %{}, :depth, 0) == 0 do
                  [
                    %{
                      type: HtmlValidator.reserve_layout_box(:flow_marker),
                      role: :explicit_block_space,
                      x: content_x,
                      y: box_top - box_height,
                      width: 0.0,
                      height:
                        content_box_height - reserved_content_height + padding.bottom +
                          border_widths.bottom,
                      flow_id: {:explicit_block_space, make_ref()}
                    }
                  ]
                else
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
                fragment_block_paints(
                  background_box,
                  flow_marker ++ content_boxes ++ trailing_space ++ extents,
                  style,
                  box_x,
                  box_top,
                  box_height
                )
                |> then(&attach_html_target(style, &1, box_x, box_top))

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

              boxes =
                if trailing_space != [] and Map.get(style, :break_inside, :auto) == :avoid do
                  {boxes, _} =
                    Enum.map_reduce(boxes, false, fn box, marked ->
                      if not marked and box.type != :page_break and
                           not Map.get(box, :out_of_flow, false) do
                        {Map.put(box, :block_placement_height, box_height), true}
                      else
                        {box, marked}
                      end
                    end)

                  boxes
                else
                  boxes
                end

              boxes =
                CssTransformValidator.mark_border_box(style, boxes, %{
                  x: box_x,
                  top: box_top,
                  width: box_width,
                  height: box_height
                })

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

  # Fieldsets and ordinary decorated blocks share the validated page-slice
  # transport. Paint never contributes the full unsplit container height to a
  # line group; its first and last content groups carry the actual edge insets.
  defp fragment_block_paints(paints, content, style, x, top, height) do
    members = Enum.reject(content, &(&1.type == :page_break or Map.get(&1, :out_of_flow, false)))

    fragment_count =
      members
      |> Enum.map(&Map.get(&1, :fragment_id, Map.get(&1, :flow_id)))
      |> Enum.uniq()
      |> length()

    reserved_space? = Enum.any?(members, &(Map.get(&1, :role) == :explicit_block_space))

    if paints != [] and (fragment_count > 1 or reserved_space?) and height > 0 and
         (Map.get(style, :height) in [nil, :auto] or reserved_space?) and
         not Map.has_key?(style, :form_kind) and not Map.has_key?(style, :embedded_document) and
         not Map.has_key?(style, :_gauge) and
         not Enum.any?(paints, &(&1.type == :layout_error)) do
      id = make_ref()
      first = hd(members)
      last = List.last(members)
      metadata = [:flow_id, :fragment_id, :break_inside, :orphans, :widows]

      paints =
        Enum.map(paints, fn paint ->
          paint
          |> Map.merge(Map.take(first, metadata))
          |> Map.put(:fieldset_paint_id, id)
          |> Map.put(:fieldset_bounds, %{top: top - paint.y, bottom: top - height - paint.y})
          |> Map.put(:role, :block_fragment_paint)
          |> then(fn paint ->
            if Map.get(first, :role) == :explicit_block_space,
              do: Map.put(paint, :flow_id, {:block_fragment_start, id}),
              else: paint
          end)
        end)

      content =
        if Map.get(last, :role) == :explicit_block_space do
          # The divisible source reservation already reaches the actual bottom
          # border. A separate zero-height ending would duplicate its group.
          content
        else
          ending =
            %{
              type: HtmlValidator.reserve_layout_box(:flow_marker),
              role: :block_fragment_end,
              x: x,
              y: top - height,
              width: 0.0,
              height: 0.0
            }
            |> Map.merge(Map.take(last, metadata))

          {trailing_breaks, reversed} =
            content |> Enum.reverse() |> Enum.split_while(&(&1.type == :page_break))

          Enum.reverse(reversed) ++ [ending] ++ Enum.reverse(trailing_breaks)
        end

      Enum.map(paints ++ content, fn box ->
        Map.update(box, :fieldset_ancestors, [id], &[id | &1])
      end)
    else
      paints ++ content
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
               Map.get(Map.get(child, :style, %{}), :position, :static) not in [:absolute, :fixed])
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

        boxes =
          Enum.map(boxes, &Map.delete(&1, :css_transform_border_box))
          |> then(
            &CssTransformValidator.mark_border_box(style, &1, %{
              x: box_x,
              top: border_top,
              width: box_width,
              height: paint_height
            })
          )

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
            |> Enum.map(&intrinsic_inline_line_width(&1, nil))
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

  defp position_container_contents(
         block,
         boxes,
         next_y,
         x,
         y,
         available_width,
         used_bounds
       ) do
    style = Map.fetch!(block, :style)
    positioned_children = Map.get(block, :positioned_children, [])

    case positioned_children do
      [] ->
        {:ok, relatively_shifted_boxes(boxes, style, x, y, available_width, next_y), next_y}

      positioned_children ->
        with {:ok, containing_block} <-
               positioned_containing_block(
                 style,
                 x,
                 y,
                 available_width,
                 next_y,
                 used_bounds || Enum.find_value(boxes, &Map.get(&1, :css_transform_border_box))
               ),
             {:ok, positioned_boxes} <-
               positioned_child_boxes(positioned_children, containing_block, boxes) do
          positioned_boxes =
            Enum.map(positioned_boxes, fn entry ->
              %{
                entry
                | boxes:
                    clip_overflow_contents(entry.boxes, style, %{
                      x: containing_block.x,
                      y: containing_block.top - containing_block.height,
                      width: containing_block.width,
                      height: containing_block.height
                    })
              }
            end)

          positioned_boxes =
            if Map.get(style, :_root_body_fragmentation, false) and
                 Map.get(ResourceContext.current() || %{}, :depth, 0) == 0 do
              Enum.map(positioned_boxes, fn entry ->
                %{entry | boxes: Enum.map(entry.boxes, &Map.put(&1, :position_anchor, :root))}
              end)
            else
              positioned_boxes
            end

          {boxes, positioned_boxes, trailing_space} =
            if Enum.any?(boxes, &(Map.get(&1, :role) == :explicit_block_space)) and
                 not Enum.any?(boxes, fn box ->
                   not Map.has_key?(box, :fieldset_paint_id) and
                     Map.get(box, :role) != :explicit_block_space and
                     box.type != :page_break and not Map.get(box, :out_of_flow, false)
                 end) do
              {trailing_space, source_boxes} =
                Enum.split_with(boxes, &(Map.get(&1, :role) == :explicit_block_space))

              owner = Enum.find(source_boxes, &Map.has_key?(&1, :fieldset_paint_id))

              positioned_boxes =
                if owner == nil do
                  positioned_boxes
                else
                  Enum.map(positioned_boxes, fn entry ->
                    metadata =
                      owner
                      |> Map.take([:flow_id, :fragment_id, :fieldset_ancestors])
                      |> Map.put(:positioned_fragment_owner, owner.fieldset_paint_id)

                    %{entry | boxes: Enum.map(entry.boxes, &Map.merge(&1, metadata))}
                  end)
                end

              {source_boxes, positioned_boxes, trailing_space}
            else
              {boxes, positioned_boxes, []}
            end

          boxes =
            (boxes
             |> mark_container_background(style, x, y, available_width, next_y)
             |> stack_positioned_boxes(positioned_boxes, is_integer(Map.get(style, :z_index)))) ++
              trailing_space

          {:ok, relatively_shifted_boxes(boxes, style, x, y, available_width, next_y), next_y}
        end
    end
  end

  defp positioned_containing_block(style, x, y, available_width, next_y, used_bounds) do
    display = Map.get(style, :display)

    case used_bounds != nil or display in [:block, :flex, :inline_flex, :grid, :inline_grid] do
      true ->
        margin = Map.get(style, :margin, edges(0.0))
        border_widths = Map.get(style, :border_widths, edges(0.0))
        box_x = if used_bounds == nil, do: x + margin.left, else: used_bounds.x
        box_top = if used_bounds == nil, do: y - margin.top, else: used_bounds.top

        box_width =
          if used_bounds == nil,
            do: positioned_element_border_width(style, available_width),
            else: used_bounds.width

        box_height =
          if used_bounds == nil,
            do: max(box_top - next_y - margin.bottom, 0.0),
            else: used_bounds.height

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

  # Reuse ordinary inline/block intrinsic measurement for HTML's fit-content UA width.
  defp resolve_fit_content_style(block, available_width) do
    case block do
      %{style: %{width: :fit_content, display: display} = style, children: children}
      when display != :image ->
        available = max(available_width - horizontal_box_size(style), 0.0)
        minimum = fieldset_content_width(children, style, :min)
        maximum = fieldset_content_width(children, style, :max)

        style =
          style
          |> Map.put(:width, max(minimum, min(maximum, available)))
          |> Map.put(:box_sizing, :content_box)

        %{block | style: style}

      _ ->
        block
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
            Map.get(style, :_containing_height)
          )

        content_width + horizontal_box_size(style)

      _ ->
        content_width =
          resolved_content_size(
            style,
            :width,
            available_width,
            if(Map.has_key?(style, :embedded_document),
              do: elem(EmbeddedViewportValidator.intrinsic_size(), 0),
              else: available_box_width - horizontal_box_size(style)
            )
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
               positioned_child_boxes(positioned_children, containing_block, normal_boxes) do
          positioned_boxes =
            Enum.map(positioned_boxes, fn entry ->
              position_anchor = if entry.position == :fixed, do: :fixed, else: anchor

              %{
                entry
                | boxes: Enum.map(entry.boxes, &Map.put(&1, :position_anchor, position_anchor))
              }
            end)

          {:ok, stack_positioned_boxes(normal_boxes, positioned_boxes, true)}
        end
    end
  end

  defp positioned_child_boxes(positioned_children, containing_block, normal_boxes) do
    static_positions = Map.new(normal_boxes, fn box -> {Map.get(box, :_dialog_anchor), box.y} end)
    containing_block = Map.put(containing_block, :static_positions, static_positions)

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
    {child, style} =
      case Map.get(style, :_math_box) do
        nil ->
          {child, style}

        measured ->
          atom_style =
            style
            |> NativeElixirPdfUtilities.Validators.MathLayoutValidator.anonymous_box_style()
            |> Map.put(:display, :inline_block)

          wrapper_style =
            atom_style
            |> Map.delete(:_math_box)
            |> Map.merge(Map.take(style, [:position, :offsets]))
            |> Map.merge(%{display: :block, width: measured.width, _math_flow: true})

          {%{child | style: wrapper_style, children: [%{child | style: atom_style}]},
           wrapper_style}
      end

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

        style = resolve_fit_content_style(%{child | style: style}, containing_block.width).style

        style =
          style
          |> Map.put(:_containing_height, containing_block.height)
          |> stretch_positioned_style(offsets, containing_block)

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
                  Map.get(
                    containing_block.static_positions,
                    Map.get(child, :_dialog_anchor),
                    containing_block.top
                  ) - margin.top
              end

            auto_margin_offset =
              if is_number(left) and is_number(right),
                do:
                  NativeElixirPdfUtilities.Validators.CssSizingValidator.auto_margin_offset(
                    style,
                    :width,
                    containing_block.width - left - right - border_width - margin.left -
                      margin.right,
                    :positioned
                  ),
                else: 0.0

            block_margin_offset =
              if is_number(top) and is_number(bottom),
                do:
                  NativeElixirPdfUtilities.Validators.CssSizingValidator.auto_margin_offset(
                    style,
                    :height,
                    containing_block.height - top - bottom - border_height - margin.top -
                      margin.bottom,
                    :positioned
                  ),
                else: 0.0

            existing_margin_offset =
              case Map.get(style, :margin_inline_auto) do
                %{left: true, right: true} ->
                  max(containing_block.width - border_width - margin.left - margin.right, 0.0) / 2

                %{left: true, right: false} ->
                  max(containing_block.width - border_width - margin.left - margin.right, 0.0)

                _ ->
                  0.0
              end

            delta_x = target_x + auto_margin_offset - margin.left - existing_margin_offset
            delta_y = target_top - block_margin_offset - -margin.top

            boxes =
              boxes
              |> shift_layout_boxes(delta_x, delta_y)
              |> Enum.map(&Map.put(&1, :out_of_flow, true))

            z_index = Map.get(style, :z_index, :auto)

            {:ok,
             %{
               z_index: z_index,
               index: index,
               boxes: boxes,
               dialog_anchor: Map.get(child, :_dialog_anchor),
               position: Map.get(style, :position, :static)
             }}

          {:error, reason} ->
            {:error, reason}
        end

      false ->
        {:error, :invalid_layout}
    end
  end

  defp stretch_positioned_style(style, offsets, containing_block) do
    if Map.get(style, :display) == :image or Map.has_key?(style, :embedded_document) do
      style
    else
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

        shifted =
          boxes
          |> shift_layout_boxes(delta_x, delta_y)
          |> Enum.map(&Map.put(&1, :_positioned_flow, true))

        # This private marker records flow geometry before the table's own
        # relative paint movement. Ancestor translations still move it normally.
        case Map.get(style, :_measure_table_reference) do
          reference when is_reference(reference) ->
            Enum.zip_with(boxes, shifted, fn source, moved ->
              if Map.get(source, :table_measurement) == reference, do: source, else: moved
            end)

          _ ->
            shifted
        end

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

    {content_boxes, nonnegative_boxes} =
      Enum.reduce(nonnegative, {content_boxes, []}, fn entry, {content, positioned} ->
        if entry.z_index == :auto and entry.dialog_anchor != nil do
          {before_anchor, after_anchor} =
            Enum.split_while(
              content,
              &(Map.get(&1, :_dialog_anchor) != entry.dialog_anchor)
            )

          {before_positioned, later_positioned} =
            Enum.split_while(
              after_anchor,
              &(not Map.get(&1, :_positioned_flow, false))
            )

          {before_anchor ++ before_positioned ++ entry.boxes ++ later_positioned, positioned}
        else
          {content, positioned ++ entry.boxes}
        end
      end)

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
    NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.translate_boxes(boxes, delta_x, delta_y)
  end

  # Reuse block layout for the popover's client area. Gutters narrow its
  # content width, while the authored outer box keeps the same dimensions.
  defp layout_popover_contents(children, style, x, top, width, metadata, previous \\ nil) do
    scrolling? =
      Map.get(style, :_popover, false) and Map.get(style, :overflow) == :auto and
        Map.get(style, :form_kind) not in [:text, :textarea, :select, :listbox]

    available_width =
      if previous != nil and previous.vertical,
        do: max(width - 11.25, 0.0),
        else: width

    with {:ok, boxes, content_height} <-
           layout_block_content(children, style, x, top, available_width, metadata) do
      if scrolling? do
        # An intrinsic block height includes an automatic horizontal gutter.
        preferred_height =
          content_height + if(previous != nil and previous.horizontal, do: 11.25, else: 0.0)

        height =
          resolved_content_size(
            style,
            :height,
            Map.get(style, :_containing_height),
            preferred_height
          )

        content = %{
          x: x,
          top: top,
          width: width,
          height: height,
          content_height: content_height
        }

        case NativeElixirPdfUtilities.Validators.ScrollbarValidator.popover_geometry(
               boxes,
               content,
               style.padding,
               previous
             ) do
          {:paint, geometry} ->
            {:ok, boxes, preferred_height, geometry}

          {:relayout, geometry} ->
            layout_popover_contents(children, style, x, top, width, metadata, geometry)
        end
      else
        {:ok, boxes, content_height, nil}
      end
    end
  end

  defp layout_block_content(children, style, x, y, width, metadata) do
    key = {__MODULE__, :svg_containing_height}
    previous = Process.get(key)
    height = resolved_content_size(style, :height, previous, nil)
    Process.put(key, height)

    try do
      resolved =
        if Map.get(style, :_math_flow_requires_inline_size, false),
          do: MathLayout.resolve_block_flow(children, width),
          else: {:ok, children}

      with {:ok, children} <- resolved do
        do_layout_block_content(children, style, x, y, width, metadata)
      end
    after
      if is_nil(previous), do: Process.delete(key), else: Process.put(key, previous)
    end
  end

  # Block, inline and flex hosts place the same validated child viewport in the
  # allocated content box; the child's clip and resource/link scope stay local.
  defp layout_embedded_document(surface, x, top, width, height, metadata) do
    with {:ok, group} <- EmbeddedViewport.layout(surface, width, height) do
      group =
        group
        |> Map.put(:viewport_paint_role, Map.get(group, :role))
        |> Map.put(:role, :embedded_viewport_host)
        |> Map.put(:x, x)
        |> Map.put(:y, top - height)
        |> Map.put(:transform, {1, 0, 0, 1, x, top - height})
        |> Map.merge(metadata)

      {:ok, [group], height}
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
      Map.has_key?(style, :_color_state) ->
        {:ok, [], max(10.5 - vertical_box_size(style), 0.0)}

      Map.has_key?(style, :embedded_document) ->
        surface = style.embedded_document

        height =
          resolved_content_size(
            style,
            :height,
            Map.get(style, :_containing_height),
            elem(EmbeddedViewportValidator.intrinsic_size(), 1)
          )

        layout_embedded_document(surface, x, y, width, height, metadata)

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
      |> Enum.reduce_while({:ok, [], y, nil, 0.0}, fn child,
                                                      {:ok, boxes, current_y,
                                                       previous_margin_bottom,
                                                       previous_line_clearance} ->
        {margin_top, margin_bottom} = flow_child_vertical_margins(child, style)

        collapsed_margin =
          if not is_nil(get_in(child, [:style, :_dialog_anchor])),
            do: 0.0,
            else: collapsed_sibling_margin(previous_margin_bottom, margin_top)

        clearance =
          max(
            (previous_margin_bottom || Map.get(style, :_ruby_clearance, 0.0)) + margin_top -
              collapsed_margin + previous_line_clearance,
            0.0
          )

        child =
          case child do
            %{style: child_style} ->
              %{child | style: Map.put(child_style, :_ruby_clearance, clearance)}

            child ->
              child
          end

        flow_style = Map.put(style, :_ruby_clearance, clearance)

        case layout_flow_child(child, flow_style, x, current_y + collapsed_margin, width) do
          {:ok, child_boxes, next_y} ->
            next_margin_bottom =
              if not is_nil(get_in(child, [:style, :_dialog_anchor])),
                do: previous_margin_bottom,
                else:
                  following_sibling_margin(
                    child_boxes,
                    previous_margin_bottom,
                    margin_top,
                    margin_bottom
                  )

            boxes = keep_interrupted_inline_start(boxes, child_boxes, next_y)
            line_clearance = following_ruby_clearance(child_boxes, next_y + margin_bottom)

            {:cont,
             {:ok, Enum.reverse(child_boxes, boxes), next_y, next_margin_bottom, line_clearance}}

          {:error, reason} ->
            {:halt, {:error, reason}}
        end
      end)

    case result do
      {:ok, boxes, next_y, _margin_bottom, _line_clearance} ->
        {:ok, position_split_inline_contexts(Enum.reverse(boxes), width), next_y}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # The source inline's leading fragment and the first block line share the
  # initial print reservation. A cut must not leave that initial fragment alone.
  # The accumulator is reversed, so only the last leading line is inspected.
  # Ruby can use the preceding flow's empty lower leading. Measure that
  # actual preceding paint, rather than subtracting leading from every ruby
  # line, including the first line of a document or a positioned ruby owner.
  defp following_ruby_clearance(boxes, flow_bottom) do
    bottom =
      Enum.reduce(boxes, nil, fn box, lowest ->
        if Map.get(box, :out_of_flow, false) or box.type == :flow_marker do
          lowest
        else
          bottom =
            case box do
              %{
                type: :text,
                y: y,
                font_size: size,
                font_face: %{descent: descent, units_per_em: units}
              } ->
                y - round(-descent / units * size / @css_pixel_points) * @css_pixel_points

              _ ->
                elem(PageGeometry.box_vertical_bounds(box), 1)
            end

          if lowest == nil, do: bottom, else: min(lowest, bottom)
        end
      end)

    if bottom == nil, do: 0.0, else: max(bottom - flow_bottom, 0.0)
  end

  defp keep_interrupted_inline_start(previous, following, bottom) do
    case {previous, following} do
      {[%{interrupted_inline_start: true, fragment_id: id} | _], [_ | _]} ->
        {line, before} = Enum.split_while(previous, &(Map.get(&1, :fragment_id) == id))

        top =
          line
          |> Enum.map(&PageGeometry.box_vertical_bounds/1)
          |> Enum.map(&elem(&1, 0))
          |> Enum.max()

        first =
          Enum.find(
            following,
            &(Map.get(&1, :out_of_flow, false) != true and Map.has_key?(&1, :line_box_height))
          )

        first_bottom =
          if first == nil, do: bottom, else: elem(PageGeometry.box_vertical_bounds(first), 1)

        Enum.map(line, &Map.put(&1, :block_placement_height, max(top - first_bottom, 0.0))) ++
          before

      _ ->
        previous
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
        boxes = inherit_split_inline_contexts(boxes, Map.get(child, :split_inline_contexts, []))

        {:ok, boxes, next_y}

      error ->
        error
    end
  end

  defp inherit_split_inline_contexts(boxes, contexts) do
    if contexts == [] do
      boxes
    else
      Enum.map(
        boxes,
        &Map.update(&1, :split_inline_contexts, contexts, fn current -> contexts ++ current end)
      )
    end
  end

  defp position_split_inline_contexts(boxes, width) do
    if Enum.any?(boxes, &Map.has_key?(&1, :split_inline_contexts)) do
      boxes
      |> Enum.map(fn box ->
        case Map.pop(box, :split_inline_contexts) do
          {nil, box} -> box
          {contexts, box} -> Map.put(box, :inline_positioning_contexts, contexts)
        end
      end)
      |> position_inline_contexts(width)
      |> Enum.map(fn box ->
        if Map.get(box, :role) == :inline_context_fragment,
          do: box |> Map.put(:width, 0.0) |> Map.drop([:link_url, :link_destination]),
          else: box
      end)
    else
      boxes
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
                if inline_fragments_need_box?(inline_style) do
                  # Splitting creates both inline ends even when the source has
                  # no text there. Their authored edge padding/borders still paint.
                  nested_groups =
                    if hd(nested_groups).type == :inline_flow,
                      do: nested_groups,
                      else: [%{type: :inline_flow, children: []} | nested_groups]

                  nested_groups =
                    if List.last(nested_groups).type == :inline_flow,
                      do: nested_groups,
                      else: nested_groups ++ [%{type: :inline_flow, children: []}]

                  last = length(nested_groups) - 1

                  context = %{
                    id: make_ref(),
                    split_inline: true,
                    style: inline_style,
                    positioned_children: Map.get(child, :positioned_children, [])
                  }

                  nested_groups =
                    Enum.with_index(nested_groups, fn group, index ->
                      group =
                        case group do
                          %{type: :inline_flow, children: contents} ->
                            fragment_style =
                              Enum.reduce(
                                [:padding, :border_widths, :margin],
                                inline_style,
                                fn key, style ->
                                  value = Map.get(style, key, edges(0.0))

                                  Map.put(style, key, %{
                                    value
                                    | left: if(index == 0, do: value.left, else: 0.0),
                                      right: if(index == last, do: value.right, else: 0.0)
                                  })
                                end
                              )
                              |> Map.put(:position, :static)
                              |> Map.put(:_split_inline_fragment, true)

                            fragment_style =
                              if index == 0,
                                do: fragment_style,
                                else: Map.delete(fragment_style, :html_target)

                            fragment =
                              child
                              |> Map.put(:children, contents)
                              |> Map.put(:style, fragment_style)
                              |> Map.put(:split_inline_start, index == 0)
                              |> Map.delete(:positioned_children)

                            fragment =
                              if Map.get(inline_style, :background_image) == nil,
                                do: fragment,
                                else:
                                  fragment
                                  |> Map.put(:split_inline_background_id, context.id)
                                  |> Map.put(:split_inline_source_style, inline_style)

                            fragment =
                              if Map.get(inline_style, :position, :static) == :relative,
                                do:
                                  fragment
                                  |> Map.put(:split_inline_contexts, [context])
                                  |> Map.put(:split_inline_text, true),
                                else: fragment

                            %{group | children: [fragment]}

                          group ->
                            group
                        end

                      group =
                        if group.type != :inline_flow and
                             Map.get(inline_style, :position, :static) == :relative do
                          Map.update(group, :split_inline_contexts, [context], &[context | &1])
                        else
                          group
                        end

                      group
                    end)

                  Enum.reduce(nested_groups, acc, &append_inline_flow_group/2)
                else
                  Enum.reduce(nested_groups, acc, &append_inline_flow_group/2)
                end
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

  # Neutral ancestors already pass inherited text properties to their real
  # descendants. Only an observable owner needs distinct inline box fragments.
  defp inline_fragments_need_box?(style) do
    Map.get(style, :background_color) != nil or Map.get(style, :background_image) != nil or
      Map.get(style, :position, :static) != :static or
      Map.get(style, :vertical_align, :baseline) != :baseline or
      Enum.any?([:link_url, :link_destination, :html_target], &(Map.get(style, &1) != nil)) or
      Enum.any?([:padding, :margin, :border_widths], fn key ->
        Enum.any?(Map.get(style, key, edges(0.0)), fn {_side, value} -> value != 0 end)
      end)
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

  defp layout_inline_flow(children, style, x, y, width, metadata \\ %{}) do
    with {:ok, runs} <- inline_runs(children, style) do
      case Enum.any?(runs, fn run ->
             Map.get(run, :type) in [
               :inline_block,
               :inline_image,
               :inline_positioning_marker,
               :soft_break
             ] or
               Map.get(run, :hard_break, false) or
               HtmlValidator.trim_ascii_whitespace(run.text) != ""
           end) do
        true ->
          metadata = Map.merge(%{flow_id: {:inline_flow, x, y}, fragment_lines: true}, metadata)

          metadata =
            if Enum.any?(children, &Map.get(&1.style, :_split_inline_fragment, false)),
              do: Map.put(metadata, :split_inline_fragment, true),
              else: metadata

          {boxes, content_height} = inline_text_boxes(runs, style, x, y, width, metadata)

          boxes =
            if Enum.any?(children, &Map.get(&1, :split_inline_start, false)),
              do: Enum.map(boxes, &Map.put(&1, :interrupted_inline_start, true)),
              else: boxes

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
          shift_layout_boxes(background ++ artwork, box_x, box_top - box_height)
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

    {content_width, content_height} =
      image_content_size(style, width, Map.get(style, :_containing_height))

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
        Map.get(style, :image, :deferred)
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

    boxes =
      attach_html_target(style, background_box ++ image_boxes ++ extents, box_x, box_top)
      |> then(
        &CssTransformValidator.mark_border_box(style, &1, %{
          x: box_x,
          top: box_top,
          width: box_width,
          height: box_height
        })
      )

    {:ok, boxes, next_y}
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
    {natural_width, natural_height, natural_ratio} =
      case Map.get(style, :image) do
        nil ->
          # HTML auto-sizes applies size containment: the UA intrinsic fallback
          # is 300 by 150 CSS pixels, without an intrinsic aspect ratio.
          {225.0, 112.5, nil}

        image ->
          ratio =
            cond do
              Map.has_key?(image, :svg_document_image) and (image.width == 0 or image.height == 0) ->
                nil

              image.height == 0 ->
                image.width_px / image.height_px

              true ->
                image.width / image.height
            end

          {image.width, image.height, ratio}
      end

    ratio = Map.get(style, :aspect_ratio, natural_ratio)
    width = resolved_content_size(style, :width, available_width, nil)
    height = resolved_content_size(style, :height, available_height, nil)

    {content_width, content_height} =
      case {width, height} do
        {width, height} when is_number(width) and is_number(height) ->
          {width, height}

        {width, _height} when is_number(width) ->
          {width,
           if(is_nil(ratio),
             do: natural_height,
             else: HtmlValidator.aspect_ratio_dimension(width, ratio, :height, :layout)
           )}

        {_width, height} when is_number(height) ->
          {if(is_nil(ratio),
             do: natural_width,
             else: HtmlValidator.aspect_ratio_dimension(height, ratio, :width, :layout)
           ), height}

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
      is_nil(ratio) or is_number(width) or is_number(height)
    )
  end

  defp layout_grid(style, children, x, y, width, previous \\ nil) do
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

    content_width =
      if previous != nil and previous.vertical,
        do: max(content_width - 11.25, 0.0),
        else: content_width

    horizontal_gutter = if previous != nil and previous.horizontal, do: 11.25, else: 0.0

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

      available_height =
        resolved_content_size(style, :height, Map.get(style, :_containing_height), nil)

      available_height =
        if is_number(available_height),
          do: max(available_height - horizontal_gutter, 0.0),
          else: nil

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
        border_widths.top + padding.top + content_height + horizontal_gutter + padding.bottom +
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
        content = %{
          x: content_x,
          top: content_top,
          width: box_width - horizontal_box_size(style),
          height: content_height + horizontal_gutter,
          content_height: content_height
        }

        case settle_popover_items(style, item_boxes, content, previous) do
          {:paint, item_boxes} ->
            next_y = box_top - box_height - margin.bottom

            boxes =
              CssTransformValidator.mark_border_box(style, background_box ++ item_boxes, %{
                x: box_x,
                top: box_top,
                width: box_width,
                height: box_height
              })

            {:ok, boxes, next_y}

          {:relayout, geometry} ->
            layout_grid(style, children, x, y, width, geometry)
        end
      end
    end
  end

  defp settle_popover_items(style, boxes, content, previous) do
    if Map.get(style, :_popover, false) and Map.get(style, :overflow) == :auto do
      case NativeElixirPdfUtilities.Validators.ScrollbarValidator.popover_geometry(
             boxes,
             content,
             style.padding,
             previous
           ) do
        {:paint, geometry} ->
          padding = style.padding

          clip = %{
            x: content.x - padding.left,
            y: content.top - content.height - padding.bottom,
            width: geometry.viewport_width,
            height: geometry.viewport_height
          }

          {:paint, clip_overflow_contents(boxes, style, clip, geometry)}

        {:relayout, geometry} ->
          {:relayout, geometry}
      end
    else
      {:paint, boxes}
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

        case HtmlValidator.trim_ascii_whitespace(text) do
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

      %{type: :element, tag: "ruby", style: style} = ruby ->
        with {:ok, runs} <- inline_runs([ruby], style),
             do: {:ok, build_grid_item(style, runs, index)}

      %{type: :element, style: %{display: :image} = style} ->
        {:ok, build_grid_image_item(style, index)}

      %{type: :element, style: %{display: :list_item} = style, children: children} ->
        build_grid_block_item(style, children, index)

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
    text_width = intrinsic_inline_line_width(runs, nil)
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
      image: Map.get(style, :image, :deferred),
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

    width_auto =
      NativeElixirPdfUtilities.Validators.CssSizingValidator.margin_flags(item.style, :width) !=
        {false, false}

    height_auto =
      NativeElixirPdfUtilities.Validators.CssSizingValidator.margin_flags(item.style, :height) !=
        {false, false}

    {box_width, box_height} =
      if Map.has_key?(item, :image) do
        {width, height} = image_content_size(item.style, area_width, area_height)
        {width + horizontal_box_size(item.style), height + vertical_box_size(item.style)}
      else
        {grid_aligned_box_size(
           item,
           :width,
           if(width_auto, do: :flex_start, else: justify),
           area_width
         ),
         grid_aligned_box_size(
           item,
           :height,
           if(height_auto, do: :flex_start, else: align),
           area_height
         )}
      end

    x_offset =
      if width_auto,
        do:
          NativeElixirPdfUtilities.Validators.CssSizingValidator.auto_margin_offset(
            item.style,
            :width,
            area_width - box_width - item.margin.left - item.margin.right,
            :flow
          ),
        else: grid_axis_position(justify, box_width, area_width)

    y_offset =
      if height_auto,
        do:
          NativeElixirPdfUtilities.Validators.CssSizingValidator.auto_margin_offset(
            item.style,
            :height,
            area_height - box_height - item.margin.top - item.margin.bottom,
            :flow
          ),
        else: grid_axis_position(align, box_height, area_height)

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

  defp layout_flex(style, children, x, y, width, previous \\ nil) do
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

    content_width =
      if previous != nil and previous.vertical,
        do: max(content_width - 11.25, 0.0),
        else: content_width

    horizontal_gutter = if previous != nil and previous.horizontal, do: 11.25, else: 0.0

    box_x = x + margin.left
    box_top = y - margin.top
    content_x = box_x + border_widths.left + padding.left
    content_top = box_top - border_widths.top - padding.top

    main_axis = flex_main_axis(style)

    containing_height =
      resolved_content_size(style, :height, Map.get(style, :_containing_height), nil)

    containing_height =
      if is_number(containing_height),
        do: max(containing_height - horizontal_gutter, 0.0),
        else: nil

    {available_main, available_cross} =
      case main_axis do
        :row -> {content_width, containing_height}
        :column -> {containing_height, content_width}
      end

    with {:ok, items} <-
           flex_items(children, main_axis, available_main, available_cross) do
      case items do
        [] ->
          content_height =
            resolved_content_size(style, :height, Map.get(style, :_containing_height), 0.0)

          box_height = content_height + vertical_box_size(style)

          background_box =
            background_box(style, box_x, box_top - box_height, box_width, box_height)

          boxes =
            CssTransformValidator.mark_border_box(
              style,
              tag_boxes(background_box, break_metadata(style)),
              %{x: box_x, top: box_top, width: box_width, height: box_height}
            )

          {:ok, boxes, box_top - box_height - margin.bottom}

        items ->
          lines = flex_lines(items, style, content_width)
          natural_height = flex_content_height(lines, style)

          outer_height =
            if horizontal_gutter > 0 do
              resolved_content_size(
                style,
                :height,
                Map.get(style, :_containing_height),
                natural_height + horizontal_gutter
              )
            else
              natural_height
            end

          content_height = max(outer_height - horizontal_gutter, 0.0)

          box_height =
            border_widths.top + padding.top + outer_height + padding.bottom +
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
            content = %{
              x: content_x,
              top: content_top,
              width: box_width - horizontal_box_size(style),
              height: outer_height,
              content_height: content_height
            }

            case settle_popover_items(style, item_boxes, content, previous) do
              {:paint, item_boxes} ->
                next_y = box_top - box_height - margin.bottom

                boxes =
                  CssTransformValidator.mark_border_box(style, background_box ++ item_boxes, %{
                    x: box_x,
                    top: box_top,
                    width: box_width,
                    height: box_height
                  })

                {:ok, boxes, next_y}

              {:relayout, geometry} ->
                layout_flex(style, children, x, y, width, geometry)
            end
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
    height = if main_axis == :row, do: available_cross, else: available_main
    child = %{child | style: Map.put(child.style, :_containing_height, height)}

    case child do
      %{type: :text, text: text} when is_binary(text) ->
        case text
             |> normalize_inline_whitespace(child.style)
             |> HtmlValidator.trim_ascii_whitespace() do
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

      %{type: :element, tag: "ruby", style: style} = ruby ->
        with {:ok, runs} <- inline_runs([ruby], style),
             do:
               {:ok,
                build_flex_item(style, runs, index, main_axis, available_main, available_cross)}

      %{type: :element, style: %{display: :image} = style} ->
        {:ok, build_flex_image_item(style, index, main_axis, available_main, available_cross)}

      %{type: :element, style: %{embedded_document: _surface} = style, children: children} ->
        build_flex_block_item(style, children, index, main_axis, available_main, available_cross)

      %{type: :element, style: %{display: :list_item} = style, children: children} ->
        build_flex_block_item(style, children, index, main_axis, available_main, available_cross)

      %{type: :element, style: %{_fieldset: true} = style, children: children} ->
        build_flex_block_item(style, children, index, main_axis, available_main, available_cross)

      %{type: :element, style: style, children: children} when is_list(children) ->
        style =
          Map.put(
            style,
            :_inline_available_height,
            resolved_content_size(style, :height, Map.get(style, :_containing_height), nil)
          )

        case inline_runs(children, style) do
          {:ok, runs} ->
            item = build_flex_item(style, runs, index, main_axis, available_main, available_cross)
            {:ok, Map.put(item, :inline_children, children)}

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
      intrinsic_inline_line_width(runs, available_main)

    {content_main, content_cross} =
      case main_axis do
        :row ->
          content_main = flex_basis(style, :width, available_main, text_width)
          content_height = inline_content_height(runs, content_main, style)

          {content_main,
           resolved_content_size(
             style,
             :height,
             Map.get(style, :_containing_height),
             content_height
           )}

        :column ->
          content_width = resolved_content_size(style, :width, available_cross, text_width)

          content_height =
            if Enum.any?(runs, &(Map.get(&1, :type) == :inline_image)),
              do: inline_content_height(runs, content_width, style),
              else: Map.get(style, :line_height, 14.4)

          {flex_basis(style, :height, available_main, content_height), content_width}
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

    intrinsic_width =
      if Map.has_key?(style, :embedded_document),
        do: elem(EmbeddedViewportValidator.intrinsic_size(), 0),
        else: flex_block_intrinsic_width(children)

    content_width =
      case main_axis do
        :row ->
          default_width =
            if Map.has_key?(style, :embedded_document),
              do: intrinsic_width,
              else: min(intrinsic_width, available_main)

          flex_basis(style, :width, available_main, default_width)

        :column ->
          default_width = max(available_cross - flex_cross_box_size(style, :column), 0.0)

          resolved_content_size(style, :width, available_cross, default_width)
      end

    case layout_container_content_height(style, children, max(content_width, 0.0)) do
      {:ok, content_height} ->
        {content_main, content_cross} =
          case main_axis do
            :row ->
              {content_width,
               resolved_content_size(
                 style,
                 :height,
                 Map.get(style, :_containing_height),
                 content_height
               )}

            :column ->
              {flex_basis(style, :height, available_main, content_height), content_width}
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
    case style do
      %{embedded_document: _surface} ->
        {:ok,
         resolved_content_size(
           style,
           :height,
           Map.get(style, :_containing_height),
           elem(EmbeddedViewportValidator.intrinsic_size(), 1)
         )}

      %{display: :table} ->
        nested_style = Map.put(style, :margin, edges(0.0))
        available_width = content_width + horizontal_box_size(style)

        with {:ok, _boxes, next_y} <-
               layout_table(nested_style, children, 0.0, 0.0, available_width) do
          {:ok, max(0.0 - next_y - vertical_box_size(style), 0.0)}
        end

      %{display: display} when display in [:grid, :inline_grid] ->
        nested_style = Map.put(style, :margin, edges(0.0))
        available_width = content_width + horizontal_box_size(style)

        with {:ok, _boxes, next_y} <-
               layout_grid(nested_style, children, 0.0, 0.0, available_width) do
          {:ok, max(0.0 - next_y - vertical_box_size(style), 0.0)}
        end

      %{display: display} when display in [:flex, :inline_flex] ->
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
      image: Map.get(style, :image, :deferred),
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
        :column -> resolved_content_size(style, :height, Map.get(style, :_containing_height), nil)
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
              resolved_content_size(
                item.style,
                :height,
                Map.get(item.style, :_containing_height),
                content_height
              ) +
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
        line_cross = flex_line_cross(line, style, length(lines), content_height)

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
      case flex_item_boxes(item, main_axis) do
        {:ok, boxes} ->
          tagged_boxes = tag_atomic_boxes(boxes, %{flow_id: container_flow_id})

          anchored_boxes =
            attach_outline_anchor(
              item,
              attach_html_target(item.style, tagged_boxes, item.x, item.y)
            )

          {:cont, {:ok, acc ++ anchored_boxes}}

        {:error, _reason} = error ->
          {:halt, error}
      end
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
    main_dimension = if main_axis == :row, do: :width, else: :height

    auto_count =
      Enum.reduce(items, 0, fn item, count ->
        {start, finish} =
          NativeElixirPdfUtilities.Validators.CssSizingValidator.margin_flags(
            item.style,
            main_dimension
          )

        count + if(start, do: 1, else: 0) + if(finish, do: 1, else: 0)
      end)

    free = max(available_main - line.main, 0.0)

    {items, main_offset, item_gap} =
      if auto_count > 0 and free > 0 do
        share = free / auto_count

        items =
          Enum.map(items, fn item ->
            {start, finish} =
              NativeElixirPdfUtilities.Validators.CssSizingValidator.margin_flags(
                item.style,
                main_dimension
              )

            {start_side, finish_side} =
              if main_axis == :row, do: {:left, :right}, else: {:top, :bottom}

            margin =
              item.margin
              |> Map.update!(start_side, &(&1 + if(start, do: share, else: 0.0)))
              |> Map.update!(finish_side, &(&1 + if(finish, do: share, else: 0.0)))

            %{
              item
              | margin: margin,
                outer_main: item.main_box + flex_main_margin_size(margin, main_axis)
            }
          end)

        {items, 0.0, gap}
      else
        {main_offset, item_gap} = flex_main_distribution(style, line, available_main, gap)
        {items, main_offset, item_gap}
      end

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
    cross_dimension = if main_axis == :row, do: :height, else: :width

    cross_auto =
      NativeElixirPdfUtilities.Validators.CssSizingValidator.margin_flags(
        item.style,
        cross_dimension
      ) != {false, false}

    align = if cross_auto, do: :flex_start, else: align
    available_cross = if main_axis == :row, do: line_cross, else: content_width

    cross_position =
      if cross_auto,
        do:
          NativeElixirPdfUtilities.Validators.CssSizingValidator.auto_margin_offset(
            item.style,
            cross_dimension,
            available_cross - item.outer_cross,
            :flow
          ),
        else: flex_cross_position(align, item.outer_cross, line_cross)

    case main_axis do
      :row ->
        box_width = item.main_box
        box_height = flex_aligned_cross_box(item, align, line_cross, :row)

        Map.merge(item, %{
          x: x + main_offset + item.margin.left,
          y: y - cross_offset - cross_position - item.margin.top,
          box_width: box_width,
          box_height: box_height,
          alignment: align
        })

      :column ->
        box_width = flex_aligned_cross_box(item, align, content_width, :column)
        box_height = item.main_box

        Map.merge(item, %{
          x: x + cross_position + item.margin.left,
          y: y - main_offset - item.margin.top,
          box_width: box_width,
          box_height: box_height,
          alignment: align
        })
    end
  end

  defp flex_item_boxes(item, main_axis) do
    style = item.style
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))

    result =
      case Map.get(item, :image) do
        nil ->
          case Map.get(item, :children) do
            nil -> flex_text_item_boxes(item, main_axis, style, padding, border_widths)
            children -> flex_block_item_boxes(item, style, padding, border_widths, children)
          end

        image ->
          flex_image_item_boxes(item, main_axis, style, padding, border_widths, image)
      end

    bounds = %{x: item.x, top: item.y, width: item.box_width, height: item.box_height}
    positioned_style = style |> Map.put(:margin, edges(0.0))

    with {:ok, boxes} <- result,
         {:ok, boxes, _} <-
           position_container_contents(
             %{
               style: positioned_style,
               positioned_children: Map.get(item, :positioned_children, [])
             },
             boxes,
             item.y - item.box_height,
             item.x,
             item.y,
             item.box_width,
             bounds
           ) do
      boxes =
        Enum.map(boxes, &Map.delete(&1, :css_transform_border_box))
        |> then(
          &CssTransformValidator.mark_border_box(style, &1, %{
            x: item.x,
            top: item.y,
            width: item.box_width,
            height: item.box_height
          })
        )

      CssTransformValidator.attach_scope(style, boxes)
    end
  end

  defp flex_text_item_boxes(item, main_axis, style, padding, border_widths) do
    content_x = item.x + border_widths.left + padding.left

    content_width =
      item.box_width - border_widths.left - padding.left - padding.right - border_widths.right

    content_top = item.y - border_widths.top - padding.top

    background_boxes =
      background_box(style, item.x, item.y - item.box_height, item.box_width, item.box_height)

    style =
      Map.put(
        style,
        :_inline_available_height,
        max(item.box_height - vertical_box_size(style), 0.0)
      )

    runs =
      case Map.fetch(item, :inline_children) do
        {:ok, children} ->
          {:ok, runs} = inline_runs(children, style)
          runs

        :error ->
          item.runs
      end

    {text_boxes, _content_height} =
      inline_text_boxes(runs, style, content_x, content_top, max(content_width, 0.0), %{})

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
          :list_item ->
            nested_style = nested_flex_item_style(style, item)
            node = %{type: :element, tag: "li", style: nested_style, children: children}

            with {:ok, boxes, _} <-
                   layout_list_item(
                     node,
                     Map.get(style, :list_marker_type, :disc),
                     1,
                     item.x,
                     item.y,
                     item.box_width
                   ) do
              {:ok, boxes}
            end

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
    |> Map.delete(:css_transform)
    |> Map.put(:margin, edges(0.0))
    |> Map.put(:width, declared_size_for_border_box(style, :width, item.box_width))
    |> Map.put(:height, declared_size_for_border_box(style, :height, item.box_height))
    |> Map.drop([:min_width, :max_width, :min_height, :max_height])
  end

  defp flex_plain_block_item_boxes(item, style, padding, border_widths, children) do
    content_x = item.x + border_widths.left + padding.left

    content_width =
      item.box_width - border_widths.left - padding.left - padding.right - border_widths.right

    content_top = item.y - border_widths.top - padding.top

    background_boxes =
      background_box(style, item.x, item.y - item.box_height, item.box_width, item.box_height)

    content =
      case Map.fetch(style, :embedded_document) do
        {:ok, surface} ->
          content_height = max(item.box_height - vertical_box_size(style), 0.0)

          layout_embedded_document(
            surface,
            content_x,
            content_top,
            max(content_width, 0.0),
            content_height,
            %{}
          )

        :error ->
          layout_block_content(
            children,
            nested_flex_item_style(style, item),
            content_x,
            content_top,
            max(content_width, 0.0),
            %{}
          )
      end

    with {:ok, content_boxes, _content_height} <- content,
         do: {:ok, background_boxes ++ content_boxes}
  end

  defp flex_image_item_boxes(item, main_axis, style, padding, border_widths, image) do
    content_x = item.x + border_widths.left + padding.left

    content_width =
      item.box_width - border_widths.left - padding.left - padding.right - border_widths.right

    content_height =
      item.box_height - border_widths.top - padding.top - padding.bottom - border_widths.bottom

    # Grid items share this painter but do not initialize a flex document canvas.
    layout_context =
      case item do
        %{alignment: alignment} ->
          %{
            main_axis: main_axis,
            alignment: alignment,
            containing_height: Map.get(style, :_containing_height),
            height:
              resolved_content_size(style, :height, Map.get(style, :_containing_height), nil)
          }

        _ ->
          nil
      end

    image_boxes =
      fitted_image_boxes(
        style,
        content_x,
        item.y - border_widths.top - padding.top - max(content_height, 0.0),
        max(content_width, 0.0),
        max(content_height, 0.0),
        image,
        layout_context
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
    intrinsic =
      case flex_main_axis(style) do
        :row ->
          Enum.reduce(lines, 0.0, &(&1.cross + &2)) +
            flex_cross_gap(style) * max(length(lines) - 1, 0)

        :column ->
          [line] = lines
          line.main
      end

    resolved_content_size(style, :height, Map.get(style, :_containing_height), intrinsic)
  end

  defp flex_line_cross(line, style, line_count, content_height) do
    case {flex_main_axis(style), line_count} do
      {:row, 1} -> content_height
      {:row, _} -> line.cross
      {:column, _} -> resolved_content_size(style, :width, nil, line.cross)
    end
  end

  defp flex_position_available_main(main_axis, content_width, content_height) do
    case main_axis do
      :row -> content_width
      :column -> content_height
    end
  end

  defp flex_main_distribution(style, line, available_main, gap) do
    free_space = available_main - line.main
    count = length(line.items)
    reversed = Map.get(style, :flex_direction, :row) in [:row_reverse, :column_reverse]
    start_offset = if reversed, do: free_space, else: 0.0

    case Map.get(style, :justify_content, :flex_start) do
      :flex_end ->
        {if(reversed, do: 0.0, else: free_space), gap}

      :center ->
        {free_space / 2, gap}

      :space_between when count > 1 and free_space > 0 ->
        {0.0, gap + free_space / (count - 1)}

      :space_around when count > 0 and free_space > 0 ->
        item_gap = gap + free_space / count
        {free_space / count / 2, item_gap}

      :space_evenly when count > 0 and free_space > 0 ->
        item_gap = gap + free_space / (count + 1)
        {free_space / (count + 1), item_gap}

      distribution when distribution in [:space_around, :space_evenly] ->
        {free_space / 2, gap}

      _ ->
        {start_offset, gap}
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
        case resolved_content_size(style, :height, Map.get(style, :_containing_height), nil) do
          height when is_number(height) ->
            height

          _ ->
            Enum.reduce(items, 0.0, &(&1.outer_main + &2)) + gap * max(length(items) - 1, 0)
        end
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
        |> HtmlValidator.trim_ascii_whitespace()
        |> text_width(style)

      %{type: :element, style: %{display: :none}} ->
        0.0

      %{type: :element, style: %{display: :image} = style} ->
        {width, _height} = image_content_size(style)
        width + horizontal_box_size(style)

      %{type: :element, style: %{embedded_document: _surface} = style} ->
        resolved_content_size(
          style,
          :width,
          nil,
          elem(EmbeddedViewportValidator.intrinsic_size(), 0)
        ) +
          horizontal_box_size(style)

      %{type: :element, style: style, children: children} when is_list(children) ->
        case inline_runs(children, style) do
          {:ok, runs} ->
            runs
            |> then(&intrinsic_inline_line_width(&1, nil))
            |> Kernel.+(horizontal_box_size(style))

          {:error, _reason} ->
            flex_block_intrinsic_width(children) + horizontal_box_size(style)
        end
    end
  end

  defp resolved_size(style, property, available_size, default) do
    size = resolve_size_value(Map.get(style, property), available_size, style) || default

    apply_size_constraints(style, property, size, available_size)
  end

  @doc "Resolves a validator-prepared CSS box content dimension with the shared min/max and box-sizing rules."
  @spec resolve_content_size(map(), :width | :height, number() | nil, number() | nil) ::
          number() | nil
  def resolve_content_size(style, property, available_size, default) do
    resolved_content_size(style, property, available_size, default)
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

  defp resolve_size_value(value, available_size, style \\ %{}) do
    case value do
      {:css_size_math, _} = calculation ->
        case NativeElixirPdfUtilities.Validators.CssSizingValidator.resolve_math(
               calculation,
               available_size,
               style
             ) do
          {:ok, size} -> size
          {:error, _} = error -> throw({:render_resource_limit, error})
        end

      {:min, sizes} when is_list(sizes) ->
        sizes
        |> Enum.map(&resolve_size_value(&1, available_size, style))
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
    |> resolve_size_value(available_size, style)
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
        |> then(fn {width, height} ->
          {apply_size_constraints(style, :width, width, available_width),
           apply_size_constraints(style, :height, height, available_height)}
        end)
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

    with {:ok,
          %{
            rows: rows,
            columns: columns,
            collapsed_edges: shared_edges,
            has_spanning_cells: spanning_cells?
          }} <-
           NativeElixirPdfUtilities.Validators.HtmlTableValidator.form(children, style) do
      # With no columns there are no cell edges for collapsed table borders.
      # Visible rows still contribute their declared height, but neither table
      # padding nor collapsed borders contribute to this grid's geometry.
      style =
        if collapsed? and table_column_count(rows) == 0 and columns == [],
          do: style |> Map.put(:border_widths, edges(0.0)) |> Map.put(:padding, edges(0.0)),
          else: style

      borders = Map.get(style, :border_widths, edges(0.0))
      padding = Map.get(style, :padding, edges(0.0))

      count = max(table_column_count(rows), length(columns))
      row_count = length(rows)

      boundary = %{
        top: for(column <- 0..max(count - 1, 0), do: {:horizontal, 0, column}),
        bottom: for(column <- 0..max(count - 1, 0), do: {:horizontal, row_count, column}),
        left: for(row <- 0..max(row_count - 1, 0), do: {:vertical, row, 0}),
        right: for(row <- 0..max(row_count - 1, 0), do: {:vertical, row, count})
      }

      outer =
        if collapsed? do
          Map.new(boundary, fn {side, keys} ->
            width =
              Enum.map(keys, &Map.get(shared_edges, &1, %{width: 0.0}).width)
              |> Enum.max()

            {side, width}
          end)
        else
          borders
        end

      outline_sides =
        Map.new(boundary, fn {side, keys} ->
          candidates = Enum.map(keys, &Map.get(shared_edges, &1)) |> Enum.reject(&is_nil/1)

          {side,
           collapsed? and Map.get(borders, side, 0.0) > 0 and
             candidates != [] and Enum.all?(candidates, &(&1.style != :hidden))}
        end)

      shared_edges =
        Enum.reduce(boundary, shared_edges, fn {side, keys}, edges ->
          if Map.fetch!(outline_sides, side) do
            keys
            |> Enum.filter(&Map.has_key?(edges, &1))
            |> Enum.reduce(edges, fn key, acc ->
              Map.update!(acc, key, fn candidate ->
                if elem(candidate.rank, 3) == 0,
                  do: Map.put(candidate, :outline, true),
                  else: candidate
              end)
            end)
          else
            edges
          end
        end)

      style =
        if collapsed? do
          outline_widths =
            Map.new(outline_sides, fn {side, paint?} ->
              {side, if(paint?, do: Map.fetch!(borders, side), else: 0.0)}
            end)

          outline_styles =
            Map.new(Map.get(style, :border_styles, edges(:none)), fn {side, border_style} ->
              paint_style =
                case border_style do
                  :inset -> :ridge
                  :outset -> :groove
                  value -> value
                end

              {side, paint_style}
            end)

          style
          |> Map.put(:border_widths, outline_widths)
          |> Map.put(:border_styles, outline_styles)
        else
          style
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
          shared_edges
        )

      tracks = Map.put(tracks, :has_spanning_cells, spanning_cells?)

      tracks =
        Map.put(
          tracks,
          :baseline_policy,
          HtmlTableValidator.baseline_policy(Map.get(style, :_table_atomic_context, false))
        )

      tracks =
        case Map.get(style, :_measure_inline_baseline) do
          reference when is_reference(reference) ->
            Map.put(tracks, :baseline_reference, reference)

          _ ->
            tracks
        end

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
          result =
            case table_boxes.after do
              [] ->
                {:ok,
                 mark_table_flow_baselines(
                   table_boxes.before ++ caption_boxes ++ row_boxes ++ bottom_caption_boxes,
                   row_fragments,
                   style
                 ), next_y - margin.bottom}

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
                        Map.get(box, :role) == :table_border and
                          Map.get(box, :table_id) == table_id
                      end)

                    content ++ outlines ++ borders
                  end)

                fragments = if row_fragments == [], do: outer_border_boxes, else: fragments

                {:ok,
                 mark_table_flow_baselines(
                   table_boxes.before ++ caption_boxes ++ fragments ++ bottom_caption_boxes,
                   row_fragments,
                   style
                 ), next_y - margin.bottom}
            end

          with {:ok, boxes, final_y} <- result do
            boxes =
              case Map.get(style, :_measure_table_reference) do
                reference when is_reference(reference) ->
                  # The private extent marker is removed by the Math adapter.
                  # Keep semantic target carriers on the source owner's paint.
                  boxes = attach_html_target(style, boxes, box_x, box_top)
                  HtmlValidator.reserve_render_resource(:max_table_grid_work, 1, :layout)

                  [
                    %{
                      type: :flow_marker,
                      role: :table_measurement,
                      table_measurement: reference,
                      x: box_x,
                      y: bottom,
                      width: 0.0,
                      height: 0.0,
                      table_measured_size: {box_width, box_height}
                    }
                    | boxes
                  ]

                _ ->
                  boxes
              end

            {:ok,
             CssTransformValidator.mark_border_box(style, boxes, %{
               x: box_x,
               top: box_top,
               width: box_width,
               height: box_top - next_y
             }), final_y}
          end
        end
      end
    end
  end

  # A table exports its row baselines, excluding caption and cell line markers.
  # The synthetic markers belong only to the current measurement identity.
  defp mark_table_flow_baselines(boxes, fragments, style) do
    case Map.get(style, :_measure_inline_baseline) do
      reference when is_reference(reference) ->
        if reference == Process.get({__MODULE__, :baseline_measurement}) and fragments != [] do
          HtmlValidator.reserve_render_resource(
            :max_table_grid_work,
            3 * length(boxes) + 2 * length(fragments),
            :layout
          )

          boxes =
            boxes
            |> Enum.reject(&(Map.get(&1, :type) == :baseline_measurement))
            |> Enum.map(&Map.delete(&1, :_measured_baseline))

          marker = fn baseline ->
            %{
              type: :baseline_measurement,
              x: 0.0,
              y: baseline,
              width: 0.0,
              height: 0.0,
              _measured_baseline: baseline
            }
          end

          [marker.(hd(fragments).baseline) | boxes] ++ [marker.(List.last(fragments).baseline)]
        else
          boxes
        end

      _ ->
        boxes
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
        boxes =
          style
          |> background_box(x, y, width, height)
          |> tag_boxes(metadata)
          |> Enum.map(&Map.put(&1, :role, :table_separate_background))

        %{before: boxes, after: []}
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
            table_id: table_id,
            table_caption_side: side
          })

        # Caption contents establish block formatting. Reuse ordinary block box
        # sizing and flow instead of maintaining an inline-only caption path.
        caption = %{caption | style: Map.put(style, :display, :block)}

        with {:ok, boxes, next_y} <- layout_block(caption, x, y, width) do
          boxes =
            Enum.map(boxes, fn box ->
              box =
                case Map.get(box, :flow_id) do
                  {:block, ^x, ^y} -> Map.put(box, :flow_id, {:table_caption, table_id})
                  _ -> box
                end

              Map.merge(box, Map.drop(flow_metadata, [:break_before, :break_after]))
            end)

          {:ok, boxes, next_y}
        end
    end
  end

  defp table_tracks(
         rows,
         columns,
         width,
         border_collapse,
         border_spacing,
         table_layout,
         shared_edges
       ) do
    case rows == [] or (table_column_count(rows) == 0 and columns == []) do
      true ->
        # Empty visible rows keep their row sizing and flow metadata. A table
        # without columns has no inter-cell spacing or row background area.
        %{rows: rows, widths: [], spacing: {0.0, 0.0}, width: width}

      false ->
        grid_rows = rows
        column_count = max(table_column_count(grid_rows), length(columns))

        grid_rows =
          case border_collapse do
            :collapse when map_size(shared_edges) == 0 ->
              # The validator's empty edge result has no visible or hidden
              # candidates. Retain zero border geometry without paint metadata.
              HtmlValidator.reserve_render_resource(
                :max_table_grid_work,
                length(grid_rows),
                :layout
              )

              Enum.map(grid_rows, fn row ->
                # Each cell issues a zero-edge map, style, cell and placement.
                HtmlValidator.reserve_render_resource(
                  :max_table_grid_work,
                  4 * length(row.cells),
                  :layout
                )

                cells =
                  Enum.map(row.cells, fn placement ->
                    cell = placement.cell

                    %{
                      placement
                      | cell: %{
                          cell
                          | style: Map.put(cell.style, :_collapsed_borders, edges(0.0))
                        }
                    }
                  end)

                %{row | cells: cells}
              end)

            :collapse ->
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

                    keys = %{
                      top: Enum.map(horizontal, &{:horizontal, row_index, &1}),
                      bottom:
                        Enum.map(horizontal, &{:horizontal, row_index + placement.rowspan, &1}),
                      left: Enum.map(vertical, &{:vertical, &1, placement.column}),
                      right:
                        Enum.map(vertical, &{:vertical, &1, placement.column + placement.colspan})
                    }

                    segments =
                      Map.new(keys, fn {side, entries} ->
                        {side,
                         Enum.map(entries, fn key ->
                           candidate = Map.fetch!(shared_edges, key)

                           Map.merge(candidate, %{
                             key: key,
                             paint:
                               candidate.owner == {row_index, placement.column, side} and
                                 not Map.get(candidate, :outline, false)
                           })
                         end)}
                      end)

                    paint_borders =
                      Map.new(segments, fn {side, candidates} ->
                        {side, Enum.any?(candidates, & &1.paint)}
                      end)

                    %{
                      placement
                      | cell: %{
                          cell
                          | style:
                              cell.style
                              |> Map.put(:_collapsed_borders, borders)
                              |> Map.put(:_collapsed_border_winners, paint_borders)
                              |> Map.put(:_collapsed_border_segments, segments)
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

        with {:ok, %{heights: row_heights, offsets: baseline_offsets, baselines: row_baselines}} <-
               table_row_heights(
                 grid_rows,
                 column_widths,
                 horizontal_spacing,
                 vertical_spacing,
                 available_height,
                 border_collapse,
                 Map.get(tracks, :baseline_reference),
                 tracks.baseline_policy
               ) do
          HtmlValidator.reserve_render_resource(
            :max_table_grid_work,
            3 * length(row_baselines),
            :layout
          )

          baseline_by_row =
            Map.new(Enum.with_index(row_baselines), fn {baseline, index} -> {index, baseline} end)

          {:ok, band_plans} =
            if tracks.baseline_policy == :fragmented_start and tracks.has_spanning_cells do
              HtmlTableValidator.prepare_row_band_geometry(
                grid_rows,
                row_heights,
                vertical_spacing
              )
            else
              {:ok, %{}}
            end

          {:ok, cell_backgrounds} =
            if tracks.has_spanning_cells do
              HtmlTableValidator.prepare_cell_background_geometry(
                grid_rows,
                row_heights,
                column_widths,
                tracks.spacing
              )
            else
              {:ok, nil}
            end

          row_width = Enum.sum(column_widths) + horizontal_spacing * (length(column_widths) + 1)

          {group_bounds, _} =
            grid_rows
            |> Enum.with_index()
            |> Enum.reduce({%{}, y - vertical_spacing}, fn {row, index}, {groups, top} ->
              bottom = top - Enum.at(row_heights, index)

              group =
                Map.get(groups, row.row_group, %{
                  x: x,
                  top: top,
                  width: row_width,
                  height: 0.0,
                  style: row.group_style || %{}
                })

              group = %{group | height: group.top - bottom}
              {Map.put(groups, row.row_group, group), bottom - vertical_spacing}
            end)

          group_scopes =
            Map.new(group_bounds, fn {id, bounds} ->
              {:ok, scope} =
                CssTransformValidator.prepare_scope(bounds.style, Map.delete(bounds, :style))

              {id, scope}
            end)

          {boxes, next_y, fragments, _pending} =
            grid_rows
            |> Enum.with_index()
            |> Enum.reduce({[], y - vertical_spacing, [], %{}}, fn {row, index},
                                                                   {boxes, current_y, fragments,
                                                                    pending} ->
              {:ok, row_boxes, next_y, metadata, continuations} =
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
                  vertical_spacing,
                  baseline_offsets,
                  band_plans,
                  cell_backgrounds,
                  y - vertical_spacing
                )

              carried = Map.get(pending, index, []) |> tag_atomic_boxes(metadata)

              {carried_backgrounds, carried_content} =
                Enum.split_with(
                  carried,
                  &(Map.get(&1, :role) in [
                      :table_row_group_background,
                      :table_row_background,
                      :table_cell_background
                    ])
                )

              {row_backgrounds, row_content} =
                Enum.split_with(
                  row_boxes,
                  &(Map.get(&1, :role) in [
                      :table_row_group_background,
                      :table_row_background,
                      :table_cell_background
                    ])
                )

              row_boxes = carried_backgrounds ++ row_backgrounds ++ carried_content ++ row_content

              row_boxes =
                CssTransformValidator.mark_border_box(row.row.style, row_boxes, %{
                  x: x,
                  top: current_y,
                  width: row_width,
                  height: current_y - next_y
                })

              {:ok, row_boxes} = CssTransformValidator.attach_scope(row.row.style, row_boxes)

              row_boxes =
                case Map.get(group_scopes, row.row_group) do
                  nil ->
                    row_boxes

                  scope ->
                    Enum.map(
                      row_boxes,
                      &Map.update(&1, :css_transform_scopes, [scope], fn scopes ->
                        [scope | scopes]
                      end)
                    )
                end

              pending =
                Enum.reduce(continuations, Map.delete(pending, index), fn {target, paint},
                                                                          pending ->
                  Map.update(pending, target, paint, &(&1 ++ paint))
                end)

              fragment = %{
                boxes: row_boxes,
                top: current_y,
                bottom: next_y,
                metadata: metadata,
                baseline:
                  case Map.get(baseline_by_row, index) do
                    nil -> nil
                    baseline -> current_y - baseline
                  end
              }

              {Enum.reverse(row_boxes, boxes), next_y - vertical_spacing, [fragment | fragments],
               pending}
            end)

          {:ok, Enum.reverse(boxes), next_y, Enum.reverse(fragments)}
        end
    end
  end

  defp layout_table_row(
         %{
           row: %{style: %{display: :table_row} = style} = row,
           section: section,
           group_style: group_style,
           row_group: row_group,
           cells: cells
         },
         table_id,
         index,
         x,
         y,
         column_widths,
         row_heights,
         border_collapse,
         horizontal_spacing,
         vertical_spacing,
         baseline_offsets,
         band_plans,
         cell_backgrounds,
         grid_top
       ) do
    row_height = Enum.at(row_heights, index)

    row_metadata =
      table_id
      |> table_row_metadata(section, index)
      |> Map.merge(break_metadata(style))
      |> Map.put(:row_group, {table_id, row_group})
      |> Map.put(:row_group_avoid, Map.get(group_style || %{}, :break_inside) == :avoid)

    group_background_boxes =
      if cell_backgrounds == nil,
        do:
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
          ),
        else: []

    row_background_boxes =
      if cell_backgrounds == nil,
        do:
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
          ),
        else: []

    {boxes, continuations} =
      Enum.reduce(cells, {[], []}, fn
        %{cell: cell, column: column, colspan: colspan, rowspan: rowspan},
        {boxes, continuations} ->
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

          cell =
            case Map.fetch(cell.style, :_collapsed_border_segments) do
              {:ok, segments} ->
                segments =
                  Map.new(segments, fn {side, candidates} ->
                    {side,
                     Enum.map(candidates, fn candidate ->
                       {offset, length} =
                         case candidate.key do
                           {:horizontal, _row, track} ->
                             {Enum.sum(Enum.slice(column_widths, column, track - column)),
                              Enum.at(column_widths, track)}

                           {:vertical, track, _column} ->
                             {Enum.sum(Enum.slice(row_heights, index, track - index)),
                              Enum.at(row_heights, track)}
                         end

                       Map.merge(candidate, %{offset: offset, length: length})
                     end)}
                  end)

                %{cell | style: Map.put(cell.style, :_collapsed_border_segments, segments)}

              :error ->
                cell
            end

          {:ok, cell_boxes} =
            layout_table_cell(
              cell,
              cell_x,
              y,
              cell_width,
              cell_height,
              row_metadata,
              border_collapse,
              Map.get(baseline_offsets, {index, column}, 0.0)
            )

          parent_backgrounds =
            case cell_backgrounds do
              nil ->
                []

              geometry ->
                geometry
                |> Map.fetch!({index, column})
                |> Enum.flat_map(fn layer ->
                  case layer.style do
                    nil ->
                      []

                    layer_style ->
                      coverage = layer.coverage
                      position = layer.position
                      clip = %{coverage | x: x + coverage.x, y: grid_top + coverage.y}
                      layer_style = Map.put(layer_style, :border_widths, edges(0.0))

                      colors =
                        css_background_box(
                          Map.put(layer_style, :background_image, nil),
                          clip.x,
                          clip.y,
                          clip.width,
                          clip.height
                        )

                      images =
                        css_background_box(
                          Map.put(layer_style, :background_color, nil),
                          x + position.x,
                          grid_top + position.y,
                          position.width,
                          position.height
                        )

                      (colors ++ images)
                      |> Enum.map(&Map.put(&1, :clip, clip))
                      |> tag_boxes(row_metadata)
                      |> Enum.map(&Map.put(&1, :role, layer.role))
                  end
                end)
            end

          cell_boxes = parent_backgrounds ++ cell_boxes

          case Map.get(band_plans, {index, column}) do
            nil ->
              {boxes ++ cell_boxes, continuations}

            bands ->
              cell_boxes =
                case HtmlTableValidator.prepare_cell_fields(
                       cell_boxes,
                       %{x: cell_x, y: y - cell_height, width: cell_width, height: cell_height},
                       ResourceContext.current(),
                       NativeElixirPdfUtilities.Validators.HtmlFormValidator.layout_mode()
                     ) do
                  {:ok, boxes} -> boxes
                  {:error, _} = failure -> throw({:render_resource_limit, failure})
                end

              clips =
                case HtmlTableValidator.prepare_cell_band_paint(cell_boxes, bands, y) do
                  {:ok, clips} -> clips
                  {:error, _} = failure -> throw({:render_resource_limit, failure})
                end

              fragments =
                Enum.map(clips, fn band ->
                  clip = band.clip

                  paint =
                    cell_boxes
                    |> Enum.reject(
                      &(Map.get(&1, :type) == :flow_marker and
                          Map.get(&1, :html_targets, []) == [])
                    )
                    |> Enum.chunk_by(&Map.get(&1, :role))
                    |> Enum.with_index()
                    |> Enum.map(fn {chunk, part} ->
                      Map.take(hd(chunk), [:role, :table_cell, :table_paint_id])
                      |> Map.merge(%{
                        type: :drawing_group,
                        x: clip.x,
                        y: clip.y,
                        width: clip.width,
                        height: clip.height,
                        baseline: 0.0,
                        transform: {1, 0, 0, 1, clip.x, clip.y},
                        clip: %{x: 0.0, y: 0.0, width: clip.width, height: clip.height},
                        boxes:
                          NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.translate_boxes(
                            chunk,
                            -clip.x,
                            -clip.y
                          ),
                        table_span_fragment:
                          Map.merge(band.cell_geometry, %{
                            source: {table_id, index, column},
                            part: part
                          })
                      })
                      |> Map.merge(row_metadata)
                    end)

                  {band.row, paint}
                end)

              {current, later} = Enum.split_with(fragments, fn {row, _paint} -> row == index end)

              if clips == [],
                do: {boxes ++ cell_boxes, continuations},
                else: {boxes ++ Enum.flat_map(current, &elem(&1, 1)), later ++ continuations}
          end
      end)

    {background_boxes, content_boxes} =
      Enum.split_with(
        boxes,
        &(Map.get(&1, :role) in [
            :table_row_group_background,
            :table_row_background,
            :table_cell_background
          ] and
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

    if Map.get(style, :position, :static) == :relative or
         Map.get(row, :positioned_children, []) != [] do
      row_x = x + horizontal_spacing
      row_width = Enum.sum(column_widths) + horizontal_spacing * max(length(column_widths) - 1, 0)
      bounds = %{x: row_x, top: y, width: row_width, height: row_height}
      row = %{row | style: Map.put(style, :border_widths, edges(0.0))}

      with {:ok, boxes, _} <-
             position_container_contents(row, boxes, y - row_height, row_x, y, row_width, bounds) do
        {:ok, boxes, y - row_height, row_metadata, continuations}
      end
    else
      {:ok, boxes, y - row_height, row_metadata, continuations}
    end
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
         %{style: %{display: :table_cell} = style, children: children} = cell,
         x,
         y,
         width,
         height,
         row_metadata,
         border_collapse,
         baseline_offset
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

      vertical_offset =
        if Map.get(style, :vertical_align, :middle) == :baseline,
          do: baseline_offset,
          else: table_cell_vertical_offset(style, content_area_height, content_height)

      content_boxes =
        Enum.map(content_boxes, fn box ->
          Map.update!(box, :y, &(&1 - vertical_offset))
        end)

      content_boxes =
        clip_overflow_contents(content_boxes, style, %{
          x: x + border_widths.left,
          y: y - height + border_widths.bottom,
          width: max(width - border_widths.left - border_widths.right, 0.0),
          height: max(height - border_widths.top - border_widths.bottom, 0.0)
        })

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

      boxes = attach_html_target(style, boxes, x, y)

      positioned_style =
        style |> Map.put(:margin, edges(0.0)) |> Map.put(:border_widths, border_widths)

      with {:ok, boxes, _} <-
             position_container_contents(
               %{cell | style: positioned_style},
               boxes,
               y - height,
               x,
               y,
               width,
               %{x: x, top: y, width: width, height: height}
             ) do
        boxes =
          CssTransformValidator.mark_border_box(style, boxes, %{
            x: x,
            top: y,
            width: width,
            height: height
          })

        CssTransformValidator.attach_scope(style, boxes)
      end
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
    case {border_collapse, Map.get(style, :_collapsed_border_segments)} do
      {:collapse, nil} ->
        []

      {:collapse, segments} ->
        uniform? =
          Enum.all?(segments, fn {_side, candidates} ->
            length(Enum.uniq(Enum.map(candidates, &{&1.width, &1.style, &1.color, &1.paint}))) ==
              1
          end)

        # A spanning side can have different winners on each underlying track.
        # Retain the ordinary rect for uniform sides; clip individual strokes
        # only when conflict resolution requires a partial side.
        entries =
          if uniform?,
            do: [%{side: nil, candidate: nil}],
            else:
              Enum.flat_map(segments, fn {side, candidates} ->
                for candidate <- candidates,
                    candidate.paint and candidate.width > 0,
                    do: %{side: side, candidate: candidate}
              end)

        Enum.flat_map(entries, fn entry ->
          widths =
            Map.new(segments, fn {side, candidates} ->
              candidate = if entry.side == nil, do: hd(candidates), else: entry.candidate

              {side,
               if((entry.side == nil or side == entry.side) and candidate.paint,
                 do: candidate.width,
                 else: 0.0
               )}
            end)

          styles =
            Map.new(segments, fn {side, candidates} ->
              candidate = if entry.side == nil, do: hd(candidates), else: entry.candidate
              # CSS2 maps these relief styles in the collapsed border model.
              paint_style =
                case candidate.style do
                  :inset -> :ridge
                  :outset -> :groove
                  value -> value
                end

              {side, paint_style}
            end)

          colors =
            Map.new(segments, fn {side, candidates} ->
              candidate = if entry.side == nil, do: hd(candidates), else: entry.candidate
              {side, candidate.color}
            end)

          if visible_border?(widths, styles, colors) do
            box = %{
              type: HtmlValidator.reserve_layout_box(:rect),
              x: x,
              y: y,
              width: width,
              height: height,
              fill_color: nil,
              stroke_color: colors.top,
              stroke_width: Enum.max(Map.values(widths)),
              border_widths: widths,
              border_colors: colors,
              border_styles: styles,
              fragment_bottom_border_width:
                if(
                  entry.side == nil and
                    Enum.all?(
                      segments.bottom,
                      &(not Map.get(&1, :outline, false) and &1.style != :hidden)
                    ),
                  do: Map.get(style, :border_widths, edges(0.0)).bottom,
                  else: 0.0
                ),
              border_radius: 0.0
            }

            clip =
              case entry.side do
                side when side in [:top, :bottom] ->
                  %{
                    x: x + entry.candidate.offset,
                    y: y - box.stroke_width,
                    width: entry.candidate.length,
                    height: height + 2 * box.stroke_width
                  }

                side when side in [:left, :right] ->
                  %{
                    x: x - box.stroke_width,
                    y: y + height - entry.candidate.offset - entry.candidate.length,
                    width: width + 2 * box.stroke_width,
                    height: entry.candidate.length
                  }

                nil ->
                  nil
              end

            [if(clip == nil, do: box, else: Map.put(box, :clip, clip))]
          else
            []
          end
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
        # Forced source line breaks remain independent even when soft wrapping
        # is disabled. Their widths cannot be added into one unbroken word.
        text
        |> String.split("\n", trim: false)
        |> Enum.flat_map(fn line ->
          tokens = inline_wrap_tokens(line, style, true)
          last = length(tokens) - 1

          tokens
          |> Enum.with_index()
          |> Enum.map(fn {token, index} -> intrinsic_text_width(token, style, index < last) end)
        end)
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
                {max(maximum, intrinsic_break_width(chunks)), atomic_width, [run], run}
              end

            run, {maximum, width, chunks, previous} ->
              line = Enum.reverse(chunks)

              token_width = intrinsic_text_width(run.text, run.style, false)

              if previous != nil and inline_break_boundary?(previous, run, line) do
                {max(maximum, intrinsic_break_width(chunks)), token_width, [run], run}
              else
                {maximum, width + token_width, [run | chunks], run}
              end
          end)

        max(maximum, width)
    end
  end

  # A min-content boundary includes the selected hyphen, while preserved source
  # spaces determine which opportunity is taken before their advances are trimmed.
  defp intrinsic_text_width(text, style, break_after?) do
    plan = inline_display_plan(text, style, break_after?)

    text =
      if Map.get(style, :white_space) in [:nowrap, :pre, :break_spaces],
        do: plan.text,
        else: HtmlValidator.trim_ascii_whitespace(plan.text)

    prepared_text_width(text, style)
  end

  defp intrinsic_break_width(chunks) do
    last = length(chunks) - 1

    chunks
    |> Enum.reverse()
    |> Enum.with_index()
    |> Enum.reduce(0.0, fn {run, index}, width ->
      # Atomic boxes retain their actual outer advance at the following break;
      # their empty source text is not a substitute for their intrinsic size.
      advance =
        if Map.get(run, :type) in [:inline_block, :inline_image],
          do: inline_run_width(run, nil),
          else: intrinsic_text_width(run.text, run.style, index == last)

      width + advance
    end)
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
         border_collapse,
         baseline_reference,
         baseline_policy
       ) do
    baseline? =
      Enum.any?(rows, fn row ->
        Enum.any?(row.cells, &(Map.get(&1.cell.style, :vertical_align, :middle) == :baseline))
      end)

    measure_rows =
      is_reference(baseline_reference) and
        baseline_reference == Process.get({__MODULE__, :baseline_measurement})

    if baseline? or measure_rows do
      result =
        Enum.reduce_while(Enum.with_index(rows), {:ok, %{}}, fn {row, index}, {:ok, metrics} ->
          measured =
            Enum.reduce_while(row.cells, {:ok, metrics}, fn placement, {:ok, metrics} ->
              width =
                table_cell_width(
                  column_widths,
                  placement.column,
                  placement.colspan,
                  horizontal_spacing
                )

              case table_cell_height(placement.cell, width, border_collapse, true) do
                {:ok, metric} ->
                  {:cont, {:ok, Map.put(metrics, {index, placement.column}, metric)}}

                error ->
                  {:halt, error}
              end
            end)

          case measured do
            {:ok, metrics} -> {:cont, {:ok, metrics}}
            error -> {:halt, error}
          end
        end)

      with {:ok, metrics} <- result,
           {:ok, geometry} <-
             NativeElixirPdfUtilities.Validators.HtmlTableValidator.prepare_baseline_geometry(
               rows,
               metrics,
               vertical_spacing,
               baseline_policy
             ) do
        heights =
          stretch_table_row_group_heights(
            rows,
            geometry.heights,
            available_height,
            vertical_spacing
          )

        heights = stretch_table_row_heights(rows, heights, available_height, vertical_spacing)

        baselines =
          Enum.zip([
            geometry.baselines,
            geometry.heights,
            heights,
            geometry.baseline_tracks_height
          ])
          |> Enum.map(fn {baseline, before, after_height, tracks_height} ->
            if tracks_height, do: baseline + after_height - before, else: baseline
          end)

        # Percentage descendants resolve only after row/group/table height
        # allocation. Re-measure affected baselines without feeding percentage
        # heights back into the intrinsic sizing pass.
        allocated =
          Enum.reduce(Enum.with_index(rows), metrics, fn {row, index}, current ->
            Enum.reduce(row.cells, current, fn placement, current ->
              cell = placement.cell

              if Map.get(cell.style, :vertical_align, :middle) == :baseline and
                   HtmlTableValidator.allocation_dependent_baseline?(cell) do
                width =
                  table_cell_width(
                    column_widths,
                    placement.column,
                    placement.colspan,
                    horizontal_spacing
                  )

                height =
                  Enum.sum(Enum.slice(heights, index, placement.rowspan)) +
                    vertical_spacing * (placement.rowspan - 1)

                {:ok, metric} =
                  table_cell_height(cell, width, border_collapse, true, height)
                  |> propagate_error()

                key = {index, placement.column}

                updated =
                  Map.merge(Map.fetch!(current, key), Map.take(metric, [:baseline, :has_flow]))

                Map.put(current, key, updated)
              else
                current
              end
            end)
          end)

        if allocated == metrics do
          {:ok, %{geometry | heights: heights, baselines: baselines}}
        else
          with {:ok, updated} <-
                 HtmlTableValidator.prepare_baseline_geometry(
                   rows,
                   allocated,
                   vertical_spacing,
                   baseline_policy
                 ) do
            {:ok, %{updated | heights: heights}}
          end
        end
      end
    else
      with {:ok, heights} <-
             table_row_heights_without_baselines(
               rows,
               column_widths,
               horizontal_spacing,
               vertical_spacing,
               available_height,
               border_collapse
             ),
           do: {:ok, %{heights: heights, offsets: %{}, baselines: []}}
    end
  end

  defp table_row_heights_without_baselines(
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

  defp table_cell_height(
         %{style: style, children: children},
         width,
         border_collapse,
         measure_baseline \\ false,
         allocated_height \\ nil
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

    content_width =
      width - border_widths.left - padding.left - padding.right - border_widths.right

    key = {__MODULE__, :baseline_measurement}
    previous = Process.get(key)
    reference = if measure_baseline, do: make_ref(), else: nil

    measured_children =
      if measure_baseline, do: inline_baseline_children(children, reference), else: children

    measured_style =
      if measure_baseline, do: Map.put(style, :_measure_inline_baseline, reference), else: style

    if measure_baseline, do: Process.put(key, reference)

    result =
      try do
        layout_table_cell_content(
          measured_children,
          measured_style,
          0.0,
          0.0,
          content_width,
          if(is_number(allocated_height),
            do:
              max(
                allocated_height - border_widths.top - padding.top - padding.bottom -
                  border_widths.bottom,
                0.0
              ),
            else: nil
          )
        )
      after
        if measure_baseline do
          if previous == nil, do: Process.delete(key), else: Process.put(key, previous)
        end
      end

    with {:ok, boxes, content_bottom} <- result do
      content_height = 0.0 - content_bottom

      border_height =
        case border_collapse do
          :collapse -> border_widths.top + border_widths.bottom
          _ -> border_widths.top + border_widths.bottom
        end

      content_box_height = padding.top + content_height + padding.bottom + border_height

      minimum_height = max(content_box_height, table_cell_declared_height(style, border_collapse))

      if measure_baseline do
        first = Enum.find(boxes, &Map.has_key?(&1, :_measured_baseline))

        has_flow =
          first != nil or
            Enum.any?(measured_children, fn child ->
              case child do
                %{style: %{display: :none}} -> false
                %{type: :text, text: text} -> String.trim(text) != ""
                _ -> true
              end
            end)

        {:ok,
         %{
           intrinsic_height: content_box_height,
           minimum_height: minimum_height,
           baseline:
             border_widths.top + padding.top +
               if(first, do: -first._measured_baseline, else: content_height),
           inset_bottom: border_widths.bottom + padding.bottom,
           has_flow: has_flow
         }}
      else
        {:ok, minimum_height}
      end
    end
  end

  defp table_cell_declared_height(style, border_collapse) do
    padding = Map.get(style, :padding, edges(0.0))
    border_widths = Map.get(style, :border_widths, edges(0.0))

    box_inset =
      case border_collapse do
        :collapse ->
          padding.top + padding.bottom +
            (Map.get(style, :_collapsed_borders, border_widths).top +
               Map.get(style, :_collapsed_borders, border_widths).bottom) / 2

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

  # Cell source lines retain their own paragraph identity beneath atomic row paint.
  defp table_cell_line_metadata(style, x, y) do
    %{
      flow_id: {:table_cell_source, x, y},
      fragment_lines: true,
      orphans: Map.get(style, :orphans, 2),
      widows: Map.get(style, :widows, 2),
      break_inside: Map.get(style, :break_inside, :auto)
    }
  end

  defp layout_table_cell_content(children, style, x, y, width, available_height) do
    case inline_runs(children, style) do
      {:ok, runs} ->
        runs = Enum.map(runs, &Map.put(&1, :_table_text, true))

        {boxes, content_height} =
          inline_text_boxes(runs, style, x, y, width, table_cell_line_metadata(style, x, y))

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
      {:ok, boxes, next_y, _margin_bottom} ->
        {:ok, position_split_inline_contexts(Enum.reverse(boxes), width), next_y}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp layout_table_cell_block(child, style, x, y, width, available_height) do
    case child do
      %{type: :inline_flow, children: children} ->
        layout_inline_flow(children, style, x, y, width, table_cell_line_metadata(style, x, y))

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
        acc + inline_fragment_advance(run, width)
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

        boxes =
          fragment_block_paints(background_box, item_boxes, style, box_x, box_top, box_height)
          |> then(
            &CssTransformValidator.mark_border_box(style, &1, %{
              x: box_x,
              top: box_top,
              width: box_width,
              height: box_height
            })
          )

        {:ok, boxes, next_y}

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

        markers =
          case Map.get(style, :list_style_image) do
            nil ->
              markers

            image ->
              image_style =
                marker_style
                |> Map.merge(%{
                  display: :image,
                  image_display: :inline,
                  image: image,
                  width: nil,
                  height: nil
                })

              [%{type: :element, tag: "img", style: image_style, children: []}]
          end

        inside? = Map.get(style, :list_style_position, :outside) == :inside

        gap =
          cond do
            markers == [] ->
              []

            Map.get(style, :list_style_image) != nil ->
              # Chromium's marker image margin is a fixed seven CSS pixels.
              [
                %{
                  type: :element,
                  tag: "span",
                  children: [],
                  style:
                    marker_style |> Map.merge(%{display: :inline_block, width: 5.25, height: 0.0})
                }
              ]

            true ->
              [%{type: :text, text: " ", style: marker_style}]
          end

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
          if is_number(marker_advance) and Map.get(style, :list_style_image) == nil do
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

        content =
          case {inside?, Map.get(style, :list_style_image)} do
            {false, %{height: height}} when height > 0 ->
              # An outside image marker participates in the first line's height
              # without consuming any of the list item's inline width.
              [
                %{
                  type: :element,
                  tag: "span",
                  children: [],
                  style:
                    Map.merge(marker_style, %{display: :inline_block, width: 0.0, height: height})
                }
                | content
              ]

            _ ->
              content
          end

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
              content_top = y - margin.top - borders.top - padding.top

              first_text =
                Enum.find(boxes, &(&1.type == :text and not Map.get(&1, :out_of_flow, false)))

              baseline =
                if first_text, do: first_text.y, else: content_top - text_baseline_depth(style)

              metadata =
                if first_text,
                  do:
                    Map.take(first_text, [
                      :flow_id,
                      :fragment_id,
                      :fragment_lines,
                      :fieldset_ancestors,
                      :break_before,
                      :break_after,
                      :break_inside
                    ]),
                  else: Map.put(break_metadata(style), :flow_id, {:list_item, x, y, index})

              {:ok, marker_runs} = inline_runs(markers, marker_style)
              [marker_runs] = inline_lines(marker_runs, nil)
              total_width = Enum.reduce(marker_runs, 0.0, &(&2 + inline_run_width(&1, nil)))

              gap_width =
                if Map.get(style, :list_style_image) != nil,
                  do: 5.25,
                  else: text_width(" ", marker_style)

              marker_x = x + margin.left - total_width - gap_width

              {marker_boxes, _} =
                Enum.map_reduce(marker_runs, marker_x, fn marker, current_x ->
                  marker_width = inline_run_width(marker, nil)

                  boxes =
                    inline_run_boxes(
                      marker,
                      current_x,
                      content_top,
                      baseline,
                      marker_width,
                      metadata
                    )

                  {boxes, current_x + marker_width}
                end)

              List.flatten(marker_boxes)
            end

          with {:ok, boxes} <- CssTransformValidator.attach_scope(style, marker_boxes ++ boxes),
               do: {:ok, boxes, next_y}
        end

      %{type: :element} ->
        layout_block(item, x, y, width)

      _ ->
        {:error, :invalid_layout}
    end
  end

  defp inline_text_boxes(runs, style, x, y, width, metadata, prepared_lines \\ nil) do
    lines = prepared_lines || inline_lines(runs, width)

    lines =
      lines
      |> Enum.with_index()
      |> Enum.map(fn {line_runs, index} ->
        clearance =
          if index == 0,
            do: Map.get(style, :_ruby_clearance, 0.0),
            else: max(style.line_height - style.font_size, 0.0)

        line_runs =
          Enum.map(line_runs, fn run ->
            case run do
              %{style: %{_ruby_box: measured}} ->
                borrowed = min(measured.annotation_ascent - measured.base_ascent, clearance)

                run
                |> Map.update!(:_inline_baseline_depth, &(&1 - borrowed))
                |> Map.update!(:_inline_content_height, &(&1 - borrowed))

              run ->
                run
            end
          end)

        line_runs
      end)

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

        metadata =
          if Enum.any?(line_runs, &match?(%{style: %{_atomic_table_flow: true}}, &1)) do
            metadata
            |> Map.put_new(:flow_id, {:inline_flow, x, y})
            |> Map.put_new(:fragment_lines, true)
          else
            metadata
          end

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

        # Empty atomic boxes use the bottom margin edge; Math and temporal flex
        # editors can also expose a fractional baseline. Preserve the shared depth
        # after rasterizing the line's top edge.
        line_metadata =
          if baseline_depth / @css_pixel_points !=
               Float.floor(baseline_depth / @css_pixel_points) and
               Enum.any?(line_runs, fn run ->
                 (Map.has_key?(run.style, :_math_box) or
                    Map.get(run, :_inline_bottom_baseline, false) or
                    Map.has_key?(run.style, :_temporal_display) or
                    Map.has_key?(run.style, :_file_display)) and
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

            {Enum.reverse(run_boxes, acc), current_x + inline_fragment_advance(run, width)}
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
          if Enum.any?(
               line_runs,
               &(Map.get(&1, :type) == :inline_image or Map.has_key?(&1.style, :_ruby_box))
             ) do
            marker =
              %{
                type: HtmlValidator.reserve_layout_box(:flow_marker),
                x: start_x,
                y: line_top - current_line_height,
                width: 0.0,
                height: current_line_height,
                role:
                  if(Enum.any?(line_runs, &Map.has_key?(&1.style, :_ruby_box)),
                    do: :ruby_line,
                    else: :inline_image_line
                  )
              }
              |> Map.merge(line_metadata)

            [marker | line_boxes]
          else
            line_boxes
          end

        line_boxes =
          if Enum.any?(line_boxes, &Map.has_key?(&1, :_css_actual_text)) do
            Enum.map(
              line_boxes,
              &Map.put(&1, :_css_text_line, {start_x, line_top, current_line_height})
            )
          else
            line_boxes
          end

        line_boxes =
          if Map.get(line_metadata, :split_inline_fragment, false) do
            marker =
              %{
                type: HtmlValidator.reserve_layout_box(:flow_marker),
                role: :inline_fragment_line,
                x: start_x,
                y: line_top - current_line_height,
                width: 0.0,
                height: current_line_height
              }
              |> Map.merge(line_metadata)

            [marker | line_boxes]
          else
            line_boxes
          end

        line_boxes = mark_inline_flow_baseline(line_boxes, line_runs, style, baseline_y)

        {line_boxes ++ boxes, consumed_height + current_line_height}
      end)

    {Enum.reverse(boxes) |> position_inline_contexts(width) |> group_discretionary_text(),
     inline_content_height(runs, lines, style)}
  end

  defp group_discretionary_text(boxes) do
    boxes
    |> Enum.chunk_by(fn box ->
      if Map.get(box, :type) == :text or Map.get(box, :role) == :discretionary_text,
        do: Map.get(box, :_css_text_line),
        else: :other
    end)
    |> Enum.flat_map(fn chunk ->
      if Enum.any?(chunk, &Map.has_key?(&1, :_css_actual_text)) do
        first = hd(chunk)
        {_origin_x, top, height} = Map.fetch!(first, :_css_text_line)
        children = Enum.filter(chunk, &(Map.get(&1, :type) == :text))
        x = Enum.min(Enum.map(chunk, & &1.x))

        right =
          Enum.max(Enum.map(children, &(&1.x + Map.get(&1, :annotation_width, 0.0))), fn -> x end)

        bottom = top - height
        actual = Enum.map_join(chunk, &Map.get(&1, :_css_actual_text, Map.get(&1, :text, "")))

        targets =
          Enum.flat_map(chunk, fn box ->
            Enum.map(Map.get(box, :html_targets, []), fn target ->
              %{target | dx: target.dx + box.x - x, dy: target.dy + box.y - bottom}
            end)
          end)

        group =
          first
          |> Map.drop([
            :text,
            :font,
            :font_face,
            :font_size,
            :letter_spacing,
            :color,
            :annotation_width,
            :_css_actual_text,
            :_css_text_line,
            :link_url,
            :link_destination,
            :role
          ])
          |> Map.merge(%{
            type: HtmlValidator.reserve_layout_box(:glyph_group),
            actual_text: actual,
            x: x,
            y: bottom,
            width: right - x,
            height: height,
            html_targets: targets,
            boxes: Enum.map(children, &Map.drop(&1, [:_css_actual_text, :_css_text_line]))
          })

        [group]
      else
        Enum.map(chunk, &Map.delete(&1, :_css_text_line))
      end
    end)
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

  # Ordinary CSS overflow uses the allocated padding box; native controls keep
  # their separately validated client/scrollbar viewport.
  defp clip_overflow_contents(boxes, style, clip, scrollport \\ nil) do
    cond do
      scrollport != nil ->
        client_clip = %{
          clip
          | width: scrollport.client_width,
            height: scrollport.client_height,
            y: clip.y + scrollport.viewport_height - scrollport.client_height
        }

        parts =
          case NativeElixirPdfUtilities.Validators.ScrollbarValidator.prepare(scrollport) do
            {:ok, parts} -> parts
            {:error, _} = error -> throw({:render_resource_limit, error})
          end

        artwork =
          parts
          |> NativeElixirPdfUtilities.HtmlToPdf.ScrollbarPainter.paint()
          |> shift_layout_boxes(clip.x, clip.y)
          |> Enum.map(&Map.put(&1, :snap_to_css_pixel_grid, true))

        # Chromium repaints a scrolling element's background in its client
        # layer after the border, before the clipped scrolling contents.
        background =
          if scrollport.vertical or scrollport.horizontal do
            style
            |> Map.merge(%{border_widths: edges(0.0), border_radius: 0.0})
            |> background_box(
              clip.x - style.border_widths.left,
              clip.y - style.border_widths.bottom,
              clip.width + style.border_widths.left + style.border_widths.right,
              clip.height + style.border_widths.top + style.border_widths.bottom
            )
            |> Enum.map(&Map.delete(&1, :paint_layer))
          else
            []
          end

        # Keep the clip in an overflow group so the writer rounds its origin
        # after positioned layout has placed the box on the printed page.
        scrolling_contents = %{
          type: HtmlValidator.reserve_layout_box(:drawing_group),
          role: :overflow_clip,
          css_scrollport: true,
          x: client_clip.x,
          y: client_clip.y,
          width: client_clip.width,
          height: client_clip.height,
          transform: {1, 0, 0, 1, 0, 0},
          clip: client_clip,
          boxes: background ++ boxes
        }

        background =
          if background == [], do: [], else: [%{scrolling_contents | boxes: background}]

        background ++
          NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.clip_boxes(boxes, client_clip) ++
          artwork

      (Map.get(style, :overflow, :visible) == :hidden or
         (Map.get(style, :_popover, false) and Map.get(style, :overflow) == :auto)) and
          Map.get(style, :form_kind) not in [:text, :textarea, :select, :listbox] ->
        NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.clip_boxes(boxes, clip)

      true ->
        boxes
    end
  end

  # Block and atomic inline controls share one viewport. Offscreen artwork
  # must not enlarge a control's page allocation, while fields keep their value.
  defp clip_control_contents(boxes, style, clip) do
    if Map.get(style, :form_kind) in [:text, :textarea, :select, :listbox] do
      offset =
        NativeElixirPdfUtilities.Validators.HtmlFormValidator.initial_control_content_offset(
          style,
          clip.height
        )

      boxes = shift_layout_boxes(boxes, 0.0, -offset)
      # A number's invisible initial spin button still occupies its fixed
      # native 15px width in the inner editor. Keep it outside the text clip.
      {boxes, clip} =
        if Map.has_key?(style, :_number_state) do
          {boxes, %{clip | width: max(clip.width - 11.25, 0.0)}}
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
    shift =
      case Map.get(run, :_split_inline_alignment, 0.0) do
        {:top, depth, _height} ->
          line_top - depth - baseline_y

        {:bottom, depth, height} ->
          line_top - metadata.line_box_height + height - depth - baseline_y

        shift ->
          shift
      end

    baseline_y = baseline_y + shift

    metadata =
      if shift == 0,
        do: metadata,
        else: Map.update!(metadata, :line_baseline_depth, &(&1 - shift))

    metadata =
      if shift == 0,
        do: metadata,
        else:
          Map.put(
            metadata,
            :baseline_snap_depth,
            -shift - Float.floor(-shift / @css_pixel_points) * @css_pixel_points
          )

    boxes =
      case run do
        %{type: :inline_block, style: %{_ruby_box: measured}} ->
          shifted = shift_layout_boxes(measured.boxes, x, baseline_y)

          Enum.zip(measured.boxes, shifted)
          |> Enum.map(fn {local, box} ->
            metadata =
              if box.type == :text and Map.has_key?(metadata, :line_baseline_depth),
                do: Map.update!(metadata, :line_baseline_depth, &(&1 - local.y)),
                else: metadata

            Map.merge(box, metadata)
          end)
          |> inherit_inline_positioning_contexts(Map.get(run, :inline_positioning_contexts, []))

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
              region = %{
                x: x + item.x,
                y: baseline_y + item.y,
                width: item.width,
                height: item.height
              }

              item.style
              |> background_box(region.x, region.y, region.width, region.height)
              |> attach_math_link(item.style, region)
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
                      math_source_fragments: Map.get(glyph, :source_fragments, []),
                      displayed_unicode: Map.get(glyph, :displayed_unicode),
                      glyph_ids: [glyph.glyph_id],
                      x: x + glyph.x,
                      y: baseline_y + glyph.y,
                      width: glyph.width,
                      height: glyph.height,
                      font: Font.pdf_name(glyph.font_face),
                      font_face: glyph.font_face,
                      font_size: glyph.font_size,
                      color: glyph.color,
                      snap_to_css_pixel_grid: false,
                      css_source_positioned: true,
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

          decorations =
            text_decoration_boxes(
              %{
                x: x,
                y: baseline_y,
                font_size: style.font_size,
                annotation_width: measured.width
              },
              style,
              metadata
            )

          link_region = %{
            x: x,
            y: baseline_y - measured.descent,
            width: measured.width,
            height: measured.ascent + measured.descent
          }

          paint_marker =
            if Map.has_key?(metadata, :flow_id) do
              case NativeElixirPdfUtilities.Validators.MathPaintValidator.prepare_geometry(
                     measured
                   ) do
                {:ok, nil} ->
                  []

                {:ok, {left, bottom, right, top}} ->
                  [
                    %{
                      type: HtmlValidator.reserve_layout_box(:flow_marker),
                      role: :math_paint,
                      x: x + left,
                      y: baseline_y + bottom,
                      width: 0.0,
                      height: top - bottom,
                      math_paint_width: right - left
                    }
                  ]

                {:error, _} = error ->
                  throw({:render_resource_limit, error})
              end
            else
              []
            end

          boxes =
            attach_math_link(
              background ++ backgrounds ++ glyphs ++ decorations ++ paint_marker,
              style,
              link_region
            )

          Enum.map(boxes, fn box ->
            inherited =
              if Map.get(box, :_math_native_paint, false),
                do:
                  Map.drop(metadata, [
                    :line_baseline_depth,
                    :line_box_height,
                    :baseline_snap_depth
                  ]),
                else: metadata

            inherited =
              if box.type == :text and not Map.get(box, :_math_native_paint, false) and
                   Map.has_key?(inherited, :line_baseline_depth),
                 do: Map.update!(inherited, :line_baseline_depth, &(&1 - (box.y - baseline_y))),
                 else: inherited

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
              Map.get(style, :image, :deferred)
            )

          (background ++ images)
          |> Enum.map(fn box ->
            box = Map.merge(box, metadata)

            box =
              if (box.type == :image or Map.get(box, :role) == :responsive_image) and
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
          |> then(
            &CssTransformValidator.mark_border_box(style, &1, %{
              x: box_x,
              top: box_top,
              width: box_width,
              height: box_height
            })
          )
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

          {:ok, boxes, _next_y} =
            layout_block(
              %{type: :element, style: style, children: children},
              x,
              baseline_y + Map.fetch!(run, :_inline_baseline_depth),
              inline_run_width(run, width)
            )
            |> propagate_error()

          Enum.map(boxes, &Map.merge(&1, metadata))

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
          content_height = run._inline_content_height
          box_width = content_width + horizontal_box_size(style)
          box_height = content_height + vertical_box_size(style)

          box_top =
            case Map.get(style, :vertical_align, :baseline) do
              :top -> line_top - margin.top
              :bottom -> line_top - metadata.line_box_height + box_height + margin.bottom
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
            HtmlTableValidator.with_layout_context(:surface, fn ->
              case run do
                %{style: %{embedded_document: surface}} ->
                  case layout_embedded_document(
                         surface,
                         content_x,
                         content_top,
                         content_width,
                         content_height,
                         metadata
                       ) do
                    {:ok, groups, height} ->
                      {groups, height}

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
                    {:error, _} = error -> throw({:render_resource_limit, error})
                  end
              end
            end)

          content_boxes =
            clip_control_contents(content_boxes, style, %{
              x: content_x,
              y: content_top - content_height,
              width: content_width,
              height: content_height
            })

          content_boxes =
            clip_overflow_contents(content_boxes, style, %{
              x: content_x - padding.left,
              y: content_top - content_height - padding.bottom,
              width: content_width + padding.left + padding.right,
              height: content_height + padding.top + padding.bottom
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

          boxes =
            CssTransformValidator.mark_border_box(style, boxes, %{
              x: box_x,
              top: box_top,
              width: box_width,
              height: box_height
            })

          boxes =
            if Map.get(style, :_atomic_table_flow, false) do
              # Keep child line metrics and table identities, while the existing
              # outer line owns the atomic flow group and its full allocation.
              flow =
                Map.drop(metadata, [:line_box_height, :line_baseline_depth, :baseline_snap_depth])

              marker =
                %{
                  type: HtmlValidator.reserve_layout_box(:flow_marker),
                  x: x,
                  y: line_top - metadata.line_box_height,
                  width: 0.0,
                  height: metadata.line_box_height,
                  role: :atomic_table_line
                }
                |> Map.merge(metadata)

              [marker | tag_atomic_boxes(boxes, flow)]
            else
              boxes
            end

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
              width: Map.get(run, :_inline_padding_width, 0.0),
              height: line_height,
              inline_positioning_contexts: Map.fetch!(run, :inline_positioning_contexts)
            }
            |> Map.merge(metadata)
          ]

        _ ->
          box = run |> text_box(x, baseline_y, width) |> Map.merge(metadata)

          boxes =
            if box.text == "" and Map.has_key?(box, :_css_actual_text) do
              [
                box
                |> Map.drop([
                  :text,
                  :font,
                  :font_face,
                  :font_size,
                  :letter_spacing,
                  :color,
                  :annotation_width,
                  :link_url,
                  :link_destination
                ])
                |> Map.merge(%{
                  type: HtmlValidator.reserve_layout_box(:flow_marker),
                  role: :discretionary_text,
                  width: 0.0,
                  y: line_top - metadata.line_box_height,
                  height: metadata.line_box_height
                })
              ]
            else
              case run.style do
                %{_text_decorations: _} ->
                  text_decoration_boxes(box, run.style, metadata) ++ [box]

                _ ->
                  [box]
              end
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
          fragment_width = inline_fragment_advance(run, width)

          backgrounds =
            Enum.flat_map(backgrounds, fn background ->
              height = background.ascent + background.descent

              paint_width = fragment_width - background.margin_width

              paint_x =
                x + if(background.margin_side == :left, do: background.margin_width, else: 0.0)

              y = baseline_y - background.descent

              context_marker =
                case background.inline_context_id do
                  nil ->
                    []

                  id ->
                    [
                      %{
                        type: HtmlValidator.reserve_layout_box(:flow_marker),
                        role: :inline_context_fragment,
                        inline_context_id: id,
                        x: paint_x,
                        y: y,
                        width: max(paint_width, 0.0),
                        height: height,
                        out_of_flow: true
                      }
                      |> Map.merge(metadata)
                      |> Map.put(:out_of_flow, true)
                    ]
                end

              if paint_width > 0 and height > 0 do
                paint =
                  if Map.get(background.style, :background_image) == nil do
                    background_box(background.style, paint_x, y, paint_width, height)
                  else
                    source = %{
                      id: background.id,
                      fragment: make_ref(),
                      style: background.source_style
                    }

                    [
                      %{
                        type: HtmlValidator.reserve_layout_box(:rect),
                        x: paint_x,
                        y: y,
                        width: paint_width,
                        height: height,
                        fill_color: {0, 0, 0},
                        stroke_color: nil,
                        stroke_width: 0.0,
                        border_radius: 0.0,
                        inline_background_image: source
                      }
                    ]
                  end

                context_marker ++
                  (paint
                   |> Enum.map(fn box ->
                     box
                     |> Map.put(:role, :inline_background)
                     |> Map.merge(metadata)
                     |> Map.put(:out_of_flow, true)
                   end))
              else
                context_marker
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

    boxes =
      case run do
        %{type: type, style: style} when type in [:inline_block, :inline_image] ->
          measured = Map.get(style, :_math_box) || Map.get(style, :_ruby_box)

          # Math and ruby use their prepared atomic geometry. Ordinary images
          # and inline blocks already mark their border box above; SVG owns
          # its source transform inside the converted image.
          boxes =
            if measured != nil and Map.get(style, :css_transform, :none) != :none and
                 not Enum.any?(boxes, &(Map.get(&1, :css_transform_border_box) != nil)) do
              margin = Map.get(style, :margin, edges(0.0))

              CssTransformValidator.mark_border_box(style, boxes, %{
                x: x + margin.left,
                top: baseline_y + measured.ascent,
                width: measured.width,
                height: measured.ascent + measured.descent
              })
            else
              boxes
            end

          {:ok, boxes} = CssTransformValidator.attach_scope(style, boxes) |> propagate_error()
          boxes

        _ ->
          boxes
      end

    boxes = inherit_split_inline_contexts(boxes, Map.get(run, :split_inline_contexts, []))

    boxes =
      if Map.get(run, :split_inline_text, false),
        do: Enum.map(boxes, &Map.put(&1, :split_inline_text, true)),
        else: boxes

    case run do
      %{outline_anchor: anchor} -> attach_outline_anchor(%{outline_anchor: anchor}, boxes)
      _ -> boxes
    end
  end

  # An enclosing link uses the validated annotation surface of the actual
  # measured equation. Glyph/native paint and source identities stay intact.
  defp attach_math_link(boxes, style, region) do
    if (Map.get(style, :link_url) != nil or Map.get(style, :link_destination) != nil) and
         region.width > 0 and region.height > 0 do
      annotation =
        region
        |> Map.put(:type, HtmlValidator.reserve_layout_box(:link_region))
        |> Map.merge(Map.take(style, [:link_url, :link_destination]))

      boxes ++ [annotation]
    else
      boxes
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

          rect = %{
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

          rect =
            case Map.get(decoration, :style, :solid) do
              pattern when pattern in [:dotted, :dashed] ->
                Map.merge(rect, %{
                  fill_color: nil,
                  border_widths: edges(0.0, 0.0, decoration.thickness, 0.0),
                  border_styles: edges(:none, :none, pattern, :none),
                  border_colors: edges(decoration.color)
                })

              _ ->
                rect
            end

          Map.merge(rect, metadata)
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

    fragment_boxes =
      if Map.get(context, :split_inline, false),
        do: Enum.filter(member_boxes, &(Map.get(&1, :inline_context_id) == context.id)),
        else: []

    containing_block =
      case fragment_boxes do
        [] ->
          inline_positioning_containing_block(member_boxes)

        [first | _] ->
          last = List.last(fragment_boxes)

          %{
            x: first.x,
            top: first.y + first.height,
            width: max(last.x + last.width - first.x, 0.0),
            height: max(first.y + first.height - last.y, 0.0)
          }
      end

    containing_block =
      if Map.get(context, :split_inline, false) do
        border = Map.get(context.style, :border_widths, edges(0.0))

        %{
          containing_block
          | x: containing_block.x + border.left,
            top: containing_block.top - border.top,
            width: max(containing_block.width - border.left - border.right, 0.0),
            height: max(containing_block.height - border.top - border.bottom, 0.0)
        }
      else
        containing_block
      end

    case positioned_child_boxes(context.positioned_children, containing_block, member_boxes) do
      {:ok, positioned_boxes} ->
        ancestors = inline_positioning_ancestors(member_boxes, context.id)

        {boxes, positioned_boxes} =
          if Map.get(context, :split_inline, false) and positioned_boxes != [] do
            {anchor, index} =
              Enum.find(member_indexes, fn {box, _} ->
                Map.get(box, :inline_context_id) == context.id
              end) || hd(member_indexes)

            boxes =
              List.update_at(
                boxes,
                index,
                &Map.put(&1, :inline_fragment_anchors, [context.id])
              )

            positioned =
              Enum.map(positioned_boxes, fn entry ->
                %{
                  entry
                  | boxes:
                      Enum.map(
                        entry.boxes,
                        &Map.put(&1, :inline_positioned_anchor, %{
                          id: context.id,
                          source_y: anchor.y
                        })
                      )
                }
              end)

            {boxes, positioned}
          else
            {boxes, positioned_boxes}
          end

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

    boxes =
      Enum.map(boxes, fn box ->
        if Map.get(box, :split_inline_text, false) and box.type == :text do
          depth = Map.get(box, :baseline_snap_depth, 0.0) - delta_y
          # Offset modulo one CSS pixel retains the pre-translation baseline
          # snapping while satisfying the writer's nonnegative depth contract.
          depth = depth - Float.floor(depth / @css_pixel_points) * @css_pixel_points
          box |> Map.put(:baseline_snap_depth, depth) |> Map.delete(:split_inline_text)
        else
          Map.delete(box, :split_inline_text)
        end
      end)

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

  defp inline_line_height(line_runs, parent_style, _available_width) do
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
            %{type: type, style: %{vertical_align: align}}
            when align in [:top, :bottom] and
                   type in [:inline_block, :inline_image] ->
              {above, below}

            %{type: type, _inline_baseline_depth: depth, style: style} = run
            when type in [:inline_block, :inline_image] ->
              margin = Map.get(style, :margin, edges(0.0))
              height = Map.fetch!(run, :_inline_content_height) + vertical_box_size(style)
              {max(above, margin.top + depth), max(below, height + margin.bottom - depth)}

            %{style: style} ->
              depth = text_baseline_depth(style)

              case Map.get(run, :_split_inline_alignment, 0.0) do
                {_side, _depth, _height} ->
                  {above, below}

                shift ->
                  {max(above, depth + shift),
                   max(below, Map.fetch!(style, :line_height) - depth - shift)}
              end
          end
        end)

      Enum.reduce(line_runs, ascent + descent, fn run, height ->
        case run do
          %{type: type, style: %{vertical_align: align} = style} = run
          when align in [:top, :bottom] and
                 type in [:inline_block, :inline_image] ->
            margin = Map.get(style, :margin, edges(0.0))

            max(
              height,
              run._inline_content_height + vertical_box_size(style) + margin.top + margin.bottom
            )

          %{type: type, _inline_baseline_depth: _depth}
          when type in [:inline_block, :inline_image] ->
            height

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
          %{type: type, style: %{vertical_align: align}}
          when align in [:top, :bottom] and
                 type in [:inline_block, :inline_image] ->
            depth

          %{type: type, _inline_baseline_depth: baseline, style: style}
          when type in [:inline_block, :inline_image] ->
            margin = Map.get(style, :margin, edges(0.0))
            max(depth, margin.top + baseline)

          %{style: style} ->
            case Map.get(run, :_split_inline_alignment, 0.0) do
              {_side, _depth, _height} -> depth
              shift -> max(depth, text_baseline_depth(style) + shift)
            end
        end
      end)

    bottom_runs =
      Enum.filter(line_runs, fn run ->
        match?({:bottom, _, _}, Map.get(run, :_split_inline_alignment)) or
          match?(
            %{type: type, style: %{vertical_align: :bottom}}
            when type in [:inline_block, :inline_image],
            run
          )
      end)

    case bottom_runs do
      [] ->
        depth

      _ ->
        baseline_runs =
          Enum.reject(line_runs, fn run ->
            match?(
              {side, _, _} when side in [:top, :bottom],
              Map.get(run, :_split_inline_alignment)
            ) or
              match?(
                %{type: type, style: %{vertical_align: align}}
                when align in [:top, :bottom] and
                       type in [:inline_block, :inline_image],
                run
              )
          end)

        base_height = inline_line_height(baseline_runs, parent_style, nil)

        Enum.reduce(bottom_runs, depth, fn run, above ->
          margin = Map.get(run.style, :margin, edges(0.0))

          height =
            case Map.get(run, :_split_inline_alignment) do
              {:bottom, _depth, height} ->
                height

              _ ->
                run._inline_content_height + vertical_box_size(run.style) + margin.top +
                  margin.bottom
            end

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

        %{type: :inline_block, style: %{_ruby_box: _, white_space: mode}}
        when mode not in [:nowrap, :pre] and is_number(width) ->
          append_wrapped_ruby(lines, run, width)

        %{type: :inline_block, style: %{display: display}}
        when display in [:inline_block, :inline_flex, :inline_grid] ->
          run = if measure_blocks?, do: measure_inline_block(run, width), else: run
          append_inline_atomic(lines, run, width)

        %{type: :inline_positioning_marker} ->
          List.update_at(lines, -1, &(&1 ++ [run]))

        %{type: type} when type in [:inline_block, :inline_image] ->
          append_inline_atomic(lines, run, width)

        _ ->
          parts = String.split(run.text, "\n", trim: false)

          {parts, _} =
            Enum.map_reduce(parts, 0, fn part, offset ->
              {NativeGlyphSourceValidator.slice_run(run, offset, part),
               offset + byte_size(part) + 1}
            end)

          parts
          |> Enum.with_index()
          |> Enum.reduce(lines, fn {part_run, index}, acc ->
            part = part_run.text

            acc =
              case part do
                "" -> acc
                _ -> append_wrapped_inline_text(acc, part_run, width)
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
        Map.get(run, :hard_break, false) or HtmlValidator.trim_ascii_whitespace(run.text) != ""
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
          HtmlValidator.trim_ascii_whitespace(text) != ""

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
                 HtmlValidator.trim_ascii_whitespace(run.text) == "")
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
          NativeGlyphSourceValidator.trim_run(run, trim_trailing_inline_text(run.text))
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

    # The containing inline context decides placement breaks. An atomic
    # control's editor white-space applies only to its own contents.
    outer_white_space = Map.get(run, :_inline_outer_white_space, Map.get(run.style, :white_space))

    case is_number(width) and outer_white_space not in [:nowrap, :pre] and
           Enum.any?(current_line, &(Map.get(&1, :type) != :inline_positioning_marker)) and
           inline_line_width(current_line, width) + inline_run_width(run, width) >
             width do
      true -> List.update_at(lines, -1, &select_discretionary_break/1) ++ [[run]]
      false -> List.update_at(lines, length(lines) - 1, &(&1 ++ [run]))
    end
  end

  defp append_wrapped_ruby(lines, run, width) do
    current = List.last(lines) || []
    available = width - inline_line_width(current, width)

    cond do
      NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator.fits_width?(
        inline_run_width(run, width),
        available
      ) ->
        List.update_at(lines, -1, &(&1 ++ [run]))

      true ->
        case NativeElixirPdfUtilities.HtmlToPdf.RubyLayout.split(
               run.style._ruby_box,
               max(available, 0.0)
             ) do
          {:ok, {prefix, suffix}} ->
            fragments =
              Enum.map([prefix, suffix], fn measured ->
                run
                |> Map.put(:style, Map.put(run.style, :_ruby_box, measured))
                |> Map.put(:_inline_content_width, measured.width)
                |> Map.put(:_inline_content_height, measured.ascent + measured.descent)
                |> Map.put(:_inline_baseline_depth, measured.ascent)
                |> Map.put(:_ruby_overhang_before, 0.0)
                |> Map.put(:_ruby_overhang_after, 0.0)
              end)

            [prefix, suffix] = fragments
            lines = List.update_at(lines, -1, &(&1 ++ [prefix]))

            append_wrapped_ruby(
              List.update_at(lines, -1, &select_discretionary_break/1) ++ [[]],
              suffix,
              width
            )

          {:ok, nil} ->
            if Enum.any?(current, &(Map.get(&1, :type) != :inline_positioning_marker)) do
              append_wrapped_ruby(
                List.update_at(lines, -1, &select_discretionary_break/1) ++ [[]],
                run,
                width
              )
            else
              List.update_at(lines, -1, &(&1 ++ [run]))
            end

          {:error, _} = error ->
            throw({:render_resource_limit, error})
        end
    end
  end

  defp append_wrapped_inline_text(lines, run, width) do
    case width do
      width when is_number(width) and width > 0 ->
        merge_key = System.unique_integer([:positive])

        tokens =
          inline_wrap_tokens(
            run.text,
            run.style,
            false,
            inline_context_text(List.last(lines) || [])
          )

        {tokens, _} =
          Enum.map_reduce(tokens, 0, fn token, offset ->
            {NativeGlyphSourceValidator.slice_run(run, offset, token), offset + byte_size(token)}
          end)

        tokens
        |> Enum.reduce(lines, fn token_run, acc ->
          append_inline_token(
            acc,
            Map.put(token_run, :merge_key, merge_key),
            width
          )
        end)

      _ ->
        List.update_at(lines, length(lines) - 1, &(&1 ++ [run]))
    end
  end

  defp inline_wrap_tokens(text, style, intrinsic?, before \\ "") do
    case style do
      %{white_space: mode} when mode in [:nowrap, :pre] ->
        [text]

      %{line_break: :anywhere} ->
        String.graphemes(text)

      %{overflow_wrap: :anywhere} when intrinsic? ->
        String.graphemes(text)

      %{word_break: :break_word} when intrinsic? ->
        String.graphemes(text)

      _ ->
        case NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator.segments(
               text,
               before,
               Map.get(style, :white_space) == :break_spaces
             ) do
          {:ok, tokens} -> tokens
          {:error, _} = error -> throw({:render_resource_limit, error})
        end
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

        {text, _width, tab_counts} =
          run.text
          |> String.split("\t", trim: false)
          |> Enum.with_index()
          |> Enum.reduce({"", start_width, []}, fn {part, index}, {text, advance, counts} ->
            spaces =
              cond do
                index == 0 -> 0
                space_width <= 0 -> 0
                true -> 8 - Integer.mod(round(advance / space_width), 8)
              end

            prefix = String.duplicate(" ", spaces)

            {text <> prefix <> part, advance + spaces * space_width + text_width(part, run.style),
             if(index == 0, do: counts, else: counts ++ [spaces])}
          end)

        NativeGlyphSourceValidator.expand_tabs(run, text, tab_counts)
      else
        run
      end

    run =
      if nowrap? and not preserve? and
           Enum.all?(current_line, &(Map.get(&1, :type) == :inline_positioning_marker)),
         do: NativeGlyphSourceValidator.trim_run(run, trim_leading_inline_whitespace(run.text)),
         else: run

    token_text =
      if mode in [:pre, :break_spaces],
        do: run.text,
        else: trim_trailing_inline_text(run.text)

    token_width = if emergency?, do: text_width(token_text, run.style)

    # Adjacent tokens in one run share one rounded fragment. Styled runs remain separate.
    candidate_width =
      current_line
      |> append_inline_line_run(%{run | text: token_text})
      |> Enum.reduce(0.0, fn fragment, total ->
        total +
          NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator.snap_fragment_width(
            inline_run_width(fragment, width)
          )
      end)

    cond do
      not preserve? and HtmlValidator.trim_ascii_whitespace(run.text) == "" and
          Enum.all?(current_line, &(Map.get(&1, :type) == :inline_positioning_marker)) ->
        lines

      # An indivisible grapheme must overflow; splitting it again cannot make progress.
      current_line == [] and emergency? and token_width > width and
          length(String.graphemes(run.text)) > 1 ->
        append_break_word_token(lines, run, width)

      current_line != [] and
          not NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator.fits_width?(
            candidate_width,
            width
          ) ->
        break? =
          case List.last(current_line) do
            %{text: text, merge_key: key}
            when text != "" and key == run.merge_key and mode not in [:nowrap, :pre] ->
              true

            _ ->
              previous =
                current_line
                |> Enum.reverse()
                |> inline_break_run()

              inline_break_boundary?(previous, run, current_line)
          end

        if break? do
          lines = List.update_at(lines, -1, &select_discretionary_break/1)

          next_run =
            NativeGlyphSourceValidator.trim_run(
              run,
              if(preserve?, do: run.text, else: trim_leading_inline_whitespace(run.text))
            )

          if emergency? and token_width > width and not Map.get(run, :emergency_break, false) do
            append_break_word_token(lines ++ [[]], next_run, width)
          else
            lines ++ [[next_run]]
          end
        else
          {prefix, tail} = split_unbroken_inline_tail(current_line, run)

          word_width =
            if emergency?,
              do: inline_line_width(tail, width) - text_width(run.text, run.style) + token_width

          cond do
            emergency? and word_width > width and not Map.get(run, :emergency_break, false) ->
              lines =
                if prefix == [],
                  do: List.replace_at(lines, -1, []),
                  else: List.replace_at(lines, -1, select_discretionary_break(prefix)) ++ [[]]

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
              List.replace_at(lines, -1, select_discretionary_break(prefix)) ++ [tail]
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

          true ->
            # UAX #14 context crosses inline markup. Atomic boxes are independent boundaries.
            before = inline_context_text(line)

            case NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator.boundary?(
                   before,
                   run.text,
                   mode == :break_spaces or Map.get(previous_style, :white_space) == :break_spaces
                 ) do
              {:ok, allowed?} -> allowed?
              {:error, _} = error -> throw({:render_resource_limit, error})
            end
        end

      _ ->
        false
    end
  end

  # Empty positioning markers retain geometry but do not terminate text context.
  # The same subject selection applies to both sides of a styled boundary.
  defp inline_break_run(runs) do
    Enum.find(runs, fn run ->
      Map.get(run, :type) in [:inline_block, :inline_image, :soft_break] or
        (Map.get(run, :type) != :inline_positioning_marker and run.text != "")
    end)
  end

  # Both tokenization and styled-boundary breaking need the same uninterrupted text context.
  defp inline_context_text(line) do
    line
    |> Enum.reverse()
    |> Enum.take_while(&(Map.get(&1, :type) not in [:inline_block, :inline_image]))
    |> Enum.reverse()
    |> Enum.map_join(& &1.text)
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

        inline_break_boundary?(previous, inline_break_run(tail), Enum.take(line, index + 1)) ->
          {:halt, {Enum.take(line, index + 1), tail}}

        Map.get(previous, :type) in [:inline_block, :inline_image] ->
          if Map.get(run.style, :white_space) in [:nowrap, :pre],
            do: {:cont, {[], [previous | tail]}},
            else: {:halt, {Enum.take(line, index + 1), tail}}

        true ->
          before = inline_context_text(Enum.take(line, index))

          last_token =
            inline_wrap_tokens(previous.text, previous.style, false, before) |> List.last()

          split = byte_size(previous.text) - byte_size(last_token)

          if split > 0 do
            prefix =
              NativeGlyphSourceValidator.slice_run(
                previous,
                0,
                binary_part(previous.text, 0, split)
              )

            suffix = NativeGlyphSourceValidator.slice_run(previous, split, last_token)
            {:halt, {Enum.take(line, index) ++ [prefix], [suffix | tail]}}
          else
            {:cont, {[], [previous | tail]}}
          end
      end
    end)
  end

  # Geometry selects the taken soft opportunity; the validator owns its glyph projection.
  defp select_discretionary_break(line) do
    # Wrapping callers require a text or atomic run before selecting a break;
    # positioning markers alone never consume a line.
    {run, index} =
      line
      |> Enum.with_index()
      |> Enum.reverse()
      |> Enum.find(fn {run, _} ->
        Map.get(run, :type) in [:inline_block, :inline_image, :soft_break] or
          (Map.get(run, :type) != :inline_positioning_marker and run.text != "")
      end)

    if Map.get(run, :type) in [:inline_block, :inline_image, :soft_break],
      do: line,
      else: List.update_at(line, index, &Map.put(&1, :_css_break_after, true))
  end

  defp append_break_word_token(lines, run, width) do
    {fragments, _} =
      run.text
      |> String.graphemes()
      |> Enum.map_reduce(0, fn grapheme, offset ->
        {NativeGlyphSourceValidator.slice_run(run, offset, grapheme),
         offset + byte_size(grapheme)}
      end)

    fragments
    |> Enum.reduce(lines, fn fragment, acc ->
      append_inline_token(
        acc,
        Map.put(fragment, :emergency_break, true),
        width
      )
    end)
  end

  defp append_inline_line_run(line, run) do
    case List.last(line) do
      %{merge_key: merge_key} ->
        case merge_key == Map.get(run, :merge_key) do
          true ->
            List.update_at(
              line,
              length(line) - 1,
              &NativeGlyphSourceValidator.merge_runs(&1, run)
            )

          false ->
            line ++ [run]
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

  # Text fragments occupy fixed CSS layout units. Atomic runs already carry
  # their resolved border-box allocation, including genuine margins and padding.
  defp inline_fragment_advance(run, available_width) do
    width = inline_run_width(run, available_width)

    case Map.get(run, :type) do
      type when type in [nil, :text] ->
        NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator.snap_fragment_width(width)

      _ ->
        width
    end
  end

  # Max-content allocation must use the same styled-fragment advance as wrapping.
  # Otherwise an exact-fit atomic box is allocated less than its own line needs.
  defp intrinsic_inline_line_width(runs, available_width) do
    Enum.reduce(runs, 0.0, fn run, total ->
      total +
        NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator.snap_fragment_width(
          inline_run_width(run, available_width)
        )
    end)
  end

  @spec inline_runs([term()], map()) ::
          {:ok, [map()]} | {:error, :invalid_layout}
  defp inline_runs(children, parent_style) do
    result =
      Enum.reduce_while(children, {:ok, []}, fn child, {:ok, runs} ->
        case append_inline_run(child, runs, parent_style) do
          {:ok, runs} -> {:cont, {:ok, runs}}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)

    case result do
      {:ok, runs} ->
        adjusted =
          runs
          |> Enum.with_index()
          |> Enum.map(fn {run, index} ->
            case run do
              %{style: %{_ruby_box: measured} = style} ->
                previous = Enum.at(runs, index - 1)
                following = Enum.at(runs, index + 1)

                before =
                  if index > 0 and match?(%{text: text} when text != "", previous),
                    do: measured.overhang,
                    else: 0.0

                after_width =
                  if match?(%{text: text} when text != "", following),
                    do: measured.overhang,
                    else: 0.0

                before_delta = before - Map.get(run, :_ruby_overhang_before, 0.0)
                after_delta = after_width - Map.get(run, :_ruby_overhang_after, 0.0)

                measured = %{
                  measured
                  | boxes: shift_layout_boxes(measured.boxes, -before_delta, 0.0)
                }

                run
                |> Map.put(
                  :_inline_content_width,
                  run._inline_content_width - before_delta - after_delta
                )
                |> Map.put(:style, Map.put(style, :_ruby_box, measured))
                |> Map.put(:_ruby_overhang_before, before)
                |> Map.put(:_ruby_overhang_after, after_width)

              run ->
                run
            end
          end)

        {:ok, adjusted}

      error ->
        error
    end
  end

  defp append_inline_run(child, runs, parent_style) do
    case child do
      %{type: :element, tag: "ruby", style: %{display: display} = style} = ruby
      when display != :none ->
        ruby = %{ruby | style: Map.delete(style, :_ruby_clearance)}

        case NativeElixirPdfUtilities.HtmlToPdf.RubyLayout.measure(
               ruby,
               ResourceContext.current()
             ) do
          {:ok, groups} ->
            contexts =
              if Map.get(style, :position, :static) == :relative do
                [
                  %{
                    id: System.unique_integer([:positive]),
                    style: style,
                    positioned_children: Map.get(ruby, :positioned_children, [])
                  }
                ]
              else
                []
              end

            ruby_runs =
              Enum.map(groups, fn measured ->
                %{
                  type: :inline_block,
                  text: "",
                  style: style |> Map.put(:display, :inline) |> Map.put(:_ruby_box, measured),
                  children: [],
                  _inline_content_width: measured.width,
                  _inline_content_height: measured.ascent + measured.descent,
                  _inline_baseline_depth: measured.ascent,
                  _inline_outer_white_space: Map.get(parent_style, :white_space, :normal),
                  inline_positioning_contexts: contexts
                }
              end)

            {:ok, runs ++ ruby_runs}

          {:error, _} = error ->
            error
        end

      %{type: :element, style: %{_math_box: measured} = style} ->
        run = %{
          type: :inline_block,
          text: "",
          style: style,
          children: [],
          _inline_content_width: measured.width,
          _inline_content_height: measured.ascent + measured.descent,
          _inline_baseline_depth: measured.ascent,
          _inline_outer_white_space: Map.get(parent_style, :white_space, :normal)
        }

        {:ok, runs ++ [run]}

      %{type: :text, text: text, style: style} when is_binary(text) and is_map(style) ->
        text = normalize_inline_whitespace(text, style)

        {:ok,
         append_whitespace_collapsed_run(
           runs,
           NativeGlyphSourceValidator.prepare_run(child, text)
         )}

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
            split_context = List.first(Map.get(element, :split_inline_contexts, []))

            child_runs =
              if child_runs == [] and split_context != nil do
                [
                  %{
                    type: :inline_positioning_marker,
                    text: "",
                    style: text_style(style),
                    inline_positioning_contexts: []
                  }
                ]
              else
                child_runs
              end

            padding = Map.get(style, :padding, edges(0.0))
            border = Map.get(style, :border_widths, edges(0.0))
            margin = Map.get(style, :margin, edges(0.0))

            child_runs =
              Enum.reduce([{:left, :start}, {:right, :end}], child_runs, fn {side, edge}, runs ->
                edge_width =
                  Map.fetch!(padding, side) + Map.fetch!(border, side) + Map.fetch!(margin, side)

                if edge_width != 0 do
                  run = %{
                    type: :inline_positioning_marker,
                    text: "",
                    style: text_style(style),
                    _inline_padding_width: edge_width,
                    _inline_margin_side: side,
                    _inline_margin_width: Map.fetch!(margin, side),
                    inline_positioning_contexts: []
                  }

                  if edge == :start, do: [run | runs], else: runs ++ [run]
                else
                  runs
                end
              end)

            child_runs =
              if split_context != nil or
                   Map.get(style, :background_color) not in [nil, :transparent] or
                   Map.get(style, :background_image) != nil or
                   Enum.any?(border, fn {_side, width} -> width > 0 end) do
                font_size = Map.fetch!(style, :font_size)

                {ascent, descent} =
                  case text_font_face(style) do
                    %{ascent: ascent, descent: descent, units_per_em: units_per_em} ->
                      {round(font_size * ascent / units_per_em / 0.75) * 0.75,
                       round(-font_size * descent / units_per_em / 0.75) * 0.75}

                    _ ->
                      {font_size, 0.0}
                  end

                last = length(child_runs) - 1
                background_id = Map.get(element, :split_inline_background_id) || make_ref()
                source_style = Map.get(element, :split_inline_source_style, style)

                Enum.with_index(child_runs, fn run, index ->
                  fragment_border = %{
                    border
                    | left: if(index == 0, do: border.left, else: 0.0),
                      right: if(index == last, do: border.right, else: 0.0)
                  }

                  background = %{
                    id: background_id,
                    inline_context_id: if(split_context != nil, do: split_context.id),
                    source_style: source_style,
                    style: Map.put(style, :border_widths, fragment_border),
                    ascent: ascent + padding.top + border.top,
                    descent: descent + padding.bottom + border.bottom,
                    margin_side: Map.get(run, :_inline_margin_side),
                    margin_width: Map.get(run, :_inline_margin_width, 0.0)
                  }

                  Map.update(run, :inline_backgrounds, [background], &[background | &1])
                end)
              else
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

            child_runs =
              if Map.get(style, :_split_inline_fragment, false) and
                   Map.get(style, :vertical_align, :baseline) != :baseline do
                depth = text_baseline_depth(style)
                height = style.line_height

                alignment =
                  case Map.get(style, :vertical_align) do
                    side when side in [:top, :bottom] ->
                      {side, depth, height}

                    :middle ->
                      height / 2 - depth +
                        (HtmlValidator.text_x_height(parent_style) |> propagate_error()) / 2

                    shift when is_number(shift) ->
                      shift
                  end

                Enum.map(child_runs, &Map.put(&1, :_split_inline_alignment, alignment))
              else
                child_runs
              end

            child_runs =
              inherit_split_inline_contexts(
                child_runs,
                Map.get(element, :split_inline_contexts, [])
              )

            child_runs =
              if Map.get(element, :split_inline_text, false),
                do: Enum.map(child_runs, &Map.put(&1, :split_inline_text, true)),
                else: child_runs

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

        style =
          if ((Map.has_key?(style, :_color_state) and
                 not Map.get(style, :_html_content_skipped, false)) or
                Map.has_key?(style, :_file_display)) and
               Map.get(style, :vertical_align, :baseline) == :middle do
            case HtmlValidator.text_x_height(parent_style) do
              height when is_number(height) ->
                # Blink stores this platform font metric in whole CSS pixels.
                Map.put(
                  style,
                  if(Map.has_key?(style, :_file_display),
                    do: :_file_parent_x_height,
                    else: :_color_parent_x_height
                  ),
                  round(height / @css_pixel_points) * @css_pixel_points
                )

              {:error, _} = error ->
                throw({:render_resource_limit, error})
            end
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
              %{
                type: :inline_block,
                text: "",
                style: style,
                runs: child_runs,
                _inline_outer_white_space: Map.get(parent_style, :white_space, :normal)
              }

            _ ->
              %{
                type: :inline_block,
                text: "",
                style: style,
                children: children,
                _inline_outer_white_space: Map.get(parent_style, :white_space, :normal)
              }
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
            HtmlValidator.text_x_height(parent_style) |> propagate_error()
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
               _inline_outer_white_space: Map.get(parent_style, :white_space, :normal),
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
        Map.get(run, :_inline_padding_width, 0.0)

      _ ->
        text_width(run.text, run.style, Map.get(run, :_css_break_after, false))
    end
  end

  # Fixed browser layout precision applies before an atomic inline box is
  # measured, aligned to its line, and painted. Intrinsic sizes retain their
  # existing font measurements.
  defp inline_content_size(style, property, available_size, default) do
    size = resolved_content_size(style, property, available_size, default)

    if is_number(Map.get(style, property)) and is_number(size) do
      # The authored numeric dimension names the border box in this mode.
      # Rounding the already reduced content would shrink an exact outer size.
      box_edges =
        if Map.get(style, :box_sizing) == :border_box,
          do:
            if(property == :width,
              do: horizontal_box_size(style),
              else: vertical_box_size(style)
            ),
          else: 0.0

      max(
        Float.floor((size + box_edges) / @css_pixel_points * 64) / 64 * @css_pixel_points -
          box_edges,
        0.0
      )
    else
      size
    end
  end

  defp inline_block_content_width(run, available_width) do
    case run do
      %{_inline_content_width: width} ->
        width

      %{style: %{_color_state: _} = style} ->
        inline_content_size(
          style,
          :width,
          available_width,
          max(37.5 - horizontal_box_size(style), 0.0)
        )

      %{style: %{svg_scene: _} = style} ->
        {width, _height} = svg_content_size(style, available_width)
        width

      %{style: %{embedded_document: _surface} = style} ->
        inline_content_size(
          style,
          :width,
          available_width,
          elem(EmbeddedViewportValidator.intrinsic_size(), 0)
        )

      _ ->
        style = Map.fetch!(run, :style)

        default_width =
          case run do
            %{runs: runs} ->
              runs
              |> inline_lines(nil, false)
              |> Enum.map(&intrinsic_inline_line_width(&1, available_width))
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
                    axis = flex_main_axis(style)

                    intrinsic_children =
                      if axis == :column do
                        # Flex basis controls the vertical main axis in a
                        # column, so horizontal intrinsic sizing uses width.
                        Enum.map(children, fn child ->
                          %{child | style: Map.delete(child.style, :flex_basis)}
                        end)
                      else
                        children
                      end

                    {:ok, items} = flex_items(intrinsic_children, :row, nil, nil)

                    if axis == :row do
                      Enum.sum(Enum.map(items, & &1.outer_main)) +
                        flex_main_gap(style) * max(length(items) - 1, 0)
                    else
                      Enum.max(Enum.map(items, & &1.outer_main), fn -> 0.0 end)
                    end

                  _ ->
                    flex_block_intrinsic_width(children)
                end

              intrinsic_width
          end

        margin = Map.get(style, :margin, edges(0.0))

        default_width =
          case available_width do
            width when is_number(width) ->
              available_content_width =
                max(width - horizontal_box_size(style) - margin.left - margin.right, 0.0)

              min(default_width, available_content_width)

            _ ->
              default_width
          end

        inline_content_size(style, :width, available_width, default_width)
    end
  end

  # Measurements are attached to the atomic run and reused by its line and paint.
  # A baseline follows normal flow, independent of the declared content height.
  defp measure_inline_block(run, available_width) do
    # Atomic inline layout has no page fragmentation, even when a valid
    # prepared descendant omitted inherited style transport fields.
    HtmlTableValidator.with_layout_context(:surface, fn ->
      do_measure_inline_block(run, available_width)
    end)
  end

  defp do_measure_inline_block(run, available_width) do
    style = run.style
    content_width = inline_block_content_width(run, available_width)

    {intrinsic_height, first_baseline, baseline, lines} =
      case run do
        %{style: %{_color_state: _}} ->
          {max(10.5 - vertical_box_size(style), 0.0), nil, nil, nil}

        %{style: %{_file_display: display}} ->
          {display.intrinsic_height, display.baseline, display.baseline, nil}

        %{style: %{_temporal_display: display}} ->
          baseline =
            (display.intrinsic_height - display.editor_height) / 2 + 0.75 + display.baseline_depth

          {display.intrinsic_height, baseline, baseline, nil}

        %{style: %{_math_box: measured}} ->
          {measured.ascent + measured.descent, measured.ascent, measured.ascent, nil}

        %{style: %{embedded_document: _surface}} ->
          {elem(EmbeddedViewportValidator.intrinsic_size(), 1), nil, nil, nil}

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
      if Map.get(style, :form_kind) in [:text, :select] and baseline != nil,
        do:
          baseline +
            NativeElixirPdfUtilities.Validators.HtmlFormValidator.initial_control_content_offset(
              style,
              content_height
            ),
        else: baseline

    baseline =
      if Map.has_key?(style, :_temporal_display),
        do:
          (content_height - style._temporal_display.editor_height) / 2 + 0.75 +
            style._temporal_display.baseline_depth,
        else: baseline

    baseline =
      if Map.has_key?(style, :_file_display), do: style._file_display.baseline, else: baseline

    # The empty swatch contributes its margin-edge baseline. The fixed UA
    # wrapper's bottom padding remains below that baseline inside the host.
    baseline =
      if Map.has_key?(style, :_color_state) and not Map.get(style, :_html_content_skipped, false),
        do:
          content_height -
            NativeElixirPdfUtilities.HtmlToPdf.ColorArtwork.wrapper_padding().vertical,
        else: baseline

    parent_x_height =
      Map.get(style, :_file_parent_x_height, Map.get(style, :_color_parent_x_height))

    baseline =
      if parent_x_height != nil,
        do:
          (content_height + vertical_box_size(style) + parent_x_height) / 2 -
            border.top - padding.top,
        else: baseline

    first_baseline =
      if Map.has_key?(style, :_color_state) and not Map.get(style, :_html_content_skipped, false),
        do: baseline,
        else: first_baseline

    bottom_baseline =
      baseline == nil or
        (Map.get(style, :overflow, :visible) != :visible and
           Map.get(style, :form_kind) not in [:text, :select] and
           not Map.has_key?(style, :_number_state) and
           not Map.has_key?(style, :_temporal_display) and
           (not Map.has_key?(style, :_color_state) or
              Map.get(style, :_html_content_skipped, false)) and
           not Map.has_key?(style, :_file_display))

    first_baseline =
      if Map.has_key?(style, :_temporal_display) or Map.has_key?(style, :_file_display),
        do: baseline,
        else: first_baseline

    first_baseline =
      if baseline == nil,
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
    NativeGlyphSourceValidator.normalize_text(text, style)
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
          case previous_text != "" and :binary.last(previous_text) in [32, 9, 10, 12, 13] do
            true -> String.replace(text, ~r/^[ \t\f]+/u, "")
            false -> text
          end

        _ ->
          text
      end

    run = NativeGlyphSourceValidator.trim_run(run, text)

    case List.last(runs) do
      %{text: previous_text} = previous ->
        same_format? =
          Map.drop(previous, [:text, :source_glyph_characters]) ==
            Map.drop(run, [:text, :source_glyph_characters])

        join? =
          if same_format? and not Map.get(run.style, :_quote_shaping, false) do
            case NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator.transparent_format_boundary?(
                   previous_text,
                   text
                 ) do
              {:ok, transparent?} -> transparent?
              {:error, _} = error -> throw({:render_resource_limit, error})
            end
          else
            false
          end

        if same_format? and (Map.get(run.style, :_quote_shaping, false) or join?) do
          List.replace_at(runs, -1, NativeGlyphSourceValidator.merge_runs(previous, run))
        else
          runs ++ [run]
        end

      _ ->
        runs ++ [run]
    end
  end

  defp trim_leading_inline_whitespace(text) do
    case text do
      <<first, _rest::binary>> when first in [32, 9, 10, 12, 13] ->
        String.replace(text, ~r/^[ \t\n\f\r]+/u, "")

      _ ->
        text
    end
  end

  defp trim_trailing_inline_text(text) do
    # A single trailing separator needs no regex. Longer runs retain the regex,
    # including its ability to match before a final LF without removing that LF.
    cond do
      text == "" ->
        text

      :binary.last(text) in [32, 9, 12, 13] and
          (byte_size(text) == 1 or
             :binary.at(text, byte_size(text) - 2) not in [32, 9, 12, 13]) ->
        binary_part(text, 0, byte_size(text) - 1)

      :binary.last(text) in [32, 9, 10, 12, 13] ->
        String.replace(text, ~r/[ \t\f\r]+$/u, "")

      true ->
        text
    end
  end

  @spec text_box(%{text: String.t(), style: map()}, number(), number(), number()) :: box()
  defp text_box(run, x, y, width) do
    style = run.style
    font_face = text_font_face(style)
    plan = inline_display_plan(run.text, style, Map.get(run, :_css_break_after, false))

    box = %{
      type: HtmlValidator.reserve_layout_box(:text),
      text: plan.text,
      x: x,
      y: y,
      snap_to_css_pixel_grid: true,
      width: width,
      annotation_width: prepared_text_width(plan.text, style),
      font: Font.pdf_name(font_face),
      font_face: font_face,
      font_size: Map.fetch!(style, :font_size),
      letter_spacing: Map.get(style, :letter_spacing, 0.0),
      line_height: Map.fetch!(style, :line_height),
      color: Map.fetch!(style, :color)
    }

    native_glyph_runs =
      NativeGlyphSourceValidator.paint_runs(
        run,
        font_face,
        Map.get(style, :letter_spacing, 0.0),
        plan
      )

    box =
      if native_glyph_runs == [],
        do: box,
        else: Map.put(box, :native_glyph_runs, native_glyph_runs)

    box = if plan.discretionary?, do: Map.put(box, :_css_actual_text, plan.actual_text), else: box

    box =
      case style do
        %{_math_native_group: group, _math_original_characters: characters} ->
          box
          |> Map.put(:_math_group, group)
          |> Map.put(:_math_actual_source, Map.get(style, :_math_native_actual_source))
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

  @doc "Paints a validator-prepared CSS background in an already allocated content rectangle."
  @spec paint_background(map(), number(), number(), number(), number()) :: [map()]
  def paint_background(style, x, y, width, height) do
    background_box(style, x, y, width, height)
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
      if Map.has_key?(style, :embedded_document) or
           match?(%{svg_document_image: _}, Map.get(style, :image)),
         do:
           Enum.map(background, fn box ->
             if Map.get(box, :role) == :iframe_border,
               do: box,
               else: Map.put(box, :role, :embedded_host_background)
           end),
         else: background

    background =
      if Map.has_key?(style, :_temporal_display),
        do:
          background ++
            NativeElixirPdfUtilities.HtmlToPdf.TemporalArtwork.boxes(style, x, y, width, height),
        else: background

    background =
      if Map.has_key?(style, :_file_display),
        do:
          background ++
            NativeElixirPdfUtilities.HtmlToPdf.FileArtwork.boxes(style, x, y, width, height),
        else: background

    background =
      if Map.has_key?(style, :_color_state) and not Map.get(style, :_html_content_skipped, false),
        do:
          background ++
            NativeElixirPdfUtilities.HtmlToPdf.ColorArtwork.boxes(style, x, y, width, height),
        else: background

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
      if Map.get(style, :form_kind) == :select and Map.get(style, :appearance, :auto) != :none and
           width > 0 and
           height > 0 do
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

    background =
      background ++
        Enum.map(
          NativeElixirPdfUtilities.Validators.HtmlFormValidator.initial_resizer_strokes(
            style,
            width,
            height
          ),
          fn stroke ->
            size = stroke.size * @css_pixel_points

            %{
              type: HtmlValidator.reserve_layout_box(:rect),
              role: :textarea_resizer,
              x: x + width - style.border_widths.right - stroke.left * @css_pixel_points,
              y: y + style.border_widths.bottom + stroke.bottom * @css_pixel_points,
              width: size,
              height: size,
              fill_color: nil,
              stroke_color: stroke.color,
              stroke_width: @css_pixel_points,
              border_radius: 0.0,
              shape: {:diagonal, :up},
              clip: %{x: x, y: y, width: width, height: height},
              snap_to_css_pixel_grid: true,
              form_owner: Map.get(style, :_form_owner)
            }
          end
        )

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

    # Keep layout accounting even when no paint or form box is needed.
    type = HtmlValidator.reserve_layout_box(:rect)

    if width > 0 and height > 0 and
         (fill_color != nil or border_visible? or Map.get(style, :background_image) != nil or
            !!Map.get(style, :_form_control) or Map.has_key?(style, :_initial_option_id)) do
      rect = %{
        type: type,
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
        border_radius: Map.get(style, :border_radius, 0.0),
        form_control: Map.get(style, :_form_control),
        form_owner: Map.get(style, :_form_owner),
        initial_option_id: Map.get(style, :_initial_option_id)
      }

      rect =
        cond do
          Map.get(style, :_iframe_border, false) ->
            Map.put(rect, :role, :iframe_border)

          Map.get(style, :form_kind) == :listbox ->
            Map.put(rect, :role, :native_listbox)

          Map.has_key?(style, :_color_state) and Map.get(style, :_native_menu, false) and
              style.border_radius == 0 ->
            # Chromium NativeThemeBase::PaintButton uses an antialiased 1px
            # stroke around a half-border inset, unlike TextField's hairline.
            color =
              {118 / 255, 118 / 255, 118 / 255,
               if(Map.get(style, :_color_disabled, false), do: 0.3, else: 1.0)}

            # NativeThemeBase::PaintButton paints only a solid fill below
            # its fixed 5 CSS-pixel minimum for rounded button decoration.
            if width < 3.75 or height < 3.75 do
              %{
                rect
                | border_radius: 0.0,
                  stroke_width: 0.0,
                  border_widths: edges(0.0),
                  border_styles: edges(:none),
                  stroke_color: nil,
                  border_colors: edges(nil)
              }
            else
              %{
                rect
                | border_radius: 1.875,
                  fill_color: {239 / 255, 239 / 255, 239 / 255},
                  stroke_width: if(border_visible?, do: @css_pixel_points, else: 0.0),
                  stroke_color: color,
                  border_colors: edges(color)
              }
            end
            |> Map.put(:role, :native_color_button)

          Map.get(style, :_native_menu, false) and style.border_radius == 0 ->
            # NativeThemeBase paints a 2px radius on a half-border inset.
            # The native 1px non-AA stroke passes through ScopedRasterFlags
            # at the fixed 0.75 CSS-to-point scale: hairline, alpha *= 0.75.
            alpha =
              cond do
                Map.get(style, :form_kind) == :select and
                    get_in(style, [:_form_control, :initial_state, :disabled]) ->
                  77 / 255 * @css_pixel_points

                (Map.has_key?(style, :_number_state) and
                   get_in(style, [:_form_control, :initial_state, :disabled])) or
                  Map.get(style, :_temporal_disabled, false) or
                    Map.get(style, :_color_disabled, false) ->
                  0.3 * @css_pixel_points

                true ->
                  @css_pixel_points
              end

            color = {118 / 255, 118 / 255, 118 / 255, alpha}

            %{
              rect
              | border_radius: 1.5 + @css_pixel_points / 2,
                fill_color:
                  if(
                    Map.has_key?(style, :_temporal_display) or
                      Map.has_key?(style, :_color_state),
                    do: fill_color,
                    else:
                      if(fill_color,
                        do:
                          if(
                            Map.get(style, :form_kind) == :select and
                              get_in(style, [:_form_control, :initial_state, :disabled]),
                            do: {1.0, 1.0, 1.0, 153 / 255},
                            else: {1.0, 1.0, 1.0}
                          ),
                        else: nil
                      )
                  ),
                stroke_width: if(border_visible?, do: @css_pixel_points, else: 0.0),
                stroke_color: color,
                border_colors: edges(color)
            }
            |> Map.put(:role, :native_menu)

          Map.has_key?(style, :_temporal_display) and style.border_radius > 0 ->
            Map.put(rect, :role, :temporal_host)

          (Map.has_key?(style, :_image_input_state) or Map.has_key?(style, :_color_state)) and
              style.border_radius > 0 ->
            Map.put(rect, :role, :css_control_host)

          Map.get(style, :form_kind) == :select ->
            Map.put(rect, :role, :css_menu)

          true ->
            rect
        end

      rect =
        if Map.get(style, :form_kind) == :select and Map.get(style, :appearance, :auto) != :none and
             rect.border_radius > 0 and
             Enum.any?(Map.values(border_styles), &(&1 in [:dashed, :dotted])) do
          Map.put(rect, :native_menu_clip, %{
            x: x,
            y: y,
            width: width,
            height: height,
            border_radius: rect.border_radius
          })
        else
          rect
        end

      case Map.get(style, :background_image) do
        nil ->
          case {fill_color, border_visible?} do
            {nil, false} ->
              if Map.get(style, :_form_control),
                do: [rect],
                else: [%{rect | fill_color: {0.0, 0.0, 0.0, 0.0}}]

            _ ->
              if Map.get(rect, :role) == :native_menu and Map.get(style, :form_kind) == :select and
                   get_in(style, [:_form_control, :initial_state, :disabled]) do
                # NativeThemeBase first paints its disabled lighten layer, then
                # the translucent checkbox-background fill, within the same group.
                lighten =
                  rect
                  |> Map.put(:type, HtmlValidator.reserve_layout_box(:rect))
                  |> Map.put(:fill_color, {169 / 255, 169 / 255, 169 / 255, 51 / 255})
                  |> Map.put(:stroke_color, nil)
                  |> Map.put(:border_colors, edges(nil))
                  |> Map.delete(:form_control)

                [lighten, rect]
              else
                [rect]
              end
          end

        image ->
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
    else
      []
    end
  end

  # A validated zero extent preserves layout space but has no image paint area.
  # Share this decision between normal and flex/grid image placement.
  defp fitted_image_boxes(style, x, y, width, height, image, layout_context \\ nil) do
    if width > 0 and height > 0 do
      box =
        case {Map.get(style, :_responsive_image), image} do
          {request, _} when is_map(request) ->
            %{
              type: HtmlValidator.reserve_layout_box(:responsive_image),
              role: :responsive_image,
              responsive_style: style,
              x: x,
              y: y,
              width: width,
              height: height
            }

          {_, image} ->
            case Map.get(image, :svg_document_image) do
              identity when is_reference(identity) ->
                case NativeElixirPdfUtilities.HtmlToPdf.SvgDocumentImage.compose(
                       image,
                       width,
                       height,
                       layout_context
                     ) do
                  {:ok, :skipped} ->
                    nil

                  {:ok, raster} ->
                    fitted_image_box(
                      Map.put(style, :object_fit, :fill),
                      x,
                      y,
                      width,
                      height,
                      raster
                    )

                  {:error, _} = error ->
                    throw({:render_resource_limit, error})
                end

              _ ->
                fitted_image_box(style, x, y, width, height, image)
            end
        end

      if is_nil(box) do
        []
      else
        box =
          case {box.type, Map.get(style, :_image_map)} do
            {:image, model} when is_map(model) ->
              Map.put(box, :_image_map_geometry, %{
                model: model,
                dx: x - box.x,
                dy: y - box.y,
                width: width,
                height: height
              })

            _ ->
              box
          end

        # Replaced content follows the content-edge curve after subtracting
        # each actual border and padding edge from the host radius.
        if Map.has_key?(style, :_image_input_state) and style.border_radius > 0 do
          insets =
            Map.new(style.border_widths, fn {side, border} ->
              {side, border + Map.fetch!(style.padding, side)}
            end)

          [
            Map.put(
              box,
              :clip,
              PageGeometry.inset_rounded_clip(
                %{
                  x: x - insets.left,
                  y: y - insets.bottom,
                  width: width + insets.left + insets.right,
                  height: height + insets.top + insets.bottom,
                  border_radius: style.border_radius
                },
                insets
              )
            )
          ]
        else
          [box]
        end
      end
    else
      []
    end
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
          x: viewport_x + paint_position(horizontal, viewport_width - rendered_width, style),
          y:
            viewport_y + viewport_height - rendered_height -
              paint_position(vertical, viewport_height - rendered_height, style),
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
    initial_x = area_x + paint_position(horizontal, area_width - tile_width, style)

    initial_y =
      area_y + area_height - tile_height -
        paint_position(vertical, area_height - tile_height, style)

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

  defp paint_position(position, remaining, style) do
    case NativeElixirPdfUtilities.Validators.CssPositionValidator.resolve(
           position,
           remaining,
           style
         ) do
      {:ok, value} -> value
      {:error, _} = error -> throw({:render_resource_limit, error})
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
      box =
        if Map.has_key?(metadata, :table_id) and Map.has_key?(box, :fragment_id),
          do:
            Map.put_new(
              box,
              :table_cell_flow,
              Map.take(box, [:flow_id, :fragment_id, :orphans, :widows, :break_inside])
            ),
          else: box

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

  @compile {:inline, text_width: 3, prepared_text_width: 2, letter_spacing_width: 2}
  @spec text_width(String.t(), map()) :: number()
  defp text_width(text, style, break_after? \\ false) do
    # Width measurements need only visible glyph text, not a paint plan map.
    visible =
      if Map.has_key?(style, :_math_native_group) do
        HtmlValidator.reserve_render_resource(:max_layout_text_work, byte_size(text), :layout)
        text
      else
        case NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator.display_text(
               text,
               text_font_face(style),
               break_after?
             ) do
          {:ok, visible} -> visible
          {:error, _} = error -> throw({:render_resource_limit, error})
        end
      end

    prepared_text_width(visible, style)
  end

  defp inline_display_plan(text, style, break_after?) do
    if Map.has_key?(style, :_math_native_group) do
      HtmlValidator.reserve_render_resource(:max_layout_text_work, byte_size(text), :layout)
      %{text: text, actual_text: text, discretionary?: false}
    else
      case NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator.display_plan(
             text,
             text_font_face(style),
             break_after?
           ) do
        {:ok, plan} -> plan
        {:error, _} = error -> throw({:render_resource_limit, error})
      end
    end
  end

  # Both normal advances and intrinsic widths measure the same prepared glyph text.
  defp prepared_text_width(text, style) do
    text
    |> Font.css_text_width(
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

  defp propagate_error(result) do
    case result do
      {:error, _} = error -> throw({:render_resource_limit, error})
      result -> result
    end
  end

  @compile {:inline, text_font_face: 1, edges: 1, edges: 4}
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
