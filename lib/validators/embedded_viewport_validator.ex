defmodule NativeElixirPdfUtilities.Validators.EmbeddedViewportValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry
  alias NativeElixirPdfUtilities.HtmlToPdf.EmbeddedViewport
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf.ResourceContext
  alias NativeElixirPdfUtilities.Validators.HtmlUrlValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.ResourceValidator

  @doc false
  @spec prepare_options(term(), ResourceContext.t()) ::
          {:ok, keyword()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_options(opts, context) do
    with :ok <- ResourceValidator.validate_context(context),
         true <- is_list(opts) and Keyword.keyword?(opts),
         {:ok, normalized} <- Font.normalize_options(opts),
         :ok <- HtmlValidator.validate_render_request("", opts, {:ok, normalized}),
         opts <-
           Keyword.take(normalized, [
             :forms,
             :assets,
             :asset_resolver,
             :stylesheets,
             :image_selection_density,
             :default_font,
             :fonts,
             :unsupported_glyphs
           ]) do
      {:ok,
       opts
       |> Keyword.put(:base_url, context.resolution_base)
       |> Keyword.put(:system_font_discovery, false)}
    else
      {:error, {_reason, _diagnostic}} = error ->
        error

      _ ->
        Diagnostics.error(
          :resource,
          :invalid_options,
          "child viewport requires valid font and render options",
          module: __MODULE__
        )
    end
  end

  @doc false
  @spec prepare_document_context(map(), ResourceContext.t()) ::
          {:ok, map(), HtmlUrlValidator.context()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_document_context(document, parent) do
    fallback =
      case document.source do
        {:file, path} ->
          "file://" <> URI.encode(path, &(&1 == ?/ or URI.char_unreserved?(&1)))

        {:srcdoc, _html} ->
          base = parent.resolution_base

          case if(is_binary(base), do: URI.parse(base)) do
            %URI{scheme: scheme} when is_binary(scheme) ->
              base

            _ ->
              {:ok, context} =
                HtmlUrlValidator.document_context(%{type: :document, children: []},
                  base_url: base
                )

              context.fallback_base
          end
      end

    with {:ok, urls} <- HtmlUrlValidator.document_context_from_url(document.dom, fallback),
         {:ok, context} <-
           ResourceContext.rebase(document.context, {:document_url, urls.resolution_base}),
         do: {:ok, %{document | context: context}, urls}
  end

  @doc "Validates separate client and media viewport point dimensions. Media includes the scrollbar gutters."
  @spec prepare_media_viewport(term(), term()) ::
          {:ok, {number(), number()}, {number(), number()}}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_media_viewport(client, media) do
    media = if is_nil(media), do: client, else: media

    case {client, media} do
      {{cw, ch}, {mw, mh}} ->
        with {:ok, client} <- HtmlValidator.prepare_viewport_dimensions(cw, ch),
             {:ok, media} <- HtmlValidator.prepare_viewport_dimensions(mw, mh) do
          if mw >= cw and mh >= ch,
            do: {:ok, client, media},
            else:
              Diagnostics.error(
                :layout,
                :invalid_layout,
                "media viewport must contain the client dimensions including scrollbar gutters",
                module: __MODULE__
              )
        end

      _ ->
        Diagnostics.error(
          :layout,
          :invalid_layout,
          "client and media viewports require point dimension pairs",
          module: __MODULE__
        )
    end
  end

  @doc false
  @spec prepare_iframe_style(map()) :: map()
  def prepare_iframe_style(style) do
    # The HTML replaced-element default is 300 by 150 CSS pixels, a fixed
    # format default rather than a configurable resource ceiling.
    display =
      if style.display in [:inline, :inline_block, :inline_flex, :inline_grid],
        do: :inline_block,
        else: :block

    style
    |> Map.put(:display, display)
    |> Map.update(:width, 225.0, fn value -> if value in [nil, :auto], do: 225.0, else: value end)
    |> Map.update(:height, 112.5, fn value -> if value in [nil, :auto], do: 112.5, else: value end)
  end

  @doc false
  @spec prepare_paint(map(), EmbeddedViewport.t(), number(), number()) ::
          {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_paint(layout, surface, _width, _height) do
    page = %{size: layout.page_size, boxes: layout.boxes}

    case NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare([page],
           forms: surface.forms
         ) do
      {:ok, [page], []} ->
        {:ok, Enum.map(page.boxes, &Map.delete(&1, :form_control))}

      {:ok, _pages, _controls} ->
        Diagnostics.error(
          :layout,
          :unsupported_form,
          "interactive child controls require transformed widget geometry; use forms: :static for their complete static artwork",
          module: __MODULE__,
          operation: :layout_embedded_viewport
        )

      {:error, {_reason, _diagnostic}} = error ->
        error
    end
  end

  @doc false
  @spec initial_viewport(EmbeddedViewport.t() | EmbeddedViewport.prepared(), number(), number()) ::
          map()
  def initial_viewport(surface, width, height) do
    # Browsing-context layout viewports have whole CSS-pixel dimensions. The
    # physical host clip remains the authored point rectangle. Surface hosts
    # retain the caller's point dimensions and have no browsing-context UI.
    {viewport_width, viewport_height} =
      if surface.host == :iframe do
        {Float.ceil(width / 0.75) * 0.75, Float.ceil(height / 0.75) * 0.75}
      else
        {width, height}
      end

    %{
      viewport_width: viewport_width,
      viewport_height: viewport_height,
      client_width: viewport_width,
      client_height: viewport_height,
      horizontal: false,
      vertical: false,
      scroll_width: viewport_width,
      scroll_height: viewport_height,
      seen: MapSet.new([{false, false}])
    }
  end

  @doc false
  @spec prepare_viewport_tree(EmbeddedViewport.t()) :: map()
  def prepare_viewport_tree(surface) do
    %{
      surface.tree
      | children:
          Enum.map(surface.tree.children, fn node ->
            case node do
              %{tag: "body", style: style} ->
                # The propagated value belongs to the viewport; its source body has
                # used overflow:visible and keeps ordinary margin collapsing.
                %{node | style: Map.put(style, :overflow, :visible)}

              _ ->
                node
            end
          end)
    }
  end

  @doc false
  @spec prepare_scroll_geometry(map(), EmbeddedViewport.t(), number(), number(), map()) ::
          {:paint, map()} | {:relayout, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_scroll_geometry(layout, surface, width, height, previous) do
    overflow =
      Enum.find_value(surface.tree.children, :visible, fn node ->
        case node do
          %{tag: "body", style: style} ->
            case Map.get(style, :document_overflow, :visible) do
              :visible -> Map.get(style, :overflow, :visible)
              value -> value
            end

          _ ->
            nil
        end
      end)

    {right, bottom} =
      Enum.reduce(layout.boxes, {0.0, elem(layout.page_size, 1)}, fn box, {right, bottom} ->
        case box do
          %{type: :page_break} ->
            {right, bottom}

          %{position_anchor: :canvas} ->
            {right, bottom}

          _ ->
            geometry = if box.type == :image, do: Map.get(box, :clip, box), else: box

            {_top, box_bottom} =
              PageGeometry.box_vertical_bounds(Map.put(geometry, :type, box.type))

            {max(right, Map.get(geometry, :x, 0) + Map.get(geometry, :width, 0)),
             min(bottom, box_bottom)}
        end
      end)

    scroll_width = max(layout.content_width, right)
    scroll_height = max(layout.content_height, elem(layout.page_size, 1) - bottom)
    enabled? = surface.host == :iframe and width > 0 and height > 0 and overflow != :hidden
    # There are four scrollbar-presence states. Track states rather than inventing a pass
    # ceiling; percentage sizing may also remove an axis after reflow.
    # Blink compares rounded scrolling extents against the unclamped client
    # size. A gutter wider than the host can therefore require the other axis
    # even though the exposed client width has already clamped to zero.
    available_width = previous.viewport_width - if(previous.vertical, do: 11.25, else: 0)
    available_height = previous.viewport_height - if(previous.horizontal, do: 11.25, else: 0)
    horizontal = enabled? and DrawingGeometry.snap_css_pixel(scroll_width) > available_width
    vertical = enabled? and DrawingGeometry.snap_css_pixel(scroll_height) > available_height

    geometry = %{
      previous
      | horizontal: horizontal,
        vertical: vertical,
        client_width: max(previous.viewport_width - if(vertical, do: 11.25, else: 0), 0),
        client_height: max(previous.viewport_height - if(horizontal, do: 11.25, else: 0), 0),
        scroll_width: scroll_width,
        scroll_height: scroll_height
    }

    cond do
      geometry.client_width == previous.client_width and
          geometry.client_height == previous.client_height ->
        {:paint, geometry}

      MapSet.member?(previous.seen, {horizontal, vertical}) ->
        Diagnostics.error(
          :layout,
          :not_implemented,
          "iframe sizing cycles between scrollbar client areas; constrain percentage-dependent child sizes or author html/body overflow:hidden for a static clipped viewport",
          module: __MODULE__,
          operation: :layout_embedded_viewport,
          source: "iframe"
        )

      true ->
        {:relayout, %{geometry | seen: MapSet.put(previous.seen, {horizontal, vertical})}}
    end
  end

  @doc false
  @spec prepare_scrollbar_parts(map()) :: [map()]
  def prepare_scrollbar_parts(geometry) do
    # Fixed Chromium 152 Fluent classic/light profile, not resource ceilings:
    # 15px gutters, 18px buttons, 9px thumbs, 17px minimum thumb length.
    # See ui/native_theme/native_theme_fluent.{h,cc} and Blink's matching theme.
    axes =
      if(geometry.vertical, do: [:vertical], else: []) ++
        if(geometry.horizontal, do: [:horizontal], else: [])

    parts = Enum.flat_map(axes, &scrollbar_parts(&1, geometry))

    if geometry.vertical and geometry.horizontal do
      parts ++
        [
          %{
            kind: :track,
            x: geometry.client_width,
            y: geometry.viewport_height - geometry.client_height - 11.25,
            width: 11.25,
            height: 11.25,
            border_radius: 0.0
          }
        ]
    else
      parts
    end
  end

  defp scrollbar_parts(axis, geometry) do
    {x, y, width, height, span, total} =
      case axis do
        :vertical ->
          {geometry.viewport_width - 11.25, geometry.viewport_height - geometry.client_height,
           11.25, geometry.client_height, geometry.client_height, geometry.scroll_height}

        :horizontal ->
          {0.0, geometry.viewport_height - geometry.client_height - 11.25, geometry.client_width,
           11.25, geometry.client_width, geometry.scroll_width}
      end

    if span > 0 do
      span_pixels = round(span / 0.75)
      button = if(span_pixels < 36, do: div(span_pixels, 2), else: 18)
      track = max(span_pixels - 2 * button, 0)
      length = min(track, max(round(span_pixels / max(total / 0.75, 1) * track), 17))
      parts = [%{kind: :track, x: x, y: y, width: width, height: height, border_radius: 0.0}]

      parts =
        if button > 0 do
          parts ++
            Enum.map([:back, :forward], &scrollbar_arrow(axis, geometry, span_pixels, button, &1))
        else
          parts
        end

      if length > 0 and geometry.client_width > 0 and geometry.client_height > 0 do
        {thumb_x, thumb_y, thumb_width, thumb_height} =
          case axis do
            :vertical ->
              {x + 2.25, y + (span_pixels - button - length) * 0.75, 6.75, length * 0.75}

            :horizontal ->
              {x + button * 0.75, y + 2.25, length * 0.75, 6.75}
          end

        parts ++
          [
            %{
              kind: :thumb,
              x: thumb_x,
              y: thumb_y,
              width: thumb_width,
              height: thumb_height,
              border_radius: min(thumb_width, thumb_height) / 2
            }
          ]
      else
        parts
      end
    else
      []
    end
  end

  defp scrollbar_arrow(axis, geometry, span, button, end_part) do
    # NativeThemeFluent::GetArrowRect and NativeThemeBase::PathForArrow's
    # font-independent triangle fallback. Linux has no Segoe Fluent Icons.
    scale = max(15, button) / 18
    side = ceil(9 * scale)
    side = side + rem(min(15, button) - side, 2)
    offset = round(if(end_part == :back, do: -scale, else: scale))
    start = if(end_part == :back, do: 0, else: span - button)

    {left, top} =
      case axis do
        :vertical ->
          {geometry.viewport_width / 0.75 - 15 + Float.floor((15 - min(15, side)) / 2),
           start + Float.floor((button - min(button, side)) / 2) + offset}

        :horizontal ->
          {start + Float.floor((button - min(button, side)) / 2) + offset,
           geometry.viewport_height / 0.75 - 15 + Float.floor((15 - min(15, side)) / 2)}
      end

    # GetArrowRect clamps its centered square to the actual button rectangle
    # before PathForArrow derives the triangle from that clamped dimension.
    long_side = min(button, side)
    short_side = min(15, side)
    arrow = div(long_side, 2) + 1

    {left, top, width, height, direction} =
      case axis do
        :vertical ->
          bottom = top + long_side - div(arrow, 2) + 1
          top = if(end_part == :back, do: bottom - arrow, else: 2 * top + long_side - bottom)
          {left, top, short_side, arrow, if(end_part == :back, do: :up, else: :down)}

        :horizontal ->
          edge = left + div(arrow, 2)
          left = if(end_part == :forward, do: edge, else: 2 * left + long_side - edge - arrow)
          {left, top, arrow, short_side, if(end_part == :back, do: :left, else: :right)}
      end

    paint_clip =
      case axis do
        :vertical ->
          %{
            x: geometry.viewport_width - 11.25,
            y: geometry.viewport_height - (start + button) * 0.75,
            width: 11.25,
            height: button * 0.75
          }

        :horizontal ->
          %{x: start * 0.75, y: 0.0, width: button * 0.75, height: 11.25}
      end

    %{
      kind: :arrow,
      paint_clip: paint_clip,
      direction: direction,
      x: left * 0.75,
      y: geometry.viewport_height - (top + height) * 0.75,
      width: width * 0.75,
      height: height * 0.75,
      border_radius: 0.0
    }
  end

  @doc false
  @spec prepare_styling(term(), term(), term()) ::
          {:ok, keyword()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_styling(prepared, width, height) do
    case prepared do
      %{
        prepared_dom: %{type: :document, children: children},
        options: opts,
        document_url: url,
        context: context
      }
      when is_list(children) and (is_binary(url) or is_nil(url)) ->
        with {:ok, _} <- prepare_layout(prepared, width, height),
             do: prepare_options(opts, context)

      _ ->
        Diagnostics.error(
          :layout,
          :invalid_layout,
          "viewport styling requires a prepared DOM, options, document URL and issued context",
          operation: :style_viewport,
          module: __MODULE__
        )
    end
  end

  @doc false
  @spec prepare_layout(term(), term(), term()) ::
          {:ok, keyword()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_layout(surface, width, height) do
    case surface do
      %{context: context, forms: _forms, host: host}
      when host in [:surface, :iframe] ->
        with :ok <- ResourceValidator.validate_context(context),
             {:ok, {width, height}} <- HtmlValidator.prepare_viewport_dimensions(width, height) do
          # Zero-area surfaces still validate and account for their entire tree.
          size = {if(width == 0, do: 0.75, else: width), if(height == 0, do: 0.75, else: height)}
          opts = [page_size: "#{elem(size, 0)}pt #{elem(size, 1)}pt", margin: 0]

          {:ok, opts}
        end

      _ ->
        Diagnostics.error(
          :layout,
          :invalid_layout,
          "child viewport requires an issued document and nonnegative finite dimensions",
          module: __MODULE__,
          operation: :layout_embedded_viewport
        )
    end
  end
end
