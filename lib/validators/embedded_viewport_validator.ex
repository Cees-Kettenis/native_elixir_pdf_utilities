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
  alias NativeElixirPdfUtilities.Validators.MimeValidator
  alias NativeElixirPdfUtilities.Validators.ResourceValidator

  @doc "Classifies an authorized initial file document using shared MIME evidence before parsing or decoding."
  @spec prepare_initial_file(term(), term()) ::
          {:ok, :html | :text | :raster | :svg, String.t()}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_initial_file(bytes, path) do
    case {bytes, path} do
      {bytes, path} when is_binary(bytes) and is_binary(path) ->
        with {:ok, mime} <- MimeValidator.prepare(bytes, path, nil) do
          case mime do
            "text/html" ->
              {:ok, :html, mime}

            "text/plain" ->
              {:ok, :text, mime}

            "application/octet-stream" ->
              {:ok, :text, "text/plain"}

            mime when mime in ["image/png", "image/jpeg"] ->
              {:ok, :raster, mime}

            "image/svg+xml" ->
              {:ok, :svg, mime}

            _ ->
              Diagnostics.error(
                :resource,
                :not_implemented,
                "initial child file MIME #{mime} requires a document renderer outside the supported HTML, plain text and still-image profile",
                module: __MODULE__,
                operation: :load_embedded_resource,
                source: path
              )
          end
        end

      _ ->
        Diagnostics.error(
          :resource,
          :invalid_document,
          "initial file requires bytes and an authorized path",
          module: __MODULE__,
          operation: :load_embedded_resource
        )
    end
  end

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
             :html_recovery,
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

        {kind, _value} when kind in [:srcdoc, :blank] ->
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

  @doc "Validates the shared legacy frame/iframe margin and initial scrollbar policy."
  @spec prepare_initialization(term(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_initialization(attributes, host) do
    case {attributes, host} do
      {attributes, host} when is_map(attributes) and host in [:iframe, :frame] ->
        with {:ok, horizontal} <- legacy_margin(Map.get(attributes, "marginwidth"), host),
             {:ok, vertical} <- legacy_margin(Map.get(attributes, "marginheight"), host) do
          case Map.get(attributes, "scrolling", "auto") do
            scrolling when is_binary(scrolling) ->
              {:ok,
               %{
                 margins: %{horizontal: horizontal, vertical: vertical},
                 scrollbars:
                   if(String.downcase(scrolling, :ascii) in ["no", "off", "noscroll"],
                     do: :none,
                     else: :auto
                   )
               }}

            _ ->
              invalid_initialization(
                "#{host} scrolling requires a legacy string value",
                Atom.to_string(host)
              )
          end
        end

      _ ->
        invalid_initialization(
          "initial child hints require an attribute map and frame/iframe host",
          "embedded document"
        )
    end
  catch
    {:render_resource_limit, error} -> error
  end

  defp legacy_margin(value, host) do
    if is_nil(value) do
      {:ok, nil}
    else
      case HtmlValidator.parse_html_legacy_int32(value) do
        {:ok, number} when number < 0 ->
          {:ok, nil}

        {:ok, number} ->
          case HtmlValidator.parse_css_number(Integer.to_string(number), 0.75) do
            {points, ""} ->
              {:ok, points}

            _ ->
              invalid_initialization(
                "#{host} margin exceeds the supported numeric range",
                Atom.to_string(host)
              )
          end

        :error ->
          {:ok, 0.0}
      end
    end
  end

  @doc "Validates initial child body margin defaults before author CSS can override them."
  @spec prepare_body_margins(term()) ::
          {:ok, nil | map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_body_margins(value) do
    case value do
      nil ->
        {:ok, nil}

      %{horizontal: horizontal, vertical: vertical} = margins when map_size(margins) == 2 ->
        if Enum.all?([horizontal, vertical], fn number ->
             is_nil(number) or
               (match?({:ok, _}, HtmlValidator.normalize_geometry_number(number)) and number >= 0)
           end) do
          {:ok,
           %{
             top: vertical || 6.0,
             bottom: vertical || 6.0,
             left: horizontal || 6.0,
             right: horizontal || 6.0
           }}
        else
          invalid_initialization(
            "frame margins require bounded nonnegative points or absent hints",
            "embedded document"
          )
        end

      _ ->
        invalid_initialization(
          "frame margins require horizontal and vertical hints",
          "embedded document"
        )
    end
  end

  defp invalid_initialization(message, source) do
    Diagnostics.error(:layout, :invalid_document, message,
      module: __MODULE__,
      operation: :initialize_child_viewport,
      source: source
    )
  end

  @doc false
  @spec prepare_iframe_style(map()) :: map()
  def prepare_iframe_style(style) do
    display =
      if style.display in [:inline, :inline_block, :inline_flex, :inline_grid],
        do: :inline_block,
        else: :block

    Map.put(style, :display, display)
  end

  @doc "Returns the fixed 300 by 150 CSS pixel fallback for an HTML replaced document without intrinsic dimensions."
  @spec intrinsic_size() :: {float(), float()}
  def intrinsic_size do
    # This is a format fallback, not a configurable resource ceiling. Computed
    # CSS auto remains auto so flex stretch can allocate a different viewport.
    {225.0, 112.5}
  end

  @doc false
  @spec prepare_paint(map(), EmbeddedViewport.t(), number(), number()) ::
          {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_paint(layout, surface, width, height) do
    case prepare_composition(layout, surface, width, height) do
      {:ok, boxes, nil} ->
        {:ok, boxes}

      {:ok, _boxes, _widgets} ->
        Diagnostics.error(
          :layout,
          :unsupported_form,
          "paint-only child preparation cannot carry interactive fields; use prepare_composition for widgets or forms: :static for static artwork",
          module: __MODULE__,
          operation: :layout_embedded_viewport
        )

      failure ->
        failure
    end
  end

  @doc "Prepares native child paint and an opaque descriptor for real interactive fields after shared form validation."
  @spec prepare_composition(
          map(),
          EmbeddedViewport.t()
          | %{
              forms: :static | :interactive,
              host: :surface | :iframe | :object | :embed | :frame,
              context: ResourceContext.t()
            },
          number(),
          number()
        ) ::
          {:ok, [map()],
           NativeElixirPdfUtilities.Validators.EmbeddedFormValidator.descriptor() | nil}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_composition(layout, surface, _width, _height) do
    page = %{size: layout.page_size, boxes: layout.boxes}

    with {:ok, [page]} <-
           NativeElixirPdfUtilities.Validators.CssTransformValidator.materialize(
             [page],
             [forms: surface.forms],
             surface.context
           ),
         {:ok, [page], controls} <-
           NativeElixirPdfUtilities.Validators.HtmlFormValidator.prepare_scoped(
             [page],
             [forms: surface.forms],
             surface.context
           ),
         {:ok, widgets} <-
           if(controls == [],
             do: {:ok, nil},
             else:
               NativeElixirPdfUtilities.Validators.EmbeddedFormValidator.issue(
                 surface.context,
                 controls
               )
           ) do
      {:ok,
       Enum.map(page.boxes, fn box ->
         box = Map.delete(box, :form_control)
         # The child canvas has its own clipped print origin. Keep its fill
         # above vertical raster ties without moving text or link geometry.
         if surface.host == :frame and box.type == :rect and
              is_nil(Map.get(box, :role)) and
              Map.get(box, :paint_layer) == :container_background and box.x == 0 and box.y == 0 and
              {box.width, box.height} == layout.page_size,
            do: Map.put(box, :role, :frameset_canvas),
            else: box
       end), widgets}
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
      if surface.host in [:iframe, :object, :embed, :frame] do
        {Float.ceil(width / 0.75) * 0.75, Float.ceil(height / 0.75) * 0.75}
      else
        {width, height}
      end

    # Blink forces an automatic vertical scrollbar for its first layout with a
    # body. The existing reflow state machine removes it when the laid-out
    # document fits, including hidden overflow. Starting without it can select
    # the wrong stable wrapping state for a document near its height boundary.
    vertical =
      surface.host in [:iframe, :object, :embed, :frame] and width > 0 and height > 0 and
        not match?({:blank, _url}, Map.get(surface, :source)) and
        (surface.host not in [:iframe, :frame] or
           Map.get(surface, :frame_scrollbars, :auto) != :none)

    %{
      viewport_width: viewport_width,
      viewport_height: viewport_height,
      client_width: max(viewport_width - if(vertical, do: 11.25, else: 0), 0),
      client_height: viewport_height,
      horizontal: false,
      vertical: vertical,
      scroll_width: viewport_width,
      scroll_height: viewport_height,
      frozen_scrollbars: MapSet.new(),
      seen: MapSet.new([{false, vertical}])
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
          {:paint, map()} | {:relayout, map()}
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

    # An empty body contributes no scrollable content, including in a parsed
    # srcdoc. Its default margins alone must not create scrollbars in a tiny
    # viewport. Painted backgrounds and anchored content keep their extents.
    initial_empty? =
      Enum.all?(surface.tree.children, fn node ->
        node.tag == "body" and
          Enum.all?(node.children, fn child ->
            child.type == :text and HtmlValidator.trim_ascii_whitespace(child.text) == ""
          end)
      end) and
        Enum.all?(layout.boxes, fn box ->
          box.type == :flow_marker and Map.get(box, :role) == :embedded_extent and
            Map.get(box, :html_targets, []) == []
        end)

    scroll_width =
      if initial_empty?, do: previous.viewport_width, else: max(layout.content_width, right)

    scroll_height =
      if initial_empty?,
        do: previous.viewport_height,
        else: max(layout.content_height, elem(layout.page_size, 1) - bottom)

    enabled? =
      surface.host in [:iframe, :object, :embed, :frame] and width > 0 and height > 0 and
        overflow != :hidden and
        (surface.host not in [:iframe, :frame] or
           Map.get(surface, :frame_scrollbars, :auto) != :none)

    # Auto gutters are provisional. When both extents fit the full viewport,
    # remove them together before reflow, as Blink's TryRemovingAutoScrollbars
    # does. Scrollbars which subsequently appear stay frozen for this layout
    # lifecycle so width-dependent heights cannot alternate client areas.
    frozen = Map.get(previous, :frozen_scrollbars, MapSet.new())

    fits_viewport? =
      DrawingGeometry.snap_css_pixel(scroll_width) <= previous.viewport_width and
        DrawingGeometry.snap_css_pixel(scroll_height) <= previous.viewport_height

    # Compare rounded extents against the unclamped client dimension. A gutter
    # wider than a tiny host can still require the other scrollbar axis.
    available_width = previous.viewport_width - if(previous.vertical, do: 11.25, else: 0)
    available_height = previous.viewport_height - if(previous.horizontal, do: 11.25, else: 0)

    horizontal =
      enabled? and
        (MapSet.member?(frozen, :horizontal) or
           (not (fits_viewport? and MapSet.size(frozen) == 0) and
              DrawingGeometry.snap_css_pixel(scroll_width) > available_width))

    vertical =
      enabled? and
        (MapSet.member?(frozen, :vertical) or
           (not (fits_viewport? and MapSet.size(frozen) == 0) and
              DrawingGeometry.snap_css_pixel(scroll_height) > available_height))

    frozen =
      Enum.reduce([:horizontal, :vertical], frozen, fn axis, frozen ->
        next = if axis == :horizontal, do: horizontal, else: vertical

        if next and not Map.fetch!(previous, axis),
          do: MapSet.put(frozen, axis),
          else: frozen
      end)

    geometry = %{
      previous
      | horizontal: horizontal,
        vertical: vertical,
        client_width: max(previous.viewport_width - if(vertical, do: 11.25, else: 0), 0),
        client_height: max(previous.viewport_height - if(horizontal, do: 11.25, else: 0), 0),
        scroll_width: scroll_width,
        scroll_height: scroll_height
    }

    if geometry.client_width == previous.client_width and
         geometry.client_height == previous.client_height do
      {:paint, geometry}
    else
      {:relayout,
       geometry
       |> Map.put(:frozen_scrollbars, frozen)
       |> Map.put(:seen, MapSet.put(previous.seen, {horizontal, vertical}))}
    end
  end

  @doc false
  @spec prepare_scrollbar_parts(map()) ::
          {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_scrollbar_parts(geometry) do
    NativeElixirPdfUtilities.Validators.ScrollbarValidator.prepare(
      Map.drop(geometry, [:seen, :frozen_scrollbars])
    )
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
      when host in [:surface, :iframe, :object, :embed, :frame] ->
        with :ok <- ResourceValidator.validate_context(context),
             true <-
               host not in [:iframe, :frame] or
                 Map.get(surface, :frame_scrollbars, :auto) in [:auto, :none],
             true <-
               Map.get(surface, :initial_document_kind) in [nil, :html, :text, :raster, :svg],
             {:ok, {width, height}} <- HtmlValidator.prepare_viewport_dimensions(width, height) do
          # Zero-area surfaces still validate and account for their entire tree.
          size = {if(width == 0, do: 0.75, else: width), if(height == 0, do: 0.75, else: height)}
          opts = [page_size: "#{elem(size, 0)}pt #{elem(size, 1)}pt", margin: 0]

          {:ok, opts}
        else
          false ->
            Diagnostics.error(
              :layout,
              :invalid_layout,
              "child viewport requires a validated initial scrollbar policy and document kind",
              module: __MODULE__,
              operation: :layout_embedded_viewport
            )

          error ->
            error
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
