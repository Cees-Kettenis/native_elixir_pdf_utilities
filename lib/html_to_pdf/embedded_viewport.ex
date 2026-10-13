defmodule NativeElixirPdfUtilities.HtmlToPdf.EmbeddedViewport do
  @moduledoc "Native child document surfaces with isolated styles and shared render budgets."

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.AssetLoader
  alias NativeElixirPdfUtilities.HtmlToPdf.EmbeddedResource
  alias NativeElixirPdfUtilities.HtmlToPdf.FontFallback
  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
  alias NativeElixirPdfUtilities.HtmlToPdf.Layout
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgBackend
  alias NativeElixirPdfUtilities.HtmlToPdf.ResourceContext
  alias NativeElixirPdfUtilities.HtmlToPdf.ScrollbarPainter
  alias NativeElixirPdfUtilities.HtmlToPdf.Style
  alias NativeElixirPdfUtilities.Validators.EmbeddedViewportValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @type t :: %{
          optional(:prepared_dom) => NativeElixirPdfUtilities.HtmlToPdf.HtmlParser.dom_tree(),
          optional(:options) => keyword(),
          optional(:document_url) => String.t() | nil,
          optional(:source) =>
            NativeElixirPdfUtilities.Validators.ResourceValidator.document_descriptor(),
          optional(:client_css_viewport) => %{width: number(), height: number()},
          optional(:media_css_viewport) => %{width: number(), height: number()},
          optional(:frame_border) => boolean(),
          optional(:frame_scrollbars) => :auto | :none,
          optional(:frame_margins) => map(),
          optional(:frame_name) => String.t() | nil,
          tree: Style.styled_tree(),
          context: ResourceContext.t(),
          forms: :static | :interactive,
          host: :surface | :iframe | :object | :embed | :frame
        }

  @typedoc "Authorized child source awaiting its actual layout viewport before styling."
  @type prepared :: %{
          optional(:frame_border) => boolean(),
          optional(:frame_scrollbars) => :auto | :none,
          optional(:frame_margins) => map(),
          optional(:frame_name) => String.t() | nil,
          prepared_dom: NativeElixirPdfUtilities.HtmlToPdf.HtmlParser.dom_tree(),
          options: keyword(),
          document_url: String.t() | nil,
          source: NativeElixirPdfUtilities.Validators.ResourceValidator.document_descriptor(),
          context: ResourceContext.t(),
          forms: :static | :interactive,
          host: :iframe | :object | :embed | :frame
        }

  @doc "Prepares authorized local or srcdoc HTML without a recursive public render or pagination."
  @spec prepare_html(term(), ResourceContext.t(), keyword()) ::
          {:ok, t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_html(source, context, opts \\ []) do
    with {:ok, document} <- EmbeddedResource.load_html(source, context, opts) do
      style_document(document, context, opts)
    end
  end

  defp style_document(document, context, opts) do
    with {:ok, document, urls} <-
           EmbeddedViewportValidator.prepare_document_context(document, context) do
      ResourceContext.with_document(document.context, fn ->
        with {:ok, opts} <- EmbeddedViewportValidator.prepare_options(opts, document.context),
             {:ok, fonts} <- local_fonts(Keyword.fetch!(opts, :fonts), document.context, opts),
             opts <- Keyword.put(opts, :fonts, fonts),
             {:ok, tree} <-
               Style.compute_child_detailed(
                 document.dom,
                 opts,
                 context.image_budget,
                 urls.fallback_base
               ),
             {:ok, tree} <-
               FontFallback.resolve(tree, Keyword.get(opts, :unsupported_glyphs, :replace)) do
          {:ok,
           %{
             tree: tree,
             source: document.source,
             context: document.context,
             forms: Keyword.get(opts, :forms, :interactive),
             host: :surface
           }}
        end
      end)
    end
  end

  @doc """
  Reads and authorizes an iframe once, dispatching HTML, literal text and PNG/JPEG
  initial documents, deferring Style and selected resources
  until layout supplies its actual client viewport. The descriptor preserves
  the issued context, prepared DOM, options and document URL for viewport-aware
  styling. Existing prepare_html/3 surfaces still expose their styled tree.
  """
  @spec prepare_iframe_document(term(), ResourceContext.t(), keyword()) ::
          {:ok, prepared()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_iframe_document(source, context, opts \\ []) do
    with {:ok, resource} <- EmbeddedResource.load_initial(source, context, opts),
         {:ok, html} <-
           (case resource.kind do
              :text ->
                with {:ok, text} <- HtmlValidator.validate_html_source(resource.bytes),
                     do: {:ok, text_html(text, :document)}

              :raster ->
                {:file, path} = resource.source
                {:ok, image_html(path, :document)}

              _ ->
                {:ok, resource.bytes}
            end),
         {:ok, dom} <- HtmlParser.parse_document_detailed(html),
         document <- %{context: resource.context, source: resource.source, html: html, dom: dom},
         {:ok, document, urls} <-
           EmbeddedViewportValidator.prepare_document_context(document, context),
         opts <-
           if(resource.kind == :raster,
             do:
               Keyword.put(
                 opts,
                 :assets,
                 Map.put(
                   Keyword.get(opts, :assets, %{}),
                   elem(resource.source, 1),
                   {:bytes, resource.bytes}
                 )
               ),
             else: opts
           ),
         {:ok, opts} <- EmbeddedViewportValidator.prepare_options(opts, document.context) do
      {:ok,
       %{
         prepared_dom: document.dom,
         source: document.source,
         options: opts,
         document_url: urls.fallback_base,
         context: document.context,
         forms: Keyword.get(opts, :forms, :interactive),
         host: :iframe
       }}
    else
      {:error, {reason, diagnostic}} ->
        {:error,
         {reason,
          Diagnostics.with_context(diagnostic,
            operation: :load_embedded_resource,
            module: __MODULE__,
            source:
              NativeElixirPdfUtilities.Validators.ResourceValidator.document_source_label(source)
          )}}
    end
  end

  @doc "Prepares selected local object HTML for its isolated layout viewport."
  @spec prepare_object_document(term(), ResourceContext.t(), keyword()) ::
          {:ok, prepared()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_object_document(source, context, opts \\ []) do
    prepare_document(source, context, opts, :object)
  end

  @doc "Prepares selected local text/HTML viewports or still-image replacements without reauthorizing document mappings."
  @spec prepare_selected(
          NativeElixirPdfUtilities.Validators.ObjectValidator.selected(),
          ResourceContext.t(),
          keyword(),
          :object | :embed
        ) ::
          {:ok, {:viewport, prepared()} | {:image, map()}}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_selected(selected, context, opts, host) do
    with {:ok, kind, source, mime} <-
           NativeElixirPdfUtilities.Validators.ObjectValidator.prepare_selection(selected, host) do
      if kind == :blank do
        with {:ok, prepared} <-
               prepare_iframe_document({:blank, source || "about:blank"}, context, opts),
             do: {:ok, {:viewport, %{prepared | host: host}}}
      else
        prepare_file(source, mime, kind, context, opts, host)
      end
    end
  end

  defp prepare_file(source, mime, kind, context, opts, host) do
    with {:ok, resource} <- EmbeddedResource.load_file(source, mime, context, opts),
         {:ok, html} <-
           (case kind do
              :image ->
                {:file, path} = resource.source
                {:ok, image_html(path)}

              :document ->
                if mime == "text/plain" do
                  with {:ok, text} <- HtmlValidator.validate_html_source(resource.bytes),
                       do: {:ok, text_html(text, :document)}
                else
                  {:ok, resource.bytes}
                end
            end),
         {:ok, dom} <- HtmlParser.parse_document_detailed(html) do
      document = %{context: resource.context, source: resource.source, html: html, dom: dom}

      case kind do
        :document ->
          with {:ok, document, urls} <-
                 EmbeddedViewportValidator.prepare_document_context(document, context),
               {:ok, opts} <- EmbeddedViewportValidator.prepare_options(opts, document.context) do
            {:ok,
             {:viewport,
              %{
                prepared_dom: document.dom,
                source: document.source,
                options: opts,
                document_url: urls.fallback_base,
                context: document.context,
                forms: Keyword.get(opts, :forms, :interactive),
                host: host
              }}}
          end

        :image ->
          if mime == "image/svg+xml" do
            ResourceContext.with_document(resource.context, fn ->
              with {:ok, _opts} <-
                     EmbeddedViewportValidator.prepare_options(opts, resource.context),
                   :ok <-
                     HtmlValidator.reserve_image_source(
                       context.image_budget,
                       byte_size(resource.bytes)
                     ),
                   {:ok, image} <-
                     SvgBackend.prepare_document(resource.bytes, context.image_budget, source) do
                {:ok, {:image, %{image: image, svg_document_fitting: %{object_fit: :fill}}}}
              end
            end)
          else
            {:file, path} = resource.source
            # Only the exact bytes read from the authorized selected source enter
            # this generated reference. Caller mappings cannot replace the object.
            opts =
              opts
              |> Keyword.put(
                :assets,
                Map.put(Keyword.get(opts, :assets, %{}), path, {:bytes, resource.bytes})
              )
              |> Keyword.put(:stylesheets, [])

            with {:ok, surface} <- style_document(document, context, opts) do
              [body] = surface.tree.children
              [image] = body.children
              {:ok, {:image, image.style}}
            end
          end
      end
    end
  end

  defp prepare_document(source, context, opts, host) do
    with {:ok, document} <-
           if(host == :object,
             do: EmbeddedResource.load_object_html(source, context, opts),
             else: EmbeddedResource.load_html(source, context, opts)
           ),
         {:ok, document, urls} <-
           EmbeddedViewportValidator.prepare_document_context(document, context),
         {:ok, opts} <- EmbeddedViewportValidator.prepare_options(opts, document.context) do
      {:ok,
       %{
         prepared_dom: document.dom,
         source: document.source,
         options: opts,
         document_url: urls.fallback_base,
         context: document.context,
         forms: Keyword.get(opts, :forms, :interactive),
         host: host
       }}
    end
  end

  @doc "Styles a prepared document at client dimensions in points, with an optional separate media viewport including gutters."
  @spec style_viewport(map(), number(), number(), {number(), number()} | nil) ::
          {:ok, t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def style_viewport(prepared, width, height, media_viewport \\ nil) do
    with {:ok, opts} <- EmbeddedViewportValidator.prepare_styling(prepared, width, height),
         {:ok, _client, media_viewport} <-
           EmbeddedViewportValidator.prepare_media_viewport({width, height}, media_viewport) do
      ResourceContext.with_document(prepared.context, fn ->
        with {:ok, fonts} <-
               local_fonts(
                 Keyword.fetch!(opts, :fonts),
                 prepared.context,
                 opts
               ),
             opts <- Keyword.put(opts, :fonts, fonts),
             {:ok, tree} <-
               Style.compute_child_detailed(
                 prepared.prepared_dom,
                 opts,
                 prepared.context.image_budget,
                 prepared.document_url,
                 {width, height},
                 media_viewport,
                 Map.get(prepared, :frame_margins)
               ),
             {:ok, tree} <-
               FontFallback.resolve(tree, Keyword.get(opts, :unsupported_glyphs, :replace)) do
          {:ok,
           prepared
           |> Map.put(:tree, tree)
           |> Map.put(:client_css_viewport, %{width: width / 0.75, height: height / 0.75})
           |> Map.put(:media_css_viewport, %{
             width: elem(media_viewport, 0) / 0.75,
             height: elem(media_viewport, 1) / 0.75
           })}
        end
      end)
    end
  end

  @doc "Prepares iframe HTML with the element's initial viewport behavior."
  @spec prepare_iframe(term(), ResourceContext.t(), keyword()) ::
          {:ok, t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_iframe(source, context, opts \\ []) do
    with {:ok, surface} <- prepare_html(source, context, opts),
         do: {:ok, %{surface | host: :iframe}}
  end

  @doc "Prepares literal UTF-8 text as a bounded native document, preserving line breaks."
  @spec prepare_text(term(), ResourceContext.t(), keyword()) ::
          {:ok, t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_text(text, context, opts \\ []) do
    with {:ok, text} <- HtmlValidator.validate_html_source(text) do
      prepare_html({:srcdoc, text_html(text, :literal)}, context, opts)
    end
  end

  @doc "Prepares a supported local or inline image using the native image decoder and shared budget."
  @spec prepare_image(String.t(), ResourceContext.t(), keyword()) ::
          {:ok, t()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_image(reference, context, opts \\ []) do
    with {:ok, reference} <- HtmlValidator.validate_html_source(reference) do
      prepare_html({:srcdoc, image_html(reference)}, context, opts)
    end
  end

  @doc "Lays out child content in its client area, reflowing for automatic scrollbars, and returns one clipped drawing group."
  @spec layout(t() | prepared(), number(), number()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def layout(surface, width, height) do
    with {:ok, _opts} <- EmbeddedViewportValidator.prepare_layout(surface, width, height) do
      ResourceContext.with_document(surface.context, fn ->
        geometry = EmbeddedViewportValidator.initial_viewport(surface, width, height)

        with {:ok, surface} <-
               (case surface do
                  %{prepared_dom: _dom, client_css_viewport: client} ->
                    if client == %{
                         width: geometry.client_width / 0.75,
                         height: geometry.client_height / 0.75
                       } and
                         Map.get(surface, :media_css_viewport) == %{
                           width: geometry.viewport_width / 0.75,
                           height: geometry.viewport_height / 0.75
                         },
                       do: {:ok, surface},
                       else:
                         style_viewport(
                           surface,
                           geometry.client_width,
                           geometry.client_height,
                           {geometry.viewport_width, geometry.viewport_height}
                         )

                  %{tree: _tree} ->
                    {:ok, surface}

                  _ ->
                    style_viewport(
                      surface,
                      geometry.client_width,
                      geometry.client_height,
                      {geometry.viewport_width, geometry.viewport_height}
                    )
                end),
             tree <- EmbeddedViewportValidator.prepare_viewport_tree(surface),
             {:ok, layout, geometry} <- layout_client(tree, surface, width, height, geometry),
             {:ok, boxes} <-
               EmbeddedViewportValidator.prepare_paint(layout, surface, width, height) do
          origin = %{x: layout.margins.left, y: geometry.client_height - layout.margins.top}

          metadata =
            layout
            |> Map.take([:link_scope, :link_expected_targets, :link_document_targets])
            |> Map.update(:link_document_targets, [], fn targets ->
              Enum.map(targets, &Map.merge(&1, origin))
            end)

          metadata =
            if Map.has_key?(metadata, :link_scope),
              do: Map.put(metadata, :link_document_origin, origin),
              else: metadata

          client_rect = %{
            x: 0.0,
            y: 0.0,
            width: geometry.client_width,
            height: geometry.client_height
          }

          metadata =
            metadata
            |> Map.put(:client_rect, client_rect)
            |> Map.put(:scroll_offset, %{x: 0.0, y: 0.0})

          metadata =
            if surface.host == :iframe,
              do: Map.put(metadata, :role, :iframe_viewport),
              else: metadata

          if not geometry.horizontal and not geometry.vertical and
               geometry.client_height == height do
            {:ok, drawing_group(width, height, boxes, {1, 0, 0, 1, 0, 0}) |> Map.merge(metadata)}
          else
            content =
              drawing_group(
                geometry.client_width,
                geometry.client_height,
                boxes,
                {1, 0, 0, 1, 0, height - geometry.client_height}
              )
              |> Map.merge(metadata)

            with {:ok, parts} <- EmbeddedViewportValidator.prepare_scrollbar_parts(geometry) do
              ui = ScrollbarPainter.paint(parts)

              ui =
                drawing_group(
                  geometry.viewport_width,
                  geometry.viewport_height,
                  ui,
                  {1, 0, 0, 1, 0, height - geometry.viewport_height}
                )

              {:ok, drawing_group(width, height, [content, ui], {1, 0, 0, 1, 0, 0})}
            end
          end
        else
          {:error, {reason, diagnostic}} ->
            {:error,
             {reason,
              Diagnostics.with_context(
                if(reason == :invalid_layout,
                  do: Map.drop(diagnostic, [:operation, :module]),
                  else: diagnostic
                ),
                operation: :layout_embedded_viewport,
                module: __MODULE__
              )}}
        end
      end)
    end
  end

  defp layout_client(tree, surface, width, height, geometry) do
    with {:ok, layout} <-
           Layout.layout_viewport(tree, geometry.client_width, geometry.client_height) do
      case EmbeddedViewportValidator.prepare_scroll_geometry(
             layout,
             surface,
             width,
             height,
             geometry
           ) do
        {:paint, geometry} ->
          {:ok, layout, geometry}

        {:relayout, geometry} ->
          if Map.has_key?(surface, :prepared_dom) do
            with {:ok, restyled} <-
                   style_viewport(
                     surface,
                     geometry.client_width,
                     geometry.client_height,
                     {geometry.viewport_width, geometry.viewport_height}
                   ),
                 tree <- EmbeddedViewportValidator.prepare_viewport_tree(restyled),
                 do: layout_client(tree, restyled, width, height, geometry)
          else
            layout_client(tree, surface, width, height, geometry)
          end

        {:error, {_reason, _diagnostic}} = error ->
          error
      end
    end
  end

  defp drawing_group(width, height, boxes, transform) do
    %{
      type: HtmlValidator.reserve_layout_box(:drawing_group),
      width: width,
      height: height,
      transform: transform,
      clip: %{x: 0.0, y: 0.0, width: width, height: height},
      boxes: boxes
    }
  end

  defp local_fonts(fonts, context, opts) do
    Enum.reduce_while(fonts, {:ok, []}, fn font, {:ok, acc} ->
      case font do
        %{path: paths} ->
          result =
            Enum.reduce_while(paths, {:ok, []}, fn path, {:ok, bytes} ->
              case AssetLoader.resolve_local(path, :font, context, opts) do
                {:ok, data} -> {:cont, {:ok, bytes ++ [data]}}
                {:error, _} = error -> {:halt, error}
              end
            end)

          case result do
            {:ok, bytes} ->
              {:cont, {:ok, acc ++ [font |> Map.delete(:path) |> Map.put(:data, bytes)]}}

            {:error, _} = error ->
              {:halt, error}
          end

        %{data: _} ->
          {:cont, {:ok, acc ++ [font]}}
      end
    end)
  end

  defp text_html(text, mode) do
    style =
      if mode == :document,
        do:
          " style='font-family:monospace;font-size:9.75pt;word-wrap:break-word;white-space:pre-wrap'",
        else: ""

    # The generated opening newline is consumed by HTML's pre parser. The
    # source's own leading newline remains literal document content.
    "<pre" <> style <> ">\n" <> escape_html(text) <> "</pre>"
  end

  defp image_html(reference, mode \\ :replacement) do
    display = if mode == :document, do: " style='display:block'", else: ""

    "<body style='margin:0'><img" <>
      display <> " src=\"" <> escape_html(reference) <> "\"></body>"
  end

  defp escape_html(text) do
    text
    |> String.replace("&", "&amp;")
    |> String.replace("<", "&lt;")
    |> String.replace(">", "&gt;")
    |> String.replace("\"", "&quot;")
  end
end
