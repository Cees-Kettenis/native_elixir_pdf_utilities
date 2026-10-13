defmodule NativeElixirPdfUtilities.Validators.SvgBackendValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, PngDecoder, RenderCache, ResourceContext}

  alias NativeElixirPdfUtilities.Validators.{
    FontValidator,
    HtmlValidator,
    SvgValidator,
    SvgClipValidator,
    SvgReferenceValidator,
    SvgPaintValidator,
    SvgEffectsValidator,
    SvgFilterValidator,
    SvgTextValidator,
    SvgLinkValidator,
    SvgDocumentViewportValidator,
    ResourceValidator
  }

  @doc false
  @spec prepare(term(), term(), term(), HtmlValidator.image_budget() | nil) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def prepare(source, options, context, image_budget) do
    prepare_request(source, options, context, image_budget, true)
  end

  defp prepare_request(source, options, context, image_budget, raster?) do
    image_budget = image_budget || HtmlValidator.new_image_budget()

    with {:ok, summary} <- SvgValidator.validate_processing(source, context),
         :ok <-
           NativeElixirPdfUtilities.Validators.SvgImageValidator.validate_descriptors(
             summary.context.resources
           ),
         context = summary.context,
         :ok <- validate_capabilities(summary, context),
         :ok <- validate_inline_geometry(summary),
         :ok <- SvgClipValidator.validate_summary(summary),
         :ok <- SvgPaintValidator.validate_summary(summary),
         :ok <- SvgEffectsValidator.validate_summary(summary),
         :ok <- SvgFilterValidator.validate_summary(summary),
         :ok <- SvgReferenceValidator.validate_summary(summary),
         :ok <- SvgTextValidator.validate_summary(summary),
         {:ok, links} <- SvgLinkValidator.prepare_summary(summary),
         {:ok, fonts} <- validate_fonts(context.fonts, summary),
         {:ok, resources} <- validate_resources(context, image_budget, summary),
         {:ok, prepared_source} <-
           validate_stroke_work(source, context.mode, fonts, summary, resources),
         {:ok, dimensions} <-
           if(raster? == :document,
             do: SvgDocumentViewportValidator.prepare(summary),
             else: SvgValidator.validate_dimensions(summary.root, options, image_budget)
           ),
         :ok <-
           if(
             raster? == true and map_size(resources) == 0 and
               Enum.any?(~w(pattern mask marker), &MapSet.member?(summary.elements, {:svg, &1})),
             do: validate_pattern_surfaces(source, dimensions, fonts),
             else: :ok
           ),
         :ok <-
           if(map_size(resources) == 0 and MapSet.member?(summary.elements, {:svg, "filter"}),
             do: validate_filter_surfaces(source, dimensions, fonts),
             else: :ok
           ),
         :ok <-
           if(map_size(resources) > 0,
             do: validate_resource_surfaces(prepared_source, dimensions, fonts, resources),
             else: :ok
           ),
         # Pinned resvg's actual max_bbox is five canvas widths/heights (25× area).
         # Image mode reserves three temporary surfaces per expanded node/text character.
         # A clip reference reserves its group pixmap, clip pixmap and alpha mask.
         # Opacity on the same group uses that group pixmap. PDF conversion
         # allocates no raster surface; its path work and PDF objects are budgeted separately.
         :ok <-
           HtmlValidator.reserve_render_resource(
             :max_aggregate_svg_surface_pixels,
             if(raster? == true, do: dimensions[:width] * dimensions[:height], else: 0) *
               if(context.mode == :inline,
                 do:
                   1 + 25 * summary.opacity_layers +
                     75 * (summary.nested_viewports + summary.clip_references),
                 else: 75 * summary.expanded_work
               ),
             :layout
           ) do
      {:ok,
       %{
         source: prepared_source,
         annotations?: links != [],
         root: summary.root,
         width: dimensions[:width],
         height: dimensions[:height],
         mode: context.mode,
         fonts: fonts,
         resources: resources,
         effects?: MapSet.member?(summary.elements, {:svg, "filter"}),
         effect_dimensions: {dimensions[:width], dimensions[:height]},
         document_viewport: if(raster? == :document, do: dimensions, else: nil)
       }}
    end
  catch
    {:render_resource_limit, {:error, {reason, diagnostic}}} ->
      {:error,
       {reason,
        Diagnostics.with_context(diagnostic,
          source: "SVG",
          module: __MODULE__,
          operation: :prepare
        )}}
  end

  @doc "Issues an image-policy SVG document request in its existing authorized child scope."
  @spec prepare_document(term(), HtmlValidator.image_budget(), term()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def prepare_document(source, image_budget, origin) do
    context = ResourceContext.current()
    origin = ResourceValidator.document_source_label(origin) || "SVG"

    with :ok <- ResourceValidator.validate_context(context),
         true <- image_budget == context.image_budget,
         {:ok, request} <- prepare_request(source, [], %{mode: :image}, image_budget, :document) do
      request = Map.put(request, :document_source, origin)
      identity = make_ref()

      RenderCache.fetch(
        context.scope,
        {:svg_document_request, context.document_id, identity},
        fn -> request end
      )

      {:ok,
       %{
         svg_document: identity,
         context: context,
         document_source: origin,
         width: request.width,
         height: request.height,
         width_px: max(round(request.width / 0.75), 1),
         height_px: max(round(request.height / 0.75), 1)
       }}
    else
      {:error, {reason, diagnostic}} ->
        {:error,
         {reason, Diagnostics.with_context(Map.delete(diagnostic, :source), source: origin)}}

      _ ->
        Diagnostics.error(
          :resource,
          :invalid_document,
          "selected SVG preparation requires the issued child document's shared image budget",
          module: __MODULE__,
          operation: :prepare_document,
          source: "SVG"
        )
    end
  end

  @doc "Resolves an issued SVG request without accepting replacement source, fonts or resource maps."
  @spec document_request(term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def document_request(descriptor) do
    with %{svg_document: identity, context: context} <- descriptor,
         true <- is_reference(identity),
         :ok <- ResourceValidator.validate_context(context),
         request when is_map(request) <-
           RenderCache.fetch(
             context.scope,
             {:svg_document_request, context.document_id, identity},
             fn -> nil end
           ),
         true <-
           descriptor[:width] == request.width and descriptor[:height] == request.height and
             descriptor[:width_px] == max(round(request.width / 0.75), 1) and
             descriptor[:height_px] == max(round(request.height / 0.75), 1) and
             descriptor[:document_source] == request.document_source do
      {:ok, request}
    else
      {:error, {_, _}} = error ->
        error

      _ ->
        Diagnostics.error(
          :resource,
          :invalid_document,
          "selected SVG request is not issued in this child document scope with its canonical intrinsic dimensions",
          module: __MODULE__,
          operation: :rasterize_document,
          source: "SVG"
        )
    end
  end

  @doc false
  @spec prepare_vector(term(), term(), term(), HtmlValidator.image_budget() | nil) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def prepare_vector(source, options, context, image_budget) do
    with {:ok, request} <- prepare_request(source, options, context, image_budget, false) do
      if request.mode == :inline,
        do: {:ok, request},
        else:
          unsupported(
            "SVG vector output requires an inline shape/text scene using authorized image resources",
            :vectorize
          )
    end
  end

  defp validate_resource_surfaces(source, dimensions, fonts, resources) do
    with :ok <- native_load_result(NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.load_native()) do
      case NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.validate_resource_surfaces(
             source,
             {dimensions[:width], dimensions[:height]},
             {Limits.get(:max_aggregate_svg_surface_pixels), Limits.get(:max_svg_filter_work)},
             fonts,
             resources
           ) do
        {:ok, {pixels, work}} ->
          HtmlValidator.reserve_render_resource(
            :max_aggregate_svg_surface_pixels,
            pixels,
            :layout
          )

          HtmlValidator.reserve_render_resource(:max_svg_filter_work, work, :layout)

        {:error, limit}
        when limit in ["max_aggregate_svg_surface_pixels", "max_svg_filter_work"] ->
          Diagnostics.error(
            :limits,
            :resource_limit_exceeded,
            "SVG resource scene exceeds " <> limit,
            source: "SVG",
            module: __MODULE__,
            operation: :prepare
          )

        {:error, message} ->
          Diagnostics.error(
            :layout,
            :invalid_document,
            "SVG resource scene preparation failed: " <> message,
            source: "SVG",
            module: __MODULE__,
            operation: :prepare
          )
      end
    end
  end

  defp validate_filter_surfaces(source, dimensions, fonts) do
    with :ok <- native_load_result(NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.load_native()) do
      case NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.validate_filter_work(
             source,
             dimensions[:width],
             dimensions[:height],
             {Limits.get(:max_aggregate_svg_surface_pixels), Limits.get(:max_svg_filter_work)},
             fonts
           ) do
        {:ok, {pixels, work}} ->
          HtmlValidator.reserve_render_resource(
            :max_aggregate_svg_surface_pixels,
            pixels,
            :layout
          )

          HtmlValidator.reserve_render_resource(:max_svg_filter_work, work, :layout)

        {:error, limit}
        when limit in ["max_aggregate_svg_surface_pixels", "max_svg_filter_work"] ->
          Diagnostics.error(
            :limits,
            :resource_limit_exceeded,
            "SVG filter evaluation exceeds " <> limit,
            source: "SVG filters",
            module: __MODULE__,
            operation: :prepare
          )

        {:error, message} ->
          Diagnostics.error(
            :layout,
            :invalid_document,
            "SVG filter preparation failed: " <> message,
            source: "SVG filters",
            module: __MODULE__,
            operation: :prepare
          )
      end
    end
  end

  defp validate_pattern_surfaces(source, dimensions, fonts) do
    with :ok <- native_load_result(NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.load_native()) do
      case NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.validate_pattern_work(
             source,
             dimensions[:width],
             dimensions[:height],
             Limits.get(:max_aggregate_svg_surface_pixels),
             fonts
           ) do
        {:ok, pixels} ->
          HtmlValidator.reserve_render_resource(
            :max_aggregate_svg_surface_pixels,
            pixels,
            :layout
          )

        {:error, "max_aggregate_svg_surface_pixels"} ->
          Diagnostics.error(
            :limits,
            :resource_limit_exceeded,
            "SVG pattern tiles and effect layers exceed max_aggregate_svg_surface_pixels",
            source: "SVG effects",
            module: __MODULE__,
            operation: :prepare
          )

        {:error, message} ->
          Diagnostics.error(
            :layout,
            :invalid_document,
            "SVG pattern preparation failed: " <> message,
            source: "SVG effects",
            module: __MODULE__,
            operation: :prepare
          )
      end
    end
  end

  defp validate_stroke_work(source, mode, fonts, summary, resources) do
    if mode == :inline do
      with :ok <- native_load_result(NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.load_native()),
           {:ok, links} <- SvgLinkValidator.prepare_summary(summary) do
        request =
          if links != [] or
               Enum.any?(summary.geometry, &Map.has_key?(&1.attributes, "textLength")) do
            {:prepare_text,
             {Limits.get(:max_svg_stroke_work), Limits.get(:max_svg_gradient_work),
              Limits.get(:max_svg_bytes), Limits.get(:max_svg_expansion_work),
              summary.expanded_work, Limits.get(:max_svg_nodes)}, fonts, resources, links}
          else
            {Limits.get(:max_svg_stroke_work), Limits.get(:max_svg_gradient_work), fonts,
             resources}
          end

        case NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.validate_shape_work(
               source,
               request
             ) do
          {:ok, {strokes, gradients, prepared_source, source_bytes, plan_work, extra_nodes}} ->
            HtmlValidator.reserve_render_resource(:max_svg_stroke_work, strokes, :layout)
            HtmlValidator.reserve_render_resource(:max_svg_gradient_work, gradients, :layout)

            HtmlValidator.reserve_render_resource(
              :max_aggregate_svg_source_bytes,
              source_bytes,
              :layout
            )

            HtmlValidator.reserve_render_resource(:max_svg_expansion_work, plan_work, :layout)
            HtmlValidator.reserve_render_resource(:max_aggregate_svg_nodes, extra_nodes, :layout)
            {:ok, prepared_source}

          {:ok, {strokes, gradients}} ->
            HtmlValidator.reserve_render_resource(:max_svg_stroke_work, strokes, :layout)
            HtmlValidator.reserve_render_resource(:max_svg_gradient_work, gradients, :layout)
            {:ok, source}

          {:error, limit}
          when limit in [
                 "max_svg_stroke_work",
                 "max_svg_gradient_work",
                 "max_svg_bytes",
                 "max_svg_expansion_work",
                 "max_svg_nodes"
               ] ->
            Diagnostics.error(
              :limits,
              :resource_limit_exceeded,
              "SVG shape/gradient preparation exceeds " <> limit,
              source: "SVG",
              module: __MODULE__,
              operation: :prepare
            )

          {:error, message} ->
            Diagnostics.error(
              :layout,
              :invalid_document,
              "SVG shape preparation failed: " <> message,
              source: "SVG",
              module: __MODULE__,
              operation: :prepare
            )
        end
      end
    else
      {:ok, source}
    end
  end

  @doc false
  @spec native_load_result(term()) :: :ok | {:error, {atom(), map()}}
  def native_load_result(result) do
    case result do
      {:module, NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes} ->
        :ok

      result ->
        Diagnostics.error(
          :layout,
          :svg_backend_unavailable,
          "SVG native artifacts could not load: #{inspect(result)}; rebuild native/svg_backend with the pinned Cargo toolchain",
          source: "SVG",
          module: __MODULE__,
          operation: :rasterize
        )
    end
  end

  @doc false
  @spec validate_capabilities(map(), map()) :: :ok | {:error, {atom(), map()}}
  def validate_capabilities(summary, context) do
    supported =
      ~w(svg g defs clipPath symbol use pattern marker mask path rect circle ellipse line polyline polygon linearGradient radialGradient stop text tspan textPath image title desc a) ++
        SvgFilterValidator.tags()

    cond do
      context.mode == :inline and
          Enum.any?(summary.elements, fn {namespace, tag} ->
            namespace != :svg or
                tag not in (~w(svg g defs symbol use pattern marker mask image linearGradient radialGradient stop clipPath path rect circle ellipse line polyline polygon text tspan textPath a title desc) ++
                              SvgFilterValidator.tags())
          end) ->
        unsupported(
          "inline SVG geometry cannot omit text, references, images, effects or native HTML; their complete scenes remain pending"
        )

      context.mode != :inline and MapSet.member?(summary.elements, {:svg, "clipPath"}) ->
        unsupported("SVG image/reference clipping remains outside this bounded adapter profile")

      MapSet.member?(summary.elements, {:svg, "foreignObject"}) ->
        unsupported(
          "usvg 0.47.0 has no foreignObject conversion branch; native HTML scene insertion is required"
        )

      Enum.any?(summary.elements, fn {namespace, tag} ->
        namespace == :html or (context.mode != :inline and tag in SvgFilterValidator.tags()) or
            (context.mode != :inline and tag in ~w(pattern marker mask))
      end) ->
        unsupported(
          "SVG effects and HTML require bounded intermediate scene composition before this adapter can render them"
        )

      Enum.any?(summary.elements, fn {_namespace, tag} -> tag not in supported end) ->
        unsupported(
          "SVG backend element is outside this bounded adapter profile; complete scene support remains pending"
        )

      summary.images >
          summary.hrefs
          |> Enum.filter(fn {_index, _href, _line, name} -> name == "image" end)
          |> Enum.map(&elem(&1, 0))
          |> Enum.uniq()
          |> length() ->
        unsupported("SVG image requires an explicit validated resource reference")

      Enum.any?(summary.hrefs, fn {_index, href, _line, name} ->
        (name == "image" and String.starts_with?(href, "#")) or
            (name not in ["image", "a"] and not String.starts_with?(href, "#"))
      end) ->
        unsupported(
          "SVG images require supplied PNG bytes; external SVG references require later scene support"
        )

      MapSet.member?(summary.elements, {:svg, "text"}) and context.fonts == [] ->
        unsupported(
          "SVG text requires selected validated font bytes; an empty backend font database would omit artwork"
        )

      true ->
        :ok
    end
  end

  defp validate_inline_geometry(summary) do
    Enum.reduce_while(summary.geometry, :ok, fn node, :ok ->
      case NativeElixirPdfUtilities.Validators.SvgSceneValidator.validate_node(node) do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  defp validate_fonts(fonts, summary) do
    FontValidator.with_budget(fn ->
      if length(fonts) > Limits.get(:max_font_count) do
        Diagnostics.error(:limits, :resource_limit_exceeded, "SVG fonts exceed max_font_count",
          source: "SVG"
        )
      else
        with {:ok, validated} <-
               Enum.reduce_while(fonts, {:ok, []}, fn font, {:ok, validated} ->
                 # Use the same parser, source fingerprints and limits as the host font registry.
                 result =
                   case font do
                     %{data: data} when is_binary(data) ->
                       FontValidator.reserve_source(data)

                       # Inline work validation and actual execution each create
                       # a byte-backed font database. Image mode executes once.
                       HtmlValidator.reserve_render_resource(
                         :max_svg_font_work_bytes,
                         byte_size(data) * if(summary.context.mode == :inline, do: 2, else: 1),
                         :font
                       )

                       case FontValidator.prepare_ttf(data) do
                         {:ok, parsed} ->
                           with :ok <-
                                  if(summary.context.mode == :inline,
                                    do:
                                      HtmlValidator.validate_font_embedding(
                                        Map.get(font, :family, "selected SVG font"),
                                        parsed.embedding_flags,
                                        parsed.variable_font?
                                      ),
                                    else: :ok
                                  ),
                                {:ok, _bounds} <-
                                  if(summary.context.mode == :inline,
                                    do:
                                      FontValidator.prepare_glyph_bounds(
                                        font
                                        |> Map.merge(parsed)
                                        |> Map.put(:type, :embedded)
                                        |> Map.put_new(:id, :crypto.hash(:sha256, data))
                                      ),
                                    else: {:ok, nil}
                                  ) do
                             {:ok, data, parsed}
                           end

                         {:error, _} = error ->
                           error

                         :error ->
                           Diagnostics.error(
                             :font,
                             :invalid_document,
                             "SVG font bytes must contain a supported TrueType font",
                             source: "SVG"
                           )
                       end

                     _ ->
                       Diagnostics.error(
                         :font,
                         :invalid_document,
                         "SVG fonts accept byte descriptors %{data: binary}, never paths",
                         source: "SVG"
                       )
                   end

                 case result do
                   {:ok, data, parsed} -> {:cont, {:ok, [{data, parsed} | validated]}}
                   error -> {:halt, error}
                 end
               end) do
          characters =
            summary.text
            |> Enum.join()
            |> String.graphemes()
            |> Enum.reject(&(&1 in ["\t", "\r", "\n"]))

          if Enum.all?(characters, fn character ->
               Enum.any?(validated, fn {_data, parsed} ->
                 Font.supports_text?(Map.put(parsed, :type, :embedded), character)
               end)
             end) do
            with :ok <- SvgTextValidator.validate_selected_text(summary, validated) do
              {:ok, validated |> Enum.reverse() |> Enum.map(&elem(&1, 0))}
            end
          else
            unsupported(
              "selected SVG font bytes do not cover all text; glyph fallback integration is required"
            )
          end
        end
      end
    end)
  end

  defp validate_resources(context, budget, summary) do
    selected =
      Enum.filter(context.resources, fn {name, _} ->
        Enum.any?(summary.hrefs, fn {_index, href, _line, tag} ->
          tag == "image" and href == name
        end)
      end)

    Enum.reduce_while(selected, {:ok, %{}}, fn {name, descriptor}, {:ok, validated} ->
      occurrences =
        Enum.reduce(summary.hrefs, 0, fn {index, href, _line, tag}, count ->
          if tag == "image" and href == name,
            do: count + Map.fetch!(summary.expansion_counts, index),
            else: count
        end)
        |> max(1)

      %{data: data, media_type: mime} = descriptor

      result =
        with :ok <-
               HtmlValidator.reserve_render_resource(
                 :max_aggregate_svg_resource_bytes,
                 byte_size(data),
                 :style
               ),
             :ok <- HtmlValidator.reserve_image_source(budget, byte_size(data)) do
          if mime == "image/svg+xml" do
            Enum.reduce_while(1..occurrences, {:ok, %{}}, fn _, {:ok, nested} ->
              case prepare_request(
                     data,
                     [],
                     %{
                       mode: :inline,
                       fonts: context.fonts,
                       resources: Map.delete(context.resources, name)
                     },
                     budget,
                     false
                   ) do
                {:ok, request} ->
                  case SvgLinkValidator.validate_nested_request(request, name) do
                    :ok ->
                      {:cont,
                       {:ok,
                        Map.merge(nested, request.resources) |> Map.put(name, request.source)}}

                    error ->
                      {:halt, error}
                  end

                error ->
                  {:halt, error}
              end
            end)
          else
            decode = fn ->
              if mime == "image/png" do
                PngDecoder.decode(data, budget, true)
              else
                case HtmlValidator.jpeg_metadata(data) do
                  {:ok, image} ->
                    {:ok, image}

                  :error ->
                    Diagnostics.error(
                      :style,
                      :invalid_document,
                      "SVG JPEG resource bytes are invalid",
                      source: name,
                      module: __MODULE__,
                      operation: :prepare
                    )
                end
              end
            end

            on_hit = fn
              {:ok, image} when mime == "image/png" ->
                HtmlValidator.reserve_decoded_image(
                  budget,
                  image.width_px,
                  image.height_px,
                  image.decoded_stride
                )

              _ ->
                :ok
            end

            decoded =
              case ResourceContext.current() do
                nil ->
                  decode.()

                context ->
                  RenderCache.fetch(
                    context.scope,
                    {:svg_resource_decode, mime, data},
                    decode,
                    on_hit
                  )
              end

            with {:ok, image} <- decoded,
                 :ok <-
                   HtmlValidator.reserve_render_resource(
                     :max_aggregate_svg_raster_pixels,
                     image.width_px * image.height_px * occurrences,
                     :style
                   ),
                 :ok <-
                   Enum.reduce_while(1..occurrences, :ok, fn _, :ok ->
                     case HtmlValidator.reserve_decoded_image(
                            budget,
                            image.width_px,
                            image.height_px,
                            4
                          ) do
                       :ok -> {:cont, :ok}
                       error -> {:halt, error}
                     end
                   end) do
              {:ok, %{name => data}}
            end
          end
        end

      case result do
        {:ok, bytes} -> {:cont, {:ok, Map.merge(validated, bytes)}}
        error -> {:halt, error}
      end
    end)
  end

  defp unsupported(message, operation \\ :rasterize) do
    Diagnostics.error(:layout, :unsupported_html, message,
      source: "SVG",
      operation: operation,
      module: __MODULE__
    )
  end
end
