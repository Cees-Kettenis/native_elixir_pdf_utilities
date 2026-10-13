defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgBackend do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.{PngDecoder, RenderCache, ResourceContext}

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    SvgBackendValidator,
    SvgDocumentViewportValidator,
    SvgValidator,
    SvgLinkValidator
  }

  @doc "Renders a complete validated SVG resource using supplied bytes only. Inline host composition remains gated."
  @spec rasterize(term(), term(), term(), HtmlValidator.image_budget() | nil) ::
          {:ok, binary()} | {:error, {atom(), map()}}
  def rasterize(source, options, context, image_budget \\ nil) do
    case rasterize_with_metadata(source, options, context, image_budget) do
      {:ok, png, _root} -> {:ok, png}
      error -> error
    end
  end

  @doc "Renders through the same validated adapter and returns its canonical root attributes for host composition."
  @spec rasterize_with_metadata(term(), term(), term(), HtmlValidator.image_budget() | nil) ::
          {:ok, binary(), map()} | {:error, {atom(), map()}}
  def rasterize_with_metadata(source, options, context, image_budget) do
    result =
      HtmlValidator.with_render_budget(fn ->
        with {:ok, request} <- SvgBackendValidator.prepare(source, options, context, image_budget),
             :ok <- SvgBackendValidator.native_load_result(load_native()),
             {:ok, png} <-
               NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.render(
                 request.source,
                 request.width,
                 request.height,
                 request.fonts,
                 request.resources,
                 request.mode == :inline
               )
               |> SvgValidator.conversion_result() do
          {:ok, png, request.root}
        end
      end)

    case result do
      {:error, {reason, diagnostic}} ->
        {:error,
         {reason,
          Diagnostics.with_context(diagnostic,
            source: "SVG",
            module: __MODULE__,
            operation: :rasterize
          )}}

      result ->
        result
    end
  end

  @doc "Prepares an exact-byte SVG document under the existing image policy without allocating its raster canvas."
  @spec prepare_document(term(), HtmlValidator.image_budget(), term()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def prepare_document(source, image_budget, origin \\ "SVG") do
    SvgBackendValidator.prepare_document(source, image_budget, origin)
  end

  @doc "Renders an issued SVG document at its actual child viewport, preserving its original source bytes."
  @spec paint_document(term(), term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def paint_document(descriptor, width, height) do
    with {:ok, request} <- SvgBackendValidator.document_request(descriptor) do
      context = descriptor.context

      ResourceContext.with_document(context, fn ->
        result =
          RenderCache.fetch(
            context.scope,
            {:svg_document_raster, context.document_id, descriptor.svg_document, width, height},
            fn ->
              with {:ok, raster} <-
                     SvgDocumentViewportValidator.prepare_raster(
                       request.document_viewport,
                       width,
                       height,
                       context.image_budget
                     ),
                   :ok <- SvgBackendValidator.native_load_result(load_native()),
                   {:ok, png} <-
                     NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.render_document(
                       request.source,
                       raster.width,
                       raster.height,
                       request.fonts,
                       request.resources,
                       raster.matrix
                     )
                     |> SvgValidator.conversion_result(),
                   {:ok, image} <- PngDecoder.decode(png, context.image_budget, false) do
                {:ok, image}
              end
            end
          )

        case result do
          {:error, {reason, diagnostic}} ->
            {:error,
             {reason,
              Diagnostics.with_context(Map.delete(diagnostic, :source),
                source: request.document_source,
                module: __MODULE__,
                operation: :rasterize_document
              )}}

          result ->
            result
        end
      end)
    end
  catch
    {:render_resource_limit, {:error, {reason, diagnostic}}} ->
      {:error,
       {reason,
        Diagnostics.with_context(diagnostic,
          operation: :rasterize_document,
          module: __MODULE__,
          source: Map.get(descriptor, :document_source, "SVG")
        )}}
  end

  @doc "Converts a validated inline SVG scene to a bounded vector PDF source using supplied bytes only."
  @spec vectorize(term(), term(), term(), HtmlValidator.image_budget() | nil) ::
          {:ok, binary()} | {:error, {atom(), map()}}
  def vectorize(source, options, context, image_budget \\ nil) do
    result =
      HtmlValidator.with_render_budget(fn ->
        with {:ok, request} <-
               SvgBackendValidator.prepare_vector(source, options, context, image_budget),
             :ok <- SvgBackendValidator.native_load_result(load_native()) do
          result =
            if request.annotations? or request.glyph_programs? do
              NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.vectorize_annotations(
                request.source,
                request.fonts,
                request.resources,
                [],
                request.effect_dimensions,
                SvgLinkValidator.native_ceilings()
              )
              |> SvgLinkValidator.native_result()
            else
              if map_size(request.resources) > 0 do
                NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.vectorize_resources(
                  request.source,
                  request.fonts,
                  request.resources,
                  [],
                  request.effect_dimensions
                )
              else
                if request.effects? do
                  NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.vectorize_effects(
                    request.source,
                    request.fonts,
                    [],
                    request.effect_dimensions
                  )
                else
                  NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.vectorize(
                    request.source,
                    request.fonts,
                    request.resources
                  )
                end
              end
            end

          case result do
            {:error, {_reason, _diagnostic}} -> result
            _ -> SvgValidator.conversion_result(result)
          end
        end
      end)

    case result do
      {:error, {reason, diagnostic}} ->
        {:error,
         {reason,
          Diagnostics.with_context(diagnostic,
            source: "SVG",
            module: __MODULE__,
            operation: :vectorize
          )}}

      result ->
        result
    end
  end

  @doc false
  @spec vectorize_slots(term(), term(), term(), map(), HtmlValidator.image_budget() | nil) ::
          {:ok, binary()} | {:error, {atom(), map()}}
  def vectorize_slots(source, options, context, slots, budget) do
    with {:ok, ids} <-
           NativeElixirPdfUtilities.Validators.SvgForeignObjectValidator.validate_slots(slots),
         {:ok, request} <- SvgBackendValidator.prepare_vector(source, options, context, budget),
         :ok <- SvgBackendValidator.native_load_result(load_native()) do
      result =
        if request.annotations? or request.glyph_programs? do
          NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.vectorize_annotations(
            request.source,
            request.fonts,
            request.resources,
            ids,
            request.effect_dimensions,
            SvgLinkValidator.native_ceilings()
          )
          |> SvgLinkValidator.native_result()
        else
          if map_size(request.resources) > 0 do
            NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.vectorize_resources(
              request.source,
              request.fonts,
              request.resources,
              ids,
              request.effect_dimensions
            )
          else
            if request.effects? do
              NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.vectorize_effects(
                request.source,
                request.fonts,
                ids,
                request.effect_dimensions
              )
            else
              NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.vectorize_slots(
                request.source,
                request.fonts,
                ids
              )
            end
          end
        end

      case result do
        {:error, {_reason, _diagnostic}} -> result
        _ -> SvgValidator.conversion_result(result)
      end
    end
  end

  @doc false
  @spec probe_slots(term(), term(), term(), map(), HtmlValidator.image_budget() | nil) ::
          {:ok, binary()} | {:error, {atom(), map()}}
  def probe_slots(source, options, context, slots, budget) do
    with {:ok, ids} <-
           NativeElixirPdfUtilities.Validators.SvgForeignObjectValidator.validate_slots(slots),
         {:ok, request} <- SvgBackendValidator.prepare_vector(source, options, context, budget),
         :ok <- SvgBackendValidator.native_load_result(load_native()) do
      NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.probe_html_slots(
        request.source,
        request.fonts,
        ids,
        request.resources
      )
      |> SvgValidator.conversion_result()
    end
  end

  @doc false
  @spec load_native() :: {:module, module()} | {:error, term()}
  def load_native do
    # The private binding is distributed with the native artifacts, outside the
    # application ebin. Code loading is serialized by the VM for concurrent calls.
    directory = :filename.join(:code.priv_dir(:native_elixir_pdf_utilities), ~c"native")
    :code.add_patha(directory)
    :code.ensure_loaded(NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes)
  end
end
