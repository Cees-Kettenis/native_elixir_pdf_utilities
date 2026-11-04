defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgDocumentImage do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    PngDecoder,
    RenderCache,
    ResourceContext,
    SvgRasterizer
  }

  alias NativeElixirPdfUtilities.Validators.{
    SvgConverterValidator,
    SvgDocumentImageValidator,
    SvgDocumentViewportValidator
  }

  @doc "Prepares an authorized selected SVG document for conversion at its actual child viewport."
  @spec prepare(term(), term(), term(), term(), term(), keyword()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def prepare(source, budget, origin, host, fonts, opts \\ []) do
    SvgDocumentImageValidator.prepare(source, budget, origin, host, fonts, opts)
  end

  @doc "Extracts the issued SVG at its used root and converts it to PNG with the established Resvg converter."
  @spec compose(term(), term(), term(), term()) ::
          {:ok, map() | :skipped} | {:error, {atom(), map()}}
  def compose(descriptor, width, height, layout_context \\ nil) do
    with {:ok, request} <- SvgDocumentImageValidator.request(descriptor) do
      context = descriptor.context

      result =
        ResourceContext.with_document(context, fn ->
          RenderCache.fetch(
            context.scope,
            {:svg_document_png, context.document_id, descriptor.svg_document_image, width, height,
             layout_context},
            fn ->
              with {:ok, raster} <-
                     SvgDocumentViewportValidator.prepare_raster(
                       request.plan,
                       width,
                       height,
                       context.image_budget,
                       %{host: request.host, layout: layout_context}
                     ) do
                case raster do
                  :skipped ->
                    {:ok, :skipped}

                  raster ->
                    with {:ok, source} <-
                           SvgConverterValidator.with_document_viewport(
                             request.source,
                             raster.root_size,
                             {width / 0.75, height / 0.75},
                             %{mode: :image, resources: request.resources, fonts: request.fonts}
                           ),
                         {:ok, png} <-
                           SvgRasterizer.rasterize(
                             source,
                             [width: raster.width, height: raster.height],
                             context.image_budget,
                             request.fonts,
                             %{mode: :image, resources: request.resources}
                           ) do
                      PngDecoder.decode(png, context.image_budget, false)
                    end
                end
              end
            end
          )
        end)

      case result do
        {:error, {reason, diagnostic}} ->
          Diagnostics.error(diagnostic.stage, reason, diagnostic.message,
            module: Map.get(diagnostic, :module),
            operation: Map.get(diagnostic, :operation),
            source:
              if(Map.get(diagnostic, :source) in [nil, "SVG"],
                do: request.origin,
                else: diagnostic.source
              ),
            line: Map.get(diagnostic, :line),
            column: Map.get(diagnostic, :column)
          )

        result ->
          result
      end
    end
  end
end
