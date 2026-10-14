defmodule NativeElixirPdfUtilities.Validators.SvgDocumentImageValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.{RenderCache, ResourceContext, SvgScene}

  alias NativeElixirPdfUtilities.Validators.{
    ResourceValidator,
    EmbeddedViewportValidator,
    SvgDocumentViewportValidator
  }

  @doc "Issues a selected SVG PNG document request in its authorized child scope."
  @spec prepare(term(), term(), term(), term(), term(), keyword()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def prepare(source, budget, origin, host, fonts, opts) do
    context = ResourceContext.current()
    origin = ResourceValidator.document_source_label(origin) || "SVG"
    caller_options = opts

    with :ok <- ResourceValidator.validate_context(context),
         true <- budget == context.image_budget,
         {:ok, opts} <- EmbeddedViewportValidator.prepare_options(opts, context),
         image_font_options = Keyword.take(caller_options, [:system_font_discovery]),
         {:ok, scene, fonts} <-
           SvgScene.extract(source, budget, Keyword.put(opts, :fonts, fonts), image_font_options),
         :ok <- NativeElixirPdfUtilities.Validators.SvgSceneValidator.validate_prepared(scene),
         :ok <-
           if(
             Enum.all?(scene.selected_fonts, fn selected ->
               Enum.any?(fonts, &(&1.data == selected.data))
             end),
             do: :ok,
             else:
               Diagnostics.error(
                 :font,
                 :invalid_font,
                 "selected SVG document text requires explicitly authorized independent font bytes; host and bundled fallback faces are not image resources",
                 module: __MODULE__,
                 operation: :prepare,
                 source: origin
               )
           ),
         {:ok, plan} <-
           SvgDocumentViewportValidator.prepare(
             %{
               source_root: scene.attributes,
               root_style:
                 Map.take(scene.style, [
                   :svg_width,
                   :svg_height,
                   :font_size,
                   :_root_font_size,
                   :overflow
                 ])
             },
             host
           ) do
      identity = make_ref()

      request = %{
        authored_source: source,
        source: scene.document_source,
        plan: plan,
        fonts: scene.selected_fonts,
        resources: scene.resources,
        origin: origin,
        host: host
      }

      RenderCache.fetch(context.scope, {:svg_document_image, context.document_id, identity}, fn ->
        request
      end)

      {:ok,
       %{
         svg_document_image: identity,
         context: context,
         document_source: origin,
         document_host: host,
         width: plan.width,
         height: plan.height,
         width_px: round(plan.width / 0.75),
         height_px: round(plan.height / 0.75)
       }}
    else
      {:error, {reason, diagnostic}} ->
        {:error, {reason, Diagnostics.with_context(diagnostic, source: origin)}}

      _ ->
        invalid(
          "selected SVG PNG preparation requires the issued child document's shared image budget"
        )
    end
  end

  @doc "Resolves a scoped PNG document request without accepting replacement source, fonts or geometry."
  @spec request(term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def request(descriptor) do
    with %{svg_document_image: identity, context: context} <- descriptor,
         true <- is_reference(identity),
         :ok <- ResourceValidator.validate_context(context),
         request when is_map(request) <-
           RenderCache.fetch(
             context.scope,
             {:svg_document_image, context.document_id, identity},
             fn -> nil end
           ),
         true <-
           descriptor[:width] == request.plan.width and descriptor[:height] == request.plan.height and
             descriptor[:width_px] == round(request.plan.width / 0.75) and
             descriptor[:height_px] == round(request.plan.height / 0.75) and
             descriptor[:document_source] == request.origin and
             descriptor[:document_host] == request.host do
      {:ok, request}
    else
      {:error, _} = error ->
        error

      _ ->
        invalid(
          "selected SVG PNG request must retain its issued child scope, original host and intrinsic dimensions"
        )
    end
  end

  defp invalid(message) do
    Diagnostics.error(:resource, :invalid_document, message,
      module: __MODULE__,
      operation: :rasterize_document,
      source: "SVG"
    )
  end
end
