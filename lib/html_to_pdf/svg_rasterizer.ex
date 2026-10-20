defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.{RenderCache, ResourceContext, PngDecoder}

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    SvgConverterValidator,
    SvgValidator,
    SvgImageValidator
  }

  @doc false
  @spec rasterize(term(), term(), HtmlValidator.image_budget() | nil) ::
          {:ok, binary()} | {:error, {atom(), map()}}
  @spec rasterize(term(), term(), HtmlValidator.image_budget() | nil, [map()]) ::
          {:ok, binary()} | {:error, {atom(), map()}}
  def rasterize(svg, options, budget, fonts \\ []) do
    rasterize(svg, options, budget, fonts, %{mode: :image})
  end

  @doc "Converts self-contained approved SVG through the existing resvg dependency."
  @spec rasterize(term(), term(), HtmlValidator.image_budget() | nil, term(), term()) ::
          {:ok, binary()} | {:error, {atom(), map()}}
  def rasterize(svg, options, budget, fonts, context) do
    rasterize(svg, options, budget, fonts, context, nil)
  end

  @doc "Converts an image using actual independent font options after canonical source preparation."
  @spec rasterize(term(), term(), HtmlValidator.image_budget() | nil, term(), term(), term()) ::
          {:ok, binary()} | {:error, {atom(), map()}}
  def rasterize(svg, options, budget, fonts, context, image_font_options) do
    with {:ok, request} <-
           SvgConverterValidator.prepare(svg, options, budget, fonts, context, image_font_options) do
      convert_prepared(request)
    end
  end

  @doc "Converts an image while retaining its authorized source for CSS viewport changes."
  @spec rasterize_image(term(), term(), term(), term(), term()) ::
          {:ok, binary(), map() | nil} | {:error, {atom(), map()}}
  def rasterize_image(svg, options, budget, fonts, font_options) do
    with {:ok, request, descriptor} <-
           SvgImageValidator.prepare_viewport_source(svg, options, budget, fonts, font_options),
         {:ok, png} <- convert_prepared(request) do
      {:ok, png, descriptor}
    end
  end

  @doc "Renders an issued SVG image at its final CSS dimensions, sharing decoded viewport results."
  @spec resize_image(map(), number(), number()) :: {:ok, map()} | {:error, {atom(), map()}}
  def resize_image(image, width, height) do
    case Map.get(image, :svg_viewport) do
      nil ->
        {:ok, image}

      descriptor ->
        with {:ok, request, context} <-
               SvgImageValidator.viewport_request(descriptor, width, height) do
          ResourceContext.with_document(context, fn ->
            scale = NativeElixirPdfUtilities.Limits.get(:svg_raster_scale)
            options = [width: ceil(width / 0.75), height: ceil(height / 0.75), scale: scale]

            # Without a viewBox, CSS stretches the converted intrinsic canvas as
            # an ordinary image. Only a viewBox establishes a new SVG fitting
            # operation at the used viewport, including root transform units.
            if not request.view_box? do
              {:ok,
               image |> Map.delete(:svg_viewport) |> Map.merge(%{width: width, height: height})}
            else
              RenderCache.fetch(
                context.scope,
                {:svg_image_viewport, descriptor.id, width, height},
                fn ->
                  with {:ok, source} <-
                         SvgConverterValidator.with_root_viewport(
                           request.source,
                           {width / 0.75, height / 0.75}
                         ),
                       {:ok, png} <-
                         rasterize(
                           source,
                           options,
                           request.budget,
                           request.fonts,
                           %{mode: :image}
                         ),
                       {:ok, resized} <- PngDecoder.decode(png, request.budget, false) do
                    {:ok, %{resized | width: width, height: height}}
                  end
                end,
                fn
                  {:ok, resized} ->
                    SvgValidator.validate_raster_dimensions(
                      resized.width_px,
                      resized.height_px,
                      request.budget
                    )

                  _ ->
                    :ok
                end
              )
            end
          end)
        end
    end
  end

  defp convert_prepared(request) do
    case ResourceContext.current() do
      nil ->
        convert(request)

      scope ->
        RenderCache.fetch(
          scope.scope,
          {:svg_converter, scope.document_id, scope.tree_scope_id, request},
          fn -> convert(request) end
        )
    end
  end

  # The converter accepts font files, so snapshot only bytes already authorized
  # by the canonical font validator. No caller path is read here.
  defp convert(request) do
    directory =
      Path.join(
        System.tmp_dir!(),
        "nepu-svg-#{System.pid()}-" <> Base.encode16(:crypto.strong_rand_bytes(16), case: :lower)
      )

    case File.mkdir(directory) do
      :ok ->
        try do
          with :ok <- File.chmod(directory, 0o700),
               {:ok, files} <-
                 Enum.reduce_while(request.fonts, {:ok, []}, fn bytes, {:ok, files} ->
                   path = Path.join(directory, Integer.to_string(length(files)) <> ".ttf")

                   case File.write(path, bytes, [:binary, :exclusive]) do
                     :ok -> {:cont, {:ok, [path | files]}}
                     error -> {:halt, error}
                   end
                 end),
               {:ok, fontdb} <-
                 Resvg.init_fontdb(
                   request.options ++
                     [font_files: Enum.reverse(files), skip_system_fonts: true]
                 ),
               result <-
                 Resvg.svg_string_to_png_binary(
                   request.source,
                   request.options ++
                     [resources_dir: directory, skip_system_fonts: true, fontdb: fontdb]
                 ) do
            SvgValidator.conversion_result(result)
          else
            {:error, reason} ->
              SvgConverterValidator.staging_error(reason)
          end
        after
          File.rm_rf(directory)
        end

      {:error, reason} ->
        SvgConverterValidator.staging_error(reason)
    end
  end
end
