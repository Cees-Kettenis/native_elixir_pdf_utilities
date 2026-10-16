defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.{RenderCache, ResourceContext}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, SvgConverterValidator, SvgValidator}

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
