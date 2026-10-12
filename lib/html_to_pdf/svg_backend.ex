defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgBackend do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, SvgBackendValidator, SvgValidator}

  @doc "Renders a complete validated SVG resource using supplied bytes only. Inline host composition remains gated."
  @spec rasterize(term(), term(), term(), HtmlValidator.image_budget() | nil) ::
          {:ok, binary()} | {:error, {atom(), map()}}
  def rasterize(source, options, context, image_budget \\ nil) do
    result =
      HtmlValidator.with_render_budget(fn ->
        with {:ok, request} <- SvgBackendValidator.prepare(source, options, context, image_budget),
             :ok <- SvgBackendValidator.native_load_result(load_native()) do
          NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.render(
            request.source,
            request.width,
            request.height,
            request.fonts,
            request.resources
          )
          |> SvgValidator.conversion_result()
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
