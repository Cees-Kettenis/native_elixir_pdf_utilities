defmodule NativeElixirPdfUtilities.Validators.NativeBackendValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics

  @doc "Normalizes loading failures for the independent native HTML utilities."
  @spec native_load_result(term()) :: :ok | {:error, {atom(), map()}}
  def native_load_result(result) do
    case result do
      {:module, NativeElixirPdfUtilities.HtmlToPdf.NativeBackend.NativeBytes} ->
        :ok

      result ->
        Diagnostics.error(
          :layout,
          :native_backend_unavailable,
          "HTML utility artifacts could not load: #{inspect(result)}; rebuild native/html_backend with the pinned Cargo toolchain",
          source: "HTML utilities",
          module: __MODULE__,
          operation: :load_native
        )
    end
  end
end
