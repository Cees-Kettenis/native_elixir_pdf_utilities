defmodule NativeElixirPdfUtilities.Application do
  @moduledoc false

  use Application

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.FontCache
  alias NativeElixirPdfUtilities.HtmlToPdf.SystemFontCache
  alias NativeElixirPdfUtilities.Validators.LimitsValidator

  @doc false
  @spec start(Application.start_type(), term()) ::
          {:ok, pid()} | {:ok, pid(), term()} | {:error, term()}
  @impl Application
  def start(_type, _arguments) do
    configured_limits = Application.get_env(:native_elixir_pdf_utilities, :limits, [])

    case LimitsValidator.validate(configured_limits) do
      {:ok, limits} ->
        :ok = Limits.install(limits)

        Supervisor.start_link(
          [
            {FontCache, maximum_entries: limits.max_font_cache_entries},
            {SystemFontCache, maximum_entries: limits.max_system_font_cache_entries}
          ],
          strategy: :one_for_one,
          name: NativeElixirPdfUtilities.Supervisor
        )

      {:error, message} ->
        {:error, {:invalid_limits_configuration, message}}
    end
  end
end
