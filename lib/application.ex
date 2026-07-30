defmodule NativeElixirPdfUtilities.Application do
  @moduledoc false

  use Application

  alias NativeElixirPdfUtilities.HtmlToPdf.FontCache

  @doc false
  @spec start(Application.start_type(), term()) ::
          {:ok, pid()} | {:ok, pid(), term()} | {:error, term()}
  @impl Application
  def start(_type, _arguments) do
    Supervisor.start_link([FontCache],
      strategy: :one_for_one,
      name: NativeElixirPdfUtilities.Supervisor
    )
  end
end
