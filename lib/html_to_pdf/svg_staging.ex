defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgStaging do
  @moduledoc false
  use GenServer

  @doc "Keeps a private directory for one synchronous converter call and removes it when its worker exits."
  @spec with_directory((Path.t() -> result)) :: result | {:error, File.posix()}
        when result: term()
  def with_directory(operation) do
    directory =
      Path.join(
        System.tmp_dir!(),
        "nepu-svg-#{System.pid()}-" <> Base.encode16(:crypto.strong_rand_bytes(16), case: :lower)
      )

    with {:ok, guard} <- GenServer.start(__MODULE__, {self(), directory}) do
      try do
        with :ok <- File.chmod(directory, 0o700), do: operation.(directory)
      after
        GenServer.stop(guard, :normal, :infinity)
      end
    end
  end

  @impl true
  def init({worker, directory}) do
    Process.monitor(worker)

    case File.mkdir(directory) do
      :ok -> {:ok, directory}
      {:error, reason} -> {:stop, reason}
    end
  end

  @impl true
  def handle_info({:DOWN, _, :process, _, _}, directory), do: {:stop, :normal, directory}

  @impl true
  def terminate(_reason, directory) do
    File.rm_rf(directory)
    :ok
  end
end
