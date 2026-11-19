defmodule NativeElixirPdfUtilities.AtomicFile do
  @moduledoc false
  use GenServer

  @doc """
  Stages one file for a bounded operation. Only its caller may authorize the
  rename, after the operation has returned successfully. The staging process
  owns cleanup even if the worker or its caller exits. The operation requests
  a private output path once and returns :ok after closing the completed file.
  """
  @spec replace(term(), ((-> {:ok, Path.t()} | {:error, File.posix()}) -> result)) ::
          result | :ok | {:error, File.posix()}
        when result: term()
  def replace(path, operation) do
    {:ok, writer} = GenServer.start(__MODULE__, {self(), path})

    try do
      with :ok <- operation.(fn -> GenServer.call(writer, :prepare, :infinity) end) do
        GenServer.call(writer, :commit, :infinity)
      end
    after
      GenServer.cast(writer, :discard)
    end
  end

  @impl true
  def init({owner, path}) do
    Process.monitor(owner)
    {:ok, %{destination: path, directory: nil, mode: nil}}
  end

  @impl true
  def handle_call(request, _from, state) do
    case request do
      :prepare ->
        with {:ok, cwd} <- File.cwd(),
             {:ok, target, mode} <- destination(state.destination, cwd, 0),
             suffix = :crypto.strong_rand_bytes(16) |> Base.url_encode64(padding: false),
             directory = Path.join(Path.dirname(target), ".pdf-output-" <> suffix),
             :ok <- File.mkdir(directory) do
          # Keep the PDF private while the worker writes it. The guard owns the
          # directory before handing out its path, including on worker death.
          reply =
            with :ok <- File.chmod(directory, 0o700) do
              {:ok, Path.join(directory, "output")}
            end

          {:reply, reply, %{state | directory: directory, destination: target, mode: mode}}
        else
          error -> {:reply, error, state}
        end

      :commit ->
        staged = Path.join(state.directory, "output")

        permissions =
          if state.mode, do: File.chmod(staged, Bitwise.band(state.mode, 0o777)), else: :ok

        result = with :ok <- permissions, do: File.rename(staged, state.destination)
        {:stop, :normal, result, state}
    end
  end

  @impl true
  def handle_cast(:discard, state), do: {:stop, :normal, state}

  @impl true
  def handle_info({:DOWN, _monitor, :process, _owner, _reason}, state),
    do: {:stop, :normal, state}

  @impl true
  def terminate(_reason, state) do
    if state.directory do
      File.rm(Path.join(state.directory, "output"))
      File.rmdir(state.directory)
    end

    :ok
  end

  defp destination(path, base, links) do
    # Keep trailing separators significant when anchoring a relative path.
    path = if path != "" and Path.type(path) == :relative, do: base <> "/" <> path, else: path

    case File.lstat(path) do
      {:error, :enoent} when path != "" ->
        {:ok, path, nil}

      {:ok, %{type: :regular, access: access, mode: mode}} when access in [:write, :read_write] ->
        {:ok, path, mode}

      {:ok, %{type: :symlink}} when links < 40 ->
        with {:ok, target} <- File.read_link(path) do
          destination(target, Path.dirname(path), links + 1)
        end

      {:ok, %{type: :symlink}} ->
        {:error, :eloop}

      {:ok, %{type: :directory}} ->
        {:error, :eisdir}

      {:ok, _} ->
        {:error, :eacces}

      error ->
        error
    end
  end
end
