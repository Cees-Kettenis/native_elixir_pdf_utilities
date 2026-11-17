defmodule NativeElixirPdfUtilities.TestSupport.BrowserReferenceCache do
  @moduledoc false
  alias NativeElixirPdfUtilities.TestSupport.{BrowserReferenceValidator, Paths}

  # Fixed retention policy for test artifacts, not a PDF processing limit.
  # Each runtime has its own directory. Writes and eviction are serialized.
  @retained_bytes 64 * 1024 * 1024

  @doc "Starts the optional disk cache only in an explicitly identified toolchain."
  @spec start() :: {:ok, pid()}
  def start do
    Agent.start_link(
      fn ->
        directory =
          System.get_env(
            "BROWSER_PARITY_CACHE",
            Paths.project(
              ".quality/browser-reference-cache/elixir-#{System.version() |> Version.parse!() |> then(&"#{&1.major}.#{&1.minor}")}"
            )
          )

        toolchain = System.get_env("BROWSER_PARITY_TOOLCHAIN")
        enabled = directory != "off" and is_binary(toolchain) and toolchain != ""

        profile =
          if enabled do
            paths = [
              Paths.fixture(""),
              Paths.project("priv/fonts"),
              Paths.project("tmp/font-corpus"),
              Paths.support(""),
              "/usr/share/fonts",
              "/usr/local/share/fonts"
            ]

            %{
              toolchain: toolchain,
              platform: :erlang.system_info(:system_architecture),
              dependencies: BrowserReferenceValidator.tree_identity(paths)
            }
          end

        if enabled do
          File.mkdir_p!(directory)
          for path <- Path.wildcard(Path.join(directory, "*.tmp")), do: File.rm!(path)
          prune!(directory, 0)
        end

        %{directory: directory, profile: profile}
      end,
      name: __MODULE__
    )
  end

  @doc "Stops the suite-local cache coordinator."
  @spec stop() :: :ok
  def stop do
    if pid = Process.whereis(__MODULE__), do: Agent.stop(pid)
    :ok
  end

  @doc "Checks capture ownership before the transformed HTML and observed resources are verified."
  @spec candidate?(String.t(), keyword()) :: boolean()
  def candidate?(fixture, opts) do
    Process.whereis(__MODULE__) != nil and
      Agent.get(__MODULE__, fn state ->
        state.profile != nil and
          BrowserReferenceValidator.cache_candidate?(fixture, opts, state.profile.dependencies)
      end)
  end

  @doc "Keeps dynamic inputs, alternate executables and browser lifecycle tests on fresh captures."
  @spec eligible?(String.t(), keyword()) :: boolean()
  def eligible?(fixture, opts) do
    Process.whereis(__MODULE__) != nil and
      Agent.get(__MODULE__, &(&1.profile != nil)) and
      BrowserReferenceValidator.cacheable_fixture?(
        fixture,
        opts,
        Agent.get(__MODULE__, & &1.profile.dependencies)
      )
  end

  @doc "Requires every observed browser request to belong to the fingerprint or the generated capture inputs."
  @spec requests_covered?([String.t()], [String.t()], String.t()) :: boolean()
  def requests_covered?(requests, inputs, directory) do
    dependencies = Agent.get(__MODULE__, & &1.profile.dependencies)
    BrowserReferenceValidator.requests_covered?(requests, dependencies, inputs, directory)
  end

  @doc "Returns a content key for an eligible reference, independent of native renderer changes."
  @spec key(map()) :: String.t()
  def key(identity) do
    profile = Agent.get(__MODULE__, & &1.profile)

    :crypto.hash(:sha256, :erlang.term_to_binary({profile, identity}))
    |> Base.encode16(case: :lower)
  end

  @doc "Restores verified browser bytes; a missing or damaged entry is a cache miss."
  @spec restore(String.t(), String.t()) :: map() | nil
  def restore(key, directory) do
    Agent.get(
      __MODULE__,
      fn state ->
        path = Path.join(state.directory, key <> ".cache")

        payload =
          with {:ok, %{size: size}} <- File.stat(path),
               true <- size <= @retained_bytes,
               {:ok, <<digest::binary-size(32), encoded::binary>>} <- File.read(path),
               true <- :crypto.hash(:sha256, encoded) == digest do
            try do
              value = :erlang.binary_to_term(encoded, [:safe])
              if BrowserReferenceValidator.cache_payload?(value), do: value
            rescue
              ArgumentError -> nil
            end
          else
            _ -> nil
          end

        if payload do
          for {name, bytes, _digest} <- payload.files,
              do: File.write!(Path.join(directory, name), bytes)

          File.touch!(path)

          %{
            pages: Enum.map(payload.pages, &Path.join(directory, &1)),
            vector_pages: Enum.map(payload.vector_pages, &Path.join(directory, &1)),
            embedded_fonts: payload.embedded_fonts
          }
        else
          File.rm(path)
          nil
        end
      end,
      :infinity
    )
  end

  @doc "Atomically stores compressed reference bytes and evicts least recently used entries to a fixed disk budget."
  @spec store(String.t(), map()) :: :ok
  def store(key, reference) do
    files =
      for path <- reference.artifacts do
        bytes = File.read!(path)
        {Path.basename(path), bytes, :crypto.hash(:sha256, bytes)}
      end

    payload = %{
      files: files,
      pages: Enum.map(reference.pages, &Path.basename/1),
      vector_pages: Enum.map(reference.vector_pages, &Path.basename/1),
      embedded_fonts: reference.embedded_fonts
    }

    encoded = :erlang.term_to_binary(payload, compressed: 1)
    bytes = <<:crypto.hash(:sha256, encoded)::binary, encoded::binary>>

    Agent.get(
      __MODULE__,
      fn state ->
        if byte_size(bytes) <= @retained_bytes do
          path = Path.join(state.directory, key <> ".cache")

          File.rm(path)
          prune!(state.directory, byte_size(bytes))
          File.write!(path <> ".tmp", bytes)
          File.rename!(path <> ".tmp", path)
        end

        :ok
      end,
      :infinity
    )
  end

  defp prune!(directory, incoming) do
    entries =
      Path.wildcard(Path.join(directory, "*.cache"))
      |> Enum.map(&{&1, File.stat!(&1, time: :posix)})
      |> Enum.sort_by(fn {name, stat} -> {stat.mtime, name} end)

    total = Enum.reduce(entries, incoming, fn {_, stat}, sum -> sum + stat.size end)

    Enum.reduce(entries, total, fn {name, stat}, size ->
      if size > @retained_bytes do
        File.rm!(name)
        size - stat.size
      else
        size
      end
    end)

    :ok
  end
end
