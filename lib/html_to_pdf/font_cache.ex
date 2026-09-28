defmodule NativeElixirPdfUtilities.HtmlToPdf.FontCache do
  @moduledoc false

  use GenServer

  alias NativeElixirPdfUtilities.Validators.FontValidator

  @table __MODULE__

  @type load_result :: {:ok, term()} | :error | {:error, {atom(), map()}}
  @type loader :: (binary() -> load_result())
  @type fingerprint :: binary()

  @doc false
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(options) do
    GenServer.start_link(__MODULE__, options, name: __MODULE__)
  end

  @doc """
  Returns a parsed font-file value from the process-wide cache.

  Each render reads one bounded snapshot per path and fingerprints its contents.
  Standalone fetch calls each read a new snapshot. The loader receives those
  same bytes and runs only when that content is not cached for the path.
  Failed loads are not retained. When the library application is not running,
  the loader runs without caching.
  """
  @spec fetch(String.t(), loader()) :: load_result()
  def fetch(path, loader) do
    FontValidator.with_budget(fn ->
      FontValidator.memo({:file, Path.expand(path)}, fn ->
        absolute_path = Path.expand(path)

        FontValidator.reserve(:max_font_candidates, 1)

        case NativeElixirPdfUtilities.FileReader.read(
               absolute_path,
               FontValidator.read_limit()
             ) do
          {:ok, data} ->
            FontValidator.reserve_source(data)
            fingerprint = :crypto.hash(:sha256, data)

            case cached(absolute_path, fingerprint) do
              {:hit, result} ->
                result

              :miss ->
                case Process.whereis(__MODULE__) do
                  nil ->
                    loader.(data)

                  _pid ->
                    GenServer.call(
                      __MODULE__,
                      {:fetch, absolute_path, fingerprint, data, loader},
                      :infinity
                    )
                end
            end

          {:error, {_reason, _diagnostic}} = error ->
            FontValidator.source_result(error)

          {:error, _reason} ->
            :error
        end
      end)
    end)
  end

  @doc """
  Reuses an installed, immutable font asset without reading it again.

  Caller-configured paths must use `fetch/2` so edits to those files remain visible.
  """
  @spec fetch_bundled(String.t(), loader()) :: load_result()
  def fetch_bundled(path, loader) do
    FontValidator.with_budget(fn ->
      absolute_path = Path.expand(path)

      FontValidator.memo({:bundled_file, absolute_path}, fn ->
        case :ets.whereis(@table) do
          :undefined ->
            fetch(absolute_path, loader)

          _table ->
            case :ets.lookup(@table, absolute_path) do
              [{^absolute_path, fingerprint, data, result, _sequence}] ->
                FontValidator.reserve(:max_font_candidates, 1)
                FontValidator.reserve_source_fingerprint(fingerprint, byte_size(data))
                result

              _ ->
                fetch(absolute_path, loader)
            end
        end
      end)
    end)
  end

  @impl GenServer
  def init(options) do
    table =
      :ets.new(@table, [
        :named_table,
        :protected,
        :set,
        read_concurrency: true
      ])

    {:ok,
     %{maximum_entries: Keyword.fetch!(options, :maximum_entries), sequence: 0, table: table}}
  end

  @impl GenServer
  def handle_call(request, _from, state) do
    case request do
      {:fetch, path, fingerprint, data, loader} ->
        case cached(path, fingerprint) do
          {:hit, result} ->
            {:reply, result, state}

          :miss ->
            result = loader.(data)

            case result do
              {:ok, _value} ->
                sequence = state.sequence + 1
                :ets.insert(state.table, {path, fingerprint, data, result, sequence})

                state.table
                |> :ets.tab2list()
                |> FontValidator.cache_evictions(state.maximum_entries, :max_font_cache_bytes)
                |> Enum.each(&:ets.delete(state.table, &1))

                {:reply, result, %{state | sequence: sequence}}

              failure ->
                {:reply, failure, state}
            end
        end
    end
  end

  defp cached(path, fingerprint) do
    case :ets.whereis(@table) do
      :undefined ->
        :miss

      _table ->
        case :ets.lookup(@table, path) do
          [{^path, ^fingerprint, _data, result, _sequence}] -> {:hit, result}
          _entry -> :miss
        end
    end
  end
end
