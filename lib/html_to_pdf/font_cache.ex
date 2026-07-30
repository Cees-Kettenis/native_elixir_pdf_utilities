defmodule NativeElixirPdfUtilities.HtmlToPdf.FontCache do
  @moduledoc false

  use GenServer

  @maximum_entries 64
  @table __MODULE__

  @type load_result :: {:ok, term()} | :error
  @type loader :: (String.t() -> load_result())
  @type fingerprint :: {
          non_neg_integer(),
          integer() | :undefined,
          integer() | :undefined,
          non_neg_integer()
        }

  @doc false
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(options) do
    GenServer.start_link(__MODULE__, options, name: __MODULE__)
  end

  @doc """
  Returns a parsed font-file value from the process-wide cache.

  The loader receives an absolute file path and runs only when the file is not
  cached with the same size, modification time, change time, and inode. Failed
  loads are not retained. When the library application is not running, the
  loader runs without caching.
  """
  @spec fetch(String.t(), loader()) :: load_result()
  def fetch(path, loader) do
    absolute_path = Path.expand(path)

    case File.stat(absolute_path, time: :posix) do
      {:ok, stat} ->
        fingerprint = {stat.size, stat.mtime, stat.ctime, stat.inode}

        case cached(absolute_path, fingerprint) do
          {:hit, result} ->
            result

          :miss ->
            case Process.whereis(__MODULE__) do
              nil ->
                loader.(absolute_path)

              _pid ->
                GenServer.call(
                  __MODULE__,
                  {:fetch, absolute_path, fingerprint, loader},
                  :infinity
                )
            end
        end

      {:error, _reason} ->
        :error
    end
  end

  @impl GenServer
  def init(_options) do
    table =
      :ets.new(@table, [
        :named_table,
        :protected,
        :set,
        read_concurrency: true
      ])

    {:ok, %{sequence: 0, table: table}}
  end

  @impl GenServer
  def handle_call(request, _from, state) do
    case request do
      {:fetch, path, fingerprint, loader} ->
        case cached(path, fingerprint) do
          {:hit, result} ->
            {:reply, result, state}

          :miss ->
            result = loader.(path)

            case result do
              {:ok, _value} ->
                sequence = state.sequence + 1
                :ets.insert(state.table, {path, fingerprint, result, sequence})

                case :ets.info(state.table, :size) > @maximum_entries do
                  true ->
                    {oldest_path, _fingerprint, _result, _sequence} =
                      state.table
                      |> :ets.tab2list()
                      |> Enum.min_by(fn {_path, _fingerprint, _result, entry_sequence} ->
                        entry_sequence
                      end)

                    :ets.delete(state.table, oldest_path)

                  false ->
                    :ok
                end

                {:reply, result, %{state | sequence: sequence}}

              :error ->
                {:reply, :error, state}
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
          [{^path, ^fingerprint, result, _sequence}] -> {:hit, result}
          _entry -> :miss
        end
    end
  end
end
