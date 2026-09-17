defmodule NativeElixirPdfUtilities.HtmlToPdf.SystemFontCache do
  @moduledoc false

  use GenServer

  alias NativeElixirPdfUtilities.Validators.FontValidator

  @table __MODULE__

  @doc false
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(options) do
    GenServer.start_link(__MODULE__, options, name: __MODULE__)
  end

  @doc false
  @spec fetch(term(), (-> term())) :: term()
  def fetch(key, loader) do
    case cached(key) do
      {:hit, result} ->
        result

      :miss ->
        case Process.whereis(__MODULE__) do
          nil -> loader.()
          _pid -> GenServer.call(__MODULE__, {:fetch, key, loader}, :infinity)
        end
    end
  end

  @impl GenServer
  def init(options) do
    table = :ets.new(@table, [:named_table, :protected, :set, read_concurrency: true])

    {:ok,
     %{maximum_entries: Keyword.fetch!(options, :maximum_entries), sequence: 0, table: table}}
  end

  @impl GenServer
  def handle_call({:fetch, key, loader}, _from, state) do
    case cached(key) do
      {:hit, result} ->
        {:reply, result, state}

      :miss ->
        result = loader.()

        case FontValidator.cacheable_result?(result) do
          true ->
            sequence = state.sequence + 1
            :ets.insert(state.table, {key, result, sequence})

            state.table
            |> :ets.tab2list()
            |> FontValidator.cache_evictions(state.maximum_entries, :max_system_font_cache_bytes)
            |> Enum.each(&:ets.delete(state.table, &1))

            {:reply, result, %{state | sequence: sequence}}

          false ->
            {:reply, result, state}
        end
    end
  end

  defp cached(key) do
    case :ets.whereis(@table) do
      :undefined ->
        :miss

      _table ->
        case :ets.lookup(@table, key) do
          [{^key, result, _sequence}] -> {:hit, result}
          [] -> :miss
        end
    end
  end
end
