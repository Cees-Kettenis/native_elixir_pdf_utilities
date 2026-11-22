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
          nil ->
            loader.()

          cache ->
            :global.trans(
              {{__MODULE__, key}, self()},
              fn ->
                case cached(key) do
                  {:hit, result} ->
                    result

                  :miss ->
                    result = loader.()

                    if FontValidator.cacheable_result?(result) do
                      entry = {key, result, System.unique_integer([:positive, :monotonic])}

                      request =
                        case FontValidator.cache_entry_size(entry) do
                          nil -> :prune
                          bytes -> {:store, entry, bytes}
                        end

                      :ok = GenServer.call(cache, request, :infinity)
                    end

                    result
                end
              end,
              [node()]
            )
        end
    end
  end

  @impl GenServer
  def init(options) do
    table = :ets.new(@table, [:named_table, :protected, :set, read_concurrency: true])

    {:ok,
     %{maximum_entries: Keyword.fetch!(options, :maximum_entries), entries: %{}, table: table}}
  end

  @impl GenServer
  def handle_call(request, _from, state) do
    # Only the caller sizes the value. Eviction needs its key, retained byte
    # count and admission order, including when a lowered limit triggers pruning.
    entries =
      case request do
        {:store, {key, _result, sequence} = entry, bytes} ->
          :ets.insert(state.table, entry)
          Map.put(state.entries, key, {key, bytes, sequence})

        :prune ->
          state.entries
      end

    evicted =
      FontValidator.cache_evictions(
        Map.values(entries),
        state.maximum_entries,
        :max_cache_bytes
      )

    Enum.each(evicted, &:ets.delete(state.table, &1))
    {:reply, :ok, %{state | entries: Map.drop(entries, evicted)}}
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
