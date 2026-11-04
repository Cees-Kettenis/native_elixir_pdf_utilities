defmodule NativeElixirPdfUtilities.Operation do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.OperationValidator

  @scope_key {__MODULE__, :request}

  @doc "Runs a public document operation in a disposable, bounded BEAM worker."
  @spec run(module(), atom(), (-> result)) ::
          result | {:error, {atom(), Diagnostics.diagnostic()}}
        when result: term()
  def run(module, operation, fun) do
    case Process.get(@scope_key) do
      nil ->
        request = OperationValidator.prepare(module, operation)
        owner = self()
        metadata = Logger.metadata()

        {worker, monitor} =
          spawn_monitor(fn ->
            Process.put(@scope_key, request)
            Logger.metadata(metadata)
            worker = self()

            # Create the linked watcher inside the worker before document work
            # starts, so caller exit during setup cannot orphan an active job.
            # Sampling can wait for the worker to be scheduled; the caller's
            # independent receive deadline must never wait on that sampling.
            spawn_link(fn -> watch_memory(Process.monitor(owner), worker, request) end)

            result =
              try do
                case OperationValidator.memory(self(), request) do
                  :exceeded ->
                    OperationValidator.failure(request, :memory)

                  :ok ->
                    # This runtime heap bound is checked at GC, with the VM's
                    # minimum heap size. Binaries are observed separately with
                    # deduplication, since the VM can count repeated references.
                    Process.flag(:max_heap_size, %{
                      size:
                        max(
                          div(request.memory, :erlang.system_info(:wordsize)),
                          elem(:erlang.system_info(:min_heap_size), 1)
                        ),
                      kill: true,
                      error_logger: false,
                      include_shared_binaries: false
                    })

                    value = fun.()

                    case OperationValidator.memory(self(), request) do
                      :ok -> value
                      :exceeded -> OperationValidator.failure(request, :memory)
                    end
                end
              catch
                kind, reason -> {:operation_raise, kind, reason, __STACKTRACE__}
              end

            exit({:operation_result, result})
          end)

        try do
          receive do
            {:DOWN, ^monitor, :process, ^worker, reason} ->
              case reason do
                {:operation_result, {:operation_raise, kind, value, stacktrace}} ->
                  :erlang.raise(kind, value, stacktrace)

                {:operation_result, result} ->
                  if System.monotonic_time(:millisecond) >= request.deadline,
                    do: OperationValidator.failure(request, :timeout),
                    else: result

                :operation_memory_exceeded ->
                  OperationValidator.failure(request, :memory)

                reason ->
                  OperationValidator.failure(request, :worker, reason)
              end
          after
            max(0, request.deadline - System.monotonic_time(:millisecond)) ->
              OperationValidator.failure(request, :timeout)
          end
        after
          Process.exit(worker, :kill)
          Process.demonitor(monitor, [:flush])
        end

      _request ->
        fun.()
    end
  end

  defp watch_memory(owner_monitor, worker, request) do
    receive do
      {:DOWN, ^owner_monitor, :process, _owner, _reason} ->
        Process.exit(worker, :kill)
    after
      request.interval ->
        case OperationValidator.memory(worker, request) do
          :ok -> watch_memory(owner_monitor, worker, request)
          :exceeded -> exit(:operation_memory_exceeded)
        end
    end
  end
end
