defmodule NativeElixirPdfUtilities.Validators.OperationValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.{Diagnostics, Limits}

  @type request :: %{
          module: module(),
          operation: atom(),
          deadline: integer(),
          memory: pos_integer(),
          interval: pos_integer(),
          timeout: pos_integer()
        }

  @doc "Prepares one operation's immutable deadline and worker-memory allowance."
  @spec prepare(module(), atom()) :: request()
  def prepare(module, operation) do
    limits = Limits.effective()

    %{
      module: module,
      operation: operation,
      deadline: System.monotonic_time(:millisecond) + limits.max_operation_timeout_ms,
      memory: limits.max_operation_memory_bytes,
      interval: limits.operation_memory_check_interval_ms,
      timeout: limits.max_operation_timeout_ms
    }
  end

  @doc "Checks observed BEAM process memory and referenced off-heap binary payloads."
  @spec memory(pid(), request()) :: :ok | :exceeded
  def memory(worker, request) do
    case Process.info(worker, [:memory, :binary]) do
      nil ->
        :ok

      [memory: bytes, binary: binaries] ->
        {total, _seen} =
          Enum.reduce(binaries, {bytes, MapSet.new()}, fn {id, size, _references}, {sum, seen} ->
            if MapSet.member?(seen, id),
              do: {sum, seen},
              else: {sum + size, MapSet.put(seen, id)}
          end)

        if total > request.memory, do: :exceeded, else: :ok
    end
  end

  @doc "Builds the shared diagnostic for a deadline, observed memory, or worker failure."
  @spec failure(request(), :timeout | :memory | :worker, term()) ::
          {:error, {atom(), Diagnostics.diagnostic()}}
  def failure(request, kind, detail \\ nil) do
    {reason, message} =
      case kind do
        :timeout ->
          {:resource_limit_exceeded,
           "operation exceeds max_operation_timeout_ms (#{request.timeout} ms); simplify the document or increase the limit. Worker termination was requested; native code may continue until its call returns"}

        :memory ->
          {:resource_limit_exceeded,
           "operation exceeds max_operation_memory_bytes (#{request.memory} bytes of observed BEAM worker memory and referenced binaries); simplify the document or increase the limit. Native allocations are outside this allowance"}

        :worker ->
          {:operation_worker_failed,
           "operation worker exited: #{inspect(detail)}. A :killed exit can indicate the GC-enforced max_operation_memory_bytes heap cap or an external kill; inspect the failure or increase the limit. Native allocations are outside the heap cap"}
      end

    Diagnostics.error(:limits, reason, message,
      module: request.module,
      operation: request.operation
    )
  end
end
