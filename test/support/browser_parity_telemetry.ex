defmodule NativeElixirPdfUtilities.TestSupport.BrowserParityTelemetry do
  @moduledoc false
  @run_id "#{System.system_time(:nanosecond)}-#{System.pid()}"

  @doc "Returns the current comparison context for a capture worker."
  @spec context() :: map()
  def context, do: Process.get({__MODULE__, :context}, %{})

  @doc "Associates a browser worker with its calling comparison."
  @spec attach(map()) :: :ok
  def attach(context) do
    Process.put({__MODULE__, :context}, context)
    :ok
  end

  @doc "Adds work counters to the current operation."
  @spec annotate(map()) :: :ok
  def annotate(details), do: attach(Map.merge(context(), details))

  @doc "Records an operation, including failed attempts, without changing its result."
  @spec measure(atom(), map(), (-> result)) :: result when result: var
  def measure(phase, details, callback) do
    parent = context()
    id = "#{System.system_time(:nanosecond)}-#{System.unique_integer([:positive])}"
    owner = Process.get(:"$process_label")

    current =
      parent
      |> Map.merge(details)
      |> Map.put(:id, id)
      |> Map.put(:parent_id, parent[:id])
      |> Map.put_new(:owner, inspect(owner))

    attach(current)
    started = System.monotonic_time(:microsecond)
    record(phase, current, started, :started)

    try do
      result = callback.()
      current = context()

      details =
        case {phase, result} do
          {:comparison, stats} ->
            Map.merge(
              current,
              Map.take(stats, [
                :page_count,
                :dpi,
                :pages,
                :font_sources,
                :chromium_version,
                :rasterizer_version,
                :reference,
                :reference_identity,
                :capture
              ])
            )

          _ ->
            current
        end

      record(phase, details, started, :ok)
      result
    catch
      kind, reason ->
        record(phase, Map.put(context(), :failure, inspect({kind, reason})), started, :error)
        :erlang.raise(kind, reason, __STACKTRACE__)
    after
      attach(parent)
    end
  end

  defp record(phase, details, started, status) do
    directory =
      System.get_env("BROWSER_PARITY_TELEMETRY", Path.join(".quality/browser-parity", @run_id))

    if directory != "off" do
      File.mkdir_p!(directory)

      event =
        Map.merge(details, %{
          phase: phase,
          status: status,
          elapsed_us: System.monotonic_time(:microsecond) - started,
          started_us: started,
          run_id: @run_id,
          source_archive_sha256: System.get_env("BROWSER_PARITY_SOURCE_SHA256"),
          elixir: System.version(),
          otp: to_string(:erlang.system_info(:otp_release)),
          partition: System.get_env("MIX_TEST_PARTITION")
        })

      path = Path.join(directory, details.id <> ".json")
      File.write!(path <> ".tmp", :json.encode(json_value(event)))
      File.rename!(path <> ".tmp", path)
    end
  end

  defp json_value(value) do
    cond do
      is_map(value) -> Map.new(value, fn {key, item} -> {key, json_value(item)} end)
      is_tuple(value) -> value |> Tuple.to_list() |> Enum.map(&json_value/1)
      is_list(value) -> Enum.map(value, &json_value/1)
      true -> value
    end
  end
end
