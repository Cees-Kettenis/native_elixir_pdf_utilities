defmodule NativeElixirPdfUtilities.HtmlToPdf.FormValueBackend do
  @moduledoc false

  @doc "Runs an issued byte-only form request in the disposable bounded native helper."
  @spec run(map()) :: {:ok, binary()} | {:exit, integer()} | :timeout | :unavailable
  def run(request) do
    caller = self()
    reply = make_ref()

    {worker, monitor} =
      spawn_monitor(fn ->
        # Keep linked port failures inside their owner and retain the genuine
        # exit status, without changing the caller's process flags or mailbox.
        Process.flag(:trap_exit, true)
        send(caller, {reply, run_native(request)})
      end)

    receive do
      {^reply, result} ->
        Process.demonitor(monitor, [:flush])
        result

      {:DOWN, ^monitor, :process, ^worker, _reason} ->
        :unavailable
    end
  end

  defp run_native(request) do
    executable =
      Path.join([
        Application.app_dir(:native_elixir_pdf_utilities, "priv"),
        "native",
        if(match?({:win32, _}, :os.type()),
          do: "native_form_values.exe",
          else: "native_form_values"
        )
      ])

    port =
      Port.open({:spawn_executable, executable}, [
        :binary,
        :exit_status,
        {:packet, 4},
        {:args, request.ceilings}
      ])

    try do
      send_native(port, <<byte_size(request.payload)::unsigned-big-32>>)

      receive_result(
        port,
        nil,
        System.monotonic_time(:millisecond) + request.timeout,
        request.payload
      )
    after
      if Port.info(port), do: Port.close(port)
    end
  rescue
    _error in [ArgumentError, ErlangError] -> :unavailable
  end

  # The native startup timer may close the port before either declaration or
  # payload is sent. Keep reading its actual exit status instead of turning a
  # configured native limit into an unavailable-backend diagnostic.
  defp send_native(port, payload) do
    try do
      Port.command(port, payload)
    rescue
      _error in [ArgumentError, ErlangError] -> :ok
    end
  end

  defp receive_result(port, response, deadline, payload) do
    receive do
      {^port, {:data, <<1>>}} when is_binary(payload) ->
        send_native(port, payload)
        receive_result(port, response, deadline, nil)

      {^port, {:data, data}} ->
        receive_result(port, data, deadline, payload)

      {^port, {:exit_status, 0}} ->
        if is_binary(response), do: {:ok, response}, else: :unavailable

      {^port, {:exit_status, status}} ->
        {:exit, status}

      {:EXIT, ^port, _reason} ->
        receive_result(port, response, deadline, payload)
    after
      max(deadline - System.monotonic_time(:millisecond), 0) -> :timeout
    end
  end
end
