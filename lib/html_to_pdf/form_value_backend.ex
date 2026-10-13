defmodule NativeElixirPdfUtilities.HtmlToPdf.FormValueBackend do
  @moduledoc false

  @doc "Runs an issued byte-only form request in the disposable bounded native helper."
  @spec run(map()) :: {:ok, binary()} | {:exit, integer()} | :timeout | :unavailable
  def run(request) do
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
      Port.command(port, request.payload)
      receive_result(port, nil, System.monotonic_time(:millisecond) + request.timeout)
    after
      if Port.info(port), do: Port.close(port)
    end
  rescue
    _error in [ArgumentError, ErlangError] -> :unavailable
  end

  defp receive_result(port, response, deadline) do
    receive do
      {^port, {:data, data}} ->
        receive_result(port, data, deadline)

      {^port, {:exit_status, 0}} ->
        if is_binary(response), do: {:ok, response}, else: :unavailable

      {^port, {:exit_status, status}} ->
        {:exit, status}
    after
      max(deadline - System.monotonic_time(:millisecond), 0) -> :timeout
    end
  end
end
