defmodule NativeElixirPdfUtilities.HtmlToPdf.FormValueBackend do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.HtmlFormValueValidator

  @doc "Runs admitted form-value work in a monitored Elixir process with a deadline."
  @spec run(map()) ::
          {:ok, atom(), map()}
          | {:limit, atom()}
          | {:unsupported_pattern, binary()}
          | :timeout
          | :unavailable
  def run(request) do
    if request.timeout == 0 do
      :timeout
    else
      {worker, monitor} =
        spawn_monitor(fn ->
          exit({:form_value_result, HtmlFormValueValidator.evaluate(request)})
        end)

      receive do
        {:DOWN, ^monitor, :process, ^worker, {:form_value_result, result}} ->
          result

        {:DOWN, ^monitor, :process, ^worker, _reason} ->
          :unavailable
      after
        request.timeout ->
          Process.exit(worker, :kill)

          receive do
            {:DOWN, ^monitor, :process, ^worker, _reason} -> :ok
          end

          :timeout
      end
    end
  end
end
