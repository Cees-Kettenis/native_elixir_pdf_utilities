defmodule NativeElixirPdfUtilities.HtmlToPdf.FormValueBackend do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.HtmlFormValueValidator

  @doc "Runs admitted form-value work with an external deadline and a GC-enforced BEAM heap cap."
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
      owner = self()

      {worker, monitor} =
        spawn_monitor(fn ->
          worker = self()

          # Establish ownership before evaluation starts. The linked watcher
          # also exits when the evaluator returns, crashes or reaches a limit.
          spawn_link(fn ->
            owner_monitor = Process.monitor(owner)

            receive do
              {:DOWN, ^owner_monitor, :process, ^owner, _reason} ->
                Process.exit(worker, :kill)
            end
          end)

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
