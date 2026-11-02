defmodule NativeElixirPdfUtilities.HtmlToPdf.FormValueLifecycleTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Operation}
  alias NativeElixirPdfUtilities.HtmlToPdf.FormValueBackend
  alias NativeElixirPdfUtilities.Validators.HtmlFormValueValidator

  setup do
    limits = Limits.effective()
    Code.ensure_loaded!(FormValueBackend)
    Code.ensure_loaded!(HtmlFormValueValidator)
    :erlang.trace_pattern({FormValueBackend, :run, 1}, true, [:local])
    :erlang.trace_pattern({HtmlFormValueValidator, :evaluate, 1}, true, [:local])

    on_exit(fn ->
      :erlang.trace_pattern({FormValueBackend, :run, 1}, false, [:local])
      :erlang.trace_pattern({HtmlFormValueValidator, :evaluate, 1}, false, [:local])
      Limits.install(limits)
    end)

    :ok
  end

  test "caller termination cancels a suspended form evaluator and its watcher" do
    assert {:ok, request} = HtmlFormValueValidator.prepare(:url, expensive_url(), [])
    processes = start_evaluation(fn -> FormValueBackend.run(request) end)
    assert processes.owner == processes.caller
    Process.exit(processes.caller, :kill)
    assert_stopped(processes)
  end

  test "public render cancellation stops real Unicode URL evaluation" do
    html = "<input type='url' value='#{expensive_url()}'>"

    processes =
      start_evaluation(
        fn -> HtmlToPdf.render(html, system_font_discovery: false) end,
        false
      )

    assert processes.owner != processes.caller
    owner_monitor = Process.monitor(processes.owner)
    Process.exit(processes.caller, :kill)
    assert_receive {:DOWN, ^owner_monitor, :process, _, :killed}, 5_000
    assert_stopped(processes)
  end

  test "an outer operation deadline cancels its nested evaluator" do
    Limits.install(Map.put(Limits.effective(), :max_operation_timeout_ms, 1_000))
    assert {:ok, request} = HtmlFormValueValidator.prepare(:url, expensive_url(), [])

    processes =
      start_evaluation(fn ->
        Operation.run(__MODULE__, :form_value, fn -> FormValueBackend.run(request) end)
      end)

    caller = processes.caller

    assert_receive {:result, ^caller, {:error, {:resource_limit_exceeded, diagnostic}}}, 5_000
    assert diagnostic.module == __MODULE__
    assert diagnostic.operation == :form_value
    assert diagnostic.message =~ "max_operation_timeout_ms"
    assert_stopped(processes)
  end

  test "worker failure and the form deadline clean up the linked watcher" do
    assert {:ok, request} = HtmlFormValueValidator.prepare(:url, expensive_url(), [])

    for outcome <- [:unavailable, :timeout] do
      request = if outcome == :timeout, do: %{request | timeout: 1_000}, else: request
      processes = start_evaluation(fn -> FormValueBackend.run(request) end)
      caller = processes.caller

      if outcome == :unavailable, do: Process.exit(processes.worker, :kill)

      assert_receive {:result, ^caller, ^outcome}, 5_000
      assert_stopped(processes)
    end
  end

  test "successful pattern evaluation leaves no watcher after normal caller completion" do
    assert {:ok, request} = HtmlFormValueValidator.prepare(:pattern, "a+", ["aaa"])
    processes = start_evaluation(fn -> FormValueBackend.run(request) end, false)
    caller = processes.caller

    assert_receive {:result, ^caller, {:ok, :matched, _}}, 5_000
    assert_stopped(processes)
    caller_monitor = processes.caller_monitor
    assert_receive {:DOWN, ^caller_monitor, :process, ^caller, :normal}, 5_000
  end

  defp expensive_url do
    # ACE decoding takes the Unicode-host path and performs repeated list work.
    # This admitted input keeps evaluation active until cancellation is observed.
    "http://xn--" <> String.duplicate("a", 100_000) <> ".é"
  end

  defp start_evaluation(run, suspend? \\ true) do
    test = self()

    caller =
      spawn(fn ->
        receive do
          :run -> send(test, {:result, self(), run.()})
        end
      end)

    caller_monitor = Process.monitor(caller)
    on_exit(fn -> Process.exit(caller, :kill) end)
    :erlang.trace(caller, true, [:call, :procs, :set_on_spawn, {:tracer, self()}])
    send(caller, :run)

    assert_receive {:trace, owner, :call, {FormValueBackend, :run, [_request]}}, 5_000
    assert_receive {:trace, worker, :call, {HtmlFormValueValidator, :evaluate, [_request]}}, 5_000
    assert_receive {:trace, ^owner, :spawn, ^worker, _}, 5_000
    on_exit(fn -> Process.exit(worker, :kill) end)
    assert_receive {:trace, ^worker, :spawn, watcher, _}, 5_000
    on_exit(fn -> Process.exit(watcher, :kill) end)

    if suspend?, do: :erlang.suspend_process(worker)

    %{
      caller: caller,
      caller_monitor: caller_monitor,
      owner: owner,
      worker: worker,
      watcher: watcher,
      worker_monitor: Process.monitor(worker),
      watcher_monitor: Process.monitor(watcher)
    }
  end

  defp assert_stopped(processes) do
    %{worker: worker, watcher: watcher, worker_monitor: worker_ref, watcher_monitor: watcher_ref} =
      processes

    assert_receive {:DOWN, ^worker_ref, :process, ^worker, _}, 5_000
    assert_receive {:DOWN, ^watcher_ref, :process, ^watcher, _}, 5_000
    refute Process.alive?(worker)
    refute Process.alive?(watcher)
  end
end
