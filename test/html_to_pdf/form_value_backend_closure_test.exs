defmodule NativeElixirPdfUtilities.HtmlToPdf.FormValueBackendClosureTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.FormValueBackend
  alias NativeElixirPdfUtilities.Validators.{HtmlFormValueValidator, HtmlValidator}

  setup do
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    :ok
  end

  test "native helper absence reports actionable rebuild failure without altering caller flags" do
    executable =
      Application.app_dir(:native_elixir_pdf_utilities, "priv/native/native_form_values")

    {:ok, stat} = File.stat(executable)

    HtmlValidator.with_render_budget(fn ->
      assert {:ok, request} = HtmlFormValueValidator.prepare(:url, "https://example.com/", [])
      flags = Process.info(self(), :trap_exit)

      try do
        assert :ok = File.chmod(executable, 0o600)
        assert :unavailable = FormValueBackend.run(request)

        assert {:error, {:native_form_backend_unavailable, diagnostic}} =
                 HtmlFormValueValidator.result(:unavailable)

        assert diagnostic.stage == :html
        assert diagnostic.operation == :initial_form_value
        assert diagnostic.message =~ "rebuild"
        assert Process.info(self(), :trap_exit) == flags
      after
        File.chmod!(executable, stat.mode)
      end
    end)
  end

  test "actual helper allocation exit preserves caller mailbox and maps the configured limit" do
    assert :ok = Limits.install(%{Limits.effective() | max_html_form_native_allocation_bytes: 1})

    HtmlValidator.with_render_budget(fn ->
      assert {:ok, request} = HtmlFormValueValidator.prepare(:pattern, "[a-z]+", ["abc"])
      assert {:exit, 73} = result = FormValueBackend.run(request)

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlFormValueValidator.result(result)

      assert diagnostic.message =~ "max_html_form_native_allocation_bytes"
      refute_receive {:EXIT, _, _}
    end)
  end

  test "an actual disposable worker death returns unavailable without leaking its monitor" do
    HtmlValidator.with_render_budget(fn ->
      assert {:ok, request} =
               HtmlFormValueValidator.prepare(:pattern, String.duplicate("a?", 5_000), [
                 String.duplicate("a", 5_000)
               ])

      caller =
        Task.async(fn ->
          receive do
            :run -> FormValueBackend.run(request)
          end
        end)

      :erlang.trace(caller.pid, true, [:procs, {:tracer, self()}])
      send(caller.pid, :run)
      assert_receive {:trace, caller_pid, :spawn, worker, _}, 1_000
      assert caller_pid == caller.pid
      Process.exit(worker, :kill)
      assert :unavailable = Task.await(caller)
    end)
  end

  test "closing the actual native worker port remains isolated from the caller" do
    HtmlValidator.with_render_budget(fn ->
      assert {:ok, request} =
               HtmlFormValueValidator.prepare(:pattern, "(a+)+b", [String.duplicate("a", 500_000)])

      caller =
        Task.async(fn ->
          receive do
            :run -> FormValueBackend.run(request)
          end
        end)

      Code.ensure_loaded!(FormValueBackend)
      assert :erlang.trace_pattern({FormValueBackend, :receive_result, 4}, true, [:local]) == 1
      :erlang.trace(caller.pid, true, [:procs, :call, :set_on_spawn, {:tracer, self()}])

      try do
        send(caller.pid, :run)
        assert_receive {:trace, caller_pid, :spawn, worker, _}, 1_000
        assert caller_pid == caller.pid

        assert_receive {:trace, ^worker, :call,
                        {FormValueBackend, :receive_result, [port, _, _, nil]}},
                       1_000

        assert :erlang.suspend_process(worker)

        try do
          assert {:connected, ^worker} = Port.info(port, :connected)
          assert Port.close(port)
        after
          :erlang.resume_process(worker)
        end

        assert Task.await(caller, 6_000) in [:unavailable, :timeout]
        refute_receive {:EXIT, ^port, _}
      after
        :erlang.trace_pattern({FormValueBackend, :receive_result, 4}, false, [:local])
      end
    end)
  end

  test "native timeout after readiness retains the actual exit when the payload send meets a closed port" do
    assert :ok = Limits.install(%{Limits.effective() | max_html_form_native_timeout_ms: 250})

    HtmlValidator.with_render_budget(fn ->
      assert {:ok, request} = HtmlFormValueValidator.prepare(:pattern, "a+", ["aaaa"])

      caller =
        Task.async(fn ->
          receive do
            :run -> FormValueBackend.run(request)
          end
        end)

      Code.ensure_loaded!(FormValueBackend)
      assert :erlang.trace_pattern({FormValueBackend, :receive_result, 4}, true, [:local]) == 1
      :erlang.trace(caller.pid, true, [:procs, :call, :set_on_spawn, {:tracer, self()}])

      try do
        send(caller.pid, :run)
        assert_receive {:trace, caller_pid, :spawn, worker, _}, 1_000
        assert caller_pid == caller.pid

        assert_receive {:trace, ^worker, :call,
                        {FormValueBackend, :receive_result, [port, nil, _, payload]}},
                       1_000

        assert payload == request.payload
        assert :erlang.suspend_process(worker)

        try do
          assert {:connected, ^worker} = Port.info(port, :connected)
          deadline = System.monotonic_time(:millisecond) + 5_000
          wait_for_native_exit(port, deadline)
          assert Port.info(port) == nil
          assert {:messages, messages} = Process.info(worker, :messages)
          assert {port, {:data, <<1>>}} in messages
          assert {port, {:exit_status, 78}} in messages
        after
          :erlang.resume_process(worker)
        end

        assert {:exit, 78} = result = Task.await(caller, 6_000)

        assert {:error, {:resource_limit_exceeded, diagnostic}} =
                 HtmlFormValueValidator.result(result)

        assert diagnostic.message =~ "max_html_form_native_timeout_ms"
        refute_receive {:EXIT, ^port, _}
      after
        :erlang.trace_pattern({FormValueBackend, :receive_result, 4}, false, [:local])
      end
    end)
  end

  defp wait_for_native_exit(port, deadline) do
    if Port.info(port) != nil do
      assert System.monotonic_time(:millisecond) < deadline
      Process.sleep(5)
      wait_for_native_exit(port, deadline)
    end
  end

  test "zero native ceilings charge actual work and retain unused URL match allowance" do
    original = Limits.effective()

    for {limit, operation, source, values, status} <- [
          {:max_html_form_native_allocation_bytes, :pattern, "a", ["a"], 73},
          {:max_html_pattern_compile_work, :pattern, "a", ["a"], 74},
          {:max_html_pattern_depth, :pattern, "(a)", ["a"], 76},
          {:max_html_pattern_match_work, :pattern, "a", ["a"], 75}
        ] do
      assert :ok = Limits.install(Map.put(original, limit, 0))

      HtmlValidator.with_render_budget(fn ->
        assert {:ok, request} = HtmlFormValueValidator.prepare(operation, source, values)
        assert {:exit, ^status} = result = FormValueBackend.run(request)

        assert {:error, {:resource_limit_exceeded, diagnostic}} =
                 HtmlFormValueValidator.result(result)

        assert diagnostic.message =~ Atom.to_string(limit)
      end)
    end

    assert :ok = Limits.install(%{original | max_html_pattern_match_work: 0})

    HtmlValidator.with_render_budget(fn ->
      assert {:ok, request} = HtmlFormValueValidator.prepare(:url, "https://example.com/", [])
      assert {:ok, _} = result = FormValueBackend.run(request)
      assert {:ok, :valid_url} = HtmlFormValueValidator.result(result)
    end)

    assert :ok = Limits.install(%{original | max_html_form_native_timeout_ms: 0})

    HtmlValidator.with_render_budget(fn ->
      assert {:ok, request} = HtmlFormValueValidator.prepare(:pattern, "a", ["a"])
      assert result = FormValueBackend.run(request)
      assert result in [:timeout, {:exit, 78}]

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlFormValueValidator.result(result)

      assert diagnostic.message =~ "max_html_form_native_timeout_ms"
    end)
  end
end
