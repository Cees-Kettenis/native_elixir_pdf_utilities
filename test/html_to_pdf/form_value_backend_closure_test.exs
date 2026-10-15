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

  test "memory exhaustion preserves caller flags and unrelated messages" do
    Limits.install(%{Limits.effective() | max_html_form_value_memory_bytes: 1})
    flags = Process.info(self(), :trap_exit)
    marker = make_ref()
    send(self(), {:caller_message, marker})
    assert {:ok, request} = HtmlFormValueValidator.prepare(:pattern, "[a-z]+", ["abc"])
    assert {:limit, :max_html_form_value_memory_bytes} = result = FormValueBackend.run(request)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlFormValueValidator.result(result)

    assert diagnostic.message =~ "max_html_form_value_memory_bytes"
    assert Process.info(self(), :trap_exit) == flags
    assert_receive {:caller_message, ^marker}
    refute_receive {:EXIT, _, _}
    refute_receive {:DOWN, _, _, _, _}
  end

  test "worker death returns unavailable and consumes its monitor" do
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
  end

  test "a running worker that stalls is killed at its nonzero deadline" do
    Limits.install(%{Limits.effective() | max_html_form_value_timeout_ms: 250})

    assert {:ok, request} =
             HtmlFormValueValidator.prepare(:pattern, String.duplicate("a?", 30_000), ["a"])

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
    :erlang.suspend_process(worker)
    :erlang.trace(caller.pid, false, [:procs])
    on_exit(fn -> if Process.alive?(worker), do: Process.exit(worker, :kill) end)

    assert :timeout == Task.await(caller)
    refute Process.alive?(worker)
    refute_receive {:DOWN, _, _, _, _}
    refute_receive {_, {:ok, _, _}}
  end

  test "configured zero ceilings identify the actual consumed resource" do
    original = Limits.effective()

    for {limit, source} <- [
          {:max_html_form_value_memory_bytes, "a"},
          {:max_html_pattern_compile_work, "a"},
          {:max_html_pattern_depth, "(a)"},
          {:max_html_pattern_match_work, "a"}
        ] do
      Limits.install(Map.put(original, limit, 0))
      assert {:ok, request} = HtmlFormValueValidator.prepare(:pattern, source, ["a"])
      assert {:limit, ^limit} = result = FormValueBackend.run(request)

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlFormValueValidator.result(result)

      assert diagnostic.stage == :html
      assert diagnostic.operation == :initial_form_value
      assert diagnostic.message =~ Atom.to_string(limit)
    end
  end

  test "URL validation does not consume unused pattern matching work" do
    Limits.install(%{Limits.effective() | max_html_pattern_match_work: 0})

    HtmlValidator.with_render_budget(fn ->
      assert {:ok, request} = HtmlFormValueValidator.prepare(:url, "https://example.test", [])
      assert {:ok, :valid_url} = HtmlFormValueValidator.result(FormValueBackend.run(request))
    end)
  end

  test "deadline exhaustion leaves later calls and the caller mailbox usable" do
    original = Limits.effective()
    Limits.install(%{original | max_html_form_value_timeout_ms: 0})

    assert {:ok, request} =
             HtmlFormValueValidator.prepare(:pattern, "(a+)+b", [String.duplicate("a", 50)])

    assert :timeout = result = FormValueBackend.run(request)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlFormValueValidator.result(result)

    assert diagnostic.message =~ "max_html_form_value_timeout_ms"
    refute_receive {:DOWN, _, _, _, _}
    refute_receive {_, {:ok, _, _}}
    Limits.install(original)
    assert {:ok, request} = HtmlFormValueValidator.prepare(:pattern, "a+", ["aaa"])
    assert {:ok, :matched} = HtmlFormValueValidator.result(FormValueBackend.run(request))
  end

  test "worker failures retain actionable shared diagnostics" do
    assert {:error, {:form_value_backend_unavailable, diagnostic}} =
             HtmlFormValueValidator.result(:unavailable)

    assert diagnostic.stage == :html
    assert diagnostic.operation == :initial_form_value
    assert diagnostic.message =~ "Elixir"
  end

  test "URL admission reserves native compile work before host parsing" do
    Limits.install(%{Limits.effective() | max_html_pattern_compile_work: 10})

    assert {:ok, request} = HtmlFormValueValidator.prepare(:url, "https://example.test", [])
    assert {:limit, :max_html_pattern_compile_work} = result = FormValueBackend.run(request)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlFormValueValidator.result(result)

    assert diagnostic.stage == :html
    assert diagnostic.operation == :initial_form_value
    assert diagnostic.message =~ "max_html_pattern_compile_work"
  end

  test "the compiled expression is charged against the form-value memory ceiling" do
    # The worker survives its entry memory check but the escaped expression is
    # three times the authored source and exceeds the ceiling.
    Limits.install(%{Limits.effective() | max_html_form_value_memory_bytes: 30_000})

    assert {:ok, request} =
             HtmlFormValueValidator.prepare(:pattern, String.duplicate("\\v", 10_000), [])

    assert {:limit, :max_html_form_value_memory_bytes} = result = FormValueBackend.run(request)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlFormValueValidator.result(result)

    assert diagnostic.stage == :html
    assert diagnostic.operation == :initial_form_value
    assert diagnostic.message =~ "max_html_form_value_memory_bytes"
  end

  test "native recursion exhaustion reports the pattern depth limit" do
    Limits.install(%{Limits.effective() | max_html_pattern_depth: 5})

    assert {:ok, request} =
             HtmlFormValueValidator.prepare(:pattern, "(a+)+b", [String.duplicate("a", 20)])

    assert {:limit, :max_html_pattern_depth} = result = FormValueBackend.run(request)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlFormValueValidator.result(result)

    assert diagnostic.stage == :html
    assert diagnostic.operation == :initial_form_value
    assert diagnostic.message =~ "max_html_pattern_depth"
  end
end
