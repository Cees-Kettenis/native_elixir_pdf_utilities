defmodule NativeElixirPdfUtilities.OperationTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Info, Limits, Operation}
  alias NativeElixirPdfUtilities.TestSupport.Paths
  alias NativeElixirPdfUtilities.Validators.{LimitsValidator, OperationValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "document operations reject an insufficient worker allowance and accept a raised limit" do
    pdf = File.read!(Paths.fixture("pdf_reader/classic-xref.pdf"))
    Limits.install(Map.put(Limits.effective(), :max_operation_memory_bytes, 1))

    operations = [
      {HtmlToPdf, :render, fn -> HtmlToPdf.render("<p>Public input</p>") end},
      {Info, :page_count, fn -> Info.page_count(pdf) end},
      {NativeElixirPdfUtilities.Merge, :merge,
       fn -> NativeElixirPdfUtilities.Merge.merge([pdf]) end},
      {NativeElixirPdfUtilities.Forms, :fields,
       fn -> NativeElixirPdfUtilities.Forms.fields(pdf) end},
      {NativeElixirPdfUtilities.Attachments, :list,
       fn -> NativeElixirPdfUtilities.Attachments.list(pdf) end},
      {NativeElixirPdfUtilities.Outlines, :get_outlines,
       fn -> NativeElixirPdfUtilities.Outlines.get(pdf) end},
      {NativeElixirPdfUtilities.Split, :split_by_page,
       fn -> NativeElixirPdfUtilities.Split.by_page(pdf) end},
      {NativeElixirPdfUtilities.Stamp, :stamp_text,
       fn -> NativeElixirPdfUtilities.Stamp.text(pdf, "Stamp") end},
      {NativeElixirPdfUtilities.Text, :extract,
       fn -> NativeElixirPdfUtilities.Text.extract(pdf) end},
      {NativeElixirPdfUtilities.Transform, :pick_pages,
       fn -> NativeElixirPdfUtilities.Transform.pick_pages(pdf, [1]) end},
      {NativeElixirPdfUtilities.Pdf.Reader, :read,
       fn -> NativeElixirPdfUtilities.Pdf.Reader.read(pdf) end}
    ]

    for {module, operation, run} <- operations do
      assert {:error, {:resource_limit_exceeded, diagnostic}} = run.()
      assert diagnostic.stage == :limits
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.module == module
      assert diagnostic.operation == operation
      assert diagnostic.message =~ "max_operation_memory_bytes"
    end

    Limits.install(Map.put(Limits.effective(), :max_operation_memory_bytes, 16_777_216))
    assert {:ok, 1} = Info.page_count(pdf)
    assert {:ok, _pdf} = HtmlToPdf.render("<p>Public input</p>")
  end

  test "a stalled resolver times out without changing caller flags or consuming its messages" do
    callback_owner = self()
    Limits.install(Map.put(Limits.effective(), :max_operation_timeout_ms, 1_000))
    owner = self()
    flags = Process.info(self(), :trap_exit)
    monitors = Process.info(self(), :monitors)
    send(callback_owner, :unrelated_message)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<img src='asset:stall'>",
               asset_resolver: fn _request ->
                 send(owner, {:resolver_worker, self()})
                 Process.sleep(:infinity)
               end
             )

    assert diagnostic.stage == :limits
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.module == HtmlToPdf
    assert diagnostic.operation == :render
    assert diagnostic.message =~ "max_operation_timeout_ms"
    assert diagnostic.message =~ "native"
    assert_received :unrelated_message
    assert_received {:resolver_worker, worker}
    assert worker != owner
    monitor = Process.monitor(worker)
    assert_receive {:DOWN, ^monitor, :process, ^worker, _reason}
    assert Process.info(self(), :trap_exit) == flags
    assert Process.info(self(), :monitors) == monitors
    refute_received {:DOWN, _, :process, _, _}

    Limits.install(Map.put(Limits.effective(), :max_operation_timeout_ms, 300_000))
    assert {:ok, _pdf} = HtmlToPdf.render("<p>Next operation</p>")
  end

  test "asset resolvers render captured caller data while keeping caller state isolated" do
    png = File.read!(Paths.fixture("html_to_pdf/browser_parity/assets/initial-small.png"))
    key = make_ref()
    Process.put(key, png)
    table = :ets.new(:resolver_assets, [:set, :private])
    :ets.insert(table, {:logo, png})
    caller = self()

    try do
      dictionary_logo = Process.get(key)
      [{:logo, table_logo}] = :ets.lookup(table, :logo)

      resolver = fn request ->
        send(caller, {:resolved_asset, request.reference, self(), Process.get(key)})
        Process.put(key, :worker_value)

        case request do
          %{reference: "asset:dictionary", kind: :image} -> {:ok, dictionary_logo}
          %{reference: "asset:private-ets", kind: :image} -> {:ok, table_logo}
          _ -> :not_found
        end
      end

      assert {:ok, pdf} =
               HtmlToPdf.render("<img src='asset:dictionary'><img src='asset:private-ets'>",
                 asset_resolver: resolver,
                 system_font_discovery: false
               )

      assert pdf =~ "/Subtype /Image"
      assert {:ok, 1} = Info.page_count(pdf)
      assert_received {:resolved_asset, "asset:dictionary", worker, dictionary_state}
      assert_received {:resolved_asset, "asset:private-ets", ^worker, table_state}
      assert worker != caller
      refute png in [dictionary_state, table_state]
      refute Process.alive?(worker)
      assert Process.get(key) == png
      assert :ets.lookup(table, :logo) == [{:logo, png}]
    after
      Process.delete(key)
      :ets.delete(table)
    end
  end

  test "unmigrated caller-private resolver lookups return public asset diagnostics" do
    png = File.read!(Paths.fixture("html_to_pdf/browser_parity/assets/initial-small.png"))
    key = make_ref()
    Process.put(key, png)
    table = :ets.new(:resolver_assets, [:set, :private])
    :ets.insert(table, {:logo, png})

    try do
      for resolver <- [
            fn _request ->
              case Process.get(key) do
                nil -> :not_found
                bytes -> {:ok, bytes}
              end
            end,
            fn _request ->
              [{:logo, bytes}] = :ets.lookup(table, :logo)
              {:ok, bytes}
            end
          ] do
        assert {:error, {:invalid_document, diagnostic}} =
                 HtmlToPdf.render("<img src='asset:logo'>",
                   asset_resolver: resolver,
                   system_font_discovery: false
                 )

        assert diagnostic.stage == :asset and diagnostic.reason == :invalid_document
        assert diagnostic.operation == :render and diagnostic.module == HtmlToPdf
        assert diagnostic.source == "asset:logo"
        assert diagnostic.message =~ "asset"
      end
    after
      Process.delete(key)
      :ets.delete(table)
    end
  end

  test "a resolver retaining excess binary memory is stopped by the worker allowance" do
    Limits.install(
      Map.merge(Limits.effective(), %{
        max_operation_memory_bytes: 16_777_216,
        max_operation_timeout_ms: 2_000,
        operation_memory_check_interval_ms: 1
      })
    )

    caller = self()

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<img src='asset:oversized'>",
               system_font_discovery: false,
               asset_resolver: fn _request ->
                 send(caller, {:resolver_memory_worker, self()})
                 Process.put(:resolver_retained, :binary.copy(<<0>>, 33_554_432))
                 Process.sleep(:infinity)
               end
             )

    assert diagnostic.stage == :limits and diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.operation == :render and diagnostic.module == HtmlToPdf
    assert diagnostic.message =~ "max_operation_memory_bytes"
    assert_received {:resolver_memory_worker, worker}
    assert worker != caller
    monitor = Process.monitor(worker)
    assert_receive {:DOWN, ^monitor, :process, ^worker, _reason}
  end

  test "nested calls share the worker and its original deadline" do
    owner = self()

    assert {:ok, worker} =
             Operation.run(__MODULE__, :outer, fn ->
               outer = self()

               Operation.run(__MODULE__, :inner, fn ->
                 assert self() == outer
                 assert self() != owner
                 {:ok, self()}
               end)
             end)

    refute Process.alive?(worker)
    assert Process.get({Operation, :request}) == nil
  end

  test "observed memory includes retained off-heap binaries after execution" do
    Limits.install(Map.put(Limits.effective(), :max_operation_memory_bytes, 1_000_000))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Operation.run(__MODULE__, :binary_output, fn ->
               Process.flag(:max_heap_size, 0)
               {:ok, :binary.copy(<<1>>, 2_000_000)}
             end)

    assert diagnostic.operation == :binary_output
    assert diagnostic.message =~ "referenced binaries"
  end

  test "sampling terminates a worker retaining excess binary memory while blocked" do
    Limits.install(
      Map.merge(Limits.effective(), %{
        max_operation_memory_bytes: 1_000_000,
        operation_memory_check_interval_ms: 1
      })
    )

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Operation.run(__MODULE__, :blocked, fn ->
               Process.flag(:max_heap_size, 0)
               Process.put(:retained, :binary.copy(<<1>>, 2_000_000))
               Process.sleep(:infinity)
             end)

    assert diagnostic.message =~ "max_operation_memory_bytes"
  end

  test "worker callbacks inherit logger metadata while keeping process dictionaries isolated" do
    marker = make_ref()
    Logger.metadata(operation_test: marker)
    Process.put(:caller_setting, :original)

    assert {:ok, ^marker} =
             Operation.run(__MODULE__, :callback_context, fn ->
               assert Process.get(:caller_setting) == nil
               Process.put(:caller_setting, :worker_value)
               {:ok, Logger.metadata()[:operation_test]}
             end)

    assert Process.get(:caller_setting) == :original
  end

  test "multiple subbinaries count their shared backing allocation once" do
    Limits.install(Map.put(Limits.effective(), :max_operation_memory_bytes, 2_000_000))

    assert {:ok, 100} =
             Operation.run(__MODULE__, :shared_binaries, fn ->
               binary = :binary.copy(<<1>>, 1_000_000)
               slices = for index <- 1..100, do: binary_part(binary, index, 900_000)
               Process.put(:slices, slices)
               {:ok, length(slices)}
             end)
  end

  test "GC heap kills return an honest worker diagnostic" do
    assert {:error, {:operation_worker_failed, diagnostic}} =
             Operation.run(__MODULE__, :heap, fn ->
               Process.flag(:max_heap_size, %{
                 size: 1_000,
                 kill: true,
                 error_logger: false,
                 include_shared_binaries: true
               })

               Process.put(:heap, Enum.to_list(1..100_000))
               :erlang.garbage_collect()
             end)

    assert diagnostic.stage == :limits
    assert diagnostic.reason == :operation_worker_failed
    assert diagnostic.operation == :heap
    assert diagnostic.message =~ "GC-enforced"
    assert diagnostic.message =~ "external kill"
  end

  test "programmer errors throws and explicit exits retain their semantics" do
    assert_raise ArgumentError, "bad callback", fn ->
      Operation.run(__MODULE__, :exception, fn -> raise ArgumentError, "bad callback" end)
    end

    assert catch_throw(Operation.run(__MODULE__, :throw, fn -> throw(:caller_throw) end)) ==
             :caller_throw

    assert catch_exit(Operation.run(__MODULE__, :exit, fn -> exit(:caller_exit) end)) ==
             :caller_exit
  end

  test "caller death cancels its worker" do
    owner = self()

    caller =
      spawn(fn ->
        Operation.run(__MODULE__, :abandoned, fn ->
          send(owner, {:abandoned_worker, self()})
          Process.sleep(:infinity)
        end)
      end)

    assert_receive {:abandoned_worker, worker}
    monitor = Process.monitor(worker)
    Process.exit(caller, :kill)
    assert_receive {:DOWN, ^monitor, :process, ^worker, :killed}, 1_000
  end

  test "a completed result received after the deadline still fails" do
    Limits.install(Map.put(Limits.effective(), :max_operation_timeout_ms, 50))
    owner = self()

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Operation.run(__MODULE__, :late, fn ->
               worker = self()

               spawn(fn ->
                 :erlang.suspend_process(owner)
                 send(worker, :suspended)
                 Process.sleep(100)
                 :erlang.resume_process(owner)
               end)

               receive do
                 :suspended -> {:ok, :late}
               end
             end)

    assert diagnostic.message =~ "max_operation_timeout_ms"
  end

  test "memory checks tolerate a worker that already exited" do
    {worker, monitor} = spawn_monitor(fn -> :ok end)
    assert_receive {:DOWN, ^monitor, :process, ^worker, :normal}
    assert :ok = OperationValidator.memory(worker, OperationValidator.prepare(__MODULE__, :done))
  end

  test "operation settings accept overrides and reject nonpositive and noninteger values" do
    for key <- [
          :max_operation_timeout_ms,
          :max_operation_memory_bytes,
          :operation_memory_check_interval_ms
        ] do
      assert {:ok, limits} = LimitsValidator.validate([{key, 123_456}])
      assert limits[key] == 123_456

      for value <- [0, -1, :infinity, 1.5] do
        assert {:error, message} = LimitsValidator.validate([{key, value}])
        assert message =~ Atom.to_string(key)
      end
    end
  end
end
