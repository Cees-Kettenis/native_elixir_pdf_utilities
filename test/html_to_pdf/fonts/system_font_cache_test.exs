defmodule NativeElixirPdfUtilities.HtmlToPdf.SystemFontCacheTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.SystemFontCache

  test "retains positive and negative discovery results" do
    {:ok, calls} = Agent.start_link(fn -> 0 end)
    key = {:retained, make_ref()}

    loader = fn ->
      Agent.update(calls, &(&1 + 1))
      :error
    end

    assert SystemFontCache.fetch(key, loader) == :error
    assert SystemFontCache.fetch(key, loader) == :error
    assert Agent.get(calls, & &1) == 1
  end

  test "an oversized discovery result stays with its caller and prunes retained entries" do
    alias NativeElixirPdfUtilities.Limits
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    key = {:oversized_discovery, make_ref()}
    retained = {:retained_discovery, make_ref()}
    assert :found = SystemFontCache.fetch(retained, fn -> :found end)

    Limits.install(%{limits | max_cache_bytes: 1})
    result = {:ok, List.duplicate("family", 32)}
    server = Process.whereis(SystemFontCache)
    :erlang.trace(server, true, [:receive])

    try do
      assert ^result = SystemFontCache.fetch(key, fn -> result end)
      assert_receive {:trace, ^server, :receive, {:"$gen_call", _, :prune}}
      refute_received {:trace, ^server, :receive, {:"$gen_call", _, {:store, _, _}}}
      assert :ets.lookup(SystemFontCache, key) == []
      assert :ets.lookup(SystemFontCache, retained) == []
    after
      :erlang.trace(server, false, [:receive])
    end
  end

  test "serializes concurrent misses and bounds retained lookups" do
    {:ok, calls} = Agent.start_link(fn -> 0 end)
    concurrent_key = {:concurrent, make_ref()}

    results =
      1..8
      |> Task.async_stream(
        fn _index ->
          SystemFontCache.fetch(concurrent_key, fn ->
            Agent.update(calls, &(&1 + 1))
            Process.sleep(20)
            :found
          end)
        end,
        max_concurrency: 8,
        ordered: false
      )
      |> Enum.to_list()

    assert Enum.all?(results, &(&1 == {:ok, :found}))
    assert Agent.get(calls, & &1) == 1

    keys = Enum.map(1..65, &{:bounded, make_ref(), &1})
    Enum.each(keys, &SystemFontCache.fetch(&1, fn -> &1 end))
    first = List.first(keys)
    assert SystemFontCache.fetch(first, fn -> :reloaded end) == :reloaded
  end

  test "loads directly while the application child is unavailable" do
    assert :ok =
             Supervisor.terminate_child(
               NativeElixirPdfUtilities.Supervisor,
               SystemFontCache
             )

    try do
      assert SystemFontCache.fetch({:unstarted, make_ref()}, fn -> :direct end) == :direct
    after
      assert {:ok, _pid} =
               Supervisor.restart_child(
                 NativeElixirPdfUtilities.Supervisor,
                 SystemFontCache
               )
    end
  end
end
