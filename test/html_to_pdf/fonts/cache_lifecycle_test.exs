defmodule NativeElixirPdfUtilities.HtmlToPdf.CacheLifecycleTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{Limits, Operation}
  alias NativeElixirPdfUtilities.HtmlToPdf.{FontCache, SystemFontCache}
  alias NativeElixirPdfUtilities.TestSupport.Paths

  setup do
    path = Paths.project("tmp/cache-lifecycle-#{System.unique_integer([:positive])}")
    File.write!(path, "font")
    File.write!(path <> ".other", "other")
    limits = Limits.effective()

    on_exit(fn ->
      File.rm(path)
      File.rm(path <> ".other")
      Limits.install(limits)
    end)

    %{path: path}
  end

  for cache <- [FontCache, SystemFontCache] do
    @cache cache

    test "#{cache} cancels loading with the caller and allows unrelated misses", %{path: path} do
      cache = @cache
      owner = self()

      fetch = fn loader ->
        fetch(cache, path, loader)
      end

      {caller, monitor} =
        spawn_monitor(fn ->
          fetch.(fn ->
            send(owner, {:loading, self()})

            receive do
              :release -> {:ok, :abandoned}
            end
          end)
        end)

      on_exit(fn -> Process.exit(caller, :kill) end)
      assert_receive {:loading, loader}
      assert loader == caller

      unrelated =
        Task.async(fn ->
          fetch(cache, path <> ".other", fn -> {:ok, :independent} end)
        end)

      assert Task.await(unrelated) == {:ok, :independent}

      Process.exit(caller, :kill)
      assert_receive {:DOWN, ^monitor, :process, ^caller, :killed}
      refute Process.alive?(loader)
      assert fetch.(fn -> {:ok, :replacement} end) == {:ok, :replacement}
      assert fetch.(fn -> flunk("replacement must be retained") end) == {:ok, :replacement}
    end

    test "#{cache} releases failed loads without crashing the shared cache", %{path: path} do
      cache = @cache
      server = Process.whereis(cache)

      fetch = fn loader ->
        fetch(cache, path, loader)
      end

      assert_raise RuntimeError, "loader failed", fn ->
        fetch.(fn -> raise "loader failed" end)
      end

      assert Process.whereis(cache) == server
      assert Process.alive?(server)
      assert fetch.(fn -> {:ok, :recovered} end) == {:ok, :recovered}
    end

    test "#{cache} loading obeys the operation deadline", %{path: path} do
      cache = @cache
      owner = self()
      Limits.install(%{Limits.effective() | max_operation_timeout_ms: 1_000})

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               Operation.run(cache, :fetch, fn ->
                 loader = fn ->
                   send(owner, {:loading, self()})
                   Process.sleep(:infinity)
                 end

                 fetch(cache, path, loader)
               end)

      assert diagnostic.stage == :limits
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.operation == :fetch
      assert diagnostic.message =~ "max_operation_timeout_ms"
      assert_receive {:loading, loader}
      monitor = Process.monitor(loader)
      assert_receive {:DOWN, ^monitor, :process, ^loader, _}
      assert Process.alive?(Process.whereis(cache))
    end
  end

  defp fetch(cache, path, loader) do
    case cache do
      FontCache -> FontCache.fetch(path, fn _ -> loader.() end)
      SystemFontCache -> SystemFontCache.fetch(path, loader)
    end
  end
end
