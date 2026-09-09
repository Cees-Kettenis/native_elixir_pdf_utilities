defmodule NativeElixirPdfUtilities.HtmlToPdf.FontCacheTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.FontCache

  test "fetch reuses successful loads while the source file is unchanged" do
    path = temporary_path("hit")
    File.write!(path, "font")
    {:ok, calls} = Agent.start_link(fn -> 0 end)

    loader = fn data ->
      Agent.update(calls, &(&1 + 1))
      {:ok, data}
    end

    assert FontCache.fetch(path, loader) == {:ok, "font"}
    assert FontCache.fetch(path, loader) == {:ok, "font"}
    assert Agent.get(calls, & &1) == 1
  after
    File.rm(temporary_path("hit"))
  end

  test "fetch invalidates a cached entry when the source file changes" do
    path = temporary_path("invalidation")
    File.write!(path, "first")
    {:ok, calls} = Agent.start_link(fn -> 0 end)

    loader = fn data ->
      Agent.update(calls, &(&1 + 1))
      {:ok, data}
    end

    assert FontCache.fetch(path, loader) == {:ok, "first"}
    File.write!(path, "second version")
    assert FontCache.fetch(path, loader) == {:ok, "second version"}
    assert Agent.get(calls, & &1) == 2
  after
    File.rm(temporary_path("invalidation"))
  end

  test "fetch detects same-size overwrites with unchanged metadata" do
    path = temporary_path("same-size")
    timestamp = 1_700_000_000
    {:ok, calls} = Agent.start_link(fn -> 0 end)

    loader = fn data ->
      Agent.update(calls, &(&1 + 1))
      {:ok, data}
    end

    # Retry only if an attempt straddles a ctime second boundary.
    matched =
      Enum.any?(1..10, fn _attempt ->
        File.write!(path, "first")
        File.touch!(path, timestamp)
        before = File.stat!(path, time: :posix)
        assert FontCache.fetch(path, loader) == {:ok, "first"}
        File.write!(path, "other")
        File.touch!(path, timestamp)
        after_write = File.stat!(path, time: :posix)

        if Map.take(before, [:size, :mtime, :ctime, :inode]) ==
             Map.take(after_write, [:size, :mtime, :ctime, :inode]) do
          previous_calls = Agent.get(calls, & &1)
          assert FontCache.fetch(path, loader) == {:ok, "other"}
          assert FontCache.fetch(path, loader) == {:ok, "other"}
          assert Agent.get(calls, & &1) == previous_calls + 1
          true
        else
          false
        end
      end)

    assert matched, "could not exercise two writes in the same timestamp second"
  after
    File.rm(temporary_path("same-size"))
  end

  test "fetch fingerprints the same snapshot that the loader parses" do
    path = temporary_path("snapshot")
    File.write!(path, "first")
    {:ok, calls} = Agent.start_link(fn -> 0 end)

    loader = fn data ->
      Agent.update(calls, &(&1 + 1))
      File.write!(path, "other")
      {:ok, data}
    end

    assert FontCache.fetch(path, loader) == {:ok, "first"}
    assert FontCache.fetch(path, loader) == {:ok, "other"}
    assert FontCache.fetch(path, loader) == {:ok, "other"}
    assert Agent.get(calls, & &1) == 2
  after
    File.rm(temporary_path("snapshot"))
  end

  test "fetch serializes concurrent misses for the same font" do
    path = temporary_path("concurrent")
    File.write!(path, "font")
    {:ok, calls} = Agent.start_link(fn -> 0 end)

    loader = fn data ->
      Agent.update(calls, &(&1 + 1))
      Process.sleep(25)
      {:ok, data}
    end

    results =
      1..12
      |> Task.async_stream(
        fn _index -> FontCache.fetch(path, loader) end,
        max_concurrency: 12,
        ordered: false
      )
      |> Enum.to_list()

    assert Enum.all?(results, &(&1 == {:ok, {:ok, "font"}}))
    assert Agent.get(calls, & &1) == 1
  after
    File.rm(temporary_path("concurrent"))
  end

  test "fetch does not retain failed loads" do
    path = temporary_path("failed")
    File.write!(path, "not ready")
    {:ok, calls} = Agent.start_link(fn -> 0 end)

    loader = fn _data ->
      call = Agent.get_and_update(calls, &{&1, &1 + 1})
      if call == 0, do: :error, else: {:ok, :corrected}
    end

    assert FontCache.fetch(path, loader) == :error
    assert FontCache.fetch(path, loader) == {:ok, :corrected}
    assert FontCache.fetch(path, loader) == {:ok, :corrected}
    assert Agent.get(calls, & &1) == 2
  after
    File.rm(temporary_path("failed"))
  end

  test "fetch bounds retained font paths" do
    paths =
      Enum.map(1..65, fn index ->
        path = temporary_path("bounded-#{index}")
        File.write!(path, "font #{index}")
        path
      end)

    {:ok, calls} = Agent.start_link(fn -> 0 end)

    loader = fn data ->
      Agent.update(calls, &(&1 + 1))
      {:ok, data}
    end

    Enum.each(paths, &FontCache.fetch(&1, loader))
    assert FontCache.fetch(List.first(paths), loader) == {:ok, "font 1"}
    assert Agent.get(calls, & &1) == 66
  after
    Enum.each(1..65, &File.rm(temporary_path("bounded-#{&1}")))
  end

  test "fetch loads without caching when the library application is unavailable" do
    path = temporary_path("unstarted")
    File.write!(path, "font")

    assert :ok =
             Supervisor.terminate_child(
               NativeElixirPdfUtilities.Supervisor,
               FontCache
             )

    try do
      assert FontCache.fetch(path, fn data ->
               {:ok, data}
             end) == {:ok, "font"}
    after
      assert {:ok, _pid} =
               Supervisor.restart_child(
                 NativeElixirPdfUtilities.Supervisor,
                 FontCache
               )
    end
  after
    File.rm(temporary_path("unstarted"))
  end

  test "fetch returns an error before loading a missing path" do
    path = temporary_path("missing")

    refute File.exists?(path)

    assert FontCache.fetch(path, fn _data ->
             flunk("missing font loader must not run")
           end) == :error
  end

  defp temporary_path(label) do
    Path.join(System.tmp_dir!(), "native-elixir-pdf-font-cache-#{label}.ttf")
  end
end
