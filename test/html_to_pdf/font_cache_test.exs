defmodule NativeElixirPdfUtilities.HtmlToPdf.FontCacheTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.FontCache

  test "fetch reuses successful loads while the source file is unchanged" do
    path = temporary_path("hit")
    File.write!(path, "font")
    {:ok, calls} = Agent.start_link(fn -> 0 end)

    loader = fn absolute_path ->
      Agent.update(calls, &(&1 + 1))
      {:ok, File.read!(absolute_path)}
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

    loader = fn absolute_path ->
      Agent.update(calls, &(&1 + 1))
      {:ok, File.read!(absolute_path)}
    end

    assert FontCache.fetch(path, loader) == {:ok, "first"}
    File.write!(path, "second version")
    assert FontCache.fetch(path, loader) == {:ok, "second version"}
    assert Agent.get(calls, & &1) == 2
  after
    File.rm(temporary_path("invalidation"))
  end

  test "fetch serializes concurrent misses for the same font" do
    path = temporary_path("concurrent")
    File.write!(path, "font")
    {:ok, calls} = Agent.start_link(fn -> 0 end)

    loader = fn absolute_path ->
      Agent.update(calls, &(&1 + 1))
      Process.sleep(25)
      {:ok, File.read!(absolute_path)}
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

    loader = fn _absolute_path ->
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

    loader = fn absolute_path ->
      Agent.update(calls, &(&1 + 1))
      {:ok, File.read!(absolute_path)}
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
      assert FontCache.fetch(path, fn absolute_path ->
               {:ok, File.read!(absolute_path)}
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

    assert FontCache.fetch(path, fn _absolute_path ->
             flunk("missing font loader must not run")
           end) == :error
  end

  defp temporary_path(label) do
    Path.join(System.tmp_dir!(), "native-elixir-pdf-font-cache-#{label}.ttf")
  end
end
