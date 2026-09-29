defmodule NativeElixirPdfUtilities.HtmlToPdf.RenderCacheTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.RenderCache

  test "stylesheet scopes share nested work and release snapshots after failure" do
    assert RenderCache.fetch_stylesheet(:css, fn -> :uncached end) == :uncached

    RenderCache.with_stylesheets(fn ->
      assert RenderCache.fetch_stylesheet(:css, fn -> :snapshot end) == :snapshot

      RenderCache.with_stylesheets(fn ->
        assert RenderCache.fetch_stylesheet(:css, fn -> flunk("nested cache miss") end) ==
                 :snapshot
      end)
    end)

    assert_raise RuntimeError, "failed", fn ->
      RenderCache.with_stylesheets(fn ->
        assert RenderCache.fetch_stylesheet(:css, fn -> :new_snapshot end) == :new_snapshot
        raise "failed"
      end)
    end

    assert RenderCache.fetch_stylesheet(:css, fn -> :after_failure end) == :after_failure
  end

  @tag :tmp_dir
  test "stylesheet loading and styling use one file snapshot per scope", %{tmp_dir: directory} do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
    path = Path.join(directory, "snapshot.css")
    File.write!(path, "p { color:red }")
    {:ok, dom} = HtmlParser.parse("<p>Snapshot</p>")
    opts = [stylesheets: [{:file, path}], default_font: "Helvetica"]

    RenderCache.with_stylesheets(fn ->
      assert {:ok, [%{css: "p { color:red }"}]} = Style.load_stylesheets(dom, opts)
      File.write!(path, "p { color:blue }")

      assert {:ok, %{children: [%{style: %{color: {1, 0, 0}}}]}} =
               Style.compute_detailed(dom, opts)
    end)

    assert {:ok, %{children: [%{style: %{color: {0, 0, 1}}}]}} =
             Style.compute_detailed(dom, opts)
  end

  test "cache entries are local to an invocation and released on success or failure" do
    parent = self()

    cache =
      RenderCache.run(fn cache ->
        assert RenderCache.fetch(cache, :key, fn -> :first end) == :first
        assert RenderCache.fetch(cache, :key, fn -> flunk("cache miss") end) == :first

        RenderCache.run(fn nested ->
          assert RenderCache.fetch(nested, :key, fn -> :nested end) == :nested
        end)

        assert RenderCache.fetch(cache, :key, fn -> flunk("lost outer cache") end) == :first
        cache
      end)

    assert Process.get(cache) == nil

    assert_raise RuntimeError, "loader failed", fn ->
      RenderCache.run(fn cache ->
        send(parent, {:cache, cache})
        RenderCache.fetch(cache, :key, fn -> raise "loader failed" end)
      end)
    end

    assert_received {:cache, failed_cache}
    assert Process.get(failed_cache) == nil
  end
end
