defmodule NativeElixirPdfUtilities.HtmlToPdf.RenderCacheTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.RenderCache

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
