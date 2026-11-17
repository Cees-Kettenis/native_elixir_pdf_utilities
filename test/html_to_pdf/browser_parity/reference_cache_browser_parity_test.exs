defmodule NativeElixirPdfUtilities.HtmlToPdf.ReferenceCacheBrowserParityTest do
  use ExUnit.Case, async: false
  @moduletag :browser_parity
  alias NativeElixirPdfUtilities.TestSupport.{BrowserReferenceCache, Paths, PdfVisualCompare}

  test "cold and warm references preserve pixels, bounds, native execution and failing assertions" do
    previous = System.get_env() |> Map.take(["BROWSER_PARITY_CACHE", "BROWSER_PARITY_TOOLCHAIN"])
    root = Paths.project("tmp/browser_parity/reference-cache-contract")
    BrowserReferenceCache.stop()
    File.rm_rf!(root)
    System.put_env("BROWSER_PARITY_CACHE", Path.join(root, "cache"))

    System.put_env(
      "BROWSER_PARITY_TOOLCHAIN",
      System.get_env("BROWSER_PARITY_TOOLCHAIN", "contract-test")
    )

    {:ok, cache} = BrowserReferenceCache.start()
    # on_exit owns teardown after this test process has exited.
    Process.unlink(cache)

    on_exit(fn ->
      BrowserReferenceCache.stop()
      for key <- ["BROWSER_PARITY_CACHE", "BROWSER_PARITY_TOOLCHAIN"], do: System.delete_env(key)
      System.put_env(previous)
      {:ok, cache} = BrowserReferenceCache.start()
      # The suite owns the restored coordinator after this cleanup worker exits.
      Process.unlink(cache)
    end)

    fixture = Paths.fixture("html_to_pdf/browser_parity/absolute_positioning.html")
    # This verifies persistent capture itself, so bypass the existing page group.
    # Exactly one fresh capture owns both cold/warm comparisons below.
    cold = PdfVisualCompare.prepare_browser_reference!(fixture)
    warm = PdfVisualCompare.prepare_browser_reference!(fixture)
    assert cold.cache_status == :miss
    assert warm.cache_status == :hit
    assert cold.identity == warm.identity
    assert Enum.map(cold.pages, &File.read!/1) == Enum.map(warm.pages, &File.read!/1)
    assert File.read!(cold.chromium_pdf) == File.read!(warm.chromium_pdf)

    assert {:ok, cold_info} =
             NativeElixirPdfUtilities.Info.page_sizes(File.read!(cold.chromium_pdf))

    assert {:ok, ^cold_info} =
             NativeElixirPdfUtilities.Info.page_sizes(File.read!(warm.chromium_pdf))

    assert {:ok, cold_spans} =
             NativeElixirPdfUtilities.Text.extract_spans(File.read!(cold.chromium_pdf))

    assert {:ok, ^cold_spans} =
             NativeElixirPdfUtilities.Text.extract_spans(File.read!(warm.chromium_pdf))

    owner = self()

    renderer = fn html, options ->
      send(owner, :native_rendered)
      NativeElixirPdfUtilities.HtmlToPdf.render(html, options)
    end

    stats =
      for {name, reference} <- [{"cold", cold}, {"warm", warm}] do
        stats =
          PdfVisualCompare.assert_browser_match!(fixture,
            reference: reference,
            native_renderer: renderer,
            artifact_dir: Path.join(root, name),
            max_changed_ratio: 0.01,
            max_average_delta: 0.007
          )

        assert_received :native_rendered
        stats
      end

    assert Enum.at(stats, 0).pages == Enum.at(stats, 1).pages

    assert_raise ExUnit.AssertionError, fn ->
      PdfVisualCompare.assert_browser_match!(fixture,
        reference: warm,
        native_renderer: fn _, _ -> {:error, :deliberately_failed_native_render} end,
        artifact_dir: Path.join(root, "native-failure")
      )
    end

    assert_raise ExUnit.AssertionError, fn ->
      PdfVisualCompare.assert_browser_match!(fixture,
        reference: warm,
        native_renderer: fn _, options ->
          NativeElixirPdfUtilities.HtmlToPdf.render("<p>Different artwork</p>", options)
        end,
        artifact_dir: Path.join(root, "different-artwork"),
        max_changed_ratio: 0.0,
        max_average_delta: 0.0
      )
    end
  end
end
