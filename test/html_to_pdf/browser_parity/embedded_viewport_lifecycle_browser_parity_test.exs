alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.EmbeddedViewportLifecycleBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :embedded_viewport_lifecycle
  @fixtures Paths.fixture("html_to_pdf/browser_parity")

  for name <- ~w(html_inline_blank_viewports html_embedded_viewport_reflow) do
    @name name
    test "original source viewport lifecycle matches Chromium for #{@name}" do
      path = Path.join(@fixtures, @name <> ".html")

      stats =
        PdfVisualCompare.with_embedded_font_configuration(path, fn ->
          PdfVisualCompare.assert_browser_match!(path,
            render_opts: [fonts: []],
            native_renderer: fn html, opts ->
              PdfVisualCompare.render_embedded_font_bytes(html, opts, path)
            end,
            max_changed_ratio: 0.01,
            max_average_delta: 0.01
          )
        end)

      assert stats.page_count == 1
      refute File.exists?(Path.join(stats.artifact_dir, "chromium-input.html"))
    end
  end

  test "original automatic scrollbar source retains its bundled font URL initialization" do
    path = Path.join(@fixtures, "html_iframe_scrollbars.html")
    # The existing original fixture has a font-root placeholder. Resolve only
    # that resource URL for both engines, using the original bundled font.
    initialize = fn html ->
      String.replace(html, "__FONT_ROOT__", Path.expand("priv/fonts/dejavu"))
    end

    stats =
      PdfVisualCompare.assert_browser_match!(path,
        chromium_transform: initialize,
        native_renderer: fn html, opts ->
          PdfVisualCompare.render_embedded_font_bytes(
            initialize.(html),
            Keyword.put(opts, :base_url, Path.expand(".")),
            path
          )
        end,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
