alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.TableSurfaceLayerBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :table_layered_surface
  @fixtures Paths.fixture("html_to_pdf/browser_parity")
  for fixture <- [
        "html_table_atomic_layered_backgrounds.html",
        "html_table_viewport_layered_backgrounds.html"
      ] do
    @fixture fixture
    test "original unfragmented cell layer ownership matches Chromium for #{@fixture}" do
      path = Path.join(@fixtures, @fixture)

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
    end
  end
end
