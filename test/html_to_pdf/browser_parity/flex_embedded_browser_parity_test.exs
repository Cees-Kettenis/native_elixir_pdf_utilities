alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.FlexEmbeddedBrowserParityTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  @moduletag :browser_parity
  @moduletag :flex_embedded_surfaces
  @fixtures Paths.fixture("html_to_pdf/browser_parity")

  for fixture <- [
        "html_flex_embedded_surfaces.html",
        "html_embedded_intrinsic_css_auto.html",
        "html_embedded_inline_css_auto.html"
      ] do
    @fixture fixture
    test "original embedded document viewport matches Chromium for #{@fixture}" do
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
      assert stats.embedded_fonts.chromium =~ "DejaVuSans"
      assert stats.embedded_fonts.native =~ "DejaVuSans"
      # Resolve the original relative child URLs in place. The native side uses
      # bytes from the same bundled font, without authorizing unrelated paths.
      refute File.exists?(Path.join(stats.artifact_dir, "chromium-input.html"))
    end
  end
end
