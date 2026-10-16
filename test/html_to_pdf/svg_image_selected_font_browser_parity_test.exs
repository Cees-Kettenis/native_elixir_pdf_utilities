defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgImageSelectedFontBrowserParityTest do
  use ExUnit.Case, async: false
  @moduletag :browser_parity
  @moduletag :selected_image_font_transport
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  test "SVG image text uses the same supplied font bytes in its isolated context" do
    fixture =
      Path.expand("test/fixtures/html_to_pdf/browser_parity/svg_image_selected_host_font.html")

    # Image documents cannot see the host @font-face rules. Make the exact
    # caller fonts available to Chrome's isolated image through Fontconfig.
    PdfVisualCompare.with_embedded_font_configuration(fixture, fn ->
      PdfVisualCompare.assert_browser_match!(fixture,
        render_opts: [system_font_discovery: false],
        native_renderer: fn html, opts ->
          PdfVisualCompare.render_embedded_font_bytes(html, opts, fixture)
        end,
        svg_reference: :png,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )
    end)
  end
end
