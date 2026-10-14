defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgImageSelectedFontBrowserParityTest do
  use ExUnit.Case, async: false
  @moduletag :browser_parity
  @moduletag :selected_image_font_transport
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  test "SVG image text uses its real isolated Chromium fallback source" do
    fixture =
      Path.expand("test/fixtures/html_to_pdf/browser_parity/svg_image_selected_host_font.html")

    # Chromium's isolated image cannot see the host webfont; its original PDF
    # selects this actual physical serif face. Supply those bytes explicitly.
    font = "/usr/share/fonts/liberation/LiberationSerif-Regular.ttf"
    assert File.regular?(font)

    assert Base.encode16(:crypto.hash(:sha256, File.read!(font)), case: :lower) ==
             "86b9ea1c2f41bed9d7c09ccad4abc2894b33df5de60e5bbbece5d48610911870"

    PdfVisualCompare.assert_browser_match!(fixture,
      render_opts: [system_font_discovery: false],
      native_renderer: fn html, opts ->
        NativeElixirPdfUtilities.HtmlToPdf.render(
          html,
          Keyword.put(opts, :fonts, [%{family: "Liberation Serif", data: File.read!(font)}])
        )
      end,
      max_changed_ratio: 0.01,
      max_average_delta: 0.01
    )
  end
end
