defmodule NativeElixirPdfUtilities.HtmlToPdf.FlexEmbeddedBrowserParityTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  @moduletag :browser_parity
  @moduletag :flex_embedded_surfaces
  @fixtures Path.expand("../fixtures/html_to_pdf/browser_parity", __DIR__)

  setup_all do
    font_dir = Path.expand("../../priv/fonts/dejavu", __DIR__)
    directory = Path.expand("tmp/flex-embedded-fontconfig")
    File.mkdir_p!(directory)
    configuration = Path.join(directory, "fonts.conf")

    File.write!(configuration, """
    <?xml version="1.0"?>
    <!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">
    <fontconfig><include>/etc/fonts/fonts.conf</include><dir>#{font_dir}</dir><cachedir>#{directory}/cache</cachedir></fontconfig>
    """)

    previous = System.get_env("FONTCONFIG_FILE")
    System.put_env("FONTCONFIG_FILE", configuration)

    on_exit(fn ->
      if previous,
        do: System.put_env("FONTCONFIG_FILE", previous),
        else: System.delete_env("FONTCONFIG_FILE")
    end)

    :ok
  end

  for fixture <- [
        "html_flex_embedded_surfaces.html",
        "html_embedded_intrinsic_css_auto.html",
        "html_embedded_inline_css_auto.html"
      ] do
    @fixture fixture
    test "original embedded document viewport matches Chromium for #{@fixture}" do
      path = Path.join(@fixtures, @fixture)

      stats =
        PdfVisualCompare.assert_browser_match!(path,
          render_opts: [fonts: []],
          native_renderer: fn html, opts ->
            PdfVisualCompare.render_embedded_font_bytes(html, opts, path)
          end,
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count == 1
      assert stats.embedded_fonts.chromium =~ "DejaVuSans"
      assert stats.embedded_fonts.native =~ "DejaVuSans"
      # Resolve the original relative child URLs in place. The native side uses
      # bytes from the same bundled font, without authorizing unrelated paths.
      refute File.exists?(Path.join(stats.artifact_dir, "chromium-input.html"))
    end
  end
end
