defmodule NativeElixirPdfUtilities.HtmlToPdf.TemplateOriginalFontBrowserTest do
  use ExUnit.Case, async: true
  @moduletag :browser_parity

  test "discarded inactive font loading preserves Chromium's active selection" do
    fixture =
      Path.expand("../fixtures/html_to_pdf/template_replacement/inactive_font.html", __DIR__)

    font =
      Path.join(:code.priv_dir(:native_elixir_pdf_utilities), "fonts/dejavu/DejaVuSansMono.ttf")

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: ".quality/orchestration/template-inactive-original-font-browser",
        render_opts: [assets: %{"provided-original.ttf" => {:file, font}}],
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_with_chromium_arial/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
    refute stats.embedded_fonts.native =~ "OriginalInactive"
  end
end
