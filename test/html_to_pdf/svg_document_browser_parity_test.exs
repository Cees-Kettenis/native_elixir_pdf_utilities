defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgDocumentBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :svg_document_viewport
  @fixtures Path.expand("../fixtures/html_to_pdf/browser_parity", __DIR__)

  for fixture <- [
        "html_svg_document_viewports.html",
        "html_svg_document_intrinsic_standalone.html",
        "html_svg_document_intrinsic.html"
      ] do
    @fixture fixture
    if fixture == "html_svg_document_intrinsic.html", do: @tag(:svg_flex_dependency)
    if fixture != "html_svg_document_intrinsic.html", do: @tag(:svg_document_supported)

    test "original selected SVG document viewport matches Chromium for #{@fixture}" do
      stats =
        PdfVisualCompare.assert_browser_match!(Path.join(@fixtures, @fixture),
          render_opts: [fonts: []],
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count == 1
      # No font or source transforms relocate either original document. Both
      # renderers resolve the same authored relative child paths in place.
      refute File.exists?(Path.join(stats.artifact_dir, "chromium-input.html"))
    end
  end
end
