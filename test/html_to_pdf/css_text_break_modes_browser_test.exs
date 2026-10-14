defmodule NativeElixirPdfUtilities.HtmlToPdf.CssTextBreakModesBrowserTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  test "ordinary and emergency wrapping keep atomic min-content widths" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/css_text_atomic_intrinsics.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: ".quality/orchestration/atomic-intrinsics-browser",
        render_opts: [system_font_discovery: false],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end

  test "preserved newlines and break-spaces retain their intrinsic widths" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/css_text_preserved_intrinsics.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: ".quality/orchestration/preserved-intrinsics-browser",
        render_opts: [system_font_discovery: false],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end

  test "numeric atomic border-box sizing keeps requested and clamped outer dimensions" do
    fixture =
      Path.expand("../fixtures/html_to_pdf/browser_parity/css_text_atomic_geometry.html", __DIR__)

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: ".quality/orchestration/atomic-geometry-browser",
        render_opts: [system_font_discovery: false],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
