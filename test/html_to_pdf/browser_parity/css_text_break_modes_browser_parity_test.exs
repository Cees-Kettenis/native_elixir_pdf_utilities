alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.CssTextBreakModesBrowserParityTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  test "ordinary and emergency wrapping keep atomic min-content widths" do
    fixture =
      Paths.fixture("html_to_pdf/browser_parity/css_text_atomic_intrinsics.html")

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir:
          "tmp/test_artifacts/css_text_break_modes_browser_parity/atomic-intrinsics-browser",
        render_opts: [system_font_discovery: false],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end

  test "preserved newlines and break-spaces retain their intrinsic widths" do
    fixture =
      Paths.fixture("html_to_pdf/browser_parity/css_text_preserved_intrinsics.html")

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir:
          "tmp/test_artifacts/css_text_break_modes_browser_parity/preserved-intrinsics-browser",
        render_opts: [system_font_discovery: false],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end

  test "numeric atomic border-box sizing keeps requested and clamped outer dimensions" do
    fixture =
      Paths.fixture("html_to_pdf/browser_parity/css_text_atomic_geometry.html")

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir:
          "tmp/test_artifacts/css_text_break_modes_browser_parity/atomic-geometry-browser",
        render_opts: [system_font_discovery: false],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
