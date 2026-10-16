defmodule NativeElixirPdfUtilities.HtmlToPdf.CssDiscretionaryBreakBrowserTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  test "an atomic inline boundary does not select a preceding discretionary hyphen" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/css_discretionary_atomic_boundary.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/css_discretionary_atomic_boundary",
        render_opts: [system_font_discovery: false],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end

  test "soft hyphens paint only at selected breaks and formatting characters do not add spacing" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/css_discretionary_hyphens.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: ".quality/orchestration/discretionary-browser",
        render_opts: [system_font_discovery: false],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled
    assert stats.page_count == 2
  end

  test "identical formatting-only spans preserve selected Latin ligatures and kerning" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/css_discretionary_shaping.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: ".quality/orchestration/discretionary-shaping-browser",
        render_opts: [system_font_discovery: false],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled
    assert stats.page_count == 1
  end
end
