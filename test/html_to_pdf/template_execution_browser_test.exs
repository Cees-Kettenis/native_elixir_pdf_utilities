defmodule NativeElixirPdfUtilities.HtmlToPdf.TemplateExecutionBrowserTest do
  use ExUnit.Case, async: true
  @moduletag :browser_parity
  test "original parser-time earlier range replacement matches Chromium" do
    fixture =
      Path.expand("../fixtures/html_to_pdf/template_replacement/earlier_range.html", __DIR__)

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: ".quality/orchestration/template-replacement-original-browser",
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_with_chromium_arial/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end

  test "original successive ranges execute in actual parser order" do
    fixture =
      Path.expand("../fixtures/html_to_pdf/template_replacement/successive_ranges.html", __DIR__)

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: ".quality/orchestration/template-successive-original-browser",
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_with_chromium_arial/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end

  test "original inert CSS and prospective styles both validate before visible insertion" do
    fixture = Path.expand("../fixtures/html_to_pdf/template_replacement/inert_css.html", __DIR__)

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: ".quality/orchestration/template-inert-css-original-browser",
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_with_chromium_arial/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end

  test "original unmatched and inactive literals validate before parser-time replacement" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/template_replacement/unmatched_literal_css.html",
        __DIR__
      )

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: ".quality/orchestration/template-unmatched-literal-original-browser",
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_with_chromium_arial/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end

  test "original scoped relative and percentage declarations execute with genuine selected MathML metrics" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/template_replacement/original_runtime_context.html",
        __DIR__
      )

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: ".quality/orchestration/template-original-runtime-context-browser",
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_with_chromium_arial/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end

  test "original MathML glyph owners complete before genuine parser replacement" do
    fixture =
      Path.expand("../fixtures/html_to_pdf/template_replacement/math_glyph_source.html", __DIR__)

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: ".quality/orchestration/template-math-glyph-source-browser",
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_with_chromium_arial/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end

  test "unapplied symbolic CSS and actual variable fallback match Chromium print selection" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/template_replacement/unapplied_symbolic_css.html",
        __DIR__
      )

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: ".quality/orchestration/template-unapplied-symbolic-browser",
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_with_chromium_arial/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end

  test "original composed Math and SVG glyph owners complete before parser replacement" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/template_replacement/composed_glyph_sources.html",
        __DIR__
      )

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: ".quality/orchestration/template-composed-glyph-source-browser",
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_with_chromium_arial/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end
end
