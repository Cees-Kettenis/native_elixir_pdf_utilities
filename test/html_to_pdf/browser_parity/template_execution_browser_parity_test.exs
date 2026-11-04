alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.TemplateExecutionBrowserParityTest do
  use ExUnit.Case, async: true
  @moduletag :browser_parity
  test "original parser-time earlier range replacement matches Chromium" do
    fixture =
      Paths.fixture("html_to_pdf/template_replacement/earlier_range.html")

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir:
          "tmp/test_artifacts/template_execution_browser_parity/template-replacement-original-browser",
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_with_chromium_arial/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end

  test "original successive ranges execute in parser order" do
    fixture =
      Paths.fixture("html_to_pdf/template_replacement/successive_ranges.html")

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir:
          "tmp/test_artifacts/template_execution_browser_parity/template-successive-original-browser",
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_with_chromium_arial/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end

  test "original inert CSS and prospective styles both validate before visible insertion" do
    fixture = Paths.fixture("html_to_pdf/template_replacement/inert_css.html")

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir:
          "tmp/test_artifacts/template_execution_browser_parity/template-inert-css-original-browser",
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_with_chromium_arial/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end

  test "original unmatched and inactive literals validate before parser-time replacement" do
    fixture =
      Paths.fixture("html_to_pdf/template_replacement/unmatched_literal_css.html")

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir:
          "tmp/test_artifacts/template_execution_browser_parity/template-unmatched-literal-original-browser",
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_with_chromium_arial/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end

  test "original scoped relative and percentage declarations execute with selected MathML metrics" do
    fixture =
      Paths.fixture("html_to_pdf/template_replacement/source_runtime_context.html")

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir:
          "tmp/test_artifacts/template_execution_browser_parity/template-original-runtime-context-browser",
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_with_chromium_arial/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end

  test "original MathML glyph owners complete before parser replacement" do
    fixture =
      Paths.fixture("html_to_pdf/template_replacement/math_glyph_source.html")

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir:
          "tmp/test_artifacts/template_execution_browser_parity/template-math-glyph-source-browser",
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_with_chromium_arial/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end

  test "unapplied symbolic CSS and variable fallback match Chromium print selection" do
    fixture =
      Paths.fixture("html_to_pdf/template_replacement/source_css_variables.html")

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir:
          "tmp/test_artifacts/template_execution_browser_parity/template-unapplied-symbolic-browser",
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_with_chromium_arial/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end

  test "original composed Math and SVG glyph owners complete before parser replacement" do
    fixture =
      Paths.fixture("html_to_pdf/template_replacement/composed_glyph_sources.html")

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir:
          "tmp/test_artifacts/template_execution_browser_parity/template-composed-glyph-source-browser",
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_with_chromium_arial/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end
end
