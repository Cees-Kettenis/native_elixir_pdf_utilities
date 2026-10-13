defmodule NativeElixirPdfUtilities.HtmlToPdf.TemplateExecutionBrowserTest do
  use ExUnit.Case, async: false
  @moduletag :browser_parity
  test "original parser-time earlier range replacement matches Chromium" do
    fixture =
      Path.expand("../fixtures/html_to_pdf/template_replacement/earlier_range.html", __DIR__)

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: ".quality/orchestration/template-replacement-original-browser",
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_initial_numbers/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end
end
