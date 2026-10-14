defmodule NativeElixirPdfUtilities.HtmlToPdf.ControlSemanticsBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity

  test "visible datalist fallback and closed initial suggestions match untouched Chromium" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/html_datalist_visible_closed.html",
          __DIR__
        ),
        artifact_dir: "tmp/browser_parity/html_datalist_visible_closed",
        render_opts: [forms: :static],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end
end
