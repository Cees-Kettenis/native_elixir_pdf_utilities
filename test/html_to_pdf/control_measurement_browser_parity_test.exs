defmodule NativeElixirPdfUtilities.HtmlToPdf.ControlMeasurementBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  test "authored inline initial select boxes and fields match Chromium" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/html_inline_select_controls.html",
          __DIR__
        ),
        artifact_dir: "tmp/browser_parity/html_inline_select_controls",
        render_opts: [forms: :interactive],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled
  end
end
