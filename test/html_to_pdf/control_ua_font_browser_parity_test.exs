defmodule NativeElixirPdfUtilities.HtmlToPdf.ControlUaFontBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @tag :control_ua_fonts
  test "ordinary UA font defaults and genuine authored ancestor font inheritance match Chromium" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Path.expand("../fixtures/html_to_pdf/browser_parity/html_control_ua_fonts.html", __DIR__),
        artifact_dir: ".quality/orchestration/control-ua-font-browser",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1 and stats.scripting == :disabled
  end
end
