alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.ControlUaFontBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @tag :control_ua_fonts
  test "ordinary UA font defaults and authored ancestor font inheritance match Chromium" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/html_control_ua_fonts.html"),
        artifact_dir: "tmp/test_artifacts/control_ua_font_browser_parity/control-ua-font-browser",
        native_renderer: &PdfVisualCompare.render_with_chromium_arial/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1 and stats.scripting == :disabled
  end
end
