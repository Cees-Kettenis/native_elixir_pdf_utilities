defmodule NativeElixirPdfUtilities.HtmlToPdf.PositionedFragmentSourceBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :positioned_fragment_source
  for name <- [
        "html_positioned_fragment_source",
        "html_root_positioned_fragment_source",
        "html_positioned_widget_fragment_source"
      ] do
    @source_fixture Path.expand("../fixtures/html_to_pdf/browser_parity/#{name}.html", __DIR__)
    test "actual source fragmentation #{name}" do
      stats =
        PdfVisualCompare.assert_browser_match!(@source_fixture,
          native_renderer: &PdfVisualCompare.render_with_chromium_arial/2,
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count == 2
    end
  end
end
