defmodule NativeElixirPdfUtilities.HtmlToPdf.ContainerControlsBrowserParityTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}

  @moduletag :browser_parity

  test "contained blocks, container sizing, zoom and static controls match Chromium" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/css_container_controls.html"),
        render_opts: [forms: :static, fonts: []],
        regions: [after_wrapped_textarea: {14, 406, 18, 3}],
        max_changed_ratio: 0.004,
        max_average_delta: 0.003
      )

    assert stats.page_count == 1
  end
end
