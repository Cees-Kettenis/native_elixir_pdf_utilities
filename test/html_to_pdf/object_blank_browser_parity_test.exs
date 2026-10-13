defmodule NativeElixirPdfUtilities.HtmlToPdf.ObjectBlankBrowserParityTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  @moduletag :browser_parity
  @moduletag :object_blank_initialization
  @fixture Path.expand(
             "../fixtures/html_to_pdf/browser_parity/html_object_blank_initialization.html",
             __DIR__
           )

  test "original object MIME and blank-source initialization matches Chromium" do
    stats =
      PdfVisualCompare.assert_browser_match!(@fixture,
        render_opts: [fonts: []],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    refute File.exists?(Path.join(stats.artifact_dir, "chromium-input.html"))
  end
end
