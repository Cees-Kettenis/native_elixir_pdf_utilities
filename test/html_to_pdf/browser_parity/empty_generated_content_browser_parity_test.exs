defmodule NativeElixirPdfUtilities.HtmlToPdf.EmptyGeneratedContentBrowserParityTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}

  @moduletag :browser_parity

  test "empty generated boxes preserve paint and layout against Chromium" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/empty_generated_content.html"),
        artifact_dir: "tmp/browser_parity/empty_generated_content",
        max_changed_ratio: 0.01,
        max_average_delta: 0.035
      )

    assert stats.page_count == 1 and stats.scripting == :disabled
  end
end
