defmodule NativeElixirPdfUtilities.HtmlToPdf.ListCounterBrowserParityTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}

  @moduletag :browser_parity

  test "list ownership and explicit counter declarations match Chromium" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/list_counter_ownership.html"),
        artifact_dir: "tmp/browser_parity/list_counter_ownership",
        max_changed_ratio: 0.01,
        max_average_delta: 0.035
      )

    assert stats.page_count == 1 and stats.scripting == :disabled
  end
end
