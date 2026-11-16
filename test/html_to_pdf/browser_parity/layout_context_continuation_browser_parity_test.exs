defmodule NativeElixirPdfUtilities.HtmlToPdf.LayoutContextContinuationBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  @moduletag :browser_parity

  test "parallel and nested fragmentainers preserve whole lines and following content" do
    # Pagination needs its original document and page boundaries. The fixed grid
    # sheet cannot represent these eleven pages. One print serves all contexts,
    # both fill modes and entry after preceding page content.
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/layout_context_continuation.html"),
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 11

    for engine <- ["native", "chromium"] do
      {:ok, text} =
        NativeElixirPdfUtilities.Text.extract(
          File.read!(Path.join(stats.artifact_dir, engine <> ".pdf"))
        )

      for caption <- [
            "FlexLeft10",
            "FlexRight10",
            "After flex",
            "GridLeft10",
            "GridRight10",
            "After grid",
            "Column24",
            "After columns",
            "Auto24",
            "After auto",
            "Entry12",
            "After entry"
          ],
          do: assert(text =~ caption)
    end
  end
end
