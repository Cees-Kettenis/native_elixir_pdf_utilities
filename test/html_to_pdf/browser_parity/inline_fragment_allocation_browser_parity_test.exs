alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.InlineFragmentAllocationBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :inline_fragment_allocation
  @fixtures Paths.fixture("html_to_pdf/browser_parity")

  test "final styled fragments retain Chromium's occupied width and line alignment" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Path.join(@fixtures, "html_inline_fragment_allocation.html"),
        render_opts: [
          fonts: [
            %{
              family: "Fixture Sans",
              path: Paths.project("priv/fonts/dejavu/DejaVuSans.ttf")
            }
          ],
          system_font_discovery: false
        ],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
