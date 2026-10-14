defmodule NativeElixirPdfUtilities.HtmlToPdf.AtomicOuterWhitespaceBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity

  test "atomic content white-space retains the containing line's source wrapping" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/html_atomic_outer_whitespace.html",
          __DIR__
        ),
        render_opts: [page_size: {225, 225}, margin: 9, system_font_discovery: false],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end
end
