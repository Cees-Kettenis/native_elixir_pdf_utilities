alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.InlineAutoWidthBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @fixtures Paths.fixture("html_to_pdf/browser_parity")

  for fixture <- ["css_inline_auto_width_clamp.html"] do
    @fixture fixture
    test "inline containing width matches Chromium for #{@fixture}" do
      stats =
        PdfVisualCompare.assert_browser_match!(Path.join(@fixtures, @fixture),
          artifact_dir: "tmp/browser_parity/" <> Path.rootname(@fixture),
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count == 1
      assert stats.scripting == :disabled
    end
  end
end
