alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlRecoveryBrowserParityTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  @moduletag :browser_parity

  for fixture <- ~w(html_recovery_unclosed_elements.html html_recovery_trailing_slash.html) do
    @fixture fixture
    test "opt-in recovery matches Chromium for #{@fixture}" do
      path = Paths.fixture("html_to_pdf/browser_parity/" <> @fixture)

      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(File.read!(path))
      assert diagnostic.stage == :html

      stats =
        PdfVisualCompare.assert_browser_match!(path,
          render_opts: [html_recovery: true],
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count == 1
    end
  end
end
