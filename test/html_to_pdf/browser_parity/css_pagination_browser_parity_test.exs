alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.CssPaginationBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :css_affine_pagination
  @fixtures_dir Paths.fixture("html_to_pdf/browser_parity")
  for name <-
        ~w(html_css_affine_positioned_tracks) do
    @fixture name
    test "authored CSS pagination source #{name}" do
      stats =
        PdfVisualCompare.assert_browser_match!(Path.join(@fixtures_dir, @fixture <> ".html"),
          artifact_dir:
            Path.join("tmp/test_artifacts/css_pagination_browser_parity/final-browser", @fixture),
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count == 1
    end
  end
end
