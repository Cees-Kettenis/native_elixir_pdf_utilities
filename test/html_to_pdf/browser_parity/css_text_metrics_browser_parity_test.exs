alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.CssTextMetricsBrowserParityTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  test "preserved spaces retain original CJK and hyphen line opportunities with configured fonts" do
    fixture =
      Paths.fixture("html_to_pdf/browser_parity/css_text_preserved_opportunities.html")

    font =
      Paths.fixture("html_to_pdf/browser_parity/assets/cjk-ruby-fixture.ttf")

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir:
          "tmp/test_artifacts/css_text_metrics_browser_parity/css-preserved-opportunities-browser",
        render_opts: [
          fonts: [
            %{family: "CJK Ruby Fixture", path: font},
            %{
              family: "DejaVu Sans",
              path: Paths.project("priv/fonts/dejavu/DejaVuSans.ttf")
            }
          ],
          system_font_discovery: false
        ],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 2
    assert stats.scripting == :disabled
  end
end
