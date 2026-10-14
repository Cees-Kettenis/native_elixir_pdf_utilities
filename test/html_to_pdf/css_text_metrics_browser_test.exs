defmodule NativeElixirPdfUtilities.HtmlToPdf.CssTextMetricsBrowserTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  test "preserved spaces retain original CJK and hyphen line opportunities with configured fonts" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/css_text_preserved_opportunities.html",
        __DIR__
      )

    font =
      Path.expand("../fixtures/html_to_pdf/browser_parity/assets/cjk-ruby-fixture.ttf", __DIR__)

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: ".quality/orchestration/css-preserved-opportunities-browser",
        render_opts: [
          fonts: [
            %{family: "CJK Ruby Fixture", path: font},
            %{
              family: "DejaVu Sans",
              path: Path.expand("../../priv/fonts/dejavu/DejaVuSans.ttf", __DIR__)
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

  test "hyphen context remains intact when an overflowing word crosses spans" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/css_text_styled_hyphen_context.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: ".quality/orchestration/css-styled-hyphen-browser",
        render_opts: [system_font_discovery: false],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    assert stats.scripting == :disabled
  end
end
