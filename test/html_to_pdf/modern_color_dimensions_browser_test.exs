defmodule NativeElixirPdfUtilities.HtmlToPdf.ModernColorDimensionsBrowserTest do
  use ExUnit.Case, async: true
  @moduletag :browser_parity

  @tag :modern_color_inputs
  test "dimensional color calculations use the original Chromium initial-value policy" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/html_modern_color_dimensional_calculations.html",
        __DIR__
      )

    fonts =
      for {family, filename} <- [
            {"DejaVu Sans", "DejaVuSans.ttf"},
            {"DejaVu Sans Mono", "DejaVuSansMono.ttf"}
          ],
          do: %{
            family: family,
            path: Path.expand("priv/fonts/dejavu/" <> filename),
            weight: 400,
            style: :normal
          }

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: ".quality/orchestration/modern-color-dimensions-browser",
        render_opts: [forms: :static, fonts: fonts, system_font_discovery: false],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end
end
