alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.ModernColorDimensionsBrowserParityTest do
  use ExUnit.Case, async: true
  @moduletag :browser_parity

  @tag :modern_color_inputs
  test "dimensional color calculations use the original Chromium initial-value policy" do
    fixture =
      Paths.fixture("html_to_pdf/browser_parity/html_modern_color_dimensional_calculations.html")

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
        artifact_dir:
          "tmp/test_artifacts/modern_color_dimensions_browser_parity/modern-color-dimensions-browser",
        render_opts: [forms: :static, fonts: fonts, system_font_discovery: false],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end
end
