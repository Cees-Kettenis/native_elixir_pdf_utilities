defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgGradientModesBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Text}
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :svg_converter_contract
  @moduletag :svg_gradient_modes

  test "repeat reflect linearRGB and varying alpha paint actual source shapes and selected glyphs" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/inline_svg_gradient_modes.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    pdf = File.read!(Path.join(stats.artifact_dir, "native.pdf"))
    assert :ok = PdfVisualCompare.assert_svg_image_without_text!(pdf)

    assert {:ok, text} =
             Text.extract(File.read!(Path.join(stats.artifact_dir, "chromium.pdf")),
               layout: false
             )

    assert text =~ "Modes ffi café Ω"
    assert :ok = PdfVisualCompare.assert_svg_image_artwork!(pdf)
    refute pdf =~ "3 Tr"
  end

  test "converted filter inputs interpolate alpha independently and radial repeat keeps its real rings" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/inline_svg_gradient_raster.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    pdf = File.read!(Path.join(stats.artifact_dir, "native.pdf"))
    assert :ok = PdfVisualCompare.assert_svg_image_artwork!(pdf)
  end

  test "sRGB repeat and reflect retain the original negative periods and selected font artwork" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/inline_svg_gradient_spread.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    pdf = File.read!(Path.join(stats.artifact_dir, "native.pdf"))
    assert :ok = PdfVisualCompare.assert_svg_image_without_text!(pdf)

    assert {:ok, text} =
             Text.extract(File.read!(Path.join(stats.artifact_dir, "chromium.pdf")),
               layout: false
             )

    assert text =~ "Modes ffi café Ω"
    assert :ok = PdfVisualCompare.assert_svg_image_artwork!(pdf)
    refute pdf =~ "3 Tr"
  end

  test "linearRGB filter input and sRGB radial repeat compare with the actual browser printer" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/inline_svg_gradient_filter_interpolation.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    pdf = File.read!(Path.join(stats.artifact_dir, "native.pdf"))
    assert :ok = PdfVisualCompare.assert_svg_image_artwork!(pdf)
  end
end
