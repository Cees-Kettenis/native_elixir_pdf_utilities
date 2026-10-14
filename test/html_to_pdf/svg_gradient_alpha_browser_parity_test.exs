defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgGradientAlphaBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Text}
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :svg_converter_contract
  @moduletag :svg_gradient_alpha

  test "independent linear and radial alpha shade actual selected glyphs and source geometry" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/inline_svg_gradient_alpha.html",
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

    assert text =~ "Alpha ffi café Ω"
    assert text =~ "Radial original Ωé"
    assert :ok = PdfVisualCompare.assert_svg_image_artwork!(pdf)
    refute pdf =~ "3 Tr"
  end

  test "varying alpha inside nested source masks preserves the parent mask intersection" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/inline_svg_gradient_alpha_masks.html",
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

    assert text =~ "Real mask Ω"
    assert :ok = PdfVisualCompare.assert_svg_image_artwork!(pdf)
    refute pdf =~ "3 Tr"
  end
end
