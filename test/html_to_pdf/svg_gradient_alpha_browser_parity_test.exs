defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgGradientAlphaBrowserParityTest do
  use ExUnit.Case, async: true
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

    # SVG 2 permits different font hinting/rasterization choices under text-rendering:auto.
    # Both engines can conform while drawing different glyph pixels:
    # https://www.w3.org/TR/SVG2/painting.html#TextRenderingProperty
    # User-approved 2% changed-pixel ceiling; retain the 1% mean-delta limit.
    # Evidence: tmp/gradient-alpha-root-cause/root-cause.json; original strict result 1.3173%.
    # Gradient-only controls differ by at most 2/255; all larger differences are in text.
    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        svg_reference: :png,
        max_changed_ratio: 0.02,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    pdf = File.read!(Path.join(stats.artifact_dir, "native.pdf"))
    assert :ok = PdfVisualCompare.assert_svg_image_without_text!(pdf)

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
        svg_reference: :png,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    pdf = File.read!(Path.join(stats.artifact_dir, "native.pdf"))
    assert :ok = PdfVisualCompare.assert_svg_image_without_text!(pdf)

    assert :ok = PdfVisualCompare.assert_svg_image_artwork!(pdf)
    refute pdf =~ "3 Tr"
  end
end
