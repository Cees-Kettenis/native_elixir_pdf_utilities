defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgPaintServerBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Text}
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :svg_converter_contract
  @moduletag :svg_type3_paint_server

  test "original glyph strokes compose full-span gradients alpha and transformed patterns" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/inline_svg_type3_paint_servers.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    pdf = File.read!(Path.join(stats.artifact_dir, "native.pdf"))

    assert {:ok, "Original selected glyph strokes share actual scene paint"} =
             Text.extract(pdf, layout: false)

    assert {:ok, text} =
             Text.extract(File.read!(Path.join(stats.artifact_dir, "chromium.pdf")),
               layout: false
             )

    for original <- [
          "Source stroke office Ω",
          "Radial café Ω",
          "Alpha stroke ffi Ω",
          "Pattern stroke café Ω"
        ],
        do: assert(text =~ original)

    assert :ok = PdfVisualCompare.assert_svg_image_artwork!(pdf)
    refute pdf =~ "3 Tr"
  end

  test "independently rotated scaled and masked original glyph samples retain browser placement" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/inline_svg_type3_sample_coordinates.html",
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
