defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgPaintEdgesBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :svg_converter_contract
  @moduletag :svg_paint_edges

  test "converted SVG paint-server fills and original solid glyph strokes retain Chromium edges" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/inline_svg_text_paint_edges.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    pdf = File.read!(Path.join(stats.artifact_dir, "native.pdf"))

    assert {:ok, "Original selected glyphs with continuous paint extents"} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, text} =
             NativeElixirPdfUtilities.Text.extract(
               File.read!(Path.join(stats.artifact_dir, "chromium.pdf")),
               layout: false
             )

    for original <- [
          "Source stroke office Ω",
          "Tile stroke café Ω",
          "Real bands office Ω",
          "Alpha tiles café Ω"
        ],
        do: assert(text =~ original)

    assert :ok = PdfVisualCompare.assert_svg_image_artwork!(pdf)
    refute pdf =~ "3 Tr"
  end
end
