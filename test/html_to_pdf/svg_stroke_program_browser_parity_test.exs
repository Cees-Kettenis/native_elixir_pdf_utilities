defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgStrokeProgramBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :svg_stroke_program
  test "unchanged original selected-font stroke fixture retains genuine glyph painting" do
    fixture =
      Path.expand("../fixtures/html_to_pdf/browser_parity/inline_svg_text_strokes.html", __DIR__)

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1

    native = File.read!(Path.join(stats.artifact_dir, "native.pdf"))
    assert :ok = PdfVisualCompare.assert_svg_image_without_text!(native)

    assert {:ok, text} =
             NativeElixirPdfUtilities.Text.extract(
               File.read!(Path.join(stats.artifact_dir, "chromium.pdf")),
               layout: false
             )

    assert text =~ "Stroke"
  end
end
