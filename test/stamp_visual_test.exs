defmodule NativeElixirPdfUtilities.StampVisualTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
  alias NativeElixirPdfUtilities.Stamp
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  @moduletag :browser_parity

  test "places a diagonal watermark across the page center" do
    assert {:ok, pdf} = PdfWriter.render([%{size: {300.0, 200.0}, boxes: []}])

    assert {:ok, watermarked} =
             Stamp.watermark(pdf, "DRAFT", color: {0.8, 0.0, 0.0}, opacity: 0.4)

    assert [stats] =
             PdfVisualCompare.assert_pdf_visual_change!(pdf, watermarked,
               artifact_dir: "tmp/browser_parity/stamp-watermark"
             )

    assert stats.width == 300
    assert stats.height == 200
    assert stats.changed_ratio > 0.005
    assert stats.changed_ratio < 0.15

    {min_x, min_y, max_x, max_y} = stats.bounds
    assert min_x in 20..130
    assert min_y in 15..100
    assert max_x in 170..280
    assert max_y in 100..185
  end

  test "places an anchored text stamp at the top right" do
    assert {:ok, pdf} = PdfWriter.render([%{size: {300.0, 200.0}, boxes: []}])

    assert {:ok, stamped} =
             Stamp.text(pdf, "APPROVED",
               position: :top_right,
               margin: 12,
               size: 18,
               color: {0.0, 0.2, 0.8}
             )

    assert [stats] =
             PdfVisualCompare.assert_pdf_visual_change!(pdf, stamped,
               artifact_dir: "tmp/browser_parity/stamp-text"
             )

    {min_x, min_y, max_x, max_y} = stats.bounds
    assert min_x > 150
    assert min_y < 40
    assert max_x < 295
    assert max_y < 60
  end
end
