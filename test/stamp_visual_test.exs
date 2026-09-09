defmodule NativeElixirPdfUtilities.StampVisualTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
  alias NativeElixirPdfUtilities.Stamp
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  @moduletag :browser_parity

  test "overlay opacity applies to composed artwork even when source alpha is explicit" do
    alias NativeElixirPdfUtilities.Pdf.{Reader, IncrementalWriter}
    assert {:ok, blank} = PdfWriter.render([%{size: {300.0, 200.0}, boxes: []}])
    assert {:ok, context} = Reader.read_validated(blank)
    page = hd(context.pages)
    {page_id, generation} = page.ref
    stream_id = context.document.trailer["Size"]

    directory =
      Path.join(System.tmp_dir!(), "stamp-opacity-#{System.unique_integer([:positive])}")

    File.mkdir_p!(directory)
    on_exit(fn -> File.rm_rf!(directory) end)

    for source_alpha <- [1.0, 0.5],
        group <- [nil, %{"S" => {:name, "Transparency"}, "I" => true}],
        opacity <- [0.0, 0.25, 1.0] do
      dictionary =
        Map.merge(page.dictionary, %{
          "Contents" => {:ref, {stream_id, 0}},
          "Resources" => %{
            "ExtGState" => %{
              "OwnAlpha" => %{
                "Type" => {:name, "ExtGState"},
                "ca" => source_alpha,
                "CA" => source_alpha
              }
            }
          }
        })

      dictionary = if group, do: Map.put(dictionary, "Group", group), else: dictionary

      assert {:ok, source} =
               IncrementalWriter.write(context, [
                 {stream_id, 0, {:stream, %{}, "/OwnAlpha gs 1 0 0 rg 10 10 50 50 re f"}},
                 {page_id, generation, {:value, dictionary}}
               ])

      assert {:ok, stamped} = Stamp.overlay(blank, source, opacity: opacity)
      pdf_path = Path.join(directory, "stamped.pdf")
      prefix = Path.join(directory, "stamped")
      File.write!(pdf_path, stamped)

      assert {_output, 0} =
               System.cmd("pdftoppm", ["-r", "72", "-singlefile", pdf_path, prefix],
                 stderr_to_stdout: true
               )

      ["P6", "300 200", "255", pixels] =
        String.split(File.read!(prefix <> ".ppm"), "\n", parts: 4)

      <<red, green, blue>> = binary_part(pixels, (150 * 300 + 20) * 3, 3)
      expected = round(255 * (1 - opacity * source_alpha))
      assert red == 255
      assert_in_delta green, expected, 1
      assert_in_delta blue, expected, 1
    end
  end

  test "isolates existing transforms and clips while preserving artwork and repeated stamps" do
    assert {:ok, blank} = PdfWriter.render([%{size: {300.0, 200.0}, boxes: []}])
    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(blank)
    page = hd(context.pages)
    {page_id, generation} = page.ref
    stream_id = context.document.trailer["Size"]

    for {state, index} <-
          Enum.with_index([
            "",
            "1 0 0 1 100 0 cm",
            "0 0 1 1 re W n",
            "2 0 0 2 20 30 cm 0 0 1 1 re W n"
          ]) do
      content = "0 0 0 rg 220 150 10 10 re f\n" <> state

      assert {:ok, target} =
               NativeElixirPdfUtilities.Pdf.IncrementalWriter.write(context, [
                 {stream_id, 0, {:stream, %{}, content}},
                 {page_id, generation,
                  {:value, Map.put(page.dictionary, "Contents", [{:ref, {stream_id, 0}}])}}
               ])

      assert {:ok, stamped} = Stamp.text(target, "MARK", position: {10, 20}, size: 12)

      assert [stats] =
               PdfVisualCompare.assert_pdf_visual_change!(target, stamped,
                 artifact_dir: "tmp/browser_parity/stamp-state-#{index}"
               )

      assert stats.changed_pixels > 50
      {min_x, min_y, max_x, max_y} = stats.bounds
      assert min_x in 10..15
      assert max_x < 50
      assert min_y in 20..30
      assert max_y < 35

      assert {:ok, repeated} = Stamp.text(stamped, "NEXT", position: {80, 20}, size: 12)

      assert [repeated_stats] =
               PdfVisualCompare.assert_pdf_visual_change!(stamped, repeated,
                 artifact_dir: "tmp/browser_parity/stamp-state-repeat-#{index}"
               )

      assert repeated_stats.changed_pixels > 50
      {repeat_x, repeat_y, repeat_right, repeat_bottom} = repeated_stats.bounds
      assert repeat_x in 80..85
      assert repeat_right < 120
      assert repeat_y in 20..30
      assert repeat_bottom < 35
    end
  end

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
