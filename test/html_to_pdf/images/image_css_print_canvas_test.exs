defmodule NativeElixirPdfUtilities.HtmlToPdf.ImageCssPrintCanvasTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, PdfWriter, PngDecoder}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, WriterValidator}

  @png Base.decode64!(
         "iVBORw0KGgoAAAANSUhEUgAAABAAAAAICAIAAAB/FOjAAAAAGElEQVR4nGM4oSGHFYkELMCKGEY10EIDAJ7zhIFJAvtOAAAAAElFTkSuQmCC"
       )

  test "CSS PNG edges use the source top-down canvas on a fractional CSS page" do
    assert {:ok, image} = PngDecoder.decode(@png, HtmlValidator.new_image_budget(), false)

    box = %{
      type: :image,
      x: 6.0,
      y: 137.0,
      width: 24.0,
      height: 12.0,
      image: image,
      snap_to_css_pixel_grid: true
    }

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {120, 155}, boxes: [box]}], compress_streams: false)

    # Authored top 6pt is an exact CSS edge even though 155pt is not a whole CSS pixel.
    assert pdf =~ "0.23999999 0 0 -0.23999999 0 155 cm 25 25 100 50 re W n 100 0 0 -50 25 75 cm"
    refute pdf =~ "23.9999"
    assert pdf =~ "/Subtype /Image"
  end

  test "the source print transform is conditional on an CSS image box" do
    box = %{type: :image, x: 6.0, y: 137.0, width: 24.0, height: 12.0}
    assert WriterValidator.prepare_image_paint(box, 155) == nil
    canvas = WriterValidator.prepare_image_paint(Map.put(box, :snap_to_css_pixel_grid, true), 155)
    assert canvas.point_scale == DrawingGeometry.css_print_canvas(155).point_scale
    assert canvas.transform == [100.0, 0, 0, -50.0, 25.0, 75.0]
    assert canvas.clip == %{x: 25.0, y: 25.0, width: 100.0, height: 50.0}
    assert canvas.height == 155
  end

  test "a CSS source transform exceeding the configured PDF magnitude returns diagnostics" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(Map.put(limits, :max_pdf_numeric_magnitude, 20))
    assert {:ok, image} = PngDecoder.decode(@png, HtmlValidator.new_image_budget(), false)

    box = %{
      type: :image,
      x: 0,
      y: 0,
      width: 10,
      height: 10,
      image: image,
      snap_to_css_pixel_grid: true
    }

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             PdfWriter.render([%{size: {12, 12}, boxes: [box]}], [])

    assert diagnostic.stage == :writer and diagnostic.operation == :write_pdf
    assert diagnostic.source == "CSS image source print coordinates"
    assert diagnostic.message =~ "max_pdf_numeric_magnitude"
  end
end
