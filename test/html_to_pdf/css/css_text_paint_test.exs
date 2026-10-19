defmodule NativeElixirPdfUtilities.HtmlToPdf.CssTextPaintTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, Font, PdfWriter}
  alias NativeElixirPdfUtilities.Validators.WriterValidator
  alias NativeElixirPdfUtilities.{Limits, Text}

  setup do
    assert {:ok, registry} = Font.load_registry([])
    assert {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    box = %{
      type: :text,
      text: "Numbered line",
      x: 43.5,
      y: 157.5,
      width: 80,
      annotation_width: 80,
      line_height: 13.5,
      font: Font.pdf_name(font),
      font_face: font,
      font_size: 10.5,
      snap_to_css_pixel_grid: true,
      color: {0, 0, 0}
    }

    {:ok, box: box, font: font}
  end

  test "selected HTML font glyphs retain the CSS source print canvas", %{box: box} do
    paint = WriterValidator.prepare_text_paint(box, 180)
    assert paint.size == 14 and paint.x == 58 and paint.y == 30
    assert box.x == 43.5 and box.y == 157.5
    assert DrawingGeometry.css_print_canvas(180).height == 180
    assert_in_delta paint.x * paint.css_scale * paint.point_scale, box.x, 0.000005

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {210, 180}, boxes: [box]}], compress_streams: false)

    assert pdf =~ "q 0.23999999 0 0 -0.23999999 0 180 cm 3.125 0 0 3.125 0 0 cm BT"
    assert pdf =~ "/F1 14 Tf"
    assert pdf =~ "1 0 0 -1 58 30 Tm"
    assert {:ok, "Numbered line"} = Text.extract(pdf, layout: false)
  end

  test "source unit spacing preserves point advance and scoped clips and links", %{box: box} do
    box =
      Map.merge(box, %{
        letter_spacing: 2.25,
        link_url: "https://example.com/selected",
        clip: %{x: 50, y: 155, width: 40, height: 15}
      })

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {210, 180}, boxes: [box]}], compress_streams: false)

    assert pdf =~ "3 Tc"
    assert pdf =~ "50 155 40 15 re W n"
    assert pdf =~ "/Subtype /Link /Rect [50 157.5 90 170]"
    assert pdf =~ "/URI (https://example.com/selected)"
    assert {:ok, "Numbered line"} = Text.extract(pdf, layout: false)
  end

  test "translated viewports retain their local page frame and clipped global link region", %{
    box: box
  } do
    child = Map.merge(box, %{x: 5, y: 8, link_url: "https://example.com/child"})

    group = %{
      type: :drawing_group,
      width: 80,
      height: 60,
      transform: {1, 0, 0, 1, 20, 50},
      clip: %{x: 0, y: 0, width: 20, height: 20},
      boxes: [child]
    }

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {150, 150}, boxes: [group]}], compress_streams: false)

    assert pdf =~ "q 0.23999999 0 0 -0.23999999 0 60 cm"
    assert pdf =~ "/Subtype /Link /Rect [25 58.25 40 70]"
    assert pdf =~ "/URI (https://example.com/child)"
  end

  test "explicit selected glyphs use the source canvas while point API glyphs keep point coordinates",
       %{box: box, font: font} do
    glyph = %{box | text: "N", font_size: 20} |> Map.put(:glyph_ids, [Map.fetch!(font.cmap, ?N)])
    paint = WriterValidator.prepare_text_paint(glyph, 180)
    assert paint.size == 26.66 and paint.x == 58 and paint.y == 30
    assert DrawingGeometry.text_position(glyph, 180) == {43.5, 157.5}

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {210, 180}, boxes: [glyph]}], compress_streams: false)

    assert pdf =~ "q 0.23999999 0 0 -0.23999999 0 180 cm"
    assert pdf =~ "/F1 26.66 Tf"
    assert pdf =~ "1 0 0 -1 58 30 Tm"
    assert {:ok, "N"} = Text.extract(pdf, layout: false)

    point_glyph = %{glyph | snap_to_css_pixel_grid: false}
    assert WriterValidator.prepare_text_paint(point_glyph, 180) == nil
    assert DrawingGeometry.text_position(point_glyph, 180) == {43.5, 157.5}

    assert {:ok, point_pdf} =
             PdfWriter.render([%{size: {210, 180}, boxes: [point_glyph]}],
               compress_streams: false
             )

    refute point_pdf =~ "0.23999999"
    assert point_pdf =~ "/F1 20 Tf"
    assert point_pdf =~ "43.5 157.5 Td"
    assert {:ok, "N"} = Text.extract(point_pdf, layout: false)
  end

  test "source coordinate conversion honors the canonical writer numeric budget", %{box: box} do
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    Limits.install(%{original | max_pdf_numeric_magnitude: 10000})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             PdfWriter.render([%{size: {210, 180}, boxes: [%{box | x: 9000}]}])

    assert diagnostic.stage == :writer and diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.operation == :write_pdf
    assert diagnostic.message =~ "max_pdf_numeric_magnitude"
    assert diagnostic.source =~ "CSS source text"
  end
end
