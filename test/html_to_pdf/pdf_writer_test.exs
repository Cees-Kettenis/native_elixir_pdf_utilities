defmodule NativeElixirPdfUtilities.HtmlToPdf.PdfWriterTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter

  test "render writes a valid PDF for a text page" do
    pages = [
      %{
        size: {595.28, 841.89},
        boxes: [
          %{
            type: :text,
            text: "Hello",
            x: 56.69,
            y: 773.2,
            font: "Helvetica",
            font_size: 12.0,
            color: {0, 0, 0}
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert String.starts_with?(pdf, "%PDF-1.4")
    assert pdf =~ "/MediaBox [0 0 595.28 841.89]"
    assert pdf =~ "BT /F1 12 Tf 0 0 0 rg 56.69 773.2 Td (Hello) Tj ET"
    assert pdf =~ "startxref"
  end

  test "render escapes PDF text literals" do
    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :text,
            text: "A (B) \\ C",
            x: 10.0,
            y: 20.0,
            font: "Helvetica",
            font_size: 12.0,
            color: {0, 0, 0}
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert pdf =~ "(A \\(B\\) \\\\ C) Tj"
  end

  test "render writes font resources for bold italic and colored runs" do
    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :text,
            text: "Bold",
            x: 10.0,
            y: 80.0,
            font: "Helvetica-Bold",
            font_size: 12.0,
            color: {1, 0, 0}
          },
          %{
            type: :text,
            text: "Italic",
            x: 40.0,
            y: 80.0,
            font: "Helvetica-Oblique",
            font_size: 12.0,
            color: {0, 0, 1}
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert pdf =~ "/BaseFont /Helvetica-Bold"
    assert pdf =~ "/BaseFont /Helvetica-Oblique"
    assert pdf =~ "BT /F1 12 Tf 1 0 0 rg 10 80 Td (Bold) Tj ET"
    assert pdf =~ "BT /F2 12 Tf 0 0 1 rg 40 80 Td (Italic) Tj ET"
  end

  test "render writes filled and stroked rectangle boxes" do
    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :rect,
            x: 10.0,
            y: 20.0,
            width: 40.0,
            height: 30.0,
            fill_color: {0.9, 0.9, 0.9},
            stroke_color: {1, 0, 0},
            stroke_width: 2.0,
            border_radius: 0.0
          },
          %{
            type: :text,
            text: "Boxed",
            x: 15.0,
            y: 35.0,
            font: "Helvetica",
            font_size: 12.0,
            color: {0, 0, 0}
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert pdf =~ "q 0.9 0.9 0.9 rg 1 0 0 RG 2 w 10 20 40 30 re B Q"
    assert pdf =~ "BT /F1 12 Tf 0 0 0 rg 15 35 Td (Boxed) Tj ET"
  end

  test "render writes rounded rectangle paths when radius is set" do
    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :rect,
            x: 10.0,
            y: 20.0,
            width: 40.0,
            height: 30.0,
            fill_color: nil,
            stroke_color: {0, 0, 1},
            stroke_width: 1.0,
            border_radius: 4.0
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert pdf =~ "0 0 1 RG 1 w"
    assert pdf =~ "14 20 m"
    assert pdf =~ "c"
    assert pdf =~ " h S Q"
  end

  test "render writes URI link annotations for linked text boxes" do
    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :text,
            text: "Docs",
            x: 10.0,
            y: 20.0,
            width: 60.0,
            annotation_width: 28.8,
            line_height: 14.4,
            font: "Helvetica",
            font_size: 12.0,
            color: {0, 0, 1},
            link_url: "https://example.com"
          }
        ]
      }
    ]

    assert {:ok, pdf} = PdfWriter.render(pages, [])
    assert pdf =~ "/Annots [6 0 R]"
    assert pdf =~ "/Subtype /Link"
    assert pdf =~ "/Rect [10 20 38.8 34.4]"
    assert pdf =~ "/A << /S /URI /URI (https://example.com) >>"
  end

  test "render rejects unsupported link annotations" do
    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :text,
            text: "Bad",
            x: 10.0,
            y: 20.0,
            width: 60.0,
            annotation_width: 21.6,
            font: "Helvetica",
            font_size: 12.0,
            color: {0, 0, 1},
            link_url: "javascript:alert(1)"
          }
        ]
      }
    ]

    assert PdfWriter.render(pages, []) == {:error, :invalid_pdf_input}
  end

  test "render rejects invalid page data" do
    assert PdfWriter.render([], []) == {:error, :invalid_pdf_input}
  end
end
