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

  test "render rejects invalid page data" do
    assert PdfWriter.render([], []) == {:error, :invalid_pdf_input}
  end
end
