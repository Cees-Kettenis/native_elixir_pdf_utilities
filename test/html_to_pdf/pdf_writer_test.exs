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

  test "render rejects invalid page data" do
    assert PdfWriter.render([], []) == {:error, :invalid_pdf_input}
  end
end
