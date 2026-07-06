defmodule NativeElixirPdfUtilities.HtmlToPdf.PdfWriterTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter

  test "render exposes the PDF writer boundary while behavior is pending" do
    assert PdfWriter.render([], []) == {:error, :not_implemented}
  end
end
