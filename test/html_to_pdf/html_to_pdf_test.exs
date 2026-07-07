defmodule NativeElixirPdfUtilities.HtmlToPdfTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf

  test "render converts a simple paragraph to a valid PDF binary" do
    assert {:ok, pdf} = HtmlToPdf.render("<p>Hello</p>")

    assert String.starts_with?(pdf, "%PDF-1.4")
    assert pdf =~ "/Type /Catalog"
    assert pdf =~ "/Type /Page"
    assert pdf =~ "(Hello) Tj"
    assert pdf =~ "xref"
    assert pdf =~ "trailer"
    assert String.ends_with?(pdf, "%%EOF\n")
  end

  test "render converts headings and inline styled text to PDF runs" do
    html =
      ~s(<h1 style="color: #336699">Title</h1><p>Hello <strong>bold</strong> <em style="color: blue">italic</em></p>)

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "(Title) Tj"
    assert pdf =~ "(Hello ) Tj"
    assert pdf =~ "(bold) Tj"
    assert pdf =~ "(italic) Tj"
    assert pdf =~ "/BaseFont /Helvetica-Bold"
    assert pdf =~ "/BaseFont /Helvetica-Oblique"
    assert pdf =~ "0.2 0.4 0.6 rg"
    assert pdf =~ "0 0 1 rg"
  end

  test "render_file writes a PDF for a supported paragraph" do
    input_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-html-to-pdf-test.html")
    output_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-html-to-pdf-test.pdf")

    File.write!(input_path, "<p>Hello</p>")

    assert HtmlToPdf.render_file(input_path, output_path) == :ok
    assert File.read!(output_path) =~ "(Hello) Tj"
    assert HtmlToPdf.render_file(input_path <> ".missing", output_path) == {:error, :enoent}
    assert HtmlToPdf.render_file(:bad_input, output_path) == {:error, :invalid_path}
  after
    input_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-html-to-pdf-test.html")
    output_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-html-to-pdf-test.pdf")

    File.rm(input_path)
    File.rm(output_path)
  end
end
