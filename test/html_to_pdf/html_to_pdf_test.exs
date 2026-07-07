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
