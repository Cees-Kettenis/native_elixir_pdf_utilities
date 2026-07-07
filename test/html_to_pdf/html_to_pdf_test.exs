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

  test "render converts block box styling to PDF drawing commands" do
    html =
      ~s(<p style="margin: 2pt; padding: 3pt; border: 1pt solid red; border-radius: 2pt; background-color: #eeeeee">Boxed</p>)

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "0.9333 0.9333 0.9333 rg"
    assert pdf =~ "1 0 0 RG 1 w"
    assert pdf =~ "(Boxed) Tj"
  end

  test "render converts lists and links to PDF text and annotations" do
    html =
      ~s(<ul><li>Read <a href="https://example.com">docs</a></li><li>Ship</li></ul>)

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "(*) Tj"
    assert pdf =~ "(Read ) Tj"
    assert pdf =~ "(docs) Tj"
    assert pdf =~ "(Ship) Tj"
    assert pdf =~ "/Subtype /Link"
    assert pdf =~ "/URI (https://example.com)"

    assert HtmlToPdf.render(~s[<p><a href="javascript:alert(1)">bad</a></p>]) ==
             {:error, :invalid_document}
  end

  test "render converts tables to PDF text boxes and cell borders" do
    html =
      ~s(<table><caption>Summary</caption><thead><tr><th>Name</th><th>Docs</th></tr></thead><tbody><tr><td>Alpha</td><td><a href="https://example.com">Link</a></td></tr></tbody></table>)

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "(Summary) Tj"
    assert pdf =~ "(Name) Tj"
    assert pdf =~ "(Docs) Tj"
    assert pdf =~ "(Alpha) Tj"
    assert pdf =~ "(Link) Tj"
    assert pdf =~ "/BaseFont /Helvetica-Bold"
    assert pdf =~ "0.9333 0.9333 0.9333 rg"
    assert pdf =~ "0 0 0 RG 1 w"
    assert pdf =~ "/Subtype /Link"
    assert pdf =~ "/URI (https://example.com)"
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
