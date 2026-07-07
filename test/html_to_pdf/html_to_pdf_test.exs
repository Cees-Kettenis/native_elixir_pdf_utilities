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

  test "render applies configured and embedded CSS before writing PDF output" do
    html =
      ~s(<style>p.notice { color: #336699; font-weight: bold; }</style><p class="notice">Styled</p>)

    assert {:ok, pdf} = HtmlToPdf.render(html, stylesheets: ["p { color: red; }"])
    assert pdf =~ "(Styled) Tj"
    assert pdf =~ "/BaseFont /Helvetica-Bold"
    assert pdf =~ "0.2 0.4 0.6 rg"
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

  test "render paginates overflowing content into multiple PDF pages" do
    rows =
      1..3
      |> Enum.map(fn index ->
        "<tr><td>Alpha #{index}</td><td>#{index}</td></tr>"
      end)
      |> Enum.join()

    html =
      "<table><thead><tr><th>Name</th><th>Count</th></tr></thead><tbody>" <>
        rows <> "</tbody></table>"

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {200, 100}, margin: 10)
    assert pdf =~ "/Type /Pages"
    assert pdf =~ "/Count 2"
    assert length(String.split(pdf, "(Name) Tj")) == 3
    assert pdf =~ "(Alpha 3) Tj"
  end

  test "render converts a flex layout subset to PDF text boxes" do
    html =
      ~s(<div style="display: flex; width: 80pt; gap: 8pt"><span style="order: 2">Second</span><span style="order: 1">First</span></div>)

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "(First) Tj"
    assert pdf =~ "(Second) Tj"
  end

  test "render converts a grid layout subset to PDF text boxes" do
    html =
      ~s(<div style="display: grid; width: 80pt; grid-template-columns: 30pt 30pt; gap: 8pt"><span style="grid-column: 2 / 3">Second</span><span style="grid-column: 1 / 2">First</span></div>)

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "(First) Tj"
    assert pdf =~ "(Second) Tj"
  end

  test "render converts a PNG data URI image to a PDF image object" do
    src = "data:image/png;base64,#{Base.encode64(png_fixture(2, 1))}"
    html = ~s(<img src="#{src}" style="width: 20pt">)

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "/Subtype /Image"
    assert pdf =~ "/Width 2"
    assert pdf =~ "/Height 1"
    assert pdf =~ "/ColorSpace /DeviceRGB"
    assert pdf =~ "/Filter /FlateDecode"
    assert pdf =~ "/Im1 Do"
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

  defp png_fixture(width, height) do
    row = :binary.copy(<<255, 0, 0>>, width)
    rows = Enum.map_join(1..height, "", fn _index -> <<0>> <> row end)

    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<width::32, height::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("IDAT", :zlib.compress(rows)) <>
      png_chunk("IEND", "")
  end

  defp png_chunk(type, data) do
    crc = :erlang.crc32(type <> data)
    <<byte_size(data)::32, type::binary, data::binary, crc::32>>
  end
end
