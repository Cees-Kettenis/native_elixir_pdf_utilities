defmodule NativeElixirPdfUtilities.HtmlToPdfTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.Text

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

  test "render uses the HTML title as PDF metadata unless explicitly overridden" do
    html = "<html><head><title>  HTML Report  </title></head><body><p>Hello</p></body></html>"

    assert {:ok, default_pdf} = HtmlToPdf.render(html)
    assert default_pdf =~ "/Title (HTML Report)"
    assert default_pdf =~ "/Info "

    assert {:ok, explicit_pdf} =
             HtmlToPdf.render(html,
               metadata: [title: "Caller title", author: "Finance", subject: "Monthly"]
             )

    assert explicit_pdf =~ "/Title (Caller title)"
    refute explicit_pdf =~ "/Title (HTML Report)"
    assert explicit_pdf =~ "/Author (Finance)"
    assert explicit_pdf =~ "/Subject (Monthly)"

    assert {:ok, map_pdf} = HtmlToPdf.render(html, metadata: %{author: "Operations"})
    assert map_pdf =~ "/Title (HTML Report)"
    assert map_pdf =~ "/Author (Operations)"
  end

  test "render supports CSS-declared local fonts inside print media" do
    font_path = ttf_font_path!()

    html = """
    <style>
      @font-face {
        font-family: "CSS Fixture";
        src: url("#{font_path}") format("truetype");
      }
      @media screen { p { color: blue; } }
      @media print { p { color: red; font-family: "CSS Fixture"; } }
    </style>
    <p>CSS font</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "/Subtype /Type0"
    assert pdf =~ "1 0 0 rg"
    refute pdf =~ "0 0 1 rg"
  end

  test "render supports commas inside quoted CSS font URLs" do
    fixture_dir = Path.join(System.tmp_dir!(), "native-elixir-pdf-comma-font-url")
    font_path = Path.join(fixture_dir, "report,sans.ttf")
    File.mkdir_p!(fixture_dir)
    File.cp!(ttf_font_path!(), font_path)

    html = """
    <style>
      @font-face {
        font-family: "Comma Fixture";
        src: url("report,sans.ttf") format("truetype");
      }
      p { font-family: "Comma Fixture"; }
    </style>
    <p>Comma font</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, base_url: fixture_dir)
    assert pdf =~ "/Subtype /Type0"
  after
    File.rm_rf(Path.join(System.tmp_dir!(), "native-elixir-pdf-comma-font-url"))
  end

  test "render identifies invalid font-face descriptors" do
    html = """
    <style>
      @font-face {
        font-family: Broken;
        src: url("broken.ttf");
        font-style: oblique;
      }
    </style>
    <p>Broken font</p>
    """

    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               source: "font-style: oblique",
               message:
                 ~s(line 5: @font-face declaration "font-style: oblique" is invalid or unsupported)
             }}} = HtmlToPdf.render(html)
  end

  test "render rejects malformed PDF metadata through the diagnostics contract" do
    assert {:error,
            {:invalid_pdf_input,
             %{
               stage: :pdf,
               reason: :invalid_pdf_input,
               operation: :write_pdf,
               module: NativeElixirPdfUtilities.HtmlToPdf.PdfWriter,
               message: "PDF metadata must use supported fields and value types"
             }}} = HtmlToPdf.render("<title>Fallback</title><p>Hello</p>", metadata: :bad)

    assert {:error, {:invalid_pdf_input, %{stage: :pdf}}} =
             HtmlToPdf.render("<title>Fallback</title><p>Hello</p>", metadata: [:bad])
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

  test "render writes generated attributes and counters into PDF text runs" do
    html = """
    <style>
      body { counter-reset: section 0; }
      h2::before { counter-increment: section; content: "Section " counter(section) ": "; }
      [data-status=ready]:not(.hidden)::after { content: " [" attr(data-status) "]"; }
    </style>
    <h2 data-status="ready">Overview</h2>
    <h2 data-status="ready">Details</h2>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "(Section 1: ) Tj"
    assert pdf =~ "(Section 2: ) Tj"
    assert length(Regex.scan(~r/\( \[ready\]\) Tj/, pdf)) == 2
    assert pdf =~ "(Overview) Tj"
    assert pdf =~ "(Details) Tj"
  end

  test "render converts block box styling to PDF drawing commands" do
    html =
      ~s(<p style="margin: 2pt; padding: 3pt; border: 1pt solid red; border-radius: 2pt; background-color: #eeeeee">Boxed</p>)

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "0.9333 0.9333 0.9333 rg"
    assert pdf =~ "1 0 0 RG 1 w"
    assert pdf =~ "(Boxed) Tj"
  end

  test "render accepts print template semantics and inch page sizes" do
    html =
      ~s(<section class="sheet"><style>@page { size: A4 landscape; margin: 7mm; }.sheet { height: 196mm; }</style><article><img src="data:image/png;base64,#{png_fixture_base64()}" alt="QR Code"><p>Sticker</p></article></section>)

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {4.92126, 1.49606})
    assert pdf =~ "/MediaBox [0 0 354.3307 107.7163]"
    assert pdf =~ "(Sticker) Tj"
    assert pdf =~ "/Subtype /Image"
  end

  test "render uses page CSS defaults unless caller overrides them" do
    html =
      ~s(<style>@page { size: A4 landscape; margin: 7mm; }</style><div style="height: 10pt; border: 1pt solid #000000">Page CSS</div>)

    assert {:ok, css_pdf} = HtmlToPdf.render(html)
    assert css_pdf =~ "/MediaBox [0 0 841.89 595.28]"
    assert css_pdf =~ "19.8425 563.4375 802.205 12 re"

    assert {:ok, override_pdf} = HtmlToPdf.render(html, page_size: {200, 100}, margin: 0)
    assert override_pdf =~ "/MediaBox [0 0 200 100]"
    assert override_pdf =~ "0 88 200 12 re"
  end

  test "render applies complete page geometry and explicit option precedence" do
    html = """
    <style>
      @page {
        size: A5 landscape;
        margin: 10pt 20pt 30pt 40pt;
        margin-left: 50pt;
      }
    </style>
    <div style="height: 10pt; border: 1pt solid #000000">Geometry</div>
    """

    assert {:ok, css_pdf} = HtmlToPdf.render(html)
    assert css_pdf =~ "/MediaBox [0 0 595.28 419.53]"
    assert css_pdf =~ "50 397.53 525.28 12 re"

    assert {:ok, override_pdf} =
             HtmlToPdf.render(html,
               page_size: {200, 100},
               margin: "1pt 2pt 3pt 4pt"
             )

    assert override_pdf =~ "/MediaBox [0 0 200 100]"
    assert override_pdf =~ "4 87 194 12 re"
  end

  test "render uses page CSS defaults from configured stylesheets" do
    assert {:ok, inline_pdf} =
             HtmlToPdf.render("<p>Configured page</p>",
               stylesheets: [{:css, "@media print { @page { size: letter; margin: 0; } }"}]
             )

    assert inline_pdf =~ "/MediaBox [0 0 612 792]"

    stylesheet_path =
      Path.join(System.tmp_dir!(), "native-elixir-pdf-configured-page-options.css")

    File.write!(stylesheet_path, "@page { size: A4 landscape; margin: 0; }")

    assert {:ok, file_pdf} =
             HtmlToPdf.render("<p>Configured file page</p>",
               stylesheets: [{:file, stylesheet_path}]
             )

    assert file_pdf =~ "/MediaBox [0 0 841.89 595.28]"
  after
    File.rm(Path.join(System.tmp_dir!(), "native-elixir-pdf-configured-page-options.css"))
  end

  test "render cascades page-margin longhands across configured and embedded stylesheets" do
    html = """
    <style>
      @page { margin-left: 40pt; margin-bottom: 30pt; }
    </style>
    <div style="height: 10pt; border: 1pt solid #000000">Cascade</div>
    """

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               page_size: {200, 100},
               stylesheets: [{:css, "@page { margin: 10pt 20pt; }"}]
             )

    assert pdf =~ "40 78 140 12 re"
  end

  test "render returns detailed diagnostics for invalid page declarations" do
    html = """
    <style>
    @page {
      nonsense: value;
      margin: bananas;
    }
    </style>
    <p>Invalid page CSS</p>
    """

    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               operation: :render,
               module: NativeElixirPdfUtilities.HtmlToPdf,
               line: 3,
               column: 3,
               source: "nonsense: value",
               message: ~s(line 3: declaration "nonsense: value" is invalid or unsupported)
             }}} = HtmlToPdf.render(html)
  end

  test "render returns detailed diagnostics for unsupported page selectors" do
    html = """
    <style>
    @page :first { size: A5; margin: 1pt; }
    </style>
    <p>Unsupported page selector</p>
    """

    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               operation: :render,
               module: NativeElixirPdfUtilities.HtmlToPdf,
               line: 2,
               column: 1,
               source: "@page :first",
               message: ~s(line 2: page rule "@page :first" is invalid or unsupported)
             }}} = HtmlToPdf.render(html)
  end

  test "render accepts valid unused page-context properties" do
    html = """
    <style>
    @page {
      margin-top: 100px;
      margin-right: auto;
      page-orientation: upright;
      background: white;
      border: 1px solid black;
      padding: 5mm;
      color: black;
    }
    </style>
    <p>Valid page CSS</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "(Valid page CSS) Tj"
  end

  test "render returns diagnostics for malformed embedded media rules" do
    html = "<style>@media print { @media print { p { color: red; } } }</style><p>x</p>"

    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               operation: :render,
               module: NativeElixirPdfUtilities.HtmlToPdf
             }}} = HtmlToPdf.render(html)
  end

  test "render falls back across CSS font sources and reports failed candidates" do
    font_path = ttf_font_path!()
    missing_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-missing-css-font.ttf")

    html = """
    <style>
      @font-face {
        font-family: FallbackFixture;
        src: url("#{missing_path}"), url("#{font_path}") format("truetype");
      }
      p { font-family: FallbackFixture; }
    </style>
    <p>Fallback font</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "/Subtype /Type0"

    failed_html = """
    <style>
      @font-face { font-family: MissingFixture; src: url("#{missing_path}"); }
      p { font-family: MissingFixture; }
    </style>
    <p>Missing font</p>
    """

    assert {:error,
            {:invalid_document,
             %{
               stage: :style,
               reason: :invalid_document,
               source: ^missing_path,
               message: message
             }}} = HtmlToPdf.render(failed_html)

    assert message =~ "CSS font sources could not be resolved or loaded"
    refute message =~ "font-family"
  end

  test "render lays out grid containers nested inside flex items" do
    html = """
    <section style="display: flex; flex-direction: column; width: 400pt">
      <div>Header</div>
      <div style="display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 8px; flex: 1">
        <article style="display: flex; flex-direction: column; border: 1px solid #ccd6e1">
          <div style="display: block; text-transform: uppercase">thread</div>
          <div>AGSYIKOFP1</div>
        </article>
      </div>
    </section>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {600, 300})
    assert pdf =~ "(Header) Tj"
    assert pdf =~ "(THREAD) Tj"
    assert pdf =~ "(AGSYIKOFP1) Tj"
  end

  test "render applies configured and embedded CSS before writing PDF output" do
    html =
      ~s(<style>p.notice { color: #336699; font-weight: bold; }</style><p class="notice">Styled</p>)

    assert {:ok, pdf} = HtmlToPdf.render(html, stylesheets: [{:css, "p { color: red; }"}])
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

    assert {:error,
            {:invalid_document,
             %{
               stage: :style,
               reason: :invalid_document,
               message: message
             }}} = HtmlToPdf.render(~s[<p><a href="javascript:alert(1)">bad</a></p>])

    assert message =~ "document style validation failed"
  end

  test "render returns detailed unsupported HTML diagnostics" do
    html = """
    <p>Before</p>
    <script>alert("bad")</script>
    """

    assert {:error,
            {:unsupported_html,
             %{
               stage: :html,
               reason: :unsupported_html,
               line: 2,
               source: ~s(<script>),
               message: message
             }}} = HtmlToPdf.render(html)

    assert message == ~s(line 2: HTML tag "<script>" is unsupported)
  end

  test "render returns detailed invalid selector diagnostics" do
    html = """
    <style>
    p > { color: red; }
    </style>
    <p>Hello</p>
    """

    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               line: 2,
               source: "p >",
               message: message
             }}} = HtmlToPdf.render(html)

    assert message == ~s(line 2: selector "p >" is invalid or unsupported)
  end

  test "render returns detailed inline CSS diagnostics" do
    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               line: 1,
               source: "display: table-row-group",
               message: message
             }}} = HtmlToPdf.render(~s(<p style="display: table-row-group">Bad</p>))

    assert message == ~s(line 1: declaration "display: table-row-group" is invalid or unsupported)
  end

  test "render returns detailed layout option diagnostics" do
    assert {:error,
            {:invalid_page_size,
             %{
               stage: :layout,
               reason: :invalid_page_size,
               message: "layout failed: invalid_page_size"
             }}} = HtmlToPdf.render("<p>Hello</p>", page_size: {0, 100})

    assert {:error,
            {:invalid_margin,
             %{
               stage: :layout,
               reason: :invalid_margin,
               message:
                 "layout failed: margin must be non-negative and leave a positive printable area"
             }}} = HtmlToPdf.render("<p>Hello</p>", margin: -1)

    assert {:error,
            {:invalid_margin,
             %{
               stage: :layout,
               reason: :invalid_margin,
               message:
                 "layout failed: margin must be non-negative and leave a positive printable area"
             }}} = HtmlToPdf.render("<p>Hello</p>", page_size: {100, 100}, margin: 50)

    assert {:error, {:invalid_margin, %{stage: :layout, reason: :invalid_margin}}} =
             HtmlToPdf.render("<p>Hello</p>", page_size: {100, 100}, margin: 60)
  end

  test "render asserts detailed failure shapes for public error categories" do
    assert {:error,
            {:invalid_html,
             %{
               stage: :html,
               reason: :invalid_html,
               message: "HTML input must be a string"
             }}} = HtmlToPdf.render(:not_html)

    assert {:error,
            {:unsupported_html,
             %{
               stage: :html,
               reason: :unsupported_html,
               line: 1,
               column: 1,
               source: "<canvas>",
               message: ~s(line 1: HTML tag "<canvas>" is unsupported)
             }}} = HtmlToPdf.render("<canvas></canvas>")

    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               line: 1,
               column: 1,
               source: "color",
               message: ~s(line 1: declaration "color" is invalid or unsupported)
             }}} = HtmlToPdf.render(~s(<p style="color">Bad CSS</p>))

    bad_background_html = """
    <style>p { background: linear-gradient(red, blue); }</style>
    <p>Bad CSS value</p>
    """

    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               line: 1,
               column: 5,
               source: "background: linear-gradient(red, blue)",
               message:
                 ~S|line 1: declaration "background: linear-gradient(red, blue)" is invalid or unsupported|
             }}} =
             HtmlToPdf.render(bad_background_html)

    for invalid_stylesheets <- [
          [:not_css],
          ["p { color: red; }"],
          [{:css, 123}],
          [{:file, 123}],
          :not_a_list
        ] do
      assert {:error,
              {:invalid_options,
               %{
                 stage: :options,
                 reason: :invalid_options,
                 message:
                   "stylesheets option must be a list of {:css, css} or {:file, path} tuples"
               }}} = HtmlToPdf.render("<p>Hello</p>", stylesheets: invalid_stylesheets)
    end

    missing_stylesheet =
      Path.join(System.tmp_dir!(), "native-elixir-pdf-missing-stylesheet.css")

    File.rm(missing_stylesheet)

    assert {:error,
            {:invalid_document,
             %{
               stage: :style,
               reason: :invalid_document,
               message: "configured stylesheet file could not be read"
             }}} =
             HtmlToPdf.render("<p>Hello</p>",
               stylesheets: [{:file, missing_stylesheet}]
             )

    assert {:error,
            {:invalid_options,
             %{
               stage: :options,
               reason: :invalid_options,
               operation: :render,
               module: NativeElixirPdfUtilities.HtmlToPdf,
               message: "render options must be a keyword list"
             }}} = HtmlToPdf.render("<p>Hello</p>", [:not_options])

    assert {:error,
            {:invalid_path,
             %{
               stage: :file,
               reason: :invalid_path,
               operation: :render_file,
               module: NativeElixirPdfUtilities.HtmlToPdf,
               message: "input and output paths must be strings"
             }}} = HtmlToPdf.render_file(:not_a_path, "/tmp/native-elixir-pdf-failure.pdf")

    missing_input = "/tmp/native-elixir-pdf-missing-input.html"

    assert {:error,
            {:enoent,
             %{
               stage: :file,
               reason: :enoent,
               operation: :read,
               module: NativeElixirPdfUtilities.HtmlToPdf,
               source: ^missing_input,
               message: "file read failed: enoent"
             }}} =
             HtmlToPdf.render_file(missing_input, "/tmp/native-elixir-pdf-failure.pdf")
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

  test "render fragments a paragraph taller than the printable page" do
    lines =
      1..13
      |> Enum.map_join("<br>", fn index ->
        "Paragraph line #{String.pad_leading(Integer.to_string(index), 2, "0")}"
      end)

    html = """
    <p style="font-size: 10pt; line-height: 12pt; margin: 0">#{lines}</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {200, 100}, margin: 10)
    assert pdf =~ "/Count 3"

    for index <- 1..13 do
      label = "Paragraph line #{String.pad_leading(Integer.to_string(index), 2, "0")}"
      assert pdf =~ "(#{label}) Tj"
    end
  end

  test "render adds opt-in running page furniture without changing default rendering" do
    html = """
    <p>First body page</p>
    <div style="page-break-after: always"></div>
    <p>Second body page</p>
    <div style="page-break-after: always"></div>
    <p>Third body page</p>
    """

    assert {:ok, default_pdf} =
             HtmlToPdf.render(html, page_size: {200, 100}, margin: 20)

    refute default_pdf =~ "(Report header) Tj"
    refute default_pdf =~ "(Page 1 of 3) Tj"

    assert {:ok, furnished_pdf} =
             HtmlToPdf.render(html,
               page_size: {200, 100},
               margin: 20,
               page_furniture: [
                 header: [
                   default: "<div style=\"font-size: 8pt\">Report header</div>",
                   first: false,
                   odd: "<div style=\"font-size: 8pt\">Odd header</div>",
                   even: "<div style=\"font-size: 8pt\">Even header</div>"
                 ],
                 footer:
                   "<div style=\"font-size: 8pt; text-align: right\">Page {{page}} of {{pages}}</div>"
               ]
             )

    assert furnished_pdf =~ "/Count 3"
    refute furnished_pdf =~ "(Report header) Tj"
    assert furnished_pdf =~ "(Odd header) Tj"
    assert furnished_pdf =~ "(Even header) Tj"
    assert furnished_pdf =~ "(Page 1 of 3) Tj"
    assert furnished_pdf =~ "(Page 2 of 3) Tj"
    assert furnished_pdf =~ "(Page 3 of 3) Tj"
  end

  test "render reports invalid page furniture through the shared diagnostics contract" do
    assert {:error,
            {:invalid_options,
             %{
               stage: :options,
               reason: :invalid_options,
               operation: :decorate_pages,
               module: NativeElixirPdfUtilities.HtmlToPdf.PageFurniture,
               message: "page_furniture contains unsupported keys: [:watermark]"
             }}} =
             HtmlToPdf.render("<p>Hello</p>",
               page_furniture: [watermark: "Draft"]
             )

    assert {:error,
            {:invalid_layout,
             %{
               stage: :layout,
               reason: :invalid_layout,
               operation: :decorate_pages,
               module: NativeElixirPdfUtilities.HtmlToPdf.PageFurniture,
               message: "footer page furniture height 14.4pt exceeds the 0.0pt page margin"
             }}} =
             HtmlToPdf.render("<p>Hello</p>", page_furniture: [footer: "Page {{page}}"])

    assert {:error,
            {:invalid_layout,
             %{
               stage: :layout,
               reason: :invalid_layout,
               operation: :decorate_pages,
               module: NativeElixirPdfUtilities.HtmlToPdf.PageFurniture,
               message: "header page furniture layout failed: invalid_layout"
             }}} =
             HtmlToPdf.render("<p>Hello</p>",
               page_size: {200, 100},
               margin: 20,
               page_furniture: [
                 header: ~s(<div style="display: flex"><ul><li>Invalid layout</li></ul></div>)
               ]
             )
  end

  test "render honors an empty manual page break element" do
    html = ~s(<p>First</p><div style="page-break-after: always"></div><p>Second</p>)

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {200, 100}, margin: 10)
    assert pdf =~ "/Count 2"
    assert pdf =~ "(First) Tj"
    assert pdf =~ "(Second) Tj"

    html = ~s(<p>First</p><div style="page-break-before: always"></div><p>Second</p>)

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {200, 100}, margin: 10)
    assert pdf =~ "/Count 2"
    assert pdf =~ "(First) Tj"
    assert pdf =~ "(Second) Tj"
  end

  test "render converts a flex layout subset to PDF text boxes" do
    html =
      ~s(<div style="display: flex; width: 80pt; gap: 8pt"><span style="order: 2">Second</span><span style="order: 1">First</span></div>)

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "(First) Tj"
    assert pdf =~ "(Second) Tj"
  end

  test "render converts row flex items with block children and percentage widths" do
    html = """
    <html>
      <head>
        <style>
          .header { display: flex; }
          .address-section {
            display: flex;
            width: 100%;
            justify-content: space-between;
            align-items: center;
            flex-wrap: wrap;
          }
        </style>
      </head>
      <body>
        <div class="header">
          <div style="width: 80%"><p>Left</p></div>
          <div style="width: 20%"><p>Right</p></div>
        </div>
        <div class="address-section">
          <div class="section"><h4>Supplier Address</h4><p>Supplier</p></div>
          <div class="section"><h4>Buyer Address</h4><p>Buyer</p></div>
          <div class="section"><h4>Consignee Address</h4><p>Consignee</p></div>
        </div>
      </body>
    </html>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: :a4)
    assert pdf =~ "(Left) Tj"
    assert pdf =~ "(Right) Tj"
    assert pdf =~ "(Supplier Address) Tj"
    assert pdf =~ "(Buyer Address) Tj"
    assert pdf =~ "(Consignee"
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

  test "render rasterizes an SVG data URI image to a PDF image object" do
    svg =
      ~s(<svg xmlns="http://www.w3.org/2000/svg" width="2" height="1"><rect width="2" height="1" fill="red"/></svg>)

    src = "data:image/svg+xml;base64,#{Base.encode64(svg)}"
    html = ~s(<img src="#{src}" style="width: 20pt">)

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "/Subtype /Image"
    assert pdf =~ "/Width 2"
    assert pdf =~ "/Height 1"
    assert pdf =~ "/ColorSpace /DeviceRGB"
    assert pdf =~ "/Filter /FlateDecode"
  end

  test "render embeds configured TTF fonts for Unicode text" do
    html = ~s(<p style="font-family: 'Fixture Sans', Helvetica">Café</p>)

    assert {:ok, pdf} =
             HtmlToPdf.render(html, fonts: [%{family: "Fixture Sans", path: ttf_font_path!()}])

    assert pdf =~ "/Subtype /Type0"
    assert pdf =~ "/Subtype /CIDFontType2"
    assert pdf =~ "/FontFile2"
    assert pdf =~ "/ToUnicode"
    assert pdf =~ "<"
    refute pdf =~ "(Café) Tj"
  end

  test "render falls back from built-in fonts for Unicode text without corrupting glyphs" do
    assert {:ok, pdf} = HtmlToPdf.render("<p>café © α €</p>")

    assert pdf =~ "/Subtype /Type1"
    assert pdf =~ "/Subtype /Type0"
    assert pdf =~ "/FontFile2"
    assert pdf =~ "/ToUnicode"

    assert {:ok, extracted} = Text.extract(pdf, layout: false)
    assert extracted =~ "é"
    assert extracted =~ "©"
    assert extracted =~ "α"
    assert extracted =~ "€"
  end

  test "render rejects invalid UTF-8 and glyphs unavailable in configured or bundled fonts" do
    assert {:error,
            {:invalid_encoding,
             %{
               stage: :html,
               reason: :invalid_encoding,
               operation: :render,
               module: HtmlToPdf,
               message: "HTML input must be valid UTF-8"
             }}} = HtmlToPdf.render(<<"<p>", 255, "</p>">>)

    assert {:error,
            {:unsupported_glyph,
             %{
               stage: :font,
               reason: :unsupported_glyph,
               operation: :resolve_fonts,
               module: NativeElixirPdfUtilities.HtmlToPdf.FontFallback,
               source: "漢"
             }}} = HtmlToPdf.render("<p>漢</p>")
  end

  test "render_file writes a PDF for a supported paragraph" do
    input_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-html-to-pdf-test.html")
    output_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-html-to-pdf-test.pdf")

    File.write!(input_path, "<p>Hello</p>")

    assert HtmlToPdf.render_file(input_path, output_path) == :ok
    assert File.read!(output_path) =~ "(Hello) Tj"

    assert {:error, {:enoent, %{stage: :file, reason: :enoent, operation: :read}}} =
             HtmlToPdf.render_file(input_path <> ".missing", output_path)

    assert {:error,
            {:invalid_path, %{stage: :file, reason: :invalid_path, operation: :render_file}}} =
             HtmlToPdf.render_file(:bad_input, output_path)
  after
    input_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-html-to-pdf-test.html")
    output_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-html-to-pdf-test.pdf")

    File.rm(input_path)
    File.rm(output_path)
  end

  test "render_file returns diagnostics for render and write failures" do
    input_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-html-to-pdf-bad-test.html")
    File.write!(input_path, "<canvas></canvas>")

    assert {:error,
            {:unsupported_html,
             %{
               stage: :html,
               reason: :unsupported_html,
               operation: :render,
               module: NativeElixirPdfUtilities.HtmlToPdf,
               source: "<canvas>",
               message: ~s(line 1: HTML tag "<canvas>" is unsupported)
             }}} = HtmlToPdf.render_file(input_path, input_path <> ".pdf")

    File.write!(input_path, "<p>Hello</p>")
    output_path = System.tmp_dir!()

    assert {:error,
            {reason,
             %{
               stage: :file,
               reason: reason,
               operation: :write,
               module: NativeElixirPdfUtilities.HtmlToPdf,
               source: ^output_path
             }}} = HtmlToPdf.render_file(input_path, output_path)
  after
    input_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-html-to-pdf-bad-test.html")
    File.rm(input_path)
    File.rm(input_path <> ".pdf")
  end

  defp png_fixture(width, height) do
    row = :binary.copy(<<255, 0, 0>>, width)
    rows = Enum.map_join(1..height, "", fn _index -> <<0>> <> row end)

    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<width::32, height::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("IDAT", :zlib.compress(rows)) <>
      png_chunk("IEND", "")
  end

  defp png_fixture_base64 do
    1
    |> png_fixture(1)
    |> Base.encode64()
  end

  defp ttf_font_path! do
    [
      "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
      "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
      "/usr/share/fonts/truetype/noto/NotoSans-Regular.ttf"
    ]
    |> Enum.find(&File.exists?/1)
    |> case do
      nil -> flunk("No local TTF font fixture found")
      path -> path
    end
  end

  defp png_chunk(type, data) do
    crc = :erlang.crc32(type <> data)
    <<byte_size(data)::32, type::binary, data::binary, crc::32>>
  end
end
