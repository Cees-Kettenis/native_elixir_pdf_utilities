<p align="center">
  <img src="assets/readme-banner.svg" alt="Native Elixir PDF Utilities" />
</p>

<p align="center">
  <a href="./LICENSE"><img src="https://img.shields.io/hexpm/l/native_elixir_pdf_utilities.svg" alt="License" /></a> <a href="https://hex.pm/packages/native_elixir_pdf_utilities"><img src="https://img.shields.io/hexpm/v/native_elixir_pdf_utilities.svg" alt="Hex.pm" /></a> <a href="https://native-elixir-pdf-utilities.hexdocs.pm/api-reference.html"><img src="https://img.shields.io/badge/hex-docs-blue.svg" alt="HexDocs" /></a> <img src="https://img.shields.io/badge/elixir-~%3E%201.18-4B275F.svg" alt="Elixir ~> 1.18" />
</p>

# Native Elixir PDF Utilities

Native Elixir PDF Utilities is a small library for developers who need practical PDF building blocks without command line tools.

PDFs are useful, awkward, and full of edge cases. This project focuses on the common structural work that Elixir applications often need: reading PDF bytes, understanding the object stream, extracting embedded text when it is available, and combining documents in a predictable way.

The goal is not to be a full PDF engine overnight. It is a steadily improving toolkit, handled by an excited developer who wants this to become a dependable native Elixir option for day-to-day PDF utility work.

## Package and Docs

- Package: https://hex.pm/packages/native_elixir_pdf_utilities
- API docs: https://native-elixir-pdf-utilities.hexdocs.pm/api-reference.html

## What It Does

1. Tokenizer - turns classic PDF byte streams into structured Elixir tokens.
2. Merger - combines multiple PDF binaries into a fresh PDF with rewritten object references.
3. Reader - extracts embedded text from PDFs when the document contains readable text data.
4. HTML to PDF - renders a strict, document-oriented HTML/CSS subset to native PDF bytes without Chromium, wkhtmltopdf, Node, Rust, Python, OS packages, or SaaS calls.

## Native HTML/CSS to PDF

`NativeElixirPdfUtilities.HtmlToPdf` is a native document renderer. It is designed for predictable server-side PDFs such as reports, invoices, labels, statements, and simple generated documents. It is not a browser engine and does not claim browser compatibility.

```elixir
alias NativeElixirPdfUtilities.HtmlToPdf

{:ok, pdf} =
  HtmlToPdf.render("""
  <style>
    h1 { color: #336699; }
    p { border: 1pt solid #336699; padding: 6pt; }
  </style>
  <h1>Invoice</h1>
  <p>Hello <strong>customer</strong></p>
  <ul><li><a href="https://example.com">View account</a></li></ul>
  """)
```

Write an HTML file directly to a PDF path:

```elixir
:ok =
  HtmlToPdf.render_file(
    "invoice.html",
    "invoice.pdf",
    page_size: :a4,
    margin: "20mm",
    stylesheets: ["assets/invoice.css"],
    base_url: "assets"
  )
```

Embed caller-supplied TTF fonts explicitly:

```elixir
{:ok, pdf} =
  HtmlToPdf.render(
    ~s(<p style="font-family: 'Report Sans', Helvetica">Café</p>),
    fonts: [
      %{family: "Report Sans", path: "priv/fonts/report-sans.ttf"}
    ]
  )
```

Render local or data URI images:

```elixir
{:ok, pdf} =
  HtmlToPdf.render(
    ~s(<img src="logo.png" style="width: 96pt"><p>Quarterly report</p>),
    base_url: "priv/static"
  )
```

For HtmlToPdf options, supported HTML elements, CSS rules, layout behavior, and known renderer boundaries, see [HTML to PDF Compatibility](docs/html-to-pdf-compatibility.md).

## Installation

```elixir
def deps do
  [
    {:native_elixir_pdf_utilities, "~> 0.2.0"}
  ]
end
```

## Development

Run the project checks before contributing:

```bash
mise exec -- mix test
MIX_ENV=test mise exec -- mix dialyzer
mise exec -- mix format
```

For HTML/CSS to PDF changes, also open representative rendered PDFs and compare layout visually across single-page, multi-page, table, flexbox, grid, image, and embedded-font examples.

## License

MIT. See [LICENSE](LICENSE).
