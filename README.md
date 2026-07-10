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

## PDF Input Handling

Merge and text extraction are designed for classic, tokenizable PDFs. Malformed
input now returns a diagnostic error rather than producing partial output or
raising. In particular, merging validates indirect objects and stream
boundaries before writing a result.

The tokenizer emits `{:error, reason}` tokens for malformed literal or
hexadecimal strings. Consumers that operate directly on tokenizer output should
handle these tokens as invalid input.

For extraction safety, unusually large ToUnicode CMaps are ignored rather than
expanded. This may reduce decoded text for documents that rely exclusively on
an oversized CMap, but it bounds memory and CPU use for untrusted PDFs.

## Native HTML/CSS to PDF

`NativeElixirPdfUtilities.HtmlToPdf` is a native document renderer. It is designed for predictable server-side PDFs such as reports, invoices, labels, statements, and simple generated documents. It is not a browser engine and does not claim browser compatibility.

For runnable render examples, styling patterns, file/image/font examples, and diagnostic error handling, see [HTML to PDF Examples](docs/html-to-pdf-exmaples.md).

For HtmlToPdf options, supported HTML elements, CSS rules, layout behavior, and known renderer boundaries, see [HTML to PDF Compatibility](docs/html-to-pdf-compatibility.md).

For planned improvements and useful future features, see [Roadmap](ROADMAP.md).

For public error shapes and diagnostic fields, see [Diagnostics](docs/diagnostics.md).

## Installation

```elixir
def deps do
  [
    {:native_elixir_pdf_utilities, "~> 0.5.1"}
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
