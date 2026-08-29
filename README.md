<p align="center">
  <img src="assets/readme-banner.svg" alt="Native Elixir PDF Utilities" />
</p>

<p align="center">
  <a href="https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/LICENSE"><img src="https://img.shields.io/hexpm/l/native_elixir_pdf_utilities.svg" alt="License" /></a> <a href="https://hex.pm/packages/native_elixir_pdf_utilities"><img src="https://img.shields.io/hexpm/v/native_elixir_pdf_utilities.svg" alt="Hex.pm" /></a> <a href="https://native-elixir-pdf-utilities.hexdocs.pm/api-reference.html"><img src="https://img.shields.io/badge/hex-docs-blue.svg" alt="HexDocs" /></a> <img src="https://img.shields.io/badge/elixir-~%3E%201.19-4B275F.svg" alt="Elixir ~> 1.19" />
</p>

# Native Elixir PDF Utilities

Native Elixir PDF Utilities is a small library for developers who need practical PDF building blocks without command line tools.

PDFs are useful, awkward, and full of edge cases. This project focuses on the common structural work that Elixir applications often need: reading PDF bytes, inspecting and updating document information, extracting embedded text when it is available, and combining documents in a predictable way.

The goal is not to be a full PDF engine overnight. It is a steadily improving toolkit, handled by an excited developer who wants this to become a dependable native Elixir option for day-to-day PDF utility work.

## Package and Docs

- Package: https://hex.pm/packages/native_elixir_pdf_utilities
- API docs: https://native-elixir-pdf-utilities.hexdocs.pm/api-reference.html

## What It Does

1. Tokenizer - turns PDF byte streams into structured Elixir tokens.
2. PDF information - reads page geometry and document metadata, detects encryption, and updates common metadata fields.
3. Merger - combines multiple PDF binaries into a fresh PDF with rewritten object references.
4. Reader and text extraction - strictly resolve embedded Unicode text from classic and modern PDFs.
5. HTML to PDF - renders a strict, document-oriented HTML/CSS subset to native PDF bytes, with configured and installed system fonts, without Chromium, wkhtmltopdf, Node, Rust, Python, OS packages, or SaaS calls.

See the [documentation](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/tree/main/docs) for feature guides, configurable resource limits, and quick reference.

## Installation

```elixir
def deps do
  [
    {:native_elixir_pdf_utilities, "~> 0.14.0"}
  ]
end
```

## Development

Run the complete supported-version quality matrix before contributing:

```bash
./scripts/quality-matrix
```

The matrix compiles and tests the supported Elixir versions, enforces formatting
and 100% coverage, runs Dialyzer, and compares HTML-to-PDF fixtures with
Chromium. See the
[contribution guide](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/CONTRIBUTING.md)
for prerequisites, quicker installed-version checks, and result interpretation.

## License

The library source is MIT licensed. Bundled DejaVu font files use the Bitstream
Vera license; WHATWG character-reference data and Adobe glyph-name data retain
their respective BSD 3-Clause notices. See the complete
[license texts](docs/licenses.md).
