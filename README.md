<p align="center">
  <img src="assets/readme-banner.svg" alt="Native Elixir PDF Utilities" />
</p>

<p align="center">
  <a href="https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/LICENSE"><img src="https://img.shields.io/hexpm/l/native_elixir_pdf_utilities.svg" alt="License" /></a> <a href="https://hex.pm/packages/native_elixir_pdf_utilities"><img src="https://img.shields.io/hexpm/v/native_elixir_pdf_utilities.svg" alt="Hex.pm" /></a> <a href="https://native-elixir-pdf-utilities.hexdocs.pm/api-reference.html"><img src="https://img.shields.io/badge/hex-docs-blue.svg" alt="HexDocs" /></a> <img src="https://img.shields.io/badge/elixir-~%3E%201.19-4B275F.svg" alt="Elixir ~> 1.19" />
</p>

# Native Elixir PDF Utilities

Native Elixir PDF Utilities is a small library for developers who need practical PDF building blocks without command line tools.

PDFs are useful, awkward, and full of edge cases. This project focuses on the common structural work that Elixir applications often need: reading PDF bytes, inspecting and updating document information, extracting embedded text when it is available, combining documents, and rebuilding selected pages in a predictable way.

The goal is not to be a full PDF engine overnight. It is a steadily improving toolkit, handled by an excited developer who wants this to become a dependable native Elixir option for day-to-day PDF utility work.

## Package and Docs

- Package: https://hex.pm/packages/native_elixir_pdf_utilities
- API docs: https://native-elixir-pdf-utilities.hexdocs.pm/api-reference.html

## What It Does

1. [Tokenizer](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/pdf-tokenizer.md) - turns PDF byte streams into structured Elixir tokens.
2. [PDF information](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/pdf-information.md) - reads page geometry and document metadata, detects encryption, and updates common metadata fields.
3. [Merger](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/pdf-merging.md) - combines multiple PDF binaries into a fresh PDF with rewritten object references.
4. [Page transforms and splitting](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/pdf-page-transforms.md) - rebuild PDFs after selecting, reordering, deleting, rotating, or splitting pages.
5. [Outlines and bookmarks](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/pdf-outlines.md) - read, replace, detect, and preserve PDF navigation trees.
6. [Reader](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/pdf-reader.md) and [text extraction](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/text-extraction.md) - strictly resolve embedded Unicode text from classic and modern PDFs.
7. [HTML to PDF](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/html-to-pdf-compatibility.md) - renders a strict, document-oriented HTML/CSS subset to native PDF bytes, with [examples](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/html-to-pdf-examples.md) for common workflows. It supports configured fonts and cross-platform discovery of installed system fonts. Rendering does not require Chromium, wkhtmltopdf, Node, Python, SaaS calls, or a Rust toolchain.

See the [documentation](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/tree/main/docs) for feature guides, configurable resource limits, and quick reference.

## Installation

```elixir
def deps do
  [
    {:native_elixir_pdf_utilities, "~> 0.15.0"}
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
