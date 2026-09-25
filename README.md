<p align="center">
  <img src="assets/readme-banner.svg" alt="Native Elixir PDF Utilities" />
</p>

<p align="center">
  <a href="https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/LICENSE"><img src="https://img.shields.io/hexpm/l/native_elixir_pdf_utilities.svg" alt="License" /></a> <a href="https://hex.pm/packages/native_elixir_pdf_utilities"><img src="https://img.shields.io/hexpm/v/native_elixir_pdf_utilities.svg" alt="Hex.pm" /></a> <a href="https://native-elixir-pdf-utilities.hexdocs.pm/api-reference.html"><img src="https://img.shields.io/badge/hex-docs-blue.svg" alt="HexDocs" /></a> <img src="https://img.shields.io/badge/elixir-~%3E%201.19-4B275F.svg" alt="Elixir ~> 1.19" />
</p>

# Native Elixir PDF Utilities

Native Elixir PDF Utilities brings HTML-to-PDF rendering and PDF editing directly
into your Elixir application, without a browser or external PDF command-line
tools. It can replace Chromium-based rendering for templates using the
[supported HTML/CSS features](docs/html-to-pdf-compatibility.md). With matched
fonts and page settings, the current development build achieves **less than 1%
changed pixels on every compared page** across 67 Chromium parity fixtures and
80 pages, including invoices, forms, purchase orders, labels, and multi-page reports.

Beyond rendering, it merges, transforms, splits, and stamps PDFs, fills forms,
embeds attachments, extracts embedded text, and reads and updates metadata and
bookmarks.

## Package and docs

- Package: https://hex.pm/packages/native_elixir_pdf_utilities
- API docs: https://native-elixir-pdf-utilities.hexdocs.pm/api-reference.html

## What it does

1. [Tokenizer](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/pdf-tokenizer.md) - turns PDF byte streams into structured Elixir tokens.
2. [PDF information](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/pdf-information.md) - reads page geometry and document metadata, detects encryption, and updates common metadata fields.
3. [Merger](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/pdf-merging.md) - combines multiple PDF binaries into a fresh PDF with rewritten object references.
4. [Page transforms and splitting](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/pdf-page-transforms.md) - rebuild PDFs after selecting, reordering, deleting, rotating, or splitting pages.
5. [Outlines and bookmarks](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/pdf-outlines.md) - read or replace bookmarks, detect headings in existing PDFs, and generate bookmarks from HTML headings. Merging, transforms, and splitting preserve supported outlines.
6. [Stamping and page numbers](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/pdf-stamping.md) - adds text stamps, translucent watermarks, page numbers, and PDF artwork overlays to existing PDFs through incremental updates.
7. [Reader](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/pdf-reader.md) and [text extraction](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/text-extraction.md) - strictly resolve embedded Unicode text from classic and modern PDFs.
8. [HTML to PDF](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/html-to-pdf-compatibility.md) - renders a strict, document-oriented HTML/CSS subset to native PDF bytes, with [examples](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/html-to-pdf-examples.md) for common workflows. It supports configured fonts and cross-platform discovery of installed system fonts. Rendering does not require Chromium, wkhtmltopdf, Node, SaaS calls, or a Rust toolchain. Local asset loading works on Windows, macOS, and Linux.

9. [PDF forms](docs/pdf-forms.md) - generates named AcroForm fields from HTML by default, inspects and fills existing fields, and flattens selected fields.
10. [Attachments](docs/pdf-attachments.md) - embeds caller-approved file bytes with MIME detection and lists embedded metadata.

See the [documentation](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/tree/main/docs) for feature guides, configurable resource limits, and quick reference.

## Installation

```elixir
def deps do
  [
    {:native_elixir_pdf_utilities, "~> 0.19.0"}
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
