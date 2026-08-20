# Documentation

Native Elixir PDF Utilities reads, merges, extracts text from, and generates
PDF documents. These guides document what each public API supports and which
work remains the caller's responsibility.

## Reading and manipulating PDFs

- [PDF tokenizer](pdf-tokenizer.md) explains lexical tokens, byte spans, stream
  length hints, and the boundary between tokenization and document parsing.
- [PDF reader](pdf-reader.md) describes the shared document model, supported
  cross-reference and object structures, stream decoding, limits, and errors.
- [Text extraction](text-extraction.md) covers reconstructed strings and
  page-preserving positioned spans, including ordering, coordinates, font
  context, and rendering modes.
- [PDF merging](pdf-merging.md) covers supported inputs, output behavior,
  diagnostics, and document-level features that are not preserved.
- [Diagnostics](diagnostics.md) documents the shared recoverable error contract
  used by the public APIs.
- [Configurable resource limits](resource-limits.md) lists every tunable
  parsing, rendering, extraction, merge, and cache ceiling and its default.

## Generating PDFs from HTML

- [HTML to PDF examples](html-to-pdf-examples.md) provides runnable rendering,
  file output, asset, font, and diagnostic examples.
- [HTML to PDF compatibility](html-to-pdf-compatibility.md) is the supported
  HTML, CSS, option, layout, and known-limitations reference.
- [HTML to PDF browser parity coverage](html-to-pdf-browser-parity-coverage.md)
  maps documented renderer behavior to Chromium comparison fixtures.

## Project references

- [Licenses](licenses.md) contains the MIT, Bitstream Vera, WHATWG, and Adobe
  license texts used by the project, bundled fonts, and derived data.
- [Changelog](../CHANGELOG.md) records released behavior and compatibility
  changes.
- [Roadmap](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/ROADMAP.md)
  describes the planned path toward `1.0.0`.
- [Contributing](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/CONTRIBUTING.md)
  explains local development and the quality gates required for changes.
