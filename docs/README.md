# Documentation

Native Elixir PDF Utilities provides native building blocks for reading,
merging, extracting text from, and generating PDF documents. The guides below
describe the supported behavior and the boundaries that application code
should account for.

## Reading and manipulating PDFs

- [PDF Reader](pdf-reader.md) describes the shared document model, supported
  cross-reference and object structures, stream decoding, limits, and errors.
- [Text Extraction](text-extraction.md) covers reconstructed strings and
  page-preserving positioned spans, including ordering, coordinates, font
  context, and rendering modes.
- [PDF Merging](pdf-merging.md) covers supported inputs, output behavior,
  diagnostics, and document-level features that are not preserved.
- [Diagnostics](diagnostics.md) documents the shared recoverable error contract
  used by the public APIs.

## Generating PDFs from HTML

- [HTML to PDF Examples](html-to-pdf-examples.md) provides runnable rendering,
  file output, asset, font, and diagnostic examples.
- [HTML to PDF Compatibility](html-to-pdf-compatibility.md) is the supported
  HTML, CSS, option, layout, and known-limitations reference.
- [HTML to PDF Browser Parity Coverage](html-to-pdf-browser-parity-coverage.md)
  maps documented renderer behavior to Chromium comparison fixtures.

## Project references

- [Changelog](../CHANGELOG.md) records released behavior and compatibility
  changes.
- [Roadmap](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/ROADMAP.md)
  describes the planned path toward `1.0.0`.
- [Contributing](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/CONTRIBUTING.md)
  explains local development and the quality gates required for changes.
