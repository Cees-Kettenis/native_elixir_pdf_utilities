# Documentation

Native Elixir PDF Utilities inspects, updates, merges, transforms, splits,
extracts text from, and generates PDF documents. These guides document what
each public API supports and which work remains the caller's responsibility.

## Reading and manipulating PDFs

- [PDF tokenizer](pdf-tokenizer.md) explains lexical tokens, byte spans, stream
  length hints, and the boundary between tokenization and document parsing.
- [PDF reader](pdf-reader.md) describes the shared document model, supported
  cross-reference and object structures, stream decoding, limits, and errors.
- [PDF information and metadata](pdf-information.md) covers document
  information, page count and geometry, encryption status, and incremental
  metadata updates.
- [Text extraction](text-extraction.md) covers reconstructed strings and
  page-preserving positioned spans, including ordering, coordinates, font
  context, and rendering modes.
- [PDF merging](pdf-merging.md) covers supported inputs, output behavior,
  diagnostics, and document-level features that are not preserved.
- [PDF page transforms and splitting](pdf-page-transforms.md) covers selecting,
  reordering, deleting, rotating, and splitting pages, including rebuild
  behavior and data-retention limitations.
- [PDF outlines and bookmarks](pdf-outlines.md) covers exact outline updates,
  best-effort detection, HTML headings, and preservation during assembly.
- [Diagnostics](diagnostics.md) explains why public APIs share one recoverable
  error shape and how callers can use its debugging context.
- [Configurable resource limits](resource-limits.md) lists every tunable
  parsing, information, rendering, extraction, merge, transform, split, and
  cache ceiling and its default.

## Generating PDFs from HTML

- [HTML to PDF examples](html-to-pdf-examples.md) provides short, runnable
  examples for common rendering workflows.
- [HTML to PDF compatibility](html-to-pdf-compatibility.md) is the supported
  options, HTML, CSS, rendering behavior, and known limitations reference.
- [HTML to PDF browser parity coverage](html-to-pdf-browser-parity-coverage.md)
  explains the Chromium comparison suite and maps behavior to its fixtures.

## Project references

- [Licenses](licenses.md) contains the MIT, Bitstream Vera, WHATWG, and Adobe
  license texts used by the project, bundled fonts, and derived data.
- [Changelog](../CHANGELOG.md) records released behavior and compatibility
  changes.
- [Roadmap](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/ROADMAP.md)
  describes the planned path toward `1.0.0`.
- [Contributing](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/CONTRIBUTING.md)
  explains local development and the quality gates required for changes.
