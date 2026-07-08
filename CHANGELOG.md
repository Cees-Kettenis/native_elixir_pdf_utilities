# Changelog

## 0.3.0 - 2026-07-08

### Added

- Added `NativeElixirPdfUtilities.HtmlToPdf`, a native HTML/CSS to PDF renderer for document-oriented templates.
- Added support for common document HTML including text, headings, paragraphs, spans, lists, links, tables, images, and nested document structure.
- Added support for common print-oriented CSS including cascade handling, box model sizing, text styling, borders, backgrounds, tables, flexbox, grid, page sizes, page breaks, and `@media print` behavior.
- Added embedded image, SVG rasterization, and custom TTF font rendering support for generated PDFs.
- Added multi-page pagination and PDF writing for rendered HTML documents.
- Added detailed render diagnostics for invalid HTML, unsupported HTML, invalid CSS, invalid layout, and invalid document failures.
- Added fixture coverage for purchase orders, material requisitions, stock stickers, and trim cards with realistic scrambled data.
- Added dedicated HTML-to-PDF compatibility and examples documentation.

### Changed

- Updated the package description to include native HTML/CSS rendering.
- Updated HexDocs metadata to include the HTML-to-PDF guides.

## 0.2.0 - 2026-07-03

### Added

- Added embedded text extraction so callers can consume readable text data from PDF binaries.

### Changed

- Refactored tokenizer, merge, and text internals to use explicit `case`/`cond` branching instead of guarded multi-head private functions.
- Split tests into focused tokenizer, merge, and text suites.
- Improved package documentation and HexDocs metadata for the release.

### Fixed

- Fixed page dictionary rewriting around empty arrays and MediaBox validation.
- Added 100% test coverage across the current public library modules.

## 0.1.0 - 2025-09-08

### Added

- Initial PDF tokenizer.
- Initial PDF merge utility.
