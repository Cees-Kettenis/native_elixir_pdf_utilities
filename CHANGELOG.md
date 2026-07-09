# Changelog

## 0.4.0 - 2026-07-09

### Added

- Added a Chromium-backed browser parity test suite for the supported HTML/CSS rendering surface.
- Added browser parity fixtures for common layout, CSS cascade, tables, flexbox, grid, pagination, image, link, unit, and production-document scenarios.
- Added browser parity coverage documentation so supported renderer behavior is tied to explicit fixtures.

### Changed

- Improved HTML-to-PDF browser accuracy for nested table, flexbox, and grid compositions.
- Improved collapsed table border sizing and painting to better match browser output.
- Improved `@page` handling in parity tests so native and Chromium renders use the same page size and margins.
- Updated contribution guidance to require focused tests and browser parity coverage for visible HTML-to-PDF feature work.

### Fixed

- Fixed CSS custom property resolution inside supported compound values such as padding and side-specific borders.
- Fixed `box-sizing: border-box` handling across block, flex, grid, table, and image layout paths.
- Fixed table layout inside flex and grid items, and flex layout directly inside table cells.
- Fixed declared table row heights and pagination metadata propagation for table rows.
- Fixed pagination edge cases around first-page parent padding, overlapping parent/child groups, and zero-height metadata groups.

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
