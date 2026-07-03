# Changelog

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
