# Changelog

## 0.10.0 - 2026-08-14

### Added

- Added layered validator modules for shared PDF structure, text extraction,
  merging, HTML rendering, and PDF writing, with prepared contexts that keep
  validation separate from execution and serialization.
- Added `NativeElixirPdfUtilities.Pdf.Reader.read_validated/1` for consumers
  that need the complete shared PDF validation context while preserving the
  existing `read/1` document projection.
- Added recoverable unsupported-glyph rendering. Missing glyphs now use the
  Unicode replacement character by default, while `unsupported_glyphs: :error`
  retains strict failure behavior with an actionable diagnostic.
- Added HTML table column definitions through `colgroup` and `col`, including
  column spans, percentage widths, fixed table layout, and separate and
  collapsed border-spacing behavior.
- Added documentation for the layered PDF validation pipeline and browser
  parity fixtures for table column layout, repeated headers near page breaks,
  border-box sizing, and unsupported-glyph replacement.

### Changed

- Centralized caller input, option, document-structure, and semantic validation
  in dedicated validators. PDF reading, text extraction, merging, HTML layout,
  pagination, font fallback, and writing now consume validated and normalized
  data instead of repeating validation rules in execution code.
- Separated normalization from validation throughout the HTML renderer so
  downstream layout and writing stages can rely on stable normalized values.
- Changed text resource preparation to validate only fonts, encodings, CMaps,
  graphics states, and Form XObjects reached by executable page content.
- Changed the HTML renderer and text extraction APIs to reject unknown options
  with the shared actionable diagnostics contract.

### Fixed

- Fixed PDF validation for page-tree depth limits, `/Parent` relationships,
  page aliases, null-valued optional entries, malformed object records, and the
  required free object-zero cross-reference entry.
- Fixed stream loading for indirect lengths in ordinary and cross-reference
  streams, including compressed length objects and stream-boundary ambiguity.
- Fixed text extraction from ExtGState font selections and Form XObjects with
  indirect transformation matrices, and rejected unbalanced graphics or text
  scopes with diagnostics instead of unsafe execution.
- Fixed merge validation for page resources, terminal page references,
  top-level page rewrites, exact object generations, and complete indirect
  reference remapping.
- Fixed resource bounds for predictor row allocation, CID font width expansion,
  and page-tree traversal.
- Fixed repeated table headers overflowing page bounds and page furniture
  bounds omitting line height.
- Fixed auto-sized `border-box` elements losing horizontal padding and bounded
  emitted PDF color channels to their valid range.

## 0.9.0 - 2026-08-07

### Added

- Added generated CSS content for `::before` and `::after`, including quoted
  `content` fragments and `attr(...)` values.
- Added attribute-presence and attribute-equality selectors, `:not(...)`,
  odd/even `:nth-child(...)`, and `:first-of-type` and `:last-of-type`
  selectors for document templates.
- Added named CSS counters through `counter-reset`, `counter-increment`, and
  `counter(name)`, including multiple counters and explicit integer values.
- Added browser-parity fixtures for generated content, counters, the expanded
  selector set, and `white-space: pre-line` rendering.
- Added a local quality matrix and matching GitHub Actions coverage for the
  supported Elixir 1.19 and 1.20 runtimes. The matrix checks compilation,
  formatting, unused dependencies, 100% test coverage, Dialyzer, and Chromium
  browser parity.
- Added code-style regression checks for the library's documented public API
  and function-head conventions.

### Changed

- Changed the minimum supported Elixir version from 1.18 to 1.19.
- Changed inline whitespace layout to collapse ordinary HTML whitespace across
  element boundaries, preserve normalized CRLF, CR, and LF line breaks with
  `white-space: pre-line`, retain explicit `<br>` breaks, and treat literal
  escaped newline sequences as text.
- Updated GitHub Actions dependencies to Node 24-compatible releases.
- Refactored HTML-to-PDF parser, style, layout, pagination, page-geometry, page
  furniture, and PDF visual-comparison internals without changing their public
  APIs.

### Fixed

- Fixed `rgba()`, eight-digit hexadecimal, and `transparent` CSS colors losing
  their alpha values. Text, backgrounds, and borders now write the required PDF
  transparency graphics states, including shaded border variants.
- Fixed PDF merging misclassifying an object as a page when `/Type /Page`
  appeared only in a nested dictionary. Page-tree traversal now uses the shared
  reader's semantic dictionaries and resolves indirect `/Kids` and `/Count`
  values.
- Fixed stream tokenization allowing a nested dictionary's `/Length` to replace
  or leak into the enclosing stream dictionary's length.
- Fixed source-order text extraction inserting spaces between consecutive PDF
  text-showing operations. Operator-defined continuity is now preserved across
  `Tj` and `TJ` operands and exposed through each span's `joins_previous?`
  field, while positioning, text-object, Form XObject, and graphics-matrix
  boundaries still begin separate segments.

## 0.8.0 - 2026-07-30

### Added

- Added opt-in running page furniture through `:page_furniture`, with
  independently configured headers and footers, `:default`, `:first`, `:odd`,
  and `:even` variants, first-page-only and except-first-page behavior, and
  `{{page}}` and `{{pages}}` tokens.
- Added complete supported page geometry shared by renderer options and bare
  `@page` rules: named and explicit page sizes, portrait and landscape forms,
  one-to-four-value margins, margin longhands, asymmetric layout, and explicit
  renderer-option precedence.
- Added bundled DejaVu Sans regular, bold, oblique, and bold-oblique fonts for
  deterministic Unicode glyph fallback after requested and configured font
  families.
- Added browser-parity coverage for page furniture, page-number tokens,
  asymmetric page geometry, paragraph fragmentation, border variants, flex and
  grid constraints, HTML character references, and multi-row table headers.

### Changed

- Changed `:stylesheets` entries to require explicit `{:css, css}` or
  `{:file, path}` tags. Bare strings are rejected so empty or comment-only CSS
  cannot be mistaken for a path and file access is always explicit.
- Changed font loading to reuse successful parsed font files through a
  supervised, bounded, process-wide cache. Cache entries invalidate when file
  metadata changes, concurrent cold loads are deduplicated, failed parses are
  not retained, and no host-application configuration is required.
- Changed the package metadata and documentation to identify the MIT,
  Bitstream Vera, and BSD 3-Clause licenses used by the source, bundled DejaVu
  fonts, and generated WHATWG character-reference data.
- Documented that repeated page furniture uses the explicit renderer option;
  CSS `position: fixed` remains deferred until positioned layout can remove
  elements from normal flow and apply offsets correctly.

### Fixed

- Fixed CSS cascade precedence so stylesheet `!important` declarations beat
  normal inline declarations, while inline `!important` declarations retain
  priority over important stylesheet declarations.
- Fixed table `rowspan` layout to reserve occupied columns and span the combined
  height of all covered rows.
- Fixed paragraphs taller than the remaining or complete printable page being
  clipped or kept together indefinitely; pagination now fragments them at
  complete visual lines, including oversized `break-inside: avoid` paragraphs.
- Fixed paginated tables repeating only one header row; all `<thead>` rows now
  repeat together when the table body continues.
- Fixed Unicode text being rejected or encoded with the wrong face by resolving
  each grapheme through the selected, requested, configured, and bundled
  fallback fonts before layout.
- Fixed text extraction across nested `q` and `Q` operators so saved graphics
  and text state is restored in LIFO order.
- Fixed Type 0 text width calculation by mapping source codes through the
  Encoding CMap before applying descendant CID widths, with strict diagnostics
  and resource limits for unsupported or malformed CMaps.
- Fixed merging pages with inherited or indirect `MediaBox`, `CropBox`,
  `Resources`, `Rotate`, `BleedBox`, `TrimBox`, `ArtBox`, and `UserUnit` values
  by materializing the nearest effective page-tree values.
- Fixed the shared reader accepting duplicate page-tree references or
  inconsistent descendant `/Count` values.
- Fixed ASCII85 decoding of delimiters, whitespace, `z` groups, partial final
  groups, and values that overflow the 32-bit group range.
- Fixed HTML named and numeric character references using the complete WHATWG
  table, including legacy semicolon rules, invalid numeric normalization,
  multi-code-point references, non-breaking spaces, and decode-once behavior.
- Fixed CSS custom properties being resolved before their cascade completed;
  ordinary declarations now use the final winning custom-property values.
- Fixed `none`, `hidden`, `dotted`, `dashed`, `solid`, `double`, `groove`,
  `ridge`, `inset`, and `outset` borders, including independent side styles and
  transparent border spacing.
- Fixed valid page-context declarations being rejected and malformed `size`,
  margin, orientation, marks, bleed, unknown, and incomplete declarations
  lacking actionable CSS diagnostics.
- Fixed zero, negative, or excessive margins being accepted when they leave no
  positive printable page area.
- Fixed merged output corrupting PDF names that require `#xx` escaping,
  including whitespace, delimiters, literal `#`, control bytes, and
  non-printable bytes.
- Fixed unsupported named or pseudo-page selectors and misspelled `@page`
  at-rules being silently applied to every page; they now return strict
  diagnostics.
- Fixed declaration-order-dependent computed CSS values by resolving `em`,
  `rem`, `currentColor`, relative semantic margins, custom properties, and
  inherited line heights against the element's final computed style.
- Fixed grid `minmax()` tracks discarding their minimum and added redistribution
  when fractional tracks reach a minimum bound.
- Fixed flex grow and shrink distribution overwriting item `min-width`,
  `max-width`, `min-height`, and `max-height` constraints.
- Fixed text extraction rejecting valid inherited indirect page `/Rotate`
  values.
- Fixed merge failures replacing the reader's machine-readable reason and
  diagnostic stage; callers can again distinguish encryption, page-tree,
  resource-limit, malformed-input, and unsupported-feature failures.
- Fixed valid empty or comment-only inline stylesheets and paths containing
  braces being misclassified by replacing content-based guessing with explicit
  stylesheet source tags.
- Fixed the public `:default_font` typespec so its documented fallback-list form
  accepts `[String.t()]`.
- Fixed a severe CSS `rem` performance regression that recursively traversed
  complete font registries and decoded runtime payloads for every element.
  Runtime font and image data is now opaque to CSS length resolution, with a
  deterministic reductions-based regression test.

## 0.7.0 - 2026-07-23

### Added

- Added local CSS `@font-face` support for embedded and configured
  stylesheets, including `font-family`, ordered `src: url(...)` fallbacks,
  `font-weight`, `font-style`, and supported `font-display` values.
- Added TrueType font loading from `.ttf` files and `.otf` files that use
  TrueType outlines. Relative font URLs resolve against the configured
  stylesheet directory or renderer `:base_url`.
- Added print media handling for `@media print`, `@media only print`,
  `@media all`, and `@media only all`, while non-print media rules are omitted
  from the active print cascade.
- Added PDF document metadata through the HTML renderer's `:metadata` option
  for title, author, subject, keywords, creation date, and modification date.
  Metadata dates accept `Date`, `NaiveDateTime`, `DateTime`, and ISO 8601
  strings, and non-ASCII values are written as Unicode PDF strings.
- Added automatic PDF title metadata from the first non-empty HTML `<title>`
  when no explicit metadata title is provided.
- Added Chromium parity coverage for CSS-declared fonts and print media, plus
  examples and compatibility documentation for fonts, print CSS, metadata,
  supported formats, URL resolution, and conversion boundaries.

### Changed

- Changed configured stylesheet handling to preserve each file's directory for
  relative assets and to apply configured `@page` rules when deriving default
  page options.
- Changed explicit and CSS-declared font registration to try ordered local
  source candidates until a supported font loads.
- Changed PNG decoding to stream decompression, require the exact expected
  decoded size, and reject images whose decoded scanlines exceed 100 MB.
- Changed RunLengthDecode to operate on binaries and enforce the PDF reader's
  decoded-stream and decompression-ratio limits.
- Changed text extraction to accumulate spans and output text without repeated
  list or binary copying, and capped extraction at 25,000 text spans per page
  with a `:resource_limit_exceeded` diagnostic.
- Changed merge failures to retain the PDF reader's reason and stage in the
  actionable merge diagnostic.

### Fixed

- Fixed merging PDFs that contain unrelated or stale catalog objects by
  resolving the active catalog from the trailer's `/Root` reference.
- Fixed hexadecimal strings with non-hexadecimal bytes being silently
  sanitized; the tokenizer now emits `:invalid_hex_string` with the offending
  byte position.
- Fixed PDF `DateTime` metadata formatting so UTC and non-UTC offsets are
  encoded correctly.
- Fixed CSS `@font-face` parsing for quoted URLs containing commas, ordered
  fallback sources, invalid or missing descriptors, unsupported formats, and
  malformed declarations.
- Fixed CSS diagnostics for malformed font and media rules so rendering returns
  actionable `:invalid_css` details with source, line, and column context.

## 0.6.0 - 2026-07-20

### Added

- Added `NativeElixirPdfUtilities.Pdf.Reader`, a shared PDF document layer with
  classic cross-reference tables, cross-reference streams, object streams,
  incremental and hybrid revision chains, recursive indirect resolution,
  supported stream filters, page-tree validation, and resource limits.
- Added committed reader fixtures for classic, xref-stream, object-stream,
  hybrid, incremental, encrypted, and malformed PDFs.
- Added `Text.extract_spans/2` and `Text.extract_file_spans/2` for
  page-preserving decoded text operations with source indexes, baseline
  coordinates, font and matrix context, and text rendering-mode metadata.
- Added strict Unicode decoding for standard simple-font encodings,
  font-specific `Differences`, Adobe glyph names, Type 0 fonts, and ToUnicode
  CMaps.
- Added PDF reader, text extraction, and merging guides covering supported
  structures, public behavior, diagnostics, limits, and known boundaries.
- Added GitHub Actions checks for compilation warnings, formatting, unused
  dependencies, tests, 100% coverage, Dialyzer, and Chromium browser parity.

### Changed

- Changed text extraction and PDF merging to consume the shared reader model so
  both utilities honor active revisions, generations, free entries, compressed
  objects, and the validated page tree.
- Changed text extraction to reject malformed content and unreliable font
  encodings with actionable diagnostics instead of guessing or returning
  partial text.
- Changed string extraction to project from the same positioned page spans
  while preserving the existing `layout: true` and `layout: false` output.

### Fixed

- Fixed merging pages with malformed inherited `/MediaBox` values by applying
  the default page box.
- Fixed valid cross-reference offsets that point to PDF whitespace immediately
  before an indirect object header being rejected.
- Fixed tokenizer comments ending at end-of-input being emitted as tokens.

## 0.5.1 - 2026-07-10

### Changed

- `Merge.merge/1` now rejects malformed classic PDF input with an
  `:invalid_pdf_input` diagnostic rather than producing an empty PDF or raising.
- `NativeElixirPdfUtilities.Tokenizer` now emits explicit error tokens for
  unterminated literal and hexadecimal strings.
- `Text.extract/2` rejects malformed token streams with an `:invalid_pdf_input`
  diagnostic. It also ignores unusually large ToUnicode CMaps to bound memory
  and CPU use during extraction.

### Fixed

- Fixed merge crashes caused by malformed tokens, incomplete streams, duplicate
  object identifiers, and invalid object identifiers.
- Fixed merged PDFs losing inherited `/MediaBox` and `/Resources` values from
  intermediate `/Pages` tree nodes.
- Fixed remapping of non-page `/Parent` references during a merge.
- Fixed malformed TTF font input causing HTML-to-PDF rendering to crash.
- Reduced avoidable repeated binary and list copying while writing larger PDFs.

## 0.5.0 - 2026-07-10

### Added

- Added `NativeElixirPdfUtilities.Diagnostics` as the shared public diagnostic contract.
- Added standardized diagnostic details for merge, text extraction, HTML rendering,
  pagination, PDF writing, and file failures.
- Added developer guidance for using the shared diagnostics contract in future public APIs.
- Added a diagnostics guide under `docs/`.

### Changed

- Changed `Merge.merge/1` to return diagnostic errors instead of raising for empty input.
- Changed recoverable failures from merge, text extraction, HTML rendering,
  pagination, PDF writing, and file operations to return
  `{:error, {reason, diagnostic}}` with `:stage`, `:reason`, `:message`,
  `:operation`, `:module`, and `:source` context when available.

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
