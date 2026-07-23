# Roadmap

This roadmap describes the planned path toward Native Elixir PDF Utilities
`1.0.0`.

The project should keep its current identity: a native, predictable PDF toolkit
for app-generated documents, not a full browser engine. The highest-value path
is to improve common document workflows first:

- render existing invoice, label, report, and statement templates with fewer
  edits
- support predictable print-oriented layout features
- add PDF manipulation tools that applications commonly need after rendering
- keep unsafe or network-dependent behavior explicit and opt-in

Milestones may move as implementation details become clearer. Before `1.0.0`,
breaking API changes are allowed, but they should be explained clearly in
`CHANGELOG.md`.

## Milestones

### 0.8.0 - Running Headers, Footers, and Page Numbers

Milestone goal: add first-class repeated page furniture for reports, invoices, statements,
contracts, and exports.

#### Scope

- Add running headers and footers during HTML-to-PDF rendering.
- Add page numbers during HTML-to-PDF rendering.
- Add page number tokens for current page and total pages.
- Add options for first-page-only or except-first-page headers and footers.
- Add options for odd and even page headers and footers.
- Evaluate `position: fixed` for repeated page furniture; implement it here if
  it fits the renderer model, otherwise document it as deferred.

#### Design Notes

- Document header, footer, page number, and page furniture behavior.

#### Completion Criteria

- Add focused layout, pagination, and PDF writer tests for page furniture.
- Add or update browser parity fixtures for headers, footers, and page numbers.

### 0.9.0 - Generated Content, Selectors, and Counters

Milestone goal: expand the CSS features commonly used by existing document templates.

#### Scope

- Add generated content support for `::before`, `::after`, `content`, and
  `attr(...)`.
- Add common document-template selectors:
  - `[attr]`
  - `[attr=value]`
  - `:not(...)`
  - `:nth-child(odd)`
  - `:nth-child(even)`
  - `:first-of-type`
  - `:last-of-type`
- Add basic CSS counter support for document numbering:
  - `counter-reset`
  - `counter-increment`
  - `counter(name)`
  - basic section, figure, table, and appendix numbering through generated
    content
  - list numbering integration if it fits the existing list model cleanly
  - `counters(name, separator)` if practical
  - `counter(page)` and `counter(pages)` only if this fits the pagination model
    cleanly; otherwise keep page numbering in the header/footer token API
- Correct documented CSS text whitespace behavior for inline layout:
  - Preserve line breaks for `white-space: pre-line`.
  - Normalize CRLF, CR, and LF sequences consistently before layout.
  - Keep default HTML text behavior browser-compatible, where raw whitespace
    collapses unless CSS or explicit `<br>` elements preserve it.
  - Document that literal escaped sequences such as `\\n` and `\\r\\n` are plain
    text unless the caller decodes them before rendering.

#### Design Notes

- Keep unsupported selector diagnostics clear and strict.
- Distinguish browser-compatible HTML whitespace collapsing from supported CSS
  preservation modes such as `white-space: pre-line`.

#### Completion Criteria

- Add focused parser, style, layout, and PDF writer tests for generated content,
  selectors, and counters.
- Add focused parser, style, layout, and PDF writer tests for CSS text whitespace
  behavior, including LF, CRLF, CR, `<br>`, and literal escaped newline
  sequences.
- Add browser parity fixtures for visible generated content and counter output.
- Add or update browser parity fixtures for visible `white-space: pre-line`
  rendering.

### 0.10.0 - Static HTML Form Rendering

Milestone goal: render common HTML form controls as static PDF content for government
forms, applications, inspections, and contracts.

#### Scope

- Add static rendering for HTML form controls:
  - `input type="text"` renders the `value` attribute as visible text
  - `input type="checkbox"` renders checked state from `checked`
  - `input type="radio"` renders checked state from `checked`
  - `select` renders the selected option text from `selected`
  - `textarea` renders its text content or value as visible text
  - `button` where it appears in existing templates

#### Design Notes

- Document that HTML form controls render as static content only, not as
  interactive PDF form fields.
- Do not add special disabled-state rendering before `1.0.0`; callers can use
  normal CSS classes or attributes if they need disabled-looking output.

#### Completion Criteria

- Add parser, style, layout, and PDF writer tests for static form controls.
- Add browser parity fixtures for visible static form rendering.
- Add or update a realistic government-style form fixture.

### 0.11.0 - Positioning, Image Fitting, and Asset Inputs

Milestone goal: support fixed-size template regions while keeping asset handling explicit
and safe.

#### Scope

- Add limited absolute positioning:
  - `position: absolute`
  - `top`
  - `right`
  - `bottom`
  - `left`
  - `z-index`
- Add browser-like image fitting:
  - `object-fit: contain`
  - `object-fit: cover`
  - `object-position`
- Add local background image support:
  - `background-image`
  - `background-size`
  - `background-position`
  - `background-repeat`
- Remove any existing built-in remote asset fetching behavior from the renderer.
- Add explicit asset inputs that consume caller-provided bytes or approved local
  paths instead of fetching remote URLs directly.
- Consider a caller-provided asset resolver callback that returns bytes for an
  asset reference, while keeping all network access outside the library.

#### Design Notes

- Clarify supported positioning boundaries in the compatibility guide.
- Keep remote background images under the same safety model as normal images:
  the renderer may consume bytes supplied by the caller, but must not fetch them.
- Reject remote asset URLs by default with clear diagnostics.
- Do not provide built-in HTTP fetching for fonts, images, stylesheets, or other
  remote assets.

#### Completion Criteria

- Add layout and PDF writer tests for absolute positioning, z-index, image
  fitting, and background painting.
- Add browser parity fixtures for visible layout and painting changes.
- Add or update realistic invoice, statement, and multi-page report fixtures.

### 0.12.0 - PDF Information and Metadata

Milestone goal: expose reliable document inspection and metadata operations on top of the
shared PDF reader.

#### Scope

- Add PDF information helpers:
  - page count
  - page sizes
  - title
  - author
  - producer
  - creation date
  - modification date
  - encryption status
- Add metadata writing for common document fields:
  - title
  - author
  - subject
  - keywords
  - producer
  - creation date
  - modification date

#### Design Notes

- Keep information and metadata APIs binary-in, structured-data-out where
  practical.
- Document behavior for malformed, encrypted, compressed, or unsupported PDFs.

#### Completion Criteria

- Add unit tests for page tree traversal, metadata parsing, metadata writing,
  encryption status, and diagnostic failures.
- Preserve 100% test coverage for public modules.

### 0.13.0 - Page Transforms and Document Assembly

Milestone goal: let applications assemble, split, rearrange, and rotate PDF documents
after rendering or receiving them.

#### Scope

- Add PDF splitting by page range or individual page.
- Add page picking so callers can reorder or delete pages.
- Add page rotation for selected pages.

#### Design Notes

- Keep transform APIs binary-in, binary-out where practical.
- Document page numbering conventions clearly.

#### Completion Criteria

- Add unit tests for page range validation, page picking, deletion, and rotation.
- Add regression fixtures for realistic merged and multi-page PDFs.

### 0.14.0 - Bookmarks and Outlines

Milestone goal: support navigation metadata for generated reports and assembled document
packets.

#### Scope

- Add bookmarks and outlines for generated or transformed PDFs.
- Add outline preservation where practical when merging or transforming PDFs.

#### Design Notes

- Document outline creation, preservation, and loss cases.

#### Completion Criteria

- Add unit and fixture tests for outline creation and outline preservation.

### 0.15.0 - Stamping and Existing-PDF Page Numbers

Milestone goal: add common overlay workflows for drafts, approvals, internal documents,
branded output, and assembled packets.

#### Scope

- Add text stamping and watermarking for existing PDFs.
- Add PDF overlay stamping for existing PDFs.
- Add page numbers to existing PDFs with configurable format, position, font,
  size, color, and page ranges.

#### Design Notes

- Document stamp coordinate systems and page range behavior.

#### Completion Criteria

- Add unit and fixture tests for stamping and page numbering.
- Add visual regression coverage for stamping and watermarking behavior.

### 0.16.0 - PDF Forms and Attachments

Milestone goal: support common operational document workflows involving existing PDF
forms and bundled sidecar files.

#### Scope

- Add AcroForm filling.
- Add optional AcroForm flattening.
- Add embedded file attachments.

#### Design Notes

- Keep form and attachment APIs explicit about unsupported PDF structures.
- Document the difference between static HTML form rendering and AcroForm
  filling.

#### Completion Criteria

- Add unit and fixture tests for form filling, flattening, and attachments.
- Verify form-filled PDFs remain readable by the shared PDF reader.

### 0.17.0 - Resource Limits and API Boundary

Milestone goal: harden the library behavior that will be difficult to change after
`1.0.0`.

#### Scope

- Finalize the stable error contract across public APIs:
  - no surprising raises for normal invalid input
  - consistent `{:ok, value} | {:error, {reason, detail}}` return shapes where
    diagnostic detail is available
  - documented reason atoms and diagnostic maps
- Add an error reference guide that lists public error return shapes and reason
  atoms by module.
- Add resource limit and safety controls for renderer and PDF utility APIs:
  - maximum input size
  - maximum page count
  - maximum image dimensions
  - maximum rendered pages
  - timeout or cancellation guidance
- Provide conservative default resource limits suitable for server-side use, with
  explicit configuration options so applications can raise, lower, or disable
  limits where appropriate.

#### Design Notes

- Audit all public APIs against the diagnostic contract defined earlier in the
  roadmap and fix modules that still return vague or inconsistent errors.
- Explicitly classify modules as stable public APIs, advanced public APIs, or
  internal implementation modules.
- Classify the normal app-facing modules as stable public APIs:
  - `NativeElixirPdfUtilities.HtmlToPdf`
  - `NativeElixirPdfUtilities.Merge`
  - `NativeElixirPdfUtilities.Text`
  - `NativeElixirPdfUtilities.Tokenizer`
  - future `Info`, `Transform`, `Split`, `Stamp`, `Forms`, `Attachments`,
    `Optimize`, and `Metadata` modules
- Classify parser and pipeline building blocks as advanced public APIs:
  - `NativeElixirPdfUtilities.HtmlToPdf.HtmlParser`
  - `NativeElixirPdfUtilities.HtmlToPdf.CssParser`
  - `NativeElixirPdfUtilities.HtmlToPdf.Style`
  - `NativeElixirPdfUtilities.HtmlToPdf.Layout`
  - `NativeElixirPdfUtilities.HtmlToPdf.Pagination`
  - `NativeElixirPdfUtilities.HtmlToPdf.Font`
  - future low-level PDF reader and object resolver modules
- Classify PDF serialization and implementation helpers as internal unless
  there is a strong user-facing reason to stabilize them.
- Keep parser-style modules open for advanced users while documenting which
  return shapes are stable and which data structures may change before `1.0.0`.
- Add a public API boundary guide that explains stable public modules, advanced
  public modules, and internal implementation modules.
- Document unsupported features with practical migration advice, such as
  pre-rendering dynamic values instead of relying on JavaScript and using local
  asset paths instead of remote assets by default.

#### Completion Criteria

- Add tests for invalid input across public APIs.
- Add tests for resource limits and safety failures.
- Add documentation examples for stable and advanced public APIs.
- Add doctests for public examples where they can run without external browser
  tooling.

### 0.18.0 - Optimization and Archive-Friendly Output

Milestone goal: improve output size and archive readiness without claiming full PDF/A
compliance before the library can validate it properly.

#### Scope

- Add conservative compression and optimization:
  - compress uncompressed streams
  - deduplicate repeated images where safe
  - remove unreachable objects where safe
  - optionally subset embedded fonts
- Add best-effort archive-friendly output mode:
  - embed required fonts
  - avoid unsupported transparency where practical
  - include document metadata
  - expose best-effort diagnostics

#### Design Notes

- Ensure optimization does not intentionally change visual output.
- Document the difference between archive-friendly output and full PDF/A
  compliance.
- Treat strict PDF/A validation and claims of fully validated archival output as
  a future version 2 objective, not a `1.0.0` requirement.

#### Completion Criteria

- Add optimization fixture tests that confirm output remains readable.
- Add fixture coverage for best-effort archive-friendly output.

### 0.19.0 - Documentation, Fixtures, and Release Polish

Milestone goal: remove small documentation and example friction before the release
candidate.

#### Scope

- Add a changelog discipline section that explains how pre-`1.0.0` breaking
  changes are recorded.
- Add final production-style fixture coverage for common business documents that
  were not already represented by browser parity fixtures.
- Add final examples for metadata, static form rendering, PDF inspection,
  transforms, stamping, and error handling.

#### Design Notes

- Tighten README and HexDocs navigation so compatibility, examples, browser
  parity coverage, roadmap, and changelog are easy to find.
- Review all unsupported-feature documentation and add caller-side alternatives
  where useful.
- Review generated documentation for stale option names, old module names, and
  examples that no longer match stable return shapes.

#### Completion Criteria

- Run doctests and normal tests after documentation example updates.
- Run browser parity tests for any fixture or visible rendering documentation
  changes.
- Confirm README, HexDocs guide links, changelog links, and roadmap links are
  valid.

### 0.20.0 - Release Candidate and API Freeze

Milestone goal: stop expanding scope and harden the public API before `1.0.0`.

#### Scope

- Add final migration notes for any pre-1.0 breaking changes.
- Add examples for the stable renderer, merge, text extraction, inspection, and
  transform workflows.
- Add missing HexDocs guides for the stable public API.

#### Design Notes

- Freeze public module names, function names, option names, return values, and
  error shapes except for bug fixes.
- Normalize diagnostics across tokenizer, merge, text extraction, HTML-to-PDF,
  and PDF transform APIs.
- Review whether advanced typography work is required before `1.0.0`.
- Confirm documentation links and file names are stable before release.

#### Stabilization Work

- Fix release-candidate bugs found by real fixtures and downstream usage.
- Remove or clearly document unstable internals that should not be treated as
  public API.

#### Completion Criteria

- Run the complete quality gate:

  ```bash
  mise exec -- mix test
  MIX_ENV=test mise exec -- mix dialyzer
  mise exec -- mix format
  mise exec -- mix test --cover
  CHROMIUM_BIN=/usr/bin/chromium mise exec -- mix test.browser_parity
  ```

### 1.0.0 - Stable Public API

Milestone goal: publish the first stable release that users can depend on.

#### Scope

- Declare the supported public API stable.
- Publish final `1.0.0` documentation and compatibility guides.
- Publish final examples for common app workflows.

#### Design Notes

- Follow SemVer for future releases:
  - `1.1.0` for backwards-compatible features
  - `1.1.1` for bug fixes
  - `2.0.0` for intentional breaking public API changes

#### Completion Criteria

- Confirm core rendering, merge, text extraction, tokenizer, inspection, and
  transform APIs are settled.
- Confirm supported HTML/CSS behavior is documented and covered by focused tests.
- Confirm browser parity fixtures cover visible HTML-to-PDF behavior promised by
  the compatibility docs.
- Confirm diagnostics and error shapes are unlikely to change casually.
- Confirm the full quality gate passes.

## Typography Track

Text rendering should improve in stages, but not every typography feature has to
block `1.0.0`.

Pre-1.0 candidates:

1. Better Unicode line breaking.
2. Soft hyphen support.
3. Optional hyphenation dictionaries.

Likely post-1.0 candidates:

1. `direction: rtl`.
2. Basic bidirectional text layout.
3. Optional complex shaping for Arabic, Indic scripts, Thai, emoji sequences,
   and other advanced typography.

## Remote Asset Policy

The renderer should consume assets; it should not fetch assets from the network.
This keeps HTTP policy, credentials, allowlists, redirects, timeouts, retries,
caching, observability, and security review inside the caller's application.

Supported asset inputs should be explicit and local:

- local file paths approved by the caller
- data URIs where already supported
- caller-provided asset bytes with content type metadata
- optional asset resolver callbacks that return bytes without the library owning
  any network access

Remote URLs in HTML or CSS should be rejected by default with clear diagnostics.
If an application needs remote fonts, images, stylesheets, or background assets,
it should fetch and validate them itself, then pass local paths, data URIs, or
bytes into the renderer.
