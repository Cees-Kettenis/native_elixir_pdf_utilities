# Roadmap

Native Elixir PDF Utilities renders application documents and edits existing
PDFs. Upcoming releases focus on broader image support, closer browser rendering,
more efficient document generation, and a stable public API.

The versions below describe planned scope, which may change as the work develops.
See [CHANGELOG.md](CHANGELOG.md) for published releases.

## 0.18.0: Forms and attachments

Create PDF forms from supported HTML controls, inspect and fill existing fields,
and flatten selected fields into document content. Static rendering will remain
available when interactive fields are not needed.

This release also introduces embedding and listing attachments, with documented
file-type handling, size limits, and supported document structures.

## 0.19.0: Image support, SVG safety, and resource limits

Use a wider range of image assets and get clearer failures when documents exceed
supported formats or resource limits.

Planned improvements:

- Broader PNG decoding, including greyscale, indexed palettes, greyscale with
  alpha, low-bit-depth and 16-bit images, and Adam7 interlacing.
- Transparency handling across the additional PNG formats.
- Isolated SVG rasterization with limits on input size, image dimensions,
  rendering complexity, memory, processing time, concurrency, and output size.
  Worker requirements and deployment support will be documented.
- Resource-limit defaults reviewed for server workloads, with guidance on
  configuration, timeouts, cancellation, and cleanup.
- More complete error and recovery documentation, plus clearer guidance on
  application-facing APIs and advanced building blocks.

SVG file size is only one part of the planned protection: a small SVG can still
require expensive rendering. Limits before rasterization and isolation during
rendering will address different parts of that risk.

General color-profile support remains outside the planned pre-1.0 scope.

## 0.20.0: Browser parity below 1%

Bring native PDF output closer to Chromium for supported document layouts,
including text placement, wrapping, tables, borders, and images.

The target is **less than 1% differing pixels** on every reference fixture's
worst page. Feasibility will be assessed when work on this release begins; the
target may need further discussion based on the remaining rendering differences.

Comparisons will retain the existing fixtures, page-count and color-difference
checks, and 72 DPI rasterization. Results will identify the browser, fonts, and
rasterizer used, so the measurements have a clear scope. This target does not
imply support for every browser feature or identical output with arbitrary fonts.

## 0.21.0: Performance and archival output

Generate larger documents with less repeated processing and smaller output files,
while preserving layout and rendering quality.

Planned improvements:

- More efficient text layout, repeated measurements, table sizing, and font and
  document-resource handling.
- Compression of remaining uncompressed PDF streams, safe removal of unused
  objects, and optional font subsetting to reduce file size.
- Best-effort archival output with embedded fonts, metadata, and documented
  handling of transparency and unsupported features.

The archival mode will not claim PDF/A conformance. Full PDF/A validation remains
an objective for a future version 2.

## 0.22.0: Guides and examples

A final documentation review before 1.0 will make the supported workflows,
examples, and limitations consistent with the library's behavior.

The review will cover rendering, forms, attachments, inspection, extraction,
metadata, page transforms, stamping, and error handling. It will also address
outdated examples, unclear unsupported-feature guidance, and navigation between
the README and HexDocs.

## 0.23.0: Release candidate and API freeze

A release candidate for validating the library with real documents and downstream
applications before 1.0.

Public module names, functions, options, return values, and error shapes will be
reviewed and frozen except for bug fixes. Migration notes and remaining guides
will explain how to adopt the candidate and which advanced interfaces may still
change.

This stage will also address release-candidate regressions and settle whether
additional typography support is required for 1.0.

## 1.0.0: Stable release

A stable public API with compatibility guides, application examples, and documented
HTML/CSS and PDF workflow boundaries.

After 1.0, releases will follow SemVer: minor versions add compatible features,
patch versions fix bugs, and major versions introduce intentional breaking changes.

## Beyond the planned releases

Broader Unicode line breaking, soft hyphens, and optional hyphenation dictionaries
remain under consideration for the pre-1.0 scope.

Right-to-left layout, bidirectional text, complex-script shaping, and emoji
sequences are likely post-1.0 work. Automatic PDF field detection and
coordinate-based form conversion also remain deferred.

Remote asset fetching will remain the application's responsibility. The renderer
accepts supported data URIs, approved local assets, caller-provided bytes, and
asset resolver callbacks, allowing applications to control how external content
is fetched and approved.
