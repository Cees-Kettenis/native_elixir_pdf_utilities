# Roadmap

Native Elixir PDF Utilities renders application documents and edits existing
PDFs. Upcoming releases focus on broader image support, closer browser rendering,
more efficient document generation, and a stable public API.

The versions below describe planned scope, which may change as the work develops.
See [CHANGELOG.md](CHANGELOG.md) for published releases.

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
