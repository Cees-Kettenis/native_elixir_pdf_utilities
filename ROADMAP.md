# Roadmap

Native Elixir PDF Utilities renders application documents and edits existing
PDFs. The next milestone prepares the existing functionality and public API for
the first stable release.

The versions below describe planned scope, which may change as the work develops.
See [CHANGELOG.md](CHANGELOG.md) for published releases.

## 1.0.0-rc.1: Documentation, API freeze, and release validation

This combines the previously planned 0.22.0 documentation review and 0.23.0
release-candidate work into one milestone. It prepares the supported workflows
for 1.0 without adding another feature milestone.

Before publishing the candidate:

- [ ] Review guides and examples for rendering, forms, attachments, inspection,
  extraction, metadata, page transforms, stamping, and error handling. Fix
  outdated examples and navigation between the README and HexDocs.
- [ ] Review and freeze public module names, functions, options, return values,
  and diagnostic shapes. Document which advanced interfaces are covered by the
  stability promise.
- [ ] Document supported HTML/CSS and PDF workflow boundaries, known limitations,
  and any migration steps from 0.21.0.
- [ ] Include the flex image sizing correction and its regression fixtures.
  Retain Chromium comparison in development and CI.
- [ ] Update release automation to accept candidate tags such as
  `v1.0.0-rc.1` and mark their GitHub releases as prereleases without marking them
  as the latest stable release.
- [ ] Pass the full quality matrix, review generated documentation, and inspect
  the package before publishing it to Hex.

After publishing the candidate:

- [ ] Test the published package in SigPortal with its real document templates
  and Chromium fallback disabled. Verify page sizes, pagination, images, text,
  and printed/scanned QR codes where applicable.
- [ ] Fix candidate regressions and publish `1.0.0-rc.2` or later candidates if
  needed, repeating the affected checks and full quality matrix.

## 1.0.0: Stable release

A stable public API with compatibility guides, application examples, and documented
HTML/CSS and PDF workflow boundaries. Publish this once the candidate checklist
is complete and no known regression blocks the supported workflows. Existing
production use contributes to that decision; a fixed waiting period is not
required.

Version 1.0 establishes the documented compatibility promise. It does not require
support for every browser feature or the absence of all future rendering bugs.
Applications can remove Chromium from production after validating their own
templates. Chromium remains the rendering reference for the library's parity
tests.

After 1.0, releases will follow SemVer: minor versions add compatible features,
patch versions fix bugs, and major versions introduce intentional breaking changes.

## After 1.0

Broader Unicode line breaking, soft hyphens, and optional hyphenation dictionaries
are deferred to later releases unless candidate testing identifies a concrete
requirement for an already supported workflow.

Right-to-left layout, bidirectional text, complex-script shaping, and emoji
sequences remain future work. Automatic PDF field detection and
coordinate-based form conversion also remain deferred.

Remote asset fetching will remain the application's responsibility. The renderer
accepts supported data URIs, approved local assets, caller-provided bytes, and
asset resolver callbacks, allowing applications to control how external content
is fetched and approved.
