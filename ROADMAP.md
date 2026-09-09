# Roadmap

Native Elixir PDF Utilities renders application documents and edits existing
PDFs. The path to 1.0 focuses on common document workflows, predictable output,
and a public API applications can depend on.

The versions below describe planned scope. Implemented work is called out where
it affects what remains; it does not imply that a version has been published.
See [CHANGELOG.md](CHANGELOG.md) for release history.

## 0.17.0: Stamping and page numbers

Add text, watermarks, PDF overlays, and page numbers to existing PDFs.

Implemented behavior includes page selection and configurable placement, fonts,
colors, and number formats. Audit fixes preserve the original page's graphics
state, apply overlay opacity to the whole stamp, and handle indirect content
arrays and stream lengths.

Maintain the coordinate-system and page-range documentation, unit tests, and
visual regressions as later PDF features land.

## 0.18.0: Forms and attachments

Let applications fill existing PDF forms and bundle supporting files.

Work to deliver:

- Fill AcroForm fields.
- Optionally flatten filled fields into page content.
- Embed file attachments.
- Document supported form structures and return diagnostics for unsupported
  ones. Explain how AcroForm filling differs from rendering HTML form controls.

Complete when fixtures cover filling, flattening, and attachments, and the
shared PDF reader can read the resulting documents.

## 0.19.0: Errors, limits, and API boundaries

Make failures predictable and define which APIs applications can rely on.
Recent validation fixes cover individual PDF and image failures. The full
public API review remains open.

Work to deliver:

- Audit public entry points for ordinary invalid input and unsupported features.
  Use the existing `{:error, {reason, diagnostic}}` contract for explainable
  failures. Keep validation in the appropriate validator and avoid raising for
  recoverable caller errors.
- Add an error reference listing return shapes, reason atoms, diagnostic fields,
  and recovery examples by module.
- Add a maximum rendered-page limit for HTML-to-PDF output. Define it in `Limits`
  and enforce validator-owned rules during pagination, including page furniture.
- Review process-wide resource defaults against representative server workloads.
  Keep limits positive and configurable; disabling them remains unsupported.
- Document caller-owned timeout, cancellation, and cleanup patterns.
- Publish an API boundary guide. Classify application-facing renderer and PDF
  operations as stable, parser and pipeline modules as advanced, and serialization
  helpers as internal. State which advanced data structures may still change.

Complete when public failure tests assert actionable diagnostics, resource tests
cover values at and beyond limits, and the guides include executable examples.

## 0.20.0: PNG decoding and CMYK JPEG handling

Render standard PNG variants and recognized four-component JPEGs correctly,
with bounded decoding and clear errors for unsupported or malformed input.

Already implemented:

- 8-bit, non-interlaced RGB and RGBA PNGs. The audit added RGB `tRNS`
  transparent-color masks, validation, and mask memory accounting.
- Adobe CMYK and YCCK polarity recognition. The writer adds the PDF `/Decode`
  inversion array, with real image fixtures and browser comparisons.

### Remaining PNG work

Complete support for this format matrix:

| Color type | Bit depths |
| --- | --- |
| Greyscale | 1, 2, 4, 8, 16 |
| RGB | 8, 16 |
| Indexed color | 1, 2, 4, 8 |
| Greyscale with alpha | 8, 16 |
| RGBA | 8, 16 |

- Decode `PLTE` palettes and all applicable `tRNS` forms. Check palette indexes
  and compare transparent colors at source precision before reducing bit depth.
- Unpack low-bit-depth samples and convert 16-bit samples. Normalize output to
  8-bit RGB with an optional 8-bit alpha mask.
- Add Adam7 interlacing, including pass sizing, unfiltering, and reconstruction.
- Validate every chunk CRC, required chunks, uniqueness, and ordering. Reject
  unknown critical chunks and safely ignore unknown ancillary chunks.
- Preserve dimension, decompression-ratio, decoded-byte, and total-image limits
  across intermediate buffers, interlacing, and alpha masks. Keep tunable limits
  in `Limits`.

### Remaining JPEG work

- Distinguish ordinary CMYK, Adobe-inverted CMYK, and YCCK in validated metadata.
  The current inversion flag alone does not describe the full color transform.
- Map each supported convention to the required PDF `/ColorSpace`, `/Decode`,
  and `/DecodeParms` settings. Keep DCT image data compressed where the PDF
  dictionary can express the correct behavior.
- Reject unsupported transforms, conflicting markers, and ambiguous component
  metadata with actionable diagnostics.
- Add a real ordinary CMYK fixture alongside the Adobe CMYK and YCCK fixtures.
  Verify grayscale and RGB JPEG behavior remains unchanged.

Complete when focused tests cover every PNG combination above, all scanline
filters and Adam7 passes, malformed chunks and samples, and resource boundaries.
Browser fixtures must cover the new PNG variants and all three JPEG conventions.
JPEG tests must check both PDF dictionaries and rendered colors.

General color-profile support remains outside the pre-1.0 scope unless common
assets require it. The remaining Chromium/Poppler CMYK color difference must
also meet the browser-parity requirement in 0.21.0.

## 0.21.0: Browser parity below 2%

Bring every fixture's worst-page changed-pixel ratio below 2% against Chromium.
Finish this before performance work so optimizations preserve the improved output.

The audit fixed flex and grid geometry, font inheritance, CSS custom properties,
quoted CSS text, zero-size text, and empty visible tables. Regression fixtures
cover those fixes, but several comparison thresholds still exceed 2%. The CMYK
fixture permits 23% because Chromium and Poppler convert its colors differently.
These allowances are not measured results or completion of this milestone.

Work to deliver:

- Record actual changed-pixel ratios, average deltas, and page counts for every
  fixture. Use the differing regions to prioritize shared rendering fixes.
- Resolve remaining font metrics, text placement, wrapping, box and table
  geometry, border placement, pixel rounding, and image-color differences.
- Enforce a maximum changed-pixel allowance of 0.02 for every fixture. Preserve
  page-count and average-delta checks, with no higher per-fixture exceptions.
- Document Chromium, fonts, rasterizer, fixture count, and the comparison metric.
  Keep the existing 72 DPI rasterization and per-channel changed-pixel definition.

Complete when every fixture measures below 2% in that environment and the suite
runs with the tightened thresholds. Keep all existing fixtures in the comparison.

## 0.22.0: Performance and archival output

Reduce rendering time, memory use, and PDF size while preserving visible output.
Add a best-effort archival mode with documented limits.

Work to deliver:

- Replace repeated list appends, binary concatenation, and full-line measurement
  in text layout with work that scales linearly. Use constant-time glyph-width
  lookup and cache repeated text measurements.
- Reuse table intrinsic measurements and precompute column geometry.
- Collect writer text and resources in linear passes. Carry font identifiers
  through style and layout instead of copying the complete registry per element.
- Compress uncompressed streams, deduplicate images, and remove unreachable
  objects where safe. Add optional embedded-font subsetting.
- Add archival output that embeds required fonts, includes metadata, avoids
  unsupported transparency where practical, and explains remaining limitations
  through diagnostics. Full PDF/A validation is a future version 2 objective.

Complete when realistic text and table benchmarks bound per-element and per-token
work using BEAM reductions alongside time and memory measurements. Optimized and
archival fixtures must remain readable, with layout, pagination, font fallback,
and browser-parity results preserved.

## 0.23.0: Guides and examples

Make the supported workflows easy to find, understand, and run.
The audit added focused regression fixtures and clarified inline-image extraction
limits. Final business-document examples and documentation review remain.

Work to deliver:

- Fill gaps in production-style invoice, report, label, and statement fixtures.
- Finish examples for metadata, static HTML forms, PDF inspection, transforms,
  stamping, and error handling.
- Review unsupported-feature guides and provide caller-side alternatives.
- Remove stale options, module names, and return shapes from generated docs.
- Make README and HexDocs navigation consistent and check guide and release links.
- Document how the changelog records breaking changes before 1.0.

Complete when executable examples pass doctests or focused tests, visible examples
have browser coverage, and documentation links resolve.

## 0.24.0: Release candidate and API freeze

Test the final feature set with real documents and downstream applications, then
freeze public module names, functions, options, return values, and error shapes
except for bug fixes.

Work to deliver:

- Recheck API classifications and diagnostic consistency after the earlier
  milestones land. Remove or document unstable internals.
- Finish migration notes and missing HexDocs guides, including renderer, merge,
  extraction, inspection, and transform examples.
- Decide whether the typography candidates below are required for 1.0.
- Fix release-candidate regressions and retain tests for them.

Complete when the candidate revision passes the full quality matrix, downstream
workflow checks pass, and public API documentation and migration notes are ready.

## 1.0.0: Stable release

Publish the stable API, compatibility guides, and final application examples.
Supported HTML/CSS behavior and PDF workflows must match the documentation and
have regression coverage. Public diagnostics and API boundaries must be settled.

Complete when the release revision passes the full quality matrix and the final
guides and release notes are published. Follow SemVer after release: minor versions
add compatible features, patch versions fix bugs, and major versions carry
intentional breaking API changes.

## Decisions outside the release sequence

Before 1.0, evaluate better Unicode line breaking, soft hyphens, and optional
hyphenation dictionaries. These are candidates, not scheduled commitments.

Right-to-left layout, bidirectional text, and complex shaping for scripts such
as Arabic, Indic scripts, and Thai, plus emoji sequences, remain likely post-1.0
work.

Asset fetching belongs to the caller. The renderer accepts approved local paths,
supported data URIs, caller-provided bytes, and asset resolver callbacks. Reject
remote URLs in HTML and CSS with diagnostics; applications should fetch and
validate remote assets before passing them to the library.

## Requirements for every milestone

- Run `./scripts/quality-matrix` on each change and resolve failures and project
  warnings before pushing. Preserve 100% test coverage.
- Add focused tests for changed behavior and diagnostics. Add browser fixtures
  for visible renderer changes.
- Record pre-1.0 breaking changes in [CHANGELOG.md](CHANGELOG.md).

See [CONTRIBUTING.md](CONTRIBUTING.md) for supported runtimes and quality checks.
