# SVG browser workflow audit

The user selected the document workflow as the acceptance scope: extract SVG, convert it to PNG, then preserve its appearance, used dimensions and position when embedding it in the PDF. Full SVG engine conformance is outside this scope. This is a deliberate reduction in acceptance scope, not a rendering fix.

Routine acceptance retains representative geometry, host CSS, viewports, pagination, selected fonts, authorized image resources, reuse, gradients, alpha, masks, filters, patterns and markers. The comparison target remains Chromium. Retained cases keep the original 1% changed-pixel ceiling and average-delta limit, except the two user-approved Chrome bug comparisons and the paint-server, gradient-alpha and font-attributes font-rendering allowances described below; combined samples also keep their original full canvas and minimum ink assertion.

## Validation after performance repair and the font-attributes allowance

The final matrix `.quality/logs/20261016T230704Z-274547/` passes on both
supported runtimes. Each version passes 3,803 ordinary tests, all 176 coverage
rows including Total at 100%, all 19 performance workloads under the unchanged
numerical guards, Dialyzer, and 380 browser checks. Parity takes 55.9 seconds on
1.19 and 56.6 seconds on 1.20. The exact host Chromium command also passes all
380 checks in 56.4 seconds. Its log is
`tmp/performance-third/host-browser-parity.log`.
There are no FAIL or WARN matrix entries. Font attributes retains its approved
2% changed-pixel ceiling with the existing 1% mean-delta limit. The strict
1.3845% failure and controls remain preserved. This optimization pass adds no
browser allowances or test registrations.

The [performance report](performance-1.1.0.md) records all three optimization
passes, the raw matched captures and the separate historical development
reference. Every current workload median is below both the fixed and fresh
original on both runtimes. Sampled peak total VM memory can increase by up to
28.7 MiB against fresh original. No baseline, timing allowance or guard changed.

## Approved paint-server font-rendering allowance

The user approved a 2% changed-pixel ceiling for
`inline_svg_text_paint_servers.html`, retaining the 1% mean-delta ceiling.
[SVG 2 text rendering](https://www.w3.org/TR/SVG2/painting.html#TextRenderingProperty)
permits different choices about hinting, legibility and geometric precision.
Resvg's unhinted glyph outlines and Chrome's font rasterization can both conform
while producing different pixels. This is a comparison allowance, not a renderer
fix. The original authored fixture and strict 1.6713% failure remain preserved.

The controls in `tmp/paint-server-root-cause/root-cause.json` show a diagnostic
`geometricPrecision` copy at 0.4257%, with byte-identical native page pixels and
unchanged Chrome character positions. Identical explicit paths leave 0.29375%.
A separate Resvg fractional-pattern repeat-spacing defect remains unresolved;
the allowance does not classify that geometry defect as spec-compliant.

## Gradient-alpha investigation

Fresh controls using core `fb9ddf1a` reproduce the original
`inline_svg_gradient_alpha.html` failure at 1.3173%. All 5,090 pixels above
the 12-channel tolerance lie in the two text regions. Removing text leaves
zero pixels above tolerance and a maximum difference of 2/255 across the linear
and radial alpha gradients. Replacing gradient paints with black still differs
by 1.4752%; setting every stop opacity to one differs by 1.5044%.

The cause is font rasterization, not a reproduced alpha-interpolation or SVG
transport defect. Resvg draws unhinted glyph outlines. Chrome's
[`geometricPrecision` font policy](https://github.com/chromium/chromium/blob/152.0.7977.82/third_party/blink/renderer/platform/fonts/font_platform_data.cc#L250-L255)
disables hinting and enables subpixel positioning. Requesting it in a diagnostic copy leaves native page pixels
byte-identical and Chrome character positions unchanged, while reducing the
comparison to 0.1612%. Supplying identical explicit glyph paths yields 0.1004%.
Chrome renders the authored and prepared SVG pages identically. The user then
approved a 2% changed-pixel ceiling for this fixture, retaining the 1% mean-delta
limit. The test documents the permitted font-rendering difference and preserves
the original 1.3173% strict failure evidence. The nested-mask comparison retains
its original 1% ceiling. Diagnostic copies are not accepted replacements.
Reproduction scripts, raw outputs and measures are in
`tmp/gradient-alpha-root-cause/`.

## Font-attributes investigation

Fresh controls using core `fb9ddf1a` reproduce
`inline_svg_text_font_attributes.html` at 1.3845%. All 4,519 above-tolerance
pixels lie in the SVG. Font selection, weight, style and size are correct,
including `DejaVuSans-BoldOblique` at 20px and the relative 14px text. Chrome
renders the authored and prepared SVG pages byte-identically; native output
is also unchanged by preparation.

Chrome's geometric-precision font policy reduces the diagnostic whole-page
comparison to 0.6860%, leaving native page pixels byte-identical and all Chrome
character positions unchanged. The same explicit glyph paths in both engines
yield 0.3272%. All 59 glyph origins match Chrome after flooring Resvg's x
coordinates to 1/64 CSS px; the largest baseline difference is 0.01465 CSS px.
This identifies font rasterization, browser coordinate quantization and residual
path-edge coverage differences, without reproducing a font/CSS transport defect.
The user approved a 2% changed-pixel ceiling for the original fixture, retaining
the 1% mean-delta limit. The original 1.3845% strict failure remains preserved. Raw controls,
glyph positions, selected font identities and measures are preserved in
`tmp/font-attributes-root-cause/`. The caption-content check ignores whitespace
inserted during PDF text extraction; SVG text must still be image-only.

## Validation after the gradient-alpha allowance

This historical checkpoint predates the font-attributes allowance and performance repair. Its matrix is `.quality/logs/20261016T125013Z-3906919/`. Both supported
runtimes pass all 3,803 ordinary tests and all 176 coverage rows, including Total,
at 100%, with zero warnings. Elixir 1.19 passes performance and Dialyzer, then
passes 379/380 browser cases in 56.0 seconds. The exact host browser command
passes 379/380 in 55.6 seconds. Font attributes at 1.3845% is the sole remaining
browser failure. Gradient alpha passes its approved ceiling at 1.3173%; its
post-comparison image-only and page assertions pass. The nested-mask case still
passes its original 1% ceiling.

Elixir 1.20 fails two existing reduction guards: full-font rendering measures
12,454,627 against 12,397,000 allowed, and 4,000 boxes measures 1,648,042 against
1,507,000. The 100-row HTML guard passes this run at 13,003,443 against
13,024,000, after failing earlier captures. No production code or performance
limits changed in this allowance/investigation. Dialyzer and parity are skipped
after the performance failure. Both FAIL logs were inspected. Full acceptance
remains open, and the PDF repository was not pushed.

Current diagnostic sources, raw controls, the exact host log and its relevant
artifacts are in `tmp/font-attributes-root-cause/`. All 1,159 runtime, test and
configuration input hashes match the final matrix run.

## Validation after the paint-server allowance

This checkpoint predates the gradient-alpha allowance. Its matrix is
`.quality/logs/20261016T123625Z-3850836/`. Both supported
runtimes pass all 3,803 ordinary tests and all 176 coverage rows, including
Total, at 100%. There are zero warnings. Elixir 1.19 passes performance and
Dialyzer, then passes 378/380 browser registrations in 56.3 seconds. The exact
host command passes 378/380 in 55.8 seconds. Font attributes at 1.3845% and
gradient alpha at 1.3173% are the two remaining browser failures. Paint servers
passes at 1.6713% under its approved 2% limit. Its caption-content and image-only
SVG assertions pass, ignoring whitespace inserted during PDF text extraction.
The threshold-contract test explicitly checks the new allowance and unchanged
mean-delta limit.

Elixir 1.20 fails the existing three reduction guards: 13,047,618 against
13,024,000 for 100-row HTML, 12,576,062 against 12,397,000 for full-font rendering,
and 1,647,931 against 1,507,000 for 4,000 boxes. Dialyzer and parity are skipped
after that failure. Both FAIL logs were inspected; full acceptance remains open.
The PDF repository was not pushed.

Focused logs, the exact host log, the three relevant host artifact directories
and diagnostic reports are preserved in `tmp/gradient-alpha-root-cause/`.
Earlier interrupted matrix attempts are not final validation evidence. All
1,159 runtime, test and configuration input hashes match the final matrix run.

## Approved Chrome bug comparison limits

The user explicitly approved exceptions for two existing registrations:

| Fixture | Changed-pixel ceiling | Mean channel-delta ceiling | Verified behavior |
| --- | ---: | ---: | --- |
| `inline_svg_filter_offsets.html` | less than 10% | 3% | Chrome's local feImage viewport adjustment shrinks fixed artwork containing text. This fixture also exposes different SourceGraphic input clipping. |
| `inline_svg_filter_text_inputs.html` | less than 3% | 1% | The same Chrome viewport adjustment shrinks fixed-size referenced text. |

Both tests include the requested `this is a known chrome bug` comment and point
to Chromium's `ComputeViewportAdjustmentScale`, crbug/260709. Chromium explicitly
acknowledges the fixed-length scaling bug; the input-clipping difference is a
separate specification deviation, not an independently acknowledged Chrome bug.
The original authored fixtures and earlier one-percent failures are unchanged.
These are approved comparison exceptions, not renderer fixes or successful
one-percent comparisons. The separate font-rendering allowances are recorded above;
all other configured limits remain unchanged.

The focused investigation under `tmp/filter-offset-investigation/` reproduces
9.137% changed pixels on the original offset page. PR #1155 gives 9.084%, still
failing the original gate. Authored and prepared SVGs produce byte-identical
PNGs within each engine. Six isolated probes across stock main, the pinned fork
and #1155 distinguish the feImage text-scaling and SourceGraphic clipping
behaviors from exact-matching rectangle and in-region offset controls.

### Refreshed converter snapshot

This validation checkpoint predates the paint-server font-rendering allowance
recorded above. Its original results and strict failure evidence remain intact.

The library now pins binding `850f23d690bd8cce9276d35e2ab43de6f487d4d5`,
release `0.6.0-pdf.2`, and core `fb9ddf1aacbffc6c1b5a5f0f50a6c079e08fb622`.
The core starts from upstream main including merged #1155, retains #1163 and
#1164, and includes #1166. The superseded #1165 patch, experimental fractional
pattern fix and unmerged font-hinting patch are omitted.

Core validation passes 1,818 tests. The binding passes 23 tests with both
source-built and precompiled binaries. All 20 release jobs succeed; all downloaded
archives match their published SHA-256 digests. Source builds retain existing
Cargo configuration deprecation and Rustler resource-macro warnings; ordinary
builds use the published binary.

Focused strict comparisons at `tmp/resvg-snapshot-v2/documents-source-strict/`
show text paths passing at 0.6698%. Filter offsets passes its approved limit at
9.0841%, filter text inputs at 2.4900%, and the marker case has zero changed
pixels. Three comparisons remain above the unchanged 1% limit: paint servers
1.6713%, font attributes 1.3845%, and gradient alpha 1.3173%. The existing source,
font bytes, viewports and Chrome references are preserved.

The earlier investigation at `tmp/svg-text-investigation/root-cause/` identifies
unhinted glyph outlines, browser glyph-position quantization and path coverage
as distinct contributors. Gradient-only rectangular controls match within two
channel values. A fractional pattern repeat defect also affects the paint-server
sample; its separate experimental fix improves that full-page comparison only
to 1.6476% and is not part of this release. Neither the optional hinting PR nor
an experiment hinting at the final 2x raster scale closes these three comparisons.
These cases remain failures, with no new threshold exceptions.

The required matrix `.quality/logs/20261016T120143Z-3724470/` passes 3,803
ordinary tests and all 176 coverage rows, including Total, at 100% on both
runtimes. There are zero matrix warnings. Elixir 1.19 passes performance and
Dialyzer, then passes 377/380 browser cases in 56.2 seconds. The exact host
command passes 377/380 in 58.9 seconds. The same three SVG cases fail in both.
Elixir 1.20 fails the existing reduction guards: 13,209,111 against 13,024,000
for 100-row HTML, 12,634,892 against 12,397,000 for full-font rendering, and
1,647,961 against 1,507,000 for 4,000 boxes. Its later matrix stages are skipped.
This dependency refresh does not close those separate performance requirements.
Supplemental checks on the same Elixir 1.20 image and build paths pass Dialyzer
and 377/380 browser cases in 55.9 seconds, with the same three failures and zero
warnings. Their logs are `elixir-1.20/supplemental-dialyzer.log` and
`elixir-1.20/supplemental-parity.log` beneath the matrix directory; raw browser
artifacts are in `elixir-1.20/supplemental-browser-parity-artifacts/`. These
checks do not turn the failed matrix into a passing run.

Release, source-build, downloaded-binary and exact host logs are retained under
`tmp/resvg-snapshot-v2/`; host browser artifacts are preserved in
`host-browser-artifacts/`. Matrix artifacts are retained alongside the runtime
logs. All original fixture sources, thresholds and earlier failure evidence
remain intact.

### Validation of the approved limits

The final matrix is `.quality/logs/20261016T095244Z-3454473/`. Both runtimes
pass all 3,803 ordinary tests with 100% coverage in every module and total.
Every matrix stage reports zero warnings. Elixir 1.19 passes performance and
Dialyzer, then passes 376/380 browser registrations in 55.8 seconds. The exact
host browser command passes 376/380 in 62.3 seconds. The two approved filter
comparisons pass; the remaining failures are text paint servers, font attributes,
gradient alpha and the text-path sheet region. All four retain the 1% limit.

Elixir 1.20 fails three existing reduction guards: 100-row HTML measures
13,193,892 against 13,024,000 allowed, full-font rendering measures 12,654,361
against 12,397,000, and 4,000 boxes measures 1,648,122 against 1,507,000.
The matrix skips its later stages. Supplemental checks on the same source,
image and build paths pass Dialyzer and report 376/380 browser registrations
passing in 53.8 seconds, with the same four failures and no warnings.

The focused run passes both approved comparisons. Its log is
`tmp/filter-offset-investigation/approved-limits-two-tests.log`. The exact host
run is `tmp/filter-offset-investigation/approved-limits-browser.log`, with raw
artifacts in `approved-limits-host-artifacts/` in the same directory. Both
runtime artifact snapshots remain with their matrix logs. Full acceptance
remains open; nothing was pushed.

## Removed comparisons

Removed 27 original cases: 22 browser registrations and five samples from a combined page. A further registration is saved by placing the retained mask sample on the other SVG page. All original authored fixtures remain unchanged. The explicit `@converter_reference_fixtures` inventory in `browser_parity_test.exs` identifies the 26 archived inputs in that fixture directory. The manual byte-adapter input remains in `svg_backend/`. There are no skipped test registrations for these inputs.

The browser suite now has 380 registrations. The earlier workflow cleanup
retained 32 focused SVG tests covering 35 original samples, before the two
intrinsic embed comparisons were retired below. Mixed MathML/template tests
also contain SVG. A reference contract test detects deliberate color, size,
placement and missing-ink regressions.
The ordinary suite had 3,804 tests after the workflow cleanup. The retained mask canvas and all three
other SVG sheet canvases keep their original dimensions and per-sample checks.

### Unstable Chrome intrinsic embed reference

At the user's request, the browser registrations for
`html_svg_document_flex_initial_canvas.html` and
`html_svg_document_intrinsic.html` are removed. Chrome paints different
intrinsic embed viewports for the same source, including within one runtime;
fresh-process printing did not consistently resolve this. The native output was
identical across runtimes. The unchanged HTML/SVG sources remain in the explicit
`@reference_only_fixtures` inventory, and the original failures and diagnostic
variants remain under `tmp/resvg-fixes/` and the recorded matrix directories.
This retirement is not reported as a renderer fix. At the retirement checkpoint, six consistent SVG comparison
failures remained. The subsequent approved exceptions leave four comparisons
to resolve. Other document viewport, intrinsic sizing and embedded-context
comparisons retain their registrations and thresholds.

The fresh matrix `.quality/logs/20261016T081928Z-3306678/` exposed the second
unstable input. Its native raster and measured SVG viewports exactly match the
earlier 1.19 run, while Chrome 155 changes both the SVG capture and PDF output.
The comparison moves from 0.066% to 1.789% changed pixels. Both raw captures
remain intact; `tmp/resvg-fixes/intrinsic-reference-instability.json` records
their hashes. This intermediate matrix ran before retiring the second test.


### Text measurement conformance

Nested textLength allocation, coordinate/rotation lists and path calibration exercise detailed SVG typesetting. Retained selected-font text, font attributes and href text paths provide representative document-image checks.

- `svg_following_length_chunks.html`
- `svg_nested_scaled_lengths.html`
- `svg_nested_text_lengths.html`
- `inline_svg_text_coordinate_lists.html`
- `inline_svg_text_path_calibration.html`
- `inline_svg_text_path_defaults.html`
- `inline_svg_text_lengths.html`
- `inline_svg_text_rotations_canonical.html`

### Stroke calibration conformance

Detailed pathLength/dash and transformed/stroked-glyph probes duplicate the old painter qualification scope. Ordinary geometry, gradients with stroked text and marker artwork remain.

- `inline_svg_path_length_dashes.html`
- `inline_svg_path_length_zero.html`
- `inline_svg_stroke_uniform_transforms.html`
- `inline_svg_text_strokes.html`

### Paint-server conformance

Dense stripe sampling, glyph edges and old Type3-oriented fixtures add little to the retained text paint-server, pattern/marker and gradient checks. The Type3 paint-server Chrome PDF raster also contains a large striped rectangle instead of the expected text; its reference needs investigation before reuse.

- `inline_svg_text_stripe_patterns.html`
- `inline_svg_text_paint_edges.html`
- `inline_svg_type3_paint_servers.html`
- `inline_svg_type3_sample_coordinates.html`

### Color-space conformance

These probe linearRGB, repeat/reflect periods and mask/filter interpolation across several painting paths. Default gradient, alpha, mask and filter samples remain. Verified stock Resvg color-space deviations are still documented.

- `inline_svg_gradient_modes.html`
- `inline_svg_gradient_spread.html`
- `inline_svg_gradient_raster.html`
- `inline_svg_gradient_filter_interpolation.html`
- `svg_image_gradient_color.html`
- `svg_image_linear_compositing.html`

### Repeated internal SVG link probes

Internal SVG links are inert PNG pixels. Ordinary tests retain that contract; the broad scoped-clipping and reuse browser cases retain the relevant artwork checks.

- `inline_svg_direct_clip_links.html`
- `inline_svg_glyph_shape_links_instances.html`

### Marker tangent conformance

Exhaustive degenerate tangents, subpaths and context paint exceed the representative marker scope. Marker viewport geometry and embedded/document contexts remain.

- `svg_marker_context_orientation.html`

### Explicitly unsupported SVG2 input

The converter rejects textPath path data. An ordinary test asserts the actionable diagnostic. A browser test expecting successful rendering was stale.

- `inline_svg_text_path_inline.html`

### Manual byte-adapter comparison

This test manually converted SVG and substituted a PNG into HTML. It bypassed the automatic extraction workflow. Ordinary converter font/resource tests and end-to-end image-resource comparisons remain.

- `svg_backend/byte_artwork.html`

## Evidence and limits

Original strict browser results remain under `tmp/browser-speed/`, including `host-browser-final.log`, `host-browser-final-failures.json` and the full-page artifact snapshots. Cleanup records and fresh validation are under `tmp/svg-workflow-cleanup/`. These local paths are evidence, not checked-in assets.

Removing a case does not establish that its SVG looks identical to Chromium. SVGs using the archived edge cases can still differ. The retained failing cases must continue to be reported as failures. Verified converter limitations remain in [SVG conversion](svg-converter-workflow.md).

## Validation after unstable-reference retirement

The final matrix is `.quality/logs/20261016T082638Z-3335933/`. Both runtimes
pass all 3,803 ordinary tests with 100% coverage in every module and total.
Every matrix stage reports zero warnings. Elixir 1.19 passes performance and
Dialyzer, then reports 374/380 browser comparisons passing in 57.1 seconds.
The exact host browser command reports the same six failures in 63.1 seconds;
its log is `tmp/resvg-fixes/browser-remove-unstable-final.log`.

Elixir 1.20 fails three existing reduction guards: 100-row HTML measures
13,207,593 against 13,024,000 allowed, full-font rendering measures 12,636,258
against 12,397,000, and 4,000 boxes measures 1,647,877 against 1,507,000.
Its Dialyzer and browser stages are skipped after that failure. No performance
ceiling changed. Full acceptance remains open; the repository was not pushed.

## Fork and PDF embedding correction

The initial binding release combined upstream PRs #1163, #1164 and #1165 on clean
Resvg main. All 20 precompiled release builds passed, and the binding's 23 tests
passed with both source-built and downloaded binaries. The refreshed snapshot
above replaces #1165 with merged #1155 and includes #1166. Current commits are
recorded in [SVG conversion](svg-converter-workflow.md).

The PDF writer now fills only fully transparent edge color samples from adjacent
visible pixels. This removes a separate color/soft-mask interpolation fringe.
The lossless PNG decoder, visible samples and alpha remain unchanged. The marker
comparison now has zero changed pixels, and the superseded XML marker viewport
rewrite is removed. Three rewrite-specific tests were consolidated into one
source-preservation check; actual marker raster and Chrome checks remain. One
new unit test covers edge colors across RGB/grayscale and 8/16-bit images. The
ordinary test count is now 3,803. Before the subsequent intrinsic-embed test retirement, browser registrations were 382; the ordinary
PNG page includes additional edges on light and dark backgrounds.

Before the approved Chrome bug exceptions, host and Elixir 1.19 results retained
these six one-percent failures. The first two now have explicit exception limits:

| Historical SVG failure | Changed pixels then | Evidence at that checkpoint |
| --- | ---: | --- |
| `inline_svg_filter_offsets.html` | 9.137% | Rotation is fixed. Chrome scales local feImage inputs containing text differently and admits SourceGraphic outside the filter input region. |
| `inline_svg_filter_text_inputs.html` | 2.490% | Chrome applies viewport adjustment to fixed-size text referenced by feImage; the minimal probe reproduces it at both raster densities. |
| `inline_svg_text_paint_servers.html` | 1.671% | Text gradient/pattern edge differences remain; a complete cause is unresolved. |
| `inline_svg_text_font_attributes.html` | 1.384% | Text edge/spacing differences remain with supplied font bytes. |
| `inline_svg_gradient_alpha.html` | 1.317% | Alpha-gradient text differs after the embedding fringe correction. |
| `inline_svg_text_paths.html` | 1.022% | Its independent region on `sheet_svg_02.html` still exceeds 1%. |

The shared feImage comparison now passes at 0.171%. These percentages use the
original page or independent-region denominators. No threshold, original SVG or
Chrome reference was substituted. The first full published-binding browser run
also caught an unsupported float declaration in the added PNG samples; that
fixture was corrected to flex layout and passes its focused comparison at
0.185%.

Before the intrinsic-embed test retirement, the matrix matching the
runtime/tests/configuration byte-for-byte,
`.quality/logs/20261016T074126Z-3111532/`, passes all 3,803
ordinary tests with 100% coverage in every module and total on both runtimes,
without warnings. Elixir 1.19 passes performance guards and Dialyzer, then passes
376/382 browser tests in 59.1s. Elixir 1.20 fails three reduction guards and skips
later stages; supplemental checks with the same image/source pass Dialyzer and
pass 375/382 browser tests in 61.6s. The exact host command passes 376/382 in
60.5s. Host log: `tmp/resvg-fixes/browser-final.log`. Acceptance remains open.

The additional 1.20 browser failure is
`html_svg_document_flex_initial_canvas.html`, at 2.863%. Both containers report
Chrome 155.0.8059.79, but Chrome paints the intrinsic stretched embed differently.
The native outputs are pixel-identical across runtimes. The 1.19 Chrome variant
differs from native by 0.652%, passing the same threshold. Both original PDFs,
rasters and statistics are retained in the matrix artifact directories; the
failing comparison is not suppressed or reported as passing. Diagnostic previews
are `tmp/resvg-fixes/flex-native*.png` and `flex-chrome-*.png`.
Three isolated comparisons of the unchanged original in the same 1.20 image
reproduce both outcomes: two pass at 0.652%, then one fails at 2.863%. This
confirms the changing reference within a single runtime/configuration; see
`tmp/resvg-fixes/flex-intrinsic-probe.log` and its retained PDFs and rasters.
Six fresh one-shot Chrome prints of that same original all pass at the identical
0.652% ratio in the container. A per-case CLI experiment also passes both
container browser runs, but fails on host Chrome at 4.995%. Therefore that
reference change was reverted. Its full matrix remains under
`.quality/logs/20261016T075815Z-3196824/`, with host output in
`tmp/resvg-fixes/browser-final-fresh.log`. It is a rejected experiment, not a fix.
At that checkpoint the original test and threshold remained. Source hashes in
`tmp/resvg-fixes/restored-source-check.json` confirm that the restored runtime,
tests and configuration match the earlier complete matrix; only documentation
differs. That run records the supported-runtime evidence before the test retirement.
The final exact host command after reverting the experiment passes 376/382 in
60.2s, retaining the six consistent failures. Its complete output is
`tmp/resvg-fixes/browser-delivery.log`.

A separate diagnostic supplies explicit 75pt by 37.5pt host dimensions. Its three
1.20 runs give the identical 0.643% changed ratio. This is a concrete authoring
workaround with the tradeoff that intrinsic sizing is no longer exercised. It
does not qualify the original case. Source, authorized SVG bytes and output are
under `tmp/resvg-fixes/flex-explicit-*` and `assets/`. The initial data-URI probe
was rejected by the existing child-resource policy; the final probe uses a local
authorized asset. This authoring workaround was not adopted for acceptance; the
original intrinsic-sizing fixture remains an explicit unresolved reference case.

Chrome's fixed-length feImage text behavior is acknowledged in its
[`ComputeViewportAdjustmentScale` implementation](https://raw.githubusercontent.com/chromium/chromium/main/third_party/blink/renderer/core/svg/graphics/filters/svg_fe_image.cc),
which notes that viewport adjustment breaks nonrelative lengths. The published
[filter primitive clipping rule](https://www.w3.org/TR/filter-effects-1/#FilterPrimitiveSubRegion)
also clips primitive inputs, while Chrome's offset probe admits outside ink. These
are demonstrated engine differences, not justification for changing tolerances
or applying an unvalidated broad Resvg filter patch. Probe sources and raw images
remain under `tmp/resvg-fixes/probes/`.

## Results before the fork integration

The exact host command `CHROMIUM_BIN=/usr/bin/chromium mise exec -- mix test.browser_parity` passes 374 of 382 registrations in 59.4 seconds. All retained
non-SVG browser tests pass. The log is `tmp/workflow-repair/browser-delivery.log`.
Chrome-generated replacement PNGs, both PDFs and statistics are preserved under
`tmp/workflow-repair/host-final-artifacts/`. Original strict vector-PDF comparisons
remain in `strict-vector-stats.exs`; failures there are not reported as passes.

| Remaining SVG sample | Changed pixels | Observed problem |
| --- | ---: | --- |
| `inline_svg_filter_offsets.html` | 9.316% | Local filter artwork is too large and loses rotation; cropped SourceGraphic also differs. Exact prepared input also differs between stock Resvg and Chrome. |
| `inline_svg_filter_images.html` | 4.638% | Local `feImage` vector inputs lose gradient/rotation in some branches, including on identical prepared input. |
| `inline_svg_filter_text_inputs.html` | 2.661% | Text painted through local `feImage` is oversized, including on identical prepared input. |
| `inline_svg_gradient_alpha.html` | 1.916% | Alpha gradients on selected text remain different; precise cause unresolved. |
| `inline_svg_text_paint_servers.html` | 1.901% | Gradient/pattern text edges and placement remain different; precise cause unresolved. |
| `inline_svg_text_font_attributes.html` | 1.516% | Glyph edges/spacing differ. Both original-to-prepared Chrome and prepared-Chrome-to-Resvg comparisons differ, so this is not attributed wholly to the converter. |
| `svg_marker_viewport_geometry.html` | 1.117% | Repeated marker edges remain different after equivalent PNG replacement. |
| `inline_svg_text_paths.html` | 1.047% | Text along paths remains different in its independent region on `sheet_svg_02.html`. |

The three filter comparisons on identical prepared SVG/font inputs differ by
8.744%, 16.808% and 4.570% of their SVG canvases respectively. Those denominators
are the SVG canvas, not the full-page acceptance denominator in the table.
Evidence is `tmp/workflow-repair/equivalent-svg-pngs.log` and `svg-captured/`.
The text-attribute split is `tmp/workflow-repair/svg-text-difference.jsonl`.
These measurements do not establish that every remaining mismatch is antialiasing.

The document root-transform correction has stronger proof: unchanged Chrome
pixels before/after the equivalent group rewrite, and zero differing pixels
between Chrome and stock Resvg after rewriting. See
`tmp/workflow-repair/document-root-transform-probe.log` and
`document-root-transform-fixed.log`.

The old `html_iframe_legacy_initialization.html`, `html_initial_child_mime.html`,
`html_frameset_margin_recovery.html` and `html_decorated_background_fragments.html`
sources remain archived. Two normal iframe/MIME variants replace the first two
on one comparison page. Extreme overflow margins, version-dependent scrollbars
and dense repeating texture sampling no longer gate this workflow. Two dead
MathML reference-helper tests were removed after restoring direct Chrome radical
and operator comparisons. The browser count fell from 387 to 382, then rose to
383 for the PNG-reference negative-control contract. Retiring the version-dependent
nested-template browser check brings the final count back to 382.

The first repair matrix `.quality/logs/20261016T055243Z-2790791/` failed four
ordinary assertions that still expected old raster dimensions or the removed
SVG group, plus one unexecuted cache-budget error branch. Those assertions now
check actual dense PNG dimensions and ordinary image placement. The budget test
checks one fitting occurrence followed by a cached occurrence exceeding the
unchanged resource ceiling. That supported-runtime matrix is
`.quality/logs/20261016T061011Z-2877245/`. Both versions pass all 3,804 ordinary
tests with 100% aggregate and per-module coverage, with no warnings. Elixir 1.19
passes performance guards and Dialyzer, then fails exactly the eight SVG browser
comparisons in 59.4 seconds. Elixir 1.20 fails two existing performance reduction
guards and skips the later matrix stages. Separate checks with its same image,
source and build paths pass Dialyzer and fail the same eight browser comparisons
in 58.6 seconds. The supplemental log and raw failure artifacts are retained in
that runtime's matrix directory. The matrix remains FAIL.

The Elixir 1.20 failures are full-font rendering at 12,545,507 reductions against
12,397,000 allowed, and 4,000 boxes at 1,648,107 against 1,507,000 allowed. No
performance guards, size bounds, absolute timing limits or original baseline
were changed. Original-baseline cumulative timing qualification remains open.

The nested-template original-source browser check is retired after a verified
Chrome 152/155 semantic difference. Chrome 155 applies the inner `template for`
replacement; Chrome 152 and the library's original-source policy keep it inert.
The ordinary nested-candidate validation/mutation tests remain, as do eight
other template browser checks. Its original fixture is unchanged and its failed
reference is retained in `tmp/workflow-repair/nested-template-chrome155-strict/`.
This is a documented template-policy deviation, not a fixed original-source
Chrome comparison. It removes one browser registration, leaving 382.
