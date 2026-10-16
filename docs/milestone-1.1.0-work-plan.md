# Milestone 1.1.0 work plan

Updated 2026-10-17. Current repair tasks and remaining milestone scope.

## Goal and order

Render useful static HTML to PDF. SVG artwork uses the existing Resvg
library. Full SVG conformance and another SVG painting engine are outside this
work. First-party runtime behavior must be Elixir. The existing Resvg and system
font discovery dependencies remain allowed. Development-only generators may use
other languages.

1. Replace the first-party Rust runtime and its build integration.
2. Repair existing behavior and tests. Do not add unrelated features.
3. Qualify unit tests, coverage, static analysis, Chromium comparisons and
   cumulative performance on the combined source.
4. Audit the remaining HTML milestone scope, then implement separately bounded
   feature batches from an accepted baseline.

All 19 workload medians are now below the original baseline on both supported
runtimes. The [performance report](performance-1.1.0.md) records the matched
measurements and memory tradeoff. The final combined source passes both runtime
matrices and the exact host Chromium command.
The remaining contract and scope audits stay open. Test coverage and test pass
rates do not measure feature completion.

First-party runtime source is Elixir, using Erlang/OTP standard libraries. There
is no first-party Rust runtime, Cargo project or native utility binary. The two
permitted native dependencies remain Resvg and system font discovery. Python
fixture generators, Bash validation scripts and CI configuration are development
tools, not library runtime implementations.

## Recovery batches

Each row is one separately reviewable batch. A wave is an integration and
validation point, not a measure of delivered functionality.

| ID | Work | Completion evidence | Status |
| --- | --- | --- | --- |
| R1 | Remove first-party Rust, Cargo compiler hooks, vendored runtime forks and native utility bindings | No first-party Rust or native utility artifacts needed by a fresh packaged build | Implemented, qualification pending |
| R2 | Replace Unicode line breaking with Elixir using the pinned Unicode data | Original CJK, punctuation, combining, emoji, discretionary and space tests pass; shared budgets remain enforced | Focused tests pass |
| R3 | Qualify the Elixir modern CSS color replacement | Existing color-input serialization, calculations, dimensional failures, malformed comma/comment cases and original Chromium fixtures pass | Focused tests and full coverage pass; full behavior qualification remains open |
| R4 | Qualify Elixir HTML pattern and initial URL behavior | Unicode-v/UTF-16, whole-value, URL, resource limits, worker death and nonzero deadlines pass; audit Unicode property vocabulary, lookbehind and URL host conformance | Focused tests pass; conformance audit open |
| R5 | Repair ordinary HTML, MathML, layout, forms and source-validation regressions | Close the retained HTML/PDF and MathML browser failures | All retained non-SVG browser comparisons pass on host and both supported runtimes. Version-dependent parser checks are explicitly archived; MathML reference adaptations remain documented |
| R6 | Qualify nested SVG font policy and authorized font-byte transport | Shape-only outer artwork with a permitted text child works; disabled discovery remains restricted | Focused tests pass |
| R7 | Preserve XML spelling and viewport identity during SVG extraction | XML/inline spelling, namespace and used viewport tests pass; valid XML with both width and WIDTH must survive extraction | Focused viewport tests pass; canonical XML transport still open |
| R8 | Repair explicit SVG image sizing | Existing requested 20x10 PNG case yields the requested dimensions with preserved artwork | Implemented; explicit two-dimensional targets stretch the authored canvas per CSS object-fit fill |
| R9 | Resolve tiny SVG canvas transport | Verify the original 1x1 marker against Chromium, then fix integration or issue a verified converter diagnostic; keep real allocation accounting | Verified: resvg drops width<=1 strokes on canvases of at most two device pixels; Chromium paints. Documented converter deviation with corrected expectation |
| R10 | Resolve converter/PDF rendering differences | Retained document-to-PNG workflow cases pass Chromium comparisons within documented limits; preserve archived conformance inputs as evidence | The refreshed fork includes merged #1155, retained #1163/#1164 and text-path fix #1166. The closed #1165 patch is omitted. Text paths passes at 0.6698%. The user approved 2% font-rendering allowances for paint servers, gradient alpha and font attributes. Font attributes is traced to font rasterization, 1/64 CSS px position quantization and path-edge coverage differences, with correct font selection and identical authored/prepared Chrome output. Two unstable intrinsic embed browser tests were removed at the user's request; original sources and failures remain archived |
| R11 | Close meaningful coverage and static checks | 100% per module and total; supported-runtime static analysis has no project warnings | The October 17 final matrix passes 3,803 ordinary tests with 100% aggregate/per-module coverage and Dialyzer on both runtimes, zero warnings |
| R12 | Repair cumulative performance | Optimize table rendering, PDF extraction/writing and form preparation toward the original baseline itself; preserve all existing guards | All 19 workload medians are below both the fixed and freshly measured original on both runtimes. Three paired captures per source/runtime retain all 15 timed samples per workload. Candidate reduction, size and absolute-time guards all pass. Sampled total VM peaks increase by up to 28.7 MiB against fresh original; see the performance report. The final combined-source matrix and exact host Chromium command pass |
| R13 | Remove superseded planning history and reconcile current documentation | One current plan; valid links; no obsolete first-party Rust or SVG vector/text claims | Old tracked planning/checkpoint files removed; wave-named test files and the 6,718-line SVG restoration file were merged into topical files this pass; documentation reconciliation ongoing |
| R14 | Accept the combined development baseline | Both supported quality matrices and exact host Chromium parity pass on the final source | Both supported runtime matrices and exact host Chromium checks pass on the retained source. Original cumulative timing now also passes below the original baseline; the historical development reference stays separate and broader milestone audits remain open |

These are 14 named recovery batches, including qualification and documentation.
They are not 14 remaining feature batches. R10 contains independent converter
differences, not one final small fix. No defensible milestone completion percentage
or delivery date exists until the six scope audits below have checked the code.

Use four waves, in order:

1. R1-R4: finish runtime replacement qualification and close its failures.
2. R5-R10: repair existing HTML/MathML regressions and SVG integration. Classify
   converter omissions only after reproducing them with stock Resvg and Chromium.
3. R11-R12: restore 100% coverage, static checks and the cumulative performance
   limits. The October 17 measurements reach the original timing target on both
   runtimes, with the complete matrix and exact host Chromium command passing.
4. R13-R14: reconcile documentation and rerun all gates on one frozen source.

These are integration waves, not a promise that four small patches finish the
milestone. New feature work starts after recovery acceptance.

## Current browser repair

The retained source passes `./scripts/quality-matrix` for both supported runtimes.
The matrix is `.quality/logs/20261016T230704Z-274547/`. Each version passes
3,803 ordinary tests, every module and Total at 100% coverage, all 19 performance
workloads under the existing numerical guards, Dialyzer, and all 380 browser
checks. Browser parity takes 55.9 seconds on 1.19 and 56.6 seconds on 1.20.
The exact host Chromium command also passes 380 checks in 56.4 seconds, retained
in `tmp/performance-third/host-browser-parity.log`.
There are no FAIL or WARN matrix entries. The font-attributes allowance remains
its approved 2%, with the 1% mean-delta ceiling and original strict evidence
preserved. No additional browser allowances or test registrations were added.

All 19 workload medians are below the original baseline itself on both runtimes,
including dense tables, full-font rendering and text extraction. Three captures
per source/runtime retain all 15 timed samples for each workload. Sampled peak
total VM memory increases by up to 28.7 MiB against fresh original. The complete
comparison, memory tradeoff and historical development references are described
in [the performance report](performance-1.1.0.md).

The user retired the unstable Chrome intrinsic-flex-embed comparisons.
A fresh run exposed the same reference instability in the related intrinsic
document input; both registrations are removed and both sources are archived. The browser
suite has 380 registrations. The user then approved two Chrome bug threshold
exceptions: filter offsets uses a 10% changed-pixel and 3% mean-delta limit;
filter text inputs uses 3% changed pixels and the existing 1% mean-delta limit.
The user subsequently approved a 2% changed-pixel allowance for text paint
servers, retaining its 1% mean-delta limit. SVG permits the different font
rasterization choices; the separate fractional-pattern spacing defect remains
documented. The user also approved a 2% gradient-alpha ceiling, retaining its 1%
mean-delta limit. Font attributes also has an approved 2% changed-pixel ceiling
and the unchanged 1% mean-delta limit. Gradient
alpha is traced to glyph rasterization: its no-text gradient controls differ by
at most 2/255, and geometric-precision text reduces the diagnostic page from
1.3173% to 0.1612% with byte-identical native pixels. Text paths passes at
0.6698% with #1166. The original inputs and stricter failure evidence remain intact. All other changed-pixel limits retain 1%.
These removals and threshold exceptions are not renderer fixes. The ordinary suite remains at 3,803 tests.

Font-attributes controls reproduce 1.3845% on the original page and 0.6860%
with Chrome geometric-precision text, leaving native page pixels identical.
The same explicit glyph paths yield 0.3272%. Both engines select the correct
fonts and sizes. All 59 glyph origins match after flooring Resvg positions to
Chrome's 1/64 CSS px precision, at most 0.01465 CSS px apart. Authored and
prepared Chrome pages are identical. No font/CSS transport defect is reproduced;
the original 1% comparison remains preserved as a strict failure. The approved
2% comparison passes focused validation. Evidence is in
`tmp/font-attributes-root-cause/`.

The matrix before the font-attributes allowance and performance repair is
`.quality/logs/20261016T125013Z-3906919/`, using
binding `850f23d6`, release `0.6.0-pdf.2` and core `fb9ddf1a`. Both runtimes
pass all 3,803 ordinary tests with 100% every-module and total coverage, with
zero warnings. Elixir 1.19 passes performance and Dialyzer, then passes 379/380
browser tests in 56.0s. The exact host command passes 379/380 in 55.6s. Font
attributes at 1.3845% is the sole remaining browser failure. Paint servers at
1.6713% and gradient alpha at 1.3173% pass their approved 2% allowances.
Elixir 1.20 fails two existing reduction guards: 12,454,627 against 12,397,000
for full-font rendering and 1,648,042 against 1,507,000 for 4,000 boxes. The
100-row HTML guard passes this run at 13,003,443 against 13,024,000. Its Dialyzer
and parity stages are skipped. Every FAIL log was inspected. Full acceptance
remains open. The matrix before the gradient-alpha allowance,
`.quality/logs/20261016T123625Z-3850836/`, retains its original two browser
failures and three performance-guard failures.

The matrix before the paint-server allowance,
`.quality/logs/20261016T120143Z-3724470/`, retains its original three browser
failures and raw artifacts. Earlier supplemental checks are recorded in
[the refreshed snapshot audit](svg-browser-workflow-audit.md#refreshed-converter-snapshot).

The earlier approved-limit matrix `.quality/logs/20261016T095244Z-3454473/`
remains available with its original four failures and raw comparison artifacts.
The refresh fixes text paths; it adds no threshold exception. The PDF repository
has not been pushed.

Before those threshold exceptions, the post-retirement matrix was `.quality/logs/20261016T082638Z-3335933/`.
Both runtimes pass 3,803 ordinary tests with 100% every-module and total coverage
and zero warnings. Elixir 1.19 passes performance and Dialyzer; browser parity
reports 374/380 passing in 57.1s. The exact host browser command reports the
same six failures in 63.1s. Elixir 1.20 fails three existing reduction guards
and skips subsequent Dialyzer and parity stages. See
[the audit](svg-browser-workflow-audit.md#validation-after-unstable-reference-retirement)
for measurements and evidence. Full acceptance remains open; nothing was pushed.

The binding now pins the combined external Resvg fork, with checksum-verified
precompiled binaries. All 20 release builds and the binding's 23 tests pass;
both source-built and downloaded binaries were tested. See
[SVG conversion](svg-converter-workflow.md) for the exact dependency commits.
The marker viewport rewrite and its private helpers were removed after qualifying
the dependency fix. Three rewrite-specific tests were combined into one source
preservation test; actual marker pixel and Chromium comparisons remain.

The PDF writer now extends visible colors into adjacent fully transparent PNG
pixels before embedding. It preserves visible color samples, alpha and decoder
output, including grayscale and 16-bit PNGs. One focused test covers these formats,
isolated edges, empty artwork and competing neighbor alpha. The existing PNG
browser page now also covers scaled transparent edges on light and dark
backgrounds. Focused renderer validation passes 102 tests. The marker comparison
has zero changed pixels. Before the threshold exceptions and converter refresh,
six original SVG samples remained above 1%.
An explicitly nil alpha plane, already accepted by the writer validator, now
behaves like an absent plane during hashing, resource allocation and embedding.
The redundant zero-number formatter left after marker rewrite removal was
deleted; no artificial coverage case was added.

Before the intrinsic-embed test retirement, the matrix matching the runtime,
tests and configuration byte-for-byte was
`.quality/logs/20261016T074126Z-3111532/`. The comparison is recorded in
`tmp/resvg-fixes/restored-source-check.json`; only evidence documentation differs.

| Runtime | Ordinary tests and coverage | Performance | Dialyzer | Browser parity |
| --- | --- | --- | --- | --- |
| Elixir 1.19 | 3,803 pass; 100% every module and total | PASS | PASS | 376/382 pass, 59.1s |
| Elixir 1.20 | 3,803 pass; 100% every module and total | FAIL, three reduction guards | PASS, supplemental | 375/382 pass, 61.6s, supplemental |

After reverting the CLI experiment, the exact host command passes 376/382 in
60.2s. Its log is `tmp/resvg-fixes/browser-delivery.log`; six consistent SVG
failures were above the unchanged threshold at that checkpoint.

Those six SVG failures were consistent across host and containers. The 1.20 run also
failed `html_svg_document_flex_initial_canvas.html`: Chrome changes the intrinsic
stretched embed viewport, while native output is pixel-identical between
runtimes. Three focused pooled prints reproduced both outcomes. Six fresh
one-shot prints passed on container Chrome, but host Chrome failed that approach
at 4.995%. The per-case CLI change was reverted; that full experiment remains
under `.quality/logs/20261016T075815Z-3196824/`. At that checkpoint the original test, references and threshold remained. The user
subsequently retired the unstable test; its source and failure evidence remain. Details are in
[the SVG workflow audit](svg-browser-workflow-audit.md).

The matching 1.20 matrix measures 13,076,974 reductions versus 13,024,000 allowed
for 100-row HTML, 12,591,570 versus 12,397,000 for full-font rendering, and
1,648,048 versus 1,507,000 for 4,000 boxes. The later experiment measures
12,584,608 and 1,648,085 for the last two and passes the first. Every capture is
retained; no ceiling or original baseline changed. The matrix
skips later stages after performance fails, so supplemental checks use the same
image, source and build paths. Every matrix stage reports zero warnings.
Acceptance remains open; the PDF repository has not been pushed.

Before this fork and PNG embedding correction, the exact host browser command
passed 374 of 382 registrations in 59.4 seconds.
At that checkpoint every retained non-SVG comparison passed; eight SVG samples
remained above the unchanged 1% threshold. See [the SVG workflow audit](svg-browser-workflow-audit.md)
for every remaining fixture, percentage, observed difference and evidence path.
This is not a fully passing acceptance suite.

Implemented repairs include MathML stretch-part placement/ink geometry, selected
script-glyph layout bounds, identical physical Arial bytes in control references,
shared 2x SVG raster density, ordinary PNG placement without an SVG wrapper group,
and the Resvg nested-root double-transform correction. Direct radical/operator
Chrome comparisons replace retired reconstructed references. Dedicated fraction
and italic-token Core references remain explicit adaptations. The token-owner
sheet selects two identical script outlines through an existing reference font;
Chrome still performs its MathML and surrounding CSS layout.

Eligible SVG checks now compare automatic native conversion with PNGs generated
by Chrome from the original SVGs at the same viewport and raster density. They
retain the original vector PDFs and separate 72-DPI strict failure results. The
new comparison passes deliberate-regression controls without changing pixel
thresholds, blurring images or using Resvg output as a Chrome reference.

Two iframe/MIME pages share one normal-workflow comparison. Version-dependent
extreme overflow/scrollbar inputs and a dense repeating background texture are
archived with their original sources and failures. Those retired cases are not
reported as fixed comparisons. At that checkpoint the ordinary suite had 3,804 tests.

The initial repair matrix is `.quality/logs/20261016T055243Z-2790791/`. It failed
four stale unit assertions about old raster sizes/wrapper groups and one cache
budget branch. The assertions now check actual PNG dimensions and ordinary image
placement. A fitting single occurrence followed by an over-budget cached
occurrence covers the live error path. That supported-runtime rerun is
`.quality/logs/20261016T061011Z-2877245/`. Both versions pass all ordinary tests
with 100% aggregate/per-module coverage and zero warnings. Elixir 1.19 passes
performance guards and Dialyzer, then fails the eight SVG comparisons in 59.4
seconds. Elixir 1.20 fails full-font rendering and 4,000-box reduction guards;
its later stages are skipped. Supplemental checks with the same image/source
pass Dialyzer and fail the same eight browser comparisons in 58.6 seconds.
The host exact command takes 59.4 seconds. The matrix remains FAIL, and no push
or full milestone acceptance is claimed. Cumulative original-baseline timing
qualification remains open.

## Earlier regression inventory

The 13 failing unit tests recorded below were closed this pass. Each was
verified against Chromium ground truth before changing code or expectations:

- Converter transport defects fixed in code: explicit SVG image sizing (a
  two-dimensional raster target now stretches the authored canvas per CSS
  object-fit fill), marker viewport clipping, zero-area marker viewBoxes and
  large glyph-turn angle reduction (all fixed by re-encoding the converter
  source, not by painting).
- Expectations authored for the deleted painter, now Chromium-verified: marker
  reference-percentage resolution against the referencing viewport canvas,
  fractional background tile snapping, and the mixed-rule clip's CSS-sized
  viewport.
- Documented resvg limitations with corrected expectations: filter-region
  SourceGraphic clipping before the primitive chain, sRGB-only gradient and
  mask-luminance interpolation, automatic marker orientation from raw segment
  tangents, and dropped width<=1 strokes on canvases of at most two device
  pixels.

Details are recorded in the focused test files and docs/svg-converter-workflow.md. Unit coverage and
static analysis still gate the milestone.

The unit-test obligations from that run are closed (see above). The Chromium
parity obligations from that run remain the open work:

The earlier complete Chromium run had 66 failures out of 511 cases. The SVG workflow audit below supersedes this historical inventory:

| Group | Failing cases | Work |
| --- | ---: | --- |
| SVG | 55 | Shapes/edges, text lengths/paths/fonts/strokes, gradients/patterns, clipping/masks/filters, markers and selected-document geometry |
| MathML | 5 | Scripts, fractions, radicals, stretch rows and operators |
| Other HTML/PDF integration | 6 | Embedded viewport reflow, blank viewports, initial child MIME rendering, default picker, decorated background fragments and tiny-matrix form flattening |

Preserve the original sources and pixel thresholds. A failure in this table is
not itself evidence that a feature is unsupported upstream. A diagnostic run at
8x SVG resolution still differed on about 2.2% of the basic geometry fixture's
pixels, and a supersampled box-filtered resvg raster still differed on about
4% of the densest SVG panel's pixels against Chromium at the same raster size.
Those measurements do not establish that the remaining differences are caused
by anti-aliasing or that strict parity is impossible. Classifying actual
integration defects and verified converter differences remains R10 work.

Verified converter limitations already return `unsupported_html` diagnostics:
foreignObject HTML, non-scaling strokes, and SVG2 textPath inline path data.
Keep their original Chromium references as limitation evidence. They do not count
as successful pixel comparisons. SVG text is raster artwork, and internal SVG
links do not become PDF links. An enclosing HTML anchor still works.

R4 also needs a focused contract review. Current pattern tests cover common
Unicode-v sets, captures and UTF-16 escapes, not full ECMAScript conformance.
Valid patterns rejected by OTP must not be silently treated as invalid authored
patterns. Matching work must be bounded across the whole values list. Current
worker memory checks occur before and after evaluation; they are not a hard
allocator cap. Initial URL admission still needs broader IDNA and non-special
authority coverage. These remain qualification work, even if existing tests pass.

## Performance repair

The October 17 retained source beats the original elapsed-time reference on all
19 workloads on both supported runtimes. The original revision and all guards
remain unchanged. Three captures per source/runtime provide 15 timed samples
per workload. All raw samples and original rerun failures remain in
[the performance report](performance-1.1.0.md).

The third pass removes cascade-only maps and retained DOM roots, skips impossible
table-header scans, avoids paint-plan allocation during measurement and reuses
bounded font compression. A scoped font-working-set collection threshold avoids
repeated major collections and restores the caller's process setting on every
exit. Sampled whole-VM memory can rise by up to 28.7 MiB against fresh original.
All 19 output probes match the pre-third-pass results byte for byte on both
runtimes. The complete matrix and exact host Chromium command pass.

The tables below describe earlier captures, before these optimizations. The historical
reference does not turn the remaining original elapsed-time failures into passes.

The original revision is still `fee6c4d6051844e8d82041ba9e83939b28e1c724`.
Fresh captures of that revision use the current font inventory and exactly
matching supported runtimes. Five added font files prevented comparison with the
older report's inventory. Existing original files and all library source were
preserved; the original reports remain retained separately.

| Runtime | Original workloads measured | Timing failures above 10% | Reduction-guard failures | Largest measured timing increase |
| --- | ---: | ---: | ---: | ---: |
| Elixir 1.19.5 / ERTS 16.4 | 19 | 9 | 7 | 61.9% |
| Elixir 1.20.2 / ERTS 17.0.4 | 19 | 8 | 7 | 76.2% |

At that earlier checkpoint, the largest regressions were the 100-row render/full-font render and PDF text
extraction workloads. Table headings, requisition forms and ordinary form
preparation also need work. Smaller near-threshold timing results need bounded
rechecks after the substantial regressions are fixed. All current output-size
guards pass with the separately approved government-form size baseline.
Timing varies between captures; both bounded captures failed overall. The
reduction failures persist on both runtimes.

The performance script now retains measurements for all original workloads and
reports all guard failures before failing the run. It changes no acceptance
threshold. Limits still apply cumulatively across the milestone.

## Remaining milestone scope after recovery

These six audits determine what is still required by the HTML milestone. Each
must produce concrete examples, existing implementation/test references, and
separate rows for implemented, missing and intentionally deferred behavior.
An unsupported diagnostic does not establish implementation of an included
renderable HTML behavior.

| ID | Audit | Required accounting |
| --- | --- | --- |
| S1 | HTML parsing and controls | Structure and omission rules; associated/selected controls; initial numeric, temporal, color, file and image states; constraint state; dialog and legacy markup |
| S2 | Layout and pagination | Intrinsic and calculated dimensions; positioned/replaced sizing; table spans, rows, captions and decorated continuation; lists and ruby wrapping |
| S3 | Resources and composed documents | Responsive selection; fitting/maps; template replacement and clone source ownership; shadow/slots; child controls, links, fragments and iframe/object/embed/frameset selection |
| S4 | SVG as HTML image artwork | Host CSS and used sizing, original source admission, authorized nested resources/fonts, cache/budget behavior and explicit converter limitations; no custom painter |
| S5 | MathML in HTML | Operators, fractions, radicals, scripts/limits/accents, grouping/errors/padding, matrices, semantics/annotations, selected fonts, baselines and mixed equation flow |
| S6 | Combined documents and support guide | Real multipage mixtures, original content, visibility, links, fields, resources, pages and truthful documented support; retain separately deferred CSS/typography/accessibility/PDF-rendering scope |

The six audits are scope-reconciliation tasks, not claims that six feature
batches will finish the milestone. Missing implementations discovered by them
must be listed individually with dependencies and acceptance tests before new
feature work starts. Previous batch/wave estimates are not reused.

## Acceptance rules

- Run `./scripts/quality-matrix` on the final combined source, for both runtimes
  in `ci/runtime-matrix.json`. Inspect every FAIL and WARN log. SKIP is unresolved.
- Run `CHROMIUM_BIN=/usr/bin/chromium mise exec -- mix test.browser_parity`.
  Preserve original authored fixtures, fonts, page/content assertions and strict
  pixel thresholds. Separate explicit unsupported-feature checks from successful
  visual comparisons.
- Require meaningful 100% coverage per module and total. Do not exclude reachable
  production code, fabricate preparation state or delete reachable diagnostics.
- Keep `fee6c4d6051844e8d82041ba9e83939b28e1c724` as the performance baseline.
  Compare identical original workloads/fonts/inputs, matching runtime and VM
  settings on the same idle host. Keep previous measurements. Re-capture the
  original revision when runtime/input compatibility prevents comparison.
- Enforce at most 10% cumulative elapsed-time regression per original workload,
  existing reduction guards, output-size bounds and absolute timing ceilings.
  The allowance applies to the whole milestone, not each batch. The user
  subsequently required optimization toward the original timing itself; merely
  reaching the allowance is not sufficient. A skipped or incompatible comparison
  does not pass.
- The government form has a separately approved PDF-size baseline of 688,413
  bytes, replacing 256,688 bytes. The retained full source font for later field
  editing contributes 382,174 compressed bytes. The existing 5% size allowance
  now caps this fixture at 722,833 bytes. This changes no timing or reduction
  baseline and does not approve size increases for other workloads.
- Use one owner for expensive validation. Record source hashes, commands, runtime,
  input identity, logs and results. Changes after validation require appropriate
  requalification.

## Earlier recovery evidence

The source used by the checks below is recorded in
`tmp/elixir-runtime-recovery/matrix-third-source.json`. Both performance
reports match that historical source. Original baseline library hashes were
verified against Git revision `fee6c4d6051844e8d82041ba9e83939b28e1c724`.

| Check | Result | Local evidence |
| --- | --- | --- |
| Required quality matrix | FAIL on both runtimes; compilation/formatting pass, coverage fails; later matrix stages skipped | `.quality/logs/20261015T013056Z-1466706/` |
| Separate supported-runtime Dialyzer | PASS on both; zero errors and zero skipped findings | `tmp/elixir-runtime-recovery/dialyzer-third-1.19.log` and `dialyzer-third-1.20.log` |
| Exact host Chromium command | FAIL; 445/511 pass, 66 fail | `tmp/elixir-runtime-recovery/chrome-third.log` |
| Matched performance captures | FAIL on both; all 19 original workloads measured, size guards pass | `.quality/performance/results/elixir-runtime-recovery/reports.json` |
| Original revision with matching font inventory | Both captures retained with original library source verified | `.quality/performance/baselines/1.1.0/font-inventory-20261015/reports.json` |

Every matrix FAIL log was inspected; there were no WARN entries. Current logs
and measurements remain local. This is an unaccepted development baseline.
The user's pre-existing `CHANGELOG.md` changes remain untouched.

## Test audit and browser runtime reduction, October 16

The user prioritized useful regression checks and shorter CI runs over test
counts. Subsequent cleanup supersedes the historical counts and coverage status
in the recovery evidence above. The full milestone remains unaccepted.

- Ordinary tests now total 3,804 after removing redundant or weak checks and
  comparisons without a Chrome reference. No production changes were needed for
  the browser runtime reduction.
- Before the SVG workflow audit below, browser registrations totaled 410. Ten typography/layout cases share two
  compact pages. Another 36 MathML, nine SVG, and 23 HTML cases share twelve
  generated pages. Original authored sources remain intact.
- Generated-sheet controls verified identical native and Chrome raster crops for
  each retained sample. The most recent 23 HTML controls are recorded in
  `tmp/browser-speed/html-accepted-crops.json`; earlier MathML/SVG controls remain
  under `tmp/browser-batching/`. Every region retains its full original canvas,
  ink-presence assertion, and one-percent changed-pixel ceiling.
- Independent ExUnit modules run concurrently with six workers. Tests that change
  process-wide Fontconfig or resource-limit settings remain serial. Chrome uses
  a fresh browser context for every comparison and reuses its processes through
  the DevTools pipe. The original command-line path remains available using
  `BROWSER_PARITY_BROWSER=cli`.
- Full host logs, failure inventories, and comparison controls for this work live
  under `tmp/browser-speed/`. The final source manifest and validation summary
  there identify the exact source and evidence. Required matrix reports remain
  under `.quality/logs/`.

The quality runner retains both supported runtimes. Elixir 1.19 pulls the published
`sigdevelopers/native-elixir-pdf-ci:elixir-1.19` tools image. Elixir 1.20 uses
SigPortal v2.8 directly. Routine validation reuses or pulls images, with no image
builds or system-package installation. Each run resolves image tags to immutable
IDs. The local image Dockerfile and publishing instructions have been removed
now that the shared image is hosted on DockerHub.

The previously suggested blurred SVG comparison was not validated and is not
implemented. The user already conditionally authorized investigating a fair
comparison. There is no pending approval for that investigation. Original strict
results remain recorded separately from the Chrome PNG workflow comparison.
The thresholds remain unchanged.

At this historical checkpoint, rendering failures, coverage, cumulative timing
and the complete matrix were still unresolved. The current validation and
performance sections above supersede that status. The earlier table and
validator scope inventory still requires reconciliation against the final
combined implementation.

## SVG workflow scope, October 16

The user narrowed routine SVG acceptance to document extraction, PNG conversion,
and faithful image appearance, dimensions and placement. The
[SVG browser audit](svg-browser-workflow-audit.md) records every removed input
and the regression coverage that remains. Specialized converter conformance,
repeated legacy painter probes, a stale unsupported-input success expectation
and a manual byte-adapter comparison are retired from routine browser acceptance.
Original authored fixtures and strict failure evidence remain intact.

This removes 27 original cases and 22 browser registrations. Moving the retained
mask sample onto the other SVG page saves one further registration. The suite
now has 382 browser registrations after the non-SVG cleanup and one PNG-reference
negative-control test. The 32 focused SVG tests still cover 35 original samples;
mixed MathML/template comparisons also exercise SVG. The ordinary suite has
3,804 tests. The later repairs above change raster density, ordinary image
placement and root-transform transport. Pixel tolerances remain unchanged.
Removed comparisons are outside the revised acceptance scope; their rendering
discrepancies have not been fixed.

The nested-template original-source browser check is retired after a verified
Chrome 152/155 semantic difference. Chrome 155 applies the inner `template for`
replacement; Chrome 152 and the library's original-source policy keep it inert.
The ordinary nested-candidate validation/mutation tests remain, as do eight
other template browser checks. Its original fixture is unchanged and its failed
reference is retained in `tmp/workflow-repair/nested-template-chrome155-strict/`.
This is a documented template-policy deviation, not a fixed original-source
Chrome comparison. It removes one browser registration, leaving 382.
