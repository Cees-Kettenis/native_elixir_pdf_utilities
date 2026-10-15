# Milestone 1.1.0 work plan

Updated 2026-10-15. Current repair tasks and remaining milestone scope.

## Goal and order

Render useful static HTML to PDF. SVG artwork uses the existing upstream Resvg
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

No clean development baseline has been accepted yet. Work in this file is not
complete merely because code has been integrated. Test coverage and test pass
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
| R3 | Qualify the Elixir modern CSS color replacement | Existing color-input serialization, calculations, dimensional failures, malformed comma/comment cases and original Chromium fixtures pass | Focused tests pass; full coverage pending |
| R4 | Qualify Elixir HTML pattern and initial URL behavior | Unicode-v/UTF-16, whole-value, URL, resource limits, worker death and nonzero deadlines pass; audit Unicode property vocabulary, lookbehind and URL host conformance | Focused tests pass; conformance audit open |
| R5 | Repair ordinary HTML, MathML, layout, forms and source-validation regressions | Close the six HTML/PDF and five MathML browser failures listed below | Unit font defaults and source diagnostics repaired; browser failures open |
| R6 | Qualify nested SVG font policy and authorized font-byte transport | Shape-only outer artwork with a permitted text child works; disabled discovery remains restricted | Focused tests pass |
| R7 | Preserve XML spelling and viewport identity during SVG extraction | XML/inline spelling, namespace and used viewport tests pass; valid XML with both width and WIDTH must survive extraction | Focused viewport tests pass; canonical XML transport still open |
| R8 | Repair explicit SVG image sizing | Existing requested 20x10 PNG case yields the requested dimensions with preserved artwork | Open |
| R9 | Resolve tiny SVG canvas transport | Verify the original 1x1 marker against Chromium, then fix integration or issue a verified converter diagnostic; keep real allocation accounting | Open |
| R10 | Resolve converter/PDF rendering differences | Each original case passes strict Chromium comparison or has a verified explicit converter diagnostic; repair HTML transport defects | 55 SVG browser failures in the last complete run |
| R11 | Close meaningful coverage and static checks | 100% per module and total; supported-runtime static analysis has no project warnings | Static analysis passes both runtimes; coverage remains below 100% in 21 modules |
| R12 | Repair cumulative performance | Optimize table rendering, PDF extraction/writing and form preparation; every original workload must meet the original guards and 10% timing allowance | Both runtimes fail; measurements below |
| R13 | Remove superseded planning history and reconcile current documentation | One current plan; valid links; no obsolete first-party Rust or SVG vector/text claims | Old tracked planning/checkpoint files removed; documentation reconciliation ongoing |
| R14 | Accept the combined development baseline | Both supported quality matrices and exact host Chromium parity pass on the final source | Open |

These are 14 named recovery batches, including qualification and documentation.
They are not 14 remaining feature batches. R10 contains independent converter
differences, not one final small fix. No defensible milestone completion percentage
or delivery date exists until the six scope audits below have checked the code.

Use four waves, in order:

1. R1-R4: finish runtime replacement qualification and close its failures.
2. R5-R10: repair existing HTML/MathML regressions and SVG integration. Classify
   converter omissions only after reproducing them with stock Resvg and Chromium.
3. R11-R12: restore 100% coverage, static checks and the cumulative performance
   limits. The current table/extraction regressions need dedicated optimization.
4. R13-R14: reconcile documentation and rerun all gates on one frozen source.

These are integration waves, not a promise that four small patches finish the
milestone. New feature work starts after recovery acceptance.

## Concrete regression inventory

Both supported matrix runs have 13 failing unit tests out of 3,903.
Coverage is 99.58% on 1.19 and 99.59% on 1.20. Source-context
expectations, foreignObject limitation expectations, empty-artwork assertions
and unreadable compressed image streams have focused repairs.

The remaining rendering obligations from that run are:

- SVG image sizing, one-pixel marker paint and large text rotations.
- Fractional background tiles, mixed clip winding and four marker geometry cases.
  The marker-percentage expectation also needs correction against actual Chromium.
- Filter ink displaced into the canvas and filter/mask color transfer.

The complete Chromium run had 66 failures out of 511 cases:

| Group | Failing cases | Work |
| --- | ---: | --- |
| SVG | 55 | Shapes/edges, text lengths/paths/fonts/strokes, gradients/patterns, clipping/masks/filters, markers and selected-document geometry |
| MathML | 5 | Scripts, fractions, radicals, stretch rows and operators |
| Other HTML/PDF integration | 6 | Embedded viewport reflow, blank viewports, initial child MIME rendering, default picker, decorated background fragments and tiny-matrix form flattening |

Preserve the original sources and pixel thresholds. A failure in this table is
not itself evidence that a feature is unsupported upstream. A diagnostic run at
8x SVG resolution still differed on about 2.2% of the basic geometry fixture's
pixels. Increasing raster resolution alone has not cleared that comparison.

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

The original revision is still `fee6c4d6051844e8d82041ba9e83939b28e1c724`.
Fresh captures of that revision use the current font inventory and exactly
matching supported runtimes. Five added font files prevented comparison with the
older report's inventory. Existing original files and all library source were
preserved; the original reports remain retained separately.

| Runtime | Original workloads measured | Timing failures above 10% | Reduction-guard failures | Largest measured timing increase |
| --- | ---: | ---: | ---: | ---: |
| Elixir 1.19.5 / ERTS 16.4 | 19 | 9 | 7 | 61.9% |
| Elixir 1.20.2 / ERTS 17.0.4 | 19 | 8 | 7 | 76.2% |

The largest regressions are the 100-row render/full-font render and PDF text
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
  The allowance applies to the whole milestone, not each batch. A skipped or
  incompatible comparison does not pass.
- The government form has a separately approved PDF-size baseline of 688,413
  bytes, replacing 256,688 bytes. The retained full source font for later field
  editing contributes 382,174 compressed bytes. The existing 5% size allowance
  now caps this fixture at 722,833 bytes. This changes no timing or reduction
  baseline and does not approve size increases for other workloads.
- Use one owner for expensive validation. Record source hashes, commands, runtime,
  input identity, logs and results. Changes after validation require appropriate
  requalification.

## Current evidence

The source used by the checks below is recorded in
`tmp/elixir-runtime-recovery/matrix-third-source.json`. Both final performance
reports match the current library source. Original baseline library hashes were
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
