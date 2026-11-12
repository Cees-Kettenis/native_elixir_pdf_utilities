# Testing

Run from the repository root. See [Contributing](../../CONTRIBUTING.md) for setup
and [architecture](architecture.md) for source ownership. Omit `mise exec --`
when Elixir/Erlang are already on your PATH.

## Focused checks

| Check | Command |
| --- | --- |
| One suite | `mise exec -- mix test test/html_to_pdf/tables/atomic_table_overflow_test.exs` |
| Formatting | `mise exec -- mix format --check-formatted` |
| Coverage | `mise exec -- mix test --cover --warnings-as-errors` |
| Dialyzer | `MIX_ENV=test mise exec -- mix dialyzer` |
| Performance | `mise exec -- ./scripts/check-performance` |

Run focused checks while working and the full matrix before submitting a PR.

## Full quality matrix

```bash
./scripts/quality-matrix
```

| Prerequisite | Requirement |
| --- | --- |
| Tools | Running Docker, Git, `tar`, `jq`, Bash 4+ |
| Linux | Docker |
| macOS | Docker Desktop and Bash 4+ |
| Windows | WSL2 with Docker Desktop integration; native PowerShell/Command Prompt unsupported |
| Storage | Writable checkout and `.quality/` |

The [runtime matrix](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/HEAD/ci/runtime-matrix.json) is shared with CI:

| Elixir | Image |
| --- | --- |
| 1.19 | `sigdevelopers/native-elixir-pdf-ci:elixir-1.19` |
| 1.20, canonical | `sigdevelopers/sigportal-ci:v2.8` |

| Check | Every runtime | Canonical only |
| --- | --- | --- |
| Compile with warnings as errors | Yes | |
| Ordinary tests and Chromium parity | Yes | |
| 100% coverage, including ordinary tests | | Yes |
| Formatting, unused dependencies, Dialyzer | | Yes |
| Performance guards/report | | Yes |

| Runner behavior | Details |
| --- | --- |
| Images | Include Chromium, Poppler and fonts; missing images pulled, immutable IDs recorded. Refresh deliberately with `docker pull <image>` |
| Source | Tracked and non-ignored untracked bytes, including edits, frozen in `.quality/logs/<run>/source.tar` |
| Isolation | Separate source copies, outputs and runtime caches; application build cleaned before compile |
| Concurrency | One correctness lane by default; raise `QUALITY_MATRIX_JOBS` only when the host can support simultaneous runtime workloads |
| Performance | Starts after all local correctness containers exit; CI runs it after ordinary checks on a runner separate from browser parity |
| Failures | Later stages skip for that runtime; other runtimes continue |
| Lock recovery | Inspect `.quality/matrix.lock` PID and `pdf-quality-<run>-*` containers; remove only after confirming the run stopped |
| CI agreement | Same check policy and `--matrix-json` validation; host orchestration tests cover concurrency, isolation, failure reporting, performance ordering and future-runtime policy |
| Canonical runtime | Newest supported minor; coverage already includes its ordinary suite |
| Runtime updates | Once a 1.21 image is available, add it to the matrix, keep three minors and measure its performance baseline |

Keep browser cases and performance thresholds unchanged.

### Results and warnings

| Evidence under `.quality/logs/<run>/` | Contains |
| --- | --- |
| `results.tsv` | Runtime, stage, result, warning count, log path and seconds |
| Per-runtime logs/workspaces | Stage logs, `container-checks.log` and generated outputs |
| Canonical performance files | `container-performance.log` and `performance.json` |
| Source/configuration/image IDs | Identity of the validated inputs and runtimes |

| Result | Action |
| --- | --- |
| `PASS` | Passed; still inspect warning counts |
| `FAIL` / `WARN` | Read the referenced log; resolve project issues and identify third-party warnings/upstream fixes |
| `SKIP` | Fix the earlier failure |
| `N/A` | Expected for canonical-only checks, or tests already included in coverage |

A successful exit can include warnings. Report commands, runtimes, results and
unresolved issues in the PR. Fix failures, rerun affected checks and require the
complete matrix to pass before review readiness.

Counts include plain `warning:` messages and `file:line[:column]: Warning:`
diagnostics, with terminal colours ignored. Project and third-party warnings both
count. Use source paths and the dependency or PLT-build context in the stage log
to identify their origin; a bare filename alone does not establish ownership.

## Publishing documentation

Use the latest supported runtime when preparing a package:

```bash
MIX_ENV=dev mise exec -- mix deps.compile
MIX_ENV=dev mise exec -- mix docs --warnings-as-errors
```

Resolve errors before publishing. Documentation builds are excluded from routine matrix/CI checks.

## Browser parity

Visible rendering changes require focused unit coverage and a Chromium fixture.
Every supported runtime runs the full parity suite.

Pin the specification section behind a CSS expectation to a dated publication
or an editor's draft commit, and identify provisional or historical syntax.
Check that the reference browser renders the behavior; accepting its syntax is
insufficient. Where Chromium lacks support, use an independently authored
reference, such as a WPT reference document, or specification-derived geometry,
font metrics or numerical vectors. Record how the expected result was obtained.
Comparing two outputs from this library alone does not establish conformance.

Reuse an existing page group or shared browser reference when adding a scenario.
Keep each scenario's original native input, thresholds and assertions. Explain
independent captures in their owning tests, and verify grouping with the
equivalence commands below on every supported runtime. Use telemetry to report
and justify added browser launches, raster work or unexpected reference fallbacks.

CI runs one complete browser parity job per runtime alongside one job containing
all ordinary checks. There are no browser test partitions. The existing
`Elixir <version>` checks require every job to pass, including failure,
cancellation and skipped-job reporting. The local matrix invokes the same full
suite once per runtime and clears any inherited `MIX_TEST_PARTITION` value.

| Task | Command |
| --- | --- |
| Complete host parity suite | `CHROMIUM_BIN=/usr/bin/chromium mise exec -- mix test.browser_parity` |
| One dedicated suite | `mise exec -- mix test.browser_parity test/html_to_pdf/browser_parity/curved_border_browser_parity_test.exs` |
| One form fixture and its native modes | `mise exec -- mix test --only reference_fixture:html_initial_number_controls.html --warnings-as-errors` |
| One scenario's owning sheet | `mise exec -- mix test --only css_text_atomic_intrinsics --warnings-as-errors` |
| Diagnose CLI printing | `BROWSER_PARITY_BROWSER=cli mise exec -- mix test.browser_parity` |

The parity alias runs one module at a time. Ordinary tests retain their normal
ExUnit concurrency. Multiple `--only` selections are alternatives, so adding a
feature selection to the parity alias also selects its existing broad
`browser_parity` tag. Use `mix test --only <feature>` for feature selection.

| Concern | Requirement or behavior |
| --- | --- |
| Host tools | Chromium, Poppler and matching fonts; matrix includes DejaVu/Liberation. Inspect PDF fonts before changing layout expectations |
| Reference | Chromium renders authored source; Poppler rasterizes both PDFs identically. Native-to-native checks cannot replace this |
| Comparisons | Preserve source bytes, geometry, fonts, viewport, scale, thresholds and page/text/ink/pixel checks across adapters and fixture moves |
| Execution | One parity module at a time; process-wide environment changes and load-sensitive cases stay serial |
| Browser state | Fresh process and context per capture, print media, author JavaScript disabled; processes/profiles cleaned afterward |
| Input encoding | Generated reference inputs declare UTF-8 with a BOM; authored fixture bytes remain unchanged |
| Readiness | Chromium virtual time pauses for pending fetches; wait for frames, fonts and image decoding, then verify a stable frame tree. Capture deadlines use wall time; diagnostics stay beside PDFs |
| Failures | Use suite timeout, preserve broken-resource fallback; never retry to hide a failed comparison |
| Shared sheets | Independent cases retain their original canvas, per-region thresholds, visible-ink and semantic checks. Scenario tags select the owning sheet. Root-document and pagination cases stay separate |
| Shared references | Compatible native variants may share immutable browser artifacts within their suite. Preparation is lazy so focused selections capture only their fixtures. Every variant retains its native render and semantic assertions; source, transformed input, fonts, geometry, density, resource base and browser environment must match |
| Shared document pages | Reviewed originals share one Chromium print, each starting on its own named page or page range. Every test still renders its original native input with its original options and checks its own reference pages, thresholds and semantic assertions |

`pages.json` records the original source hashes, page counts, physical page bounds,
browser options and compiled document hash. Nonpainting boundary annotations check
every scenario's start page before extracting its reference PDF. Sources with
conflicting identifiers, document-dependent behavior or changed rendering retain
independent captures. No frames are added to compose scenarios.

The page references are lazy and scoped to one test run. They apply only to the
reviewed Chromium, Poppler, supplied font bytes, Fontconfig configuration and
locale. An unverified environment or different browser options uses the original
capture path. Set `BROWSER_PARITY_PAGES=off` to compare the same suite with
independent original captures. Failed shared captures fail the test without a retry.

Run `MIX_ENV=test mise exec -- mix run scripts/fixtures/check-browser-page-equivalence.exs`
in each supported runtime after changing grouped sources or the page compiler.
It compares every original Chromium page byte for byte after rasterization, then
checks physical page bounds, text semantics and positioned text separately.
Keep the originals and their existing native render paths. Record fresh hashes
and the verified tool/font/environment profile only after equivalence passes.

The text comparison sheets and new regions in the table sheet use an explicitly
limited composition route independent of the production CSS parser. Their
`sheets.json` scenario metadata records source
hashes, feature owners, original canvases/margins, thresholds and region text
assertions. Fractional print margins also record their effective position after
CSS pixel rounding. Changed sources need a new equivalence check. Unsupported selectors
fail composition rather than receiving an approximate rewrite. Percentage widths
are limited to reviewed table columns with an explicit fixed table width. The
retained original files remain provenance inputs.

Run `MIX_ENV=test mise exec -- mix run scripts/fixtures/check-browser-sheet-equivalence.exs`
after changing those samples. It verifies original versus sheet-region pixels
and positioned text separately for native and Chromium output. Sheet extensions
also reconstruct the previous sheet, verify its recorded HTML hash, and check
that additions cover only previously white space. Existing pixels, positioned
text and public form fields must remain unchanged. Set `BROWSER_SHEET_BASELINE`
to a directory of previously captured sheet artifacts to use those instead of
reconstructing them. Keep evidence in `.quality/` and run the check in each
supported runtime environment.

Parity records invocation and phase data under `.quality/browser-parity/<run>/`
for host/CI commands and under each matrix runtime's `parity-events/` directory.
Use `BROWSER_PARITY_TELEMETRY=<directory>` to choose a separate evidence directory,
or `off` for an uninstrumented investigation. Records include failed and
interrupted probes. Expected failure tests can produce error records even when
ExUnit passes; telemetry never replaces the test result.

Summarize an evidence directory with
`MIX_ENV=test mise exec -- mix run --no-start scripts/browser-parity-report.exs <directory>`.
The report counts launches, native pipelines, raster pages/pixels and comparison
owners. Phase timings overlap, so do not sum them or substitute them for the full
command's elapsed time. CI uploads telemetry on success and failure; failed
comparisons retain PDFs, raster pages, readiness logs and font information.

See [fixture scripts](scripts.md) for regeneration; inspect affected regions afterward.

Tests that install application limits or change process-wide environment must
run with `async: false`. Synchronize worker lifecycle assertions with messages
before triggering cancellation; a longer timeout cannot fix a monitor race.
For deadline behavior, advance an injected clock after the tested operation
starts so browser startup speed does not determine the assertion.

## Performance measurements

| Guard | Requirement |
| --- | --- |
| Reductions | At or below recorded v1.0.0 baselines, zero regression allowance |
| Time and size | One-second workload ceiling plus output-size guard |
| Baseline changes | Only intentional workload changes or measured, explained tradeoffs; adding features never resets existing baselines |
| VM settings | Use `scripts/check-performance`: 4 normal, 4 dirty CPU, 10 dirty IO schedulers locally and in CI |

Save a complete report:

```bash
PERFORMANCE_REPORT=.quality/performance/candidate.json mise exec -- ./scripts/check-performance
```

Compare cumulative timing against the original capture from the start of the work:

```bash
PERFORMANCE_BASELINE_REPORT=.quality/performance/baselines/reports.json \
PERFORMANCE_REPORT=.quality/performance/candidate.json mise exec -- ./scripts/check-performance
```

| Comparison/report detail | Rule |
| --- | --- |
| Fixed capture | Same idle host, runtime, scheduler settings, workloads, fixture/font hashes and dependency lock |
| Baseline file | Single report or bundle; bundle defaults to `elixir-<major.minor>.json`, overridden by `PERFORMANCE_BASELINE_KEY` |
| Timing | Every existing workload must meet its original elapsed baseline across the whole change; preserve that baseline when recapturing |
| Evidence | Keep samples, medians, reductions, PDF sizes, runtime details and hashes, including failures; CI uploads failed reports too |
| Memory | Separate BEAM sampling avoids timing effects but misses peaks/native allocations; use an OS profiler for native memory |
| Retries | Other builds distort timing. Retain all attempts, compare whole workload sets and keep inputs/limits fixed; never combine selected passing workloads |
| Slowdowns | Investigate reproducible baseline regressions or obtain an explicit exception |
| Storage | Reports/logs in ignored `.quality/`; investigations in ignored `tmp/` |

## Test organization

Paths are relative to `test/`; grouped `html_to_pdf/` entries name sibling directories.

| Directory | Coverage |
| --- | --- |
| `api/` | Public operations and rendered results |
| `core/` | Diagnostics and limits |
| `pdf/` | Reader, object mapping, encodings and appearances |
| `validators/` | Input boundaries and diagnostics |
| `project/` | Style and project structure |
| `html_to_pdf/{parsing,css}/` | DOM and computed styles |
| `html_to_pdf/{layout,tables,ruby}/` | Geometry and pagination |
| `html_to_pdf/{forms,images,fonts}/` | Controls, images and fonts |
| `html_to_pdf/{composition,embedded}/` | Templates, shadows, source ownership and child documents |
| `html_to_pdf/{mathml,svg}/` | Math and SVG conversion |
| `html_to_pdf/{resources,writing}/` | Resource scope, caches and PDF paint |
| `html_to_pdf/integration/` | Cross-feature behavior |
| `html_to_pdf/browser_parity/` | Dedicated Chromium comparisons |
| `support/` | Shared builders, paths, geometry and browser helpers |

`*_browser_parity_test.exs` suites and individual `:browser_parity` cases compare with Chromium.

## Naming

| Item | Convention |
| --- | --- |
| Suite/module | `<feature>_<scenario>_test.exs` / `FeatureScenarioTest` |
| Example | `tables/atomic_table_overflow_test.exs` under `test/html_to_pdf/` |
| Test description | Observable behavior and trigger, with a clear feature owner or integration purpose |
| Fixtures, tags, artifacts | Same feature/scenario vocabulary, such as `atomic_table_hidden_overflow.html` |
| Temporary labels | Batch numbers and unfinished-work labels belong in local records |

## Shared paths and fixtures

Use `TestSupport.Paths` so file nesting does not affect input lookup:

```elixir
alias NativeElixirPdfUtilities.TestSupport.Paths

html = File.read!(Paths.fixture("html_to_pdf/browser_parity/atomic_table_hidden_overflow.html"))
font = Paths.project("priv/fonts/dejavu/DejaVuSans.ttf")
```

| Location/concern | Rule |
| --- | --- |
| `fixtures/html_to_pdf/browser_parity/` | Registered HTML comparisons, feature sheets and reference-only inputs; preserve the shared `assets/` resource base |
| `fixtures/html_to_pdf/documents/` | Invoice, order, label and report samples |
| `fixtures/images/jpeg/` | JPEG encoding cases |
| Other fixtures | Existing PDF, font, template and converter feature directories |
| Temporary outputs | Unique feature/scenario directories in `tmp/`; create and clean up owned inputs |
| Browser sibling files | Helper preserves the authored relative-resource base and removes temporary input after printing |

Font tests share two small variable-font sources and six independently encoded
WOFF2 samples. `test/support/font_corpus.ex` constructs color tables, metric/control
variants, collection wrappers and long programs under ignored `tmp/font-corpus/`
at suite startup. It needs no external encoder or installed test fonts. Separate
fixture manifests retain the independently measured outlines, metrics and paint
matrices. The browser page-equivalence command prepares the same corpus.

Unicode typography data and conformance vectors are pinned to Unicode 17.0.0.
The grapheme suite checks the complete 766-case extended grapheme corpus,
including Indic conjuncts, emoji joins and regional-indicator pairs. Data source
hashes are in `scripts/data/unicode-text-source.json`; corpus files retain their
source URL and hash. The builders under `scripts/data/` consume local UCD files
and do not download data during tests or rendering.
