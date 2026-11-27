# Testing

Run from the repository root. See [Contributing](../../CONTRIBUTING.md) for setup
and [architecture](architecture.md) for source ownership. Omit `mise exec --`
when Elixir/Erlang are already on your PATH.

## Focused checks

| Check | Command |
| --- | --- |
| One suite | `./scripts/quality-matrix --check test test/html_to_pdf/tables/atomic_table_overflow_test.exs` |
| Compile, formatting and unused dependencies | `./scripts/quality-matrix --check format` |
| Development app routes (from `dev/manual_web/`) | `mix deps.get && mix test --warnings-as-errors` |
| Standalone 100% coverage | `./scripts/quality-matrix --check coverage` |
| One Chromium parity suite | `./scripts/quality-matrix --check parity test/html_to_pdf/browser_parity/curved_border_browser_parity_test.exs` |
| Full Chromium parity only | `./scripts/quality-matrix --check parity` |
| Dialyzer | `MIX_ENV=test mise exec -- mix dialyzer` |
| Performance | `mise exec -- ./scripts/check-performance` |

Each `--check` uses the CI images and freezes the current source at startup.
Test, format and coverage use the canonical runtime. Parity uses every supported
runtime. Supply paths or Mix test arguments to `test` and `parity` to check a
specific change and its affected existing behavior. Coverage runs the full
ordinary suite and enforces the same threshold as CI.

Run parity and coverage in separate terminals or background jobs while reviewing
the change. Each check and runtime has its own build and browser reference cache.
Set `QUALITY_MATRIX_JOBS=2` on the parity command to run two runtimes together
when the host has CPU and memory to spare. Wait for running checks before starting
performance measurements or the full matrix. Results, warnings and source
snapshots are under `.quality/checks/<check>/logs/<run>/`. Restart affected
checks after changing their inputs.

All nine SVG converter discrepancy cases compare against Chromium on every run.
The fractional-pattern case also captures twice and checks identical PNG bytes.
Before publishing, run the repeat check for every case on both supported runtimes:

```bash
SVG_DISCREPANCY_REPEATS=all ./scripts/quality-matrix --check parity test/html_to_pdf/browser_parity/svg_converter_discrepancy_browser_parity_test.exs
```

The tag workflow runs this check before creating the GitHub release. Push and
pull-request CI repeat every case unless the browser-reference archive for the
base commit matches the current runtime image and SVG, dependency, fixture and
capture-harness inputs. A missing archive selects all repeats.

For changes beyond documentation, run focused checks while working and the full
matrix before submitting a PR. Documentation-only changes, including edits limited
to `@doc` and `@spec`, are exempt from all tests and the quality matrix when no
executable code, configuration, tests or fixtures change. Review the content and
state that exemption in the handoff.

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
| CI agreement | Local and CI runs share the library gates and `--matrix-json` validation. CI also runs the development app route tests listed above. Host orchestration tests cover concurrency, isolation, failure reporting, performance ordering and future-runtime policy |
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
unresolved issues in the PR. For changes beyond documentation, fix failures,
rerun affected checks and require the complete matrix to pass before review
readiness.

Counts include plain `warning:` messages and `file:line[:column]: Warning:`
diagnostics, with terminal colours ignored. Project and third-party warnings both
count. Use source paths and the dependency or PLT-build context in the stage log
to identify their origin; a bare filename alone does not establish ownership.

The local coordinator retains the current matrix run and the four newest previous
run directories in `.quality/logs/`, including failed or interrupted attempts.
Older runs are removed when a new run starts, after acquiring the matrix lock.
Copy evidence elsewhere before starting another run if it needs longer retention.
Runtime dependency/build/PLT caches, the bounded browser-reference cache and other
local records outside those run directories are preserved. Cleanup ignores
symlinks and directories whose names do not match the coordinator's run format.

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

Mix caches are owned separately by checks and parity jobs. Only the checks job
creates the canonical dependency PLT under `_build/test/`; parity must not save
an earlier archive under that same immutable key. Runtime and dependency-lock
identities still control restoration and invalidation.

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

Use `async: true` for independent ordinary suites. Cases in a module still run
one at a time; other async modules can run alongside it. Keep mixed features'
global-state cases in a separate serial module, and keep shared fixture helpers
with their dependent cases rather than duplicating them to enable concurrency.

Tests that install application limits, change process-wide environment or compare
shared process/cache/temporary-directory inventories must run with `async: false`.
Synchronize worker lifecycle assertions with messages
before triggering cancellation; a longer timeout cannot fix a monitor race.
For deadline behavior, advance an injected clock after the tested operation
starts so browser startup speed does not determine the assertion.

### Compositor references for static effects

The compositing sheet uses `compositor_reference: :png` for its marked HTML
viewports. Chromium's PDF painter drops backface hiding, depth intersections and
mask exclusion, and paints colored form text with the wrong group opacity.
The shared capture adapter saves the original printed PDF, captures those source
viewports under print media, checks that capture leaves their geometry and styles
unchanged, then prints their PNG replacements. The authored native HTML is unchanged.
Existing vector regions retain their 1% thresholds; raster regions use the SVG
comparison tolerance and independent geometry or channel assertions.

The forty scenes share one browser reference. Nine compositor viewports add nine
screenshots and one second print in that capture, using the same browser process.
They participate in the normal bounded reference cache with a separate capture
mode identity. A cache hit still renders and compares every native region.

### Persistent Chromium references

The local matrix reuses compressed browser references in
`.quality/browser-reference-cache/elixir-<minor>/`. Each runtime has a fixed
64 MiB retention budget. Startup removes interrupted writes; stores evict the
least recently used entries before writing. Oversized entries are not retained.
Use a separate directory for simultaneous independent suite processes.
Removing this directory is safe. Set `BROWSER_PARITY_CACHE=off` for fresh captures,
or set it to an ignored absolute directory for a separate experiment.

Reuse requires `BROWSER_PARITY_TOOLCHAIN` to identify the immutable container
image. The matrix supplies its recorded image ID. Host commands without that
identity use fresh captures. CI reads the job container's immutable image ID
from Docker and restores the same bounded cache in each parity job. Archives
are keyed by runtime, image ID and commit, with earlier commits in the same
toolchain available as a fallback. A same-commit rerun restores the exact archive;
the entry checks below still decide whether each reference can be reused.

For release verification, record the full parity command's elapsed time and its
reference-cache telemetry for each runtime. The target is 300 seconds per
runtime with one active Chromium capture. Record cache-populating and
cache-reusing runs when the cache format changes.

Keys include browser HTML, source bytes, the resource directory, physical fonts,
geometry, DPI, browser and rasterizer versions, locale, and a fingerprint of
bundled, fixture and system fonts and the browser helpers. Each entry also
records the content hashes of the local resources requested by Chromium.
Changing an unrelated fixture keeps that entry eligible. Changing or removing
a recorded dependency causes a fresh capture. Each suite fingerprints its
inputs at startup and keeps that snapshot for the run.

Only static HTML fixtures under `test/fixtures/html_to_pdf/` with local
dependencies covered by the snapshot are eligible.
The authored source remains part of the identity, while resource eligibility is
checked against the final transformed browser HTML. Known local placeholder
substitutions can therefore reuse references. Image-map navigation links are
treated like anchor links. Base64 SVG images are checked recursively for resource
dependencies; malformed encodings, scripts, animation and external XML declarations
remain ineligible.
Before storing a fresh capture, the helper also checks every observed Chromium
request against the dependency snapshot and its generated capture inputs.
Network resources, ambiguous resource syntax, external inputs and alternate
browser executables use fresh captures. Fontconfig reuse is limited to the
helper's exact generated declaration of fingerprinted local font directories;
other configurations use fresh captures. Explicit
`reference_cache: false` also disables persistence for a comparison. Browser
lifecycle and readiness tests continue exercising fresh processes directly.
Missing, damaged or incompatible entries regenerate the reference. Every native
render, rasterization, comparison and scenario assertion still runs; pass/fail
results are never cached. Prepared references retain their existing identity
and artifact integrity checks.


Docker matrix and CI containers put browser profiles in a dedicated 128 MiB tmpfs
at `/browser-profiles`. This is a fixed test-infrastructure storage budget, shared
by the fresh captures in that container. Captures still wait for real Chromium
exit before deleting their profiles; container removal releases the tmpfs. Logs,
PDFs and rasters remain in their normal artifact directories. Host probes use the
OS temporary directory unless `CHROMIUM_PROFILE_ROOT` names another writable
profile directory.


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

New or changed public failure modes need focused tests asserting the shared
`{:error, {reason, diagnostic}}` shape and the actionable diagnostic fields.
Follow the [public failure contract](architecture.md#public-failures), including
available source and operation context.

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
