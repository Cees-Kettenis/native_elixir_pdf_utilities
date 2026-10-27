# Testing

See [Contributing](../../CONTRIBUTING.md) for setup and
[architecture](architecture.md) for source ownership. Run commands from the repository root. Tests use
`NativeElixirPdfUtilities.TestSupport.Paths` to locate repository inputs without
assuming how deeply the test file is nested.

| Directory | Coverage |
| --- | --- |
| `api/` | Public PDF operations and HTML rendering results |
| `core/` | Diagnostics and configurable limits |
| `pdf/` | PDF reader, object mapping, encodings and form appearances |
| `validators/` | Validator input boundaries and actionable diagnostics |
| `project/` | Code style and maintained project structure |
| `html_to_pdf/parsing/`, `css/` | Document construction, CSS source and computed styles |
| `html_to_pdf/layout/`, `tables/`, `ruby/` | Geometry, pagination and feature-specific layout |
| `html_to_pdf/forms/`, `images/`, `fonts/` | Controls, replaced images and selected fonts |
| `html_to_pdf/composition/`, `embedded/` | Templates, shadows, source ownership and child documents |
| `html_to_pdf/mathml/`, `svg/` | Math layout and SVG conversion/transport |
| `html_to_pdf/resources/`, `writing/` | Resource scope, caching and native PDF paint |
| `html_to_pdf/integration/` | Interactions across multiple feature owners |
| `html_to_pdf/browser_parity/` | Dedicated Chromium comparison suites |
| `support/` | Shared input builders, path lookup, geometry assertions and browser tools |

Directory names in the table are relative to `test/`. Feature directories below
`html_to_pdf/` remain siblings. A suite named `*_browser_parity_test.exs` compares
native output with Chromium. Some feature suites also carry individual
`:browser_parity` cases next to their unit assertions.

## Naming

Use `<feature>_<scenario>_test.exs` and the corresponding `FeatureScenarioTest`
module name. A test description states observable behavior, including the trigger
when it matters. A new suite should have an identifiable owner or a specific
integration purpose.

Examples:

- `html_to_pdf/tables/atomic_table_overflow_test.exs`
- `html_to_pdf/forms/clipped_widget_test.exs`
- `html_to_pdf/integration/list_ruby_table_pagination_test.exs`
- `validators/html_table_validator_test.exs`

Name fixtures with the same vocabulary, such as
`atomic_table_hidden_overflow.html`. Use feature/scenario names in tags and
artifact directories too. Development wave numbers, followups and unfinished-work
labels belong in development records.

## Shared paths and fixtures

```elixir
alias NativeElixirPdfUtilities.TestSupport.Paths

html = File.read!(Paths.fixture("html_to_pdf/browser_parity/atomic_table_hidden_overflow.html"))
font = Paths.project("priv/fonts/dejavu/DejaVuSans.ttf")
```

`fixtures/html_to_pdf/browser_parity/` contains the browser source corpus and its
shared `assets/`. Its explicit registration verifies that every HTML fixture has
a comparison, a source sheet or a documented reference-only purpose. The sheet
names describe their feature groups. Keep source geometry, fonts and thresholds
when adding or reorganizing comparisons.

`fixtures/html_to_pdf/documents/` contains invoice, order, label and report samples.
`fixtures/images/jpeg/` contains JPEG encoding cases. PDF, font, template and
converter fixtures use their existing feature directories under `fixtures/`.

Temporary test outputs belong under `tmp/`, using a feature/scenario name and a
unique directory when cases can run concurrently. Tests must create their inputs
and clean up temporary files they own. The browser helper creates a temporary
sibling input only while preserving an authored document's relative-resource
base; it removes that input after printing.

## Focused checks

Run the relevant suite while implementing a change:

```bash
mise exec -- mix test test/html_to_pdf/tables/atomic_table_overflow_test.exs
```

| Check | Command |
| --- | --- |
| Formatting | `mise exec -- mix format --check-formatted` |
| Coverage | `mise exec -- mix test --cover --warnings-as-errors` |
| Performance | `mise exec -- ./scripts/check-performance` |
| Dialyzer | `MIX_ENV=test mise exec -- mix dialyzer` |

If Elixir and Erlang are already on your PATH, the `mix` commands can run without
`mise exec --`. Focused host checks give feedback during development. The full
matrix below is required before submitting a pull request.

## Full quality matrix

Run from the repository root:

```bash
./scripts/quality-matrix
```

The helper requires Docker, Git, `tar`, `jq` and Bash 4 or newer, with Docker running. Linux
can use Docker directly. macOS needs Docker Desktop and Bash 4+. Windows uses
WSL2 with Docker Desktop integration; native PowerShell and Command Prompt are
not supported by this helper. The checkout and `.quality/` must be writable by
the current user.

The shared [runtime matrix](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/HEAD/ci/runtime-matrix.json)
selects the same images locally and in CI:

| Elixir | Image |
| --- | --- |
| 1.19 | `sigdevelopers/native-elixir-pdf-ci:elixir-1.19` |
| 1.20 | `sigdevelopers/sigportal-ci:v2.8` |

The images include Chromium, Poppler, qpdf and fonts. The runner pulls missing
images and resolves tags to immutable image IDs for the run. Update an existing
local image deliberately with `docker pull <image>`.

The runner freezes tracked and non-ignored untracked source bytes, including
uncommitted changes, in `.quality/logs/<run>/source.tar`. Each runtime receives a
separate copy with its own writable test outputs. Dependency, build and tool
caches remain isolated by runtime under `.quality/`. The application build is
cleaned before compilation.

| Check | Every supported minor | Latest minor only |
| --- | --- | --- |
| Compile with warnings as errors | Yes | |
| Full ordinary test suite | Yes | |
| Full Chromium browser parity | Yes | |
| 100% coverage, during the ordinary suite | | Yes |
| Formatting and unused dependencies | | Yes |
| Dialyzer | | Yes |
| Performance guards and report | | Yes |

The canonical runtime must be the newest supported minor. Its coverage command
already runs the ordinary suite, so that suite is not repeated.
No browser cases or performance thresholds are relaxed.
A failed stage skips later stages for that runtime; other runtimes continue.

Two correctness lanes run concurrently by default. Set `QUALITY_MATRIX_JOBS=1`
to run serially on a small host, or increase it for more supported runtimes when
resources allow. Latest-runtime performance starts only after all correctness
containers exit, so concurrent validation does not distort its timing. A lock
prevents two matrix runs from sharing caches. If a terminated run leaves
`.quality/matrix.lock`, inspect its `pid` and owned `pdf-quality-<run>-*` containers
and remove the lock only after confirming that run has stopped.

CI uses independent runners for each minor and the same check policy. Each
runner finishes its correctness checks before performance. CI and the local
runner share configuration validation through `./scripts/quality-matrix --matrix-json`.
Host-side orchestration integration checks exercise concurrency, source/output
isolation, failure reporting, the performance barrier and future-runtime policy
without starting containers. Both the local matrix and CI's setup job run them.

Support moves to a rolling three-minor window when an Elixir 1.21 container is
available. Add it to the runtime matrix, make it canonical and measure its
performance baseline. Older minors retain the ordinary tests and browser parity.

### Results and warnings

The runner prints stage logs under `.quality/logs/<run>/`. Its `results.tsv`
records runtime, stage, result, warning count, log path and elapsed seconds. Each
runtime retains `container-checks.log`, and the canonical runtime also retains
`container-performance.log` and `performance.json`. The source archive, runtime
configuration and immutable image IDs identify what was validated. Workspace
outputs remain under each runtime's log directory.

| Result | Action |
| --- | --- |
| `PASS` | The stage passed. |
| `FAIL` | Inspect its log and resolve the failure. |
| `WARN` | Resolve project warnings. Identify third-party warnings and any available upstream fix in the pull request. |
| `SKIP` | Resolve the earlier failure that prevented this check. |
| `N/A` | Expected for latest-only checks on older minors, or the separate ordinary-test stage already included in canonical coverage. |

A successful exit can include warnings, so inspect every `FAIL` and `WARN` entry.
Include the commands, runtime versions, results and unresolved issues in your
pull request. When a check fails, read its log, fix the cause and rerun the
affected checks. The complete matrix must pass before the pull request is ready.

## Publishing documentation

Build documentation when preparing a package for publishing, on the latest
supported runtime:

```bash
MIX_ENV=dev mise exec -- mix deps.compile
MIX_ENV=dev mise exec -- mix docs --warnings-as-errors
```

Resolve documentation errors before publishing. The documentation build runs
as a publishing check; it is excluded from the routine quality matrix and CI.

## Browser parity

Visible HTML, CSS, image, font, layout or pagination changes need focused unit
coverage and a Chromium comparison fixture. The full matrix runs browser parity
on every supported runtime. For focused feedback or host/container diagnosis, run:

```bash
CHROMIUM_BIN=/usr/bin/chromium mise exec -- mix test.browser_parity
```

Host comparisons need Chromium, Poppler and matching fonts. The matrix supplies
these tools plus DejaVu and Liberation. If output differs, inspect the fonts in
both PDFs before changing layout expectations.

The browser suite compares native and Chromium output from the authored source,
using the existing page, text, visible-ink and pixel checks. Keep the source,
viewport, scale and configured thresholds when reorganizing fixtures. Chromium
is the visual reference; Poppler rasterizes both PDFs with the same settings.
Native-versus-native comparisons do not replace browser acceptance.

The alias runs up to six modules concurrently. Process-wide environment changes
and load-sensitive reference cases stay serial. Each comparison uses a fresh
browser context with author JavaScript disabled and print media selected. The
suite closes browser processes and removes temporary profiles. Before capture,
the shared helper waits for outstanding frame/resource loads, settles fonts and
image decoding in each document, and checks that the frame tree stayed stable.
Readiness state and timeout diagnostics are retained beside the comparison PDF.
Fontconfig overrides use a fresh browser process with the same readiness checks.
The helper uses the suite timeout, keeps author scripts disabled and preserves
broken-resource fallback rather than retrying a failed visual comparison.

For a smaller host, reduce concurrency with `mix test.browser_parity --max-cases 2`.
Use `BROWSER_PARITY_BROWSER=cli` to diagnose with the command-line printing path.
Keep the same source and thresholds with either adapter.

Small independent cases may share generated comparison sheets, with separate
regions and unchanged checks. Root-document, pagination and custom-reference
cases need their own comparisons. See [fixture scripts](scripts.md) for sheet
regeneration. Preserve original input bytes and check the affected regions after
regenerating a sheet.

## Performance measurements

The harness requires existing workloads to stay at or below their recorded
v1.0.0 reductions baselines, with no regression allowance. Each workload also
has a one-second elapsed-time ceiling and an output-size guard.
Change a baseline only for an intentional workload change or a justified tradeoff
with measurements and a reason. Adding a feature does not reset an existing
workload's baseline.

Retain a complete measurement report when investigating performance:

```bash
PERFORMANCE_REPORT=.quality/performance/candidate.json mise exec -- ./scripts/check-performance
```

For cumulative timing comparisons, supply the fixed baseline from the start of
the work, captured on the same idle host with the same runtime and VM settings:

```bash
PERFORMANCE_BASELINE_REPORT=.quality/performance/baselines/reports.json \
PERFORMANCE_REPORT=.quality/performance/candidate.json mise exec -- ./scripts/check-performance
```

The baseline can be a single report or a bundle. A bundle selects
`elixir-<major.minor>.json` by default; `PERFORMANCE_BASELINE_KEY` selects another
capture. Runtime, scheduler count, workload names, fixture/font hashes and the
dependency lock must match. Each existing workload must stay at or below its
fixed elapsed-time baseline, covering the whole work rather than each
individual change. Preserve that original baseline when recapturing under
controlled conditions.

The shared `scripts/check-performance` entry point starts the benchmark VM with
four normal schedulers, four dirty CPU schedulers and ten dirty IO schedulers in
both local matrix runs and CI. Use that entry point for comparable measurements.
CI uploads its complete performance report even when a workload or fixed-baseline
comparison fails. Existing reduction and one-second ceilings are unchanged.

The report retains timed samples, medians, reductions, PDF sizes, runtime details
and input hashes, including on failure. Separate memory runs sample total BEAM
memory without affecting timed samples. VM memory excludes native allocations and
can miss short peaks; use an OS profiler when native memory matters.

Other builds can distort elapsed time. Retain all attempts when retrying under
load, compare complete workload sets and keep baseline inputs and limits fixed.
Do not assemble a passing result from selected workloads across different runs.
Investigate reproducible slowdowns above the baseline or obtain an explicit
exception. Store reports and logs in ignored `.quality/`, and investigations in
ignored `tmp/`. Publish the reusable measurement procedure here.
