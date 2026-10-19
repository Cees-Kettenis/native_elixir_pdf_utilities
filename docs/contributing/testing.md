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
| Performance | `mise exec -- mix test.performance` |
| Dialyzer | `MIX_ENV=test mise exec -- mix dialyzer` |
| Documentation | `mise exec -- mix docs --warnings-as-errors` |

If Elixir and Erlang are already on your PATH, the `mix` commands can run without
`mise exec --`. Focused host checks give feedback during development. The full
matrix below is required before submitting a pull request.

## Full quality matrix

Run from the repository root:

```bash
./scripts/quality-matrix
```

The helper requires Docker, `jq` and Bash 4 or newer, with Docker running. Linux
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

Each runtime gets one container and isolated build, dependency and tool caches
under `.quality/`. The matrix cleans the application build before compiling, then
checks compilation, formatting, unused dependencies, 100% test coverage,
performance, Dialyzer and browser parity. Formatting runs on the canonical
runtime only. A failed stage skips later stages for that runtime; the other
runtime still runs.

Support moves to a rolling three-minor window when an Elixir 1.21 container is
available. Add measured performance baselines when admitting a runtime and keep
one runtime marked canonical for formatting.

### Results and warnings

The runner prints stage logs under `.quality/logs/<run>/`. Its `results.tsv`
records runtime, stage, result, warning count and log path. Each runtime also
retains complete output in `container.log`.

| Result | Action |
| --- | --- |
| `PASS` | The stage passed. |
| `FAIL` | Inspect its log and resolve the failure. |
| `WARN` | Resolve project warnings. Identify third-party warnings and any available upstream fix in the pull request. |
| `SKIP` | Resolve the earlier failure that prevented this check. |
| `N/A` | Expected for formatting outside the canonical runtime. |

A successful exit can include warnings, so inspect every `FAIL` and `WARN` entry.
Include the commands, runtime versions, results and unresolved issues in your
pull request. When a check fails, read its log, fix the cause and rerun the
affected checks. The complete matrix must pass before the pull request is ready.

## Browser parity

Visible HTML, CSS, image, font, layout or pagination changes need focused unit
coverage and a Chromium comparison fixture. Run:

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
suite closes browser processes and removes temporary profiles.

For a smaller host, reduce concurrency with `mix test.browser_parity --max-cases 2`.
Use `BROWSER_PARITY_BROWSER=cli` to diagnose with the command-line printing path.
Keep the same source and thresholds with either adapter.

Small independent cases may share generated comparison sheets, with separate
regions and unchanged checks. Root-document, pagination and custom-reference
cases need their own comparisons. See [fixture scripts](scripts.md) for sheet
regeneration. Preserve original input bytes and check the affected regions after
regenerating a sheet.

## Performance measurements

The harness guards existing workloads against reduction and PDF-size growth.
Change a baseline only for an intentional workload change or a justified tradeoff
with measurements and a reason. Adding a feature does not reset an existing
workload's baseline.

Retain a complete measurement report when investigating performance:

```bash
PERFORMANCE_REPORT=.quality/performance/candidate.json mise exec -- mix test.performance
```

For cumulative timing comparisons, supply the fixed baseline from the start of
the work, captured on the same idle host with the same runtime and VM settings:

```bash
PERFORMANCE_BASELINE_REPORT=.quality/performance/baselines/reports.json \
PERFORMANCE_REPORT=.quality/performance/candidate.json mise exec -- mix test.performance
```

The baseline can be a single report or a bundle. A bundle selects
`elixir-<major.minor>.json` by default; `PERFORMANCE_BASELINE_KEY` selects another
capture. Runtime, scheduler count, workload names, fixture/font hashes and the
dependency lock must match. The cumulative timing allowance is 10% per existing
workload against the fixed baseline, covering the whole work rather than each
individual change. Preserve that original baseline when recapturing under
controlled conditions.

The report retains timed samples, medians, reductions, PDF sizes, runtime details
and input hashes, including on failure. Separate memory runs sample total BEAM
memory without affecting timed samples. VM memory excludes native allocations and
can miss short peaks; use an OS profiler when native memory matters.

Other builds can distort elapsed time. Retain all attempts when retrying under
load, compare complete workload sets and keep baseline inputs and limits fixed.
Do not assemble a passing result from selected workloads across different runs.
Investigate reproducible slowdowns above the allowance or obtain an explicit
exception. Store reports and logs in ignored `.quality/`, and investigations in
ignored `tmp/`. Publish the reusable measurement procedure here.
