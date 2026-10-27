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
| Concurrency | Two correctness lanes; `QUALITY_MATRIX_JOBS=1` runs serially |
| Performance | Starts after all local correctness containers exit; CI finishes correctness first on each runner |
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

| Task | Command |
| --- | --- |
| Focused host feedback | `CHROMIUM_BIN=/usr/bin/chromium mise exec -- mix test.browser_parity` |
| Smaller host | `mise exec -- mix test.browser_parity --max-cases 2` |
| Diagnose CLI printing | `BROWSER_PARITY_BROWSER=cli mise exec -- mix test.browser_parity` |

| Concern | Requirement or behavior |
| --- | --- |
| Host tools | Chromium, Poppler and matching fonts; matrix includes DejaVu/Liberation. Inspect PDF fonts before changing layout expectations |
| Reference | Chromium renders authored source; Poppler rasterizes both PDFs identically. Native-to-native checks cannot replace this |
| Comparisons | Preserve source bytes, geometry, fonts, viewport, scale, thresholds and page/text/ink/pixel checks across adapters and fixture moves |
| Execution | Up to six modules; process-wide environment changes and load-sensitive cases stay serial |
| Browser state | Fresh context, print media, author JavaScript disabled; processes/profiles cleaned afterward |
| Readiness | Wait for frame/resources, fonts and image decoding; verify stable frame tree; retain readiness/timeout diagnostics beside PDFs |
| Fontconfig | Fresh browser process with the same readiness checks |
| Failures | Use suite timeout, preserve broken-resource fallback; never retry to hide a failed comparison |
| Shared sheets | Only independent cases with separate regions and unchanged checks; root-document, pagination and custom-reference cases stay separate |

See [fixture scripts](scripts.md) for regeneration; inspect affected regions afterward.

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
