# Contributing

Use this guide to set up the project, make a change, and open a GitHub pull request.
For using the library in an application, see the [user guide](docs/README.md).

## Start locally

Develop on the latest stable Elixir release series with a compatible Erlang/OTP
version. Any patch version within that series is fine. Use your preferred
installation tools.

Fork the repository on GitHub, clone your fork, and create a branch. Replace
`YOUR_USERNAME` with your GitHub username:

```bash
git clone https://github.com/YOUR_USERNAME/native_elixir_pdf_utilities.git
cd native_elixir_pdf_utilities
git switch -c my-change
mix deps.get
```

## Check your changes

Run focused tests while working. Before your pull request is ready to merge,
run the full quality matrix:

```bash
./scripts/quality-matrix
```

It checks compilation, formatting, unused dependencies, 100% test coverage,
performance, Dialyzer, and Chromium rendering parity. Local validation and
required CI checks must pass. If you cannot run the matrix, report which checks
you ran; a maintainer must run it locally before marking the change ready.

### Matrix setup

The helper needs Docker, `jq`, and Bash 4 or newer. Docker must be running.

| Platform | Setup |
| --- | --- |
| Linux | Docker, `jq`, and Bash 4+ |
| macOS | Docker Desktop, `jq`, and Bash 4+ |
| Windows | WSL2 with Docker Desktop integration; run the helper inside WSL |

Native PowerShell and Command Prompt are not supported by the helper.

[ci/runtime-matrix.json](ci/runtime-matrix.json) defines the local and CI
runtimes. It currently tests Elixir 1.19 and 1.20 using the same published images
locally and in CI:

| Elixir | Image |
| --- | --- |
| 1.19 | `sigdevelopers/native-elixir-pdf-ci:elixir-1.19` |
| 1.20 | `sigdevelopers/sigportal-ci:v2.8` |

Both images include Chrome, fonts, and the PDF tools. The runner reuses local
images, pulls missing ones, and resolves each tag to an immutable image ID for
that run. It does not build images or install system packages during validation.
Run `docker pull <image>` when updating an existing local image.

Each runtime uses one container for the complete sequence of checks. Build,
dependency and tool caches stay separate for each Elixir version under `.quality/`.
If a stage fails, later stages for that runtime are marked `SKIP`; the other
runtime still runs. No additional validation image is created.

The support policy moves to a rolling three-minor window when the Elixir 1.21
container is available. Add measured performance baselines when adding a runtime.

### Read the results

Each stage prints a log path under `.quality/logs/`.
The terminal summary and `results.tsv` contain one row per runtime and stage,
with its status, warning count and log path. Each runtime also retains the
complete container output in `container.log`.

| Result | Action |
| --- | --- |
| `PASS` | The stage passed. |
| `FAIL` | Read the stage log and fix the failure. |
| `WARN` | Fix library warnings. Identify third-party warnings and any available upgrade or upstream fix in the PR. |
| `SKIP` | Fix the earlier failure that prevented this stage from running. |
| `N/A` | Expected for formatting outside the canonical runtime. |

A successful exit can still include dependency warnings. Review them before
handing over the change. Formatting runs only on the canonical runtime because
formatter output can differ between Elixir versions.

Performance baselines live in
[scripts/performance-regression.exs](scripts/performance-regression.exs). Checks
allow up to 10% growth in median reductions and 5% growth in measured PDF size, with separate
timing limits. Milestone acceptance allows up to 10% cumulative elapsed-time
growth per existing workload against the fixed pre-milestone revision. This is
one allowance for the whole milestone, not an allowance for every batch. The final milestone audit records elapsed time,
reductions, memory and output size for user review before release.
Change a baseline only for an intentional
workload change or a justified tradeoff, and include measurements and the reason
in the PR.

For milestone comparisons, retain reports from the fixed pre-milestone revision,
the previous change, and the candidate on each supported runtime:

```bash
PERFORMANCE_REPORT=.quality/performance/candidate.json mix test.performance
```

To enforce the cumulative elapsed-time allowance, provide a fixed report captured
on the same idle host with the same runtime and VM settings:

```bash
PERFORMANCE_BASELINE_REPORT=.quality/performance/baselines/1.1.0/reports.json \
PERFORMANCE_REPORT=.quality/performance/candidate.json mix test.performance
```

The baseline can be a single report or a bundle. Bundles select
`elixir-<major.minor>.json` by default; `PERFORMANCE_BASELINE_KEY` selects another
retained capture. Runtime, scheduler count, workload names, fixture/font hashes
and dependency lock must match. Preserve the original baseline revision when
recapturing under controlled conditions; never replace it with a newer batch.
The candidate report is written before comparison, including when comparison fails.

Keep benchmark JSON reports under the ignored `.quality/` directory.
Retained milestone evidence uses one `reports.json` bundle per baseline or
implementation batch under `.quality/performance/baselines/` and
`.quality/performance/results/`. Reports remain local and must not be committed.
The bundle's `reports` map keeps every original capture name and complete report;
`report_sha256` retains the SHA-256 of each original JSON file. Preserve distinct
capture names and existing measurements when adding a capture. This keeps the
fixed baseline and previous-change evidence without adding tracked report files.

The optional JSON report includes all five timed samples, medians, reductions,
PDF sizes, runtime details, and hashes of the benchmark, fixtures, bundled fonts,
and dependency lock. It also runs three separate memory measurements per workload
and records sampled total BEAM memory every 10 ms. Sampling does not affect the
timed runs. VM memory includes shared caches and retained benchmark outputs,
excludes native allocations, and can miss short peaks. Use an OS profiler for
native memory. Reports are measurements, not automatic approval of a slowdown.
Keep the inputs, runtime, fonts, host, and runtime settings the same, run on an
idle host. Increases within the cumulative 10% allowance do not block acceptance
when the other quality gates pass. For inconsistent timing, retain a bounded set
of independent runs and report uncertainty rather than repeatedly investigating
small differences. Reproducible increases above 10% require optimization or an
explicitly approved exception. Preserve the fixed report rather than replacing it after each
change. Benchmark new features separately when adding them.
The quality matrix writes a report alongside each runtime's performance log.

### Quick checks

These give feedback on your installed runtime while you work:

| Task | Command |
| --- | --- |
| Test one area | `mix test test/html_to_pdf/layout_test.exs` |
| Check formatting | `mix format --check-formatted` |
| Check coverage | `mix test --cover --warnings-as-errors` |
| Check performance | `mix test.performance` |
| Run Dialyzer | `MIX_ENV=test mix dialyzer` |
| Build documentation | `mix docs` |

For visible rendering changes, add a Chromium parity fixture and run:

```bash
CHROMIUM_BIN=/usr/bin/chromium mix test.browser_parity --warnings-as-errors
```

Adjust the Chromium path for your host. Local comparisons need Chromium,
Poppler, and matching fonts. The matrix supplies these tools, DejaVu, and
Liberation. If a comparison differs, check the fonts embedded in both PDFs.
On Linux, `fc-match` shows the native Fontconfig aliases.

The browser alias runs up to six independent ExUnit modules concurrently. Cases
that change Fontconfig or other process-wide settings remain serial. Chrome's
load-sensitive intrinsic embedded-SVG references also retain serial scheduling.
Each worker
reuses a Chrome process through its DevTools pipe, creates a fresh browser context
for each comparison, disables author JavaScript, and selects print media before
loading the fixture. It waits for page load and the existing virtual-time budget
before printing. The suite closes Chrome before removing its temporary profiles.
Comparisons that install a temporary Fontconfig configuration use a separate
Chrome process so it inherits the correct environment.

Use `mix test.browser_parity --max-cases 2` to reduce concurrency on a smaller
runner. To replay the original command-line printing path for diagnosis, run
`BROWSER_PARITY_BROWSER=cli CHROMIUM_BIN=/usr/bin/chromium mix test.browser_parity`.
Both paths use stock Chrome and the same pixel thresholds. The comparator caches
executable versions within the test VM; it still records fonts for every PDF.

Chromium is the reference for visual acceptance. Poppler only rasterizes the
Chromium and native PDFs with the same settings and records embedded fonts.
Do not add standalone Poppler pixel expectations or native-versus-native PDF
image comparisons to the browser suite. Keep ordinary structural, text,
geometry, and diagnostic tests alongside the Chromium comparisons.

## Make a change

Keep the change focused. Add regression tests for behavior changes and update
the relevant user guide. Use synthetic data in document fixtures.

Follow the [project conventions](AGENTS.md):

- Give public functions `@doc` and `@spec`; document public web endpoints in OpenAPI.
- Keep input, option, and document validation in the appropriate validator.
  Consumers should use its validated result rather than repeat the checks.
- Return recoverable failures through shared [diagnostics](docs/diagnostics.md).
  Build errors with `Diagnostics` and test their actionable fields.
- Put tunable resource limits in `NativeElixirPdfUtilities.Limits`.
- Reuse existing helpers. Extract private functions when they remove duplication
  or clarify complex behavior, and prefer explicit `case`, `cond`, or `with`
  branching.

Renderer changes need focused parser, style, layout, pagination, or PDF writer
coverage. Add browser parity fixtures when the change affects visible output.
Review and understand everything you submit, including code produced with AI tools.

Keep each test tied to an observable contract. Image presence alone does not
establish glyph placement, clipping, gradients, or text sizing. Prefer a focused
pixel, geometry, text, field, or diagnostic assertion. Consolidate repeated
checks of the same input and configuration, and keep converter painting tests
focused on the library's source, font, resource, and image integration.

Small visual cases can share a page when their styles and geometry remain
independent. The compact browser fixtures retain a separate comparison region
for every sample, with the same one-percent ceiling and an ink-presence check.
Keep root-document and pagination cases separate. Preserve the original fixture
sources when consolidating them. The HTML, SVG, and MathML sheets are generated
from those originals using `test/fixtures/html_to_pdf/browser_parity/sheets.json`:

```bash
MIX_ENV=test mise exec -- mix run scripts/build-browser-sheets.exs
```

The fixture inventory check verifies that every sheet matches its sources.
Keep the original canvas sizes and scale, use separate regions, and keep SVG IDs
disjoint across samples. Sheet origins use a three-point grid, equivalent to
four CSS pixels, to avoid introducing fractional CSS placement. Verify the native
and Chromium raster crops against the original individual fixtures after changing
a layout. Cases with changed rendering, viewport dependencies, pagination, or
custom reference adapters need their own comparison.

Share large assertion bodies outside generated
`for` tests so the compiler processes them once.

## Try the manual app

[dev/manual_web](dev/manual_web) provides browser helpers for the main rendering
and PDF editing workflows, SVG conversion, and tokenizer inspection. Run it from
its own Mix project:

```bash
cd dev/manual_web
mix deps.get
mix run --no-halt
```

Open [the app](http://127.0.0.1:4001) or its
[OpenAPI document](http://127.0.0.1:4001/openapi.json). Use synthetic files from
[test/fixtures](test/fixtures) to try operations.

The helpers expose common options. Use IEx or focused tests for low-level PDF
object inspection and renderer options such as custom fonts, asset callbacks,
and page headers or footers. The app is development tooling and is not included
in the Hex package.

## Open a pull request

Commit the change on your branch and push it to your fork. Open a pull request
against this repository's `main` branch.

Explain the problem, resulting behavior, and checks you ran. Link any related
issue and report known warnings or checks you could not run. Address review
feedback and rerun the affected checks after changes.

The maintainer decides when to create a release.

## Report an issue

Open a [GitHub issue](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/issues)
with the library version, a small reproduction using synthetic data, expected
behavior, and any diagnostics. See [SECURITY.md](SECURITY.md) for security reports.
