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
runtimes. It currently tests Elixir 1.19 and 1.20. The support policy moves to a
rolling three-minor window when the Elixir 1.21 container is available. Add
measured performance baselines when adding a runtime.

### Read the results

Each stage prints a log path under `.quality/logs/`.

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
allow up to 5% growth in median reductions and measured PDF size, with separate
timing limits. Fix unintended slowdowns. Change a baseline only for an intentional
workload change or a justified tradeoff, and include measurements and the reason
in the PR.

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
