# Contributing

Thanks for contributing to Native Elixir PDF Utilities.

## Development Setup

1. Install Elixir 1.19 or newer using `mise` or a platform-appropriate Elixir
   installation method.
2. When using `mise`, install the configured toolchain:
   - `mise install`
3. Fetch dependencies:
   - `mix deps.get`

## Build and Test

Local validation is required before a change is considered ready for a pull
request. GitHub Actions confirms the local work; it is not a substitute for
running the applicable checks before submission.

For quick feedback on the installed Elixir version, run:

- `mix format --check-formatted`
- `mix test --cover --warnings-as-errors`
- `MIX_ENV=test mix dialyzer`

For HTML-to-PDF rendering changes, also run browser parity when Chromium is
available:

- `CHROMIUM_BIN=/path/to/chromium mix test.browser_parity --warnings-as-errors`

Before a maintainer considers a change PR-ready, run the complete supported
Elixir matrix:

- `./scripts/quality-matrix`

If a contributor cannot run the complete matrix on their host, they should run
the installed-version checks and say so clearly. A maintainer must then run the
complete matrix locally before marking the change ready; passing remote CI
alone does not replace that local validation.

### Local Matrix Platform Support

The matrix helper requires Docker, `jq`, and Bash 4 or newer.

| Platform | Support and requirements |
| --- | --- |
| Linux | Supported directly with Docker, `jq`, and Bash 4+. |
| macOS | Supported with Docker Desktop, `jq`, and Bash 4+. The Bash 3 version bundled with macOS is not sufficient; install a current Bash version first. |
| Windows | Supported through WSL2 with Docker Desktop integration. Run the helper inside WSL. Native PowerShell and Command Prompt are not currently supported by the helper. |

The helper uses the runtime definitions in `ci/runtime-matrix.json`, retains
full stage logs under `.quality/logs/`, and prints a final summary table.
GitHub Actions reads the same runtime definitions.

### Matrix Results

- `FAIL` means the change is not ready. Open the referenced stage log and fix
  the failure.
- `WARN` in the project `compile`, `coverage`, `dialyzer`, or `parity` stages
  must be investigated and resolved when it originates in this library.
- `WARN` in `dependency_compile` originates while compiling third-party code.
  Identify the dependency and warning explicitly, then decide whether an
  upgrade or upstream fix is available. The matrix may still exit successfully
  so an unavoidable dependency warning does not block unrelated work.
- `N/A` means a release-dependent check intentionally ran elsewhere. Formatting
  is checked only on the canonical Elixir runtime because formatter output can
  differ between releases.

The matrix compiles with warnings as errors, enforces 100% test coverage, runs
Dialyzer, and runs Chromium browser parity for every configured Elixir runtime.
The support policy becomes a rolling three-minor window when the Elixir 1.21
container is available. The currently tested window is Elixir 1.19 and 1.20;
adding 1.21 to `ci/runtime-matrix.json` will add the third local and CI lane.

- Generate documentation locally:
  - `mise exec -- mix docs`

## Manual testing app

The local app under `dev/manual_web` provides browser forms for rendering HTML,
merging, transforming and splitting PDFs, inspecting and updating outlines,
extracting text, inspecting and updating document information, and tokenizing
PDF syntax. It also publishes its OpenAPI document at
`http://127.0.0.1:4001/openapi.json`.

Run it from its own Mix project:

```bash
cd dev/manual_web
mix deps.get
mix run --no-halt
```

Open `http://127.0.0.1:4001` after the server starts. This app is development
tooling and is not included in the Hex package.

## Pull Request Guidelines

- Keep PRs focused and small where possible.
- Include a clear description of what changed and why.
- Add or update tests for behavior changes.
- For new HTML-to-PDF renderer features, include focused coverage in the
  relevant parser/style/layout/pagination/PDF tests and add or update browser
  parity fixtures when the feature affects visible rendering.
- Update `README.md` when public behavior, options, or examples change.
- Complete the applicable local checks before opening a PR, and complete the
  full local matrix before the change is marked ready. All required GitHub
  Actions jobs must also pass before the PR can be accepted.

## Versioning Guidelines

This project uses SemVer-style versioning to describe the public API promise:

- `1.0.0` is the first stable release. It means the public API is defined and
  should not be broken casually.
- `1.1.0` is a backwards-compatible minor release. Use this for new features,
  new options, new modules, or behavior improvements that do not break existing
  callers.
- `1.1.1` is a patch release. Use this for bug fixes, documentation fixes, and
  small internal corrections that preserve public behavior.
- `2.0.0` is a major release. After `1.0.0`, use this when changing, removing,
  renaming, or moving public API in a way that can break existing users.

While the package is still `0.x`, breaking public API changes should bump the
minor version, such as `0.4.0` to `0.5.0`, and must be documented clearly in the
changelog.

Examples of breaking public API changes include:

- renaming a public function or module
- changing return values, such as `{:ok, pdf_binary}` to
  `{:ok, %{pdf: pdf_binary, diagnostics: diagnostics}}`
- changing option names or option shapes
- removing a public function
- changing documented behavior in a way that can break caller code

Before proposing `1.0.0`, make sure the main modules, function names, return
values, options, diagnostics, supported HTML/CSS behavior, and documented
examples are stable enough to support as the public API.

## Coding Guidelines

- Follow existing Elixir patterns in `lib/` and tests in `test/`.
- Public-facing functions must include `@doc` and `@spec`.
- Prefer `case`, `cond`, `with`, or clearly named private helpers over hidden branching through guarded function heads.
- Only extract a `defp` when it reduces real duplication, simplifies genuinely complex code, or names a non-obvious rule.
- Prefer inline code when a private function is used only once and does not make the caller easier to understand.
- Do not duplicate shared helpers for common concerns already handled elsewhere.
- Define every tunable resource or complexity limit in
  `NativeElixirPdfUtilities.Limits`. Keep format- or protocol-mandated bounds
  fixed and clearly identify them as non-configurable invariants when they
  could be mistaken for resource limits, such as the maximum PDF CID value of
  65,535.

## Diagnostic Error Guidelines

Public APIs should return recoverable failures as `{:error, {reason, diagnostic}}`
when the library knows why an operation cannot continue. Use
`NativeElixirPdfUtilities.Diagnostics` to build these diagnostics.

Diagnostic maps must include:

- `:stage` - the pipeline or utility stage that failed
- `:reason` - the machine-readable reason atom
- `:message` - a human-readable explanation suitable for developer debugging

Include these fields when available:

- `:operation` - the public operation or file operation being performed
- `:module` - the public module returning the error
- `:source` - the relevant path, source snippet, or caller-provided input label
- `:line` and `:column` - source location details for parser-style failures

Do not raise for ordinary invalid caller input, missing files, unsupported
documents, unsupported HTML/CSS, or empty extraction results. Prefer diagnostic
error tuples and add focused tests that assert the important fields.

Example:

```elixir
{:error,
 {:invalid_path,
  %{
    stage: :file,
    reason: :invalid_path,
    message: "path must be a string",
    operation: :extract_file,
    module: NativeElixirPdfUtilities.Text
  }}}
```

## AI-Assisted Development

AI tools such as OpenAI Codex may be used to assist with development, testing, documentation, and debugging.

Contributors are responsible for understanding, reviewing, and validating any AI-assisted code before submitting it.

## Reporting Issues

Use GitHub Issues for bugs and feature requests:

- https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/issues

For security issues, see [SECURITY.md](SECURITY.md).
