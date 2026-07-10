# Contributing

Thanks for contributing to Native Elixir PDF Utilities.

## Development Setup

1. Install `mise`.
2. Install the configured toolchain:
   - `mise install`
3. Fetch dependencies:
   - `mise exec -- mix deps.get`

## Build and Test

- Run tests:
  - `mise exec -- mix test`
- Run coverage:
  - `mise exec -- mix test --cover`
  - Coverage must remain at 100%.
- Run Dialyzer:
  - `MIX_ENV=test mise exec -- mix dialyzer`
- Format code:
  - `mise exec -- mix format`
- Run browser parity tests after HTML-to-PDF rendering changes:
  - `CHROMIUM_BIN=/usr/bin/chromium mise exec -- mix test.browser_parity`
- Generate documentation locally:
  - `mise exec -- mix docs`

## Pull Request Guidelines

- Keep PRs focused and small where possible.
- Include a clear description of what changed and why.
- Add or update tests for behavior changes.
- For new HTML-to-PDF renderer features, include focused coverage in the
  relevant parser/style/layout/pagination/PDF tests and add or update browser
  parity fixtures when the feature affects visible rendering.
- Update `README.md` when public behavior, options, or examples change.
- Ensure tests, 100% coverage, Dialyzer, and formatting pass before opening a PR.

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
