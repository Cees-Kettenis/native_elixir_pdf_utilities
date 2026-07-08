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
- Generate documentation locally:
  - `mise exec -- mix docs`

## Pull Request Guidelines

- Keep PRs focused and small where possible.
- Include a clear description of what changed and why.
- Add or update tests for behavior changes.
- Update `README.md` when public behavior, options, or examples change.
- Ensure tests, 100% coverage, Dialyzer, and formatting pass before opening a PR.

## Coding Guidelines

- Follow existing Elixir patterns in `lib/` and tests in `test/`.
- Public-facing functions must include `@doc` and `@spec`.
- Prefer `case`, `cond`, `with`, or clearly named private helpers over hidden branching through guarded function heads.
- Only extract a `defp` when it reduces real duplication, simplifies genuinely complex code, or names a non-obvious rule.
- Prefer inline code when a private function is used only once and does not make the caller easier to understand.
- Do not duplicate shared helpers for common concerns already handled elsewhere.

## AI-Assisted Development

AI tools such as OpenAI Codex may be used to assist with development, testing, documentation, and debugging.

Contributors are responsible for understanding, reviewing, and validating any AI-assisted code before submitting it.

## Reporting Issues

Use GitHub Issues for bugs and feature requests:

- https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/issues

For security issues, see [SECURITY.md](SECURITY.md).
