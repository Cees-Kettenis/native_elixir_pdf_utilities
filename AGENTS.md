# Agent Guidelines

This repository is an Elixir Library

## General Rules (these are not suggestions)

- Only create a `defp` when it provides a clear improvement, such as:
- - reducing real code duplication,
  - simplifying a genuinely complex function,
  - naming a non-obvious business rule that improves readability.
- Prefer inline code over extracting a `defp` when the extracted function is used only once and does not make the calling code easier to understand.
- Do not duplicate existing helpers for common concerns such as number parsing, date formatting, string cleanup, or similar utility behavior already handled in shared modules.
- All input, option, document-structure, and semantic validation must be implemented in the appropriate validator module. Validators are the single source of truth for whether data is valid. Public APIs, parsers, readers, writers, and execution modules must delegate to validators and must not duplicate or independently reinterpret validation rules, because duplicated checks can contradict one another. After validation succeeds, downstream code should operate on the validated result and may rely on its guarantees. If validation cannot reasonably live in a validator, explain the reason to the user before implementing it elsewhere.
- When moving an existing check into a validator, remove the corresponding check from the consuming module. Do not retain a second “defensive” validation path unless it represents an internal invariant rather than caller-input validation.
- Define every tunable resource or complexity limit in `NativeElixirPdfUtilities.Limits`; do not add module-local configurable ceilings. Keep format- or protocol-mandated bounds fixed, and clearly identify them as non-configurable invariants when they could be mistaken for resource limits (for example, the maximum PDF CID value of 65,535).

# Forbidden style:

```
def some_fun(_), do: nil

def some_fun(value) when is_binary(value) do
  something(value)
end
```

Preferred style:

```
@doc "Explains what the function does."
@spec some_fun(term()) :: result_type()
def some_fun(value) do
  case value do
    value when is_binary(value) ->
      something(value)

    _ ->
      nil
  end
end
```

Use case/cond/with or clearly named private functions instead of hidden branching through guarded function heads.

## Documentation

- Keep user-facing guides, supported behavior, API usage, examples and limitations
  in `docs/` and the root `README.md`.
- Keep maintained public contributor guidance in `docs/contributing/`, with
  `CONTRIBUTING.md` as its entry point. Architecture, test organization, setup and
  reusable quality-check procedures belong there for human contributors.
- `AGENTS.md` is exclusively for coding agents. Do not link to it from human-facing
  documentation, include it in ExDoc or package it. Keep agent-specific operating
  instructions here rather than copying them into contributor guides. Agents may
  read contributor guides for project context; those guides do not replace these
  agent instructions.
- Keep implementation plans, wave records, investigations, audits, parity coverage
  inventories and performance reports under ignored `tmp/`, preferably
  `tmp/development/`. Never stage, commit, push, publish or package these local
  records. Do not create a `docs/development/` directory.
- ExDoc extras and package contents may include user and public contributor
  guides. Do not link to local work records from public guides. Keep repository policies,
  public release notes, the product roadmap and asset license/provenance notices
  with the files they govern.

- All public-facing functions must include `@doc` and `@spec`.
- Controllers and public endpoints must include Swagger/OpenAPI definitions.

## Diagnostics

- Public APIs must return failures using the shared diagnostics contract when
  the library can explain why an operation cannot continue:
  `{:error, {reason, diagnostic}}`.
- Build diagnostic maps with `NativeElixirPdfUtilities.Diagnostics` instead of
  inventing per-module error shapes.
- Diagnostic maps must include actionable `:stage`, `:reason`, and `:message`
  fields, and should include `:operation`, `:module`, `:source`, `:line`, and
  `:column` when that context is available.
- Changes to the shared diagnostics API, including its tuple shape, fields, and
  types, are permitted only when there is no other way to return correct debug
  information to the developer using the existing contract. Prefer expressing
  additional detail through the existing `:message` and `:source` fields.
- Do not raise for ordinary caller/input failures in public APIs. Reserve
  exceptions for programmer errors that cannot reasonably be represented as
  recoverable library results.
- Add focused tests for new or changed public failure modes, asserting the
  diagnostic shape and the important actionable fields.

## HTML to PDF Rendering Changes

- Any new HTML, CSS, layout, pagination, image, or font behavior added to the
  HTML-to-PDF renderer must include focused unit coverage in the relevant
  parser, style, layout, pagination, or PDF writer tests.
- If the feature changes visible rendering, add or update a browser parity
  fixture so the native output is compared against Chromium.
- Browser parity work must pass on every supported runtime through the quality
  matrix. Use `CHROMIUM_BIN=/usr/bin/chromium mise exec -- mix test.browser_parity`
  for focused host feedback or to investigate a host/container discrepancy.

## Quality Gates

Run `./scripts/quality-matrix` for every change. Do not consider agent-authored
work complete or push it unless the matrix succeeds. Inspect every `FAIL` or
`WARN` entry and its referenced log; resolve project warnings and identify any
third-party warning explicitly when handing work back. Do not substitute
host-only Mix commands for the matrix. Platform setup, output interpretation,
and the supported-version policy are documented in `docs/contributing/testing.md`.
Every supported runtime runs compilation, ordinary tests and browser parity.
The newest supported runtime is canonical and also owns formatting, dependency
hygiene, 100% coverage, Dialyzer and performance. Local correctness
lanes use isolated source snapshots in parallel; performance runs after they finish.
Build documentation with warnings as errors when preparing a package for publishing,
rather than during routine matrix or CI checks.

## Project structure and workspace

- Read `docs/contributing/architecture.md` and `docs/contributing/testing.md`
  before adding or moving files.
- Start by inspecting Git status and the existing diff. Preserve existing work;
  use a separate worktree for unrelated changes. Do not discard, automatically
  stash or commit another task's changes to obtain a clean status.
- Source paths must match their module after the `NativeElixirPdfUtilities`
  prefix. Put public entry points at `lib/<api>.ex` and implementations under
  their owning namespace directory.
- Name maintained tests, fixtures, tags and artifacts for their feature and
  scenario. Keep development wave/batch numbers and temporary audit labels in
  development records. Split suites when their cases have different owners.
- Reuse `NativeElixirPdfUtilities.TestSupport.Paths` for test inputs and the
  existing helpers in `test/support/` for shared setup.
- Put experimental outputs in ignored `tmp/` and validation evidence/runtime
  caches in ignored `.quality/`. Preserve checked-in reference input bytes when
  moving files. Keep generated data and fixture provenance with their generators.
- The quality matrix cleans each runtime's application build before compilation.
  Keep dependency/tool caches isolated by runtime and associate results with the
  source actually validated.
