# Agent Guidelines

This repository is an Elixir Library

## General Rules

- Only create a `defp` when it provides a clear improvement, such as:
- - reducing real code duplication,
  - simplifying a genuinely complex function,
  - naming a non-obvious business rule that improves readability.
- Prefer inline code over extracting a `defp` when the extracted function is used only once and does not make the calling code easier to understand.
- Do not duplicate existing helpers for common concerns such as number parsing, date formatting, string cleanup, or similar utility behavior already handled in shared modules.

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
- Browser parity work must pass with:
  `CHROMIUM_BIN=/usr/bin/chromium mise exec -- mix test.browser_parity`

## Quality Gates

All changes must pass the following commands so execute them:

- `mise exec -- mix test`
- `MIX_ENV=test mise exec -- mix dialyzer`
- `mise exec -- mix format`
- `mise exec -- mix test --cover` should be at 100% for this library

If warnings or errors are introduced, they must be resolved before considering the work complete.
