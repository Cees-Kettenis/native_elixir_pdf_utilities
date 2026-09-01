# Diagnostics

Native Elixir PDF Utilities uses one result shape for recoverable failures that
the library can explain:

```elixir
{:error, {reason, diagnostic}}
```

This contract is for developer convenience. Reading, validating, transforming,
and rendering documents involve different parts of the library, but callers
should not need a separate error handler for each one. The same pattern works
across public APIs, and every diagnostic carries as much useful context as the
failing operation can provide.

That consistency is especially useful in application boundaries. A controller,
job, or file-processing pipeline can branch on the reason, log the diagnostic,
and pass it to monitoring or support tooling without first translating several
module-specific error formats.

## The reason and the diagnostic

The outer `reason` is an atom intended for program logic. Applications can use
it to choose what happens next, such as rejecting invalid input or asking an
operator to raise a resource limit.

The `diagnostic` is a map intended for debugging. It explains where and why the
operation stopped. The reason also appears inside this map so the diagnostic
remains meaningful when it is logged, stored, or passed around without the
outer tuple.

For example:

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

The `:operation` identifies the public function the caller used. The `:stage`
identifies the part of that operation that failed. Keeping both makes a failure
such as `:invalid_pdf_input` easier to trace without exposing the library's
internal call structure.

## Diagnostic fields

Every diagnostic includes:

- `:stage` identifies where processing stopped.
- `:reason` is the machine-readable reason atom.
- `:message` explains the failure in plain language.

When the information is available, a diagnostic also includes:

- `:operation` identifies the public API or file operation being performed.
- `:module` identifies the public module returning the error.
- `:source` identifies the relevant path, input value, page, font, or source
  snippet.
- `:line` and `:column` locate malformed parser input.

Optional fields depend on the failure. A file error usually has a path but no
line number. A CSS parser error may have a source snippet, line, and column. The
shape stays the same even when the available detail differs.

## Handling diagnostics

One case expression can handle failures from any API that follows the shared
contract:

```elixir
case NativeElixirPdfUtilities.Text.extract_file(path) do
  {:ok, text} ->
    text

  {:error, {:resource_limit_exceeded, diagnostic}} ->
    Logger.warning("PDF limit reached: #{diagnostic.message}")

  {:error, {reason, diagnostic}} ->
    Logger.warning("PDF operation failed with #{reason}: #{diagnostic.message}")
end
```

Use the reason atom when application behavior depends on the failure. Use the
diagnostic fields for logs and investigation. Since context fields are optional,
read them with `Map.get/2` unless the API documentation guarantees a specific
field for that failure.

The contract covers ordinary caller and document failures that the library can
describe. It does not turn programming errors or unexpected library defects
into generic diagnostics, since doing so would hide bugs that should remain
visible.
