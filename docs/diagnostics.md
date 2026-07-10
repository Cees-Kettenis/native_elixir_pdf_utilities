# Diagnostics

Native Elixir PDF Utilities returns recoverable public API failures as
`{:error, {reason, diagnostic}}` when the library can explain why an operation
cannot continue.

The `reason` atom is intended for programmatic branching. The `diagnostic` map is
intended for developer debugging, logs, and user-facing support messages.

## Diagnostic Fields

Diagnostic maps always include:

- `:stage` - the pipeline or utility stage that failed
- `:reason` - the machine-readable reason atom
- `:message` - a human-readable explanation

Diagnostic maps may also include:

- `:operation` - the public API or file operation being performed
- `:module` - the public module returning the error
- `:source` - a path, source snippet, or caller-provided input label
- `:line` and `:column` - source location details when parser input can be located

## Example

```elixir
case NativeElixirPdfUtilities.Text.extract_file(path) do
  {:ok, text} ->
    text

  {:error, {_reason, diagnostic}} ->
    Logger.warning(diagnostic.message)
end
```

## Example Diagnostic

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

## Contributor Guidance

Use `NativeElixirPdfUtilities.Diagnostics` for new public API failures instead
of inventing per-module error shapes.

Do not raise for ordinary caller/input failures such as invalid paths, missing
files, unsupported documents, unsupported HTML/CSS, or empty extraction results.
Prefer diagnostic error tuples and add focused tests that assert the important
fields.
