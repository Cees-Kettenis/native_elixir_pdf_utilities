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

Changes to the shared diagnostics API, including its tuple shape, fields, and
types, are permitted only when there is no other way to return correct debug
information to the developer using the existing contract. Prefer expressing
additional detail through the existing `:message` and `:source` fields.

Do not raise for ordinary caller/input failures such as invalid paths, missing
files, unsupported documents, unsupported HTML/CSS, or empty extraction results.
Prefer diagnostic error tuples and add focused tests that assert the important
fields.

## Malformed PDF Input

The shared reader used by `Merge.merge/1` and `Text.extract/2` validates PDF
headers, final xref pointers, object boundaries, stream lengths, page trees,
and indirect references before continuing. Malformed input returns an
`:invalid_pdf_input` diagnostic rather than producing partial output or
raising. Encrypted PDFs return `:encrypted_pdf`; unsupported stream features
return `:unsupported_pdf_feature`; custom fonts without a reliable Unicode
mapping return `:unsupported_text_encoding`; image-only PDFs return
`:no_extractable_text` from the reconstructed string API.

The tokenizer represents malformed literal and hexadecimal strings as
`{:error, reason}` tokens. This is primarily useful to callers using
`NativeElixirPdfUtilities.Tokenizer` directly; merge and text extraction convert
such tokenization failures into their public diagnostic error shape.

To protect extraction from resource exhaustion, the shared reader applies input,
decoded-stream, ratio, object/page, CMap, and recursion limits. A limit failure
returns `:resource_limit_exceeded`; it is never converted to a partial result.
