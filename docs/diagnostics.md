# Diagnostics

Native Elixir PDF Utilities returns recoverable public API failures as
`{:error, {reason, diagnostic}}` when the library can explain why an operation
cannot continue.

Use the `reason` atom for programmatic branching. Use the `diagnostic` map for
debugging, logs, and user-facing support messages.

## Diagnostic fields

Diagnostic maps always include:

- `:stage` - the pipeline or utility stage that failed
- `:reason` - the machine-readable reason atom
- `:message` - a human-readable explanation

Diagnostic maps may also include:

- `:operation` - the public API or file operation being performed
- `:module` - the public module returning the error
- `:source` - a path, source snippet, or caller-provided input label
- `:line` and `:column` - source location details when parser input can be located

## Logging a diagnostic

```elixir
case NativeElixirPdfUtilities.Text.extract_file(path) do
  {:ok, text} ->
    text

  {:error, {_reason, diagnostic}} ->
    Logger.warning(diagnostic.message)
end
```

## Diagnostic tuple

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

## Contributor guidance

Build new public API failures with `NativeElixirPdfUtilities.Diagnostics`.
Do not create a separate error shape for each module.

Keep the shared tuple shape, fields, and types stable. Change them only when the
existing contract cannot report the correct debugging information. Put extra
detail in `:message` and `:source` when those fields are sufficient.

Do not raise for caller errors such as invalid paths, missing files, unsupported
documents, unsupported HTML/CSS, or empty extraction results. Return a
diagnostic tuple and test its important fields.

## Malformed PDF input

Before inspecting information, updating metadata, merging, or extracting text,
the shared reader validates PDF headers, final xref pointers, object boundaries,
stream lengths, page trees, and indirect references. Malformed input returns
`:invalid_pdf_input` instead of a partial result or exception. Encrypted PDFs
return `:encrypted_pdf` from operations that require document objects;
`Info.encrypted?/1` reports their encryption status without loading those
objects.
Unsupported stream operations return `:unsupported_pdf_feature`. Custom fonts
without a reliable Unicode mapping return `:unsupported_text_encoding`.
Image-only PDFs return `:no_extractable_text` from the string API.

The tokenizer represents malformed literal and hexadecimal strings as
`{:error, reason}` tokens. Callers of `NativeElixirPdfUtilities.Tokenizer` can
inspect those tokens directly. Information, merge, and text extraction APIs
convert tokenizer failures to the shared diagnostic tuple.

The shared reader and text validators limit input size, decoded streams,
decompression ratios, objects, pages, CMaps, recursion, aggregate decoded
content, stream uses, instructions, and Form expansions. A limit failure
returns `:resource_limit_exceeded`, never a partial result. Extraction decodes
and tokenizes each repeated indirect stream once, but charges every semantic
use to the operation budget.
