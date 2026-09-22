# Handling errors

PDF operations and HTML rendering return explainable failures as:

```elixir
{:error, {reason, diagnostic}}
```

Use `reason` to decide what your application does next. Use
`diagnostic.message` to explain or log the failure.

```elixir
require Logger

case NativeElixirPdfUtilities.Text.extract_file("report.pdf") do
  {:ok, text} ->
    text

  {:error, {reason, diagnostic}} ->
    Logger.warning("PDF operation failed: #{reason}: #{diagnostic.message}")
    {:error, reason}
end
```

## Understanding the diagnostic

| Field                       | Meaning                                 | Always present? |
| --------------------------- | --------------------------------------- | --------------- |
| `:reason`                 | The same reason atom as the outer tuple | Yes             |
| `:message`                | A description of the problem            | Yes             |
| `:stage`                  | Where processing stopped                | Yes             |
| `:operation`, `:module` | The operation and module reporting it   | No              |
| `:source`                 | A relevant path or input snippet        | No              |
| `:line`, `:column`      | A position in the source input          | No              |

Read optional fields with `Map.get/2`. Messages may change between releases;
match reason atoms rather than message text. Operation labels may differ from
function names, such as `:stamp_text` or a file action named `:read`.

## Common failures

| Reason                                                       | What to check                                                            |
| ------------------------------------------------------------ | ------------------------------------------------------------------------ |
| `:invalid_options`                                         | Option names, types, and accepted values in the task guide               |
| `:invalid_html`, `:unsupported_html`, `:invalid_css`   | The source location and [supported HTML/CSS](html-to-pdf-compatibility.md) |
| `:invalid_pdf_input`                                       | Whether the input is a complete, readable PDF                            |
| `:encrypted_pdf`                                           | Supply an unencrypted PDF; decryption is not supported                   |
| `:unsupported_pdf_feature`, `:unsupported_text_encoding` | The relevant operation's supported inputs                                |
| `:resource_limit_exceeded`                                 | Document size/complexity and your [configured limits](resource-limits.md)  |

## Forms and attachments

`Forms.fields/1`, `Forms.fill/3`, `Forms.flatten/2`, `Attachments.list/1` and
`Attachments.embed/3` use the same error tuple. Errors include the public module
and operation. Field-specific write errors include the field name in `:source`.

| Reason | Action |
| --- | --- |
| `:invalid_form` | Correct malformed or ambiguous field structures or HTML names. |
| `:unknown_form_field` | Inspect `Forms.fields/1` and use a returned name. |
| `:invalid_form_value` | Match the field type, declared choices and text constraints. |
| `:read_only_form_field` | Leave the read-only field unchanged. |
| `:unsupported_form` | Check the message for unsupported features, appearances or signed-document restrictions. |
| `:invalid_attachment` | Correct attachment metadata, duplicates or malformed embedded-file structures. |
| `:invalid_mime_type` | Correct conflicting type evidence or unsupported container metadata. |
| `:resource_limit_exceeded` | Reduce the workload or configure the named limit. |

Shared reader failures retain their original diagnostic stage and reason.
See [forms](pdf-forms.md) and [attachments](pdf-attachments.md) for supported
inputs and examples.

## API boundaries

Use the application-facing modules for untrusted documents and ordinary application
work. Their explainable input failures return `{:error, {reason, diagnostic}}`.
The common fields and recovery examples appear above.

### Application-facing operations

`HtmlToPdf`, `Info`, `Text`, `Merge`, `Transform`, `Split`, `Stamp`, `Outlines`,
`Forms`, and `Attachments` validate their documented inputs. File operations
also return diagnostic errors for ordinary I/O failures. `Limits.defaults/0`
and `Limits.effective/0` expose the configured resource policy.

Supported formats and operation-specific limitations still apply. A successful
edit or inspection does not certify a document as sanitized for a PDF viewer.

### Advanced building blocks

`Pdf.Reader` exposes validated PDF objects and reference resolution. The HTML
parser, CSS parser, style, layout, pagination, font and PDF-writer modules expose
pipeline stages. Their intermediate maps are advanced interfaces and may change
before the public API is frozen. Prefer values returned by the preceding stage
over constructing those maps yourself.

`PdfWriter.render/2` validates its page and drawing models, including every font
field it consumes. Embedded font metrics must fit the TrueType integer fields,
and font mappings must fit PDF's fixed 65,535 nonzero CID capacity. Supplied
font bytes must be an already approved, parsed TrueType font. The writer is not
a separate font-file decoder.

For detailed parse failures, use `HtmlParser.parse_detailed/1`,
`CssParser.parse_detailed/1`, `CssParser.parse_declarations_detailed/1`, and
`Style.compute_detailed/2`. Their convenience counterparts intentionally retain
legacy reason-only errors. `Layout.layout/2` also returns stage reason atoms;
`HtmlToPdf.render/2` converts those failures to the shared diagnostic contract.

The tokenizer constructor `Tokenizer.new/1` requires a binary and returns its
state directly. Pass that state to token-reading functions, which return
lexical failures with diagnostics. Passing arbitrary terms to constructors or
font measurement/encoding helpers is a programming error. Those helpers are
not substitutes for the facade or the appropriate input validator.

### Internal modules

Modules marked `@moduledoc false` or functions marked `@doc false` are internal.
This includes serialization helpers, incremental writers, caches, and validator
preparation contexts. Callers should not depend on their map layouts or invoke
internal execution functions with unvalidated structures. Validation belongs to
the validator for the corresponding operation; execution assumes that prepared
context.

## Image failures

PNG failures identify `"PNG"` in `:source`, with an explanation such as an invalid
palette index, chunk CRC, scanline filter, or compressed payload. SVG validation
identifies `"SVG"` and includes an XML line number when available. Returned Resvg
errors preserve the converter's explanation in `:message`.

Treat `:invalid_document` as an input problem. Re-export the asset or remove an
unsupported SVG construct before retrying. For `:resource_limit_exceeded`, reduce
the image size or document complexity, or review the named configuration limit.
Do not automatically retry the same input with higher limits. See the
[image format requirements](image-processing.md).
