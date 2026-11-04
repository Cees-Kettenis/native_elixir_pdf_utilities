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

Read optional fields with `Map.get/2`. Match reason atoms; message text may change.
Operation labels can differ from function names, such as `:stamp_text` or `:read`.

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

Form and attachment operations use the same error tuple. Diagnostics identify
the module and operation. Field write errors include the field name in `:source`.

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

### Application-facing operations

Use these modules for application work and untrusted input:

| Task | Modules |
| --- | --- |
| Render HTML | `HtmlToPdf` |
| Inspect PDFs | `Info`, `Text` |
| Edit PDFs | `Merge`, `Transform`, `Split`, `Stamp`, `Outlines`, `Forms`, `Attachments` |
| Check resource policy | `Limits.defaults/0`, `Limits.effective/0` |

Document operations validate their inputs. Ordinary file I/O failures also return
diagnostics. Successful processing does not mean a PDF is safe to open.

### Advanced building blocks

Pass each stage's output to the next. Avoid constructing intermediate maps yourself.
Documented stages and example fields share the 1.x compatibility promise.
Breaking changes require a new major version.

| Interface | Expected input and behavior |
| --- | --- |
| `Pdf.Reader` | Reads validated PDF objects and resolves references |
| HTML/CSS, style, layout, pagination, font and writer stages | Use validated output from the preceding stage |
| `PdfWriter.render/2` | Validates pages, drawings and font fields. Supply approved, parsed TrueType fonts; it does not decode font files |
| Writer font bounds | Metrics must fit TrueType integer fields. PDF permits at most 65,535 nonzero CIDs |
| `Tokenizer.new/1` | Takes a binary and returns state directly. Token-reading functions return lexical diagnostics |
| Font measurement/encoding helpers | Require valid inputs. Invalid constructor/helper arguments are programming errors |

For detailed errors, use:

| Stage | Function |
| --- | --- |
| HTML | `HtmlParser.parse_detailed/1` |
| CSS stylesheet | `CssParser.parse_detailed/1` |
| CSS declarations | `CssParser.parse_declarations_detailed/1` |
| Style | `Style.compute_detailed/2` |

Convenience parser and style functions return reason atoms. `Layout.layout/2`
does too. `HtmlToPdf.render/2` converts pipeline failures to diagnostics.

### Internal modules

`@moduledoc false` modules and `@doc false` functions are internal. Their maps
can change. Use public APIs instead of calling internal writers, caches or
validator preparation code with unchecked data.

## Image failures

| Source | Diagnostic details |
| --- | --- |
| `"PNG"` | Palette, CRC, scanline or compressed-data failures |
| `"SVG"` | XML line number when available; converter errors keep their message |

| Reason | Next step |
| --- | --- |
| `:invalid_document` | Re-export the image or remove unsupported SVG features |
| `:resource_limit_exceeded` | Reduce image size/complexity or review the named limit |

Do not automatically retry with higher limits. See [image requirements](image-processing.md).
