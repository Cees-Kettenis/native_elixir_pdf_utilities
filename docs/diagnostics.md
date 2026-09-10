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
| `:invalid_html`, `:unsupported_html`, `:invalid_css`   | The source location and[supported HTML/CSS](html-to-pdf-compatibility.md) |
| `:invalid_pdf_input`                                       | Whether the input is a complete, readable PDF                            |
| `:encrypted_pdf`                                           | Supply an unencrypted PDF; decryption is not supported                   |
| `:unsupported_pdf_feature`, `:unsupported_text_encoding` | The relevant operation's supported inputs                                |
| `:resource_limit_exceeded`                                 | Document size/complexity and your[configured limits](resource-limits.md)  |
