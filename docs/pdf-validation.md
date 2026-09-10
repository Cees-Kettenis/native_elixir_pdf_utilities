# Checking a PDF

Use `Reader.read/1` to check that the library can read a PDF's document
structure:

```elixir
alias NativeElixirPdfUtilities.Pdf.Reader

with {:ok, pdf} <- File.read("report.pdf"),
     {:ok, document} <- Reader.read(pdf) do
  {:ok, length(document.pages)}
end
```

You do not need to call this before another PDF operation. Each operation
checks its own input.

## What success means

A successful read means the library can parse the active PDF objects and page
tree. It does not certify PDF/A compliance, visual correctness, digital
signatures, or support for every operation on the document.

For example, a PDF can have a readable page tree but a font encoding that
[text extraction](text-extraction.md) cannot decode. Call the operation you
intend to use and handle its result.

## Common problems

| Problem | Result |
| --- | --- |
| Missing or malformed document structure | `:invalid_pdf_input` |
| Encrypted content | `:encrypted_pdf` |
| A feature the operation cannot process | `:unsupported_pdf_feature` |
| Input or processing limits exceeded | `:resource_limit_exceeded` |

Failures include a [diagnostic](diagnostics.md) explaining the problem. The
library does not repair damaged PDFs or decrypt them. To check encryption
without loading encrypted content, use [Info.encrypted?/1](pdf-information.md#encryption-status).

See [PDF reader](pdf-reader.md) for object inspection and supported structures,
and [Resource limits](resource-limits.md) for size and processing bounds.
