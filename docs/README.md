# User guide

Choose the task you want to perform.

| I want to | Guide |
| --- | --- |
| Create a PDF from HTML | [Rendering examples](html-to-pdf-examples.md) |
| Check which HTML and CSS I can use | [HTML and CSS support](html-to-pdf-compatibility.md) |
| Compare supported rendering with a browser | [Browser rendering](html-to-pdf-browser-parity-coverage.md) |
| Read or change a PDF's title, author, or dates | [PDF information](pdf-information.md) |
| Get page counts and dimensions | [Page geometry](pdf-information.md#page-count-and-geometry) |
| Extract text or its position on a page | [Text extraction](text-extraction.md) |
| Combine PDFs | [Merging](pdf-merging.md) |
| Select, reorder, delete, rotate, or split pages | [Page transforms](pdf-page-transforms.md) |
| Add or read bookmarks | [Outlines](pdf-outlines.md) |
| Add a watermark, stamp, letterhead, or page numbers | [Stamping](pdf-stamping.md) |
| Check whether a PDF can be read | [PDF validation](pdf-validation.md) |
| Handle an error | [Diagnostics](diagnostics.md) |
| Adjust document size or processing limits | [Resource limits](resource-limits.md) |

## Working with files

Most PDF operations accept file contents as a binary:

```elixir
alias NativeElixirPdfUtilities.Info

with {:ok, pdf} <- File.read("report.pdf") do
  Info.page_count(pdf)
end
```

HTML rendering and text extraction also provide file helpers. Each task guide
shows the relevant function and its return value.

## Inspecting PDF syntax

For access to individual PDF objects, use the [PDF reader](pdf-reader.md).
For raw syntax tokens and byte positions, use the [tokenizer](pdf-tokenizer.md).
Neither is needed for the common tasks above.

See [Licenses](licenses.md) for the project and bundled-data notices, and the
[Changelog](../CHANGELOG.md) for released changes.
