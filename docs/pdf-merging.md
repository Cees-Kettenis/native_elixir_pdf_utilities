# Combining PDFs

Use `NativeElixirPdfUtilities.Merge.merge/1` to combine PDF binaries in order.

```elixir
alias NativeElixirPdfUtilities.Merge

with {:ok, first} <- File.read("first.pdf"),
     {:ok, second} <- File.read("second.pdf"),
     {:ok, combined} <- Merge.merge([first, second]) do
  File.write("combined.pdf", combined)
end
```

The list must contain at least one PDF. All pages from the first input appear
before all pages from the second. For only part of a document, use
[page selection](pdf-page-transforms.md#select-reorder-or-delete-pages) first.

## What the output keeps

| Kept | Not preserved as document features |
| --- | --- |
| Page content, images, resources, and page geometry | Metadata, viewer preferences, portfolios, and form configuration |
| Bookmarks, in input order, with updated page targets | Original document signatures |
| External URI links and resolvable internal page links | Source destination dictionaries and name trees |

Resolved named links become explicit page links. Unresolved names remain in
copied annotations and may not work. See [bookmark behavior](pdf-outlines.md#merge-and-transform-behavior).

Merging creates a new PDF 1.7 document. It can retain unused source objects,
so it is not a way to remove confidential content. Use
[stamping](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/pdf-stamping.md) to add content while preserving a target document's
metadata and form configuration.

## Errors and limits

An empty list returns `:empty_pdf_list`. Encrypted, malformed, or unsupported
inputs return a [diagnostic](diagnostics.md). No partial merged PDF is returned.

See [supported PDF inputs](pdf-reader.md#supported-inputs) and
[merge limits](resource-limits.md#merging-and-splitting).
