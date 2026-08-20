# PDF merging

`NativeElixirPdfUtilities.Merge` combines a non-empty list of PDF binaries and
returns a new PDF.

```elixir
alias NativeElixirPdfUtilities.Merge

with {:ok, first} <- File.read("first.pdf"),
     {:ok, second} <- File.read("second.pdf"),
     {:ok, merged} <- Merge.merge([first, second]),
     :ok <- File.write("merged.pdf", merged) do
  :ok
end
```

Input order determines page order. `merge/1` accepts PDF binaries rather than
paths so callers control file access, storage, and error handling.

## Supported inputs

Merging uses the shared PDF reader and therefore supports:

- classic cross-reference tables
- cross-reference streams and object streams
- incremental revisions and hybrid-reference files
- active object generations and free entries
- inherited page `Resources`, `MediaBox`, `CropBox`, and `Rotate` values,
  including indirectly referenced page rectangles
- the stream filters and resource limits described in the
  [PDF reader guide](pdf-reader.md)

Malformed, encrypted, unsupported, or resource-intensive inputs fail through
the shared diagnostic contract. The merger does not decrypt PDFs.

## Output behavior

The merger emits a PDF 1.7 document with a new cross-reference table,
trailer, catalog, and flat page tree. Active input objects receive new object
numbers, their indirect references are rewritten, and stream bytes and filter
declarations are preserved. Effective inherited page resources, media and crop
boxes, and rotation values are written onto the merged pages where required.
Existing page-level entries such as `BleedBox`, `TrimBox`, `ArtBox`, and
`UserUnit` remain on copied page dictionaries.
If required page geometry is missing or malformed, the merger returns a
diagnostic that identifies the problem. It does not invent a page size.

The merger combines page content. It rebuilds the top-level catalog, so it does
not carry metadata, outlines and bookmarks, named destinations, viewer
preferences, portfolios, or AcroForm configuration into the output. Add those
items in a later transformation, or use a merger that supports them.

## Errors

An empty input list and invalid PDF input return diagnostics:

```elixir
case Merge.merge(pdf_binaries) do
  {:ok, merged_pdf} ->
    merged_pdf

  {:error, {_reason, diagnostic}} ->
    Logger.warning(diagnostic.message)
end
```

See [Diagnostics](diagnostics.md) for the common error shape and the fields
available for logging and support.
