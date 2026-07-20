# PDF Merging

`NativeElixirPdfUtilities.Merge` combines a non-empty list of PDF binaries and
returns a newly serialized PDF.

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
- inherited page resources and MediaBox values
- the stream filters and resource limits described in the
  [PDF reader guide](pdf-reader.md)

Malformed, encrypted, unsupported, or resource-intensive inputs fail through
the shared diagnostic contract. The merger does not decrypt PDFs.

## Output behavior

The merger emits a fresh PDF 1.7 document with a new cross-reference table,
trailer, catalog, and flat page tree. Active input objects receive new object
numbers, their indirect references are rewritten, and stream bytes and filter
declarations are preserved. Page-level inherited resources and MediaBox values
are written onto the merged pages where required.

The result is intended for combining page content. Because the top-level
catalog is rebuilt, document-level features such as metadata, outlines and
bookmarks, named destinations, viewer preferences, portfolios, and AcroForm
configuration are not preserved as a merged document contract. Applications
that require those features should restore them in a later transformation or
use a merger designed for that specific feature.

## Errors

An empty input list and invalid PDF input return actionable diagnostics:

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
