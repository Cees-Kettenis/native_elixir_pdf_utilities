# PDF reader

`NativeElixirPdfUtilities.Pdf.Reader` parses existing PDF documents for the
other PDF utilities. It builds on `NativeElixirPdfUtilities.Tokenizer`:

- The tokenizer converts PDF bytes into lexical tokens. It does not decide
  which revision of an object is active or resolve references.
- The reader starts at the final `startxref`, follows the document's
  cross-reference revisions, and loads the active objects. The PDF validator
  then checks references, streams, the catalog, and the page tree before the
  reader returns the document map.

For text extraction or merging, use `NativeElixirPdfUtilities.Text` or
`NativeElixirPdfUtilities.Merge`. Use the reader to inspect parsed documents or
build another PDF utility on the same document model.

`Reader.read_validated/1` returns the full validation context needed by another
PDF operation. `Reader.read/1` returns the existing map
with `:binary`, `:objects`, `:trailer`, `:pages`, and `:xref` fields.

## Supported object structures

The reader supports:

- classic cross-reference tables
- cross-reference streams, including `/W` and `/Index`
- object streams and type-2 compressed-object entries
- incremental revisions through `/Prev`
- hybrid-reference files through `/XRefStm`
- active generation and free-entry selection from the newest revision
- recursive indirect value and stream resolution with cycle detection
- page-tree traversal with cycle and duplicate-reference detection, required
  `/Count` validation, and descendant-count consistency checks

The returned `:xref` map describes the active entry for each object number.
The returned `:objects` map is keyed by `{object_number, generation}`. It omits
free entries and superseded revisions.

## Streams

`Reader.decoded_stream/2` validates `/Length` and supports these PDF filters,
including their abbreviated names:

- Flate
- ASCII hexadecimal
- ASCII85
- run-length
- LZW

TIFF predictor 2 and PNG predictors 10 through 15 are supported through
`/DecodeParms`. Filter arrays are applied in declaration order. Unknown filters
and predictors return `:unsupported_pdf_feature` diagnostics. ASCII85 decoding
enforces group boundaries, the 32-bit value ceiling, and valid final partial
groups.

## Errors and limits

Reader failures use the shared diagnostic result:

```elixir
{:error, {reason, diagnostic}}
```

Malformed headers, final xref pointers, xref records, object boundaries,
reference chains, stream metadata, and page trees return
`:invalid_pdf_input`. Encrypted files are detected and return `:encrypted_pdf`;
the reader does not decrypt them.

Input size, object count, revision depth, page count, decoded stream size, and
decompression ratio are bounded. Exceeding a bound returns
`:resource_limit_exceeded` instead of a partial document.

The reader currently does not support encrypted content or stream filters other
than those listed above. Linearization metadata is tolerated but is not used as
an alternate loading path; the final cross-reference chain remains
authoritative.

## Shared utility behavior

Text extraction and merging both use the same validated context. They share
the active revision, compressed-object
handling, stream validation, page-tree traversal, encryption detection, and
malformed-input diagnostics. New PDF inspection or transformation utilities
should use `PdfValidator.validate_pdf/1` or `Reader.read_validated/1` instead of
scanning every token for indirect objects. See
[Layered PDF validation](pdf-validation.md) for invariant ownership.
