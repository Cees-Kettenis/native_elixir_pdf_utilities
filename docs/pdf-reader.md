# PDF Reader

`NativeElixirPdfUtilities.Pdf.Reader` is the shared document layer used by PDF
utilities that need to understand existing files. It sits above
`NativeElixirPdfUtilities.Tokenizer`:

- The tokenizer converts PDF bytes into lexical tokens. It does not decide
  which revision of an object is active or resolve references.
- The reader starts at the final `startxref`, follows the document's
  cross-reference revisions, loads the active objects, resolves indirect
  references, decodes supported streams, and validates the page tree.

Application code should normally use `NativeElixirPdfUtilities.Text` or
`NativeElixirPdfUtilities.Merge`. The reader is public for inspection and for
building additional PDF utilities on the same parsed document model.

## Supported object structures

The reader supports:

- classic cross-reference tables
- cross-reference streams, including `/W` and `/Index`
- object streams and type-2 compressed-object entries
- incremental revisions through `/Prev`
- hybrid-reference files through `/XRefStm`
- active generation and free-entry selection from the newest revision
- recursive indirect value and stream resolution with cycle detection

The returned `:xref` map describes the active entry for each object number.
The returned `:objects` map is keyed by `{object_number, generation}`. Free
entries and superseded revisions are intentionally absent from `:objects`.

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
and predictors return `:unsupported_pdf_feature` diagnostics.

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

Text extraction and merging both consume the reader model. This means they use
the same active revision, compressed-object handling, stream validation, page
tree traversal, encryption detection, and malformed-input diagnostics. New PDF
inspection or transformation utilities should use the reader instead of
scanning the complete token stream for indirect objects.
