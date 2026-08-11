# Layered PDF Validation

PDF-consuming features use validators under
`NativeElixirPdfUtilities.Validators`. The validation pipeline separates byte
parsing from reusable document invariants and operation-specific preparation.

## Validation pipeline

`NativeElixirPdfUtilities.Pdf.Reader` owns PDF byte handling: headers,
cross-reference revisions, active object loading, object streams, encryption
detection, parsing safety, stream decoding, and resource limits.

`NativeElixirPdfUtilities.Validators.PdfValidator` owns semantics shared by PDF
operations:

- catalog and page-tree identity
- exact indirect references, missing objects, and reference cycles
- `/Kids`, `/Count`, duplicate pages, and page-tree cycles
- effective inherited `Resources`, `MediaBox`, `CropBox`, and `Rotate` values
- indirect stream identity and `/Length` structure
- fixed-length numeric arrays used by page geometry and matrices

`PdfValidator.validate_pdf/1` parses and validates a binary and returns the
prepared shared context. `PdfValidator.validate/2` validates a document model
that has already been parsed. `Reader.read/1` remains compatible by returning
the `:document` projection from the same prepared context, while
`Reader.read_validated/1` retains the full context for utilities that need it.

`NativeElixirPdfUtilities.Validators.TextValidator` consumes the shared context
and owns page geometry, content-stream references, decoded content preparation,
content syntax, supported text-operator operands, and reusable PDF numeric-token
conversion. Text execution retains responsibility for state-dependent behavior
and for preparing only fonts, encodings, and CMaps that reachable text
operations actually use. Unused font resources are not grounds for rejecting
text extraction.

`NativeElixirPdfUtilities.Validators.MergeValidator` consumes the shared
context and owns page materialization, inherited serialization tokens,
serializable object tokens, exact object generations, output identifier
allocation, and complete indirect-reference remapping. The writer only receives
prepared inputs and therefore never leaves an unknown reference unchanged.

## Context boundary

Validated contexts contain semantic values for traversal and decisions. Raw
tokens are retained only where merge serialization must reproduce an existing
value faithfully. Raw tokens are not used to rediscover page identity, choose
page-tree children, or calculate inheritance.

Validation is deterministic and performs no file writes, PDF writes, or
network access. Every explainable failure uses the shared diagnostic result:

```elixir
{:error, {reason, diagnostic}}
```

Public façades replace the diagnostic `:operation` and `:module` with the API
the caller invoked while retaining the actionable validation stage, reason,
message, and source details.

## Reuse by future operations

New PDF inspection and transformation features should begin with
`PdfValidator.validate_pdf/1` or a context already returned by
`Reader.read_validated/1`. They should add a focused operation validator only
for invariants not guaranteed by the shared context. They must not scan PDF
bytes again or independently walk the raw page tree.
