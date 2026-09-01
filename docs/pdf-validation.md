# Layered PDF validation

PDF operations share validators under `NativeElixirPdfUtilities.Validators`.
The pipeline separates byte parsing, document rules shared by every operation,
and preparation needed by only one operation.

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

`PdfValidator.validate_pdf/1` parses and validates a binary, then returns the
shared context. `PdfValidator.validate/2` validates an already parsed document
model. `Reader.read/1` returns the `:document` map from that context.
`Reader.read_validated/1` returns the full context for utilities that need it.

`NativeElixirPdfUtilities.Validators.InfoValidator` uses the shared context to
resolve and validate the active information dictionary, effective page sizes,
and caller-provided metadata patches. It prepares serialized field values and
checks information-specific byte and incremental-object limits before the
writer appends an update.

`NativeElixirPdfUtilities.Validators.TextValidator` uses the shared context and
checks page geometry, content-stream references, decoded content, content
syntax, text-operator operands, and PDF numeric tokens. The text executor
handles state-dependent behavior. It prepares only the fonts, encodings, and
CMaps used by reachable text operations, so an unused font resource does not
cause extraction to fail.

`NativeElixirPdfUtilities.Validators.MergeValidator` consumes the shared
context and owns page materialization, inherited serialization tokens,
serializable object tokens, exact object generations, output identifier
allocation, and complete indirect-reference remapping. The writer only receives
prepared inputs and therefore never leaves an unknown reference unchanged.

`NativeElixirPdfUtilities.Validators.TransformValidator` and
`NativeElixirPdfUtilities.Validators.SplitValidator` own page-selection,
range, rotation, and split-limit validation. The internal assembly validator
builds a selected-page dependency closure, removes internal links to discarded
pages, and rejects other dependencies that would reintroduce an unselected
page. The assembly writer receives only validated, completely remapped objects.

## What the validated context contains

Validated contexts contain semantic values for traversal and decisions. Raw
tokens are retained only where merge serialization must reproduce an existing
value faithfully. Raw tokens are not used to rediscover page identity, choose
page-tree children, or calculate inheritance.

Validation is deterministic and performs no file writes, PDF writes, or
network access. Every explainable failure uses the shared diagnostic result:

```elixir
{:error, {reason, diagnostic}}
```

Public APIs set diagnostic `:operation` and `:module` to the function the caller
used. They keep the validation stage, reason, message, and source details.

## Reuse by future operations

New PDF inspection and transformation operations should begin with
`PdfValidator.validate_pdf/1` or a context already returned by
`Reader.read_validated/1`. They should add a focused operation validator only
for invariants not guaranteed by the shared context. They must not scan PDF
bytes again or independently walk the raw page tree.
