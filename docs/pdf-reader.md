# Inspecting PDF objects

Use `NativeElixirPdfUtilities.Pdf.Reader` when you need individual PDF
objects. For common tasks, use [information](pdf-information.md),
[text extraction](text-extraction.md), or [merging](pdf-merging.md) directly.

## Read a PDF

```elixir
alias NativeElixirPdfUtilities.Pdf.Reader

{:ok, pdf} = File.read("report.pdf")
{:ok, document} = Reader.read(pdf)
{:ok, catalog} = Reader.dictionary(document, document.trailer["Root"])
```

The document contains `:pages`, `:objects`, `:trailer`, `:xref`, `:xref_offset`,
and the original `:binary`. Objects are keyed by `{object_number, generation}`.
Each object has a parsed `:value`, optional raw `:stream`, `:tokens`, and
`:offset`. Superseded and free objects are omitted.

## Inspecting values

Dictionary keys are strings. Values can be numbers, booleans, `nil`, lists,
dictionaries, or tagged PDF values:

| Value | Meaning |
| --- | --- |
| `{:name, "Page"}` | A PDF name |
| `{:string, bytes}` or `{:hex, bytes}` | String bytes; not necessarily UTF-8 |
| `{:ref, {object, generation}}` | An indirect reference |

`resolve/2` follows a reference. `dictionary/2` also checks that the resolved
value is a dictionary. `fetch/3` resolves a dictionary and returns a key's value,
or `nil` if absent; it does not resolve the returned value.

```elixir
{:ok, pages_ref} = Reader.fetch(document, catalog, "Pages")
{:ok, page_tree} = Reader.dictionary(document, pages_ref)
```

Each `document.pages` entry has `:ref`, `:resources`, `:media_box`, and
`:rotate`. Resolve `{:ref, page.ref}` for its dictionary. For page dimensions,
use [Info.page_sizes/1](pdf-information.md#page-count-and-geometry).

`read_validated/1` returns a richer context with `:document`, `:catalog`,
`:catalog_ref`, `:page_tree_ref`, and `:pages`. Its page entries also expose the
page dictionary, CropBox, and inherited values.

## Streams

`Reader.decoded_stream(document, stream_reference)` returns `{:ok, bytes}`.
Supported filters are `FlateDecode`, `ASCIIHexDecode`, `ASCII85Decode`,
`RunLengthDecode`, and `LZWDecode`, including their PDF abbreviations. TIFF
predictor 2 and PNG predictors 10 through 15 are supported.

Unknown filters return `:unsupported_pdf_feature`. Malformed or truncated
compressed streams return an error rather than partial decoded bytes.

## Supported inputs

The reader supports classic cross-reference tables, cross-reference streams,
object streams, hybrid files, and incremental revisions. It reads the active
revision, not the full edit history. Encrypted documents are rejected.

See [Checking a PDF](pdf-validation.md), [Diagnostics](diagnostics.md), and
[Resource limits](resource-limits.md) for failure handling.
