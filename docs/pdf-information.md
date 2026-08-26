# PDF information and metadata

`NativeElixirPdfUtilities.Info` reads document information and page geometry
from existing PDF binaries. It can also update common information fields
without rebuilding the document.

```elixir
alias NativeElixirPdfUtilities.Info

{:ok, information} = Info.get(pdf)

information.title
information.author
information.subject
information.keywords
information.producer
information.creation_date
information.modification_date
```

Every key is present in the returned map. A missing field has the value `nil`.
The text decoder accepts PDFDocEncoding and Unicode information strings. PDF
dates become `NaiveDateTime` values. A PDF date's timezone suffix is validated,
but the returned value retains the document's wall-clock time.

## Page count and geometry

Use `page_count/1` for the validated page-tree count:

```elixir
{:ok, 3} = Info.page_count(pdf)
```

`page_sizes/1` returns one entry per page. It resolves inherited MediaBox and
rotation values, normalizes rotation to `0`, `90`, `180`, or `270`, and reports
dimensions in PDF points. Width and height reflect the normalized rotation.

```elixir
{:ok, pages} = Info.page_sizes(pdf)

[
  %{
    page_number: 1,
    width: 841.89,
    height: 595.28,
    unit: :point,
    rotation: 90,
    media_box: %{
      left: 0.0,
      bottom: 0.0,
      right: 595.28,
      top: 841.89
    }
  }
] = pages
```

## Encryption status

`encrypted?/1` validates the PDF header, cross-reference chain, and active
trailer before checking for encryption. It does not load or decrypt encrypted
objects.

```elixir
case Info.encrypted?(pdf) do
  {:ok, false} -> Info.get(pdf)
  {:ok, true} -> {:error, :encrypted_document}
  {:error, {_reason, diagnostic}} -> {:error, diagnostic}
end
```

The other `Info` operations reject encrypted documents because the library does
not decrypt PDF content.

## Updating information

`put/2` accepts a map or keyword list. Omitted fields remain unchanged and
`nil` removes a field. Keywords may be a string or a list of strings.

```elixir
{:ok, updated_pdf} =
  Info.put(pdf,
    title: "August statement",
    author: "Finance Operations",
    subject: "Customer statement",
    keywords: ["statement", "monthly"],
    producer: "Accounts service",
    modification_date: DateTime.utc_now()
  )
```

Supported date inputs are `Date`, `NaiveDateTime`, `DateTime`, ISO 8601 strings,
and valid PDF date strings. Text must be valid UTF-8. Unknown fields and invalid
values return the shared diagnostic tuple.

An update appends an incremental revision. The original bytes remain at the
start of the result, and the writer preserves unspecified common fields,
unknown information dictionary entries, the document root, and the permanent
trailer identifier. An empty patch returns the original binary unchanged.

The API updates the PDF information dictionary only. It does not read or write
XMP metadata, decrypt documents, add signatures, or guarantee that an existing
signature remains valid after an update.

## Supported inputs and limits

Information operations use the shared PDF reader and support its classic
cross-reference tables, cross-reference streams, object streams, hybrid files,
and incremental revisions. Malformed, unsupported, encrypted, or oversized
inputs return:

```elixir
{:error, {reason, diagnostic}}
```

Metadata update values are bounded by `max_pdf_info_value_bytes` per value and
`max_pdf_info_total_bytes` in total. Reader input, object, page, reference, and
revision limits apply to every information operation. See [Configurable resource
limits](resource-limits.md) and [Diagnostics](diagnostics.md).
