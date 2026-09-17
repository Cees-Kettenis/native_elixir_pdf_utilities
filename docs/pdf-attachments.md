# PDF attachments

`NativeElixirPdfUtilities.Attachments` stores caller-supplied files inside a
PDF. Attachments are separate from its pages. Use `Merge` to append another
PDF's pages instead.

## Embed and inspect files

```elixir
alias NativeElixirPdfUtilities.{Attachments, HtmlToPdf}

{:ok, pdf} = HtmlToPdf.render("<p>Invoice</p>")
{:ok, bundled} = Attachments.embed(pdf, [
  %{
    filename: "invoice-data.csv",
    bytes: "item,amount\nConsulting,100\n",
    description: "Original invoice data"
  }
])
{:ok, files} = Attachments.list(bundled)
File.write!("invoice-with-data.pdf", bundled)
```

Each input map requires binary `:bytes` and a UTF-8 `:filename`. Optional
`:description` defaults to an empty string. Optional `:mime_type` must be a
`type/subtype` string without parameters. Filenames cannot be empty, `.` or
`..`, or contain path separators or NUL. Unknown metadata keys and duplicate
names are rejected, including collisions with existing attachments.

`embed/3` currently accepts only an empty options list. Existing attachments
and unrelated catalog name trees are preserved. Empty input returns the
original PDF. Writes are incremental and reject signed documents.

`list/1` returns maps containing `:filename`, `:description`, `:mime_type`,
and `:size`. Size is the declared uncompressed size and can be `nil` for an
existing attachment. MIME type can also be absent. Listing reads metadata
without extracting, executing or saving files. There is no public extraction
API in this release.

## MIME detection

Recognized signatures take precedence over case-insensitive extension
inference. If neither is known, an explicit MIME type is retained or
`application/octet-stream` is used. A supplied generic
`application/octet-stream` does not suppress a recognized type. Clear conflicts
between signatures, known extensions and supplied MIME types return
`:invalid_mime_type` diagnostics.

| Evidence | Supported detection |
| --- | --- |
| Magic bytes | PNG, JPEG, GIF, WebP, BMP, TIFF, ICO, PDF, GZIP, ID3-tagged MP3, WAV, Ogg, recognized MP4 `ftyp` brands, OLE, ZIP |
| Extension fallback | Those formats plus SVG, text, CSV, JSON, XML, HTML, Markdown, DOC, XLS, PPT, DOCX, XLSX and PPTX |
| ZIP metadata | DOCX, XLSX and PPTX when `[Content_Types].xml` and the corresponding main document entry exist |
| OLE container | DOC, XLS or PPT extension or explicit compatible MIME hint, otherwise `application/x-ole-storage` |

ZIP detection reads bounded central-directory metadata and never inflates
entries. Malformed directories, duplicate names, conflicting Office types,
split archives and ZIP64 inspection are rejected. Generic ZIP content with an
Office extension is a conflict. OLE identification cannot distinguish Office
file types without a filename or explicit MIME hint. Detection is intentionally limited; it
does not fully validate a file's structure or establish that a container's
entries contain valid documents.

Accepted MIME aliases are `image/jpg`, `image/x-icon`,
`application/x-zip-compressed`, `application/x-gzip` and `text/xml`, normalized
to their canonical types. Generic ZIP and OLE MIME hints are compatible with
their recognized document subtypes. Unknown `ftyp` brands are left to extension
or caller inference rather than being labeled MP4.

## Caller responsibilities

Arbitrary binary payloads are supported within
[resource limits](resource-limits.md#forms-and-attachments). The library
preserves bytes exactly and does not fetch remote files or create automatic
launch actions. The caller reads local files and decides what to embed.

MIME detection describes content and is not malware scanning. Callers must
approve attachment contents, apply their upload policy and scan files when
needed. A filename or MIME type does not establish that a file is safe.
PDF viewers vary in which attachments they expose, preview or permit users to
open. Embedding a file does not guarantee that a particular viewer will display
it.
