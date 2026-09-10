# PDF information and metadata

Use `NativeElixirPdfUtilities.Info` to read metadata, count pages, check page
sizes, or update a document's title and other information.

```elixir
alias NativeElixirPdfUtilities.Info

{:ok, pdf} = File.read("report.pdf")
{:ok, information} = Info.get(pdf)
{:ok, page_count} = Info.page_count(pdf)
```

## Read metadata

`Info.get/1` returns a map with these keys. Missing fields are `nil`.

| Keys | Returned value |
| --- | --- |
| `:title`, `:author`, `:subject`, `:keywords`, `:producer` | String or `nil` |
| `:creation_date`, `:modification_date` | `NaiveDateTime` or `nil` |

Dates retain the document's wall-clock time. A timezone suffix is validated,
but the returned date is not converted to UTC. XMP metadata is not read.

## Updating information

```elixir
{:ok, updated_pdf} =
  Info.put(pdf,
    title: "Monthly statement",
    author: "Finance team",
    keywords: ["statement", "monthly"],
    modification_date: DateTime.utc_now()
  )

File.write!("updated.pdf", updated_pdf)
```

Pass a map or keyword list using the fields above. Omitted fields stay
unchanged; `nil` removes a field. Keywords accept a string or a list of strings.
Text must be valid UTF-8. Dates accept `Date`, `NaiveDateTime`, `DateTime`,
ISO 8601 strings, or valid PDF date strings.

The update preserves page content and unrelated information. It appends a
revision, so old metadata remains in the file's earlier bytes. This is not
secure deletion and may affect existing signatures. XMP is not updated.
An empty patch returns the original binary.

## Page count and geometry

```elixir
{:ok, count} = Info.page_count(pdf)
{:ok, pages} = Info.page_sizes(pdf)

Enum.each(pages, fn page ->
  IO.inspect({page.page_number, page.width, page.height, page.rotation})
end)
```

Each size entry contains:

| Field | Meaning |
| --- | --- |
| `:page_number` | One-based page number |
| `:width`, `:height` | MediaBox dimensions after rotation and UserUnit scaling |
| `:unit` | Always `:point`, or 1/72 inch |
| `:rotation` | `0`, `90`, `180`, or `270` degrees |
| `:media_box` | `%{left: ..., bottom: ..., right: ..., top: ...}` before UserUnit scaling |

Dimensions use the MediaBox, not the visible CropBox. For placing stamps, see
[stamp coordinates](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/pdf-stamping.md#coordinates-and-page-geometry).

## Encryption status

```elixir
{:ok, encrypted?} = Info.encrypted?(pdf)
```

This checks encryption without decrypting the document. Other information
operations require an unencrypted PDF.

Failures return the shared [diagnostic tuple](diagnostics.md). See
[PDF input support](pdf-reader.md#supported-inputs) and
[metadata limits](resource-limits.md#metadata-and-bookmarks) for restrictions.
