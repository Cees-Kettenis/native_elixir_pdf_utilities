# PDF bookmarks

Bookmarks appear in a PDF viewer's navigation panel. They do not add visible
text to a page. Use `NativeElixirPdfUtilities.Outlines` to read, replace, or
suggest bookmarks.

## Read and write exact outlines

```elixir
alias NativeElixirPdfUtilities.Outlines

{:ok, pdf} = File.read("report.pdf")
{:ok, existing} = Outlines.get(pdf)

{:ok, updated} =
  Outlines.put(pdf, [
    {"Summary", 1},
    {"Results", 3, [{"Revenue", 4}, {"Expenses", 5}]}
  ])

File.write!("bookmarked.pdf", updated)
```

Pages are one-based and must exist in the input. `get/1` returns `{:ok, []}`
when there are no bookmarks. `put(pdf, [])` removes the active bookmark tree.
Titles must be non-empty UTF-8 strings.

## Control the destination and expansion

Use maps when you need more than a title and page:

```elixir
item = %{
  title: "Results",
  page: 3,
  view: {:fit_h, 720},
  open: false,
  children: [{"Revenue", 4}]
}

{:ok, updated} = Outlines.put(pdf, [item])
```

| Field | Default | Meaning |
| --- | --- | --- |
| `:title` | Required | Bookmark text |
| `:page` | `nil` | Target page; `nil` creates a grouping item |
| `:view` | `:fit` | How the viewer positions the target page |
| `:open` | `true` | Whether children start expanded |
| `:children` | `[]` | Nested bookmarks |

Returned items always contain all five fields. Leaves normalize `:open` to
`true` because they have no children to collapse.

Views are `:fit`, `:fit_b`, `{:fit_h, top}`, `{:fit_v, left}`,
`{:fit_bh, top}`, `{:fit_bv, left}`, `{:fit_r, left, bottom, right, top}`,
and `{:xyz, left, top, zoom}`. Coordinates use PDF page user space.
View numbers may be `nil` to leave that setting unchanged, except in `:fit_r`.

## Automatic detection

```elixir
{:ok, proposed} = Outlines.detect(pdf)
# Review or edit proposed before saving.
{:ok, updated} = Outlines.put(pdf, proposed)
```

Detection reuses existing bookmarks. Otherwise it guesses headings from text
size and position. Review the result for your document. It does not perform OCR.
`Outlines.automatic(pdf)` combines detection and writing when no review is needed.

If no suitable text or existing bookmarks are found, detection returns a
[diagnostic](diagnostics.md) with reason `:no_outline_source`.

## HTML headings

When creating a PDF from HTML, use `outlines: :headings` to turn visible
`h1` through `h6` elements into bookmarks. Their hierarchy follows heading
levels. Hidden and empty headings are skipped. You can also pass an exact
bookmark list. See the [HTML example](html-to-pdf-examples.md#create-bookmarks-from-headings).

## Merge and transform behavior

[Merging](pdf-merging.md) appends bookmarks in input order and updates target
pages. [Page transforms and splitting](pdf-page-transforms.md) keep bookmarks
for retained pages. A removed destination with retained children becomes a
grouping item; an item with neither is dropped.

Updates preserve page content by appending a revision. Previous bookmarks
remain in earlier file bytes, and existing signatures may be affected.
Unsupported bookmark actions are read as destinationless titles.

Invalid input returns `:invalid_outlines`; malformed source bookmarks return
`:invalid_pdf_input`. See [Diagnostics](diagnostics.md) and
[bookmark limits](resource-limits.md#metadata-and-bookmarks).
