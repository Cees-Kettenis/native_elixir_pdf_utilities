# PDF page transforms and splitting

`NativeElixirPdfUtilities.Transform` selects, reorders, deletes, and rotates
pages. `NativeElixirPdfUtilities.Split` rebuilds one source document as several
PDFs. Both APIs accept PDF binaries so callers retain control of file and
storage access.

## Page numbering

Page numbers begin at one and follow the source PDF page-tree order. Ranges are
inclusive and must be ascending. The APIs reject zero, negative, descending,
duplicate, and out-of-bounds selections instead of changing them silently.

Selections refer to the original input document. For example:

```elixir
{:ok, reordered} = Transform.pick_pages(pdf, [4, 1..2])
{:ok, without_appendix} = Transform.delete_pages(pdf, [8..10])
```

The first operation outputs source pages 4, 1, and 2 in that order. The second
removes source pages 8 through 10 while retaining the order of every other
page. Picking the same page more than once and deleting every page are rejected.

## Rotation

`Transform.rotate_pages/3` rotates pages clockwise by an integer multiple of 90
degrees. Rotation is relative to each page's effective existing rotation and is
normalized to 0, 90, 180, or 270 degrees.

```elixir
{:ok, rotated} = Transform.rotate_pages(pdf, 90, pages: [1, 3..5])
{:ok, all_rotated} = Transform.rotate_pages(pdf, -90)
```

The `:pages` option defaults to `:all`.

## Splitting

`Split.by_page/1` returns one rebuilt PDF per source page. An empty source
document returns an empty list.

```elixir
{:ok, page_pdfs} = Split.by_page(pdf)
```

`Split.by_ranges/2` returns one PDF per inclusive range. Ranges may overlap
because each describes an independent output.

```elixir
{:ok, [summary, appendix]} = Split.by_ranges(pdf, [1..3, 8..10])
```

`Split.after_page/2` always returns exactly two non-empty PDFs. The selected
page ends the first output.

```elixir
{:ok, {first_packet, second_packet}} = Split.after_page(pdf, 5)
```

The split point must fall between the first and penultimate source pages.

## Rebuild behavior

Every successful operation writes a fresh PDF 1.7 catalog, flat page tree,
object-number mapping, cross-reference table, and trailer. It copies only the
selected pages and the objects reachable from those pages. Page content and
other streams retain their original bytes while indirect references receive
new object numbers.

The rebuild materializes effective inherited `Resources`, `MediaBox`,
`CropBox`, and `Rotate` values. Every emitted indirect reference resolves.
Internal link annotations targeting retained pages are remapped. Links to
removed pages and named destinations are omitted, while external URI links
remain.

Deleting a page is not secure redaction. A resource shared with a retained page
must remain in the output. Catalog-level outlines, named destinations, forms,
page labels, viewer preferences, and metadata are not preserved by these
operations.

Malformed input, invalid selections, unsupported page dependencies, and
resource limits use the shared diagnostic result:

```elixir
{:error, {reason, diagnostic}}
```

See [Diagnostics](diagnostics.md) and [Resource limits](resource-limits.md) for
the common fields and process-wide limits.
