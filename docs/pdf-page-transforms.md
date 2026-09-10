# Selecting, rotating, and splitting pages

`Transform` changes which pages appear in a PDF. `Split` returns separate PDFs.
Both accept PDF binaries and return new binaries.

```elixir
alias NativeElixirPdfUtilities.{Split, Transform}

{:ok, pdf} = File.read("report.pdf")
```

## Select, reorder, or delete pages

```elixir
{:ok, selected} = Transform.pick_pages(pdf, [4, 1..2])
{:ok, shortened} = Transform.delete_pages(pdf, [8..10])
```

The first example produces pages 4, 1, and 2 in that order. The second removes
pages 8 through 10 and keeps the remaining order. Save a result with
`File.write/2`.

Selections use one-based page numbers from the input. Mix numbers and ascending,
unit-step ranges. Duplicates and out-of-range pages are rejected. You cannot
pick an empty selection or delete every page. Deleting an empty selection
rebuilds the document without removing pages.

## Rotation

```elixir
{:ok, rotated} = Transform.rotate_pages(pdf, 90, pages: [1, 3..5])
{:ok, all_rotated} = Transform.rotate_pages(pdf, -90)
```

Positive angles rotate clockwise relative to the existing page rotation.
Use an integer multiple of 90. The `:pages` option defaults to `:all`.
Check the result with [Info.page_sizes/1](pdf-information.md#page-count-and-geometry).

## Splitting

| Function | Result on success |
| --- | --- |
| `Split.by_page(pdf)` | `{:ok, [page_pdf, ...]}` |
| `Split.by_ranges(pdf, [1..3, 8..10])` | `{:ok, [first_pdf, second_pdf]}` |
| `Split.after_page(pdf, 5)` | `{:ok, {first_pdf, second_pdf}}` |

`after_page/2` puts the selected page at the end of the first output. The split
point must leave at least one page in each output. `by_ranges/2` requires a
non-empty list of ascending, unit-step ranges; ranges may overlap because
each output is independent. `by_page/1` returns `{:ok, []}` for a zero-page PDF.

## Rebuild behavior

| Preserved | Not preserved as document features |
| --- | --- |
| Selected page content, images, resources, and geometry | Metadata, page labels, viewer preferences, and form configuration |
| External URI links and internal links to retained pages | Links to removed pages and unresolved named links |
| Bookmarks with retained destinations or children | Bookmarks whose destinations and children were all removed |

Outputs are rebuilt PDFs. A retained page may share resources with removed
pages, so deletion is not secure redaction. Interactive form behavior is not
guaranteed. See [bookmark preservation](pdf-outlines.md#merge-and-transform-behavior).

Failures return [diagnostics](diagnostics.md). Invalid selections, unsupported
page dependencies, and [split limits](resource-limits.md#merging-and-splitting)
can prevent an operation from completing.
