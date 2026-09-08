# PDF outlines and bookmarks

PDF viewers commonly call outline items bookmarks. They appear in a navigation
panel and do not add visible content to a page.

`NativeElixirPdfUtilities.Outlines` accepts PDF binaries and provides four
operations:

| Function | Successful result |
| --- | --- |
| `get/1` | `{:ok, items}` for the active outline, or `{:ok, []}` when absent |
| `put/2` | `{:ok, pdf}` with the exact replacement outline |
| `detect/1` | `{:ok, items}` with the existing outline or a validated detected proposal |
| `automatic/1` | `{:ok, pdf}` after detection and writing |

## Read and write exact outlines

`NativeElixirPdfUtilities.Outlines.get/1` returns normalized nested items with
one-based page numbers:

```elixir
alias NativeElixirPdfUtilities.Outlines

{:ok, items} = Outlines.get(pdf)
```

`put/2` replaces the active outline. The concise tuple form covers ordinary
page destinations:

```elixir
items = [
  {"Executive summary", 1},
  {"Financial results", 8,
   [
     {"Revenue", 9},
     {"Expenses", 13},
     {"Cash flow", 18}
   ]}
]

{:ok, updated_pdf} = Outlines.put(pdf, items)
```

Maps add control over expansion and destination views:

```elixir
%{
  title: "Financial results",
  page: 8,
  view: {:fit_h, 720},
  open: false,
  children: []
}
```

Supported views are `:fit`, `:fit_b`, `{:fit_h, top}`, `{:fit_v, left}`,
`{:fit_bh, top}`, `{:fit_bv, left}`, `{:fit_r, left, bottom, right, top}`,
and `{:xyz, left, top, zoom}`. Values allowed to inherit from the current view
may be `nil`. Passing an empty list removes the active outline.

Normalized items always have `:title`, `:page`, `:view`, `:open`, and
`:children`. A `nil` page makes an item a destinationless grouping item. Titles
must be non-empty UTF-8 strings, and page numbers must exist in the document.
The default view is `:fit`, the default expansion state is `true`, and the
default children list is empty. A leaf always normalizes `open` to `true`
because it has no children to expand or collapse.

The update is appended as a new PDF revision. Existing page content and
unrelated catalog entries remain unchanged. As with any PDF modification, the
new revision can affect the status of existing digital signatures.

Outline writes use the same [trailer identifier rules](pdf-information.md#updating-information)
as metadata updates.

## Automatic detection

Detection returns a proposal without changing the PDF:

```elixir
with {:ok, proposed} <- Outlines.detect(pdf) do
  adjusted = edit_for_this_document(proposed)
  Outlines.put(pdf, adjusted)
end
```

An existing outline is returned unchanged. When none exists, detection uses
extractable painted text, visual order, repeated-line filtering, and relative
font sizes to guess headings. PDF content streams normally contain positioned
text rather than heading semantics, so callers should treat the result as
best-effort.

`detect/1` validates proposals against the same rules and limits as exact input.

`automatic/1` combines detection and writing:

```elixir
{:ok, updated_pdf} = Outlines.automatic(pdf)
```

It returns `:no_outline_source` when the document has neither an existing
outline nor distinguishable heading text. It does not perform OCR.

## HTML headings

The HTML renderer can use the original heading semantics instead of guessing:

```elixir
{:ok, pdf} =
  NativeElixirPdfUtilities.HtmlToPdf.render(html,
    outlines: :headings
  )
```

Visible `h1` through `h6` elements become nested outline items. Their
destinations use their final positions after pagination. Hidden and empty
headings do not create items. An exact outline list may also be supplied through
the `:outlines` option.

Heading bookmarks also work in flex and grid containers and across forced page
breaks. A heading in a repeated table header contributes one bookmark at its
first occurrence. Line breaks in a heading preserve word separation in the
bookmark title. Heading destinations use `{:fit_h, top}` at the final page
position. Outlines are disabled when the option is omitted, `false`, or `nil`.
Page-furniture headings do not contribute to the body heading outline.

## Merge and transform behavior

Merging appends the top-level outline items from each input in document order.
Page picking, deletion, rotation, and splitting preserve items whose target
pages remain and rewrite their page numbers. An item targeting a removed page
is dropped when it has no retained children. If it still has children, it is
kept as a destinationless grouping item.

The reader preserves hierarchy, expanded or collapsed state, Unicode titles,
explicit local destinations, and local `GoTo` actions. Legacy and name-tree
named destinations are resolved and written as explicit destinations.
Unsupported actions are retained as destinationless titles and are not copied
or executed. Malformed outline structures return the shared diagnostic error
shape with the `:outlines` stage.

## Errors and limits

Failures use `{:error, {reason, diagnostic}}`. Invalid exact input returns
`:invalid_outlines`; malformed PDF outline structures return
`:invalid_pdf_input`. Missing detection candidates return `:no_outline_source`.
Reader failures use the same diagnostic contract.

Outline limits cover item count, depth, title bytes, destination lookup, and
output object capacity. Top-level items are depth one; a leaf at the maximum
depth is valid. Merge checks combined item and title-byte totals. Destination
counts include duplicate names across legacy dictionaries and name trees.

Exceeding a limit returns `:resource_limit_exceeded`. See
[Configurable resource limits](resource-limits.md) for defaults and settings.
