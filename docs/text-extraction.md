# Text Extraction

`NativeElixirPdfUtilities.Text` provides two related extraction surfaces:

- `extract/2` and `extract_file/2` return reconstructed strings for convenient
  reading and searching.
- `extract_spans/2` and `extract_file_spans/2` return decoded, positioned text
  operations for applications that need to interpret rows, columns, tables, or
  another document-specific layout.

PDF files normally contain drawing and text-positioning operations rather than
semantic tables. The library exposes reliable PDF-level information; callers
remain responsible for deciding what that information means in their domain.
No extraction function performs OCR.

## Positioned text

```elixir
alias NativeElixirPdfUtilities.Text

with {:ok, document} <- Text.extract_file_spans("invoice.pdf") do
  Enum.each(document.pages, fn page ->
    Enum.each(page.spans, fn span ->
      IO.inspect({page.number, span.source_index, span.text, span.x, span.y})
    end)
  end)
end
```

The document result contains `:page_count` and a `:pages` list. Every resolved
page is retained, including a page whose `:spans` list is empty. Each page
contains its one-based `:number`, `[left, bottom, right, top]` `:media_box`,
effective inherited `:rotation`, and positioned spans.

Span baseline coordinates use a normalized display system:

- The origin is the top-left of the rotated MediaBox.
- X increases to the right and Y increases downward.
- Page rotation and page/Form current transformation matrices are applied.
- Values are in PDF default user-space units, normally 1/72 inch.
- `x` and `y` are the baseline start; `end_x` and `end_y` are the baseline end,
  not a glyph or ink bounding box.

The end point follows the text advance calculated from the PDF font widths,
font size, spacing, and horizontal scale available to the extractor. It can be
approximate when a PDF font omits explicit width metrics.

`text_matrix` is the PDF text matrix at the start of the decoded operand. `ctm`
is the active PDF current transformation matrix and includes Form XObject
transforms. These raw PDF matrices do not include the final page-rotation and
MediaBox normalization used by the baseline coordinates. `font_resource` is
the active PDF resource key, such as `"F1"`; it is not guaranteed to be a font
family or PostScript name. `font_size` is the text-space `Tf` value rather than
a calculated display-space height.

## Source and visual order

Source execution order is the default:

```elixir
Text.extract_spans(pdf, order: :source)
```

`source_index` is zero-based within each page. Page content streams are
traversed in `/Contents` order. A Form XObject is traversed where its `Do`
operator occurs, including nested and repeated Forms, so each emitted span has
a deterministic execution index.

For convenience, callers can request the same best-effort visual line grouping
used by string layout extraction:

```elixir
Text.extract_spans(pdf, order: :visual)
```

Visual ordering changes the span list order but never changes `source_index`,
so source order can be restored with `Enum.sort_by(spans, & &1.source_index)`.

## Rendering modes and visibility

Positioned extraction retains decoded text for all PDF text rendering modes:

| Mode | PDF operation | `paints_text?` | `adds_to_clip_path?` |
| ---: | --- | --- | --- |
| 0 | Fill | true | false |
| 1 | Stroke | true | false |
| 2 | Fill and stroke | true | false |
| 3 | Neither paint nor clip | false | false |
| 4 | Fill and add to clipping path | true | true |
| 5 | Stroke and add to clipping path | true | true |
| 6 | Fill, stroke, and add to clipping path | true | true |
| 7 | Add to clipping path only | false | true |

These flags describe what the text rendering mode requests. They are not a
claim of visual visibility. The extractor does not evaluate the active clipping
path, transparency, optional-content state, later occlusion, or whether painted
content falls outside the visible crop. In particular, `adds_to_clip_path?`
does not mean the text was itself visually clipped.

The string API keeps its existing behavior and excludes modes 3 and 7. A valid
PDF containing only empty pages or non-text content returns a positioned `:ok`
result with empty span lists, while the string API returns
`:no_extractable_text`.

## Preservation boundary

Positioned extraction does not claim a lossless representation of every PDF
text feature. Its guarantee is narrower: every non-empty text operand that the
strict decoder successfully maps to Unicode is retained, even when its render
mode does not paint text.

The result does not expose internal reader or font structs. It also does not
provide OCR, semantic table cells, glyph outlines, exact ink bounds,
clipping-path visibility, or a partial result for undecodable content. An
unsupported encoding, malformed stream, invalid supported text operator, or
other explainable extraction failure returns the shared diagnostic contract:

```elixir
{:error, {reason, diagnostic}}
```

Use the positioned API for domain-specific interpretation and the string API
when a best-effort readable projection is sufficient.

## Public contract

The four extraction functions and the documented `text_document`, `text_page`,
and `text_span` types are public APIs. Existing required fields will not be
renamed, removed, or assigned a different meaning in a compatible release. New
fields or options may be added where that remains backward compatible.

`source_index` is a deterministic ordering key, not a persistent identifier for
a piece of PDF content. Editing the PDF or a future release gaining support for
additional text operations can introduce spans and therefore change later
indexes. The relative execution-order guarantee remains the stable contract.
