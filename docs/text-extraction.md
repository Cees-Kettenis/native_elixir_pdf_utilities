# Extracting text

Use `NativeElixirPdfUtilities.Text` to read embedded text. It does not perform
OCR on scanned pages.

## Reconstructed strings

```elixir
alias NativeElixirPdfUtilities.Text

{:ok, text} = Text.extract_file("invoice.pdf")
```

For a binary already in memory, use `Text.extract(pdf)`.

| Option | Result |
| --- | --- |
| `layout: true`, the default | Approximate visual lines and spacing; text pages separated by `"\f"` |
| `layout: false` | Text in drawing order; text pages separated by `"\n"` |

Both omit pages with no painted text. A document with no extractable painted
text returns `:no_extractable_text`. Extraction fails if shown text cannot be
decoded reliably; it does not return a partial result.

## Replacement text

A marked-content `ActualText` string replaces the enclosed decoded text operands
in reconstructed string extraction. Positioned extraction keeps the physical
operands by default. Set `actual_text: true` to request replacement spans:

```elixir
{:ok, document} = Text.extract_spans(pdf, actual_text: true)
```

Replacement text is emitted once per group. Nested groups use the outermost
replacement; an empty replacement suppresses the covered pieces. Adjacent
groups join without an added word break in `layout: false` output.

Replacement spans use the first text piece's baseline and the last piece's
endpoint, rather than the group's bounding box. An image drawn with an
`ActualText` label can also supply replacement text, using its transformed
position. This reads the label; it does not perform OCR.

Covered text still needs reliable font mappings and valid geometry. Text and
labels share the [extraction limits](resource-limits.md#text-extraction).
Unbalanced marked-content groups return diagnostics. Structure-tree recovery
and replacement text for vector-only artwork are unsupported.

## Positioned text

Use spans when you need to interpret columns, rows, or other document-specific
layout:

```elixir
{:ok, document} = Text.extract_file_spans("invoice.pdf")

Enum.each(document.pages, fn page ->
  Enum.each(page.spans, fn span ->
    IO.inspect({page.number, span.text, span.x, span.y})
  end)
end)
```

For a binary, use `Text.extract_spans(pdf)`. The result contains `:page_count`
and `:pages`. Every page is retained, even if its `:spans` list is empty.
Each page has `:number`, `:media_box`, `:rotation`, and `:spans`.
Each non-empty decoded text operand retains its own span, source index and
coordinates, even inside an `ActualText` group with an empty replacement.

### Span fields

| Field | Meaning |
| --- | --- |
| `:text` | Decoded text |
| `:x`, `:y` | Start of the text baseline |
| `:end_x`, `:end_y` | End of the baseline, based on text advance |
| `:source_index` | Zero-based drawing-order index within the page |
| `:font_resource` | PDF font resource name, not necessarily a family name |
| `:font_size` | Font size in PDF text space |
| `:text_matrix`, `:ctm` | Original text and current transformation matrices |
| `:render_mode` | PDF text rendering mode, from 0 through 7 |
| `:paints_text?` | Whether the mode requests filled or stroked text |
| `:adds_to_clip_path?` | Whether the mode adds text to the clipping path |
| `:joins_previous?` | Whether this text continues the preceding showing operation |

Coordinates start at the top-left of the rotated MediaBox. X increases right;
Y increases down. They use PDF default user-space units, normally 1/72 inch.
CropBox offsets and UserUnit scaling are not applied. Baselines are not glyph
bounding boxes, and endpoints can be approximate when font widths are absent.
Type 3 widths use the horizontal component of `FontMatrix`; codes outside the
declared width array advance by zero. Rotation in the glyph matrix does not
rotate the text baseline. This follows the Type 3 width rules in
[PDF 32000-1, Table 112](https://opensource.adobe.com/dc-acrobat-sdk-docs/standards/pdfstandards/pdf/PDF32000_2008.pdf).

These coordinates differ from [stamp coordinates](pdf-stamping.md#coordinates-and-page-geometry).
Use [Info.page_sizes/1](pdf-information.md#page-count-and-geometry) for physical
page dimensions.

## Source and visual order

Span order defaults to `:source`. Request `order: :visual` for approximate
line grouping:

```elixir
{:ok, document} = Text.extract_spans(pdf, order: :visual)
```

`source_index` stays unchanged, so you can restore drawing order by sorting on
it. It is an ordering key, not a persistent identifier across edited PDFs.
Neither order identifies semantic table cells automatically.

## Rendering modes and visibility

Spans include invisible text modes 3 and 7. String extraction excludes them.
The `:paints_text?` flag describes the drawing mode, not guaranteed visibility:
text may still be hidden by clipping, transparency, or other content.

## Errors and limits

Unsupported font encodings, vertical Type0 CMaps, inherited `usecmap` mappings,
and inline images can prevent extraction. Encrypted PDFs are not supported.
Failures use the [diagnostic tuple](diagnostics.md).
Hexadecimal strings in ToUnicode and Type0 Encoding CMaps accept an odd final
digit by padding it with zero. Invalid or mismatched codespace endpoints return
`:invalid_pdf_input` with page and font context.

[Extraction limits](resource-limits.md#text-extraction) cap content, work, spans,
and reconstructed spacing. Oversized numeric operands or font metrics return
`:invalid_pdf_input` before execution. Overflow in composed transforms or text
positioning also returns `:invalid_pdf_input`, with the page and failing
operator or positioning step in the diagnostic. No partial text is returned
on failure.
