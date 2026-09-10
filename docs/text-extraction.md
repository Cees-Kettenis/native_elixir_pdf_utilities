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

These coordinates differ from [stamp coordinates](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/pdf-stamping.md#coordinates-and-page-geometry).
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

[Extraction limits](resource-limits.md#text-extraction) cap content, work, spans,
and reconstructed spacing. Oversized numeric operands or font metrics return
`:invalid_pdf_input` before execution. No partial text is returned on failure.
