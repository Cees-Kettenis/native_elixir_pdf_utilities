# Stamps, watermarks, and page numbers

Use `NativeElixirPdfUtilities.Stamp` to add content to an existing PDF. All
operations return `{:ok, updated_pdf}` or an [error](diagnostics.md).

```elixir
alias NativeElixirPdfUtilities.Stamp

{:ok, pdf} = File.read("report.pdf")
```

## Text stamps and watermarks

```elixir
{:ok, approved} = Stamp.text(pdf, "APPROVED", position: :top_right)
{:ok, confidential} = Stamp.watermark(pdf, "CONFIDENTIAL")

File.write!("confidential.pdf", confidential)
```

Text stamps default to centered, 12-point text. Watermarks default to centered
text, an automatically fitted size, 15% opacity, and 45-degree clockwise rotation.

## Page numbers

```elixir
{:ok, numbered} =
  Stamp.page_numbers(pdf,
    pages: [3..8],
    format: "{{page}} / {{pages}}"
  )
```

Numbers default to the bottom center at 9 points. The default format is
`"Page {{page}} of {{pages}}"`. `numbering: :document` uses physical page numbers
and the full page count. Use `numbering: :selection` to number the selected
pages starting at one and count only those pages.

## Text options

Text stamps, watermarks, and page numbers share these options:

| Option | Default for text stamps | Meaning |
| --- | --- | --- |
| `:pages` | `:all` | Pages to stamp |
| `:position` | `:center` | Named position or `{x, y}` |
| `:margin` | `24` | Edge inset in points for named positions |
| `:size` | `12` | Positive point size or `:auto` |
| `:color` | `{0.25, 0.25, 0.25}` | RGB values, each from 0 to 1 |
| `:opacity` | `1.0` | From transparent `0` to opaque `1` |
| `:rotation` | `0` | Clockwise angle in degrees |
| `:font` | `"DejaVu Sans"` | Font family |
| `:font_weight` | `400` | Weight from 100 through 900 |
| `:font_style` | `:normal` | `:normal` or `:italic` |
| `:fonts` | `[]` | [Registered TrueType fonts](html-to-pdf-examples.md#register-a-font) |
| `:system_font_discovery` | `false` | Allow installed-font lookup |

Named positions combine `top`, `center`, or `bottom` with `left`, `center`, or
`right`: for example, `:top_left`, `:center_right`, and `:bottom_center`.
The middle position is `:center`.

Text must be non-empty UTF-8 and supported by the chosen face. Unlike HTML
rendering, stamping does not replace missing characters or switch fonts for
individual characters.

## PDF overlays

Use another PDF's artwork as a letterhead or overlay:

```elixir
{:ok, letterhead} = File.read("letterhead.pdf")
{:ok, branded} = Stamp.overlay(pdf, letterhead, fit: :contain, opacity: 0.9)
```

By default, overlay page one repeats on every target page. Use
`overlay_pages: {:repeat, n}` to choose another page, or `overlay_pages: :match`
to pair overlay pages with selected target pages in order. Matching requires
equal counts. `:pages` selects the target pages and defaults to `:all`.

| `:fit` | Behavior |
| --- | --- |
| `:exact`, the default | Requires equal displayed page sizes |
| `:contain` | Fits all artwork while keeping its aspect ratio |
| `:cover` | Fills the target while clipping overflow |
| `:stretch` | Scales width and height independently |

`:opacity` defaults to `1.0` and fades the composed overlay, not the target.
Only artwork and its resources are imported. Overlay annotations, bookmarks,
metadata, and interactive forms are not copied.

## Coordinates and page geometry

Selections use one-based page numbers and ascending, unit-step ranges, such as
`pages: [1, 4..6]`. Empty selections, duplicates, and out-of-range pages fail.

The coordinate origin is the displayed page's top-left after cropping,
rotation, and UserUnit scaling. X increases right; Y increases down. Explicit
`{x, y}` positions place the text's top-left anchor in points, or 1/72 inch.
[Text-extraction coordinates](text-extraction.md#positioned-text) use a different
page reference.

## Preservation and limits

Stamping appends a revision and preserves the target's original content,
metadata, bookmarks, and form configuration. Existing signatures may not cover
the new content. Stamping is not redaction.

Text and overlays are bounded by [stamping limits](resource-limits.md#stamping).
Invalid selections, unavailable fonts, missing glyphs, unsupported PDFs, and
limit failures return [diagnostics](diagnostics.md).
