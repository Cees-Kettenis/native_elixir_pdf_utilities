# PDF Stamping and Page Numbers

`NativeElixirPdfUtilities.Stamp` adds text, watermarks, page numbers, or the
artwork from another PDF to an existing PDF. Each successful operation appends
an incremental revision, so the original bytes and unrelated document objects
remain intact.

## Text stamps and watermarks

Add a text stamp to every page:

```elixir
pdf = File.read!("invoice.pdf")

{:ok, stamped} =
  NativeElixirPdfUtilities.Stamp.text(pdf, "APPROVED",
    position: :top_right,
    margin: 30,
    size: 18,
    color: {0.0, 0.45, 0.12},
    opacity: 0.8
  )

File.write!("approved.pdf", stamped)
```

`watermark/3` uses useful document-watermark defaults: centered text, an
automatically fitted font size, 15 percent opacity, and 45 degrees clockwise
rotation.

```elixir
{:ok, watermarked} =
  NativeElixirPdfUtilities.Stamp.watermark(pdf, "CONFIDENTIAL",
    pages: [1, 3..5]
  )
```

Both functions accept these options:

| Option | Default | Meaning |
| --- | --- | --- |
| `:pages` | `:all` | One-based page numbers and inclusive ranges |
| `:position` | `:center` | A named anchor or an explicit `{x, y}` point |
| `:margin` | `24` | Inset used by named edge and corner positions |
| `:font` | `"DejaVu Sans"` | Font-family name |
| `:fonts` | `[]` | Additional font configuration accepted by the HTML renderer |
| `:font_weight` | `400` | CSS-style weight from 100 through 900 |
| `:font_style` | `:normal` | `:normal` or `:italic` |
| `:size` | `12` | Positive point size; watermarks default to `:auto` |
| `:color` | `{0.25, 0.25, 0.25}` | RGB components from `0.0` through `1.0` |
| `:opacity` | `1.0` | Opacity from `0.0` through `1.0` |
| `:rotation` | `0.0` | Clockwise rotation in degrees |
| `:system_font_discovery` | `false` | Allow discovery of installed system fonts |

Named positions are `:top_left`, `:top_center`, `:top_right`, `:center_left`,
`:center`, `:center_right`, `:bottom_left`, `:bottom_center`, and
`:bottom_right`.

## Page numbers

Page-number formats may contain `{{page}}`, `{{pages}}`, or both. By default,
numbers reflect their physical position in the document.

```elixir
{:ok, numbered} =
  NativeElixirPdfUtilities.Stamp.page_numbers(pdf,
    pages: [3..8],
    format: "{{page}} / {{pages}}",
    position: :bottom_center,
    size: 9
  )
```

Set `numbering: :selection` to start at one within the selected pages and make
`{{pages}}` the number of selected pages. With `numbering: :document`, the
default, `{{page}}` is the physical document page and `{{pages}}` is the full
document page count. Page numbers accept the same visual options as text
stamps. Their default position is `:bottom_center` and their default size is 9
points.

## PDF overlays

An overlay imports page artwork from a second PDF and paints it above the
existing target page:

```elixir
letterhead = File.read!("letterhead.pdf")

{:ok, branded} =
  NativeElixirPdfUtilities.Stamp.overlay(pdf, letterhead,
    pages: [1..3],
    overlay_pages: {:repeat, 1},
    fit: :contain,
    opacity: 0.9
  )
```

`overlay_pages: {:repeat, n}` repeats one overlay page across every selected
target page. Page one is used by default. `overlay_pages: :match` maps the
overlay pages to selected target pages in order and requires the two counts to
match.

The default `fit: :exact` requires equal displayed page dimensions. Other fit
modes are:

- `:contain` preserves aspect ratio and fits all artwork inside the target.
- `:cover` preserves aspect ratio and fills the target, clipping overflow.
- `:stretch` scales each axis independently to fill the target.

PDF overlays copy page content and the resources reachable from it. They do
not copy overlay annotations, outlines, metadata, interactive form behavior,
or other document-level features.

## Coordinates and page geometry

Selections are one-based. `:all` selects every page. A selection list may mix
page numbers and inclusive ranges, such as `[1, 4..6, 9]`; duplicate or
out-of-bounds pages are rejected.

Stamp coordinates describe the displayed page after the effective CropBox or
MediaBox, page rotation, and UserUnit have been applied. The origin is the
displayed top-left corner. X increases to the right and Y increases downward.
Explicit `{x, y}` positions place the text's top-left anchor in PDF points in
this coordinate system. The same contract keeps named positions and page
overlays consistent on cropped, rotated, and scaled pages.

## Fonts, limits, and diagnostics

Text must be valid UTF-8 and supported by the selected font. Additional fonts
use the same configuration accepted by `NativeElixirPdfUtilities.HtmlToPdf`.
Installed system-font discovery is disabled unless explicitly enabled.

Stamp text and decoded overlay content are bounded by `:max_stamp_text_bytes`
and `:max_stamp_decoded_content_bytes`. Existing PDF parsing and object-count
limits also apply. See [Configurable resource limits](resource-limits.md).

Recoverable failures use the shared diagnostics contract:

```elixir
{:error, {reason, diagnostic}}
```

Common reasons include `:invalid_options`, `:invalid_page_selection`,
`:page_out_of_bounds`, `:unsupported_glyph`, `:unsupported_pdf_feature`, and
`:resource_limit_exceeded`. See [Diagnostics](diagnostics.md).

Because stamping appends a new revision, signatures covering an earlier
revision remain present but may not approve the newly appended content. The
calling application should apply or reapply signatures after its final stamp
when signature policy requires the visible result to be covered.
