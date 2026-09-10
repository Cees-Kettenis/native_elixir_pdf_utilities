# Supported browser rendering

The HTML renderer supports a document-oriented subset of browser rendering for
reports, invoices, statements, forms, and labels. Use the overview below to
choose a layout, then follow the linked reference for supported values and
restrictions.

## Supported behavior

| Area                                                                         | What you can use                                                                                              |
| ---------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------- |
| [HTML and text](html-to-pdf-compatibility.md#html-support)                    | Headings, paragraphs, inline emphasis, lists, links, character references, and static form controls           |
| [CSS](html-to-pdf-compatibility.md#css-support)                               | Supported selectors, the cascade, custom properties, generated content, colors, borders, and spacing          |
| [Sizing and layout](html-to-pdf-compatibility.md#css-support)                 | Block and inline layout, flexbox, grid, size constraints, text wrapping, and relative or absolute positioning |
| [Tables](html-to-pdf-compatibility.md#tables)                                 | Column sizing, spanning cells, nested tables, repeated headers, and separate or collapsed borders             |
| [Images and backgrounds](html-to-pdf-compatibility.md#images-and-backgrounds) | JPEG, supported PNG formats, self-contained SVG, image fitting, and background sizing and repetition          |
| [Pages](html-to-pdf-compatibility.md#pagination)                              | Page sizes and margins, automatic and explicit breaks, running headers and footers, and page numbers          |
| [Fonts](html-to-pdf-compatibility.md#fonts-and-text)                          | Bundled DejaVu Sans, registered TrueType fonts, optional system-font discovery, and glyph fallback            |

This subset does not include JavaScript, interactive PDF form fields, or full
browser layout and typography. See [Known limits](html-to-pdf-compatibility.md#known-limits)
before adapting a browser template. The renderer reports unsupported HTML and
CSS through the [diagnostic contract](diagnostics.md).

## Visual parity

We compare rendered documents with Chromium. Most reference documents use a
tolerance of at most **5% differing pixels**. Our target is **less than 2%** across the supported subset.

These percentages describe visual comparisons, not a percentage of browser
features supported. Start with the [rendering examples](html-to-pdf-examples.md)
for working templates.
