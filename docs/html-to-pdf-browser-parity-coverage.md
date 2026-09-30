# Supported browser rendering

The HTML renderer supports a document-oriented subset of browser rendering for
reports, invoices, statements, forms, and labels. Use the overview below to
choose a layout, then follow the linked reference for supported values and
restrictions.

## Supported behavior

| Area                                                                         | What you can use                                                                                              |
| ---------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------- |
| [HTML and text](html-to-pdf-compatibility.md#html-support)                    | Headings, paragraphs, inline emphasis, lists, links, character references, and supported form controls           |
| [CSS](html-to-pdf-compatibility.md#css-support)                               | Supported selectors, the cascade, custom properties, generated content, colors, borders, and spacing          |
| [Sizing and layout](html-to-pdf-compatibility.md#css-support)                 | Block and inline layout, flexbox, grid, size constraints, text wrapping, and relative or absolute positioning |
| [Tables](html-to-pdf-compatibility.md#tables)                                 | Column sizing, spanning cells, nested tables, repeated headers, and separate or collapsed borders             |
| [Images and backgrounds](html-to-pdf-compatibility.md#images-and-backgrounds) | JPEG, supported PNG formats, self-contained SVG, image fitting, and background sizing and repetition          |
| [Pages](html-to-pdf-compatibility.md#pagination)                              | Page sizes and margins, automatic and explicit breaks, running headers and footers, and page numbers          |
| [Fonts](html-to-pdf-compatibility.md#fonts-and-text)                          | Bundled DejaVu Sans, registered TrueType fonts, optional system-font discovery, and glyph fallback            |

Supported controls create interactive AcroForm fields by default; use
`forms: :static` for artwork only. See [PDF forms](pdf-forms.md).
This subset does not include JavaScript or full browser layout and typography. See [Known limits](html-to-pdf-compatibility.md#known-limits)
before adapting a browser template. Unsupported HTML tags and CSS properties
are reported through the [diagnostic contract](diagnostics.md). Some accepted
`@page` declarations are ignored, including `page-orientation`, `marks`, `bleed`,
and margins that cannot be normalized. See
[page declarations without an effect](html-to-pdf-compatibility.md#page-declarations-without-an-effect)
before relying on successful rendering as evidence that page settings were applied.

## Visual parity

We compare rendered documents with Chromium at 72 DPI. Every fixture enforces
a strict ceiling of **less than 1% differing pixels**. A pixel differs when at least one
RGB channel differs by more than 12 out of 255. Each fixture also checks the
page count and its existing average channel-delta limit.

Chromium and the native renderer load the same bundled DejaVu font files. Two
font fixtures also use the same bundled Liberation font files. Each run records
SHA-256 hashes of these files alongside its raster output. The quality matrix
checks the threshold on every supported Elixir runtime. Browser and rasterizer
updates can change the result, so failures must be investigated and corrected
when those tools change. The measured bound applies to these representative
fixtures within the supported HTML/CSS subset; arbitrary documents or different
font files may have a different result.

These percentages describe visual comparisons, not a percentage of browser
features supported. Start with the [rendering examples](html-to-pdf-examples.md)
for working templates.

## Regression coverage

The suite includes invoices, statements, forms, labels, and focused fixtures for
supported layout behavior. Recent cases cover `white-space: nowrap`, leading
decimal lengths, letter spacing and ligatures, collapsed table borders across
pages, and body padding, backgrounds, and grid layout.
Flex alignment checks preserve explicit image and block dimensions while still
stretching automatic dimensions. A two-page compact stock-sticker fixture uses
synthetic item data and square QR images on 5 cm by 3 cm labels.
PNG coverage includes all legal static color types and sample depths, including
Adam7 interlacing. Font checks include subsetting and signed TrueType glyph deltas.

See the [parity tests](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/test/html_to_pdf/browser_parity_test.exs) for the
current fixtures and thresholds. Each comparison writes its measurements and
tool versions to `tmp/browser_parity/<fixture>/stats.exs`.
