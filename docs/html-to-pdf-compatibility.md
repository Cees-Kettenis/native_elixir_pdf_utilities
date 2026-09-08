# HTML to PDF compatibility

`NativeElixirPdfUtilities.HtmlToPdf` implements a strict, document-oriented
HTML and CSS subset. It is intended for reports, invoices, labels, statements,
and forms. It is not a browser engine.

The renderer rejects malformed or unsupported HTML and CSS instead of silently
ignoring it. Failures use the shared [diagnostic contract](diagnostics.md). See
[HTML to PDF examples](html-to-pdf-examples.md) for runnable code.

## Render options

| Option | Accepted values |
| --- | --- |
| `:page_size` | `:a5`, `:a4`, `:a3`, `:b5`, `:b4`, `:jis_b5`, `:jis_b4`, `:letter`, `:legal`, `:ledger`, a `{name, orientation}` pair, a CSS two-length string, or `{width, height}` |
| `:margin` | A PDF-point number, a one-to-four-value CSS length string, or a map with `:top`, `:right`, `:bottom`, and `:left` |
| `:base_url` | A local path or `file://` URL used as the root for document-selected local images and fonts |
| `:assets` | A map of document references to `{:bytes, binary}` or `{:file, path}` |
| `:asset_resolver` | A one-argument function that returns `{:ok, binary}`, `:not_found`, or `{:error, reason}` |
| `:stylesheets` | A list of `{:css, css}` and `{:file, path}` entries |
| `:default_font` | A font family or fallback list; defaults to bundled DejaVu Sans |
| `:fonts` | Static TrueType faces supplied as maps, keyword lists, or `{family, path}` tuples |
| `:system_font_discovery` | Enables installed-font lookup; defaults to `true` |
| `:metadata` | `:title`, `:author`, `:subject`, `:keywords`, `:producer`, `:creation_date`, and `:modification_date` |
| `:outlines` | `:headings`, an exact outline list, `false`, or `nil`; see [PDF outlines and bookmarks](pdf-outlines.md) |
| `:page_furniture` | Optional `:header` and `:footer` templates with `:default`, `:first`, `:odd`, and `:even` variants |
| `:unsupported_glyphs` | `:replace`, the default, or `:error` |

Explicit `:page_size` and `:margin` values override `@page` settings. Tuple
page sizes up to `20 x 20` are inches; larger tuples are PDF points.
Metadata dates accept calendar structs, ISO 8601 strings, and PDF date strings.

The renderer never fetches remote assets. An `:asset_resolver` may supply bytes
for any document reference. Local paths selected by the document must stay
beneath `:base_url`; traversal and symlink components are rejected.

## HTML support

| Area | Supported |
| --- | --- |
| Document | `doctype html`, `html`, `head`, `body`, `style`, `meta`, `title` |
| Blocks | `article`, `aside`, `div`, `footer`, `header`, `main`, `nav`, `section`, `p`, `h1` through `h6` |
| Inline text | `span`, `strong`, `b`, `em`, `i`, `a`, `br`, and WHATWG named and numeric character references |
| Lists | `ul`, `ol`, `li` |
| Tables | `table`, `caption`, `colgroup`, `col`, `thead`, `tbody`, `tfoot`, `tr`, `th`, `td` |
| Images | `img` with a required `src` |
| Static forms | Text, checkbox, and radio `input`; `select` and `option`; `textarea`; `button` |
| Links | `https://`, `http://`, and `mailto:` annotations |

Supported attributes include `id`, `class`, `style`, `title`, `role`, `data-*`,
`aria-*`, `lang`, `href`, `src`, `alt`, table spans, header `scope`, and the
form attributes `type`, `value`, `name`, `checked`, `selected`, and `disabled`.

## CSS support

| Area | Supported |
| --- | --- |
| Selectors | Universal, element, class, ID, attribute presence/equality, descendant, direct child, comma groups, `:not()` with a simple selector, root and child/type position selectors, `::before`, `::after` |
| Cascade | Specificity, source order, inline styles, `!important`, inherited text styles, custom properties, `var()`, `currentColor` |
| Generated content | Quoted `content`, `attr()`, named `counter()`, `counter-reset`, `counter-increment` |
| Units | `pt`, `px`, `rem`, `mm`, `cm`, `in`, supported percentages, and unitless `0` |
| Display | `block`, `inline`, `inline-block`, `none`, `flex`, `inline-flex`, `grid`, `inline-grid` |
| Sizing and spacing | Width and height, min/max constraints, `min()`, `aspect-ratio`, `box-sizing`, margins, padding, and gaps |
| Borders and backgrounds | Side and shorthand borders, standard border styles, radius, table border modes and spacing, colors, images, size, position, and repeat |
| Positioning | Static, relative, and absolute positioning; insets; positioned containing blocks; integer `z-index` |
| Images | `object-fit` and `object-position` |
| Text | Color, family, size, weight, style, line height, alignment, transform, vertical alignment, line and word breaking, `white-space: normal` and `pre-line`, letter spacing |
| Page rules | Bare `@page` size and margins; `@media print`, `only print`, and `all`; supported page-break properties |
| Fonts | Local `@font-face` with `font-family`, `src`, optional weight, style, and display |
| Flexbox | Direction, wrapping, gaps, ordering, grow/shrink/basis, main-axis min/max constraints, and justify/align properties |
| Grid | Explicit and automatic tracks, `repeat()`, `minmax()`, row and column placement, `grid-area`, gaps, and justify/align properties |

Unknown properties, unsupported values, malformed selectors, and invalid
`@page` declarations return `:invalid_css`.

## Rendering behavior

### Cascade and generated content

Positional selectors distinguish sibling elements even when their tags,
attributes, and text are identical. Custom property names are case-sensitive,
so `--Accent` and `--accent` are separate variables in stylesheets and inline
declarations. Supported quoted selector values and generated-content strings
preserve embedded punctuation, escaped quotes, and comment-like text.

Counters follow document order through elements and their pseudo-elements.
Nested resets establish inner scopes; later outer siblings resume the enclosing
counter. Counters created by preceding siblings and pseudo-elements remain
available where their scope permits.

### Sizing and wrapping

Percentage widths resolve against the containing width. Supported minimum
dimensions take precedence when a minimum exceeds the corresponding maximum,
including flex items and images. Emergency word breaking keeps an indivisible
grapheme intact; it may overflow a line narrower than that grapheme instead of
repeatedly attempting to split it.

### Pagination

The renderer handles automatic and explicit page breaks, paragraph
fragmentation, repeated table headers, page margins, and best-effort
`break-inside: avoid`. Content taller than a page still fragments.

Page furniture is placed inside the existing margins and does not change body
pagination. `{{page}}` and `{{pages}}` insert page numbers. Furniture that does
not fit returns `:invalid_layout`. On page one, `:first` takes precedence;
later pages use `:odd` or `:even`, then `:default`.

Image and background-image clipping regions move with page furniture into the
header or footer margin, including images using `object-fit`.

### Tables

Tables support automatic and fixed layouts, column hints, `colspan`, `rowspan`,
`tfoot`, repeated multi-row headers, nested tables, explicit heights, and
separate or collapsed borders. A short row does not span undeclared trailing
columns.

A cell's `rowspan` stops at its row-group boundary. It cannot occupy cells in
a later `thead`, `tbody`, or `tfoot` group.

### Static forms

Text inputs show their `value`. Checkbox and radio states follow `checked`.
Selects show the selected option, or the first option by default. Textareas and
buttons use their child text. These controls are drawn as PDF content and are
not editable PDF fields.

### Images and backgrounds

SVGs must be self-contained shapes, text, and internal literal `#id` references.
SVG image elements, including filter images and data URIs, and XML entity declarations are
rejected before rasterization. SVG resources do not inherit authorization from
`:assets`, `:base_url`, or the asset resolver.

The renderer accepts JPEG, 8-bit non-interlaced RGB or RGBA PNG, and SVG data
URIs. Images work in block, table, flex, and grid layouts. Background images
support explicit sizes, `cover`, `contain`, repeat modes, and positioning.

### Fonts and text

Bundled DejaVu Sans provides the default face and final fallback. Explicitly
registered fonts take precedence, followed by requested installed fonts when
system discovery is enabled. Register fonts explicitly when output must be the
same across hosts.

Font fallback happens before layout. Missing graphemes become U+FFFD by default;
`unsupported_glyphs: :error` returns `:unsupported_glyph` instead.

### Performance and cache lifetime

The renderer indexes selectors and reuses parsed inline styles, resolved fonts,
and fallback candidates within each style or fallback computation. CSS values
still resolve against each element's inherited styles. These optimizations
require no new options.

Body and page-furniture computations use separate temporary caches. The caches
are discarded after use and have no separate size limit. Shared font-cache
limits are described in [Configurable resource limits](resource-limits.md#cache-scope).

Large tables still measure and lay out cell content separately. Repeated images
are decoded or rasterized during styling before the PDF writer deduplicates
resources. Use the [render benchmark](html-to-pdf-examples.md#benchmark-rendering)
to measure your workload.

## Known limits

The renderer does not support:

- JavaScript or runtime DOM behavior.
- `script`, `canvas`, `video`, `audio`, `iframe`, or interactive form behavior.
- Remote fetching by the renderer itself.
- Floats, fixed positioning, transforms, or animations.
- Page selectors, named pages, or page-margin CSS boxes. Use
  `:page_furniture` for running headers, footers, and page numbers.
- Browser-complete table, flexbox, or grid algorithms.
- Nested `counters()`, counter styles, list-marker counters, or CSS
  `counter(page)` and `counter(pages)`.
- Greyscale, indexed-color, 16-bit, or interlaced PNG files.
- WOFF, WOFF2, variable fonts, or CFF-flavored OpenType fonts. Font embedding
  flags may also prohibit a face.
- Complex shaping and bidirectional layout for Arabic, Indic scripts, Thai,
  emoji sequences, and similar typography.

Processing limits for HTML, images, SVG, fonts, layout, and PDF output are
listed in [Configurable resource limits](resource-limits.md).

## Browser comparison

The parity suite compares the documented rendering subset with Chromium. Its
scope, fixtures, thresholds, and command are listed in
[HTML to PDF browser parity coverage](html-to-pdf-browser-parity-coverage.md).
