# HTML and CSS support

Render static reports, invoices, forms and labels. Start with the
[examples](html-to-pdf-examples.md). Unsupported features return
[diagnostics](diagnostics.md).

## HTML support

| Content | Supported |
| --- | --- |
| Text | Headings, paragraphs, sections, inline formatting, quotations and preformatted text |
| Lists | Ordered, unordered and description lists, including nesting and numbering |
| Tables | Captions, column/row groups, spanning cells and repeated headings |
| Images | PNG, JPEG, SVG and responsive `picture`/`srcset`/`sizes` selection |
| Links | URI and same-document fragment destinations, including static image maps |
| Forms | Initial artwork and supported editable PDF fields |
| Embedded documents | Authorized local HTML, plaintext and supported images; iframe `srcdoc` |
| Math | Native MathML equations |

HTML must be UTF-8. Close nonvoid elements by default; `html_recovery: true`
accepts supported omitted closing tags and nonvoid trailing slashes. JavaScript,
event handlers and linked stylesheets are excluded. Supply CSS through `:stylesheets`.

Rendering checks the supported subset, not complete HTML validity.

### Visibility and composition

Supports initial disclosure states, hidden/inert content, declarative shadow
roots, slots and named template replacement. Scripts and later DOM updates do
not run. Hidden or replaced source content still requires valid, authorized resources.

### Forms

Text inputs, textareas, checkboxes, radios and supported selects become editable
fields. Buttons stay artwork; hidden inputs produce no output. Use `forms: :static`
for artwork only. Other input types have restrictions; mutable sliders and temporal
pickers are unsupported. See [PDF forms](pdf-forms.md) for names, values and filling.

Supported child-document controls remain editable but cannot repeat across pages
or page furniture. Unsupported widget transforms return diagnostics.

## CSS support

| Area | Supported |
| --- | --- |
| Selectors | Tags, classes, IDs, attributes, descendants/children, positional selectors, `:lang()`, simple `:not()` and initial control states |
| Styles | Cascade, inheritance, `!important`, custom properties, `var()` and `currentColor` |
| Layout | Block, inline, inline-block, flex, grid, sizing, margins, padding and gaps |
| Text | Fonts, alignment, spacing, wrapping, whitespace, list markers and solid text decorations |
| Boxes | Colors, borders, rounded corners and background images |
| Generated content | `::before`, `::after`, strings, `attr()` and counters |
| Printing | Print media queries, bare `@page` size/margins and page breaks |

Support is a subset of browser CSS. Common body lengths include `pt`, `px`,
`mm`, `cm`, `in`, `rem` and supported percentages.

Ordinary HTML text paints Unicode line/paragraph separators as spaces, matching Chromium.
Ordinary newlines follow `white-space`.

### Positioning and transforms

Static, relative and absolute positioning and 2D `transform` lists are supported.
Transforms move artwork without reflowing surrounding content. Fixed positioning
is limited to MathML owners and static popover artwork. Floats, 3D transforms,
perspective and animations are unsupported.

## Render options

Pass options to `HtmlToPdf.render/2` or `render_file/3`.

| Option | Default | Use |
| --- | --- | --- |
| `:page_size` | `:a4` | Named size, orientation tuple, dimension string or numeric tuple |
| `:margin` | `0` | Points, CSS string or side map |
| `:html_recovery` | `false` | Enable supported HTML recovery |
| `:stylesheets` | `[]` | List of `{:css, css}` or `{:file, path}` |
| `:base_url` | None | URL resolution base; a local directory also authorizes assets beneath it |
| `:assets` | `%{}` | Map references to `{:bytes, bytes}` or `{:file, path}` |
| `:asset_resolver` | None | Callback supplying approved asset bytes |
| `:print_viewport` | Page-derived | Positive `{width, height}` in CSS pixels |
| `:image_selection_density` | `1` | Positive responsive-image density |
| `:default_font` | `"DejaVu Sans"` | Family name or fallback list |
| `:fonts` | `[]` | Font definitions with `:family` and `:path` or `:data` |
| `:system_font_discovery` | `true` | Look up installed fonts |
| `:unsupported_glyphs` | `:replace` | Replace missing characters or use `:error` |
| `:compress_streams` | `true` | Compress eligible PDF streams |
| `:subset_fonts` | `true` | Embed only needed TrueType outlines when permitted |
| `:forms` | `:interactive` | Editable fields or `:static` artwork |
| `:metadata` | HTML title | Document information such as title and author |
| `:outlines` | Disabled | `:headings` or an explicit bookmark list |
| `:page_furniture` | Disabled | Header/footer templates and page-number tokens |

### Page size and margins

| Setting | Examples and limits |
| --- | --- |
| Page size | `:a4`, `:letter`, `{:a4, :landscape}` or `"100mm 60mm"` |
| Numeric sizes | Both values ≤ 20: inches; otherwise points |
| Margins | `36`, `"18mm 12mm"` or `%{top: 36, bottom: "12mm"}` |
| `@page` | Supported size and absolute margins only; explicit options take priority |
| Ignored page settings | Rotation, marks, bleed, decoration and unsupported size/margin values |

## Tables

| Feature | Support |
| --- | --- |
| Layout | Automatic/fixed sizing, nested tables, separate/collapsed borders |
| Spans | `colspan` and `rowspan`; rowspans stop at row-group boundaries |

### Table page breaks

| Content | Behavior |
| --- | --- |
| `<thead>` | Repeats in ordinary tables |
| `<tbody>` | `break-inside: avoid` keeps fitting groups together; larger groups split between rows |
| Individual rows | Never split; oversized rows may overflow |
| Oversized inline-block tables | Slice across pages without repeating headings |

[Grouped table example](html-to-pdf-examples.md#paginate-a-table-with-row-groups).

## Images and backgrounds

| Feature | Support |
| --- | --- |
| Formats | PNG, JPEG and rasterized SVG; [image restrictions](image-processing.md) |
| SVG text and links | Text becomes pixels; internal links are inactive |
| Image styling | `object-fit`, `object-position` |
| Backgrounds | Sizing, positioning and repetition |

### Assets and local files

| Setting | Behavior |
| --- | --- |
| Resolution order | `:assets` → authorized local files → `:asset_resolver`; no fallback on resource limits |
| Local files | Require a directory `:base_url`; `render_file/3` does not infer it |
| Path checks | Traversal and symlinks checked; use trusted directories or approved bytes |
| Explicit file paths | Caller-configured asset mappings, fonts and stylesheets are trusted |
| Other sources | Supported data URIs; remote resources are never fetched |
| Resolver request | `%{reference: reference, kind: kind}`; kind is `:image`, `:background_image` or `:font` |
| Resolver return | `{:ok, bytes}`, `:not_found` or `{:error, reason}` |

[Asset examples](html-to-pdf-examples.md#load-local-images).

## Fonts and text

| Setting | Support |
| --- | --- |
| Default | DejaVu Sans; installed-font discovery enabled |
| Consistent output | Supply fonts and set `system_font_discovery: false` |
| Custom fonts | Embeddable static TrueType; [registration example](html-to-pdf-examples.md#register-a-font) |
| Missing characters | Fallback fonts, then replacement; use `unsupported_glyphs: :error` to fail |

## MathML in HTML

| Feature | Support |
| --- | --- |
| Equations | Tokens, operators, fractions, radicals, scripts, limits, accents and matrices |
| Fonts | Bundled STIX Two Math; custom fonts need required MATH metrics |
| Page breaks | Equations stay together; oversized equations return a diagnostic |

[MathML examples](html-to-pdf-examples.md#mathml-equations).

## Pagination

| Feature | Support |
| --- | --- |
| Breaks | Automatic or supported CSS page breaks |
| Headers/footers | First/odd/even variants; reserve space in page margins |
| Page numbers | `{{page}}` and `{{pages}}` in templates |

[Header and footer examples](html-to-pdf-examples.md#add-headers-footers-and-page-numbers).

## Known limits

| Unsupported | Alternative |
| --- | --- |
| JavaScript, media playback, canvas drawing APIs | Static HTML and images |
| Embedded PDFs, XFA, JavaScript form actions | Supported HTML and PDF forms |
| Named pages, page-margin boxes, CSS page counters | Page options and header/footer templates |
| WOFF/WOFF2, variable fonts, CFF OpenType | Embeddable static TrueType fonts |
| Complex shaping, bidirectional text, vertical writing | A renderer supporting those scripts and layouts |

Embedded content and framesets have restricted static support. SVG, control
styling and some layouts can differ from browsers. Check your generated PDF.

[Resource limits](resource-limits.md) · [Diagnostics](diagnostics.md).
