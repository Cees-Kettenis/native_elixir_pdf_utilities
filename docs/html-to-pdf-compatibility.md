# HTML and CSS support

Use the renderer for reports, invoices, statements, forms, and labels. Start
with [a working example](html-to-pdf-examples.md#render-html), then choose the
features you need below. Unsupported HTML or CSS returns an
[error](diagnostics.md) rather than being silently ignored.

## HTML support

| Content | Supported elements |
| --- | --- |
| Document | `html`, `head`, `body`, `title`, `meta`, `style`, and an HTML doctype |
| Headings and sections | `h1` through `h6`, `p`, `div`, `section`, `article`, `aside`, `header`, `footer`, `main`, `nav` |
| Inline text | `span`, `strong`, `b`, `em`, `i`, `a`, `br`, and character references such as `&amp;` |
| Lists | `ul`, `ol`, `li` |
| Tables | `table`, `caption`, `colgroup`, `col`, `thead`, `tbody`, `tfoot`, `tr`, `th`, `td` |
| Images | `img` with a `src` |
| Static controls | Text, checkbox, and radio `input`; `select`, `option`, `textarea`, `button` |

Quote attribute values and close non-void tags. Put inline elements and form
controls inside a block such as `div` or `p`. They cannot sit directly under
`body` or at the document root. Controls show their values but are not editable
PDF fields. Links support `http://`, `https://`, and `mailto:` destinations.

Use `id`, `class`, and `style` to apply CSS. Supported elements also accept
relevant image, link, table-span, and form-value attributes.

## CSS support

| I need | Supported CSS |
| --- | --- |
| Select elements | Tag, class, ID, universal, attribute presence/equality, descendant and child selectors; comma groups; simple `:not()`; `:root`; first/last child and type selectors; `:nth-child(odd/even/n)` |
| Reuse styles | The cascade, `!important`, inherited text styles, custom properties, `var()`, `currentColor` |
| Arrange content | Block, inline, inline-block, flex, and grid layouts; relative and absolute positioning; `z-index` |
| Set sizes and spacing | Width/height, min/max sizes, `min()`, aspect ratio, box sizing, margins, padding, gaps |
| Style text | Font family/size/weight/style, line height, alignment, text transform, letter spacing, line and word breaking, `white-space: normal` or `pre-line` |
| Decorate boxes | Colors, borders, rounded corners, background images, sizing, positioning, and repetition |
| Generate text | `::before`, `::after`, quoted `content`, `attr()`, `counter()`, counter reset and increment |
| Control pages | Bare `@page` size/margins, print media rules, page breaks, and best-effort `break-inside: avoid` |

Body lengths support `pt`, `px`, `mm`, `cm`, `in`, `rem`, supported percentages,
and unitless `0`. Letter spacing also accepts `em`. Flex and grid support gaps,
alignment, ordering, and track/item sizing, but not every browser layout rule.

## Render options

Pass options to `HtmlToPdf.render(html, options)` or `render_file/3`.

| Option | Use it to | Default | Supported values |
| --- | --- | --- | --- |
| `:page_size` | Set page dimensions | `:a4` | Named size, orientation tuple, size string, or numeric tuple. See [page sizes](#page-size-and-margins). |
| `:margin` | Set page margins | `0` | Nonnegative point number, CSS string, or side map. See [margin formats](#page-size-and-margins). |
| `:stylesheets` | Add CSS | `[]` | List of `{:css, css}` or `{:file, path}` entries |
| `:base_url` | Allow local images/fonts | None | Directory path such as `"priv/static"`. See [asset sources](#assets-and-local-files). |
| `:assets` | Map asset references | `%{}` | Map of references to `{:bytes, binary}` or `{:file, path}` |
| `:asset_resolver` | Resolve asset references | None | One-argument function or `nil`. See the [callback contract](#assets-and-local-files). |
| `:default_font` | Choose text fonts | `"DejaVu Sans"` | Family name string or a list of family names in fallback order |
| `:fonts` | Register TrueType fonts | `[]` | List of font definitions with `:family` and `:path` or `:data`. See [font registration](html-to-pdf-examples.md#register-a-font). |
| `:system_font_discovery` | Look up installed fonts | `true` | `true` or `false` |
| `:unsupported_glyphs` | Handle missing characters | `:replace` | `:replace` or `:error` |
| `:metadata` | Set document information | HTML title when present | Metadata fields such as `:title` and `:author`. See [metadata options](html-to-pdf-examples.md#set-pdf-metadata). |
| `:outlines` | Add PDF bookmarks | Disabled | `:headings`, bookmark list, `false`, or `nil`. See [bookmark examples](html-to-pdf-examples.md#create-bookmarks-from-headings). |
| `:page_furniture` | Add headers and footers | Disabled | Keyword list or map of templates. See [template options](html-to-pdf-examples.md#add-headers-footers-and-page-numbers). |

### Page size and margins

| Page-size format | Supported values or example |
| --- | --- |
| Named size | `:a5`, `:a4`, `:a3`, `:b5`, `:b4`, `:jis_b5`, `:jis_b4`, `:letter`, `:legal`, `:ledger` |
| Named size with orientation | `{:a4, :landscape}` or `{:a4, :portrait}` |
| Size string | `"a4 landscape"` or custom dimensions such as `"100mm 60mm"` |
| Numeric `{width, height}` | `{8.5, 11}` uses inches; `{612, 792}` uses points. Both values use inches when both are at most 20; otherwise both use points. |

Prefer size strings with explicit units for custom dimensions.

| Margin format | Example | Meaning |
| --- | --- | --- |
| Point number | `36` | 36 points on every side |
| CSS string with one value | `"12mm"` | Same margin on every side |
| CSS string with two values | `"18mm 12mm"` | Top/bottom, then left/right |
| CSS string with three values | `"18mm 12mm 10mm"` | Top, left/right, bottom |
| CSS string with four values | `"18mm 12mm 10mm 8mm"` | Top, right, bottom, left |
| Side map | `%{top: 36, bottom: "12mm"}` | Set individual sides; omitted sides are zero |

Page-size and margin strings accept `pt`, `px`, `mm`, `cm`, `in`, `q`, and `pc`.
Explicit `:page_size` and `:margin` options override `@page`.

## Tables

| Feature | Support and limits |
| --- | --- |
| Column sizing | Automatic sizing or `table-layout: fixed` |
| Spanning cells | `colspan` and `rowspan`; a `rowspan` ends at its row-group boundary |
| Nested tables | Supported inside cells |
| Borders | Separate or collapsed borders |
| Headers across pages | Table headers repeat on subsequent pages |
| Keeping content together | `break-inside: avoid` is best effort; content taller than a page can split |

See the [table example](html-to-pdf-examples.md#add-a-styled-table).

## Images and backgrounds

| Format | Supported content |
| --- | --- |
| JPEG | Grayscale, RGB, and CMYK |
| PNG | 8-bit, non-interlaced RGB/RGBA; transparency is supported |
| SVG | Self-contained SVG that does not load other images or external resources |

| Image styling | Supported values or behavior |
| --- | --- |
| `object-fit` | `fill`, `contain`, or `cover` |
| `object-position` | Position an image within its box |
| `background-size` | Explicit sizes, `cover`, or `contain` |
| `background-position` | Position a background within its box |
| `background-repeat` | `repeat`, `repeat-x`, `repeat-y`, or `no-repeat` |

### Assets and local files

| Source | How to supply it | Rules |
| --- | --- | --- |
| Local directory | `base_url: "priv/static"` with `src="images/logo.png"` | Paths must stay beneath the directory and cannot contain symlinks. `render_file/3` does not infer this directory. |
| Explicit asset map | `assets: %{"logo.png" => {:bytes, bytes}}` or a `{:file, path}` value | Mappings take precedence; mapped files do not require `:base_url`. |
| Resolver callback | `asset_resolver: fn request -> ... end` | Used for references that are not authorized local paths. An unreadable authorized file returns an error. |
| Data URI | Put a supported data URI in the image reference | Image format restrictions still apply. |

The resolver receives `%{reference: reference, kind: kind}`, where `kind` is
`:image`, `:background_image`, or `:font`.

| Resolver return | Meaning |
| --- | --- |
| `{:ok, bytes}` | Use the supplied asset bytes |
| `:not_found` | The resolver could not find the asset |
| `{:error, reason}` | Asset resolution failed |

The renderer never downloads remote assets itself. Caller-configured font
paths and stylesheet files do not require `:base_url`.
See [image and asset examples](html-to-pdf-examples.md#load-local-images).

## Fonts and text

Choose a font with CSS `font-family` or the `:default_font` render option.

| Font source | How to use it |
| --- | --- |
| Bundled | DejaVu Sans is included and used by default. |
| Installed on the rendering machine | System-font discovery is enabled by default. Use an installed family by name, such as `font-family: "Liberation Sans"` or `default_font: "Liberation Sans"`, if that family is installed. No `:fonts` registration is needed. |
| Supplied by your application | Register a static TrueType font with `:family` and either `:path` or `:data`. Optional `:weight` and `:style` default to `400` and `:normal`. |

Installed fonts must meet the same [font-format and embedding requirements](#known-limits)
as supplied fonts. For consistent font selection across machines, supply your
fonts and set `system_font_discovery: false`.
See [Register a font](html-to-pdf-examples.md#register-a-font).

Missing characters use available fallback fonts, then U+FFFD replacement.
Set `unsupported_glyphs: :error` to fail instead of replacing them.

## Pagination

Pages break automatically or at supported CSS page breaks. Use
`:page_furniture` for running headers, footers, and page numbers. Reserve enough
margin for the templates; they do not push body content out of the way.

Templates can vary on the first, odd, and even pages. A matching `false` or
`nil` variant hides that template. See [headers and footers](html-to-pdf-examples.md#add-headers-footers-and-page-numbers).
For PDF viewer bookmarks, see [outlines from headings](html-to-pdf-examples.md#create-bookmarks-from-headings).

## Known limits

| Not supported | What to use instead |
| --- | --- |
| JavaScript, canvas, video, audio, iframe | Static HTML and supported images |
| Interactive form fields | Render the form's values as static controls |
| Floats, fixed positioning, transforms, animations | Block, flex, grid, relative, or absolute layout |
| Named pages and CSS page-margin boxes | Page options and header/footer templates |
| CSS `counter(page)` or `counter(pages)` | `{{page}}` and `{{pages}}` in header/footer templates |
| Indexed, grayscale, 16-bit, or interlaced PNG | RGB/RGBA PNG or JPEG |
| WOFF/WOFF2, variable fonts, CFF OpenType | Static TrueType fonts that permit embedding |
| Complex shaping or bidirectional text | A renderer with shaping support for Arabic, Indic scripts, Thai, and complex emoji sequences |

See [Resource limits](resource-limits.md) for configurable size limits and
[Browser rendering](html-to-pdf-browser-parity-coverage.md) for visual tolerances.
