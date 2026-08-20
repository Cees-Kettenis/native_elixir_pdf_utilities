# HTML to PDF compatibility

`NativeElixirPdfUtilities.HtmlToPdf` renders the documented HTML/CSS subset for
reports, invoices, labels, statements, and other server-generated documents.
It is not a browser engine and does not claim full browser compatibility.

For runnable templates, styling patterns, and error handling, see
[HTML to PDF examples](html-to-pdf-examples.md).

The renderer rejects malformed document structure and unsupported HTML/CSS
instead of guessing. By default, it replaces unsupported text graphemes with a
visible U+FFFD. Set `unsupported_glyphs: :error` to return a font-coverage
diagnostic instead. Rendering failures include a reason atom and diagnostic map:

```elixir
{:error,
 {:invalid_css,
  %{
    stage: :css,
    reason: :invalid_css,
    message: ~s(line 18: selector "li >" is invalid or unsupported),
    line: 18,
    column: 1,
    source: "li >"
  }}}
```

The map always includes `:stage`, `:reason`, and `:message`. It also includes
`:line`, `:column`, and `:source` when the renderer can locate the source.
Unknown declarations and unsupported CSS values return `:invalid_css`.

## `HtmlToPdf` options

| Option            | Supported values                                                                | Notes                                                                                                                                                                     |
| ----------------- | ------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `:page_size`    | Named size, oriented named size, CSS two-length string, or `{width, height}` | Named sizes are `:a5`, `:a4`, `:a3`, `:b5`, `:b4`, `:jis_b5`, `:jis_b4`, `:letter`, `:legal`, and `:ledger`; pair a name with `:portrait` or `:landscape`. Custom tuples up to `20 x 20` are treated as inches for compatibility; larger tuples are PDF points. |
| `:margin`       | Number, one-to-four-value CSS length string, or side map | Numbers are uniform PDF-point margins. CSS strings follow top/right/bottom/left shorthand rules. Maps accept `:top`, `:right`, `:bottom`, and `:left`; omitted sides are zero. |
| `:base_url`     | Local path or `file://` URL                                                   | Authorization root for document-selected image and embedded stylesheet font paths. Relative and absolute paths must remain beneath it; traversal and symlink components are rejected. Remote fetching is unsupported. |
| `:stylesheets`  | Tagged inline CSS and local files: `{:css, css}` or `{:file, path}`             | Configured stylesheets load before embedded `<style>` tags; bare strings are rejected so file access is always explicit.                                                |
| `:default_font` | Font family or fallback list                                                    | Defaults to `"Helvetica"`. Unsupported glyphs are resolved through configured fonts and the bundled DejaVu Sans faces before layout.                                                                                                    |
| `:fonts`        | `%{family: ..., path: ...}` maps, keyword lists, or `{family, path}` tuples | TrueType fonts. `:weight` and `:style` are optional. OpenType files using TrueType outlines share this path; CFF-flavored OTF is unsupported. Configured faces participate in automatic glyph fallback. |
| `:metadata`     | Keyword list or map                                                              | Supports `:title`, `:author`, `:subject`, `:keywords`, `:creation_date`, and `:modification_date`. Dates accept calendar structs or ISO 8601 strings. An HTML `<title>` is the default PDF title. |
| `:page_furniture` | Keyword list or map with `:header` and `:footer` | Opt-in running page furniture. Each position accepts HTML or `:default`, `:first`, `:odd`, and `:even` variants. Omitted, `nil`, and `false` furniture is disabled. |
| `:unsupported_glyphs` | `:replace` or `:error` | Defaults to `:replace`, which substitutes U+FFFD for each grapheme absent from every candidate font. `:error` returns the strict `:unsupported_glyph` diagnostic. |

## Running headers, footers, and page numbers

Running page furniture is an opt-in rendering option:

```elixir
HtmlToPdf.render(html,
  margin: "18mm",
  page_furniture: [
    header: [
      default: "<div>Account statement</div>",
      first: false,
      odd: "<div>Account statement</div>",
      even: "<div style=\"text-align: right\">Account statement</div>"
    ],
    footer: "<div style=\"text-align: right\">Page {{page}} of {{pages}}</div>"
  ]
)
```

Templates use the same supported HTML, CSS, configured stylesheets, local
images, and font registry as normal rendering. A plain text fragment is also
accepted. Main-document embedded `<style>` rules are not automatically copied
into a separate furniture template, so shared rules should be supplied through
`:stylesheets` or included in the furniture HTML.

Variant selection follows this order:

1. Use `:first` for page one when present.
2. Otherwise, use the matching `:odd` or `:even` variant when present.
3. Otherwise, use `:default`.

A `false` or `nil` variant renders nothing. First-page-only furniture uses
`[default: false, first: template]`; except-first-page furniture uses
`[default: template, first: false]`.

`{{page}}` expands to the current one-based page number and `{{pages}}` expands
to the final total page count. Substitution happens before each furniture
template is laid out.

The renderer places furniture inside the existing page margins without changing
body pagination. Headers must fit the top margin, and footers must fit the
bottom margin. If a template does not fit, rendering returns an
`:invalid_layout` diagnostic with the furniture position, measured height, and
available margin. Reserve enough `@page` or `:margin` space for both.

## HTML support matrix

| Area              | Supported                                                                                                                                                           |
| ----------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Document wrappers | `doctype html`, `html`, `head`, `body`, `style`, `meta`, `title`                                                                                      |
| Blocks            | `article`, `aside`, `div`, `footer`, `header`, `main`, `nav`, `section`, `p`, `h1` through `h6`                                               |
| Inline text       | `span`, `strong`, `b`, `em`, `i`, `a`, `br`; WHATWG named and numeric HTML character references are decoded once, including multi-code-point references and non-breaking spaces |
| Lists             | `ul`, `ol`, `li`                                                                                                                                              |
| Tables            | `table`, `caption`, `colgroup`, `col`, `thead`, `tbody`, `tfoot`, `tr`, `th`, `td`                                                                    |
| Images            | Strict `img` with required `src`                                                                                                                                 |
| Static forms      | `input` types `text`, `checkbox`, and `radio`; `select` with text-only `option` children; text-only `textarea`; and `button` with text and basic inline emphasis. Controls become ordinary visible PDF content, never interactive fields. |
| Attributes        | Global `id`, `class`, `style`, `title`, `role`, `data-*`, and `aria-*`; `lang` on `html`, metadata attributes on `meta`, `href` on links, `src`/`alt` on images, `span` on column elements, `colspan`/`rowspan` on cells, `scope` on `th`, and the documented form `type`, `value`, `name`, `checked`, `selected`, and `disabled` attributes |
| Links             | `https://`, `http://`, and `mailto:` URI annotations                                                                                                          |

## CSS support matrix

| Area                  | Supported                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| --------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Selectors             | Universal `*`, element, `.class`, `#id`, element/class combinations, `[attr]`, `[attr=value]`, descendant, direct child, comma groups, `:not(simple-selector)`, `:root`, `:first-child`, `:last-child`, integer/`odd`/`even` `:nth-child(...)`, `:first-of-type`, `:last-of-type`, `::before`, and `::after` |
| Generated content     | Quoted `content` fragments, `attr(name)`, named `counter(name)`, `counter-reset`, and `counter-increment`, including multiple named counters and explicit integer values |
| Cascade               | Specificity, source order, inline style priority, `!important`, inheritance for text styles, recursively resolved CSS custom properties via `var(--name)` after the custom-property cascade, cycle rejection, and dependent computed values such as `em` letter spacing and `currentColor` resolved against the element's final `font-size` and `color`                                                                                                                                                                                                                                             |
| Units                 | `pt`, `px`, root-font-relative `rem`, `mm`, `cm`, `in`, percentages for `width`/`height`/`min-height`, and unitless `0`                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| Display               | `block`, `inline`, atomic `inline-block`, `none`, `flex`, `inline-flex`, `grid`, `inline-grid`                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| Box model             | `width`, `height`, `min-width`, `min-height`, `max-width`, `max-height`, `min()`, `aspect-ratio`, `box-sizing`, `margin`, negative margins, `padding`, side-specific margin/padding, `border`, side-specific `border-*`, one-to-four-value `border-width`, side-specific `border-*-width`, one-to-four-value `border-color`, side-specific `border-*-color`, one-to-four-value `border-style` and side-specific `border-*-style` with `none`, `hidden`, `dotted`, `dashed`, `solid`, `double`, `groove`, `ridge`, `inset`, and `outset`, `border-radius`, `border-collapse`, one- and two-value `border-spacing`, `table-layout: auto/fixed`, `background`, `background-color`, `background-image`, `background-size`, `background-position`, `background-repeat`, and accepted compatibility values for `overflow: visible/hidden` |
| Positioning           | `position: static/relative/absolute`, `top`, `right`, `bottom`, `left`, percentage insets, relative containing blocks, absolute descendants removed from normal flow, and integer `z-index` paint ordering |
| Replaced images       | `object-fit: fill/contain/cover/none/scale-down` and one- or two-value `object-position` using lengths, percentages, or the documented position keywords |
| Text                  | `color`, including `rgba()`, eight-digit hex alpha, and `transparent`, `font-family`, `font-size`, `font-weight`, `font-style`, `line-height`, `text-align`, `text-transform`, `vertical-align`, `line-break`, `word-break`, `word-wrap`, `overflow-wrap`, `white-space: normal/pre-line`, and `letter-spacing`; `pre-line` normalizes CRLF/CR/LF and preserves line breaks, default whitespace collapses across inline boundaries, `<br>` remains explicit, and literal escaped newline sequences remain text; unitless and `normal` line heights retain relative inheritance while absolute line heights inherit as fixed lengths |
| Page rules and breaks | Bare `@page { ... }` rules apply accepted named sizes, portrait/landscape forms, explicit two-absolute-length sizes, one-to-four-value margins, and the four `margin-*` longhands with normal cascade order. Page selectors, named-page preludes, and misspelled `@page` at-rules fail with `:invalid_css` rather than being broadened to every page. Explicit renderer geometry overrides stylesheet defaults. Other recognized page-context declarations remain compatibility no-ops; malformed declarations, unknown properties, and invalid `size`, `margin*`, `page-orientation`, `marks`, or `bleed` values fail with `:invalid_css`. `@media print`, `@media only print`, and `@media all` participate in the print cascade while other media are skipped; page-break properties support the documented `auto`, `page`, `always`, and best-effort `avoid` values. |
| Fonts                 | Local `@font-face` with `font-family`, `src: url(...)`, optional `font-weight`, `font-style`, and `font-display`; TrueType and OpenType sources backed by TrueType outlines are supported; bundled DejaVu Sans regular, bold, oblique, and bold-oblique faces provide deterministic glyph fallback                                                                                                                                                                                                                                                                                               |
| Flexbox subset        | `flex-direction`, `flex-wrap`, `gap`, `row-gap`, `column-gap`, `justify-content`, `align-items`, `align-self`, `justify-self`, `order`, `flex-grow`, `flex-shrink`, `flex-basis`, `flex`, main-axis `min-width`/`max-width` and `min-height`/`max-height` freezing and redistribution                                                                                                                                                                                                                                                                                      |
| Grid subset           | `grid-template-columns`, `grid-template-rows`, `grid-auto-columns`, `grid-auto-rows`, `repeat()`, `minmax()` with absolute-length or `auto` minimums and absolute-length, `auto`, or `fr` maximums, minimum-bound overflow, `grid-column`, `grid-column-start`, `grid-column-end`, `grid-row`, `grid-row-start`, `grid-row-end`, `grid-area`, `gap`, `row-gap`, `column-gap`, `justify-items`, `justify-self`, `align-items`, `justify-content`, `align-content`                                                                                                    |

## Layout details

The renderer implements the documented block, list, table, flexbox, and grid
subset, not the full browser algorithms.

Tables calculate column widths from declared widths, available table width, and
intrinsic unbreakable content. Ordered `colgroup` and `col` elements with
positive `span` attributes may supply column hints. With `table-layout: fixed`,
the renderer uses those hints and first-row cell widths instead of intrinsic
content from later rows. A short row leaves its trailing declared columns
empty. Only an explicit `colspan` expands a cell across those columns.

Separate-border tables honor one- or two-value `border-spacing`; collapsed
borders ignore it. Cells default to transparent backgrounds, no border, and
one CSS pixel of padding. Declare visible borders and backgrounds in CSS.
Explicit table heights distribute remaining height across rows. Percentage
row heights receive the available remainder, and percentage-height nested
tables resolve against their containing cell. Tables also support cell
backgrounds, `colspan`, `rowspan`, repeated multi-row headers, nested
collapsed-border paint order, and missing trailing cells. Flexbox and grid
support text, images, and nested block cards within the documented subset.

Static form controls participate in block, table, flex, and grid layout. Text
inputs show `value`. Checkbox and radio states follow the presence of
`checked`. Selects show the selected option, or the first option when none is
selected. Textarea and button child content take precedence over each
control's `value`. A select may contain at most one selected option.

Boolean `checked`, `selected`, and `disabled` attributes accept valueless HTML
syntax and quoted values. The renderer gives `disabled` no built-in appearance,
but CSS can target it with selectors such as `[disabled]`.

These controls produce only text and drawing operations. They do not create PDF
AcroForm fields, widget annotations, focus behavior, submission behavior, or
editable values.

Pagination supports automatic breaks, manual breaks around complete block boxes
and their children, page margins, line-level paragraph fragmentation, and
repeated table headers. It tries to keep `break-inside: avoid` content together.
A paragraph taller than the printable page still fragments so no text is lost.

Images support 8-bit, non-interlaced RGB and RGBA PNGs, JPEGs, and SVG data URIs.
Paths authorized beneath `:base_url` support the same PNG subset and JPEGs.
Replaced images honor explicit box dimensions, `object-fit`, and
`object-position`. Background images support explicit sizes, `cover`,
`contain`, repeat modes, and positioned painting inside the element box.

Relative boxes remain in normal flow and establish containing blocks for
absolute descendants. Absolute boxes leave normal flow, resolve their inset
offsets against the nearest positioned ancestor or page content box, and paint
in integer `z-index` order. Fixed positioning is not supported.

The `resvg` NIF rasterizes SVG data URIs to PNG in the local process. SVG input
is limited to 5 MB, 8,192 pixels per axis, and 16,777,216 total raster pixels.
Excess input returns `:resource_limit_exceeded` before native raster allocation.
Remote URLs, traversal, and symlink components are rejected. Greyscale,
indexed-color, 16-bit, and Adam7-interlaced PNG decoding is scheduled for
`0.21.0`.

Fonts include the built-in PDF families `Helvetica`, `Courier`, and
`Times-Roman`, with their bold and italic variants. The renderer also accepts
explicit font options and local CSS `@font-face` declarations. Bundled DejaVu
Sans regular, bold, oblique, and bold-oblique faces handle glyph fallback.

Document-selected `@font-face` URLs must remain beneath `:base_url`.
Configured stylesheet files resolve their own trusted relative font paths.
The renderer rejects HTTP(S), data URLs, traversal, symlink components,
`local(...)`-only sources, WOFF, WOFF2, and CFF-flavored OpenType fonts. Convert
unsupported web fonts to TTF before rendering.

The renderer resolves font fallback before layout, so wrapping and pagination
use the final glyph metrics. For each Unicode grapheme, it tries the selected
CSS face, the remaining requested families, configured faces in declaration
order, and then the closest bundled DejaVu Sans weight and style. Adjacent
graphemes that use the same face remain one text run.

Invalid UTF-8 returns `:invalid_encoding`. By default, the renderer replaces a
grapheme missing from every candidate with U+FFFD in an available fallback
face. Set `unsupported_glyphs: :error` to return `:unsupported_glyph` with the
original grapheme and codepoints.

Embedded fonts use TrueType glyph widths, Type0/CID PDF resources, and basic
Unicode mapping. Glyph availability does not imply complex shaping. Arabic,
Indic scripts, Thai, emoji sequences, bidirectional layout, and other advanced
typography remain unsupported.

## Unsupported features

The renderer does not support:

- JavaScript and runtime DOM behavior.
- `script`, `canvas`, `video`, `audio`, `iframe`, interactive form behavior, and
  input types other than `text`, `checkbox`, and `radio`.
- Remote asset fetching.
- CSS floats, fixed positioning, transforms, animations, media queries beyond
  the documented print subset, pseudo-elements beyond `::before` and
  `::after`, and pseudo-classes beyond the documented selector subset.
  Repeated page furniture uses the `:page_furniture` option.
- Nested `counters(...)`, counter styles, list-marker counter integration, and
  CSS `counter(page)` or `counter(pages)`. Repeated page numbering uses the
  page-furniture `{{page}}` and `{{pages}}` tokens.
- Full browser-compatible table, flexbox, and grid algorithms.
- Complex text shaping and bidirectional layout.

## Validation expectations

Automated tests cover parsing, CSS cascade, layout dimensions, pagination, PDF
objects, images, fonts, links, and complete renders. Review rendered pages when
changing layout. A PDF can remain structurally valid while its layout is visibly
wrong.

The slower browser parity suite renders each fixture with Chromium and the
native renderer. It rasterizes both PDFs with `pdftoppm`, then compares page
pixels with a tolerance for font antialiasing.

For the fixture-by-feature audit, see
[HTML to PDF browser parity coverage](html-to-pdf-browser-parity-coverage.md).

The parity suite is excluded from normal `mix test` runs because it requires
local browser tooling. Run it when changing CSS, layout, tables, flexbox, grid,
borders, page sizing, fonts, images, or pagination:

```bash
CHROMIUM_BIN=/usr/bin/chromium mise exec -- mix test.browser_parity
```

Set `PDFTOPPM_BIN` if `pdftoppm` is not on `PATH`.

New HTML-to-PDF behavior needs focused unit coverage in the relevant parser,
style, layout, pagination, or PDF writer tests. If it changes visible output,
add or update a browser parity fixture before documenting support.

The synthetic and production fixture catalogs are in
[HTML to PDF browser parity coverage](html-to-pdf-browser-parity-coverage.md).
That guide maps each documented support area to a Chromium fixture. A failed
comparison writes Chromium and native PDFs, plus rasterized PPM pages, under
`tmp/browser_parity/<fixture-name>/`.
