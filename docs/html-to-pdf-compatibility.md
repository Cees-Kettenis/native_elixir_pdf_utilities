# HTML to PDF Compatibility

`NativeElixirPdfUtilities.HtmlToPdf` is a native document renderer for predictable server-side PDFs such as reports, invoices, labels, statements, and simple generated documents. It is not a browser engine and does not claim full browser compatibility.

For runnable templates, styling patterns, and caller-side error handling examples, see [HTML to PDF Examples](html-to-pdf-examples.md).

Unsupported or malformed input is rejected instead of being silently approximated. Rendering failures return a broad reason with diagnostic detail, for example:

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

The detail map always includes `:stage`, `:reason`, and `:message`. It includes `:line`, `:column`, and `:source` when the renderer can locate the source snippet. CSS is strict: unknown declarations and unsupported values fail with `:invalid_css` rather than being ignored.

## HtmlToPdf Options

| Option            | Supported values                                                                | Notes                                                                                                                                                                     |
| ----------------- | ------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `:page_size`    | `:a4`, `:letter`, or `{width, height}`                                    | Custom page sizes must be positive numbers. Tuples up to`20 x 20` are treated as inches for ChromicPDF-compatible label sizes; larger tuples are treated as PDF points. |
| `:margin`       | Number of points or CSS length string                                           | Examples:`24`, `"20mm"`, `"0.5in"`.                                                                                                                                 |
| `:base_url`     | Local path or`file://` URL                                                    | Used for relative image paths. Remote HTTP fetching is not supported.                                                                                                     |
| `:stylesheets`  | CSS strings or local CSS file paths                                             | Configured stylesheets load before embedded`<style>` tags.                                                                                                              |
| `:default_font` | Font family or fallback list                                                    | Defaults to`"Helvetica"`.                                                                                                                                               |
| `:fonts`        | `%{family: ..., path: ...}` maps, keyword lists, or `{family, path}` tuples | TTF only.`:weight` and `:style` are optional. System font discovery is intentionally not required.                                                                    |

## HTML Support Matrix

| Area              | Supported                                                                                                                                                           |
| ----------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Document wrappers | `doctype html`, `html`, `head`, `body`, `style`, `meta`, `title`                                                                                      |
| Blocks            | `article`, `aside`, `div`, `footer`, `header`, `main`, `nav`, `section`, `p`, `h1` through `h6`                                               |
| Inline text       | `span`, `strong`, `b`, `em`, `i`, `a`, `br`; common named and numeric HTML entities are decoded                                                       |
| Lists             | `ul`, `ol`, `li`                                                                                                                                              |
| Tables            | `table`, `caption`, `thead`, `tbody`, `tfoot`, `tr`, `th`, `td`                                                                                     |
| Images            | Strict`img` with required `src`                                                                                                                                 |
| Attributes        | `id`, `class`, `style`, `lang` on `html`, metadata attributes on `meta`, `href` on links, `src`/`alt` on images, `colspan`/`rowspan` on cells |
| Links             | `https://`, `http://`, and `mailto:` URI annotations                                                                                                          |

## CSS Support Matrix

| Area                  | Supported                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| --------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Selectors             | Universal`*`, element, `.class`, `#id`, `element.class`, descendant, direct child, comma groups, `:root`, `:first-child`, `:last-child`, integer `:nth-child(n)`                                                                                                                                                                                                                                                                                                                                                                                                                 |
| Cascade               | Specificity, source order, inline style priority,`!important`, inheritance for text styles, and CSS custom properties via `var(--name)`                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| Units                 | `pt`, `px`, `rem`, `mm`, `cm`, `in`, percentages for `width`/`height`/`min-height`, and unitless `0`                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| Display               | `block`, `inline`, `inline-block` as a block formatting box, `none`, `flex`, `inline-flex`, `grid`, `inline-grid`                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| Box model             | `width`, `height`, `min-width`, `min-height`, `max-width`, `max-height`, `min()`, `aspect-ratio`, `box-sizing`, `margin`, negative margins, `padding`, side-specific margin/padding, `border`, side-specific `border-*`, `border-width`, side-specific `border-*-width`, `border-color`, side-specific `border-*-color`, `border-style`, side-specific `border-*-style`, `border-radius`, `border-collapse`, `background`, `background-color`, accepted compatibility values for `overflow: visible/hidden`, no-op `position: static/relative` |
| Text                  | `color`, `font-family`, `font-size`, `font-weight`, `font-style`, `line-height`, `text-align`, `text-transform`, `vertical-align`, `line-break`, `word-break`, `word-wrap`, `overflow-wrap`, `white-space`, `letter-spacing`; hex colors, common named colors, `rgb()`, `rgba()`, `currentColor`, and transparent backgrounds are accepted, with alpha ignored for painted text and borders                                                                                                                                                                  |
| Page rules and breaks | Simple`@page` blocks are accepted, page options control size; `break-before`, `break-after`, `page-break-before`, `page-break-after` with `auto`, `page`, or `always`; `page-break-inside: auto/avoid` is accepted                                                                                                                                                                                                                                                                                                                                                             |
| Flexbox subset        | `flex-direction`, `flex-wrap`, `gap`, `row-gap`, `column-gap`, `justify-content`, `align-items`, `align-self`, `justify-self`, `order`, `flex-grow`, `flex-shrink`, `flex-basis`, `flex`                                                                                                                                                                                                                                                                                                                                                                             |
| Grid subset           | `grid-template-columns`, `grid-template-rows`, `grid-auto-columns`, `grid-auto-rows`, `repeat()`, `minmax()`, `grid-column`, `grid-column-start`, `grid-column-end`, `grid-row`, `grid-row-start`, `grid-row-end`, `grid-area`, `gap`, `row-gap`, `column-gap`, `justify-items`, `justify-self`, `align-items`, `justify-content`, `align-content`                                                                                                                                                                                                     |

## Layout Details

Block, list, table, flexbox, and grid layout are deterministic and intentionally narrower than browser layout. Tables use deterministic column sizing based on declared widths, available table width, and intrinsic unbreakable content, with support for collapsed borders, cell backgrounds, `colspan`, repeated headers, and missing trailing cells in shorter rows. Flexbox and grid support document-oriented text, images, and nested block-card items, not the full browser algorithms.

Pagination supports automatic page breaks, manual page breaks, page margins, basic keep-together behavior for emitted flow units, and repeated table headers when table bodies continue across pages.

Images support PNG, JPEG, and SVG data URIs, plus PNG/JPEG from absolute local paths and `base_url`-relative paths. SVG data URIs are rasterized to PNG with the lightweight `resvg` NIF using local in-process rendering; remote URLs and unsafe relative paths are rejected.

Fonts support built-in PDF fonts (`Helvetica`, `Courier`, `Times-Roman` and their bold/italic variants) plus explicit TTF embedding. Embedded fonts use TTF glyph widths, Type0/CID PDF resources, and basic Unicode mapping. Complex shaping for Arabic, Indic scripts, Thai, emoji sequences, and other advanced typography is not supported.

## Unsupported Features

These features are intentionally outside the current renderer boundary:

- JavaScript and runtime DOM behavior.
- `script`, `canvas`, `video`, `audio`, `iframe`, and interactive form behavior.
- Remote asset fetching.
- CSS floats, absolute/fixed positioning, transforms, animations, media queries, pseudo-elements, and pseudo-classes beyond the documented selector subset.
- Full browser-compatible table, flexbox, and grid algorithms.
- Complex text shaping and bidirectional layout.

## Validation Expectations

Automated tests cover parsing, CSS cascade, layout dimensions, pagination, PDF object output, images, fonts, links, and end-to-end rendering. Human visual validation is still required before accepting broad layout changes because PDF layout regressions can be visually obvious while remaining structurally valid.

Browser parity tests are available as an explicit, slower conformance suite. They render small HTML fixtures with Chromium, render the same fixtures with the native renderer, rasterize both PDFs with `pdftoppm`, and compare page pixels with a tolerance for font antialiasing.

For the current fixture-by-feature coverage audit, see [HTML to PDF Browser Parity Coverage](html-to-pdf-browser-parity-coverage.md).

The parity suite is excluded from normal `mix test` runs because it requires local browser tooling. Run it when changing CSS, layout, table, flexbox, grid, border, page sizing, font, image, or pagination behavior:

```bash
CHROMIUM_BIN=/usr/bin/chromium mise exec -- mix test.browser_parity
```

Set `PDFTOPPM_BIN` if `pdftoppm` is not on `PATH`.

New HTML-to-PDF features must include focused unit coverage in the relevant parser, style, layout, pagination, or PDF writer tests. If the feature changes visible rendering, add or update a browser parity fixture and keep the parity suite green before documenting the feature as supported.

Current parity fixtures cover:

| Fixture                                 | Coverage                                                                                                                                                                                                                          |
| --------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `block_box_model.html`                | margin, padding, width, min-height, side-specific borders, background, border radius, text alignment                                                                                                                              |
| `box_sizing_and_margins.html`         | `box-sizing`, min/max width, percentage width, negative margins, clamped block sizing                                                                                                                                           |
| `break_variants.html`                 | `break-before`, `break-after`, `page-break-before`, `page-break-inside`, auto and forced break values                                                                                                                     |
| `css_cascade_selectors.html`          | universal, element, class, id, child, descendant,`:first-child`, `:last-child`, `:nth-child`, `!important`, custom properties, `display: none`                                                                          |
| `css_remaining_supported_values.html` | grouped selectors, source order, inline style priority, inherited text styles, inline flex/grid,`min()`, named colors, no-op `overflow`/`position`, side padding/borders, `vertical-align`, `line-break`, `word-wrap` |
| `display_lists_and_inline_block.html` | inline-block layout, hidden elements, unordered lists, ordered lists, list item spacing                                                                                                                                           |
| `html_semantics_typography.html`      | semantic block aliases, metadata wrappers,`title`, `lang`, `h1`-`h6`, `b`, `i`                                                                                                                                        |
| `images_data_uris.html`               | PNG, JPEG, and SVG data URI images in block, table, flex, and grid contexts                                                                                                                                                       |
| `inline_text_flow.html`               | inline runs, bold, italic, colors, line-height, wrapping,`<br>`, text transform                                                                                                                                                 |
| `links_entities_and_protocols.html`   | links,`https`, `http`, `mailto`, named entities, decimal and hex numeric entities                                                                                                                                           |
| `text_style_variants.html`            | `rgb()`, `rgba()`, `currentColor`, transparent borders, white-space, word breaking, letter spacing, text transforms                                                                                                         |
| `units_and_sizing.html`               | `pt`, `px`, `mm`, `cm`, `in`, `rem`, percentages, `aspect-ratio`, fixed height, min-height                                                                                                                          |

Real production environment fixtures are also included:

| Fixture                                   | Production settings                                                                                             |
| ----------------------------------------- | --------------------------------------------------------------------------------------------------------------- |
| `purchase_order.html`                   | A4 production document size                                                                                   |
| `material_requisition.html`             | A4 production document size                                                                                   |
| `stock_sticker.html`                    | `{4.92126, 1.49606}` production label size                                                                    |
| `trim_card.html`                        | `{11.6929, 8.2677}` production landscape document size                                                        |
| `flex_grid_alignment.html`              | flex order, wrapping, gap, alignment, grid template tracks, grid spans, row/column gaps                         |
| `flex_direction_and_justification.html` | flex row, row-reverse, column, grow/shrink/basis,`justify-content`, `align-items`, `align-self`, row gaps |
| `grid_tracks_and_placement.html`        | `repeat()`, `minmax()`, auto rows/columns, `grid-column`, `grid-row`, `grid-area`, item alignment     |
| `layout_compositions_remaining.html`    | grid containing table, flex containing table, table containing direct flexbox                                   |
| `table_collapsed_borders.html`          | table captions, headers, collapsed borders, side-specific border precedence,`colspan`, missing trailing cells |
| `table_pagination_headers.html`         | table overflow pagination, repeated header expectations, page breaks around table rows                          |
| `table_rowspan_tfoot.html`              | `rowspan`, `tfoot`, vertical alignment, footer rows, `colspan` totals                                     |
| `table_separate_borders.html`           | separate borders, captions, headers, cell padding,`colspan`, right-aligned table content                      |
| `nested_table_grid_flex.html`           | table cell containing grid, grid item containing flexbox, ordering, gaps, nested borders                        |
| `nested_table_collapsed_borders.html`   | collapsed outer table containing a collapsed inner table, nested`colspan`, side-specific nested borders       |
| `page_rules_landscape.html`             | CSS`@page` landscape sizing, page margins, explicit page-sized drawing geometry                               |
| `pagination_breaks.html`                | explicit page breaks, repeated page-sized sections, page count parity                                           |

The artifact directory for a failing fixture is reported in the assertion message under `tmp/browser_parity/<fixture-name>/` and contains Chromium/native PDFs plus rasterized PPM pages for inspection.
