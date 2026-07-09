# HTML to PDF Browser Parity Coverage

This document tracks which documented HTML/CSS/layout features have Chromium visual parity fixtures. A fixture being present means the feature is exercised by the browser-parity harness; it does not mean the feature currently passes.

Run the parity suite with:

```bash
CHROMIUM_BIN=/usr/bin/chromium mise exec -- mix test.browser_parity
```

## Current Result

The browser-parity suite contains 24 synthetic HTML fixtures, 4 production-style Sigportal document fixtures, and guard tests that ensure every fixture has configured thresholds.

As of this audit, the suite is intentionally not green. The failing areas are:

| Fixture | Current signal |
| --- | --- |
| `css_cascade_selectors.html` | Failing because `var(--panel-pad)` in `padding` is rejected. |
| `block_box_model.html` | Failing, slightly above visual-diff threshold. |
| `flex_grid_alignment.html` | Failing, slightly above visual-diff threshold. |
| `html_semantics_typography.html` | Failing page-count parity; native layout overflows to two pages while Chromium stays on one. |
| `layout_compositions_remaining.html` | Failing with `:invalid_layout` for grid/flex containing tables and table containing direct flexbox. |
| `page_rules_landscape.html` | Failing page-count parity; native page sizing/pagination differs from Chromium. |
| `table_collapsed_borders.html` | Failing, collapsed border rendering diverges from Chromium. |
| `table_pagination_headers.html` | Failing page-count parity; native table pagination differs from Chromium. |
| `table_rowspan_tfoot.html` | Failing, rowspan/tfoot table rendering is above visual-diff threshold. |
| `table_separate_borders.html` | Failing, slightly above visual-diff threshold. |
| `nested_table_collapsed_borders.html` | Failing, nested collapsed borders diverge from Chromium. |
| `stock_sticker.html` | Failing average pixel-delta threshold under production label size `{4.92126, 1.49606}`. |

## Fixture Catalog

| Fixture | Coverage |
| --- | --- |
| `block_box_model.html` | margin, padding, width, min-height, side-specific borders, background, border radius, text alignment |
| `box_sizing_and_margins.html` | `box-sizing`, min/max width, percentage width, negative margins, clamped block sizing |
| `break_variants.html` | `break-before`, `break-after`, `page-break-before`, `page-break-inside`, auto and forced break values |
| `css_cascade_selectors.html` | universal, element, class, id, child, descendant, `:first-child`, `:last-child`, `:nth-child`, `!important`, custom properties, `display: none` |
| `css_remaining_supported_values.html` | grouped selectors, source order, inline style priority, inheritance, inline flex/grid, `min()`, named colors, no-op `overflow`/`position`, side padding/borders, `vertical-align`, `line-break`, `word-wrap` |
| `display_lists_and_inline_block.html` | inline-block layout, hidden elements, unordered lists, ordered lists, list item spacing |
| `flex_direction_and_justification.html` | flex row, row-reverse, column, grow/shrink/basis, `justify-content`, `align-items`, `align-self`, row gaps |
| `flex_grid_alignment.html` | flex order, wrapping, gap, alignment, grid template tracks, grid spans, row/column gaps |
| `grid_tracks_and_placement.html` | `repeat()`, `minmax()`, auto rows/columns, `grid-column`, `grid-row`, `grid-area`, item alignment |
| `html_semantics_typography.html` | semantic block aliases, metadata wrappers, `title`, `lang`, `h1`-`h6`, `b`, `i` |
| `images_data_uris.html` | PNG, JPEG, and SVG data URI images in block, table, flex, and grid contexts |
| `inline_text_flow.html` | inline runs, bold, italic, colors, line-height, wrapping, `<br>`, text transform |
| `layout_compositions_remaining.html` | grid containing table, flex containing table, table containing direct flexbox |
| `links_entities_and_protocols.html` | links, `https`, `http`, `mailto`, named entities, decimal and hex numeric entities |
| `nested_table_collapsed_borders.html` | collapsed outer table containing a collapsed inner table, nested `colspan`, side-specific nested borders |
| `nested_table_grid_flex.html` | table cell containing grid, grid item containing flexbox, ordering, gaps, nested borders |
| `page_rules_landscape.html` | CSS `@page` landscape sizing, page margins, explicit page-sized drawing geometry |
| `pagination_breaks.html` | explicit page breaks, repeated page-sized sections, page count parity |
| `table_collapsed_borders.html` | table captions, headers, collapsed borders, side-specific border precedence, `colspan`, missing trailing cells |
| `table_pagination_headers.html` | table overflow pagination, repeated header expectations, page breaks around table rows |
| `table_rowspan_tfoot.html` | `rowspan`, `tfoot`, vertical alignment, footer rows, `colspan` totals |
| `table_separate_borders.html` | separate borders, captions, headers, cell padding, `colspan`, right-aligned table content |
| `text_style_variants.html` | `rgb()`, `rgba()`, `currentColor`, transparent borders, white-space, word breaking, letter spacing, text transforms |
| `units_and_sizing.html` | `pt`, `px`, `mm`, `cm`, `in`, `rem`, percentages, `aspect-ratio`, fixed height, min-height |

## Production Fixture Catalog

These fixtures mirror documents currently used by Sigportal and use the same print sizes found in `/home/cees/Desktop/code/sig-portal`.

| Fixture | Production source | Browser parity setting |
| --- | --- | --- |
| `purchase_order.html` | `SigportalWeb.PurchaseOrderLive.Show.generate_purchase_order_printout/1` | A4, matching `Sigportal.EmailUtils.render_pdf(html_pages, :a4)` |
| `material_requisition.html` | `SigportalWeb.MaterialRequisitionLive.Show.generate_mr_printout/2` | A4, matching `Sigportal.EmailUtils.render_pdf(html_pages, :a4)` |
| `stock_sticker.html` | `Sigportal.Inventory.StockHelper` sticker rendering | `{4.92126, 1.49606}`, matching `Sigportal.EmailUtils.render_pdf(html, {4.92126, 1.49606})` |
| `trim_card.html` | `SigportalWeb.JobOrderLive.Show.generate_trim_card_printout/1` | `{11.6929, 8.2677}`, matching the A4 landscape size passed to `Sigportal.EmailUtils.render_pdf/2` |

Current production fixture parity:

| Fixture | Current signal |
| --- | --- |
| `purchase_order.html` | Passing current parity thresholds. |
| `material_requisition.html` | Passing current parity thresholds. |
| `trim_card.html` | Passing current parity thresholds. |
| `stock_sticker.html` | Failing average-delta threshold; artifacts are written under `tmp/browser_parity/stock_sticker/`. |

## HTML Coverage

| Feature | Status | Fixtures |
| --- | --- | --- |
| `doctype`, `html`, `head`, `body`, `style`, `meta`, `title` | Covered | All fixtures, `html_semantics_typography.html` |
| Block tags: `article`, `aside`, `div`, `footer`, `header`, `main`, `nav`, `section`, `p` | Covered | `html_semantics_typography.html`, suite-wide block fixtures |
| Headings `h1` through `h6` | Covered | `html_semantics_typography.html` |
| Inline tags: `span`, `strong`, `b`, `em`, `i`, `a`, `br` | Covered | `inline_text_flow.html`, `html_semantics_typography.html`, `links_entities_and_protocols.html` |
| HTML entities | Covered | `links_entities_and_protocols.html` |
| Lists: `ul`, `ol`, `li` | Covered | `display_lists_and_inline_block.html` |
| Tables: `table`, `caption`, `thead`, `tbody`, `tfoot`, `tr`, `th`, `td` | Covered | Table fixtures, especially `table_rowspan_tfoot.html` |
| Images: PNG, JPEG, SVG data URIs | Covered | `images_data_uris.html` |
| Attributes: `id`, `class`, `style`, `lang`, metadata attributes, `href`, `src`, `alt`, `colspan`, `rowspan` | Covered | `css_cascade_selectors.html`, `html_semantics_typography.html`, `links_entities_and_protocols.html`, `images_data_uris.html`, table fixtures |
| Link protocols: `https`, `http`, `mailto` | Covered | `links_entities_and_protocols.html` |

## CSS Coverage

| Feature | Status | Fixtures |
| --- | --- | --- |
| Selectors: universal, element, class, id, `element.class`, descendant, direct child, comma groups, `:root`, `:first-child`, `:last-child`, `:nth-child(n)` | Covered | `css_cascade_selectors.html`, `css_remaining_supported_values.html` |
| Cascade: specificity, source order, inline style priority, `!important`, inheritance, custom properties via `var()` | Covered / Failing | `css_cascade_selectors.html`, `css_remaining_supported_values.html`; custom property use in padding currently fails |
| Units: `pt`, `px`, `rem`, `mm`, `cm`, `in`, percentages, unitless `0` | Covered | `units_and_sizing.html`, reset rules across fixtures |
| Display: `block`, `inline`, `inline-block`, `none`, `flex`, `inline-flex`, `grid`, `inline-grid` | Covered | `display_lists_and_inline_block.html`, `css_remaining_supported_values.html`, flex/grid fixtures |
| Box model: width/height, min/max width/height, `min()`, `aspect-ratio`, `box-sizing`, margin, negative margin, padding, side-specific padding, borders, border radius, border collapse, background, overflow, position | Covered / Failing | `block_box_model.html`, `box_sizing_and_margins.html`, `css_remaining_supported_values.html`, table fixtures |
| Text: color, font family/size/weight/style, line height, text alignment, transform, vertical align, line breaking, word breaking, white-space, letter spacing | Covered | `inline_text_flow.html`, `text_style_variants.html`, `css_remaining_supported_values.html`, table fixtures |
| Colors: hex, named colors, `rgb()`, `rgba()`, `currentColor`, transparent | Covered | `text_style_variants.html`, `css_remaining_supported_values.html` |
| Page rules and breaks: `@page`, `break-before`, `break-after`, `page-break-before`, `page-break-after`, `page-break-inside` | Covered / Failing | `page_rules_landscape.html`, `pagination_breaks.html`, `break_variants.html` |
| Flexbox subset: direction, wrap, gap, row/column gap, justify/align, order, grow/shrink/basis, `flex`, inline flex | Covered / Failing | `flex_direction_and_justification.html`, `flex_grid_alignment.html`, `css_remaining_supported_values.html` |
| Grid subset: template rows/columns, auto rows/columns, `repeat()`, `minmax()`, placement, area, gaps, justify/align items/content/self, inline grid | Covered | `grid_tracks_and_placement.html`, `flex_grid_alignment.html`, `css_remaining_supported_values.html` |

## Layout Interaction Coverage

| Interaction | Status | Fixtures |
| --- | --- | --- |
| Block flow with nested inline text | Covered | `inline_text_flow.html`, `block_box_model.html` |
| Lists in normal flow | Covered | `display_lists_and_inline_block.html` |
| Table with collapsed borders | Covered / Failing | `table_collapsed_borders.html` |
| Table with separate borders | Covered / Failing | `table_separate_borders.html` |
| Table with `colspan`, `rowspan`, `tfoot`, and missing trailing cells | Covered / Failing | `table_collapsed_borders.html`, `table_rowspan_tfoot.html` |
| Table repeated headers and table pagination | Covered / Failing | `table_pagination_headers.html` |
| Nested table inside collapsed table | Covered / Failing | `nested_table_collapsed_borders.html` |
| Table cell containing grid containing flex | Covered | `nested_table_grid_flex.html` |
| Grid containing table, flex containing table, table containing direct flex | Covered / Failing | `layout_compositions_remaining.html` |
| Page breaks around blocks | Covered | `pagination_breaks.html`, `break_variants.html` |
| Page breaks inside/around tables | Covered / Failing | `table_pagination_headers.html` |
| Images inside block/table/flex/grid | Covered | `images_data_uris.html` |
| Embedded/system font metrics vs Chromium | Covered representative | All fixtures use `DejaVu Sans` when available via the test helper |

## Summary

Every documented HTML/CSS/layout support area now has at least one Chromium parity fixture. The suite is not a proof that the renderer is browser-accurate yet; it is now a release checklist that exposes where the implementation diverges.

Before making browser parity a required release gate, resolve the failing fixtures above or narrow the documented support matrix where the renderer intentionally does not match Chromium.
