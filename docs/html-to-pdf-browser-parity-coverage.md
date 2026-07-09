# HTML to PDF Browser Parity Coverage

This document tracks which documented HTML/CSS/layout features have Chromium visual parity fixtures. A fixture being present means the feature is exercised by the browser-parity harness and must stay within the configured visual-diff threshold.

Run the parity suite with:

```bash
CHROMIUM_BIN=/usr/bin/chromium mise exec -- mix test.browser_parity
```

## Current Result

The browser-parity suite contains 24 synthetic HTML fixtures, 4 real production environment document fixtures, and guard tests that ensure every fixture has configured thresholds.

Current status: passing. The full suite passes with:

```bash
CHROMIUM_BIN=/usr/bin/chromium mise exec -- mix test.browser_parity
```

New HTML-to-PDF renderer features must add focused unit coverage and, when they affect visible output, a browser parity fixture or an update to an existing fixture. Do not mark new HTML/CSS/layout behavior as supported until the relevant unit tests and browser parity suite pass.

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

These fixtures mirror documents currently used in real production environments and use the same print sizes as those production render calls.

| Fixture | Production document type | Browser parity setting |
| --- | --- | --- |
| `purchase_order.html` | Purchase order printout | A4 production document size |
| `material_requisition.html` | Material requisition printout | A4 production document size |
| `stock_sticker.html` | Stock sticker label | `{4.92126, 1.49606}` production label size |
| `trim_card.html` | Trim card printout | `{11.6929, 8.2677}` production landscape document size |

Current production fixture parity:

| Fixture | Current signal |
| --- | --- |
| `purchase_order.html` | Passing current parity thresholds. |
| `material_requisition.html` | Passing current parity thresholds. |
| `trim_card.html` | Passing current parity thresholds. |
| `stock_sticker.html` | Passing current parity thresholds. |

## HTML Coverage

| Feature | Status | Fixtures |
| --- | --- | --- |
| `doctype`, `html`, `head`, `body`, `style`, `meta`, `title` | Passing | All fixtures, `html_semantics_typography.html` |
| Block tags: `article`, `aside`, `div`, `footer`, `header`, `main`, `nav`, `section`, `p` | Passing | `html_semantics_typography.html`, suite-wide block fixtures |
| Headings `h1` through `h6` | Passing | `html_semantics_typography.html` |
| Inline tags: `span`, `strong`, `b`, `em`, `i`, `a`, `br` | Passing | `inline_text_flow.html`, `html_semantics_typography.html`, `links_entities_and_protocols.html` |
| HTML entities | Passing | `links_entities_and_protocols.html` |
| Lists: `ul`, `ol`, `li` | Passing | `display_lists_and_inline_block.html` |
| Tables: `table`, `caption`, `thead`, `tbody`, `tfoot`, `tr`, `th`, `td` | Passing | Table fixtures, especially `table_rowspan_tfoot.html` |
| Images: PNG, JPEG, SVG data URIs | Passing | `images_data_uris.html` |
| Attributes: `id`, `class`, `style`, `lang`, metadata attributes, `href`, `src`, `alt`, `colspan`, `rowspan` | Passing | `css_cascade_selectors.html`, `html_semantics_typography.html`, `links_entities_and_protocols.html`, `images_data_uris.html`, table fixtures |
| Link protocols: `https`, `http`, `mailto` | Passing | `links_entities_and_protocols.html` |

## CSS Coverage

| Feature | Status | Fixtures |
| --- | --- | --- |
| Selectors: universal, element, class, id, `element.class`, descendant, direct child, comma groups, `:root`, `:first-child`, `:last-child`, `:nth-child(n)` | Passing | `css_cascade_selectors.html`, `css_remaining_supported_values.html` |
| Cascade: specificity, source order, inline style priority, `!important`, inheritance, custom properties via `var()` | Passing | `css_cascade_selectors.html`, `css_remaining_supported_values.html` |
| Units: `pt`, `px`, `rem`, `mm`, `cm`, `in`, percentages, unitless `0` | Passing | `units_and_sizing.html`, reset rules across fixtures |
| Display: `block`, `inline`, `inline-block`, `none`, `flex`, `inline-flex`, `grid`, `inline-grid` | Passing | `display_lists_and_inline_block.html`, `css_remaining_supported_values.html`, flex/grid fixtures |
| Box model: width/height, min/max width/height, `min()`, `aspect-ratio`, `box-sizing`, margin, negative margin, padding, side-specific padding, borders, border radius, border collapse, background, overflow, position | Passing | `block_box_model.html`, `box_sizing_and_margins.html`, `css_remaining_supported_values.html`, table fixtures |
| Text: color, font family/size/weight/style, line height, text alignment, transform, vertical align, line breaking, word breaking, white-space, letter spacing | Passing | `inline_text_flow.html`, `text_style_variants.html`, `css_remaining_supported_values.html`, table fixtures |
| Colors: hex, named colors, `rgb()`, `rgba()`, `currentColor`, transparent | Passing | `text_style_variants.html`, `css_remaining_supported_values.html` |
| Page rules and breaks: `@page`, `break-before`, `break-after`, `page-break-before`, `page-break-after`, `page-break-inside` | Passing | `page_rules_landscape.html`, `pagination_breaks.html`, `break_variants.html` |
| Flexbox subset: direction, wrap, gap, row/column gap, justify/align, order, grow/shrink/basis, `flex`, inline flex | Passing | `flex_direction_and_justification.html`, `flex_grid_alignment.html`, `css_remaining_supported_values.html` |
| Grid subset: template rows/columns, auto rows/columns, `repeat()`, `minmax()`, placement, area, gaps, justify/align items/content/self, inline grid | Passing | `grid_tracks_and_placement.html`, `flex_grid_alignment.html`, `css_remaining_supported_values.html` |

## Layout Interaction Coverage

| Interaction | Status | Fixtures |
| --- | --- | --- |
| Block flow with nested inline text | Passing | `inline_text_flow.html`, `block_box_model.html` |
| Lists in normal flow | Passing | `display_lists_and_inline_block.html` |
| Table with collapsed borders | Passing | `table_collapsed_borders.html` |
| Table with separate borders | Passing | `table_separate_borders.html` |
| Table with `colspan`, `rowspan`, `tfoot`, and missing trailing cells | Passing | `table_collapsed_borders.html`, `table_rowspan_tfoot.html` |
| Table repeated headers and table pagination | Passing | `table_pagination_headers.html` |
| Nested table inside collapsed table | Passing | `nested_table_collapsed_borders.html` |
| Table cell containing grid containing flex | Passing | `nested_table_grid_flex.html` |
| Grid containing table, flex containing table, table containing direct flex | Passing | `layout_compositions_remaining.html` |
| Page breaks around blocks | Passing | `pagination_breaks.html`, `break_variants.html` |
| Page breaks inside/around tables | Passing | `table_pagination_headers.html` |
| Images inside block/table/flex/grid | Passing | `images_data_uris.html` |
| Embedded/system font metrics vs Chromium | Passing | Synthetic fixtures use `DejaVu Sans` when available; production fixtures use their declared font families when registered by the test helper |

## Summary

Every documented HTML/CSS/layout support area now has at least one Chromium parity fixture, and the current suite passes. The suite is still not a claim of full browser compatibility; it is a release checklist for the documented renderer surface. When expanding that surface, update this coverage map, add or update fixtures, and keep `mix test.browser_parity` green.
