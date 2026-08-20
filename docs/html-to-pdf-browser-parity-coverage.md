# HTML to PDF browser parity coverage

This document tracks which documented HTML/CSS/layout features have Chromium visual parity fixtures. A fixture being present means the feature is exercised by the browser-parity harness and must stay within the configured visual-diff threshold.

Run the parity suite with:

```bash
CHROMIUM_BIN=/usr/bin/chromium mise exec -- mix test.browser_parity
```

## Current result

The browser-parity suite contains 38 synthetic HTML fixtures, 8 realistic document fixtures, and guard tests that ensure every fixture has configured thresholds. The changed-pixel limit is 5% for the general fixtures, 1% for the geometric absolute-positioning fixture, and 5.5% for the trim-card fixture where the remaining difference comes from subpixel text and border rasterization. Average channel-delta limits remain at or below 5%.

Current status: passing. The full suite passes with:

```bash
CHROMIUM_BIN=/usr/bin/chromium mise exec -- mix test.browser_parity
```

New HTML-to-PDF renderer features must add focused unit coverage and, when they affect visible output, a browser parity fixture or an update to an existing fixture. Do not mark new HTML/CSS/layout behavior as supported until the relevant unit tests and browser parity suite pass.

## Fixture catalog

| Fixture | Coverage |
| --- | --- |
| `absolute_positioning.html` | relative containing blocks, absolute offsets, percentages, nested positioning, negative and positive `z-index`, and removal from normal flow |
| `background_images.html` | data URI background images, repeat and no-repeat painting, explicit sizing, `cover`, and positioned backgrounds |
| `block_box_model.html` | margin, padding, width, min-height, side-specific borders, background, border radius, text alignment |
| `border_style_variants.html` | all ten standard border styles, one-to-four-value side styles and colors, and transparent borders |
| `box_sizing_and_margins.html` | `box-sizing`, min/max width, percentage width, negative margins, clamped block sizing |
| `break_variants.html` | `break-before`, `break-after`, `page-break-before`, `page-break-inside`, auto and forced break values |
| `css_cascade_selectors.html` | universal, element, class, id, child, descendant, `:first-child`, `:last-child`, `:nth-child`, `!important`, forward-referenced custom properties, `display: none` |
| `generated_content_counters.html` | attribute selectors, `:not()`, odd/even and type-position selectors, `::before`, `::after`, `attr()`, named counter reset/increment/output |
| `css_remaining_supported_values.html` | grouped selectors, source order, inline style priority, inheritance, inline flex/grid, `min()`, named colors, no-op `overflow`/`position`, side padding/borders, `vertical-align`, `line-break`, `word-wrap` |
| `display_lists_and_inline_block.html` | inline-block layout with inline and block contents, mixed inline/block flow, hidden elements, unordered lists, ordered lists, list item spacing |
| `fonts_and_print_media.html` | local CSS `@font-face`, print-only media cascade, embedded font output, bundled Unicode glyph fallback from a built-in font |
| `flex_direction_and_justification.html` | flex row, row-reverse, column, grow/shrink/basis, main-axis min/max freezing and redistribution, `justify-content`, `align-items`, `align-self`, row gaps |
| `flex_grid_alignment.html` | flex order, wrapping, gap, alignment, grid template tracks, grid spans, row/column gaps |
| `grid_tracks_and_placement.html` | `repeat()`, `minmax()` growth and minimum-bound overflow, auto rows/columns, `grid-column`, `grid-row`, `grid-area`, item alignment |
| `html_semantics_typography.html` | semantic block aliases, metadata wrappers, `title`, `lang`, `h1`-`h6`, `b`, `i`, font-relative semantic margins |
| `images_data_uris.html` | 8-bit non-interlaced RGB PNG, JPEG, and SVG data URI images in block, table, flex, and grid contexts |
| `image_object_fitting.html` | replaced-image sizing, `object-fit: contain/cover`, percentage object positions, clipping, and image borders |
| `inline_text_flow.html` | inline runs, bold, italic, colors, line-height, wrapping, `<br>`, text transform |
| `layout_compositions_remaining.html` | grid containing table, flex containing table, table containing direct flexbox |
| `links_entities_and_protocols.html` | links, `https`, `http`, `mailto`, named entities, decimal and hex numeric entities, non-breaking-space wrapping |
| `static_form_controls.html` | static text inputs, checked and unchecked checkbox/radio state, selected options, multiline textarea content, buttons, Boolean attributes, and CSS-targeted disabled appearance |
| `nested_table_collapsed_borders.html` | collapsed outer table containing a collapsed inner table, nested `colspan`, side-specific nested borders, and nested background/border paint order |
| `nested_table_grid_flex.html` | table cell containing grid, grid item containing flexbox, ordering, gaps, nested borders |
| `page_geometry_asymmetric.html` | named page sizing, landscape orientation, and asymmetric one-to-four-value `@page` margins |
| `page_rules_landscape.html` | CSS `@page` landscape sizing, page margins, explicit page-sized drawing geometry |
| `page_furniture.html` | repeated header and footer placement inside page margins across multiple pages, including visible current-page and total-page tokens |
| `paragraph_pagination.html` | default line-level paragraph fragmentation across automatic page breaks |
| `pagination_breaks.html` | explicit page breaks, repeated page-sized sections, page count parity |
| `table_collapsed_borders.html` | table captions, headers, collapsed borders, side-specific border precedence, `colspan`, missing trailing cells |
| `table_column_layout.html` | `colgroup`, `col`, column `span`, percentage column widths, lone cells remaining in their first declared column, `table-layout: fixed`, and separate/collapsed `border-spacing` behavior |
| `table_header_near_page_row.html` | a repeated table header kept with the first body row when both fit on a fresh page |
| `table_pagination_headers.html` | table overflow pagination, repeated multi-row header expectations, page breaks around table rows |
| `table_rowspan_tfoot.html` | `rowspan`, `tfoot`, vertical alignment, footer rows, `colspan` totals |
| `table_separate_borders.html` | separate borders, captions, headers, cell padding, `colspan`, right-aligned table content |
| `text_style_variants.html` | `rgb()`, `rgba()` text and backgrounds, transparent text and borders, `currentColor`, nested custom properties, inherited unitless line height, white-space, word breaking, letter spacing, text transforms |
| `unsupported_glyph_replacement.html` | browser-compatible continuation for unsupported text, including C1 controls and private-use glyphs, with deterministic U+FFFD replacement in native output |
| `units_and_sizing.html` | `pt`, `px`, `mm`, `cm`, `in`, root-font-relative `rem`, percentages, `aspect-ratio`, fixed height, min-height |
| `whitespace_pre_line.html` | default whitespace collapse, `white-space: pre-line`, `<br>`, literal escaped newline text, centered and right-aligned lines |

## Production fixture catalog

These fixtures mirror documents currently used in real production environments and use the same print sizes as those production render calls.

| Fixture | Production document type | Browser parity setting |
| --- | --- | --- |
| `government_application_form.html` | Government-style permit application with static form controls | A4 document size |
| `purchase_order.html` | Purchase order printout | A4 production document size |
| `material_requisition.html` | Material requisition printout | A4 production document size |
| `invoice_012.html` | Production-style invoice | A4 production document size |
| `statement_012.html` | Production-style account statement | A4 production document size |
| `multi_page_report_012.html` | Production-style multi-page report | A4 production document size |
| `stock_sticker.html` | Stock sticker label | `{4.92126, 1.49606}` production label size |
| `trim_card.html` | Trim card printout using the production table, column-group, full-height nested-table, and sheet-break structure | `{11.6929, 8.2677}` production landscape document size |

Current production fixture parity:

| Fixture | Current signal |
| --- | --- |
| `government_application_form.html` | Passing current parity thresholds. |
| `purchase_order.html` | Passing current parity thresholds. |
| `material_requisition.html` | Passing current parity thresholds. |
| `invoice_012.html` | Passing current parity thresholds. |
| `statement_012.html` | Passing current parity thresholds. |
| `multi_page_report_012.html` | Passing current parity thresholds. |
| `trim_card.html` | Passing current parity thresholds. |
| `stock_sticker.html` | Passing current parity thresholds. |

## HTML coverage

| Feature | Status | Fixtures |
| --- | --- | --- |
| `doctype`, `html`, `head`, `body`, `style`, `meta`, `title` | Passing | All fixtures, `html_semantics_typography.html` |
| Block tags: `article`, `aside`, `div`, `footer`, `header`, `main`, `nav`, `section`, `p` | Passing | `html_semantics_typography.html`, suite-wide block fixtures |
| Headings `h1` through `h6` | Passing | `html_semantics_typography.html` |
| Inline tags: `span`, `strong`, `b`, `em`, `i`, `a`, `br` | Passing | `inline_text_flow.html`, `html_semantics_typography.html`, `links_entities_and_protocols.html` |
| HTML entities | Passing | `links_entities_and_protocols.html` |
| Lists: `ul`, `ol`, `li` | Passing | `display_lists_and_inline_block.html` |
| Tables: `table`, `caption`, `colgroup`, `col`, `thead`, `tbody`, `tfoot`, `tr`, `th`, `td` | Passing | Table fixtures, especially `table_column_layout.html` and `table_rowspan_tfoot.html` |
| Images: 8-bit non-interlaced RGB/RGBA PNG, JPEG, SVG data URIs | Passing | `images_data_uris.html` |
| Static controls: text input, checkbox, radio, select/option, textarea, button | Passing | `static_form_controls.html`, `government_application_form.html` |
| Attributes: `id`, `class`, `style`, `title`, `role`, `data-*`, `aria-*`, `lang`, metadata attributes, `href`, `src`, `alt`, form `type`/`value`/`name`/`checked`/`selected`/`disabled`, column `span`, cell `colspan`/`rowspan`, and header `scope` | Passing | `generated_content_counters.html`, `css_cascade_selectors.html`, `html_semantics_typography.html`, `links_entities_and_protocols.html`, `images_data_uris.html`, `static_form_controls.html`, table fixtures |
| Link protocols: `https`, `http`, `mailto` | Passing | `links_entities_and_protocols.html` |

## CSS coverage

| Feature | Status | Fixtures |
| --- | --- | --- |
| Selectors: universal, element, class, id, element/class, attribute presence/equality, descendant, direct child, comma groups, `:not()`, `:root`, child position, type position, `::before`, `::after` | Passing | `generated_content_counters.html`, `css_cascade_selectors.html`, `css_remaining_supported_values.html` |
| Generated content and counters: quoted content, `attr()`, `counter()`, reset, increment | Passing | `generated_content_counters.html` |
| Cascade: specificity, source order, inline style priority, `!important`, inheritance, recursively resolved custom properties via `var()` | Passing | `css_cascade_selectors.html`, `css_remaining_supported_values.html`, `text_style_variants.html` |
| Units: `pt`, `px`, root-font-relative `rem`, `mm`, `cm`, `in`, percentages, unitless `0` | Passing | `units_and_sizing.html`, reset rules across fixtures |
| Display: `block`, `inline`, `inline-block`, `none`, `flex`, `inline-flex`, `grid`, `inline-grid` | Passing | `display_lists_and_inline_block.html`, `css_remaining_supported_values.html`, flex/grid fixtures |
| Box model and painting: width/height, min/max width/height, `min()`, `aspect-ratio`, `box-sizing`, margin, negative margin, padding, side-specific padding, borders, border radius, border collapse, border spacing, table layout, backgrounds, background images, and overflow | Passing | `block_box_model.html`, `box_sizing_and_margins.html`, `background_images.html`, `css_remaining_supported_values.html`, `table_column_layout.html`, table fixtures |
| Positioning: static, relative and absolute boxes, inset offsets, containing blocks, and `z-index` paint ordering | Passing | `absolute_positioning.html` |
| Text: color, font family/size/weight/style, relative and absolute line-height inheritance, left/center/right alignment, transform, vertical align, line breaking, word breaking, default and `pre-line` whitespace, letter spacing | Passing | `whitespace_pre_line.html`, `inline_text_flow.html`, `text_style_variants.html`, `css_remaining_supported_values.html`, table fixtures |
| Colors: hex, named colors, `rgb()`, `rgba()`, `currentColor`, transparent | Passing | `text_style_variants.html`, `css_remaining_supported_values.html` |
| Page rules and breaks: `@page`, `@media print`, `break-before`, `break-after`, `page-break-before`, `page-break-after`, `page-break-inside` | Passing | `page_geometry_asymmetric.html`, `page_rules_landscape.html`, `fonts_and_print_media.html`, `pagination_breaks.html`, `break_variants.html` |
| Flexbox subset: direction, wrap, gap, row/column gap, justify/align, order, grow/shrink/basis, main-axis min/max freezing and redistribution, `flex`, inline flex | Passing | `flex_direction_and_justification.html`, `flex_grid_alignment.html`, `css_remaining_supported_values.html` |
| Grid subset: template rows/columns, auto rows/columns, `repeat()`, `minmax()` growth and minimum-bound overflow, placement, area, gaps, justify/align items/content/self, inline grid | Passing | `grid_tracks_and_placement.html`, `flex_grid_alignment.html`, `css_remaining_supported_values.html` |

## Layout interaction coverage

| Interaction | Status | Fixtures |
| --- | --- | --- |
| Block flow with nested inline text | Passing | `inline_text_flow.html`, `block_box_model.html` |
| Lists in normal flow | Passing | `display_lists_and_inline_block.html` |
| Table with collapsed borders | Passing | `table_collapsed_borders.html` |
| Table with separate borders | Passing | `table_separate_borders.html` |
| Table column groups, fixed layout, border spacing, explicit table heights, and percentage-height nested tables | Passing | `table_column_layout.html`, `trim_card.html` |
| Table with `colspan`, `rowspan`, `tfoot`, and missing trailing cells | Passing | `table_collapsed_borders.html`, `table_rowspan_tfoot.html` |
| Table repeated headers and table pagination | Passing | `table_pagination_headers.html` |
| Nested table inside collapsed table | Passing | `nested_table_collapsed_borders.html` |
| Table cell containing grid containing flex | Passing | `nested_table_grid_flex.html` |
| Grid containing table, flex containing table, table containing direct flex | Passing | `layout_compositions_remaining.html` |
| Page breaks around complete blocks, including backgrounds and nested children | Passing | `pagination_breaks.html`, `break_variants.html`, `trim_card.html` |
| Page breaks inside/around tables | Passing | `table_pagination_headers.html` |
| Running headers, footers, and current/total page-number tokens inside page margins | Passing | `page_furniture.html` |
| Images inside block/table/flex/grid | Passing | `images_data_uris.html` |
| Replaced-image `contain` and `cover` fitting with clipping and object positioning | Passing | `image_object_fitting.html` |
| Repeated and fitted background images inside bordered boxes | Passing | `background_images.html` |
| Absolute descendants inside relative containing blocks with ordered painting | Passing | `absolute_positioning.html` |
| Embedded/system font metrics, Unicode fallback, and unsupported glyph replacement vs Chromium | Passing | `fonts_and_print_media.html` exercises both declared DejaVu Sans and built-in Helvetica text that falls back to bundled DejaVu glyphs; `unsupported_glyph_replacement.html` exercises visible continuation for unsupported graphemes; production fixtures use their declared font families when registered by the test helper |

## Summary

Every documented HTML/CSS/layout support area now has at least one Chromium parity fixture, and the current suite passes. The suite is still not a claim of full browser compatibility; it is a release checklist for the documented renderer surface. When expanding that surface, update this coverage map, add or update fixtures, and keep `mix test.browser_parity` green.
