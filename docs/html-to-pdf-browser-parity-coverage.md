# HTML to PDF browser parity coverage

The browser parity suite checks the renderer's documented visual behavior
against Chromium. It does not claim full browser compatibility. The supported
HTML and CSS subset is defined in
[HTML to PDF compatibility](html-to-pdf-compatibility.md).

## How comparison works

For each fixture, the suite:

1. Renders one PDF with Chromium and one with the native renderer.
2. Checks that both PDFs have the same page count.
3. Rasterizes each page with `pdftoppm`.
4. Compares changed pixels and average color-channel differences against the
   fixture's thresholds.

Thresholds live in
`test/html_to_pdf/browser_parity_test.exs`. Guard tests require a threshold for
every fixture. Failed comparisons write PDFs and rasterized pages to
`tmp/browser_parity/<fixture-name>/`.

Run the suite with:

```bash
CHROMIUM_BIN=/usr/bin/chromium mise exec -- mix test.browser_parity
```

Set `PDFTOPPM_BIN` when `pdftoppm` is not on `PATH`.

## Coverage

The suite has 41 focused fixtures and 8 production-document fixtures. All 49
comparisons currently pass their configured thresholds.

| Area | Focused fixtures |
| --- | --- |
| HTML and text flow | `html_semantics_typography.html`, `links_entities_and_protocols.html`, `inline_text_flow.html`, `whitespace_pre_line.html`, `static_form_controls.html`, `display_lists_and_inline_block.html` |
| Cascade, generated content, and box styling | `block_box_model.html`, `border_style_variants.html`, `box_sizing_and_margins.html`, `css_cascade_selectors.html`, `css_remaining_supported_values.html`, `generated_content_counters.html`, `text_style_variants.html`, `units_and_sizing.html` |
| Positioning | `absolute_positioning.html`, `inline_positioning.html`, `root_absolute_pagination.html` |
| Images and backgrounds | `images_data_uris.html`, `image_object_fitting.html`, `background_images.html` |
| Flexbox, grid, and mixed layouts | `flex_direction_and_justification.html`, `flex_grid_alignment.html`, `grid_tracks_and_placement.html`, `layout_compositions_remaining.html`, `nested_table_grid_flex.html` |
| Tables | `nested_table_collapsed_borders.html`, `table_collapsed_borders.html`, `table_column_layout.html`, `table_header_near_page_row.html`, `table_pagination_headers.html`, `table_rowspan_tfoot.html`, `table_separate_borders.html` |
| Pages, fonts, and glyph fallback | `break_variants.html`, `fonts_and_print_media.html`, `page_furniture.html`, `page_geometry_asymmetric.html`, `page_rules_landscape.html`, `pagination_breaks.html`, `paragraph_pagination.html`, `system_font_inheritance.html`, `unsupported_glyph_replacement.html` |

The production fixtures cover these document types:

| Fixtures | Document type |
| --- | --- |
| `government_application_form.html`, `purchase_order.html`, `material_requisition.html`, `invoice_012.html`, `statement_012.html`, `multi_page_report_012.html` | A4 forms, orders, invoices, statements, and multi-page reports |
| `stock_sticker.html` | Production-size stock label |
| `trim_card.html` | Production-size landscape trim card with nested tables and page breaks |

## Adding coverage

New renderer behavior needs a focused unit test. If it changes visible output:

1. Add or update a focused parity fixture.
2. Add its thresholds to `browser_parity_test.exs`.
3. Update the coverage table above.
4. Run the parity suite and inspect the rendered output.
