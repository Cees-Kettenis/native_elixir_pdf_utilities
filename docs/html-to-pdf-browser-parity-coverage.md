# Supported browser rendering

The HTML renderer supports a document-oriented subset of browser rendering for
reports, invoices, statements, forms, and labels. Use the overview below to
choose a layout, then follow the linked reference for supported values and
restrictions.

## Supported behavior

| Area                                                                         | What you can use                                                                                              |
| ---------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------- |
| [HTML and text](html-to-pdf-compatibility.md#html-support)                    | Headings, paragraphs, inline emphasis, lists, links, character references, and static form controls           |
| [CSS](html-to-pdf-compatibility.md#css-support)                               | Supported selectors, the cascade, custom properties, generated content, colors, borders, and spacing          |
| [Sizing and layout](html-to-pdf-compatibility.md#css-support)                 | Block and inline layout, flexbox, grid, size constraints, text wrapping, and relative or absolute positioning |
| [Tables](html-to-pdf-compatibility.md#tables)                                 | Column sizing, spanning cells, nested tables, repeated headers, and separate or collapsed borders             |
| [Images and backgrounds](html-to-pdf-compatibility.md#images-and-backgrounds) | JPEG, supported PNG formats, self-contained SVG, image fitting, and background sizing and repetition          |
| [Pages](html-to-pdf-compatibility.md#pagination)                              | Page sizes and margins, automatic and explicit breaks, running headers and footers, and page numbers          |
| [Fonts](html-to-pdf-compatibility.md#fonts-and-text)                          | Bundled DejaVu Sans, registered TrueType fonts, optional system-font discovery, and glyph fallback            |

This subset does not include JavaScript, interactive PDF form fields, or full
browser layout and typography. See [Known limits](html-to-pdf-compatibility.md#known-limits)
before adapting a browser template. The renderer reports unsupported HTML and
CSS through the [diagnostic contract](diagnostics.md).

## Visual parity

We compare rendered documents with Chromium at 72 DPI. Every fixture enforces
a ceiling of **2% differing pixels** or less. A pixel differs when at least one
RGB channel differs by more than 12 out of 255. Each fixture also checks the
page count and its existing average channel-delta limit.

These percentages describe visual comparisons, not a percentage of browser
features supported. Start with the [rendering examples](html-to-pdf-examples.md)
for working templates.

## Measured comparisons

The table records the maximum changed-pixel ratio and maximum average channel
delta across each fixture's pages, before and after the rendering corrections.
Measurements used Chromium 152.0.7977.82 and Poppler 26.08.0. Values are fractions,
rounded to six decimal places. Average deltas use channel differences divided
by 255. Page counts match Chromium in both runs. Chromium and Poppler versions
can change rasterization, so the tests remain the source of truth for a local
build. Each comparison writes its measurements to
`tmp/browser_parity/<fixture>/stats.exs`.

| Fixture | Before changed | After changed | Before average | After average | Pages |
| --- | ---: | ---: | ---: | ---: | ---: |
| `absolute_positioning` | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 1 |
| `background_images` | 0.005026 | 0.005026 | 0.000924 | 0.000923 | 1 |
| `block_box_model` | 0.013326 | 0.003759 | 0.006966 | 0.001071 | 1 |
| `border_style_variants` | 0.040866 | 0.013835 | 0.015812 | 0.005687 | 1 |
| `box_sizing_and_margins` | 0.048978 | 0.004186 | 0.014912 | 0.002189 | 1 |
| `break_variants` | 0.014273 | 0.005261 | 0.008917 | 0.003360 | 4 |
| `cmyk_jpeg_colors` | 0.225000 | 0.000000 | 0.024118 | 0.000000 | 1 |
| `computed_custom_properties` | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 1 |
| `css_cascade_selectors` | 0.025316 | 0.009598 | 0.006620 | 0.002997 | 1 |
| `css_remaining_supported_values` | 0.047344 | 0.004182 | 0.013802 | 0.001858 | 1 |
| `display_lists_and_inline_block` | 0.020615 | 0.008614 | 0.009093 | 0.003884 | 1 |
| `distributed_gaps` | 0.008247 | 0.008247 | 0.005498 | 0.005498 | 1 |
| `empty_flex_height` | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 1 |
| `flex_direction_and_justification` | 0.039538 | 0.013691 | 0.017569 | 0.007099 | 1 |
| `flex_grid_alignment` | 0.022682 | 0.004863 | 0.009078 | 0.002474 | 1 |
| `fonts_and_print_media` | 0.010392 | 0.004778 | 0.003419 | 0.001776 | 1 |
| `generated_content_counters` | 0.021549 | 0.006030 | 0.007696 | 0.001746 | 1 |
| `government_application_form` | 0.033214 | 0.019625 | 0.011544 | 0.006463 | 1 |
| `grid_explicit_sizes` | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 1 |
| `grid_span_end` | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 1 |
| `grid_sparse_placement` | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 1 |
| `grid_tracks_and_placement` | 0.019325 | 0.004126 | 0.009635 | 0.002252 | 1 |
| `hidden_tables` | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 1 |
| `html_semantics_typography` | 0.047726 | 0.009228 | 0.023937 | 0.005865 | 1 |
| `image_object_fitting` | 0.005234 | 0.002049 | 0.002474 | 0.000532 | 1 |
| `images_data_uris` | 0.026862 | 0.006088 | 0.013614 | 0.003376 | 1 |
| `inline_positioning` | 0.000699 | 0.000157 | 0.000139 | 0.000036 | 1 |
| `inline_text_flow` | 0.010324 | 0.007043 | 0.003267 | 0.002320 | 1 |
| `invoice_012` | 0.044961 | 0.003449 | 0.012295 | 0.001212 | 1 |
| `layout_compositions_remaining` | 0.045748 | 0.018748 | 0.024946 | 0.009580 | 1 |
| `links_entities_and_protocols` | 0.018426 | 0.005234 | 0.010822 | 0.003469 | 1 |
| `material_requisition` | 0.039841 | 0.012483 | 0.010872 | 0.002522 | 2 |
| `multi_page_report_012` | 0.027334 | 0.007759 | 0.007918 | 0.002245 | 2 |
| `nested_table_collapsed_borders` | 0.025919 | 0.010732 | 0.014692 | 0.005843 | 1 |
| `nested_table_grid_flex` | 0.023053 | 0.011168 | 0.011224 | 0.005438 | 1 |
| `page_furniture` | 0.006598 | 0.005560 | 0.002504 | 0.002144 | 2 |
| `page_geometry_asymmetric` | 0.002973 | 0.000680 | 0.001405 | 0.000343 | 1 |
| `page_rules_landscape` | 0.000928 | 0.000928 | 0.000536 | 0.000536 | 1 |
| `pagination_breaks` | 0.004403 | 0.000126 | 0.002268 | 0.000020 | 2 |
| `paragraph_pagination` | 0.014722 | 0.000000 | 0.002449 | 0.000000 | 2 |
| `png_transparent_color` | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 1 |
| `purchase_order` | 0.044869 | 0.014795 | 0.013453 | 0.003071 | 1 |
| `quoted_at_rules` | 0.003071 | 0.000710 | 0.000562 | 0.000135 | 1 |
| `quoted_variables` | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 1 |
| `root_absolute_pagination` | 0.000000 | 0.000000 | 0.000000 | 0.000000 | 2 |
| `statement_012` | 0.033536 | 0.012086 | 0.011265 | 0.002704 | 1 |
| `static_form_controls` | 0.009466 | 0.008300 | 0.003197 | 0.003086 | 1 |
| `stock_sticker` | 0.044596 | 0.002616 | 0.007420 | 0.000380 | 1 |
| `system_font_inheritance` | 0.013080 | 0.010256 | 0.003986 | 0.003182 | 1 |
| `table_collapsed_borders` | 0.032956 | 0.010788 | 0.016993 | 0.004559 | 1 |
| `table_column_layout` | 0.042236 | 0.003707 | 0.024260 | 0.001972 | 1 |
| `table_header_near_page_row` | 0.035185 | 0.006620 | 0.015151 | 0.004810 | 2 |
| `table_pagination_headers` | 0.015746 | 0.001754 | 0.008630 | 0.001063 | 2 |
| `table_rowspan_tfoot` | 0.022139 | 0.006373 | 0.010748 | 0.002237 | 1 |
| `table_separate_borders` | 0.024107 | 0.005216 | 0.014058 | 0.002555 | 1 |
| `text_style_variants` | 0.041601 | 0.010272 | 0.018766 | 0.004529 | 1 |
| `trim_card` | 0.052570 | 0.015661 | 0.018627 | 0.003926 | 1 |
| `units_and_sizing` | 0.032484 | 0.015789 | 0.009215 | 0.003437 | 1 |
| `unsupported_glyph_replacement` | 0.012193 | 0.012026 | 0.006146 | 0.006101 | 1 |
| `weight_inheritance` | 0.045833 | 0.000000 | 0.018333 | 0.000000 | 1 |
| `whitespace_pre_line` | 0.024448 | 0.005610 | 0.008130 | 0.001844 | 1 |
| `zero_font_size` | 0.014896 | 0.000000 | 0.003094 | 0.000000 | 1 |
