# Supported browser rendering

The HTML renderer supports a document-oriented subset of browser rendering for
reports, invoices, statements, forms, and labels. Use the overview below to
choose a layout, then follow the linked reference for supported values and
restrictions.

## Supported behavior

| Area                                                                         | What you can use                                                                                              |
| ---------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------- |
| [HTML and text](html-to-pdf-compatibility.md#html-support)                    | Headings, paragraphs, inline emphasis, lists, links, character references, and supported form controls           |
| [CSS](html-to-pdf-compatibility.md#css-support)                               | Supported selectors, the cascade, custom properties, generated content, colors, borders, and spacing          |
| [Sizing and layout](html-to-pdf-compatibility.md#css-support)                 | Block and inline layout, flexbox, grid, size constraints, text wrapping, and relative or absolute positioning |
| [Tables](html-to-pdf-compatibility.md#tables)                                 | Column sizing, spanning cells, nested tables, repeated headers, and separate or collapsed borders             |
| [Images and backgrounds](html-to-pdf-compatibility.md#images-and-backgrounds) | JPEG, supported PNG formats, self-contained SVG, image fitting, and background sizing and repetition          |
| [Pages](html-to-pdf-compatibility.md#pagination)                              | Page sizes and margins, automatic and explicit breaks, running headers and footers, and page numbers          |
| [Fonts](html-to-pdf-compatibility.md#fonts-and-text)                          | Bundled DejaVu Sans, registered TrueType fonts, optional system-font discovery, and glyph fallback            |

Supported controls create interactive AcroForm fields by default; use
`forms: :static` for artwork only. See [PDF forms](pdf-forms.md).
This subset does not include JavaScript or full browser layout and typography. See [Known limits](html-to-pdf-compatibility.md#known-limits)
before adapting a browser template. Unsupported HTML tags and CSS properties
are reported through the [diagnostic contract](diagnostics.md). Some accepted
`@page` declarations are ignored, including `page-orientation`, `marks`, `bleed`,
and margins that cannot be normalized. See
[page declarations without an effect](html-to-pdf-compatibility.md#page-declarations-without-an-effect)
before relying on successful rendering as evidence that page settings were applied.

## Visual parity

We compare rendered documents with Chromium at 72 DPI. Every fixture enforces
a strict ceiling of **less than 1% differing pixels**. A pixel differs when at least one
RGB channel differs by more than 12 out of 255. Each fixture also checks the
page count and its existing average channel-delta limit.

Chromium and the native renderer load the same bundled DejaVu font files. Two
font fixtures also use the same bundled Liberation font files. Each run records
SHA-256 hashes of these files alongside its raster output. The quality matrix
checks the threshold on every supported Elixir runtime. Browser and rasterizer
updates can change the result, so failures must be investigated and corrected
when those tools change. The measured bound applies to these representative
fixtures within the supported HTML/CSS subset; arbitrary documents or different
font files may have a different result.

These percentages describe visual comparisons, not a percentage of browser
features supported. Start with the [rendering examples](html-to-pdf-examples.md)
for working templates.

The suite covers all supported PNG color types and sample depths, including
Adam7 interlacing, in the `png_formats` fixture. The table below includes every
current fixture, including invoices, statements, forms, and labels.

## Measured comparisons

The following run used Chromium 151.0.7922.173 and Poppler 25.03.0 at 72 DPI.
Values are fractions rounded to six decimal places. For multipage documents,
"Changed" and "Average" show the maximum page value. All 63 fixtures met the
strict `<0.01` changed-pixel gate; the largest value was `0.009673` for the
government application form. Each comparison writes its measurements to
`tmp/browser_parity/<fixture>/stats.exs`.

| Fixture | Changed | Average | Pages |
| --- | ---: | ---: | ---: |
| `absolute_positioning` | 0.000000 | 0.000000 | 1 |
| `background_images` | 0.005026 | 0.000923 | 1 |
| `block_box_model` | 0.001999 | 0.000473 | 1 |
| `border_style_variants` | 0.009581 | 0.002911 | 1 |
| `box_sizing_and_margins` | 0.001316 | 0.000219 | 1 |
| `break_variants` | 0.000640 | 0.000348 | 4 |
| `cmyk_jpeg_colors` | 0.000000 | 0.000000 | 1 |
| `computed_custom_properties` | 0.000000 | 0.000000 | 1 |
| `css_cascade_selectors` | 0.007879 | 0.002018 | 1 |
| `css_remaining_supported_values` | 0.001587 | 0.000795 | 1 |
| `display_lists_and_inline_block` | 0.006266 | 0.002542 | 1 |
| `distributed_gaps` | 0.001389 | 0.000926 | 1 |
| `empty_flex_height` | 0.000000 | 0.000000 | 1 |
| `flex_direction_and_justification` | 0.001673 | 0.000690 | 1 |
| `flex_grid_alignment` | 0.001112 | 0.000384 | 1 |
| `fonts_and_print_media` | 0.000287 | 0.000037 | 1 |
| `generated_content_counters` | 0.007572 | 0.002173 | 1 |
| `government_application_form` | 0.009673 | 0.003022 | 1 |
| `grid_explicit_sizes` | 0.000000 | 0.000000 | 1 |
| `grid_span_end` | 0.000000 | 0.000000 | 1 |
| `grid_sparse_placement` | 0.000000 | 0.000000 | 1 |
| `grid_tracks_and_placement` | 0.001919 | 0.000439 | 1 |
| `hidden_tables` | 0.000000 | 0.000000 | 1 |
| `html_semantics_typography` | 0.004044 | 0.002309 | 1 |
| `image_object_fitting` | 0.002047 | 0.000531 | 1 |
| `images_data_uris` | 0.004778 | 0.002689 | 1 |
| `inline_positioning` | 0.000157 | 0.000036 | 1 |
| `inline_text_flow` | 0.006012 | 0.002430 | 1 |
| `invoice_012` | 0.004505 | 0.000971 | 1 |
| `layout_compositions_remaining` | 0.002315 | 0.000925 | 1 |
| `links_entities_and_protocols` | 0.000988 | 0.000380 | 1 |
| `material_requisition` | 0.005162 | 0.001862 | 2 |
| `multi_page_report_012` | 0.007759 | 0.002245 | 2 |
| `nested_table_collapsed_borders` | 0.008050 | 0.003232 | 1 |
| `nested_table_grid_flex` | 0.006274 | 0.002049 | 1 |
| `page_furniture` | 0.003336 | 0.001532 | 2 |
| `page_geometry_asymmetric` | 0.000680 | 0.000343 | 1 |
| `page_rules_landscape` | 0.000928 | 0.000168 | 1 |
| `pagination_breaks` | 0.000120 | 0.000019 | 2 |
| `paragraph_pagination` | 0.000000 | 0.000000 | 2 |
| `png_formats` | 0.007718 | 0.002293 | 1 |
| `png_transparent_color` | 0.000000 | 0.000000 | 1 |
| `purchase_order` | 0.004020 | 0.001019 | 1 |
| `quoted_at_rules` | 0.000710 | 0.000135 | 1 |
| `quoted_variables` | 0.000000 | 0.000000 | 1 |
| `root_absolute_pagination` | 0.000000 | 0.000000 | 2 |
| `statement_012` | 0.005791 | 0.001390 | 1 |
| `static_form_controls` | 0.005775 | 0.001791 | 1 |
| `stock_sticker` | 0.002616 | 0.000380 | 1 |
| `system_font_inheritance` | 0.002356 | 0.000702 | 1 |
| `table_collapsed_borders` | 0.009329 | 0.003444 | 1 |
| `table_column_layout` | 0.002494 | 0.000774 | 1 |
| `table_header_near_page_row` | 0.001481 | 0.000260 | 2 |
| `table_pagination_headers` | 0.002727 | 0.001988 | 2 |
| `table_rowspan_tfoot` | 0.007336 | 0.002667 | 1 |
| `table_separate_borders` | 0.000475 | 0.000244 | 1 |
| `text_style_variants` | 0.006913 | 0.001900 | 1 |
| `trim_card` | 0.008870 | 0.001629 | 1 |
| `units_and_sizing` | 0.003377 | 0.001431 | 1 |
| `unsupported_glyph_replacement` | 0.001875 | 0.000860 | 1 |
| `weight_inheritance` | 0.000000 | 0.000000 | 1 |
| `whitespace_pre_line` | 0.000559 | 0.000102 | 1 |
| `zero_font_size` | 0.000000 | 0.000000 | 1 |
