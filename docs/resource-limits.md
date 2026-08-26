# Configurable resource limits

Native Elixir PDF Utilities limits the resources used to process untrusted PDF,
HTML, CSS, image, SVG, and font data. The built-in values apply unless the
application overrides them.

Configure limits under the library's OTP application in `config/config.exs` or
`config/runtime.exs`:

```elixir
config :native_elixir_pdf_utilities,
  limits: [
    max_svg_bytes: 10_000_000,
    max_svg_raster_dimension: 16_284
  ]
```

Omitted keys retain their built-in defaults. Configuration is validated when
the library application starts. Unknown keys, repeated keys, non-positive
values, excessively large integers, and inconsistent aggregate limits stop the
application from starting. The startup error identifies the invalid setting.

Limits are process-wide and immutable for one application instance. Restart
the application after changing them. Public PDF and rendering options cannot
override these values.

## Available limits

| Key | Default | Scope |
| --- | ---: | --- |
| `max_svg_bytes` | 5,000,000 | Encoded SVG source |
| `max_svg_raster_dimension` | 8,192 | SVG raster width or height |
| `max_svg_raster_pixels` | 16,777,216 | SVG raster pixel count |
| `max_image_count` | 1,000 | Images in one render |
| `max_image_source_bytes` | 10,000,000 | One encoded image |
| `max_aggregate_image_source_bytes` | 50,000,000 | Encoded images in one render |
| `max_decoded_image_bytes` | 40,000,000 | One decoded image |
| `max_aggregate_decoded_image_bytes` | 80,000,000 | Decoded images in one render |
| `max_background_image_tiles` | 10,000 | Repeated background tiles in one render |
| `max_layout_cardinality` | 1,000 | Grid tracks, placements, and table spans |
| `max_pdf_input_bytes` | 50,000,000 | One PDF input |
| `max_pdf_objects` | 100,000 | Parsed PDF objects |
| `max_pdf_object_stream_entries` | 10,000 | Entries in one PDF object stream |
| `max_pdf_pages` | 10,000 | Pages in one PDF |
| `max_pdf_page_tree_depth` | 1,000 | PDF page-tree nesting |
| `max_pdf_reference_chain_depth` | 1,000 | Indirect-reference nesting |
| `max_pdf_reference_resolution_work` | 25,000 | Aggregate reference resolutions |
| `max_pdf_value_depth` | 100 | Nested PDF arrays and dictionaries |
| `max_pdf_decoded_stream_bytes` | 25,000,000 | One decoded PDF stream |
| `max_pdf_decompression_ratio` | 100 | Decoded-to-encoded stream ratio |
| `max_pdf_xref_length_candidates` | 1,000 | Candidate indirect `/Length` objects |
| `max_pdf_xref_revisions` | 1,000 | Incremental cross-reference revisions |
| `max_pdf_info_value_bytes` | 1,000,000 | One metadata value supplied for PDF generation or update |
| `max_pdf_info_total_bytes` | 5,000,000 | Metadata values supplied for one PDF generation or update |
| `max_merge_inputs` | 100 | PDFs in one merge |
| `max_aggregate_merge_input_bytes` | 100,000,000 | PDF bytes in one merge |
| `max_merged_objects` | 100,000 | Objects in merged output |
| `max_merged_pages` | 10,000 | Pages in merged output |
| `max_text_decoded_content_bytes` | 50,000,000 | Decoded content used by extraction |
| `max_text_parsed_instructions` | 100,000 | Unique parsed content instructions |
| `max_text_stream_uses` | 100,000 | Content stream references traversed |
| `max_text_instruction_uses` | 1,000,000 | Aggregate content instruction work |
| `max_text_form_expansions` | 10,000 | Form XObject executions |
| `max_text_spans` | 25,000 | Extracted spans per page |
| `max_cmap_bytes` | 1,000,000 | One CMap stream |
| `max_cmap_entries` | 100,000 | Mappings in one CMap |
| `max_cid_width_entries` | 65,536 | CID width entries |
| `max_form_xobject_depth` | 20 | Form XObject nesting |
| `max_font_cache_entries` | 64 | Parsed font files retained by the cache |

`NativeElixirPdfUtilities.Limits.defaults/0` returns these built-in values, and
`NativeElixirPdfUtilities.Limits.effective/0` returns the values loaded for the
current application instance.

Increasing a limit increases the maximum memory, CPU, parsing work, or native
raster allocation an untrusted document can request. Tune limits against the
application's request-size controls, concurrency, memory budget, and timeout
policy rather than considering each value in isolation.

PDF-format bounds, CSS validity ranges, and local-resource authorization are not
resource tuning controls. Those checks remain fixed. In particular, local
images and document-selected fonts still require a per-render `base_url`
authorization root.
