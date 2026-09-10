# Resource limits

The library limits document size and processing work. When a limit is reached,
the operation returns a [diagnostic](diagnostics.md), usually with reason
`:resource_limit_exceeded`.

## Configure limits

Set only the values you want to change in `config/config.exs` or
`config/runtime.exs`:

```elixir
config :native_elixir_pdf_utilities,
  limits: [
    max_pdf_input_bytes: 75_000_000,
    max_merge_inputs: 20
  ]
```

Unspecified settings keep their defaults. Restart the application after a
change. Limits apply across the application and cannot be overridden for one
call. Invalid settings prevent startup and identify the problem.

Values must be positive integers. Aggregate image and metadata limits must
allow at least one maximum-size item. The settings below do not impose an
application-wide memory cap, timeout, or general HTML-source/output byte limit.

## PDF inputs

These limits apply when reading existing PDFs.

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_pdf_numeric_magnitude` | 1,000,000,000 | Absolute numeric operands and font metrics used for text extraction, and shared PDF geometry arrays |
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

## HTML, images, and layout

| Setting | Default | Applies to |
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
| `max_css_numeric_magnitude` | 1,000,000,000 | Absolute CSS numeric operands and converted lengths, excluding identifiers and quoted text |
| `max_layout_cardinality` | 1,000 | Grid tracks, placements, and table spans |

See [render options and supported formats](html-to-pdf-compatibility.md).

## Fonts

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_font_cmap_work` | 1,000,000 | Encoding records, segments, and codepoint visits prepared per TTF character map |
| `max_font_cache_entries` | 64 | Parsed font files retained by the cache |
| `max_system_font_cache_entries` | 64 | Positive and negative installed-font discovery results retained by the cache |

See [font configuration](html-to-pdf-compatibility.md#fonts-and-text).

## Metadata and bookmarks

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_pdf_info_value_bytes` | 1,000,000 | One metadata value supplied for PDF generation or update |
| `max_pdf_info_total_bytes` | 5,000,000 | Metadata values supplied for one PDF generation or update |
| `max_pdf_outline_items` | 10,000 | Outline items read or generated for one PDF |
| `max_pdf_outline_depth` | 64 | Nested outline levels |
| `max_pdf_outline_title_bytes` | 16,384 | UTF-8 bytes in one outline title |
| `max_pdf_outline_total_title_bytes` | 5,000,000 | UTF-8 title bytes across one outline tree |
| `max_pdf_name_tree_nodes` | 10,000 | Name-tree nodes traversed for named destinations |
| `max_pdf_named_destinations` | 10,000 | Legacy and name-tree destination entries in one PDF |

See [metadata updates](pdf-information.md#updating-information) and [bookmarks](pdf-outlines.md).

## Stamping

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_stamp_text_bytes` | 1,000,000 | One text/watermark input, or all generated page-number strings |
| `max_stamp_decoded_content_bytes` | 50,000,000 | Decoded stream occurrences and joining newlines across distinct overlay source pages |

The text limit counts one text/watermark input or all generated page-number
strings. Overlay bytes count each content-stream occurrence and joining
newline across distinct source pages; reusing an overlay page does not charge
its content again. See [Stamping](https://github.com/Cees-Kettenis/native_elixir_pdf_utilities/blob/main/docs/pdf-stamping.md).

## Merging and splitting

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_merge_inputs` | 100 | PDFs in one merge |
| `max_aggregate_merge_input_bytes` | 100,000,000 | PDF bytes in one merge |
| `max_merged_objects` | 100,000 | Objects in merged output |
| `max_merged_pages` | 10,000 | Pages in merged output |
| `max_split_outputs` | 1,000 | PDFs returned by one split operation |
| `max_split_object_writes` | 1,000,000 | Aggregate objects serialized by one split operation |
| `max_aggregate_split_output_bytes` | 100,000,000 | Aggregate bytes returned by one split operation |

Merge limits apply across the combined input/output. Split byte and object
limits apply across all returned PDFs. See [Merging](pdf-merging.md) and
[Splitting](pdf-page-transforms.md#splitting).

## Text extraction

| Setting | Default | Applies to |
| --- | ---: | --- |
| `max_text_decoded_content_bytes` | 50,000,000 | Decoded content used by extraction |
| `max_text_parsed_instructions` | 100,000 | Unique parsed content instructions |
| `max_text_stream_uses` | 100,000 | Content stream references traversed |
| `max_text_instruction_uses` | 1,000,000 | Aggregate content instruction work |
| `max_text_form_expansions` | 10,000 | Form XObject executions |
| `max_text_spans` | 25,000 | Extracted spans per page |
| `max_text_layout_whitespace_bytes` | 1,000,000 | Coordinate-based padding spaces reconstructed across one text extraction |
| `max_cmap_bytes` | 1,000,000 | One CMap stream |
| `max_cmap_entries` | 100,000 | Mappings in one CMap |
| `max_cid_width_entries` | 65,536 | CID width entries |
| `max_form_xobject_depth` | 20 | Form XObject nesting |

Reconstructed whitespace counts added padding, not original text or page
separators. It applies only to `layout: true`. The PDF CID range is fixed at
0 through 65,535, so `max_cid_width_entries` cannot exceed 65,536.
See [Text extraction](text-extraction.md).

## Check active settings

`NativeElixirPdfUtilities.Limits.defaults/0` returns built-in values.
`NativeElixirPdfUtilities.Limits.effective/0` returns the active configuration.
